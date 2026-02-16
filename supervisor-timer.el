;;; supervisor-timer.el --- Timer subsystem for supervisor.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; Author: telecommuter <telecommuter@riseup.net>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of supervisor.el.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Timer subsystem for supervisor.el.
;; Run M-x supervisor-handbook for full documentation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Forward declarations for supervisor-core functions we depend on
(declare-function supervisor--log "supervisor-core" (level format-string &rest args))
(declare-function supervisor--emit-event "supervisor-core" (type id msg &optional metadata))
(declare-function supervisor--build-plan "supervisor-core" (programs))
(declare-function supervisor--effective-programs "supervisor-core" ())
(declare-function supervisor--start-entry-async "supervisor-core" (entry callback))
(declare-function supervisor--get-effective-enabled "supervisor-core" (id config-enabled))
(declare-function supervisor-plan-entries "supervisor-core" (plan))
(declare-function supervisor-plan-by-target "supervisor-core" (plan))
(declare-function supervisor-plan-target-members "supervisor-core" (plan))
(declare-function supervisor-entry-id "supervisor-core" (entry))
(declare-function supervisor-entry-type "supervisor-core" (entry))
(declare-function supervisor-entry-enabled-p "supervisor-core" (entry))
(declare-function supervisor--get-entry-for-id "supervisor-core" (id))
(declare-function supervisor--manual-start "supervisor-core" (id))
(declare-function supervisor--dag-start-with-deps "supervisor-core" (entries callback))
(declare-function supervisor--expand-transaction "supervisor-core"
  (root entries-by-id target-members order-index))
(declare-function supervisor--materialize-target-members "supervisor-core" (entries))
(declare-function supervisor-plan-order-index "supervisor-core" (plan))

;; Forward declarations for variables from supervisor-core
(defvar supervisor-mode)
(defvar supervisor--shutting-down)
(defvar supervisor--processes)
(defvar supervisor--invalid)
(defvar supervisor--oneshot-completed)
(defvar supervisor--target-convergence)
(defvar supervisor--target-converging)
(defvar supervisor--target-members)
(defvar supervisor--target-member-reverse)
(defvar supervisor--current-plan)
(defvar supervisor--mask-override)

;;; Timer Subsystem Gate

;;;###autoload
(define-minor-mode supervisor-timer-subsystem-mode
  "Toggle the timer subsystem for supervisor.el.

When enabled, supervisor will process timer definitions from
`supervisor-timers' (and built-in timers such as the daily
logrotate maintenance job) and schedule units accordingly.

The timer subsystem requires `supervisor-mode' to be enabled.  If
`supervisor-mode' is disabled, timer functionality is a no-op even
if this mode is enabled.

When disabled, timer code will not:
- Build timer plans/lists in startup or reconcile flows
- Start, tick, or reschedule scheduler loops
- Trigger timer-driven unit runs
- Execute retry or catch-up logic
- Emit timer-specific events
- Load or save timer-state persistence data"
  :global t
  :group 'supervisor-timer
  :init-value t
  :lighter nil
  (if supervisor-timer-subsystem-mode
      (when (fboundp 'supervisor--log)
        (supervisor--log 'info "timer subsystem mode enabled"))
    ;; When disabling, stop the scheduler immediately
    (when (fboundp 'supervisor-timer-scheduler-stop)
      (supervisor-timer-scheduler-stop))
    (when (fboundp 'supervisor--log)
      (supervisor--log 'info "timer subsystem mode disabled"))))

(defun supervisor-timer-subsystem-active-p ()
  "Return non-nil if timer subsystem is active.
The timer subsystem is active when both `supervisor-timer-subsystem-mode'
and `supervisor-mode' are enabled."
  (and supervisor-timer-subsystem-mode
       (bound-and-true-p supervisor-mode)))

;;; Configuration

(defgroup supervisor-timer nil
  "Timer subsystem for supervisor.el."
  :group 'supervisor)

;; supervisor-timers defcustom is defined in supervisor-core.el
(defvar supervisor-timers)

(defcustom supervisor-timer-state-file
  (expand-file-name "supervisor/timer-state.eld"
                    (or (getenv "XDG_STATE_HOME")
                        (expand-file-name "~/.local/state")))
  "File path for persisting timer state.
Set to nil to disable timer state persistence."
  :type '(choice file (const nil))
  :group 'supervisor-timer)

(defcustom supervisor-timer-retry-intervals '(30 120 600)
  "List of retry intervals in seconds for failed timer runs.
Each element is the delay before the next retry attempt.
Default is 30s, 2m, 10m (3 attempts total).
Set to nil to disable retries."
  :type '(repeat integer)
  :group 'supervisor-timer)

(defcustom supervisor-timer-catch-up-limit (* 24 60 60)
  "Maximum age in seconds for catch-up runs after downtime.
When scheduler starts, timers with `:persistent t' that missed
runs within this window will trigger a catch-up run.
Default is 24 hours.  Set to 0 to disable catch-up."
  :type 'integer
  :group 'supervisor-timer)

;;; Timer Schema

(defconst supervisor-timer-schema-version 1
  "Schema version for timer definitions.")

(cl-defstruct (supervisor-timer (:constructor supervisor-timer--create)
                                (:copier nil))
  "Timer definition for scheduled unit execution.

Timers can trigger oneshot, simple, and target units.

Field documentation:
  id              - Unique identifier string (required)
  target          - ID of unit to trigger (required)
  enabled         - Whether this timer is active (boolean, default t)
  on-calendar     - Calendar schedule plist (optional)
  on-startup-sec  - Seconds after startup to trigger (optional)
  on-unit-active-sec - Seconds after target completes to trigger again (optional)
  persistent      - Enable catch-up after downtime (boolean, default t)"
  (id nil :type string :documentation "Unique timer identifier (required)")
  (target nil :type string :documentation "Target unit ID (required)")
  (enabled t :type boolean :documentation "Whether timer is active")
  (on-calendar nil :type (or null list) :documentation "Calendar schedule plist")
  (on-startup-sec nil :type (or null integer) :documentation "Startup delay seconds")
  (on-unit-active-sec nil :type (or null integer) :documentation "Repeat interval seconds")
  (persistent t :type boolean :documentation "Enable catch-up after downtime"))

(defconst supervisor-timer-required-fields '(id target)
  "List of required fields in a timer definition.")

(defconst supervisor-timer-trigger-fields
  '(on-calendar on-startup-sec on-unit-active-sec)
  "List of trigger fields.  At least one must be specified.")

(defconst supervisor-timer-valid-keywords
  '(:id :target :enabled :on-calendar :on-startup-sec :on-unit-active-sec :persistent)
  "List of valid timer keywords.")

(defconst supervisor-timer-calendar-fields
  '(:minute :hour :day-of-month :month :day-of-week)
  "Valid fields in an :on-calendar schedule.")

;;; Timer Runtime State
;;
;; These variables use supervisor-- prefix for backward compatibility with
;; dashboard and CLI code that reference them directly.

(defvar supervisor--invalid-timers (make-hash-table :test 'equal)
  "Hash table mapping invalid timer IDs to reason strings.")

(defvar supervisor--timer-state (make-hash-table :test 'equal)
  "Hash table mapping timer ID to runtime state plist.
State keys:
  :last-run-at         - timestamp of last trigger
  :last-success-at     - timestamp of last successful completion
  :last-failure-at     - timestamp of last failed completion
  :last-exit           - exit code of last run (0 = success)
  :retry-attempt       - current retry attempt number (0 = not retrying)
  :retry-next-at       - timestamp of next retry (nil if not pending)
  :last-missed-at      - timestamp of last missed run
  :last-miss-reason    - symbol: overlap, downtime, disabled
  :next-run-at         - timestamp of next scheduled run
  :last-result         - symbol: success, failure, skip (v2)
  :last-result-reason  - symbol or nil: overlap, etc. (v2)
  :last-target-type    - symbol: oneshot, simple, target (v2)
All timestamps are float seconds since epoch.")

(defvar supervisor--timer-scheduler nil
  "Timer object for the scheduler loop.")

(defvar supervisor--timer-list nil
  "List of validated timer structs for active scheduling.")

(defvar supervisor--scheduler-startup-time nil
  "Timestamp when scheduler was started (for monotonic triggers).")

(defvar supervisor--timer-state-loaded nil
  "Non-nil when timer state has been loaded from file.")

;;; Timer State Persistence

(defconst supervisor-timer-state-schema-version 2
  "Schema version for persistent timer state file.
Version 2 adds :last-result, :last-result-reason, :last-target-type.")

(defconst supervisor-timer--state-persist-keys
  '(:last-run-at :last-success-at :last-failure-at :last-exit
    :last-missed-at :last-miss-reason
    :last-result :last-result-reason :last-target-type)
  "State keys that should be persisted across restarts.
Excludes transient keys like :next-run-at, :retry-attempt, :retry-next-at,
and :startup-triggered which are computed fresh each session.
Schema v2 added :last-result, :last-result-reason, :last-target-type.")

(defun supervisor--timer-state-file-path ()
  "Return the path to the timer state file, or nil if disabled."
  supervisor-timer-state-file)

(defun supervisor-timer--ensure-state-dir ()
  "Ensure the directory for the timer state file exists."
  (when-let* ((path (supervisor--timer-state-file-path)))
    (let ((dir (file-name-directory path)))
      (unless (file-directory-p dir)
        (make-directory dir t)))))

(defun supervisor--timer-state-to-alist ()
  "Convert timer state hash to sorted alist for persistence.
Only includes keys from `supervisor-timer--state-persist-keys'.
Output is sorted by timer ID for deterministic serialization."
  (let (result)
    (maphash
     (lambda (id state)
       (when state
         (let (filtered)
           (dolist (key supervisor-timer--state-persist-keys)
             ;; Persist every key present in state (even if nil).
             (when (plist-member state key)
               (setq filtered (plist-put filtered key
                                         (plist-get state key)))))
           (when filtered
             (push (cons id filtered) result)))))
     supervisor--timer-state)
    ;; Sort by ID for deterministic output
    (sort result (lambda (a b) (string< (car a) (car b))))))

(cl-defun supervisor-timer--save-state ()
  "Save timer state to file using atomic write.
Uses temp file + rename pattern for crash safety.
Returns t on success, nil on failure.
Does nothing if timer subsystem is not active."
  (unless (supervisor-timer-subsystem-active-p)
    (cl-return-from supervisor-timer--save-state nil))
  (let ((path (supervisor--timer-state-file-path)))
    (when path
      (supervisor-timer--ensure-state-dir)
      (let* ((state-alist (supervisor--timer-state-to-alist))
             (data `((version . ,supervisor-timer-state-schema-version)
                     (timers . ,state-alist)))
             (temp-file (concat path ".tmp"))
             (coding-system-for-write 'utf-8-unix))
        (condition-case err
            (progn
              (with-temp-file temp-file
                (insert ";; Supervisor timer state - do not edit manually\n")
                (insert ";; Schema version: "
                        (number-to-string supervisor-timer-state-schema-version)
                        "\n")
                (pp data (current-buffer)))
              (rename-file temp-file path t)
              t)
          (error
           (supervisor--log 'warning "Failed to save timer state: %s"
                            (error-message-string err))
           (when (file-exists-p temp-file)
             (delete-file temp-file))
           nil))))))

(cl-defun supervisor-timer--load-state ()
  "Load timer state from file into memory.
Returns t on success, nil on failure or if file does not exist.
Merges loaded state with existing in-memory state, preferring file values
for persisted keys while preserving runtime-computed keys.
Does nothing if timer subsystem is not active."
  (unless (supervisor-timer-subsystem-active-p)
    (cl-return-from supervisor-timer--load-state nil))
  (let ((path (supervisor--timer-state-file-path)))
    (when (and path (file-exists-p path))
      (condition-case err
          (let* ((data (with-temp-buffer
                         (insert-file-contents path)
                         (read (current-buffer))))
                 (version (alist-get 'version data))
                 (timers (alist-get 'timers data)))
            ;; Check version compatibility - accept v1 and v2, reject future
            (when (or (null version)
                      (> version supervisor-timer-state-schema-version))
              (supervisor--log 'warning
                               "Timer state file version %s is incompatible (supported: %s), skipping"
                               version supervisor-timer-state-schema-version)
              (signal 'error (list "Incompatible timer state schema version")))
            ;; v1 loads fine -- new v2 keys are simply absent (nil defaults)
            (when (and version (= version 1))
              (supervisor--log 'info "Upgrading timer state from schema v1 to v2"))
            ;; Merge loaded state into hash
            (dolist (entry timers)
              (let ((id (car entry))
                    (saved-state (cdr entry)))
                (let ((current (or (gethash id supervisor--timer-state) nil)))
                  ;; Merge saved keys into current state
                  (dolist (key supervisor-timer--state-persist-keys)
                    (when (plist-member saved-state key)
                      (setq current
                            (plist-put current key
                                       (plist-get saved-state key)))))
                  (puthash id current supervisor--timer-state))))
            (setq supervisor--timer-state-loaded t)
            (supervisor--log 'info "Loaded timer state for %d timers from %s"
                             (length timers) path)
            t)
        (error
         (supervisor--log 'warning "Failed to load timer state from %s: %s"
                          path (error-message-string err))
         nil)))))

;;; Timer Validation and Parsing

(defun supervisor-timer--calendar-plist-p (obj)
  "Return non-nil if OBJ is a valid calendar plist structure.
Must be a proper (non-dotted) list starting with a keyword."
  (and (proper-list-p obj)
       obj
       (keywordp (car obj))))

(defun supervisor-timer--validate-single-calendar (calendar)
  "Validate a single CALENDAR schedule plist.
Return nil if valid, or an error message string."
  (cond
   ((not (proper-list-p calendar))
    ":on-calendar must be a proper plist, not a dotted pair")
   ((null calendar)
    ":on-calendar entry cannot be empty")
   ((not (keywordp (car calendar)))
    ":on-calendar entry must start with a keyword")
   (t
    (let ((valid-fields supervisor-timer-calendar-fields)
          (field-ranges '((:minute 0 59)
                          (:hour 0 23)
                          (:day-of-month 1 31)
                          (:month 1 12)
                          (:day-of-week 0 6))))
      (cl-loop for (key val) on calendar by #'cddr
               unless (memq key valid-fields)
               return (format ":on-calendar has unknown field %s" key)
               unless (or (eq val '*)
                          (integerp val)
                          (and (proper-list-p val) val (cl-every #'integerp val)))
               return (format ":on-calendar field %s must be integer, non-empty list of integers, or *" key)
               ;; Range validation
               for range = (cdr (assq key field-ranges))
               for min-val = (car range)
               for max-val = (cadr range)
               when (and range (not (eq val '*)))
               unless (if (proper-list-p val)
                          (cl-every (lambda (v) (and (>= v min-val) (<= v max-val))) val)
                        (and (>= val min-val) (<= val max-val)))
               return (format ":on-calendar field %s value out of range (%d-%d)" key min-val max-val))))))

(defun supervisor-timer--validate-calendar (calendar)
  "Validate CALENDAR schedule (single plist or list of plists).
Return nil if valid, or an error message string."
  (cond
   ((not (listp calendar))
    ":on-calendar must be a plist or list of plists")
   ((null calendar)
    ":on-calendar cannot be empty")
   ((keywordp (car calendar))
    (supervisor-timer--validate-single-calendar calendar))
   ((listp (car calendar))
    (cl-loop for entry in calendar
             unless (supervisor-timer--calendar-plist-p entry)
             return (format ":on-calendar list entry must be a plist, got %S" entry)
             for err = (supervisor-timer--validate-single-calendar entry)
             when err return err))
   (t
    ":on-calendar must be a plist or list of plists")))

(defun supervisor-timer--validate (timer-plist plan)
  "Validate TIMER-PLIST against PLAN.
Return nil if valid, or an error message string.
PLAN is used to verify the target exists and is oneshot, simple, or target."
  (catch 'invalid
    (let ((id (plist-get timer-plist :id))
          (target (plist-get timer-plist :target)))
      ;; Check for unknown keywords
      (let ((unknown-err
             (cl-loop for (key _val) on timer-plist by #'cddr
                      unless (memq key supervisor-timer-valid-keywords)
                      return (format "unknown keyword %s" key))))
        (when unknown-err (throw 'invalid unknown-err)))
      ;; Check required fields
      (unless (and id (stringp id) (not (string-empty-p id)))
        (throw 'invalid ":id must be a non-empty string"))
      (unless (and target (stringp target) (not (string-empty-p target)))
        (throw 'invalid ":target must be a non-empty string"))
      ;; Check at least one trigger with non-nil value
      (unless (or (plist-get timer-plist :on-calendar)
                  (plist-get timer-plist :on-startup-sec)
                  (plist-get timer-plist :on-unit-active-sec))
        (throw 'invalid
               "at least one trigger required (:on-calendar, :on-startup-sec, or :on-unit-active-sec)"))
      ;; Validate trigger field types
      (when (plist-member timer-plist :on-startup-sec)
        (let ((startup-sec (plist-get timer-plist :on-startup-sec)))
          (unless (and startup-sec (integerp startup-sec) (> startup-sec 0))
            (throw 'invalid ":on-startup-sec must be a positive integer"))))
      (when (plist-member timer-plist :on-unit-active-sec)
        (let ((active-sec (plist-get timer-plist :on-unit-active-sec)))
          (unless (and active-sec (integerp active-sec) (> active-sec 0))
            (throw 'invalid ":on-unit-active-sec must be a positive integer"))))
      ;; Validate calendar schedule
      (when (plist-member timer-plist :on-calendar)
        (let ((calendar (plist-get timer-plist :on-calendar)))
          (when-let* ((err (supervisor-timer--validate-calendar calendar)))
            (throw 'invalid err))))
      ;; Validate enabled field
      (when (plist-member timer-plist :enabled)
        (unless (booleanp (plist-get timer-plist :enabled))
          (throw 'invalid ":enabled must be a boolean")))
      ;; Validate persistent field
      (when (plist-member timer-plist :persistent)
        (unless (booleanp (plist-get timer-plist :persistent))
          (throw 'invalid ":persistent must be a boolean")))
      ;; Validate target exists and is oneshot, simple, or target
      (when plan
        (let* ((entries (supervisor-plan-entries plan))
               (target-entry (cl-find target entries
                                      :key #'supervisor-entry-id
                                      :test #'equal)))
          (unless target-entry
            (throw 'invalid
                   (format ":target '%s' not found in loaded unit files" target)))
          (let ((ttype (supervisor-entry-type target-entry)))
            (unless (memq ttype '(oneshot simple target))
              (throw 'invalid
                     (format ":target '%s' must be oneshot, simple, or target, not %s"
                             target ttype)))
            (when (and (eq ttype 'target)
                       (not (string-suffix-p ".target" target)))
              (throw 'invalid
                     (format ":target '%s' is type target but ID does not end in .target"
                             target))))))
      ;; Valid
      nil)))

(defun supervisor-timer--parse (timer-plist)
  "Parse TIMER-PLIST into a supervisor-timer struct.
Assumes validation has already passed."
  (supervisor-timer--create
   :id (plist-get timer-plist :id)
   :target (plist-get timer-plist :target)
   :enabled (if (plist-member timer-plist :enabled)
                (plist-get timer-plist :enabled)
              t)
   :on-calendar (plist-get timer-plist :on-calendar)
   :on-startup-sec (plist-get timer-plist :on-startup-sec)
   :on-unit-active-sec (plist-get timer-plist :on-unit-active-sec)
   :persistent (if (plist-member timer-plist :persistent)
                   (plist-get timer-plist :persistent)
                 t)))

(defun supervisor-timer-build-list (plan)
  "Build list of validated timer structs from user and built-in timers.
User timers from `supervisor-timers' are merged with built-in timers
from `supervisor--builtin-timers'; a user timer with the same `:id'
overrides the built-in one.  Invalid timers are added to
`supervisor--invalid-timers' hash.  PLAN is used for target validation.
Note: No gate check - validation should work even when subsystem is off."
  (clrhash supervisor--invalid-timers)
  (let* ((user-ids (mapcar (lambda (tp) (plist-get tp :id))
                           supervisor-timers))
         (merged (append supervisor-timers
                         (cl-remove-if
                          (lambda (bt) (member (plist-get bt :id) user-ids))
                          (if (boundp 'supervisor--builtin-timers)
                              supervisor--builtin-timers
                            nil))))
         (timers nil)
         (seen-ids (make-hash-table :test 'equal)))
    (dolist (timer-plist merged)
      (let* ((id (or (plist-get timer-plist :id)
                     (format "timer#%d" (hash-table-count supervisor--invalid-timers)))))
        (cond
         ;; Check for duplicates first (before validation) for deterministic rejection
         ((gethash id seen-ids)
          ;; Only add to invalid if not already there (preserve first error)
          (unless (gethash id supervisor--invalid-timers)
            (puthash id "duplicate timer ID" supervisor--invalid-timers))
          (supervisor--log 'warning "duplicate timer ID '%s', skipping" id))
         ;; Then validate
         (t
          ;; Mark ID as seen regardless of validity
          (puthash id t seen-ids)
          (let ((error-reason (supervisor-timer--validate timer-plist plan)))
            (if error-reason
                (progn
                  (puthash id error-reason supervisor--invalid-timers)
                  (supervisor--log 'warning "INVALID timer %s - %s" id error-reason))
              (push (supervisor-timer--parse timer-plist) timers)))))))
    (nreverse timers)))

;;; Calendar Trigger Computation

(defun supervisor-timer--calendar-field-matches-p (field-value time-value)
  "Return non-nil if FIELD-VALUE matches TIME-VALUE.
FIELD-VALUE can be *, an integer, or a list of integers."
  (cond
   ((eq field-value '*) t)
   ((integerp field-value) (= field-value time-value))
   ((listp field-value) (memq time-value field-value))
   (t nil)))

(defun supervisor-timer--calendar-matches-time-p (calendar decoded-time)
  "Return non-nil if CALENDAR spec matches DECODED-TIME.
DECODED-TIME is from `decode-time'.  CALENDAR is a plist with
:minute, :hour, :day-of-month, :month, :day-of-week fields."
  (let ((minute (plist-get calendar :minute))
        (hour (plist-get calendar :hour))
        (day-of-month (plist-get calendar :day-of-month))
        (month (plist-get calendar :month))
        (day-of-week (plist-get calendar :day-of-week)))
    (and (or (null minute)
             (supervisor-timer--calendar-field-matches-p minute (decoded-time-minute decoded-time)))
         (or (null hour)
             (supervisor-timer--calendar-field-matches-p hour (decoded-time-hour decoded-time)))
         (or (null day-of-month)
             (supervisor-timer--calendar-field-matches-p day-of-month (decoded-time-day decoded-time)))
         (or (null month)
             (supervisor-timer--calendar-field-matches-p month (decoded-time-month decoded-time)))
         (or (null day-of-week)
             (supervisor-timer--calendar-field-matches-p day-of-week (decoded-time-weekday decoded-time))))))

(defun supervisor-timer--calendar-field-to-list (field min-val max-val)
  "Convert calendar FIELD to sorted list of integers.
FIELD can be *, an integer, or a list of integers.
For *, returns range from MIN-VAL to MAX-VAL."
  (sort (cond ((eq field '*) (number-sequence min-val max-val))
              ((listp field) (copy-sequence field))
              (t (list field)))
        #'<))

(defun supervisor-timer--calendar-find-time-on-day (year month day cal-hour cal-minute min-hour min-minute)
  "Find first time matching CAL-HOUR:CAL-MINUTE on YEAR-MONTH-DAY.
YEAR, MONTH, and DAY specify the date to search.
CAL-HOUR and CAL-MINUTE are calendar field specs (*, int, or list).
Only consider times at or after MIN-HOUR:MIN-MINUTE.
Return timestamp or nil if no valid time on this day.

Handles DST gaps by validating that encoded time matches requested fields."
  (when (< min-hour 24)
    (let ((hours (supervisor-timer--calendar-field-to-list cal-hour 0 23))
          (minutes (supervisor-timer--calendar-field-to-list cal-minute 0 59)))
      ;; Filter to valid ranges (hour 0-23, minute 0-59)
      (setq hours (cl-remove-if-not (lambda (h) (and (>= h 0) (<= h 23))) hours))
      (setq minutes (cl-remove-if-not (lambda (m) (and (>= m 0) (<= m 59))) minutes))
      (catch 'found
        (dolist (h hours)
          (when (>= h min-hour)
            (let ((start-m (if (= h min-hour) min-minute 0)))
              (dolist (m minutes)
                (when (>= m start-m)
                  ;; Encode the candidate time
                  (let* ((candidate (encode-time 0 m h day month year))
                         (decoded (decode-time candidate))
                         (actual-hour (decoded-time-hour decoded))
                         (actual-minute (decoded-time-minute decoded)))
                    ;; DST gap check: verify encode-time didn't shift the time
                    (when (and (= actual-hour h) (= actual-minute m))
                      (throw 'found (float-time candidate)))))))))
        nil))))

(defun supervisor-timer--calendar-next-minute (from-time calendar max-days)
  "Find next time matching CALENDAR starting strictly after FROM-TIME.
Search at most MAX-DAYS days into the future.
Return timestamp or nil if not found within limit.

Uses day-by-day iteration for efficiency (vs minute-by-minute)."
  (let* ((decoded (decode-time from-time))
         (from-year (decoded-time-year decoded))
         (from-month (decoded-time-month decoded))
         (from-day (decoded-time-day decoded))
         (from-hour (decoded-time-hour decoded))
         (from-min (decoded-time-minute decoded))
         ;; Calendar constraints (default to * which matches anything)
         (cal-hour (or (plist-get calendar :hour) '*))
         (cal-minute (or (plist-get calendar :minute) '*))
         (cal-month (or (plist-get calendar :month) '*))
         (cal-dom (or (plist-get calendar :day-of-month) '*))
         (cal-dow (or (plist-get calendar :day-of-week) '*)))
    (catch 'found
      (dotimes (day-offset max-days)
        ;; Compute the date for this day
        (let* ((day-time (encode-time 0 0 0 (+ from-day day-offset) from-month from-year))
               (day-dec (decode-time day-time))
               (year (decoded-time-year day-dec))
               (month (decoded-time-month day-dec))
               (day (decoded-time-day day-dec))
               (dow (decoded-time-weekday day-dec)))
          ;; Check day-level constraints
          (when (and (supervisor-timer--calendar-field-matches-p cal-month month)
                     (supervisor-timer--calendar-field-matches-p cal-dom day)
                     (supervisor-timer--calendar-field-matches-p cal-dow dow))
            ;; Day matches - find first valid hour:minute
            (let* ((is-from-day (= day-offset 0))
                   ;; On first day, need time strictly after from-time
                   (min-hour (if is-from-day from-hour 0))
                   (min-minute (if is-from-day (1+ from-min) 0)))
              ;; Handle minute overflow
              (when (> min-minute 59)
                (setq min-minute 0)
                (setq min-hour (1+ min-hour)))
              (let ((match-time (supervisor-timer--calendar-find-time-on-day
                                 year month day cal-hour cal-minute min-hour min-minute)))
                (when match-time
                  (throw 'found match-time)))))))
      nil)))

(defun supervisor-timer--next-calendar-time (timer from-time)
  "Compute next calendar trigger time for TIMER after FROM-TIME.
Return timestamp or nil if no calendar trigger configured."
  (let ((calendar (supervisor-timer-on-calendar timer)))
    (when calendar
      ;; Handle list of calendars - find earliest match
      ;; Search limit: 28 years (10228 days) covers full leap-day + weekday cycle
      (if (and (listp calendar) (listp (car calendar)))
          ;; List of calendar plists
          (let ((times (cl-loop for cal in calendar
                                for next = (supervisor-timer--calendar-next-minute
                                            from-time cal 10228)
                                when next collect next)))
            (when times (apply #'min times)))
        ;; Single calendar plist
        (supervisor-timer--calendar-next-minute from-time calendar 10228)))))

;;; Monotonic Trigger Computation

(defun supervisor-timer--next-startup-time (timer)
  "Compute next startup trigger time for TIMER.
Return timestamp or nil if no startup trigger or already fired."
  (when-let* ((delay (supervisor-timer-on-startup-sec timer)))
    (when supervisor--scheduler-startup-time
      (let* ((id (supervisor-timer-id timer))
             (state (gethash id supervisor--timer-state))
             (startup-triggered (plist-get state :startup-triggered))
             (trigger-time (+ supervisor--scheduler-startup-time delay)))
        ;; Only return if startup trigger hasn't fired yet this session
        (unless startup-triggered
          trigger-time)))))

(defun supervisor-timer--next-unit-active-time (timer)
  "Compute next unit-active trigger time for TIMER.
Return timestamp or nil if no unit-active trigger configured."
  (when-let* ((interval (supervisor-timer-on-unit-active-sec timer)))
    (let* ((id (supervisor-timer-id timer))
           (state (gethash id supervisor--timer-state))
           (last-success (plist-get state :last-success-at)))
      ;; Trigger interval seconds after last successful completion
      (when last-success
        (+ last-success interval)))))

;;; Next Due Time Computation

(defun supervisor-timer--compute-next-run (timer from-time)
  "Compute next run time for TIMER after FROM-TIME.
Return timestamp or nil if no trigger is due."
  (let ((calendar-next (supervisor-timer--next-calendar-time timer from-time))
        (startup-next (supervisor-timer--next-startup-time timer))
        (unit-active-next (supervisor-timer--next-unit-active-time timer)))
    ;; Return earliest non-nil time
    (let ((candidates (delq nil (list calendar-next startup-next unit-active-next))))
      (when candidates
        (apply #'min candidates)))))

(defun supervisor-timer--update-next-run (timer-id)
  "Update the :next-run-at for TIMER-ID in state table."
  (when-let* ((timer (cl-find timer-id supervisor--timer-list
                              :key #'supervisor-timer-id :test #'equal)))
    (let* ((state (or (gethash timer-id supervisor--timer-state)
                      (puthash timer-id nil supervisor--timer-state)))
           (next-run (supervisor-timer--compute-next-run timer (float-time))))
      (puthash timer-id (plist-put state :next-run-at next-run)
               supervisor--timer-state))))

;;; Overlap Detection

(defun supervisor-timer--target-active-p (timer)
  "Return non-nil if TIMER's target oneshot is currently active."
  (let* ((target-id (supervisor-timer-target timer))
         (proc (gethash target-id supervisor--processes)))
    (and proc (process-live-p proc))))

;;; Timer Execution

(defun supervisor-timer--get-entry-for-id (id)
  "Get the parsed entry for ID.
Delegates to `supervisor--get-entry-for-id' in supervisor-core.
Return a list of entry properties or nil if not found."
  (supervisor--get-entry-for-id id))

(defun supervisor-timer--maybe-mark-startup-consumed (timer)
  "Mark TIMER's startup trigger as consumed if due.
Call early in trigger flow to prevent retry loops on skipped runs.
Returns non-nil if startup trigger was just consumed."
  (when-let* ((startup-sec (supervisor-timer-on-startup-sec timer))
              (startup-time supervisor--scheduler-startup-time))
    (let* ((id (supervisor-timer-id timer))
           (state (gethash id supervisor--timer-state))
           (trigger-time (+ startup-time startup-sec))
           (now (float-time)))
      (when (and (>= now trigger-time)
                 (not (plist-get state :startup-triggered)))
        (setq state (plist-put state :startup-triggered t))
        (puthash id state supervisor--timer-state)
        t))))

(cl-defun supervisor-timer--trigger (timer reason)
  "Trigger TIMER's target unit.
REASON is a symbol describing why: scheduled, retry, manual, catch-up.
Returns t if triggered, nil if skipped."
  (let* ((id (supervisor-timer-id timer))
         (target-id (supervisor-timer-target timer)))
    ;; Check if timer is enabled
    (unless (supervisor-timer-enabled timer)
      (supervisor-timer--record-miss id 'disabled)
      (supervisor-timer--record-result id 'skip 'disabled)
      (supervisor--log 'info "timer %s: skipped (disabled)" id)
      (cl-return-from supervisor-timer--trigger nil))
    ;; Mark startup trigger consumed early to prevent retry loops
    (supervisor-timer--maybe-mark-startup-consumed timer)
    ;; Get the parsed entry for the target
    (let ((entry (supervisor-timer--get-entry-for-id target-id)))
      (unless entry
        (supervisor--log 'warning "timer %s: target %s not found" id target-id)
        (supervisor-timer--record-result id 'failure 'target-not-found)
        (let ((state (gethash id supervisor--timer-state)))
          (setq state (plist-put state :last-failure-at (float-time)))
          (puthash id state supervisor--timer-state))
        (supervisor--emit-event 'timer-failure id nil
                                (list :target target-id :reason 'not-found))
        (cl-return-from supervisor-timer--trigger nil))
      (let ((target-type (supervisor-entry-type entry)))
        ;; Record target type in state
        (let ((state (or (gethash id supervisor--timer-state) nil)))
          (setq state (plist-put state :last-target-type target-type))
          (puthash id state supervisor--timer-state))
        ;; Check if target is disabled or masked
        (let ((enabled-p (supervisor-entry-enabled-p entry)))
          (unless (supervisor--get-effective-enabled target-id enabled-p)
            (let* ((masked (eq (gethash target-id supervisor--mask-override)
                               'masked))
                   (reason (if masked 'masked-target 'disabled-target)))
              (supervisor-timer--record-miss id reason)
              (supervisor-timer--record-result id 'skip reason)
              (supervisor--log 'info "timer %s: skipped (target %s %s)"
                               id target-id
                               (if masked "masked" "disabled")))
            (cl-return-from supervisor-timer--trigger nil)))
        ;; Dispatch by target unit type
        (pcase target-type
          ('oneshot (supervisor-timer--trigger-oneshot timer reason entry))
          ('simple (supervisor-timer--trigger-simple timer reason entry))
          ('target (supervisor-timer--trigger-target timer reason))
          (_ (supervisor--log 'warning
                              "timer %s: unsupported target type %s"
                              id target-type)
             nil))))))

(cl-defun supervisor-timer--trigger-oneshot (timer reason entry)
  "Trigger oneshot ENTRY for TIMER with REASON.
Returns t if triggered, nil if skipped."
  (let ((id (supervisor-timer-id timer))
        (target-id (supervisor-timer-target timer))
        (now (float-time)))
    ;; Check for overlap
    (when (supervisor-timer--target-active-p timer)
      (supervisor-timer--record-miss id 'overlap)
      (supervisor-timer--record-result id 'skip 'overlap)
      (supervisor--log 'info "timer %s: skipped (target %s still active)"
                       id target-id)
      (supervisor--emit-event 'timer-overlap id nil
                              (list :target target-id :reason reason))
      (cl-return-from supervisor-timer--trigger-oneshot nil))
    ;; Start the oneshot
    (supervisor--log 'info "timer %s: triggering oneshot %s (%s)"
                     id target-id reason)
    (supervisor-timer--record-trigger id now reason target-id)
    (supervisor--start-entry-async
     entry
     (lambda (success)
       (supervisor-timer--on-target-complete id target-id success)))
    t))

(cl-defun supervisor-timer--trigger-simple (timer reason entry)
  "Trigger simple ENTRY for TIMER with REASON.
If already running, record success with already-active.
Returns t if triggered, nil if skipped."
  (let ((id (supervisor-timer-id timer))
        (target-id (supervisor-timer-target timer))
        (now (float-time)))
    ;; If already running, no-op success
    (when (supervisor-timer--target-active-p timer)
      (supervisor--log 'info "timer %s: target %s already active (no-op)"
                       id target-id)
      (supervisor-timer--record-trigger id now reason target-id)
      (supervisor-timer--record-result id 'success 'already-active)
      (cl-return-from supervisor-timer--trigger-simple t))
    ;; Start the simple service
    (supervisor--log 'info "timer %s: triggering simple %s (%s)"
                     id target-id reason)
    (supervisor-timer--record-trigger id now reason target-id)
    (supervisor--start-entry-async
     entry
     (lambda (success)
       (supervisor-timer--on-simple-complete id target-id success)))
    t))

(cl-defun supervisor-timer--trigger-target (timer reason)
  "Trigger target activation for TIMER with REASON.
Returns t if triggered, nil if skipped."
  (let ((id (supervisor-timer-id timer))
        (target-id (supervisor-timer-target timer))
        (now (float-time)))
    ;; Check if target is already converging
    (when (and (hash-table-p supervisor--target-converging)
               (gethash target-id supervisor--target-converging))
      (supervisor-timer--record-miss id 'target-converging)
      (supervisor-timer--record-result id 'skip 'target-converging)
      (supervisor--log 'info "timer %s: skipped (target %s converging)"
                       id target-id)
      (cl-return-from supervisor-timer--trigger-target nil))
    ;; Check if target is already reached
    (when (and (hash-table-p supervisor--target-convergence)
               (eq 'reached (gethash target-id supervisor--target-convergence)))
      (supervisor--log 'info "timer %s: target %s already reached (no-op)"
                       id target-id)
      (supervisor-timer--record-trigger id now reason target-id)
      (supervisor-timer--record-result id 'success 'already-reached)
      (cl-return-from supervisor-timer--trigger-target t))
    ;; Start target activation: compute closure and DAG-start members
    (supervisor--log 'info "timer %s: triggering target %s (%s)"
                     id target-id reason)
    (supervisor-timer--record-trigger id now reason target-id)
    (let* ((plan supervisor--current-plan)
           (members
            (when plan
              (or (supervisor-plan-target-members plan)
                  (supervisor--materialize-target-members
                   (supervisor-plan-entries plan)))))
           (closure
            (when plan
              (let ((entries-by-id (make-hash-table :test 'equal)))
                (dolist (e (supervisor-plan-entries plan))
                  (puthash (supervisor-entry-id e) e entries-by-id))
                (supervisor--expand-transaction
                 target-id entries-by-id members
                 (supervisor-plan-order-index plan)))))
           (to-start
            (when (and plan closure)
              (cl-remove-if
               (lambda (e)
                 (let* ((eid (supervisor-entry-id e))
                        (proc (gethash eid supervisor--processes)))
                   (or (not (gethash eid closure))
                       ;; Exclude targets OTHER than the triggered one
                       (and (eq (supervisor-entry-type e) 'target)
                            (not (equal eid target-id)))
                       (and proc (process-live-p proc)))))
               (supervisor-plan-by-target plan)))))
      ;; Initialize convergence infrastructure for this target.
      ;; Update target-members so target-check-convergence can read
      ;; membership, and rebuild the reverse index so dag-mark-ready
      ;; triggers convergence checks when members complete.
      (when (and members closure)
        (let ((target-mem (gethash target-id members)))
          (when target-mem
            (unless (hash-table-p supervisor--target-members)
              (setq supervisor--target-members
                    (make-hash-table :test 'equal)))
            (puthash target-id target-mem supervisor--target-members)))
        ;; Rebuild reverse index for closure members
        (unless (hash-table-p supervisor--target-member-reverse)
          (setq supervisor--target-member-reverse
                (make-hash-table :test 'equal)))
        (let ((mem (gethash target-id members)))
          (when mem
            (dolist (mid (plist-get mem :requires))
              (when (gethash mid closure)
                (puthash mid (cons target-id
                                   (gethash mid
                                            supervisor--target-member-reverse))
                         supervisor--target-member-reverse)))
            (dolist (mid (plist-get mem :wants))
              (when (gethash mid closure)
                (puthash mid (cons target-id
                                   (gethash mid
                                            supervisor--target-member-reverse))
                         supervisor--target-member-reverse))))))
      (if (null to-start)
          ;; Nothing to start, check convergence immediately
          (supervisor-timer--on-target-converge id target-id)
        (supervisor--dag-start-with-deps
         to-start
         (lambda ()
           (supervisor-timer--on-target-converge id target-id)))))
    t))

(defun supervisor-timer--record-trigger (timer-id now reason
                                        &optional target-id)
  "Record trigger event for TIMER-ID at time NOW with REASON.
Optional TARGET-ID is included in the event data."
  (let ((state (or (gethash timer-id supervisor--timer-state) nil)))
    (setq state (plist-put state :last-run-at now))
    (puthash timer-id state supervisor--timer-state))
  (supervisor-timer--save-state)
  (supervisor--emit-event 'timer-trigger timer-id nil
                          (list :target target-id :reason reason)))

(defun supervisor-timer--record-result (timer-id result result-reason)
  "Record RESULT and RESULT-REASON for TIMER-ID.
RESULT is success, failure, or skip.
RESULT-REASON is a symbol like already-active, overlap, etc."
  (let ((state (or (gethash timer-id supervisor--timer-state) nil))
        (now (float-time)))
    (setq state (plist-put state :last-result result))
    (setq state (plist-put state :last-result-reason result-reason))
    (when (eq result 'success)
      (setq state (plist-put state :last-success-at now)))
    ;; Clear retry state on success or skip (not failure).
    ;; Skip outcomes (disabled, overlap, target-converging) must not
    ;; preserve stale retry-next-at from a prior failure.
    (when (memq result '(success skip))
      (setq state (plist-put state :retry-attempt 0))
      (setq state (plist-put state :retry-next-at nil)))
    (puthash timer-id state supervisor--timer-state)
    (supervisor-timer--save-state)
    (supervisor-timer--update-next-run timer-id)))

(defun supervisor-timer--on-simple-complete (timer-id target-id success)
  "Handle completion of simple service trigger for TIMER-ID.
TARGET-ID is the simple service.  SUCCESS is t if spawned."
  (if success
      (progn
        (supervisor-timer--record-result timer-id 'success nil)
        (supervisor--emit-event 'timer-success timer-id nil
                                (list :target target-id)))
    (supervisor-timer--record-result timer-id 'failure 'spawn-failed)
    (let ((state (gethash timer-id supervisor--timer-state)))
      (setq state (plist-put state :last-failure-at (float-time)))
      (setq state (plist-put state :last-exit -1))
      ;; Schedule retry for spawn failures (uniform retry policy)
      (when-let* ((updated (supervisor-timer--schedule-retry timer-id state)))
        (setq state updated))
      (puthash timer-id state supervisor--timer-state)
      (supervisor-timer--save-state)
      (supervisor--emit-event 'timer-failure timer-id nil
                              (list :target target-id))))
  (supervisor--timer-scheduler-tick))

(defun supervisor-timer--on-target-converge (timer-id target-id)
  "Handle convergence check after target activation for TIMER-ID.
TARGET-ID is the target unit.
Only `reached' is classified as success.  `degraded' and nil/unknown
convergence states are classified as failure."
  (let ((conv (when (hash-table-p supervisor--target-convergence)
                (gethash target-id supervisor--target-convergence))))
    (pcase conv
      ('reached
       (supervisor-timer--record-result timer-id 'success 'target-reached)
       (supervisor--emit-event 'timer-success timer-id nil
                               (list :target target-id)))
      ('degraded
       (supervisor-timer--record-result timer-id 'failure 'target-degraded)
       (let ((state (gethash timer-id supervisor--timer-state)))
         (setq state (plist-put state :last-failure-at (float-time)))
         ;; Schedule retry for degraded targets (uniform retry policy)
         (when-let* ((updated (supervisor-timer--schedule-retry timer-id state)))
           (setq state updated))
         (puthash timer-id state supervisor--timer-state)
         (supervisor-timer--save-state))
       (supervisor--emit-event 'timer-failure timer-id nil
                               (list :target target-id :reason 'degraded)))
      ('converging
       ;; DAG completed but target still converging -- classify as failure
       (supervisor-timer--record-result timer-id 'failure 'target-not-converged)
       (let ((state (gethash timer-id supervisor--timer-state)))
         (setq state (plist-put state :last-failure-at (float-time)))
         (when-let* ((updated (supervisor-timer--schedule-retry timer-id state)))
           (setq state updated))
         (puthash timer-id state supervisor--timer-state)
         (supervisor-timer--save-state))
       (supervisor--emit-event 'timer-failure timer-id nil
                               (list :target target-id :reason 'not-converged)))
      (_
       ;; nil or unknown -- convergence never initialized, treat as failure
       (supervisor-timer--record-result timer-id 'failure 'convergence-unknown)
       (let ((state (gethash timer-id supervisor--timer-state)))
         (setq state (plist-put state :last-failure-at (float-time)))
         (puthash timer-id state supervisor--timer-state)
         (supervisor-timer--save-state))
       (supervisor--emit-event 'timer-failure timer-id nil
                               (list :target target-id
                                     :reason 'convergence-unknown)))))
  (supervisor--timer-scheduler-tick))

(defun supervisor-timer--failure-retryable-p (exit-code)
  "Return non-nil if EXIT-CODE represents a retryable failure.
Positive exit codes (normal process exits with non-zero status) are retryable.
Signal deaths are stored as negative values and are not retryable."
  (and exit-code
       (integerp exit-code)
       (> exit-code 0)))

(defun supervisor-timer--schedule-retry (timer-id state)
  "Schedule a retry for TIMER-ID based on current STATE.
Returns updated state with :retry-attempt and :retry-next-at set,
or nil if no retry should be scheduled."
  (when supervisor-timer-retry-intervals
    (let* ((attempt (or (plist-get state :retry-attempt) 0))
           (max-attempts (length supervisor-timer-retry-intervals)))
      (when (< attempt max-attempts)
        (let ((delay (nth attempt supervisor-timer-retry-intervals))
              (now (float-time)))
          (setq state (plist-put state :retry-attempt (1+ attempt)))
          (setq state (plist-put state :retry-next-at (+ now delay)))
          (supervisor--log 'info "timer %s: retry %d/%d in %ds"
                           timer-id (1+ attempt) max-attempts delay)
          state)))))

(defun supervisor-timer--on-target-complete (timer-id target-id success)
  "Handle completion of TIMER-ID's oneshot target TARGET-ID.
SUCCESS is t if completed successfully, nil otherwise."
  (let* ((now (float-time))
         (state (or (gethash timer-id supervisor--timer-state) nil))
         (exit-code (gethash target-id supervisor--oneshot-completed)))
    ;; Update state based on result
    (if (and success (or (null exit-code) (= exit-code 0)))
        (progn
          (setq state (plist-put state :last-success-at now))
          (setq state (plist-put state :last-exit 0))
          (setq state (plist-put state :last-result 'success))
          (setq state (plist-put state :last-result-reason nil))
          (setq state (plist-put state :retry-attempt 0))
          (setq state (plist-put state :retry-next-at nil))
          (supervisor--emit-event 'timer-success timer-id nil
                                  (list :target target-id)))
      ;; Failure - check if retryable
      (setq state (plist-put state :last-failure-at now))
      (setq state (plist-put state :last-exit (or exit-code -1)))
      (setq state (plist-put state :last-result 'failure))
      (setq state (plist-put state :last-result-reason nil))
      (supervisor--emit-event 'timer-failure timer-id nil
                              (list :target target-id :exit exit-code))
      ;; Schedule retry if eligible
      (when (supervisor-timer--failure-retryable-p exit-code)
        (when-let* ((updated (supervisor-timer--schedule-retry timer-id state)))
          (setq state updated))))
    (puthash timer-id state supervisor--timer-state)
    ;; Persist state
    (supervisor-timer--save-state)
    ;; Update next run time
    (supervisor-timer--update-next-run timer-id)
    ;; Reschedule the scheduler tick
    (supervisor--timer-scheduler-tick)))

(defun supervisor-timer--record-miss (timer-id reason)
  "Record a missed run for TIMER-ID with REASON symbol."
  (let ((state (or (gethash timer-id supervisor--timer-state) nil)))
    (setq state (plist-put state :last-missed-at (float-time)))
    (setq state (plist-put state :last-miss-reason reason))
    (puthash timer-id state supervisor--timer-state)))

;;; Scheduler Loop

(cl-defun supervisor--timer-scheduler-tick ()
  "Check for due timers and trigger them.
Reschedules itself for the next due time.
Does nothing if timer subsystem is not active."
  ;; Cancel any existing scheduler
  (when (timerp supervisor--timer-scheduler)
    (cancel-timer supervisor--timer-scheduler)
    (setq supervisor--timer-scheduler nil))
  ;; Gate check
  (unless (supervisor-timer-subsystem-active-p)
    (cl-return-from supervisor--timer-scheduler-tick nil))
  ;; Don't run if shutting down
  (when supervisor--shutting-down
    (cl-return-from supervisor--timer-scheduler-tick nil))
  (let ((now (float-time))
        (next-check nil))
    ;; Check each timer
    (dolist (timer supervisor--timer-list)
      (when (supervisor-timer-enabled timer)
        (let* ((id (supervisor-timer-id timer))
               (state (gethash id supervisor--timer-state))
               (next-run (plist-get state :next-run-at))
               (retry-at (plist-get state :retry-next-at)))
          (cond
           ;; Retry is due (takes priority over regular schedule)
           ((and retry-at (<= retry-at now))
            (supervisor-timer--trigger timer 'retry)
            ;; Clear retry on trigger
            (setq state (gethash id supervisor--timer-state))
            (setq state (plist-put state :retry-next-at nil))
            (puthash id state supervisor--timer-state)
            ;; Get next time to check
            (setq next-run (plist-get state :next-run-at))
            (when next-run
              (setq next-check (if next-check (min next-check next-run) next-run))))
           ;; Timer is due
           ((and next-run (<= next-run now))
            ;; Reset retry budget for fresh scheduled execution
            (setq state (plist-put state :retry-attempt 0))
            (setq state (plist-put state :retry-next-at nil))
            (puthash id state supervisor--timer-state)
            (supervisor-timer--trigger timer 'scheduled)
            ;; Update next run time
            (supervisor-timer--update-next-run id)
            ;; Get updated next-run for scheduling
            (setq state (gethash id supervisor--timer-state))
            (setq next-run (plist-get state :next-run-at))
            (when next-run
              (setq next-check (if next-check (min next-check next-run) next-run))))
           ;; Not due yet - track next check time
           (t
            (when retry-at
              (setq next-check (if next-check (min next-check retry-at) retry-at)))
            (when next-run
              (setq next-check (if next-check (min next-check next-run) next-run))))))))
    ;; Schedule next tick
    (when next-check
      (let ((delay (max 1 (- next-check now)))) ; At least 1 second
        (setq supervisor--timer-scheduler
              (run-at-time delay nil #'supervisor--timer-scheduler-tick))))))

(defun supervisor-timer--needs-catch-up-p (timer)
  "Return non-nil if TIMER needs a catch-up run after downtime.
Checks if timer is persistent and missed a run within catch-up limit."
  (when (and (supervisor-timer-persistent timer)
             (> supervisor-timer-catch-up-limit 0))
    (let* ((id (supervisor-timer-id timer))
           (state (gethash id supervisor--timer-state))
           (last-run (plist-get state :last-run-at))
           (now (float-time))
           (cutoff (- now supervisor-timer-catch-up-limit)))
      (when last-run
        (let ((would-have-run (supervisor-timer--compute-next-run timer (1+ last-run))))
          (and would-have-run
               (< would-have-run now)
               (>= would-have-run cutoff)))))))

(defun supervisor-timer--process-catch-ups ()
  "Process catch-up triggers for persistent timers after downtime.
Called during scheduler start."
  (let ((catch-up-count 0))
    (dolist (timer supervisor--timer-list)
      (when (and (supervisor-timer-enabled timer)
                 (supervisor-timer--needs-catch-up-p timer))
        (let ((id (supervisor-timer-id timer)))
          (supervisor--log 'info "timer %s: triggering catch-up run" id)
          (supervisor-timer--trigger timer 'catch-up)
          (cl-incf catch-up-count))))
    (when (> catch-up-count 0)
      (supervisor--log 'info "processed %d catch-up runs" catch-up-count))))

;;; Public API (called from supervisor-core)

(cl-defun supervisor-timer-scheduler-start ()
  "Start the timer scheduler.
Call after supervisor-start has completed stage startup.
Does nothing if timer subsystem is not active."
  (unless (supervisor-timer-subsystem-active-p)
    (supervisor--log 'info "timer subsystem disabled, skipping scheduler start")
    (cl-return-from supervisor-timer-scheduler-start nil))
  (setq supervisor--scheduler-startup-time (float-time))
  ;; Build timer list from config
  (let ((plan (supervisor--build-plan (supervisor--effective-programs))))
    (setq supervisor--timer-list (supervisor-timer-build-list plan)))
  ;; Load persisted state
  (supervisor-timer--load-state)
  ;; Initialize state for each timer
  (let ((active-ids (make-hash-table :test 'equal)))
    (dolist (timer supervisor--timer-list)
      (let ((id (supervisor-timer-id timer)))
        (puthash id t active-ids)
        ;; Preserve existing state or create new
        (unless (gethash id supervisor--timer-state)
          (puthash id nil supervisor--timer-state))
        ;; Compute initial next-run
        (supervisor-timer--update-next-run id)))
    ;; Prune stale timer IDs not in current config
    (let ((stale-ids nil))
      (maphash (lambda (id _state)
                 (unless (gethash id active-ids)
                   (push id stale-ids)))
               supervisor--timer-state)
      (dolist (id stale-ids)
        (remhash id supervisor--timer-state))
      (when stale-ids
        (supervisor--log 'info "pruned %d stale timer IDs from state"
                         (length stale-ids)))))
  ;; Process catch-up runs for persistent timers
  (supervisor-timer--process-catch-ups)
  ;; Start the scheduler tick
  (supervisor--timer-scheduler-tick)
  (supervisor--log 'info "timer scheduler started with %d timers"
                   (length supervisor--timer-list)))

(defun supervisor-timer-scheduler-stop ()
  "Stop the timer scheduler.
Saves timer state to disk before stopping.
Safe to call even if timer subsystem is not active."
  (when (timerp supervisor--timer-scheduler)
    (cancel-timer supervisor--timer-scheduler)
    (setq supervisor--timer-scheduler nil))
  (when (supervisor-timer-subsystem-active-p)
    (supervisor-timer--save-state)
    (supervisor--log 'info "timer scheduler stopped")))

(defun supervisor-timer-clear-state ()
  "Clear all timer runtime state.
Called during supervisor-start to reset state."
  (clrhash supervisor--timer-state)
  (clrhash supervisor--invalid-timers)
  (setq supervisor--timer-list nil)
  (setq supervisor--scheduler-startup-time nil)
  (setq supervisor--timer-state-loaded nil))

;;; Accessors for Dashboard/CLI

(defun supervisor-timer-get-list ()
  "Return the current list of validated timer structs."
  supervisor--timer-list)

(defun supervisor-timer-get-state (timer-id)
  "Return the runtime state plist for TIMER-ID."
  (gethash timer-id supervisor--timer-state))

(defun supervisor-timer-get-invalid ()
  "Return hash table of invalid timer IDs to reason strings."
  supervisor--invalid-timers)

(provide 'supervisor-timer)

;;; supervisor-timer.el ends here
