;;; supervisor-core.el --- Supervisor core engine -*- lexical-binding: t -*-

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

;; Core engine for supervisor.el.
;; Run M-x supervisor-handbook for full documentation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;; Timer Subsystem Gate (Experimental)
;;
;; The timer subsystem is gated behind this mode variable.
;; The full mode definition is in supervisor-timer.el.

(defvar supervisor-timer-subsystem-mode nil
  "Non-nil when the experimental timer subsystem is enabled.
Use `supervisor-timer-subsystem-mode' command to toggle.
See supervisor-timer.el for the full mode definition.")

;; Forward declarations - implementations in supervisor-timer.el
;; These are optional; core guards calls with fboundp for standalone operation.
(declare-function supervisor-timer-subsystem-active-p "supervisor-timer" ())
(declare-function supervisor-timer-build-list "supervisor-timer" (plan))
(declare-function supervisor-timer-scheduler-start "supervisor-timer" ())
(declare-function supervisor-timer-scheduler-stop "supervisor-timer" ())
(declare-function supervisor-timer-id "supervisor-timer" (timer))
(declare-function supervisor-timer-enabled "supervisor-timer" (timer))
(declare-function supervisor-timer-target "supervisor-timer" (timer))

;; Forward declarations for unit-file module
(declare-function supervisor--load-programs "supervisor-units" ())
(defvar supervisor-unit-directory)

;; Forward declarations for optional features
(declare-function file-notify-add-watch "filenotify" (file flags callback))
(declare-function file-notify-rm-watch "filenotify" (descriptor))

;; Forward declaration for dashboard refresh (optional cross-module call)
(declare-function supervisor--refresh-dashboard "supervisor-dashboard" ())

(defun supervisor--maybe-refresh-dashboard ()
  "Refresh dashboard if the dashboard module is loaded.
This allows supervisor-core to work standalone without dashboard."
  (when (fboundp 'supervisor--refresh-dashboard)
    (supervisor--refresh-dashboard)))

(defgroup supervisor nil
  "Emacs Lisp process supervisor."
  :group 'processes)

(defcustom supervisor-timers nil
  "List of timer definitions for scheduled oneshot execution.
Each entry is a plist with required keys :id and :target.

Required keys:
  :id       - string, unique timer identifier
  :target   - string, ID of oneshot service (defined as a unit file)

Trigger keys (at least one required):
  :on-calendar      - calendar schedule (see below)
  :on-startup-sec   - seconds after supervisor-start to run
  :on-unit-active-sec - seconds after target completes to run again

Optional keys:
  :enabled    - boolean, default t
  :persistent - boolean, default t (enables catch-up after downtime)

Calendar schedule format:
  (:minute M :hour H :day-of-month D :month MO :day-of-week DOW)
  Each field is an integer, list of integers, or \\='* for any.

Example:
  \\='((:id \"backup-daily\"
     :target \"backup-script\"
     :on-calendar (:hour 3 :minute 0))
    (:id \"cleanup-startup\"
     :target \"cleanup-script\"
     :on-startup-sec 60))"
  :type '(repeat plist)
  :group 'supervisor)

(defcustom supervisor-log-directory
  (expand-file-name "supervisor" user-emacs-directory)
  "Directory for supervisor log files."
  :type 'directory
  :group 'supervisor)

(defcustom supervisor-restart-delay 2
  "Seconds to wait before restarting a crashed process."
  :type 'integer
  :group 'supervisor)

(defcustom supervisor-max-restarts 3
  "Max restarts in `supervisor-restart-window' before marking failed."
  :type 'integer
  :group 'supervisor)

(defcustom supervisor-restart-window 60
  "Time window in seconds for counting restarts."
  :type 'integer
  :group 'supervisor)

(defcustom supervisor-shutdown-timeout 3
  "Seconds to wait for processes to exit gracefully before SIGKILL."
  :type 'integer
  :group 'supervisor)

(defcustom supervisor-oneshot-default-wait t
  "Default wait behavior for oneshot processes.
When t, oneshots block stage completion (wait for exit).
When nil, oneshots are async (fire-and-forget, don't block stage)."
  :type 'boolean
  :group 'supervisor)

(defcustom supervisor-oneshot-timeout 30
  "Default timeout in seconds for blocking oneshots.
If a blocking oneshot takes longer, warn and continue.
Set to nil for infinite wait."
  :type '(choice integer (const nil))
  :group 'supervisor)

(defcustom supervisor-stage-timeout nil
  "Timeout in seconds for each stage.
If a stage takes longer than this, log a warning and proceed to the next stage.
Set to nil (default) for no timeout."
  :type '(choice integer (const nil))
  :group 'supervisor)

(defcustom supervisor-max-concurrent-starts nil
  "Maximum number of processes to start concurrently within a stage.
When set, limits parallel process spawning to avoid thundering herd.
Set to nil (default) for unlimited concurrent starts."
  :type '(choice integer (const nil))
  :group 'supervisor)

(defcustom supervisor-verbose nil
  "When non-nil, log all events including informational messages.
When nil, only warnings and errors are logged.
Verbose events include stage start/completion, process start,
dependency unlocks, and restarts."
  :type 'boolean
  :group 'supervisor)

(defcustom supervisor-log-to-file nil
  "When non-nil, write supervisor events to a log file.
The log file is `supervisor.log' in `supervisor-log-directory'.
This is independent of `supervisor-verbose' - all events are logged
to file when enabled, regardless of verbose setting."
  :type 'boolean
  :group 'supervisor)

(defcustom supervisor-watch-config nil
  "When non-nil, watch the Emacs init file for modification.
When the init file is modified, automatically run `supervisor--reconcile'.
The file watched is the value of `user-init-file'.
Set to a file path string to watch a specific file instead."
  :type '(choice (const :tag "Disabled" nil)
                 (const :tag "Watch init file" t)
                 (file :tag "Watch specific file"))
  :group 'supervisor)

(defvar supervisor--config-watch-descriptor nil
  "File notification descriptor for config watching.")

(defvar supervisor--current-plan nil
  "The last plan built by `supervisor-daemon-reload'.
When non-nil, holds a `supervisor-plan' struct representing the most
recently loaded configuration.  Set by `supervisor-daemon-reload' and
cleared by `supervisor-start' (which builds its own fresh plan).")

;;; Program Loading

(defun supervisor--effective-programs ()
  "Return the program list by loading unit files from disk.
Always reads unit files via `supervisor--load-programs' so that
newly added or modified files are visible immediately.  Returns
nil when the units module is not loaded (standalone core mode)."
  (when (fboundp 'supervisor--load-programs)
    (supervisor--load-programs)))

(defvar supervisor--invalid)

(defun supervisor--merge-unit-file-invalid ()
  "Merge invalid unit-file entries into `supervisor--invalid'.
Copies entries from `supervisor--unit-file-invalid' into the runtime
invalid hash so they appear in the dashboard and CLI."
  (when (boundp 'supervisor--unit-file-invalid)
    (maphash (lambda (k v) (puthash k v supervisor--invalid))
             (symbol-value 'supervisor--unit-file-invalid))))

;;; Logging

(defun supervisor--supervisor-log-file ()
  "Return path to the supervisor-level log file."
  (expand-file-name "supervisor.log" supervisor-log-directory))

(defun supervisor--log (level format-string &rest args)
  "Log a message at LEVEL with FORMAT-STRING and ARGS.
LEVEL is one of:
  `error'   - always shown
  `warning' - always shown (prefixed with WARNING)
  `info'    - only shown when `supervisor-verbose' is non-nil
When `supervisor-log-to-file' is non-nil, all events are also
written to the supervisor log file."
  (let* ((prefix (pcase level
                   ('warning "WARNING - ")
                   ('error "ERROR - ")
                   (_ "")))
         (msg (format "Supervisor: %s%s" prefix (apply #'format format-string args))))
    ;; Write to log file if enabled (all levels)
    (when supervisor-log-to-file
      (supervisor--ensure-log-directory)
      (let ((timestamp (format-time-string "[%Y-%m-%d %H:%M:%S] ")))
        (write-region (concat timestamp msg "\n") nil
                      (supervisor--supervisor-log-file) t 'silent)))
    ;; Display message based on level and verbose setting
    (when (or supervisor-verbose
              (memq level '(error warning)))
      (message "%s" msg))))

;;; Stages

(defconst supervisor-stages
  '((stage1 . 0) (stage2 . 1) (stage3 . 2) (stage4 . 3))
  "Stage name to priority mapping.  Lower numbers run first.")

(defconst supervisor-stage-names
  '(stage1 stage2 stage3 stage4)
  "Ordered list of stage names.")

(defun supervisor--stage-to-int (stage)
  "Convert STAGE symbol to integer priority.  Default to stage3 (2)."
  (cond
   ((not (symbolp stage))
    (supervisor--log 'warning ":stage must be a symbol, got %S" stage)
    2)
   ((not (assq stage supervisor-stages))
    (supervisor--log 'warning "unknown :stage '%s', using stage3" stage)
    2)
   (t (alist-get stage supervisor-stages))))

(defun supervisor--int-to-stage (n)
  "Convert integer N to stage symbol."
  (car (rassq n supervisor-stages)))

(defun supervisor--normalize-after (after)
  "Normalize AFTER to a list of ID strings."
  (cond ((null after) nil)
        ((stringp after) (list after))
        ((listp after) after)
        (t nil)))

(defun supervisor--oneshot-wait-p (plist)
  "Return non-nil if oneshot should block stage completion.
Check PLIST for :oneshot-wait, :async, and fall back to default."
  (cond
   ((plist-member plist :oneshot-wait)
    (plist-get plist :oneshot-wait))
   ((plist-member plist :async)
    (not (plist-get plist :async)))
   (t supervisor-oneshot-default-wait)))

(defun supervisor--oneshot-timeout-value (plist)
  "Return timeout in seconds for a oneshot, or nil for infinite.
Check PLIST for :oneshot-timeout and fall back to default."
  (if (plist-member plist :oneshot-timeout)
      (let ((val (plist-get plist :oneshot-timeout)))
        (if (and val (not (numberp val)))
            (progn
              (supervisor--log 'warning ":oneshot-timeout must be number or nil, using default")
              supervisor-oneshot-timeout)
          val))
    supervisor-oneshot-timeout))

;;; Entry Validation

(defconst supervisor--valid-keywords
  '(:id :type :stage :delay :after :requires :enabled :disabled
    :restart :no-restart :logging :oneshot-wait :async :oneshot-timeout :tags)
  "List of valid keywords for entry plists.")

(defconst supervisor--valid-types '(simple oneshot)
  "List of valid :type values.")

(defconst supervisor--simple-only-keywords '(:restart :no-restart)
  "Keywords valid only for :type simple.")

(defconst supervisor--oneshot-only-keywords '(:oneshot-wait :async :oneshot-timeout)
  "Keywords valid only for :type oneshot.")

(defun supervisor--validate-entry (entry)
  "Validate ENTRY configuration.
Return nil if valid, or a reason string if invalid."
  (cond
   ;; String entries are always valid (use all defaults)
   ((stringp entry) nil)
   ;; Must be a list with command string first
   ((not (and (listp entry) (stringp (car entry))))
    "entry must be a string or list starting with command string")
   (t
    (let* ((plist (cdr entry))
           (type (or (plist-get plist :type) 'simple))
           (errors nil))
      ;; Check for unknown keywords
      (let ((keys plist))
        (while keys
          (let ((key (car keys)))
            (unless (memq key supervisor--valid-keywords)
              (push (format "unknown keyword %s" key) errors)))
          (setq keys (cddr keys))))
      ;; Check :type is valid symbol
      (when (plist-member plist :type)
        (let ((type-val (plist-get plist :type)))
          (cond
           ((not (symbolp type-val))
            (push ":type must be a symbol, not a string" errors))
           ((not (memq type-val supervisor--valid-types))
            (push (format ":type must be one of %s" supervisor--valid-types) errors)))))
      ;; Check :stage is valid symbol
      (when (plist-member plist :stage)
        (let ((stage (plist-get plist :stage)))
          (cond
           ((not (symbolp stage))
            (push ":stage must be a symbol, not a string" errors))
           ((not (assq stage supervisor-stages))
            (push (format ":stage must be one of %s" supervisor-stage-names) errors)))))
      ;; Check :id is a string when provided
      (when (plist-member plist :id)
        (let ((id (plist-get plist :id)))
          (unless (stringp id)
            (push ":id must be a string" errors))))
      ;; Check :delay is a non-negative number
      (when (plist-member plist :delay)
        (let ((delay (plist-get plist :delay)))
          (unless (and (numberp delay) (>= delay 0))
            (push ":delay must be a non-negative number" errors))))
      ;; Check :oneshot-timeout is number or nil
      (when (plist-member plist :oneshot-timeout)
        (let ((timeout (plist-get plist :oneshot-timeout)))
          (unless (or (null timeout) (numberp timeout))
            (push ":oneshot-timeout must be a number or nil" errors))))
      ;; Mutually exclusive: :enabled and :disabled
      (when (and (plist-member plist :enabled)
                 (plist-member plist :disabled))
        (push ":enabled and :disabled are mutually exclusive" errors))
      ;; Mutually exclusive: :restart and :no-restart
      (when (and (plist-member plist :restart)
                 (plist-member plist :no-restart))
        (push ":restart and :no-restart are mutually exclusive" errors))
      ;; Mutually exclusive: :oneshot-wait and :async
      (when (and (plist-member plist :oneshot-wait)
                 (plist-member plist :async))
        (push ":oneshot-wait and :async are mutually exclusive" errors))
      ;; Type-specific keyword restrictions
      (when (eq type 'oneshot)
        (dolist (kw supervisor--simple-only-keywords)
          (when (plist-member plist kw)
            (push (format "%s is invalid for :type oneshot" kw) errors))))
      (when (eq type 'simple)
        (dolist (kw supervisor--oneshot-only-keywords)
          (when (plist-member plist kw)
            (push (format "%s is invalid for :type simple" kw) errors))))
      ;; Check :after value type (string or list of strings)
      ;; Use proper-list-p to handle dotted lists safely (Bug 3 fix)
      (when (plist-member plist :after)
        (let ((after (plist-get plist :after)))
          (unless (or (null after)
                      (stringp after)
                      (and (proper-list-p after)
                           (cl-every #'stringp after)))
            (push ":after must be a string or list of strings" errors))))
      ;; Check :requires value type (string or list of strings)
      (when (plist-member plist :requires)
        (let ((requires (plist-get plist :requires)))
          (unless (or (null requires)
                      (stringp requires)
                      (and (proper-list-p requires)
                           (cl-every #'stringp requires)))
            (push ":requires must be a string or list of strings" errors))))
      ;; Return nil if valid, or joined error string
      (when errors
        (mapconcat #'identity (nreverse errors) "; "))))))

;; Forward declarations for supervisor-validate and supervisor-dry-run
(defvar supervisor--invalid)
(defvar supervisor--invalid-timers)
(defvar supervisor--cycle-fallback-ids)
(defvar supervisor--computed-deps)

(defun supervisor--extract-id (entry idx)
  "Extract a stable ID from ENTRY at position IDX in the program list.
For valid entries, returns the configured or derived ID.
For malformed entries, returns \"malformed#IDX\" for consistency."
  (cond
   ;; String entry - use basename
   ((stringp entry)
    (file-name-nondirectory (car (split-string-and-unquote entry))))
   ;; List with string command - use :id or basename
   ((and (listp entry) (stringp (car entry)))
    (or (plist-get (cdr entry) :id)
        (file-name-nondirectory (car (split-string-and-unquote (car entry))))))
   ;; Malformed - use index-based ID
   (t (format "malformed#%d" idx))))

;;;###autoload
(defun supervisor-handbook ()
  "Open the supervisor.el handbook (README.org) in a read-only buffer."
  (interactive)
  (let* ((lib-file (locate-library "supervisor"))
         (dir (and lib-file (file-name-directory lib-file)))
         (readme (and dir (expand-file-name "README.org" dir))))
    (if (and readme (file-exists-p readme))
        (with-current-buffer (find-file-noselect readme)
          (read-only-mode 1)
          (switch-to-buffer (current-buffer)))
      (user-error "Cannot locate README.org in supervisor package directory"))))

;;;###autoload
(defun supervisor-validate ()
  "Validate all unit-file entries and `supervisor-timers'.
Display results in a temporary buffer and populate `supervisor--invalid'
and `supervisor--invalid-timers' so the dashboard reflects validation state."
  (interactive)
  (clrhash supervisor--invalid)
  (when (boundp 'supervisor--invalid-timers)
    (clrhash supervisor--invalid-timers))

  ;; Build plan to get entry validation results
  (let* ((plan (supervisor--build-plan (supervisor--effective-programs)))
         (entry-valid (length (supervisor-plan-entries plan)))
         (entry-results nil))
    ;; Populate supervisor--invalid from plan and unit-file invalids
    (maphash (lambda (k v) (puthash k v supervisor--invalid))
             (supervisor-plan-invalid plan))
    (supervisor--merge-unit-file-invalid)
    ;; Collect entry results (entry is a tuple where car is the id)
    (dolist (entry (supervisor-plan-entries plan))
      (push (format "OK      %s" (car entry)) entry-results))
    (maphash (lambda (id reason)
               (push (format "INVALID %s: %s" id reason) entry-results))
             supervisor--invalid)
    ;; Validate timers using plan (only if timer module is loaded)
    (let* ((entry-invalid (hash-table-count supervisor--invalid))
           (timers (when (fboundp 'supervisor-timer-build-list)
                     (supervisor-timer-build-list plan)))
           (timer-valid (length timers))
           (timer-invalid (if (boundp 'supervisor--invalid-timers)
                              (hash-table-count supervisor--invalid-timers)
                            0))
           (timer-results nil))
      ;; Collect timer results (only if timer module loaded)
      (when (fboundp 'supervisor-timer-id)
        (dolist (timer timers)
          (push (format "OK      %s" (supervisor-timer-id timer)) timer-results)))
      (when (boundp 'supervisor--invalid-timers)
        (maphash (lambda (id reason)
                   (push (format "INVALID %s: %s" id reason) timer-results))
                 supervisor--invalid-timers))
      ;; Display results
      (with-output-to-temp-buffer "*supervisor-validate*"
        (princ (format "Services: %d valid, %d invalid\n" entry-valid entry-invalid))
        (dolist (line (nreverse entry-results))
          (princ (format "  %s\n" line)))
        (when (and supervisor-timers (fboundp 'supervisor-timer-build-list))
          (princ (format "\nTimers: %d valid, %d invalid\n" timer-valid timer-invalid))
          (dolist (line (nreverse timer-results))
            (princ (format "  %s\n" line)))))))
  ;; Refresh dashboard if open
  (supervisor--maybe-refresh-dashboard))

;;;###autoload
(defun supervisor-dry-run ()
  "Validate entries and show startup order without starting processes.
Display staged startup order, including dependency resolution and
cycle fallback behavior.  Uses the pure plan builder internally."
  (interactive)

  ;; Build plan using pure function
  (let ((plan (supervisor--build-plan (supervisor--effective-programs))))
    ;; Populate legacy globals for dashboard compatibility
    (clrhash supervisor--invalid)
    (when (boundp 'supervisor--invalid-timers)
      (clrhash supervisor--invalid-timers))
    (clrhash supervisor--cycle-fallback-ids)
    (clrhash supervisor--computed-deps)
    (maphash (lambda (k v) (puthash k v supervisor--invalid))
             (supervisor-plan-invalid plan))
    (supervisor--merge-unit-file-invalid)
    (maphash (lambda (k v) (puthash k v supervisor--cycle-fallback-ids))
             (supervisor-plan-cycle-fallback-ids plan))
    (maphash (lambda (k v) (puthash k v supervisor--computed-deps))
             (supervisor-plan-deps plan))
    ;; Validate timers (only if timer module loaded)
    (let ((timers (when (fboundp 'supervisor-timer-build-list)
                    (supervisor-timer-build-list plan))))
      ;; Display from plan artifact
      (with-output-to-temp-buffer "*supervisor-dry-run*"
        (princ "=== Supervisor Dry Run ===\n\n")
        (princ (format "Services: %d valid, %d invalid\n"
                       (length (supervisor-plan-entries plan))
                       (hash-table-count supervisor--invalid)))
        (when (and supervisor-timers (fboundp 'supervisor-timer-build-list))
          (princ (format "Timers: %d valid, %d invalid\n"
                         (length timers)
                         (if (boundp 'supervisor--invalid-timers)
                             (hash-table-count supervisor--invalid-timers)
                           0))))
        (princ "\n")
        ;; Show invalid entries first (includes unit-file invalids)
        (when (> (hash-table-count supervisor--invalid) 0)
          (princ "--- Invalid Services (skipped) ---\n")
          (maphash (lambda (id reason)
                     (princ (format "  %s: %s\n" id reason)))
                   supervisor--invalid)
          (princ "\n"))
        ;; Show invalid timers (only if timer module loaded)
        (when (and (boundp 'supervisor--invalid-timers)
                   (> (hash-table-count supervisor--invalid-timers) 0))
          (princ "--- Invalid Timers (skipped) ---\n")
          (maphash (lambda (id reason)
                     (princ (format "  %s: %s\n" id reason)))
                   supervisor--invalid-timers)
          (princ "\n"))
        ;; Display each stage from the plan's by-stage data
        (dolist (stage-pair (supervisor-plan-by-stage plan))
          (let* ((stage-int (car stage-pair))
                 (stage-name (supervisor--int-to-stage stage-int))
                 (sorted-entries (cdr stage-pair)))
            (princ (format "--- Stage: %s (%d entries) ---\n"
                           stage-name (length sorted-entries)))
            (let ((order 1))
              (dolist (entry sorted-entries)
                (let* ((id (nth 0 entry))
                       (type (nth 6 entry))
                       (delay (nth 2 entry))
                       (enabled-p (nth 3 entry))
                       (deps (gethash id (supervisor-plan-deps plan)))
                       (cycle (gethash id (supervisor-plan-cycle-fallback-ids plan))))
                  (princ (format "  %d. %s [%s]%s%s%s\n"
                                 order id type
                                 (if (not enabled-p) " DISABLED" "")
                                 (if (> delay 0) (format " delay=%ds" delay) "")
                                 (if cycle " (CYCLE FALLBACK)"
                                   (if deps
                                       (format " after=%s"
                                               (mapconcat #'identity deps ","))
                                     ""))))
                  (cl-incf order))))
            (princ "\n")))
        ;; Show timer summary if any
        (when timers
          (princ "--- Timers ---\n")
          (dolist (timer timers)
            (princ (format "  %s -> %s%s\n"
                           (supervisor-timer-id timer)
                           (supervisor-timer-target timer)
                           (if (supervisor-timer-enabled timer) "" " DISABLED"))))
          (princ "\n"))
        (princ "=== End Dry Run ===\n")))))

;;; State Variables

(defvar supervisor--processes (make-hash-table :test 'equal)
  "Hash table mapping program names to their process objects.")

(defvar supervisor--restart-override (make-hash-table :test 'equal)
  "Hash table of restart overrides: nil=inherit config, `enabled', `disabled'.")

(defvar supervisor--enabled-override (make-hash-table :test 'equal)
  "Hash table of enabled overrides: nil=inherit config, `enabled', `disabled'.
Runtime overrides take effect on next start (manual or restart).")

(defvar supervisor--mask-override (make-hash-table :test 'equal)
  "Hash table of mask overrides: id to `masked' or nil.
Masked entries are always disabled regardless of enable overrides.")

(defvar supervisor--restart-times (make-hash-table :test 'equal)
  "Hash table mapping program names to list of recent restart timestamps.")

(defvar supervisor--failed (make-hash-table :test 'equal)
  "Hash table of program names that have crash-looped and are marked failed.")

(defvar supervisor--logging (make-hash-table :test 'equal)
  "Hash table tracking logging state per process (runtime override).")

(defvar supervisor--manually-stopped (make-hash-table :test 'equal)
  "Hash table tracking entries manually stopped via CLI or dashboard.
Entries in this table should not auto-restart until explicitly started again.
This is cleared when an entry is started, allowing normal restart behavior.")

(defvar supervisor--manually-started (make-hash-table :test 'equal)
  "Hash table tracking entries manually started via CLI or dashboard.
Used by reconcile to preserve disabled units that were explicitly started.
Per the systemctl model, `start' on a disabled unit runs it this session
without changing enabled state, and reconcile should not stop it.")

;;; Persistent Overrides (Schema v1)

(defcustom supervisor-overrides-file
  (expand-file-name "supervisor/overrides.eld"
                    (or (getenv "XDG_STATE_HOME")
                        (expand-file-name ".local/state" (getenv "HOME"))))
  "File path for persistent service overrides.
Overrides are stored in Emacs Lisp Data format (eld).
Set to nil to disable persistence (overrides only live in memory)."
  :type '(choice (file :tag "Path to overrides file")
                 (const :tag "Disable persistence" nil))
  :group 'supervisor)

(defconst supervisor-overrides-schema-version 1
  "Schema version for persistent overrides file.")

(defvar supervisor--overrides-loaded nil
  "Non-nil if overrides have been loaded from file this session.")

(defun supervisor--overrides-file-path ()
  "Return the overrides file path, or nil if persistence is disabled."
  supervisor-overrides-file)

(defun supervisor--ensure-overrides-dir ()
  "Ensure the directory for the overrides file exists."
  (when-let* ((path (supervisor--overrides-file-path)))
    (let ((dir (file-name-directory path)))
      (unless (file-directory-p dir)
        (make-directory dir t)))))

(defun supervisor--overrides-to-alist ()
  "Collect current in-memory overrides into an alist.
Returns nil if no overrides are set."
  (let ((overrides nil))
    (maphash (lambda (id val)
               (push (cons id (list :enabled val)) overrides))
             supervisor--enabled-override)
    (maphash (lambda (id val)
               (let ((existing (assoc id overrides)))
                 (if existing
                     (setcdr existing (plist-put (cdr existing) :restart val))
                   (push (cons id (list :restart val)) overrides))))
             supervisor--restart-override)
    (maphash (lambda (id val)
               (let ((existing (assoc id overrides)))
                 (if existing
                     (setcdr existing (plist-put (cdr existing) :logging val))
                   (push (cons id (list :logging val)) overrides))))
             supervisor--logging)
    (maphash (lambda (id val)
               (let ((existing (assoc id overrides)))
                 (if existing
                     (setcdr existing (plist-put (cdr existing) :mask val))
                   (push (cons id (list :mask val)) overrides))))
             supervisor--mask-override)
    overrides))

(defun supervisor--save-overrides ()
  "Save current overrides to file using atomic write.
Uses temp file + rename pattern for crash safety.
Returns t on success, nil on failure."
  (let ((path (supervisor--overrides-file-path)))
    (when path
      (supervisor--ensure-overrides-dir)
      (let* ((overrides (supervisor--overrides-to-alist))
             (data `((version . ,supervisor-overrides-schema-version)
                     (timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
                     (overrides . ,overrides)))
             (temp-file (concat path ".tmp"))
             (coding-system-for-write 'utf-8-unix))
        (condition-case err
            (progn
              (with-temp-file temp-file
                (insert ";; Supervisor overrides file - do not edit manually\n")
                (insert ";; Schema version: " (number-to-string supervisor-overrides-schema-version) "\n")
                (pp data (current-buffer)))
              (rename-file temp-file path t)
              t)
          (error
           (supervisor--log 'warning "Failed to save overrides: %s" (error-message-string err))
           (when (file-exists-p temp-file)
             (delete-file temp-file))
           nil))))))

(defun supervisor--load-overrides ()
  "Load overrides from file into memory.
Returns t on success, nil on failure or if file doesn't exist.
Handles corrupt files safely by logging a warning and continuing.
Clears existing in-memory overrides before loading to prevent stale state."
  (let ((path (supervisor--overrides-file-path)))
    (when (and path (file-exists-p path))
      (condition-case err
          (let* ((data (with-temp-buffer
                         (insert-file-contents path)
                         (read (current-buffer))))
                 (version (alist-get 'version data))
                 (overrides (alist-get 'overrides data)))
            ;; Check version compatibility
            (unless (and version (<= version supervisor-overrides-schema-version))
              (supervisor--log 'warning
                "Overrides file version %s is newer than supported %s"
                version supervisor-overrides-schema-version))
            ;; Clear existing overrides before loading to prevent stale state
            (clrhash supervisor--enabled-override)
            (clrhash supervisor--restart-override)
            (clrhash supervisor--logging)
            (clrhash supervisor--mask-override)
            ;; Load overrides into hashes
            (dolist (entry overrides)
              (let ((id (car entry))
                    (plist (cdr entry)))
                (when-let* ((enabled (plist-get plist :enabled)))
                  (puthash id enabled supervisor--enabled-override))
                (when-let* ((restart (plist-get plist :restart)))
                  (puthash id restart supervisor--restart-override))
                (when-let* ((logging (plist-get plist :logging)))
                  (puthash id logging supervisor--logging))
                (when-let* ((mask (plist-get plist :mask)))
                  (puthash id mask supervisor--mask-override))))
            (setq supervisor--overrides-loaded t)
            (supervisor--log 'info "Loaded %d overrides from %s"
                             (length overrides) path)
            t)
        (error
         (supervisor--log 'warning "Failed to load overrides from %s: %s"
                          path (error-message-string err))
         ;; Don't delete corrupted file - leave for manual inspection
         nil)))))

(defun supervisor--merge-override (id key value)
  "Set override KEY for ID to VALUE and save.
KEY is one of :enabled, :restart, :logging, or :mask.
VALUE is `enabled', `disabled', `masked', or nil (to clear)."
  (let ((hash (pcase key
                (:enabled supervisor--enabled-override)
                (:restart supervisor--restart-override)
                (:logging supervisor--logging)
                (:mask supervisor--mask-override))))
    (if (null value)
        (remhash id hash)
      (puthash id value hash)))
  (supervisor--save-overrides))

(defun supervisor--clear-all-overrides ()
  "Clear all overrides from memory and file."
  (clrhash supervisor--enabled-override)
  (clrhash supervisor--restart-override)
  (clrhash supervisor--logging)
  (clrhash supervisor--mask-override)
  (when-let* ((path (supervisor--overrides-file-path)))
    (when (file-exists-p path)
      (delete-file path)))
  (supervisor--log 'info "Cleared all persistent overrides"))

(defun supervisor-overrides-load ()
  "Interactively load overrides from the configured file.
This reloads overrides even if they were already loaded."
  (interactive)
  (setq supervisor--overrides-loaded nil)
  (if (supervisor--load-overrides)
      (progn
        (supervisor--maybe-refresh-dashboard)
        (message "Supervisor: Loaded overrides from %s"
                 (supervisor--overrides-file-path)))
    (message "Supervisor: No overrides to load or file not found")))

(defun supervisor-overrides-save ()
  "Interactively save current overrides to file."
  (interactive)
  (if (supervisor--save-overrides)
      (message "Supervisor: Saved overrides to %s"
               (supervisor--overrides-file-path))
    (message "Supervisor: Failed to save overrides")))

(defun supervisor-overrides-clear ()
  "Interactively clear all persistent overrides."
  (interactive)
  (when (yes-or-no-p "Clear all supervisor overrides? ")
    (supervisor--clear-all-overrides)
    (supervisor--maybe-refresh-dashboard)
    (message "Supervisor: Cleared all overrides")))

(defvar supervisor--oneshot-completed (make-hash-table :test 'equal)
  "Hash table tracking oneshot completion.  Value is exit code.")

(defvar supervisor--oneshot-callbacks (make-hash-table :test 'equal)
  "Hash table of ID -> (callback . timeout-timer) for oneshot wait.
Used for event-driven oneshot completion notification.")

(defvar supervisor--shutdown-callback nil
  "Callback to invoke when all processes have terminated during shutdown.")

(defvar supervisor--shutdown-remaining 0
  "Count of processes still alive during shutdown.")

(defvar supervisor--shutdown-timer nil
  "Timer for SIGKILL timeout during graceful shutdown.")

(defvar supervisor--shutdown-complete-flag nil
  "Non-nil means shutdown has completed.  For callers that need to poll.")

(defvar supervisor--timers nil
  "List of pending delayed-start timers.")

(defvar supervisor--restart-timers (make-hash-table :test 'equal)
  "Hash table mapping IDs to pending restart timers.")

(defvar supervisor--shutting-down nil
  "Non-nil when supervisor is shutting down (prevents restarts).")

(defvar supervisor--invalid (make-hash-table :test 'equal)
  "Hash table mapping invalid entry IDs to reason strings.")

(defvar supervisor--start-times (make-hash-table :test 'equal)
  "Hash table mapping entry IDs to start timestamps (float-time).")

(defvar supervisor--ready-times (make-hash-table :test 'equal)
  "Hash table mapping entry IDs to ready timestamps (float-time).")

(defvar supervisor--current-stage nil
  "Currently executing stage symbol, or nil if not running.")

(defvar supervisor--completed-stages nil
  "List of completed stage symbols.")

(defvar supervisor--cycle-fallback-ids (make-hash-table :test 'equal)
  "Hash table of entry IDs that had :after cleared due to cycle fallback.")

(defvar supervisor--computed-deps (make-hash-table :test 'equal)
  "Hash table of ID -> validated :after list (same-stage, existing deps only).")

(defvar supervisor--entry-state (make-hash-table :test 'equal)
  "Hash table of ID -> state symbol for detailed status.
States: waiting-on-deps, delayed, disabled, stage-not-started,
failed-to-spawn, stage-timeout, started.")

;;; Entry Lifecycle FSM (Phase 5)

(defconst supervisor--valid-states
  '(stage-not-started waiting-on-deps delayed disabled
    started failed-to-spawn stage-timeout)
  "Valid entry lifecycle states.
- `stage-not-started': initial state, stage hasn't started yet
- `waiting-on-deps': waiting for :after dependencies to complete
- `delayed': waiting for :delay timer to expire
- `disabled': entry is disabled (skipped)
- `started': process successfully spawned
- `failed-to-spawn': process failed to start
- `stage-timeout': stage timed out before entry could start")

(defconst supervisor--state-transitions
  '((nil . (stage-not-started waiting-on-deps disabled stage-timeout))
    (stage-not-started . (stage-not-started waiting-on-deps delayed disabled
                          started failed-to-spawn stage-timeout))
    (waiting-on-deps . (waiting-on-deps delayed disabled started
                        failed-to-spawn stage-timeout))
    (delayed . (delayed started failed-to-spawn stage-timeout disabled)))
  "Valid state transitions as alist of (from-state . allowed-to-states).
Self-transitions are allowed for idempotent operations.
States not listed as keys are terminal (no transitions out).
The nil key represents initial state assignment.")

(defun supervisor--transition-state (id new-state &optional force)
  "Transition entry ID to NEW-STATE.
Validates that NEW-STATE is a valid state and that the transition
from the current state is legal.  Signals an error for invalid
transitions unless FORCE is non-nil.

Returns t if transition succeeded, nil if forced through invalid."
  (unless (memq new-state supervisor--valid-states)
    (error "Invalid state %s for entry %s" new-state id))
  (let* ((current (gethash id supervisor--entry-state))
         (allowed (cdr (assq current supervisor--state-transitions))))
    (cond
     ;; Valid transition
     ((memq new-state allowed)
      (puthash id new-state supervisor--entry-state)
      t)
     ;; Invalid but forced
     (force
      (supervisor--log 'warning "Forced invalid transition %s -> %s for %s"
                       current new-state id)
      (puthash id new-state supervisor--entry-state)
      nil)
     ;; Invalid, not forced - signal error
     (t
      (error "Invalid transition %s -> %s for entry %s" current new-state id)))))

;;; Structured Event Dispatcher (Phase 6)

(defconst supervisor--event-types
  '(stage-start stage-complete process-started process-ready
    process-exit process-failed cleanup
    timer-trigger timer-overlap timer-success timer-failure)
  "Valid event types for the structured event system.
- `stage-start': stage begins processing
- `stage-complete': stage finished processing
- `process-started': process successfully spawned
- `process-ready': process became ready (simple=spawned, oneshot=exited)
- `process-exit': supervised process exited
- `process-failed': process failed to spawn
- `cleanup': cleanup phase beginning
- `timer-trigger': timer fired its target oneshot
- `timer-overlap': timer skipped due to target still running
- `timer-success': timer's target completed successfully
- `timer-failure': timer's target failed")

(defvar supervisor-event-hook nil
  "Hook run for all supervisor events.
Called with one argument: an event plist with the following keys:
  :type  - event type symbol (see `supervisor--event-types')
  :ts    - timestamp (float-time)
  :id    - entry ID string (for process events, nil for stage/global)
  :stage - stage name symbol (for stage events, nil otherwise)
  :data  - additional payload plist (event-type specific)")

(defun supervisor--emit-event (type &optional id stage data)
  "Emit a structured event of TYPE with optional ID, STAGE, and DATA.
TYPE must be a member of `supervisor--event-types'.
Runs `supervisor-event-hook' with the event plist."
  (unless (memq type supervisor--event-types)
    (error "Invalid event type: %s" type))
  (let ((event (list :type type
                     :ts (float-time)
                     :id id
                     :stage stage
                     :data data)))
    (run-hook-with-args 'supervisor-event-hook event)
    ;; Log stage transitions to *Messages*
    (pcase type
      ('stage-start
       (message "Supervisor: %s starting" stage))
      ('stage-complete
       (message "Supervisor: %s complete" stage)))))

;;; DAG Scheduler State (per-stage, reset between stages)

(defvar supervisor--dag-in-degree nil
  "Hash table of ID -> remaining in-degree count.")

(defvar supervisor--dag-dependents nil
  "Hash table of ID -> list of IDs that depend on it.")

(defvar supervisor--dag-entries nil
  "Hash table of ID -> parsed entry.")

(defvar supervisor--dag-blocking nil
  "Hash table of blocking oneshot IDs still pending.")

(defvar supervisor--dag-started nil
  "Hash table of IDs that have been started (or skipped).")

(defvar supervisor--dag-ready nil
  "Hash table of IDs that are ready (spawned/completed/skipped).")

(defvar supervisor--dag-timeout-timers nil
  "Hash table of ID -> timeout timer for oneshots.")

(defvar supervisor--dag-delay-timers nil
  "Hash table of ID -> delay timer for delayed entries.")

(defvar supervisor--dag-id-to-index nil
  "Hash table of ID -> original list index for stable ordering.")

(defvar supervisor--dag-stage-complete-callback nil
  "Callback to invoke when current stage is complete.")

(defvar supervisor--dag-stage-timeout-timer nil
  "Timer for current stage timeout.")

(defvar supervisor--dag-pending-starts nil
  "Queue of entry IDs waiting to start (for max-concurrent-starts).")

(defvar supervisor--dag-active-starts 0
  "Count of currently starting processes (for max-concurrent-starts).")

;;; Plan Artifact (Phase 2 Data-Driven Architecture)

(cl-defstruct (supervisor-plan (:constructor supervisor-plan--create))
  "Immutable execution plan for a supervisor session.
This struct represents the validated, deterministic plan computed from
configuration.  It is pure data with no side effects on global state.

Determinism note: All fields except `meta' are deterministic for
identical input.  The `meta' field contains a wall-clock timestamp,
so full struct equality requires excluding it."
  entries          ; list of parsed valid entries
  invalid          ; hash: id -> reason string
  by-stage         ; alist of (stage-int . sorted-entries)
  deps             ; hash: id -> validated :after deps (ordering only)
  requires-deps    ; hash: id -> validated :requires deps (pull-in + ordering)
  dependents       ; hash: id -> list of dependent ids (combined)
  cycle-fallback-ids ; hash: id -> t for entries with cleared edges
  order-index      ; hash: id -> original index for stable ordering
  meta)            ; plist with :version, :timestamp (non-deterministic)

(defconst supervisor-plan-version 1
  "Schema version for supervisor-plan struct.")

;;; Service Definition Schema (v1)

(defconst supervisor-service-schema-version 1
  "Schema version for service definitions.
Version history:
  1 - Initial versioned schema with explicit dependency model split.")

(cl-defstruct (supervisor-service (:constructor supervisor-service--create)
                                  (:copier nil))
  "Canonical service record for supervisor entries (schema v1).
This struct represents a fully parsed and validated service definition.
All internal functions should use this struct, not raw config entries
or legacy tuple indexing.

Field documentation:
  id             - Unique identifier string (required)
  command        - Shell command string to execute (required)
  type           - Process type: `simple' (daemon) or `oneshot' (run-once)
                   Default: `simple'
  stage          - Startup stage: `stage1', `stage2', `stage3', or `stage4'
                   Default: `stage3'
  delay          - Seconds to wait before starting (non-negative number)
                   Default: 0
  enabled        - Whether to start this service (boolean)
                   Default: t
  restart        - Whether to restart on crash, for simple type only (boolean)
                   Default: t
  logging        - Whether to log stdout/stderr to file (boolean)
                   Default: t
  after          - Ordering dependencies: list of service IDs (same stage only)
                   These control start ORDER but do not pull in services.
                   Default: nil
  requires       - Requirement dependencies: list of service IDs
                   These PULL IN services and also imply ordering.
                   Cross-stage requires cause an error (must use after).
                   Default: nil
  oneshot-wait   - For oneshots: block stage completion until exit (boolean)
                   Default: value of `supervisor-oneshot-default-wait'
  oneshot-timeout - For oneshots: timeout in seconds, or nil for infinite
                   Default: value of `supervisor-oneshot-timeout'
  tags           - List of symbols/strings for filtering
                   Default: nil"
  (id nil :type string :documentation "Unique identifier (required)")
  (command nil :type string :documentation "Shell command to execute (required)")
  (type 'simple :type symbol :documentation "Process type: simple or oneshot")
  (stage 'stage3 :type symbol :documentation "Startup stage")
  (delay 0 :type number :documentation "Delay before starting (seconds)")
  (enabled t :type boolean :documentation "Whether to start this service")
  (restart t :type boolean :documentation "Restart on crash (simple only)")
  (logging t :type boolean :documentation "Log stdout/stderr to file")
  (after nil :type list :documentation "Ordering dependencies (same stage)")
  (requires nil :type list :documentation "Requirement dependencies")
  (oneshot-wait nil :type boolean :documentation "Block stage for oneshot exit")
  (oneshot-timeout nil :type (or null number) :documentation "Oneshot timeout")
  (tags nil :type list :documentation "Filter tags"))

(defconst supervisor-service-required-fields '(id command)
  "List of required fields in a service record.")

(defconst supervisor-service-optional-fields
  '((type . simple)
    (stage . stage3)
    (delay . 0)
    (enabled . t)
    (restart . t)
    (logging . t)
    (after . nil)
    (requires . nil)
    (oneshot-wait . :defer)  ; resolved at runtime from global default
    (oneshot-timeout . :defer)
    (tags . nil))
  "Alist of optional fields with their default values.
A value of :defer means the default is resolved at runtime.")

;;; Parsed Entry Accessors (Schema v1)
;;
;; These functions abstract the internal tuple representation.
;; Use these instead of direct (nth N entry) indexing for maintainability.
;; The tuple format is:
;;   (id cmd delay enabled-p restart-p logging-p type stage after
;;    oneshot-wait oneshot-timeout tags requires)

(defsubst supervisor-entry-id (entry)
  "Return the ID of parsed ENTRY."
  (nth 0 entry))

(defsubst supervisor-entry-command (entry)
  "Return the command of parsed ENTRY."
  (nth 1 entry))

(defsubst supervisor-entry-delay (entry)
  "Return the delay in seconds of parsed ENTRY."
  (nth 2 entry))

(defsubst supervisor-entry-enabled-p (entry)
  "Return non-nil if parsed ENTRY is enabled."
  (nth 3 entry))

(defsubst supervisor-entry-restart-p (entry)
  "Return non-nil if parsed ENTRY should restart on crash."
  (nth 4 entry))

(defsubst supervisor-entry-logging-p (entry)
  "Return non-nil if parsed ENTRY should log to file."
  (nth 5 entry))

(defsubst supervisor-entry-type (entry)
  "Return the type (`simple' or `oneshot') of parsed ENTRY."
  (nth 6 entry))

(defsubst supervisor-entry-stage (entry)
  "Return the stage symbol of parsed ENTRY."
  (nth 7 entry))

(defsubst supervisor-entry-after (entry)
  "Return the ordering dependencies (after) of parsed ENTRY."
  (nth 8 entry))

(defsubst supervisor-entry-oneshot-wait (entry)
  "Return non-nil if oneshot ENTRY blocks stage completion."
  (nth 9 entry))

(defsubst supervisor-entry-oneshot-timeout (entry)
  "Return the timeout in seconds for oneshot ENTRY, or nil."
  (nth 10 entry))

(defsubst supervisor-entry-tags (entry)
  "Return the tags list of parsed ENTRY."
  (nth 11 entry))

(defsubst supervisor-entry-requires (entry)
  "Return the requirement dependencies of parsed ENTRY."
  (nth 12 entry))

;; Forward declarations for timer state variables (defined in supervisor-timer.el)
;; These are only accessed when timer module is loaded.
(defvar supervisor--timer-state)
(defvar supervisor--timer-scheduler)
(defvar supervisor--timer-list)
(defvar supervisor--scheduler-startup-time)
(defvar supervisor--timer-state-loaded)

(defun supervisor--get-entry-for-id (id)
  "Get the parsed entry for ID.
Return a list of entry properties or nil if not found.
Loads unit files from disk via `supervisor--effective-programs'.
Skips invalid/malformed entries to avoid parse errors."
  (let ((idx 0))
    (cl-loop for entry in (supervisor--effective-programs)
             for entry-id = (supervisor--extract-id entry idx)
             do (cl-incf idx)
             ;; Skip invalid entries - don't try to parse them
             unless (gethash entry-id supervisor--invalid)
             ;; Only parse valid entries
             when (string= entry-id id)
             return (supervisor--parse-entry entry))))

(defun supervisor--compute-entry-status (id type &optional snapshot)
  "Compute status string and PID string for entry ID of TYPE.
If SNAPSHOT is provided, read from it; otherwise read from globals.
Return a cons cell (STATUS . PID)."
  (let* ((mask-hash (if snapshot
                        (or (supervisor-snapshot-mask-override snapshot)
                            (make-hash-table :test 'equal))
                      supervisor--mask-override))
         (masked (eq (gethash id mask-hash) 'masked))
         (alive (if snapshot
                    (gethash id (supervisor-snapshot-process-alive snapshot))
                  (let ((proc (gethash id supervisor--processes)))
                    (and proc (process-live-p proc)))))
         (pid-num (if snapshot
                      (gethash id (supervisor-snapshot-process-pids snapshot))
                    (let ((proc (gethash id supervisor--processes)))
                      (and proc (process-live-p proc) (process-id proc)))))
         (failed (gethash id (if snapshot
                                 (supervisor-snapshot-failed snapshot)
                               supervisor--failed)))
         (oneshot-p (eq type 'oneshot))
         (oneshot-exit (gethash id (if snapshot
                                       (supervisor-snapshot-oneshot-exit snapshot)
                                     supervisor--oneshot-completed)))
         (oneshot-done (not (null oneshot-exit)))
         (oneshot-failed (and oneshot-done (/= oneshot-exit 0)))
         (pid (cond (alive (number-to-string pid-num))
                    ((and oneshot-p oneshot-done) (format "exit:%d" oneshot-exit))
                    (t "-")))
         (status (cond (masked "masked")
                       (alive "running")
                       (failed "dead")
                       ((and oneshot-p oneshot-failed) "failed")
                       ((and oneshot-p oneshot-done) "done")
                       (oneshot-p "pending")
                       (t "stopped"))))
    (cons status pid)))

(defun supervisor--compute-entry-reason (id type &optional snapshot)
  "Compute reason string for entry ID of TYPE.
Returns a reason string, or nil if no specific reason applies.
If SNAPSHOT is provided, read from it; otherwise read from globals."
  (let* ((mask-hash (if snapshot
                        (or (supervisor-snapshot-mask-override snapshot)
                            (make-hash-table :test 'equal))
                      supervisor--mask-override))
         (masked (eq (gethash id mask-hash) 'masked))
         (alive (if snapshot
                    (gethash id (supervisor-snapshot-process-alive snapshot))
                  (let ((proc (gethash id supervisor--processes)))
                    (and proc (process-live-p proc)))))
         (failed (gethash id (if snapshot
                                 (supervisor-snapshot-failed snapshot)
                               supervisor--failed)))
         (oneshot-p (eq type 'oneshot))
         (oneshot-exit (gethash id (if snapshot
                                       (supervisor-snapshot-oneshot-exit snapshot)
                                     supervisor--oneshot-completed)))
         (oneshot-done (not (null oneshot-exit)))
         (entry-state (gethash id (if snapshot
                                      (supervisor-snapshot-entry-state snapshot)
                                    supervisor--entry-state))))
    (cond
     (masked "masked")
     (alive nil)
     ((and oneshot-p oneshot-done) nil)
     ((eq entry-state 'disabled) "disabled")
     ((eq entry-state 'delayed) "delayed")
     ((eq entry-state 'waiting-on-deps) "waiting-on-deps")
     ((eq entry-state 'stage-not-started) "stage-not-started")
     ((eq entry-state 'failed-to-spawn) "failed-to-spawn")
     ((eq entry-state 'stage-timeout) "stage-timeout")
     (failed "crash-loop")
     (t nil))))

;;; Migration Layer (Schema v1)
;;
;; Functions to normalize program entries to canonical
;; schema v1 plist format.

(defun supervisor--migrate-entry-to-plist (entry)
  "Migrate raw ENTRY to a canonical schema v1 plist format.
Returns a plist in canonical program entry format.
This normalizes short-form entries to their explicit form."
  (let* ((parsed (supervisor--parse-entry entry))
         (id (supervisor-entry-id parsed))
         (cmd (supervisor-entry-command parsed))
         (type (supervisor-entry-type parsed))
         (stage (supervisor-entry-stage parsed))
         (delay (supervisor-entry-delay parsed))
         (enabled (supervisor-entry-enabled-p parsed))
         (restart (supervisor-entry-restart-p parsed))
         (logging (supervisor-entry-logging-p parsed))
         (after (supervisor-entry-after parsed))
         (requires (supervisor-entry-requires parsed))
         (oneshot-wait (supervisor-entry-oneshot-wait parsed))
         (oneshot-timeout (supervisor-entry-oneshot-timeout parsed))
         (tags (supervisor-entry-tags parsed))
         (plist nil))
    ;; Build plist from parsed values, only including non-default values
    (when tags
      (setq plist (plist-put plist :tags tags)))
    (when requires
      (setq plist (plist-put plist :requires requires)))
    (when after
      (setq plist (plist-put plist :after after)))
    (when (eq type 'oneshot)
      (when (not (eq oneshot-wait supervisor-oneshot-default-wait))
        (setq plist (plist-put plist :oneshot-wait oneshot-wait)))
      (when (not (eq oneshot-timeout supervisor-oneshot-timeout))
        (setq plist (plist-put plist :oneshot-timeout oneshot-timeout))))
    (when (not logging)
      (setq plist (plist-put plist :logging nil)))
    (when (and (eq type 'simple) (not restart))
      (setq plist (plist-put plist :no-restart t)))
    (when (not enabled)
      (setq plist (plist-put plist :disabled t)))
    (when (> delay 0)
      (setq plist (plist-put plist :delay delay)))
    (when (not (eq stage 'stage3))
      (setq plist (plist-put plist :stage stage)))
    (when (not (eq type 'simple))
      (setq plist (plist-put plist :type type)))
    ;; Only include :id if it differs from the derived ID
    (let ((derived-id (file-name-nondirectory
                       (car (split-string-and-unquote cmd)))))
      (when (not (equal id derived-id))
        (setq plist (plist-put plist :id id))))
    ;; Return as (cmd . plist) or just cmd if no options
    (if plist
        (cons cmd plist)
      cmd)))

(defun supervisor--migrate-all-entries ()
  "Migrate all loaded entries to schema v1 format.
Returns a list of migrated entries with a summary of changes.
Invalid and duplicate entries are skipped with a warning."
  (let ((seen (make-hash-table :test 'equal))
        (migrated nil)
        (skipped nil)
        (idx 0))
    (dolist (entry (supervisor--effective-programs))
      (let* ((id (supervisor--extract-id entry idx))
             (reason (supervisor--validate-entry entry)))
        (cond
         (reason
          (push (cons id reason) skipped))
         ((gethash id seen)
          (push (cons id "duplicate ID") skipped))
         (t
          (puthash id t seen)
          (push (supervisor--migrate-entry-to-plist entry) migrated)))
        (cl-incf idx)))
    (list :migrated (nreverse migrated)
          :skipped (nreverse skipped))))

(defun supervisor-migrate-config ()
  "Display loaded entries in unit-file plist format.
Shows the canonical form of all valid entries as unit-file plists,
one per service.  Save each to a `.el' file in
`supervisor-unit-directory' to complete the migration."
  (interactive)
  (let* ((result (supervisor--migrate-all-entries))
         (migrated (plist-get result :migrated))
         (skipped (plist-get result :skipped)))
    (with-output-to-temp-buffer "*supervisor-migrate*"
      (princ ";;; Supervisor Unit Files (Schema v1)\n")
      (princ ";; Generated by supervisor-migrate-config\n")
      (princ (format ";; Version: %d\n" supervisor-service-schema-version))
      (princ (format ";; Target directory: %s\n\n"
                     (abbreviate-file-name supervisor-unit-directory)))
      (when skipped
        (princ ";; Skipped entries (invalid or duplicate):\n")
        (dolist (entry skipped)
          (princ (format ";;   %s: %s\n" (car entry) (cdr entry))))
        (princ "\n"))
      (dolist (entry migrated)
        (let* ((cmd (if (stringp entry) entry (car entry)))
               (plist (if (stringp entry) nil (cdr entry)))
               (id (or (plist-get plist :id) cmd)))
          (princ (format ";; Save as: %s.el\n" id))
          (princ (format "(:id %S\n :command %S" id cmd))
          (let ((keys plist))
            (while keys
              (let ((key (car keys))
                    (val (cadr keys)))
                (unless (eq key :id)
                  (princ (format "\n %s %S" key val))))
              (setq keys (cddr keys))))
          (princ ")\n\n"))))))

(defun supervisor-migrate-entry-to-service (entry)
  "Convert raw config ENTRY directly to a `supervisor-service' struct.
This is the high-level migration function for programmatic use."
  (let* ((reason (supervisor--validate-entry entry)))
    (if reason
        (error "Invalid entry: %s" reason)
      (supervisor-entry-to-service (supervisor--parse-entry entry)))))

(cl-defstruct (supervisor-snapshot (:constructor supervisor-snapshot--create))
  "Runtime state snapshot for read-only operations.
Captures current state of all supervised processes for dashboard,
status queries, and reconciliation without direct global access."
  process-alive    ; hash: id -> t if process is alive
  process-pids     ; hash: id -> pid (integer) or nil
  failed           ; hash: id -> t if crash-looped
  oneshot-exit     ; hash: id -> exit code or nil
  entry-state      ; hash: id -> state symbol
  invalid          ; hash: id -> reason string
  enabled-override ; hash: id -> 'enabled, 'disabled, or nil
  restart-override ; hash: id -> 'enabled, 'disabled, or nil
  logging-override ; hash: id -> 'enabled, 'disabled, or nil
  mask-override    ; hash: id -> 'masked or nil
  manually-started ; hash: id -> t if manually started (for reconcile)
  timestamp)       ; float-time when snapshot was taken

(defun supervisor--build-snapshot ()
  "Build a snapshot of current runtime state.
Returns a `supervisor-snapshot' struct capturing process states,
completion status, and overrides.  Safe to call at any time."
  (let ((process-alive (make-hash-table :test 'equal))
        (process-pids (make-hash-table :test 'equal)))
    ;; Capture process liveness and PIDs
    (maphash (lambda (id proc)
               (when (process-live-p proc)
                 (puthash id t process-alive)
                 (puthash id (process-id proc) process-pids)))
             supervisor--processes)
    (supervisor-snapshot--create
     :process-alive process-alive
     :process-pids process-pids
     :failed (copy-hash-table supervisor--failed)
     :oneshot-exit (copy-hash-table supervisor--oneshot-completed)
     :entry-state (copy-hash-table supervisor--entry-state)
     :invalid (copy-hash-table supervisor--invalid)
     :enabled-override (copy-hash-table supervisor--enabled-override)
     :restart-override (copy-hash-table supervisor--restart-override)
     :logging-override (copy-hash-table supervisor--logging)
     :mask-override (copy-hash-table supervisor--mask-override)
     :manually-started (copy-hash-table supervisor--manually-started)
     :timestamp (float-time))))

(defun supervisor--build-plan (programs)
  "Build an immutable execution plan from PROGRAMS.
This function does not modify global runtime state, but does emit
warnings for invalid entries, duplicate IDs, and invalid :after refs.
Returns a `supervisor-plan' struct with all computed scheduling data.

The plan includes:
- Parsed and validated entries
- Invalid entries with reasons
- Entries grouped and sorted by stage
- Validated :after dependencies (ordering only, same-stage)
- Validated :requires dependencies (pull-in + ordering, same-stage)
- Reverse dependency index (combined)
- Cycle fallback tracking
- Stable ordering index"
  (let ((seen (make-hash-table :test 'equal))
        (invalid (make-hash-table :test 'equal))
        (order-index (make-hash-table :test 'equal))
        (deps (make-hash-table :test 'equal))
        (requires-deps (make-hash-table :test 'equal))
        (dependents (make-hash-table :test 'equal))
        (cycle-fallback-ids (make-hash-table :test 'equal))
        (valid-entries nil)
        (idx 0))
    ;; Phase 1: Parse and validate all entries, collecting valid ones
    (dolist (entry programs)
      (let* ((raw-id (supervisor--extract-id entry idx))
             (reason (supervisor--validate-entry entry))
             (parsed (unless reason (supervisor--parse-entry entry))))
        ;; Only record first-occurrence index (don't let duplicates overwrite)
        (unless (gethash raw-id order-index)
          (puthash raw-id idx order-index))
        (cond
         ;; Invalid entry - only record if not already seen (first valid wins)
         (reason
          (unless (gethash raw-id seen)
            (puthash raw-id reason invalid))
          (supervisor--log 'warning "INVALID %s - %s" raw-id reason))
         ;; Duplicate ID
         ((gethash raw-id seen)
          (supervisor--log 'warning "duplicate ID '%s', skipping" raw-id))
         ;; Valid entry - first valid wins, clear any stale invalid state
         (t
          (remhash raw-id invalid)  ; clear if earlier invalid had same ID
          (puthash raw-id t seen)
          (push parsed valid-entries)))
        (cl-incf idx)))
    (setq valid-entries (nreverse valid-entries))
    ;; Phase 2: Partition by stage
    (let ((by-stage-hash (make-hash-table))
          (all-ids (mapcar #'car valid-entries)))
      (dolist (entry valid-entries)
        (let* ((stage (nth 7 entry))
               (stage-int (supervisor--stage-to-int stage)))
          (puthash stage-int (cons entry (gethash stage-int by-stage-hash))
                   by-stage-hash)))
      ;; Phase 3: For each stage, validate deps and topo sort
      (let ((by-stage nil)
            (combined-deps (make-hash-table :test 'equal)))
        (dolist (stage-int '(0 1 2 3))
          (when-let* ((stage-entries (gethash stage-int by-stage-hash)))
            (setq stage-entries (nreverse stage-entries))
            (let* ((stage-ids (mapcar #'car stage-entries))
                   ;; Validate :after references (ordering only, same-stage)
                   (validated-entries
                    (mapcar
                     (lambda (entry)
                       (let* ((id (supervisor-entry-id entry))
                              (after (supervisor-entry-after entry))
                              (requires (supervisor-entry-requires entry))
                              ;; Validate :after - warn on cross-stage/missing
                              (valid-after
                               (cl-remove-if-not
                                (lambda (dep)
                                  (cond
                                   ((not (member dep stage-ids))
                                    (if (member dep all-ids)
                                        (supervisor--log 'warning
                                          ":after '%s' for %s is in different stage, ignoring"
                                          dep id)
                                      (supervisor--log 'warning
                                        ":after '%s' for %s does not exist, ignoring"
                                        dep id))
                                    nil)
                                   (t t)))
                                after))
                              ;; Validate :requires - error on cross-stage, warn on missing
                              (valid-requires
                               (cl-remove-if-not
                                (lambda (dep)
                                  (cond
                                   ((not (member dep all-ids))
                                    (supervisor--log 'warning
                                      ":requires '%s' for %s does not exist, ignoring"
                                      dep id)
                                    nil)
                                   ((not (member dep stage-ids))
                                    ;; Cross-stage :requires is an error
                                    (puthash id
                                             (format ":requires '%s' is in different stage (cross-stage requires not allowed)"
                                                     dep)
                                             invalid)
                                    (supervisor--log 'error
                                      ":requires '%s' for %s is in different stage (not allowed)"
                                      dep id)
                                    nil)
                                   (t t)))
                                requires)))
                         (puthash id valid-after deps)
                         (puthash id valid-requires requires-deps)
                         ;; Combined deps for topo sort (union of after + requires)
                         (puthash id (cl-union valid-after valid-requires :test #'equal)
                                  combined-deps)
                         ;; Return entry with validated deps
                         (let ((new-entry entry))
                           (unless (equal after valid-after)
                             (setq new-entry
                                   (append (cl-subseq new-entry 0 8)
                                           (list valid-after)
                                           (cl-subseq new-entry 9))))
                           (unless (equal requires valid-requires)
                             (setq new-entry
                                   (append (cl-subseq new-entry 0 12)
                                           (list valid-requires))))
                           new-entry)))
                     stage-entries))
                   ;; Filter out entries that were marked invalid during :requires validation
                   (validated-entries
                    (cl-remove-if (lambda (entry)
                                    (gethash (supervisor-entry-id entry) invalid))
                                  validated-entries))
                   ;; Build dependents graph (combined :after + :requires)
                   (_ (dolist (entry validated-entries)
                        (let* ((id (supervisor-entry-id entry))
                               (all-deps (gethash id combined-deps)))
                          (puthash id nil dependents)
                          (dolist (dep all-deps)
                            (puthash dep (cons id (gethash dep dependents))
                                     dependents)))))
                   ;; Topo sort using combined deps
                   (sorted-entries
                    (supervisor--build-plan-topo-sort
                     validated-entries combined-deps order-index cycle-fallback-ids))
                   ;; If cycle was detected, also clear deps and requires-deps
                   (_ (dolist (entry sorted-entries)
                        (let ((id (supervisor-entry-id entry)))
                          (when (gethash id cycle-fallback-ids)
                            (puthash id nil deps)
                            (puthash id nil requires-deps))))))
              ;; Only push non-empty stages (Bug 4 fix)
              (when sorted-entries
                (push (cons stage-int sorted-entries) by-stage)))))
        ;; Filter valid-entries to exclude entries invalidated in phase 2/3
        ;; (Bug 2 fix: cross-stage :requires entries should not be in plan.entries)
        (let ((final-entries (cl-remove-if
                              (lambda (entry)
                                (gethash (supervisor-entry-id entry) invalid))
                              valid-entries)))
          ;; Return the plan struct
          (supervisor-plan--create
           :entries final-entries
           :invalid invalid
           :by-stage (nreverse by-stage)
           :deps deps
           :requires-deps requires-deps
           :dependents dependents
           :cycle-fallback-ids cycle-fallback-ids
           :order-index order-index
           :meta (list :version supervisor-plan-version
                       :timestamp (float-time))))))))

(defun supervisor--build-plan-topo-sort (entries deps order-index cycle-fallback-ids)
  "Stable topological sort for plan building (pure helper).
ENTRIES is the list of entries to sort.
DEPS is hash of id -> validated deps.
ORDER-INDEX is hash of id -> original index.
CYCLE-FALLBACK-IDS is hash to record cycle fallbacks (mutated).
Returns sorted entries list."
  (let* ((ids (mapcar #'car entries))
         (id-to-entry (make-hash-table :test 'equal))
         (in-degree (make-hash-table :test 'equal))
         (local-dependents (make-hash-table :test 'equal))
         (result nil))
    ;; Build lookup tables
    (dolist (entry entries)
      (let ((id (car entry)))
        (puthash id entry id-to-entry)
        (puthash id 0 in-degree)
        (puthash id nil local-dependents)))
    ;; Compute in-degrees from deps
    (dolist (entry entries)
      (let ((id (car entry))
            (entry-deps (gethash (car entry) deps)))
        (dolist (dep entry-deps)
          (when (gethash dep id-to-entry)
            (cl-incf (gethash id in-degree 0))
            (puthash dep (cons id (gethash dep local-dependents))
                     local-dependents)))))
    ;; Kahn's algorithm with stable ordering
    (let ((ready (cl-remove-if-not
                  (lambda (id) (= 0 (gethash id in-degree)))
                  ids)))
      (setq ready (sort ready (lambda (a b)
                                (< (gethash a order-index 999)
                                   (gethash b order-index 999)))))
      (while ready
        (let* ((id (pop ready))
               (entry (gethash id id-to-entry)))
          (push entry result)
          (dolist (dep-id (gethash id local-dependents))
            (cl-decf (gethash dep-id in-degree))
            (when (= 0 (gethash dep-id in-degree))
              (setq ready (sort (cons dep-id ready)
                                (lambda (a b)
                                  (< (gethash a order-index 999)
                                     (gethash b order-index 999)))))))))
      ;; Check for cycle
      (if (= (length result) (length entries))
          (nreverse result)
        ;; Cycle detected: mark all and clear deps
        (dolist (entry entries)
          (let ((id (supervisor-entry-id entry)))
            (puthash id t cycle-fallback-ids)
            (puthash id nil deps)))
        ;; Return entries with :after and :requires stripped
        ;; (full 13-element entries from parse-entry)
        (mapcar (lambda (entry)
                  (append (cl-subseq entry 0 8)
                          (list nil)                ; clear :after (index 8)
                          (cl-subseq entry 9 12)
                          (list nil)))              ; clear :requires (index 12)
                entries)))))

;;; Helpers

(defun supervisor--parse-entry (entry)
  "Parse ENTRY into a normalized list of entry properties.
Return a list: (id cmd delay enabled-p restart-p logging-p type
stage after oneshot-wait oneshot-timeout tags requires).

Indices (schema v1):
  0  id             - unique identifier string
  1  cmd            - shell command string
  2  delay          - seconds to wait before starting
  3  enabled-p      - whether to start this service
  4  restart-p      - whether to restart on crash (simple only)
  5  logging-p      - whether to log stdout/stderr
  6  type           - `simple' or `oneshot'
  7  stage          - startup stage symbol
  8  after          - ordering dependencies (same stage, start order only)
  9  oneshot-wait   - block stage for oneshot exit
  10 oneshot-timeout - timeout for blocking oneshots
  11 tags           - list of filter tags
  12 requires       - requirement dependencies (pull-in + ordering)

ENTRY can be a command string or a list (COMMAND . PLIST).
Use accessor functions instead of direct indexing for new code."
  (if (stringp entry)
      (let ((id (file-name-nondirectory (car (split-string-and-unquote entry)))))
        (list id entry 0 t t t 'simple 'stage3 nil
              supervisor-oneshot-default-wait supervisor-oneshot-timeout nil nil))
    (let* ((cmd (car entry))
           (plist (cdr entry))
           (id (or (plist-get plist :id)
                   (file-name-nondirectory (car (split-string-and-unquote cmd)))))
           (delay (or (plist-get plist :delay) 0))
           (type-raw (plist-get plist :type))
           (type (cond ((null type-raw) 'simple)
                       ((symbolp type-raw) type-raw)
                       ((stringp type-raw) (intern type-raw))
                       (t 'simple)))
           (_ (unless (memq type '(simple oneshot))
                (supervisor--log 'warning "unknown :type '%s' for %s, using simple" type id)))
           ;; :enabled t (default) or :disabled t (inverse) - whether to start at all
           (enabled (cond ((plist-member plist :enabled)
                           (plist-get plist :enabled))
                          ((plist-member plist :disabled)
                           (not (plist-get plist :disabled)))
                          (t t)))
           ;; :restart t (default) or :no-restart t (inverse) - whether to restart on crash
           (restart (cond ((plist-member plist :restart)
                           (plist-get plist :restart))
                          ((plist-member plist :no-restart)
                           (not (plist-get plist :no-restart)))
                          (t t)))
           (logging (if (plist-member plist :logging)
                        (plist-get plist :logging)
                      t))
           ;; :stage (default stage3)
           (stage-raw (plist-get plist :stage))
           (stage (if stage-raw
                      (let ((s (supervisor--stage-to-int stage-raw)))
                        (supervisor--int-to-stage s))
                    'stage3))
           ;; :after - ordering dependencies (same stage only, start order)
           (after (supervisor--normalize-after (plist-get plist :after)))
           ;; :requires - requirement dependencies (pull-in + ordering)
           (requires (supervisor--normalize-after (plist-get plist :requires)))
           ;; Oneshot wait/timeout settings
           (oneshot-wait (supervisor--oneshot-wait-p plist))
           (oneshot-timeout (supervisor--oneshot-timeout-value plist))
           ;; Tags for filtering (list of symbols or strings)
           (tags-raw (plist-get plist :tags))
           (tags (cond ((null tags-raw) nil)
                       ((listp tags-raw) tags-raw)
                       (t (list tags-raw)))))
      (list id cmd delay enabled restart logging type stage after
            oneshot-wait oneshot-timeout tags requires))))

;;; Entry/Service Conversion Functions

(defun supervisor-entry-to-service (entry)
  "Convert parsed ENTRY tuple to a `supervisor-service' struct."
  (supervisor-service--create
   :id (supervisor-entry-id entry)
   :command (supervisor-entry-command entry)
   :type (supervisor-entry-type entry)
   :stage (supervisor-entry-stage entry)
   :delay (supervisor-entry-delay entry)
   :enabled (supervisor-entry-enabled-p entry)
   :restart (supervisor-entry-restart-p entry)
   :logging (supervisor-entry-logging-p entry)
   :after (supervisor-entry-after entry)
   :requires (supervisor-entry-requires entry)
   :oneshot-wait (supervisor-entry-oneshot-wait entry)
   :oneshot-timeout (supervisor-entry-oneshot-timeout entry)
   :tags (supervisor-entry-tags entry)))

(defun supervisor-service-to-entry (service)
  "Convert SERVICE struct to a parsed entry tuple."
  (list (supervisor-service-id service)
        (supervisor-service-command service)
        (supervisor-service-delay service)
        (supervisor-service-enabled service)
        (supervisor-service-restart service)
        (supervisor-service-logging service)
        (supervisor-service-type service)
        (supervisor-service-stage service)
        (supervisor-service-after service)
        (supervisor-service-oneshot-wait service)
        (supervisor-service-oneshot-timeout service)
        (supervisor-service-tags service)
        (supervisor-service-requires service)))

(defun supervisor--check-crash-loop (id)
  "Check if ID is crash-looping.  Return t if should NOT restart."
  (let* ((now (float-time))
         (times (gethash id supervisor--restart-times))
         (recent (cl-remove-if (lambda (ts) (> (- now ts) supervisor-restart-window)) times)))
    (puthash id (cons now recent) supervisor--restart-times)
    ;; Fail after max-restarts restarts (so with max=3, fail on 4th crash)
    (when (>= (length recent) supervisor-max-restarts)
      (puthash id t supervisor--failed)
      (supervisor--log 'warning "%s crash-looping, marked as FAILED" id)
      t)))

(defun supervisor--format-exit-status (proc-status exit-code)
  "Format exit as human-readable string for logging.
PROC-STATUS is from `process-status' (signal or exit).
EXIT-CODE is from `process-exit-status' (signal number or exit code)."
  (pcase proc-status
    ('signal (format "killed by signal %d" exit-code))
    ('exit (if (= exit-code 0)
               "exited successfully"
             (format "exited with code %d" exit-code)))
    (_ "terminated")))

;;; Staging and Dependency Resolution

(defun supervisor--partition-by-stage (parsed-entries)
  "Partition PARSED-ENTRIES by stage.  Return alist of (stage . entries)."
  (let ((by-stage (make-hash-table)))
    (dolist (entry parsed-entries)
      (let* ((stage (nth 7 entry))
             (stage-int (supervisor--stage-to-int stage)))
        (puthash stage-int (cons entry (gethash stage-int by-stage)) by-stage)))
    ;; Convert to sorted alist, reverse each list to preserve original order
    (let (result)
      (dolist (stage-int '(0 1 2 3))
        (when-let* ((entries (gethash stage-int by-stage)))
          (push (cons stage-int (nreverse entries)) result)))
      (nreverse result))))

(defun supervisor--validate-after (entries stage-ids all-ids)
  "Validate :after references in ENTRIES.
STAGE-IDS is the set of valid IDs in this stage.  ALL-IDS is the set of
all valid IDs across all stages.  Return ENTRIES with invalid :after
edges removed.  Stores computed deps in `supervisor--computed-deps'."
  (mapcar
   (lambda (entry)
     (let* ((id (nth 0 entry))
            (after (nth 8 entry))
            (valid-after
             (cl-remove-if-not
              (lambda (dep)
                (cond
                 ((not (member dep stage-ids))
                  (if (member dep all-ids)
                      (supervisor--log 'warning ":after '%s' for %s is in different stage, ignoring" dep id)
                    (supervisor--log 'warning ":after '%s' for %s does not exist, ignoring" dep id))
                  nil)
                 (t t)))
              after)))
       ;; Store the computed valid dependencies
       (puthash id valid-after supervisor--computed-deps)
       (if (equal after valid-after)
           entry
         ;; Return entry with filtered :after, preserving remaining fields
         (append (cl-subseq entry 0 8) (list valid-after) (cl-subseq entry 9)))))
   entries))

(defun supervisor--all-parsed-entries ()
  "Parse and validate all entries, returning list of valid parsed entries.
Invalid entries are stored in `supervisor--invalid' with reason strings.
Duplicate IDs are skipped with a warning.
Loads unit files from disk via `supervisor--effective-programs'."
  (let ((seen (make-hash-table :test 'equal))
        (idx 0)
        result)
    (dolist (entry (supervisor--effective-programs))
      (let* ((id (supervisor--extract-id entry idx))
             (reason (supervisor--validate-entry entry))
             (parsed (unless reason (supervisor--parse-entry entry))))
        (cl-incf idx)
        (cond
         ;; Invalid entry
         (reason
          (puthash id reason supervisor--invalid)
          (supervisor--log 'warning "INVALID %s - %s" id reason))
         ;; Duplicate ID
         ((gethash id seen)
          (supervisor--log 'warning "duplicate ID '%s', skipping" id))
         ;; Valid entry
         (t
          (puthash id t seen)
          (push parsed result)))))
    (nreverse result)))

(defun supervisor--stable-topo-sort (entries)
  "Stable topological sort of ENTRIES respecting :after dependencies.
Use original order as tie-breaker.  Return sorted list or original on cycle."
  (let* ((ids (mapcar #'car entries))
         (id-to-entry (make-hash-table :test 'equal))
         (id-to-index (make-hash-table :test 'equal))
         (in-degree (make-hash-table :test 'equal))
         (dependents (make-hash-table :test 'equal))
         (result nil)
         (idx 0))
    ;; Build lookup tables
    (dolist (entry entries)
      (let ((id (car entry)))
        (puthash id entry id-to-entry)
        (puthash id idx id-to-index)
        (puthash id 0 in-degree)
        (puthash id nil dependents)
        (cl-incf idx)))
    ;; Build graph and record computed deps (validated, same-stage only)
    ;; Combine :after and :requires for dependency tracking
    (dolist (entry entries)
      (let* ((id (supervisor-entry-id entry))
             (after (supervisor-entry-after entry))
             (requires (supervisor-entry-requires entry))
             (all-deps (cl-union after requires :test #'equal))
             (valid-deps nil))
        (dolist (dep all-deps)
          (when (gethash dep id-to-entry)  ; only count valid deps
            (push dep valid-deps)
            (cl-incf (gethash id in-degree 0))
            (puthash dep (cons id (gethash dep dependents)) dependents)))
        ;; Store computed deps for this entry (reversed to maintain order)
        (puthash id (nreverse valid-deps) supervisor--computed-deps)))
    ;; Kahn's algorithm with stable ordering (by original index)
    (let ((ready (cl-remove-if-not
                  (lambda (id) (= 0 (gethash id in-degree)))
                  ids)))
      ;; Sort ready queue by original index
      (setq ready (sort ready (lambda (a b)
                                (< (gethash a id-to-index)
                                   (gethash b id-to-index)))))
      (while ready
        (let* ((id (pop ready))
               (entry (gethash id id-to-entry)))
          (push entry result)
          ;; Update dependents
          (dolist (dep-id (gethash id dependents))
            (cl-decf (gethash dep-id in-degree))
            (when (= 0 (gethash dep-id in-degree))
              ;; Insert in sorted position by original index
              (setq ready (sort (cons dep-id ready)
                                (lambda (a b)
                                  (< (gethash a id-to-index)
                                     (gethash b id-to-index)))))))))
      ;; Check for cycle
      (if (= (length result) (length entries))
          (nreverse result)
        (supervisor--log 'warning "cycle detected in dependencies, using list order")
        ;; Mark all entries in this stage as having cycle fallback and clear
        ;; computed deps to reflect post-fallback state (no edges)
        (dolist (entry entries)
          (let ((id (supervisor-entry-id entry)))
            (puthash id t supervisor--cycle-fallback-ids)
            (puthash id nil supervisor--computed-deps)))
        ;; Return entries with :after and :requires stripped to break the cycle
        ;; Handle both 11-element (legacy) and 13-element (schema v1) entries
        (mapcar (lambda (entry)
                  (let ((len (length entry)))
                    (cond
                     ;; Full 13-element entry: clear both :after (8) and :requires (12)
                     ((>= len 13)
                      (append (cl-subseq entry 0 8)
                              (list nil)                ; clear :after
                              (cl-subseq entry 9 12)
                              (list nil)))              ; clear :requires
                     ;; 11-element legacy entry: just clear :after (8)
                     ((>= len 11)
                      (append (cl-subseq entry 0 8)
                              (list nil)                ; clear :after
                              (cl-subseq entry 9)))
                     ;; Shorter entries: return as-is
                     (t entry))))
                entries)))))

;;; Logging

(defun supervisor--ensure-log-directory ()
  "Create log directory if it doesn't exist."
  (unless (file-directory-p supervisor-log-directory)
    (make-directory supervisor-log-directory t)))

(defun supervisor--log-file (prog)
  "Return the log file path for PROG."
  (expand-file-name (format "log-%s.log" prog) supervisor-log-directory))

(defun supervisor--rotate-logs ()
  "Rotate existing log files by adding timestamp suffix.
Called once at supervisor startup."
  (supervisor--ensure-log-directory)
  (dolist (file (directory-files supervisor-log-directory t "^log-.*\\.log$"))
    ;; Only rotate files without timestamp (current session logs)
    (when (string-match "^log-\\([^.]+\\)\\.log$" (file-name-nondirectory file))
      (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
             (new-name (replace-regexp-in-string
                        "\\.log$"
                        (format ".%s.log" timestamp)
                        file)))
        (rename-file file new-name t)))))

;;; DAG Scheduler

(defun supervisor--dag-init (entries)
  "Initialize DAG scheduler state for ENTRIES.
Disabled entries are marked ready immediately per spec."
  (setq supervisor--dag-in-degree (make-hash-table :test 'equal))
  (setq supervisor--dag-dependents (make-hash-table :test 'equal))
  (setq supervisor--dag-entries (make-hash-table :test 'equal))
  (setq supervisor--dag-blocking (make-hash-table :test 'equal))
  (setq supervisor--dag-started (make-hash-table :test 'equal))
  (setq supervisor--dag-ready (make-hash-table :test 'equal))
  (setq supervisor--dag-timeout-timers (make-hash-table :test 'equal))
  (setq supervisor--dag-delay-timers (make-hash-table :test 'equal))
  (setq supervisor--dag-id-to-index (make-hash-table :test 'equal))
  ;; Collect disabled entries to mark ready after graph is built
  (let ((disabled-ids nil)
        (idx 0))
    ;; Build lookup tables
    (dolist (entry entries)
      (let* ((id (supervisor-entry-id entry))
             (enabled-p (supervisor-entry-enabled-p entry))
             (type (supervisor-entry-type entry))
             (after (supervisor-entry-after entry))
             (requires (supervisor-entry-requires entry))
             (oneshot-wait (supervisor-entry-oneshot-wait entry))
             ;; Combine :after and :requires for dependency tracking
             (all-deps (cl-union after requires :test #'equal)))
        (puthash id entry supervisor--dag-entries)
        (puthash id (length all-deps) supervisor--dag-in-degree)
        (puthash id nil supervisor--dag-dependents)
        (puthash id idx supervisor--dag-id-to-index)
        (cl-incf idx)
        ;; Track blocking oneshots (only if enabled)
        (when (and enabled-p (eq type 'oneshot) oneshot-wait)
          (puthash id t supervisor--dag-blocking))
        ;; Set initial state via FSM transition
        (cond
         ((not enabled-p)
          (push id disabled-ids)
          (supervisor--transition-state id 'disabled))
         ((> (length all-deps) 0)
          (supervisor--transition-state id 'waiting-on-deps))
         (t
          (supervisor--transition-state id 'stage-not-started)))))
    ;; Build dependents graph from combined :after + :requires
    (dolist (entry entries)
      (let* ((id (supervisor-entry-id entry))
             (after (supervisor-entry-after entry))
             (requires (supervisor-entry-requires entry))
             (all-deps (cl-union after requires :test #'equal)))
        (dolist (dep all-deps)
          (when (gethash dep supervisor--dag-entries)
            (puthash dep (cons id (gethash dep supervisor--dag-dependents))
                     supervisor--dag-dependents)))))
    ;; Mark disabled entries as ready immediately (spec requirement)
    ;; This decrements in-degree of their dependents
    (dolist (id disabled-ids)
      (puthash id t supervisor--dag-started)
      (supervisor--dag-mark-ready id))))

(defun supervisor--dag-mark-ready (id)
  "Mark ID as ready and unlock its dependents.
Called when a process is spawned (simple) or exits (oneshot).
No-op if DAG scheduler is not active (e.g., manual starts)."
  (when (and supervisor--dag-ready
             (not (gethash id supervisor--dag-ready)))
    (puthash id t supervisor--dag-ready)
    ;; Record ready time for blame view
    (puthash id (float-time) supervisor--ready-times)
    ;; Cancel any timeout timer
    (when-let* ((timer (gethash id supervisor--dag-timeout-timers)))
      (when (timerp timer)
        (cancel-timer timer))
      (remhash id supervisor--dag-timeout-timers))
    ;; Remove from blocking set
    (remhash id supervisor--dag-blocking)
    ;; Log that this entry is now ready (event emission is caller's responsibility)
    (supervisor--log 'info "%s ready" id)
    ;; Unlock dependents and collect newly ready ones
    (let ((newly-ready nil))
      (dolist (dep-id (gethash id supervisor--dag-dependents))
        (when (gethash dep-id supervisor--dag-in-degree)
          (cl-decf (gethash dep-id supervisor--dag-in-degree))
          (when (= 0 (gethash dep-id supervisor--dag-in-degree))
            (supervisor--log 'info "%s unlocked by %s" dep-id id)
            (push dep-id newly-ready))))
      ;; Sort by original index for stable ordering, then start
      (setq newly-ready (sort newly-ready
                              (lambda (a b)
                                (< (gethash a supervisor--dag-id-to-index 999)
                                   (gethash b supervisor--dag-id-to-index 999)))))
      (dolist (dep-id newly-ready)
        (supervisor--dag-try-start-entry dep-id)))
    ;; Check if stage is complete
    (supervisor--dag-check-stage-complete)))

(defun supervisor--dag-process-pending-starts ()
  "Process pending start queue if under max-concurrent-starts limit."
  (while (and supervisor--dag-pending-starts
              (or (null supervisor-max-concurrent-starts)
                  (< supervisor--dag-active-starts supervisor-max-concurrent-starts)))
    (let* ((id (pop supervisor--dag-pending-starts))
           (entry (gethash id supervisor--dag-entries)))
      (when (and entry
                 (not (gethash id supervisor--dag-started))
                 (not supervisor--shutting-down))
        ;; Count is incremented in supervisor--dag-do-start when spawn occurs
        (supervisor--dag-start-entry-async entry)))))

(defun supervisor--dag-try-start-entry (id)
  "Try to start entry ID if not already started and in-degree is 0."
  (when (and (not (gethash id supervisor--dag-started))
             (not (gethash id supervisor--dag-delay-timers))  ; not already scheduled
             (= 0 (gethash id supervisor--dag-in-degree 0))
             (not supervisor--shutting-down))
    ;; Check max-concurrent-starts limit
    (if (and supervisor-max-concurrent-starts
             (>= supervisor--dag-active-starts supervisor-max-concurrent-starts))
        ;; Queue for later
        (unless (member id supervisor--dag-pending-starts)
          (setq supervisor--dag-pending-starts
                (append supervisor--dag-pending-starts (list id))))
      ;; Start immediately (count is managed in supervisor--dag-do-start)
      (let ((entry (gethash id supervisor--dag-entries)))
        (supervisor--dag-start-entry-async entry)))))

(defun supervisor--dag-start-entry-async (entry)
  "Start ENTRY asynchronously.
Mark ready immediately for simple processes, on exit for oneshot."
  (pcase-let ((`(,id ,cmd ,delay ,enabled-p ,restart-p ,logging-p ,type ,_stage ,_after ,oneshot-wait ,oneshot-timeout ,_tags) entry))
    ;; Check effective enabled state (config + runtime override)
    (let ((effective-enabled (supervisor--get-effective-enabled id enabled-p)))
      (cond
       ;; Disabled: mark started and ready immediately
       ((not effective-enabled)
        (supervisor--log 'info "%s disabled, skipping" id)
        (supervisor--transition-state id 'disabled)
        (puthash id t supervisor--dag-started)
        (supervisor--dag-mark-ready id))
       ;; Delay: schedule start after delay (don't mark started yet)
       ((> delay 0)
        (supervisor--log 'info "scheduling %s after %ds delay..." id delay)
        (supervisor--transition-state id 'delayed)
        (puthash id
                 (run-at-time delay nil
                              (lambda ()
                                (remhash id supervisor--dag-delay-timers)
                                (supervisor--dag-do-start id cmd logging-p type restart-p oneshot-wait oneshot-timeout)))
                 supervisor--dag-delay-timers))
       ;; Start immediately
       (t
        (supervisor--dag-do-start id cmd logging-p type restart-p oneshot-wait oneshot-timeout))))))

(defun supervisor--dag-finish-spawn-attempt ()
  "Finish a spawn attempt by decrementing count and processing queue."
  (cl-decf supervisor--dag-active-starts)
  (supervisor--dag-process-pending-starts))

(defun supervisor--dag-handle-spawn-failure (id)
  "Handle spawn failure for ID by marking state and unblocking dependents."
  (supervisor--transition-state id 'failed-to-spawn)
  (supervisor--emit-event 'process-failed id nil nil)
  (supervisor--dag-finish-spawn-attempt)
  (supervisor--dag-mark-ready id))

(defun supervisor--dag-setup-oneshot-timeout (id timeout)
  "Set up timeout timer for oneshot ID with TIMEOUT seconds."
  (puthash id
           (run-at-time timeout nil
                        (lambda ()
                          (supervisor--log 'warning "oneshot %s timed out after %ds" id timeout)
                          (supervisor--dag-mark-ready id)
                          (supervisor--emit-event 'process-ready id nil
                                                  (list :type 'oneshot :timeout t))))
           supervisor--dag-timeout-timers))

(defun supervisor--dag-handle-spawn-success (id type oneshot-timeout)
  "Handle successful spawn of ID with TYPE and ONESHOT-TIMEOUT."
  (supervisor--transition-state id 'started)
  (supervisor--emit-event 'process-started id nil (list :type type))
  (supervisor--dag-finish-spawn-attempt)
  (if (eq type 'oneshot)
      (progn
        (supervisor--log 'info "started oneshot %s" id)
        (when oneshot-timeout
          (supervisor--dag-setup-oneshot-timeout id oneshot-timeout)))
    ;; Simple process: spawned = ready
    (supervisor--log 'info "started %s" id)
    (supervisor--dag-mark-ready id)
    (supervisor--emit-event 'process-ready id nil (list :type type))))

(defun supervisor--dag-do-start (id cmd logging-p type restart-p _oneshot-wait oneshot-timeout)
  "Start process ID with CMD, LOGGING-P, TYPE, RESTART-P, and ONESHOT-TIMEOUT."
  (puthash id t supervisor--dag-started)
  (puthash id (float-time) supervisor--start-times)
  (cl-incf supervisor--dag-active-starts)
  (let ((args (split-string-and-unquote cmd)))
    (if (not (executable-find (car args)))
        (progn
          (supervisor--log 'warning "executable not found for %s: %s" id (car args))
          (supervisor--dag-handle-spawn-failure id))
      (let ((proc (supervisor--start-process id cmd logging-p type restart-p)))
        (if (not proc)
            (supervisor--dag-handle-spawn-failure id)
          (supervisor--dag-handle-spawn-success id type oneshot-timeout))))))

(defun supervisor--dag-check-stage-complete ()
  "Check if current stage is complete and invoke callback if so."
  (when (and supervisor--dag-stage-complete-callback
             ;; All entries started (actually spawned, not just scheduled)
             (= (hash-table-count supervisor--dag-started)
                (hash-table-count supervisor--dag-entries))
             ;; No delayed entries pending
             (= 0 (hash-table-count supervisor--dag-delay-timers))
             ;; No blocking oneshots pending
             (= 0 (hash-table-count supervisor--dag-blocking)))
    (supervisor--log 'info "stage complete")
    (let ((callback supervisor--dag-stage-complete-callback))
      (setq supervisor--dag-stage-complete-callback nil)
      (funcall callback))))

(defun supervisor--dag-cleanup ()
  "Clean up DAG scheduler state."
  ;; Cancel any remaining timeout timers
  (when supervisor--dag-timeout-timers
    (maphash (lambda (_id timer)
               (when (timerp timer)
                 (cancel-timer timer)))
             supervisor--dag-timeout-timers))
  ;; Cancel any remaining delay timers
  (when supervisor--dag-delay-timers
    (maphash (lambda (_id timer)
               (when (timerp timer)
                 (cancel-timer timer)))
             supervisor--dag-delay-timers))
  ;; Cancel stage timeout timer
  (when (timerp supervisor--dag-stage-timeout-timer)
    (cancel-timer supervisor--dag-stage-timeout-timer))
  (setq supervisor--dag-in-degree nil)
  (setq supervisor--dag-dependents nil)
  (setq supervisor--dag-entries nil)
  (setq supervisor--dag-blocking nil)
  (setq supervisor--dag-started nil)
  (setq supervisor--dag-ready nil)
  (setq supervisor--dag-timeout-timers nil)
  (setq supervisor--dag-delay-timers nil)
  (setq supervisor--dag-id-to-index nil)
  (setq supervisor--dag-stage-complete-callback nil)
  (setq supervisor--dag-stage-timeout-timer nil)
  (setq supervisor--dag-pending-starts nil)
  (setq supervisor--dag-active-starts 0))

;;; Process Management

(defun supervisor--get-effective-logging (id default-logging)
  "Get effective logging state for ID with DEFAULT-LOGGING as fallback."
  (let ((override (gethash id supervisor--logging)))
    (if override (eq override 'enabled) default-logging)))

(defun supervisor--get-effective-restart (id config-restart)
  "Get effective restart state for ID.
CONFIG-RESTART is the config's :restart value (t or nil).
Return t if restart is enabled, nil if disabled."
  (let ((override (gethash id supervisor--restart-override)))
    (cond ((eq override 'enabled) t)
          ((eq override 'disabled) nil)
          (t config-restart))))

(defun supervisor--get-effective-enabled (id config-enabled)
  "Get effective enabled state for ID.
CONFIG-ENABLED is the config's :enabled value (t or nil).
Return t if enabled, nil if disabled.  Masked entries are always
disabled.  Runtime overrides take effect on next start."
  (if (eq (gethash id supervisor--mask-override) 'masked)
      nil
    (let ((override (gethash id supervisor--enabled-override)))
      (cond ((eq override 'enabled) t)
            ((eq override 'disabled) nil)
            (t config-enabled)))))

(defun supervisor--handle-oneshot-exit (name proc-status exit-code)
  "Handle exit of oneshot process NAME.
PROC-STATUS is the process status symbol, EXIT-CODE is the exit code.
Logs completion, invokes callbacks, and notifies DAG scheduler.
For signal deaths, stores negated signal number (e.g., -9 for SIGKILL)
to distinguish from normal exits with positive exit codes."
  (let ((stored-code (if (eq proc-status 'signal)
                         (- exit-code)  ; Negate signal number
                       exit-code)))
    (puthash name stored-code supervisor--oneshot-completed))
  (if (eq proc-status 'signal)
      (supervisor--log 'warning "oneshot %s %s"
                       name (supervisor--format-exit-status proc-status exit-code))
    (if (> exit-code 0)
        (supervisor--log 'warning "oneshot %s %s"
                         name (supervisor--format-exit-status proc-status exit-code))
      (supervisor--log 'info "oneshot %s completed" name)))
  ;; Invoke any registered wait callback (event-driven)
  (when-let* ((cb-entry (gethash name supervisor--oneshot-callbacks)))
    (remhash name supervisor--oneshot-callbacks)
    (when (cdr cb-entry)
      (cancel-timer (cdr cb-entry)))
    (funcall (car cb-entry) (not supervisor--shutting-down)))
  ;; Notify DAG scheduler and emit ready event
  (supervisor--dag-mark-ready name)
  (supervisor--emit-event 'process-ready name nil
                          (list :type 'oneshot :exit-code exit-code)))

(defun supervisor--handle-shutdown-exit ()
  "Handle process exit during shutdown.
Decrement remaining count and signal completion when all done."
  (cl-decf supervisor--shutdown-remaining)
  (when (<= supervisor--shutdown-remaining 0)
    ;; Cancel timeout timer (early completion)
    (when supervisor--shutdown-timer
      (cancel-timer supervisor--shutdown-timer)
      (setq supervisor--shutdown-timer nil))
    ;; Mark complete and invoke callback if provided
    (let ((cb supervisor--shutdown-callback))
      (setq supervisor--shutdown-callback nil)
      (clrhash supervisor--processes)
      (setq supervisor--shutdown-complete-flag t)
      (when cb (funcall cb)))))

(defun supervisor--schedule-restart (id cmd default-logging type config-restart proc-status exit-code)
  "Schedule restart of process ID after crash.
CMD, DEFAULT-LOGGING, TYPE, CONFIG-RESTART are original process params.
PROC-STATUS and EXIT-CODE describe the exit for logging."
  (supervisor--log 'info "%s %s, restarting..."
                   id (supervisor--format-exit-status proc-status exit-code))
  (puthash id
           (run-at-time supervisor-restart-delay nil
                        #'supervisor--start-process id cmd default-logging type config-restart t)
           supervisor--restart-timers))

(defun supervisor--make-process-sentinel (id cmd default-logging type config-restart)
  "Create a process sentinel for ID with captured parameters.
CMD, DEFAULT-LOGGING, TYPE, CONFIG-RESTART are stored for restart."
  (lambda (p _event)
    (unless (process-live-p p)
      (let* ((name (process-name p))
             (proc-status (process-status p))
             (exit-code (process-exit-status p))
             (exit-status (pcase proc-status
                            ('signal 'signal)
                            ('exit 'exited)
                            (_ 'unknown))))
        (remhash name supervisor--processes)
        ;; Emit process exit event
        (supervisor--emit-event 'process-exit name nil
                                (list :status exit-status :code exit-code))
        ;; Handle oneshot completion
        (when (eq type 'oneshot)
          (supervisor--handle-oneshot-exit name proc-status exit-code))
        (supervisor--maybe-refresh-dashboard)
        ;; Handle shutdown tracking
        (when supervisor--shutting-down
          (supervisor--handle-shutdown-exit))
        ;; Schedule restart if appropriate
        (unless (or (eq type 'oneshot)
                    supervisor--shutting-down
                    (gethash name supervisor--manually-stopped)
                    (eq (gethash name supervisor--enabled-override) 'disabled)
                    (not (supervisor--get-effective-restart name config-restart))
                    (gethash name supervisor--failed)
                    (supervisor--check-crash-loop name))
          (supervisor--schedule-restart id cmd default-logging type config-restart
                                        proc-status exit-code))))))

(defun supervisor--start-process (id cmd default-logging type config-restart &optional is-restart)
  "Start CMD with identifier ID.
DEFAULT-LOGGING is the config value; runtime override is checked at restart.
TYPE is `simple' (long-running) or `oneshot' (run once).
CONFIG-RESTART is the config's :restart value (t or nil).
IS-RESTART is t when called from a crash-triggered restart timer."
  ;; Clear any pending restart timer for this ID first
  (when-let* ((timer (gethash id supervisor--restart-timers)))
    (when (timerp timer)
      (cancel-timer timer))
    (remhash id supervisor--restart-timers))
  ;; Clear manually-stopped flag - starting an entry re-enables restart behavior
  (remhash id supervisor--manually-stopped)
  ;; Guard conditions - don't start if:
  ;; - shutting down
  ;; - this is a crash-restart AND (restart disabled or failed or runtime-disabled)
  ;; - already running (race with pending timer)
  (unless (or supervisor--shutting-down
              (and is-restart (not (supervisor--get-effective-restart id config-restart)))
              (and is-restart (gethash id supervisor--failed))
              (and is-restart (eq (gethash id supervisor--enabled-override) 'disabled))
              (and (gethash id supervisor--processes)
                   (process-live-p (gethash id supervisor--processes))))
    (let* ((args (split-string-and-unquote cmd))
           (logging (supervisor--get-effective-logging id default-logging))
           (log-file (when logging
                       (supervisor--ensure-log-directory)
                       (supervisor--log-file id)))
           (proc (make-process
                  :name id
                  :command args
                  :connection-type 'pipe
                  ;; Merge stderr into stdout - both captured by filter
                  :stderr nil
                  :filter (when logging
                            (lambda (_proc output)
                              (write-region output nil log-file t 'silent)))
                  :sentinel (supervisor--make-process-sentinel
                             id cmd default-logging type config-restart))))
      (set-process-query-on-exit-flag proc nil)
      (puthash id proc supervisor--processes)
      proc)))

(defun supervisor--wait-for-oneshot (id &optional timeout callback)
  "Wait for oneshot ID to complete asynchronously (event-driven).
TIMEOUT is seconds; nil means wait indefinitely.
CALLBACK is called with t on completion, nil on timeout/shutdown.
If CALLBACK is nil, returns immediately (fire-and-forget).
Uses sentinel-driven notification instead of polling."
  (when callback
    (cond
     ;; Already completed
     ((gethash id supervisor--oneshot-completed)
      (funcall callback t))
     ;; Already shutting down
     (supervisor--shutting-down
      (funcall callback nil))
     ;; Register callback for sentinel to invoke
     (t
      (let ((timeout-timer
             (when timeout
               (run-at-time timeout nil
                            (lambda ()
                              (when-let* ((cb-entry (gethash id supervisor--oneshot-callbacks)))
                                (remhash id supervisor--oneshot-callbacks)
                                (funcall (car cb-entry) nil)))))))
        (puthash id (cons callback timeout-timer) supervisor--oneshot-callbacks))))))

(defun supervisor--start-entry-async (entry callback)
  "Start ENTRY asynchronously for dashboard manual start.
CALLBACK is called with t on success, nil on error.  CALLBACK may be nil."
  (pcase-let ((`(,id ,cmd ,_delay ,enabled-p ,restart-p ,logging-p ,type ,_stage ,_after ,_owait ,otimeout ,_tags) entry))
    (if (not (supervisor--get-effective-enabled id enabled-p))
        (progn
          (supervisor--log 'info "%s disabled (config or override), skipping" id)
          (when callback (funcall callback t)))
      (let ((args (split-string-and-unquote cmd)))
        (if (not (executable-find (car args)))
            (progn
              (supervisor--log 'warning "executable not found for %s: %s" id (car args))
              (supervisor--emit-event 'process-failed id nil nil)
              (when callback (funcall callback nil)))
          ;; Start the process
          (let ((proc (supervisor--start-process id cmd logging-p type restart-p)))
            (if (not proc)
                (progn
                  (supervisor--emit-event 'process-failed id nil nil)
                  (when callback (funcall callback nil)))
              (supervisor--emit-event 'process-started id nil (list :type type))
              (if (eq type 'oneshot)
                  ;; Wait for oneshot via timer (no polling loop)
                  ;; process-ready is emitted by oneshot exit handler
                  (progn
                    (supervisor--log 'info "waiting for oneshot %s..." id)
                    (supervisor--wait-for-oneshot
                     id otimeout
                     (lambda (completed)
                       (if completed
                           (supervisor--log 'info "oneshot %s completed" id)
                         (supervisor--log 'warning "oneshot %s timed out" id))
                       (when callback (funcall callback completed)))))
                ;; Simple process: spawned = ready
                (supervisor--log 'info "started %s" id)
                (supervisor--emit-event 'process-ready id nil (list :type type))
                (when callback (funcall callback t))))))))))

(defun supervisor--manual-start (id)
  "Attempt to manually start entry with ID.
Returns a plist with :status and :reason.
:status is one of: started, skipped, error
:reason explains why (for skipped/error cases).

This function handles all pre-start checks and state clearing,
ensuring consistent behavior between dashboard and CLI.

Per the systemctl model, disabled units CAN be manually started
\(session-only, no enabled override change).  Only masked units
are blocked.  Manually started disabled units are tracked in
`supervisor--manually-started' so that reconcile does not stop them."
  (cond
   ;; Entry is invalid (check first - get-entry-for-id skips invalid entries)
   ((gethash id supervisor--invalid)
    (list :status 'error :reason "invalid entry"))
   ;; Already running
   ((and (gethash id supervisor--processes)
         (process-live-p (gethash id supervisor--processes)))
    (list :status 'skipped :reason "already running"))
   ;; Check if entry exists and can be started
   (t
    (let ((entry (supervisor--get-entry-for-id id)))
      (if (not entry)
          (list :status 'error :reason "not found")
        (pcase-let ((`(,_id ,cmd ,_delay ,_enabled-p ,restart-p ,logging-p ,type ,_stage ,_after ,_owait ,_otimeout ,_tags) entry))
          (cond
           ;; Masked (highest precedence - only mask blocks manual start)
           ((eq (gethash id supervisor--mask-override) 'masked)
            (list :status 'skipped :reason "masked"))
           ;; Executable not found
           ((not (executable-find (car (split-string-and-unquote cmd))))
            (list :status 'error :reason "executable not found"))
           ;; All checks passed - start
           (t
            ;; Clear failed state and oneshot completion on manual start
            (remhash id supervisor--failed)
            (remhash id supervisor--restart-times)
            (remhash id supervisor--oneshot-completed)
            ;; Clear manually-stopped so restart works again
            (remhash id supervisor--manually-stopped)
            (let ((proc (supervisor--start-process id cmd logging-p type restart-p)))
              (if proc
                  (progn
                    ;; Track manual start only on success so reconcile
                    ;; preserves disabled running units.
                    (puthash id t supervisor--manually-started)
                    (list :status 'started :reason nil))
                (list :status 'error :reason "failed to start process")))))))))))

(defun supervisor--manual-stop (id)
  "Attempt to manually stop entry with ID.
Returns a plist with :status and :reason.
:status is one of: stopped, skipped, error
:reason explains why (for skipped/error cases).

This function sets manually-stopped to suppress restart,
ensuring consistent behavior between dashboard and CLI."
  (let ((proc (gethash id supervisor--processes)))
    (cond
     ((not proc)
      (list :status 'skipped :reason "not running"))
     ((not (process-live-p proc))
      (list :status 'skipped :reason "not running"))
     (t
      ;; Mark as manually stopped so sentinel doesn't restart it
      (puthash id t supervisor--manually-stopped)
      ;; Clear manually-started so reconcile can treat it normally
      (remhash id supervisor--manually-started)
      (delete-process proc)
      (list :status 'stopped :reason nil)))))

(defun supervisor--manual-kill (id &optional signal)
  "Attempt to send SIGNAL to running entry with ID.
Returns a plist with :status and :reason.
:status is one of: signaled, skipped, error
:reason explains why (for skipped/error cases).

SIGNAL defaults to `SIGTERM'.  Unlike `supervisor--manual-stop', this
does not mark the entry as manually stopped, so restart policy remains
in effect."
  (let* ((sig (or signal 'SIGTERM))
         (proc (gethash id supervisor--processes)))
    (cond
     ((not proc)
      (list :status 'skipped :reason "not running"))
     ((not (process-live-p proc))
      (list :status 'skipped :reason "not running"))
     (t
      (signal-process proc sig)
      (list :status 'signaled :reason nil)))))

(defun supervisor--dag-force-stage-complete ()
  "Force stage completion due to timeout.
Mark all unstarted entries as timed out and invoke the callback."
  (supervisor--log 'warning "stage %s timed out, forcing completion"
                   supervisor--current-stage)
  ;; Mark all unstarted entries
  (maphash (lambda (id _entry)
             (unless (gethash id supervisor--dag-started)
               (puthash id t supervisor--dag-started)
               (supervisor--transition-state id 'stage-timeout)))
           supervisor--dag-entries)
  ;; Clear blocking oneshots
  (clrhash supervisor--dag-blocking)
  ;; Cancel delay timers
  (maphash (lambda (_id timer)
             (when (timerp timer)
               (cancel-timer timer)))
           supervisor--dag-delay-timers)
  (clrhash supervisor--dag-delay-timers)
  ;; Invoke callback
  (when supervisor--dag-stage-complete-callback
    (let ((callback supervisor--dag-stage-complete-callback))
      (setq supervisor--dag-stage-complete-callback nil)
      (funcall callback))))

(defun supervisor--start-stage-async (stage-name entries callback)
  "Start ENTRIES for STAGE-NAME asynchronously.  Call CALLBACK when complete."
  (supervisor--log 'info "=== Stage: %s ===" stage-name)
  (setq supervisor--current-stage stage-name)
  (supervisor--emit-event 'stage-start nil stage-name nil)
  (if (null entries)
      ;; Empty stage, proceed immediately (callback handles hook and tracking)
      (funcall callback)
    ;; Initialize DAG scheduler
    (supervisor--dag-init entries)
    (setq supervisor--dag-stage-complete-callback callback)
    ;; Set up stage timeout if configured
    (when supervisor-stage-timeout
      (setq supervisor--dag-stage-timeout-timer
            (run-at-time supervisor-stage-timeout nil
                         #'supervisor--dag-force-stage-complete)))
    ;; Initialize pending starts queue for max-concurrent-starts
    (setq supervisor--dag-pending-starts nil)
    (setq supervisor--dag-active-starts 0)
    ;; Start all initially ready entries (in-degree = 0)
    (let ((ready-ids nil))
      (maphash (lambda (id in-deg)
                 (when (= 0 in-deg)
                   (push id ready-ids)))
               supervisor--dag-in-degree)
      ;; Sort by original order (entries are already sorted)
      (setq ready-ids (sort ready-ids
                            (lambda (a b)
                              (let ((idx-a (cl-position a entries :key #'car :test #'equal))
                                    (idx-b (cl-position b entries :key #'car :test #'equal)))
                                (< (or idx-a 999) (or idx-b 999))))))
      (if (null ready-ids)
          ;; No entries to start (all have dependencies), this shouldn't happen
          (funcall callback)
        ;; Start all ready entries
        (dolist (id ready-ids)
          (supervisor--dag-try-start-entry id))
        ;; Check if stage is already complete (e.g., all entries were disabled)
        (supervisor--dag-check-stage-complete)))))

;;;###autoload
(defun supervisor-start ()
  "Start all programs defined in unit files by stage.
Uses async DAG scheduler - stages run sequentially, but entries within
a stage can run in parallel based on :after dependencies.

Ready semantics (when dependents are unblocked):
- Simple process: spawned (process started)
- Oneshot: exited (success or failure) or timed out
- Disabled entry: immediately ready
- Start failure: immediately ready (dependents proceed)"
  (interactive)
  (setq supervisor--shutting-down nil)
  ;; Cancel any existing timers before clearing (prevents orphaned timers)
  (dolist (timer supervisor--timers)
    (when (timerp timer)
      (cancel-timer timer)))
  (setq supervisor--timers nil)
  (maphash (lambda (_id timer)
             (when (timerp timer)
               (cancel-timer timer)))
           supervisor--restart-timers)
  ;; Stop any existing timer scheduler (if timer module loaded)
  (when (fboundp 'supervisor-timer-scheduler-stop)
    (supervisor-timer-scheduler-stop))
  ;; Reset runtime state for clean session
  (clrhash supervisor--restart-override)
  (clrhash supervisor--enabled-override)
  (clrhash supervisor--mask-override)
  (clrhash supervisor--manually-stopped)
  (clrhash supervisor--manually-started)
  (clrhash supervisor--failed)
  (clrhash supervisor--invalid)
  (when (boundp 'supervisor--invalid-timers)
    (clrhash supervisor--invalid-timers))
  (clrhash supervisor--logging)
  (clrhash supervisor--restart-times)
  (clrhash supervisor--restart-timers)
  (clrhash supervisor--oneshot-completed)
  (clrhash supervisor--start-times)
  (clrhash supervisor--ready-times)
  (clrhash supervisor--cycle-fallback-ids)
  (clrhash supervisor--computed-deps)
  (clrhash supervisor--entry-state)
  ;; Reset timer state (if timer module loaded)
  (when (boundp 'supervisor--timer-state)
    (clrhash supervisor--timer-state))
  (when (boundp 'supervisor--timer-state-loaded)
    (setq supervisor--timer-state-loaded nil))
  (when (boundp 'supervisor--timer-list)
    (setq supervisor--timer-list nil))
  (when (boundp 'supervisor--scheduler-startup-time)
    (setq supervisor--scheduler-startup-time nil))
  (setq supervisor--current-stage nil)
  (setq supervisor--completed-stages nil)
  (setq supervisor--current-plan nil)
  ;; Load persisted overrides (restores enabled/restart/logging overrides)
  (supervisor--load-overrides)
  (supervisor--dag-cleanup)
  (supervisor--rotate-logs)

  ;; Build execution plan (pure, deterministic)
  (let ((plan (supervisor--build-plan (supervisor--effective-programs))))
    ;; Populate legacy globals from plan for dashboard/other code
    (maphash (lambda (k v) (puthash k v supervisor--invalid))
             (supervisor-plan-invalid plan))
    (supervisor--merge-unit-file-invalid)
    (maphash (lambda (k v) (puthash k v supervisor--cycle-fallback-ids))
             (supervisor-plan-cycle-fallback-ids plan))
    (maphash (lambda (k v) (puthash k v supervisor--computed-deps))
             (supervisor-plan-deps plan))
    ;; Validate timers only when timer subsystem is active (and timer module loaded)
    (when (and (fboundp 'supervisor-timer-subsystem-active-p)
               (supervisor-timer-subsystem-active-p)
               (fboundp 'supervisor-timer-build-list))
      (supervisor-timer-build-list plan))
    ;; Initialize all entry states to stage-not-started via FSM
    (dolist (entry (supervisor-plan-entries plan))
      (supervisor--transition-state (car entry) 'stage-not-started))
    ;; Process stages asynchronously using plan's pre-sorted by-stage data
    (supervisor--start-stages-from-plan (supervisor-plan-by-stage plan))))

(defun supervisor--start-stages-from-plan (remaining-stages)
  "Process REMAINING-STAGES from prebuilt plan asynchronously.
REMAINING-STAGES is an alist of (stage-int . sorted-entries) from the plan.
Entries are already validated and topologically sorted."
  (if (or (null remaining-stages) supervisor--shutting-down)
      (progn
        (supervisor--dag-cleanup)
        (setq supervisor--current-stage nil)
        (supervisor--log 'info "startup complete")
        ;; Start timer scheduler after all stages complete (if timer module loaded)
        (when (and supervisor-timers
                   (not supervisor--shutting-down)
                   (fboundp 'supervisor-timer-scheduler-start))
          (supervisor-timer-scheduler-start)))
    (let* ((stage-pair (car remaining-stages))
           (rest (cdr remaining-stages))
           (stage-int (car stage-pair))
           (stage-name (supervisor--int-to-stage stage-int))
           (entries (cdr stage-pair)))
      ;; Entries are already sorted by plan builder - no recomputation needed
      (supervisor--start-stage-async
       stage-name entries
       (lambda ()
         (supervisor--emit-event 'stage-complete nil stage-name nil)
         (push stage-name supervisor--completed-stages)
         (supervisor--dag-cleanup)
         (supervisor--start-stages-from-plan rest))))))

;;;###autoload
(defun supervisor-stop-now ()
  "Stop all supervised processes immediately with SIGKILL.
This is a synchronous function suitable for `kill-emacs-hook'.
Unlike `supervisor-stop', it sends SIGKILL immediately and waits
briefly for processes to terminate, ensuring a clean exit."
  (interactive)
  (setq supervisor--shutting-down t)
  ;; Stop timer scheduler (if timer module loaded)
  (when (fboundp 'supervisor-timer-scheduler-stop)
    (supervisor-timer-scheduler-stop))
  ;; Cancel all timers
  (dolist (timer supervisor--timers)
    (when (timerp timer)
      (cancel-timer timer)))
  (setq supervisor--timers nil)
  (maphash (lambda (_id timer)
             (when (timerp timer)
               (cancel-timer timer)))
           supervisor--restart-timers)
  (clrhash supervisor--restart-timers)
  (supervisor--dag-cleanup)
  (supervisor--emit-event 'cleanup nil nil nil)
  ;; Send SIGKILL to all processes immediately
  (maphash (lambda (_name proc)
             (when (process-live-p proc)
               (signal-process proc 'SIGKILL)))
           supervisor--processes)
  ;; Brief wait for processes to die (max 0.5s)
  (let ((deadline (+ (float-time) 0.5)))
    (while (and (< (float-time) deadline)
                (cl-some #'process-live-p
                         (hash-table-values supervisor--processes)))
      (sleep-for 0.05)))
  ;; Clean up state
  (clrhash supervisor--processes)
  (setq supervisor--shutdown-complete-flag t
        supervisor--shutting-down nil))

;;;###autoload
(defun supervisor-stop (&optional callback)
  "Stop all supervised processes gracefully (async).
Sends SIGTERM immediately and returns.  A timer handles the graceful
shutdown period: after `supervisor-shutdown-timeout' seconds, any
survivors receive SIGKILL.  Optional CALLBACK is called with no
arguments when shutdown completes.  Check `supervisor--shutdown-complete-flag'
to poll completion status if needed.

For `kill-emacs-hook', use `supervisor-stop-now' instead."
  (interactive)
  (setq supervisor--shutting-down t)
  ;; Stop timer scheduler (if timer module loaded)
  (when (fboundp 'supervisor-timer-scheduler-stop)
    (supervisor-timer-scheduler-stop))
  ;; Cancel any pending delayed starts
  (dolist (timer supervisor--timers)
    (when (timerp timer)
      (cancel-timer timer)))
  (setq supervisor--timers nil)
  ;; Cancel any pending restart timers
  (maphash (lambda (_id timer)
             (when (timerp timer)
               (cancel-timer timer)))
           supervisor--restart-timers)
  (clrhash supervisor--restart-timers)
  ;; Clean up DAG scheduler
  (supervisor--dag-cleanup)
  (supervisor--emit-event 'cleanup nil nil nil)
  ;; Send SIGTERM to all
  (maphash (lambda (_name proc)
             (when (process-live-p proc)
               (signal-process proc 'SIGTERM)))
           supervisor--processes)
  ;; Set up event-driven shutdown (sentinel-driven, no polling)
  (let ((live-count 0))
    (maphash (lambda (_name proc)
               (when (process-live-p proc)
                 (cl-incf live-count)))
             supervisor--processes)
    (if (zerop live-count)
        ;; No live processes, cleanup immediately
        (progn
          (clrhash supervisor--processes)
          (setq supervisor--shutdown-complete-flag t)
          (when callback (funcall callback)))
      ;; Set up event-driven completion via sentinel
      (setq supervisor--shutdown-complete-flag nil
            supervisor--shutdown-remaining live-count
            supervisor--shutdown-callback callback)
      ;; Set up one-shot SIGKILL timeout timer (safety net)
      (setq supervisor--shutdown-timer
            (run-at-time
             supervisor-shutdown-timeout nil
             (lambda ()
               (setq supervisor--shutdown-timer nil)
               ;; SIGKILL any survivors
               (maphash (lambda (_name proc)
                          (when (process-live-p proc)
                            (signal-process proc 'SIGKILL)))
                        supervisor--processes)
               ;; Complete shutdown
               (let ((cb supervisor--shutdown-callback))
                 (setq supervisor--shutdown-callback nil
                       supervisor--shutdown-remaining 0)
                 (clrhash supervisor--processes)
                 (setq supervisor--shutdown-complete-flag t)
                 (when cb (funcall cb)))))))))

;;; Declarative Reconciler (Phase 4)

(defun supervisor--compute-actions (plan snapshot)
  "Compute reconciliation actions from PLAN and SNAPSHOT.
This is a pure function that returns a list of action plists.
Each action has :op, :id, and :reason keys.

Action ops:
  - `stop'   : stop a running process
  - `start'  : start a new process
  - `noop'   : no action needed (already converged)
  - `skip'   : skip due to failed/completed state

This function does not modify any state."
  (let ((actions nil)
        (plan-entries (supervisor-plan-entries plan))
        (plan-ids (mapcar #'car (supervisor-plan-entries plan)))
        (process-alive (supervisor-snapshot-process-alive snapshot))
        (failed-hash (supervisor-snapshot-failed snapshot))
        (oneshot-hash (supervisor-snapshot-oneshot-exit snapshot))
        (enabled-override (supervisor-snapshot-enabled-override snapshot))
        (mask-override (or (supervisor-snapshot-mask-override snapshot)
                           (make-hash-table :test 'equal)))
        (manually-started (or (supervisor-snapshot-manually-started snapshot)
                              (make-hash-table :test 'equal))))
    ;; Collect running IDs from snapshot
    (let ((running-ids nil))
      (maphash (lambda (id _) (push id running-ids)) process-alive)
      ;; Check for processes to stop (running but not in plan, or now disabled/masked)
      (dolist (id running-ids)
        (let ((in-plan (member id plan-ids)))
          (if (not in-plan)
              ;; Removed from config
              (push (list :op 'stop :id id :reason 'removed) actions)
            ;; Check if now disabled or masked
            (let* ((entry (cl-find id plan-entries :key #'car :test #'equal))
                   (enabled-p (nth 3 entry))
                   (is-masked (eq (gethash id mask-override) 'masked))
                   (override (gethash id enabled-override))
                   (effective-enabled (cond (is-masked nil)
                                            ((eq override 'enabled) t)
                                            ((eq override 'disabled) nil)
                                            (t enabled-p))))
              (cond
               ;; Masked: always stop (mask overrides manual start)
               (is-masked
                (push (list :op 'stop :id id :reason 'masked) actions))
               ;; Disabled but manually started: preserve (systemctl model)
               ((and (not effective-enabled)
                     (gethash id manually-started))
                (push (list :op 'noop :id id :reason 'manually-started)
                      actions))
               ;; Disabled (not manually started): stop
               ((not effective-enabled)
                (push (list :op 'stop :id id :reason 'disabled) actions))
               ;; Enabled and running: no-op
               (t
                (push (list :op 'noop :id id :reason 'already-running)
                      actions))))))))
    ;; Check for processes to start (in plan but not running)
    (dolist (entry plan-entries)
      (let* ((id (car entry))
             (enabled-p (nth 3 entry))
             (type (nth 6 entry))
             (is-masked (eq (gethash id mask-override) 'masked))
             (override (gethash id enabled-override))
             (effective-enabled (cond (is-masked nil)
                                      ((eq override 'enabled) t)
                                      ((eq override 'disabled) nil)
                                      (t enabled-p)))
             (is-running (gethash id process-alive))
             (is-failed (gethash id failed-hash))
             (oneshot-done (gethash id oneshot-hash)))
        (cond
         (is-running
          ;; Already handled in running-ids loop
          nil)
         ((not effective-enabled)
          (push (list :op 'skip :id id
                      :reason (if is-masked 'masked 'disabled))
                actions))
         (is-failed
          (push (list :op 'skip :id id :reason 'failed) actions))
         ((and (eq type 'oneshot) oneshot-done)
          (push (list :op 'skip :id id :reason 'oneshot-completed) actions))
         (t
          (push (list :op 'start :id id :reason 'new-entry) actions)))))
    ;; Return actions in stable order (by ID)
    (sort (nreverse actions)
          (lambda (a b) (string< (plist-get a :id) (plist-get b :id))))))

(defun supervisor--apply-actions (actions plan)
  "Apply reconciliation ACTIONS using entry data from PLAN.
Returns a plist with :stopped and :started counts."
  (let ((stopped 0)
        (started 0)
        (plan-entries (supervisor-plan-entries plan)))
    (dolist (action actions)
      (let ((op (plist-get action :op))
            (id (plist-get action :id))
            (reason (plist-get action :reason)))
        (pcase op
          ('stop
           (when-let* ((proc (gethash id supervisor--processes)))
             (when (process-live-p proc)
               (supervisor--log 'info "reconcile: stopping %s entry %s" reason id)
               ;; Mark as manually stopped so sentinel doesn't restart it
               (puthash id t supervisor--manually-stopped)
               (kill-process proc)
               (cl-incf stopped))))
          ('start
           (when-let* ((entry (cl-find id plan-entries :key #'car :test #'equal)))
             (pcase-let ((`(,_id ,cmd ,_delay ,enabled-p ,restart-p ,logging-p
                                 ,type ,_stage ,_after ,_owait ,_otimeout ,_tags)
                          entry))
               (when (supervisor--get-effective-enabled id enabled-p)
                 (let ((args (split-string-and-unquote cmd)))
                   (if (not (executable-find (car args)))
                       (progn
                         (supervisor--log 'warning "reconcile: executable not found for %s: %s"
                                          id (car args))
                         (supervisor--emit-event 'process-failed id nil nil))
                     (supervisor--log 'info "reconcile: starting %s entry %s" reason id)
                     (let ((proc (supervisor--start-process id cmd logging-p type restart-p)))
                       (if proc
                           (progn
                             (supervisor--emit-event 'process-started id nil (list :type type))
                             ;; Simple processes are immediately ready; oneshots ready on exit
                             (when (eq type 'simple)
                               (supervisor--emit-event 'process-ready id nil (list :type type)))
                             (cl-incf started))
                         (supervisor--emit-event 'process-failed id nil nil)))))))))
          ;; noop and skip actions require no work
          (_ nil))))
    (list :stopped stopped :started started)))

(defun supervisor--reconcile ()
  "Reconcile configuration and running processes.
Internal function called by the config-watcher and CLI reconcile command.
Uses declarative reconciliation: build plan, build snapshot, compute
actions, then apply.  The action list can be inspected before apply
by calling `supervisor--compute-actions' directly.

Stops processes removed from config or now disabled, starts new processes.
Does not restart changed entries - use dashboard kill/start for that."
  ;; Build plan and snapshot
  (let* ((plan (supervisor--build-plan (supervisor--effective-programs)))
         (snapshot (supervisor--build-snapshot))
         ;; Compute actions (pure)
         (actions (supervisor--compute-actions plan snapshot))
         ;; Apply actions
         (result (supervisor--apply-actions actions plan))
         (stopped (plist-get result :stopped))
         (started (plist-get result :started)))
    ;; Populate legacy globals from plan for dashboard
    (clrhash supervisor--invalid)
    (when (boundp 'supervisor--invalid-timers)
      (clrhash supervisor--invalid-timers))
    (maphash (lambda (k v) (puthash k v supervisor--invalid))
             (supervisor-plan-invalid plan))
    (supervisor--merge-unit-file-invalid)
    ;; Validate timers only when timer subsystem is active (and timer module loaded)
    (when (and (fboundp 'supervisor-timer-subsystem-active-p)
               (supervisor-timer-subsystem-active-p)
               (fboundp 'supervisor-timer-build-list))
      (supervisor-timer-build-list plan))
    (supervisor--maybe-refresh-dashboard)
    (message "Supervisor reconcile: stopped %d, started %d" stopped started)))

(defun supervisor--reload-find-entry (id)
  "Find and parse entry for ID by re-reading from disk.
Unlike `supervisor--get-entry-for-id', this reads from
`supervisor--effective-programs' (including unit files) and does
not skip entries present in `supervisor--invalid'.  This allows
reload to pick up both unit-file-only entries and entries whose
config has been fixed since the last plan build.
Return the parsed entry, the symbol `parse-error' if the entry
exists but cannot be parsed, or nil if ID is not found."

  (let ((idx 0))
    (cl-loop for entry in (supervisor--effective-programs)
             for entry-id = (supervisor--extract-id entry idx)
             do (cl-incf idx)
             when (string= entry-id id)
             return (condition-case nil
                        (supervisor--parse-entry entry)
                      (error 'parse-error)))))

(defun supervisor--reload-unit (id)
  "Reload a single unit ID from current config.
Re-reads the unit definition from unit files by calling
`supervisor--reload-find-entry', then applies the change:

- Running simple process: stop gracefully, start with new definition.
  Bypasses enabled/disabled policy so a running unit is never left
  stopped by reload.
- Running oneshot: update definition only (let it finish naturally).
- Not running: update stored definition only (next start uses new config).
- Masked: skip with warning.
- Unknown ID: error.

Returns a plist (:id ID :action ACTION) where ACTION is one of:
  \"reloaded\"        - running simple unit restarted with new config
  \"updated\"         - definition refreshed (stopped or running oneshot)
  \"skipped (masked)\" - unit is masked, no action taken
  \"error: REASON\"   - not found, invalid config, or start failed"
  (cond
   ;; Masked: skip
   ((eq (gethash id supervisor--mask-override) 'masked)
    (list :id id :action "skipped (masked)"))
   ;; Look up entry fresh from effective programs (disk)
   (t
    (let ((entry (supervisor--reload-find-entry id)))
      (cond
       ((null entry)
        (list :id id :action "error: not found"))
       ((eq entry 'parse-error)
        (list :id id :action "error: invalid config"))
       (t
        ;; Entry found and parseable - clear stale invalid state
        (remhash id supervisor--invalid)
        (let* ((proc (gethash id supervisor--processes))
               (running (and proc (process-live-p proc)))
               (type (nth 6 entry)))
          (cond
           ;; Running simple: stop + start with new definition.
           ;; Bypass supervisor--manual-start to avoid disabled-policy
           ;; refusal; reload's contract is config hot-swap, not
           ;; enable/disable enforcement.
           ((and running (eq type 'simple))
            (supervisor--manual-stop id)
            (remhash id supervisor--failed)
            (remhash id supervisor--restart-times)
            (remhash id supervisor--manually-stopped)
            (let* ((cmd (nth 1 entry))
                   (logging-p (nth 5 entry))
                   (restart-p (nth 4 entry))
                   (exe (car (split-string-and-unquote cmd))))
              (if (not (executable-find exe))
                  (list :id id :action "error: executable not found")
                (let ((new-proc (supervisor--start-process
                                 id cmd logging-p type restart-p)))
                  (if new-proc
                      (list :id id :action "reloaded")
                    (list :id id
                          :action "error: failed to start process"))))))
           ;; Running oneshot: don't interrupt; let it finish naturally.
           ;; Next invocation uses the new definition.
           (running
            (list :id id :action "updated"))
           ;; Not running: update definition only.  Clear stale state.
           (t
            (remhash id supervisor--failed)
            (remhash id supervisor--restart-times)
            (remhash id supervisor--oneshot-completed)
            (list :id id :action "updated"))))))))))

;;;###autoload
(defun supervisor-daemon-reload ()
  "Reload unit definitions from disk into memory without affecting runtime.
Re-reads unit files from `supervisor-unit-directory', rebuilds the plan,
and stores the result in `supervisor--current-plan'.

Does NOT start, stop, or restart anything.  Runtime state is untouched.
After daemon-reload, the next `start' or `reload' operates on the
refreshed plan.

Returns a plist with :entries and :invalid counts."
  (interactive)

  (let ((plan (supervisor--build-plan (supervisor--effective-programs))))
    ;; Store plan for later use
    (setq supervisor--current-plan plan)
    ;; Populate legacy globals from plan for dashboard/CLI visibility
    (clrhash supervisor--invalid)
    (when (boundp 'supervisor--invalid-timers)
      (clrhash supervisor--invalid-timers))
    (maphash (lambda (k v) (puthash k v supervisor--invalid))
             (supervisor-plan-invalid plan))
    (supervisor--merge-unit-file-invalid)
    ;; Validate timers when subsystem is active
    (when (and (fboundp 'supervisor-timer-subsystem-active-p)
               (supervisor-timer-subsystem-active-p)
               (fboundp 'supervisor-timer-build-list))
      (supervisor-timer-build-list plan))
    (supervisor--maybe-refresh-dashboard)
    (let ((n-entries (length (supervisor-plan-entries plan)))
          (n-invalid (hash-table-count supervisor--invalid)))
      (message "Supervisor daemon-reload: %d entries, %d invalid"
               n-entries n-invalid)
      (list :entries n-entries :invalid n-invalid))))


;;; File Watch

(defun supervisor--config-watch-file ()
  "Return the file to watch for config modification."
  (cond
   ((stringp supervisor-watch-config) supervisor-watch-config)
   ((and supervisor-watch-config user-init-file) user-init-file)
   (t nil)))

(defun supervisor--config-watch-callback (_event)
  "Handle config file modification.  Trigger reconcile after debounce."
  ;; Simple debounce: cancel pending reconcile, schedule new one
  (when (timerp (get 'supervisor--config-watch-callback 'timer))
    (cancel-timer (get 'supervisor--config-watch-callback 'timer)))
  (put 'supervisor--config-watch-callback 'timer
       (run-at-time 1 nil
                    (lambda ()
                      (supervisor--log 'info "config file changed, reconciling...")
                      (supervisor--reconcile)))))

(defun supervisor--start-config-watch ()
  "Start watching config file if configured."
  (when-let* ((file (supervisor--config-watch-file)))
    (when (file-exists-p file)
      (require 'filenotify)
      (setq supervisor--config-watch-descriptor
            (file-notify-add-watch file '(change)
                                   #'supervisor--config-watch-callback))
      (supervisor--log 'info "watching %s for changes" file))))

(defun supervisor--stop-config-watch ()
  "Stop watching config file and cancel any pending debounce timer."
  ;; Cancel debounce timer if pending
  (when-let* ((timer (get 'supervisor--config-watch-callback 'timer)))
    (when (timerp timer)
      (cancel-timer timer))
    (put 'supervisor--config-watch-callback 'timer nil))
  ;; Remove file watch
  (when supervisor--config-watch-descriptor
    (require 'filenotify)
    (file-notify-rm-watch supervisor--config-watch-descriptor)
    (setq supervisor--config-watch-descriptor nil)))

;;; Global Minor Mode

;;;###autoload
(define-minor-mode supervisor-mode
  "Global minor mode for process supervision.

When enabled, starts all configured processes via `supervisor-start'.
When disabled, stops all supervised processes via `supervisor-stop'.

This mode is intended for use in your init file to manage session
processes, particularly when using Emacs as a window manager (EXWM).

Services are defined as unit files in `supervisor-unit-directory'
\\=(default: ~/.config/supervisor/units/).  Each file contains a plist:

  ;; ~/.config/supervisor/units/nm-applet.el
  \\=(:id \"nm-applet\"
   :command \"nm-applet\"
   :type simple
   :stage stage3)

Enable in your init file:

  (require \\='supervisor)
  (supervisor-mode 1)

Or with `use-package':

  (use-package supervisor
    :config
    (supervisor-mode 1))

Process types:
- simple: Long-running daemons, restarted on crash if :restart t
- oneshot: Run-once scripts, exit is expected

Stages (run sequentially):
- stage1, stage2, stage3, stage4: run in order (stage3 is default)

Use `M-x supervisor' to open the dashboard for monitoring and control.
Use `M-x supervisor-validate' to check config without starting processes."
  :global t
  :group 'supervisor
  :lighter " Sup"
  (if supervisor-mode
      (progn
        (supervisor-start)
        (supervisor--start-config-watch))
    (supervisor--stop-config-watch)
    (supervisor-stop)))

(provide 'supervisor-core)

;;; supervisor-core.el ends here
