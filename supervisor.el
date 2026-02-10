;;; supervisor.el --- Emacs Lisp process supervisor -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>
;;
;; Author: telecommuter <telecommuter@riseup.net>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: processes, unix
;; URL: https://github.com/cypherpunk2001/supervisor.el

;; This file is not part of GNU Emacs.

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

;; Supervisor is a lightweight process supervisor for managing background
;; processes from Emacs.  It is useful for starting session services when
;; using Emacs as a window manager (EXWM) or for managing development daemons.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Forward declarations for optional features
(declare-function file-notify-add-watch "filenotify" (file flags callback))
(declare-function file-notify-rm-watch "filenotify" (descriptor))

(defgroup supervisor nil
  "Emacs Lisp process supervisor."
  :group 'processes)

(defcustom supervisor-programs nil
  "List of programs to supervise.
Each entry is either a command string or (COMMAND . PLIST).
See README.org for available keywords."
  :type '(repeat (choice string (list string &rest plist)))
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
When the init file is modified, automatically run `supervisor-reload'.
The file watched is the value of `user-init-file'.
Set to a file path string to watch a specific file instead."
  :type '(choice (const :tag "Disabled" nil)
                 (const :tag "Watch init file" t)
                 (file :tag "Watch specific file"))
  :group 'supervisor)

(defvar supervisor--config-watch-descriptor nil
  "File notification descriptor for config watching.")

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
  '((early . 0) (services . 1) (session . 2) (ui . 3))
  "Stage name to priority mapping.  Lower numbers run first.")

(defconst supervisor-stage-names
  '(early services session ui)
  "Ordered list of stage names.")

(defun supervisor--stage-to-int (stage)
  "Convert STAGE symbol to integer priority.  Default to session (2)."
  (cond
   ((not (symbolp stage))
    (supervisor--log 'warning ":stage must be a symbol, got %S" stage)
    2)
   ((not (assq stage supervisor-stages))
    (supervisor--log 'warning "unknown :stage '%s', using session" stage)
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
  '(:id :type :stage :delay :after :enabled :disabled
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
      ;; Return nil if valid, or joined error string
      (when errors
        (mapconcat #'identity (nreverse errors) "; "))))))

;; Forward declarations for supervisor-validate and supervisor-dry-run
(defvar supervisor--invalid)
(defvar supervisor--cycle-fallback-ids)
(defvar supervisor--computed-deps)

(defun supervisor--extract-id (entry idx)
  "Extract a stable ID from ENTRY at position IDX in `supervisor-programs'.
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
(defun supervisor-validate ()
  "Validate all entries in `supervisor-programs' without starting.
Display results in a temporary buffer and populate `supervisor--invalid'
so the dashboard reflects validation state."
  (interactive)
  (clrhash supervisor--invalid)
  (let ((valid 0)
        (invalid 0)
        (results nil)
        (seen (make-hash-table :test 'equal))
        (idx 0))
    (dolist (entry supervisor-programs)
      (let* ((id (supervisor--extract-id entry idx))
             (reason (supervisor--validate-entry entry)))
        (cl-incf idx)
        ;; Skip duplicates
        (unless (gethash id seen)
          (puthash id t seen)
          (if reason
              (progn
                (cl-incf invalid)
                (puthash id reason supervisor--invalid)
                (push (format "INVALID %s: %s" id reason) results))
            (cl-incf valid)
            (push (format "OK      %s" id) results)))))
    (with-output-to-temp-buffer "*supervisor-validate*"
      (princ (format "Validation: %d valid, %d invalid\n\n" valid invalid))
      (dolist (line (nreverse results))
        (princ line)
        (princ "\n")))
    ;; Refresh dashboard if open
    (supervisor--refresh-dashboard)))

;;;###autoload
(defun supervisor-dry-run ()
  "Validate entries and show startup order without starting processes.
Display staged startup order, including dependency resolution and
cycle fallback behavior."
  (interactive)
  (clrhash supervisor--invalid)
  (clrhash supervisor--cycle-fallback-ids)
  (clrhash supervisor--computed-deps)
  (let* ((all-entries (supervisor--all-parsed-entries))
         (all-ids (mapcar #'car all-entries))
         (stage-order '(early services session ui)))
    (with-output-to-temp-buffer "*supervisor-dry-run*"
      (princ "=== Supervisor Dry Run ===\n\n")
      (princ (format "Total entries: %d valid, %d invalid\n\n"
                     (length all-entries)
                     (hash-table-count supervisor--invalid)))
      ;; Show invalid entries first
      (when (> (hash-table-count supervisor--invalid) 0)
        (princ "--- Invalid Entries (skipped) ---\n")
        (maphash (lambda (id reason)
                   (princ (format "  %s: %s\n" id reason)))
                 supervisor--invalid)
        (princ "\n"))
      ;; Process each stage (same validation path as real startup)
      (dolist (stage stage-order)
        (let* ((stage-entries (cl-remove-if-not
                               (lambda (e) (eq (nth 7 e) stage))
                               all-entries))
               (stage-ids (mapcar #'car stage-entries))
               ;; Validate :after references (same as real startup)
               (validated (when stage-entries
                            (supervisor--validate-after stage-entries stage-ids all-ids)))
               (sorted (when validated
                         (supervisor--stable-topo-sort validated))))
          (when stage-entries
            (princ (format "--- Stage: %s (%d entries) ---\n"
                           stage (length stage-entries)))
            (let ((order 1))
              (dolist (entry sorted)
                (let* ((id (nth 0 entry))
                       (type (nth 6 entry))
                       (delay (nth 2 entry))
                       (enabled-p (nth 3 entry))
                       (deps (gethash id supervisor--computed-deps))
                       (cycle (gethash id supervisor--cycle-fallback-ids)))
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
            (princ "\n"))))
      (princ "=== End Dry Run ===\n"))))

;;; State Variables

(defvar supervisor--processes (make-hash-table :test 'equal)
  "Hash table mapping program names to their process objects.")

(defvar supervisor--restart-override (make-hash-table :test 'equal)
  "Hash table of restart overrides: nil=inherit config, `enabled', `disabled'.")

(defvar supervisor--enabled-override (make-hash-table :test 'equal)
  "Hash table of enabled overrides: nil=inherit config, `enabled', `disabled'.
Runtime overrides take effect on next start (manual or restart).")

(defvar supervisor--restart-times (make-hash-table :test 'equal)
  "Hash table mapping program names to list of recent restart timestamps.")

(defvar supervisor--failed (make-hash-table :test 'equal)
  "Hash table of program names that have crash-looped and are marked failed.")

(defvar supervisor--logging (make-hash-table :test 'equal)
  "Hash table tracking logging state per process (runtime override).")

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

(defvar supervisor-cleanup-hook nil
  "Hook run during cleanup, before killing processes.")

(defvar supervisor-stage-start-hook nil
  "Hook run when a stage begins.
Called with one argument: the stage name symbol (early, services, session, ui).")

(defvar supervisor-stage-complete-hook nil
  "Hook run when a stage completes.
Called with one argument: the stage name symbol.")

(defvar supervisor-process-exit-hook nil
  "Hook run when a supervised process exits.
Called with three arguments: ID (string), STATUS (symbol), and CODE (integer).
STATUS is one of:
  `exited'   - process exited normally (CODE is exit code, 0 = success)
  `signal'   - process killed by signal (CODE is signal number)
  `unknown'  - exit status could not be determined")

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

;;; Helpers

(defun supervisor--parse-entry (entry)
  "Parse ENTRY into a normalized list of entry properties.
Return a list: (id cmd delay enabled-p restart-p logging-p type
stage after oneshot-wait oneshot-timeout tags).  ENTRY can be a command
string or a list (COMMAND . PLIST)."
  (if (stringp entry)
      (let ((id (file-name-nondirectory (car (split-string-and-unquote entry)))))
        (list id entry 0 t t t 'simple 'session nil
              supervisor-oneshot-default-wait supervisor-oneshot-timeout nil))
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
           ;; :stage (default session)
           (stage-raw (plist-get plist :stage))
           (stage (if stage-raw
                      (let ((s (supervisor--stage-to-int stage-raw)))
                        (supervisor--int-to-stage s))
                    'session))
           ;; :after - dependencies (same stage only)
           (after (supervisor--normalize-after (plist-get plist :after)))
           ;; Oneshot wait/timeout settings
           (oneshot-wait (supervisor--oneshot-wait-p plist))
           (oneshot-timeout (supervisor--oneshot-timeout-value plist))
           ;; Tags for filtering (list of symbols or strings)
           (tags-raw (plist-get plist :tags))
           (tags (cond ((null tags-raw) nil)
                       ((listp tags-raw) tags-raw)
                       (t (list tags-raw)))))
      (list id cmd delay enabled restart logging type stage after oneshot-wait oneshot-timeout tags))))

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
Duplicate IDs are skipped with a warning."
  (let ((seen (make-hash-table :test 'equal))
        (idx 0)
        result)
    (dolist (entry supervisor-programs)
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
    (dolist (entry entries)
      (let ((id (car entry))
            (after (nth 8 entry))
            (valid-deps nil))
        (dolist (dep after)
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
        (supervisor--log 'warning "cycle detected in :after dependencies, using list order")
        ;; Mark all entries in this stage as having cycle fallback and clear
        ;; computed deps to reflect post-fallback state (no edges)
        (dolist (entry entries)
          (let ((id (car entry)))
            (puthash id t supervisor--cycle-fallback-ids)
            (puthash id nil supervisor--computed-deps)))
        ;; Return entries with :after stripped to break the cycle
        (mapcar (lambda (entry)
                  (append (cl-subseq entry 0 8) (list nil) (cl-subseq entry 9)))
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
      (let* ((id (nth 0 entry))
             (enabled-p (nth 3 entry))
             (type (nth 6 entry))
             (after (nth 8 entry))
             (oneshot-wait (nth 9 entry)))
        (puthash id entry supervisor--dag-entries)
        (puthash id (length after) supervisor--dag-in-degree)
        (puthash id nil supervisor--dag-dependents)
        (puthash id idx supervisor--dag-id-to-index)
        (cl-incf idx)
        ;; Track blocking oneshots (only if enabled)
        (when (and enabled-p (eq type 'oneshot) oneshot-wait)
          (puthash id t supervisor--dag-blocking))
        ;; Set initial state
        (cond
         ((not enabled-p)
          (push id disabled-ids)
          (puthash id 'disabled supervisor--entry-state))
         ((> (length after) 0)
          (puthash id 'waiting-on-deps supervisor--entry-state))
         (t
          (puthash id 'stage-not-started supervisor--entry-state)))))
    ;; Build dependents graph
    (dolist (entry entries)
      (let ((id (nth 0 entry))
            (after (nth 8 entry)))
        (dolist (dep after)
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
    ;; Log that this entry is now ready
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
        (puthash id 'disabled supervisor--entry-state)
        (puthash id t supervisor--dag-started)
        (supervisor--dag-mark-ready id))
       ;; Delay: schedule start after delay (don't mark started yet)
       ((> delay 0)
        (supervisor--log 'info "scheduling %s after %ds delay..." id delay)
        (puthash id 'delayed supervisor--entry-state)
        (puthash id
                 (run-at-time delay nil
                              (lambda ()
                                (remhash id supervisor--dag-delay-timers)
                                (supervisor--dag-do-start id cmd logging-p type restart-p oneshot-wait oneshot-timeout)))
                 supervisor--dag-delay-timers))
       ;; Start immediately
       (t
        (supervisor--dag-do-start id cmd logging-p type restart-p oneshot-wait oneshot-timeout))))))

(defun supervisor--dag-do-start (id cmd logging-p type restart-p _oneshot-wait oneshot-timeout)
  "Start process ID with CMD, LOGGING-P, TYPE, RESTART-P, and ONESHOT-TIMEOUT."
  ;; Mark as started now (process is spawning)
  (puthash id t supervisor--dag-started)
  ;; Record start time for blame view
  (puthash id (float-time) supervisor--start-times)
  ;; Increment active count (for max-concurrent-starts)
  (cl-incf supervisor--dag-active-starts)
  (let ((args (split-string-and-unquote cmd)))
    (if (not (executable-find (car args)))
        (progn
          (supervisor--log 'warning "executable not found for %s: %s" id (car args))
          (puthash id 'failed-to-spawn supervisor--entry-state)
          ;; Decrement count (spawn attempt done) and process queue
          (cl-decf supervisor--dag-active-starts)
          (supervisor--dag-process-pending-starts)
          ;; Mark ready anyway so dependents can proceed
          (supervisor--dag-mark-ready id))
      ;; Start the process
      (let ((proc (supervisor--start-process id cmd logging-p type restart-p)))
        ;; Decrement count (spawn attempt done) and process queue
        (cl-decf supervisor--dag-active-starts)
        (supervisor--dag-process-pending-starts)
        (if (not proc)
            ;; Failed to start, mark ready so stage can proceed
            (progn
              (puthash id 'failed-to-spawn supervisor--entry-state)
              (supervisor--dag-mark-ready id))
          ;; Started successfully
          (puthash id 'started supervisor--entry-state)
          (if (eq type 'oneshot)
              (progn
                (supervisor--log 'info "started oneshot %s" id)
                ;; Set up timeout timer if needed
                (when oneshot-timeout
                  (puthash id
                           (run-at-time oneshot-timeout nil
                                        (lambda ()
                                          (supervisor--log 'warning "oneshot %s timed out after %ds" id oneshot-timeout)
                                          (supervisor--dag-mark-ready id)))
                           supervisor--dag-timeout-timers)))
            ;; Simple process: mark ready immediately (spawned = ready)
            (supervisor--log 'info "started %s" id)
            (supervisor--dag-mark-ready id)))))))

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
Return t if enabled, nil if disabled.  Runtime overrides take
effect on next start (manual or restart)."
  (let ((override (gethash id supervisor--enabled-override)))
    (cond ((eq override 'enabled) t)
          ((eq override 'disabled) nil)
          (t config-enabled))))

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
    (supervisor--ensure-log-directory)
    (let* ((args (split-string-and-unquote cmd))
           (logging (supervisor--get-effective-logging id default-logging))
           (log-file (when logging (supervisor--log-file id)))
           ;; Create stderr process to capture stderr to same log file
           (stderr-proc (when logging
                          (make-pipe-process
                           :name (format "%s-stderr" id)
                           :filter (lambda (_proc output)
                                     (write-region output nil log-file t 'silent)))))
           (proc (make-process
                  :name id
                  :command args
                  :connection-type 'pipe
                  :stderr stderr-proc
                  :filter (when logging
                            (lambda (_proc output)
                              (write-region output nil log-file t 'silent)))
                  :sentinel (lambda (p _event)
                              (unless (process-live-p p)
                                (let* ((name (process-name p))
                                       (proc-status (process-status p))
                                       (exit-code (process-exit-status p))
                                       ;; For hook: 'signal, 'exited, or 'unknown
                                       (exit-status (pcase proc-status
                                                      ('signal 'signal)
                                                      ('exit 'exited)
                                                      (_ 'unknown))))
                                  ;; Clean up stderr process
                                  (when-let* ((stderr (get-process (format "%s-stderr" name))))
                                    (delete-process stderr))
                                  (remhash name supervisor--processes)
                                  ;; Run process exit hook
                                  (run-hook-with-args 'supervisor-process-exit-hook
                                                      name exit-status exit-code)
                                  ;; Track oneshot completion with exit code
                                  (when (eq type 'oneshot)
                                    (puthash name exit-code supervisor--oneshot-completed)
                                    (if (eq proc-status 'signal)
                                        (supervisor--log 'warning "oneshot %s %s"
                                                         name (supervisor--format-exit-status proc-status exit-code))
                                      (if (> exit-code 0)
                                          (supervisor--log 'warning "oneshot %s %s"
                                                           name (supervisor--format-exit-status proc-status exit-code))
                                        (supervisor--log 'info "oneshot %s completed" name)))
                                    ;; Invoke any registered wait callback (event-driven)
                                    ;; Pass nil if shutting down (process was killed), t otherwise
                                    (when-let* ((cb-entry (gethash name supervisor--oneshot-callbacks)))
                                      (remhash name supervisor--oneshot-callbacks)
                                      (when (cdr cb-entry)  ; cancel timeout timer
                                        (cancel-timer (cdr cb-entry)))
                                      (funcall (car cb-entry) (not supervisor--shutting-down)))
                                    ;; Notify DAG scheduler
                                    (supervisor--dag-mark-ready name))
                                  (supervisor--refresh-dashboard)
                                  ;; Track shutdown completion (event-driven)
                                  (when supervisor--shutting-down
                                    (cl-decf supervisor--shutdown-remaining)
                                    (when (and (<= supervisor--shutdown-remaining 0)
                                               supervisor--shutdown-callback)
                                      ;; Cancel SIGKILL timeout timer (early completion)
                                      (when supervisor--shutdown-timer
                                        (cancel-timer supervisor--shutdown-timer)
                                        (setq supervisor--shutdown-timer nil))
                                      (let ((cb supervisor--shutdown-callback))
                                        (setq supervisor--shutdown-callback nil)
                                        (clrhash supervisor--processes)
                                        (setq supervisor--shutdown-complete-flag t)
                                        (funcall cb))))
                                  ;; Oneshot processes don't restart - exit is expected
                                  ;; Also skip if runtime-disabled (check early to avoid noise)
                                  (unless (or (eq type 'oneshot)
                                              supervisor--shutting-down
                                              (eq (gethash name supervisor--enabled-override) 'disabled)
                                              (not (supervisor--get-effective-restart name config-restart))
                                              (gethash name supervisor--failed)
                                              (supervisor--check-crash-loop name))
                                    (supervisor--log 'info "%s %s, restarting..."
                                                     name (supervisor--format-exit-status proc-status exit-code))
                                    ;; Track restart timer so it can be canceled
                                    (puthash name
                                             (run-at-time supervisor-restart-delay nil
                                                          #'supervisor--start-process id cmd default-logging type config-restart t)
                                             supervisor--restart-timers))))))))
      (set-process-query-on-exit-flag proc nil)
      (when stderr-proc
        (set-process-query-on-exit-flag stderr-proc nil))
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
CALLBACK is called with t on success, nil on error."
  (pcase-let ((`(,id ,cmd ,_delay ,enabled-p ,restart-p ,logging-p ,type ,_stage ,_after ,_owait ,otimeout ,_tags) entry))
    (if (not (supervisor--get-effective-enabled id enabled-p))
        (progn
          (supervisor--log 'info "%s disabled (config or override), skipping" id)
          (funcall callback t))
      (let ((args (split-string-and-unquote cmd)))
        (if (not (executable-find (car args)))
            (progn
              (supervisor--log 'warning "executable not found for %s: %s" id (car args))
              (funcall callback nil))
          ;; Start the process
          (let ((proc (supervisor--start-process id cmd logging-p type restart-p)))
            (if (not proc)
                (funcall callback nil)
              (if (eq type 'oneshot)
                  ;; Wait for oneshot via timer (no polling loop)
                  (progn
                    (supervisor--log 'info "waiting for oneshot %s..." id)
                    (supervisor--wait-for-oneshot
                     id otimeout
                     (lambda (completed)
                       (if completed
                           (supervisor--log 'info "oneshot %s completed" id)
                         (supervisor--log 'warning "oneshot %s timed out" id))
                       (funcall callback completed))))
                ;; Simple process: spawned immediately
                (supervisor--log 'info "started %s" id)
                (funcall callback t)))))))))

(defun supervisor--dag-force-stage-complete ()
  "Force stage completion due to timeout.
Mark all unstarted entries as timed out and invoke the callback."
  (supervisor--log 'warning "stage %s timed out, forcing completion"
                   supervisor--current-stage)
  ;; Mark all unstarted entries
  (maphash (lambda (id _entry)
             (unless (gethash id supervisor--dag-started)
               (puthash id t supervisor--dag-started)
               (puthash id 'stage-timeout supervisor--entry-state)))
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
  (run-hook-with-args 'supervisor-stage-start-hook stage-name)
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
          (supervisor--dag-try-start-entry id))))))

;;;###autoload
(defun supervisor-start ()
  "Start all programs in `supervisor-programs' by stage.
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
  ;; Reset runtime state for clean session
  (clrhash supervisor--restart-override)
  (clrhash supervisor--enabled-override)
  (clrhash supervisor--failed)
  (clrhash supervisor--invalid)
  (clrhash supervisor--logging)
  (clrhash supervisor--restart-times)
  (clrhash supervisor--restart-timers)
  (clrhash supervisor--oneshot-completed)
  (clrhash supervisor--start-times)
  (clrhash supervisor--ready-times)
  (clrhash supervisor--cycle-fallback-ids)
  (clrhash supervisor--computed-deps)
  (clrhash supervisor--entry-state)
  (setq supervisor--current-stage nil)
  (setq supervisor--completed-stages nil)
  (supervisor--dag-cleanup)
  (supervisor--rotate-logs)
  ;; Parse and deduplicate entries
  (let* ((all-entries (supervisor--all-parsed-entries))
         (all-ids (mapcar #'car all-entries))
         (by-stage (supervisor--partition-by-stage all-entries)))
    ;; Initialize all entry states to stage-not-started
    (dolist (entry all-entries)
      (puthash (car entry) 'stage-not-started supervisor--entry-state))
    ;; Process stages asynchronously with continuation
    (supervisor--start-stages-async by-stage all-ids)))

(defun supervisor--start-stages-async (remaining-stages all-ids)
  "Process REMAINING-STAGES asynchronously.
ALL-IDS is the list of all valid entry IDs for cross-stage validation."
  (if (or (null remaining-stages) supervisor--shutting-down)
      (progn
        (supervisor--dag-cleanup)
        (setq supervisor--current-stage nil)
        (supervisor--log 'info "startup complete"))
    (let* ((stage-pair (car remaining-stages))
           (rest (cdr remaining-stages))
           (stage-int (car stage-pair))
           (stage-name (supervisor--int-to-stage stage-int))
           (entries (cdr stage-pair))
           (stage-ids (mapcar #'car entries)))
      ;; Validate :after references
      (setq entries (supervisor--validate-after entries stage-ids all-ids))
      ;; Note: we don't need topo sort for the DAG scheduler,
      ;; it handles ordering via in-degree. But we use it for stable ordering.
      (setq entries (supervisor--stable-topo-sort entries))
      ;; Start this stage, with callback to process next stage
      (supervisor--start-stage-async
       stage-name entries
       (lambda ()
         (run-hook-with-args 'supervisor-stage-complete-hook stage-name)
         (push stage-name supervisor--completed-stages)
         (supervisor--dag-cleanup)
         (supervisor--start-stages-async rest all-ids))))))

;;;###autoload
(defun supervisor-stop (&optional callback)
  "Stop all supervised processes gracefully (async).
Sends SIGTERM immediately and returns.  A timer handles the graceful
shutdown period: after `supervisor-shutdown-timeout' seconds, any
survivors receive SIGKILL.  Optional CALLBACK is called with no
arguments when shutdown completes.  Check `supervisor--shutdown-complete-flag'
to poll completion status if needed."
  (interactive)
  (setq supervisor--shutting-down t)
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
  (run-hooks 'supervisor-cleanup-hook)
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

;;;###autoload
(defun supervisor-reload ()
  "Reload configuration and reconcile running processes.
Stops processes removed from config or now disabled, starts new processes.
Does not restart changed entries - use dashboard kill/start for that."
  (interactive)
  (let* ((new-entries (supervisor--all-parsed-entries))
         (new-ids (mapcar #'car new-entries))
         (running-ids (hash-table-keys supervisor--processes))
         ;; IDs to stop: running but not in new config
         (removed-ids (cl-remove-if (lambda (id) (member id new-ids)) running-ids))
         ;; IDs to stop: running but now disabled in config
         (disabled-ids
          (cl-remove-if-not
           (lambda (id)
             (let ((entry (cl-find id new-entries :key #'car :test #'equal)))
               (when entry
                 (pcase-let ((`(,_id ,_cmd ,_delay ,enabled-p ,_restart-p
                                     ,_logging-p ,_type ,_stage ,_after
                                     ,_owait ,_otimeout ,_tags) entry))
                   (not (supervisor--get-effective-enabled id enabled-p))))))
           running-ids))
         (to-stop (cl-union removed-ids disabled-ids :test #'equal))
         ;; IDs to start: in new config but not running (and not failed/completed oneshot)
         (to-start (cl-remove-if
                    (lambda (id)
                      (or (gethash id supervisor--processes)
                          (gethash id supervisor--failed)
                          (gethash id supervisor--oneshot-completed)))
                    new-ids))
         (stopped 0)
         (started 0))
    ;; Stop removed/disabled processes
    (dolist (id to-stop)
      (when-let* ((proc (gethash id supervisor--processes)))
        (when (process-live-p proc)
          (supervisor--log 'info "reload: stopping %s entry %s"
                           (if (member id removed-ids) "removed" "disabled") id)
          (puthash id 'disabled supervisor--restart-override)  ; prevent restart
          (kill-process proc)
          (cl-incf stopped))))
    ;; Start new processes (respect runtime override)
    (dolist (id to-start)
      (let ((entry (cl-find id new-entries :key #'car :test #'equal)))
        (when entry
          (pcase-let ((`(,_id ,cmd ,_delay ,enabled-p ,restart-p ,logging-p ,type ,_stage ,_after ,_owait ,_otimeout ,_tags) entry))
            (when (supervisor--get-effective-enabled id enabled-p)
              (let ((args (split-string-and-unquote cmd)))
                (if (not (executable-find (car args)))
                    (supervisor--log 'warning "reload: executable not found for %s: %s" id (car args))
                  (supervisor--log 'info "reload: starting new entry %s" id)
                  (supervisor--start-process id cmd logging-p type restart-p)
                  (cl-incf started))))))))
    (supervisor--refresh-dashboard)
    (message "Supervisor reload: stopped %d, started %d" stopped started)))


;;; Dashboard

(defvar-local supervisor--dashboard-stage-filter nil
  "Current stage filter for dashboard.
nil means show all stages, otherwise a stage symbol.")

(defvar-local supervisor--dashboard-tag-filter nil
  "Current tag filter for dashboard.
nil means show all entries, otherwise a tag symbol or string.")

(defvar supervisor-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "e" #'supervisor-dashboard-toggle-enabled)
    (define-key map "f" #'supervisor-dashboard-cycle-filter)
    (define-key map "t" #'supervisor-dashboard-cycle-tag-filter)
    (define-key map "r" #'supervisor-dashboard-toggle-restart)
    (define-key map "k" #'supervisor-dashboard-kill)
    (define-key map "s" #'supervisor-dashboard-start)
    (define-key map "l" #'supervisor-dashboard-toggle-logging)
    (define-key map "L" #'supervisor-dashboard-view-log)
    (define-key map "p" #'proced)
    (define-key map "P" #'supervisor-dashboard-toggle-proced-auto-update)
    (define-key map "g" #'supervisor-dashboard-refresh)
    (define-key map "?" #'supervisor-dashboard-describe-entry)
    (define-key map "d" #'supervisor-dashboard-show-deps)
    (define-key map "D" #'supervisor-dashboard-show-graph)
    (define-key map "B" #'supervisor-dashboard-blame)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `supervisor-dashboard-mode'.")

(define-derived-mode supervisor-dashboard-mode tabulated-list-mode "Supervisor"
  "Major mode for the supervisor dashboard."
  (setq tabulated-list-format [("ID" 15 t)
                               ("Type" 7 t)
                               ("Stage" 8 t)
                               ("Enabled" 7 t)
                               ("Status" 8 t)
                               ("Restart" 7 t)
                               ("Log" 3 t)
                               ("PID" 6 t)
                               ("Reason" 30 t)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

(defun supervisor--get-entry-for-id (id)
  "Get the parsed entry for ID.
Return a list of entry properties or nil if not found.
Skips invalid/malformed entries to avoid parse errors."
  (let ((idx 0))
    (cl-loop for entry in supervisor-programs
             for entry-id = (supervisor--extract-id entry idx)
             do (cl-incf idx)
             ;; Skip invalid entries - don't try to parse them
             unless (gethash entry-id supervisor--invalid)
             ;; Only parse valid entries
             when (string= entry-id id)
             return (supervisor--parse-entry entry))))

(defun supervisor--get-entries ()
  "Generate entries for the dashboard (deduplicates on the fly).
Respects `supervisor--dashboard-stage-filter' and tag filter when set."
  (let ((entries nil)
        (seen (make-hash-table :test 'equal))
        (stage-filter supervisor--dashboard-stage-filter)
        (tag-filter supervisor--dashboard-tag-filter)
        (idx 0))
    (dolist (entry supervisor-programs)
      ;; Extract ID even for potentially invalid entries
      ;; Use idx-based fallback for truly malformed entries to avoid collisions
      (let* ((raw-id (supervisor--extract-id entry idx))
             (invalid-reason (gethash raw-id supervisor--invalid)))
        (cl-incf idx)
        ;; Skip duplicates
        (unless (gethash raw-id seen)
          (puthash raw-id t seen)
          (if invalid-reason
              ;; Invalid entry - show with reason (only if no filter or cannot determine stage)
              (unless stage-filter
                (push (list raw-id
                            (vector raw-id "-" "-" "-" "invalid" "-" "-" "-"
                                    invalid-reason))
                      entries))
            ;; Valid entry - parse and show full info
            (pcase-let ((`(,id ,_cmd ,_delay ,enabled-p ,restart-p ,logging-p
                               ,type ,stage ,_after ,_owait ,_otimeout ,tags)
                         (supervisor--parse-entry entry)))
              ;; Apply stage and tag filters
              (when (and (or (null stage-filter) (eq stage stage-filter))
                         (or (null tag-filter) (member tag-filter tags)))
              (let* ((proc (gethash id supervisor--processes))
                     (alive (and proc (process-live-p proc)))
                     (failed (gethash id supervisor--failed))
                     (pid (if alive (number-to-string (process-id proc)) "-"))
                     (oneshot-p (eq type 'oneshot))
                     (oneshot-exit (gethash id supervisor--oneshot-completed))
                     (oneshot-done (not (null oneshot-exit)))
                     (oneshot-failed (and oneshot-done (> oneshot-exit 0)))
                     (status (cond (alive "running")
                                   (failed "dead")
                                   ((and oneshot-p oneshot-failed) "failed")
                                   ((and oneshot-p oneshot-done) "done")
                                   (oneshot-p "pending")
                                   (t "stopped")))
                     ;; Enabled column - check for runtime overrides
                     (effective-enabled (supervisor--get-effective-enabled id enabled-p))
                     ;; Restart column - check for overrides (n/a for oneshot)
                     (restart-str (if oneshot-p
                                      "n/a"
                                    (let ((eff (supervisor--get-effective-restart
                                                id restart-p)))
                                      (if eff "yes" "no"))))
                     ;; Logging column - check for overrides
                     (log-override (gethash id supervisor--logging))
                     (effective-logging (cond ((eq log-override 'enabled) t)
                                              ((eq log-override 'disabled) nil)
                                              (t logging-p)))
                     ;; Reason column - detailed state for non-running entries
                     (entry-state (gethash id supervisor--entry-state))
                     (reason (cond
                              (alive "")
                              ((and oneshot-p oneshot-done) "")
                              ((eq entry-state 'disabled) "disabled")
                              ((eq entry-state 'delayed) "delayed")
                              ((eq entry-state 'waiting-on-deps) "waiting-on-deps")
                              ((eq entry-state 'stage-not-started) "stage-not-started")
                              ((eq entry-state 'failed-to-spawn) "failed-to-spawn")
                              ((eq entry-state 'stage-timeout) "stage-timeout")
                              (failed "crash-loop")
                              (t ""))))
                (push (list id
                            (vector id
                                    (symbol-name type)
                                    (symbol-name stage)
                                    (if effective-enabled "yes" "no")
                                    status
                                    restart-str
                                    (if effective-logging "yes" "no")
                                    pid
                                    reason))
                      entries))))))))
    (nreverse entries)))

(defun supervisor--stage-progress-banner ()
  "Return ASCII banner showing stage progress."
  (let ((all-stages '(early services session ui))
        (parts nil))
    (dolist (stage all-stages)
      (push (cond ((member stage supervisor--completed-stages)
                   (format "[%s *]" stage))
                  ((eq stage supervisor--current-stage)
                   (format "[%s >]" stage))
                  (t
                   (format "[%s  ]" stage)))
            parts))
    (mapconcat #'identity (nreverse parts) " ")))

(defvar supervisor--help-text
  "Keys: e:enable f:filter s:start k:kill r:restart l:log L:view ?:info d:deps D:graph B:blame g:refresh q:quit")

(defun supervisor--refresh-dashboard ()
  "Refresh the dashboard buffer if it exists."
  (when-let* ((buf (get-buffer "*supervisor*")))
    (with-current-buffer buf
      (let ((pos (point)))
        (setq tabulated-list-entries (supervisor--get-entries))
        (tabulated-list-print t)
        (setq header-line-format
              (concat (supervisor--stage-progress-banner)
                      "  " supervisor--help-text))
        (goto-char (min pos (point-max)))))))

(defun supervisor-dashboard-refresh ()
  "Refresh the dashboard."
  (interactive)
  (supervisor--refresh-dashboard))

(defun supervisor-dashboard-cycle-filter ()
  "Cycle dashboard stage filter: all -> early -> services -> session -> ui -> all."
  (interactive)
  (setq supervisor--dashboard-stage-filter
        (pcase supervisor--dashboard-stage-filter
          ('nil 'early)
          ('early 'services)
          ('services 'session)
          ('session 'ui)
          ('ui nil)))
  (message "Stage filter: %s"
           (if supervisor--dashboard-stage-filter
               (symbol-name supervisor--dashboard-stage-filter)
             "all"))
  (supervisor--refresh-dashboard))

(defun supervisor--all-tags ()
  "Return list of all unique tags used in entries."
  (let ((tags nil))
    (dolist (entry supervisor-programs)
      (unless (stringp entry)
        (let ((entry-tags (plist-get (cdr entry) :tags)))
          (dolist (tag (if (listp entry-tags) entry-tags (list entry-tags)))
            (when tag
              (cl-pushnew tag tags :test #'equal))))))
    (sort tags (lambda (a b)
                 (string< (format "%s" a) (format "%s" b))))))

(defun supervisor-dashboard-cycle-tag-filter ()
  "Cycle dashboard tag filter through all available tags."
  (interactive)
  (let* ((all-tags (supervisor--all-tags))
         (current supervisor--dashboard-tag-filter)
         (idx (cl-position current all-tags :test #'equal)))
    (setq supervisor--dashboard-tag-filter
          (cond
           ((null all-tags) nil)
           ((null idx) (car all-tags))
           ((= idx (1- (length all-tags))) nil)
           (t (nth (1+ idx) all-tags))))
    (message "Tag filter: %s"
             (if supervisor--dashboard-tag-filter
                 (format "%s" supervisor--dashboard-tag-filter)
               "all"))
    (supervisor--refresh-dashboard)))

(defun supervisor-dashboard-describe-entry ()
  "Show detailed information about entry at point."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (let ((invalid-reason (gethash id supervisor--invalid)))
      (if invalid-reason
          ;; Invalid entry - show the reason
          (message "INVALID: %s" invalid-reason)
        ;; Valid entry - show resolved config
        (let ((entry (supervisor--get-entry-for-id id)))
          (if entry
              (pcase-let ((`(,id ,_cmd ,delay ,enabled-p ,restart-p ,logging-p
                                 ,type ,stage ,after ,oneshot-wait ,oneshot-timeout ,_tags)
                           entry))
                (message "%s: type=%s stage=%s enabled=%s restart=%s log=%s delay=%s after=%s%s"
                         id type stage
                         (if enabled-p "yes" "no")
                         (if (eq type 'oneshot) "n/a" (if restart-p "yes" "no"))
                         (if logging-p "yes" "no")
                         delay
                         (or after "none")
                         (if (eq type 'oneshot)
                             (format " wait=%s timeout=%s"
                                     (if oneshot-wait "yes" "no")
                                     (or oneshot-timeout "none"))
                           "")))
            (message "Entry not found: %s" id)))))))

(defun supervisor-dashboard-toggle-restart ()
  "Toggle auto-restart for process at point (no-op for oneshot).
Cycles: config default -> override opposite -> back to config default."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (if (gethash id supervisor--invalid)
        (message "Cannot toggle restart for invalid entry: %s" id)
      (when-let* ((entry (supervisor--get-entry-for-id id)))
        (pcase-let ((`(,_id ,_cmd ,_delay ,_enabled-p ,restart-p ,_logging-p ,type ,_stage ,_after ,_owait ,_otimeout ,_tags) entry))
          ;; Oneshot processes don't have restart semantics
          (unless (eq type 'oneshot)
            (let* ((currently-enabled (supervisor--get-effective-restart id restart-p))
                   (new-enabled (not currently-enabled)))
              ;; If new state matches config, clear override; otherwise set override
              (if (eq new-enabled restart-p)
                  (remhash id supervisor--restart-override)
                (puthash id (if new-enabled 'enabled 'disabled) supervisor--restart-override))
              ;; Cancel pending restart timer when disabling
              (unless new-enabled
                (when-let* ((timer (gethash id supervisor--restart-timers)))
                  (when (timerp timer)
                    (cancel-timer timer))
                  (remhash id supervisor--restart-timers))))
            (supervisor--refresh-dashboard)))))))

(defun supervisor-dashboard-toggle-enabled ()
  "Toggle enabled state for entry at point.
Runtime override takes effect on next start (manual or automatic restart).
Cycles: config default -> override opposite -> back to config default."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (if (gethash id supervisor--invalid)
        (message "Cannot toggle enabled for invalid entry: %s" id)
      (when-let* ((entry (supervisor--get-entry-for-id id)))
        (pcase-let ((`(,_id ,_cmd ,_delay ,enabled-p ,_restart-p ,_logging-p ,_type ,_stage ,_after ,_owait ,_otimeout ,_tags) entry))
          (let* ((currently-enabled (supervisor--get-effective-enabled id enabled-p))
                 (new-enabled (not currently-enabled)))
            ;; If new state matches config, clear override; otherwise set override
            (if (eq new-enabled enabled-p)
                (remhash id supervisor--enabled-override)
              (puthash id (if new-enabled 'enabled 'disabled) supervisor--enabled-override))
            (supervisor--refresh-dashboard)))))))

(defun supervisor-dashboard-kill ()
  "Stop process at point, disable auto-restart, and send kill signal."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (proc (gethash id supervisor--processes)))
    (when (process-live-p proc)
      ;; Disable auto-restart first so sentinel doesn't restart it
      (puthash id 'disabled supervisor--restart-override)
      (kill-process proc)
      (supervisor--refresh-dashboard))))

(defun supervisor-dashboard-start ()
  "Start process at point if stopped.
Respects runtime enable/disable overrides."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (if (gethash id supervisor--invalid)
        (message "Cannot start invalid entry: %s" id)
      (when-let* ((entry (supervisor--get-entry-for-id id)))
        (unless (and (gethash id supervisor--processes)
                     (process-live-p (gethash id supervisor--processes)))
          (pcase-let ((`(,_id ,cmd ,_delay ,enabled-p ,restart-p ,logging-p ,type ,_stage ,_after ,_owait ,_otimeout ,_tags) entry))
            (if (not (supervisor--get-effective-enabled id enabled-p))
                (message "Entry %s is disabled (use 'e' to enable)" id)
              (let ((args (split-string-and-unquote cmd)))
                (if (not (executable-find (car args)))
                    (supervisor--log 'warning "executable not found: %s" (car args))
                  ;; Clear failed state and oneshot completion on manual start
                  (remhash id supervisor--failed)
                  (remhash id supervisor--restart-times)
                  (remhash id supervisor--oneshot-completed)
                  (supervisor--start-process id cmd logging-p type restart-p)
                  (supervisor--refresh-dashboard))))))))))

(defun supervisor-dashboard-toggle-logging ()
  "Toggle logging for process at point (takes effect on next start)."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (if (gethash id supervisor--invalid)
        (message "Cannot toggle logging for invalid entry: %s" id)
      (let* ((current (gethash id supervisor--logging))
             (entry (supervisor--get-entry-for-id id))
             (config-logging (if entry (nth 5 entry) t))  ; logging-p is index 5
             (effective (if current (eq current 'enabled) config-logging)))
        (puthash id (if effective 'disabled 'enabled) supervisor--logging))
      (supervisor--refresh-dashboard))))

(defun supervisor-dashboard-view-log ()
  "Open the log file for process at point."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (let ((log-file (supervisor--log-file id)))
      (if (file-exists-p log-file)
          (find-file log-file)
        (message "No log file for %s" id)))))

(defvar proced-auto-update-flag)  ; from proced.el
(declare-function proced-toggle-auto-update "proced" (&optional arg))

(defun supervisor-dashboard-toggle-proced-auto-update ()
  "Toggle proced auto-update in the Proced buffer, or globally if no buffer."
  (interactive)
  (require 'proced)
  (if-let* ((proced-buf (get-buffer "*Proced*")))
      (with-current-buffer proced-buf
        (proced-toggle-auto-update)  ; no arg to cycle
        (message "Proced auto-update: %s"
                 (pcase proced-auto-update-flag
                   ('nil "off") ('visible "visible") (_ "on"))))
    ;; No proced buffer, cycle global default: nil -> visible -> t -> nil
    (setq proced-auto-update-flag
          (pcase proced-auto-update-flag
            ('nil 'visible)
            ('visible t)
            (_ nil)))
    (message "Proced auto-update (global): %s"
             (pcase proced-auto-update-flag
               ('nil "off") ('visible "visible") (_ "on")))))

(defun supervisor-dashboard-show-deps ()
  "Show computed dependencies for entry at point.
Shows post-validation edges: after cycle fallback and stage filtering.
Run `supervisor-start' first to populate computed dependency data."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (if (gethash id supervisor--invalid)
        (message "Cannot show dependencies for invalid entry: %s" id)
      (let ((entry (supervisor--get-entry-for-id id)))
        (if entry
            (let* ((my-stage (nth 7 entry))
                   (cycle-fallback (gethash id supervisor--cycle-fallback-ids))
                   ;; Use computed deps if available, else show raw (pre-start)
                   (computed (gethash id supervisor--computed-deps))
                   (effective-deps (if cycle-fallback
                                       nil  ; cycle fallback clears all deps
                                     (or computed (nth 8 entry))))
                   ;; Find entries that depend on this one (computed)
                   (dependents nil))
              ;; Scan computed-deps for entries that list this ID
              (maphash (lambda (e-id e-deps)
                         (when (and (not (string= e-id id))
                                    (not (gethash e-id supervisor--cycle-fallback-ids))
                                    (member id e-deps))
                           (push e-id dependents)))
                       supervisor--computed-deps)
              (message "%s [%s]%s: depends-on=%s blocks=%s"
                       id my-stage
                       (if cycle-fallback " (cycle fallback)" "")
                       (if effective-deps (mapconcat #'identity effective-deps ", ") "none")
                       (if dependents (mapconcat #'identity dependents ", ") "none")))
          (message "Entry not found: %s" id))))))

(defun supervisor-dashboard-blame ()
  "Show startup timing blame view sorted by duration."
  (interactive)
  (if (= 0 (hash-table-count supervisor--start-times))
      (message "No timing data available (run supervisor-start first)")
    (let ((entries nil))
      (maphash (lambda (id start-time)
                 (let* ((ready-time (gethash id supervisor--ready-times))
                        (duration (if ready-time
                                      (- ready-time start-time)
                                    nil)))
                   (push (list id start-time ready-time duration) entries)))
               supervisor--start-times)
      ;; Sort by duration descending (nil durations at end)
      (setq entries (sort entries
                          (lambda (a b)
                            (let ((da (nth 3 a))
                                  (db (nth 3 b)))
                              (cond ((and da db) (> da db))
                                    (da t)
                                    (t nil))))))
      ;; Display in a help buffer
      (with-help-window "*supervisor-blame*"
        (princ "Supervisor Startup Blame (sorted by duration)\n")
        (princ (make-string 50 ?=))
        (princ "\n\n")
        (dolist (e entries)
          (let ((id (nth 0 e))
                (duration (nth 3 e)))
            (princ (format "%-20s %s\n"
                           id
                           (if duration
                               (format "%.3fs" duration)
                             "(not ready)")))))))))

(defun supervisor-dashboard-show-graph ()
  "Show full dependency graph for all entries.
Displays computed dependencies after validation and cycle fallback."
  (interactive)
  (if (= 0 (hash-table-count supervisor--computed-deps))
      (message "No dependency data available (run supervisor-start first)")
    (with-help-window "*supervisor-deps*"
      (princ "Supervisor Dependency Graph\n")
      (princ (make-string 50 ?=))
      (princ "\n\n")
      ;; Group by stage
      (dolist (stage-name '(early services session ui))
        (let ((stage-entries nil))
          ;; Collect entries for this stage
          (let ((idx 0))
            (dolist (entry supervisor-programs)
              (let ((id (supervisor--extract-id entry idx)))
                (cl-incf idx)
                (unless (gethash id supervisor--invalid)
                  (let ((parsed (supervisor--parse-entry entry)))
                    (when (eq (nth 7 parsed) stage-name)
                      (push id stage-entries)))))))
          (when stage-entries
            (princ (format "=== %s ===\n" (upcase (symbol-name stage-name))))
            (dolist (id (nreverse stage-entries))
              (let* ((deps (gethash id supervisor--computed-deps))
                     (cycle-fallback (gethash id supervisor--cycle-fallback-ids))
                     (dependents nil))
                ;; Find entries that depend on this one
                (maphash (lambda (e-id e-deps)
                           (when (member id e-deps)
                             (push e-id dependents)))
                         supervisor--computed-deps)
                (princ (format "  %s%s\n"
                               id
                               (if cycle-fallback " [CYCLE FALLBACK]" "")))
                (when deps
                  (princ (format "    <- %s\n" (mapconcat #'identity deps ", "))))
                (when dependents
                  (princ (format "    -> %s\n" (mapconcat #'identity dependents ", "))))))
            (princ "\n")))))))

;;;###autoload
(defun supervisor ()
  "Open the supervisor dashboard."
  (interactive)
  (let ((buf (get-buffer-create "*supervisor*")))
    (with-current-buffer buf
      (supervisor-dashboard-mode)
      (setq tabulated-list-entries (supervisor--get-entries))
      (tabulated-list-print)
      (setq-local header-line-format
                  (concat (supervisor--stage-progress-banner)
                          "  " supervisor--help-text)))
    (pop-to-buffer buf)))

;;; File Watch

(defun supervisor--config-watch-file ()
  "Return the file to watch for config modification."
  (cond
   ((stringp supervisor-watch-config) supervisor-watch-config)
   ((and supervisor-watch-config user-init-file) user-init-file)
   (t nil)))

(defun supervisor--config-watch-callback (_event)
  "Handle config file modification.  Trigger reload after debounce."
  ;; Simple debounce: cancel pending reload, schedule new one
  (when (timerp (get 'supervisor--config-watch-callback 'timer))
    (cancel-timer (get 'supervisor--config-watch-callback 'timer)))
  (put 'supervisor--config-watch-callback 'timer
       (run-at-time 1 nil
                    (lambda ()
                      (supervisor--log 'info "config file changed, reloading...")
                      (supervisor-reload)))))

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
  "Stop watching config file."
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

Example configuration:

  (require \\='supervisor)

  (setq supervisor-programs
        \\='((\"sh -c \\='xrdb ~/.Xresources\\='\" :type oneshot :stage early)
          (\"nm-applet\" :type simple :stage session)
          (\"blueman-applet\" :type simple :restart t)))

  (supervisor-mode 1)

Or with `use-package':

  (use-package supervisor
    :config
    (setq supervisor-programs \\='(...))
    (supervisor-mode 1))

Process types:
- simple: Long-running daemons, restarted on crash if :restart t
- oneshot: Run-once scripts, exit is expected

Stages (run sequentially):
- early: X settings, keyboard config
- services: System daemons (polkit, dbus)
- session: User services (default)
- ui: Compositor, tray apps

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

(provide 'supervisor)
;;; supervisor.el ends here
