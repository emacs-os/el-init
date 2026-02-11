;;; supervisor.el --- Emacs Lisp process supervisor -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>
;;
;; Author: telecommuter <telecommuter@riseup.net>
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))
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

;; See README.org for full documentation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)

;; Forward declarations for optional features
(declare-function file-notify-add-watch "filenotify" (file flags callback))
(declare-function file-notify-rm-watch "filenotify" (descriptor))
(declare-function supervisor-dashboard-menu "supervisor" ())

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

;;; Dashboard Faces

(defface supervisor-status-running
  '((t :foreground "#00ff00" :weight bold))
  "Face for running status in dashboard."
  :group 'supervisor)

(defface supervisor-status-done
  '((t :foreground "#00bfff"))
  "Face for done/completed oneshot status in dashboard."
  :group 'supervisor)

(defface supervisor-status-failed
  '((t :foreground "#ff4444" :weight bold))
  "Face for failed status in dashboard."
  :group 'supervisor)

(defface supervisor-status-dead
  '((t :foreground "#ff8c00" :weight bold))
  "Face for dead/crash-loop status in dashboard."
  :group 'supervisor)

(defface supervisor-status-invalid
  '((t :foreground "#ff00ff" :weight bold))
  "Face for invalid config entry in dashboard."
  :group 'supervisor)

(defface supervisor-status-pending
  '((t :foreground "#ffd700"))
  "Face for pending/not-yet-started status in dashboard."
  :group 'supervisor)

(defface supervisor-status-stopped
  '((t :foreground "#888888"))
  "Face for stopped status in dashboard."
  :group 'supervisor)

(defface supervisor-type-simple
  '((t :foreground "#87ceeb"))
  "Face for simple process type in dashboard."
  :group 'supervisor)

(defface supervisor-type-oneshot
  '((t :foreground "#dda0dd"))
  "Face for oneshot process type in dashboard."
  :group 'supervisor)

(defface supervisor-stage-1
  '((t :foreground "#ff6b6b"))
  "Face for stage1 in dashboard."
  :group 'supervisor)

(defface supervisor-stage-2
  '((t :foreground "#feca57"))
  "Face for stage2 in dashboard."
  :group 'supervisor)

(defface supervisor-stage-3
  '((t :foreground "#48dbfb"))
  "Face for stage3 in dashboard."
  :group 'supervisor)

(defface supervisor-stage-4
  '((t :foreground "#1dd1a1"))
  "Face for stage4 in dashboard."
  :group 'supervisor)

(defface supervisor-enabled-yes
  '((t :foreground "#00ff00"))
  "Face for enabled=yes in dashboard."
  :group 'supervisor)

(defface supervisor-enabled-no
  '((t :foreground "#ff6b6b"))
  "Face for enabled=no in dashboard."
  :group 'supervisor)

(defface supervisor-reason
  '((t :foreground "#ffa500" :slant italic))
  "Face for reason column in dashboard."
  :group 'supervisor)

(defface supervisor-stage-separator
  '((t :foreground "#5f87af" :weight bold :underline t))
  "Face for stage separator rows in dashboard."
  :group 'supervisor)

(defcustom supervisor-stage-descriptions
  '((stage1 . "X Setup")
    (stage2 . "System")
    (stage3 . "Services")
    (stage4 . "Applets"))
  "Alist mapping stage symbols to human-readable descriptions.
Used in dashboard stage separators for better readability."
  :type '(alist :key-type symbol :value-type string)
  :group 'supervisor)

(defcustom supervisor-dashboard-group-by-stage t
  "When non-nil, group dashboard entries by stage with separators."
  :type 'boolean
  :group 'supervisor)

(defcustom supervisor-dashboard-show-header-hints nil
  "When non-nil, show key hints in dashboard header line.
When nil (default), the header shows only the health summary.
Press \\`h' in the dashboard for full keybinding help."
  :type 'boolean
  :group 'supervisor)

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
cycle fallback behavior.  Uses the pure plan builder internally."
  (interactive)
  ;; Build plan using pure function
  (let ((plan (supervisor--build-plan supervisor-programs)))
    ;; Populate legacy globals for dashboard compatibility
    (clrhash supervisor--invalid)
    (clrhash supervisor--cycle-fallback-ids)
    (clrhash supervisor--computed-deps)
    (maphash (lambda (k v) (puthash k v supervisor--invalid))
             (supervisor-plan-invalid plan))
    (maphash (lambda (k v) (puthash k v supervisor--cycle-fallback-ids))
             (supervisor-plan-cycle-fallback-ids plan))
    (maphash (lambda (k v) (puthash k v supervisor--computed-deps))
             (supervisor-plan-deps plan))
    ;; Display from plan artifact
    (with-output-to-temp-buffer "*supervisor-dry-run*"
      (princ "=== Supervisor Dry Run ===\n\n")
      (princ (format "Total entries: %d valid, %d invalid\n\n"
                     (length (supervisor-plan-entries plan))
                     (hash-table-count (supervisor-plan-invalid plan))))
      ;; Show invalid entries first
      (when (> (hash-table-count (supervisor-plan-invalid plan)) 0)
        (princ "--- Invalid Entries (skipped) ---\n")
        (maphash (lambda (id reason)
                   (princ (format "  %s: %s\n" id reason)))
                 (supervisor-plan-invalid plan))
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
            ;; Load overrides into hashes
            (dolist (entry overrides)
              (let ((id (car entry))
                    (plist (cdr entry)))
                (when-let* ((enabled (plist-get plist :enabled)))
                  (puthash id enabled supervisor--enabled-override))
                (when-let* ((restart (plist-get plist :restart)))
                  (puthash id restart supervisor--restart-override))
                (when-let* ((logging (plist-get plist :logging)))
                  (puthash id logging supervisor--logging))))
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
KEY is one of :enabled, :restart, or :logging.
VALUE is `enabled', `disabled', or nil (to clear)."
  (let ((hash (pcase key
                (:enabled supervisor--enabled-override)
                (:restart supervisor--restart-override)
                (:logging supervisor--logging))))
    (if (null value)
        (remhash id hash)
      (puthash id value hash)))
  (supervisor--save-overrides))

(defun supervisor--clear-all-overrides ()
  "Clear all overrides from memory and file."
  (clrhash supervisor--enabled-override)
  (clrhash supervisor--restart-override)
  (clrhash supervisor--logging)
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
        (supervisor--refresh-dashboard)
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
    (supervisor--refresh-dashboard)
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
    process-exit process-failed cleanup)
  "Valid event types for the structured event system.
- `stage-start': stage begins processing
- `stage-complete': stage finished processing
- `process-started': process successfully spawned
- `process-ready': process became ready (simple=spawned, oneshot=exited)
- `process-exit': supervised process exited
- `process-failed': process failed to spawn
- `cleanup': cleanup phase beginning")

(defvar supervisor-event-hook nil
  "Hook run for all supervisor events.
Called with one argument: an event plist with the following keys:
  :type  - event type symbol (see `supervisor--event-types')
  :ts    - timestamp (float-time)
  :id    - entry ID string (for process events, nil for stage/global)
  :stage - stage name symbol (for stage events, nil otherwise)
  :data  - additional payload plist (event-type specific)

This is the unified event interface.  Legacy hooks remain for
backward compatibility but are dispatched via this system.")

(defun supervisor--emit-event (type &optional id stage data)
  "Emit a structured event of TYPE with optional ID, STAGE, and DATA.
TYPE must be a member of `supervisor--event-types'.
Runs `supervisor-event-hook' with the event plist, then dispatches
to legacy hooks for backward compatibility."
  (unless (memq type supervisor--event-types)
    (error "Invalid event type: %s" type))
  (let ((event (list :type type
                     :ts (float-time)
                     :id id
                     :stage stage
                     :data data)))
    ;; Run unified event hook
    (run-hook-with-args 'supervisor-event-hook event)
    ;; Log stage transitions to *Messages* (always visible, not warnings)
    (pcase type
      ('stage-start
       (message "Supervisor: %s starting" stage))
      ('stage-complete
       (message "Supervisor: %s complete" stage)))
    ;; Dispatch to legacy hooks for backward compatibility
    (pcase type
      ('stage-start
       (run-hook-with-args 'supervisor-stage-start-hook stage))
      ('stage-complete
       (run-hook-with-args 'supervisor-stage-complete-hook stage))
      ('process-exit
       (let ((status (plist-get data :status))
             (code (plist-get data :code)))
         (run-hook-with-args 'supervisor-process-exit-hook id status code)))
      ('cleanup
       (run-hooks 'supervisor-cleanup-hook)))))

(defvar supervisor-cleanup-hook nil
  "Hook run during cleanup, before killing processes.
Legacy hook; prefer `supervisor-event-hook' for new code.")

(defvar supervisor-stage-start-hook nil
  "Hook run when a stage begins.
Called with one argument: the stage name symbol.
Legacy hook; prefer `supervisor-event-hook' for new code.")

(defvar supervisor-stage-complete-hook nil
  "Hook run when a stage completes.
Called with one argument: the stage name symbol.
Legacy hook; prefer `supervisor-event-hook' for new code.")

(defvar supervisor-process-exit-hook nil
  "Hook run when a supervised process exits.
Called with three arguments: ID (string), STATUS (symbol), and CODE (integer).
STATUS is one of:
  `exited'   - process exited normally (CODE is exit code, 0 = success)
  `signal'   - process killed by signal (CODE is signal number)
  `unknown'  - exit status could not be determined
Legacy hook; prefer `supervisor-event-hook' for new code.")

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

;;; Migration Layer (Schema v1)
;;
;; Functions to help migrate from legacy supervisor-programs format
;; to the schema v1 service definition format.

(defun supervisor--migrate-entry-to-plist (entry)
  "Migrate raw ENTRY to a canonical schema v1 plist format.
Returns a plist suitable for use in supervisor-programs.
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
  "Migrate all entries in `supervisor-programs' to schema v1 format.
Returns a list of migrated entries with a summary of changes.
Invalid and duplicate entries are skipped with a warning."
  (let ((seen (make-hash-table :test 'equal))
        (migrated nil)
        (skipped nil)
        (idx 0))
    (dolist (entry supervisor-programs)
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
  "Display the current config migrated to schema v1 format.
Shows the canonical form of all valid entries, suitable for
replacing the existing `supervisor-programs' definition."
  (interactive)
  (let* ((result (supervisor--migrate-all-entries))
         (migrated (plist-get result :migrated))
         (skipped (plist-get result :skipped)))
    (with-output-to-temp-buffer "*supervisor-migrate*"
      (princ ";;; Supervisor Configuration (Schema v1)\n")
      (princ ";; Generated by supervisor-migrate-config\n")
      (princ (format ";; Version: %d\n\n" supervisor-service-schema-version))
      (when skipped
        (princ ";; Skipped entries (invalid or duplicate):\n")
        (dolist (entry skipped)
          (princ (format ";;   %s: %s\n" (car entry) (cdr entry))))
        (princ "\n"))
      (princ "(setq supervisor-programs\n")
      (princ "      '(")
      (let ((first t))
        (dolist (entry migrated)
          (if first
              (setq first nil)
            (princ "\n        "))
          (if (stringp entry)
              (princ (format "%S" entry))
            (princ "(")
            (princ (format "%S" (car entry)))
            (let ((plist (cdr entry)))
              (while plist
                (princ (format " %s %S" (car plist) (cadr plist)))
                (setq plist (cddr plist))))
            (princ ")"))))
      (princ "))\n")
      (princ "\n;; To use: Copy this to your init file, replacing your\n")
      (princ ";; existing supervisor-programs definition.\n"))))

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
Return t if enabled, nil if disabled.  Runtime overrides take
effect on next start (manual or restart)."
  (let ((override (gethash id supervisor--enabled-override)))
    (cond ((eq override 'enabled) t)
          ((eq override 'disabled) nil)
          (t config-enabled))))

(defun supervisor--handle-oneshot-exit (name proc-status exit-code)
  "Handle exit of oneshot process NAME.
PROC-STATUS is the process status symbol, EXIT-CODE is the exit code.
Logs completion, invokes callbacks, and notifies DAG scheduler."
  (puthash name exit-code supervisor--oneshot-completed)
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
        ;; Emit process exit event (dispatches to legacy hook)
        (supervisor--emit-event 'process-exit name nil
                                (list :status exit-status :code exit-code))
        ;; Handle oneshot completion
        (when (eq type 'oneshot)
          (supervisor--handle-oneshot-exit name proc-status exit-code))
        (supervisor--refresh-dashboard)
        ;; Handle shutdown tracking
        (when supervisor--shutting-down
          (supervisor--handle-shutdown-exit))
        ;; Schedule restart if appropriate
        (unless (or (eq type 'oneshot)
                    supervisor--shutting-down
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
  ;; Load persisted overrides (restores enabled/restart/logging overrides)
  (supervisor--load-overrides)
  (supervisor--dag-cleanup)
  (supervisor--rotate-logs)
  ;; Build execution plan (pure, deterministic)
  (let ((plan (supervisor--build-plan supervisor-programs)))
    ;; Populate legacy globals from plan for dashboard/other code
    (maphash (lambda (k v) (puthash k v supervisor--invalid))
             (supervisor-plan-invalid plan))
    (maphash (lambda (k v) (puthash k v supervisor--cycle-fallback-ids))
             (supervisor-plan-cycle-fallback-ids plan))
    (maphash (lambda (k v) (puthash k v supervisor--computed-deps))
             (supervisor-plan-deps plan))
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
        (supervisor--log 'info "startup complete"))
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
        (enabled-override (supervisor-snapshot-enabled-override snapshot)))
    ;; Collect running IDs from snapshot
    (let ((running-ids nil))
      (maphash (lambda (id _) (push id running-ids)) process-alive)
      ;; Check for processes to stop (running but not in plan, or now disabled)
      (dolist (id running-ids)
        (let ((in-plan (member id plan-ids)))
          (if (not in-plan)
              ;; Removed from config
              (push (list :op 'stop :id id :reason 'removed) actions)
            ;; Check if now disabled
            (let* ((entry (cl-find id plan-entries :key #'car :test #'equal))
                   (enabled-p (nth 3 entry))
                   (override (gethash id enabled-override))
                   (effective-enabled (cond ((eq override 'enabled) t)
                                            ((eq override 'disabled) nil)
                                            (t enabled-p))))
              (if (not effective-enabled)
                  (push (list :op 'stop :id id :reason 'disabled) actions)
                (push (list :op 'noop :id id :reason 'already-running) actions)))))))
    ;; Check for processes to start (in plan but not running)
    (dolist (entry plan-entries)
      (let* ((id (car entry))
             (enabled-p (nth 3 entry))
             (type (nth 6 entry))
             (override (gethash id enabled-override))
             (effective-enabled (cond ((eq override 'enabled) t)
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
          (push (list :op 'skip :id id :reason 'disabled) actions))
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
               (supervisor--log 'info "reload: stopping %s entry %s" reason id)
               (puthash id 'disabled supervisor--restart-override)
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
                         (supervisor--log 'warning "reload: executable not found for %s: %s"
                                          id (car args))
                         (supervisor--emit-event 'process-failed id nil nil))
                     (supervisor--log 'info "reload: starting %s entry %s" reason id)
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

;;;###autoload
(defun supervisor-reload ()
  "Reload configuration and reconcile running processes.
Uses declarative reconciliation: build plan, build snapshot, compute
actions, then apply.  The action list can be inspected before apply
by calling `supervisor--compute-actions' directly.

Stops processes removed from config or now disabled, starts new processes.
Does not restart changed entries - use dashboard kill/start for that."
  (interactive)
  ;; Build plan and snapshot
  (let* ((plan (supervisor--build-plan supervisor-programs))
         (snapshot (supervisor--build-snapshot))
         ;; Compute actions (pure)
         (actions (supervisor--compute-actions plan snapshot))
         ;; Apply actions
         (result (supervisor--apply-actions actions plan))
         (stopped (plist-get result :stopped))
         (started (plist-get result :started)))
    ;; Populate legacy globals from plan for dashboard
    (clrhash supervisor--invalid)
    (maphash (lambda (k v) (puthash k v supervisor--invalid))
             (supervisor-plan-invalid plan))
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
    (define-key map "K" #'supervisor-dashboard-kill-force)
    (define-key map "s" #'supervisor-dashboard-start)
    (define-key map "l" #'supervisor-dashboard-toggle-logging)
    (define-key map "L" #'supervisor-dashboard-view-log)
    (define-key map "p" #'proced)
    (define-key map "P" #'supervisor-dashboard-toggle-proced-auto-update)
    (define-key map "g" #'supervisor-dashboard-refresh)
    (define-key map "G" #'supervisor-dashboard-toggle-auto-refresh)
    (define-key map "h" #'supervisor-dashboard-help)
    (define-key map "i" #'supervisor-dashboard-describe-entry)
    (define-key map "d" #'supervisor-dashboard-show-deps)
    (define-key map "D" #'supervisor-dashboard-show-graph)
    (define-key map "B" #'supervisor-dashboard-blame)
    (define-key map "?" #'supervisor-dashboard-menu-open)
    (define-key map "q" #'supervisor-dashboard-quit)
    map)
  "Keymap for `supervisor-dashboard-mode'.")

(defvar supervisor--dashboard-menu-defined nil
  "Non-nil if the transient menu has been defined.")

(defun supervisor--define-dashboard-menu ()
  "Define the transient menu for the dashboard.
This is called on first use to avoid loading transient at package load time."
  (unless supervisor--dashboard-menu-defined
    (require 'transient)
    (eval
     '(transient-define-prefix supervisor-dashboard-menu ()
        "Supervisor dashboard actions."
        [:description
         (lambda ()
           (supervisor--health-summary (supervisor--build-snapshot)))
         ["Entry Actions"
          ("s" "Start" supervisor-dashboard-start)
          ("k" "Kill" supervisor-dashboard-kill)
          ("K" "Kill (force)" supervisor-dashboard-kill-force)]
         ["Toggles"
          ("e" "Enabled" supervisor-dashboard-toggle-enabled)
          ("r" "Restart" supervisor-dashboard-toggle-restart)
          ("l" "Logging" supervisor-dashboard-toggle-logging)]
         ["Views"
          ("L" "View log" supervisor-dashboard-view-log)
          ("d" "Dependencies" supervisor-dashboard-show-deps)
          ("D" "Dep graph" supervisor-dashboard-show-graph)
          ("B" "Blame" supervisor-dashboard-blame)]]
        [["Filter"
          ("f" "Cycle stage" supervisor-dashboard-cycle-filter)
          ("t" "Cycle tag" supervisor-dashboard-cycle-tag-filter)]
         ["Refresh"
          ("g" "Refresh" supervisor-dashboard-refresh)
          ("G" "Auto-refresh" supervisor-dashboard-toggle-auto-refresh)]
         ["System"
          ("p" "Proced" proced)
          ("P" "Proced auto" supervisor-dashboard-toggle-proced-auto-update)]
         ["Help"
          ("i" "Entry info" supervisor-dashboard-describe-entry)
          ("h" "Full help" supervisor-dashboard-help)
          ("q" "Quit" supervisor-dashboard-quit)]]))
    (setq supervisor--dashboard-menu-defined t)))

(defun supervisor-dashboard-menu-open ()
  "Open the supervisor dashboard transient menu.
Requires the `transient' package to be installed."
  (interactive)
  (unless (require 'transient nil t)
    (user-error "The `transient' package is required for this feature"))
  (supervisor--define-dashboard-menu)
  (call-interactively #'supervisor-dashboard-menu))

(defvar-local supervisor--dashboard-last-id nil
  "Last echoed entry ID, to avoid repeated messages.")

(defun supervisor--dashboard-echo-id ()
  "Echo full entry ID in minibuffer if point moved to new row."
  (when-let* ((id (tabulated-list-get-id)))
    (unless (equal id supervisor--dashboard-last-id)
      (setq supervisor--dashboard-last-id id)
      ;; Only echo string IDs longer than column width (15)
      ;; Skip separator rows (symbol IDs)
      (when (and (stringp id) (> (length id) 15))
        (message "%s" id)))))

(define-derived-mode supervisor-dashboard-mode tabulated-list-mode "Supervisor"
  "Major mode for the supervisor dashboard."
  (setq tabulated-list-format [("ID" 15 t)
                               ("Type" 7 t)
                               ("Stage" 8 t)
                               ("Enabled" 7 t)
                               ("Status" 8 t)
                               ("Restart" 7 t)
                               ("Log" 3 t)
                               ("PID" 7 t)
                               ("Reason" 30 t)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header)
  ;; Echo full ID in minibuffer when point moves between rows
  (add-hook 'post-command-hook #'supervisor--dashboard-echo-id nil t)
  ;; Clean up auto-refresh timer when buffer is killed
  (add-hook 'kill-buffer-hook #'supervisor--cleanup-auto-refresh nil t))

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

(defun supervisor--status-face (status)
  "Return the face for STATUS string."
  (pcase status
    ("running" 'supervisor-status-running)
    ("done" 'supervisor-status-done)
    ("failed" 'supervisor-status-failed)
    ("dead" 'supervisor-status-dead)
    ("invalid" 'supervisor-status-invalid)
    ("pending" 'supervisor-status-pending)
    ("stopped" 'supervisor-status-stopped)
    (_ nil)))

(defun supervisor--propertize-status (status)
  "Return STATUS string with appropriate face applied."
  (let ((face (supervisor--status-face status)))
    (if face
        (propertize status 'face face)
      status)))

(defun supervisor--make-stage-separator (stage)
  "Create a separator row for STAGE."
  (let* ((desc (or (cdr (assq stage supervisor-stage-descriptions))
                   (symbol-name stage)))
         (label (propertize (format "── %s: %s " stage desc)
                            'face 'supervisor-stage-separator)))
    (list (intern (format "--%s--" stage))  ; Use symbol for separator ID
          (vector label "" "" "" "" "" "" "" ""))))

(defun supervisor--separator-row-p (id)
  "Return non-nil if ID represents a stage separator row."
  (and id (symbolp id) (string-prefix-p "--" (symbol-name id))))

(defun supervisor--compute-entry-status (id type &optional snapshot)
  "Compute status string and PID string for entry ID of TYPE.
If SNAPSHOT is provided, read from it; otherwise read from globals.
Return a cons cell (STATUS . PID)."
  (let* ((alive (if snapshot
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
         (oneshot-failed (and oneshot-done (> oneshot-exit 0)))
         (pid (cond (alive (number-to-string pid-num))
                    ((and oneshot-p oneshot-done) (format "exit:%d" oneshot-exit))
                    (t "-")))
         (status (cond (alive "running")
                       (failed "dead")
                       ((and oneshot-p oneshot-failed) "failed")
                       ((and oneshot-p oneshot-done) "done")
                       (oneshot-p "pending")
                       (t "stopped"))))
    (cons status pid)))

(defun supervisor--compute-entry-reason (id type &optional snapshot)
  "Compute reason string for entry ID of TYPE.
If SNAPSHOT is provided, read from it; otherwise read from globals."
  (let* ((alive (if snapshot
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

(defun supervisor--make-dashboard-entry (id type stage enabled-p restart-p logging-p
                                            &optional snapshot)
  "Create a dashboard entry vector for ID.
TYPE, STAGE, ENABLED-P, RESTART-P, LOGGING-P are parsed entry fields.
If SNAPSHOT is provided, read runtime state from it."
  (let* ((status-pid (supervisor--compute-entry-status id type snapshot))
         (status (car status-pid))
         (pid (cdr status-pid))
         (reason (supervisor--compute-entry-reason id type snapshot))
         ;; For overrides, use snapshot if provided, otherwise globals
         (enabled-override (if snapshot
                               (gethash id (supervisor-snapshot-enabled-override snapshot))
                             (gethash id supervisor--enabled-override)))
         (effective-enabled (cond ((eq enabled-override 'enabled) t)
                                  ((eq enabled-override 'disabled) nil)
                                  (t enabled-p)))
         (restart-override (if snapshot
                               (gethash id (supervisor-snapshot-restart-override snapshot))
                             (gethash id supervisor--restart-override)))
         (effective-restart (cond ((eq restart-override 'enabled) t)
                                  ((eq restart-override 'disabled) nil)
                                  (t restart-p)))
         (restart-str (if (eq type 'oneshot)
                          "-"
                        (if effective-restart "yes" "no")))
         (log-override (if snapshot
                           (gethash id (supervisor-snapshot-logging-override snapshot))
                         (gethash id supervisor--logging)))
         (effective-logging (cond ((eq log-override 'enabled) t)
                                  ((eq log-override 'disabled) nil)
                                  (t logging-p))))
    (vector id
            (propertize (symbol-name type)
                        'face (if (eq type 'oneshot)
                                  'supervisor-type-oneshot
                                'supervisor-type-simple))
            (propertize (symbol-name stage)
                        'face (pcase stage
                                ('stage1 'supervisor-stage-1)
                                ('stage2 'supervisor-stage-2)
                                ('stage3 'supervisor-stage-3)
                                ('stage4 'supervisor-stage-4)
                                (_ nil)))
            (propertize (if effective-enabled "yes" "no")
                        'face (if effective-enabled
                                  'supervisor-enabled-yes
                                'supervisor-enabled-no))
            (supervisor--propertize-status status)
            restart-str
            (if effective-logging "yes" "no")
            pid
            (if (string-empty-p reason)
                reason
              (propertize reason 'face 'supervisor-reason)))))

(defun supervisor--group-entries-by-stage (entries)
  "Group ENTRIES by stage with separator rows.
ENTRIES is a list of (id vector stage) tuples."
  (let* ((sorted (sort entries
                       (lambda (a b)
                         (let* ((stage-a (nth 2 a))
                                (stage-b (nth 2 b))
                                (sa (if stage-a (supervisor--stage-to-int stage-a) 99))
                                (sb (if stage-b (supervisor--stage-to-int stage-b) 99)))
                           (< sa sb)))))
         (result nil)
         (current-stage nil))
    (dolist (entry sorted)
      (let ((entry-stage (nth 2 entry)))
        (when (and entry-stage (not (eq entry-stage current-stage)))
          (setq current-stage entry-stage)
          (push (supervisor--make-stage-separator entry-stage) result))
        (push (list (car entry) (cadr entry)) result)))
    (nreverse result)))

(defun supervisor--get-entries (&optional snapshot)
  "Generate entries for the dashboard (deduplicates on the fly).
Respects `supervisor--dashboard-stage-filter' and tag filter when set.
When `supervisor-dashboard-group-by-stage' is non-nil and no stage filter
is active, entries are grouped by stage with separator rows.
If SNAPSHOT is provided, read runtime state from it."
  (let* ((snapshot (or snapshot (supervisor--build-snapshot)))
         (entries nil)
         (seen (make-hash-table :test 'equal))
         (stage-filter supervisor--dashboard-stage-filter)
         (tag-filter supervisor--dashboard-tag-filter)
         (invalid-hash (supervisor-snapshot-invalid snapshot))
         (idx 0))
    (dolist (entry supervisor-programs)
      (let* ((raw-id (supervisor--extract-id entry idx))
             (invalid-reason (gethash raw-id invalid-hash)))
        (cl-incf idx)
        (unless (gethash raw-id seen)
          (puthash raw-id t seen)
          (if invalid-reason
              (unless stage-filter
                (push (list raw-id
                            (vector raw-id "-" "-" "-"
                                    (supervisor--propertize-status "invalid")
                                    "-" "-" "-"
                                    invalid-reason)
                            nil)
                      entries))
            (pcase-let ((`(,id ,_cmd ,_delay ,enabled-p ,restart-p ,logging-p
                               ,type ,stage ,_after ,_owait ,_otimeout ,tags)
                         (supervisor--parse-entry entry)))
              (when (and (or (null stage-filter) (eq stage stage-filter))
                         (or (null tag-filter) (member tag-filter tags)))
                (push (list id
                            (supervisor--make-dashboard-entry
                             id type stage enabled-p restart-p logging-p snapshot)
                            stage)
                      entries)))))))
    (setq entries (nreverse entries))
    (if (and supervisor-dashboard-group-by-stage (null stage-filter))
        (supervisor--group-entries-by-stage entries)
      (mapcar (lambda (e) (list (car e) (cadr e))) entries))))

(defun supervisor--health-summary (&optional snapshot)
  "Return compact health summary string.
If SNAPSHOT is provided, read state from it; otherwise read from globals."
  (let ((running 0) (done 0) (failed 0) (invalid 0) (pending 0)
        (seen (make-hash-table :test 'equal))
        (invalid-hash (if snapshot
                          (supervisor-snapshot-invalid snapshot)
                        supervisor--invalid))
        (process-alive (when snapshot (supervisor-snapshot-process-alive snapshot)))
        (failed-hash (if snapshot
                         (supervisor-snapshot-failed snapshot)
                       supervisor--failed))
        (oneshot-hash (if snapshot
                          (supervisor-snapshot-oneshot-exit snapshot)
                        supervisor--oneshot-completed))
        (idx 0))
    (dolist (entry supervisor-programs)
      (let ((raw-id (supervisor--extract-id entry idx)))
        (cl-incf idx)
        ;; Skip duplicates to match runtime behavior
        (unless (gethash raw-id seen)
          (puthash raw-id t seen)
          (if (gethash raw-id invalid-hash)
              (cl-incf invalid)
            (let ((parsed (ignore-errors (supervisor--parse-entry entry))))
              (if (null parsed)
                  (cl-incf invalid)
                (let* ((id (car parsed))
                       (type (nth 6 parsed))
                       (alive (if snapshot
                                  (gethash id process-alive)
                                (let ((proc (gethash id supervisor--processes)))
                                  (and proc (process-live-p proc)))))
                       (is-failed (gethash id failed-hash))
                       (oneshot-p (eq type 'oneshot))
                       (oneshot-exit (gethash id oneshot-hash)))
                  (cond
                   (alive (cl-incf running))
                   (is-failed (cl-incf failed))
                   ((and oneshot-p oneshot-exit (> oneshot-exit 0)) (cl-incf failed))
                   ((and oneshot-p oneshot-exit) (cl-incf done))
                   (oneshot-p (cl-incf pending))
                   (t (cl-incf pending))))))))))
    (concat (propertize (format "%d" running) 'face 'supervisor-status-running)
            " run | "
            (propertize (format "%d" done) 'face 'supervisor-status-done)
            " done | "
            (propertize (format "%d" pending) 'face 'supervisor-status-pending)
            " pend | "
            (propertize (format "%d" failed) 'face 'supervisor-status-failed)
            " fail | "
            (propertize (format "%d" invalid) 'face 'supervisor-status-invalid)
            " inv")))

(defvar supervisor--help-text
  (concat "[e]nable [f]ilter [t]ag [s]tart [k]ill [K]force [r]estart "
          "[l]og [L]view [p]roced [P]auto [d]eps [D]graph "
          "[B]lame [g]refresh [G]live [?]menu [i]nfo [h]elp [q]uit")
  "Key hints displayed in dashboard header.")

(defun supervisor--refresh-dashboard ()
  "Refresh the dashboard buffer if it exists.
Builds a single snapshot and uses it for both entries and health summary,
ensuring consistency within a single refresh cycle."
  (when-let* ((buf (get-buffer "*supervisor*")))
    (with-current-buffer buf
      (let* ((snapshot (supervisor--build-snapshot))
             (pos (point)))
        (setq tabulated-list-entries (supervisor--get-entries snapshot))
        (tabulated-list-print t)
        (setq header-line-format
              (concat (supervisor--health-summary snapshot)
                      (when supervisor-dashboard-show-header-hints
                        (concat "  " supervisor--help-text))))
        (goto-char (min pos (point-max)))))))

(defun supervisor-dashboard-refresh ()
  "Refresh the dashboard."
  (interactive)
  (supervisor--refresh-dashboard))

(defun supervisor-dashboard-cycle-filter ()
  "Cycle dashboard stage filter through all stages."
  (interactive)
  (setq supervisor--dashboard-stage-filter
        (pcase supervisor--dashboard-stage-filter
          ('nil 'stage1)
          ('stage1 'stage2)
          ('stage2 'stage3)
          ('stage3 'stage4)
          ('stage4 nil)))
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

(defvar supervisor--status-legend
  "Status: running=active process | done=oneshot completed ok | failed=crashed/exit>0 | dead=crash-loop | pending=not yet started | stopped=terminated | invalid=config error"
  "Legend explaining status column values.")

(defun supervisor-dashboard-describe-entry ()
  "Show detailed information about entry at point.
With prefix argument, show status legend instead."
  (interactive)
  (if current-prefix-arg
      (message "%s" supervisor--status-legend)
    (let ((id (tabulated-list-get-id)))
      (cond
       ((null id)
        (message "%s" supervisor--status-legend))
       ((supervisor--separator-row-p id)
        (message "Stage separator row"))
       (t
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
                (message "Entry not found: %s" id))))))))))

(defun supervisor-dashboard-help ()
  "Show full help for supervisor dashboard."
  (interactive)
  (with-help-window "*Supervisor Help*"
    (princ "Supervisor Dashboard Help\n")
    (princ "=========================\n\n")
    (princ "KEYBINDINGS\n")
    (princ "-----------\n")
    (princ "  e     Toggle enabled/disabled for entry\n")
    (princ "  f     Cycle stage filter (all -> stage1 -> stage2 -> ...)\n")
    (princ "  t     Cycle tag filter\n")
    (princ "  s     Start process (if stopped)\n")
    (princ "  k     Kill process (with confirmation)\n")
    (princ "  K     Kill process (force, no confirmation)\n")
    (princ "  r     Toggle auto-restart for entry\n")
    (princ "  l     Toggle logging for entry\n")
    (princ "  L     View log file for entry\n")
    (princ "  p     Open proced (system process list)\n")
    (princ "  P     Toggle proced auto-update mode\n")
    (princ "  g     Refresh dashboard\n")
    (princ "  G     Toggle auto-refresh (live monitoring)\n")
    (princ "  ?     Open action menu (transient)\n")
    (princ "  i     Show entry details (C-u i for status legend)\n")
    (princ "  h     Show this help\n")
    (princ "  d     Show dependencies for entry\n")
    (princ "  D     Show dependency graph\n")
    (princ "  B     Blame: show why entry is in current state\n")
    (princ "  q     Quit dashboard\n\n")
    (princ "STATUS VALUES\n")
    (princ "-------------\n")
    (princ "  running   Process is alive and running\n")
    (princ "  done      Oneshot completed successfully (exit 0)\n")
    (princ "  failed    Process crashed or oneshot exited non-zero\n")
    (princ "  dead      Process crash-looped (exceeded restart limit)\n")
    (princ "  pending   Not yet started (waiting for stage/deps)\n")
    (princ "  stopped   Simple process terminated (not crash-loop)\n")
    (princ "  invalid   Config entry has errors\n\n")
    (princ "COLUMN MEANINGS\n")
    (princ "---------------\n")
    (princ "  ID        Process identifier (from :id or command)\n")
    (princ "  Type      simple (daemon) or oneshot (run-once)\n")
    (princ "  Stage     Startup stage (stage1-4, lower runs first)\n")
    (princ "  Enabled   Whether process will start (yes/no)\n")
    (princ "  Status    Current state (see above)\n")
    (princ "  Restart   Auto-restart on crash (simple only)\n")
    (princ "  Log       Whether output is logged to file\n")
    (princ "  PID       Process ID or exit code (exit:N)\n")
    (princ "  Reason    Why process is in current state\n")))

(defvar-local supervisor--auto-refresh-timer nil
  "Timer for auto-refresh in dashboard buffer.")

(defcustom supervisor-auto-refresh-interval 2
  "Interval in seconds for dashboard auto-refresh when enabled."
  :type 'integer
  :group 'supervisor)

(defun supervisor--cleanup-auto-refresh ()
  "Clean up auto-refresh timer when dashboard buffer is killed."
  (when supervisor--auto-refresh-timer
    (cancel-timer supervisor--auto-refresh-timer)
    (setq supervisor--auto-refresh-timer nil)))

(defun supervisor-dashboard-toggle-auto-refresh ()
  "Toggle automatic dashboard refresh for live monitoring."
  (interactive)
  (if supervisor--auto-refresh-timer
      (progn
        (cancel-timer supervisor--auto-refresh-timer)
        (setq supervisor--auto-refresh-timer nil)
        (message "Auto-refresh disabled"))
    (setq supervisor--auto-refresh-timer
          (run-with-timer supervisor-auto-refresh-interval
                          supervisor-auto-refresh-interval
                          (lambda ()
                            (when (buffer-live-p (get-buffer "*supervisor*"))
                              (supervisor--refresh-dashboard)))))
    (message "Auto-refresh enabled (every %ds)" supervisor-auto-refresh-interval)))

(defun supervisor-dashboard-quit ()
  "Quit dashboard and clean up auto-refresh timer."
  (interactive)
  (supervisor--cleanup-auto-refresh)
  (quit-window))

(defun supervisor-dashboard-toggle-restart ()
  "Toggle auto-restart for process at point (no-op for oneshot).
Cycles: config default -> override opposite -> back to config default."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (when (supervisor--separator-row-p id)
      (user-error "Cannot toggle restart on separator row"))
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
              ;; Persist the override
              (supervisor--save-overrides)
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
    (when (supervisor--separator-row-p id)
      (user-error "Cannot toggle enabled on separator row"))
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
            ;; Persist the override
            (supervisor--save-overrides)
            (supervisor--refresh-dashboard)))))))

(defun supervisor-dashboard-kill (&optional force)
  "Stop process at point with confirmation.
Disables auto-restart and sends kill signal.
With prefix argument FORCE, skip confirmation."
  (interactive "P")
  (let ((id (tabulated-list-get-id)))
    (when (supervisor--separator-row-p id)
      (user-error "Cannot kill separator row")))
  (when-let* ((id (tabulated-list-get-id))
              (proc (gethash id supervisor--processes)))
    (when (process-live-p proc)
      (when (or force
                (yes-or-no-p (format "Kill process '%s'? " id)))
        ;; Disable auto-restart first so sentinel doesn't restart it
        (puthash id 'disabled supervisor--restart-override)
        (kill-process proc)
        (supervisor--refresh-dashboard)))))

(defun supervisor-dashboard-kill-force ()
  "Stop process at point without confirmation.
Disables auto-restart and sends kill signal immediately."
  (interactive)
  (supervisor-dashboard-kill t))

(defun supervisor-dashboard-start ()
  "Start process at point if stopped.
Respects runtime enable/disable overrides."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (when (supervisor--separator-row-p id)
      (user-error "Cannot start separator row"))
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
    (when (supervisor--separator-row-p id)
      (user-error "Cannot toggle logging on separator row"))
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
    (when (supervisor--separator-row-p id)
      (user-error "No log file for separator row"))
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
    (when (supervisor--separator-row-p id)
      (user-error "No dependencies for separator row"))
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
      (dolist (stage-name '(stage1 stage2 stage3 stage4))
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
  "Open the supervisor dashboard.
Builds a single snapshot for both entries and header to ensure consistency."
  (interactive)
  (let ((buf (get-buffer-create "*supervisor*")))
    (with-current-buffer buf
      (supervisor-dashboard-mode)
      ;; Build snapshot once and use for both entries and header
      (let ((snapshot (supervisor--build-snapshot)))
        (setq tabulated-list-entries (supervisor--get-entries snapshot))
        (tabulated-list-print)
        (setq-local header-line-format
                    (concat (supervisor--health-summary snapshot)
                            (when supervisor-dashboard-show-header-hints
                              (concat "  " supervisor--help-text))))))
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

Example configuration:

  (require \\='supervisor)

  (setq supervisor-programs
        \\='((\"sh -c \\='xrdb ~/.Xresources\\='\" :type oneshot :stage stage1)
          (\"nm-applet\" :type simple :stage stage3)
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

;;; CLI Control Plane
;;
;; Non-interactive command-line interface for controlling supervisor
;; via emacsclient. All business logic remains in Elisp; the external
;; wrapper (sbin/supervisorctl) is transport-only.

(defconst supervisor-cli-exit-success 0
  "Exit code for successful operation.")

(defconst supervisor-cli-exit-failure 1
  "Exit code for runtime failure.")

(defconst supervisor-cli-exit-invalid-args 2
  "Exit code for invalid arguments.")

(defconst supervisor-cli-exit-server-unavailable 3
  "Exit code when Emacs server is unavailable.")

(defconst supervisor-cli-exit-validation-failed 4
  "Exit code when config validation fails.")

(defconst supervisor-cli-version "1.0.0"
  "CLI version string.")

(cl-defstruct (supervisor-cli-result (:constructor supervisor-cli-result--create))
  "Result returned from CLI dispatcher.
EXITCODE is an integer (0=success, 1=failure, 2=invalid args, etc).
FORMAT is `human' or `json'.
OUTPUT is the string to print."
  (exitcode 0 :type integer)
  (format 'human :type symbol)
  (output "" :type string))

(defun supervisor--cli-make-result (exitcode format output)
  "Create CLI result with EXITCODE, FORMAT, and OUTPUT."
  (supervisor-cli-result--create :exitcode exitcode :format format :output output))

(defun supervisor--cli-success (output &optional format)
  "Create success result with OUTPUT and optional FORMAT (default human)."
  (supervisor--cli-make-result supervisor-cli-exit-success
                               (or format 'human) output))

(defun supervisor--cli-error (exitcode message &optional format)
  "Create error result with EXITCODE, MESSAGE, and optional FORMAT.
When FORMAT is `json', the error is returned as a JSON object."
  (let ((fmt (or format 'human)))
    (supervisor--cli-make-result
     exitcode fmt
     (if (eq fmt 'json)
         (json-encode `((error . t) (message . ,message) (exitcode . ,exitcode)))
       (format "Error: %s\n" message)))))

(defun supervisor--cli-split-at-separator (args)
  "Split ARGS at \"--\" separator into (before-list . after-list).
The \"--\" itself is not included in either list.
If no \"--\" is present, returns (ARGS . nil)."
  (let ((pos (cl-position "--" args :test #'equal)))
    (if pos
        (cons (cl-subseq args 0 pos)
              (cl-subseq args (1+ pos)))
      (cons args nil))))

(defun supervisor--cli-has-unknown-flags (args &optional known-flags)
  "Check if ARGS contain unknown flags (starting with - or --).
KNOWN-FLAGS is a list of exact allowed flags like \"--tail\" \"--signal\".
Returns the first unknown flag or nil.  Checks both -- and single - flags.
Stops checking at \"--\" separator (POSIX end-of-options convention)."
  (let* ((known (or known-flags '()))
         (split (supervisor--cli-split-at-separator args))
         (check-args (car split)))
    (cl-find-if (lambda (arg)
                  (and (string-prefix-p "-" arg)
                       (not (member arg known))))
                check-args)))

(defun supervisor--cli-strip-separator (args)
  "Remove \"--\" separator from ARGS, concatenating before and after parts."
  (let ((split (supervisor--cli-split-at-separator args)))
    (append (car split) (cdr split))))

(defun supervisor--cli-reject-extra-args (args json-p)
  "Return error result if ARGS is non-nil.  JSON-P controls format.
Returns nil if args is empty, otherwise returns an error result."
  (when args
    (supervisor--cli-error supervisor-cli-exit-invalid-args
                           (format "Unexpected arguments: %s"
                                   (mapconcat #'identity args " "))
                           (if json-p 'json 'human))))

(defun supervisor--cli-ensure-array (list)
  "Ensure LIST is encoded as JSON array, not null for empty list."
  (or list (vector)))

;;; CLI Status/List helpers

(defun supervisor--cli-entry-info (entry &optional snapshot)
  "Build info alist for parsed ENTRY, using optional SNAPSHOT for state.
Returns alist with all fields needed for status display."
  (let* ((id (supervisor-entry-id entry))
         (type (supervisor-entry-type entry))
         (stage (supervisor-entry-stage entry))
         (enabled-cfg (supervisor-entry-enabled-p entry))
         (restart-cfg (supervisor-entry-restart-p entry))
         (logging-cfg (supervisor-entry-logging-p entry))
         (delay (supervisor-entry-delay entry))
         (after (supervisor-entry-after entry))
         (requires (supervisor-entry-requires entry))
         (status-result (supervisor--compute-entry-status id type snapshot))
         (status (car status-result))
         (reason (supervisor--compute-entry-reason id type snapshot))
         ;; Get effective values with overrides
         (enabled-eff (supervisor--get-effective-enabled id enabled-cfg))
         (restart-eff (if (eq type 'oneshot) nil
                        (supervisor--get-effective-restart id restart-cfg)))
         (logging-eff (supervisor--get-effective-logging id logging-cfg))
         ;; Get PID if running
         (pid (when snapshot
                (gethash id (supervisor-snapshot-process-pids snapshot))))
         (pid-global (unless snapshot
                       (let ((proc (gethash id supervisor--processes)))
                         (when (and proc (process-live-p proc))
                           (process-id proc)))))
         ;; Get timing info
         (start-time (gethash id supervisor--start-times))
         (ready-time (gethash id supervisor--ready-times)))
    `((id . ,id)
      (type . ,type)
      (stage . ,stage)
      (enabled . ,enabled-eff)
      (enabled-config . ,enabled-cfg)
      (restart . ,restart-eff)
      (restart-config . ,restart-cfg)
      (logging . ,logging-eff)
      (logging-config . ,logging-cfg)
      (delay . ,delay)
      (after . ,after)
      (requires . ,requires)
      (status . ,status)
      (reason . ,reason)
      (pid . ,(or pid pid-global))
      (start-time . ,start-time)
      (ready-time . ,ready-time)
      (duration . ,(when (and start-time ready-time)
                     (- ready-time start-time))))))

(defun supervisor--cli-all-entries-info (&optional snapshot)
  "Build info alists for all valid entries, using optional SNAPSHOT.
Returns (entries invalid) where entries is list of alists and
invalid is list of (id . reason) pairs, sorted by ID for determinism."
  (let* ((plan (supervisor--build-plan supervisor-programs))
         (entries (supervisor-plan-entries plan))
         (invalid-hash (supervisor-plan-invalid plan))
         (invalid-list nil)
         (entry-infos nil))
    ;; Collect invalid entries
    (maphash (lambda (id reason)
               (push `((id . ,id) (reason . ,reason)) invalid-list))
             invalid-hash)
    ;; Sort invalid list by ID for deterministic output
    (setq invalid-list (sort invalid-list
                             (lambda (a b)
                               (string< (alist-get 'id a) (alist-get 'id b)))))
    ;; Collect valid entries info
    (dolist (entry entries)
      (push (supervisor--cli-entry-info entry snapshot) entry-infos))
    (list (nreverse entry-infos) invalid-list)))

;;; CLI Human Formatters

(defun supervisor--cli-format-bool (val)
  "Format boolean VAL as yes/no string."
  (if val "yes" "no"))

(defun supervisor--cli-format-status-line (info)
  "Format single entry INFO alist as status table row."
  (let ((id (alist-get 'id info))
        (type (alist-get 'type info))
        (stage (alist-get 'stage info))
        (enabled (alist-get 'enabled info))
        (status (alist-get 'status info))
        (restart (alist-get 'restart info))
        (logging (alist-get 'logging info))
        (pid (alist-get 'pid info))
        (reason (alist-get 'reason info)))
    (format "%-16s %-8s %-8s %-8s %-10s %-8s %-5s %-7s %s\n"
            (truncate-string-to-width (or id "-") 16)
            (or (symbol-name type) "-")
            (if stage (symbol-name stage) "-")
            (supervisor--cli-format-bool enabled)
            (or status "-")
            (if (eq type 'oneshot) "n/a"
              (supervisor--cli-format-bool restart))
            (supervisor--cli-format-bool logging)
            (if pid (number-to-string pid) "-")
            (or reason "-"))))

(defun supervisor--cli-format-invalid-line (info)
  "Format invalid entry INFO as status table row."
  (let ((id (alist-get 'id info))
        (reason (alist-get 'reason info)))
    (format "%-16s %-8s %-8s %-8s %-10s %-8s %-5s %-7s %s\n"
            (truncate-string-to-width (or id "-") 16)
            "-" "-" "-" "invalid" "-" "-" "-"
            (or reason "-"))))

(defun supervisor--cli-status-human (entries invalid)
  "Format ENTRIES and INVALID as human-readable status table."
  (let ((header (format "%-16s %-8s %-8s %-8s %-10s %-8s %-5s %-7s %s\n"
                        "ID" "TYPE" "STAGE" "ENABLED" "STATUS" "RESTART" "LOG" "PID" "REASON"))
        (sep (make-string 100 ?-)))
    (concat header sep "\n"
            (mapconcat #'supervisor--cli-format-status-line entries "")
            (when invalid
              (mapconcat #'supervisor--cli-format-invalid-line invalid "")))))

(defun supervisor--cli-describe-human (info)
  "Format single entry INFO as human-readable detail view."
  (let ((id (alist-get 'id info))
        (type (alist-get 'type info))
        (stage (alist-get 'stage info))
        (enabled (alist-get 'enabled info))
        (enabled-cfg (alist-get 'enabled-config info))
        (restart (alist-get 'restart info))
        (restart-cfg (alist-get 'restart-config info))
        (logging (alist-get 'logging info))
        (logging-cfg (alist-get 'logging-config info))
        (delay (alist-get 'delay info))
        (after (alist-get 'after info))
        (requires (alist-get 'requires info))
        (status (alist-get 'status info))
        (reason (alist-get 'reason info))
        (pid (alist-get 'pid info))
        (start-time (alist-get 'start-time info))
        (ready-time (alist-get 'ready-time info))
        (duration (alist-get 'duration info)))
    (concat
     (format "ID: %s\n" id)
     (format "Type: %s\n" type)
     (format "Stage: %s\n" stage)
     (format "Status: %s%s\n" status (if reason (format " (%s)" reason) ""))
     (format "Enabled: %s%s\n"
             (supervisor--cli-format-bool enabled)
             (if (not (eq enabled enabled-cfg)) " (override)" ""))
     (when (not (eq type 'oneshot))
       (format "Restart: %s%s\n"
               (supervisor--cli-format-bool restart)
               (if (not (eq restart restart-cfg)) " (override)" "")))
     (format "Logging: %s%s\n"
             (supervisor--cli-format-bool logging)
             (if (not (eq logging logging-cfg)) " (override)" ""))
     (format "Delay: %s\n" delay)
     (format "After: %s\n" (if after (mapconcat #'identity after ", ") "none"))
     (format "Requires: %s\n" (if requires (mapconcat #'identity requires ", ") "none"))
     (when pid (format "PID: %d\n" pid))
     (when start-time (format "Start time: %.3f\n" start-time))
     (when ready-time (format "Ready time: %.3f\n" ready-time))
     (when duration (format "Duration: %.3fs\n" duration)))))

;;; CLI JSON Formatters

(defun supervisor--cli-entry-to-json-obj (info)
  "Convert entry INFO alist to JSON-compatible alist."
  `((id . ,(alist-get 'id info))
    (type . ,(symbol-name (alist-get 'type info)))
    (stage . ,(symbol-name (alist-get 'stage info)))
    (enabled . ,(if (alist-get 'enabled info) t :json-false))
    (status . ,(alist-get 'status info))
    (restart . ,(if (alist-get 'restart info) t :json-false))
    (logging . ,(if (alist-get 'logging info) t :json-false))
    (pid . ,(or (alist-get 'pid info) :null))
    (reason . ,(or (alist-get 'reason info) :null))
    (delay . ,(alist-get 'delay info))
    (after . ,(or (alist-get 'after info) []))
    (requires . ,(or (alist-get 'requires info) []))
    (start_time . ,(or (alist-get 'start-time info) :null))
    (ready_time . ,(or (alist-get 'ready-time info) :null))
    (duration . ,(or (alist-get 'duration info) :null))))

(defun supervisor--cli-invalid-to-json-obj (info)
  "Convert invalid entry INFO to JSON-compatible alist."
  `((id . ,(alist-get 'id info))
    (reason . ,(alist-get 'reason info))))

(defun supervisor--cli-status-json (entries invalid)
  "Format ENTRIES and INVALID as JSON status object.
Empty lists are encoded as arrays, not null."
  (let ((obj `((entries . ,(supervisor--cli-ensure-array
                            (mapcar #'supervisor--cli-entry-to-json-obj entries)))
               (invalid . ,(supervisor--cli-ensure-array
                            (mapcar #'supervisor--cli-invalid-to-json-obj invalid))))))
    (json-encode obj)))

;;; CLI Subcommand Handlers

(defun supervisor--cli-cmd-status (args json-p)
  "Handle `status' command with ARGS.  JSON-P enables JSON output."
  ;; Check for unknown flags (args starting with --)
  (let ((unknown (supervisor--cli-has-unknown-flags args)))
    (if unknown
        (supervisor--cli-error supervisor-cli-exit-invalid-args
                               (format "Unknown option: %s" unknown)
                               (if json-p 'json 'human))
      (let* ((snapshot (supervisor--build-snapshot))
             (result (supervisor--cli-all-entries-info snapshot))
             (entries (car result))
             (invalid (cadr result))
             ;; Filter by IDs if specified
             (entries (if args
                          (cl-remove-if-not
                           (lambda (e) (member (alist-get 'id e) args))
                           entries)
                        entries)))
        (supervisor--cli-success
         (if json-p
             (supervisor--cli-status-json entries invalid)
           (supervisor--cli-status-human entries invalid))
         (if json-p 'json 'human))))))

(defun supervisor--cli-cmd-list (args json-p)
  "Handle `list' command with ARGS.  JSON-P enables JSON output."
  ;; list is alias for status
  (supervisor--cli-cmd-status args json-p))

(defun supervisor--cli-cmd-describe (args json-p)
  "Handle `describe ID' command with ARGS.  JSON-P enables JSON output."
  (cond
   ((null args)
    (supervisor--cli-error supervisor-cli-exit-invalid-args
                           "describe requires an ID argument"
                           (if json-p 'json 'human)))
   ((cdr args)
    (supervisor--cli-error supervisor-cli-exit-invalid-args
                           (format "describe takes exactly one ID, got: %s"
                                   (mapconcat #'identity args " "))
                           (if json-p 'json 'human)))
   (t
    (let* ((id (car args))
           (snapshot (supervisor--build-snapshot))
           (result (supervisor--cli-all-entries-info snapshot))
           (entries (car result))
           (info (cl-find id entries :key (lambda (e) (alist-get 'id e)) :test #'equal)))
      (if info
          (supervisor--cli-success
           (if json-p
               (json-encode (supervisor--cli-entry-to-json-obj info))
             (supervisor--cli-describe-human info))
           (if json-p 'json 'human))
        (supervisor--cli-error supervisor-cli-exit-failure
                               (format "No entry with ID '%s'" id)
                               (if json-p 'json 'human)))))))

(defun supervisor--cli-cmd-validate (args json-p)
  "Handle `validate' command with ARGS.  JSON-P enables JSON output."
  (let ((extra-err (supervisor--cli-reject-extra-args args json-p)))
    (if extra-err extra-err
      (let* ((plan (supervisor--build-plan supervisor-programs))
             (valid-count (length (supervisor-plan-entries plan)))
             (invalid-hash (supervisor-plan-invalid plan))
             (invalid-list nil))
        (maphash (lambda (id reason)
                   (push `((id . ,id) (reason . ,reason)) invalid-list))
                 invalid-hash)
        ;; Sort invalid list by ID for deterministic output
        (setq invalid-list (sort invalid-list
                                 (lambda (a b)
                                   (string< (alist-get 'id a) (alist-get 'id b)))))
        (let ((invalid-count (length invalid-list)))
          (if json-p
              (supervisor--cli-make-result
               (if (> invalid-count 0) supervisor-cli-exit-validation-failed
                 supervisor-cli-exit-success)
               'json
               (json-encode `((valid . ,valid-count)
                              (invalid . ,invalid-count)
                              (errors . ,(supervisor--cli-ensure-array
                                          (mapcar #'supervisor--cli-invalid-to-json-obj
                                                  invalid-list))))))
            (supervisor--cli-make-result
             (if (> invalid-count 0) supervisor-cli-exit-validation-failed
               supervisor-cli-exit-success)
             'human
             (concat (format "Validation: %d valid, %d invalid\n" valid-count invalid-count)
                     (mapconcat (lambda (e)
                                  (format "INVALID %s: %s\n"
                                          (alist-get 'id e)
                                          (alist-get 'reason e)))
                                invalid-list "")))))))))

(defun supervisor--cli-cmd-start (args json-p)
  "Handle `start [-- ID...]' command with ARGS.  JSON-P enables JSON output.
Use -- before IDs that start with a hyphen."
  (let ((unknown (supervisor--cli-has-unknown-flags args)))
    (if unknown
        (supervisor--cli-error supervisor-cli-exit-invalid-args
                               (format "Unknown option: %s" unknown)
                               (if json-p 'json 'human))
      (let ((args (supervisor--cli-strip-separator args)))
        (cond
         (args
      ;; Start specific entries
      (let ((started nil)
            (errors nil))
        (dolist (id args)
          (let ((entry (supervisor--get-entry-for-id id)))
            (if entry
                (progn
                  (supervisor--start-entry-async entry nil)
                  (push id started))
              (push id errors))))
        (let ((msg (concat
                    (when started
                      (format "Started: %s\n" (mapconcat #'identity (nreverse started) ", ")))
                    (when errors
                      (format "Not found: %s\n" (mapconcat #'identity errors ", "))))))
          (if errors
              (supervisor--cli-make-result supervisor-cli-exit-failure
                                           (if json-p 'json 'human)
                                           (if json-p
                                               (json-encode `((started . ,(nreverse started))
                                                              (errors . ,errors)))
                                             msg))
            (supervisor--cli-success
             (if json-p
                 (json-encode `((started . ,(nreverse started))))
               msg)
             (if json-p 'json 'human))))))
         (t
          ;; Start all via supervisor-start
          (supervisor-start)
          (supervisor--cli-success
           (if json-p
               (json-encode '((message . "Supervisor started")))
             "Supervisor started\n")
           (if json-p 'json 'human))))))))

(defun supervisor--cli-cmd-stop (args json-p)
  "Handle `stop [-- ID...]' command with ARGS.  JSON-P enables JSON output.
Use -- before IDs that start with a hyphen."
  (let ((unknown (supervisor--cli-has-unknown-flags args)))
    (if unknown
        (supervisor--cli-error supervisor-cli-exit-invalid-args
                               (format "Unknown option: %s" unknown)
                               (if json-p 'json 'human))
      (let ((args (supervisor--cli-strip-separator args)))
        (cond
         (args
          ;; Stop specific entries
          (let ((stopped nil)
                (errors nil))
            (dolist (id args)
              (let ((proc (gethash id supervisor--processes)))
                (if (and proc (process-live-p proc))
                    (progn
                      (delete-process proc)
                      (push id stopped))
                  (push id errors))))
            (let ((msg (concat
                        (when stopped
                          (format "Stopped: %s\n" (mapconcat #'identity (nreverse stopped) ", ")))
                        (when errors
                          (format "Not running: %s\n" (mapconcat #'identity errors ", "))))))
              (supervisor--cli-success
               (if json-p
                   (json-encode `((stopped . ,(nreverse stopped))
                                  (not_running . ,errors)))
                 msg)
               (if json-p 'json 'human)))))
         (t
          ;; Stop all via supervisor-stop-now (synchronous for CLI)
          (supervisor-stop-now)
          (supervisor--cli-success
           (if json-p
               (json-encode '((message . "Supervisor stopped")))
             "Supervisor stopped\n")
           (if json-p 'json 'human))))))))

(defun supervisor--cli-cmd-restart (args json-p)
  "Handle `restart [-- ID...]' command with ARGS.  JSON-P enables JSON output.
Use -- before IDs that start with a hyphen."
  (let ((unknown (supervisor--cli-has-unknown-flags args)))
    (if unknown
        (supervisor--cli-error supervisor-cli-exit-invalid-args
                               (format "Unknown option: %s" unknown)
                               (if json-p 'json 'human))
      (let ((args (supervisor--cli-strip-separator args)))
        (cond
         (args
          ;; Restart specific entries
          (let ((restarted nil)
                (errors nil))
            (dolist (id args)
              (let* ((proc (gethash id supervisor--processes))
                     (entry (supervisor--get-entry-for-id id)))
                (cond
                 ((not entry)
                  (push id errors))
                 ((and proc (process-live-p proc))
                  ;; Kill then restart
                  (delete-process proc)
                  (supervisor--start-entry-async entry nil)
                  (push id restarted))
                 (t
                  ;; Just start
                  (supervisor--start-entry-async entry nil)
                  (push id restarted)))))
            (let ((msg (concat
                        (when restarted
                          (format "Restarted: %s\n" (mapconcat #'identity (nreverse restarted) ", ")))
                        (when errors
                          (format "Not found: %s\n" (mapconcat #'identity errors ", "))))))
              (if errors
                  (supervisor--cli-make-result supervisor-cli-exit-failure
                                               (if json-p 'json 'human)
                                               (if json-p
                                                   (json-encode `((restarted . ,(nreverse restarted))
                                                                  (errors . ,errors)))
                                                 msg))
                (supervisor--cli-success
                 (if json-p
                     (json-encode `((restarted . ,(nreverse restarted))))
                   msg)
                 (if json-p 'json 'human))))))
         (t
          ;; Restart all: stop then start
          (supervisor-stop-now)
          (supervisor-start)
          (supervisor--cli-success
           (if json-p
               (json-encode '((message . "Supervisor restarted")))
             "Supervisor restarted\n")
           (if json-p 'json 'human))))))))

(defun supervisor--cli-cmd-reload (args json-p)
  "Handle `reload' command with ARGS.  JSON-P enables JSON output."
  (let ((extra-err (supervisor--cli-reject-extra-args args json-p)))
    (if extra-err extra-err
      (progn
        (supervisor-reload)
        (supervisor--cli-success
         (if json-p
             (json-encode '((message . "Reload complete")))
           "Reload complete\n")
         (if json-p 'json 'human))))))

(defun supervisor--cli-cmd-enable (args json-p)
  "Handle `enable [--] ID...' command with ARGS.  JSON-P enables JSON output.
Use -- before IDs that start with a hyphen."
  (let ((unknown (supervisor--cli-has-unknown-flags args)))
    (if unknown
        (supervisor--cli-error supervisor-cli-exit-invalid-args
                               (format "Unknown option: %s" unknown)
                               (if json-p 'json 'human))
      (let ((args (supervisor--cli-strip-separator args)))
        (cond
         ((null args)
          (supervisor--cli-error supervisor-cli-exit-invalid-args
                                 "enable requires at least one ID"
                                 (if json-p 'json 'human)))
         (t
          (dolist (id args)
            (puthash id 'enabled supervisor--enabled-override))
          (supervisor--save-overrides)
          (supervisor--cli-success
           (if json-p
               (json-encode `((enabled . ,args)))
             (format "Enabled: %s\n" (mapconcat #'identity args ", ")))
           (if json-p 'json 'human))))))))

(defun supervisor--cli-cmd-disable (args json-p)
  "Handle `disable [--] ID...' command with ARGS.  JSON-P enables JSON output.
Use -- before IDs that start with a hyphen."
  (let ((unknown (supervisor--cli-has-unknown-flags args)))
    (if unknown
        (supervisor--cli-error supervisor-cli-exit-invalid-args
                               (format "Unknown option: %s" unknown)
                               (if json-p 'json 'human))
      (let ((args (supervisor--cli-strip-separator args)))
        (cond
         ((null args)
          (supervisor--cli-error supervisor-cli-exit-invalid-args
                                 "disable requires at least one ID"
                                 (if json-p 'json 'human)))
         (t
          (dolist (id args)
            (puthash id 'disabled supervisor--enabled-override))
          (supervisor--save-overrides)
          (supervisor--cli-success
           (if json-p
               (json-encode `((disabled . ,args)))
             (format "Disabled: %s\n" (mapconcat #'identity args ", ")))
           (if json-p 'json 'human))))))))

(defun supervisor--cli-cmd-restart-policy (args json-p)
  "Handle `restart-policy (on|off) [--] ID...' with ARGS.  JSON-P for JSON.
Use -- before IDs that start with a hyphen."
  (let ((unknown (supervisor--cli-has-unknown-flags args)))
    (if unknown
        (supervisor--cli-error supervisor-cli-exit-invalid-args
                               (format "Unknown option: %s" unknown)
                               (if json-p 'json 'human))
      (let ((args (supervisor--cli-strip-separator args)))
        (cond
         ((< (length args) 2)
          (supervisor--cli-error supervisor-cli-exit-invalid-args
                                 "restart-policy requires (on|off) and at least one ID"
                                 (if json-p 'json 'human)))
         (t
          (let* ((policy (car args))
                 (ids (cdr args))
                 (value (cond ((equal policy "on") 'enabled)
                              ((equal policy "off") 'disabled)
                              (t nil))))
            (if (null value)
                (supervisor--cli-error supervisor-cli-exit-invalid-args
                                       "restart-policy requires 'on' or 'off'"
                                       (if json-p 'json 'human))
              (dolist (id ids)
                (puthash id value supervisor--restart-override))
              (supervisor--save-overrides)
              (supervisor--cli-success
               (if json-p
                   (json-encode `((restart_policy . ,policy) (ids . ,ids)))
                 (format "Restart policy %s: %s\n" policy (mapconcat #'identity ids ", ")))
               (if json-p 'json 'human))))))))))

(defun supervisor--cli-cmd-logging (args json-p)
  "Handle `logging (on|off) [--] ID...' with ARGS.  JSON-P for JSON.
Use -- before IDs that start with a hyphen."
  (let ((unknown (supervisor--cli-has-unknown-flags args)))
    (if unknown
        (supervisor--cli-error supervisor-cli-exit-invalid-args
                               (format "Unknown option: %s" unknown)
                               (if json-p 'json 'human))
      (let ((args (supervisor--cli-strip-separator args)))
        (cond
         ((< (length args) 2)
          (supervisor--cli-error supervisor-cli-exit-invalid-args
                                 "logging requires (on|off) and at least one ID"
                                 (if json-p 'json 'human)))
         (t
          (let* ((policy (car args))
                 (ids (cdr args))
                 (value (cond ((equal policy "on") 'enabled)
                              ((equal policy "off") 'disabled)
                              (t nil))))
            (if (null value)
                (supervisor--cli-error supervisor-cli-exit-invalid-args
                                       "logging requires 'on' or 'off'"
                                       (if json-p 'json 'human))
              (dolist (id ids)
                (puthash id value supervisor--logging))
              (supervisor--save-overrides)
              (supervisor--cli-success
               (if json-p
                   (json-encode `((logging . ,policy) (ids . ,ids)))
                 (format "Logging %s: %s\n" policy (mapconcat #'identity ids ", ")))
               (if json-p 'json 'human))))))))))

(defun supervisor--cli-cmd-blame (args json-p)
  "Handle `blame' command with ARGS.  JSON-P enables JSON output."
  (let ((extra-err (supervisor--cli-reject-extra-args args json-p)))
    (if extra-err extra-err
      (let ((blame-data nil))
        (maphash (lambda (id start-time)
                   (let ((ready-time (gethash id supervisor--ready-times)))
                     (push `((id . ,id)
                             (start_time . ,start-time)
                             (ready_time . ,(or ready-time :null))
                             (duration . ,(if ready-time (- ready-time start-time) :null)))
                           blame-data)))
                 supervisor--start-times)
        ;; Sort by start time for deterministic output
        (setq blame-data (sort blame-data
                               (lambda (a b)
                                 (< (alist-get 'start_time a)
                                    (alist-get 'start_time b)))))
        (supervisor--cli-success
         (if json-p
             (json-encode `((blame . ,(supervisor--cli-ensure-array blame-data))))
           (concat
            (format "%-20s %-18s %-18s %s\n" "ID" "START" "READY" "DURATION")
            (make-string 70 ?-)
            "\n"
            (mapconcat (lambda (e)
                         (format "%-20s %-18.3f %-18s %s\n"
                                 (alist-get 'id e)
                                 (alist-get 'start_time e)
                                 (let ((r (alist-get 'ready_time e)))
                                   (if (eq r :null) "-" (format "%.3f" r)))
                                 (let ((d (alist-get 'duration e)))
                                   (if (eq d :null) "-" (format "%.3fs" d)))))
                       blame-data "")))
         (if json-p 'json 'human))))))

(defun supervisor--cli-cmd-graph (args json-p)
  "Handle `graph [ID]' command with ARGS.  JSON-P enables JSON output."
  ;; Check for extra args (graph takes at most 1 ID)
  (cond
   ((> (length args) 1)
    (supervisor--cli-error supervisor-cli-exit-invalid-args
                           (format "graph takes at most one ID, got: %s"
                                   (mapconcat #'identity args " "))
                           (if json-p 'json 'human)))
   ((and args (string-prefix-p "-" (car args)))
    (supervisor--cli-error supervisor-cli-exit-invalid-args
                           (format "Unknown option: %s" (car args))
                           (if json-p 'json 'human)))
   (t
    (let* ((plan (supervisor--build-plan supervisor-programs))
           (deps (supervisor-plan-deps plan))
           (requires (supervisor-plan-requires-deps plan))
           (dependents (supervisor-plan-dependents plan))
           (id (car args)))
      (if id
          ;; Graph for single ID
          (let ((after-deps (gethash id deps))
                (req-deps (gethash id requires))
                (blocks (gethash id dependents)))
            (supervisor--cli-success
             (if json-p
                 (json-encode `((id . ,id)
                                (after . ,(supervisor--cli-ensure-array after-deps))
                                (requires . ,(supervisor--cli-ensure-array req-deps))
                                (blocks . ,(supervisor--cli-ensure-array blocks))))
               (concat
                (format "ID: %s\n" id)
                (format "After: %s\n" (if after-deps (mapconcat #'identity after-deps ", ") "none"))
                (format "Requires: %s\n" (if req-deps (mapconcat #'identity req-deps ", ") "none"))
                (format "Blocks: %s\n" (if blocks (mapconcat #'identity blocks ", ") "none"))))
             (if json-p 'json 'human)))
        ;; Full graph
        (let ((edges nil))
          (maphash (lambda (id dep-list)
                     (dolist (dep dep-list)
                       (push `(,dep ,id) edges)))
                   dependents)
          ;; Sort edges for deterministic output
          (setq edges (sort edges
                            (lambda (a b)
                              (or (string< (car a) (car b))
                                  (and (string= (car a) (car b))
                                       (string< (cadr a) (cadr b)))))))
          (supervisor--cli-success
           (if json-p
               (json-encode `((edges . ,(supervisor--cli-ensure-array edges))))
             (concat "Dependency Graph (dep -> dependent):\n"
                     (mapconcat (lambda (e)
                                  (format "  %s -> %s\n" (car e) (cadr e)))
                                edges "")))
           (if json-p 'json 'human))))))))

(defun supervisor--cli-cmd-logs (args json-p)
  "Handle `logs [--] ID [--tail N]' command with ARGS.  JSON-P for JSON.
Use -- before IDs that start with a hyphen."
  ;; Check for unknown flags (only --tail is valid, stops at --)
  (let* ((unknown (supervisor--cli-has-unknown-flags args '("--tail")))
         (has-separator (member "--" args)))
    (cond
     (unknown
      (supervisor--cli-error supervisor-cli-exit-invalid-args
                             (format "Unknown option: %s" unknown)
                             (if json-p 'json 'human)))
     (t
      (let ((args (supervisor--cli-strip-separator args)))
        (cond
         ((null args)
          (supervisor--cli-error supervisor-cli-exit-invalid-args
                                 "logs requires an ID argument"
                                 (if json-p 'json 'human)))
         ;; Only check for option-as-first-arg if no -- was used
         ((and (not has-separator) (string-prefix-p "-" (car args)))
          (supervisor--cli-error supervisor-cli-exit-invalid-args
                                 "logs requires ID as first argument"
                                 (if json-p 'json 'human)))
         (t
          (let* ((id (car args))
                 (tail-arg (member "--tail" args))
                 (tail-n (if (and tail-arg (cadr tail-arg))
                             (string-to-number (cadr tail-arg))
                           50))
                 (log-file (supervisor--log-file id)))
            (if (file-exists-p log-file)
                (let ((content (with-temp-buffer
                                 (insert-file-contents log-file)
                                 (let ((lines (split-string (buffer-string) "\n" t)))
                                   (if (> (length lines) tail-n)
                                       (mapconcat #'identity (last lines tail-n) "\n")
                                     (buffer-string))))))
                  (supervisor--cli-success
                   (if json-p
                       (json-encode `((id . ,id) (log_file . ,log-file) (content . ,content)))
                     (concat (format "=== Log for %s (%s) ===\n" id log-file)
                             content "\n"))
                   (if json-p 'json 'human)))
              (supervisor--cli-error supervisor-cli-exit-failure
                                     (format "No log file for '%s'" id)
                                     (if json-p 'json 'human)))))))))))

(defun supervisor--cli-cmd-ping (args json-p)
  "Handle `ping' command with ARGS.  JSON-P enables JSON output."
  (let ((extra-err (supervisor--cli-reject-extra-args args json-p)))
    (if extra-err extra-err
      (supervisor--cli-success
       (if json-p
           (json-encode `((status . "ok") (server . "supervisor")))
         "pong\n")
       (if json-p 'json 'human)))))

(defun supervisor--cli-cmd-version (args json-p)
  "Handle `version' command with ARGS.  JSON-P enables JSON output."
  (let ((extra-err (supervisor--cli-reject-extra-args args json-p)))
    (if extra-err extra-err
      (supervisor--cli-success
       (if json-p
           (json-encode `((version . ,supervisor-cli-version)
                          (emacs . ,emacs-version)))
         (format "supervisorctl %s (Emacs %s)\n" supervisor-cli-version emacs-version))
       (if json-p 'json 'human)))))

(defun supervisor--cli-cmd-kill (args json-p)
  "Handle `kill [--] ID [--signal SIG]' command with ARGS.  JSON-P for JSON.
Use -- before IDs that start with a hyphen."
  ;; Check for unknown flags (only --signal is valid, stops at --)
  (let* ((unknown (supervisor--cli-has-unknown-flags args '("--signal")))
         (has-separator (member "--" args)))
    (cond
     (unknown
      (supervisor--cli-error supervisor-cli-exit-invalid-args
                             (format "Unknown option: %s" unknown)
                             (if json-p 'json 'human)))
     (t
      (let ((args (supervisor--cli-strip-separator args)))
        (cond
         ((null args)
          (supervisor--cli-error supervisor-cli-exit-invalid-args
                                 "kill requires an ID argument"
                                 (if json-p 'json 'human)))
         ;; Only check for option-as-first-arg if no -- was used
         ((and (not has-separator) (string-prefix-p "-" (car args)))
          (supervisor--cli-error supervisor-cli-exit-invalid-args
                                 "kill requires ID as first argument"
                                 (if json-p 'json 'human)))
         (t
          (let* ((id (car args))
                 (sig-arg (member "--signal" args))
                 (sig (if (and sig-arg (cadr sig-arg))
                          (intern (upcase (cadr sig-arg)))
                        'SIGTERM))
                 (proc (gethash id supervisor--processes)))
            (if (and proc (process-live-p proc))
                (progn
                  (signal-process proc sig)
                  (supervisor--cli-success
                   (if json-p
                       (json-encode `((id . ,id) (signal . ,(symbol-name sig))))
                     (format "Sent %s to %s\n" sig id))
                   (if json-p 'json 'human)))
              (supervisor--cli-error supervisor-cli-exit-failure
                                     (format "Process '%s' not running" id)
                                     (if json-p 'json 'human)))))))))))

;;; CLI Dispatcher

(defun supervisor--cli-parse-args (argv)
  "Parse ARGV into (command args json-p).
ARGV is either a string or a list of strings.
Returns a list (COMMAND ARGS JSON-P)."
  (let* ((argv (if (stringp argv)
                   (split-string-and-unquote argv)
                 argv))
         (json-p (member "--json" argv))
         (argv (cl-remove "--json" argv :test #'equal))
         (command (car argv))
         (args (cdr argv)))
    (list command args (if json-p t nil))))

(defun supervisor--cli-dispatch (argv)
  "Dispatch CLI command from ARGV.
ARGV is a list of strings (command and arguments).
Returns a `supervisor-cli-result' struct."
  (let* ((parsed (supervisor--cli-parse-args argv))
         (command (nth 0 parsed))
         (args (nth 1 parsed))
         (json-p (nth 2 parsed)))
    (condition-case err
        (cond
         ((null command)
          (supervisor--cli-success
           (concat "Usage: supervisorctl COMMAND [ARGS...]\n\n"
                   "Commands: status, list, describe, start, stop, restart,\n"
                   "          reload, validate, enable, disable, restart-policy,\n"
                   "          logging, blame, graph, logs, kill, ping, version\n\n"
                   "Options: --json (output as JSON)\n")
           (if json-p 'json 'human)))
         ((equal command "status")
          (supervisor--cli-cmd-status args json-p))
         ((equal command "list")
          (supervisor--cli-cmd-list args json-p))
         ((equal command "describe")
          (supervisor--cli-cmd-describe args json-p))
         ((equal command "validate")
          (supervisor--cli-cmd-validate args json-p))
         ((equal command "start")
          (supervisor--cli-cmd-start args json-p))
         ((equal command "stop")
          (supervisor--cli-cmd-stop args json-p))
         ((equal command "restart")
          (supervisor--cli-cmd-restart args json-p))
         ((equal command "reload")
          (supervisor--cli-cmd-reload args json-p))
         ((equal command "enable")
          (supervisor--cli-cmd-enable args json-p))
         ((equal command "disable")
          (supervisor--cli-cmd-disable args json-p))
         ((equal command "restart-policy")
          (supervisor--cli-cmd-restart-policy args json-p))
         ((equal command "logging")
          (supervisor--cli-cmd-logging args json-p))
         ((equal command "blame")
          (supervisor--cli-cmd-blame args json-p))
         ((equal command "graph")
          (supervisor--cli-cmd-graph args json-p))
         ((equal command "logs")
          (supervisor--cli-cmd-logs args json-p))
         ((equal command "kill")
          (supervisor--cli-cmd-kill args json-p))
         ((equal command "ping")
          (supervisor--cli-cmd-ping args json-p))
         ((equal command "version")
          (supervisor--cli-cmd-version args json-p))
         (t
          (supervisor--cli-error supervisor-cli-exit-invalid-args
                                 (format "Unknown command: %s" command)
                                 (if json-p 'json 'human))))
      (error
       (supervisor--cli-error supervisor-cli-exit-failure
                              (format "Internal error: %s" (error-message-string err))
                              (if json-p 'json 'human))))))

(defun supervisor--cli-dispatch-for-wrapper (argv-list)
  "Dispatch CLI command for external wrapper.
ARGV-LIST is a list of strings passed from the shell wrapper.
Returns a string in format \"EXITCODE:BASE64OUTPUT\" for the wrapper to parse.
Exit code and base64-encoded output are separated by colon.
Base64 encoding avoids escaping issues with newlines and special characters."
  (let ((result (supervisor--cli-dispatch argv-list)))
    (format "%d:%s"
            (supervisor-cli-result-exitcode result)
            (base64-encode-string
             (encode-coding-string (supervisor-cli-result-output result) 'utf-8)
             t))))

(provide 'supervisor)
;;; supervisor.el ends here
