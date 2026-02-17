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

;;; Timer Subsystem Gate
;;
;; The timer subsystem is gated behind this mode variable.
;; The full mode definition is in supervisor-timer.el.

(defvar supervisor-timer-subsystem-mode t
  "Non-nil when the timer subsystem is enabled.
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
(declare-function supervisor--ensure-default-maintenance-units "supervisor-units" ())
(declare-function supervisor--publish-authority-snapshot "supervisor-units" ())
(declare-function supervisor--read-authority-snapshot "supervisor-units" ())
(declare-function supervisor--active-authority-roots "supervisor-units" ())
(declare-function supervisor--unit-file-path "supervisor-units" (id))
(defvar supervisor-unit-directory)
(defvar supervisor-unit-authority-path)

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

(defvar supervisor--builtin-timers
  '((:id "logrotate-daily"
     :target "logrotate"
     :on-calendar (:hour 3 :minute 0)
     :enabled t
     :persistent t)
    (:id "log-prune-daily"
     :target "log-prune"
     :on-calendar (:hour 3 :minute 5)
     :enabled t
     :persistent t))
  "Built-in timer definitions merged with `supervisor-timers'.
A user timer with the same `:id' overrides the built-in one.")

(defcustom supervisor-log-directory
  (expand-file-name "supervisor" user-emacs-directory)
  "Directory for supervisor log files."
  :type 'directory
  :group 'supervisor)

;;;; Log writer and maintenance paths

(defcustom supervisor-logd-command
  (expand-file-name "libexec/supervisor-logd"
                    (file-name-directory (or load-file-name
                                             buffer-file-name "")))
  "Path to the per-service log writer helper.
The helper reads service output from stdin and writes to a log file
with append semantics, enforcing a per-file size cap.  See
`supervisor-logd-max-file-size' for the default cap."
  :type 'string
  :group 'supervisor)

(defcustom supervisor-logrotate-command
  (expand-file-name "sbin/supervisor-logrotate"
                    (file-name-directory (or load-file-name
                                             buffer-file-name "")))
  "Path to the log rotation script.
Rotates active service logs and prunes rotated files older than
`supervisor-logrotate-keep-days'."
  :type 'string
  :group 'supervisor)

(defcustom supervisor-log-prune-command
  (expand-file-name "sbin/supervisor-log-prune"
                    (file-name-directory (or load-file-name
                                             buffer-file-name "")))
  "Path to the global log prune script.
Enforces a hard cap on total bytes in the log directory.
See `supervisor-log-prune-max-total-bytes'."
  :type 'string
  :group 'supervisor)

(defcustom supervisor-logrotate-keep-days 14
  "Number of days to keep rotated log files.
Rotated files older than this are pruned by the logrotate script."
  :type 'integer
  :group 'supervisor)

(defcustom supervisor-logd-max-file-size 52428800
  "Maximum size in bytes for a single log file (default 50 MiB).
The log writer rotates the file locally when this limit is reached."
  :type 'integer
  :group 'supervisor)

(defcustom supervisor-log-prune-max-total-bytes 1073741824
  "Maximum total bytes allowed in the log directory (default 1 GiB).
The prune script deletes oldest rotated files first until the
directory is at or below this cap."
  :type 'integer
  :group 'supervisor)

(defcustom supervisor-logd-prune-min-interval 60
  "Minimum seconds between logd-triggered prune invocations.
Throttles prune calls from the log writer to avoid excessive I/O."
  :type 'integer
  :group 'supervisor)

(defcustom supervisor-logd-pid-directory nil
  "Directory for log writer PID files.
Used by the rotation script to discover writer processes for
reopen signaling.  When nil, defaults to `supervisor-log-directory'."
  :type '(choice directory (const nil))
  :group 'supervisor)

(defcustom supervisor-libexec-build-on-startup 'prompt
  "Control libexec helper builds during `supervisor-start'.
When set to `prompt', startup asks before building missing or stale
helpers.  When set to `automatic', startup builds without prompting.
When set to `never', startup never builds helpers automatically."
  :type '(choice (const :tag "Prompt before building" prompt)
                 (const :tag "Build automatically" automatic)
                 (const :tag "Never build on startup" never))
  :group 'supervisor)

(defcustom supervisor-libexec-compiler-candidates
  '("cc" "clang" "gcc")
  "Candidate compiler commands for building libexec helpers.
The first command found in variable `exec-path' is used."
  :type '(repeat string)
  :group 'supervisor)

(defcustom supervisor-libexec-cflags
  '("-Wall" "-Wextra" "-Werror" "-pedantic" "-std=c99" "-O2")
  "C compiler flags for libexec helper builds."
  :type '(repeat string)
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

(defcustom supervisor-oneshot-default-blocking t
  "Default blocking behavior for oneshot processes.
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

(defcustom supervisor-startup-timeout nil
  "Timeout in seconds for startup.
If startup takes longer than this, log a warning and force completion.
Set to nil (default) for no timeout."
  :type '(choice integer (const nil))
  :group 'supervisor)

(defcustom supervisor-max-concurrent-starts nil
  "Maximum number of processes to start concurrently within a stage.
When set, limits parallel process spawning to avoid thundering herd.
Set to nil (default) for unlimited concurrent starts."
  :type '(choice integer (const nil))
  :group 'supervisor)

(defcustom supervisor-default-target "default.target"
  "Target to activate at startup.
The default value \"default.target\" is an alias that resolves
to `supervisor-default-target-link'."
  :type 'string
  :group 'supervisor)

(defcustom supervisor-default-target-link "graphical.target"
  "Target that \"default.target\" resolves to.
Must end in \".target\" and must not be \"default.target\"."
  :type 'string
  :group 'supervisor)

(defcustom supervisor-sandbox-allow-raw-bwrap nil
  "When non-nil, allow per-unit raw bubblewrap arguments.
The `:sandbox-raw-args' unit key is rejected unless this gate is
enabled.  Enabling raw args bypasses the curated profile model and
is intended for expert users only."
  :type 'boolean
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
The log file is `supervisor.log' in the effective log directory.
If `supervisor-log-directory' is not writable, supervisor falls back
to the default user-local log directory.
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

(defvar supervisor--programs-cache)
(defvar supervisor--target-alias-map)

(defun supervisor--effective-programs ()
  "Return the cached program list from the last authority resolution.
Return the result stored in `supervisor--programs-cache' by the most
recent call to `supervisor--load-programs'.  If no cache exists yet,
perform a lazy initialization by loading from disk.  Returns nil
when the units module is not loaded (standalone core mode).

The cache is explicitly refreshed by `supervisor-start' and
`supervisor-daemon-reload' via `supervisor--refresh-programs'.
Other consumers (dashboard, CLI, runtime lookups) read from the
cache without re-reading disk, matching systemd semantics where
unit file changes require an explicit daemon-reload."
  (when (fboundp 'supervisor--load-programs)
    (if (and (boundp 'supervisor--programs-cache)
             (not (eq supervisor--programs-cache :not-yet-loaded)))
        supervisor--programs-cache
      (supervisor--load-programs))))

(defun supervisor--refresh-programs ()
  "Refresh the programs cache by re-reading unit files from disk.
Call this before operations that need a fresh view of disk content,
such as `supervisor-start' and `supervisor-daemon-reload'.  Sets
`supervisor--programs-cache' to the new result."
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
  "Return path to the supervisor-level log file, or nil."
  (when-let* ((log-directory (supervisor--effective-log-directory)))
    (expand-file-name "supervisor.log" log-directory)))

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
      (when-let* ((log-file (supervisor--supervisor-log-file)))
        (let ((timestamp (format-time-string "[%Y-%m-%d %H:%M:%S] ")))
          (condition-case err
              (write-region (concat timestamp msg "\n") nil log-file t 'silent)
            (error
             (supervisor--warn-log-directory
              "cannot write supervisor log file %s: %s"
              log-file (error-message-string err)))))))
    ;; Display message based on level and verbose setting
    (when (or supervisor-verbose
              (memq level '(error warning)))
      (message "%s" msg))))

(defun supervisor--normalize-after (after)
  "Normalize AFTER to a list of ID strings."
  (cond ((null after) nil)
        ((stringp after) (list after))
        ((listp after) after)
        (t nil)))

(defun supervisor--oneshot-blocking-p (plist)
  "Return non-nil if oneshot should block stage completion.
Check PLIST for `:oneshot-blocking', `:oneshot-async', and fall back to default."
  (cond
   ((plist-member plist :oneshot-blocking)
    (plist-get plist :oneshot-blocking))
   ((plist-member plist :oneshot-async)
    (not (plist-get plist :oneshot-async)))
   (t supervisor-oneshot-default-blocking)))

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

(defun supervisor--plist-duplicate-keys (plist)
  "Return a list of duplicate keyword keys in PLIST, or nil.
Scan PLIST and collect any key that appears more than once."
  (let ((seen nil)
        (dupes nil))
    (let ((keys plist))
      (while keys
        (let ((key (car keys)))
          (if (memq key seen)
              (unless (memq key dupes)
                (push key dupes))
            (push key seen)))
        (setq keys (cddr keys))))
    (nreverse dupes)))

;;; Entry Validation

(defconst supervisor--valid-keywords
  '(:id :type :stage :delay :after :requires :enabled :disabled
    :restart :no-restart :logging :oneshot-blocking :oneshot-async :oneshot-timeout :tags
    :stdout-log-file :stderr-log-file
    :working-directory :environment :environment-file
    :exec-stop :exec-reload :restart-sec
    :description :documentation :before :wants
    :kill-signal :kill-mode :remain-after-exit :success-exit-status
    :user :group
    :wanted-by :required-by
    :sandbox-profile :sandbox-network :sandbox-ro-bind :sandbox-rw-bind
    :sandbox-tmpfs :sandbox-raw-args)
  "List of valid keywords for entry plists.")

(defconst supervisor--valid-types '(simple oneshot target)
  "List of valid :type values.")

(defconst supervisor--target-invalid-keywords
  '(:delay :restart :no-restart :logging :stdout-log-file :stderr-log-file
    :oneshot-blocking :oneshot-async :oneshot-timeout
    :working-directory :environment :environment-file
    :exec-stop :exec-reload :restart-sec
    :kill-signal :kill-mode :remain-after-exit :success-exit-status
    :user :group :stage :wanted-by :required-by
    :sandbox-profile :sandbox-network :sandbox-ro-bind :sandbox-rw-bind
    :sandbox-tmpfs :sandbox-raw-args)
  "Keywords invalid for :type target entries.")

(defconst supervisor--valid-restart-policies '(no on-success on-failure always)
  "List of valid restart policy symbols.
`no' means never restart.
`on-success' means restart only on clean exit (exit 0 or clean signal).
`on-failure' means restart only on failure (non-zero exit or unclean signal).
`always' means restart on any exit.")

(defconst supervisor--clean-exit-signals '(1 2 13 15)
  "Signal numbers considered clean exits.
HUP (1), INT (2), PIPE (13), TERM (15).")

(defconst supervisor--simple-only-keywords
  '(:restart :no-restart :exec-stop :exec-reload :restart-sec
    :success-exit-status)
  "Keywords valid only for :type simple.")

(defconst supervisor--oneshot-only-keywords
  '(:oneshot-blocking :oneshot-async :oneshot-timeout :remain-after-exit)
  "Keywords valid only for :type oneshot.")

(defconst supervisor--sandbox-profiles '(none strict service desktop)
  "Valid sandbox profile symbols.
`none' disables sandboxing (default).  `strict' isolates all
namespaces with no host home bind.  `service' is restrictive
filesystem with shared network.  `desktop' extends service with
runtime socket and bus paths.")

(defconst supervisor--sandbox-network-modes '(shared isolated)
  "Valid sandbox network mode symbols.")

(defconst supervisor--sandbox-forbidden-paths '("/proc" "/dev")
  "Paths forbidden in custom sandbox bind and tmpfs lists.")

(defconst supervisor--signal-number-alist
  '((SIGHUP . 1) (SIGINT . 2) (SIGQUIT . 3) (SIGILL . 4) (SIGTRAP . 5)
    (SIGABRT . 6) (SIGBUS . 7) (SIGFPE . 8) (SIGKILL . 9) (SIGUSR1 . 10)
    (SIGSEGV . 11) (SIGUSR2 . 12) (SIGPIPE . 13) (SIGALRM . 14) (SIGTERM . 15)
    (SIGSTKFLT . 16) (SIGCHLD . 17) (SIGCONT . 18) (SIGSTOP . 19)
    (SIGTSTP . 20) (SIGTTIN . 21) (SIGTTOU . 22) (SIGURG . 23)
    (SIGXCPU . 24) (SIGXFSZ . 25) (SIGVTALRM . 26) (SIGPROF . 27)
    (SIGWINCH . 28) (SIGIO . 29) (SIGPWR . 30) (SIGSYS . 31))
  "Mapping of signal name symbols to POSIX signal numbers.
Covers all signals in `supervisor--known-signals'.")

(defun supervisor--signal-to-number (sig)
  "Return the signal number for signal symbol SIG, or nil if unknown."
  (cdr (assq sig supervisor--signal-number-alist)))

(defun supervisor--clean-exit-p (proc-status exit-code
                                              &optional success-exit-status)
  "Return non-nil if PROC-STATUS and EXIT-CODE indicate a clean exit.
A clean exit is exit code 0, or a signal in `supervisor--clean-exit-signals'.
SUCCESS-EXIT-STATUS, if non-nil, is a plist (:codes INTS :signals SYMS)
that extends the clean exit criteria."
  (or (and (eq proc-status 'exit) (eq exit-code 0))
      (and (eq proc-status 'signal)
           (memq exit-code supervisor--clean-exit-signals))
      (and success-exit-status
           (eq proc-status 'exit)
           (memq exit-code (plist-get success-exit-status :codes)))
      (and success-exit-status
           (eq proc-status 'signal)
           (cl-some (lambda (sig)
                      (eq exit-code (supervisor--signal-to-number sig)))
                    (plist-get success-exit-status :signals)))))

(defun supervisor--should-restart-p (policy proc-status exit-code
                                            &optional success-exit-status)
  "Return non-nil if POLICY dictates restart given PROC-STATUS and EXIT-CODE.
POLICY is a restart policy symbol: `always', `no', `on-failure', `on-success'.
Legacy boolean values are accepted: t is treated as `always', nil as `no'.
SUCCESS-EXIT-STATUS, if non-nil, extends the clean exit criteria."
  (let ((p (cond ((eq policy t) 'always)
                 ((eq policy nil) 'no)
                 (t policy))))
    (pcase p
      ('always t)
      ('no nil)
      ('on-failure
       (not (supervisor--clean-exit-p proc-status exit-code success-exit-status)))
      ('on-success
       (supervisor--clean-exit-p proc-status exit-code success-exit-status))
      (_ nil))))

(defun supervisor--normalize-restart-policy (value)
  "Normalize VALUE to a restart policy symbol.
Accepts boolean t/nil and policy symbols.
Returns a member of `supervisor--valid-restart-policies'."
  (cond ((eq value t) 'always)
        ((eq value nil) 'no)
        ((memq value supervisor--valid-restart-policies) value)
        (t value)))

(defun supervisor--restart-policy-to-bool (policy)
  "Convert restart POLICY symbol to boolean for display.
Returns non-nil for any policy other than `no' or nil."
  (and policy (not (eq policy 'no))))

(defun supervisor--cycle-restart-policy (current)
  "Return the next restart policy after CURRENT in the cycle.
Cycles through `no' -> `on-success' -> `on-failure' -> `always' -> `no'."
  (pcase current
    ('no 'on-success)
    ('on-success 'on-failure)
    ('on-failure 'always)
    ('always 'no)
    (_ 'no)))

(defun supervisor--validate-entry (entry)
  "Validate ENTRY configuration.
Return nil if valid, or a reason string if invalid."
  (cond
   ;; String entries: must be non-empty/non-whitespace
   ((stringp entry)
    (when (string-empty-p (string-trim entry))
      "command string must not be empty or whitespace-only"))
   ;; Must be a list; car is command string (or nil for target type)
   ((not (and (listp entry)
              (or (stringp (car entry))
                  (and (null (car entry))
                       (eq (plist-get (cdr entry) :type) 'target)))))
    "entry must be a string or list starting with command string")
   ;; List entry: non-target command must be non-empty/non-whitespace
   ((and (stringp (car entry))
         (string-empty-p (string-trim (car entry)))
         (not (eq (plist-get (cdr entry) :type) 'target)))
    "command must not be empty or whitespace-only")
   (t
    (let ((plist (cdr entry)))
      (cond
       ((not (proper-list-p plist))
        "malformed plist: must be a proper key/value list")
       ((cl-oddp (length plist))
        "malformed plist: odd number of elements (missing value?)")
       (t
        (let* ((type (or (plist-get plist :type) 'simple))
               (errors nil))
          ;; Check for duplicate keys
          (let ((dupes (supervisor--plist-duplicate-keys plist)))
        (dolist (key dupes)
          (push (format "duplicate key %s" key) errors)))
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
      ;; :stage is deprecated
      (when (plist-member plist :stage)
        (push ":stage is removed; use :wanted-by and :required-by to assign services to targets"
              errors))
      ;; Check :id is a valid string when provided
      (when (plist-member plist :id)
        (let ((id (plist-get plist :id)))
          (cond
           ((not (stringp id))
            (push ":id must be a string" errors))
           ((string-empty-p id)
            (push ":id must not be empty" errors))
           ((not (string-match-p "\\`[A-Za-z0-9._:@-]+\\'" id))
            (push ":id contains invalid characters (allowed: A-Z a-z 0-9 . _ : @ -)" errors)))))
      ;; Enforce .target suffix rules
      (let ((eid (or (plist-get plist :id)
                     (when (stringp (car entry))
                       (file-name-nondirectory (car entry))))))
        (when (stringp eid)
          (cond
           ((and (eq type 'target)
                 (not (string-suffix-p ".target" eid)))
            (push ":type target requires ID ending in .target" errors))
           ((and (not (eq type 'target))
                 (string-suffix-p ".target" eid))
            (push "non-target ID must not end in .target" errors)))))
      ;; Check :delay is a non-negative number
      (when (plist-member plist :delay)
        (let ((delay (plist-get plist :delay)))
          (unless (and (numberp delay) (>= delay 0))
            (push ":delay must be a non-negative number" errors))))
      ;; Check :restart value is boolean or valid policy symbol
      (when (plist-member plist :restart)
        (let ((val (plist-get plist :restart)))
          (unless (or (eq val t) (eq val nil)
                      (memq val supervisor--valid-restart-policies))
            (push (format ":restart must be t, nil, or one of %s"
                          supervisor--valid-restart-policies)
                  errors))))
      ;; Check :oneshot-timeout is number or nil
      (when (plist-member plist :oneshot-timeout)
        (let ((timeout (plist-get plist :oneshot-timeout)))
          (unless (or (null timeout) (and (numberp timeout) (> timeout 0)))
            (push ":oneshot-timeout must be a positive number or nil" errors))))
      ;; Strict boolean checks for flag keywords
      (dolist (key '(:enabled :disabled :logging :no-restart
                     :oneshot-blocking :oneshot-async))
        (when (plist-member plist key)
          (let ((val (plist-get plist key)))
            (unless (or (eq val t) (eq val nil))
              (push (format "%s must be t or nil, got %S" key val) errors)))))
      ;; Check stream log target file options
      (dolist (spec '((:stdout-log-file . ":stdout-log-file")
                      (:stderr-log-file . ":stderr-log-file")))
        (when (plist-member plist (car spec))
          (let ((val (plist-get plist (car spec))))
            (unless (or (null val)
                        (and (stringp val)
                             (not (string-empty-p (string-trim val)))))
              (push (format "%s must be a non-empty string or nil"
                            (cdr spec))
                    errors)))))
      ;; Mutually exclusive: :enabled and :disabled
      (when (and (plist-member plist :enabled)
                 (plist-member plist :disabled))
        (push ":enabled and :disabled are mutually exclusive" errors))
      ;; Mutually exclusive: :restart and :no-restart
      (when (and (plist-member plist :restart)
                 (plist-member plist :no-restart))
        (push ":restart and :no-restart are mutually exclusive" errors))
      ;; Mutually exclusive: :oneshot-blocking and :oneshot-async
      (when (and (plist-member plist :oneshot-blocking)
                 (plist-member plist :oneshot-async))
        (push ":oneshot-blocking and :oneshot-async are mutually exclusive" errors))
      ;; Cross-keyword contradiction: :restart-sec with disabled restart
      (when (and (plist-member plist :restart-sec)
                 (plist-get plist :restart-sec))
        (let ((restart-val (plist-get plist :restart))
              (no-restart-val (plist-get plist :no-restart)))
          (when (or (eq no-restart-val t)
                    (eq restart-val 'no)
                    (and (plist-member plist :restart)
                         (eq restart-val nil)))
            (push ":restart-sec is contradictory with disabled restart policy"
                  errors))))
      ;; Type-specific keyword restrictions
      (when (eq type 'oneshot)
        (dolist (kw supervisor--simple-only-keywords)
          (when (plist-member plist kw)
            (push (format "%s is invalid for :type oneshot" kw) errors))))
      (when (eq type 'simple)
        (dolist (kw supervisor--oneshot-only-keywords)
          (when (plist-member plist kw)
            (push (format "%s is invalid for :type simple" kw) errors))))
      (when (eq type 'target)
        (dolist (kw supervisor--target-invalid-keywords)
          (when (plist-member plist kw)
            (push (format "%s is invalid for :type target" kw) errors)))
        ;; Target must not have a non-empty command
        (when (and (listp entry) (stringp (car entry))
                   (not (string-empty-p (string-trim (car entry)))))
          (push "target entry must not have a command" errors))
        ;; Alias targets are passive: no dependency edges allowed
        (let ((eid (or (plist-get plist :id)
                       (when (stringp (car entry))
                         (file-name-nondirectory (car entry))))))
          (when (and (stringp eid)
                     (assoc eid supervisor--target-alias-map))
            (dolist (kw '(:requires :wants :after :before
                          :wanted-by :required-by))
              (when (plist-member plist kw)
                (push (format "alias target \"%s\" is passive and must not have %s"
                              eid kw)
                      errors))))))
      ;; Check :wanted-by and :required-by shape
      (dolist (spec '((:wanted-by . ":wanted-by")
                      (:required-by . ":required-by")))
        (when (plist-member plist (car spec))
          (let ((val (plist-get plist (car spec))))
            (unless (or (null val) (stringp val)
                        (and (proper-list-p val) (cl-every #'stringp val)))
              (push (format "%s must be a string or list of strings"
                            (cdr spec))
                    errors)))))
      ;; Check :tags is a symbol, string, or list of symbols/strings
      (when (plist-member plist :tags)
        (let ((val (plist-get plist :tags)))
          (cond
           ((null val)) ; nil is ok
           ((or (symbolp val) (and (stringp val)
                                   (not (string-empty-p (string-trim val))))))
           ((and (stringp val) (string-empty-p (string-trim val)))
            (push ":tags must not contain empty strings" errors))
           ((proper-list-p val)
            (cond
             ((cl-some #'null val)
              (push ":tags must not contain nil values" errors))
             ((not (cl-every (lambda (item)
                               (or (symbolp item)
                                   (and (stringp item)
                                        (not (string-empty-p
                                              (string-trim item))))))
                             val))
              (if (cl-some (lambda (item)
                             (and (stringp item)
                                  (string-empty-p (string-trim item))))
                           val)
                  (push ":tags must not contain empty strings" errors)
                (push ":tags must be a symbol, string, or list of symbols/strings"
                      errors)))))
           (t
            (push ":tags must be a symbol, string, or list of symbols/strings"
                  errors)))))
      ;; Check :working-directory is a string
      (when (plist-member plist :working-directory)
        (let ((val (plist-get plist :working-directory)))
          (unless (or (null val) (stringp val))
            (push ":working-directory must be a string or nil" errors))))
      ;; Check :environment is an alist of (string . string) pairs
      (when (plist-member plist :environment)
        (let ((val (plist-get plist :environment)))
          (if (not (or (null val)
                       (and (proper-list-p val)
                            (cl-every (lambda (pair)
                                        (and (consp pair)
                                             (stringp (car pair))
                                             (stringp (cdr pair))))
                                      val))))
              (push ":environment must be an alist of (KEY . VALUE) string pairs"
                    errors)
            ;; Shape is valid; check key names and duplicates
            (when val
              (dolist (pair val)
                (unless (string-match-p
                         "\\`[A-Za-z_][A-Za-z0-9_]*\\'" (car pair))
                  (push (format ":environment key %S is not a valid variable name"
                                (car pair))
                        errors)))
              (let ((seen nil))
                (dolist (pair val)
                  (if (member (car pair) seen)
                      (push (format ":environment contains duplicate key %S"
                                    (car pair))
                            errors)
                    (push (car pair) seen))))))))
      ;; Check :environment-file is string, list of strings, or nil
      (when (plist-member plist :environment-file)
        (let ((val (plist-get plist :environment-file)))
          (unless (or (null val)
                      (stringp val)
                      (and (proper-list-p val)
                           (cl-every #'stringp val)))
            (push ":environment-file must be a string, list of strings, or nil" errors))))
      ;; Check :exec-stop is string, list of strings, or nil
      (when (plist-member plist :exec-stop)
        (let ((val (plist-get plist :exec-stop)))
          (if (not (or (null val)
                       (stringp val)
                       (and (proper-list-p val)
                            (cl-every #'stringp val))))
              (push ":exec-stop must be a string, list of strings, or nil" errors)
            ;; Check for empty/whitespace commands
            (let ((cmds (cond ((null val) nil)
                              ((stringp val) (list val))
                              (t val))))
              (when (cl-some (lambda (s)
                               (string-empty-p (string-trim s)))
                             cmds)
                (push ":exec-stop must not contain empty commands" errors))))))
      ;; Check :exec-reload is string, list of strings, or nil
      (when (plist-member plist :exec-reload)
        (let ((val (plist-get plist :exec-reload)))
          (if (not (or (null val)
                       (stringp val)
                       (and (proper-list-p val)
                            (cl-every #'stringp val))))
              (push ":exec-reload must be a string, list of strings, or nil" errors)
            ;; Check for empty/whitespace commands
            (let ((cmds (cond ((null val) nil)
                              ((stringp val) (list val))
                              (t val))))
              (when (cl-some (lambda (s)
                               (string-empty-p (string-trim s)))
                             cmds)
                (push ":exec-reload must not contain empty commands" errors))))))
      ;; Check :restart-sec is a non-negative number or nil
      (when (plist-member plist :restart-sec)
        (let ((val (plist-get plist :restart-sec)))
          (unless (or (null val)
                      (and (numberp val) (>= val 0)))
            (push ":restart-sec must be a non-negative number or nil" errors))))
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
      ;; Check :description is a string or nil
      (when (plist-member plist :description)
        (let ((val (plist-get plist :description)))
          (unless (or (null val) (stringp val))
            (push ":description must be a string or nil" errors))))
      ;; Check :documentation is string, list of strings, or nil
      (when (plist-member plist :documentation)
        (let ((val (plist-get plist :documentation)))
          (unless (or (null val)
                      (stringp val)
                      (and (proper-list-p val)
                           (cl-every #'stringp val)))
            (push ":documentation must be a string, list of strings, or nil" errors))))
      ;; Check :before is string or list of strings
      (when (plist-member plist :before)
        (let ((val (plist-get plist :before)))
          (unless (or (null val)
                      (stringp val)
                      (and (proper-list-p val)
                           (cl-every #'stringp val)))
            (push ":before must be a string or list of strings" errors))))
      ;; Check :wants is string or list of strings
      (when (plist-member plist :wants)
        (let ((val (plist-get plist :wants)))
          (unless (or (null val)
                      (stringp val)
                      (and (proper-list-p val)
                           (cl-every #'stringp val)))
            (push ":wants must be a string or list of strings" errors))))
      ;; Check dependency lists for empty strings and self-references
      (let ((effective-id (if (plist-member plist :id)
                              (plist-get plist :id)
                            (file-name-nondirectory (car entry)))))
        (dolist (dep-spec '((:after . ":after")
                            (:requires . ":requires")
                            (:before . ":before")
                            (:wants . ":wants")
                            (:wanted-by . ":wanted-by")
                            (:required-by . ":required-by")))
          (when (plist-member plist (car dep-spec))
            (let* ((val (plist-get plist (car dep-spec)))
                   (items (cond ((null val) nil)
                                ((stringp val) (list val))
                                ((proper-list-p val) val))))
              (when (cl-some (lambda (s)
                               (and (stringp s)
                                    (string-empty-p (string-trim s))))
                             items)
                (push (format "%s must not contain empty dependency IDs"
                              (cdr dep-spec))
                      errors))
              (when (and effective-id
                         (cl-some (lambda (s) (equal s effective-id))
                                  items))
                (push (format "%s must not reference the entry's own ID"
                              (cdr dep-spec))
                      errors))))))
      ;; Check :kill-signal is a valid signal name
      (when (plist-member plist :kill-signal)
        (let ((val (plist-get plist :kill-signal)))
          (unless (or (null val)
                      (supervisor--normalize-signal-name val))
            (push ":kill-signal must be a valid signal name" errors))))
      ;; Check :kill-mode is process or mixed
      (when (plist-member plist :kill-mode)
        (let ((val (plist-get plist :kill-mode)))
          (unless (or (null val)
                      (supervisor--normalize-kill-mode val))
            (push ":kill-mode must be process or mixed" errors))))
      ;; Check :remain-after-exit is boolean
      (when (plist-member plist :remain-after-exit)
        (let ((val (plist-get plist :remain-after-exit)))
          (unless (or (eq val t) (eq val nil))
            (push ":remain-after-exit must be t or nil" errors))))
      ;; Check :success-exit-status shape: each item must be int or known signal
      (when (plist-member plist :success-exit-status)
        (let* ((val (plist-get plist :success-exit-status))
               (items (cond ((null val) nil)
                            ((listp val) val)
                            (t (list val)))))
          (dolist (item items)
            (cond
             ((integerp item)
              (unless (and (>= item 0) (<= item 255))
                (push (format ":success-exit-status code %d is outside valid range 0-255"
                              item)
                      errors)))
             ((or (symbolp item) (stringp item))
              (unless (supervisor--normalize-signal-name item)
                (push (format ":success-exit-status contains unknown signal %s" item)
                      errors)))
             (t
              (push (format ":success-exit-status item must be int or signal name, got %S" item)
                    errors))))))
      ;; Check :user shape (string, integer, or nil)
      (when (plist-member plist :user)
        (let ((val (plist-get plist :user)))
          (unless (or (null val) (stringp val) (integerp val))
            (push ":user must be a string, integer, or nil" errors))))
      ;; Check :group shape (string, integer, or nil)
      (when (plist-member plist :group)
        (let ((val (plist-get plist :group)))
          (unless (or (null val) (stringp val) (integerp val))
            (push ":group must be a string, integer, or nil" errors))))
      ;; Sandbox validation
      ;; Check :sandbox-profile value
      (when (plist-member plist :sandbox-profile)
        (let ((val (plist-get plist :sandbox-profile)))
          (let ((sym (cond ((symbolp val) val)
                           ((stringp val) (intern val))
                           (t nil))))
            (unless (memq sym supervisor--sandbox-profiles)
              (push (format ":sandbox-profile must be one of %s"
                            supervisor--sandbox-profiles)
                    errors)))))
      ;; Check :sandbox-network value
      (when (plist-member plist :sandbox-network)
        (let ((val (plist-get plist :sandbox-network)))
          (let ((sym (cond ((symbolp val) val)
                           ((stringp val) (intern val))
                           (t nil))))
            (unless (memq sym supervisor--sandbox-network-modes)
              (push (format ":sandbox-network must be one of %s"
                            supervisor--sandbox-network-modes)
                    errors)))))
      ;; Check path-list sandbox keys.  Each spec is (KEY LABEL CHECK-EXISTS).
      ;; Bind sources must exist; tmpfs destinations are created by bwrap.
      (dolist (spec '((:sandbox-ro-bind ":sandbox-ro-bind" t)
                      (:sandbox-rw-bind ":sandbox-rw-bind" t)
                      (:sandbox-tmpfs ":sandbox-tmpfs" nil)))
        (when (plist-member plist (car spec))
          (let ((val (plist-get plist (car spec)))
                (label (nth 1 spec))
                (check-exists (nth 2 spec)))
            (cond
             ((null val))
             ((stringp val)
              (cond
               ((string-empty-p val)
                (push (format "%s must not contain empty paths" label)
                      errors))
               ((not (file-name-absolute-p val))
                (push (format "%s paths must be absolute" label)
                      errors))
               ((member val supervisor--sandbox-forbidden-paths)
                (push (format "%s must not include forbidden path %s"
                              label val)
                      errors))
               ((and check-exists (not (file-exists-p val)))
                (push (format "%s source path does not exist: %s"
                              label val)
                      errors))))
             ((and (proper-list-p val) (cl-every #'stringp val))
              (dolist (path val)
                (cond
                 ((string-empty-p path)
                  (push (format "%s must not contain empty paths" label)
                        errors))
                 ((not (file-name-absolute-p path))
                  (push (format "%s paths must be absolute" label)
                        errors))
                 ((member path supervisor--sandbox-forbidden-paths)
                  (push (format "%s must not include forbidden path %s"
                                label path)
                        errors))
                 ((and check-exists (not (file-exists-p path)))
                  (push (format "%s source path does not exist: %s"
                                label path)
                        errors)))))
             (t
              (push (format "%s must be a string or list of absolute path strings"
                            label)
                    errors))))))
      ;; Check :sandbox-raw-args shape and gate
      (when (plist-member plist :sandbox-raw-args)
        (let ((val (plist-get plist :sandbox-raw-args)))
          (cond
           ((not supervisor-sandbox-allow-raw-bwrap)
            (push ":sandbox-raw-args requires supervisor-sandbox-allow-raw-bwrap to be enabled"
                  errors))
           ((null val))
           ((not (and (proper-list-p val) (cl-every #'stringp val)))
            (push ":sandbox-raw-args must be a list of strings" errors)))))
      ;; Check sandbox-requesting units for OS and binary prerequisites.
      ;; Per plan contract: a unit is sandbox-requesting when
      ;; :sandbox-profile is set to anything other than none, OR any
      ;; other sandbox key is present (plist-member).  Runtime
      ;; supervisor--sandbox-requesting-p uses truthy checks on parsed
      ;; tuples (where nil and absent are indistinguishable), but
      ;; validation is the gating point and operates on plists where
      ;; key presence is distinguishable.
      (let ((sandbox-requesting
             (or (and (plist-member plist :sandbox-profile)
                      (let ((p (plist-get plist :sandbox-profile)))
                        (not (memq (if (stringp p) (intern p) p)
                                   '(nil none)))))
                 (plist-member plist :sandbox-network)
                 (plist-member plist :sandbox-ro-bind)
                 (plist-member plist :sandbox-rw-bind)
                 (plist-member plist :sandbox-tmpfs)
                 (plist-member plist :sandbox-raw-args))))
        (when sandbox-requesting
          (unless (eq system-type 'gnu/linux)
            (push "sandbox is only supported on GNU/Linux" errors))
          (unless (executable-find "bwrap")
            (push "sandbox requires bwrap (bubblewrap) but bwrap is not found in PATH"
                  errors))))
          ;; Return nil if valid, or joined error string
          (when errors
            (mapconcat #'identity (nreverse errors) "; ")))))))))

;; Forward declarations for supervisor-verify and supervisor-dry-run
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
   ;; List with nil car (target entry) - use :id
   ((and (listp entry) (null (car entry))
         (plist-get (cdr entry) :id))
    (plist-get (cdr entry) :id))
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
(defun supervisor-verify ()
  "Verify all unit-file entries and `supervisor-timers'.
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
      (with-output-to-temp-buffer "*supervisor-verify*"
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
    ;; Compute activation closure for dry-run display
    (let ((root-id nil)
          (closure nil)
          (all-sorted (supervisor-plan-by-target plan)))
      (when (supervisor-plan-entries plan)
        (let ((valid-ids (make-hash-table :test 'equal))
              (entries-by-id (make-hash-table :test 'equal)))
          (dolist (entry (supervisor-plan-entries plan))
            (puthash (supervisor-entry-id entry) t valid-ids)
            (puthash (supervisor-entry-id entry) entry entries-by-id))
          (condition-case nil
              (let* ((resolved (supervisor--resolve-startup-root valid-ids))
                     (members (supervisor--materialize-target-members
                               (supervisor-plan-entries plan)))
                     (expanded (supervisor--expand-transaction
                                resolved entries-by-id members
                                (supervisor-plan-order-index plan))))
                (setq root-id resolved)
                (setq closure expanded))
            (user-error nil))))
      ;; Validate timers (only if timer module loaded)
      (let ((timers (when (fboundp 'supervisor-timer-build-list)
                      (supervisor-timer-build-list plan))))
        ;; Display from plan artifact
        (with-output-to-temp-buffer "*supervisor-dry-run*"
          (princ "=== Supervisor Dry Run ===\n\n")
          (princ (format "Services: %d valid, %d invalid\n"
                         (length (supervisor-plan-entries plan))
                         (hash-table-count supervisor--invalid)))
          (when root-id
            (princ (format "Activation root: %s\n" root-id)))
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
          ;; Display activated entries (closure) or full list if no root
          (let ((activated (if closure
                               (cl-remove-if-not
                                (lambda (e)
                                  (gethash (supervisor-entry-id e) closure))
                                all-sorted)
                             all-sorted)))
            (princ (format "--- Activation order (%d entries) ---\n"
                           (length activated)))
            (let ((order 1))
              (dolist (entry activated)
                (let* ((id (supervisor-entry-id entry))
                       (type (supervisor-entry-type entry))
                       (delay (supervisor-entry-delay entry))
                       (enabled-p (supervisor-entry-enabled-p entry))
                       (deps (gethash id (supervisor-plan-deps plan)))
                       (cycle (gethash id (supervisor-plan-cycle-fallback-ids
                                           plan))))
                  (princ (format "  %d. %s [%s]%s%s%s\n"
                                 order id type
                                 (if (not enabled-p) " DISABLED" "")
                                 (if (> delay 0)
                                     (format " delay=%ds" delay) "")
                                 (if cycle " (CYCLE FALLBACK)"
                                   (if deps
                                       (format " after=%s"
                                               (mapconcat #'identity deps ","))
                                     ""))))
                  (cl-incf order)))
              (princ "\n"))
            ;; Show non-activated entries when closure filtering is active
            (when closure
              (let ((not-activated (cl-remove-if
                                    (lambda (e)
                                      (gethash (supervisor-entry-id e)
                                               closure))
                                    all-sorted)))
                (when not-activated
                  (princ (format "--- Not activated (%d entries) ---\n"
                                 (length not-activated)))
                  (dolist (entry not-activated)
                    (princ (format "  %s [%s]\n"
                                   (supervisor-entry-id entry)
                                   (supervisor-entry-type entry))))
                  (princ "\n")))))
          ;; Show timer summary if any
          (when timers
            (princ "--- Timers ---\n")
            (dolist (timer timers)
              (princ (format "  %s -> %s%s\n"
                             (supervisor-timer-id timer)
                             (supervisor-timer-target timer)
                             (if (supervisor-timer-enabled timer)
                                 "" " DISABLED"))))
            (princ "\n"))
          (princ "=== End Dry Run ===\n"))))))

;;; State Variables

(defvar supervisor--processes (make-hash-table :test 'equal)
  "Hash table mapping program names to their process objects.")

(defvar supervisor--restart-override (make-hash-table :test 'equal)
  "Hash table of restart overrides: nil=inherit config.
Values are policy symbols (`always', `no', `on-success', `on-failure')
or legacy values (`enabled', `disabled') which are migrated on read.")

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

(defvar supervisor--writers (make-hash-table :test 'equal)
  "Hash table mapping service ID to writer process for per-service log writers.
Each writer is a `supervisor-logd' subprocess that receives service output
on stdin and handles all disk I/O.")

(defvar supervisor--stderr-writers (make-hash-table :test 'equal)
  "Hash table mapping service ID to dedicated stderr writer processes.
Entries are present only when a unit uses split stdout/stderr log files.")

(defvar supervisor--stderr-pipes (make-hash-table :test 'equal)
  "Hash table mapping service ID to stderr pipe process objects.
These pipe processes receive stderr output from service processes and
forward it to `supervisor--stderr-writers'.")

(defvar supervisor--manually-stopped (make-hash-table :test 'equal)
  "Hash table tracking entries manually stopped via CLI or dashboard.
Entries in this table should not auto-restart until explicitly started again.
This is cleared when an entry is started, allowing normal restart behavior.")

(defvar supervisor--manually-started (make-hash-table :test 'equal)
  "Hash table tracking entries manually started via CLI or dashboard.
Used by reconcile to preserve disabled units that were explicitly started.
Per the systemctl model, `start' on a disabled unit runs it this session
without changing enabled state, and reconcile should not stop it.")

(defvar supervisor--last-exit-info (make-hash-table :test 'equal)
  "Hash table mapping entry IDs to last exit information.
Each value is a plist (:status STATUS :code CODE :timestamp TIME)
where STATUS is `exited', `signal', or `unknown', CODE is the exit
code or signal number, and TIME is the `float-time' of exit.")

;;; Persistent Overrides (Schema v1)

(defcustom supervisor-overrides-file
  (expand-file-name "supervisor/overrides.eld"
                    (or (getenv "XDG_CONFIG_HOME")
                        (expand-file-name ".config" (getenv "HOME"))))
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

(defvar supervisor--default-target-link-override nil
  "Persisted override for default-target-link, or nil.")

(defun supervisor--overrides-file-path ()
  "Return the overrides file path, or nil if persistence is disabled."
  supervisor-overrides-file)

(defun supervisor--ensure-overrides-loaded ()
  "Ensure in-memory overrides are initialized from persistent storage.
Return t when overrides are safe to mutate and save.
If persistence is disabled or file is absent, mark overrides as loaded.
If the file exists but cannot be loaded, return nil."
  (or supervisor--overrides-loaded
      (let ((path (supervisor--overrides-file-path)))
        (cond
         ((null path)
          (setq supervisor--overrides-loaded t)
          t)
         ((not (file-exists-p path))
          (setq supervisor--overrides-loaded t)
          t)
         ((zerop (or (nth 7 (file-attributes path)) 0))
          (setq supervisor--overrides-loaded t)
          t)
         (t
          (supervisor--load-overrides))))))

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
    ;; Prepend default-target-link override if set
    (when supervisor--default-target-link-override
      (push (cons :default-target-link
                  supervisor--default-target-link-override)
            overrides))
    overrides))

(defun supervisor--save-overrides ()
  "Save current overrides to file using atomic write.
Uses temp file + rename pattern for crash safety.
Returns t on success, nil on failure."
  (let ((path (supervisor--overrides-file-path)))
    (when path
      (if (not (supervisor--ensure-overrides-loaded))
          (progn
            (supervisor--log 'warning
                             "Refusing to save overrides; load failed from %s"
                             path)
            nil)
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
                  (insert ";; Schema version: "
                          (number-to-string supervisor-overrides-schema-version)
                          "\n")
                  (pp data (current-buffer)))
                (rename-file temp-file path t)
                t)
            (error
             (supervisor--log 'warning "Failed to save overrides: %s"
                              (error-message-string err))
             (when (file-exists-p temp-file)
               (delete-file temp-file))
             nil)))))))

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
            ;; Extract default-target-link override before per-entry loop
            (when-let* ((link-entry (assq :default-target-link overrides)))
              (when (and (stringp (cdr link-entry))
                         (string-suffix-p ".target" (cdr link-entry)))
                (setq supervisor--default-target-link-override (cdr link-entry)))
              (setq overrides (assq-delete-all :default-target-link overrides)))
            ;; Load overrides into hashes
            (dolist (entry overrides)
              (let ((id (car entry))
                    (plist (cdr entry)))
                (when-let* ((enabled (plist-get plist :enabled)))
                  (puthash id enabled supervisor--enabled-override))
                (when-let* ((restart (plist-get plist :restart)))
                  ;; Migrate legacy enabled/disabled to policy symbols
                  (let ((migrated (cond ((eq restart 'enabled) 'always)
                                        ((eq restart 'disabled) 'no)
                                        (t restart))))
                    (puthash id migrated supervisor--restart-override)))
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

(defun supervisor--clear-all-overrides ()
  "Clear all overrides from memory and file."
  (clrhash supervisor--enabled-override)
  (clrhash supervisor--restart-override)
  (clrhash supervisor--logging)
  (clrhash supervisor--mask-override)
  (setq supervisor--default-target-link-override nil)
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

(defvar supervisor--remain-active (make-hash-table :test 'equal)
  "Hash table tracking oneshot units in active latch state.
Non-nil value means the unit has `:remain-after-exit' t and exited
successfully, so its status is `active' until explicitly stopped.")

(defvar supervisor--oneshot-callbacks (make-hash-table :test 'equal)
  "Hash table of ID -> (callback . timeout-timer) for oneshot blocking.
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

(defvar supervisor--cycle-fallback-ids (make-hash-table :test 'equal)
  "Hash table of entry IDs with deps cleared due to cycle fallback.
When a cycle is detected, :after, :requires, and :wants edges are
all cleared for the affected entries.")

(defvar supervisor--computed-deps (make-hash-table :test 'equal)
  "Hash table of ID -> validated :after list (same-stage, existing deps only).")

(defvar supervisor--entry-state (make-hash-table :test 'equal)
  "Hash table of ID -> state symbol for detailed status.
States: waiting-on-deps, delayed, disabled, pending,
failed-to-spawn, startup-timeout, started.")

(defvar supervisor--spawn-failure-reason (make-hash-table :test 'equal)
  "Hash table of ID -> string with specific spawn failure reason.
Populated when `supervisor--start-process' rejects a launch due to
identity or other precondition failures.  Consulted by
`supervisor--compute-entry-reason' for richer diagnostics.")

;;; Entry Lifecycle FSM (Phase 5)

(defconst supervisor--valid-states
  '(pending waiting-on-deps delayed disabled
    started failed-to-spawn startup-timeout)
  "Valid entry lifecycle states.
- `pending': initial state, not yet started
- `waiting-on-deps': waiting for :after dependencies to complete
- `delayed': waiting for :delay timer to expire
- `disabled': entry is disabled (skipped)
- `started': process successfully spawned
- `failed-to-spawn': process failed to start
- `startup-timeout': startup timed out before entry could start")

(defconst supervisor--state-transitions
  '((nil . (pending waiting-on-deps disabled startup-timeout))
    (pending . (pending waiting-on-deps delayed disabled
                started failed-to-spawn startup-timeout))
    (waiting-on-deps . (waiting-on-deps delayed disabled started
                        failed-to-spawn startup-timeout))
    (delayed . (delayed started failed-to-spawn startup-timeout disabled)))
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
  '(startup-begin startup-complete process-started process-ready
    process-exit process-failed cleanup
    timer-trigger timer-overlap timer-success timer-failure
    target-reached target-degraded)
  "Valid event types for the structured event system.
- `startup-begin': startup begins processing
- `startup-complete': startup finished processing
- `process-started': process successfully spawned
- `process-ready': process became ready (simple=spawned, oneshot=exited)
- `process-exit': supervised process exited
- `process-failed': process failed to spawn
- `cleanup': cleanup phase beginning
- `timer-trigger': timer fired its target oneshot
- `timer-overlap': timer skipped due to target still running
- `timer-success': timer's target completed successfully
- `timer-failure': timer's target failed
- `target-reached': all required members of a target reached terminal state
- `target-degraded': target converged but some required members failed")

(defvar supervisor-event-hook nil
  "Hook run for all supervisor events.
Called with one argument: an event plist with the following keys:
  :type  - event type symbol (see `supervisor--event-types')
  :ts    - timestamp (float-time)
  :id    - entry ID string (for process events, nil for startup/global)
  :stage - stage name symbol (for startup events, nil otherwise)
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
    ;; Log startup transitions to *Messages*
    (pcase type
      ('startup-begin
       (message "Supervisor: starting"))
      ('startup-complete
       (message "Supervisor: complete")))))

;;; DAG Scheduler State

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

(defvar supervisor--dag-complete-callback nil
  "Callback to invoke when all entries are complete.")

(defvar supervisor--dag-timeout-timer nil
  "Timer for startup timeout.")

(defvar supervisor--dag-pending-starts nil
  "Queue of entry IDs waiting to start (for max-concurrent-starts).")

(defvar supervisor--dag-active-starts 0
  "Count of currently starting processes (for max-concurrent-starts).")

(defvar supervisor--target-convergence nil
  "Hash table of target ID to convergence state symbol.
Valid states: converging, reached, degraded.")

(defvar supervisor--target-convergence-reasons nil
  "Hash table of target ID to list of reason strings for degraded.")

(defvar supervisor--target-converging nil
  "Hash table of target ID to t for targets still converging.
Used by `supervisor--dag-check-complete' to block premature completion.")

(defvar supervisor--target-member-reverse nil
  "Hash table of member ID to list of target IDs containing it.
Built from `supervisor--target-members' inverse for convergence callbacks.")

(defvar supervisor--target-members nil
  "Hash table of target ID to plist (:requires (ids) :wants (ids)).
Populated from plan during startup for convergence checks.")

;;; Plan Artifact (Phase 2 Data-Driven Architecture)

(cl-defstruct (supervisor-plan (:constructor supervisor-plan--create))
  "Immutable execution plan for a supervisor session.
This struct represents the validated, deterministic plan computed from
configuration.  It is pure data with no side effects on global state.

Determinism note: All fields except `meta' are deterministic for
identical input.  Within `meta', only `:timestamp' is non-deterministic;
`:version' and `:fingerprint' are deterministic for identical input."
  entries          ; list of parsed valid entries
  invalid          ; hash: id -> reason string
  by-target        ; globally sorted entries list (topological order)
  deps             ; hash: id -> validated :after deps (ordering only)
  requires-deps    ; hash: id -> validated :requires deps (pull-in + ordering)
  dependents       ; hash: id -> list of dependent ids (combined)
  cycle-fallback-ids ; hash: id -> t for entries with cleared edges
  order-index      ; hash: id -> original index for stable ordering
  target-members   ; hash: target-id -> (:requires (ids) :wants (ids))
  activation-root  ; resolved root target ID string, or nil
  activation-closure ; hash: id -> t for entries in the closure, or nil
  meta)            ; plist with :version, :timestamp (non-deterministic)

(defconst supervisor-plan-version 2
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
  type           - Entry type: `simple' (daemon), `oneshot' (run-once),
                   or `target' (grouping unit).  Default: `simple'
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
  stdout-log-file - Optional stdout log file path (string or nil)
                   When nil, stdout uses the default per-service log file.
                   Default: nil
  stderr-log-file - Optional stderr log file path (string or nil)
                   When nil, stderr follows stdout target (merged by default).
                   Default: nil
  after          - Ordering dependencies: list of service IDs (same stage only)
                   These control start ORDER but do not pull in services.
                   Default: nil
  requires       - Requirement dependencies: list of service IDs
                   These PULL IN services and also imply ordering.
                   Cross-stage requires cause an error (must use after).
                   Default: nil
  oneshot-blocking - For oneshots: block stage completion until exit (boolean)
                   Default: value of `supervisor-oneshot-default-blocking'
  oneshot-timeout - For oneshots: timeout in seconds, or nil for infinite
                   Default: value of `supervisor-oneshot-timeout'
  tags           - List of symbols/strings for filtering
                   Default: nil
  working-directory - Working directory for the process (absolute path or nil)
                   Default: nil
  environment    - Ordered alist of (KEY . VALUE) string pairs, or nil
                   Default: nil
  environment-file - Ordered list of env-file paths, or nil
                   Default: nil
  exec-stop      - Ordered list of stop command strings, simple only, or nil
                   Default: nil
  exec-reload    - Ordered list of reload command strings, simple only, or nil
                   Default: nil
  restart-sec    - Per-unit restart delay in seconds, simple only, or nil
                   Default: nil (uses global `supervisor-restart-delay')
  description    - Human-readable one-line description or nil
                   Default: nil
  documentation  - List of documentation URIs/paths, or nil
                   Default: nil
  before         - Before-ordering deps: list of IDs (same stage only)
                   Inverted into :after edges during scheduling.
                   Default: nil
  wants          - Soft dependencies: list of IDs (same stage only)
                   Missing/disabled/failed wanted units do not block.
                   Default: nil
  kill-signal    - Graceful stop signal symbol (e.g., `SIGTERM'), or nil
                   Default: nil (uses SIGTERM)
  kill-mode      - Kill mode symbol: `process' or `mixed', or nil
                   Default: nil (uses process mode)
  remain-after-exit - Oneshot active latch (boolean), oneshot only
                   Default: nil
  success-exit-status - Extra success criteria plist (:codes :signals)
                   Simple only.  Default: nil
  wanted-by      - List of target IDs this service belongs to (soft)
                   Default: nil
  required-by    - List of target IDs this service is required by
                   Default: nil
  sandbox-profile - Sandbox profile symbol: none, strict, service, desktop
                   Default: nil (no sandbox)
  sandbox-network - Network mode symbol: shared or isolated
                   Default: nil (profile default)
  sandbox-ro-bind - List of read-only bind mount absolute paths
                   Default: nil
  sandbox-rw-bind - List of read-write bind mount absolute paths
                   Default: nil
  sandbox-tmpfs  - List of tmpfs mount absolute paths
                   Default: nil
  sandbox-raw-args - List of raw bwrap argument strings (expert gate)
                   Default: nil"
  (id nil :type string :documentation "Unique identifier (required)")
  (command nil :type string :documentation "Shell command to execute (required)")
  (type 'simple :type symbol :documentation "Process type: simple or oneshot")
  (stage 'stage3 :type symbol :documentation "Startup stage")
  (delay 0 :type number :documentation "Delay before starting (seconds)")
  (enabled t :type boolean :documentation "Whether to start this service")
  (restart 'always :type symbol :documentation "Restart policy: always, no, on-success, on-failure")
  (logging t :type boolean :documentation "Log stdout/stderr to file")
  (stdout-log-file nil :type (or null string)
                   :documentation "Optional stdout log file path")
  (stderr-log-file nil :type (or null string)
                   :documentation "Optional stderr log file path")
  (after nil :type list :documentation "Ordering dependencies (same stage)")
  (requires nil :type list :documentation "Requirement dependencies")
  (oneshot-blocking nil :type boolean :documentation "Block stage for oneshot exit")
  (oneshot-timeout nil :type (or null number) :documentation "Oneshot timeout")
  (tags nil :type list :documentation "Filter tags")
  (working-directory nil :type (or null string) :documentation "Process working directory")
  (environment nil :type list :documentation "Environment alist ((KEY . VALUE) ...)")
  (environment-file nil :type list :documentation "Environment file paths")
  (exec-stop nil :type list :documentation "Stop command list (simple only)")
  (exec-reload nil :type list :documentation "Reload command list (simple only)")
  (restart-sec nil :type (or null number) :documentation "Per-unit restart delay")
  (description nil :type (or null string) :documentation "Human-readable description")
  (documentation nil :type list :documentation "Documentation URI/path list")
  (before nil :type list :documentation "Before-ordering dependencies")
  (wants nil :type list :documentation "Soft dependencies (same stage)")
  (kill-signal nil :type (or null symbol) :documentation "Graceful stop signal")
  (kill-mode nil :type (or null symbol) :documentation "Kill mode: process or mixed")
  (remain-after-exit nil :type boolean :documentation "Oneshot active latch")
  (success-exit-status nil :type list :documentation "Extra success criteria plist")
  (user nil :type (or null string integer) :documentation "Run-as user (root only)")
  (group nil :type (or null string integer) :documentation "Run-as group (root only)")
  (wanted-by nil :type list :documentation "Target IDs this service belongs to (soft)")
  (required-by nil :type list :documentation "Target IDs this service is required by")
  (sandbox-profile nil :type (or null symbol) :documentation "Sandbox profile symbol")
  (sandbox-network nil :type (or null symbol) :documentation "Sandbox network mode")
  (sandbox-ro-bind nil :type list :documentation "Read-only bind mount paths")
  (sandbox-rw-bind nil :type list :documentation "Read-write bind mount paths")
  (sandbox-tmpfs nil :type list :documentation "Tmpfs mount paths")
  (sandbox-raw-args nil :type list :documentation "Raw bwrap argument list"))

(defconst supervisor-service-required-fields '(id command)
  "List of required fields in a service record.")

(defconst supervisor-service-optional-fields
  '((type . simple)
    (stage . stage3)
    (delay . 0)
    (enabled . t)
    (restart . always)
    (logging . t)
    (stdout-log-file . nil)
    (stderr-log-file . nil)
    (after . nil)
    (requires . nil)
    (oneshot-blocking . :defer)  ; resolved at runtime from global default
    (oneshot-timeout . :defer)
    (tags . nil)
    (working-directory . nil)
    (environment . nil)
    (environment-file . nil)
    (exec-stop . nil)
    (exec-reload . nil)
    (restart-sec . nil)
    (description . nil)
    (documentation . nil)
    (before . nil)
    (wants . nil)
    (kill-signal . nil)
    (kill-mode . nil)
    (remain-after-exit . nil)
    (success-exit-status . nil)
    (user . nil)
    (group . nil)
    (wanted-by . nil)
    (required-by . nil)
    (sandbox-profile . nil)
    (sandbox-network . nil)
    (sandbox-ro-bind . nil)
    (sandbox-rw-bind . nil)
    (sandbox-tmpfs . nil)
    (sandbox-raw-args . nil))
  "Alist of optional fields with their default values.
A value of :defer means the default is resolved at runtime.")

;;; Parsed Entry Accessors (Schema v1)
;;
;; These functions abstract the internal tuple representation.
;; Use these instead of direct (nth N entry) indexing for maintainability.
;; The tuple format is:
;;   (id cmd delay enabled-p restart-policy logging-p
;;    stdout-log-file stderr-log-file
;;    type stage after
;;    oneshot-blocking oneshot-timeout tags requires
;;    working-directory environment environment-file
;;    exec-stop exec-reload restart-sec
;;    description documentation before wants
;;    kill-signal kill-mode remain-after-exit success-exit-status
;;    user group wanted-by required-by
;;    sandbox-profile sandbox-network sandbox-ro-bind sandbox-rw-bind
;;    sandbox-tmpfs sandbox-raw-args)

(defun supervisor-entry-id (entry)
  "Return the ID of parsed ENTRY."
  (nth 0 entry))

(defun supervisor-entry-command (entry)
  "Return the command of parsed ENTRY."
  (nth 1 entry))

(defun supervisor-entry-delay (entry)
  "Return the delay in seconds of parsed ENTRY."
  (nth 2 entry))

(defun supervisor-entry-enabled-p (entry)
  "Return non-nil if parsed ENTRY is enabled."
  (nth 3 entry))

(defun supervisor-entry-restart-p (entry)
  "Return non-nil if parsed ENTRY has any restart policy other than `no'."
  (not (eq (nth 4 entry) 'no)))

(defun supervisor-entry-restart-policy (entry)
  "Return the restart policy symbol of parsed ENTRY.
Value is one of `always', `no', `on-success', or `on-failure'."
  (nth 4 entry))

(defun supervisor-entry-logging-p (entry)
  "Return non-nil if parsed ENTRY should log to file."
  (nth 5 entry))

(defun supervisor-entry-stdout-log-file (entry)
  "Return the stdout log file path for parsed ENTRY, or nil."
  (and (>= (length entry) 31)
       (nth 6 entry)))

(defun supervisor-entry-stderr-log-file (entry)
  "Return the stderr log file path for parsed ENTRY, or nil."
  (and (>= (length entry) 31)
       (nth 7 entry)))

(defun supervisor-entry-type (entry)
  "Return the type (`simple', `oneshot', or `target') of parsed ENTRY."
  (nth (if (>= (length entry) 31) 8 6) entry))

(defun supervisor-entry-stage (entry)
  "Return the stage symbol of parsed ENTRY."
  (nth (if (>= (length entry) 31) 9 7) entry))

(defun supervisor-entry-after (entry)
  "Return the ordering dependencies (after) of parsed ENTRY."
  (nth (if (>= (length entry) 31) 10 8) entry))

(defun supervisor-entry-oneshot-blocking (entry)
  "Return non-nil if oneshot ENTRY blocks stage completion."
  (nth (if (>= (length entry) 31) 11 9) entry))

(defun supervisor-entry-oneshot-timeout (entry)
  "Return the timeout in seconds for oneshot ENTRY, or nil."
  (nth (if (>= (length entry) 31) 12 10) entry))

(defun supervisor-entry-tags (entry)
  "Return the tags list of parsed ENTRY."
  (nth (if (>= (length entry) 31) 13 11) entry))

(defun supervisor-entry-requires (entry)
  "Return the requirement dependencies of parsed ENTRY."
  (nth (if (>= (length entry) 31) 14 12) entry))

(defun supervisor-entry-working-directory (entry)
  "Return the working directory of parsed ENTRY, or nil."
  (nth (if (>= (length entry) 31) 15 13) entry))

(defun supervisor-entry-environment (entry)
  "Return the environment alist of parsed ENTRY, or nil."
  (nth (if (>= (length entry) 31) 16 14) entry))

(defun supervisor-entry-environment-file (entry)
  "Return the environment file list of parsed ENTRY, or nil."
  (nth (if (>= (length entry) 31) 17 15) entry))

(defun supervisor-entry-exec-stop (entry)
  "Return the stop command list of parsed ENTRY, or nil."
  (nth (if (>= (length entry) 31) 18 16) entry))

(defun supervisor-entry-exec-reload (entry)
  "Return the reload command list of parsed ENTRY, or nil."
  (nth (if (>= (length entry) 31) 19 17) entry))

(defun supervisor-entry-restart-sec (entry)
  "Return the per-unit restart delay of parsed ENTRY, or nil."
  (nth (if (>= (length entry) 31) 20 18) entry))

(defun supervisor-entry-description (entry)
  "Return the description string of parsed ENTRY, or nil."
  (nth (if (>= (length entry) 31) 21 19) entry))

(defun supervisor-entry-documentation (entry)
  "Return the documentation list of parsed ENTRY, or nil."
  (nth (if (>= (length entry) 31) 22 20) entry))

(defun supervisor-entry-before (entry)
  "Return the before-ordering dependencies of parsed ENTRY."
  (nth (if (>= (length entry) 31) 23 21) entry))

(defun supervisor-entry-wants (entry)
  "Return the soft dependencies of parsed ENTRY."
  (nth (if (>= (length entry) 31) 24 22) entry))

(defun supervisor-entry-kill-signal (entry)
  "Return the kill signal symbol of parsed ENTRY, or nil."
  (nth (if (>= (length entry) 31) 25 23) entry))

(defun supervisor-entry-kill-mode (entry)
  "Return the kill mode symbol of parsed ENTRY, or nil."
  (nth (if (>= (length entry) 31) 26 24) entry))

(defun supervisor-entry-remain-after-exit (entry)
  "Return non-nil if oneshot ENTRY latches active on success."
  (nth (if (>= (length entry) 31) 27 25) entry))

(defun supervisor-entry-success-exit-status (entry)
  "Return the success-exit-status plist of parsed ENTRY, or nil."
  (nth (if (>= (length entry) 31) 28 26) entry))

(defun supervisor-entry-user (entry)
  "Return the run-as user of parsed ENTRY, or nil."
  (nth (if (>= (length entry) 31) 29 27) entry))

(defun supervisor-entry-group (entry)
  "Return the run-as group of parsed ENTRY, or nil."
  (nth (if (>= (length entry) 31) 30 28) entry))

(defun supervisor-entry-wanted-by (entry)
  "Return the wanted-by target list of parsed ENTRY."
  (and (>= (length entry) 33) (nth 31 entry)))

(defun supervisor-entry-required-by (entry)
  "Return the required-by target list of parsed ENTRY."
  (and (>= (length entry) 33) (nth 32 entry)))

(defun supervisor-entry-sandbox-profile (entry)
  "Return the sandbox profile symbol of parsed ENTRY, or nil."
  (and (>= (length entry) 39) (nth 33 entry)))

(defun supervisor-entry-sandbox-network (entry)
  "Return the sandbox network mode symbol of parsed ENTRY, or nil."
  (and (>= (length entry) 39) (nth 34 entry)))

(defun supervisor-entry-sandbox-ro-bind (entry)
  "Return the sandbox read-only bind list of parsed ENTRY, or nil."
  (and (>= (length entry) 39) (nth 35 entry)))

(defun supervisor-entry-sandbox-rw-bind (entry)
  "Return the sandbox read-write bind list of parsed ENTRY, or nil."
  (and (>= (length entry) 39) (nth 36 entry)))

(defun supervisor-entry-sandbox-tmpfs (entry)
  "Return the sandbox tmpfs mount list of parsed ENTRY, or nil."
  (and (>= (length entry) 39) (nth 37 entry)))

(defun supervisor-entry-sandbox-raw-args (entry)
  "Return the sandbox raw argument list of parsed ENTRY, or nil."
  (and (>= (length entry) 39) (nth 38 entry)))

(defun supervisor--sandbox-requesting-p (entry)
  "Return non-nil if parsed ENTRY requests sandbox.
A unit is sandbox-requesting when `:sandbox-profile' is set to
anything other than `none', or when any other sandbox field has a
truthy (non-nil) value.

NOTE: This function operates on parsed tuples where nil and absent
are indistinguishable.  An entry with a nil-valued sandbox key
\(e.g., `:sandbox-ro-bind nil') will not be detected here.
Validation (`supervisor--validate-entry') is the authoritative
gating point and uses `plist-member' for key-presence semantics.
All runtime call sites are behind validation, so this is safe."
  (or (and (supervisor-entry-sandbox-profile entry)
           (not (eq (supervisor-entry-sandbox-profile entry) 'none)))
      (supervisor-entry-sandbox-network entry)
      (supervisor-entry-sandbox-ro-bind entry)
      (supervisor-entry-sandbox-rw-bind entry)
      (supervisor-entry-sandbox-tmpfs entry)
      (supervisor-entry-sandbox-raw-args entry)))

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
Reads from the cached program list via `supervisor--effective-programs'.
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
         (remain-active-p (gethash id (if snapshot
                                           (or (supervisor-snapshot-remain-active snapshot)
                                               (make-hash-table :test 'equal))
                                         supervisor--remain-active)))
         (manually-stopped-p
          (gethash id (if snapshot
                          (or (supervisor-snapshot-manually-stopped snapshot)
                              (make-hash-table :test 'equal))
                        supervisor--manually-stopped)))
         (entry-state (gethash id (if snapshot
                                      (supervisor-snapshot-entry-state snapshot)
                                    supervisor--entry-state)))
         (pid (cond (alive (number-to-string pid-num))
                    ((and oneshot-p oneshot-done) (format "exit:%d" oneshot-exit))
                    (t "-")))
         (target-p (eq type 'target))
         (status (cond (masked "masked")
                       (target-p
                        (let* ((effective-id
                                (if (equal id "default.target")
                                    (supervisor--resolve-default-target-link)
                                  id))
                               (conv
                                (when (hash-table-p
                                       supervisor--target-convergence)
                                  (gethash effective-id
                                           supervisor--target-convergence))))
                          (pcase conv
                            ('reached "reached")
                            ('degraded "degraded")
                            ('converging "converging")
                            (_ "pending"))))
                       (alive "running")
                       (failed "dead")
                       ((and oneshot-p oneshot-failed) "failed")
                       ((and oneshot-p remain-active-p) "active")
                       ((and oneshot-p oneshot-done) "done")
                       ((and oneshot-p manually-stopped-p) "stopped")
                       ;; Disabled entries show "disabled" (not "pending")
                       ((eq entry-state 'disabled) "disabled")
                       ;; Not in activation closure: unreachable
                       ((let ((closure (and (supervisor-plan-p
                                            supervisor--current-plan)
                                           (supervisor-plan-activation-closure
                                            supervisor--current-plan))))
                          (and (hash-table-p closure)
                               (not (gethash id closure))))
                        "unreachable")
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
     ((eq type 'target)
      (let ((reasons (when (hash-table-p
                            supervisor--target-convergence-reasons)
                       (gethash id
                                supervisor--target-convergence-reasons))))
        (when reasons
          (mapconcat #'identity reasons "; "))))
     (alive nil)
     ((and oneshot-p oneshot-done) nil)
     ((eq entry-state 'disabled) "disabled")
     ((eq entry-state 'delayed) "delayed")
     ((eq entry-state 'waiting-on-deps) "waiting-on-deps")
     ((eq entry-state 'pending) "pending")
     ((eq entry-state 'failed-to-spawn)
      (or (gethash id supervisor--spawn-failure-reason)
          "failed-to-spawn"))
     ((eq entry-state 'startup-timeout) "startup-timeout")
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
         (delay (supervisor-entry-delay parsed))
         (enabled (supervisor-entry-enabled-p parsed))
         (restart (supervisor-entry-restart-policy parsed))
         (logging (supervisor-entry-logging-p parsed))
         (stdout-log-file (supervisor-entry-stdout-log-file parsed))
         (stderr-log-file (supervisor-entry-stderr-log-file parsed))
         (after (supervisor-entry-after parsed))
         (requires (supervisor-entry-requires parsed))
         (oneshot-blocking (supervisor-entry-oneshot-blocking parsed))
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
      (when (not (eq oneshot-blocking supervisor-oneshot-default-blocking))
        (setq plist (plist-put plist :oneshot-blocking oneshot-blocking)))
      (when (not (eq oneshot-timeout supervisor-oneshot-timeout))
        (setq plist (plist-put plist :oneshot-timeout oneshot-timeout))))
    (when (not logging)
      (setq plist (plist-put plist :logging nil)))
    (when stdout-log-file
      (setq plist (plist-put plist :stdout-log-file stdout-log-file)))
    (when stderr-log-file
      (setq plist (plist-put plist :stderr-log-file stderr-log-file)))
    (when (and (eq type 'simple) (not (eq restart 'always)))
      (setq plist (plist-put plist :restart restart)))
    (when (not enabled)
      (setq plist (plist-put plist :disabled t)))
    (when (> delay 0)
      (setq plist (plist-put plist :delay delay)))
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
one per service.  Save each to a `.el' file in the highest-precedence
authority root to complete the migration."
  (interactive)
  (let* ((result (supervisor--migrate-all-entries))
         (migrated (plist-get result :migrated))
         (skipped (plist-get result :skipped)))
    (with-output-to-temp-buffer "*supervisor-migrate*"
      (princ ";;; Supervisor Unit Files (Schema v1)\n")
      (princ ";; Generated by supervisor-migrate-config\n")
      (princ (format ";; Version: %d\n" supervisor-service-schema-version))
      (princ (format ";; Target authority root: %s\n\n"
                     (abbreviate-file-name
                      (or (car (last (supervisor--active-authority-roots)))
                          supervisor-unit-directory))))
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
  restart-override ; hash: id -> policy symbol or legacy 'enabled/'disabled
  logging-override ; hash: id -> 'enabled, 'disabled, or nil
  mask-override    ; hash: id -> 'masked or nil
  manually-started ; hash: id -> t if manually started (for reconcile)
  manually-stopped ; hash: id -> t if manually stopped
  remain-active    ; hash: id -> t if oneshot latched active (remain-after-exit)
  last-exit-info   ; hash: id -> plist (:status :code :timestamp)
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
     :manually-stopped (copy-hash-table supervisor--manually-stopped)
     :remain-active (copy-hash-table supervisor--remain-active)
     :last-exit-info (copy-hash-table supervisor--last-exit-info)
     :timestamp (float-time))))

(defun supervisor--validate-references (valid-entries invalid)
  "Validate cross-entry references in VALID-ENTRIES.
INVALID is a hash table to which newly invalid entries are added.
Return the filtered list of valid entries with soft refs dropped."
  (let ((id-type (make-hash-table :test 'equal)))
    ;; Build ID-to-type lookup
    (dolist (entry valid-entries)
      (puthash (supervisor-entry-id entry)
               (supervisor-entry-type entry)
               id-type))
    ;; Check :wanted-by/:required-by target references
    (dolist (entry valid-entries)
      (let ((id (supervisor-entry-id entry)))
        (dolist (spec (list (cons (supervisor-entry-wanted-by entry) ":wanted-by")
                            (cons (supervisor-entry-required-by entry) ":required-by")))
          (dolist (target-id (car spec))
            (let ((target-type (gethash target-id id-type)))
              (cond
               ((not target-type)
                (puthash id (format "%s references non-existent target '%s'"
                                    (cdr spec) target-id)
                         invalid)
                (supervisor--log 'error "%s '%s' for %s does not exist"
                                 (cdr spec) target-id id))
               ((not (eq target-type 'target))
                (puthash id (format "%s references '%s' which is not a target"
                                    (cdr spec) target-id)
                         invalid)
                (supervisor--log 'error "%s '%s' for %s is not :type target"
                                 (cdr spec) target-id id))))))))
    ;; Check target entries' :requires refs exist
    (dolist (entry valid-entries)
      (when (eq (supervisor-entry-type entry) 'target)
        (let ((id (supervisor-entry-id entry)))
          (dolist (req (supervisor-entry-requires entry))
            (unless (gethash req id-type)
              (puthash id (format ":requires '%s' does not exist" req)
                       invalid)
              (supervisor--log 'error
                               "target %s :requires '%s' does not exist"
                               id req)))
          ;; Soft deps: drop missing :wants/:after/:before with warning
          (dolist (dep-spec (list (cons (supervisor-entry-wants entry) ":wants")
                                  (cons (supervisor-entry-after entry) ":after")
                                  (cons (supervisor-entry-before entry) ":before")))
            (dolist (dep (car dep-spec))
              (unless (gethash dep id-type)
                (supervisor--log 'warning
                                 "target %s %s '%s' does not exist, dropping"
                                 id (cdr dep-spec) dep)))))))
    ;; Cycle detection on :requires graph among targets
    (let ((target-requires (make-hash-table :test 'equal))
          (cycle-ids nil))
      (dolist (entry valid-entries)
        (when (and (eq (supervisor-entry-type entry) 'target)
                   (not (gethash (supervisor-entry-id entry) invalid)))
          (puthash (supervisor-entry-id entry)
                   (supervisor-entry-requires entry)
                   target-requires)))
      ;; DFS cycle detection
      (let ((white (make-hash-table :test 'equal))
            (gray (make-hash-table :test 'equal)))
        (maphash (lambda (id _) (puthash id t white)) target-requires)
        (cl-labels
            ((dfs (node path)
               (cond
                ((gethash node gray)
                 ;; Found a cycle
                 (let ((cycle-start (cl-position node path :test #'equal)))
                   (when cycle-start
                     (dolist (cid (nthcdr cycle-start path))
                       (push cid cycle-ids)))))
                ((gethash node white)
                 (remhash node white)
                 (puthash node t gray)
                 (dolist (dep (gethash node target-requires))
                   (when (gethash dep target-requires)
                     (dfs dep (append path (list node)))))
                 (remhash node gray)))))
          (maphash (lambda (id _)
                     (when (gethash id white)
                       (dfs id nil)))
                   target-requires)))
      ;; Mark cycle participants invalid
      (dolist (cid (cl-remove-duplicates cycle-ids :test #'equal))
        (puthash cid (format "target :requires cycle detected involving '%s'" cid)
                 invalid)
        (supervisor--log 'error "target :requires cycle: %s" cid)))
    ;; Filter out newly-invalidated entries
    (cl-remove-if (lambda (entry)
                    (gethash (supervisor-entry-id entry) invalid))
                  valid-entries)))

(defun supervisor--materialize-target-members (entries)
  "Build inverse membership map from service target declarations.
ENTRIES is a list of parsed valid entries.
Returns hash of TARGET-ID -> (:requires (IDS...) :wants (IDS...)).
Services declaring :required-by contribute to target requires-members.
Services declaring :wanted-by contribute to target wants-members."
  (let ((members (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (let ((id (supervisor-entry-id entry)))
        ;; :required-by -> target's :requires member list
        (dolist (target-id (supervisor-entry-required-by entry))
          (let ((cur (gethash target-id members)))
            (puthash target-id
                     (plist-put cur :requires
                                (cons id (plist-get cur :requires)))
                     members)))
        ;; :wanted-by -> target's :wants member list
        (dolist (target-id (supervisor-entry-wanted-by entry))
          (let ((cur (gethash target-id members)))
            (puthash target-id
                     (plist-put cur :wants
                                (cons id (plist-get cur :wants)))
                     members)))))
    ;; Merge target's own :requires into membership (top-down direction)
    (dolist (entry entries)
      (when (eq (supervisor-entry-type entry) 'target)
        (let ((id (supervisor-entry-id entry)))
          (dolist (req (supervisor-entry-requires entry))
            (let ((cur (gethash id members)))
              (puthash id
                       (plist-put cur :requires
                                  (cons req (plist-get cur :requires)))
                       members))))))
    ;; Reverse lists to preserve entry order and deduplicate
    (maphash (lambda (tid plist)
               (puthash tid
                        (list :requires (cl-remove-duplicates
                                         (nreverse
                                          (plist-get plist :requires))
                                         :test #'equal)
                              :wants (nreverse (plist-get plist :wants)))
                        members))
             members)
    members))

(defun supervisor--expand-transaction (root entries-by-id
                                            target-members order-index)
  "Compute activation closure from ROOT target.
ENTRIES-BY-ID is hash of id -> parsed entry.
TARGET-MEMBERS is hash from `supervisor--materialize-target-members'.
ORDER-INDEX is hash of id -> original index for deterministic ordering.
Returns hash of id -> t for all IDs in the closure."
  (let ((closure (make-hash-table :test 'equal))
        (queue nil))
    (when (gethash root entries-by-id)
      (puthash root t closure)
      (push root queue))
    (while queue
      (let* ((current-id (pop queue))
             (entry (gethash current-id entries-by-id))
             (new-ids nil))
        (when entry
          (let ((type (supervisor-entry-type entry)))
            (if (eq type 'target)
                ;; Target: pull in own :requires/:wants + membership edges
                (progn
                  (dolist (dep (supervisor-entry-requires entry))
                    (push dep new-ids))
                  (dolist (dep (supervisor-entry-wants entry))
                    (push dep new-ids))
                  ;; Membership edges (from :required-by/:wanted-by inverse)
                  (let ((mem (gethash current-id target-members)))
                    (when mem
                      (dolist (dep (plist-get mem :requires))
                        (push dep new-ids))
                      (dolist (dep (plist-get mem :wants))
                        (push dep new-ids)))))
              ;; Service: pull in own :requires/:wants
              (dolist (dep (supervisor-entry-requires entry))
                (push dep new-ids))
              (dolist (dep (supervisor-entry-wants entry))
                (push dep new-ids))))
          ;; Filter to existing, not-yet-in-closure entries
          (setq new-ids
                (cl-remove-if
                 (lambda (id)
                   (or (not (gethash id entries-by-id))
                       (gethash id closure)))
                 new-ids))
          ;; Sort by order-index for determinism
          (setq new-ids
                (sort new-ids
                      (lambda (a b)
                        (< (or (gethash a order-index) 0)
                           (or (gethash b order-index) 0)))))
          ;; Add to closure and queue
          (dolist (id new-ids)
            (puthash id t closure)
            (setq queue (append queue (list id)))))))
    closure))

(defun supervisor--build-plan (programs)
  "Build an immutable execution plan from PROGRAMS.
This function does not modify global runtime state, but does emit
warnings for invalid entries, duplicate IDs, and invalid :after refs.
Returns a `supervisor-plan' struct with all computed scheduling data.

The plan includes:
- Parsed and validated entries
- Invalid entries with reasons
- Globally sorted entries (topological order)
- Validated :after dependencies (ordering only)
- Validated :requires dependencies (pull-in + ordering)
- Reverse dependency index (combined)
- Cycle fallback tracking
- Stable ordering index
- Deterministic fingerprint (SHA-1 of plan content)"
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
    ;; Phase 1b: Validate cross-entry references
    (setq valid-entries
          (supervisor--validate-references valid-entries invalid))
    ;; Phase 2: Global dep validation and topo sort (single pass, no stage partitioning)
    (let ((all-ids (mapcar #'car valid-entries))
          (combined-deps (make-hash-table :test 'equal)))
      ;; Pre-pass: build :before inversion map across ALL entries
      (let ((before-map (make-hash-table :test 'equal)))
        (dolist (entry valid-entries)
          (let ((src-id (supervisor-entry-id entry))
                (before (supervisor-entry-before entry)))
            (dolist (target before)
              (cond
               ((not (member target all-ids))
                (supervisor--log 'warning
                  ":before '%s' for %s does not exist, ignoring"
                  target src-id))
               (t
                (puthash target
                         (cons src-id (gethash target before-map))
                         before-map))))))
        ;; Validate :after, :requires, :wants references globally
        (let ((validated-entries
               (mapcar
                (lambda (entry)
                  (let* ((id (supervisor-entry-id entry))
                         (after (supervisor-entry-after entry))
                         (requires (supervisor-entry-requires entry))
                         (wants (supervisor-entry-wants entry))
                         ;; Validate :after - warn + drop missing
                         (valid-after
                          (cl-remove-if-not
                           (lambda (dep)
                             (cond
                              ((not (member dep all-ids))
                               (supervisor--log 'warning
                                 ":after '%s' for %s does not exist, ignoring"
                                 dep id)
                               nil)
                              (t t)))
                           after))
                         ;; Merge inverted :before edges
                         (before-edges (gethash id before-map))
                         (valid-after
                          (supervisor--deduplicate-stable
                           (append valid-after before-edges)))
                         ;; Validate :requires - warn + drop missing
                         ;; For targets, a missing :requires invalidates
                         ;; the target (consistent with early validation)
                         (valid-requires
                          (cl-remove-if-not
                           (lambda (dep)
                             (cond
                              ((not (member dep all-ids))
                               (if (eq (supervisor-entry-type entry) 'target)
                                   (progn
                                     (puthash id
                                              (format
                                               ":requires '%s' does not exist"
                                               dep)
                                              invalid)
                                     (supervisor--log 'error
                                       "target %s :requires '%s' does not exist"
                                       id dep))
                                 (supervisor--log 'warning
                                   ":requires '%s' for %s does not exist, ignoring"
                                   dep id))
                               nil)
                              (t t)))
                           requires))
                         ;; Validate :wants - silently drop missing
                         (valid-wants
                          (cl-remove-if-not
                           (lambda (dep)
                             (member dep all-ids))
                           wants))
                         ;; Auto-order rule: for target entries, :requires
                         ;; and :wants imply :after (ordering edges)
                         (valid-after
                          (if (eq (supervisor-entry-type entry) 'target)
                              (supervisor--deduplicate-stable
                               (append valid-after
                                       (cl-union valid-requires valid-wants
                                                 :test #'equal)))
                            valid-after)))
                    (puthash id valid-after deps)
                    (puthash id valid-requires requires-deps)
                    ;; Combined deps for topo sort
                    ;; (union of after + requires + wants)
                    (puthash id (supervisor--deduplicate-stable
                                 (append
                                  (cl-union valid-after valid-requires
                                            :test #'equal)
                                  valid-wants))
                             combined-deps)
                    ;; Return entry with validated deps
                    ;; :after is at index 10, :requires at index 14
                    (let ((new-entry entry))
                      (unless (equal after valid-after)
                        (setq new-entry
                              (append (cl-subseq new-entry 0 10)
                                      (list valid-after)
                                      (cl-subseq new-entry 11))))
                      (unless (equal requires valid-requires)
                        (setq new-entry
                              (append (cl-subseq new-entry 0 14)
                                      (list valid-requires)
                                      (cl-subseq new-entry 15))))
                      new-entry)))
                valid-entries)))
          ;; Filter out entries marked invalid during validation
          (setq validated-entries
                (cl-remove-if (lambda (entry)
                                (gethash (supervisor-entry-id entry) invalid))
                              validated-entries))
          ;; Build dependents graph (combined :after + :requires + :wants)
          (dolist (entry validated-entries)
            (let* ((id (supervisor-entry-id entry))
                   (all-deps (gethash id combined-deps)))
              (puthash id nil dependents)
              (dolist (dep all-deps)
                (puthash dep (cons id (gethash dep dependents))
                         dependents))))
          ;; Global topo sort
          (let ((sorted-entries
                 (supervisor--build-plan-topo-sort
                  validated-entries combined-deps order-index cycle-fallback-ids)))
            ;; If cycle was detected, also clear deps and requires-deps
            (dolist (entry sorted-entries)
              (let ((id (supervisor-entry-id entry)))
                (when (gethash id cycle-fallback-ids)
                  (puthash id nil deps)
                  (puthash id nil requires-deps))))
            ;; Filter valid-entries to exclude entries invalidated in phase 2
            (let ((final-entries (cl-remove-if
                                  (lambda (entry)
                                    (gethash (supervisor-entry-id entry) invalid))
                                  valid-entries)))
              ;; Return the plan struct
              (supervisor-plan--create
               :entries final-entries
               :invalid invalid
               :by-target sorted-entries
               :deps deps
               :requires-deps requires-deps
               :dependents dependents
               :cycle-fallback-ids cycle-fallback-ids
               :order-index order-index
               :target-members nil
               :activation-root nil
               :activation-closure nil
               :meta (list :version supervisor-plan-version
                           :timestamp (float-time)
                           :fingerprint
                           (supervisor--plan-fingerprint
                            sorted-entries deps requires-deps
                            order-index))))))))))

(defun supervisor--plan-fingerprint (entries deps requires-deps order-index)
  "Compute deterministic fingerprint of plan content.
ENTRIES is the sorted entry list.  DEPS and REQUIRES-DEPS are hashes
of id to dependency lists.  ORDER-INDEX is hash of id to original index.
Returns a hex string (SHA-1 of serialized plan data)."
  (let ((parts nil))
    (dolist (entry entries)
      (let* ((id (supervisor-entry-id entry))
             (d (gethash id deps))
             (r (gethash id requires-deps))
             (idx (gethash id order-index)))
        (push (format "%s:%s:%s:%s" id
                      (prin1-to-string d)
                      (prin1-to-string r)
                      idx)
              parts)))
    (sha1 (mapconcat #'identity (nreverse parts) "\n"))))

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
        ;; Return entries with :after, :requires, and :wants stripped.
        (mapcar
         (lambda (entry)
           (let ((len (length entry)))
             (cond
              ;; Current schema entry.
              ((>= len 31)
               (append (cl-subseq entry 0 10)
                       (list nil)                ; clear :after (index 10)
                       (cl-subseq entry 11 14)
                       (list nil)                ; clear :requires (index 14)
                       (cl-subseq entry 15 24)
                       (list nil)                ; clear :wants (index 24)
                       (cl-subseq entry 25)))
              ;; Legacy full entry.
              ((>= len 23)
               (append (cl-subseq entry 0 8)
                       (list nil)                ; clear :after (index 8)
                       (cl-subseq entry 9 12)
                       (list nil)                ; clear :requires (index 12)
                       (cl-subseq entry 13 22)
                       (list nil)                ; clear :wants (index 22)
                       (cl-subseq entry 23)))
              ;; Legacy 13+ entry.
              ((>= len 13)
               (append (cl-subseq entry 0 8)
                       (list nil)                ; clear :after
                       (cl-subseq entry 9 12)
                       (list nil)                ; clear :requires
                       (cl-subseq entry 13)))
              ;; Legacy 11+ entry.
              ((>= len 11)
               (append (cl-subseq entry 0 8)
                       (list nil)                ; clear :after
                       (cl-subseq entry 9)))
              (t entry))))
         entries)))))

;;; Helpers

(defun supervisor--normalize-string-or-list (val)
  "Normalize VAL to a list of strings.
If VAL is a string, return a one-element list.
If VAL is a proper list of strings, return as-is.
If VAL is nil, return nil."
  (cond ((null val) nil)
        ((stringp val) (list val))
        ((and (proper-list-p val) (cl-every #'stringp val)) val)
        (t nil)))

(defun supervisor--deduplicate-stable (list)
  "Return LIST with duplicates removed, preserving first occurrence order.
Uses `equal' for comparison."
  (let ((seen (make-hash-table :test 'equal))
        (result nil))
    (dolist (item list)
      (unless (gethash item seen)
        (puthash item t seen)
        (push item result)))
    (nreverse result)))

(defconst supervisor--known-signals
  '(SIGHUP SIGINT SIGQUIT SIGILL SIGTRAP SIGABRT SIGBUS SIGFPE
    SIGKILL SIGUSR1 SIGSEGV SIGUSR2 SIGPIPE SIGALRM SIGTERM
    SIGSTKFLT SIGCHLD SIGCONT SIGSTOP SIGTSTP SIGTTIN SIGTTOU
    SIGURG SIGXCPU SIGXFSZ SIGVTALRM SIGPROF SIGWINCH SIGIO
    SIGPWR SIGSYS)
  "List of known POSIX signal names as symbols.")

(defun supervisor--normalize-signal-name (name)
  "Normalize signal NAME to canonical SIGXXX symbol.
Accept symbol or string forms like TERM, SIGTERM, term, sigterm.
Return the canonical symbol (e.g., `SIGTERM'), or nil if unknown."
  (let* ((s (upcase (if (symbolp name) (symbol-name name)
                      (if (stringp name) name ""))))
         (full (if (string-prefix-p "SIG" s) s (concat "SIG" s)))
         (sym (intern full)))
    (when (memq sym supervisor--known-signals)
      sym)))

(defun supervisor--normalize-kill-mode (val)
  "Normalize VAL to a kill-mode symbol.
Accept `process' or `mixed' (symbol or string).
Return the symbol, or nil if invalid."
  (let ((sym (cond ((symbolp val) val)
                   ((stringp val) (intern val))
                   (t nil))))
    (when (memq sym '(process mixed))
      sym)))

(defun supervisor--normalize-success-exit-status (val)
  "Normalize VAL to a success-exit-status plist with :codes and :signals.
Accept integer, signal name (symbol/string), or list of mixed.
Return (:codes (INTS...) :signals (SYMS...)) with stable dedup,
or nil if VAL is nil."
  (when val
    (let ((items (if (listp val) val (list val)))
          (codes nil)
          (signals nil))
      (dolist (item items)
        (cond
         ((integerp item)
          (push item codes))
         ((or (symbolp item) (stringp item))
          (let ((sig (supervisor--normalize-signal-name item)))
            (when sig
              (push sig signals))))))
      (list :codes (supervisor--deduplicate-stable (nreverse codes))
            :signals (supervisor--deduplicate-stable (nreverse signals))))))

(defun supervisor--parse-entry (entry)
  "Parse ENTRY into a normalized list of entry properties.
Return a 39-element list: (id cmd delay enabled-p restart-policy
logging-p stdout-log-file stderr-log-file type stage after
oneshot-blocking oneshot-timeout tags requires working-directory
environment environment-file exec-stop exec-reload restart-sec
description documentation before wants kill-signal kill-mode
remain-after-exit success-exit-status user group wanted-by
required-by sandbox-profile sandbox-network sandbox-ro-bind
sandbox-rw-bind sandbox-tmpfs sandbox-raw-args).

Indices (schema v1):
  0  id                  - unique identifier string
  1  cmd                 - shell command string
  2  delay               - seconds to wait before starting
  3  enabled-p           - whether to start this service
  4  restart-policy      - restart policy symbol
  5  logging-p           - whether to log stdout/stderr
  6  stdout-log-file     - explicit stdout log file path or nil
  7  stderr-log-file     - explicit stderr log file path or nil
  8  type                - `simple', `oneshot', or `target'
  9  stage               - startup stage symbol
  10 after               - ordering dependencies (same stage)
  11 oneshot-blocking    - block stage for oneshot exit
  12 oneshot-timeout     - timeout for blocking oneshots
  13 tags                - list of filter tags
  14 requires            - requirement dependencies
  15 working-directory   - process working directory or nil
  16 environment         - environment alist or nil
  17 environment-file    - list of env-file paths or nil
  18 exec-stop           - list of stop commands or nil
  19 exec-reload         - list of reload commands or nil
  20 restart-sec         - per-unit restart delay or nil
  21 description         - human-readable description or nil
  22 documentation       - list of doc URIs/paths or nil
  23 before              - before-ordering deps or nil
  24 wants               - soft deps or nil
  25 kill-signal         - canonical signal symbol or nil
  26 kill-mode           - `process' or `mixed' or nil
  27 remain-after-exit   - oneshot active latch boolean
  28 success-exit-status - plist (:codes :signals) or nil
  29 user                - run-as user string/int or nil
  30 group               - run-as group string/int or nil
  31 wanted-by           - target IDs (soft membership) or nil
  32 required-by         - target IDs (hard requirement) or nil
  33 sandbox-profile     - sandbox profile symbol or nil
  34 sandbox-network     - sandbox network mode symbol or nil
  35 sandbox-ro-bind     - list of read-only bind paths or nil
  36 sandbox-rw-bind     - list of read-write bind paths or nil
  37 sandbox-tmpfs       - list of tmpfs mount paths or nil
  38 sandbox-raw-args    - list of raw bwrap argument strings or nil

ENTRY can be a command string or a list (COMMAND . PLIST).
Use accessor functions instead of direct indexing for new code."
  (if (stringp entry)
      (let* ((tokens (split-string-and-unquote entry))
             (id (or (car tokens)
                     (error "Supervisor: empty command string")))
             (id (file-name-nondirectory id)))
        (list id entry 0 t 'always t nil nil 'simple 'stage3 nil
              supervisor-oneshot-default-blocking supervisor-oneshot-timeout nil nil
              nil nil nil nil nil nil
              nil nil nil nil nil nil nil nil
              nil nil nil nil
              nil nil nil nil nil nil))
    (let* ((cmd (car entry))
           (plist (cdr entry))
           (type-raw (plist-get plist :type))
           (type (cond ((null type-raw) 'simple)
                       ((symbolp type-raw) type-raw)
                       ((stringp type-raw) (intern type-raw))
                       (t 'simple)))
           (cmd-tokens (unless (or (eq type 'target) (null cmd))
                         (split-string-and-unquote cmd)))
           (_ (when (and (not (eq type 'target)) (not cmd-tokens))
                (error "Supervisor: empty command in entry")))
           (id (or (plist-get plist :id)
                   (if (eq type 'target)
                       (error "Supervisor: target entry requires :id")
                     (file-name-nondirectory (car cmd-tokens)))))
           (cmd (if (eq type 'target) nil cmd))
           (delay (or (plist-get plist :delay) 0))
           (_ (unless (memq type '(simple oneshot target))
                (supervisor--log 'warning "unknown :type '%s' for %s, using simple" type id)))
           ;; :enabled t (default) or :disabled t (inverse) - whether to start at all
           (enabled (cond ((plist-member plist :enabled)
                           (plist-get plist :enabled))
                          ((plist-member plist :disabled)
                           (not (plist-get plist :disabled)))
                          (t t)))
           ;; :restart POLICY or :no-restart t — restart policy symbol
           ;; Accepts: symbol (always, no, on-success, on-failure),
           ;;          boolean t (→ always) or nil (→ no),
           ;;          :no-restart t (→ no)
           (restart (cond ((plist-member plist :restart)
                           (supervisor--normalize-restart-policy
                            (plist-get plist :restart)))
                          ((plist-member plist :no-restart)
                           (if (plist-get plist :no-restart) 'no 'always))
                          (t 'always)))
           (logging (if (plist-member plist :logging)
                        (plist-get plist :logging)
                      t))
           (stdout-log-file (plist-get plist :stdout-log-file))
           (stderr-log-file (plist-get plist :stderr-log-file))
           ;; :stage is vestigial (always stage3)
           (stage 'stage3)
           ;; :after - ordering dependencies (same stage only, start order)
           (after (supervisor--normalize-after (plist-get plist :after)))
           ;; :requires - requirement dependencies (pull-in + ordering)
           (requires (supervisor--normalize-after (plist-get plist :requires)))
           ;; Oneshot blocking/timeout settings
           (oneshot-blocking (supervisor--oneshot-blocking-p plist))
           (oneshot-timeout (supervisor--oneshot-timeout-value plist))
           ;; Tags for filtering (list of symbols or strings)
           (tags-raw (plist-get plist :tags))
           (tags (cond ((null tags-raw) nil)
                       ((listp tags-raw) tags-raw)
                       (t (list tags-raw))))
           ;; P2 fields (indices 13-18)
           (working-directory (plist-get plist :working-directory))
           (environment (plist-get plist :environment))
           (environment-file
            (supervisor--normalize-string-or-list
             (plist-get plist :environment-file)))
           (exec-stop
            (supervisor--normalize-string-or-list
             (plist-get plist :exec-stop)))
           (exec-reload
            (supervisor--normalize-string-or-list
             (plist-get plist :exec-reload)))
           (restart-sec (plist-get plist :restart-sec))
           ;; PT3 fields (indices 19-26)
           (description (plist-get plist :description))
           (documentation-raw (plist-get plist :documentation))
           (documentation
            (supervisor--deduplicate-stable
             (supervisor--normalize-string-or-list documentation-raw)))
           (before
            (supervisor--deduplicate-stable
             (supervisor--normalize-after (plist-get plist :before))))
           (wants
            (supervisor--deduplicate-stable
             (supervisor--normalize-after (plist-get plist :wants))))
           (kill-signal-raw (plist-get plist :kill-signal))
           (kill-signal (when kill-signal-raw
                          (supervisor--normalize-signal-name kill-signal-raw)))
           (kill-mode-raw (plist-get plist :kill-mode))
           (kill-mode (when kill-mode-raw
                        (supervisor--normalize-kill-mode kill-mode-raw)))
           (remain-after-exit (plist-get plist :remain-after-exit))
           (success-exit-status-raw (plist-get plist :success-exit-status))
           (success-exit-status
            (supervisor--normalize-success-exit-status success-exit-status-raw))
           ;; Identity fields (indices 27-28)
           (user (plist-get plist :user))
           (group (plist-get plist :group))
           ;; Target membership fields (indices 29-30)
           (wanted-by
            (supervisor--deduplicate-stable
             (supervisor--normalize-after (plist-get plist :wanted-by))))
           (required-by
            (supervisor--deduplicate-stable
             (supervisor--normalize-after (plist-get plist :required-by))))
           ;; Sandbox fields (indices 33-38)
           (sandbox-profile-raw (plist-get plist :sandbox-profile))
           (sandbox-profile (when sandbox-profile-raw
                              (if (stringp sandbox-profile-raw)
                                  (intern sandbox-profile-raw)
                                sandbox-profile-raw)))
           (sandbox-network-raw (plist-get plist :sandbox-network))
           (sandbox-network (when sandbox-network-raw
                              (if (stringp sandbox-network-raw)
                                  (intern sandbox-network-raw)
                                sandbox-network-raw)))
           (sandbox-ro-bind
            (supervisor--deduplicate-stable
             (supervisor--normalize-string-or-list
              (plist-get plist :sandbox-ro-bind))))
           (sandbox-rw-bind
            (supervisor--deduplicate-stable
             (supervisor--normalize-string-or-list
              (plist-get plist :sandbox-rw-bind))))
           (sandbox-tmpfs
            (supervisor--deduplicate-stable
             (supervisor--normalize-string-or-list
              (plist-get plist :sandbox-tmpfs))))
           (sandbox-raw-args
            (supervisor--normalize-string-or-list
             (plist-get plist :sandbox-raw-args))))
      (list id cmd delay enabled restart logging
            stdout-log-file stderr-log-file
            type stage after
            oneshot-blocking oneshot-timeout tags requires
            working-directory environment environment-file
            exec-stop exec-reload restart-sec
            description documentation before wants
            kill-signal kill-mode remain-after-exit success-exit-status
            user group wanted-by required-by
            sandbox-profile sandbox-network sandbox-ro-bind sandbox-rw-bind
            sandbox-tmpfs sandbox-raw-args))))

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
   :restart (supervisor-entry-restart-policy entry)
   :logging (supervisor-entry-logging-p entry)
   :stdout-log-file (supervisor-entry-stdout-log-file entry)
   :stderr-log-file (supervisor-entry-stderr-log-file entry)
   :after (supervisor-entry-after entry)
   :requires (supervisor-entry-requires entry)
   :oneshot-blocking (supervisor-entry-oneshot-blocking entry)
   :oneshot-timeout (supervisor-entry-oneshot-timeout entry)
   :tags (supervisor-entry-tags entry)
   :working-directory (supervisor-entry-working-directory entry)
   :environment (supervisor-entry-environment entry)
   :environment-file (supervisor-entry-environment-file entry)
   :exec-stop (supervisor-entry-exec-stop entry)
   :exec-reload (supervisor-entry-exec-reload entry)
   :restart-sec (supervisor-entry-restart-sec entry)
   :description (supervisor-entry-description entry)
   :documentation (supervisor-entry-documentation entry)
   :before (supervisor-entry-before entry)
   :wants (supervisor-entry-wants entry)
   :kill-signal (supervisor-entry-kill-signal entry)
   :kill-mode (supervisor-entry-kill-mode entry)
   :remain-after-exit (supervisor-entry-remain-after-exit entry)
   :success-exit-status (supervisor-entry-success-exit-status entry)
   :user (supervisor-entry-user entry)
   :group (supervisor-entry-group entry)
   :wanted-by (supervisor-entry-wanted-by entry)
   :required-by (supervisor-entry-required-by entry)
   :sandbox-profile (supervisor-entry-sandbox-profile entry)
   :sandbox-network (supervisor-entry-sandbox-network entry)
   :sandbox-ro-bind (supervisor-entry-sandbox-ro-bind entry)
   :sandbox-rw-bind (supervisor-entry-sandbox-rw-bind entry)
   :sandbox-tmpfs (supervisor-entry-sandbox-tmpfs entry)
   :sandbox-raw-args (supervisor-entry-sandbox-raw-args entry)))

(defun supervisor-service-to-entry (service)
  "Convert SERVICE struct to a parsed entry tuple."
  (list (supervisor-service-id service)
        (supervisor-service-command service)
        (supervisor-service-delay service)
        (supervisor-service-enabled service)
        (supervisor-service-restart service)
        (supervisor-service-logging service)
        (supervisor-service-stdout-log-file service)
        (supervisor-service-stderr-log-file service)
        (supervisor-service-type service)
        (supervisor-service-stage service)
        (supervisor-service-after service)
        (supervisor-service-oneshot-blocking service)
        (supervisor-service-oneshot-timeout service)
        (supervisor-service-tags service)
        (supervisor-service-requires service)
        (supervisor-service-working-directory service)
        (supervisor-service-environment service)
        (supervisor-service-environment-file service)
        (supervisor-service-exec-stop service)
        (supervisor-service-exec-reload service)
        (supervisor-service-restart-sec service)
        (supervisor-service-description service)
        (supervisor-service-documentation service)
        (supervisor-service-before service)
        (supervisor-service-wants service)
        (supervisor-service-kill-signal service)
        (supervisor-service-kill-mode service)
        (supervisor-service-remain-after-exit service)
        (supervisor-service-success-exit-status service)
        (supervisor-service-user service)
        (supervisor-service-group service)
        (supervisor-service-wanted-by service)
        (supervisor-service-required-by service)
        (supervisor-service-sandbox-profile service)
        (supervisor-service-sandbox-network service)
        (supervisor-service-sandbox-ro-bind service)
        (supervisor-service-sandbox-rw-bind service)
        (supervisor-service-sandbox-tmpfs service)
        (supervisor-service-sandbox-raw-args service)))

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

;;; Telemetry Accessors

(defun supervisor--telemetry-uptime (id)
  "Return uptime in seconds for ID, or nil if not running.
Computes elapsed time since the entry was started."
  (when-let* ((start (gethash id supervisor--start-times))
              (proc (gethash id supervisor--processes)))
    (when (process-live-p proc)
      (- (float-time) start))))

(defun supervisor--telemetry-restart-count (id)
  "Return number of recent restarts for ID within the restart window.
Returns 0 if no restarts have occurred."
  (let* ((now (float-time))
         (times (gethash id supervisor--restart-times))
         (recent (cl-remove-if
                  (lambda (ts) (> (- now ts) supervisor-restart-window))
                  times)))
    (length recent)))

(defun supervisor--telemetry-last-exit (id)
  "Return formatted last exit string for ID, or nil if never exited."
  (when-let* ((info (gethash id supervisor--last-exit-info)))
    (let ((status (plist-get info :status))
          (code (plist-get info :code)))
      (pcase status
        ('signal (format "killed by signal %d" code))
        ('exited (if (= code 0)
                     "exited successfully"
                   (format "exited with code %d" code)))
        (_ "terminated")))))

(defun supervisor--telemetry-last-exit-info (id &optional snapshot)
  "Return raw last exit info plist for ID, or nil if never exited.
If SNAPSHOT is provided, read from it; otherwise read from globals."
  (gethash id (if snapshot
                  (or (supervisor-snapshot-last-exit-info snapshot)
                      (make-hash-table :test 'equal))
                supervisor--last-exit-info)))

(defun supervisor--telemetry-next-restart-eta (id)
  "Return `float-time' of next scheduled restart for ID, or nil.
Returns the time when the pending restart timer will fire."
  (when-let* ((timer (gethash id supervisor--restart-timers)))
    (when (timerp timer)
      (let ((secs (timer--time timer)))
        (when secs
          (float-time secs))))))

(defun supervisor--telemetry-process-metrics (pid)
  "Return best-effort process metrics for PID, or nil.
Returns a plist with available keys: :rss (KB), :pcpu (percent),
:pmem (percent), :etime (seconds), :thcount (thread count).
Gracefully returns nil if process attributes are unavailable."
  (condition-case nil
      (when-let* ((attrs (process-attributes pid)))
        (let ((result nil))
          (when-let* ((rss (alist-get 'rss attrs)))
            (setq result (plist-put result :rss rss)))
          (when-let* ((pcpu (alist-get 'pcpu attrs)))
            (setq result (plist-put result :pcpu pcpu)))
          (when-let* ((pmem (alist-get 'pmem attrs)))
            (setq result (plist-put result :pmem pmem)))
          (when-let* ((etime (alist-get 'etime attrs)))
            (setq result (plist-put result :etime etime)))
          (when-let* ((thcount (alist-get 'thcount attrs)))
            (setq result (plist-put result :thcount thcount)))
          result))
    (error nil)))

(defun supervisor--telemetry-log-tail (id &optional lines)
  "Return last LINES lines from the log file for ID.
LINES defaults to 5.  Returns nil if no log file exists."
  (let ((log-file (supervisor--log-file id))
        (n (or lines 5)))
    (when (file-exists-p log-file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents log-file)
            (goto-char (point-max))
            (forward-line (- n))
            (buffer-substring-no-properties (point) (point-max)))
        (error nil)))))

(defun supervisor--telemetry-process-tree (pid)
  "Return process-tree summary for PID as a plist, or nil.
Returns (:count N :pids (PID1 PID2 ...)) where :pids is bounded
to the first 20 descendants.  Returns nil if PID has no children."
  (when-let* ((descendants (supervisor--process-descendants pid)))
    (let ((count (length descendants))
          (bounded (cl-subseq descendants 0 (min 20 (length descendants)))))
      (list :count count :pids bounded))))

(defun supervisor--all-parsed-entries ()
  "Parse and validate all entries, returning list of valid parsed entries.
Invalid entries are stored in `supervisor--invalid' with reason strings.
Duplicate IDs are skipped with a warning.
Reads from the cached program list via `supervisor--effective-programs'."
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
    (supervisor--merge-unit-file-invalid)
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
    ;; Combine :after, :requires, and :wants for dependency tracking
    (dolist (entry entries)
      (let* ((id (supervisor-entry-id entry))
             (after (supervisor-entry-after entry))
             (requires (supervisor-entry-requires entry))
             (wants (supervisor-entry-wants entry))
             (all-deps (supervisor--deduplicate-stable
                        (append (cl-union after requires :test #'equal)
                                wants)))
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
        ;; Return entries with :after, :requires, and :wants stripped
        ;; Handle full (31-element), 13-element, and 11-element entries
        (mapcar (lambda (entry)
                  (let ((len (length entry)))
                    (cond
                     ;; Current schema entry: clear :after (10), :requires (14),
                     ;; and :wants (24).
                     ((>= len 31)
                      (append (cl-subseq entry 0 10)
                              (list nil)                ; clear :after
                              (cl-subseq entry 11 14)
                              (list nil)                ; clear :requires
                              (cl-subseq entry 15 24)
                              (list nil)                ; clear :wants
                              (cl-subseq entry 25)))    ; preserve remaining
                     ;; Legacy full entry: clear :after (8), :requires (12),
                     ;; and :wants (22).
                     ((>= len 23)
                      (append (cl-subseq entry 0 8)
                              (list nil)                ; clear :after
                              (cl-subseq entry 9 12)
                              (list nil)                ; clear :requires
                              (cl-subseq entry 13 22)
                              (list nil)                ; clear :wants
                              (cl-subseq entry 23)))
                     ;; 13+ element entry: clear :after (8) and :requires (12)
                     ((>= len 13)
                      (append (cl-subseq entry 0 8)
                              (list nil)                ; clear :after
                              (cl-subseq entry 9 12)
                              (list nil)                ; clear :requires
                              (cl-subseq entry 13)))    ; preserve new fields
                     ;; 11-element legacy entry: just clear :after (8)
                     ((>= len 11)
                      (append (cl-subseq entry 0 8)
                              (list nil)                ; clear :after
                              (cl-subseq entry 9)))
                     ;; Shorter entries: return as-is
                     (t entry))))
                entries)))))

;;; Logging

(defvar supervisor--last-log-directory-warning nil
  "Last warning string emitted for log-directory fallback issues.")

(defun supervisor--default-log-directory ()
  "Return the default user-local log directory."
  (expand-file-name "supervisor" user-emacs-directory))

(defun supervisor--warn-log-directory (format-string &rest args)
  "Emit a throttled warning for log-directory issues.
Use FORMAT-STRING and ARGS to compose the warning text.
This uses `message' directly to avoid recursive file logging when the
configured log directory is unavailable."
  (let ((warning (apply #'format format-string args)))
    (unless (equal warning supervisor--last-log-directory-warning)
      (setq supervisor--last-log-directory-warning warning)
      (message "Supervisor: WARNING - %s" warning))))

(defun supervisor--ensure-directory-writable (directory)
  "Ensure DIRECTORY exists and is writable.
Return DIRECTORY when usable, otherwise return nil."
  (condition-case nil
      (progn
        (unless (file-directory-p directory)
          (make-directory directory t))
        (and (file-directory-p directory)
             (file-writable-p directory)
             directory))
    (error nil)))

(defun supervisor--effective-log-directory ()
  "Return a writable log directory path, or nil.
Prefer `supervisor-log-directory'.  If it is not writable, fall back to
`supervisor--default-log-directory'.  If neither is writable, return nil."
  (let* ((configured supervisor-log-directory)
         (configured-dir (supervisor--ensure-directory-writable configured))
         (fallback (supervisor--default-log-directory))
         (fallback-dir (and (not configured-dir)
                            (not (equal configured fallback))
                            (supervisor--ensure-directory-writable fallback))))
    (cond
     (configured-dir configured-dir)
     (fallback-dir
      (supervisor--warn-log-directory
       "log directory %s is not writable; using %s"
       configured fallback-dir)
      fallback-dir)
     (t
      (supervisor--warn-log-directory
       "log directory %s is not writable; file logging disabled"
       configured)
      nil))))

(defun supervisor--ensure-log-directory ()
  "Return the effective writable log directory, or nil."
  (supervisor--effective-log-directory))

(defun supervisor--log-file (prog)
  "Return the log file path for PROG, or nil."
  (when-let* ((log-directory (supervisor--ensure-log-directory)))
    (expand-file-name (format "log-%s.log" prog) log-directory)))

;;;; Log writer lifecycle

(defun supervisor--logd-pid-dir ()
  "Return the directory for logd PID files.
Use `supervisor-logd-pid-directory' when set, otherwise
`supervisor-log-directory'."
  (or supervisor-logd-pid-directory
      (supervisor--effective-log-directory)
      supervisor-log-directory))

(defun supervisor--stream-writer-id (id stream)
  "Return PID-file ID for service ID and STREAM."
  (if (eq stream 'stderr)
      (format "%s.stderr" id)
    id))

(defun supervisor--write-logd-pid-file (writer-id proc)
  "Write a PID file for log writer PROC serving WRITER-ID.
The file is named `logd-WRITER-ID.pid' in `supervisor--logd-pid-dir'.
The rotation script reads these files when `--signal-reopen' is
given to send SIGHUP to all active writers."
  (let ((pid-file (expand-file-name (format "logd-%s.pid" writer-id)
                                    (supervisor--logd-pid-dir))))
    (condition-case err
        (write-region (number-to-string (process-id proc))
                      nil pid-file nil 'silent)
      (error
       (supervisor--log 'warning "%s: could not write PID file: %s"
                        writer-id (error-message-string err))))))

(defun supervisor--remove-logd-pid-file (writer-id)
  "Remove the PID file for log writer serving WRITER-ID."
  (let ((pid-file (expand-file-name (format "logd-%s.pid" writer-id)
                                    (supervisor--logd-pid-dir))))
    (when (file-exists-p pid-file)
      (delete-file pid-file))))

(defun supervisor--start-stream-writer (id stream log-file table)
  "Start log writer for service ID STREAM writing to LOG-FILE.
TABLE receives the process keyed by ID.  STREAM is `stdout' or `stderr'."
  (condition-case err
      (let* ((writer-id (supervisor--stream-writer-id id stream))
             (log-directory-raw (or (file-name-directory log-file)
                                    (supervisor--ensure-log-directory)
                                    supervisor-log-directory))
             (log-directory (directory-file-name log-directory-raw))
             (cmd (list supervisor-logd-command
                        "--file" log-file
                        "--max-file-size-bytes"
                        (number-to-string supervisor-logd-max-file-size)
                        "--log-dir" log-directory
                        "--prune-cmd"
                        (supervisor--build-prune-command)
                        "--prune-min-interval-sec"
                        (number-to-string
                         supervisor-logd-prune-min-interval)))
             (proc (make-process
                    :name (format "logd-%s" writer-id)
                    :command cmd
                    :connection-type 'pipe
                    :noquery t)))
        (set-process-query-on-exit-flag proc nil)
        (puthash id proc table)
        (supervisor--write-logd-pid-file writer-id proc)
        proc)
    (error
     (supervisor--log 'warning "%s(%s): log writer failed to start: %s"
                      id stream (error-message-string err))
     nil)))

(defun supervisor--start-writer (id log-file)
  "Start stdout log writer process for service ID writing to LOG-FILE."
  (supervisor--start-stream-writer id 'stdout log-file supervisor--writers))

(defun supervisor--start-stderr-writer (id log-file)
  "Start dedicated stderr log writer process for service ID and LOG-FILE."
  (supervisor--start-stream-writer id 'stderr log-file supervisor--stderr-writers))

(defun supervisor--start-stderr-pipe (id writer)
  "Start an internal stderr pipe process for service ID using WRITER."
  (condition-case err
      (let ((pipe
             (make-pipe-process
              :name (format "supervisor-stderr-%s" id)
              :coding 'no-conversion
              :filter (lambda (_proc output)
                        (when (process-live-p writer)
                          (process-send-string writer output)))
              :sentinel (lambda (_proc _event) nil)
              :noquery t)))
        (set-process-query-on-exit-flag pipe nil)
        (puthash id pipe supervisor--stderr-pipes)
        pipe)
    (error
     (supervisor--log 'warning "%s(stderr): could not create stderr pipe: %s"
                      id (error-message-string err))
     nil)))

(defun supervisor--stop-stream-writer (id stream table)
  "Stop stream writer for service ID STREAM from TABLE."
  (when-let* ((writer (gethash id table)))
    (when (process-live-p writer)
      (signal-process writer 'SIGTERM))
    (supervisor--remove-logd-pid-file (supervisor--stream-writer-id id stream))
    (remhash id table)))

(defun supervisor--stop-writer (id)
  "Stop all log writer state for service ID."
  (supervisor--stop-stream-writer id 'stdout supervisor--writers)
  (supervisor--stop-stream-writer id 'stderr supervisor--stderr-writers)
  (when-let* ((stderr-pipe (gethash id supervisor--stderr-pipes)))
    (when (process-live-p stderr-pipe)
      (delete-process stderr-pipe))
    (remhash id supervisor--stderr-pipes)))

(defun supervisor--stop-all-writers ()
  "Stop all log writer processes and clear writer state hashes."
  (maphash (lambda (id writer)
             (when (process-live-p writer)
               (signal-process writer 'SIGTERM))
             (supervisor--remove-logd-pid-file
              (supervisor--stream-writer-id id 'stdout)))
           supervisor--writers)
  (maphash (lambda (id writer)
             (when (process-live-p writer)
               (signal-process writer 'SIGTERM))
             (supervisor--remove-logd-pid-file
              (supervisor--stream-writer-id id 'stderr)))
           supervisor--stderr-writers)
  (maphash (lambda (_id pipe)
             (when (process-live-p pipe)
               (delete-process pipe)))
           supervisor--stderr-pipes)
  (clrhash supervisor--writers)
  (clrhash supervisor--stderr-writers)
  (clrhash supervisor--stderr-pipes))

(defun supervisor--build-prune-command ()
  "Build the shell command string for logd's --prune-cmd flag.
Return a string suitable for passing to logd as the value of its
`--prune-cmd' argument.  The command invokes the prune script with
the current `supervisor-log-directory' and
`supervisor-log-prune-max-total-bytes'."
  (let ((log-directory (or (supervisor--effective-log-directory)
                           supervisor-log-directory)))
    (format "%s --log-dir %s --max-total-bytes %d"
            (shell-quote-argument supervisor-log-prune-command)
            (shell-quote-argument log-directory)
            supervisor-log-prune-max-total-bytes)))

(defun supervisor--signal-writers-reopen ()
  "Send SIGHUP to all live log writers to trigger file reopen.
After external rotation renames the active log file, each logd
writer must reopen its file descriptor.  logd handles SIGHUP by
closing and reopening the configured log file."
  (maphash (lambda (_id writer)
             (when (process-live-p writer)
               (signal-process writer 'SIGHUP)))
           supervisor--writers)
  (maphash (lambda (_id writer)
             (when (process-live-p writer)
               (signal-process writer 'SIGHUP)))
           supervisor--stderr-writers))

(defun supervisor-run-log-maintenance ()
  "Run log maintenance: rotate, signal writers to reopen, then prune.
Execute the scheduled maintenance path asynchronously:
1. Run `supervisor-logrotate-command' to rotate active logs.
2. Signal all live writers to reopen their files.
3. Run `supervisor-log-prune-command' to enforce the directory size cap."
  (interactive)
  (if-let* ((log-dir (supervisor--effective-log-directory)))
      (let ((keep-days (number-to-string supervisor-logrotate-keep-days))
            (max-bytes (number-to-string supervisor-log-prune-max-total-bytes)))
        (supervisor--log 'info "log maintenance: rotating")
        (set-process-sentinel
         (start-process "supervisor-logrotate" nil
                        supervisor-logrotate-command
                        "--log-dir" log-dir
                        "--keep-days" keep-days)
         (lambda (_proc event)
           (when (string-match-p "finished" event)
             (supervisor--signal-writers-reopen)
             (supervisor--log 'info "log maintenance: pruning")
             (start-process "supervisor-log-prune" nil
                            supervisor-log-prune-command
                            "--log-dir" log-dir
                            "--max-total-bytes" max-bytes)))))
    (supervisor--log 'warning
                     "log maintenance skipped: no writable log directory")))

(defun supervisor--builtin-logrotate-command ()
  "Build the shell command for the built-in logrotate oneshot unit."
  (let ((log-directory (or (supervisor--effective-log-directory)
                           supervisor-log-directory)))
    (format "%s --log-dir %s --keep-days %d --signal-reopen --pid-dir %s"
            (shell-quote-argument supervisor-logrotate-command)
            (shell-quote-argument log-directory)
            supervisor-logrotate-keep-days
            (shell-quote-argument (supervisor--logd-pid-dir)))))

(defun supervisor--builtin-log-prune-command ()
  "Build the shell command for the built-in log-prune oneshot unit."
  (let ((log-directory (or (supervisor--effective-log-directory)
                           supervisor-log-directory)))
    (format "%s --log-dir %s --max-total-bytes %d"
            (shell-quote-argument supervisor-log-prune-command)
            (shell-quote-argument log-directory)
            supervisor-log-prune-max-total-bytes)))

(defun supervisor--builtin-programs ()
  "Return list of built-in program entries.
These are appended to disk-loaded programs at lowest priority.
A user unit file with the same ID overrides the built-in entry."
  (list
   (cons (supervisor--builtin-logrotate-command)
         (list :id "logrotate"
               :type 'oneshot
               :description "Rotate supervisor log files"))
   (cons (supervisor--builtin-log-prune-command)
         (list :id "log-prune"
               :type 'oneshot
               :after '("logrotate")
               :requires '("logrotate")
               :description "Prune supervisor log files"))
   ;; Built-in targets (lowest authority, user overrides win)
   (list nil :id "basic.target" :type 'target
         :description "Basic system initialization target")
   (list nil :id "multi-user.target" :type 'target
         :requires '("basic.target")
         :description "Multi-user services target")
   (list nil :id "graphical.target" :type 'target
         :requires '("multi-user.target")
         :description "Graphical session target")
   (list nil :id "default.target" :type 'target
         :description "Default startup target (alias)")
   ;; Init-transition canonical targets
   (list nil :id "rescue.target" :type 'target
         :requires '("basic.target")
         :description "Single-user rescue target")
   (list nil :id "shutdown.target" :type 'target
         :description "Shutdown synchronization barrier")
   (list nil :id "poweroff.target" :type 'target
         :requires '("shutdown.target")
         :description "Power-off target")
   (list nil :id "reboot.target" :type 'target
         :requires '("shutdown.target")
         :description "Reboot target")
   ;; Runlevel alias targets (immutable alias -> canonical mapping)
   (list nil :id "runlevel0.target" :type 'target
         :description "Alias for poweroff.target")
   (list nil :id "runlevel1.target" :type 'target
         :description "Alias for rescue.target")
   (list nil :id "runlevel2.target" :type 'target
         :description "Alias for multi-user.target")
   (list nil :id "runlevel3.target" :type 'target
         :description "Alias for multi-user.target")
   (list nil :id "runlevel4.target" :type 'target
         :description "Alias for multi-user.target")
   (list nil :id "runlevel5.target" :type 'target
         :description "Alias for graphical.target")
   (list nil :id "runlevel6.target" :type 'target
         :description "Alias for reboot.target")))

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
    ;; First pass: register all entries so we can filter :wants
    (dolist (entry entries)
      (puthash (supervisor-entry-id entry) entry supervisor--dag-entries))
    ;; Second pass: build dep graph (all deps filtered to DAG entries)
    (dolist (entry entries)
      (let* ((id (supervisor-entry-id entry))
             (enabled-p (supervisor-entry-enabled-p entry))
             (type (supervisor-entry-type entry))
             (after (cl-remove-if-not
                     (lambda (dep) (gethash dep supervisor--dag-entries))
                     (supervisor-entry-after entry)))
             (requires (cl-remove-if-not
                        (lambda (dep) (gethash dep supervisor--dag-entries))
                        (supervisor-entry-requires entry)))
             ;; Filter :wants to existing, non-masked entries (soft dep)
             (wants (cl-remove-if-not
                     (lambda (dep)
                       (and (gethash dep supervisor--dag-entries)
                            (not (eq 'masked
                                     (gethash dep supervisor--mask-override)))))
                     (supervisor-entry-wants entry)))
             (oneshot-blocking (supervisor-entry-oneshot-blocking entry))
             ;; Combine :after, :requires, and :wants for dependency tracking
             (all-deps (supervisor--deduplicate-stable
                        (append (cl-union after requires :test #'equal)
                                wants))))
        (puthash id (length all-deps) supervisor--dag-in-degree)
        (puthash id nil supervisor--dag-dependents)
        (puthash id idx supervisor--dag-id-to-index)
        (cl-incf idx)
        ;; Track blocking oneshots (only if enabled)
        (when (and enabled-p (eq type 'oneshot) oneshot-blocking)
          (puthash id t supervisor--dag-blocking))
        ;; Set initial state via FSM transition
        (cond
         ((not enabled-p)
          (push id disabled-ids)
          (supervisor--transition-state id 'disabled))
         ((> (length all-deps) 0)
          (supervisor--transition-state id 'waiting-on-deps))
         (t
          (supervisor--transition-state id 'pending)))))
    ;; Build dependents graph (all deps filtered to DAG entries)
    (dolist (entry entries)
      (let* ((id (supervisor-entry-id entry))
             (after (cl-remove-if-not
                     (lambda (dep) (gethash dep supervisor--dag-entries))
                     (supervisor-entry-after entry)))
             (requires (cl-remove-if-not
                        (lambda (dep) (gethash dep supervisor--dag-entries))
                        (supervisor-entry-requires entry)))
             (wants (cl-remove-if-not
                     (lambda (dep)
                       (and (gethash dep supervisor--dag-entries)
                            (not (eq 'masked
                                     (gethash dep supervisor--mask-override)))))
                     (supervisor-entry-wants entry)))
             (all-deps (supervisor--deduplicate-stable
                        (append (cl-union after requires :test #'equal)
                                wants))))
        (dolist (dep all-deps)
          (puthash dep (cons id (gethash dep supervisor--dag-dependents))
                   supervisor--dag-dependents))))
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
    ;; Check if this member's readiness unblocks any converging targets
    (when supervisor--target-member-reverse
      (dolist (target-id (gethash id supervisor--target-member-reverse))
        (when (gethash target-id supervisor--target-converging)
          (supervisor--target-check-convergence target-id))))
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
    (supervisor--dag-check-complete)))

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

;;; Target Convergence Protocol

(defun supervisor--target-init-convergence (target-members-hash)
  "Initialize convergence state from TARGET-MEMBERS-HASH.
TARGET-MEMBERS-HASH maps target ID to plist (:requires (ids) :wants (ids)).
Builds the reverse index for convergence callbacks."
  (setq supervisor--target-members target-members-hash)
  (setq supervisor--target-convergence (make-hash-table :test 'equal))
  (setq supervisor--target-convergence-reasons (make-hash-table :test 'equal))
  (setq supervisor--target-converging (make-hash-table :test 'equal))
  ;; Build reverse index: member-id -> list of target IDs
  (setq supervisor--target-member-reverse (make-hash-table :test 'equal))
  (maphash (lambda (target-id members)
             (dolist (mid (plist-get members :requires))
               (puthash mid (cons target-id
                                  (gethash mid supervisor--target-member-reverse))
                        supervisor--target-member-reverse))
             (dolist (mid (plist-get members :wants))
               (puthash mid (cons target-id
                                  (gethash mid supervisor--target-member-reverse))
                        supervisor--target-member-reverse)))
           target-members-hash))

(defun supervisor--target-begin-convergence (id)
  "Begin convergence for target ID.
Set state to converging and check for immediate resolution."
  (puthash id 'converging supervisor--target-convergence)
  (puthash id t supervisor--target-converging)
  (supervisor--target-check-convergence id))

(defun supervisor--target-check-convergence (id)
  "Check convergence for target ID and resolve if all required members terminal.
A required member is terminal when it appears in `supervisor--dag-ready'.
If all required members are terminal, resolve as reached (all healthy) or
degraded (any failed).  Wanted-member failures do not block convergence."
  (let* ((members (gethash id supervisor--target-members))
         (required (plist-get members :requires))
         (all-terminal t)
         (degraded-reasons nil))
    ;; Empty required list -> immediate reached
    (dolist (mid required)
      (cond
       ((gethash mid supervisor--dag-ready)
        ;; Terminal -- check if failure state
        (let ((state (gethash mid supervisor--entry-state)))
          (when (memq state '(failed-to-spawn startup-timeout))
            (push (format "%s: %s" mid state) degraded-reasons))))
       (t
        (setq all-terminal nil))))
    (when all-terminal
      (remhash id supervisor--target-converging)
      (if degraded-reasons
          (progn
            (puthash id 'degraded supervisor--target-convergence)
            (puthash id (nreverse degraded-reasons)
                     supervisor--target-convergence-reasons)
            (supervisor--log 'warning "target %s degraded: %s"
                             id (mapconcat #'identity
                                           (gethash id
                                                    supervisor--target-convergence-reasons)
                                           ", "))
            (supervisor--emit-event 'target-degraded id nil
                                    (list :reasons degraded-reasons)))
        (puthash id 'reached supervisor--target-convergence)
        (supervisor--log 'info "target %s reached" id)
        (supervisor--emit-event 'target-reached id nil nil))
      ;; Unlock dependents regardless of reached/degraded
      (supervisor--dag-mark-ready id))))

(defun supervisor--dag-start-entry-async (entry)
  "Start ENTRY asynchronously.
Mark ready immediately for simple processes, on exit for oneshot."
  (let ((id (supervisor-entry-id entry))
        (cmd (supervisor-entry-command entry))
        (delay (supervisor-entry-delay entry))
        (enabled-p (supervisor-entry-enabled-p entry))
        (restart-policy (supervisor-entry-restart-policy entry))
        (logging-p (supervisor-entry-logging-p entry))
        (stdout-log-file (supervisor-entry-stdout-log-file entry))
        (stderr-log-file (supervisor-entry-stderr-log-file entry))
        (type (supervisor-entry-type entry))
        (oneshot-blocking (supervisor-entry-oneshot-blocking entry))
        (oneshot-timeout (supervisor-entry-oneshot-timeout entry))
        (working-directory (supervisor-entry-working-directory entry))
        (environment (supervisor-entry-environment entry))
        (environment-file (supervisor-entry-environment-file entry))
        (restart-sec (supervisor-entry-restart-sec entry))
        (unit-file-directory (supervisor--unit-file-directory-for-id
                              (supervisor-entry-id entry)))
        (user (supervisor-entry-user entry))
        (group (supervisor-entry-group entry))
        (sandbox-entry (when (supervisor--sandbox-requesting-p entry) entry)))
    ;; Check effective enabled state (config + runtime override)
    (let ((effective-enabled (supervisor--get-effective-enabled id enabled-p)))
      (cond
       ;; Target: passive node, begin convergence protocol
       ((eq type 'target)
        (supervisor--transition-state id 'started)
        (puthash id t supervisor--dag-started)
        (supervisor--target-begin-convergence id))
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
                                (supervisor--dag-do-start
                                 id cmd logging-p stdout-log-file stderr-log-file
                                 type restart-policy
                                 oneshot-blocking oneshot-timeout
                                 working-directory environment
                                 environment-file restart-sec
                                 unit-file-directory
                                 user group
                                 sandbox-entry)))
                 supervisor--dag-delay-timers))
       ;; Start immediately
       (t
        (supervisor--dag-do-start
         id cmd logging-p stdout-log-file stderr-log-file
         type restart-policy
         oneshot-blocking oneshot-timeout
         working-directory environment
         environment-file restart-sec
         unit-file-directory
         user group
         sandbox-entry))))))

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

(defun supervisor--dag-do-start (id cmd logging-p stdout-log-file stderr-log-file
                                    type restart-policy
                                    _oneshot-blocking oneshot-timeout
                                    &optional working-directory environment
                                    environment-file restart-sec
                                    unit-file-directory
                                    user group
                                    sandbox-entry)
  "Start process ID with CMD, LOGGING-P, TYPE, RESTART-POLICY, ONESHOT-TIMEOUT.
STDOUT-LOG-FILE and STDERR-LOG-FILE are per-stream log file overrides.
Optional WORKING-DIRECTORY, ENVIRONMENT, ENVIRONMENT-FILE,
RESTART-SEC, UNIT-FILE-DIRECTORY, USER, GROUP, and SANDBOX-ENTRY
are passed through to `supervisor--start-process'."
  (puthash id t supervisor--dag-started)
  (puthash id (float-time) supervisor--start-times)
  (cl-incf supervisor--dag-active-starts)
  (let ((args (supervisor--build-launch-command cmd user group
                                                sandbox-entry)))
    (if (not (executable-find (car args)))
        (progn
          (supervisor--log 'warning "executable not found for %s: %s" id (car args))
          (supervisor--dag-handle-spawn-failure id))
      (let ((proc (supervisor--start-process
                   id cmd logging-p type restart-policy nil
                   working-directory environment
                   environment-file restart-sec
                   unit-file-directory
                   user group
                   stdout-log-file stderr-log-file
                   sandbox-entry)))
        (if (not proc)
            (supervisor--dag-handle-spawn-failure id)
          (supervisor--dag-handle-spawn-success id type oneshot-timeout))))))

(defun supervisor--dag-check-complete ()
  "Check if current stage is complete and invoke callback if so."
  (when (and supervisor--dag-complete-callback
             ;; All entries started (actually spawned, not just scheduled)
             (= (hash-table-count supervisor--dag-started)
                (hash-table-count supervisor--dag-entries))
             ;; No delayed entries pending
             (= 0 (hash-table-count supervisor--dag-delay-timers))
             ;; No blocking oneshots pending
             (= 0 (hash-table-count supervisor--dag-blocking))
             ;; No targets still converging
             (or (null supervisor--target-converging)
                 (= 0 (hash-table-count supervisor--target-converging))))
    (supervisor--log 'info "all entries complete")
    (let ((callback supervisor--dag-complete-callback))
      (setq supervisor--dag-complete-callback nil)
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
  (when (timerp supervisor--dag-timeout-timer)
    (cancel-timer supervisor--dag-timeout-timer))
  (setq supervisor--dag-in-degree nil)
  (setq supervisor--dag-dependents nil)
  (setq supervisor--dag-entries nil)
  (setq supervisor--dag-blocking nil)
  (setq supervisor--dag-started nil)
  (setq supervisor--dag-ready nil)
  (setq supervisor--dag-timeout-timers nil)
  (setq supervisor--dag-delay-timers nil)
  (setq supervisor--dag-id-to-index nil)
  (setq supervisor--dag-complete-callback nil)
  (setq supervisor--dag-timeout-timer nil)
  (setq supervisor--dag-pending-starts nil)
  (setq supervisor--dag-active-starts 0)
  ;; Clear DAG-only convergence temporaries (execution-phase tracking).
  ;; Preserve supervisor--target-convergence, convergence-reasons, and
  ;; target-members: these must outlive the DAG so the dashboard and
  ;; CLI can display convergence state after startup completes.
  (setq supervisor--target-converging nil)
  (setq supervisor--target-member-reverse nil))

;;; Process Management

(defun supervisor--get-effective-logging (id default-logging)
  "Get effective logging state for ID with DEFAULT-LOGGING as fallback."
  (let ((override (gethash id supervisor--logging)))
    (if override (eq override 'enabled) default-logging)))

(defun supervisor--get-effective-restart (id config-restart)
  "Get effective restart policy for ID.
CONFIG-RESTART is the config's restart policy symbol.
Legacy boolean values in config are normalized.
Returns a policy symbol from `supervisor--valid-restart-policies'."
  (let ((override (gethash id supervisor--restart-override))
        (normalized (supervisor--normalize-restart-policy config-restart)))
    (cond ((eq override 'enabled) 'always)
          ((eq override 'disabled) 'no)
          ((eq override 'always) 'always)
          ((eq override 'no) 'no)
          ((eq override 'on-failure) 'on-failure)
          ((eq override 'on-success) 'on-success)
          (t normalized))))

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

;;; Policy Mutators
;;
;; Shared policy mutation helpers used by both CLI and dashboard.
;; Each validates the entry and applies config-default normalization.
;; Callers are responsible for saving overrides and refreshing UI.
;; Return value: plist (:status STATUS :message MSG) where STATUS
;; is `applied', `skipped', or `error'.

(defun supervisor--policy-enable (id)
  "Enable entry ID by setting the appropriate override.
If the config default is already enabled, clear any stale override
instead of writing a redundant one.
Return a plist with `:status' and `:message'."
  (cond
   ((not (supervisor--ensure-overrides-loaded))
    (list :status 'error
          :message "Cannot enable entry: failed to load overrides"))
   ((gethash id supervisor--invalid)
    (list :status 'error :message (format "Cannot enable invalid entry: %s" id)))
   (t
    (let ((entry (supervisor--get-entry-for-id id)))
      (if (null entry)
          (list :status 'error :message (format "Unknown entry: %s" id))
        (let* ((config-enabled (supervisor-entry-enabled-p entry))
               (effective (supervisor--get-effective-enabled id config-enabled)))
          (if effective
              (list :status 'skipped :message (format "%s is already enabled" id))
            (if config-enabled
                (remhash id supervisor--enabled-override)
              (puthash id 'enabled supervisor--enabled-override))
            (list :status 'applied :message (format "Enabled %s" id)))))))))

(defun supervisor--policy-disable (id)
  "Disable entry ID by setting the appropriate override.
If the config default is already disabled, clear any stale override
instead of writing a redundant one.
Return a plist with `:status' and `:message'."
  (cond
   ((not (supervisor--ensure-overrides-loaded))
    (list :status 'error
          :message "Cannot disable entry: failed to load overrides"))
   ((gethash id supervisor--invalid)
    (list :status 'error
          :message (format "Cannot disable invalid entry: %s" id)))
   (t
    (let ((entry (supervisor--get-entry-for-id id)))
      (if (null entry)
          (list :status 'error :message (format "Unknown entry: %s" id))
        (let* ((config-enabled (supervisor-entry-enabled-p entry))
               (effective (supervisor--get-effective-enabled id config-enabled)))
          (if (not effective)
              (list :status 'skipped
                    :message (format "%s is already disabled" id))
            (if (not config-enabled)
                (remhash id supervisor--enabled-override)
              (puthash id 'disabled supervisor--enabled-override))
            (list :status 'applied
                  :message (format "Disabled %s" id)))))))))

(defun supervisor--policy-mask (id)
  "Mask entry ID so it is always disabled.
Return a plist with `:status' and `:message'."
  (cond
   ((not (supervisor--ensure-overrides-loaded))
    (list :status 'error
          :message "Cannot mask entry: failed to load overrides"))
   ((gethash id supervisor--invalid)
    (list :status 'error
          :message (format "Cannot mask invalid entry: %s" id)))
   (t
    (let ((entry (supervisor--get-entry-for-id id)))
      (if (null entry)
          (list :status 'error :message (format "Unknown entry: %s" id))
        (if (eq (gethash id supervisor--mask-override) 'masked)
            (list :status 'skipped
                  :message (format "%s is already masked" id))
          (puthash id 'masked supervisor--mask-override)
          (list :status 'applied :message (format "Masked %s" id))))))))

(defun supervisor--policy-unmask (id)
  "Unmask entry ID so the enabled state takes effect again.
Return a plist with `:status' and `:message'."
  (cond
   ((not (supervisor--ensure-overrides-loaded))
    (list :status 'error
          :message "Cannot unmask entry: failed to load overrides"))
   ((gethash id supervisor--invalid)
    (list :status 'error
          :message (format "Cannot unmask invalid entry: %s" id)))
   (t
    (let ((entry (supervisor--get-entry-for-id id)))
      (if (null entry)
          (list :status 'error :message (format "Unknown entry: %s" id))
        (if (not (eq (gethash id supervisor--mask-override) 'masked))
            (list :status 'skipped :message (format "%s is not masked" id))
          (remhash id supervisor--mask-override)
          (list :status 'applied
                :message (format "Unmasked %s" id))))))))

(defun supervisor--policy-set-restart (id policy)
  "Set restart POLICY for entry ID.
POLICY must be a member of `supervisor--valid-restart-policies'.
If POLICY matches the config default, clear the override instead.
When POLICY is `no', cancel any pending restart timer.
Return a plist with `:status' and `:message'."
  (cond
   ((not (supervisor--ensure-overrides-loaded))
    (list :status 'error
          :message "Cannot set restart policy: failed to load overrides"))
   ((not (memq policy supervisor--valid-restart-policies))
    (list :status 'error
          :message (format "Invalid restart policy: %s" policy)))
   ((gethash id supervisor--invalid)
    (list :status 'error
          :message (format "Cannot set restart for invalid entry: %s" id)))
   (t
    (let ((entry (supervisor--get-entry-for-id id)))
      (if (null entry)
          (list :status 'error :message (format "Unknown entry: %s" id))
        (if (memq (supervisor-entry-type entry) '(oneshot target))
            (list :status 'error
                  :message
                  (format "Restart policy not applicable to %s entries"
                          (supervisor-entry-type entry)))
          (let ((config-policy (supervisor--normalize-restart-policy
                                (supervisor-entry-restart-policy entry))))
            (if (eq policy config-policy)
                (remhash id supervisor--restart-override)
              (puthash id policy supervisor--restart-override))
            (when (eq policy 'no)
              (when-let* ((timer (gethash id supervisor--restart-timers)))
                (when (timerp timer)
                  (cancel-timer timer))
                (remhash id supervisor--restart-timers)))
            (list :status 'applied
                  :message (format "Restart policy for %s: %s"
                                   id policy)))))))))

(defun supervisor--policy-set-logging (id enabled-p)
  "Set logging for entry ID.  ENABLED-P is t for on, nil for off.
If ENABLED-P matches the config default, clear the override instead.
Return a plist with `:status' and `:message'."
  (cond
   ((not (supervisor--ensure-overrides-loaded))
    (list :status 'error
          :message "Cannot set logging: failed to load overrides"))
   ((gethash id supervisor--invalid)
    (list :status 'error
          :message (format "Cannot set logging for invalid entry: %s" id)))
   (t
    (let ((entry (supervisor--get-entry-for-id id)))
      (cond
       ((null entry)
        (list :status 'error :message (format "Unknown entry: %s" id)))
       ((eq (supervisor-entry-type entry) 'target)
        (list :status 'error
              :message "Logging not applicable to target entries"))
       (t
        (let ((config-logging (supervisor-entry-logging-p entry)))
          (if (eq enabled-p config-logging)
              (remhash id supervisor--logging)
            (puthash id (if enabled-p 'enabled 'disabled)
                     supervisor--logging))
          (list :status 'applied
                :message (format "Logging for %s: %s"
                                 id (if enabled-p "on" "off"))))))))))

(defun supervisor--handle-oneshot-exit (name proc-status exit-code)
  "Handle exit of oneshot process NAME.
PROC-STATUS is the process status symbol, EXIT-CODE is the exit code.
Logs completion, invokes callbacks, and notifies DAG scheduler.
For signal deaths, stores negated signal number (e.g., -9 for SIGKILL)
to distinguish from normal exits with positive exit codes.
When `:remain-after-exit' is t and exit is successful, sets the active
latch so the unit reports status `active' until explicitly stopped."
  (let ((stored-code (if (eq proc-status 'signal)
                         (- exit-code)  ; Negate signal number
                       exit-code)))
    (puthash name stored-code supervisor--oneshot-completed))
  ;; Set remain-after-exit active latch on successful exit
  (when (and (eq proc-status 'exit) (= exit-code 0))
    (when-let* ((entry (supervisor--get-entry-for-id name)))
      (when (supervisor-entry-remain-after-exit entry)
        (puthash name t supervisor--remain-active)
        (supervisor--log 'info "oneshot %s active (remain-after-exit)" name))))
  (if (eq proc-status 'signal)
      (supervisor--log 'warning "oneshot %s %s"
                       name (supervisor--format-exit-status proc-status exit-code))
    (if (> exit-code 0)
        (supervisor--log 'warning "oneshot %s %s"
                         name (supervisor--format-exit-status proc-status exit-code))
      (unless (gethash name supervisor--remain-active)
        (supervisor--log 'info "oneshot %s completed" name))))
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
Decrement remaining count and signal completion when all done.
Only activates after `supervisor--shutdown-remaining' has been set
to a positive value; early exits during the exec-stop phase are
ignored."
  (when (> supervisor--shutdown-remaining 0)
    (cl-decf supervisor--shutdown-remaining)
    (when (<= supervisor--shutdown-remaining 0)
      ;; Cancel timeout timer (early completion)
      (when supervisor--shutdown-timer
        (cancel-timer supervisor--shutdown-timer)
        (setq supervisor--shutdown-timer nil))
      ;; All services exited; stop log writers and complete
      (supervisor--stop-all-writers)
      (let ((cb supervisor--shutdown-callback))
        (setq supervisor--shutdown-callback nil)
        (clrhash supervisor--processes)
        (setq supervisor--shutdown-complete-flag t)
        (when cb (funcall cb))))))

(defun supervisor--schedule-restart (id cmd default-logging type config-restart
                                        proc-status exit-code
                                        &optional working-directory environment
                                        environment-file restart-sec
                                        unit-file-directory
                                        user group
                                        stdout-log-file stderr-log-file
                                        sandbox-entry)
  "Schedule restart of process ID after crash.
CMD, DEFAULT-LOGGING, TYPE, CONFIG-RESTART are original process params.
PROC-STATUS and EXIT-CODE describe the exit for logging.
WORKING-DIRECTORY, ENVIRONMENT, ENVIRONMENT-FILE, RESTART-SEC,
and UNIT-FILE-DIRECTORY are per-unit overrides preserved across restarts.
USER and GROUP are identity parameters preserved for restart.
STDOUT-LOG-FILE and STDERR-LOG-FILE preserve per-stream log targets.
SANDBOX-ENTRY, when non-nil, is the parsed entry tuple for sandbox."
  (supervisor--log 'info "%s %s, restarting..."
                   id (supervisor--format-exit-status proc-status exit-code))
  (let ((delay (or restart-sec supervisor-restart-delay)))
    (puthash id
             (run-at-time delay nil
                          #'supervisor--start-process
                          id cmd default-logging type config-restart t
                          working-directory environment
                          environment-file restart-sec
                          unit-file-directory
                          user group
                          stdout-log-file stderr-log-file
                          sandbox-entry)
             supervisor--restart-timers)))

(defun supervisor--make-process-sentinel (id cmd default-logging type config-restart
                                             &optional working-directory environment
                                             environment-file restart-sec
                                             unit-file-directory
                                             user group
                                             stdout-log-file stderr-log-file
                                             sandbox-entry)
  "Create a process sentinel for ID with captured parameters.
CMD, DEFAULT-LOGGING, TYPE, CONFIG-RESTART are stored for restart.
WORKING-DIRECTORY, ENVIRONMENT, ENVIRONMENT-FILE, RESTART-SEC,
and UNIT-FILE-DIRECTORY are per-unit overrides preserved across restarts.
USER and GROUP are identity parameters preserved for restart.
STDOUT-LOG-FILE and STDERR-LOG-FILE preserve per-stream log targets.
SANDBOX-ENTRY, when non-nil, is the parsed entry tuple for sandbox."
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
        ;; Stop the log writer for this service
        (supervisor--stop-writer name)
        ;; Record last exit info for telemetry
        (puthash name (list :status exit-status :code exit-code
                            :timestamp (float-time))
                 supervisor--last-exit-info)
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
                    (not (let ((ses (when-let* ((e (supervisor--get-entry-for-id name)))
                                     (supervisor-entry-success-exit-status e))))
                           (supervisor--should-restart-p
                            (supervisor--get-effective-restart name config-restart)
                            proc-status exit-code ses)))
                    (gethash name supervisor--failed)
                    (supervisor--check-crash-loop name))
	          (supervisor--schedule-restart id cmd default-logging type config-restart
	                                        proc-status exit-code
	                                        working-directory environment
	                                        environment-file restart-sec
	                                        unit-file-directory
	                                        user group
	                                        stdout-log-file stderr-log-file
	                                        sandbox-entry))))))

;;; Process Environment and Working Directory Helpers

(defun supervisor--parse-env-file (path)
  "Parse environment file at PATH and return an alist of (KEY . VALUE) pairs.
Ignore blank lines and comment lines (starting with `#' or `;').
Accept optional `export ' prefix.  Parse first `=' as separator.
Key must match [A-Za-z_][A-Za-z0-9_]*.  Lines that do not match
are logged as warnings."
  (let ((result nil)
        (line-num 0))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (while (not (eobp))
        (cl-incf line-num)
        (let ((line (string-trim (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position)))))
          ;; Skip blank lines and comments
          (unless (or (string-empty-p line)
                      (string-prefix-p "#" line)
                      (string-prefix-p ";" line))
            ;; Strip optional "export " prefix
            (when (string-prefix-p "export " line)
              (setq line (substring line 7)))
            ;; Parse KEY=VALUE
            (if (string-match "\\`\\([A-Za-z_][A-Za-z0-9_]*\\)=\\(.*\\)\\'" line)
                (push (cons (match-string 1 line)
                            (string-trim (match-string 2 line)))
                      result)
              (supervisor--log 'warning "%s:%d: invalid env line: %s"
                               path line-num line))))
        (forward-line 1)))
    (nreverse result)))

(defun supervisor--resolve-env-file-path (path unit-file-directory)
  "Resolve environment file PATH relative to UNIT-FILE-DIRECTORY.
Strip leading `-' prefix (optional missing marker) and return
a cons (OPTIONAL-P . RESOLVED-PATH)."
  (let ((optional (string-prefix-p "-" path)))
    (when optional
      (setq path (substring path 1)))
    (let ((resolved (expand-file-name path unit-file-directory)))
      (cons optional resolved))))

(defun supervisor--build-process-environment (env-files env-alist unit-file-directory)
  "Build effective `process-environment' for a unit.
Start from current `process-environment', apply ENV-FILES in order,
then apply ENV-ALIST pairs in order.  Later assignments for the
same key override earlier ones.  UNIT-FILE-DIRECTORY is used for
relative path resolution in env-files."
  (let ((env (copy-sequence process-environment)))
    ;; Apply env-files in order
    (dolist (path env-files)
      (let* ((resolved (supervisor--resolve-env-file-path path unit-file-directory))
             (optional (car resolved))
             (file (cdr resolved)))
        (if (file-readable-p file)
            (dolist (pair (supervisor--parse-env-file file))
              (setq env (cons (format "%s=%s" (car pair) (cdr pair))
                              env)))
          (unless optional
            (error "Required environment file not found: %s" file)))))
    ;; Apply env-alist in order (later wins for same key)
    (dolist (pair env-alist)
      (setq env (cons (format "%s=%s" (car pair) (cdr pair))
                      env)))
    env))

(defun supervisor--resolve-working-directory (dir unit-file-directory)
  "Resolve working directory DIR for a unit.
Expand `~' paths.  Resolve relative paths against UNIT-FILE-DIRECTORY.
Return the resolved absolute path, or signal an error if the directory
does not exist."
  (let ((resolved (expand-file-name dir unit-file-directory)))
    (unless (file-directory-p resolved)
      (error "Working directory does not exist: %s" resolved))
    resolved))

(defun supervisor--unit-file-directory-for-id (id)
  "Return the directory of the authoritative unit file for ID.
Return nil if no unit file is found or if `supervisor-units' is not loaded."
  (when (fboundp 'supervisor--unit-file-path)
    (when-let* ((path (supervisor--unit-file-path id)))
      (file-name-directory path))))

(defun supervisor--run-command-with-timeout (cmd timeout dir env log-file)
  "Run CMD synchronously with per-command TIMEOUT in seconds.
DIR is the working directory.  ENV is the process environment.
LOG-FILE, when non-nil, receives stdout and stderr output.
Return 0 on success, non-zero on failure.  Timeout returns exit
code 124 (matching coreutils timeout convention)."
  (let* ((default-directory (or dir default-directory))
         (process-environment (or env process-environment))
         (timed-out nil)
         (finished nil)
         (proc (make-process
                :name "supervisor-exec-cmd"
                :command (list shell-file-name shell-command-switch cmd)
                :connection-type 'pipe
                :stderr nil
                :filter (when log-file
                          (lambda (_p output)
                            (write-region output nil log-file t 'silent)))
                :sentinel (lambda (_p _e) (setq finished t))))
         (timer (run-at-time timeout nil
                             (lambda ()
                               (when (process-live-p proc)
                                 (setq timed-out t)
                                 (delete-process proc))))))
    (set-process-query-on-exit-flag proc nil)
    (while (not finished)
      (accept-process-output proc 0.1))
    (cancel-timer timer)
    (if timed-out 124 (process-exit-status proc))))

(defun supervisor--exec-command-chain (commands id dir env log-file timeout)
  "Execute COMMANDS sequentially for unit ID.
DIR is the working directory.  ENV is the process environment.
LOG-FILE receives output when non-nil.  TIMEOUT is the
per-command timeout in seconds.
Return t if all commands succeed (exit 0), nil otherwise.
All commands are executed regardless of individual failures."
  (let ((all-ok t))
    (dolist (cmd commands)
      (supervisor--log 'info "%s: executing: %s" id cmd)
      (let ((exit-code
             (condition-case err
                 (supervisor--run-command-with-timeout
                  cmd timeout dir env log-file)
               (error
                (supervisor--log 'warning "%s: command error: %s"
                                 id (error-message-string err))
                1))))
        (cond
         ((= exit-code 124)
          (supervisor--log 'warning "%s: command timed out after %ds: %s"
                           id timeout cmd)
          (setq all-ok nil))
         ((/= exit-code 0)
          (supervisor--log 'warning "%s: command failed (exit %d): %s"
                           id exit-code cmd)
          (setq all-ok nil)))))
    all-ok))

(defun supervisor--run-exec-stop-for-id (id)
  "Run exec-stop commands for unit ID if configured.
Look up the entry, verify it is a simple-type unit with
`:exec-stop' commands, resolve the unit's effective working
directory and environment, and execute the stop command chain.

Skip silently if the entry has no exec-stop commands, is not
a simple type, or cannot be found.  If context resolution fails,
log a warning and skip the chain rather than running commands in
the wrong context."
  (when-let* ((entry (supervisor--get-entry-for-id id))
              (exec-stop (supervisor-entry-exec-stop entry)))
    (when (eq (supervisor-entry-type entry) 'simple)
      (let* ((working-directory (supervisor-entry-working-directory entry))
             (environment (supervisor-entry-environment entry))
             (environment-file (supervisor-entry-environment-file entry))
             (logging-p (supervisor-entry-logging-p entry))
             (unit-file-directory (supervisor--unit-file-directory-for-id id))
             (dir (if working-directory
                      (condition-case err
                          (supervisor--resolve-working-directory
                           working-directory unit-file-directory)
                        (error
                         (supervisor--log
                          'warning
                          "%s: cannot resolve working directory for exec-stop: %s"
                          id (error-message-string err))
                         nil))
                    default-directory))
             (env (when dir
                    (if (or environment-file environment)
                        (condition-case err
                            (supervisor--build-process-environment
                             environment-file environment unit-file-directory)
                          (error
                           (supervisor--log
                            'warning
                            "%s: cannot resolve environment for exec-stop: %s"
                            id (error-message-string err))
                           nil))
                      process-environment))))
        (when (and dir env)
          (let* ((logging (supervisor--get-effective-logging id logging-p))
                 (log-file (when logging
                             (supervisor--log-file id))))
            (supervisor--exec-command-chain
             exec-stop id dir env log-file supervisor-shutdown-timeout)))))))

(defun supervisor--identity-source-trusted-p (id)
  "Return non-nil if the unit source for ID is trusted for identity change.
When supervisor runs as root and a unit requests identity change
via `:user' or `:group', the unit must come from a verifiable,
root-owned unit file that is not world-writable.  If the units
module is not loaded (standalone core mode) or the file cannot be
verified, return nil (fail closed).

This enforces the PLAN-user-group-privdrop.md requirement that
root-mode supervisor never runs identity-changing units from
untrusted sources."
  (when (fboundp 'supervisor--unit-file-path)
    (when-let* ((path (supervisor--unit-file-path id)))
      (when (file-exists-p path)
        (let ((attrs (file-attributes path 'integer)))
          (and attrs
               ;; File must be owned by root (uid 0)
               (zerop (file-attribute-user-id attrs))
               ;; File must not be world-writable
               (let ((mode (file-modes path)))
                 (and mode (zerop (logand mode #o002))))))))))

(defvar supervisor-runas-command
  (expand-file-name "libexec/supervisor-runas"
                    (file-name-directory (or load-file-name
                                             buffer-file-name "")))
  "Path to the supervisor-runas privilege-drop helper.
Used when `:user' or `:group' is set on a unit entry.")

(defun supervisor--libexec-build-targets ()
  "Return build target specs for bundled libexec helpers.
Each spec is a plist with keys `:name', `:binary-file', and `:source-file'."
  (list (list :name "supervisor-logd"
              :binary-file supervisor-logd-command
              :source-file (concat supervisor-logd-command ".c"))
        (list :name "supervisor-runas"
              :binary-file supervisor-runas-command
              :source-file (concat supervisor-runas-command ".c"))))

(defun supervisor--libexec-target-needs-build-p (target)
  "Return non-nil when TARGET needs compilation.
TARGET is a plist from `supervisor--libexec-build-targets'."
  (let ((source-file (plist-get target :source-file))
        (binary-file (plist-get target :binary-file)))
    (or (not (file-exists-p binary-file))
        (not (file-executable-p binary-file))
        (and (file-exists-p source-file)
             (file-newer-than-file-p source-file binary-file)))))

(defun supervisor--libexec-pending-build-targets ()
  "Return libexec helper targets that are missing, stale, or non-executable."
  (let ((pending nil))
    (dolist (target (supervisor--libexec-build-targets))
      (when (supervisor--libexec-target-needs-build-p target)
        (push target pending)))
    (nreverse pending)))

(defun supervisor--libexec-target-names (targets)
  "Return comma-separated helper names for TARGETS."
  (mapconcat (lambda (target) (plist-get target :name)) targets ", "))

(defun supervisor--find-libexec-compiler ()
  "Return the first available C compiler command, or nil."
  (cl-loop for candidate in supervisor-libexec-compiler-candidates
           for compiler = (executable-find candidate)
           when compiler return compiler))

(defun supervisor--compile-libexec-target (compiler target)
  "Compile TARGET with COMPILER.
COMPILER is a command path.  TARGET is a target plist.
Return nil on success, or an error string on failure."
  (let* ((name (plist-get target :name))
         (source-file (plist-get target :source-file))
         (binary-file (plist-get target :binary-file))
         (args (append supervisor-libexec-cflags
                       (list "-o" binary-file source-file))))
    (make-directory (file-name-directory binary-file) t)
    (with-temp-buffer
      (let ((status (apply #'call-process compiler nil t nil args))
            (output nil))
        (setq output (string-trim (buffer-string)))
        (if (and (integerp status) (zerop status))
            nil
          (format "%s build failed (%s): %s"
                  name status
                  (if (string-empty-p output)
                      (mapconcat #'identity (cons compiler args) " ")
                    output)))))))

;;;###autoload
(defun supervisor-build-libexec-helpers (&optional build-all)
  "Compile bundled libexec helper binaries.
When BUILD-ALL is non-nil, compile every helper that has a
corresponding source file.  Otherwise compile only missing or stale
helpers.  Return a plist with summary keys:
`:built', `:attempted', `:failed', and `:missing-source'."
  (interactive "P")
  (let ((targets nil)
        (missing-source nil)
        (failed nil)
        (built 0))
    (dolist (target (supervisor--libexec-build-targets))
      (let ((source-file (plist-get target :source-file))
            (binary-file (plist-get target :binary-file))
            (name (plist-get target :name)))
        (cond
         ((file-exists-p source-file)
          (when (or build-all
                    (supervisor--libexec-target-needs-build-p target))
            (push target targets)))
         ((or build-all
              (not (file-exists-p binary-file))
              (not (file-executable-p binary-file)))
          (push (format "%s source file missing: %s" name source-file)
                missing-source)))))
    (setq targets (nreverse targets))
    (let ((compiler (and targets (supervisor--find-libexec-compiler))))
      (when (and targets (not compiler))
        (push (format "No C compiler found in `supervisor-libexec-compiler-candidates': %S"
                      supervisor-libexec-compiler-candidates)
              failed))
      (when compiler
        (dolist (target targets)
          (let ((err (supervisor--compile-libexec-target compiler target)))
            (if err
                (push err failed)
              (cl-incf built))))))
    (setq failed (nreverse failed))
    (setq missing-source (nreverse missing-source))
    (let ((result (list :built built
                        :attempted (length targets)
                        :failed failed
                        :missing-source missing-source)))
      (when (called-interactively-p 'interactive)
        (cond
         ((and (zerop (plist-get result :attempted))
               (null failed)
               (null missing-source))
          (message "supervisor: libexec helpers are up to date"))
         ((and failed (null missing-source))
          (message "supervisor: libexec build failures: %d"
                   (length failed)))
         ((and (null failed) missing-source)
          (message "supervisor: helper sources missing: %d"
                   (length missing-source)))
         (t
          (message "supervisor: libexec built=%d failed=%d missing-source=%d"
                   built (length failed) (length missing-source)))))
      result)))

(defun supervisor--log-libexec-build-result (result)
  "Log warnings for libexec helper build RESULT."
  (dolist (failure (plist-get result :failed))
    (supervisor--log 'warning "%s" failure))
  (dolist (missing (plist-get result :missing-source))
    (supervisor--log 'warning "%s" missing)))

(defun supervisor--maybe-build-libexec-helpers ()
  "Maybe compile libexec helpers during startup.
When `supervisor-libexec-build-on-startup' is `prompt', show a
`y-or-n-p' prompt in graphical Emacs sessions.  In batch mode the
prompt is skipped and an info message is logged instead."
  (let ((pending (supervisor--libexec-pending-build-targets)))
    (when pending
      (pcase supervisor-libexec-build-on-startup
        ('never nil)
        ('automatic
         (supervisor--log-libexec-build-result
          (supervisor-build-libexec-helpers)))
        ('prompt
         (if (not noninteractive)
             (when (y-or-n-p
                    (format "Supervisor requires compiled C helpers (%s).  Build now? "
                            (supervisor--libexec-target-names pending)))
               (supervisor--log-libexec-build-result
                (supervisor-build-libexec-helpers)))
           (supervisor--log
            'info
            "libexec helpers need build (%s); run M-x supervisor-build-libexec-helpers"
            (supervisor--libexec-target-names pending))))
        (_ nil)))))

(defun supervisor--shell-metachar-p (cmd)
  "Return non-nil if CMD contain shell metacharacters.
When present, CMD must be run via `sh -c' rather than split
directly, because `split-string-and-unquote' does not interpret
shell operators like `&&', `||', pipes, or redirections."
  (string-match-p "[&|;<>$`\n]" cmd))

;;; Sandbox Profile Registry

(defun supervisor--sandbox-profile-args (profile)
  "Return the bwrap argument list for sandbox PROFILE.
PROFILE is a symbol: `none', `strict', `service', or `desktop'.
Returns nil for `none' (no sandbox).  The returned list does not
include the `bwrap' executable itself -- callers prepend that."
  (pcase profile
    ('none nil)
    ('strict
     (list "--unshare-all"
           "--die-with-parent"
           "--ro-bind" "/" "/"
           "--tmpfs" "/tmp"
           "--proc" "/proc"
           "--dev" "/dev"))
    ('service
     (list "--unshare-pid"
           "--unshare-ipc"
           "--unshare-uts"
           "--die-with-parent"
           "--ro-bind" "/" "/"
           "--tmpfs" "/tmp"
           "--proc" "/proc"
           "--dev" "/dev"
           "--dev-bind" "/dev/null" "/dev/null"
           "--dev-bind" "/dev/urandom" "/dev/urandom"))
    ('desktop
     (list "--unshare-pid"
           "--unshare-ipc"
           "--unshare-uts"
           "--die-with-parent"
           "--ro-bind" "/" "/"
           "--tmpfs" "/tmp"
           "--proc" "/proc"
           "--dev" "/dev"
           "--dev-bind" "/dev/null" "/dev/null"
           "--dev-bind" "/dev/urandom" "/dev/urandom"
           "--bind" (or (getenv "XDG_RUNTIME_DIR")
                        (format "/run/user/%d" (user-uid)))
           (or (getenv "XDG_RUNTIME_DIR")
               (format "/run/user/%d" (user-uid)))
           "--ro-bind" "/tmp/.X11-unix" "/tmp/.X11-unix"))
    (_ nil)))

(defun supervisor--sandbox-profile-default-network (profile)
  "Return the default network mode symbol for PROFILE.
`strict' defaults to `isolated'; others default to `shared'."
  (if (eq profile 'strict) 'isolated 'shared))

(defun supervisor--sandbox-build-argv (entry)
  "Build the bwrap wrapper argv for a sandbox-requesting ENTRY.
Return a list of strings starting with the bwrap executable path,
or nil if the entry does not request sandbox.  The returned argv
does not include the service command itself -- callers append that."
  (let ((profile (or (supervisor-entry-sandbox-profile entry) 'none)))
    (when (or (not (eq profile 'none))
              (supervisor-entry-sandbox-network entry)
              (supervisor-entry-sandbox-ro-bind entry)
              (supervisor-entry-sandbox-rw-bind entry)
              (supervisor-entry-sandbox-tmpfs entry)
              (supervisor-entry-sandbox-raw-args entry))
      (let* ((bwrap (or (executable-find "bwrap") "bwrap"))
             (base-args (or (supervisor--sandbox-profile-args profile)
                            ;; Minimal base when profile is none but knobs set
                            (list "--die-with-parent"
                                  "--ro-bind" "/" "/"
                                  "--proc" "/proc"
                                  "--dev" "/dev")))
             (effective-network
              (or (supervisor-entry-sandbox-network entry)
                  (supervisor--sandbox-profile-default-network profile)))
             (argv (list bwrap)))
        ;; Append profile base args
        (setq argv (append argv base-args))
        ;; Apply network override
        (when (eq effective-network 'isolated)
          (unless (member "--unshare-net" argv)
            (setq argv (append argv (list "--unshare-net")))))
        ;; Apply read-only bind overrides
        (dolist (path (supervisor-entry-sandbox-ro-bind entry))
          (setq argv (append argv (list "--ro-bind" path path))))
        ;; Apply read-write bind overrides
        (dolist (path (supervisor-entry-sandbox-rw-bind entry))
          (setq argv (append argv (list "--bind" path path))))
        ;; Apply tmpfs overrides
        (dolist (path (supervisor-entry-sandbox-tmpfs entry))
          (setq argv (append argv (list "--tmpfs" path))))
        ;; Apply raw args (expert mode, already validated)
        (when (supervisor-entry-sandbox-raw-args entry)
          (setq argv (append argv (supervisor-entry-sandbox-raw-args entry))))
        ;; Separator before service command
        (append argv (list "--"))))))

(defun supervisor--build-launch-command (cmd &optional user group
                                                       sandbox-entry)
  "Build the command argument list for launching CMD.
CMD is a shell command string.  USER and GROUP are optional
identity parameters for privilege drop.  When either is non-nil,
`supervisor-runas-command' is prepended with identity arguments
and the target program is resolved to an absolute path (the helper
uses direct execv without PATH search).
SANDBOX-ENTRY, when non-nil, is a parsed entry tuple from which
sandbox wrapper arguments are extracted.  Launch ordering is:
supervisor-runas -> bwrap -> service executable.
When CMD contains shell metacharacters (`&&', pipes, etc.), it is
wrapped as `sh -c CMD' so the shell interprets operators correctly.
Return a list suitable for the `make-process' :command keyword."
  (let* ((args (if (supervisor--shell-metachar-p cmd)
                   (list shell-file-name shell-command-switch cmd)
                 (split-string-and-unquote cmd)))
         (sandbox-argv (when sandbox-entry
                         (supervisor--sandbox-build-argv sandbox-entry))))
    ;; Build from inside out: args -> sandbox-argv+args -> runas+all
    (when sandbox-argv
      ;; Resolve program to absolute path inside sandbox
      (let ((resolved (executable-find (car args))))
        (when resolved
          (setcar args resolved)))
      (setq args (append sandbox-argv args)))
    (if (or user group)
        (let ((helper-args (list supervisor-runas-command))
              (resolved (unless sandbox-argv
                          ;; Only resolve when not already resolved above
                          (executable-find (car args)))))
          ;; Resolve program to absolute path for execv
          (when resolved
            (setcar args resolved))
          (when user
            (setq helper-args
                  (append helper-args
                          (list "--user" (if (integerp user)
                                            (number-to-string user)
                                          user)))))
          (when group
            (setq helper-args
                  (append helper-args
                          (list "--group" (if (integerp group)
                                             (number-to-string group)
                                           group)))))
          (append helper-args (list "--") args))
      args)))

(defun supervisor--start-process (id cmd default-logging type config-restart
                                     &optional is-restart
                                     working-directory environment
                                     environment-file restart-sec
                                     unit-file-directory
                                     user group
                                     stdout-log-file stderr-log-file
                                     sandbox-entry)
  "Start CMD with identifier ID.
DEFAULT-LOGGING is the config value; runtime override is checked at restart.
TYPE is `simple' (long-running) or `oneshot' (run once).
CONFIG-RESTART is the restart policy symbol.
IS-RESTART is t when called from a crash-triggered restart timer.
WORKING-DIRECTORY, ENVIRONMENT, ENVIRONMENT-FILE, and RESTART-SEC
are per-unit overrides from the unit file.
UNIT-FILE-DIRECTORY is the directory of the authoritative unit file,
captured at plan time for deterministic relative-path resolution.
USER and GROUP are optional identity parameters for privilege drop.
When set, `supervisor-runas-command' is used to launch the process
as the specified identity.  Requires root privileges.
STDOUT-LOG-FILE and STDERR-LOG-FILE are optional per-unit stream log
targets.  By default stderr follows stdout into the same log file.
SANDBOX-ENTRY, when non-nil, is a parsed entry tuple from which
sandbox wrapper arguments are built via bubblewrap."
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
              (and is-restart (eq (supervisor--get-effective-restart id config-restart) 'no))
              (and is-restart (gethash id supervisor--failed))
              (and is-restart (eq (gethash id supervisor--enabled-override) 'disabled))
              (and (gethash id supervisor--processes)
                   (process-live-p (gethash id supervisor--processes)))
              ;; Non-root supervisor cannot change process identity
              (and (or user group)
                   (not (zerop (user-uid)))
                   (progn
                     (puthash id
                              (format "identity change requires root (user=%s group=%s)"
                                      user group)
                              supervisor--spawn-failure-reason)
                     (supervisor--log
                      'warning
                      "%s: identity change requires root (user=%s group=%s)"
                      id user group)
                     t))
              ;; Root supervisor requires trusted unit source for identity change
              (and (or user group)
                   (zerop (user-uid))
                   (not (supervisor--identity-source-trusted-p id))
                   (progn
                     (puthash id
                              (format "unit source not trusted (user=%s group=%s)"
                                      user group)
                              supervisor--spawn-failure-reason)
                     (supervisor--log
                      'warning
                      "%s: identity change blocked - unit source not trusted (user=%s group=%s)"
                      id user group)
                     t)))
    (let* ((default-directory
            (if working-directory
                (condition-case err
                    (supervisor--resolve-working-directory
                     working-directory unit-file-directory)
                  (error
                   (supervisor--log 'warning "%s: %s" id (error-message-string err))
                   nil))
              default-directory))
           (process-environment
            (if (or environment-file environment)
                (condition-case err
                    (supervisor--build-process-environment
                     environment-file environment unit-file-directory)
                  (error
                   (supervisor--log 'warning "%s: %s" id (error-message-string err))
                   nil))
              process-environment)))
      (when (and default-directory process-environment)
        (let* ((args (supervisor--build-launch-command cmd user group
                                                        sandbox-entry))
               (logging (supervisor--get-effective-logging id default-logging))
               (default-log-file (when logging
                                   (supervisor--log-file id)))
               (stdout-log-target (when logging
                                    (or stdout-log-file default-log-file)))
               (stderr-log-target (when logging
                                    (or stderr-log-file stdout-log-target)))
               (split-stderr (and stderr-log-target
                                  (not (equal stderr-log-target
                                              stdout-log-target))))
               (writer (when stdout-log-target
                         (supervisor--start-writer id stdout-log-target)))
               (stderr-writer (when split-stderr
                                (supervisor--start-stderr-writer
                                 id stderr-log-target)))
               (stderr-pipe (when stderr-writer
                              (supervisor--start-stderr-pipe id stderr-writer)))
               (_ (when (and stderr-writer (not stderr-pipe))
                    (supervisor--stop-stream-writer
                     id 'stderr supervisor--stderr-writers)))
               (effective-split (and stderr-writer stderr-pipe))
               (proc (condition-case err
                         (make-process
                          :name id
                          :command args
                          :connection-type 'pipe
                          ;; Default is merged streams (stderr=nil).  In split mode
                          ;; stderr is routed through a dedicated pipe/writer pair.
                          :stderr (when effective-split stderr-pipe)
                          :filter (when writer
                                    (lambda (_proc output)
                                      (when (process-live-p writer)
                                        (process-send-string writer output))))
                          :sentinel (supervisor--make-process-sentinel
                                     id cmd default-logging type config-restart
                                     working-directory environment
                                     environment-file restart-sec
                                     unit-file-directory
                                     user group
                                     stdout-log-file stderr-log-file
                                     sandbox-entry))
                       (error
                        (supervisor--stop-writer id)
                        (puthash id (error-message-string err)
                                 supervisor--spawn-failure-reason)
                        (supervisor--log 'warning "%s: %s"
                                         id (error-message-string err))
                        nil))))
          (when proc
            (set-process-query-on-exit-flag proc nil)
            (puthash id proc supervisor--processes))
          proc)))))

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
  (let ((id (supervisor-entry-id entry))
        (cmd (supervisor-entry-command entry))
        (enabled-p (supervisor-entry-enabled-p entry))
        (restart-policy (supervisor-entry-restart-policy entry))
        (logging-p (supervisor-entry-logging-p entry))
        (stdout-log-file (supervisor-entry-stdout-log-file entry))
        (stderr-log-file (supervisor-entry-stderr-log-file entry))
        (type (supervisor-entry-type entry))
        (otimeout (supervisor-entry-oneshot-timeout entry))
        (working-directory (supervisor-entry-working-directory entry))
        (environment (supervisor-entry-environment entry))
        (environment-file (supervisor-entry-environment-file entry))
        (restart-sec (supervisor-entry-restart-sec entry))
        (unit-file-directory (supervisor--unit-file-directory-for-id
                              (supervisor-entry-id entry)))
        (user (supervisor-entry-user entry))
        (group (supervisor-entry-group entry))
        (sandbox-entry (when (supervisor--sandbox-requesting-p entry) entry)))
    (if (not (supervisor--get-effective-enabled id enabled-p))
        (progn
          (supervisor--log 'info "%s disabled (config or override), skipping" id)
          (when callback (funcall callback t)))
      (let ((args (supervisor--build-launch-command cmd user group
                                                    sandbox-entry)))
        (if (not (executable-find (car args)))
            (progn
              (supervisor--log 'warning "executable not found for %s: %s" id (car args))
              (supervisor--emit-event 'process-failed id nil nil)
              (when callback (funcall callback nil)))
          ;; Start the process
          (let ((proc (supervisor--start-process
                       id cmd logging-p type restart-policy nil
                       working-directory environment
                       environment-file restart-sec
                       unit-file-directory
                       user group
                       stdout-log-file stderr-log-file
                       sandbox-entry)))
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
   ;; Target entries are passive (no process to start)
   ((let ((e (supervisor--get-entry-for-id id)))
      (and e (eq (supervisor-entry-type e) 'target)))
    (list :status 'error :reason "cannot start a target unit"))
   ;; Already running
   ((and (gethash id supervisor--processes)
         (process-live-p (gethash id supervisor--processes)))
    (list :status 'skipped :reason "already running"))
   ;; Active remain-after-exit unit (no-op)
   ((gethash id supervisor--remain-active)
    (list :status 'skipped :reason "already active"))
   ;; Check if entry exists and can be started
   (t
    (let ((entry (supervisor--get-entry-for-id id)))
      (if (not entry)
          (list :status 'error :reason "not found")
        (let ((cmd (supervisor-entry-command entry))
              (restart-policy (supervisor-entry-restart-policy entry))
              (logging-p (supervisor-entry-logging-p entry))
              (stdout-log-file (supervisor-entry-stdout-log-file entry))
              (stderr-log-file (supervisor-entry-stderr-log-file entry))
              (type (supervisor-entry-type entry))
              (working-directory (supervisor-entry-working-directory entry))
              (environment (supervisor-entry-environment entry))
              (environment-file (supervisor-entry-environment-file entry))
              (restart-sec (supervisor-entry-restart-sec entry))
              (unit-file-directory (supervisor--unit-file-directory-for-id id))
              (user (supervisor-entry-user entry))
              (group (supervisor-entry-group entry))
              (sandbox-entry (when (supervisor--sandbox-requesting-p entry)
                               entry)))
          (cond
           ;; Masked (highest precedence - only mask blocks manual start)
           ((eq (gethash id supervisor--mask-override) 'masked)
            (list :status 'skipped :reason "masked"))
           ;; Non-root cannot change identity
           ((and (or user group) (not (zerop (user-uid))))
            (list :status 'error
                  :reason (format "identity change requires root (user=%s group=%s)"
                                  user group)))
           ;; Root trust gate: identity change requires trusted unit source
           ((and (or user group) (zerop (user-uid))
                 (not (supervisor--identity-source-trusted-p id)))
            (list :status 'error
                  :reason (format "unit source not trusted (user=%s group=%s)"
                                  user group)))
           ;; Executable not found
           ((not (executable-find
                  (car (supervisor--build-launch-command cmd user group
                                                        sandbox-entry))))
            (list :status 'error :reason "executable not found"))
           ;; All checks passed - start
           (t
            ;; Clear failed state and oneshot completion on manual start
            (remhash id supervisor--failed)
            (remhash id supervisor--restart-times)
            (remhash id supervisor--oneshot-completed)
            (remhash id supervisor--remain-active)
            (remhash id supervisor--spawn-failure-reason)
            ;; Clear manually-stopped so restart works again
            (remhash id supervisor--manually-stopped)
            (let ((proc (supervisor--start-process
                         id cmd logging-p type restart-policy nil
                         working-directory environment
                         environment-file restart-sec
                         unit-file-directory
                         user group
                         stdout-log-file stderr-log-file
                         sandbox-entry)))
              (if proc
                  (progn
                    ;; Track manual start only on success so reconcile
                    ;; preserves disabled running units.
                    (puthash id t supervisor--manually-started)
                    (list :status 'started :reason nil))
                (list :status 'error :reason "failed to start process")))))))))))

;;;; Kill Controls (PT3-N4)

(defun supervisor--kill-signal-for-id (id)
  "Return the effective kill signal for unit ID.
Return the configured `:kill-signal' if set, or `SIGTERM' as default."
  (if-let* ((entry (supervisor--get-entry-for-id id)))
      (or (supervisor-entry-kill-signal entry) 'SIGTERM)
    'SIGTERM))

(defun supervisor--kill-mode-for-id (id)
  "Return the effective kill mode for unit ID.
Return the configured `:kill-mode' if set, or `process' as default."
  (if-let* ((entry (supervisor--get-entry-for-id id)))
      (or (supervisor-entry-kill-mode entry) 'process)
    'process))

(defun supervisor--process-descendants (pid)
  "Return list of PIDs that are descendants of PID.
Use `list-system-processes' and `process-attributes' for tree traversal.
Snapshot all descendants at call time via breadth-first search.
Return nil and log a warning if OS/process metadata is unavailable."
  (condition-case err
      (let ((all-pids (list-system-processes))
            (descendants nil)
            (to-check (list pid)))
        (while to-check
          (let ((parent (pop to-check)))
            (dolist (cpid all-pids)
              (when-let* ((attrs (process-attributes cpid))
                          (ppid (alist-get 'ppid attrs)))
                (when (= ppid parent)
                  (push cpid descendants)
                  (push cpid to-check))))))
        descendants)
    (error
     (supervisor--log 'warning "PID %d: descendant discovery failed: %s"
                      pid (error-message-string err))
     nil)))

(defun supervisor--kill-with-descendants (proc)
  "Send SIGKILL to PROC and its discovered descendants.
If descendant discovery fails, log a warning and fall back to
process-only SIGKILL.  Descendant PIDs are snapshotted at call time."
  (when (process-live-p proc)
    (let* ((pid (process-id proc))
           (descendants (when pid
                          (supervisor--process-descendants pid))))
      ;; Kill main process
      (signal-process proc 'SIGKILL)
      ;; Kill descendants (best effort)
      (dolist (dpid descendants)
        (condition-case nil
            (signal-process dpid 'SIGKILL)
          (error nil)))
      (when descendants
        (supervisor--log 'info "%s: killed %d descendant(s) (mixed mode)"
                         (process-name proc) (length descendants))))))

(defun supervisor--manual-stop (id)
  "Attempt to manually stop entry with ID.
Returns a plist with :status and :reason.
:status is one of: stopped, skipped, error
:reason explains why (for skipped/error cases).

This function sets manually-stopped to suppress restart,
ensuring consistent behavior between dashboard and CLI.

When the unit has `:exec-stop' commands, they are executed
before the process is signaled.  The configured `:kill-signal'
\(default SIGTERM) is then sent to the main process.  If the
process does not exit within `supervisor-shutdown-timeout'
seconds, SIGKILL is sent.  For units with `:kill-mode' `mixed',
SIGKILL is also sent to discovered descendants at escalation time."
  (let ((entry (supervisor--get-entry-for-id id)))
    (if (and entry (eq (supervisor-entry-type entry) 'target))
        (list :status 'error :reason "cannot stop a target unit")
  (let ((proc (gethash id supervisor--processes)))
    (cond
     ;; Active remain-after-exit unit: process exited but latched active
     ((and (not (and proc (process-live-p proc)))
           (gethash id supervisor--remain-active))
      (remhash id supervisor--remain-active)
      (remhash id supervisor--oneshot-completed)
      (puthash id t supervisor--manually-stopped)
      (remhash id supervisor--manually-started)
      (list :status 'stopped :reason nil))
     ((not proc)
      (list :status 'skipped :reason "not running"))
     ((not (process-live-p proc))
      (list :status 'skipped :reason "not running"))
     (t
      ;; Mark as manually stopped so sentinel doesn't restart it
      (puthash id t supervisor--manually-stopped)
      ;; Clear manually-started so reconcile can treat it normally
      (remhash id supervisor--manually-started)
      ;; Execute exec-stop commands if configured
      (supervisor--run-exec-stop-for-id id)
      ;; Send configured kill-signal (default SIGTERM)
      (let ((kill-signal (supervisor--kill-signal-for-id id))
            (kill-mode (supervisor--kill-mode-for-id id)))
        (when (process-live-p proc)
          (signal-process proc kill-signal)
          ;; Set up SIGKILL escalation timer
          (run-at-time supervisor-shutdown-timeout nil
                       (lambda ()
                         (when (process-live-p proc)
                           (if (eq kill-mode 'mixed)
                               (supervisor--kill-with-descendants proc)
                             (signal-process proc 'SIGKILL)))))))
      (list :status 'stopped :reason nil)))))))

(defun supervisor--manual-kill (id &optional signal)
  "Attempt to send SIGNAL to running entry with ID.
Returns a plist with :status and :reason.
:status is one of: signaled, skipped, error
:reason explains why (for skipped/error cases).

SIGNAL defaults to the unit's configured `:kill-signal' or
`SIGTERM'.  Unlike `supervisor--manual-stop', this does not mark
the entry as manually stopped, so restart policy remains in
effect."
  (let* ((sig (or signal (supervisor--kill-signal-for-id id)))
         (proc (gethash id supervisor--processes)))
    (cond
     ((not proc)
      (list :status 'skipped :reason "not running"))
     ((not (process-live-p proc))
      (list :status 'skipped :reason "not running"))
     (t
      (signal-process proc sig)
      (list :status 'signaled :reason nil)))))

(defun supervisor--reset-failed (id)
  "Reset the failed state for entry ID.
Returns a plist with :status and :reason.
:status is one of: reset, skipped
Clears crash-loop tracking and failed oneshot completion results
so the entry can be restarted."
  (if (not (gethash id supervisor--failed))
      (list :status 'skipped :reason "not failed")
    (remhash id supervisor--failed)
    (remhash id supervisor--restart-times)
    (remhash id supervisor--spawn-failure-reason)
    ;; Clear failed oneshot completion (non-zero or signal exit).
    (let ((oneshot-exit (gethash id supervisor--oneshot-completed)))
      (when (and oneshot-exit (not (eql oneshot-exit 0)))
        (remhash id supervisor--oneshot-completed)))
    (list :status 'reset :reason nil)))

(defun supervisor--dag-force-complete ()
  "Force startup completion due to timeout.
Mark all unstarted entries as timed out and invoke the callback."
  (supervisor--log 'warning "startup timed out, forcing completion")
  ;; Mark all unstarted entries
  (maphash (lambda (id _entry)
             (unless (gethash id supervisor--dag-started)
               (puthash id t supervisor--dag-started)
               (supervisor--transition-state id 'startup-timeout)))
           supervisor--dag-entries)
  ;; Clear blocking oneshots
  (clrhash supervisor--dag-blocking)
  ;; Cancel delay timers
  (maphash (lambda (_id timer)
             (when (timerp timer)
               (cancel-timer timer)))
           supervisor--dag-delay-timers)
  (clrhash supervisor--dag-delay-timers)
  ;; Force-resolve any converging targets as degraded
  (when (and supervisor--target-converging
             (> (hash-table-count supervisor--target-converging) 0))
    (maphash (lambda (id _)
               (puthash id 'degraded supervisor--target-convergence)
               (puthash id '("startup timeout")
                        supervisor--target-convergence-reasons)
               (supervisor--log 'warning
                                "target %s degraded: startup timeout" id))
             supervisor--target-converging)
    (clrhash supervisor--target-converging))
  ;; Invoke callback
  (when supervisor--dag-complete-callback
    (let ((callback supervisor--dag-complete-callback))
      (setq supervisor--dag-complete-callback nil)
      (funcall callback))))

(defun supervisor--start-entries-async (entries callback
                                       &optional target-members)
  "Start ENTRIES asynchronously.  Call CALLBACK when complete.
Optional TARGET-MEMBERS is a hash of target ID to member plists
for convergence tracking."
  (if (null entries)
      (funcall callback)
    ;; Initialize DAG scheduler
    (supervisor--dag-init entries)
    (supervisor--target-init-convergence (or target-members
                                             (make-hash-table :test 'equal)))
    (setq supervisor--dag-complete-callback callback)
    ;; Set up startup timeout if configured
    (when supervisor-startup-timeout
      (setq supervisor--dag-timeout-timer
            (run-at-time supervisor-startup-timeout nil
                         #'supervisor--dag-force-complete)))
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
        ;; Check if already complete (e.g., all entries were disabled)
        (supervisor--dag-check-complete)))))

(defun supervisor--dag-start-with-deps (entries callback)
  "Start ENTRIES respecting DAG dependency and oneshot-blocking order.
Call CALLBACK when all entries are ready.

Unlike `supervisor--start-entries-async', this does not reinitialize
target convergence state and does not set a startup timeout.  This is
intended for mid-run operations like isolate that need DAG-ordered
starts without disrupting existing runtime state."
  (if (null entries)
      (funcall callback)
    ;; Initialize DAG scheduler (resets DAG variables only)
    (supervisor--dag-init entries)
    (setq supervisor--dag-complete-callback callback)
    (setq supervisor--dag-pending-starts nil)
    (setq supervisor--dag-active-starts 0)
    ;; Start all initially ready entries (in-degree = 0)
    (let ((ready-ids nil))
      (maphash (lambda (id in-deg)
                 (when (= 0 in-deg)
                   (push id ready-ids)))
               supervisor--dag-in-degree)
      ;; Sort by original order for stable startup
      (setq ready-ids (sort ready-ids
                            (lambda (a b)
                              (< (gethash a supervisor--dag-id-to-index 999)
                                 (gethash b supervisor--dag-id-to-index 999)))))
      (if (null ready-ids)
          (funcall callback)
        (dolist (id ready-ids)
          (supervisor--dag-try-start-entry id))
        (supervisor--dag-check-complete)))))

;;; Default target resolution

(defconst supervisor--target-alias-map
  '(("runlevel0.target" . "poweroff.target")
    ("runlevel1.target" . "rescue.target")
    ("runlevel2.target" . "multi-user.target")
    ("runlevel3.target" . "multi-user.target")
    ("runlevel4.target" . "multi-user.target")
    ("runlevel5.target" . "graphical.target")
    ("runlevel6.target" . "reboot.target"))
  "Immutable mapping of alias targets to canonical targets.
Uses systemd runlevel mapping semantics.")

(defconst supervisor--init-transition-targets
  '("rescue.target" "shutdown.target" "poweroff.target" "reboot.target"
    "runlevel0.target" "runlevel1.target" "runlevel2.target"
    "runlevel3.target" "runlevel4.target" "runlevel5.target"
    "runlevel6.target")
  "Targets not eligible for timer triggering.
Init-transition targets represent system state changes that must
not be triggered by the timer subsystem.")

(defun supervisor--resolve-target-alias (target-id)
  "Resolve TARGET-ID through the alias map.
Return the canonical target ID if TARGET-ID is an alias, or
TARGET-ID itself if it is not an alias."
  (or (cdr (assoc target-id supervisor--target-alias-map))
      target-id))

(defun supervisor--target-alias-p (target-id)
  "Return non-nil if TARGET-ID is a known alias target."
  (assoc target-id supervisor--target-alias-map))

(defun supervisor--resolve-default-target-link ()
  "Return the effective default-target-link value."
  (or supervisor--default-target-link-override
      supervisor-default-target-link))

(defun supervisor--resolve-startup-root (valid-id-set)
  "Resolve the startup root target from configuration.
VALID-ID-SET is a hash of valid entry IDs.
Signal `user-error' if resolved target does not exist or is not a target."
  (let* ((root supervisor-default-target)
         (resolved (if (equal root "default.target")
                       (supervisor--resolve-default-target-link)
                     root))
         ;; Resolve alias targets (e.g., runlevelN.target -> canonical)
         (resolved (supervisor--resolve-target-alias resolved)))
    (when (equal resolved "default.target")
      (user-error "Supervisor: default-target-link must not be \"default.target\" (circular alias)"))
    (unless (string-suffix-p ".target" resolved)
      (user-error "Supervisor: startup root `%s' is not a target (must end in .target)"
                  resolved))
    (unless (gethash resolved valid-id-set)
      (user-error "Supervisor: startup root target `%s' does not exist"
                  resolved))
    resolved))

;;;###autoload
(defun supervisor-start ()
  "Start all programs defined in unit files.
Uses async DAG scheduler with global topological ordering.

Ready semantics (when dependents are unblocked):
- Simple process: spawned (process started)
- Oneshot: exited (success or failure) or timed out
- Disabled entry: immediately ready
- Start failure: immediately ready (dependents proceed)"
  (interactive)
  (supervisor--maybe-build-libexec-helpers)
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
  (setq supervisor--overrides-loaded nil)
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
  (clrhash supervisor--writers)
  (clrhash supervisor--stderr-writers)
  (clrhash supervisor--stderr-pipes)
  (clrhash supervisor--restart-times)
  (clrhash supervisor--restart-timers)
  (clrhash supervisor--last-exit-info)
  (clrhash supervisor--oneshot-completed)
  (clrhash supervisor--remain-active)
  (clrhash supervisor--start-times)
  (clrhash supervisor--ready-times)
  (clrhash supervisor--cycle-fallback-ids)
  (clrhash supervisor--computed-deps)
  (clrhash supervisor--entry-state)
  (clrhash supervisor--spawn-failure-reason)
  ;; Reset timer state (if timer module loaded)
  (when (boundp 'supervisor--timer-state)
    (clrhash supervisor--timer-state))
  (when (boundp 'supervisor--timer-state-loaded)
    (setq supervisor--timer-state-loaded nil))
  (when (boundp 'supervisor--timer-list)
    (setq supervisor--timer-list nil))
  (when (boundp 'supervisor--scheduler-startup-time)
    (setq supervisor--scheduler-startup-time nil))
  (setq supervisor--current-plan nil)
  (setq supervisor--default-target-link-override nil)
  ;; Load persisted overrides (restores enabled/restart/logging overrides)
  (supervisor--load-overrides)
  (supervisor--dag-cleanup)

  ;; Seed default maintenance units on disk when missing.
  (when (fboundp 'supervisor--ensure-default-maintenance-units)
    (supervisor--ensure-default-maintenance-units))
  ;; Refresh programs cache from disk before building plan
  (supervisor--refresh-programs)
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
    ;; Resolve startup root and compute activation closure.
    ;; Only entries reachable from the root target are activated.
    ;; Skip when plan is empty (standalone core mode).
    (when (supervisor-plan-entries plan)
      (let ((valid-ids (make-hash-table :test 'equal))
            (entries-by-id (make-hash-table :test 'equal)))
        (dolist (entry (supervisor-plan-entries plan))
          (puthash (supervisor-entry-id entry) t valid-ids)
          (puthash (supervisor-entry-id entry) entry entries-by-id))
        (let* ((root (supervisor--resolve-startup-root valid-ids))
               (members (supervisor--materialize-target-members
                         (supervisor-plan-entries plan)))
               (closure (supervisor--expand-transaction
                         root entries-by-id members
                         (supervisor-plan-order-index plan))))
          (setf (supervisor-plan-target-members plan) members)
          (setf (supervisor-plan-activation-root plan) root)
          (setf (supervisor-plan-activation-closure plan) closure)
          ;; Filter by-target to only closure entries
          (setf (supervisor-plan-by-target plan)
                (cl-remove-if-not
                 (lambda (e)
                   (gethash (supervisor-entry-id e) closure))
                 (supervisor-plan-by-target plan))))))
    ;; Validate timers only when timer subsystem is active (and timer module loaded)
    (when (and (fboundp 'supervisor-timer-subsystem-active-p)
               (supervisor-timer-subsystem-active-p)
               (fboundp 'supervisor-timer-build-list))
      (supervisor-timer-build-list plan))
    ;; Initialize all entry states to pending via FSM
    (dolist (entry (supervisor-plan-entries plan))
      (supervisor--transition-state (car entry) 'pending))
    ;; Start entries asynchronously using plan's globally sorted data
    (supervisor--start-entries-async
     (supervisor-plan-by-target plan)
     (lambda ()
       (supervisor--dag-cleanup)
       (supervisor--log 'info "startup complete")
       (when (and (not supervisor--shutting-down)
                  (fboundp 'supervisor-timer-scheduler-start))
         (supervisor-timer-scheduler-start)))
     (supervisor-plan-target-members plan))))

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
  (supervisor--stop-all-writers)
  (clrhash supervisor--processes)
  (setq supervisor--shutdown-complete-flag t
        supervisor--shutting-down nil))

;;;###autoload
(defun supervisor-stop (&optional callback)
  "Stop all supervised processes gracefully (async).
First runs `:exec-stop' command chains for applicable simple units,
then sends each unit's configured `:kill-signal' (default SIGTERM)
to remaining live processes and returns.  A timer handles the
graceful shutdown period: after `supervisor-shutdown-timeout' seconds,
any survivors receive SIGKILL.  For units with `:kill-mode' `mixed',
SIGKILL is also sent to discovered descendants at escalation time.
Optional CALLBACK is called with no arguments when shutdown completes.
Check `supervisor--shutdown-complete-flag' to poll completion status
if needed.

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
  ;; Run exec-stop for applicable units before signal escalation.
  ;; The shutdown-remaining guard in supervisor--handle-shutdown-exit
  ;; prevents premature completion if processes exit during this phase.
  (let ((ids nil))
    (maphash (lambda (id _proc) (push id ids))
             supervisor--processes)
    (dolist (id ids)
      (when-let* ((proc (gethash id supervisor--processes)))
        (when (process-live-p proc)
          (supervisor--run-exec-stop-for-id id)))))
  ;; Cancel any restart timers scheduled by sentinels during exec-stop
  (maphash (lambda (_id timer)
             (when (timerp timer)
               (cancel-timer timer)))
           supervisor--restart-timers)
  (clrhash supervisor--restart-timers)
  ;; Send per-unit kill-signal (default SIGTERM) to all remaining live processes
  (maphash (lambda (name proc)
             (when (process-live-p proc)
               (signal-process proc (supervisor--kill-signal-for-id name))))
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
          (supervisor--stop-all-writers)
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
               ;; SIGKILL any survivors, respecting kill-mode
               (maphash (lambda (name proc)
                          (when (process-live-p proc)
                            (if (eq (supervisor--kill-mode-for-id name) 'mixed)
                                (supervisor--kill-with-descendants proc)
                              (signal-process proc 'SIGKILL))))
                        supervisor--processes)
               ;; Complete shutdown
               (supervisor--stop-all-writers)
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
                   (enabled-p (supervisor-entry-enabled-p entry))
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
             (enabled-p (supervisor-entry-enabled-p entry))
             (type (supervisor-entry-type entry))
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
             (let ((cmd (supervisor-entry-command entry))
                   (enabled-p (supervisor-entry-enabled-p entry))
                   (restart-policy (supervisor-entry-restart-policy entry))
                   (logging-p (supervisor-entry-logging-p entry))
                   (stdout-log-file (supervisor-entry-stdout-log-file entry))
                   (stderr-log-file (supervisor-entry-stderr-log-file entry))
                   (type (supervisor-entry-type entry))
                   (working-directory (supervisor-entry-working-directory entry))
                   (environment (supervisor-entry-environment entry))
                   (environment-file (supervisor-entry-environment-file entry))
                   (restart-sec (supervisor-entry-restart-sec entry))
                   (unit-file-directory (supervisor--unit-file-directory-for-id id))
                   (user (supervisor-entry-user entry))
                   (group (supervisor-entry-group entry))
                   (sandbox-entry (when (supervisor--sandbox-requesting-p entry)
                                    entry)))
               (when (supervisor--get-effective-enabled id enabled-p)
                 (let ((args (supervisor--build-launch-command cmd user group
                                                              sandbox-entry)))
                   (if (not (executable-find (car args)))
                       (progn
                         (supervisor--log 'warning "reconcile: executable not found for %s: %s"
                                          id (car args))
                         (supervisor--emit-event 'process-failed id nil nil))
                     (supervisor--log 'info "reconcile: starting %s entry %s" reason id)
                     (let ((proc (supervisor--start-process
                                  id cmd logging-p type restart-policy nil
                                  working-directory environment
                                  environment-file restart-sec
                                  unit-file-directory
                                  user group
                                  stdout-log-file stderr-log-file
                                  sandbox-entry)))
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
Internal function called by the config-watcher.
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
  "Find and parse entry for ID from the cached program list.
Unlike `supervisor--get-entry-for-id', this does not skip entries
present in `supervisor--invalid'.  This allows reload to pick up
entries whose config has been fixed since the last plan build.
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

- Running simple with `:exec-reload': execute reload commands without
  stopping or restarting the process.
- Running simple without `:exec-reload': stop gracefully, start with
  new definition.  Bypasses enabled/disabled policy so a running unit
  is never left stopped by reload.
- Running oneshot: update definition only (let it finish naturally).
- Not running: update stored definition only (next start uses new config).
- Masked: skip with warning.
- Unknown ID: error.

Returns a plist (:id ID :action ACTION) where ACTION is one of:
  \"reloaded\"        - running simple unit restarted or reload commands run
  \"updated\"         - definition refreshed (stopped or running oneshot)
  \"skipped (masked)\" - unit is masked, no action taken
  \"error: REASON\"   - not found, invalid config, or start/reload failed"
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
               (type (supervisor-entry-type entry)))
          (cond
           ;; Running simple with exec-reload: run reload commands,
           ;; keep process running.
           ((and running (eq type 'simple)
                 (supervisor-entry-exec-reload entry))
            (let* ((exec-reload (supervisor-entry-exec-reload entry))
                   (working-directory (supervisor-entry-working-directory entry))
                   (environment (supervisor-entry-environment entry))
                   (environment-file (supervisor-entry-environment-file entry))
                   (logging-p (supervisor-entry-logging-p entry))
                   (unit-file-directory (supervisor--unit-file-directory-for-id id))
                   (dir (if working-directory
                            (condition-case err
                                (supervisor--resolve-working-directory
                                 working-directory unit-file-directory)
                              (error
                               (supervisor--log
                                'warning
                                "%s: cannot resolve working directory for exec-reload: %s"
                                id (error-message-string err))
                               nil))
                          default-directory))
                   (env (when dir
                          (if (or environment-file environment)
                              (condition-case err
                                  (supervisor--build-process-environment
                                   environment-file environment
                                   unit-file-directory)
                                (error
                                 (supervisor--log
                                  'warning
                                  "%s: cannot resolve environment for exec-reload: %s"
                                  id (error-message-string err))
                                 nil))
                            process-environment))))
              (if (not (and dir env))
                  (list :id id
                        :action "error: cannot resolve exec-reload context")
                (let* ((logging (supervisor--get-effective-logging id logging-p))
                       (log-file (when logging
                                   (supervisor--log-file id))))
                  (if (supervisor--exec-command-chain
                       exec-reload id dir env log-file
                       supervisor-shutdown-timeout)
                      (list :id id :action "reloaded")
                    (list :id id
                          :action "error: reload command failed"))))))
           ;; Running simple without exec-reload: stop + start with
           ;; new definition.  Bypass supervisor--manual-start to avoid
           ;; disabled-policy refusal; reload's contract is config
           ;; hot-swap, not enable/disable enforcement.
           ((and running (eq type 'simple))
            (supervisor--manual-stop id)
            (remhash id supervisor--failed)
            (remhash id supervisor--restart-times)
            (remhash id supervisor--manually-stopped)
            (let* ((cmd (supervisor-entry-command entry))
                   (logging-p (supervisor-entry-logging-p entry))
                   (stdout-log-file (supervisor-entry-stdout-log-file entry))
                   (stderr-log-file (supervisor-entry-stderr-log-file entry))
                   (restart-policy (supervisor-entry-restart-policy entry))
                   (working-directory (supervisor-entry-working-directory entry))
                   (environment (supervisor-entry-environment entry))
                   (environment-file (supervisor-entry-environment-file entry))
                   (restart-sec (supervisor-entry-restart-sec entry))
                   (unit-file-directory (supervisor--unit-file-directory-for-id id))
                   (user (supervisor-entry-user entry))
                   (group (supervisor-entry-group entry))
                   (sandbox-entry (when (supervisor--sandbox-requesting-p entry)
                                    entry))
                   (exe (car (supervisor--build-launch-command cmd user group
                                                              sandbox-entry))))
              (if (not (executable-find exe))
                  (list :id id :action "error: executable not found")
                (let ((new-proc (supervisor--start-process
                                 id cmd logging-p type restart-policy nil
                                 working-directory environment
                                 environment-file restart-sec
                                 unit-file-directory
                                 user group
                                 stdout-log-file stderr-log-file
                                 sandbox-entry)))
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
            (remhash id supervisor--remain-active)
            (list :id id :action "updated"))))))))))

;;;###autoload
(defun supervisor-daemon-reload ()
  "Reload unit definitions from disk into memory without affecting runtime.
Re-reads unit files from authority roots, rebuilds the plan, and stores
the result in `supervisor--current-plan'.

Does NOT start, stop, or restart anything.  Runtime state is untouched.
After daemon-reload, the next `start' or `reload' operates on the
refreshed plan.

Returns a plist with :entries and :invalid counts."
  (interactive)

  ;; Seed default maintenance units on disk when missing.
  (when (fboundp 'supervisor--ensure-default-maintenance-units)
    (supervisor--ensure-default-maintenance-units))
  ;; Refresh programs cache from disk
  (supervisor--refresh-programs)
  (let ((plan (supervisor--build-plan (supervisor--effective-programs))))
    ;; Populate legacy globals from plan for dashboard/CLI visibility
    (clrhash supervisor--invalid)
    (when (boundp 'supervisor--invalid-timers)
      (clrhash supervisor--invalid-timers))
    (maphash (lambda (k v) (puthash k v supervisor--invalid))
             (supervisor-plan-invalid plan))
    (supervisor--merge-unit-file-invalid)
    ;; Resolve target metadata (activation-root, target-members, closure)
    ;; so dashboard target filter, header root, and drill-down work after
    ;; reload without requiring a full restart.
    (when (supervisor-plan-entries plan)
      (let ((valid-ids (make-hash-table :test 'equal))
            (entries-by-id (make-hash-table :test 'equal)))
        (dolist (entry (supervisor-plan-entries plan))
          (puthash (supervisor-entry-id entry) t valid-ids)
          (puthash (supervisor-entry-id entry) entry entries-by-id))
        (condition-case nil
            (let* ((root (supervisor--resolve-startup-root valid-ids))
                   (members (supervisor--materialize-target-members
                             (supervisor-plan-entries plan)))
                   (closure (supervisor--expand-transaction
                             root entries-by-id members
                             (supervisor-plan-order-index plan))))
              (setf (supervisor-plan-target-members plan) members)
              (setf (supervisor-plan-activation-root plan) root)
              (setf (supervisor-plan-activation-closure plan) closure)
              ;; Update runtime global so drill-down reflects reloaded state
              (setq supervisor--target-members members))
          (user-error nil))))
    ;; Store plan for later use (after metadata population)
    (setq supervisor--current-plan plan)
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

Services are defined as unit files in `supervisor-unit-authority-path'
\\=(default: ~/.config/supervisor.el/).  Each file contains a plist:

  ;; ~/.config/supervisor.el/nm-applet.el
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
- simple: Long-running daemons, restarted on crash per :restart policy
- oneshot: Run-once scripts, exit is expected

Stages (run sequentially):
- stage1, stage2, stage3, stage4: run in order (stage3 is default)

Use `M-x supervisor' to open the dashboard for monitoring and control.
Use `M-x supervisor-verify' to check config without starting processes."
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
