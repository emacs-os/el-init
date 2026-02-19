;;; elinit-core.el --- Elinit core engine -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; Author: telecommuter <telecommuter@riseup.net>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of elinit.el.

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

;; Core engine for elinit.el.
;; Run M-x elinit-handbook for full documentation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;; Timer Subsystem Gate
;;
;; The timer subsystem is gated behind this mode variable.
;; The full mode definition is in elinit-timer.el.

(defvar elinit-timer-subsystem-mode t
  "Non-nil when the timer subsystem is enabled.
Use `elinit-timer-subsystem-mode' command to toggle.
See elinit-timer.el for the full mode definition.")

;; Forward declarations - implementations in elinit-timer.el
;; These are optional; core guards calls with fboundp for standalone operation.
(declare-function elinit-timer-subsystem-active-p "elinit-timer" ())
(declare-function elinit-timer-build-list "elinit-timer" (plan))
(declare-function elinit-timer-scheduler-start "elinit-timer" ())
(declare-function elinit-timer-scheduler-stop "elinit-timer" ())
(declare-function elinit-timer-id "elinit-timer" (timer))
(declare-function elinit-timer-enabled "elinit-timer" (timer))
(declare-function elinit-timer-target "elinit-timer" (timer))

;; Forward declarations for unit-file module
(declare-function elinit--load-programs "elinit-units" ())
(declare-function elinit--ensure-default-maintenance-units "elinit-units" ())
(declare-function elinit--publish-authority-snapshot "elinit-units" ())
(declare-function elinit--read-authority-snapshot "elinit-units" ())
(declare-function elinit--active-authority-roots "elinit-units" ())
(declare-function elinit--unit-file-path "elinit-units" (id))
(defvar elinit-unit-directory)
(defvar elinit-unit-authority-path)

;; Forward declarations for optional features
(declare-function file-notify-add-watch "filenotify" (file flags callback))
(declare-function json-encode "json" (object))
(declare-function file-notify-rm-watch "filenotify" (descriptor))

;; Forward declaration for dashboard refresh (optional cross-module call)
(declare-function elinit--refresh-dashboard "elinit-dashboard" ())

(defun elinit--maybe-refresh-dashboard ()
  "Refresh dashboard if the dashboard module is loaded.
This allows elinit-core to work standalone without dashboard."
  (when (fboundp 'elinit--refresh-dashboard)
    (elinit--refresh-dashboard)))

(defgroup elinit nil
  "Emacs Lisp service manager."
  :group 'processes)

(defcustom elinit-timers nil
  "List of timer definitions for scheduled oneshot execution.
Each entry is a plist with required keys :id and :target.

Required keys:
  :id       - string, unique timer identifier
  :target   - string, ID of oneshot service (defined as a unit file)

Trigger keys (at least one required):
  :on-calendar      - calendar schedule (see below)
  :on-startup-sec   - seconds after elinit-start to run
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
  :group 'elinit)

(defvar elinit--builtin-timers
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
  "Built-in timer definitions merged with `elinit-timers'.
A user timer with the same `:id' overrides the built-in one.")

(defcustom elinit-log-directory
  (expand-file-name "elinit" user-emacs-directory)
  "Directory for elinit log files."
  :type 'directory
  :group 'elinit)

;;;; Log writer and maintenance paths

(defcustom elinit-logd-command
  (expand-file-name "libexec/elinit-logd"
                    (file-name-directory (or load-file-name
                                             buffer-file-name "")))
  "Path to the per-service log writer helper.
The helper reads service output from stdin and writes to a log file
with append semantics, enforcing a per-file size cap.  See
`elinit-logd-max-file-size' for the default cap."
  :type 'string
  :group 'elinit)

(defcustom elinit-logrotate-command
  (expand-file-name "sbin/elinit-logrotate"
                    (file-name-directory (or load-file-name
                                             buffer-file-name "")))
  "Path to the log rotation script.
Rotates active service logs and prunes rotated files older than
`elinit-logrotate-keep-days'."
  :type 'string
  :group 'elinit)

(defcustom elinit-log-prune-command
  (expand-file-name "sbin/elinit-log-prune"
                    (file-name-directory (or load-file-name
                                             buffer-file-name "")))
  "Path to the global log prune script.
Enforces a hard cap on total bytes in the log directory.
See `elinit-log-prune-max-total-bytes'."
  :type 'string
  :group 'elinit)

(defcustom elinit-logrotate-keep-days 14
  "Number of days to keep rotated log files.
Rotated files older than this are pruned by the logrotate script."
  :type 'integer
  :group 'elinit)

(defcustom elinit-logd-max-file-size 52428800
  "Maximum size in bytes for a single log file (default 50 MiB).
The log writer rotates the file locally when this limit is reached."
  :type 'integer
  :group 'elinit)

(defcustom elinit-log-prune-max-total-bytes 1073741824
  "Maximum total bytes allowed in the log directory (default 1 GiB).
The prune script deletes oldest rotated files first until the
directory is at or below this cap."
  :type 'integer
  :group 'elinit)

(defcustom elinit-logd-prune-min-interval 60
  "Minimum seconds between logd-triggered prune invocations.
Throttles prune calls from the log writer to avoid excessive I/O."
  :type 'integer
  :group 'elinit)

(defcustom elinit-log-follow-interval 1.0
  "Seconds between polls in journal follow mode."
  :type 'number
  :group 'elinit)

(defcustom elinit-log-default-max-bytes (* 5 1024 1024)
  "Default maximum bytes to read from log files in tail mode.
When journal or telemetry tail reads a log file without an explicit
record count, at most this many bytes from the end of the file are
decoded.  Set to nil to disable the cap (decode entire file)."
  :type '(choice (integer :tag "Bytes")
                 (const :tag "No limit" nil))
  :group 'elinit)

(defcustom elinit-logd-pid-directory nil
  "Directory for log writer PID files.
Used by the rotation script to discover writer processes for
reopen signaling.  When nil, defaults to `elinit-log-directory'."
  :type '(choice directory (const nil))
  :group 'elinit)

(defcustom elinit-libexec-build-on-startup 'prompt
  "Control libexec helper builds during `elinit-start'.
When set to `prompt', startup asks before building missing or stale
helpers.  When set to `automatic', startup builds without prompting.
When set to `never', startup never builds helpers automatically."
  :type '(choice (const :tag "Prompt before building" prompt)
                 (const :tag "Build automatically" automatic)
                 (const :tag "Never build on startup" never))
  :group 'elinit)

(defcustom elinit-libexec-compiler-candidates
  '("cc" "clang" "gcc")
  "Candidate compiler commands for building libexec helpers.
The first command found in variable `exec-path' is used."
  :type '(repeat string)
  :group 'elinit)

(defcustom elinit-libexec-cflags
  '("-Wall" "-Wextra" "-Werror" "-pedantic" "-std=c99" "-O2")
  "C compiler flags for libexec helper builds."
  :type '(repeat string)
  :group 'elinit)

(defcustom elinit-restart-delay 2
  "Seconds to wait before restarting a crashed process."
  :type 'integer
  :group 'elinit)

(defcustom elinit-max-restarts 3
  "Max restarts in `elinit-restart-window' before marking failed."
  :type 'integer
  :group 'elinit)

(defcustom elinit-restart-window 60
  "Time window in seconds for counting restarts."
  :type 'integer
  :group 'elinit)

(defcustom elinit-shutdown-timeout 3
  "Seconds to wait for processes to exit gracefully before SIGKILL."
  :type 'integer
  :group 'elinit)

(defcustom elinit-oneshot-default-blocking t
  "Default blocking behavior for oneshot processes.
When t, oneshots block convergence (wait for exit).
When nil, oneshots are async (fire-and-forget, don't block convergence)."
  :type 'boolean
  :group 'elinit)

(defcustom elinit-oneshot-timeout 30
  "Default timeout in seconds for blocking oneshots.
If a blocking oneshot takes longer, warn and continue.
Set to nil for infinite wait."
  :type '(choice integer (const nil))
  :group 'elinit)

(defcustom elinit-startup-timeout nil
  "Timeout in seconds for startup.
If startup takes longer than this, log a warning and force completion.
Set to nil (default) for no timeout."
  :type '(choice integer (const nil))
  :group 'elinit)

(defcustom elinit-max-concurrent-starts nil
  "Maximum number of processes to start concurrently during startup.
When set, limits parallel process spawning to avoid thundering herd.
Set to nil (default) for unlimited concurrent starts."
  :type '(choice integer (const nil))
  :group 'elinit)

(defcustom elinit-default-target "default.target"
  "Target to activate at startup.
The default value \"default.target\" is an alias that resolves
to `elinit-default-target-link'."
  :type 'string
  :group 'elinit)

(defcustom elinit-default-target-link "graphical.target"
  "Target that \"default.target\" resolves to.
Must end in \".target\" and must not be \"default.target\"."
  :type 'string
  :group 'elinit)

(defcustom elinit-sandbox-allow-raw-bwrap nil
  "When non-nil, allow per-unit raw bubblewrap arguments.
The `:sandbox-raw-args' unit key is rejected unless this gate is
enabled.  Enabling raw args bypasses the curated profile model and
is intended for expert users only."
  :type 'boolean
  :group 'elinit)

(defcustom elinit-log-format-binary-enable nil
  "When non-nil, allow per-unit binary log format.
The `:log-format binary' value is rejected unless this gate is
enabled.  Binary logging is experimental."
  :type 'boolean
  :group 'elinit)

(defcustom elinit-verbose nil
  "When non-nil, log all events including informational messages.
When nil, only warnings and errors are logged.
Verbose events include startup transitions, process start,
dependency unlocks, and restarts."
  :type 'boolean
  :group 'elinit)

(defcustom elinit-log-to-file nil
  "When non-nil, write elinit events to a log file.
The log file is `elinit.log' in the effective log directory.
If `elinit-log-directory' is not writable, elinit falls back
to the default user-local log directory.
This is independent of `elinit-verbose' - all events are logged
to file when enabled, regardless of verbose setting."
  :type 'boolean
  :group 'elinit)

(defcustom elinit-watch-config nil
  "When non-nil, watch the Emacs init file for modification.
When the init file is modified, automatically run `elinit--reconcile'.
The file watched is the value of `user-init-file'.
Set to a file path string to watch a specific file instead."
  :type '(choice (const :tag "Disabled" nil)
                 (const :tag "Watch init file" t)
                 (file :tag "Watch specific file"))
  :group 'elinit)

(defvar elinit--config-watch-descriptor nil
  "File notification descriptor for config watching.")

(defvar elinit--current-plan nil
  "The last plan built by `elinit-daemon-reload'.
When non-nil, holds a `elinit-plan' struct representing the most
recently loaded configuration.  Set by `elinit-daemon-reload' and
cleared by `elinit-start' (which builds its own fresh plan).")

;;; Program Loading

(defvar elinit--programs-cache)
(defvar elinit--target-alias-map)

(defun elinit--effective-programs ()
  "Return the cached program list from the last authority resolution.
Return the result stored in `elinit--programs-cache' by the most
recent call to `elinit--load-programs'.  If no cache exists yet,
perform a lazy initialization by loading from disk.  Returns nil
when the units module is not loaded (standalone core mode).

The cache is explicitly refreshed by `elinit-start' and
`elinit-daemon-reload' via `elinit--refresh-programs'.
Other consumers (dashboard, CLI, runtime lookups) read from the
cache without re-reading disk, matching systemd semantics where
unit file changes require an explicit daemon-reload."
  (when (fboundp 'elinit--load-programs)
    (if (and (boundp 'elinit--programs-cache)
             (not (eq elinit--programs-cache :not-yet-loaded)))
        elinit--programs-cache
      (elinit--load-programs))))

(defun elinit--refresh-programs ()
  "Refresh the programs cache by re-reading unit files from disk.
Call this before operations that need a fresh view of disk content,
such as `elinit-start' and `elinit-daemon-reload'.  Sets
`elinit--programs-cache' to the new result."
  (when (fboundp 'elinit--load-programs)
    (elinit--load-programs)))

(defvar elinit--invalid)

(defun elinit--merge-unit-file-invalid ()
  "Merge invalid unit-file entries into `elinit--invalid'.
Copies entries from `elinit--unit-file-invalid' into the runtime
invalid hash so they appear in the dashboard and CLI."
  (when (boundp 'elinit--unit-file-invalid)
    (maphash (lambda (k v) (puthash k v elinit--invalid))
             (symbol-value 'elinit--unit-file-invalid))))

;;; Logging

(defun elinit--elinit-log-file ()
  "Return path to the elinit-level log file, or nil."
  (when-let* ((log-directory (elinit--effective-log-directory)))
    (expand-file-name "elinit.log" log-directory)))

(defun elinit--log (level format-string &rest args)
  "Log a message at LEVEL with FORMAT-STRING and ARGS.
LEVEL is one of:
  `error'   - always shown
  `warning' - always shown (prefixed with WARNING)
  `info'    - only shown when `elinit-verbose' is non-nil
When `elinit-log-to-file' is non-nil, all events are also
written to the elinit log file."
  (let* ((prefix (pcase level
                   ('warning "WARNING - ")
                   ('error "ERROR - ")
                   (_ "")))
         (msg (format "Elinit: %s%s" prefix (apply #'format format-string args))))
    ;; Write to log file if enabled (all levels)
    (when elinit-log-to-file
      (when-let* ((log-file (elinit--elinit-log-file)))
        (let ((timestamp (format-time-string "[%Y-%m-%d %H:%M:%S] ")))
          (condition-case err
              (write-region (concat timestamp msg "\n") nil log-file t 'silent)
            (error
             (elinit--warn-log-directory
              "cannot write elinit log file %s: %s"
              log-file (error-message-string err)))))))
    ;; Display message based on level and verbose setting
    (when (or elinit-verbose
              (memq level '(error warning)))
      (message "%s" msg))))

(defun elinit--normalize-after (after)
  "Normalize AFTER to a list of ID strings."
  (cond ((null after) nil)
        ((stringp after) (list after))
        ((listp after) after)
        (t nil)))

(defun elinit--oneshot-blocking-p (plist)
  "Return non-nil if oneshot should block convergence.
Check PLIST for `:oneshot-blocking', `:oneshot-async', and fall back to default."
  (cond
   ((plist-member plist :oneshot-blocking)
    (plist-get plist :oneshot-blocking))
   ((plist-member plist :oneshot-async)
    (not (plist-get plist :oneshot-async)))
   (t elinit-oneshot-default-blocking)))

(defun elinit--oneshot-timeout-value (plist)
  "Return timeout in seconds for a oneshot, or nil for infinite.
Check PLIST for :oneshot-timeout and fall back to default."
  (if (plist-member plist :oneshot-timeout)
      (let ((val (plist-get plist :oneshot-timeout)))
        (if (and val (not (numberp val)))
            (progn
              (elinit--log 'warning ":oneshot-timeout must be number or nil, using default")
              elinit-oneshot-timeout)
          val))
    elinit-oneshot-timeout))

(defun elinit--plist-duplicate-keys (plist)
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

(defconst elinit--valid-keywords
  '(:id :type :delay :after :requires :enabled :disabled
    :restart :no-restart :logging :oneshot-blocking :oneshot-async :oneshot-timeout :tags
    :stdout-log-file :stderr-log-file
    :working-directory :environment :environment-file
    :exec-stop :exec-reload :restart-sec
    :description :documentation :before :wants
    :kill-signal :kill-mode :remain-after-exit :success-exit-status
    :user :group
    :wanted-by :required-by
    :sandbox-profile :sandbox-network :sandbox-ro-bind :sandbox-rw-bind
    :sandbox-tmpfs :sandbox-raw-args
    :log-format
    :limit-nofile :limit-nproc :limit-core :limit-fsize :limit-as
    :conflicts)
  "List of valid keywords for entry plists.")

(defconst elinit--valid-types '(simple oneshot target)
  "List of valid :type values.")

(defconst elinit--target-invalid-keywords
  '(:delay :restart :no-restart :logging :stdout-log-file :stderr-log-file
    :oneshot-blocking :oneshot-async :oneshot-timeout
    :working-directory :environment :environment-file
    :exec-stop :exec-reload :restart-sec
    :kill-signal :kill-mode :remain-after-exit :success-exit-status
    :user :group :wanted-by :required-by
    :sandbox-profile :sandbox-network :sandbox-ro-bind :sandbox-rw-bind
    :sandbox-tmpfs :sandbox-raw-args
    :log-format
    :limit-nofile :limit-nproc :limit-core :limit-fsize :limit-as)
  "Keywords invalid for :type target entries.")

(defconst elinit--valid-restart-policies '(no on-success on-failure always)
  "List of valid restart policy symbols.
`no' means never restart.
`on-success' means restart only on clean exit (exit 0 or clean signal).
`on-failure' means restart only on failure (non-zero exit or unclean signal).
`always' means restart on any exit.")

(defconst elinit--clean-exit-signals '(1 2 13 15)
  "Signal numbers considered clean exits.
HUP (1), INT (2), PIPE (13), TERM (15).")

(defconst elinit--simple-only-keywords
  '(:restart :no-restart :exec-stop :exec-reload :restart-sec
    :success-exit-status)
  "Keywords valid only for :type simple.")

(defconst elinit--oneshot-only-keywords
  '(:oneshot-blocking :oneshot-async :oneshot-timeout :remain-after-exit)
  "Keywords valid only for :type oneshot.")

(defconst elinit--sandbox-profiles '(none strict service desktop)
  "Valid sandbox profile symbols.
`none' disables sandboxing (default).  `strict' isolates all
namespaces with no host home bind.  `service' is restrictive
filesystem with shared network.  `desktop' extends service with
runtime socket and bus paths.")

(defconst elinit--sandbox-network-modes '(shared isolated)
  "Valid sandbox network mode symbols.")

(defconst elinit--sandbox-forbidden-paths '("/proc" "/dev")
  "Paths forbidden in custom sandbox bind and tmpfs lists.")

(defconst elinit--limit-keywords
  '(:limit-nofile :limit-nproc :limit-core :limit-fsize :limit-as)
  "Keywords for ulimit-style resource limits.
Each accepts an integer (applied as both soft and hard), a
string \"SOFT:HARD\" with integer components, or the symbol
`infinity' for unlimited.")

(defconst elinit--signal-number-alist
  '((SIGHUP . 1) (SIGINT . 2) (SIGQUIT . 3) (SIGILL . 4) (SIGTRAP . 5)
    (SIGABRT . 6) (SIGBUS . 7) (SIGFPE . 8) (SIGKILL . 9) (SIGUSR1 . 10)
    (SIGSEGV . 11) (SIGUSR2 . 12) (SIGPIPE . 13) (SIGALRM . 14) (SIGTERM . 15)
    (SIGSTKFLT . 16) (SIGCHLD . 17) (SIGCONT . 18) (SIGSTOP . 19)
    (SIGTSTP . 20) (SIGTTIN . 21) (SIGTTOU . 22) (SIGURG . 23)
    (SIGXCPU . 24) (SIGXFSZ . 25) (SIGVTALRM . 26) (SIGPROF . 27)
    (SIGWINCH . 28) (SIGIO . 29) (SIGPWR . 30) (SIGSYS . 31))
  "Mapping of signal name symbols to POSIX signal numbers.
Covers all signals in `elinit--known-signals'.")

(defun elinit--signal-to-number (sig)
  "Return the signal number for signal symbol SIG, or nil if unknown."
  (cdr (assq sig elinit--signal-number-alist)))

(defun elinit--clean-exit-p (proc-status exit-code
                                              &optional success-exit-status)
  "Return non-nil if PROC-STATUS and EXIT-CODE indicate a clean exit.
A clean exit is exit code 0, or a signal in `elinit--clean-exit-signals'.
SUCCESS-EXIT-STATUS, if non-nil, is a plist (:codes INTS :signals SYMS)
that extends the clean exit criteria."
  (or (and (eq proc-status 'exit) (eq exit-code 0))
      (and (eq proc-status 'signal)
           (memq exit-code elinit--clean-exit-signals))
      (and success-exit-status
           (eq proc-status 'exit)
           (memq exit-code (plist-get success-exit-status :codes)))
      (and success-exit-status
           (eq proc-status 'signal)
           (cl-some (lambda (sig)
                      (eq exit-code (elinit--signal-to-number sig)))
                    (plist-get success-exit-status :signals)))))

(defun elinit--should-restart-p (policy proc-status exit-code
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
       (not (elinit--clean-exit-p proc-status exit-code success-exit-status)))
      ('on-success
       (elinit--clean-exit-p proc-status exit-code success-exit-status))
      (_ nil))))

(defun elinit--normalize-restart-policy (value)
  "Normalize VALUE to a restart policy symbol.
Accepts boolean t/nil and policy symbols.
Returns a member of `elinit--valid-restart-policies'."
  (cond ((eq value t) 'always)
        ((eq value nil) 'no)
        ((memq value elinit--valid-restart-policies) value)
        (t value)))

(defun elinit--restart-policy-to-bool (policy)
  "Convert restart POLICY symbol to boolean for display.
Returns non-nil for any policy other than `no' or nil."
  (and policy (not (eq policy 'no))))

(defun elinit--cycle-restart-policy (current)
  "Return the next restart policy after CURRENT in the cycle.
Cycles through `no' -> `on-success' -> `on-failure' -> `always' -> `no'."
  (pcase current
    ('no 'on-success)
    ('on-success 'on-failure)
    ('on-failure 'always)
    ('always 'no)
    (_ 'no)))

(defconst elinit--rlimit-max (1- (expt 2 64))
  "Maximum value representable by the C rlimits helper.
The helper uses unsigned long long, which maxes at 2^64-1.
Values equal to this sentinel are rejected by the helper, so the
effective maximum is 2^64-2.")

(defun elinit--limit-component-valid-p (s)
  "Return non-nil if string S is a valid limit component.
Valid components are \"infinity\" or a non-negative integer that
fits in the C helper's unsigned long long range."
  (or (string= s "infinity")
      (and (string-match-p "\\`[0-9]+\\'" s)
           (let ((n (string-to-number s)))
             (and (integerp n)
                  (>= n 0)
                  (< n elinit--rlimit-max))))))

(defun elinit--validate-limit-value (key val)
  "Validate resource limit VAL for keyword KEY.
Return nil if valid, or an error string if invalid.
Accepted forms: non-negative integer, symbol `infinity',
string \"SOFT:HARD\" where SOFT and HARD are non-negative
integers or the word \"infinity\".  When both SOFT and HARD
are numeric, SOFT must not exceed HARD."
  (cond
   ((eq val 'infinity) nil)
   ((integerp val)
    (cond
     ((< val 0)
      (format "%s must be non-negative integer, got: %d" key val))
     ((>= val elinit--rlimit-max)
      (format "%s value too large for runtime helper, got: %d" key val))
     (t nil)))
   ((stringp val)
    (let ((parts (split-string val ":")))
      (if (not (= (length parts) 2))
          (format "%s string must be \"SOFT:HARD\", got: %S" key val)
        (let ((soft (car parts))
              (hard (cadr parts)))
          (cond
           ((not (and (elinit--limit-component-valid-p soft)
                      (elinit--limit-component-valid-p hard)))
            (format "%s components must be non-negative integers or \"infinity\", got: %S"
                    key val))
           ;; infinity soft with finite hard is always soft > hard
           ((and (string= soft "infinity") (not (string= hard "infinity")))
            (format "%s soft limit (infinity) exceeds hard limit (%s)"
                    key hard))
           ;; Both finite: soft must not exceed hard
           ((and (not (string= soft "infinity"))
                 (not (string= hard "infinity"))
                 (> (string-to-number soft) (string-to-number hard)))
            (format "%s soft limit (%s) exceeds hard limit (%s)"
                    key soft hard))
           (t nil))))))
   (t (format "%s must be integer, \"SOFT:HARD\" string, or symbol `infinity', got: %S"
              key val))))

(defun elinit--validate-entry (entry)
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
          (let ((dupes (elinit--plist-duplicate-keys plist)))
        (dolist (key dupes)
          (push (format "duplicate key %s" key) errors)))
      ;; Check for unknown keywords
      (let ((keys plist))
        (while keys
          (let ((key (car keys)))
            (unless (memq key elinit--valid-keywords)
              (push (format "unknown keyword %s" key) errors)))
          (setq keys (cddr keys))))
      ;; Check :type is valid symbol
      (when (plist-member plist :type)
        (let ((type-val (plist-get plist :type)))
          (cond
           ((not (symbolp type-val))
            (push ":type must be a symbol, not a string" errors))
           ((not (memq type-val elinit--valid-types))
            (push (format ":type must be one of %s" elinit--valid-types) errors)))))
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
                      (memq val elinit--valid-restart-policies))
            (push (format ":restart must be t, nil, or one of %s"
                          elinit--valid-restart-policies)
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
        (dolist (kw elinit--simple-only-keywords)
          (when (plist-member plist kw)
            (push (format "%s is invalid for :type oneshot" kw) errors))))
      (when (eq type 'simple)
        (dolist (kw elinit--oneshot-only-keywords)
          (when (plist-member plist kw)
            (push (format "%s is invalid for :type simple" kw) errors))))
      (when (eq type 'target)
        (dolist (kw elinit--target-invalid-keywords)
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
                     (assoc eid elinit--target-alias-map))
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
      ;; Check :conflicts is string or list of strings
      (when (plist-member plist :conflicts)
        (let ((val (plist-get plist :conflicts)))
          (unless (or (null val)
                      (stringp val)
                      (and (proper-list-p val)
                           (cl-every #'stringp val)))
            (push ":conflicts must be a string or list of strings" errors))))
      ;; Check dependency lists for empty strings and self-references
      (let ((effective-id (if (plist-member plist :id)
                              (plist-get plist :id)
                            (file-name-nondirectory (car entry)))))
        (dolist (dep-spec '((:after . ":after")
                            (:requires . ":requires")
                            (:before . ":before")
                            (:wants . ":wants")
                            (:wanted-by . ":wanted-by")
                            (:required-by . ":required-by")
                            (:conflicts . ":conflicts")))
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
                      (elinit--normalize-signal-name val))
            (push ":kill-signal must be a valid signal name" errors))))
      ;; Check :kill-mode is process or mixed
      (when (plist-member plist :kill-mode)
        (let ((val (plist-get plist :kill-mode)))
          (unless (or (null val)
                      (elinit--normalize-kill-mode val))
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
              (unless (elinit--normalize-signal-name item)
                (push (format ":success-exit-status contains unknown signal %s" item)
                      errors)))
             (t
              (push (format ":success-exit-status item must be int or signal name, got %S" item)
                    errors))))))
      ;; Check :user shape (string, non-negative integer, or nil)
      (when (plist-member plist :user)
        (let ((val (plist-get plist :user)))
          (cond
           ((null val) nil)
           ((and (stringp val) (string-empty-p val))
            (push ":user must not be an empty string" errors))
           ((and (integerp val) (< val 0))
            (push ":user must be a non-negative integer" errors))
           ((not (or (stringp val) (integerp val)))
            (push ":user must be a string, integer, or nil" errors)))))
      ;; Check :group shape (string, non-negative integer, or nil)
      (when (plist-member plist :group)
        (let ((val (plist-get plist :group)))
          (cond
           ((null val) nil)
           ((and (stringp val) (string-empty-p val))
            (push ":group must not be an empty string" errors))
           ((and (integerp val) (< val 0))
            (push ":group must be a non-negative integer" errors))
           ((not (or (stringp val) (integerp val)))
            (push ":group must be a string, integer, or nil" errors)))))
      ;; Sandbox validation
      ;; Check :sandbox-profile value
      (when (plist-member plist :sandbox-profile)
        (let ((val (plist-get plist :sandbox-profile)))
          (let ((sym (cond ((symbolp val) val)
                           ((stringp val) (intern val))
                           (t nil))))
            (unless (memq sym elinit--sandbox-profiles)
              (push (format ":sandbox-profile must be one of %s"
                            elinit--sandbox-profiles)
                    errors)))))
      ;; Check :sandbox-network value
      (when (plist-member plist :sandbox-network)
        (let ((val (plist-get plist :sandbox-network)))
          (let ((sym (cond ((symbolp val) val)
                           ((stringp val) (intern val))
                           (t nil))))
            (unless (memq sym elinit--sandbox-network-modes)
              (push (format ":sandbox-network must be one of %s"
                            elinit--sandbox-network-modes)
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
              (let ((normalized (directory-file-name (expand-file-name val))))
                (cond
                 ((string-empty-p val)
                  (push (format "%s must not contain empty paths" label)
                        errors))
                 ((not (file-name-absolute-p val))
                  (push (format "%s paths must be absolute" label)
                        errors))
                 ((member normalized elinit--sandbox-forbidden-paths)
                  (push (format "%s must not include forbidden path %s"
                                label val)
                        errors))
                 ((and check-exists (not (file-exists-p val)))
                  (push (format "%s source path does not exist: %s"
                                label val)
                        errors)))))
             ((and (proper-list-p val) (cl-every #'stringp val))
              (dolist (path val)
                (let ((normalized (directory-file-name (expand-file-name path))))
                  (cond
                   ((string-empty-p path)
                    (push (format "%s must not contain empty paths" label)
                          errors))
                   ((not (file-name-absolute-p path))
                    (push (format "%s paths must be absolute" label)
                          errors))
                   ((member normalized elinit--sandbox-forbidden-paths)
                    (push (format "%s must not include forbidden path %s"
                                  label path)
                          errors))
                   ((and check-exists (not (file-exists-p path)))
                    (push (format "%s source path does not exist: %s"
                                  label path)
                          errors))))))
             (t
              (push (format "%s must be a string or list of absolute path strings"
                            label)
                    errors))))))
      ;; Check :sandbox-raw-args shape and gate
      (when (plist-member plist :sandbox-raw-args)
        (let ((val (plist-get plist :sandbox-raw-args)))
          (cond
           ((not elinit-sandbox-allow-raw-bwrap)
            (push ":sandbox-raw-args requires elinit-sandbox-allow-raw-bwrap to be enabled"
                  errors))
           ((null val))
           ((not (and (proper-list-p val) (cl-every #'stringp val)))
            (push ":sandbox-raw-args must be a list of strings" errors))
           (t
            ;; Validate against conflicting/unsafe raw arg combinations.
            ;; Resolve effective network: explicit :sandbox-network wins,
            ;; otherwise use profile default (strict -> isolated, others
            ;; -> shared) per PLAN-bubble-wrap-integration.md line 133.
            (let* ((raw-network (plist-get plist :sandbox-network))
                   (raw-profile (plist-get plist :sandbox-profile))
                   (profile-sym (if (stringp raw-profile)
                                    (intern raw-profile)
                                  (or raw-profile 'none)))
                   (effective-network
                    (if raw-network
                        (if (stringp raw-network)
                            (intern raw-network)
                          raw-network)
                      (elinit--sandbox-profile-default-network
                       profile-sym))))
              (when (and (member "--share-net" val)
                         (eq effective-network 'isolated))
                (push ":sandbox-raw-args \"--share-net\" conflicts with effective network isolated"
                      errors))
              (when (and (member "--unshare-net" val)
                         (eq effective-network 'shared))
                (push ":sandbox-raw-args \"--unshare-net\" conflicts with effective network shared"
                      errors)))
            ;; Reject args that duplicate or conflict with profile-managed
            ;; namespace and mount setup.
            (dolist (arg '("--unshare-all" "--die-with-parent"
                           "--proc" "--dev"))
              (when (member arg val)
                (push (format ":sandbox-raw-args \"%s\" conflicts with profile-managed setup"
                              arg)
                      errors)))))))
      ;; Check :log-format value and gate
      (when (plist-member plist :log-format)
        (let ((val (plist-get plist :log-format)))
          (cond
           ((not (symbolp val))
            (push (format ":log-format must be symbol `text' or `binary', got: %S" val)
                  errors))
           ((not (memq val '(text binary)))
            (push (format ":log-format must be `text' or `binary', got: %S" val)
                  errors))
           ((and (eq val 'binary)
                 (not elinit-log-format-binary-enable))
            (push ":log-format binary requires elinit-log-format-binary-enable to be enabled"
                  errors)))))
      ;; Check resource limit values
      (dolist (key elinit--limit-keywords)
        (when (plist-member plist key)
          (let* ((val (plist-get plist key))
                 (err (elinit--validate-limit-value key val)))
            (when err (push err errors)))))
      ;; Check sandbox-requesting units for OS and binary prerequisites.
      ;; Per plan contract: a unit is sandbox-requesting when
      ;; :sandbox-profile is set to anything other than none, OR any
      ;; other sandbox key is present (plist-member).  Runtime
      ;; elinit--sandbox-requesting-p uses truthy checks on parsed
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

;; Forward declarations for elinit-verify and elinit-dry-run
(defvar elinit--invalid)
(defvar elinit--invalid-timers)
(defvar elinit--cycle-fallback-ids)
(defvar elinit--computed-deps)

(defun elinit--extract-id (entry idx)
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
(defun elinit-handbook ()
  "Open the elinit.el handbook (README.org) in a read-only buffer."
  (interactive)
  (let* ((lib-file (locate-library "elinit"))
         (dir (and lib-file (file-name-directory lib-file)))
         (readme (and dir (expand-file-name "README.org" dir))))
    (if (and readme (file-exists-p readme))
        (with-current-buffer (find-file-noselect readme)
          (read-only-mode 1)
          (switch-to-buffer (current-buffer)))
      (user-error "Cannot locate README.org in elinit package directory"))))

;;;###autoload
(defun elinit-verify ()
  "Verify all unit-file entries and `elinit-timers'.
Display results in a temporary buffer and populate `elinit--invalid'
and `elinit--invalid-timers' so the dashboard reflects validation state."
  (interactive)
  (clrhash elinit--invalid)
  (when (boundp 'elinit--invalid-timers)
    (clrhash elinit--invalid-timers))

  ;; Build plan to get entry validation results
  (let* ((plan (elinit--build-plan (elinit--effective-programs)))
         (entry-valid (length (elinit-plan-entries plan)))
         (entry-results nil))
    ;; Populate elinit--invalid from plan and unit-file invalids
    (maphash (lambda (k v) (puthash k v elinit--invalid))
             (elinit-plan-invalid plan))
    (elinit--merge-unit-file-invalid)
    ;; Collect entry results (entry is a tuple where car is the id)
    (dolist (entry (elinit-plan-entries plan))
      (push (format "OK      %s" (car entry)) entry-results))
    (maphash (lambda (id reason)
               (push (format "INVALID %s: %s" id reason) entry-results))
             elinit--invalid)
    ;; Validate timers using plan (only if timer module is loaded)
    (let* ((entry-invalid (hash-table-count elinit--invalid))
           (timers (when (fboundp 'elinit-timer-build-list)
                     (elinit-timer-build-list plan)))
           (timer-valid (length timers))
           (timer-invalid (if (boundp 'elinit--invalid-timers)
                              (hash-table-count elinit--invalid-timers)
                            0))
           (timer-results nil))
      ;; Collect timer results (only if timer module loaded)
      (when (fboundp 'elinit-timer-id)
        (dolist (timer timers)
          (push (format "OK      %s" (elinit-timer-id timer)) timer-results)))
      (when (boundp 'elinit--invalid-timers)
        (maphash (lambda (id reason)
                   (push (format "INVALID %s: %s" id reason) timer-results))
                 elinit--invalid-timers))
      ;; Display results
      (with-output-to-temp-buffer "*elinit-verify*"
        (princ (format "Services: %d valid, %d invalid\n" entry-valid entry-invalid))
        (dolist (line (nreverse entry-results))
          (princ (format "  %s\n" line)))
        (when (and elinit-timers (fboundp 'elinit-timer-build-list))
          (princ (format "\nTimers: %d valid, %d invalid\n" timer-valid timer-invalid))
          (dolist (line (nreverse timer-results))
            (princ (format "  %s\n" line)))))))
  ;; Refresh dashboard if open
  (elinit--maybe-refresh-dashboard))

;;;###autoload
(defun elinit-dry-run ()
  "Validate entries and show startup order without starting processes.
Display startup order, including dependency resolution and
cycle fallback behavior.  Uses the pure plan builder internally."
  (interactive)

  ;; Build plan using pure function
  (let ((plan (elinit--build-plan (elinit--effective-programs))))
    ;; Populate legacy globals for dashboard compatibility
    (clrhash elinit--invalid)
    (when (boundp 'elinit--invalid-timers)
      (clrhash elinit--invalid-timers))
    (clrhash elinit--cycle-fallback-ids)
    (clrhash elinit--computed-deps)
    (maphash (lambda (k v) (puthash k v elinit--invalid))
             (elinit-plan-invalid plan))
    (elinit--merge-unit-file-invalid)
    (maphash (lambda (k v) (puthash k v elinit--cycle-fallback-ids))
             (elinit-plan-cycle-fallback-ids plan))
    (maphash (lambda (k v) (puthash k v elinit--computed-deps))
             (elinit-plan-deps plan))
    ;; Compute activation closure for dry-run display
    (let ((root-id nil)
          (closure nil)
          (all-sorted (elinit-plan-by-target plan)))
      (when (elinit-plan-entries plan)
        (let ((valid-ids (make-hash-table :test 'equal))
              (entries-by-id (make-hash-table :test 'equal)))
          (dolist (entry (elinit-plan-entries plan))
            (puthash (elinit-entry-id entry) t valid-ids)
            (puthash (elinit-entry-id entry) entry entries-by-id))
          (condition-case nil
              (let* ((resolved (elinit--resolve-startup-root valid-ids))
                     (members (elinit--materialize-target-members
                               (elinit-plan-entries plan)))
                     (expanded (elinit--expand-transaction
                                resolved entries-by-id members
                                (elinit-plan-order-index plan))))
                (setq root-id resolved)
                (setq closure expanded))
            (user-error nil))))
      ;; Validate timers (only if timer module loaded)
      (let ((timers (when (fboundp 'elinit-timer-build-list)
                      (elinit-timer-build-list plan))))
        ;; Display from plan artifact
        (with-output-to-temp-buffer "*elinit-dry-run*"
          (princ "=== Elinit Dry Run ===\n\n")
          (princ (format "Services: %d valid, %d invalid\n"
                         (length (elinit-plan-entries plan))
                         (hash-table-count elinit--invalid)))
          (when root-id
            (princ (format "Activation root: %s\n" root-id)))
          (when (and elinit-timers (fboundp 'elinit-timer-build-list))
            (princ (format "Timers: %d valid, %d invalid\n"
                           (length timers)
                           (if (boundp 'elinit--invalid-timers)
                               (hash-table-count elinit--invalid-timers)
                             0))))
          (princ "\n")
          ;; Show invalid entries first (includes unit-file invalids)
          (when (> (hash-table-count elinit--invalid) 0)
            (princ "--- Invalid Services (skipped) ---\n")
            (maphash (lambda (id reason)
                       (princ (format "  %s: %s\n" id reason)))
                     elinit--invalid)
            (princ "\n"))
          ;; Show invalid timers (only if timer module loaded)
          (when (and (boundp 'elinit--invalid-timers)
                     (> (hash-table-count elinit--invalid-timers) 0))
            (princ "--- Invalid Timers (skipped) ---\n")
            (maphash (lambda (id reason)
                       (princ (format "  %s: %s\n" id reason)))
                     elinit--invalid-timers)
            (princ "\n"))
          ;; Display activated entries (closure) or full list if no root
          (let ((activated (if closure
                               (cl-remove-if-not
                                (lambda (e)
                                  (gethash (elinit-entry-id e) closure))
                                all-sorted)
                             all-sorted)))
            (princ (format "--- Activation order (%d entries) ---\n"
                           (length activated)))
            (let ((order 1))
              (dolist (entry activated)
                (let* ((id (elinit-entry-id entry))
                       (type (elinit-entry-type entry))
                       (delay (elinit-entry-delay entry))
                       (enabled-p (elinit-entry-enabled-p entry))
                       (deps (gethash id (elinit-plan-deps plan)))
                       (cycle (gethash id (elinit-plan-cycle-fallback-ids
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
                                      (gethash (elinit-entry-id e)
                                               closure))
                                    all-sorted)))
                (when not-activated
                  (princ (format "--- Not activated (%d entries) ---\n"
                                 (length not-activated)))
                  (dolist (entry not-activated)
                    (princ (format "  %s [%s]\n"
                                   (elinit-entry-id entry)
                                   (elinit-entry-type entry))))
                  (princ "\n")))))
          ;; Show timer summary if any
          (when timers
            (princ "--- Timers ---\n")
            (dolist (timer timers)
              (princ (format "  %s -> %s%s\n"
                             (elinit-timer-id timer)
                             (elinit-timer-target timer)
                             (if (elinit-timer-enabled timer)
                                 "" " DISABLED"))))
            (princ "\n"))
          (princ "=== End Dry Run ===\n"))))))

;;; State Variables

(defvar elinit--processes (make-hash-table :test 'equal)
  "Hash table mapping program names to their process objects.")

(defvar elinit--restart-override (make-hash-table :test 'equal)
  "Hash table of restart overrides: nil=inherit config.
Values are policy symbols (`always', `no', `on-success', `on-failure')
or legacy values (`enabled', `disabled') which are migrated on read.")

(defvar elinit--enabled-override (make-hash-table :test 'equal)
  "Hash table of enabled overrides: nil=inherit config, `enabled', `disabled'.
Runtime overrides take effect on next start (manual or restart).")

(defvar elinit--mask-override (make-hash-table :test 'equal)
  "Hash table of mask overrides: id to `masked' or nil.
Masked entries are always disabled regardless of enable overrides.")

(defvar elinit--restart-times (make-hash-table :test 'equal)
  "Hash table mapping program names to list of recent restart timestamps.")

(defvar elinit--failed (make-hash-table :test 'equal)
  "Hash table of program names that have crash-looped and are marked failed.")

(defvar elinit--logging (make-hash-table :test 'equal)
  "Hash table tracking logging state per process (runtime override).")

(defvar elinit--writers (make-hash-table :test 'equal)
  "Hash table mapping service ID to writer process for per-service log writers.
Each writer is a `elinit-logd' subprocess that receives service output
on stdin and handles all disk I/O.")

(defvar elinit--stderr-writers (make-hash-table :test 'equal)
  "Hash table mapping service ID to dedicated stderr writer processes.
Entries are present only when a unit uses split stdout/stderr log files.")

(defvar elinit--stderr-pipes (make-hash-table :test 'equal)
  "Hash table mapping service ID to stderr pipe process objects.
These pipe processes receive stderr output from service processes and
forward it to `elinit--stderr-writers'.")

(defvar elinit--manually-stopped (make-hash-table :test 'equal)
  "Hash table tracking entries manually stopped via CLI or dashboard.
Entries in this table should not auto-restart until explicitly started again.
This is cleared when an entry is started, allowing normal restart behavior.")

(defvar elinit--manually-started (make-hash-table :test 'equal)
  "Hash table tracking entries manually started via CLI or dashboard.
Used by reconcile to preserve disabled units that were explicitly started.
Per the systemctl model, `start' on a disabled unit runs it this session
without changing enabled state, and reconcile should not stop it.")

(defvar elinit--last-exit-info (make-hash-table :test 'equal)
  "Hash table mapping entry IDs to last exit information.
Each value is a plist (:status STATUS :code CODE :timestamp TIME)
where STATUS is `exited', `signal', or `unknown', CODE is the exit
code or signal number, and TIME is the `float-time' of exit.")

;;; Overrides (see elinit-overrides.el)
;;
;; The overrides persistence subsystem, effective state getters, and
;; policy mutators are extracted to elinit-overrides.el.
;; All defvar definitions referenced by elinit-overrides are
;; above this point, so the require is safe here.
(require 'elinit-overrides)

(defvar elinit--oneshot-completed (make-hash-table :test 'equal)
  "Hash table tracking oneshot completion.  Value is exit code.")

(defvar elinit--remain-active (make-hash-table :test 'equal)
  "Hash table tracking oneshot units in active latch state.
Non-nil value means the unit has `:remain-after-exit' t and exited
successfully, so its status is `active' until explicitly stopped.")

(defvar elinit--oneshot-callbacks (make-hash-table :test 'equal)
  "Hash table of ID -> (callback . timeout-timer) for oneshot blocking.
Used for event-driven oneshot completion notification.")

(defvar elinit--shutdown-callback nil
  "Callback to invoke when all processes have terminated during shutdown.")

(defvar elinit--shutdown-remaining 0
  "Count of processes still alive during shutdown.")

(defvar elinit--shutdown-timer nil
  "Timer for SIGKILL timeout during graceful shutdown.")

(defvar elinit--shutdown-complete-flag nil
  "Non-nil means shutdown has completed.  For callers that need to poll.")

(defvar elinit--timers nil
  "List of pending delayed-start timers.")

(defvar elinit--restart-timers (make-hash-table :test 'equal)
  "Hash table mapping IDs to pending restart timers.")

(defvar elinit--conflict-suppressed (make-hash-table :test 'equal)
  "Hash table of entry IDs suppressed by conflict.
Maps ID to the conflict-requester ID that caused the suppression.
Suppressed entries do not auto-restart until explicitly reactivated.")

(defvar elinit--shutting-down nil
  "Non-nil when elinit is shutting down (prevents restarts).")

(defvar elinit--invalid (make-hash-table :test 'equal)
  "Hash table mapping invalid entry IDs to reason strings.")

(defvar elinit--start-times (make-hash-table :test 'equal)
  "Hash table mapping entry IDs to start timestamps (float-time).")

(defvar elinit--ready-times (make-hash-table :test 'equal)
  "Hash table mapping entry IDs to ready timestamps (float-time).")

(defvar elinit--cycle-fallback-ids (make-hash-table :test 'equal)
  "Hash table of entry IDs with deps cleared due to cycle fallback.
When a cycle is detected, :after, :requires, and :wants edges are
all cleared for the affected entries.")

(defvar elinit--computed-deps (make-hash-table :test 'equal)
  "Hash table of ID -> validated :after list (existing deps only).")

(defvar elinit--entry-state (make-hash-table :test 'equal)
  "Hash table of ID -> state symbol for detailed status.
States: waiting-on-deps, delayed, disabled, pending,
failed-to-spawn, startup-timeout, started.")

(defvar elinit--spawn-failure-reason (make-hash-table :test 'equal)
  "Hash table of ID -> string with specific spawn failure reason.
Populated when `elinit--start-process' rejects a launch due to
identity or other precondition failures.  Consulted by
`elinit--compute-entry-reason' for richer diagnostics.")

;;; Entry Lifecycle FSM (Phase 5)

(defconst elinit--valid-states
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

(defconst elinit--state-transitions
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

(defun elinit--transition-state (id new-state &optional force)
  "Transition entry ID to NEW-STATE.
Validates that NEW-STATE is a valid state and that the transition
from the current state is legal.  Signals an error for invalid
transitions unless FORCE is non-nil.

Returns t if transition succeeded, nil if forced through invalid."
  (unless (memq new-state elinit--valid-states)
    (error "Invalid state %s for entry %s" new-state id))
  (let* ((current (gethash id elinit--entry-state))
         (allowed (cdr (assq current elinit--state-transitions))))
    (cond
     ;; Valid transition
     ((memq new-state allowed)
      (puthash id new-state elinit--entry-state)
      t)
     ;; Invalid but forced
     (force
      (elinit--log 'warning "Forced invalid transition %s -> %s for %s"
                       current new-state id)
      (puthash id new-state elinit--entry-state)
      nil)
     ;; Invalid, not forced - signal error
     (t
      (error "Invalid transition %s -> %s for entry %s" current new-state id)))))

;;; Structured Event Dispatcher (Phase 6)

(defconst elinit--event-types
  '(startup-begin startup-complete process-started process-ready
    process-exit process-failed cleanup
    timer-trigger timer-overlap timer-success timer-failure
    target-reached target-degraded)
  "Valid event types for the structured event system.
- `startup-begin': startup begins processing
- `startup-complete': startup finished processing
- `process-started': process successfully spawned
- `process-ready': process became ready (simple=spawned, oneshot=exited)
- `process-exit': managed process exited
- `process-failed': process failed to spawn
- `cleanup': cleanup phase beginning
- `timer-trigger': timer fired its target oneshot
- `timer-overlap': timer skipped due to target still running
- `timer-success': timer's target completed successfully
- `timer-failure': timer's target failed
- `target-reached': all required members of a target reached terminal state
- `target-degraded': target converged but some required members failed")

(defvar elinit-event-hook nil
  "Hook run for all elinit events.
Called with one argument: an event plist with the following keys:
  :type  - event type symbol (see `elinit--event-types')
  :ts    - timestamp (float-time)
  :id    - entry ID string (for process events, nil for startup/global)
  :data  - additional payload plist (event-type specific)")

(defun elinit--emit-event (type &optional id data)
  "Emit a structured event of TYPE with optional ID and DATA.
TYPE must be a member of `elinit--event-types'.
Runs `elinit-event-hook' with the event plist."
  (unless (memq type elinit--event-types)
    (error "Invalid event type: %s" type))
  (let ((event (list :type type
                     :ts (float-time)
                     :id id
                     :data data)))
    (run-hook-with-args 'elinit-event-hook event)
    ;; Log startup transitions to *Messages*
    (pcase type
      ('startup-begin
       (message "Elinit: starting"))
      ('startup-complete
       (message "Elinit: complete")))))

;;; DAG Scheduler State

(defvar elinit--dag-in-degree nil
  "Hash table of ID -> remaining in-degree count.")

(defvar elinit--dag-dependents nil
  "Hash table of ID -> list of IDs that depend on it.")

(defvar elinit--dag-entries nil
  "Hash table of ID -> parsed entry.")

(defvar elinit--dag-blocking nil
  "Hash table of blocking oneshot IDs still pending.")

(defvar elinit--dag-started nil
  "Hash table of IDs that have been started (or skipped).")

(defvar elinit--dag-ready nil
  "Hash table of IDs that are ready (spawned/completed/skipped).")

(defvar elinit--dag-timeout-timers nil
  "Hash table of ID -> timeout timer for oneshots.")

(defvar elinit--dag-delay-timers nil
  "Hash table of ID -> delay timer for delayed entries.")

(defvar elinit--dag-id-to-index nil
  "Hash table of ID -> original list index for stable ordering.")

(defvar elinit--dag-complete-callback nil
  "Callback to invoke when all entries are complete.")

(defvar elinit--dag-timeout-timer nil
  "Timer for startup timeout.")

(defvar elinit--dag-pending-starts nil
  "Queue of entry IDs waiting to start (for max-concurrent-starts).")

(defvar elinit--dag-active-starts 0
  "Count of currently starting processes (for max-concurrent-starts).")

(defvar elinit--target-convergence nil
  "Hash table of target ID to convergence state symbol.
Valid states: converging, reached, degraded.")

(defvar elinit--target-convergence-reasons nil
  "Hash table of target ID to list of reason strings for degraded.")

(defvar elinit--target-converging nil
  "Hash table of target ID to t for targets still converging.
Used by `elinit--dag-check-complete' to block premature completion.")

(defvar elinit--target-member-reverse nil
  "Hash table of member ID to list of target IDs containing it.
Built from `elinit--target-members' inverse for convergence callbacks.")

(defvar elinit--target-members nil
  "Hash table of target ID to plist (:requires (ids) :wants (ids)).
Populated from plan during startup for convergence checks.")

;;; Plan Artifact (Phase 2 Data-Driven Architecture)

(cl-defstruct (elinit-plan (:constructor elinit-plan--create))
  "Immutable execution plan for a elinit session.
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
  conflicts-deps   ; hash: id -> validated :conflicts list
  conflict-reverse  ; hash: id -> list of ids that conflict with this id
  activation-root  ; resolved root target ID string, or nil
  activation-closure ; hash: id -> t for entries in the closure, or nil
  meta)            ; plist with :version, :timestamp (non-deterministic)

(defconst elinit-plan-version 2
  "Schema version for elinit-plan struct.")

;;; Service Definition Schema (v1)

(defconst elinit-service-schema-version 1
  "Schema version for service definitions.
Version history:
  1 - Initial versioned schema with explicit dependency model split.")

(cl-defstruct (elinit-service (:constructor elinit-service--create)
                                  (:copier nil))
  "Canonical service record for elinit entries (schema v1).
This struct represents a fully parsed and validated service definition.
All internal functions should use this struct, not raw config entries
or legacy tuple indexing.

Field documentation:
  id             - Unique identifier string (required)
  command        - Shell command string to execute (required)
  type           - Entry type: `simple' (daemon), `oneshot' (run-once),
                   or `target' (grouping unit).  Default: `simple'
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
  after          - Ordering dependencies: list of service IDs
                   These control start ORDER but do not pull in services.
                   Default: nil
  requires       - Requirement dependencies: list of service IDs
                   These PULL IN services and also imply ordering.
                   Default: nil
  oneshot-blocking - For oneshots: block convergence until exit (boolean)
                   Default: value of `elinit-oneshot-default-blocking'
  oneshot-timeout - For oneshots: timeout in seconds, or nil for infinite
                   Default: value of `elinit-oneshot-timeout'
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
                   Default: nil (uses global `elinit-restart-delay')
  description    - Human-readable one-line description or nil
                   Default: nil
  documentation  - List of documentation URIs/paths, or nil
                   Default: nil
  before         - Before-ordering deps: list of IDs
                   Inverted into :after edges during scheduling.
                   Default: nil
  wants          - Soft dependencies: list of IDs
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
                   Default: nil
  log-format     - Log format symbol: text or binary
                   Default: nil (effective text)
  limit-nofile   - NOFILE resource limit (integer, \"SOFT:HARD\", or infinity)
                   Default: nil
  limit-nproc    - NPROC resource limit
                   Default: nil
  limit-core     - CORE resource limit
                   Default: nil
  limit-fsize    - FSIZE resource limit
                   Default: nil
  limit-as       - AS (address space) resource limit
                   Default: nil"
  (id nil :type string :documentation "Unique identifier (required)")
  (command nil :type string :documentation "Shell command to execute (required)")
  (type 'simple :type symbol :documentation "Process type: simple or oneshot")
  (delay 0 :type number :documentation "Delay before starting (seconds)")
  (enabled t :type boolean :documentation "Whether to start this service")
  (restart 'always :type symbol :documentation "Restart policy: always, no, on-success, on-failure")
  (logging t :type boolean :documentation "Log stdout/stderr to file")
  (stdout-log-file nil :type (or null string)
                   :documentation "Optional stdout log file path")
  (stderr-log-file nil :type (or null string)
                   :documentation "Optional stderr log file path")
  (after nil :type list :documentation "Ordering dependencies")
  (requires nil :type list :documentation "Requirement dependencies")
  (oneshot-blocking nil :type boolean :documentation "Block convergence for oneshot exit")
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
  (wants nil :type list :documentation "Soft dependencies")
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
  (sandbox-raw-args nil :type list :documentation "Raw bwrap argument list")
  (log-format nil :type (or null symbol) :documentation "Log format: text or binary")
  (limit-nofile nil :documentation "NOFILE resource limit")
  (limit-nproc nil :documentation "NPROC resource limit")
  (limit-core nil :documentation "CORE resource limit")
  (limit-fsize nil :documentation "FSIZE resource limit")
  (limit-as nil :documentation "AS (address space) resource limit")
  (conflicts nil :type list :documentation "Conflicting service IDs"))

(defconst elinit-service-required-fields '(id command)
  "List of required fields in a service record.")

(defconst elinit-service-optional-fields
  '((type . simple)
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
    (sandbox-raw-args . nil)
    (log-format . nil)
    (limit-nofile . nil)
    (limit-nproc . nil)
    (limit-core . nil)
    (limit-fsize . nil)
    (limit-as . nil)
    (conflicts . nil))
  "Alist of optional fields with their default values.
A value of :defer means the default is resolved at runtime.")

;;; Parsed Entry Accessors (Schema v1)
;;
;; These functions abstract the internal tuple representation.
;; Use these instead of direct (nth N entry) indexing for maintainability.
;; The tuple format is:
;;   (id cmd delay enabled-p restart-policy logging-p
;;    stdout-log-file stderr-log-file
;;    type after
;;    oneshot-blocking oneshot-timeout tags requires
;;    working-directory environment environment-file
;;    exec-stop exec-reload restart-sec
;;    description documentation before wants
;;    kill-signal kill-mode remain-after-exit success-exit-status
;;    user group wanted-by required-by
;;    sandbox-profile sandbox-network sandbox-ro-bind sandbox-rw-bind
;;    sandbox-tmpfs sandbox-raw-args
;;    log-format
;;    limit-nofile limit-nproc limit-core limit-fsize limit-as
;;    conflicts)

(defun elinit-entry-id (entry)
  "Return the ID of parsed ENTRY."
  (nth 0 entry))

(defun elinit-entry-command (entry)
  "Return the command of parsed ENTRY."
  (nth 1 entry))

(defun elinit-entry-delay (entry)
  "Return the delay in seconds of parsed ENTRY."
  (nth 2 entry))

(defun elinit-entry-enabled-p (entry)
  "Return non-nil if parsed ENTRY is enabled."
  (nth 3 entry))

(defun elinit-entry-restart-p (entry)
  "Return non-nil if parsed ENTRY has any restart policy other than `no'."
  (not (eq (nth 4 entry) 'no)))

(defun elinit-entry-restart-policy (entry)
  "Return the restart policy symbol of parsed ENTRY.
Value is one of `always', `no', `on-success', or `on-failure'."
  (nth 4 entry))

(defun elinit-entry-logging-p (entry)
  "Return non-nil if parsed ENTRY should log to file."
  (nth 5 entry))

(defun elinit-entry-stdout-log-file (entry)
  "Return the stdout log file path for parsed ENTRY, or nil."
  (and (>= (length entry) 30)
       (nth 6 entry)))

(defun elinit-entry-stderr-log-file (entry)
  "Return the stderr log file path for parsed ENTRY, or nil."
  (and (>= (length entry) 30)
       (nth 7 entry)))

(defun elinit-entry-type (entry)
  "Return the type (`simple', `oneshot', or `target') of parsed ENTRY."
  (nth (if (>= (length entry) 30) 8 6) entry))

(defun elinit-entry-after (entry)
  "Return the ordering dependencies (after) of parsed ENTRY."
  (nth (if (>= (length entry) 30) 9 7) entry))

(defun elinit-entry-oneshot-blocking (entry)
  "Return non-nil if oneshot ENTRY blocks convergence."
  (nth (if (>= (length entry) 30) 10 8) entry))

(defun elinit-entry-oneshot-timeout (entry)
  "Return the timeout in seconds for oneshot ENTRY, or nil."
  (nth (if (>= (length entry) 30) 11 9) entry))

(defun elinit-entry-tags (entry)
  "Return the tags list of parsed ENTRY."
  (nth (if (>= (length entry) 30) 12 10) entry))

(defun elinit-entry-requires (entry)
  "Return the requirement dependencies of parsed ENTRY."
  (nth (if (>= (length entry) 30) 13 11) entry))

(defun elinit-entry-working-directory (entry)
  "Return the working directory of parsed ENTRY, or nil."
  (nth (if (>= (length entry) 30) 14 12) entry))

(defun elinit-entry-environment (entry)
  "Return the environment alist of parsed ENTRY, or nil."
  (nth (if (>= (length entry) 30) 15 13) entry))

(defun elinit-entry-environment-file (entry)
  "Return the environment file list of parsed ENTRY, or nil."
  (nth (if (>= (length entry) 30) 16 14) entry))

(defun elinit-entry-exec-stop (entry)
  "Return the stop command list of parsed ENTRY, or nil."
  (nth (if (>= (length entry) 30) 17 15) entry))

(defun elinit-entry-exec-reload (entry)
  "Return the reload command list of parsed ENTRY, or nil."
  (nth (if (>= (length entry) 30) 18 16) entry))

(defun elinit-entry-restart-sec (entry)
  "Return the per-unit restart delay of parsed ENTRY, or nil."
  (nth (if (>= (length entry) 30) 19 17) entry))

(defun elinit-entry-description (entry)
  "Return the description string of parsed ENTRY, or nil."
  (nth (if (>= (length entry) 30) 20 18) entry))

(defun elinit-entry-documentation (entry)
  "Return the documentation list of parsed ENTRY, or nil."
  (nth (if (>= (length entry) 30) 21 19) entry))

(defun elinit-entry-before (entry)
  "Return the before-ordering dependencies of parsed ENTRY."
  (nth (if (>= (length entry) 30) 22 20) entry))

(defun elinit-entry-wants (entry)
  "Return the soft dependencies of parsed ENTRY."
  (nth (if (>= (length entry) 30) 23 21) entry))

(defun elinit-entry-kill-signal (entry)
  "Return the kill signal symbol of parsed ENTRY, or nil."
  (nth (if (>= (length entry) 30) 24 22) entry))

(defun elinit-entry-kill-mode (entry)
  "Return the kill mode symbol of parsed ENTRY, or nil."
  (nth (if (>= (length entry) 30) 25 23) entry))

(defun elinit-entry-remain-after-exit (entry)
  "Return non-nil if oneshot ENTRY latches active on success."
  (nth (if (>= (length entry) 30) 26 24) entry))

(defun elinit-entry-success-exit-status (entry)
  "Return the success-exit-status plist of parsed ENTRY, or nil."
  (nth (if (>= (length entry) 30) 27 25) entry))

(defun elinit-entry-user (entry)
  "Return the run-as user of parsed ENTRY, or nil."
  (nth (if (>= (length entry) 30) 28 26) entry))

(defun elinit-entry-group (entry)
  "Return the run-as group of parsed ENTRY, or nil."
  (nth (if (>= (length entry) 30) 29 27) entry))

(defun elinit-entry-wanted-by (entry)
  "Return the wanted-by target list of parsed ENTRY."
  (and (>= (length entry) 32) (nth 30 entry)))

(defun elinit-entry-required-by (entry)
  "Return the required-by target list of parsed ENTRY."
  (and (>= (length entry) 32) (nth 31 entry)))

(defun elinit-entry-sandbox-profile (entry)
  "Return the sandbox profile symbol of parsed ENTRY, or nil."
  (and (>= (length entry) 38) (nth 32 entry)))

(defun elinit-entry-sandbox-network (entry)
  "Return the sandbox network mode symbol of parsed ENTRY, or nil."
  (and (>= (length entry) 38) (nth 33 entry)))

(defun elinit-entry-sandbox-ro-bind (entry)
  "Return the sandbox read-only bind list of parsed ENTRY, or nil."
  (and (>= (length entry) 38) (nth 34 entry)))

(defun elinit-entry-sandbox-rw-bind (entry)
  "Return the sandbox read-write bind list of parsed ENTRY, or nil."
  (and (>= (length entry) 38) (nth 35 entry)))

(defun elinit-entry-sandbox-tmpfs (entry)
  "Return the sandbox tmpfs mount list of parsed ENTRY, or nil."
  (and (>= (length entry) 38) (nth 36 entry)))

(defun elinit-entry-sandbox-raw-args (entry)
  "Return the sandbox raw argument list of parsed ENTRY, or nil."
  (and (>= (length entry) 38) (nth 37 entry)))

(defun elinit-entry-log-format (entry)
  "Return the log format symbol of parsed ENTRY, or nil."
  (and (>= (length entry) 39) (nth 38 entry)))

(defun elinit-entry-limit-nofile (entry)
  "Return the NOFILE resource limit of parsed ENTRY, or nil."
  (and (>= (length entry) 44) (nth 39 entry)))

(defun elinit-entry-limit-nproc (entry)
  "Return the NPROC resource limit of parsed ENTRY, or nil."
  (and (>= (length entry) 44) (nth 40 entry)))

(defun elinit-entry-limit-core (entry)
  "Return the CORE resource limit of parsed ENTRY, or nil."
  (and (>= (length entry) 44) (nth 41 entry)))

(defun elinit-entry-limit-fsize (entry)
  "Return the FSIZE resource limit of parsed ENTRY, or nil."
  (and (>= (length entry) 44) (nth 42 entry)))

(defun elinit-entry-limit-as (entry)
  "Return the AS (address space) resource limit of parsed ENTRY, or nil."
  (and (>= (length entry) 44) (nth 43 entry)))

(defun elinit-entry-conflicts (entry)
  "Return the conflicts list of parsed ENTRY."
  (and (>= (length entry) 45) (nth 44 entry)))

(defun elinit--limits-requesting-p (entry)
  "Return non-nil if parsed ENTRY has any resource limit set."
  (or (elinit-entry-limit-nofile entry)
      (elinit-entry-limit-nproc entry)
      (elinit-entry-limit-core entry)
      (elinit-entry-limit-fsize entry)
      (elinit-entry-limit-as entry)))

(defun elinit--sandbox-requesting-p (entry)
  "Return non-nil if parsed ENTRY requests sandbox.
A unit is sandbox-requesting when `:sandbox-profile' is set to
anything other than `none', or when any other sandbox field has a
truthy (non-nil) value.

NOTE: This function operates on parsed tuples where nil and absent
are indistinguishable.  An entry with a nil-valued sandbox key
\(e.g., `:sandbox-ro-bind nil') will not be detected here.
Validation (`elinit--validate-entry') is the authoritative
gating point and uses `plist-member' for key-presence semantics.
All runtime call sites are behind validation, so this is safe."
  (or (and (elinit-entry-sandbox-profile entry)
           (not (eq (elinit-entry-sandbox-profile entry) 'none)))
      (elinit-entry-sandbox-network entry)
      (elinit-entry-sandbox-ro-bind entry)
      (elinit-entry-sandbox-rw-bind entry)
      (elinit-entry-sandbox-tmpfs entry)
      (elinit-entry-sandbox-raw-args entry)))

;; Forward declarations for timer state variables (defined in elinit-timer.el)
;; These are only accessed when timer module is loaded.
(defvar elinit--timer-state)
(defvar elinit--timer-scheduler)
(defvar elinit--timer-list)
(defvar elinit--scheduler-startup-time)
(defvar elinit--timer-state-loaded)

(defun elinit--get-entry-for-id (id)
  "Get the parsed entry for ID.
Return a list of entry properties or nil if not found.
Reads from the cached program list via `elinit--effective-programs'.
Skips invalid/malformed entries to avoid parse errors."
  (let ((idx 0))
    (cl-loop for entry in (elinit--effective-programs)
             for entry-id = (elinit--extract-id entry idx)
             do (cl-incf idx)
             ;; Skip invalid entries - don't try to parse them
             unless (gethash entry-id elinit--invalid)
             ;; Only parse valid entries
             when (string= entry-id id)
             return (elinit--parse-entry entry))))

(defun elinit--compute-entry-status (id type &optional snapshot)
  "Compute status string and PID string for entry ID of TYPE.
If SNAPSHOT is provided, read from it; otherwise read from globals.
Return a cons cell (STATUS . PID)."
  (let* ((mask-hash (if snapshot
                        (or (elinit-snapshot-mask-override snapshot)
                            (make-hash-table :test 'equal))
                      elinit--mask-override))
         (masked (eq (gethash id mask-hash) 'masked))
         (alive (if snapshot
                    (gethash id (elinit-snapshot-process-alive snapshot))
                  (let ((proc (gethash id elinit--processes)))
                    (and proc (process-live-p proc)))))
         (pid-num (if snapshot
                      (gethash id (elinit-snapshot-process-pids snapshot))
                    (let ((proc (gethash id elinit--processes)))
                      (and proc (process-live-p proc) (process-id proc)))))
         (failed (gethash id (if snapshot
                                 (elinit-snapshot-failed snapshot)
                               elinit--failed)))
         (oneshot-p (eq type 'oneshot))
         (oneshot-exit (gethash id (if snapshot
                                       (elinit-snapshot-oneshot-exit snapshot)
                                     elinit--oneshot-completed)))
         (oneshot-done (not (null oneshot-exit)))
         (oneshot-failed (and oneshot-done (/= oneshot-exit 0)))
         (remain-active-p (gethash id (if snapshot
                                           (or (elinit-snapshot-remain-active snapshot)
                                               (make-hash-table :test 'equal))
                                         elinit--remain-active)))
         (manually-stopped-p
          (gethash id (if snapshot
                          (or (elinit-snapshot-manually-stopped snapshot)
                              (make-hash-table :test 'equal))
                        elinit--manually-stopped)))
         (entry-state (gethash id (if snapshot
                                      (elinit-snapshot-entry-state snapshot)
                                    elinit--entry-state)))
         (pid (cond (alive (number-to-string pid-num))
                    ((and oneshot-p oneshot-done) (format "exit:%d" oneshot-exit))
                    (t "-")))
         (target-p (eq type 'target))
         (status (cond (masked "masked")
                       (target-p
                        (let* ((effective-id
                                (elinit--resolve-target-runtime-id id))
                               (conv
                                (when (hash-table-p
                                       elinit--target-convergence)
                                  (gethash effective-id
                                           elinit--target-convergence)))
                               (closure
                                (and (elinit-plan-p
                                      elinit--current-plan)
                                     (elinit-plan-activation-closure
                                      elinit--current-plan))))
                          (cond
                           ((eq conv 'reached) "reached")
                           ((eq conv 'degraded) "degraded")
                           ((eq conv 'converging) "converging")
                           ;; Not in activation closure: unreachable
                           ((and (hash-table-p closure)
                                 (not (gethash effective-id closure)))
                            "unreachable")
                           (t "pending"))))
                       (alive "running")
                       (failed "dead")
                       ((and oneshot-p oneshot-failed) "failed")
                       ((and oneshot-p remain-active-p) "active")
                       ((and oneshot-p oneshot-done) "done")
                       ((and oneshot-p manually-stopped-p) "stopped")
                       ;; Disabled entries show "disabled" (not "pending")
                       ((eq entry-state 'disabled) "disabled")
                       ;; Not in activation closure: unreachable
                       ((let ((closure (and (elinit-plan-p
                                            elinit--current-plan)
                                           (elinit-plan-activation-closure
                                            elinit--current-plan))))
                          (and (hash-table-p closure)
                               (not (gethash id closure))))
                        "unreachable")
                       (oneshot-p "pending")
                       (t "stopped"))))
    (cons status pid)))

(defun elinit--compute-entry-reason (id type &optional snapshot)
  "Compute reason string for entry ID of TYPE.
Returns a reason string, or nil if no specific reason applies.
If SNAPSHOT is provided, read from it; otherwise read from globals."
  (let* ((mask-hash (if snapshot
                        (or (elinit-snapshot-mask-override snapshot)
                            (make-hash-table :test 'equal))
                      elinit--mask-override))
         (masked (eq (gethash id mask-hash) 'masked))
         (alive (if snapshot
                    (gethash id (elinit-snapshot-process-alive snapshot))
                  (let ((proc (gethash id elinit--processes)))
                    (and proc (process-live-p proc)))))
         (failed (gethash id (if snapshot
                                 (elinit-snapshot-failed snapshot)
                               elinit--failed)))
         (oneshot-p (eq type 'oneshot))
         (oneshot-exit (gethash id (if snapshot
                                       (elinit-snapshot-oneshot-exit snapshot)
                                     elinit--oneshot-completed)))
         (oneshot-done (not (null oneshot-exit)))
         (entry-state (gethash id (if snapshot
                                      (elinit-snapshot-entry-state snapshot)
                                    elinit--entry-state))))
    (cond
     (masked "masked")
     ((eq type 'target)
      (let* ((effective-id (elinit--resolve-target-runtime-id id))
             (reasons (when (hash-table-p
                             elinit--target-convergence-reasons)
                        (gethash effective-id
                                 elinit--target-convergence-reasons))))
        (when reasons
          (mapconcat #'identity reasons "; "))))
     (alive nil)
     ((and oneshot-p oneshot-done) nil)
     ((eq entry-state 'disabled) "disabled")
     ((eq entry-state 'delayed) "delayed")
     ((eq entry-state 'waiting-on-deps) "waiting-on-deps")
     ((eq entry-state 'pending) "pending")
     ((eq entry-state 'failed-to-spawn)
      (or (gethash id elinit--spawn-failure-reason)
          "failed-to-spawn"))
     ((eq entry-state 'startup-timeout) "startup-timeout")
     (failed "crash-loop")
     (t nil))))

;;; Migration Layer (Schema v1)
;;
;; Functions to normalize program entries to canonical
;; schema v1 plist format.

(defun elinit--migrate-entry-to-plist (entry)
  "Migrate raw ENTRY to a canonical schema v1 plist format.
Returns a plist in canonical program entry format.
This normalizes short-form entries to their explicit form."
  (let* ((parsed (elinit--parse-entry entry))
         (id (elinit-entry-id parsed))
         (cmd (elinit-entry-command parsed))
         (type (elinit-entry-type parsed))
         (delay (elinit-entry-delay parsed))
         (enabled (elinit-entry-enabled-p parsed))
         (restart (elinit-entry-restart-policy parsed))
         (logging (elinit-entry-logging-p parsed))
         (stdout-log-file (elinit-entry-stdout-log-file parsed))
         (stderr-log-file (elinit-entry-stderr-log-file parsed))
         (after (elinit-entry-after parsed))
         (requires (elinit-entry-requires parsed))
         (oneshot-blocking (elinit-entry-oneshot-blocking parsed))
         (oneshot-timeout (elinit-entry-oneshot-timeout parsed))
         (tags (elinit-entry-tags parsed))
         (plist nil))
    ;; Build plist from parsed values, only including non-default values
    (when tags
      (setq plist (plist-put plist :tags tags)))
    (when requires
      (setq plist (plist-put plist :requires requires)))
    (when after
      (setq plist (plist-put plist :after after)))
    (when (eq type 'oneshot)
      (when (not (eq oneshot-blocking elinit-oneshot-default-blocking))
        (setq plist (plist-put plist :oneshot-blocking oneshot-blocking)))
      (when (not (eq oneshot-timeout elinit-oneshot-timeout))
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

(defun elinit--migrate-all-entries ()
  "Migrate all loaded entries to schema v1 format.
Returns a list of migrated entries with a summary of changes.
Invalid and duplicate entries are skipped with a warning."
  (let ((seen (make-hash-table :test 'equal))
        (migrated nil)
        (skipped nil)
        (idx 0))
    (dolist (entry (elinit--effective-programs))
      (let* ((id (elinit--extract-id entry idx))
             (reason (elinit--validate-entry entry)))
        (cond
         (reason
          (push (cons id reason) skipped))
         ((gethash id seen)
          (push (cons id "duplicate ID") skipped))
         (t
          (puthash id t seen)
          (push (elinit--migrate-entry-to-plist entry) migrated)))
        (cl-incf idx)))
    (list :migrated (nreverse migrated)
          :skipped (nreverse skipped))))

(defun elinit-migrate-config ()
  "Display loaded entries in unit-file plist format.
Shows the canonical form of all valid entries as unit-file plists,
one per service.  Save each to a `.el' file in the highest-precedence
authority root to complete the migration."
  (interactive)
  (let* ((result (elinit--migrate-all-entries))
         (migrated (plist-get result :migrated))
         (skipped (plist-get result :skipped)))
    (with-output-to-temp-buffer "*elinit-migrate*"
      (princ ";;; Elinit Unit Files (Schema v1)\n")
      (princ ";; Generated by elinit-migrate-config\n")
      (princ (format ";; Version: %d\n" elinit-service-schema-version))
      (princ (format ";; Target authority root: %s\n\n"
                     (abbreviate-file-name
                      (or (car (last (elinit--active-authority-roots)))
                          elinit-unit-directory))))
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

(defun elinit-migrate-entry-to-service (entry)
  "Convert raw config ENTRY directly to a `elinit-service' struct.
This is the high-level migration function for programmatic use."
  (let* ((reason (elinit--validate-entry entry)))
    (if reason
        (error "Invalid entry: %s" reason)
      (elinit-entry-to-service (elinit--parse-entry entry)))))

(cl-defstruct (elinit-snapshot (:constructor elinit-snapshot--create))
  "Runtime state snapshot for read-only operations.
Captures current state of all managed processes for dashboard,
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
  conflict-suppressed ; hash: id -> requester ID if conflict-suppressed
  remain-active    ; hash: id -> t if oneshot latched active (remain-after-exit)
  last-exit-info   ; hash: id -> plist (:status :code :timestamp)
  timestamp)       ; float-time when snapshot was taken

(defun elinit--build-snapshot ()
  "Build a snapshot of current runtime state.
Returns a `elinit-snapshot' struct capturing process states,
completion status, and overrides.  Safe to call at any time."
  (let ((process-alive (make-hash-table :test 'equal))
        (process-pids (make-hash-table :test 'equal)))
    ;; Capture process liveness and PIDs
    (maphash (lambda (id proc)
               (when (process-live-p proc)
                 (puthash id t process-alive)
                 (puthash id (process-id proc) process-pids)))
             elinit--processes)
    (elinit-snapshot--create
     :process-alive process-alive
     :process-pids process-pids
     :failed (copy-hash-table elinit--failed)
     :oneshot-exit (copy-hash-table elinit--oneshot-completed)
     :entry-state (copy-hash-table elinit--entry-state)
     :invalid (copy-hash-table elinit--invalid)
     :enabled-override (copy-hash-table elinit--enabled-override)
     :restart-override (copy-hash-table elinit--restart-override)
     :logging-override (copy-hash-table elinit--logging)
     :mask-override (copy-hash-table elinit--mask-override)
     :manually-started (copy-hash-table elinit--manually-started)
     :manually-stopped (copy-hash-table elinit--manually-stopped)
     :conflict-suppressed (copy-hash-table elinit--conflict-suppressed)
     :remain-active (copy-hash-table elinit--remain-active)
     :last-exit-info (copy-hash-table elinit--last-exit-info)
     :timestamp (float-time))))

(defun elinit--validate-references (valid-entries invalid)
  "Validate cross-entry references in VALID-ENTRIES.
INVALID is a hash table to which newly invalid entries are added.
Return the filtered list of valid entries with soft refs dropped."
  (let ((id-type (make-hash-table :test 'equal)))
    ;; Build ID-to-type lookup
    (dolist (entry valid-entries)
      (puthash (elinit-entry-id entry)
               (elinit-entry-type entry)
               id-type))
    ;; Check :wanted-by/:required-by target references
    (dolist (entry valid-entries)
      (let ((id (elinit-entry-id entry)))
        (dolist (spec (list (cons (elinit-entry-wanted-by entry) ":wanted-by")
                            (cons (elinit-entry-required-by entry) ":required-by")))
          (dolist (target-id (car spec))
            (let ((target-type (gethash target-id id-type)))
              (cond
               ((not target-type)
                (puthash id (format "%s references non-existent target '%s'"
                                    (cdr spec) target-id)
                         invalid)
                (elinit--log 'error "%s '%s' for %s does not exist"
                                 (cdr spec) target-id id))
               ((not (eq target-type 'target))
                (puthash id (format "%s references '%s' which is not a target"
                                    (cdr spec) target-id)
                         invalid)
                (elinit--log 'error "%s '%s' for %s is not :type target"
                                 (cdr spec) target-id id))))))))
    ;; Check target entries' :requires refs exist
    (dolist (entry valid-entries)
      (when (eq (elinit-entry-type entry) 'target)
        (let ((id (elinit-entry-id entry)))
          (dolist (req (elinit-entry-requires entry))
            (unless (gethash req id-type)
              (puthash id (format ":requires '%s' does not exist" req)
                       invalid)
              (elinit--log 'error
                               "target %s :requires '%s' does not exist"
                               id req)))
          ;; Soft deps: drop missing :wants/:after/:before with warning
          (dolist (dep-spec (list (cons (elinit-entry-wants entry) ":wants")
                                  (cons (elinit-entry-after entry) ":after")
                                  (cons (elinit-entry-before entry) ":before")))
            (dolist (dep (car dep-spec))
              (unless (gethash dep id-type)
                (elinit--log 'warning
                                 "target %s %s '%s' does not exist, dropping"
                                 id (cdr dep-spec) dep)))))))
    ;; Cycle detection on :requires graph among targets
    (let ((target-requires (make-hash-table :test 'equal))
          (cycle-ids nil))
      (dolist (entry valid-entries)
        (when (and (eq (elinit-entry-type entry) 'target)
                   (not (gethash (elinit-entry-id entry) invalid)))
          (puthash (elinit-entry-id entry)
                   (elinit-entry-requires entry)
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
        (elinit--log 'error "target :requires cycle: %s" cid)))
    ;; Filter out newly-invalidated entries
    (cl-remove-if (lambda (entry)
                    (gethash (elinit-entry-id entry) invalid))
                  valid-entries)))

(defun elinit--materialize-target-members (entries)
  "Build inverse membership map from service target declarations.
ENTRIES is a list of parsed valid entries.
Returns hash of TARGET-ID -> (:requires (IDS...) :wants (IDS...)).
Services declaring :required-by contribute to target requires-members.
Services declaring :wanted-by contribute to target wants-members."
  (let ((members (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (let ((id (elinit-entry-id entry)))
        ;; :required-by -> target's :requires member list
        (dolist (target-id (elinit-entry-required-by entry))
          (let ((cur (gethash target-id members)))
            (puthash target-id
                     (plist-put cur :requires
                                (cons id (plist-get cur :requires)))
                     members)))
        ;; :wanted-by -> target's :wants member list
        (dolist (target-id (elinit-entry-wanted-by entry))
          (let ((cur (gethash target-id members)))
            (puthash target-id
                     (plist-put cur :wants
                                (cons id (plist-get cur :wants)))
                     members)))))
    ;; Merge target's own :requires into membership (top-down direction)
    (dolist (entry entries)
      (when (eq (elinit-entry-type entry) 'target)
        (let ((id (elinit-entry-id entry)))
          (dolist (req (elinit-entry-requires entry))
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

(defun elinit--expand-transaction (root entries-by-id
                                            target-members order-index)
  "Compute activation closure from ROOT target.
ENTRIES-BY-ID is hash of id -> parsed entry.
TARGET-MEMBERS is hash from `elinit--materialize-target-members'.
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
          (let ((type (elinit-entry-type entry)))
            (if (eq type 'target)
                ;; Target: pull in own :requires/:wants + membership edges
                (progn
                  (dolist (dep (elinit-entry-requires entry))
                    (push dep new-ids))
                  (dolist (dep (elinit-entry-wants entry))
                    (push dep new-ids))
                  ;; Membership edges (from :required-by/:wanted-by inverse)
                  (let ((mem (gethash current-id target-members)))
                    (when mem
                      (dolist (dep (plist-get mem :requires))
                        (push dep new-ids))
                      (dolist (dep (plist-get mem :wants))
                        (push dep new-ids)))))
              ;; Service: pull in own :requires/:wants
              (dolist (dep (elinit-entry-requires entry))
                (push dep new-ids))
              (dolist (dep (elinit-entry-wants entry))
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

(defun elinit--build-plan (programs)
  "Build an immutable execution plan from PROGRAMS.
This function does not modify global runtime state, but does emit
warnings for invalid entries, duplicate IDs, and invalid :after refs.
Returns a `elinit-plan' struct with all computed scheduling data.

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
      (let* ((raw-id (elinit--extract-id entry idx))
             (reason (elinit--validate-entry entry))
             (parsed (unless reason (elinit--parse-entry entry))))
        ;; Only record first-occurrence index (don't let duplicates overwrite)
        (unless (gethash raw-id order-index)
          (puthash raw-id idx order-index))
        (cond
         ;; Invalid entry - only record if not already seen (first valid wins)
         (reason
          (unless (gethash raw-id seen)
            (puthash raw-id reason invalid))
          (elinit--log 'warning "INVALID %s - %s" raw-id reason))
         ;; Duplicate ID
         ((gethash raw-id seen)
          (elinit--log 'warning "duplicate ID '%s', skipping" raw-id))
         ;; Valid entry - first valid wins, clear any stale invalid state
         (t
          (remhash raw-id invalid)  ; clear if earlier invalid had same ID
          (puthash raw-id t seen)
          (push parsed valid-entries)))
        (cl-incf idx)))
    (setq valid-entries (nreverse valid-entries))
    ;; Phase 1b: Validate cross-entry references
    (setq valid-entries
          (elinit--validate-references valid-entries invalid))
    ;; Phase 2: Global dep validation and topo sort (single pass)
    (let ((all-ids (mapcar #'car valid-entries))
          (combined-deps (make-hash-table :test 'equal)))
      ;; Pre-pass: build :before inversion map across ALL entries
      (let ((before-map (make-hash-table :test 'equal)))
        (dolist (entry valid-entries)
          (let ((src-id (elinit-entry-id entry))
                (before (elinit-entry-before entry)))
            (dolist (target before)
              (cond
               ((not (member target all-ids))
                (elinit--log 'warning
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
                  (let* ((id (elinit-entry-id entry))
                         (after (elinit-entry-after entry))
                         (requires (elinit-entry-requires entry))
                         (wants (elinit-entry-wants entry))
                         ;; Validate :after - warn + drop missing
                         (valid-after
                          (cl-remove-if-not
                           (lambda (dep)
                             (cond
                              ((not (member dep all-ids))
                               (elinit--log 'warning
                                 ":after '%s' for %s does not exist, ignoring"
                                 dep id)
                               nil)
                              (t t)))
                           after))
                         ;; Merge inverted :before edges
                         (before-edges (gethash id before-map))
                         (valid-after
                          (elinit--deduplicate-stable
                           (append valid-after before-edges)))
                         ;; Validate :requires - warn + drop missing
                         ;; For targets, a missing :requires invalidates
                         ;; the target (consistent with early validation)
                         (valid-requires
                          (cl-remove-if-not
                           (lambda (dep)
                             (cond
                              ((not (member dep all-ids))
                               (if (eq (elinit-entry-type entry) 'target)
                                   (progn
                                     (puthash id
                                              (format
                                               ":requires '%s' does not exist"
                                               dep)
                                              invalid)
                                     (elinit--log 'error
                                       "target %s :requires '%s' does not exist"
                                       id dep))
                                 (elinit--log 'warning
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
                         ;; Validate :conflicts - warn + drop missing
                         (conflicts (elinit-entry-conflicts entry))
                         (valid-conflicts
                          (cl-remove-if-not
                           (lambda (dep)
                             (cond
                              ((not (member dep all-ids))
                               (elinit--log 'warning
                                 ":conflicts '%s' for %s does not exist, dropping"
                                 dep id)
                               nil)
                              (t t)))
                           conflicts))
                         ;; Auto-order rule: for target entries, :requires
                         ;; and :wants imply :after (ordering edges)
                         (valid-after
                          (if (eq (elinit-entry-type entry) 'target)
                              (elinit--deduplicate-stable
                               (append valid-after
                                       (cl-union valid-requires valid-wants
                                                 :test #'equal)))
                            valid-after)))
                    (puthash id valid-after deps)
                    (puthash id valid-requires requires-deps)
                    ;; Combined deps for topo sort
                    ;; (union of after + requires + wants)
                    (puthash id (elinit--deduplicate-stable
                                 (append
                                  (cl-union valid-after valid-requires
                                            :test #'equal)
                                  valid-wants))
                             combined-deps)
                    ;; Return entry with validated deps
                    ;; :after is at index 9, :requires at index 13,
                    ;; :conflicts at index 44
                    (let ((new-entry entry))
                      (unless (equal after valid-after)
                        (setq new-entry
                              (append (cl-subseq new-entry 0 9)
                                      (list valid-after)
                                      (cl-subseq new-entry 10))))
                      (unless (equal requires valid-requires)
                        (setq new-entry
                              (append (cl-subseq new-entry 0 13)
                                      (list valid-requires)
                                      (cl-subseq new-entry 14))))
                      (unless (equal conflicts valid-conflicts)
                        (setq new-entry
                              (append (cl-subseq new-entry 0 44)
                                      (list valid-conflicts))))
                      new-entry)))
                valid-entries)))
          ;; Filter out entries marked invalid during validation
          (setq validated-entries
                (cl-remove-if (lambda (entry)
                                (gethash (elinit-entry-id entry) invalid))
                              validated-entries))
          ;; Build dependents graph (combined :after + :requires + :wants)
          (dolist (entry validated-entries)
            (let* ((id (elinit-entry-id entry))
                   (all-deps (gethash id combined-deps)))
              (puthash id nil dependents)
              (dolist (dep all-deps)
                (puthash dep (cons id (gethash dep dependents))
                         dependents))))
          ;; Global topo sort
          (let ((sorted-entries
                 (elinit--build-plan-topo-sort
                  validated-entries combined-deps order-index cycle-fallback-ids)))
            ;; If cycle was detected, also clear deps and requires-deps
            (dolist (entry sorted-entries)
              (let ((id (elinit-entry-id entry)))
                (when (gethash id cycle-fallback-ids)
                  (puthash id nil deps)
                  (puthash id nil requires-deps))))
            ;; Filter valid-entries to exclude entries invalidated in phase 2
            (let ((final-entries (cl-remove-if
                                  (lambda (entry)
                                    (gethash (elinit-entry-id entry) invalid))
                                  valid-entries))
                  ;; Build conflicts maps from validated entries
                  (conflicts-deps (make-hash-table :test 'equal))
                  (conflict-reverse (make-hash-table :test 'equal)))
              (dolist (entry sorted-entries)
                (let* ((id (elinit-entry-id entry))
                       (clist (elinit-entry-conflicts entry)))
                  (when clist
                    (puthash id clist conflicts-deps)
                    (dolist (target clist)
                      (puthash target
                               (cons id (gethash target conflict-reverse))
                               conflict-reverse)))))
              ;; Return the plan struct
              (elinit-plan--create
               :entries final-entries
               :invalid invalid
               :by-target sorted-entries
               :deps deps
               :requires-deps requires-deps
               :dependents dependents
               :cycle-fallback-ids cycle-fallback-ids
               :order-index order-index
               :conflicts-deps conflicts-deps
               :conflict-reverse conflict-reverse
               :target-members nil
               :activation-root nil
               :activation-closure nil
               :meta (list :version elinit-plan-version
                           :timestamp (float-time)
                           :fingerprint
                           (elinit--plan-fingerprint
                            sorted-entries deps requires-deps
                            order-index conflicts-deps))))))))))

(defun elinit--plan-fingerprint (entries deps requires-deps
                                          order-index conflicts-deps)
  "Compute deterministic fingerprint of plan content.
ENTRIES is the sorted entry list.  DEPS and REQUIRES-DEPS are hashes
of id to dependency lists.  ORDER-INDEX is hash of id to original index.
CONFLICTS-DEPS is hash of id to validated conflicts list.
Returns a hex string (SHA-1 of serialized plan data)."
  (let ((parts nil))
    (dolist (entry entries)
      (let* ((id (elinit-entry-id entry))
             (d (gethash id deps))
             (r (gethash id requires-deps))
             (idx (gethash id order-index))
             (c (gethash id conflicts-deps)))
        (push (format "%s:%s:%s:%s:%s" id
                      (prin1-to-string d)
                      (prin1-to-string r)
                      idx
                      (prin1-to-string c))
              parts)))
    (sha1 (mapconcat #'identity (nreverse parts) "\n"))))

(defun elinit--build-plan-topo-sort (entries deps order-index cycle-fallback-ids)
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
          (let ((id (elinit-entry-id entry)))
            (puthash id t cycle-fallback-ids)
            (puthash id nil deps)))
        ;; Return entries with :after, :requires, and :wants stripped.
        (mapcar
         (lambda (entry)
           (let ((len (length entry)))
             (cond
              ;; Current schema entry.
              ((>= len 30)
               (append (cl-subseq entry 0 9)
                       (list nil)                ; clear :after (index 9)
                       (cl-subseq entry 10 13)
                       (list nil)                ; clear :requires (index 13)
                       (cl-subseq entry 14 23)
                       (list nil)                ; clear :wants (index 23)
                       (cl-subseq entry 24)))
              ;; Legacy full entry.
              ((>= len 22)
               (append (cl-subseq entry 0 7)
                       (list nil)                ; clear :after (index 7)
                       (cl-subseq entry 8 11)
                       (list nil)                ; clear :requires (index 11)
                       (cl-subseq entry 12 21)
                       (list nil)                ; clear :wants (index 21)
                       (cl-subseq entry 22)))
              ;; Legacy 12+ entry.
              ((>= len 12)
               (append (cl-subseq entry 0 7)
                       (list nil)                ; clear :after
                       (cl-subseq entry 8 11)
                       (list nil)                ; clear :requires
                       (cl-subseq entry 12)))
              ;; Legacy 10+ entry.
              ((>= len 10)
               (append (cl-subseq entry 0 7)
                       (list nil)                ; clear :after
                       (cl-subseq entry 8)))
              (t entry))))
         entries)))))

;;; Helpers

(defun elinit--normalize-string-or-list (val)
  "Normalize VAL to a list of strings.
If VAL is a string, return a one-element list.
If VAL is a proper list of strings, return as-is.
If VAL is nil, return nil."
  (cond ((null val) nil)
        ((stringp val) (list val))
        ((and (proper-list-p val) (cl-every #'stringp val)) val)
        (t nil)))

(defun elinit--deduplicate-stable (list)
  "Return LIST with duplicates removed, preserving first occurrence order.
Uses `equal' for comparison."
  (let ((seen (make-hash-table :test 'equal))
        (result nil))
    (dolist (item list)
      (unless (gethash item seen)
        (puthash item t seen)
        (push item result)))
    (nreverse result)))

(defconst elinit--known-signals
  '(SIGHUP SIGINT SIGQUIT SIGILL SIGTRAP SIGABRT SIGBUS SIGFPE
    SIGKILL SIGUSR1 SIGSEGV SIGUSR2 SIGPIPE SIGALRM SIGTERM
    SIGSTKFLT SIGCHLD SIGCONT SIGSTOP SIGTSTP SIGTTIN SIGTTOU
    SIGURG SIGXCPU SIGXFSZ SIGVTALRM SIGPROF SIGWINCH SIGIO
    SIGPWR SIGSYS)
  "List of known POSIX signal names as symbols.")

(defun elinit--normalize-signal-name (name)
  "Normalize signal NAME to canonical SIGXXX symbol.
Accept symbol or string forms like TERM, SIGTERM, term, sigterm.
Return the canonical symbol (e.g., `SIGTERM'), or nil if unknown."
  (let* ((s (upcase (if (symbolp name) (symbol-name name)
                      (if (stringp name) name ""))))
         (full (if (string-prefix-p "SIG" s) s (concat "SIG" s)))
         (sym (intern full)))
    (when (memq sym elinit--known-signals)
      sym)))

(defun elinit--normalize-kill-mode (val)
  "Normalize VAL to a kill-mode symbol.
Accept `process' or `mixed' (symbol or string).
Return the symbol, or nil if invalid."
  (let ((sym (cond ((symbolp val) val)
                   ((stringp val) (intern val))
                   (t nil))))
    (when (memq sym '(process mixed))
      sym)))

(defun elinit--normalize-success-exit-status (val)
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
          (let ((sig (elinit--normalize-signal-name item)))
            (when sig
              (push sig signals))))))
      (list :codes (elinit--deduplicate-stable (nreverse codes))
            :signals (elinit--deduplicate-stable (nreverse signals))))))

(defun elinit--parse-entry (entry)
  "Parse ENTRY into a normalized list of entry properties.
Return a 45-element list: (id cmd delay enabled-p restart-policy
logging-p stdout-log-file stderr-log-file type after
oneshot-blocking oneshot-timeout tags requires working-directory
environment environment-file exec-stop exec-reload restart-sec
description documentation before wants kill-signal kill-mode
remain-after-exit success-exit-status user group wanted-by
required-by sandbox-profile sandbox-network sandbox-ro-bind
sandbox-rw-bind sandbox-tmpfs sandbox-raw-args log-format
limit-nofile limit-nproc limit-core limit-fsize limit-as
conflicts).

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
  9  after               - ordering dependencies
  10 oneshot-blocking    - block convergence for oneshot exit
  11 oneshot-timeout     - timeout for blocking oneshots
  12 tags                - list of filter tags
  13 requires            - requirement dependencies
  14 working-directory   - process working directory or nil
  15 environment         - environment alist or nil
  16 environment-file    - list of env-file paths or nil
  17 exec-stop           - list of stop commands or nil
  18 exec-reload         - list of reload commands or nil
  19 restart-sec         - per-unit restart delay or nil
  20 description         - human-readable description or nil
  21 documentation       - list of doc URIs/paths or nil
  22 before              - before-ordering deps or nil
  23 wants               - soft deps or nil
  24 kill-signal         - canonical signal symbol or nil
  25 kill-mode           - `process' or `mixed' or nil
  26 remain-after-exit   - oneshot active latch boolean
  27 success-exit-status - plist (:codes :signals) or nil
  28 user                - run-as user string/int or nil
  29 group               - run-as group string/int or nil
  30 wanted-by           - target IDs (soft membership) or nil
  31 required-by         - target IDs (hard requirement) or nil
  32 sandbox-profile     - sandbox profile symbol or nil
  33 sandbox-network     - sandbox network mode symbol or nil
  34 sandbox-ro-bind     - list of read-only bind paths or nil
  35 sandbox-rw-bind     - list of read-write bind paths or nil
  36 sandbox-tmpfs       - list of tmpfs mount paths or nil
  37 sandbox-raw-args    - list of raw bwrap argument strings or nil
  38 log-format          - log format symbol (text or binary) or nil
  39 limit-nofile        - NOFILE resource limit or nil
  40 limit-nproc         - NPROC resource limit or nil
  41 limit-core          - CORE resource limit or nil
  42 limit-fsize         - FSIZE resource limit or nil
  43 limit-as            - AS (address space) resource limit or nil
  44 conflicts           - list of conflicting unit IDs or nil

ENTRY can be a command string or a list (COMMAND . PLIST).
Use accessor functions instead of direct indexing for new code."
  (if (stringp entry)
      (let* ((tokens (split-string-and-unquote entry))
             (id (or (car tokens)
                     (error "Elinit: empty command string")))
             (id (file-name-nondirectory id)))
        (list id entry 0 t 'always t nil nil 'simple nil
              elinit-oneshot-default-blocking elinit-oneshot-timeout nil nil
              nil nil nil nil nil nil
              nil nil nil nil nil nil nil nil
              nil nil nil nil
              nil nil nil nil nil nil
              nil
              nil nil nil nil nil
              nil))
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
                (error "Elinit: empty command in entry")))
           (id (or (plist-get plist :id)
                   (if (eq type 'target)
                       (error "Elinit: target entry requires :id")
                     (file-name-nondirectory (car cmd-tokens)))))
           (cmd (if (eq type 'target) nil cmd))
           (delay (or (plist-get plist :delay) 0))
           (_ (unless (memq type '(simple oneshot target))
                (elinit--log 'warning "unknown :type '%s' for %s, using simple" type id)))
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
                           (elinit--normalize-restart-policy
                            (plist-get plist :restart)))
                          ((plist-member plist :no-restart)
                           (if (plist-get plist :no-restart) 'no 'always))
                          (t 'always)))
           (logging (if (plist-member plist :logging)
                        (plist-get plist :logging)
                      t))
           (stdout-log-file (plist-get plist :stdout-log-file))
           (stderr-log-file (plist-get plist :stderr-log-file))
           ;; :after - ordering dependencies (start order)
           (after (elinit--normalize-after (plist-get plist :after)))
           ;; :requires - requirement dependencies (pull-in + ordering)
           (requires (elinit--normalize-after (plist-get plist :requires)))
           ;; Oneshot blocking/timeout settings
           (oneshot-blocking (elinit--oneshot-blocking-p plist))
           (oneshot-timeout (elinit--oneshot-timeout-value plist))
           ;; Tags for filtering (list of symbols or strings)
           (tags-raw (plist-get plist :tags))
           (tags (cond ((null tags-raw) nil)
                       ((listp tags-raw) tags-raw)
                       (t (list tags-raw))))
           ;; P2 fields (indices 14-19)
           (working-directory (plist-get plist :working-directory))
           (environment (plist-get plist :environment))
           (environment-file
            (elinit--normalize-string-or-list
             (plist-get plist :environment-file)))
           (exec-stop
            (elinit--normalize-string-or-list
             (plist-get plist :exec-stop)))
           (exec-reload
            (elinit--normalize-string-or-list
             (plist-get plist :exec-reload)))
           (restart-sec (plist-get plist :restart-sec))
           ;; PT3 fields (indices 20-27)
           (description (plist-get plist :description))
           (documentation-raw (plist-get plist :documentation))
           (documentation
            (elinit--deduplicate-stable
             (elinit--normalize-string-or-list documentation-raw)))
           (before
            (elinit--deduplicate-stable
             (elinit--normalize-after (plist-get plist :before))))
           (wants
            (elinit--deduplicate-stable
             (elinit--normalize-after (plist-get plist :wants))))
           (kill-signal-raw (plist-get plist :kill-signal))
           (kill-signal (when kill-signal-raw
                          (elinit--normalize-signal-name kill-signal-raw)))
           (kill-mode-raw (plist-get plist :kill-mode))
           (kill-mode (when kill-mode-raw
                        (elinit--normalize-kill-mode kill-mode-raw)))
           (remain-after-exit (plist-get plist :remain-after-exit))
           (success-exit-status-raw (plist-get plist :success-exit-status))
           (success-exit-status
            (elinit--normalize-success-exit-status success-exit-status-raw))
           ;; Identity fields (indices 28-29)
           (user (plist-get plist :user))
           (group (plist-get plist :group))
           ;; Target membership fields (indices 30-31)
           (wanted-by
            (elinit--deduplicate-stable
             (elinit--normalize-after (plist-get plist :wanted-by))))
           (required-by
            (elinit--deduplicate-stable
             (elinit--normalize-after (plist-get plist :required-by))))
           ;; Sandbox fields (indices 32-37)
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
            (elinit--deduplicate-stable
             (elinit--normalize-string-or-list
              (plist-get plist :sandbox-ro-bind))))
           (sandbox-rw-bind
            (elinit--deduplicate-stable
             (elinit--normalize-string-or-list
              (plist-get plist :sandbox-rw-bind))))
           (sandbox-tmpfs
            (elinit--deduplicate-stable
             (elinit--normalize-string-or-list
              (plist-get plist :sandbox-tmpfs))))
           (sandbox-raw-args
            (elinit--normalize-string-or-list
             (plist-get plist :sandbox-raw-args)))
           (log-format-raw (plist-get plist :log-format))
           (log-format (when log-format-raw
                         (if (stringp log-format-raw)
                             (intern log-format-raw)
                           log-format-raw)))
           ;; Resource limit fields (indices 39-43)
           (limit-nofile (plist-get plist :limit-nofile))
           (limit-nproc (plist-get plist :limit-nproc))
           (limit-core (plist-get plist :limit-core))
           (limit-fsize (plist-get plist :limit-fsize))
           (limit-as (plist-get plist :limit-as))
           ;; Conflict field (index 44)
           (conflicts
            (elinit--deduplicate-stable
             (elinit--normalize-after (plist-get plist :conflicts)))))
      (list id cmd delay enabled restart logging
            stdout-log-file stderr-log-file
            type after
            oneshot-blocking oneshot-timeout tags requires
            working-directory environment environment-file
            exec-stop exec-reload restart-sec
            description documentation before wants
            kill-signal kill-mode remain-after-exit success-exit-status
            user group wanted-by required-by
            sandbox-profile sandbox-network sandbox-ro-bind sandbox-rw-bind
            sandbox-tmpfs sandbox-raw-args
            log-format
            limit-nofile limit-nproc limit-core limit-fsize limit-as
            conflicts))))

;;; Entry/Service Conversion Functions

(defun elinit-entry-to-service (entry)
  "Convert parsed ENTRY tuple to a `elinit-service' struct."
  (elinit-service--create
   :id (elinit-entry-id entry)
   :command (elinit-entry-command entry)
   :type (elinit-entry-type entry)
   :delay (elinit-entry-delay entry)
   :enabled (elinit-entry-enabled-p entry)
   :restart (elinit-entry-restart-policy entry)
   :logging (elinit-entry-logging-p entry)
   :stdout-log-file (elinit-entry-stdout-log-file entry)
   :stderr-log-file (elinit-entry-stderr-log-file entry)
   :after (elinit-entry-after entry)
   :requires (elinit-entry-requires entry)
   :oneshot-blocking (elinit-entry-oneshot-blocking entry)
   :oneshot-timeout (elinit-entry-oneshot-timeout entry)
   :tags (elinit-entry-tags entry)
   :working-directory (elinit-entry-working-directory entry)
   :environment (elinit-entry-environment entry)
   :environment-file (elinit-entry-environment-file entry)
   :exec-stop (elinit-entry-exec-stop entry)
   :exec-reload (elinit-entry-exec-reload entry)
   :restart-sec (elinit-entry-restart-sec entry)
   :description (elinit-entry-description entry)
   :documentation (elinit-entry-documentation entry)
   :before (elinit-entry-before entry)
   :wants (elinit-entry-wants entry)
   :kill-signal (elinit-entry-kill-signal entry)
   :kill-mode (elinit-entry-kill-mode entry)
   :remain-after-exit (elinit-entry-remain-after-exit entry)
   :success-exit-status (elinit-entry-success-exit-status entry)
   :user (elinit-entry-user entry)
   :group (elinit-entry-group entry)
   :wanted-by (elinit-entry-wanted-by entry)
   :required-by (elinit-entry-required-by entry)
   :sandbox-profile (elinit-entry-sandbox-profile entry)
   :sandbox-network (elinit-entry-sandbox-network entry)
   :sandbox-ro-bind (elinit-entry-sandbox-ro-bind entry)
   :sandbox-rw-bind (elinit-entry-sandbox-rw-bind entry)
   :sandbox-tmpfs (elinit-entry-sandbox-tmpfs entry)
   :sandbox-raw-args (elinit-entry-sandbox-raw-args entry)
   :log-format (elinit-entry-log-format entry)
   :limit-nofile (elinit-entry-limit-nofile entry)
   :limit-nproc (elinit-entry-limit-nproc entry)
   :limit-core (elinit-entry-limit-core entry)
   :limit-fsize (elinit-entry-limit-fsize entry)
   :limit-as (elinit-entry-limit-as entry)
   :conflicts (elinit-entry-conflicts entry)))

(defun elinit-service-to-entry (service)
  "Convert SERVICE struct to a parsed entry tuple."
  (list (elinit-service-id service)
        (elinit-service-command service)
        (elinit-service-delay service)
        (elinit-service-enabled service)
        (elinit-service-restart service)
        (elinit-service-logging service)
        (elinit-service-stdout-log-file service)
        (elinit-service-stderr-log-file service)
        (elinit-service-type service)
        (elinit-service-after service)
        (elinit-service-oneshot-blocking service)
        (elinit-service-oneshot-timeout service)
        (elinit-service-tags service)
        (elinit-service-requires service)
        (elinit-service-working-directory service)
        (elinit-service-environment service)
        (elinit-service-environment-file service)
        (elinit-service-exec-stop service)
        (elinit-service-exec-reload service)
        (elinit-service-restart-sec service)
        (elinit-service-description service)
        (elinit-service-documentation service)
        (elinit-service-before service)
        (elinit-service-wants service)
        (elinit-service-kill-signal service)
        (elinit-service-kill-mode service)
        (elinit-service-remain-after-exit service)
        (elinit-service-success-exit-status service)
        (elinit-service-user service)
        (elinit-service-group service)
        (elinit-service-wanted-by service)
        (elinit-service-required-by service)
        (elinit-service-sandbox-profile service)
        (elinit-service-sandbox-network service)
        (elinit-service-sandbox-ro-bind service)
        (elinit-service-sandbox-rw-bind service)
        (elinit-service-sandbox-tmpfs service)
        (elinit-service-sandbox-raw-args service)
        (elinit-service-log-format service)
        (elinit-service-limit-nofile service)
        (elinit-service-limit-nproc service)
        (elinit-service-limit-core service)
        (elinit-service-limit-fsize service)
        (elinit-service-limit-as service)
        (elinit-service-conflicts service)))

(defun elinit--check-crash-loop (id)
  "Check if ID is crash-looping.  Return t if should NOT restart."
  (let* ((now (float-time))
         (times (gethash id elinit--restart-times))
         (recent (cl-remove-if (lambda (ts) (> (- now ts) elinit-restart-window)) times)))
    (puthash id (cons now recent) elinit--restart-times)
    ;; Fail after max-restarts restarts (so with max=3, fail on 4th crash)
    (when (>= (length recent) elinit-max-restarts)
      (puthash id t elinit--failed)
      (elinit--log 'warning "%s crash-looping, marked as FAILED" id)
      t)))

(defun elinit--format-exit-status (proc-status exit-code)
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

(defun elinit--telemetry-uptime (id)
  "Return uptime in seconds for ID, or nil if not running.
Computes elapsed time since the entry was started."
  (when-let* ((start (gethash id elinit--start-times))
              (proc (gethash id elinit--processes)))
    (when (process-live-p proc)
      (- (float-time) start))))

(defun elinit--telemetry-restart-count (id)
  "Return number of recent restarts for ID within the restart window.
Returns 0 if no restarts have occurred."
  (let* ((now (float-time))
         (times (gethash id elinit--restart-times))
         (recent (cl-remove-if
                  (lambda (ts) (> (- now ts) elinit-restart-window))
                  times)))
    (length recent)))

(defun elinit--telemetry-last-exit (id)
  "Return formatted last exit string for ID, or nil if never exited."
  (when-let* ((info (gethash id elinit--last-exit-info)))
    (let ((status (plist-get info :status))
          (code (plist-get info :code)))
      (pcase status
        ('signal (format "killed by signal %d" code))
        ('exited (if (= code 0)
                     "exited successfully"
                   (format "exited with code %d" code)))
        (_ "terminated")))))

(defun elinit--telemetry-last-exit-info (id &optional snapshot)
  "Return raw last exit info plist for ID, or nil if never exited.
If SNAPSHOT is provided, read from it; otherwise read from globals."
  (gethash id (if snapshot
                  (or (elinit-snapshot-last-exit-info snapshot)
                      (make-hash-table :test 'equal))
                elinit--last-exit-info)))

(defun elinit--telemetry-next-restart-eta (id)
  "Return `float-time' of next scheduled restart for ID, or nil.
Returns the time when the pending restart timer will fire."
  (when-let* ((timer (gethash id elinit--restart-timers)))
    (when (timerp timer)
      (let ((secs (timer--time timer)))
        (when secs
          (float-time secs))))))

(defun elinit--telemetry-process-metrics (pid)
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

(defun elinit--telemetry-log-tail (id &optional lines)
  "Return last LINES lines from the log file for ID.
LINES defaults to 5.  Returns nil if no log file exists.
All formats are decoded through the structured decoder."
  (let ((log-file (elinit--log-file id))
        (n (or lines 5)))
    (when (file-exists-p log-file)
      (condition-case nil
          (let* ((decoded (elinit--log-decode-file
                           log-file n nil
                           (or (when n (* n 512))
                               elinit-log-default-max-bytes)))
                 (records (plist-get decoded :records)))
            (when records
              (concat (mapconcat #'elinit--log-format-record-human
                                 records "\n")
                      "\n")))
        (error nil)))))

(defun elinit--telemetry-process-tree (pid)
  "Return process-tree summary for PID as a plist, or nil.
Returns (:count N :pids (PID1 PID2 ...)) where :pids is bounded
to the first 20 descendants.  Returns nil if PID has no children."
  (when-let* ((descendants (elinit--process-descendants pid)))
    (let ((count (length descendants))
          (bounded (cl-subseq descendants 0 (min 20 (length descendants)))))
      (list :count count :pids bounded))))

(defun elinit--all-parsed-entries ()
  "Parse and validate all entries, returning list of valid parsed entries.
Invalid entries are stored in `elinit--invalid' with reason strings.
Duplicate IDs are skipped with a warning.
Reads from the cached program list via `elinit--effective-programs'."
  (let ((seen (make-hash-table :test 'equal))
        (idx 0)
        result)
    (dolist (entry (elinit--effective-programs))
      (let* ((id (elinit--extract-id entry idx))
             (reason (elinit--validate-entry entry))
             (parsed (unless reason (elinit--parse-entry entry))))
        (cl-incf idx)
        (cond
         ;; Invalid entry
         (reason
          (puthash id reason elinit--invalid)
          (elinit--log 'warning "INVALID %s - %s" id reason))
         ;; Duplicate ID
         ((gethash id seen)
          (elinit--log 'warning "duplicate ID '%s', skipping" id))
         ;; Valid entry
         (t
          (puthash id t seen)
          (push parsed result)))))
    (elinit--merge-unit-file-invalid)
    (nreverse result)))

(defun elinit--stable-topo-sort (entries)
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
    ;; Build graph and record computed deps (validated, existing only)
    ;; Combine :after, :requires, and :wants for dependency tracking
    (dolist (entry entries)
      (let* ((id (elinit-entry-id entry))
             (after (elinit-entry-after entry))
             (requires (elinit-entry-requires entry))
             (wants (elinit-entry-wants entry))
             (all-deps (elinit--deduplicate-stable
                        (append (cl-union after requires :test #'equal)
                                wants)))
             (valid-deps nil))
        (dolist (dep all-deps)
          (when (gethash dep id-to-entry)  ; only count valid deps
            (push dep valid-deps)
            (cl-incf (gethash id in-degree 0))
            (puthash dep (cons id (gethash dep dependents)) dependents)))
        ;; Store computed deps for this entry (reversed to maintain order)
        (puthash id (nreverse valid-deps) elinit--computed-deps)))
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
        (elinit--log 'warning "cycle detected in dependencies, using list order")
        ;; Mark all entries as having cycle fallback and clear
        ;; computed deps to reflect post-fallback state (no edges)
        (dolist (entry entries)
          (let ((id (elinit-entry-id entry)))
            (puthash id t elinit--cycle-fallback-ids)
            (puthash id nil elinit--computed-deps)))
        ;; Return entries with :after, :requires, and :wants stripped
        ;; Handle full (30-element), 13-element, and 11-element entries
        (mapcar (lambda (entry)
                  (let ((len (length entry)))
                    (cond
                     ;; Current schema entry: clear :after (9), :requires (13),
                     ;; and :wants (23).
                     ((>= len 30)
                      (append (cl-subseq entry 0 9)
                              (list nil)                ; clear :after
                              (cl-subseq entry 10 13)
                              (list nil)                ; clear :requires
                              (cl-subseq entry 14 23)
                              (list nil)                ; clear :wants
                              (cl-subseq entry 24)))    ; preserve remaining
                     ;; Legacy full entry: clear :after (7), :requires (11),
                     ;; and :wants (21).
                     ((>= len 22)
                      (append (cl-subseq entry 0 7)
                              (list nil)                ; clear :after
                              (cl-subseq entry 8 11)
                              (list nil)                ; clear :requires
                              (cl-subseq entry 12 21)
                              (list nil)                ; clear :wants
                              (cl-subseq entry 22)))
                     ;; 12+ element entry: clear :after (7) and :requires (11)
                     ((>= len 12)
                      (append (cl-subseq entry 0 7)
                              (list nil)                ; clear :after
                              (cl-subseq entry 8 11)
                              (list nil)                ; clear :requires
                              (cl-subseq entry 12)))    ; preserve new fields
                     ;; 10-element legacy entry: just clear :after (7)
                     ((>= len 10)
                      (append (cl-subseq entry 0 7)
                              (list nil)                ; clear :after
                              (cl-subseq entry 8)))
                     ;; Shorter entries: return as-is
                     (t entry))))
                entries)))))


;;; Logging (see elinit-log.el)
;;
;; The logging subsystem is extracted to elinit-log.el.
;; All defcustom/defvar definitions referenced by elinit-log are
;; above this point, so the require is safe here.
(require 'elinit-log)


(defun elinit--builtin-programs ()
  "Return list of built-in program entries.
These are appended to disk-loaded programs at lowest priority.
A user unit file with the same ID overrides the built-in entry."
  (list
   (cons (elinit--builtin-logrotate-command)
         (list :id "logrotate"
               :type 'oneshot
               :description "Rotate elinit log files"))
   (cons (elinit--builtin-log-prune-command)
         (list :id "log-prune"
               :type 'oneshot
               :after '("logrotate")
               :requires '("logrotate")
               :description "Prune elinit log files"))
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

(defun elinit--dag-init (entries)
  "Initialize DAG scheduler state for ENTRIES.
Disabled entries are marked ready immediately per spec."
  (setq elinit--dag-in-degree (make-hash-table :test 'equal))
  (setq elinit--dag-dependents (make-hash-table :test 'equal))
  (setq elinit--dag-entries (make-hash-table :test 'equal))
  (setq elinit--dag-blocking (make-hash-table :test 'equal))
  (setq elinit--dag-started (make-hash-table :test 'equal))
  (setq elinit--dag-ready (make-hash-table :test 'equal))
  (setq elinit--dag-timeout-timers (make-hash-table :test 'equal))
  (setq elinit--dag-delay-timers (make-hash-table :test 'equal))
  (setq elinit--dag-id-to-index (make-hash-table :test 'equal))
  ;; Collect disabled entries to mark ready after graph is built
  (let ((disabled-ids nil)
        (idx 0))
    ;; Build lookup tables
    ;; First pass: register all entries so we can filter :wants
    (dolist (entry entries)
      (puthash (elinit-entry-id entry) entry elinit--dag-entries))
    ;; Second pass: build dep graph (all deps filtered to DAG entries)
    (dolist (entry entries)
      (let* ((id (elinit-entry-id entry))
             (enabled-p (elinit-entry-enabled-p entry))
             (type (elinit-entry-type entry))
             (after (cl-remove-if-not
                     (lambda (dep) (gethash dep elinit--dag-entries))
                     (elinit-entry-after entry)))
             (requires (cl-remove-if-not
                        (lambda (dep) (gethash dep elinit--dag-entries))
                        (elinit-entry-requires entry)))
             ;; Filter :wants to existing, non-masked entries (soft dep)
             (wants (cl-remove-if-not
                     (lambda (dep)
                       (and (gethash dep elinit--dag-entries)
                            (not (eq 'masked
                                     (gethash dep elinit--mask-override)))))
                     (elinit-entry-wants entry)))
             (oneshot-blocking (elinit-entry-oneshot-blocking entry))
             ;; Combine :after, :requires, and :wants for dependency tracking
             (all-deps (elinit--deduplicate-stable
                        (append (cl-union after requires :test #'equal)
                                wants))))
        (puthash id (length all-deps) elinit--dag-in-degree)
        (puthash id nil elinit--dag-dependents)
        (puthash id idx elinit--dag-id-to-index)
        (cl-incf idx)
        ;; Track blocking oneshots (only if enabled)
        (when (and enabled-p (eq type 'oneshot) oneshot-blocking)
          (puthash id t elinit--dag-blocking))
        ;; Set initial state via FSM transition
        (cond
         ((not enabled-p)
          (push id disabled-ids)
          (elinit--transition-state id 'disabled))
         ((> (length all-deps) 0)
          (elinit--transition-state id 'waiting-on-deps))
         (t
          (elinit--transition-state id 'pending)))))
    ;; Build dependents graph (all deps filtered to DAG entries)
    (dolist (entry entries)
      (let* ((id (elinit-entry-id entry))
             (after (cl-remove-if-not
                     (lambda (dep) (gethash dep elinit--dag-entries))
                     (elinit-entry-after entry)))
             (requires (cl-remove-if-not
                        (lambda (dep) (gethash dep elinit--dag-entries))
                        (elinit-entry-requires entry)))
             (wants (cl-remove-if-not
                     (lambda (dep)
                       (and (gethash dep elinit--dag-entries)
                            (not (eq 'masked
                                     (gethash dep elinit--mask-override)))))
                     (elinit-entry-wants entry)))
             (all-deps (elinit--deduplicate-stable
                        (append (cl-union after requires :test #'equal)
                                wants))))
        (dolist (dep all-deps)
          (puthash dep (cons id (gethash dep elinit--dag-dependents))
                   elinit--dag-dependents))))
    ;; Mark disabled entries as ready immediately (spec requirement)
    ;; This decrements in-degree of their dependents
    (dolist (id disabled-ids)
      (puthash id t elinit--dag-started)
      (elinit--dag-mark-ready id))))

(defun elinit--dag-mark-ready (id)
  "Mark ID as ready and unlock its dependents.
Called when a process is spawned (simple) or exits (oneshot).
No-op if DAG scheduler is not active (e.g., manual starts)."
  (when (and elinit--dag-ready
             (not (gethash id elinit--dag-ready)))
    (puthash id t elinit--dag-ready)
    ;; Record ready time for blame view
    (puthash id (float-time) elinit--ready-times)
    ;; Check if this member's readiness unblocks any converging targets
    (when elinit--target-member-reverse
      (dolist (target-id (gethash id elinit--target-member-reverse))
        (when (gethash target-id elinit--target-converging)
          (elinit--target-check-convergence target-id))))
    ;; Cancel any timeout timer
    (when-let* ((timer (gethash id elinit--dag-timeout-timers)))
      (when (timerp timer)
        (cancel-timer timer))
      (remhash id elinit--dag-timeout-timers))
    ;; Remove from blocking set
    (remhash id elinit--dag-blocking)
    ;; Log that this entry is now ready (event emission is caller's responsibility)
    (elinit--log 'info "%s ready" id)
    ;; Unlock dependents and collect newly ready ones
    (let ((newly-ready nil))
      (dolist (dep-id (gethash id elinit--dag-dependents))
        (when (gethash dep-id elinit--dag-in-degree)
          (cl-decf (gethash dep-id elinit--dag-in-degree))
          (when (= 0 (gethash dep-id elinit--dag-in-degree))
            (elinit--log 'info "%s unlocked by %s" dep-id id)
            (push dep-id newly-ready))))
      ;; Sort by original index for stable ordering, then start
      (setq newly-ready (sort newly-ready
                              (lambda (a b)
                                (< (gethash a elinit--dag-id-to-index 999)
                                   (gethash b elinit--dag-id-to-index 999)))))
      (dolist (dep-id newly-ready)
        (elinit--dag-try-start-entry dep-id)))
    ;; Check if startup is complete
    (elinit--dag-check-complete)))

(defun elinit--dag-process-pending-starts ()
  "Process pending start queue if under max-concurrent-starts limit."
  (while (and elinit--dag-pending-starts
              (or (null elinit-max-concurrent-starts)
                  (< elinit--dag-active-starts elinit-max-concurrent-starts)))
    (let* ((id (pop elinit--dag-pending-starts))
           (entry (gethash id elinit--dag-entries)))
      (when (and entry
                 (not (gethash id elinit--dag-started))
                 (not elinit--shutting-down))
        ;; Count is incremented in elinit--dag-do-start when spawn occurs
        (elinit--dag-start-entry-async entry)))))

(defun elinit--dag-try-start-entry (id)
  "Try to start entry ID if not already started and in-degree is 0."
  (when (and (not (gethash id elinit--dag-started))
             (not (gethash id elinit--dag-delay-timers))  ; not already scheduled
             (= 0 (gethash id elinit--dag-in-degree 0))
             (not elinit--shutting-down))
    ;; Check max-concurrent-starts limit
    (if (and elinit-max-concurrent-starts
             (>= elinit--dag-active-starts elinit-max-concurrent-starts))
        ;; Queue for later
        (unless (member id elinit--dag-pending-starts)
          (setq elinit--dag-pending-starts
                (append elinit--dag-pending-starts (list id))))
      ;; Start immediately (count is managed in elinit--dag-do-start)
      (let ((entry (gethash id elinit--dag-entries)))
        (elinit--dag-start-entry-async entry)))))

;;; Target Convergence Protocol

(defun elinit--target-init-convergence (target-members-hash)
  "Initialize convergence state from TARGET-MEMBERS-HASH.
TARGET-MEMBERS-HASH maps target ID to plist (:requires (ids) :wants (ids)).
Builds the reverse index for convergence callbacks."
  (setq elinit--target-members target-members-hash)
  (setq elinit--target-convergence (make-hash-table :test 'equal))
  (setq elinit--target-convergence-reasons (make-hash-table :test 'equal))
  (setq elinit--target-converging (make-hash-table :test 'equal))
  ;; Build reverse index: member-id -> list of target IDs
  (setq elinit--target-member-reverse (make-hash-table :test 'equal))
  (maphash (lambda (target-id members)
             (dolist (mid (plist-get members :requires))
               (puthash mid (cons target-id
                                  (gethash mid elinit--target-member-reverse))
                        elinit--target-member-reverse))
             (dolist (mid (plist-get members :wants))
               (puthash mid (cons target-id
                                  (gethash mid elinit--target-member-reverse))
                        elinit--target-member-reverse)))
           target-members-hash))

(defun elinit--target-begin-convergence (id)
  "Begin convergence for target ID.
Set state to converging and check for immediate resolution."
  (puthash id 'converging elinit--target-convergence)
  (puthash id t elinit--target-converging)
  (elinit--target-check-convergence id))

(defun elinit--target-check-convergence (id)
  "Check convergence for target ID and resolve if all required members terminal.
A required member is terminal when it appears in `elinit--dag-ready'.
If all required members are terminal, resolve as reached (all healthy) or
degraded (any failed).  Wanted-member failures do not block convergence."
  (let* ((members (gethash id elinit--target-members))
         (required (plist-get members :requires))
         (all-terminal t)
         (degraded-reasons nil))
    ;; Empty required list -> immediate reached
    (dolist (mid required)
      (cond
       ((gethash mid elinit--dag-ready)
        ;; Terminal -- check if failure state
        (let ((state (gethash mid elinit--entry-state)))
          (when (memq state '(failed-to-spawn startup-timeout))
            (push (format "%s: %s" mid state) degraded-reasons))))
       (t
        (setq all-terminal nil))))
    (when all-terminal
      (remhash id elinit--target-converging)
      (if degraded-reasons
          (progn
            (puthash id 'degraded elinit--target-convergence)
            (puthash id (nreverse degraded-reasons)
                     elinit--target-convergence-reasons)
            (elinit--log 'warning "target %s degraded: %s"
                             id (mapconcat #'identity
                                           (gethash id
                                                    elinit--target-convergence-reasons)
                                           ", "))
            (elinit--emit-event 'target-degraded id
                                    (list :reasons degraded-reasons)))
        (puthash id 'reached elinit--target-convergence)
        (elinit--log 'info "target %s reached" id)
        (elinit--emit-event 'target-reached id nil))
      ;; Unlock dependents regardless of reached/degraded
      (elinit--dag-mark-ready id))))

(defun elinit--dag-start-entry-async (entry)
  "Start ENTRY asynchronously.
Mark ready immediately for simple processes, on exit for oneshot."
  (let ((id (elinit-entry-id entry))
        (cmd (elinit-entry-command entry))
        (delay (elinit-entry-delay entry))
        (enabled-p (elinit-entry-enabled-p entry))
        (restart-policy (elinit-entry-restart-policy entry))
        (logging-p (elinit-entry-logging-p entry))
        (stdout-log-file (elinit-entry-stdout-log-file entry))
        (stderr-log-file (elinit-entry-stderr-log-file entry))
        (type (elinit-entry-type entry))
        (oneshot-blocking (elinit-entry-oneshot-blocking entry))
        (oneshot-timeout (elinit-entry-oneshot-timeout entry))
        (working-directory (elinit-entry-working-directory entry))
        (environment (elinit-entry-environment entry))
        (environment-file (elinit-entry-environment-file entry))
        (restart-sec (elinit-entry-restart-sec entry))
        (unit-file-directory (elinit--unit-file-directory-for-id
                              (elinit-entry-id entry)))
        (user (elinit-entry-user entry))
        (group (elinit-entry-group entry))
        (sandbox-entry (when (elinit--sandbox-requesting-p entry) entry))
        (log-format (elinit-entry-log-format entry))
        (limits-entry (when (elinit--limits-requesting-p entry) entry)))
    ;; Check effective enabled state (config + runtime override)
    (let ((effective-enabled (elinit--get-effective-enabled id enabled-p)))
      (cond
       ;; Target: passive node, begin convergence protocol
       ((eq type 'target)
        (elinit--transition-state id 'started)
        (puthash id t elinit--dag-started)
        (elinit--target-begin-convergence id))
       ;; Disabled: mark started and ready immediately
       ((not effective-enabled)
        (elinit--log 'info "%s disabled, skipping" id)
        (elinit--transition-state id 'disabled)
        (puthash id t elinit--dag-started)
        (elinit--dag-mark-ready id))
       ;; Delay: schedule start after delay (don't mark started yet)
       ((> delay 0)
        (elinit--log 'info "scheduling %s after %ds delay..." id delay)
        (elinit--transition-state id 'delayed)
        (puthash id
                 (run-at-time delay nil
                              (lambda ()
                                (remhash id elinit--dag-delay-timers)
                                (elinit--dag-do-start
                                 id cmd logging-p stdout-log-file stderr-log-file
                                 type restart-policy
                                 oneshot-blocking oneshot-timeout
                                 working-directory environment
                                 environment-file restart-sec
                                 unit-file-directory
                                 user group
                                 sandbox-entry log-format
                                 limits-entry)))
                 elinit--dag-delay-timers))
       ;; Start immediately
       (t
        (elinit--dag-do-start
         id cmd logging-p stdout-log-file stderr-log-file
         type restart-policy
         oneshot-blocking oneshot-timeout
         working-directory environment
         environment-file restart-sec
         unit-file-directory
         user group
         sandbox-entry log-format
         limits-entry))))))

(defun elinit--dag-finish-spawn-attempt ()
  "Finish a spawn attempt by decrementing count and processing queue."
  (cl-decf elinit--dag-active-starts)
  (elinit--dag-process-pending-starts))

(defun elinit--dag-handle-spawn-failure (id)
  "Handle spawn failure for ID by marking state and unblocking dependents."
  (elinit--transition-state id 'failed-to-spawn)
  (elinit--emit-event 'process-failed id nil)
  (elinit--dag-finish-spawn-attempt)
  (elinit--dag-mark-ready id))

(defun elinit--dag-setup-oneshot-timeout (id timeout)
  "Set up timeout timer for oneshot ID with TIMEOUT seconds."
  (puthash id
           (run-at-time timeout nil
                        (lambda ()
                          (elinit--log 'warning "oneshot %s timed out after %ds" id timeout)
                          (elinit--dag-mark-ready id)
                          (elinit--emit-event 'process-ready id
                                                  (list :type 'oneshot :timeout t))))
           elinit--dag-timeout-timers))

(defun elinit--dag-handle-spawn-success (id type oneshot-timeout)
  "Handle successful spawn of ID with TYPE and ONESHOT-TIMEOUT."
  (elinit--transition-state id 'started)
  (elinit--emit-event 'process-started id (list :type type))
  (elinit--dag-finish-spawn-attempt)
  (if (eq type 'oneshot)
      (progn
        (elinit--log 'info "started oneshot %s" id)
        (when oneshot-timeout
          (elinit--dag-setup-oneshot-timeout id oneshot-timeout)))
    ;; Simple process: spawned = ready
    (elinit--log 'info "started %s" id)
    (elinit--dag-mark-ready id)
    (elinit--emit-event 'process-ready id (list :type type))))

(defun elinit--dag-do-start (id cmd logging-p stdout-log-file stderr-log-file
                                    type restart-policy
                                    _oneshot-blocking oneshot-timeout
                                    &optional working-directory environment
                                    environment-file restart-sec
                                    unit-file-directory
                                    user group
                                    sandbox-entry log-format
                                    limits-entry)
  "Start process ID with CMD, LOGGING-P, TYPE, RESTART-POLICY, ONESHOT-TIMEOUT.
STDOUT-LOG-FILE and STDERR-LOG-FILE are per-stream log file overrides.
Optional WORKING-DIRECTORY, ENVIRONMENT, ENVIRONMENT-FILE,
RESTART-SEC, UNIT-FILE-DIRECTORY, USER, GROUP, SANDBOX-ENTRY,
LOG-FORMAT, and LIMITS-ENTRY are passed through to
`elinit--start-process'."
  (puthash id t elinit--dag-started)
  (puthash id (float-time) elinit--start-times)
  (cl-incf elinit--dag-active-starts)
  ;; Stop any conflicting active units before starting
  (elinit--conflict-preflight id elinit--current-plan)
  (let ((args (elinit--build-launch-command cmd user group
                                                sandbox-entry
                                                limits-entry)))
    (if (not (executable-find (car args)))
        (progn
          (elinit--log 'warning "executable not found for %s: %s" id (car args))
          (elinit--dag-handle-spawn-failure id))
      (let ((proc (elinit--start-process
                   id cmd logging-p type restart-policy nil
                   working-directory environment
                   environment-file restart-sec
                   unit-file-directory
                   user group
                   stdout-log-file stderr-log-file
                   sandbox-entry log-format
                   limits-entry)))
        (if (not proc)
            (elinit--dag-handle-spawn-failure id)
          (elinit--dag-handle-spawn-success id type oneshot-timeout))))))

(defun elinit--dag-check-complete ()
  "Check if startup is complete and invoke callback if so."
  (when (and elinit--dag-complete-callback
             ;; All entries started (actually spawned, not just scheduled)
             (= (hash-table-count elinit--dag-started)
                (hash-table-count elinit--dag-entries))
             ;; No delayed entries pending
             (= 0 (hash-table-count elinit--dag-delay-timers))
             ;; No blocking oneshots pending
             (= 0 (hash-table-count elinit--dag-blocking))
             ;; No targets still converging
             (or (null elinit--target-converging)
                 (= 0 (hash-table-count elinit--target-converging))))
    (elinit--log 'info "all entries complete")
    (let ((callback elinit--dag-complete-callback))
      (setq elinit--dag-complete-callback nil)
      (funcall callback))))

(defun elinit--dag-cleanup ()
  "Clean up DAG scheduler state."
  ;; Cancel any remaining timeout timers
  (when elinit--dag-timeout-timers
    (maphash (lambda (_id timer)
               (when (timerp timer)
                 (cancel-timer timer)))
             elinit--dag-timeout-timers))
  ;; Cancel any remaining delay timers
  (when elinit--dag-delay-timers
    (maphash (lambda (_id timer)
               (when (timerp timer)
                 (cancel-timer timer)))
             elinit--dag-delay-timers))
  ;; Cancel startup timeout timer
  (when (timerp elinit--dag-timeout-timer)
    (cancel-timer elinit--dag-timeout-timer))
  (setq elinit--dag-in-degree nil)
  (setq elinit--dag-dependents nil)
  (setq elinit--dag-entries nil)
  (setq elinit--dag-blocking nil)
  (setq elinit--dag-started nil)
  (setq elinit--dag-ready nil)
  (setq elinit--dag-timeout-timers nil)
  (setq elinit--dag-delay-timers nil)
  (setq elinit--dag-id-to-index nil)
  (setq elinit--dag-complete-callback nil)
  (setq elinit--dag-timeout-timer nil)
  (setq elinit--dag-pending-starts nil)
  (setq elinit--dag-active-starts 0)
  ;; Clear DAG-only convergence temporaries (execution-phase tracking).
  ;; Preserve elinit--target-convergence, convergence-reasons, and
  ;; target-members: these must outlive the DAG so the dashboard and
  ;; CLI can display convergence state after startup completes.
  (setq elinit--target-converging nil)
  (setq elinit--target-member-reverse nil))

;;; Process Management

(defun elinit--handle-oneshot-exit (name proc-status exit-code)
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
    (puthash name stored-code elinit--oneshot-completed))
  ;; Set remain-after-exit active latch on successful exit
  (when (and (eq proc-status 'exit) (= exit-code 0))
    (when-let* ((entry (elinit--get-entry-for-id name)))
      (when (elinit-entry-remain-after-exit entry)
        (puthash name t elinit--remain-active)
        (elinit--log 'info "oneshot %s active (remain-after-exit)" name))))
  (if (eq proc-status 'signal)
      (elinit--log 'warning "oneshot %s %s"
                       name (elinit--format-exit-status proc-status exit-code))
    (if (> exit-code 0)
        (elinit--log 'warning "oneshot %s %s"
                         name (elinit--format-exit-status proc-status exit-code))
      (unless (gethash name elinit--remain-active)
        (elinit--log 'info "oneshot %s completed" name))))
  ;; Invoke any registered wait callback (event-driven)
  (when-let* ((cb-entry (gethash name elinit--oneshot-callbacks)))
    (remhash name elinit--oneshot-callbacks)
    (when (cdr cb-entry)
      (cancel-timer (cdr cb-entry)))
    (funcall (car cb-entry) (not elinit--shutting-down)))
  ;; Notify DAG scheduler and emit ready event
  (elinit--dag-mark-ready name)
  (elinit--emit-event 'process-ready name
                          (list :type 'oneshot :exit-code exit-code)))

(defun elinit--handle-shutdown-exit ()
  "Handle process exit during shutdown.
Decrement remaining count and signal completion when all done.
Only activates after `elinit--shutdown-remaining' has been set
to a positive value; early exits during the exec-stop phase are
ignored."
  (when (> elinit--shutdown-remaining 0)
    (cl-decf elinit--shutdown-remaining)
    (when (<= elinit--shutdown-remaining 0)
      ;; Cancel timeout timer (early completion)
      (when elinit--shutdown-timer
        (cancel-timer elinit--shutdown-timer)
        (setq elinit--shutdown-timer nil))
      ;; All services exited; stop log writers and complete
      (elinit--stop-all-writers)
      (let ((cb elinit--shutdown-callback))
        (setq elinit--shutdown-callback nil)
        (clrhash elinit--processes)
        (setq elinit--shutdown-complete-flag t)
        (when cb (funcall cb))))))

(defun elinit--schedule-restart (id cmd default-logging type config-restart
                                        proc-status exit-code
                                        &optional working-directory environment
                                        environment-file restart-sec
                                        unit-file-directory
                                        user group
                                        stdout-log-file stderr-log-file
                                        sandbox-entry log-format
                                        limits-entry)
  "Schedule restart of process ID after crash.
CMD, DEFAULT-LOGGING, TYPE, CONFIG-RESTART are original process params.
PROC-STATUS and EXIT-CODE describe the exit for logging.
WORKING-DIRECTORY, ENVIRONMENT, ENVIRONMENT-FILE, RESTART-SEC,
and UNIT-FILE-DIRECTORY are per-unit overrides preserved across restarts.
USER and GROUP are identity parameters preserved for restart.
STDOUT-LOG-FILE and STDERR-LOG-FILE preserve per-stream log targets.
SANDBOX-ENTRY, when non-nil, is the parsed entry tuple for sandbox.
LOG-FORMAT is the structured log format symbol (`text' or `binary').
LIMITS-ENTRY, when non-nil, is the parsed entry tuple for resource limits."
  (elinit--log 'info "%s %s, restarting..."
                   id (elinit--format-exit-status proc-status exit-code))
  (let ((delay (or restart-sec elinit-restart-delay)))
    (puthash id
             (run-at-time delay nil
                          (lambda ()
                            ;; Stop conflicting units before restarting
                            (elinit--conflict-preflight
                             id elinit--current-plan)
                            (elinit--start-process
                             id cmd default-logging type config-restart t
                             working-directory environment
                             environment-file restart-sec
                             unit-file-directory
                             user group
                             stdout-log-file stderr-log-file
                             sandbox-entry log-format
                             limits-entry)))
             elinit--restart-timers)))

(defun elinit--make-process-sentinel (id cmd default-logging type config-restart
                                             &optional working-directory environment
                                             environment-file restart-sec
                                             unit-file-directory
                                             user group
                                             stdout-log-file stderr-log-file
                                             sandbox-entry log-format
                                             limits-entry)
  "Create a process sentinel for ID with captured parameters.
CMD, DEFAULT-LOGGING, TYPE, CONFIG-RESTART are stored for restart.
WORKING-DIRECTORY, ENVIRONMENT, ENVIRONMENT-FILE, RESTART-SEC,
and UNIT-FILE-DIRECTORY are per-unit overrides preserved across restarts.
USER and GROUP are identity parameters preserved for restart.
STDOUT-LOG-FILE and STDERR-LOG-FILE preserve per-stream log targets.
SANDBOX-ENTRY, when non-nil, is the parsed entry tuple for sandbox.
LOG-FORMAT is the structured log format symbol (`text' or `binary').
LIMITS-ENTRY, when non-nil, is the parsed entry tuple for resource limits."
  (lambda (p _event)
    (unless (process-live-p p)
      (let* ((name (process-name p))
             (pid (process-id p))
             (proc-status (process-status p))
             (exit-code (process-exit-status p))
             (exit-status (pcase proc-status
                            ('signal 'signal)
                            ('exit 'exited)
                            (_ 'unknown))))
        (remhash name elinit--processes)
        ;; Send exit frames to writers, then EOF so logd can drain
        ;; before teardown.  EOF causes logd's read() to return 0,
        ;; letting it process any buffered frames and exit cleanly.
        (let ((exit-status-wire (elinit--exit-status-code exit-status)))
          (dolist (table (list elinit--writers elinit--stderr-writers))
            (when-let* ((w (gethash name table)))
              (when (process-live-p w)
                (elinit--log-send-frame w 2 3 (or pid 0) name nil
                                            exit-code exit-status-wire)
                (condition-case nil
                    (process-send-eof w)
                  (error nil))))))
        ;; Defer writer teardown to allow logd to drain buffered
        ;; frames after EOF.  The timer fires after 0.2s; if logd
        ;; has already exited by then, stop-writer is a no-op.
        ;; Capture current writer processes so a fast restart
        ;; (restart-sec=0) that replaces them is not affected.
        (let ((deferred-name name)
              (old-stdout (gethash name elinit--writers))
              (old-stderr (gethash name elinit--stderr-writers))
              (old-pipe (gethash name elinit--stderr-pipes)))
          (run-at-time 0.2 nil
                       (lambda ()
                         (elinit--stop-writer-if-same
                          deferred-name
                          old-stdout old-stderr old-pipe))))
        ;; Record last exit info for telemetry
        (puthash name (list :status exit-status :code exit-code
                            :timestamp (float-time))
                 elinit--last-exit-info)
        ;; Emit process exit event
        (elinit--emit-event 'process-exit name
                                (list :status exit-status :code exit-code))
        ;; Handle oneshot completion
        (when (eq type 'oneshot)
          (elinit--handle-oneshot-exit name proc-status exit-code))
        (elinit--maybe-refresh-dashboard)
        ;; Handle shutdown tracking
        (when elinit--shutting-down
          (elinit--handle-shutdown-exit))
        ;; Schedule restart if appropriate
        (unless (or (eq type 'oneshot)
                    elinit--shutting-down
                    (gethash name elinit--manually-stopped)
                    (eq (gethash name elinit--enabled-override) 'disabled)
                    (not (let ((ses (when-let* ((e (elinit--get-entry-for-id name)))
                                     (elinit-entry-success-exit-status e))))
                           (elinit--should-restart-p
                            (elinit--get-effective-restart name config-restart)
                            proc-status exit-code ses)))
                    (gethash name elinit--failed)
                    (elinit--check-crash-loop name))
	          (elinit--schedule-restart id cmd default-logging type config-restart
	                                        proc-status exit-code
	                                        working-directory environment
	                                        environment-file restart-sec
	                                        unit-file-directory
	                                        user group
	                                        stdout-log-file stderr-log-file
	                                        sandbox-entry log-format
	                                        limits-entry))))))

;;; Process Environment and Working Directory Helpers

(defun elinit--parse-env-file (path)
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
              (elinit--log 'warning "%s:%d: invalid env line: %s"
                               path line-num line))))
        (forward-line 1)))
    (nreverse result)))

(defun elinit--resolve-env-file-path (path unit-file-directory)
  "Resolve environment file PATH relative to UNIT-FILE-DIRECTORY.
Strip leading `-' prefix (optional missing marker) and return
a cons (OPTIONAL-P . RESOLVED-PATH)."
  (let ((optional (string-prefix-p "-" path)))
    (when optional
      (setq path (substring path 1)))
    (let ((resolved (expand-file-name path unit-file-directory)))
      (cons optional resolved))))

(defun elinit--build-process-environment (env-files env-alist unit-file-directory)
  "Build effective `process-environment' for a unit.
Start from current `process-environment', apply ENV-FILES in order,
then apply ENV-ALIST pairs in order.  Later assignments for the
same key override earlier ones.  UNIT-FILE-DIRECTORY is used for
relative path resolution in env-files."
  (let ((env (copy-sequence process-environment)))
    ;; Apply env-files in order
    (dolist (path env-files)
      (let* ((resolved (elinit--resolve-env-file-path path unit-file-directory))
             (optional (car resolved))
             (file (cdr resolved)))
        (if (file-readable-p file)
            (dolist (pair (elinit--parse-env-file file))
              (setq env (cons (format "%s=%s" (car pair) (cdr pair))
                              env)))
          (unless optional
            (error "Required environment file not found: %s" file)))))
    ;; Apply env-alist in order (later wins for same key)
    (dolist (pair env-alist)
      (setq env (cons (format "%s=%s" (car pair) (cdr pair))
                      env)))
    env))

(defun elinit--resolve-working-directory (dir unit-file-directory)
  "Resolve working directory DIR for a unit.
Expand `~' paths.  Resolve relative paths against UNIT-FILE-DIRECTORY.
Return the resolved absolute path, or signal an error if the directory
does not exist."
  (let ((resolved (expand-file-name dir unit-file-directory)))
    (unless (file-directory-p resolved)
      (error "Working directory does not exist: %s" resolved))
    resolved))

(defun elinit--unit-file-directory-for-id (id)
  "Return the directory of the authoritative unit file for ID.
Return nil if no unit file is found or if `elinit-units' is not loaded."
  (when (fboundp 'elinit--unit-file-path)
    (when-let* ((path (elinit--unit-file-path id)))
      (file-name-directory path))))

(defun elinit--run-command-with-timeout (cmd timeout dir env log-file)
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
                :name "elinit-exec-cmd"
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

(defun elinit--exec-command-chain (commands id dir env log-file timeout)
  "Execute COMMANDS sequentially for unit ID.
DIR is the working directory.  ENV is the process environment.
LOG-FILE receives output when non-nil.  TIMEOUT is the
per-command timeout in seconds.
Return t if all commands succeed (exit 0), nil otherwise.
All commands are executed regardless of individual failures."
  (let ((all-ok t))
    (dolist (cmd commands)
      (elinit--log 'info "%s: executing: %s" id cmd)
      (let ((exit-code
             (condition-case err
                 (elinit--run-command-with-timeout
                  cmd timeout dir env log-file)
               (error
                (elinit--log 'warning "%s: command error: %s"
                                 id (error-message-string err))
                1))))
        (cond
         ((= exit-code 124)
          (elinit--log 'warning "%s: command timed out after %ds: %s"
                           id timeout cmd)
          (setq all-ok nil))
         ((/= exit-code 0)
          (elinit--log 'warning "%s: command failed (exit %d): %s"
                           id exit-code cmd)
          (setq all-ok nil)))))
    all-ok))

(defun elinit--run-exec-stop-for-id (id)
  "Run exec-stop commands for unit ID if configured.
Look up the entry, verify it is a simple-type unit with
`:exec-stop' commands, resolve the unit's effective working
directory and environment, and execute the stop command chain.

Skip silently if the entry has no exec-stop commands, is not
a simple type, or cannot be found.  If context resolution fails,
log a warning and skip the chain rather than running commands in
the wrong context."
  (when-let* ((entry (elinit--get-entry-for-id id))
              (exec-stop (elinit-entry-exec-stop entry)))
    (when (eq (elinit-entry-type entry) 'simple)
      (let* ((working-directory (elinit-entry-working-directory entry))
             (environment (elinit-entry-environment entry))
             (environment-file (elinit-entry-environment-file entry))
             (logging-p (elinit-entry-logging-p entry))
             (unit-file-directory (elinit--unit-file-directory-for-id id))
             (dir (if working-directory
                      (condition-case err
                          (elinit--resolve-working-directory
                           working-directory unit-file-directory)
                        (error
                         (elinit--log
                          'warning
                          "%s: cannot resolve working directory for exec-stop: %s"
                          id (error-message-string err))
                         nil))
                    default-directory))
             (env (when dir
                    (if (or environment-file environment)
                        (condition-case err
                            (elinit--build-process-environment
                             environment-file environment unit-file-directory)
                          (error
                           (elinit--log
                            'warning
                            "%s: cannot resolve environment for exec-stop: %s"
                            id (error-message-string err))
                           nil))
                      process-environment))))
        (when (and dir env)
          (let* ((logging (elinit--get-effective-logging id logging-p))
                 (log-file (when logging
                             (elinit--log-file id))))
            (elinit--exec-command-chain
             exec-stop id dir env log-file elinit-shutdown-timeout)))))))

(defun elinit--identity-source-trusted-p (id)
  "Return non-nil if the unit source for ID is trusted for identity change.
When elinit runs as root and a unit requests identity change
via `:user' or `:group', the unit must come from a verifiable,
root-owned unit file that is not world-writable.  If the units
module is not loaded (standalone core mode) or the file cannot be
verified, return nil (fail closed).

This enforces the PLAN-user-group-privdrop.md requirement that
root-mode elinit never runs identity-changing units from
untrusted sources."
  (when (fboundp 'elinit--unit-file-path)
    (when-let* ((path (elinit--unit-file-path id)))
      (when (file-exists-p path)
        (let ((attrs (file-attributes path 'integer)))
          (and attrs
               ;; File must be owned by root (uid 0)
               (zerop (file-attribute-user-id attrs))
               ;; File must not be world-writable
               (let ((mode (file-modes path)))
                 (and mode (zerop (logand mode #o002))))))))))

;;; Libexec helpers (see elinit-libexec.el)
;;
;; Build target enumeration, compiler selection, and build invocation
;; are extracted to elinit-libexec.el.
(require 'elinit-libexec)

(defun elinit--shell-metachar-p (cmd)
  "Return non-nil if CMD contain shell metacharacters.
When present, CMD must be run via `sh -c' rather than split
directly, because `split-string-and-unquote' does not interpret
shell operators like `&&', `||', pipes, or redirections."
  (string-match-p "[&|;<>$`\n]" cmd))

;;; Sandbox profiles (see elinit-sandbox.el)
;;
;; Profile argument construction and bwrap argv wrapping are
;; extracted to elinit-sandbox.el.
(require 'elinit-sandbox)

(defun elinit--format-limit-value (val)
  "Format resource limit VAL as a \"SOFT:HARD\" string for the helper CLI.
VAL is an integer (both soft and hard), symbol `infinity', or
string \"SOFT:HARD\"."
  (cond
   ((eq val 'infinity) "infinity:infinity")
   ((integerp val) (format "%d:%d" val val))
   ((stringp val) val)
   (t (error "Invalid limit value: %S" val))))

(defun elinit--build-launch-command (cmd &optional user group
                                                       sandbox-entry
                                                       limits-entry)
  "Build the command argument list for launching CMD.
CMD is a shell command string.  USER and GROUP are optional
identity parameters for privilege drop.  When either is non-nil,
`elinit-runas-command' is prepended with identity arguments
and the target program is resolved to an absolute path (the helper
uses direct execv without PATH search).
SANDBOX-ENTRY, when non-nil, is a parsed entry tuple from which
sandbox wrapper arguments are extracted.
LIMITS-ENTRY, when non-nil, is a parsed entry tuple from which
resource limit arguments are extracted.  Launch ordering is:
elinit-rlimits -> elinit-runas -> bwrap -> service executable.
When CMD contains shell metacharacters (`&&', pipes, etc.), it is
wrapped as `sh -c CMD' so the shell interprets operators correctly.
Return a list suitable for the `make-process' :command keyword."
  (let* ((args (if (elinit--shell-metachar-p cmd)
                   (list shell-file-name shell-command-switch cmd)
                 (split-string-and-unquote cmd)))
         (sandbox-argv (when sandbox-entry
                         (elinit--sandbox-build-argv sandbox-entry))))
    ;; Build from inside out: args -> sandbox-argv+args -> runas+all -> rlimits+all
    (when sandbox-argv
      ;; Resolve program to absolute path inside sandbox
      (let ((resolved (executable-find (car args))))
        (when resolved
          (setcar args resolved)))
      (setq args (append sandbox-argv args)))
    (when (or user group)
      (let ((helper-args (list elinit-runas-command))
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
        (setq args (append helper-args (list "--") args))))
    ;; Outermost wrapper: resource limits
    (when (and limits-entry (elinit--limits-requesting-p limits-entry))
      (let ((rlimits-args (list elinit-rlimits-command)))
        (when-let* ((v (elinit-entry-limit-nofile limits-entry)))
          (setq rlimits-args
                (append rlimits-args
                        (list "--nofile" (elinit--format-limit-value v)))))
        (when-let* ((v (elinit-entry-limit-nproc limits-entry)))
          (setq rlimits-args
                (append rlimits-args
                        (list "--nproc" (elinit--format-limit-value v)))))
        (when-let* ((v (elinit-entry-limit-core limits-entry)))
          (setq rlimits-args
                (append rlimits-args
                        (list "--core" (elinit--format-limit-value v)))))
        (when-let* ((v (elinit-entry-limit-fsize limits-entry)))
          (setq rlimits-args
                (append rlimits-args
                        (list "--fsize" (elinit--format-limit-value v)))))
        (when-let* ((v (elinit-entry-limit-as limits-entry)))
          (setq rlimits-args
                (append rlimits-args
                        (list "--as" (elinit--format-limit-value v)))))
        (setq args (append rlimits-args (list "--") args))))
    args))

(defun elinit--start-process (id cmd default-logging type config-restart
                                     &optional is-restart
                                     working-directory environment
                                     environment-file restart-sec
                                     unit-file-directory
                                     user group
                                     stdout-log-file stderr-log-file
                                     sandbox-entry log-format
                                     limits-entry)
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
When set, `elinit-runas-command' is used to launch the process
as the specified identity.  Requires root privileges.
STDOUT-LOG-FILE and STDERR-LOG-FILE are optional per-unit stream log
targets.  By default stderr follows stdout into the same log file.
SANDBOX-ENTRY, when non-nil, is a parsed entry tuple from which
sandbox wrapper arguments are built via bubblewrap.
LOG-FORMAT is the structured log format symbol (`text' or `binary').
LIMITS-ENTRY, when non-nil, is the parsed entry tuple from which
resource limit arguments are extracted."
  ;; Clear any pending restart timer for this ID first
  (when-let* ((timer (gethash id elinit--restart-timers)))
    (when (timerp timer)
      (cancel-timer timer))
    (remhash id elinit--restart-timers))
  ;; Clear manually-stopped flag - starting an entry re-enables restart behavior
  (remhash id elinit--manually-stopped)
  ;; Guard conditions - don't start if:
  ;; - shutting down
  ;; - this is a crash-restart AND (restart disabled or failed or runtime-disabled)
  ;; - already running (race with pending timer)
  (unless (or elinit--shutting-down
              (and is-restart (eq (elinit--get-effective-restart id config-restart) 'no))
              (and is-restart (gethash id elinit--failed))
              (and is-restart (eq (gethash id elinit--enabled-override) 'disabled))
              (and (gethash id elinit--processes)
                   (process-live-p (gethash id elinit--processes)))
              ;; Non-root elinit cannot change process identity
              (and (or user group)
                   (not (zerop (user-uid)))
                   (progn
                     (puthash id
                              (format "identity change requires root (user=%s group=%s)"
                                      user group)
                              elinit--spawn-failure-reason)
                     (elinit--log
                      'warning
                      "%s: identity change requires root (user=%s group=%s)"
                      id user group)
                     t))
              ;; Root elinit requires trusted unit source for identity change
              (and (or user group)
                   (zerop (user-uid))
                   (not (elinit--identity-source-trusted-p id))
                   (progn
                     (puthash id
                              (format "unit source not trusted (user=%s group=%s)"
                                      user group)
                              elinit--spawn-failure-reason)
                     (elinit--log
                      'warning
                      "%s: identity change blocked - unit source not trusted (user=%s group=%s)"
                      id user group)
                     t)))
    (let* ((default-directory
            (if working-directory
                (condition-case err
                    (elinit--resolve-working-directory
                     working-directory unit-file-directory)
                  (error
                   (elinit--log 'warning "%s: %s" id (error-message-string err))
                   nil))
              default-directory))
           (process-environment
            (if (or environment-file environment)
                (condition-case err
                    (elinit--build-process-environment
                     environment-file environment unit-file-directory)
                  (error
                   (elinit--log 'warning "%s: %s" id (error-message-string err))
                   nil))
              process-environment)))
      (when (and default-directory process-environment)
        (let* ((args (elinit--build-launch-command cmd user group
                                                        sandbox-entry
                                                        limits-entry))
               (logging (elinit--get-effective-logging id default-logging))
               (default-log-file (when logging
                                   (elinit--log-file id)))
               (stdout-log-target (when logging
                                    (or stdout-log-file default-log-file)))
               (stderr-log-target (when logging
                                    (or stderr-log-file stdout-log-target)))
               (split-stderr (and stderr-log-target
                                  (not (equal stderr-log-target
                                              stdout-log-target))))
               (writer (when stdout-log-target
                         (elinit--start-writer id stdout-log-target
                                                   log-format)))
               (stderr-writer (when split-stderr
                                (elinit--start-stderr-writer
                                 id stderr-log-target log-format)))
               ;; When logging is enabled with merged streams, stderr
               ;; still needs a dedicated pipe so its output is tagged
               ;; stream=2 (stderr).  The pipe sends frames to the
               ;; stdout writer.  This applies whether log-format is
               ;; explicit or omitted (framed transport is always used).
               (merged-stderr-pipe (when (and writer
                                              (not split-stderr)
                                              stderr-log-target)
                                     (elinit--start-stderr-pipe
                                      id writer)))
               (stderr-pipe (or (when stderr-writer
                                  (elinit--start-stderr-pipe id stderr-writer))
                                merged-stderr-pipe))
               ;; If a pipe was needed but creation failed, abort
               ;; start to preserve stream identity.  Clean up any
               ;; writers already started.
               (pipe-failed (and (or stderr-writer
                                     (and writer (not split-stderr)
                                          stderr-log-target))
                                 (not stderr-pipe)))
               (_ (when pipe-failed
                    (when stderr-writer
                      (elinit--stop-stream-writer
                       id 'stderr elinit--stderr-writers))
                    (elinit--stop-writer id)
                    (let ((msg "stderr pipe creation failed"))
                      (puthash id msg elinit--spawn-failure-reason)
                      (elinit--log 'warning "%s: %s" id msg))))
               (effective-split (or (and stderr-writer stderr-pipe)
                                    merged-stderr-pipe))
               (proc (unless pipe-failed
                       (condition-case err
                         (make-process
                          :name id
                          :command args
                          :connection-type 'pipe
                          :coding 'no-conversion
                          ;; In split mode or framed merged mode, stderr is
                          ;; routed through a dedicated pipe.
                          :stderr (when effective-split stderr-pipe)
                          :filter (when writer
                                    (lambda (proc output)
                                      (when (process-live-p writer)
                                        (elinit--log-send-frame
                                         writer 1 1 (process-id proc)
                                         id output))))
                          :sentinel (elinit--make-process-sentinel
                                     id cmd default-logging type config-restart
                                     working-directory environment
                                     environment-file restart-sec
                                     unit-file-directory
                                     user group
                                     stdout-log-file stderr-log-file
                                     sandbox-entry log-format
                                     limits-entry))
                       (error
                        (elinit--stop-writer id)
                        (puthash id (error-message-string err)
                                 elinit--spawn-failure-reason)
                        (elinit--log 'warning "%s: %s"
                                         id (error-message-string err))
                        nil)))))
          (when proc
            (set-process-query-on-exit-flag proc nil)
            (puthash id proc elinit--processes))
          proc)))))

(defun elinit--wait-for-oneshot (id &optional timeout callback)
  "Wait for oneshot ID to complete asynchronously (event-driven).
TIMEOUT is seconds; nil means wait indefinitely.
CALLBACK is called with t on completion, nil on timeout/shutdown.
If CALLBACK is nil, returns immediately (fire-and-forget).
Uses sentinel-driven notification instead of polling."
  (when callback
    (cond
     ;; Already completed
     ((gethash id elinit--oneshot-completed)
      (funcall callback t))
     ;; Already shutting down
     (elinit--shutting-down
      (funcall callback nil))
     ;; Register callback for sentinel to invoke
     (t
      (let ((timeout-timer
             (when timeout
               (run-at-time timeout nil
                            (lambda ()
                              (when-let* ((cb-entry (gethash id elinit--oneshot-callbacks)))
                                (remhash id elinit--oneshot-callbacks)
                                (funcall (car cb-entry) nil)))))))
        (puthash id (cons callback timeout-timer) elinit--oneshot-callbacks))))))

(defun elinit--start-entry-async (entry callback)
  "Start ENTRY asynchronously for dashboard manual start.
CALLBACK is called with t on success, nil on error.  CALLBACK may be nil."
  (let ((id (elinit-entry-id entry))
        (cmd (elinit-entry-command entry))
        (enabled-p (elinit-entry-enabled-p entry))
        (restart-policy (elinit-entry-restart-policy entry))
        (logging-p (elinit-entry-logging-p entry))
        (stdout-log-file (elinit-entry-stdout-log-file entry))
        (stderr-log-file (elinit-entry-stderr-log-file entry))
        (type (elinit-entry-type entry))
        (otimeout (elinit-entry-oneshot-timeout entry))
        (working-directory (elinit-entry-working-directory entry))
        (environment (elinit-entry-environment entry))
        (environment-file (elinit-entry-environment-file entry))
        (restart-sec (elinit-entry-restart-sec entry))
        (unit-file-directory (elinit--unit-file-directory-for-id
                              (elinit-entry-id entry)))
        (user (elinit-entry-user entry))
        (group (elinit-entry-group entry))
        (sandbox-entry (when (elinit--sandbox-requesting-p entry) entry))
        (log-format (elinit-entry-log-format entry))
        (limits-entry (when (elinit--limits-requesting-p entry) entry)))
    (if (not (elinit--get-effective-enabled id enabled-p))
        (progn
          (elinit--log 'info "%s disabled (config or override), skipping" id)
          (when callback (funcall callback t)))
      (let ((args (elinit--build-launch-command cmd user group
                                                    sandbox-entry
                                                    limits-entry)))
        (if (not (executable-find (car args)))
            (progn
              (elinit--log 'warning "executable not found for %s: %s" id (car args))
              (elinit--emit-event 'process-failed id nil)
              (when callback (funcall callback nil)))
          ;; Start the process
          (let ((proc (elinit--start-process
                       id cmd logging-p type restart-policy nil
                       working-directory environment
                       environment-file restart-sec
                       unit-file-directory
                       user group
                       stdout-log-file stderr-log-file
                       sandbox-entry log-format
                       limits-entry)))
            (if (not proc)
                (progn
                  (elinit--emit-event 'process-failed id nil)
                  (when callback (funcall callback nil)))
              (elinit--emit-event 'process-started id (list :type type))
              (if (eq type 'oneshot)
                  ;; Wait for oneshot via timer (no polling loop)
                  ;; process-ready is emitted by oneshot exit handler
                  (progn
                    (elinit--log 'info "waiting for oneshot %s..." id)
                    (elinit--wait-for-oneshot
                     id otimeout
                     (lambda (completed)
                       (if completed
                           (elinit--log 'info "oneshot %s completed" id)
                         (elinit--log 'warning "oneshot %s timed out" id))
                       (when callback (funcall callback completed)))))
                ;; Simple process: spawned = ready
                (elinit--log 'info "started %s" id)
                (elinit--emit-event 'process-ready id (list :type type))
                (when callback (funcall callback t))))))))))

;;; Conflict Transaction Primitives

(defvar elinit--current-plan nil
  "The most recent plan struct for conflict lookups.
Set during `elinit-start', `elinit--reconcile', and isolate.")

(defun elinit--conflict-preflight (id plan)
  "Stop active units that conflict with ID according to PLAN.
Look up both forward conflicts (ID declares :conflicts) and reverse
conflicts (other units declare :conflicts listing ID).  For each
active conflicting unit, stop it and mark it conflict-suppressed.
Return the list of stopped IDs."
  (let ((stopped nil))
    (when plan
      (let* ((forward (gethash id (elinit-plan-conflicts-deps plan)))
             (reverse (gethash id (elinit-plan-conflict-reverse plan)))
             (all-conflicts (elinit--deduplicate-stable
                             (append forward reverse))))
        (dolist (cid all-conflicts)
          (unless (equal cid id)
            (let ((proc (gethash cid elinit--processes)))
              (when (and proc (process-live-p proc))
                ;; Cancel any pending restart timer
                (let ((timer (gethash cid elinit--restart-timers)))
                  (when (and timer (timerp timer))
                    (cancel-timer timer)
                    (remhash cid elinit--restart-timers)))
                ;; Mark as conflict-suppressed and manually-stopped
                (puthash cid id elinit--conflict-suppressed)
                (puthash cid t elinit--manually-stopped)
                ;; Stop the process
                (let ((kill-signal (elinit--kill-signal-for-id cid)))
                  (signal-process proc kill-signal))
                (elinit--log 'info
                  "conflict: stopping %s (conflicts with %s)" cid id)
                (push cid stopped)))))))
    stopped))

(defun elinit--conflict-clear-suppression (id)
  "Clear conflict suppression for ID.
Called when a unit is explicitly started (manual start, reconcile)."
  (remhash id elinit--conflict-suppressed))

(defun elinit--manual-start (id)
  "Attempt to manually start entry with ID.
Returns a plist with :status and :reason.
:status is one of: started, skipped, error
:reason explains why (for skipped/error cases).

This function handles all pre-start checks and state clearing,
ensuring consistent behavior between dashboard and CLI.

Per the systemctl model, disabled units CAN be manually started
\(session-only, no enabled override change).  Only masked units
are blocked.  Manually started disabled units are tracked in
`elinit--manually-started' so that reconcile does not stop them."
  (cond
   ;; Entry is invalid (check first - get-entry-for-id skips invalid entries)
   ((gethash id elinit--invalid)
    (list :status 'error :reason "invalid entry"))
   ;; Target entries are passive (no process to start)
   ((let ((e (elinit--get-entry-for-id id)))
      (and e (eq (elinit-entry-type e) 'target)))
    (list :status 'error :reason "cannot start a target unit"))
   ;; Already running
   ((and (gethash id elinit--processes)
         (process-live-p (gethash id elinit--processes)))
    (list :status 'skipped :reason "already running"))
   ;; Active remain-after-exit unit (no-op)
   ((gethash id elinit--remain-active)
    (list :status 'skipped :reason "already active"))
   ;; Check if entry exists and can be started
   (t
    (let ((entry (elinit--get-entry-for-id id)))
      (if (not entry)
          (list :status 'error :reason "not found")
        (let ((cmd (elinit-entry-command entry))
              (restart-policy (elinit-entry-restart-policy entry))
              (logging-p (elinit-entry-logging-p entry))
              (stdout-log-file (elinit-entry-stdout-log-file entry))
              (stderr-log-file (elinit-entry-stderr-log-file entry))
              (type (elinit-entry-type entry))
              (working-directory (elinit-entry-working-directory entry))
              (environment (elinit-entry-environment entry))
              (environment-file (elinit-entry-environment-file entry))
              (restart-sec (elinit-entry-restart-sec entry))
              (unit-file-directory (elinit--unit-file-directory-for-id id))
              (user (elinit-entry-user entry))
              (group (elinit-entry-group entry))
              (sandbox-entry (when (elinit--sandbox-requesting-p entry)
                               entry))
              (log-format (elinit-entry-log-format entry))
              (limits-entry (when (elinit--limits-requesting-p entry)
                              entry)))
          (cond
           ;; Masked (highest precedence - only mask blocks manual start)
           ((eq (gethash id elinit--mask-override) 'masked)
            (list :status 'skipped :reason "masked"))
           ;; Non-root cannot change identity
           ((and (or user group) (not (zerop (user-uid))))
            (list :status 'error
                  :reason (format "identity change requires root (user=%s group=%s)"
                                  user group)))
           ;; Root trust gate: identity change requires trusted unit source
           ((and (or user group) (zerop (user-uid))
                 (not (elinit--identity-source-trusted-p id)))
            (list :status 'error
                  :reason (format "unit source not trusted (user=%s group=%s)"
                                  user group)))
           ;; Executable not found
           ((not (executable-find
                  (car (elinit--build-launch-command cmd user group
                                                        sandbox-entry
                                                        limits-entry))))
            (list :status 'error :reason "executable not found"))
           ;; All checks passed - start
           (t
            ;; Clear failed state and oneshot completion on manual start
            (remhash id elinit--failed)
            (remhash id elinit--restart-times)
            (remhash id elinit--oneshot-completed)
            (remhash id elinit--remain-active)
            (remhash id elinit--spawn-failure-reason)
            ;; Clear manually-stopped so restart works again
            (remhash id elinit--manually-stopped)
            ;; Clear conflict suppression and stop conflicting units
            (elinit--conflict-clear-suppression id)
            (elinit--conflict-preflight id elinit--current-plan)
            (let ((proc (elinit--start-process
                         id cmd logging-p type restart-policy nil
                         working-directory environment
                         environment-file restart-sec
                         unit-file-directory
                         user group
                         stdout-log-file stderr-log-file
                         sandbox-entry log-format
                         limits-entry)))
              (if proc
                  (progn
                    ;; Track manual start only on success so reconcile
                    ;; preserves disabled running units.
                    (puthash id t elinit--manually-started)
                    (list :status 'started :reason nil))
                (list :status 'error :reason "failed to start process")))))))))))

;;;; Kill Controls (PT3-N4)

(defun elinit--kill-signal-for-id (id)
  "Return the effective kill signal for unit ID.
Return the configured `:kill-signal' if set, or `SIGTERM' as default."
  (if-let* ((entry (elinit--get-entry-for-id id)))
      (or (elinit-entry-kill-signal entry) 'SIGTERM)
    'SIGTERM))

(defun elinit--kill-mode-for-id (id)
  "Return the effective kill mode for unit ID.
Return the configured `:kill-mode' if set, or `process' as default."
  (if-let* ((entry (elinit--get-entry-for-id id)))
      (or (elinit-entry-kill-mode entry) 'process)
    'process))

(defun elinit--process-descendants (pid)
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
     (elinit--log 'warning "PID %d: descendant discovery failed: %s"
                      pid (error-message-string err))
     nil)))

(defun elinit--kill-with-descendants (proc)
  "Send SIGKILL to PROC and its discovered descendants.
If descendant discovery fails, log a warning and fall back to
process-only SIGKILL.  Descendant PIDs are snapshotted at call time."
  (when (process-live-p proc)
    (let* ((pid (process-id proc))
           (descendants (when pid
                          (elinit--process-descendants pid))))
      ;; Kill main process
      (signal-process proc 'SIGKILL)
      ;; Kill descendants (best effort)
      (dolist (dpid descendants)
        (condition-case nil
            (signal-process dpid 'SIGKILL)
          (error nil)))
      (when descendants
        (elinit--log 'info "%s: killed %d descendant(s) (mixed mode)"
                         (process-name proc) (length descendants))))))

(defun elinit--manual-stop (id)
  "Attempt to manually stop entry with ID.
Returns a plist with :status and :reason.
:status is one of: stopped, skipped, error
:reason explains why (for skipped/error cases).

This function sets manually-stopped to suppress restart,
ensuring consistent behavior between dashboard and CLI.

When the unit has `:exec-stop' commands, they are executed
before the process is signaled.  The configured `:kill-signal'
\(default SIGTERM) is then sent to the main process.  If the
process does not exit within `elinit-shutdown-timeout'
seconds, SIGKILL is sent.  For units with `:kill-mode' `mixed',
SIGKILL is also sent to discovered descendants at escalation time."
  (let ((entry (elinit--get-entry-for-id id)))
    (if (and entry (eq (elinit-entry-type entry) 'target))
        (list :status 'error :reason "cannot stop a target unit")
  (let ((proc (gethash id elinit--processes)))
    (cond
     ;; Active remain-after-exit unit: process exited but latched active
     ((and (not (and proc (process-live-p proc)))
           (gethash id elinit--remain-active))
      (remhash id elinit--remain-active)
      (remhash id elinit--oneshot-completed)
      (puthash id t elinit--manually-stopped)
      (remhash id elinit--manually-started)
      (list :status 'stopped :reason nil))
     ((not proc)
      (list :status 'skipped :reason "not running"))
     ((not (process-live-p proc))
      (list :status 'skipped :reason "not running"))
     (t
      ;; Mark as manually stopped so sentinel doesn't restart it
      (puthash id t elinit--manually-stopped)
      ;; Clear manually-started so reconcile can treat it normally
      (remhash id elinit--manually-started)
      ;; Execute exec-stop commands if configured
      (elinit--run-exec-stop-for-id id)
      ;; Send configured kill-signal (default SIGTERM)
      (let ((kill-signal (elinit--kill-signal-for-id id))
            (kill-mode (elinit--kill-mode-for-id id)))
        (when (process-live-p proc)
          (signal-process proc kill-signal)
          ;; Set up SIGKILL escalation timer
          (run-at-time elinit-shutdown-timeout nil
                       (lambda ()
                         (when (process-live-p proc)
                           (if (eq kill-mode 'mixed)
                               (elinit--kill-with-descendants proc)
                             (signal-process proc 'SIGKILL)))))))
      (list :status 'stopped :reason nil)))))))

(defun elinit--manual-kill (id &optional signal)
  "Attempt to send SIGNAL to running entry with ID.
Returns a plist with :status and :reason.
:status is one of: signaled, skipped, error
:reason explains why (for skipped/error cases).

SIGNAL defaults to the unit's configured `:kill-signal' or
`SIGTERM'.  Unlike `elinit--manual-stop', this does not mark
the entry as manually stopped, so restart policy remains in
effect."
  (let* ((sig (or signal (elinit--kill-signal-for-id id)))
         (proc (gethash id elinit--processes)))
    (cond
     ((not proc)
      (list :status 'skipped :reason "not running"))
     ((not (process-live-p proc))
      (list :status 'skipped :reason "not running"))
     (t
      (signal-process proc sig)
      (list :status 'signaled :reason nil)))))

(defun elinit--reset-failed (id)
  "Reset the failed state for entry ID.
Returns a plist with :status and :reason.
:status is one of: reset, skipped
Clears crash-loop tracking and failed oneshot completion results
so the entry can be restarted."
  (if (not (gethash id elinit--failed))
      (list :status 'skipped :reason "not failed")
    (remhash id elinit--failed)
    (remhash id elinit--restart-times)
    (remhash id elinit--spawn-failure-reason)
    ;; Clear failed oneshot completion (non-zero or signal exit).
    (let ((oneshot-exit (gethash id elinit--oneshot-completed)))
      (when (and oneshot-exit (not (eql oneshot-exit 0)))
        (remhash id elinit--oneshot-completed)))
    (list :status 'reset :reason nil)))

(defun elinit--dag-force-complete ()
  "Force startup completion due to timeout.
Mark all unstarted entries as timed out and invoke the callback."
  (elinit--log 'warning "startup timed out, forcing completion")
  ;; Mark all unstarted entries
  (maphash (lambda (id _entry)
             (unless (gethash id elinit--dag-started)
               (puthash id t elinit--dag-started)
               (elinit--transition-state id 'startup-timeout)))
           elinit--dag-entries)
  ;; Clear blocking oneshots
  (clrhash elinit--dag-blocking)
  ;; Cancel delay timers
  (maphash (lambda (_id timer)
             (when (timerp timer)
               (cancel-timer timer)))
           elinit--dag-delay-timers)
  (clrhash elinit--dag-delay-timers)
  ;; Force-resolve any converging targets as degraded
  (when (and elinit--target-converging
             (> (hash-table-count elinit--target-converging) 0))
    (maphash (lambda (id _)
               (puthash id 'degraded elinit--target-convergence)
               (puthash id '("startup timeout")
                        elinit--target-convergence-reasons)
               (elinit--log 'warning
                                "target %s degraded: startup timeout" id))
             elinit--target-converging)
    (clrhash elinit--target-converging))
  ;; Invoke callback
  (when elinit--dag-complete-callback
    (let ((callback elinit--dag-complete-callback))
      (setq elinit--dag-complete-callback nil)
      (funcall callback))))

(defun elinit--start-entries-async (entries callback
                                       &optional target-members)
  "Start ENTRIES asynchronously.  Call CALLBACK when complete.
Optional TARGET-MEMBERS is a hash of target ID to member plists
for convergence tracking."
  (if (null entries)
      (funcall callback)
    ;; Initialize DAG scheduler
    (elinit--dag-init entries)
    (elinit--target-init-convergence (or target-members
                                             (make-hash-table :test 'equal)))
    (setq elinit--dag-complete-callback callback)
    ;; Set up startup timeout if configured
    (when elinit-startup-timeout
      (setq elinit--dag-timeout-timer
            (run-at-time elinit-startup-timeout nil
                         #'elinit--dag-force-complete)))
    ;; Initialize pending starts queue for max-concurrent-starts
    (setq elinit--dag-pending-starts nil)
    (setq elinit--dag-active-starts 0)
    ;; Start all initially ready entries (in-degree = 0)
    (let ((ready-ids nil))
      (maphash (lambda (id in-deg)
                 (when (= 0 in-deg)
                   (push id ready-ids)))
               elinit--dag-in-degree)
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
          (elinit--dag-try-start-entry id))
        ;; Check if already complete (e.g., all entries were disabled)
        (elinit--dag-check-complete)))))

(defun elinit--dag-start-with-deps (entries callback)
  "Start ENTRIES respecting DAG dependency and oneshot-blocking order.
Call CALLBACK when all entries are ready.

Unlike `elinit--start-entries-async', this does not reinitialize
target convergence state and does not set a startup timeout.  This is
intended for mid-run operations like isolate that need DAG-ordered
starts without disrupting existing runtime state."
  (if (null entries)
      (funcall callback)
    ;; Initialize DAG scheduler (resets DAG variables only)
    (elinit--dag-init entries)
    (setq elinit--dag-complete-callback callback)
    (setq elinit--dag-pending-starts nil)
    (setq elinit--dag-active-starts 0)
    ;; Start all initially ready entries (in-degree = 0)
    (let ((ready-ids nil))
      (maphash (lambda (id in-deg)
                 (when (= 0 in-deg)
                   (push id ready-ids)))
               elinit--dag-in-degree)
      ;; Sort by original order for stable startup
      (setq ready-ids (sort ready-ids
                            (lambda (a b)
                              (< (gethash a elinit--dag-id-to-index 999)
                                 (gethash b elinit--dag-id-to-index 999)))))
      (if (null ready-ids)
          (funcall callback)
        (dolist (id ready-ids)
          (elinit--dag-try-start-entry id))
        (elinit--dag-check-complete)))))

;;; Default target resolution

(defconst elinit--target-alias-map
  '(("runlevel0.target" . "poweroff.target")
    ("runlevel1.target" . "rescue.target")
    ("runlevel2.target" . "multi-user.target")
    ("runlevel3.target" . "multi-user.target")
    ("runlevel4.target" . "multi-user.target")
    ("runlevel5.target" . "graphical.target")
    ("runlevel6.target" . "reboot.target"))
  "Immutable mapping of alias targets to canonical targets.
Uses systemd runlevel mapping semantics.")

(defconst elinit--init-transition-targets
  '("rescue.target" "shutdown.target" "poweroff.target" "reboot.target"
    "runlevel0.target" "runlevel1.target" "runlevel2.target"
    "runlevel3.target" "runlevel4.target" "runlevel5.target"
    "runlevel6.target")
  "Targets not eligible for timer triggering.
Init-transition targets represent system state changes that must
not be triggered by the timer subsystem.")

(defun elinit--resolve-target-alias (target-id)
  "Resolve TARGET-ID through the alias map.
Return the canonical target ID if TARGET-ID is an alias, or
TARGET-ID itself if it is not an alias."
  (or (cdr (assoc target-id elinit--target-alias-map))
      target-id))

(defun elinit--target-alias-p (target-id)
  "Return non-nil if TARGET-ID is a known alias target."
  (assoc target-id elinit--target-alias-map))

(defun elinit--resolve-default-target-link ()
  "Return the effective default-target-link value."
  (or elinit--default-target-link-override
      elinit-default-target-link))

(defun elinit--resolve-target-runtime-id (target-id)
  "Resolve TARGET-ID for target convergence and status lookups.
Resolve `default.target' via `elinit--resolve-default-target-link',
then resolve runlevel aliases to canonical target IDs."
  (elinit--resolve-target-alias
   (if (equal target-id "default.target")
       (elinit--resolve-default-target-link)
     target-id)))

(defun elinit--resolve-startup-root (valid-id-set)
  "Resolve the startup root target from configuration.
VALID-ID-SET is a hash of valid entry IDs.
Signal `user-error' if resolved target does not exist or is not a target."
  (let* ((root elinit-default-target)
         (resolved (if (equal root "default.target")
                       (elinit--resolve-default-target-link)
                     root))
         ;; Resolve alias targets (e.g., runlevelN.target -> canonical)
         (resolved (elinit--resolve-target-alias resolved)))
    (when (equal resolved "default.target")
      (user-error "Elinit: default-target-link must not be \"default.target\" (circular alias)"))
    (unless (string-suffix-p ".target" resolved)
      (user-error "Elinit: startup root `%s' is not a target (must end in .target)"
                  resolved))
    (unless (gethash resolved valid-id-set)
      (user-error "Elinit: startup root target `%s' does not exist"
                  resolved))
    resolved))

;;;###autoload
(defun elinit-start ()
  "Start all programs defined in unit files.
Uses async DAG scheduler with global topological ordering.

Ready semantics (when dependents are unblocked):
- Simple process: spawned (process started)
- Oneshot: exited (success or failure) or timed out
- Disabled entry: immediately ready
- Start failure: immediately ready (dependents proceed)"
  (interactive)
  (elinit--maybe-build-libexec-helpers)
  (setq elinit--shutting-down nil)
  ;; Cancel any existing timers before clearing (prevents orphaned timers)
  (dolist (timer elinit--timers)
    (when (timerp timer)
      (cancel-timer timer)))
  (setq elinit--timers nil)
  (maphash (lambda (_id timer)
             (when (timerp timer)
               (cancel-timer timer)))
           elinit--restart-timers)
  ;; Stop any existing timer scheduler (if timer module loaded)
  (when (fboundp 'elinit-timer-scheduler-stop)
    (elinit-timer-scheduler-stop))
  ;; Reset runtime state for clean session
  (setq elinit--overrides-loaded nil)
  (clrhash elinit--restart-override)
  (clrhash elinit--enabled-override)
  (clrhash elinit--mask-override)
  (clrhash elinit--manually-stopped)
  (clrhash elinit--manually-started)
  (clrhash elinit--conflict-suppressed)
  (clrhash elinit--failed)
  (clrhash elinit--invalid)
  (when (boundp 'elinit--invalid-timers)
    (clrhash elinit--invalid-timers))
  (clrhash elinit--logging)
  (clrhash elinit--writers)
  (clrhash elinit--stderr-writers)
  (clrhash elinit--stderr-pipes)
  (clrhash elinit--restart-times)
  (clrhash elinit--restart-timers)
  (clrhash elinit--last-exit-info)
  (clrhash elinit--oneshot-completed)
  (clrhash elinit--remain-active)
  (clrhash elinit--start-times)
  (clrhash elinit--ready-times)
  (clrhash elinit--cycle-fallback-ids)
  (clrhash elinit--computed-deps)
  (clrhash elinit--entry-state)
  (clrhash elinit--spawn-failure-reason)
  ;; Reset timer state (if timer module loaded)
  (when (boundp 'elinit--timer-state)
    (clrhash elinit--timer-state))
  (when (boundp 'elinit--timer-state-loaded)
    (setq elinit--timer-state-loaded nil))
  (when (boundp 'elinit--timer-list)
    (setq elinit--timer-list nil))
  (when (boundp 'elinit--scheduler-startup-time)
    (setq elinit--scheduler-startup-time nil))
  (setq elinit--current-plan nil)
  (setq elinit--default-target-link-override nil)
  ;; Load persisted overrides (restores enabled/restart/logging overrides)
  (elinit--load-overrides)
  (elinit--dag-cleanup)

  ;; Seed default maintenance units on disk when missing.
  (when (fboundp 'elinit--ensure-default-maintenance-units)
    (elinit--ensure-default-maintenance-units))
  ;; Refresh programs cache from disk before building plan
  (elinit--refresh-programs)
  ;; Build execution plan (pure, deterministic)
  (let ((plan (elinit--build-plan (elinit--effective-programs))))
    ;; Populate legacy globals from plan for dashboard/other code
    (maphash (lambda (k v) (puthash k v elinit--invalid))
             (elinit-plan-invalid plan))
    (elinit--merge-unit-file-invalid)
    (maphash (lambda (k v) (puthash k v elinit--cycle-fallback-ids))
             (elinit-plan-cycle-fallback-ids plan))
    (maphash (lambda (k v) (puthash k v elinit--computed-deps))
             (elinit-plan-deps plan))
    ;; Resolve startup root and compute activation closure.
    ;; Only entries reachable from the root target are activated.
    ;; Skip when plan is empty (standalone core mode).
    (when (elinit-plan-entries plan)
      (let ((valid-ids (make-hash-table :test 'equal))
            (entries-by-id (make-hash-table :test 'equal)))
        (dolist (entry (elinit-plan-entries plan))
          (puthash (elinit-entry-id entry) t valid-ids)
          (puthash (elinit-entry-id entry) entry entries-by-id))
        (let* ((root (elinit--resolve-startup-root valid-ids))
               (members (elinit--materialize-target-members
                         (elinit-plan-entries plan)))
               (closure (elinit--expand-transaction
                         root entries-by-id members
                         (elinit-plan-order-index plan))))
          (setf (elinit-plan-target-members plan) members)
          (setf (elinit-plan-activation-root plan) root)
          (setf (elinit-plan-activation-closure plan) closure)
          ;; Filter by-target to only closure entries
          (setf (elinit-plan-by-target plan)
                (cl-remove-if-not
                 (lambda (e)
                   (gethash (elinit-entry-id e) closure))
                 (elinit-plan-by-target plan))))))
    ;; Validate timers only when timer subsystem is active (and timer module loaded)
    (when (and (fboundp 'elinit-timer-subsystem-active-p)
               (elinit-timer-subsystem-active-p)
               (fboundp 'elinit-timer-build-list))
      (elinit-timer-build-list plan))
    ;; Expose the active plan for dashboard/CLI status and closure-aware views.
    (setq elinit--current-plan plan)
    ;; Initialize all entry states to pending via FSM
    (dolist (entry (elinit-plan-entries plan))
      (elinit--transition-state (car entry) 'pending))
    ;; Start entries asynchronously using plan's globally sorted data
    (elinit--start-entries-async
     (elinit-plan-by-target plan)
     (lambda ()
       (elinit--dag-cleanup)
       (elinit--log 'info "startup complete")
       (when (and (not elinit--shutting-down)
                  (fboundp 'elinit-timer-scheduler-start))
         (elinit-timer-scheduler-start)))
     (elinit-plan-target-members plan))))

;;;###autoload
(defun elinit-stop-now ()
  "Stop all managed processes immediately with SIGKILL.
This is a synchronous function suitable for `kill-emacs-hook'.
Unlike `elinit-stop', it sends SIGKILL immediately and waits
briefly for processes to terminate, ensuring a clean exit."
  (interactive)
  (setq elinit--shutting-down t)
  ;; Stop timer scheduler (if timer module loaded)
  (when (fboundp 'elinit-timer-scheduler-stop)
    (elinit-timer-scheduler-stop))
  ;; Cancel all timers
  (dolist (timer elinit--timers)
    (when (timerp timer)
      (cancel-timer timer)))
  (setq elinit--timers nil)
  (maphash (lambda (_id timer)
             (when (timerp timer)
               (cancel-timer timer)))
           elinit--restart-timers)
  (clrhash elinit--restart-timers)
  (elinit--dag-cleanup)
  (elinit--emit-event 'cleanup nil nil)
  ;; Send SIGKILL to all processes immediately
  (maphash (lambda (_name proc)
             (when (process-live-p proc)
               (signal-process proc 'SIGKILL)))
           elinit--processes)
  ;; Brief wait for processes to die (max 0.5s)
  (let ((deadline (+ (float-time) 0.5)))
    (while (and (< (float-time) deadline)
                (cl-some #'process-live-p
                         (hash-table-values elinit--processes)))
      (sleep-for 0.05)))
  ;; Clean up state
  (elinit--stop-all-writers)
  (clrhash elinit--processes)
  (setq elinit--shutdown-complete-flag t
        elinit--shutting-down nil))

;;;###autoload
(defun elinit-stop (&optional callback)
  "Stop all managed processes gracefully (async).
First runs `:exec-stop' command chains for applicable simple units,
then sends each unit's configured `:kill-signal' (default SIGTERM)
to remaining live processes and returns.  A timer handles the
graceful shutdown period: after `elinit-shutdown-timeout' seconds,
any survivors receive SIGKILL.  For units with `:kill-mode' `mixed',
SIGKILL is also sent to discovered descendants at escalation time.
Optional CALLBACK is called with no arguments when shutdown completes.
Check `elinit--shutdown-complete-flag' to poll completion status
if needed.

For `kill-emacs-hook', use `elinit-stop-now' instead."
  (interactive)
  (setq elinit--shutting-down t)
  ;; Stop timer scheduler (if timer module loaded)
  (when (fboundp 'elinit-timer-scheduler-stop)
    (elinit-timer-scheduler-stop))
  ;; Cancel any pending delayed starts
  (dolist (timer elinit--timers)
    (when (timerp timer)
      (cancel-timer timer)))
  (setq elinit--timers nil)
  ;; Cancel any pending restart timers
  (maphash (lambda (_id timer)
             (when (timerp timer)
               (cancel-timer timer)))
           elinit--restart-timers)
  (clrhash elinit--restart-timers)
  ;; Clean up DAG scheduler
  (elinit--dag-cleanup)
  (elinit--emit-event 'cleanup nil nil)
  ;; Run exec-stop for applicable units before signal escalation.
  ;; The shutdown-remaining guard in elinit--handle-shutdown-exit
  ;; prevents premature completion if processes exit during this phase.
  (let ((ids nil))
    (maphash (lambda (id _proc) (push id ids))
             elinit--processes)
    (dolist (id ids)
      (when-let* ((proc (gethash id elinit--processes)))
        (when (process-live-p proc)
          (elinit--run-exec-stop-for-id id)))))
  ;; Cancel any restart timers scheduled by sentinels during exec-stop
  (maphash (lambda (_id timer)
             (when (timerp timer)
               (cancel-timer timer)))
           elinit--restart-timers)
  (clrhash elinit--restart-timers)
  ;; Send per-unit kill-signal (default SIGTERM) to all remaining live processes
  (maphash (lambda (name proc)
             (when (process-live-p proc)
               (signal-process proc (elinit--kill-signal-for-id name))))
           elinit--processes)
  ;; Set up event-driven shutdown (sentinel-driven, no polling)
  (let ((live-count 0))
    (maphash (lambda (_name proc)
               (when (process-live-p proc)
                 (cl-incf live-count)))
             elinit--processes)
    (if (zerop live-count)
        ;; No live processes, cleanup immediately
        (progn
          (elinit--stop-all-writers)
          (clrhash elinit--processes)
          (setq elinit--shutdown-complete-flag t)
          (when callback (funcall callback)))
      ;; Set up event-driven completion via sentinel
      (setq elinit--shutdown-complete-flag nil
            elinit--shutdown-remaining live-count
            elinit--shutdown-callback callback)
      ;; Set up one-shot SIGKILL timeout timer (safety net)
      (setq elinit--shutdown-timer
            (run-at-time
             elinit-shutdown-timeout nil
             (lambda ()
               (setq elinit--shutdown-timer nil)
               ;; SIGKILL any survivors, respecting kill-mode
               (maphash (lambda (name proc)
                          (when (process-live-p proc)
                            (if (eq (elinit--kill-mode-for-id name) 'mixed)
                                (elinit--kill-with-descendants proc)
                              (signal-process proc 'SIGKILL))))
                        elinit--processes)
               ;; Complete shutdown
               (elinit--stop-all-writers)
               (let ((cb elinit--shutdown-callback))
                 (setq elinit--shutdown-callback nil
                       elinit--shutdown-remaining 0)
                 (clrhash elinit--processes)
                 (setq elinit--shutdown-complete-flag t)
                 (when cb (funcall cb)))))))))

;;; Declarative Reconciler (Phase 4)

(defun elinit--compute-actions (plan snapshot)
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
        (plan-entries (elinit-plan-entries plan))
        (plan-ids (mapcar #'car (elinit-plan-entries plan)))
        (process-alive (elinit-snapshot-process-alive snapshot))
        (failed-hash (elinit-snapshot-failed snapshot))
        (oneshot-hash (elinit-snapshot-oneshot-exit snapshot))
        (enabled-override (elinit-snapshot-enabled-override snapshot))
        (mask-override (or (elinit-snapshot-mask-override snapshot)
                           (make-hash-table :test 'equal)))
        (manually-started (or (elinit-snapshot-manually-started snapshot)
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
                   (enabled-p (elinit-entry-enabled-p entry))
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
             (enabled-p (elinit-entry-enabled-p entry))
             (type (elinit-entry-type entry))
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

(defun elinit--apply-actions (actions plan)
  "Apply reconciliation ACTIONS using entry data from PLAN.
Returns a plist with :stopped and :started counts."
  (let ((stopped 0)
        (started 0)
        (plan-entries (elinit-plan-entries plan)))
    (dolist (action actions)
      (let ((op (plist-get action :op))
            (id (plist-get action :id))
            (reason (plist-get action :reason)))
        (pcase op
          ('stop
           (when-let* ((proc (gethash id elinit--processes)))
             (when (process-live-p proc)
               (elinit--log 'info "reconcile: stopping %s entry %s" reason id)
               ;; Mark as manually stopped so sentinel doesn't restart it
               (puthash id t elinit--manually-stopped)
               (kill-process proc)
               (cl-incf stopped))))
          ('start
           (when-let* ((entry (cl-find id plan-entries :key #'car :test #'equal)))
             (let ((cmd (elinit-entry-command entry))
                   (enabled-p (elinit-entry-enabled-p entry))
                   (restart-policy (elinit-entry-restart-policy entry))
                   (logging-p (elinit-entry-logging-p entry))
                   (stdout-log-file (elinit-entry-stdout-log-file entry))
                   (stderr-log-file (elinit-entry-stderr-log-file entry))
                   (type (elinit-entry-type entry))
                   (working-directory (elinit-entry-working-directory entry))
                   (environment (elinit-entry-environment entry))
                   (environment-file (elinit-entry-environment-file entry))
                   (restart-sec (elinit-entry-restart-sec entry))
                   (unit-file-directory (elinit--unit-file-directory-for-id id))
                   (user (elinit-entry-user entry))
                   (group (elinit-entry-group entry))
                   (sandbox-entry (when (elinit--sandbox-requesting-p entry)
                                    entry))
                   (log-format (elinit-entry-log-format entry))
                   (limits-entry (when (elinit--limits-requesting-p entry)
                                   entry)))
               (when (elinit--get-effective-enabled id enabled-p)
                 (let ((args (elinit--build-launch-command cmd user group
                                                              sandbox-entry
                                                              limits-entry)))
                   (if (not (executable-find (car args)))
                       (progn
                         (elinit--log 'warning "reconcile: executable not found for %s: %s"
                                          id (car args))
                         (elinit--emit-event 'process-failed id nil))
                     (elinit--log 'info "reconcile: starting %s entry %s" reason id)
                     ;; Clear conflict suppression and stop conflicting units
                     (elinit--conflict-clear-suppression id)
                     (elinit--conflict-preflight id plan)
                     (let ((proc (elinit--start-process
                                  id cmd logging-p type restart-policy nil
                                  working-directory environment
                                  environment-file restart-sec
                                  unit-file-directory
                                  user group
                                  stdout-log-file stderr-log-file
                                  sandbox-entry log-format
                                  limits-entry)))
                       (if proc
                           (progn
                             (elinit--emit-event 'process-started id (list :type type))
                             ;; Simple processes are immediately ready; oneshots ready on exit
                             (when (eq type 'simple)
                               (elinit--emit-event 'process-ready id (list :type type)))
                             (cl-incf started))
                         (elinit--emit-event 'process-failed id nil)))))))))
          ;; noop and skip actions require no work
          (_ nil))))
    (list :stopped stopped :started started)))

(defun elinit--reconcile ()
  "Reconcile configuration and running processes.
Internal function called by the config-watcher.
Uses declarative reconciliation: build plan, build snapshot, compute
actions, then apply.  The action list can be inspected before apply
by calling `elinit--compute-actions' directly.

Stops processes removed from config or now disabled, starts new processes.
Does not restart changed entries - use dashboard kill/start for that."
  ;; Build plan and snapshot
  (let* ((plan (elinit--build-plan (elinit--effective-programs)))
         (snapshot (elinit--build-snapshot))
         ;; Compute actions (pure)
         (actions (elinit--compute-actions plan snapshot))
         ;; Apply actions
         (result (elinit--apply-actions actions plan))
         (stopped (plist-get result :stopped))
         (started (plist-get result :started)))
    ;; Populate legacy globals from plan for dashboard
    (clrhash elinit--invalid)
    (when (boundp 'elinit--invalid-timers)
      (clrhash elinit--invalid-timers))
    (maphash (lambda (k v) (puthash k v elinit--invalid))
             (elinit-plan-invalid plan))
    (elinit--merge-unit-file-invalid)
    ;; Validate timers only when timer subsystem is active (and timer module loaded)
    (when (and (fboundp 'elinit-timer-subsystem-active-p)
               (elinit-timer-subsystem-active-p)
               (fboundp 'elinit-timer-build-list))
      (elinit-timer-build-list plan))
    (elinit--maybe-refresh-dashboard)
    (message "Elinit reconcile: stopped %d, started %d" stopped started)))

(defun elinit--reload-find-entry (id)
  "Find and parse entry for ID from the cached program list.
Unlike `elinit--get-entry-for-id', this does not skip entries
present in `elinit--invalid'.  This allows reload to pick up
entries whose config has been fixed since the last plan build.
Return the parsed entry, the symbol `parse-error' if the entry
exists but cannot be parsed, or nil if ID is not found."

  (let ((idx 0))
    (cl-loop for entry in (elinit--effective-programs)
             for entry-id = (elinit--extract-id entry idx)
             do (cl-incf idx)
             when (string= entry-id id)
             return (condition-case nil
                        (elinit--parse-entry entry)
                      (error 'parse-error)))))

(defun elinit--reload-unit (id)
  "Reload a single unit ID from current config.
Re-reads the unit definition from unit files by calling
`elinit--reload-find-entry', then applies the change:

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
   ((eq (gethash id elinit--mask-override) 'masked)
    (list :id id :action "skipped (masked)"))
   ;; Look up entry fresh from effective programs (disk)
   (t
    (let ((entry (elinit--reload-find-entry id)))
      (cond
       ((null entry)
        (list :id id :action "error: not found"))
       ((eq entry 'parse-error)
        (list :id id :action "error: invalid config"))
       (t
        ;; Entry found and parseable - clear stale invalid state
        (remhash id elinit--invalid)
        (let* ((proc (gethash id elinit--processes))
               (running (and proc (process-live-p proc)))
               (type (elinit-entry-type entry)))
          (cond
           ;; Running simple with exec-reload: run reload commands,
           ;; keep process running.
           ((and running (eq type 'simple)
                 (elinit-entry-exec-reload entry))
            (let* ((exec-reload (elinit-entry-exec-reload entry))
                   (working-directory (elinit-entry-working-directory entry))
                   (environment (elinit-entry-environment entry))
                   (environment-file (elinit-entry-environment-file entry))
                   (logging-p (elinit-entry-logging-p entry))
                   (unit-file-directory (elinit--unit-file-directory-for-id id))
                   (dir (if working-directory
                            (condition-case err
                                (elinit--resolve-working-directory
                                 working-directory unit-file-directory)
                              (error
                               (elinit--log
                                'warning
                                "%s: cannot resolve working directory for exec-reload: %s"
                                id (error-message-string err))
                               nil))
                          default-directory))
                   (env (when dir
                          (if (or environment-file environment)
                              (condition-case err
                                  (elinit--build-process-environment
                                   environment-file environment
                                   unit-file-directory)
                                (error
                                 (elinit--log
                                  'warning
                                  "%s: cannot resolve environment for exec-reload: %s"
                                  id (error-message-string err))
                                 nil))
                            process-environment))))
              (if (not (and dir env))
                  (list :id id
                        :action "error: cannot resolve exec-reload context")
                (let* ((logging (elinit--get-effective-logging id logging-p))
                       (log-file (when logging
                                   (elinit--log-file id))))
                  (if (elinit--exec-command-chain
                       exec-reload id dir env log-file
                       elinit-shutdown-timeout)
                      (list :id id :action "reloaded")
                    (list :id id
                          :action "error: reload command failed"))))))
           ;; Running simple without exec-reload: stop + start with
           ;; new definition.  Bypass elinit--manual-start to avoid
           ;; disabled-policy refusal; reload's contract is config
           ;; hot-swap, not enable/disable enforcement.
           ((and running (eq type 'simple))
            (elinit--manual-stop id)
            (remhash id elinit--failed)
            (remhash id elinit--restart-times)
            (remhash id elinit--manually-stopped)
            (let* ((cmd (elinit-entry-command entry))
                   (logging-p (elinit-entry-logging-p entry))
                   (stdout-log-file (elinit-entry-stdout-log-file entry))
                   (stderr-log-file (elinit-entry-stderr-log-file entry))
                   (restart-policy (elinit-entry-restart-policy entry))
                   (working-directory (elinit-entry-working-directory entry))
                   (environment (elinit-entry-environment entry))
                   (environment-file (elinit-entry-environment-file entry))
                   (restart-sec (elinit-entry-restart-sec entry))
                   (unit-file-directory (elinit--unit-file-directory-for-id id))
                   (user (elinit-entry-user entry))
                   (group (elinit-entry-group entry))
                   (sandbox-entry (when (elinit--sandbox-requesting-p entry)
                                    entry))
                   (log-format (elinit-entry-log-format entry))
                   (limits-entry (when (elinit--limits-requesting-p entry)
                                   entry))
                   (exe (car (elinit--build-launch-command cmd user group
                                                              sandbox-entry
                                                              limits-entry))))
              (if (not (executable-find exe))
                  (list :id id :action "error: executable not found")
                (let ((new-proc (elinit--start-process
                                 id cmd logging-p type restart-policy nil
                                 working-directory environment
                                 environment-file restart-sec
                                 unit-file-directory
                                 user group
                                 stdout-log-file stderr-log-file
                                 sandbox-entry log-format
                                 limits-entry)))
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
            (remhash id elinit--failed)
            (remhash id elinit--restart-times)
            (remhash id elinit--oneshot-completed)
            (remhash id elinit--remain-active)
            (list :id id :action "updated"))))))))))

;;;###autoload
(defun elinit-daemon-reload ()
  "Reload unit definitions from disk into memory without affecting runtime.
Re-reads unit files from authority roots, rebuilds the plan, and stores
the result in `elinit--current-plan'.

Does NOT start, stop, or restart anything.  Runtime state is untouched.
After daemon-reload, the next `start' or `reload' operates on the
refreshed plan.

Returns a plist with :entries and :invalid counts."
  (interactive)

  ;; Seed default maintenance units on disk when missing.
  (when (fboundp 'elinit--ensure-default-maintenance-units)
    (elinit--ensure-default-maintenance-units))
  ;; Refresh programs cache from disk
  (elinit--refresh-programs)
  (let ((plan (elinit--build-plan (elinit--effective-programs))))
    ;; Populate legacy globals from plan for dashboard/CLI visibility
    (clrhash elinit--invalid)
    (when (boundp 'elinit--invalid-timers)
      (clrhash elinit--invalid-timers))
    (maphash (lambda (k v) (puthash k v elinit--invalid))
             (elinit-plan-invalid plan))
    (elinit--merge-unit-file-invalid)
    ;; Resolve target metadata (activation-root, target-members, closure)
    ;; so dashboard target filter, header root, and drill-down work after
    ;; reload without requiring a full restart.
    (when (elinit-plan-entries plan)
      (let ((valid-ids (make-hash-table :test 'equal))
            (entries-by-id (make-hash-table :test 'equal)))
        (dolist (entry (elinit-plan-entries plan))
          (puthash (elinit-entry-id entry) t valid-ids)
          (puthash (elinit-entry-id entry) entry entries-by-id))
        (condition-case nil
            (let* ((root (elinit--resolve-startup-root valid-ids))
                   (members (elinit--materialize-target-members
                             (elinit-plan-entries plan)))
                   (closure (elinit--expand-transaction
                             root entries-by-id members
                             (elinit-plan-order-index plan))))
              (setf (elinit-plan-target-members plan) members)
              (setf (elinit-plan-activation-root plan) root)
              (setf (elinit-plan-activation-closure plan) closure)
              ;; Update runtime global so drill-down reflects reloaded state
              (setq elinit--target-members members))
          (user-error nil))))
    ;; Store plan for later use (after metadata population)
    (setq elinit--current-plan plan)
    ;; Validate timers when subsystem is active
    (when (and (fboundp 'elinit-timer-subsystem-active-p)
               (elinit-timer-subsystem-active-p)
               (fboundp 'elinit-timer-build-list))
      (elinit-timer-build-list plan))
    (elinit--maybe-refresh-dashboard)
    (let ((n-entries (length (elinit-plan-entries plan)))
          (n-invalid (hash-table-count elinit--invalid)))
      (message "Elinit daemon-reload: %d entries, %d invalid"
               n-entries n-invalid)
      (list :entries n-entries :invalid n-invalid))))


;;; File Watch

(defun elinit--config-watch-file ()
  "Return the file to watch for config modification."
  (cond
   ((stringp elinit-watch-config) elinit-watch-config)
   ((and elinit-watch-config user-init-file) user-init-file)
   (t nil)))

(defun elinit--config-watch-callback (_event)
  "Handle config file modification.  Trigger reconcile after debounce."
  ;; Simple debounce: cancel pending reconcile, schedule new one
  (when (timerp (get 'elinit--config-watch-callback 'timer))
    (cancel-timer (get 'elinit--config-watch-callback 'timer)))
  (put 'elinit--config-watch-callback 'timer
       (run-at-time 1 nil
                    (lambda ()
                      (elinit--log 'info "config file changed, reconciling...")
                      (elinit--reconcile)))))

(defun elinit--start-config-watch ()
  "Start watching config file if configured."
  (when-let* ((file (elinit--config-watch-file)))
    (when (file-exists-p file)
      (require 'filenotify)
      (setq elinit--config-watch-descriptor
            (file-notify-add-watch file '(change)
                                   #'elinit--config-watch-callback))
      (elinit--log 'info "watching %s for changes" file))))

(defun elinit--stop-config-watch ()
  "Stop watching config file and cancel any pending debounce timer."
  ;; Cancel debounce timer if pending
  (when-let* ((timer (get 'elinit--config-watch-callback 'timer)))
    (when (timerp timer)
      (cancel-timer timer))
    (put 'elinit--config-watch-callback 'timer nil))
  ;; Remove file watch
  (when elinit--config-watch-descriptor
    (require 'filenotify)
    (file-notify-rm-watch elinit--config-watch-descriptor)
    (setq elinit--config-watch-descriptor nil)))

;;; Global Minor Mode

;;;###autoload
(define-minor-mode elinit-mode
  "Global minor mode for service management.

When enabled, starts all configured processes via `elinit-start'.
When disabled, stops all managed processes via `elinit-stop'.

This mode is intended for use in your init file to manage session
processes, particularly when using Emacs as a window manager (EXWM).

Services are defined as unit files in `elinit-unit-authority-path'
\\=(default: ~/.config/elinit.el/).  Each file contains a plist:

  ;; ~/.config/elinit.el/nm-applet.el
  \\=(:id \"nm-applet\"
   :command \"nm-applet\"
   :type simple)

Enable in your init file:

  (require \\='elinit)
  (elinit-mode 1)

Or with `use-package':

  (use-package elinit
    :config
    (elinit-mode 1))

Process types:
- simple: Long-running daemons, restarted on crash per :restart policy
- oneshot: Run-once scripts, exit is expected
- target: Grouping units for dependency ordering

Use `M-x elinit' to open the dashboard for monitoring and control.
Use `M-x elinit-verify' to check config without starting processes."
  :global t
  :group 'elinit
  :lighter " Sup"
  (if elinit-mode
      (progn
        (elinit-start)
        (elinit--start-config-watch))
    (elinit--stop-config-watch)
    (elinit-stop)))

(provide 'elinit-core)

;;; elinit-core.el ends here
