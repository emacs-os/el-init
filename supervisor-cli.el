;;; supervisor-cli.el --- Supervisor CLI interface -*- lexical-binding: t -*-

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

;; CLI interface for supervisor.el.
;; Run M-x supervisor-handbook for full documentation.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'supervisor-core)

;; Forward declarations for timer subsystem (defined in supervisor-timer.el)
(declare-function supervisor-timer-subsystem-active-p "supervisor-timer" ())
(declare-function supervisor-timer-build-list "supervisor-timer" (plan))
(declare-function supervisor-timer-id "supervisor-timer" (timer))
(declare-function supervisor-timer-target "supervisor-timer" (timer))
(declare-function supervisor-timer-enabled "supervisor-timer" (timer))
(declare-function supervisor-timer-persistent "supervisor-timer" (timer))

;; Forward declarations for unit-file module (optional)
(declare-function supervisor--unit-file-path "supervisor-units" (id))
(declare-function supervisor--unit-file-scaffold "supervisor-units" (id))
(declare-function supervisor--authority-root-for-id "supervisor-units" (id))
(declare-function supervisor--authority-tier-for-id "supervisor-units" (id))

;; Forward declarations for timer state variables (defined in supervisor-timer.el)
(defvar supervisor--invalid-timers)
(defvar supervisor--timer-list)
(defvar supervisor--timer-state)


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

(defconst supervisor-cli-exit-not-active 3
  "Exit code for `is-active' when unit exists but is not active.
Matches systemctl LSB convention.")

(defconst supervisor-cli-exit-no-such-unit 4
  "Exit code for `is-*' predicates when the unit does not exist.
Matches systemctl convention.")

(defconst supervisor-cli-exit-validation-failed 4
  "Exit code when config validation fails.")

(defconst supervisor-cli-exit-server-unavailable 69
  "Exit code when Emacs server is unavailable.
Uses sysexits.h EX_UNAVAILABLE to avoid collision with systemctl codes.")

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

(defun supervisor--cli-policy-batch (ids mutator-fn verb json-p)
  "Apply MUTATOR-FN to each ID in IDS and return a CLI result.
MUTATOR-FN is a core policy function that accepts an ID and returns
a plist with `:status' and `:message'.  VERB is the past-tense action
word for human output (e.g., \"Enabled\").  JSON-P enables JSON output.
Saves overrides once after all IDs are processed."
  (let ((applied nil) (skipped nil) (errors nil))
    (dolist (id ids)
      (let ((result (funcall mutator-fn id)))
        (pcase (plist-get result :status)
          ('applied (push id applied))
          ('skipped
           (push (cons id (plist-get result :message)) skipped))
          ('error
           (push (cons id (plist-get result :message)) errors)))))
    (supervisor--save-overrides)
    (setq applied (nreverse applied)
          skipped (nreverse skipped)
          errors (nreverse errors))
    (let ((fmt (if json-p 'json 'human))
          (msg (concat
                (when applied
                  (format "%s: %s\n" verb
                          (mapconcat #'identity applied ", ")))
                (when skipped
                  (mapconcat
                   (lambda (pair)
                     (format "Skipped: %s (%s)\n" (car pair) (cdr pair)))
                   skipped ""))
                (when errors
                  (mapconcat
                   (lambda (pair)
                     (format "Error: %s (%s)\n" (car pair) (cdr pair)))
                   errors "")))))
      (if errors
          (supervisor--cli-make-result
           supervisor-cli-exit-failure fmt
           (if json-p
               (json-encode
                `((applied . ,(or applied []))
                  (skipped
                   . ,(mapcar (lambda (p)
                                `((id . ,(car p)) (reason . ,(cdr p))))
                              skipped))
                  (errors
                   . ,(mapcar (lambda (p)
                                `((id . ,(car p)) (reason . ,(cdr p))))
                              errors))))
             msg))
        (supervisor--cli-success
         (if json-p
             (json-encode
              `((applied . ,(or applied []))
                (skipped
                 . ,(mapcar (lambda (p)
                              `((id . ,(car p)) (reason . ,(cdr p))))
                            skipped))))
           msg)
         fmt)))))

(defun supervisor--cli-split-at-separator (args)
  "Split ARGS at \"--\" separator into (before-list . after-list).
The \"--\" itself is not included in either list.
If no \"--\" is present, returns (ARGS . nil)."
  (let ((pos (cl-position "--" args :test #'equal)))
    (if pos
        (cons (cl-subseq args 0 pos)
              (cl-subseq args (1+ pos)))
      (cons args nil))))

(defun supervisor--cli-parse-option (args option)
  "Parse OPTION and its value from ARGS by position.
OPTION is a string like \"--tail\".
Returns a plist with :value, :missing, :duplicate, :positional.
:value is the option value if present and valid, nil otherwise.
:missing is t if option is present but value is missing or looks like a flag.
:duplicate is t if option appears more than once.
:positional is the remaining args with option and its value removed by position."
  (let ((positional nil)
        (value nil)
        (missing nil)
        (duplicate nil)
        (seen nil)
        (skip-next nil)
        (rest args))
    (while rest
      (let ((arg (car rest)))
        (cond
         (skip-next
          (setq skip-next nil))
         ((equal arg option)
          (if seen
              (setq duplicate t)
            (setq seen t))
          (let ((next (cadr rest)))
            (if (or (null next) (string-prefix-p "-" next))
                (setq missing t)
              (setq value next)
              (setq skip-next t))))
         (t
          (push arg positional))))
      (setq rest (cdr rest)))
    (list :value value :missing missing :duplicate duplicate
          :positional (nreverse positional))))

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
         (restart-cfg (supervisor-entry-restart-policy entry))
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
         (ready-time (gethash id supervisor--ready-times))
         ;; Telemetry
         (actual-pid (or pid pid-global))
         (uptime (supervisor--telemetry-uptime id))
         (restart-count (supervisor--telemetry-restart-count id))
         (last-exit (supervisor--telemetry-last-exit-info id snapshot))
         (next-eta (supervisor--telemetry-next-restart-eta id))
         (metrics (when actual-pid
                    (supervisor--telemetry-process-metrics actual-pid))))
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
      (pid . ,actual-pid)
      (start-time . ,start-time)
      (ready-time . ,ready-time)
      (duration . ,(when (and start-time ready-time)
                     (- ready-time start-time)))
      (uptime . ,uptime)
      (restart-count . ,restart-count)
      (last-exit . ,last-exit)
      (next-restart-eta . ,next-eta)
      (metrics . ,metrics)
      (unit-file . ,(when (fboundp 'supervisor--unit-file-path)
                      (supervisor--unit-file-path id)))
      (working-directory . ,(supervisor-entry-working-directory entry))
      (environment . ,(supervisor-entry-environment entry))
      (environment-file . ,(supervisor-entry-environment-file entry))
      (exec-stop . ,(supervisor-entry-exec-stop entry))
      (exec-reload . ,(supervisor-entry-exec-reload entry))
      (restart-sec . ,(supervisor-entry-restart-sec entry))
      (description . ,(supervisor-entry-description entry))
      (documentation . ,(supervisor-entry-documentation entry)))))

(defun supervisor--cli-all-entries-info (&optional snapshot)
  "Build info alists for all valid entries, using optional SNAPSHOT.
Returns (entries invalid) where entries is list of alists and
invalid is list of (id . reason) pairs, sorted by ID for determinism.
Includes both plan-level and unit-file-level invalid entries."
  (let* ((plan (supervisor--build-plan (supervisor--effective-programs)))
         (entries (supervisor-plan-entries plan))
         (combined-invalid (make-hash-table :test 'equal))
         (invalid-list nil)
         (entry-infos nil))
    ;; Merge plan invalids and unit-file invalids into combined hash
    (maphash (lambda (k v) (puthash k v combined-invalid))
             (supervisor-plan-invalid plan))
    (when (boundp 'supervisor--unit-file-invalid)
      (maphash (lambda (k v) (puthash k v combined-invalid))
               (symbol-value 'supervisor--unit-file-invalid)))
    ;; Collect invalid entries
    (maphash (lambda (id reason)
               (push `((id . ,id) (reason . ,reason)) invalid-list))
             combined-invalid)
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

(defun supervisor--cli-format-duration (seconds)
  "Format SECONDS as a human-readable duration string."
  (let ((s (round seconds)))
    (cond
     ((< s 60) (format "%ds" s))
     ((< s 3600) (format "%dm%ds" (/ s 60) (% s 60)))
     ((< s 86400) (format "%dh%dm" (/ s 3600) (% (/ s 60) 60)))
     (t (format "%dd%dh" (/ s 86400) (% (/ s 3600) 24))))))

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
    (format "%-16s %-8s %-8s %-8s %-10s %-11s %-5s %-7s %s\n"
            (truncate-string-to-width (or id "-") 16)
            (or (symbol-name type) "-")
            (if stage (symbol-name stage) "-")
            (supervisor--cli-format-bool enabled)
            (or status "-")
            (if (eq type 'oneshot) "n/a"
              (if restart (symbol-name restart) "-"))
            (supervisor--cli-format-bool logging)
            (if pid (number-to-string pid) "-")
            (or reason "-"))))

(defun supervisor--cli-format-invalid-line (info)
  "Format invalid entry INFO as status table row."
  (let ((id (alist-get 'id info))
        (reason (alist-get 'reason info)))
    (format "%-16s %-8s %-8s %-8s %-10s %-11s %-5s %-7s %s\n"
            (truncate-string-to-width (or id "-") 16)
            "-" "-" "-" "invalid" "-" "-" "-"
            (or reason "-"))))

(defun supervisor--cli-status-human (entries invalid)
  "Format ENTRIES and INVALID as human-readable status table."
  (let ((header (format "%-16s %-8s %-8s %-8s %-10s %-11s %-5s %-7s %s\n"
                        "ID" "TYPE" "STAGE" "ENABLED" "STATUS" "RESTART" "LOG" "PID" "REASON"))
        (sep (make-string 103 ?-)))
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
     (let ((desc (alist-get 'description info)))
       (when desc (format "Description: %s\n" desc)))
     (format "Type: %s\n" type)
     (format "Stage: %s\n" stage)
     (format "Status: %s%s\n" status (if reason (format " (%s)" reason) ""))
     (format "Enabled: %s%s\n"
             (supervisor--cli-format-bool enabled)
             (if (not (eq enabled enabled-cfg)) " (override)" ""))
     (when (not (eq type 'oneshot))
       (format "Restart: %s%s\n"
               (if restart (symbol-name restart) "-")
               (if (not (eq restart restart-cfg)) " (override)" "")))
     (format "Logging: %s%s\n"
             (supervisor--cli-format-bool logging)
             (if (not (eq logging logging-cfg)) " (override)" ""))
     (format "Delay: %s\n" delay)
     (format "After: %s\n" (if after (mapconcat #'identity after ", ") "none"))
     (format "Requires: %s\n" (if requires (mapconcat #'identity requires ", ") "none"))
     (let ((wd (alist-get 'working-directory info)))
       (when wd (format "Working directory: %s\n" wd)))
     (let ((env (alist-get 'environment info)))
       (when env
         (format "Environment: %s\n"
                 (mapconcat (lambda (pair)
                              (format "%s=%s" (car pair) (cdr pair)))
                            env ", "))))
     (let ((ef (alist-get 'environment-file info)))
       (when ef
         (format "Environment file: %s\n"
                 (mapconcat #'identity ef ", "))))
     (let ((es (alist-get 'exec-stop info)))
       (when es
         (format "Exec stop: %s\n"
                 (mapconcat #'identity es "; "))))
     (let ((er (alist-get 'exec-reload info)))
       (when er
         (format "Exec reload: %s\n"
                 (mapconcat #'identity er "; "))))
     (let ((rs (alist-get 'restart-sec info)))
       (when rs (format "Restart delay: %ss\n" rs)))
     (when pid (format "PID: %d\n" pid))
     (when start-time (format "Start time: %.3f\n" start-time))
     (when ready-time (format "Ready time: %.3f\n" ready-time))
     (when duration (format "Duration: %.3fs\n" duration))
     (let ((uptime (alist-get 'uptime info)))
       (when uptime (format "Uptime: %s\n"
                            (supervisor--cli-format-duration uptime))))
     (let ((rc (alist-get 'restart-count info)))
       (when (and rc (> rc 0))
         (format "Restarts: %d (in window)\n" rc)))
     (let ((exit-info (alist-get 'last-exit info)))
       (when exit-info
         (let ((status (plist-get exit-info :status))
               (code (plist-get exit-info :code))
               (ts (plist-get exit-info :timestamp)))
           (format "Last exit: %s (code %d, %s)\n"
                   status code
                   (if ts
                       (format "%.1fs ago"
                               (- (float-time) ts))
                     "unknown time")))))
     (let ((eta (alist-get 'next-restart-eta info)))
       (when eta
         (let ((remaining (- eta (float-time))))
           (when (> remaining 0)
             (format "Next restart: in %.1fs\n" remaining)))))
     (let ((metrics (alist-get 'metrics info)))
       (when metrics
         (format "Metrics: %s\n"
                 (mapconcat
                  (lambda (pair)
                    (format "%s=%s" (car pair) (cdr pair)))
                  (cl-loop for (k v) on metrics by #'cddr
                           collect (cons (substring (symbol-name k) 1)
                                         (if (floatp v)
                                             (format "%.1f" v)
                                           (format "%s" v))))
                  " "))))
     (let ((uf (alist-get 'unit-file info)))
       (when uf (format "Unit file: %s\n" uf)))
     (let ((docs (alist-get 'documentation info)))
       (when docs
         (format "Documentation: %s\n"
                 (mapconcat #'identity docs ", ")))))))

(defun supervisor--cli-describe-invalid-human (info)
  "Format invalid entry INFO as human-readable detail view."
  (let ((id (alist-get 'id info))
        (reason (alist-get 'reason info)))
    (concat
     (format "ID: %s\n" id)
     (format "Status: invalid\n")
     (format "Reason: %s\n" (or reason "-")))))

;;; CLI JSON Formatters

(defun supervisor--cli-entry-to-json-obj (info)
  "Convert entry INFO alist to JSON-compatible alist."
  `((id . ,(alist-get 'id info))
    (type . ,(symbol-name (alist-get 'type info)))
    (stage . ,(symbol-name (alist-get 'stage info)))
    (enabled . ,(if (alist-get 'enabled info) t :json-false))
    (status . ,(alist-get 'status info))
    (restart . ,(let ((r (alist-get 'restart info)))
                   (if r (symbol-name r) "n/a")))
    (logging . ,(if (alist-get 'logging info) t :json-false))
    (pid . ,(alist-get 'pid info))
    (reason . ,(alist-get 'reason info))
    (delay . ,(alist-get 'delay info))
    (after . ,(or (alist-get 'after info) []))
    (requires . ,(or (alist-get 'requires info) []))
    (start_time . ,(alist-get 'start-time info))
    (ready_time . ,(alist-get 'ready-time info))
    (duration . ,(alist-get 'duration info))
    (unit_file . ,(alist-get 'unit-file info))
    (working_directory . ,(alist-get 'working-directory info))
    (environment . ,(let ((env (alist-get 'environment info)))
                      (if env
                          (mapcar (lambda (pair)
                                    `((key . ,(car pair))
                                      (value . ,(cdr pair))))
                                  env)
                        [])))
    (environment_file . ,(or (alist-get 'environment-file info) []))
    (exec_stop . ,(or (alist-get 'exec-stop info) []))
    (exec_reload . ,(or (alist-get 'exec-reload info) []))
    (restart_sec . ,(alist-get 'restart-sec info))
    (uptime . ,(alist-get 'uptime info))
    (restart_count . ,(or (alist-get 'restart-count info) 0))
    (last_exit . ,(let ((exit-info (alist-get 'last-exit info)))
                    (if exit-info
                        `((status . ,(plist-get exit-info :status))
                          (code . ,(plist-get exit-info :code))
                          (timestamp . ,(plist-get exit-info :timestamp)))
                      nil)))
    (next_restart_eta . ,(alist-get 'next-restart-eta info))
    (metrics . ,(let ((m (alist-get 'metrics info)))
                  (if m
                      (cl-loop for (k v) on m by #'cddr
                               collect (cons (intern
                                              (substring (symbol-name k) 1))
                                             v))
                    nil)))
    (description . ,(alist-get 'description info))
    (documentation . ,(or (alist-get 'documentation info) []))))

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
  "Handle `status [ID...]' command with ARGS.  JSON-P enables JSON output.
With IDs, show detailed status for each unit (valid or invalid).
Print output for found units, invalid detail for invalid units,
and append warnings for truly missing IDs (exit non-zero if any missing).
Without IDs, show overview table (same as `list-units')."
  (let ((unknown (supervisor--cli-has-unknown-flags args)))
    (if unknown
        (supervisor--cli-error supervisor-cli-exit-invalid-args
                               (format "Unknown option: %s" unknown)
                               (if json-p 'json 'human))
      (if args
          ;; Detailed status for specific IDs
          (let* ((snapshot (supervisor--build-snapshot))
                 (result (supervisor--cli-all-entries-info snapshot))
                 (entries (car result))
                 (invalid (cadr result))
                 (valid-out nil)
                 (invalid-out nil)
                 (missing nil))
            (dolist (id args)
              (let ((info (cl-find id entries
                                   :key (lambda (e) (alist-get 'id e))
                                   :test #'equal)))
                (if info
                    (push info valid-out)
                  (let ((inv (cl-find id invalid
                                      :key (lambda (e) (alist-get 'id e))
                                      :test #'equal)))
                    (if inv
                        (push inv invalid-out)
                      (push id missing))))))
            (setq valid-out (nreverse valid-out))
            (setq invalid-out (nreverse invalid-out))
            (setq missing (nreverse missing))
            (let* ((detail
                    (if json-p
                        (json-encode
                         `((entries
                            . ,(supervisor--cli-ensure-array
                                (mapcar #'supervisor--cli-entry-to-json-obj
                                        valid-out)))
                           (invalid
                            . ,(supervisor--cli-ensure-array
                                (mapcar #'supervisor--cli-invalid-to-json-obj
                                        invalid-out)))
                           (not_found
                            . ,(supervisor--cli-ensure-array missing))))
                      (concat
                       (when valid-out
                         (mapconcat #'supervisor--cli-describe-human
                                    valid-out "\n"))
                       (when invalid-out
                         (concat
                          (when valid-out "\n")
                          (mapconcat #'supervisor--cli-describe-invalid-human
                                     invalid-out "\n")))
                       (when missing
                         (concat
                          (when (or valid-out invalid-out) "\n")
                          (mapconcat
                           (lambda (id)
                             (format "Unit %s could not be found.\n" id))
                           missing ""))))))
                   (exitcode (if missing
                                 supervisor-cli-exit-failure
                               supervisor-cli-exit-success)))
              (supervisor--cli-make-result
               exitcode (if json-p 'json 'human) detail)))
        ;; No IDs: overview table (same as list-units)
        (supervisor--cli-cmd-list-units nil json-p)))))

(defun supervisor--cli-cmd-list-units (args json-p)
  "Handle `list-units [ID...]' command with ARGS.  JSON-P enables JSON output.
Show overview status table for all or filtered entries.
When IDs are specified, both valid and invalid rows are filtered."
  (let ((unknown (supervisor--cli-has-unknown-flags args)))
    (if unknown
        (supervisor--cli-error supervisor-cli-exit-invalid-args
                               (format "Unknown option: %s" unknown)
                               (if json-p 'json 'human))
      (let* ((snapshot (supervisor--build-snapshot))
             (result (supervisor--cli-all-entries-info snapshot))
             (entries (car result))
             (invalid (cadr result))
             (entries (if args
                          (cl-remove-if-not
                           (lambda (e) (member (alist-get 'id e) args))
                           entries)
                        entries))
             (invalid (if args
                          (cl-remove-if-not
                           (lambda (e) (member (alist-get 'id e) args))
                           invalid)
                        invalid)))
        (supervisor--cli-success
         (if json-p
             (supervisor--cli-status-json entries invalid)
           (supervisor--cli-status-human entries invalid))
         (if json-p 'json 'human))))))

(defun supervisor--cli-cmd-show (args json-p)
  "Handle `show ID' command with ARGS.  JSON-P enables JSON output.
Show all properties of a single unit."
  (cond
   ((null args)
    (supervisor--cli-error supervisor-cli-exit-invalid-args
                           "show requires an ID argument"
                           (if json-p 'json 'human)))
   ((cdr args)
    (supervisor--cli-error supervisor-cli-exit-invalid-args
                           (format "show takes exactly one ID, got: %s"
                                   (mapconcat #'identity args " "))
                           (if json-p 'json 'human)))
   (t
    (let* ((id (car args))
           (snapshot (supervisor--build-snapshot))
           (result (supervisor--cli-all-entries-info snapshot))
           (entries (car result))
           (invalid (cadr result))
           (info (cl-find id entries
                          :key (lambda (e) (alist-get 'id e))
                          :test #'equal))
           (inv (unless info
                  (cl-find id invalid
                           :key (lambda (e) (alist-get 'id e))
                           :test #'equal))))
      (cond
       (info
        (supervisor--cli-success
         (if json-p
             (json-encode (supervisor--cli-entry-to-json-obj info))
           (supervisor--cli-describe-human info))
         (if json-p 'json 'human)))
       (inv
        (supervisor--cli-success
         (if json-p
             (json-encode (supervisor--cli-invalid-to-json-obj inv))
           (supervisor--cli-describe-invalid-human inv))
         (if json-p 'json 'human)))
       (t
        (supervisor--cli-error supervisor-cli-exit-failure
                               (format "No entry with ID '%s'" id)
                               (if json-p 'json 'human))))))))

(defun supervisor--cli-cmd-verify (args json-p)
  "Handle `verify' command with ARGS.  JSON-P enables JSON output."

  (let ((extra-err (supervisor--cli-reject-extra-args args json-p)))
    (if extra-err extra-err
      (let* ((plan (supervisor--build-plan (supervisor--effective-programs)))
             (service-valid (length (supervisor-plan-entries plan)))
             (combined-invalid (make-hash-table :test 'equal))
             (service-invalid-list nil))
        ;; Merge plan invalids and unit-file invalids
        (maphash (lambda (k v) (puthash k v combined-invalid))
                 (supervisor-plan-invalid plan))
        (when (boundp 'supervisor--unit-file-invalid)
          (maphash (lambda (k v) (puthash k v combined-invalid))
                   (symbol-value 'supervisor--unit-file-invalid)))
        (maphash (lambda (id reason)
                   (push `((id . ,id) (reason . ,reason)) service-invalid-list))
                 combined-invalid)
        ;; Sort invalid list by ID for deterministic output
        (setq service-invalid-list
              (sort service-invalid-list
                    (lambda (a b)
                      (string< (alist-get 'id a) (alist-get 'id b)))))
        ;; Validate timers
        (let* ((timers (supervisor-timer-build-list plan))
               (timer-valid (length timers))
               (timer-invalid-list nil))
          (maphash (lambda (id reason)
                     (push `((id . ,id) (reason . ,reason)) timer-invalid-list))
                   supervisor--invalid-timers)
          (setq timer-invalid-list
                (sort timer-invalid-list
                      (lambda (a b)
                        (string< (alist-get 'id a) (alist-get 'id b)))))
          (let ((service-invalid (length service-invalid-list))
                (timer-invalid (length timer-invalid-list))
                (has-errors (or (> (length service-invalid-list) 0)
                                (> (length timer-invalid-list) 0))))
            (if json-p
                (supervisor--cli-make-result
                 (if has-errors supervisor-cli-exit-validation-failed
                   supervisor-cli-exit-success)
                 'json
                 (json-encode
                  `((services . ((valid . ,service-valid)
                                 (invalid . ,service-invalid)
                                 (errors . ,(supervisor--cli-ensure-array
                                             (mapcar #'supervisor--cli-invalid-to-json-obj
                                                     service-invalid-list)))))
                    (timers . ((valid . ,timer-valid)
                               (invalid . ,timer-invalid)
                               (errors . ,(supervisor--cli-ensure-array
                                           (mapcar #'supervisor--cli-invalid-to-json-obj
                                                   timer-invalid-list))))))))
              (supervisor--cli-make-result
               (if has-errors supervisor-cli-exit-validation-failed
                 supervisor-cli-exit-success)
               'human
               (concat
                (format "Services: %d valid, %d invalid\n" service-valid service-invalid)
                (mapconcat (lambda (e)
                             (format "  INVALID %s: %s\n"
                                     (alist-get 'id e)
                                     (alist-get 'reason e)))
                           service-invalid-list "")
                (when supervisor-timers
                  (concat
                   (format "Timers: %d valid, %d invalid\n" timer-valid timer-invalid)
                   (mapconcat (lambda (e)
                                (format "  INVALID %s: %s\n"
                                        (alist-get 'id e)
                                        (alist-get 'reason e)))
                              timer-invalid-list ""))))))))))))

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
                (skipped nil)
                (errors nil))
            (dolist (id args)
              (let ((result (supervisor--manual-start id)))
                (pcase (plist-get result :status)
                  ('started (push id started))
                  ('skipped (push (cons id (plist-get result :reason)) skipped))
                  ('error (push (cons id (plist-get result :reason)) errors)))))
            ;; Reverse once to preserve order (push builds in reverse)
            (setq started (nreverse started)
                  skipped (nreverse skipped)
                  errors (nreverse errors))
            (let ((msg (concat
                        (when started
                          (format "Started: %s\n"
                                  (mapconcat #'identity started ", ")))
                        (when skipped
                          (mapconcat (lambda (pair)
                                       (format "Skipped: %s (%s)\n" (car pair) (cdr pair)))
                                     skipped ""))
                        (when errors
                          (mapconcat (lambda (pair)
                                       (format "Error: %s (%s)\n" (car pair) (cdr pair)))
                                     errors "")))))
              (if errors
                  (supervisor--cli-make-result supervisor-cli-exit-failure
                                               (if json-p 'json 'human)
                                               (if json-p
                                                   (json-encode
                                                    `((started . ,started)
                                                      (skipped . ,(mapcar (lambda (p)
                                                                            `((id . ,(car p))
                                                                              (reason . ,(cdr p))))
                                                                          skipped))
                                                      (errors . ,(mapcar (lambda (p)
                                                                           `((id . ,(car p))
                                                                             (reason . ,(cdr p))))
                                                                         errors))))
                                                 msg))
                (supervisor--cli-success
                 (if json-p
                     (json-encode
                      `((started . ,started)
                        (skipped . ,(mapcar (lambda (p)
                                              `((id . ,(car p))
                                                (reason . ,(cdr p))))
                                            skipped))))
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

(defun supervisor--cli-stop-all-gracefully ()
  "Stop all supervised processes and wait for completion.
Returns non-nil when graceful shutdown completed before timeout.
This is used by CLI `stop' and `restart' for systemctl-like stop behavior."
  (let ((deadline (+ (float-time) supervisor-shutdown-timeout 1.0)))
    (setq supervisor--shutdown-complete-flag nil)
    (supervisor-stop)
    (while (and (not supervisor--shutdown-complete-flag)
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    supervisor--shutdown-complete-flag))

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
                (skipped nil))
            (dolist (id args)
              (let ((result (supervisor--manual-stop id)))
                (pcase (plist-get result :status)
                  ('stopped (push id stopped))
                  ('skipped (push (cons id (plist-get result :reason)) skipped)))))
            ;; Reverse once to preserve order
            (setq stopped (nreverse stopped)
                  skipped (nreverse skipped))
            (let ((msg (concat
                        (when stopped
                          (format "Stopped: %s\n"
                                  (mapconcat #'identity stopped ", ")))
                        (when skipped
                          (mapconcat (lambda (pair)
                                       (format "Skipped: %s (%s)\n" (car pair) (cdr pair)))
                                     skipped "")))))
              (supervisor--cli-success
               (if json-p
                   (json-encode
                    `((stopped . ,stopped)
                      (skipped . ,(mapcar (lambda (p)
                                            `((id . ,(car p))
                                              (reason . ,(cdr p))))
                                          skipped))))
                 msg)
               (if json-p 'json 'human)))))
         (t
          ;; Stop all via graceful shutdown, with forced fallback on timeout.
          (unless (supervisor--cli-stop-all-gracefully)
            (supervisor-stop-now))
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
                (skipped nil)
                (errors nil))
            (dolist (id args)
              ;; Stop first if running (ignore result, we just want it stopped)
              (supervisor--manual-stop id)
              ;; Then start
              (let ((result (supervisor--manual-start id)))
                (pcase (plist-get result :status)
                  ('started (push id restarted))
                  ('skipped (push (cons id (plist-get result :reason)) skipped))
                  ('error (push (cons id (plist-get result :reason)) errors)))))
            ;; Reverse once to preserve order
            (setq restarted (nreverse restarted)
                  skipped (nreverse skipped)
                  errors (nreverse errors))
            (let ((msg (concat
                        (when restarted
                          (format "Restarted: %s\n"
                                  (mapconcat #'identity restarted ", ")))
                        (when skipped
                          (mapconcat (lambda (pair)
                                       (format "Skipped: %s (%s)\n" (car pair) (cdr pair)))
                                     skipped ""))
                        (when errors
                          (mapconcat (lambda (pair)
                                       (format "Error: %s (%s)\n" (car pair) (cdr pair)))
                                     errors "")))))
              (if errors
                  (supervisor--cli-make-result supervisor-cli-exit-failure
                                               (if json-p 'json 'human)
                                               (if json-p
                                                   (json-encode
                                                    `((restarted . ,restarted)
                                                      (skipped . ,(mapcar (lambda (p)
                                                                            `((id . ,(car p))
                                                                              (reason . ,(cdr p))))
                                                                          skipped))
                                                      (errors . ,(mapcar (lambda (p)
                                                                           `((id . ,(car p))
                                                                             (reason . ,(cdr p))))
                                                                         errors))))
                                                 msg))
                (supervisor--cli-success
                 (if json-p
                     (json-encode
                      `((restarted . ,restarted)
                        (skipped . ,(mapcar (lambda (p)
                                              `((id . ,(car p))
                                                (reason . ,(cdr p))))
                                            skipped))))
                   msg)
                 (if json-p 'json 'human))))))
         (t
          ;; Restart all: graceful stop then start, with forced fallback.
          (unless (supervisor--cli-stop-all-gracefully)
            (supervisor-stop-now))
          (supervisor-start)
          (supervisor--cli-success
           (if json-p
               (json-encode '((message . "Supervisor restarted")))
             "Supervisor restarted\n")
           (if json-p 'json 'human))))))))

(defun supervisor--cli-cmd-daemon-reload (args json-p)
  "Handle `daemon-reload' command with ARGS.  JSON-P enables JSON output.
Reload unit definitions without affecting runtime state."
  (let ((extra-err (supervisor--cli-reject-extra-args args json-p)))
    (if extra-err extra-err
      (let ((result (supervisor-daemon-reload)))
        (supervisor--cli-success
         (if json-p
             (json-encode `((reloaded . t)
                            (entries . ,(plist-get result :entries))
                            (invalid . ,(plist-get result :invalid))))
           (format "Reloaded: %d entries, %d invalid\n"
                   (plist-get result :entries)
                   (plist-get result :invalid)))
         (if json-p 'json 'human))))))

(defun supervisor--cli-cmd-reload (args json-p)
  "Handle `reload [--] ID...' command with ARGS.  JSON-P enables JSON output.
Hot-reload specific units: re-reads config and restarts running units.
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
                                 "reload requires at least one ID"
                                 (if json-p 'json 'human)))
         (t
          (let ((results nil)
                (has-error nil))
            (dolist (id args)
              (let ((r (supervisor--reload-unit id)))
                (push r results)
                (when (string-prefix-p "error:" (plist-get r :action))
                  (setq has-error t))))
            (setq results (nreverse results))
            (supervisor--maybe-refresh-dashboard)
            (let ((msg (mapconcat
                        (lambda (r)
                          (format "%s: %s\n"
                                  (plist-get r :id) (plist-get r :action)))
                        results "")))
              (if has-error
                  (supervisor--cli-make-result
                   supervisor-cli-exit-failure
                   (if json-p 'json 'human)
                   (if json-p
                       (json-encode
                        `((results
                           . ,(mapcar (lambda (r)
                                        `((id . ,(plist-get r :id))
                                          (action . ,(plist-get r :action))))
                                      results))))
                     msg))
                (supervisor--cli-success
                 (if json-p
                     (json-encode
                      `((results
                         . ,(mapcar (lambda (r)
                                      `((id . ,(plist-get r :id))
                                        (action . ,(plist-get r :action))))
                                    results))))
                   msg)
                 (if json-p 'json 'human)))))))))))

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
          (supervisor--cli-policy-batch
           args #'supervisor--policy-enable "Enabled" json-p)))))))

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
          (supervisor--cli-policy-batch
           args #'supervisor--policy-disable "Disabled" json-p)))))))

(defun supervisor--cli-cmd-mask (args json-p)
  "Handle `mask [--] ID...' command with ARGS.  JSON-P enables JSON output.
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
                                 "mask requires at least one ID"
                                 (if json-p 'json 'human)))
         (t
          (supervisor--cli-policy-batch
           args #'supervisor--policy-mask "Masked" json-p)))))))

(defun supervisor--cli-cmd-unmask (args json-p)
  "Handle `unmask [--] ID...' command with ARGS.  JSON-P enables JSON output.
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
                                 "unmask requires at least one ID"
                                 (if json-p 'json 'human)))
         (t
          (supervisor--cli-policy-batch
           args #'supervisor--policy-unmask "Unmasked" json-p)))))))

(defun supervisor--cli-cmd-is-active (args json-p)
  "Handle `is-active ID' command with ARGS.  JSON-P enables JSON output.
Exit 0 if the unit is active (running or latched), exit 3 if not active,
exit 4 if no such unit.  Uses systemctl-compatible exit code semantics."
  (cond
   ((null args)
    (supervisor--cli-error supervisor-cli-exit-invalid-args
                           "is-active requires an ID argument"
                           (if json-p 'json 'human)))
   ((cdr args)
    (supervisor--cli-error supervisor-cli-exit-invalid-args
                           (format "is-active takes exactly one ID, got: %s"
                                   (mapconcat #'identity args " "))
                           (if json-p 'json 'human)))
   (t
    (let* ((id (car args))
           (snapshot (supervisor--build-snapshot))
           (result (supervisor--cli-all-entries-info snapshot))
           (entries (car result))
           (info (cl-find id entries
                          :key (lambda (e) (alist-get 'id e))
                          :test #'equal)))
      (if (not info)
          (supervisor--cli-make-result
           supervisor-cli-exit-no-such-unit
           (if json-p 'json 'human)
           (if json-p
               (json-encode `((id . ,id) (active . :json-false)
                              (status . "inactive")))
             (format "inactive\n")))
        (let* ((status (alist-get 'status info))
               (active (or (equal status "running")
                           (equal status "active")))
               (exitcode (if active
                             supervisor-cli-exit-success
                           supervisor-cli-exit-not-active)))
          (supervisor--cli-make-result
           exitcode
           (if json-p 'json 'human)
           (if json-p
               (json-encode `((id . ,id)
                              (active . ,(if active t :json-false))
                              (status . ,status)))
             (format "%s\n" status)))))))))

(defun supervisor--cli-cmd-is-enabled (args json-p)
  "Handle `is-enabled ID' command with ARGS.  JSON-P enables JSON output.
Exit 0 if the unit is enabled, exit 1 if disabled or masked,
exit 4 if no such unit.  Uses systemctl-compatible exit codes."
  (cond
   ((null args)
    (supervisor--cli-error supervisor-cli-exit-invalid-args
                           "is-enabled requires an ID argument"
                           (if json-p 'json 'human)))
   ((cdr args)
    (supervisor--cli-error supervisor-cli-exit-invalid-args
                           (format "is-enabled takes exactly one ID, got: %s"
                                   (mapconcat #'identity args " "))
                           (if json-p 'json 'human)))
   (t
    (let* ((id (car args))
           (snapshot (supervisor--build-snapshot))
           (result (supervisor--cli-all-entries-info snapshot))
           (entries (car result))
           (info (cl-find id entries
                          :key (lambda (e) (alist-get 'id e))
                          :test #'equal)))
      (if (not info)
          (supervisor--cli-make-result
           supervisor-cli-exit-no-such-unit
           (if json-p 'json 'human)
           (if json-p
               (json-encode `((id . ,id) (enabled . :json-false)
                              (state . "not-found")))
             (format "not-found\n")))
        (let* ((is-masked (eq (gethash id supervisor--mask-override) 'masked))
               (enabled (alist-get 'enabled info))
               (state (cond (is-masked "masked")
                            (enabled "enabled")
                            (t "disabled")))
               (exitcode (if enabled
                             supervisor-cli-exit-success
                           supervisor-cli-exit-failure)))
          (supervisor--cli-make-result
           exitcode
           (if json-p 'json 'human)
           (if json-p
               (json-encode `((id . ,id)
                              (enabled . ,(if enabled t :json-false))
                              (state . ,state)))
             (format "%s\n" state)))))))))

(defun supervisor--cli-cmd-is-failed (args json-p)
  "Handle `is-failed ID' command with ARGS.  JSON-P enables JSON output.
Exit 0 if the unit is in a failed state, exit 1 if not failed,
exit 4 if no such unit.  Uses systemctl-compatible exit codes."
  (cond
   ((null args)
    (supervisor--cli-error supervisor-cli-exit-invalid-args
                           "is-failed requires an ID argument"
                           (if json-p 'json 'human)))
   ((cdr args)
    (supervisor--cli-error supervisor-cli-exit-invalid-args
                           (format "is-failed takes exactly one ID, got: %s"
                                   (mapconcat #'identity args " "))
                           (if json-p 'json 'human)))
   (t
    (let* ((id (car args))
           (snapshot (supervisor--build-snapshot))
           (result (supervisor--cli-all-entries-info snapshot))
           (entries (car result))
           (info (cl-find id entries
                          :key (lambda (e) (alist-get 'id e))
                          :test #'equal)))
      (if (not info)
          (supervisor--cli-make-result
           supervisor-cli-exit-no-such-unit
           (if json-p 'json 'human)
           (if json-p
               (json-encode `((id . ,id) (failed . :json-false)
                              (status . "inactive")))
             (format "inactive\n")))
        (let* ((status (alist-get 'status info))
               (is-failed (member status '("dead" "failed")))
               (exitcode (if is-failed
                             supervisor-cli-exit-success
                           supervisor-cli-exit-failure)))
          (supervisor--cli-make-result
           exitcode
           (if json-p 'json 'human)
           (if json-p
               (json-encode `((id . ,id)
                              (failed . ,(if is-failed t :json-false))
                              (status . ,status)))
             (format "%s\n" status)))))))))

(defun supervisor--cli-cmd-reset-failed (args json-p)
  "Handle `reset-failed [--] [ID...]' with ARGS.  JSON-P enables JSON output.
With IDs: reset failed state for those entries.
Without IDs: reset all failed entries.
Use -- before IDs that start with a hyphen."
  (let ((unknown (supervisor--cli-has-unknown-flags args)))
    (if unknown
        (supervisor--cli-error supervisor-cli-exit-invalid-args
                               (format "Unknown option: %s" unknown)
                               (if json-p 'json 'human))
      (let* ((ids (supervisor--cli-strip-separator args))
             (targets (if ids
                         ids
                       ;; No IDs: collect all failed entries
                       (let ((all nil))
                         (maphash (lambda (k _v) (push k all))
                                  supervisor--failed)
                         (sort all #'string<)))))
        (if (null targets)
            (supervisor--cli-success
             (if json-p
                 (json-encode '((reset . []) (skipped . [])))
               "No failed units\n")
             (if json-p 'json 'human))
          (let ((reset-ids nil)
                (skipped nil))
            (dolist (id targets)
              (let ((result (supervisor--reset-failed id)))
                (pcase (plist-get result :status)
                  ('reset (push id reset-ids))
                  ('skipped (push (cons id (plist-get result :reason)) skipped)))))
            (setq reset-ids (nreverse reset-ids)
                  skipped (nreverse skipped))
            (supervisor--maybe-refresh-dashboard)
            (supervisor--cli-success
             (if json-p
                 (json-encode
                  `((reset . ,reset-ids)
                    (skipped . ,(mapcar (lambda (p)
                                          `((id . ,(car p))
                                            (reason . ,(cdr p))))
                                        skipped))))
               (concat
                (when reset-ids
                  (format "Reset: %s\n"
                          (mapconcat #'identity reset-ids ", ")))
                (when skipped
                  (mapconcat (lambda (pair)
                               (format "Skipped: %s (%s)\n" (car pair) (cdr pair)))
                             skipped ""))))
             (if json-p 'json 'human))))))))

(defun supervisor--cli-cmd-restart-policy (args json-p)
  "Handle `restart-policy POLICY [--] ID...' with ARGS.  JSON-P for JSON.
POLICY is one of: no, on-success, on-failure, always.
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
                                 "restart-policy requires (no|on-success|on-failure|always) and at least one ID"
                                 (if json-p 'json 'human)))
         (t
          (let* ((policy-str (car args))
                 (ids (cdr args))
                 (policy (intern policy-str)))
            (if (not (memq policy supervisor--valid-restart-policies))
                (supervisor--cli-error
                 supervisor-cli-exit-invalid-args
                 (format "Invalid restart policy: %s (must be no, on-success, on-failure, or always)"
                         policy-str)
                 (if json-p 'json 'human))
              (supervisor--cli-policy-batch
               ids
               (lambda (id) (supervisor--policy-set-restart id policy))
               (format "Restart policy %s" policy-str)
               json-p)))))))))

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
                 (enabled-p (cond ((equal policy "on") 'on)
                                  ((equal policy "off") 'off)
                                  (t nil))))
            (if (null enabled-p)
                (supervisor--cli-error supervisor-cli-exit-invalid-args
                                       "logging requires 'on' or 'off'"
                                       (if json-p 'json 'human))
              (supervisor--cli-policy-batch
               ids
               (lambda (id)
                 (supervisor--policy-set-logging id (eq enabled-p 'on)))
               (format "Logging %s" policy)
               json-p)))))))))

(defun supervisor--cli-cmd-blame (args json-p)
  "Handle `blame' command with ARGS.  JSON-P enables JSON output."
  (let ((extra-err (supervisor--cli-reject-extra-args args json-p)))
    (if extra-err extra-err
      (let ((blame-data nil))
        (maphash (lambda (id start-time)
                   (let ((ready-time (gethash id supervisor--ready-times)))
                     (push `((id . ,id)
                             (start_time . ,start-time)
                             (ready_time . ,ready-time)
                             (duration . ,(when ready-time (- ready-time start-time))))
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
                                   (if (null r) "-" (format "%.3f" r)))
                                 (let ((d (alist-get 'duration e)))
                                   (if (null d) "-" (format "%.3fs" d)))))
                       blame-data "")))
         (if json-p 'json 'human))))))

(defun supervisor--cli-cmd-list-dependencies (args json-p)
  "Handle `list-dependencies [ID]' command with ARGS.  JSON-P for JSON."
  (cond
   ((> (length args) 1)
    (supervisor--cli-error supervisor-cli-exit-invalid-args
                           (format "list-dependencies takes at most one ID, got: %s"
                                   (mapconcat #'identity args " "))
                           (if json-p 'json 'human)))
   ((and args (string-prefix-p "-" (car args)))
    (supervisor--cli-error supervisor-cli-exit-invalid-args
                           (format "Unknown option: %s" (car args))
                           (if json-p 'json 'human)))
   (t
    (let* ((plan (supervisor--build-plan (supervisor--effective-programs)))
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
  "Handle `logs [--tail N] [--] ID' command with ARGS.  JSON-P for JSON.
Options must come before --.  Use -- before IDs that start with hyphen."
  ;; Split at -- separator: options before, ID after
  (let* ((split (supervisor--cli-split-at-separator args))
         (before (car split))
         (after (cdr split))
         ;; Check for unknown flags in before-part only
         (unknown (supervisor--cli-has-unknown-flags before '("--tail")))
         ;; Parse --tail by position (not value) to avoid collision bugs
         (parsed (supervisor--cli-parse-option before "--tail"))
         (tail-val (plist-get parsed :value))
         (tail-missing (plist-get parsed :missing))
         (tail-duplicate (plist-get parsed :duplicate))
         (before-positional (plist-get parsed :positional))
         ;; Validate --tail value if present
         (tail-invalid (and tail-val
                            (not (string-match-p "\\`[0-9]+\\'" tail-val))))
         (tail-n (if (and tail-val (not tail-invalid))
                     (string-to-number tail-val)
                   50))
         ;; ID is first positional from before or first from after
         (id (or (car after) (car before-positional)))
         ;; Extra args check: should have at most 1 ID total
         (extra-ids (append (cdr before-positional) (cdr after))))
    (cond
     (unknown
      (supervisor--cli-error supervisor-cli-exit-invalid-args
                             (format "Unknown option: %s" unknown)
                             (if json-p 'json 'human)))
     (tail-duplicate
      (supervisor--cli-error supervisor-cli-exit-invalid-args
                             "--tail specified multiple times"
                             (if json-p 'json 'human)))
     (tail-missing
      (supervisor--cli-error supervisor-cli-exit-invalid-args
                             "--tail requires a numeric value"
                             (if json-p 'json 'human)))
     (tail-invalid
      (supervisor--cli-error supervisor-cli-exit-invalid-args
                             (format "--tail value must be a number, got: %s" tail-val)
                             (if json-p 'json 'human)))
     ((null id)
      (supervisor--cli-error supervisor-cli-exit-invalid-args
                             "logs requires an ID argument"
                             (if json-p 'json 'human)))
     ;; Reject if first positional in before-part looks like an option
     ((and (null after) before-positional (string-prefix-p "-" (car before-positional)))
      (supervisor--cli-error supervisor-cli-exit-invalid-args
                             "logs requires ID as first argument (use -- for IDs starting with -)"
                             (if json-p 'json 'human)))
     ;; Reject extra IDs
     (extra-ids
      (supervisor--cli-error supervisor-cli-exit-invalid-args
                             (format "logs takes exactly one ID, got extra: %s"
                                     (mapconcat #'identity extra-ids " "))
                             (if json-p 'json 'human)))
     (t
      (let ((log-file (supervisor--log-file id)))
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
                                 (if json-p 'json 'human))))))))

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

(defconst supervisor--valid-signals
  '(SIGHUP SIGINT SIGQUIT SIGILL SIGTRAP SIGABRT SIGBUS SIGFPE
    SIGKILL SIGUSR1 SIGSEGV SIGUSR2 SIGPIPE SIGALRM SIGTERM
    SIGCHLD SIGCONT SIGSTOP SIGTSTP SIGTTIN SIGTTOU SIGURG
    SIGXCPU SIGXFSZ SIGVTALRM SIGPROF SIGWINCH SIGIO SIGSYS)
  "List of valid POSIX signal names for CLI kill command.")

(defun supervisor--cli-cmd-kill (args json-p)
  "Handle `kill [--signal SIG] [--] ID' command with ARGS.  JSON-P for JSON.
Options must come before --.  Use -- before IDs that start with hyphen."
  ;; Split at -- separator: options before, ID after
  (let* ((split (supervisor--cli-split-at-separator args))
         (before (car split))
         (after (cdr split))
         ;; Check for unknown flags in before-part only
         (unknown (supervisor--cli-has-unknown-flags before '("--signal")))
         ;; Parse --signal by position (not value) to avoid collision bugs
         (parsed (supervisor--cli-parse-option before "--signal"))
         (sig-val (plist-get parsed :value))
         (sig-missing (plist-get parsed :missing))
         (sig-duplicate (plist-get parsed :duplicate))
         (before-positional (plist-get parsed :positional))
         ;; Normalize signal name: accept both TERM and SIGTERM forms
         (sig-name (when sig-val
                     (let ((upper (upcase sig-val)))
                       (if (string-prefix-p "SIG" upper) upper (concat "SIG" upper)))))
         (sig-sym (when sig-name (intern sig-name)))
         (sig-invalid (and sig-val (not (memq sig-sym supervisor--valid-signals))))
         (sig (if sig-sym sig-sym 'SIGTERM))
         ;; ID is first positional from before or first from after
         (id (or (car after) (car before-positional)))
         ;; Extra args check: should have at most 1 ID total
         (extra-ids (append (cdr before-positional) (cdr after))))
    (cond
     (unknown
      (supervisor--cli-error supervisor-cli-exit-invalid-args
                             (format "Unknown option: %s" unknown)
                             (if json-p 'json 'human)))
     (sig-duplicate
      (supervisor--cli-error supervisor-cli-exit-invalid-args
                             "--signal specified multiple times"
                             (if json-p 'json 'human)))
     (sig-missing
      (supervisor--cli-error supervisor-cli-exit-invalid-args
                             "--signal requires a signal name"
                             (if json-p 'json 'human)))
     (sig-invalid
      (supervisor--cli-error supervisor-cli-exit-invalid-args
                             (format "Invalid signal name: %s" sig-val)
                             (if json-p 'json 'human)))
     ((null id)
      (supervisor--cli-error supervisor-cli-exit-invalid-args
                             "kill requires an ID argument"
                             (if json-p 'json 'human)))
     ;; Reject if first positional in before-part looks like an option
     ((and (null after) before-positional (string-prefix-p "-" (car before-positional)))
      (supervisor--cli-error supervisor-cli-exit-invalid-args
                             "kill requires ID as first argument (use -- for IDs starting with -)"
                             (if json-p 'json 'human)))
     ;; Reject extra IDs
     (extra-ids
      (supervisor--cli-error supervisor-cli-exit-invalid-args
                             (format "kill takes exactly one ID, got extra: %s"
                                     (mapconcat #'identity extra-ids " "))
                             (if json-p 'json 'human)))
     (t
      (let ((result (supervisor--manual-kill id sig)))
        (pcase (plist-get result :status)
          ('signaled
           (supervisor--cli-success
            (if json-p
                (json-encode `((id . ,id) (signal . ,(symbol-name sig))))
              (format "Sent %s to %s\n" sig id))
            (if json-p 'json 'human)))
          (_
           (supervisor--cli-error supervisor-cli-exit-failure
                                  (format "Process '%s' not running" id)
                                  (if json-p 'json 'human)))))))))

;;; Timer Status Output

(defun supervisor--cli-gather-timer-info ()
  "Gather timer info from supervisor state.
Returns a list of alists, one per timer."
  (let ((result nil))
    (dolist (timer supervisor--timer-list)
      (let* ((id (supervisor-timer-id timer))
             (target (supervisor-timer-target timer))
             (enabled (supervisor-timer-enabled timer))
             (persistent (supervisor-timer-persistent timer))
             (state (gethash id supervisor--timer-state))
             (last-run (plist-get state :last-run-at))
             (last-success (plist-get state :last-success-at))
             (last-failure (plist-get state :last-failure-at))
             (last-exit (plist-get state :last-exit))
             (next-run (plist-get state :next-run-at))
             (last-miss (plist-get state :last-missed-at))
             (miss-reason (plist-get state :last-miss-reason))
             (retry-at (plist-get state :retry-next-at)))
        (push `((id . ,id)
                (target . ,target)
                (enabled . ,enabled)
                (persistent . ,persistent)
                (last-run . ,last-run)
                (last-success . ,last-success)
                (last-failure . ,last-failure)
                (last-exit . ,last-exit)
                (next-run . ,next-run)
                (last-miss . ,last-miss)
                (miss-reason . ,miss-reason)
                (retry-at . ,retry-at))
              result)))
    (nreverse result)))

(defun supervisor--cli-format-relative-time (timestamp)
  "Format TIMESTAMP as relative time string like \"5m ago\" or \"in 5m\"."
  (if (null timestamp)
      "-"
    (let* ((now (float-time))
           (diff (- now timestamp))
           (abs-diff (abs diff))
           (suffix (if (> diff 0) " ago" ""))
           (prefix (if (< diff 0) "in " "")))
      (cond
       ((< abs-diff 60) (format "%s%ds%s" prefix (round abs-diff) suffix))
       ((< abs-diff 3600) (format "%s%dm%s" prefix (round (/ abs-diff 60)) suffix))
       ((< abs-diff 86400) (format "%s%dh%s" prefix (round (/ abs-diff 3600)) suffix))
       (t (format "%s%dd%s" prefix (round (/ abs-diff 86400)) suffix))))))

(defun supervisor--cli-format-timer-line (info)
  "Format single timer INFO alist as status line."
  (let ((id (alist-get 'id info))
        (target (alist-get 'target info))
        (enabled (alist-get 'enabled info))
        (last-run (alist-get 'last-run info))
        (next-run (alist-get 'next-run info))
        (last-exit (alist-get 'last-exit info))
        (miss-reason (alist-get 'miss-reason info)))
    (format "%-16s %-16s %-8s %-12s %-12s %-8s %s\n"
            (or id "-")
            (or target "-")
            (if enabled "yes" "no")
            (supervisor--cli-format-relative-time last-run)
            (supervisor--cli-format-relative-time next-run)
            (if (null last-exit) "-" (number-to-string last-exit))
            (if miss-reason (symbol-name miss-reason) "-"))))

(defun supervisor--cli-timers-human (timers invalid)
  "Format TIMERS and INVALID as human-readable timer status table."
  (if (and (null timers) (null invalid))
      "No timers configured.\n"
    (let ((header (format "%-16s %-16s %-8s %-12s %-12s %-8s %s\n"
                          "ID" "TARGET" "ENABLED" "LAST-RUN" "NEXT-RUN" "EXIT" "MISS"))
          (sep (make-string 90 ?-)))
      (concat header sep "\n"
              (mapconcat #'supervisor--cli-format-timer-line timers "")
              (when invalid
                (concat "\nInvalid timers:\n"
                        (mapconcat (lambda (inv)
                                     (format "  %s: %s\n"
                                             (plist-get inv :id)
                                             (plist-get inv :reason)))
                                   invalid "")))))))

(defun supervisor--cli-timer-to-json-obj (info)
  "Convert timer INFO alist to JSON-compatible alist."
  `((id . ,(alist-get 'id info))
    (target . ,(alist-get 'target info))
    (enabled . ,(if (alist-get 'enabled info) t :json-false))
    (persistent . ,(if (alist-get 'persistent info) t :json-false))
    (last_run_at . ,(alist-get 'last-run info))
    (last_success_at . ,(alist-get 'last-success info))
    (last_failure_at . ,(alist-get 'last-failure info))
    (last_exit . ,(alist-get 'last-exit info))
    (next_run_at . ,(alist-get 'next-run info))
    (last_miss_at . ,(alist-get 'last-miss info))
    (miss_reason . ,(if (alist-get 'miss-reason info)
                        (symbol-name (alist-get 'miss-reason info))
                      :json-null))
    (retry_at . ,(alist-get 'retry-at info))))

(defun supervisor--cli-invalid-timer-to-json-obj (info)
  "Convert invalid timer INFO plist to JSON-compatible alist."
  `((id . ,(plist-get info :id))
    (reason . ,(plist-get info :reason))))

(defun supervisor--cli-timers-json (timers invalid)
  "Format TIMERS and INVALID as JSON timer status object."
  (let ((obj `((timers . ,(supervisor--cli-ensure-array
                           (mapcar #'supervisor--cli-timer-to-json-obj timers)))
               (invalid . ,(supervisor--cli-ensure-array
                            (mapcar #'supervisor--cli-invalid-timer-to-json-obj invalid))))))
    (json-encode obj)))

(defun supervisor--cli-cmd-list-timers (args json-p)
  "Handle `list-timers' command with ARGS.  JSON-P enables JSON output."
  (let ((unknown (supervisor--cli-has-unknown-flags args)))
    (cond
     (unknown
      (supervisor--cli-error supervisor-cli-exit-invalid-args
                             (format "Unknown option: %s" unknown)
                             (if json-p 'json 'human)))
     (args
      (supervisor--cli-error supervisor-cli-exit-invalid-args
                             "list-timers command does not accept arguments"
                             (if json-p 'json 'human)))
     ;; Check if timer subsystem is enabled
     ((not (supervisor-timer-subsystem-active-p))
      (let ((msg "Timer subsystem is disabled (experimental feature).\nEnable with: (supervisor-timer-subsystem-mode 1)"))
        (supervisor--cli-success
         (if json-p
             (json-encode `((status . "disabled")
                            (message . "Timer subsystem is disabled (experimental feature)")))
           (concat msg "\n"))
         (if json-p 'json 'human))))
     (t
      (let* ((timers (supervisor--cli-gather-timer-info))
               ;; Convert hash table entries (id -> reason) to plists
               (invalid (let (result)
                          (maphash (lambda (id reason)
                                     (push (list :id id :reason reason) result))
                                   supervisor--invalid-timers)
                          (nreverse result)))
               (output (if json-p
                           (supervisor--cli-timers-json timers invalid)
                         (supervisor--cli-timers-human timers invalid))))
          (supervisor--cli-success output (if json-p 'json 'human)))))))

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

(defun supervisor--cli-cmd-cat (args json-p)
  "Handle `cat ID' command with ARGS.  JSON-P enables JSON output.
Output literal raw content of a unit file."
  (cond
   ((null args)
    (supervisor--cli-error supervisor-cli-exit-invalid-args
                           "cat requires an ID argument"
                           (if json-p 'json 'human)))
   ((cdr args)
    (supervisor--cli-error supervisor-cli-exit-invalid-args
                           (format "cat takes exactly one ID, got: %s"
                                   (mapconcat #'identity args " "))
                           (if json-p 'json 'human)))
   (t
    (let* ((id (car args))
           (path (supervisor--unit-file-path id)))
      (if (and path (file-exists-p path))
          (let ((content (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))))
            (if json-p
                (supervisor--cli-success
                 (json-encode `((path . ,path)
                                (content . ,content)))
                 'json)
              (supervisor--cli-success content 'human)))
        (supervisor--cli-error supervisor-cli-exit-failure
                               (format "Unit file not found: %s"
                                       (or path id))
                               (if json-p 'json 'human)))))))

(defun supervisor--cli-edit-launch-editor (editor path)
  "Launch EDITOR on PATH synchronously.
Returns the exit status of the editor process."
  (let* ((parts (split-string editor))
         (program (car parts))
         (extra-args (cdr parts)))
    (apply #'call-process program nil nil nil
           (append extra-args (list path)))))

(defun supervisor--cli-cmd-edit (args json-p)
  "Handle `edit ID' command with ARGS.  JSON-P enables JSON output.
Open unit file for editing.  Create scaffold if file does not exist.
In non-interactive context, launch $VISUAL or $EDITOR."
  (cond
   ((null args)
    (supervisor--cli-error supervisor-cli-exit-invalid-args
                           "edit requires an ID argument"
                           (if json-p 'json 'human)))
   ((cdr args)
    (supervisor--cli-error supervisor-cli-exit-invalid-args
                           (format "edit takes exactly one ID, got: %s"
                                   (mapconcat #'identity args " "))
                           (if json-p 'json 'human)))
   (t
    (let* ((id (car args))
           (path (supervisor--unit-file-path id))
           (root (or (when (fboundp 'supervisor--authority-root-for-id)
                       (supervisor--authority-root-for-id id))
                     (when path (file-name-directory path))))
           (tier (when (fboundp 'supervisor--authority-tier-for-id)
                   (supervisor--authority-tier-for-id id))))
      (if (not path)
          (supervisor--cli-error
           supervisor-cli-exit-failure
           "No active authority roots configured"
           (if json-p 'json 'human))
        (let ((created (not (file-exists-p path))))
          ;; Ensure directory exists
          (when created
            (make-directory (file-name-directory path) t)
            (write-region (supervisor--unit-file-scaffold id) nil path))
          (if json-p
              (supervisor--cli-success
               (json-encode `((path . ,path)
                              (root . ,root)
                              (tier . ,tier)
                              (created . ,(if created t :json-false))))
               'json)
            ;; Non-interactive: launch external editor
            (let* ((preamble
                    (concat (if created
                                (format "Created new unit file: %s\n"
                                        path)
                              "")
                            (format "Unit file: %s\n" path)
                            (when root
                              (format "Authority root: %s (tier %s)\n"
                                      root (or tier "?")))
                            ""))
                   (editor (or (getenv "VISUAL") (getenv "EDITOR"))))
              ;; Report tier/path before launching editor.
              ;; Use `message' so it shows in echo area / *Messages*
              ;; without corrupting the wrapper transport format.
              (message "%s" (string-trim-right preamble))
              (if editor
                  (let ((exit-status
                         (supervisor--cli-edit-launch-editor
                          editor path)))
                    (if (= 0 exit-status)
                        (supervisor--cli-success
                         (concat preamble
                                 "\nNext steps after editing:\n"
                                 "  supervisorctl daemon-reload"
                                 "    Reload unit definitions")
                         'human)
                      (supervisor--cli-error
                       supervisor-cli-exit-failure
                       (format "%sEditor exited with status %d"
                               preamble exit-status)
                       'human)))
                (supervisor--cli-error
                 supervisor-cli-exit-failure
                 (format "%sNo $VISUAL or $EDITOR set.  Edit the file manually:\n  %s\n\nNext steps after editing:\n  supervisorctl daemon-reload    Reload unit definitions"
                         preamble path)
                 'human))))))))))

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
                   "Systemctl-compatible commands:\n"
                   "  status [ID...]             Show unit status (detail with IDs, overview without)\n"
                   "  list-units [ID...]         List units (overview table)\n"
                   "  show ID                    Show all properties of a unit\n"
                   "  start [-- ID...]           Start units\n"
                   "  stop [-- ID...]            Stop units\n"
                   "  restart [-- ID...]         Restart units\n"
                   "  enable [--] ID...          Enable units\n"
                   "  disable [--] ID...         Disable units\n"
                   "  mask [--] ID...            Mask units (always disabled)\n"
                   "  unmask [--] ID...          Unmask units\n"
                   "  kill [--signal SIG] [--] ID  Send signal to unit\n"
                   "  is-active ID               Test if unit is active (0/3/4)\n"
                   "  is-enabled ID              Test if unit is enabled (0/1/4)\n"
                   "  is-failed ID               Test if unit is failed (0/1/4)\n"
                   "  cat ID                     Show unit file content\n"
                   "  edit ID                    Edit unit file\n"
                   "  daemon-reload              Reload unit definitions from disk\n"
                   "  reload [--] ID...          Hot-reload specific units\n"
                   "  list-dependencies [ID]     Show dependency graph\n"
                   "  list-timers                Show timer units\n\n"
                   "Supervisor-specific commands:\n"
                   "  verify                     Verify config\n"
                   "  reset-failed [--] [ID...]  Reset failed state\n"
                   "  restart-policy POLICY ID...    Set restart policy (no|on-success|on-failure|always)\n"
                   "  logging (on|off) ID...     Set logging policy\n"
                   "  blame                      Show startup timing\n"
                   "  logs [--tail N] [--] ID    View unit log file\n"
                   "  ping                       Check supervisor liveness\n"
                   "  version                    Show version\n\n"
                   "Options: --json (output as JSON)\n")
           (if json-p 'json 'human)))
         ((equal command "status")
          (supervisor--cli-cmd-status args json-p))
         ((equal command "list-units")
          (supervisor--cli-cmd-list-units args json-p))
         ((equal command "show")
          (supervisor--cli-cmd-show args json-p))
         ((equal command "verify")
          (supervisor--cli-cmd-verify args json-p))
         ((equal command "start")
          (supervisor--cli-cmd-start args json-p))
         ((equal command "stop")
          (supervisor--cli-cmd-stop args json-p))
         ((equal command "restart")
          (supervisor--cli-cmd-restart args json-p))
         ((equal command "daemon-reload")
          (supervisor--cli-cmd-daemon-reload args json-p))
         ((equal command "reload")
          (supervisor--cli-cmd-reload args json-p))
         ((equal command "enable")
          (supervisor--cli-cmd-enable args json-p))
         ((equal command "disable")
          (supervisor--cli-cmd-disable args json-p))
         ((equal command "mask")
          (supervisor--cli-cmd-mask args json-p))
         ((equal command "unmask")
          (supervisor--cli-cmd-unmask args json-p))
         ((equal command "restart-policy")
          (supervisor--cli-cmd-restart-policy args json-p))
         ((equal command "logging")
          (supervisor--cli-cmd-logging args json-p))
         ((equal command "blame")
          (supervisor--cli-cmd-blame args json-p))
         ((equal command "list-dependencies")
          (supervisor--cli-cmd-list-dependencies args json-p))
         ((equal command "logs")
          (supervisor--cli-cmd-logs args json-p))
         ((equal command "kill")
          (supervisor--cli-cmd-kill args json-p))
         ((equal command "ping")
          (supervisor--cli-cmd-ping args json-p))
         ((equal command "version")
          (supervisor--cli-cmd-version args json-p))
         ((equal command "list-timers")
          (supervisor--cli-cmd-list-timers args json-p))
         ((equal command "cat")
          (supervisor--cli-cmd-cat args json-p))
         ((equal command "edit")
          (supervisor--cli-cmd-edit args json-p))
         ((equal command "is-active")
          (supervisor--cli-cmd-is-active args json-p))
         ((equal command "is-enabled")
          (supervisor--cli-cmd-is-enabled args json-p))
         ((equal command "is-failed")
          (supervisor--cli-cmd-is-failed args json-p))
         ((equal command "reset-failed")
          (supervisor--cli-cmd-reset-failed args json-p))
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


(provide 'supervisor-cli)

;;; supervisor-cli.el ends here
