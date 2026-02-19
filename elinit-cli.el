;;; elinit-cli.el --- Elinit CLI interface -*- lexical-binding: t -*-

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

;; CLI interface for elinit.el.
;; Run M-x elinit-handbook for full documentation.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'elinit-core)

;; Forward declarations for timer subsystem (defined in elinit-timer.el)
(declare-function elinit-timer-build-list "elinit-timer" (plan))
(declare-function elinit-timer-id "elinit-timer" (timer))
(declare-function elinit-timer-target "elinit-timer" (timer))
(declare-function elinit-timer-enabled "elinit-timer" (timer))
(declare-function elinit-timer-persistent "elinit-timer" (timer))

;; Forward declarations for unit-file module (optional)
(declare-function elinit--unit-file-path "elinit-units" (id))
(declare-function elinit--unit-file-existing-path "elinit-units" (id))
(declare-function elinit--unit-file-scaffold "elinit-units" (id))
(declare-function elinit--authority-root-for-id "elinit-units" (id))
(declare-function elinit--authority-tier-for-id "elinit-units" (id))

;; Forward declarations for logging module (defined in elinit-log.el)
(declare-function elinit--log-file "elinit-log" (prog))
(declare-function elinit--log-parse-timestamp "elinit-log" (ts-string))
(declare-function elinit--log-decode-file "elinit-log"
                  (file &optional limit offset max-bytes))
(declare-function elinit--log-filter-records "elinit-log"
                  (records &optional since until priority))
(declare-function elinit--log-format-record-human "elinit-log" (record))
(declare-function elinit--log-record-to-json "elinit-log" (record))
(declare-function elinit--log-record-priority "elinit-log" (record))

;; Forward declarations for timer state variables (defined in elinit-timer.el)
(defvar elinit--invalid-timers)
(defvar elinit--timer-list)
(defvar elinit--timer-state)


;;; CLI Control Plane
;;
;; Non-interactive command-line interface for controlling elinit
;; via emacsclient. All business logic remains in Elisp; the external
;; wrapper (sbin/elinitctl) is transport-only.

(defconst elinit-cli-exit-success 0
  "Exit code for successful operation.")

(defconst elinit-cli-exit-failure 1
  "Exit code for runtime failure.")

(defconst elinit-cli-exit-invalid-args 2
  "Exit code for invalid arguments.")

(defconst elinit-cli-exit-not-active 3
  "Exit code for `is-active' when unit exists but is not active.
Matches systemctl LSB convention.")

(defconst elinit-cli-exit-no-such-unit 4
  "Exit code for `is-*' predicates when the unit does not exist.
Matches systemctl convention.")

(defconst elinit-cli-exit-validation-failed 4
  "Exit code when config validation fails.")

(defconst elinit-cli-exit-server-unavailable 69
  "Exit code when Emacs server is unavailable.
Uses sysexits.h EX_UNAVAILABLE to avoid collision with systemctl codes.")

(defconst elinit-cli-version "1.0.0"
  "CLI version string.")

(defcustom elinit-cli-follow-max-age 3600
  "Seconds before an inactive follow session is cleaned up.
The activity timestamp resets only when the log file produces new
records.  A session for a quiet unit will expire after this many
seconds of silence, which also catches orphaned sessions whose
client disconnected without cleanup."
  :type 'integer
  :group 'elinit)

(defvar elinit--cli-follow-sessions (make-hash-table :test 'equal)
  "Active follow sessions.  Keys are session-id strings.
Values are plists with keys: `:unit', `:log-file', `:offset',
`:since-ts', `:until-ts', `:priority', `:json-p', `:follow-file',
`:timer', `:last-activity'.")

(cl-defstruct (elinit-cli-result (:constructor elinit-cli-result--create))
  "Result returned from CLI dispatcher.
EXITCODE is an integer (0=success, 1=failure, 2=invalid args, etc).
FORMAT is `human' or `json'.
OUTPUT is the string to print."
  (exitcode 0 :type integer)
  (format 'human :type symbol)
  (output "" :type string))

(defun elinit--cli-make-result (exitcode format output)
  "Create CLI result with EXITCODE, FORMAT, and OUTPUT."
  (elinit-cli-result--create :exitcode exitcode :format format :output output))

(defun elinit--cli-success (output &optional format)
  "Create success result with OUTPUT and optional FORMAT (default human)."
  (elinit--cli-make-result elinit-cli-exit-success
                               (or format 'human) output))

(defun elinit--cli-error (exitcode message &optional format)
  "Create error result with EXITCODE, MESSAGE, and optional FORMAT.
When FORMAT is `json', the error is returned as a JSON object."
  (let ((fmt (or format 'human)))
    (elinit--cli-make-result
     exitcode fmt
     (if (eq fmt 'json)
         (json-encode `((error . t) (message . ,message) (exitcode . ,exitcode)))
       (format "Error: %s\n" message)))))

(defun elinit--cli-policy-batch (ids mutator-fn verb json-p)
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
    (when applied
      (elinit--save-overrides))
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
          (elinit--cli-make-result
           elinit-cli-exit-failure fmt
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
        (elinit--cli-success
         (if json-p
             (json-encode
              `((applied . ,(or applied []))
                (skipped
                 . ,(mapcar (lambda (p)
                              `((id . ,(car p)) (reason . ,(cdr p))))
                            skipped))))
           msg)
         fmt)))))

(defun elinit--cli-split-at-separator (args)
  "Split ARGS at \"--\" separator into (before-list . after-list).
The \"--\" itself is not included in either list.
If no \"--\" is present, returns (ARGS . nil)."
  (let ((pos (cl-position "--" args :test #'equal)))
    (if pos
        (cons (cl-subseq args 0 pos)
              (cl-subseq args (1+ pos)))
      (cons args nil))))

(defun elinit--cli-parse-option (args option)
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

(defun elinit--cli-has-unknown-flags (args &optional known-flags)
  "Check if ARGS contain unknown flags (starting with - or --).
KNOWN-FLAGS is a list of exact allowed flags like \"--tail\" \"--signal\".
Returns the first unknown flag or nil.  Checks both -- and single - flags.
Stops checking at \"--\" separator (POSIX end-of-options convention)."
  (let* ((known (or known-flags '()))
         (split (elinit--cli-split-at-separator args))
         (check-args (car split)))
    (cl-find-if (lambda (arg)
                  (and (string-prefix-p "-" arg)
                       (not (member arg known))))
                check-args)))

(defun elinit--cli-strip-separator (args)
  "Remove \"--\" separator from ARGS, concatenating before and after parts."
  (let ((split (elinit--cli-split-at-separator args)))
    (append (car split) (cdr split))))

(defun elinit--cli-reject-extra-args (args json-p)
  "Return error result if ARGS is non-nil.  JSON-P controls format.
Returns nil if args is empty, otherwise returns an error result."
  (when args
    (elinit--cli-error elinit-cli-exit-invalid-args
                           (format "Unexpected arguments: %s"
                                   (mapconcat #'identity args " "))
                           (if json-p 'json 'human))))

(defun elinit--cli-ensure-array (list)
  "Ensure LIST is encoded as JSON array, not null for empty list."
  (or list (vector)))

;;; CLI Status/List helpers

(defun elinit--cli-entry-info (entry &optional snapshot)
  "Build info alist for parsed ENTRY, using optional SNAPSHOT for state.
Returns alist with all fields needed for status display."
  (let* ((id (elinit-entry-id entry))
         (type (elinit-entry-type entry))
         (enabled-cfg (elinit-entry-enabled-p entry))
         (restart-cfg (elinit-entry-restart-policy entry))
         (logging-cfg (elinit-entry-logging-p entry))
         (delay (elinit-entry-delay entry))
         (after (elinit-entry-after entry))
         (requires (elinit-entry-requires entry))
         (status-result (elinit--compute-entry-status id type snapshot))
         (status (car status-result))
         (reason (elinit--compute-entry-reason id type snapshot))
         ;; Get effective values with overrides
         (enabled-eff (elinit--get-effective-enabled id enabled-cfg))
         (restart-eff (if (eq type 'oneshot) nil
                        (elinit--get-effective-restart id restart-cfg)))
         (logging-eff (elinit--get-effective-logging id logging-cfg))
         ;; Get PID if running
         (pid (when snapshot
                (gethash id (elinit-snapshot-process-pids snapshot))))
         (pid-global (unless snapshot
                       (let ((proc (gethash id elinit--processes)))
                         (when (and proc (process-live-p proc))
                           (process-id proc)))))
         ;; Get timing info
         (start-time (gethash id elinit--start-times))
         (ready-time (gethash id elinit--ready-times))
         ;; Telemetry
         (actual-pid (or pid pid-global))
         (uptime (elinit--telemetry-uptime id))
         (restart-count (elinit--telemetry-restart-count id))
         (last-exit (elinit--telemetry-last-exit-info id snapshot))
         (next-eta (elinit--telemetry-next-restart-eta id))
         (metrics (when actual-pid
                    (elinit--telemetry-process-metrics actual-pid)))
         (process-tree (when actual-pid
                         (elinit--telemetry-process-tree actual-pid))))
    `((id . ,id)
      (type . ,type)
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
      (process-tree . ,process-tree)
      (unit-file . ,(cond
                     ((fboundp 'elinit--unit-file-existing-path)
                      (elinit--unit-file-existing-path id))
                     ((fboundp 'elinit--unit-file-path)
                      (when-let* ((path (elinit--unit-file-path id)))
                        (when (file-exists-p path)
                          path)))))
      (authority-tier . ,(when (fboundp 'elinit--authority-tier-for-id)
                           (elinit--authority-tier-for-id id)))
      (working-directory . ,(elinit-entry-working-directory entry))
      (environment . ,(elinit-entry-environment entry))
      (environment-file . ,(elinit-entry-environment-file entry))
      (exec-stop . ,(elinit-entry-exec-stop entry))
      (exec-reload . ,(elinit-entry-exec-reload entry))
      (restart-sec . ,(elinit-entry-restart-sec entry))
      (user . ,(elinit-entry-user entry))
      (group . ,(elinit-entry-group entry))
      (description . ,(elinit-entry-description entry))
      (documentation . ,(elinit-entry-documentation entry))
      (sandbox-profile . ,(elinit-entry-sandbox-profile entry))
      (sandbox-network . ,(let ((profile (or (elinit-entry-sandbox-profile entry) 'none)))
                            (or (elinit-entry-sandbox-network entry)
                                (elinit--sandbox-profile-default-network
                                 profile))))
      (sandbox-enabled . ,(elinit--sandbox-requesting-p entry))
      (limit-nofile . ,(elinit-entry-limit-nofile entry))
      (limit-nproc . ,(elinit-entry-limit-nproc entry))
      (limit-core . ,(elinit-entry-limit-core entry))
      (limit-fsize . ,(elinit-entry-limit-fsize entry))
      (limit-as . ,(elinit-entry-limit-as entry))
      (log-tail . ,(elinit--telemetry-log-tail id 5)))))

(defun elinit--cli-all-entries-info (&optional snapshot)
  "Build info alists for all valid entries, using optional SNAPSHOT.
Returns (entries invalid) where entries is list of alists and
invalid is list of (id . reason) pairs, sorted by ID for determinism.
Includes both plan-level and unit-file-level invalid entries."
  (let* ((plan (elinit--build-plan (elinit--effective-programs)))
         (entries (elinit-plan-entries plan))
         (combined-invalid (make-hash-table :test 'equal))
         (invalid-list nil)
         (entry-infos nil))
    ;; Merge plan invalids and unit-file invalids into combined hash
    (maphash (lambda (k v) (puthash k v combined-invalid))
             (elinit-plan-invalid plan))
    (when (boundp 'elinit--unit-file-invalid)
      (maphash (lambda (k v) (puthash k v combined-invalid))
               (symbol-value 'elinit--unit-file-invalid)))
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
      (push (elinit--cli-entry-info entry snapshot) entry-infos))
    (list (nreverse entry-infos) invalid-list)))

;;; CLI Human Formatters

(defun elinit--cli-format-bool (val)
  "Format boolean VAL as yes/no string."
  (if val "yes" "no"))

(defun elinit--cli-format-duration (seconds)
  "Format SECONDS as a human-readable duration string."
  (let ((s (round seconds)))
    (cond
     ((< s 60) (format "%ds" s))
     ((< s 3600) (format "%dm%ds" (/ s 60) (% s 60)))
     ((< s 86400) (format "%dh%dm" (/ s 3600) (% (/ s 60) 60)))
     (t (format "%dd%dh" (/ s 86400) (% (/ s 3600) 24))))))

(defun elinit--cli-format-status-line (info)
  "Format single entry INFO alist as status table row."
  (let ((id (alist-get 'id info))
        (type (alist-get 'type info))
        (enabled (alist-get 'enabled info))
        (status (alist-get 'status info))
        (restart (alist-get 'restart info))
        (logging (alist-get 'logging info))
        (pid (alist-get 'pid info))
        (reason (alist-get 'reason info)))
    (format "%-16s %-8s %-8s %-10s %-11s %-5s %-7s %s\n"
            (truncate-string-to-width (or id "-") 16)
            (or (symbol-name type) "-")
            (elinit--cli-format-bool enabled)
            (or status "-")
            (if (eq type 'oneshot) "n/a"
              (if restart (symbol-name restart) "-"))
            (elinit--cli-format-bool logging)
            (if pid (number-to-string pid) "-")
            (or reason "-"))))

(defun elinit--cli-format-invalid-line (info)
  "Format invalid entry INFO as status table row."
  (let ((id (alist-get 'id info))
        (reason (alist-get 'reason info)))
    (format "%-16s %-8s %-8s %-10s %-11s %-5s %-7s %s\n"
            (truncate-string-to-width (or id "-") 16)
            "-" "-" "invalid" "-" "-" "-"
            (or reason "-"))))

(defun elinit--cli-status-human (entries invalid)
  "Format ENTRIES and INVALID as human-readable status table."
  (let ((header (format "%-16s %-8s %-8s %-10s %-11s %-5s %-7s %s\n"
                        "ID" "TYPE" "ENABLED" "STATUS" "RESTART" "LOG" "PID" "REASON"))
        (sep (make-string 94 ?-)))
    (concat header sep "\n"
            (mapconcat #'elinit--cli-format-status-line entries "")
            (when invalid
              (mapconcat #'elinit--cli-format-invalid-line invalid "")))))

(defun elinit--cli-describe-human (info)
  "Format single entry INFO as human-readable detail view."
  (let ((id (alist-get 'id info))
        (type (alist-get 'type info))
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
     (format "Status: %s%s\n" status (if reason (format " (%s)" reason) ""))
     (format "Enabled: %s%s\n"
             (elinit--cli-format-bool enabled)
             (if (not (eq enabled enabled-cfg)) " (override)" ""))
     (when (not (eq type 'oneshot))
       (format "Restart: %s%s\n"
               (if restart (symbol-name restart) "-")
               (if (not (eq restart restart-cfg)) " (override)" "")))
     (format "Logging: %s%s\n"
             (elinit--cli-format-bool logging)
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
     (let ((u (alist-get 'user info))
           (g (alist-get 'group info)))
       (when (or u g)
         (concat
          (when u (format "User: %s\n" u))
          (when g (format "Group: %s\n" g)))))
     (when (alist-get 'sandbox-enabled info)
       (format "Sandbox: %s (network %s)\n"
               (or (alist-get 'sandbox-profile info) "none")
               (alist-get 'sandbox-network info)))
     (let ((limits nil))
       (dolist (pair '((limit-nofile . "nofile") (limit-nproc . "nproc")
                       (limit-core . "core") (limit-fsize . "fsize")
                       (limit-as . "as")))
         (when-let* ((v (alist-get (car pair) info)))
           (push (format "%s=%s" (cdr pair) v) limits)))
       (when limits
         (format "Limits: %s\n" (mapconcat #'identity (nreverse limits) ", "))))
     (when pid (format "PID: %d\n" pid))
     (let ((tree (alist-get 'process-tree info)))
       (when tree
         (let ((count (plist-get tree :count))
               (pids (plist-get tree :pids)))
           (format "Process tree: %d descendant%s [%s]\n"
                   count (if (= count 1) "" "s")
                   (mapconcat (lambda (p) (format "%d" p))
                              pids ", ")))))
     (when start-time (format "Start time: %.3f\n" start-time))
     (when ready-time (format "Ready time: %.3f\n" ready-time))
     (when duration (format "Duration: %.3fs\n" duration))
     (let ((uptime (alist-get 'uptime info)))
       (when uptime (format "Uptime: %s\n"
                            (elinit--cli-format-duration uptime))))
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
     (let ((uf (alist-get 'unit-file info))
           (tier (alist-get 'authority-tier info)))
       (when uf (format "Unit file: %s%s\n" uf
                        (if tier (format " (tier %d)" tier) ""))))
     (let ((docs (alist-get 'documentation info)))
       (when docs
         (format "Documentation: %s\n"
                 (mapconcat #'identity docs ", "))))
     (let ((log-tail (alist-get 'log-tail info)))
       (when log-tail
         (format "\nRecent log:\n%s" log-tail))))))

(defun elinit--cli-describe-invalid-human (info)
  "Format invalid entry INFO as human-readable detail view."
  (let ((id (alist-get 'id info))
        (reason (alist-get 'reason info)))
    (concat
     (format "ID: %s\n" id)
     (format "Status: invalid\n")
     (format "Reason: %s\n" (or reason "-")))))

;;; CLI JSON Formatters

(defun elinit--cli-entry-to-json-obj (info)
  "Convert entry INFO alist to JSON-compatible alist."
  `((id . ,(alist-get 'id info))
    (type . ,(symbol-name (alist-get 'type info)))
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
    (authority_tier . ,(alist-get 'authority-tier info))
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
    (user . ,(alist-get 'user info))
    (group . ,(alist-get 'group info))
    (sandbox_enabled . ,(if (alist-get 'sandbox-enabled info) t :json-false))
    (sandbox_profile . ,(let ((p (alist-get 'sandbox-profile info)))
                          (if p (symbol-name p) "none")))
    (sandbox_network . ,(let ((n (alist-get 'sandbox-network info)))
                          (if n (symbol-name n) "shared")))
    (limit_nofile . ,(alist-get 'limit-nofile info))
    (limit_nproc . ,(alist-get 'limit-nproc info))
    (limit_core . ,(alist-get 'limit-core info))
    (limit_fsize . ,(alist-get 'limit-fsize info))
    (limit_as . ,(alist-get 'limit-as info))
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
    (process_tree . ,(let ((tree (alist-get 'process-tree info)))
                       (if tree
                           `((count . ,(plist-get tree :count))
                             (pids . ,(or (plist-get tree :pids) [])))
                         nil)))
    (description . ,(alist-get 'description info))
    (documentation . ,(or (alist-get 'documentation info) []))
    (log_tail . ,(alist-get 'log-tail info))))

(defun elinit--cli-invalid-to-json-obj (info)
  "Convert invalid entry INFO to JSON-compatible alist."
  `((id . ,(alist-get 'id info))
    (reason . ,(alist-get 'reason info))))

(defun elinit--cli-status-json (entries invalid)
  "Format ENTRIES and INVALID as JSON status object.
Empty lists are encoded as arrays, not null."
  (let ((obj `((entries . ,(elinit--cli-ensure-array
                            (mapcar #'elinit--cli-entry-to-json-obj entries)))
               (invalid . ,(elinit--cli-ensure-array
                            (mapcar #'elinit--cli-invalid-to-json-obj invalid))))))
    (json-encode obj)))

;;; CLI Subcommand Handlers

(defun elinit--cli-cmd-status (args json-p)
  "Handle `status [ID...]' command with ARGS.  JSON-P enables JSON output.
With IDs, show detailed status for each unit (valid or invalid).
Print output for found units, invalid detail for invalid units,
and append warnings for truly missing IDs (exit non-zero if any missing).
Without IDs, show overview table (same as `list-units')."
  (let ((unknown (elinit--cli-has-unknown-flags args)))
    (if unknown
        (elinit--cli-error elinit-cli-exit-invalid-args
                               (format "Unknown option: %s" unknown)
                               (if json-p 'json 'human))
      (if args
          ;; Detailed status for specific IDs
          (let* ((snapshot (elinit--build-snapshot))
                 (result (elinit--cli-all-entries-info snapshot))
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
                            . ,(elinit--cli-ensure-array
                                (mapcar #'elinit--cli-entry-to-json-obj
                                        valid-out)))
                           (invalid
                            . ,(elinit--cli-ensure-array
                                (mapcar #'elinit--cli-invalid-to-json-obj
                                        invalid-out)))
                           (not_found
                            . ,(elinit--cli-ensure-array missing))))
                      (concat
                       (when valid-out
                         (mapconcat #'elinit--cli-describe-human
                                    valid-out "\n"))
                       (when invalid-out
                         (concat
                          (when valid-out "\n")
                          (mapconcat #'elinit--cli-describe-invalid-human
                                     invalid-out "\n")))
                       (when missing
                         (concat
                          (when (or valid-out invalid-out) "\n")
                          (mapconcat
                           (lambda (id)
                             (format "Unit %s could not be found.\n" id))
                           missing ""))))))
                   (exitcode (if missing
                                 elinit-cli-exit-failure
                               elinit-cli-exit-success)))
              (elinit--cli-make-result
               exitcode (if json-p 'json 'human) detail)))
        ;; No IDs: overview table (same as list-units)
        (elinit--cli-cmd-list-units nil json-p)))))

(defun elinit--cli-cmd-list-units (args json-p)
  "Handle `list-units [ID...]' command with ARGS.  JSON-P enables JSON output.
Show overview status table for all or filtered entries.
When IDs are specified, both valid and invalid rows are filtered."
  (let ((unknown (elinit--cli-has-unknown-flags args)))
    (if unknown
        (elinit--cli-error elinit-cli-exit-invalid-args
                               (format "Unknown option: %s" unknown)
                               (if json-p 'json 'human))
      (let* ((snapshot (elinit--build-snapshot))
             (result (elinit--cli-all-entries-info snapshot))
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
        (elinit--cli-success
         (if json-p
             (elinit--cli-status-json entries invalid)
           (elinit--cli-status-human entries invalid))
         (if json-p 'json 'human))))))

(defun elinit--cli-cmd-show (args json-p)
  "Handle `show ID' command with ARGS.  JSON-P enables JSON output.
Show all properties of a single unit."
  (cond
   ((null args)
    (elinit--cli-error elinit-cli-exit-invalid-args
                           "show requires an ID argument"
                           (if json-p 'json 'human)))
   ((cdr args)
    (elinit--cli-error elinit-cli-exit-invalid-args
                           (format "show takes exactly one ID, got: %s"
                                   (mapconcat #'identity args " "))
                           (if json-p 'json 'human)))
   (t
    (let* ((id (car args))
           (snapshot (elinit--build-snapshot))
           (result (elinit--cli-all-entries-info snapshot))
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
        (elinit--cli-success
         (if json-p
             (json-encode (elinit--cli-entry-to-json-obj info))
           (elinit--cli-describe-human info))
         (if json-p 'json 'human)))
       (inv
        (elinit--cli-success
         (if json-p
             (json-encode (elinit--cli-invalid-to-json-obj inv))
           (elinit--cli-describe-invalid-human inv))
         (if json-p 'json 'human)))
       (t
        (elinit--cli-error elinit-cli-exit-failure
                               (format "No entry with ID '%s'" id)
                               (if json-p 'json 'human))))))))

(defun elinit--cli-cmd-verify (args json-p)
  "Handle `verify' command with ARGS.  JSON-P enables JSON output."

  (let ((extra-err (elinit--cli-reject-extra-args args json-p)))
    (if extra-err extra-err
      (let* ((plan (elinit--build-plan (elinit--effective-programs)))
             (service-valid (length (elinit-plan-entries plan)))
             (combined-invalid (make-hash-table :test 'equal))
             (service-invalid-list nil))
        ;; Merge plan invalids and unit-file invalids
        (maphash (lambda (k v) (puthash k v combined-invalid))
                 (elinit-plan-invalid plan))
        (when (boundp 'elinit--unit-file-invalid)
          (maphash (lambda (k v) (puthash k v combined-invalid))
                   (symbol-value 'elinit--unit-file-invalid)))
        (maphash (lambda (id reason)
                   (push `((id . ,id) (reason . ,reason)) service-invalid-list))
                 combined-invalid)
        ;; Sort invalid list by ID for deterministic output
        (setq service-invalid-list
              (sort service-invalid-list
                    (lambda (a b)
                      (string< (alist-get 'id a) (alist-get 'id b)))))
        ;; Validate timers
        (let* ((timers (elinit-timer-build-list plan))
               (timer-valid (length timers))
               (timer-invalid-list nil))
          (maphash (lambda (id reason)
                     (push `((id . ,id) (reason . ,reason)) timer-invalid-list))
                   elinit--invalid-timers)
          (setq timer-invalid-list
                (sort timer-invalid-list
                      (lambda (a b)
                        (string< (alist-get 'id a) (alist-get 'id b)))))
          (let ((service-invalid (length service-invalid-list))
                (timer-invalid (length timer-invalid-list))
                (has-errors (or (> (length service-invalid-list) 0)
                                (> (length timer-invalid-list) 0))))
            (if json-p
                (elinit--cli-make-result
                 (if has-errors elinit-cli-exit-validation-failed
                   elinit-cli-exit-success)
                 'json
                 (json-encode
                  `((services . ((valid . ,service-valid)
                                 (invalid . ,service-invalid)
                                 (errors . ,(elinit--cli-ensure-array
                                             (mapcar #'elinit--cli-invalid-to-json-obj
                                                     service-invalid-list)))))
                    (timers . ((valid . ,timer-valid)
                               (invalid . ,timer-invalid)
                               (errors . ,(elinit--cli-ensure-array
                                           (mapcar #'elinit--cli-invalid-to-json-obj
                                                   timer-invalid-list))))))))
              (elinit--cli-make-result
               (if has-errors elinit-cli-exit-validation-failed
                 elinit-cli-exit-success)
               'human
               (concat
                (format "Services: %d valid, %d invalid\n" service-valid service-invalid)
                (mapconcat (lambda (e)
                             (format "  INVALID %s: %s\n"
                                     (alist-get 'id e)
                                     (alist-get 'reason e)))
                           service-invalid-list "")
                (when elinit-timers
                  (concat
                   (format "Timers: %d valid, %d invalid\n" timer-valid timer-invalid)
                   (mapconcat (lambda (e)
                                (format "  INVALID %s: %s\n"
                                        (alist-get 'id e)
                                        (alist-get 'reason e)))
                              timer-invalid-list ""))))))))))))

(defun elinit--cli-cmd-start (args json-p)
  "Handle `start [--target T] [-- ID...]' command with ARGS.
JSON-P enables JSON output.  Use -- before IDs that start with a hyphen.
When --target is specified with no IDs, override the startup root for
that invocation only."
  (let* ((split (elinit--cli-split-at-separator args))
         (before (car split))
         (after (cdr split))
         (parsed (elinit--cli-parse-option before "--target"))
         (target-val (plist-get parsed :value))
         (target-missing (plist-get parsed :missing))
         (target-dup (plist-get parsed :duplicate))
         (before-positional (plist-get parsed :positional))
         (unknown (elinit--cli-has-unknown-flags
                   before-positional '("--target")))
         (ids (append before-positional after)))
    (cond
     (unknown
      (elinit--cli-error elinit-cli-exit-invalid-args
                             (format "Unknown option: %s" unknown)
                             (if json-p 'json 'human)))
     (target-dup
      (elinit--cli-error elinit-cli-exit-invalid-args
                             "--target specified multiple times"
                             (if json-p 'json 'human)))
     (target-missing
      (elinit--cli-error elinit-cli-exit-invalid-args
                             "--target requires a value"
                             (if json-p 'json 'human)))
     ((and target-val ids)
      (elinit--cli-error elinit-cli-exit-invalid-args
                             "--target cannot be combined with specific IDs"
                             (if json-p 'json 'human)))
     ((and target-val (not (string-suffix-p ".target" target-val)))
      (elinit--cli-error elinit-cli-exit-invalid-args
                             (format "'%s' is not a target (must end in .target)"
                                     target-val)
                             (if json-p 'json 'human)))
     (ids
      ;; Start specific entries
      (let ((started nil)
            (skipped nil)
            (errors nil))
        (dolist (id ids)
          (let ((result (elinit--manual-start id)))
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
              (elinit--cli-make-result elinit-cli-exit-failure
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
            (elinit--cli-success
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
      ;; Start all via elinit-start, optionally with --target override
      (if target-val
          ;; Pre-validate target exists before calling elinit-start
          (let* ((plan (elinit--build-plan
                        (elinit--effective-programs)))
                 (entries (elinit-plan-entries plan))
                 (found (cl-find target-val entries
                                 :key #'elinit-entry-id
                                 :test #'equal)))
            (cond
             ((not found)
              (elinit--cli-error
               elinit-cli-exit-no-such-unit
               (format "Target '%s' does not exist" target-val)
               (if json-p 'json 'human)))
             ((not (eq (elinit-entry-type found) 'target))
              (elinit--cli-error
               elinit-cli-exit-invalid-args
               (format "'%s' exists but is not a target unit" target-val)
               (if json-p 'json 'human)))
             (t
              (let ((elinit-default-target target-val))
                (elinit-start))
              (elinit--cli-success
               (if json-p
                   (json-encode `((message . "Elinit started")
                                  (target . ,target-val)))
                 (format "Elinit started (target: %s)\n" target-val))
               (if json-p 'json 'human)))))
        (elinit-start)
        (elinit--cli-success
         (if json-p
             (json-encode '((message . "Elinit started")))
           "Elinit started\n")
         (if json-p 'json 'human)))))))

(defun elinit--cli-stop-all-gracefully ()
  "Stop all managed processes and wait for completion.
Returns non-nil when graceful shutdown completed before timeout.
This is used by CLI `stop' and `restart' for systemctl-like stop behavior."
  (let ((deadline (+ (float-time) elinit-shutdown-timeout 1.0)))
    (setq elinit--shutdown-complete-flag nil)
    (elinit-stop)
    (while (and (not elinit--shutdown-complete-flag)
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    elinit--shutdown-complete-flag))

(defun elinit--cli-cmd-stop (args json-p)
  "Handle `stop [-- ID...]' command with ARGS.  JSON-P enables JSON output.
Use -- before IDs that start with a hyphen."
  (let ((unknown (elinit--cli-has-unknown-flags args)))
    (if unknown
        (elinit--cli-error elinit-cli-exit-invalid-args
                               (format "Unknown option: %s" unknown)
                               (if json-p 'json 'human))
      (let ((args (elinit--cli-strip-separator args)))
        (cond
         (args
          ;; Stop specific entries
          (let ((stopped nil)
                (skipped nil))
            (dolist (id args)
              (let ((result (elinit--manual-stop id)))
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
              (elinit--cli-success
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
          (unless (elinit--cli-stop-all-gracefully)
            (elinit-stop-now))
          (elinit--cli-success
           (if json-p
               (json-encode '((message . "Elinit stopped")))
             "Elinit stopped\n")
           (if json-p 'json 'human))))))))

(defun elinit--cli-cmd-restart (args json-p)
  "Handle `restart [-- ID...]' command with ARGS.  JSON-P enables JSON output.
Use -- before IDs that start with a hyphen."
  (let ((unknown (elinit--cli-has-unknown-flags args)))
    (if unknown
        (elinit--cli-error elinit-cli-exit-invalid-args
                               (format "Unknown option: %s" unknown)
                               (if json-p 'json 'human))
      (let ((args (elinit--cli-strip-separator args)))
        (cond
         (args
          ;; Restart specific entries
          (let ((restarted nil)
                (skipped nil)
                (errors nil))
            (dolist (id args)
              ;; Stop first if running (ignore result, we just want it stopped)
              (elinit--manual-stop id)
              ;; Then start
              (let ((result (elinit--manual-start id)))
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
                  (elinit--cli-make-result elinit-cli-exit-failure
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
                (elinit--cli-success
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
          (unless (elinit--cli-stop-all-gracefully)
            (elinit-stop-now))
          (elinit-start)
          (elinit--cli-success
           (if json-p
               (json-encode '((message . "Elinit restarted")))
             "Elinit restarted\n")
           (if json-p 'json 'human))))))))

(defun elinit--cli-cmd-daemon-reload (args json-p)
  "Handle `daemon-reload' command with ARGS.  JSON-P enables JSON output.
Reload unit definitions without affecting runtime state."
  (let ((extra-err (elinit--cli-reject-extra-args args json-p)))
    (if extra-err extra-err
      (let ((result (elinit-daemon-reload)))
        (elinit--cli-success
         (if json-p
             (json-encode `((reloaded . t)
                            (entries . ,(plist-get result :entries))
                            (invalid . ,(plist-get result :invalid))))
           (format "Reloaded: %d entries, %d invalid\n"
                   (plist-get result :entries)
                   (plist-get result :invalid)))
         (if json-p 'json 'human))))))

(defun elinit--cli-cmd-reload (args json-p)
  "Handle `reload [--] ID...' command with ARGS.  JSON-P enables JSON output.
Hot-reload specific units: re-reads config and restarts running units.
Use -- before IDs that start with a hyphen."
  (let ((unknown (elinit--cli-has-unknown-flags args)))
    (if unknown
        (elinit--cli-error elinit-cli-exit-invalid-args
                               (format "Unknown option: %s" unknown)
                               (if json-p 'json 'human))
      (let ((args (elinit--cli-strip-separator args)))
        (cond
         ((null args)
          (elinit--cli-error elinit-cli-exit-invalid-args
                                 "reload requires at least one ID"
                                 (if json-p 'json 'human)))
         (t
          (let ((results nil)
                (has-error nil))
            (dolist (id args)
              (let ((r (elinit--reload-unit id)))
                (push r results)
                (when (string-prefix-p "error:" (plist-get r :action))
                  (setq has-error t))))
            (setq results (nreverse results))
            (elinit--maybe-refresh-dashboard)
            (let ((msg (mapconcat
                        (lambda (r)
                          (format "%s: %s\n"
                                  (plist-get r :id) (plist-get r :action)))
                        results "")))
              (if has-error
                  (elinit--cli-make-result
                   elinit-cli-exit-failure
                   (if json-p 'json 'human)
                   (if json-p
                       (json-encode
                        `((results
                           . ,(mapcar (lambda (r)
                                        `((id . ,(plist-get r :id))
                                          (action . ,(plist-get r :action))))
                                      results))))
                     msg))
                (elinit--cli-success
                 (if json-p
                     (json-encode
                      `((results
                         . ,(mapcar (lambda (r)
                                      `((id . ,(plist-get r :id))
                                        (action . ,(plist-get r :action))))
                                    results))))
                   msg)
                 (if json-p 'json 'human)))))))))))

(defun elinit--cli-cmd-enable (args json-p)
  "Handle `enable [--] ID...' command with ARGS.  JSON-P enables JSON output.
Use -- before IDs that start with a hyphen."
  (let ((unknown (elinit--cli-has-unknown-flags args)))
    (if unknown
        (elinit--cli-error elinit-cli-exit-invalid-args
                               (format "Unknown option: %s" unknown)
                               (if json-p 'json 'human))
      (let ((args (elinit--cli-strip-separator args)))
        (cond
         ((null args)
          (elinit--cli-error elinit-cli-exit-invalid-args
                                 "enable requires at least one ID"
                                 (if json-p 'json 'human)))
         (t
          (elinit--cli-policy-batch
           args #'elinit--policy-enable "Enabled" json-p)))))))

(defun elinit--cli-cmd-disable (args json-p)
  "Handle `disable [--] ID...' command with ARGS.  JSON-P enables JSON output.
Use -- before IDs that start with a hyphen."
  (let ((unknown (elinit--cli-has-unknown-flags args)))
    (if unknown
        (elinit--cli-error elinit-cli-exit-invalid-args
                               (format "Unknown option: %s" unknown)
                               (if json-p 'json 'human))
      (let ((args (elinit--cli-strip-separator args)))
        (cond
         ((null args)
          (elinit--cli-error elinit-cli-exit-invalid-args
                                 "disable requires at least one ID"
                                 (if json-p 'json 'human)))
         (t
          (elinit--cli-policy-batch
           args #'elinit--policy-disable "Disabled" json-p)))))))

(defun elinit--cli-cmd-mask (args json-p)
  "Handle `mask [--] ID...' command with ARGS.  JSON-P enables JSON output.
Use -- before IDs that start with a hyphen."
  (let ((unknown (elinit--cli-has-unknown-flags args)))
    (if unknown
        (elinit--cli-error elinit-cli-exit-invalid-args
                               (format "Unknown option: %s" unknown)
                               (if json-p 'json 'human))
      (let ((args (elinit--cli-strip-separator args)))
        (cond
         ((null args)
          (elinit--cli-error elinit-cli-exit-invalid-args
                                 "mask requires at least one ID"
                                 (if json-p 'json 'human)))
         (t
          (elinit--cli-policy-batch
           args #'elinit--policy-mask "Masked" json-p)))))))

(defun elinit--cli-cmd-unmask (args json-p)
  "Handle `unmask [--] ID...' command with ARGS.  JSON-P enables JSON output.
Use -- before IDs that start with a hyphen."
  (let ((unknown (elinit--cli-has-unknown-flags args)))
    (if unknown
        (elinit--cli-error elinit-cli-exit-invalid-args
                               (format "Unknown option: %s" unknown)
                               (if json-p 'json 'human))
      (let ((args (elinit--cli-strip-separator args)))
        (cond
         ((null args)
          (elinit--cli-error elinit-cli-exit-invalid-args
                                 "unmask requires at least one ID"
                                 (if json-p 'json 'human)))
         (t
          (elinit--cli-policy-batch
           args #'elinit--policy-unmask "Unmasked" json-p)))))))

(defun elinit--cli-cmd-is-active (args json-p)
  "Handle `is-active ID' command with ARGS.  JSON-P enables JSON output.
Exit 0 if the unit is active (running or latched), exit 3 if not active,
exit 4 if no such unit.  Uses systemctl-compatible exit code semantics."
  (cond
   ((null args)
    (elinit--cli-error elinit-cli-exit-invalid-args
                           "is-active requires an ID argument"
                           (if json-p 'json 'human)))
   ((cdr args)
    (elinit--cli-error elinit-cli-exit-invalid-args
                           (format "is-active takes exactly one ID, got: %s"
                                   (mapconcat #'identity args " "))
                           (if json-p 'json 'human)))
   (t
    (let* ((id (car args))
           (snapshot (elinit--build-snapshot))
           (result (elinit--cli-all-entries-info snapshot))
           (entries (car result))
           (info (cl-find id entries
                          :key (lambda (e) (alist-get 'id e))
                          :test #'equal)))
      (if (not info)
          (elinit--cli-make-result
           elinit-cli-exit-no-such-unit
           (if json-p 'json 'human)
           (if json-p
               (json-encode `((id . ,id) (active . :json-false)
                              (status . "inactive")))
             (format "inactive\n")))
        (let* ((status (alist-get 'status info))
               (active (or (equal status "running")
                           (equal status "active")))
               (exitcode (if active
                             elinit-cli-exit-success
                           elinit-cli-exit-not-active)))
          (elinit--cli-make-result
           exitcode
           (if json-p 'json 'human)
           (if json-p
               (json-encode `((id . ,id)
                              (active . ,(if active t :json-false))
                              (status . ,status)))
             (format "%s\n" status)))))))))

(defun elinit--cli-cmd-is-enabled (args json-p)
  "Handle `is-enabled ID' command with ARGS.  JSON-P enables JSON output.
Exit 0 if the unit is enabled, exit 1 if disabled or masked,
exit 4 if no such unit.  Uses systemctl-compatible exit codes."
  (cond
   ((null args)
    (elinit--cli-error elinit-cli-exit-invalid-args
                           "is-enabled requires an ID argument"
                           (if json-p 'json 'human)))
   ((cdr args)
    (elinit--cli-error elinit-cli-exit-invalid-args
                           (format "is-enabled takes exactly one ID, got: %s"
                                   (mapconcat #'identity args " "))
                           (if json-p 'json 'human)))
   (t
    (let* ((id (car args))
           (snapshot (elinit--build-snapshot))
           (result (elinit--cli-all-entries-info snapshot))
           (entries (car result))
           (info (cl-find id entries
                          :key (lambda (e) (alist-get 'id e))
                          :test #'equal)))
      (if (not info)
          (elinit--cli-make-result
           elinit-cli-exit-no-such-unit
           (if json-p 'json 'human)
           (if json-p
               (json-encode `((id . ,id) (enabled . :json-false)
                              (state . "not-found")))
             (format "not-found\n")))
        (let* ((is-masked (eq (gethash id elinit--mask-override) 'masked))
               (enabled (alist-get 'enabled info))
               (state (cond (is-masked "masked")
                            (enabled "enabled")
                            (t "disabled")))
               (exitcode (if enabled
                             elinit-cli-exit-success
                           elinit-cli-exit-failure)))
          (elinit--cli-make-result
           exitcode
           (if json-p 'json 'human)
           (if json-p
               (json-encode `((id . ,id)
                              (enabled . ,(if enabled t :json-false))
                              (state . ,state)))
             (format "%s\n" state)))))))))

(defun elinit--cli-cmd-is-failed (args json-p)
  "Handle `is-failed ID' command with ARGS.  JSON-P enables JSON output.
Exit 0 if the unit is in a failed state, exit 1 if not failed,
exit 4 if no such unit.  Uses systemctl-compatible exit codes."
  (cond
   ((null args)
    (elinit--cli-error elinit-cli-exit-invalid-args
                           "is-failed requires an ID argument"
                           (if json-p 'json 'human)))
   ((cdr args)
    (elinit--cli-error elinit-cli-exit-invalid-args
                           (format "is-failed takes exactly one ID, got: %s"
                                   (mapconcat #'identity args " "))
                           (if json-p 'json 'human)))
   (t
    (let* ((id (car args))
           (snapshot (elinit--build-snapshot))
           (result (elinit--cli-all-entries-info snapshot))
           (entries (car result))
           (info (cl-find id entries
                          :key (lambda (e) (alist-get 'id e))
                          :test #'equal)))
      (if (not info)
          (elinit--cli-make-result
           elinit-cli-exit-no-such-unit
           (if json-p 'json 'human)
           (if json-p
               (json-encode `((id . ,id) (failed . :json-false)
                              (status . "inactive")))
             (format "inactive\n")))
        (let* ((status (alist-get 'status info))
               (is-failed (member status '("dead" "failed")))
               (exitcode (if is-failed
                             elinit-cli-exit-success
                           elinit-cli-exit-failure)))
          (elinit--cli-make-result
           exitcode
           (if json-p 'json 'human)
           (if json-p
               (json-encode `((id . ,id)
                              (failed . ,(if is-failed t :json-false))
                              (status . ,status)))
             (format "%s\n" status)))))))))

(defun elinit--cli-cmd-reset-failed (args json-p)
  "Handle `reset-failed [--] [ID...]' with ARGS.  JSON-P enables JSON output.
With IDs: reset failed state for those entries.
Without IDs: reset all failed entries.
Use -- before IDs that start with a hyphen."
  (let ((unknown (elinit--cli-has-unknown-flags args)))
    (if unknown
        (elinit--cli-error elinit-cli-exit-invalid-args
                               (format "Unknown option: %s" unknown)
                               (if json-p 'json 'human))
      (let* ((ids (elinit--cli-strip-separator args))
             (targets (if ids
                         ids
                       ;; No IDs: collect all failed entries
                       (let ((all nil))
                         (maphash (lambda (k _v) (push k all))
                                  elinit--failed)
                         (sort all #'string<)))))
        (if (null targets)
            (elinit--cli-success
             (if json-p
                 (json-encode '((reset . []) (skipped . [])))
               "No failed units\n")
             (if json-p 'json 'human))
          (let ((reset-ids nil)
                (skipped nil))
            (dolist (id targets)
              (let ((result (elinit--reset-failed id)))
                (pcase (plist-get result :status)
                  ('reset (push id reset-ids))
                  ('skipped (push (cons id (plist-get result :reason)) skipped)))))
            (setq reset-ids (nreverse reset-ids)
                  skipped (nreverse skipped))
            (elinit--maybe-refresh-dashboard)
            (elinit--cli-success
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

(defun elinit--cli-cmd-restart-policy (args json-p)
  "Handle `restart-policy POLICY [--] ID...' with ARGS.  JSON-P for JSON.
POLICY is one of: no, on-success, on-failure, always.
Use -- before IDs that start with a hyphen."
  (let ((unknown (elinit--cli-has-unknown-flags args)))
    (if unknown
        (elinit--cli-error elinit-cli-exit-invalid-args
                               (format "Unknown option: %s" unknown)
                               (if json-p 'json 'human))
      (let ((args (elinit--cli-strip-separator args)))
        (cond
         ((< (length args) 2)
          (elinit--cli-error elinit-cli-exit-invalid-args
                                 "restart-policy requires (no|on-success|on-failure|always) and at least one ID"
                                 (if json-p 'json 'human)))
         (t
          (let* ((policy-str (car args))
                 (ids (cdr args))
                 (policy (intern policy-str)))
            (if (not (memq policy elinit--valid-restart-policies))
                (elinit--cli-error
                 elinit-cli-exit-invalid-args
                 (format "Invalid restart policy: %s (must be no, on-success, on-failure, or always)"
                         policy-str)
                 (if json-p 'json 'human))
              (elinit--cli-policy-batch
               ids
               (lambda (id) (elinit--policy-set-restart id policy))
               (format "Restart policy %s" policy-str)
               json-p)))))))))

(defun elinit--cli-cmd-logging (args json-p)
  "Handle `logging (on|off) [--] ID...' with ARGS.  JSON-P for JSON.
Use -- before IDs that start with a hyphen."
  (let ((unknown (elinit--cli-has-unknown-flags args)))
    (if unknown
        (elinit--cli-error elinit-cli-exit-invalid-args
                               (format "Unknown option: %s" unknown)
                               (if json-p 'json 'human))
      (let ((args (elinit--cli-strip-separator args)))
        (cond
         ((< (length args) 2)
          (elinit--cli-error elinit-cli-exit-invalid-args
                                 "logging requires (on|off) and at least one ID"
                                 (if json-p 'json 'human)))
         (t
          (let* ((policy (car args))
                 (ids (cdr args))
                 (enabled-p (cond ((equal policy "on") 'on)
                                  ((equal policy "off") 'off)
                                  (t nil))))
            (if (null enabled-p)
                (elinit--cli-error elinit-cli-exit-invalid-args
                                       "logging requires 'on' or 'off'"
                                       (if json-p 'json 'human))
              (elinit--cli-policy-batch
               ids
               (lambda (id)
                 (elinit--policy-set-logging id (eq enabled-p 'on)))
               (format "Logging %s" policy)
               json-p)))))))))

(defun elinit--cli-cmd-blame (args json-p)
  "Handle `blame' command with ARGS.  JSON-P enables JSON output."
  (let ((extra-err (elinit--cli-reject-extra-args args json-p)))
    (if extra-err extra-err
      (let ((blame-data nil))
        (maphash (lambda (id start-time)
                   (let ((ready-time (gethash id elinit--ready-times)))
                     (push `((id . ,id)
                             (start_time . ,start-time)
                             (ready_time . ,ready-time)
                             (duration . ,(when ready-time (- ready-time start-time))))
                           blame-data)))
                 elinit--start-times)
        ;; Sort by start time for deterministic output
        (setq blame-data (sort blame-data
                               (lambda (a b)
                                 (< (alist-get 'start_time a)
                                    (alist-get 'start_time b)))))
        (elinit--cli-success
         (if json-p
             (json-encode `((blame . ,(elinit--cli-ensure-array blame-data))))
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

(defun elinit--cli-cmd-list-dependencies (args json-p)
  "Handle `list-dependencies [ID]' command with ARGS.  JSON-P for JSON."
  (cond
   ((> (length args) 1)
    (elinit--cli-error elinit-cli-exit-invalid-args
                           (format "list-dependencies takes at most one ID, got: %s"
                                   (mapconcat #'identity args " "))
                           (if json-p 'json 'human)))
   ((and args (string-prefix-p "-" (car args)))
    (elinit--cli-error elinit-cli-exit-invalid-args
                           (format "Unknown option: %s" (car args))
                           (if json-p 'json 'human)))
   (t
    (let* ((plan (elinit--build-plan (elinit--effective-programs)))
           (deps (elinit-plan-deps plan))
           (requires (elinit-plan-requires-deps plan))
           (dependents (elinit-plan-dependents plan))
           (id (car args)))
      (if id
          ;; Graph for single ID
          (let ((after-deps (gethash id deps))
                (req-deps (gethash id requires))
                (blocks (gethash id dependents)))
            (elinit--cli-success
             (if json-p
                 (json-encode `((id . ,id)
                                (after . ,(elinit--cli-ensure-array after-deps))
                                (requires . ,(elinit--cli-ensure-array req-deps))
                                (blocks . ,(elinit--cli-ensure-array blocks))))
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
          (elinit--cli-success
           (if json-p
               (json-encode `((edges . ,(elinit--cli-ensure-array edges))))
             (concat "Dependency Graph (dep -> dependent):\n"
                     (mapconcat (lambda (e)
                                  (format "  %s -> %s\n" (car e) (cadr e)))
                                edges "")))
           (if json-p 'json 'human))))))))

(defun elinit--cli-cmd-logs (args json-p)
  "Handle `logs [--tail N] [--] ID' command with ARGS.  JSON-P for JSON.
Options must come before --.  Use -- before IDs that start with hyphen."
  ;; Split at -- separator: options before, ID after
  (let* ((split (elinit--cli-split-at-separator args))
         (before (car split))
         (after (cdr split))
         ;; Check for unknown flags in before-part only
         (unknown (elinit--cli-has-unknown-flags before '("--tail")))
         ;; Parse --tail by position (not value) to avoid collision bugs
         (parsed (elinit--cli-parse-option before "--tail"))
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
      (elinit--cli-error elinit-cli-exit-invalid-args
                             (format "Unknown option: %s" unknown)
                             (if json-p 'json 'human)))
     (tail-duplicate
      (elinit--cli-error elinit-cli-exit-invalid-args
                             "--tail specified multiple times"
                             (if json-p 'json 'human)))
     (tail-missing
      (elinit--cli-error elinit-cli-exit-invalid-args
                             "--tail requires a numeric value"
                             (if json-p 'json 'human)))
     (tail-invalid
      (elinit--cli-error elinit-cli-exit-invalid-args
                             (format "--tail value must be a number, got: %s" tail-val)
                             (if json-p 'json 'human)))
     ((null id)
      (elinit--cli-error elinit-cli-exit-invalid-args
                             "logs requires an ID argument"
                             (if json-p 'json 'human)))
     ;; Reject if first positional in before-part looks like an option
     ((and (null after) before-positional (string-prefix-p "-" (car before-positional)))
      (elinit--cli-error elinit-cli-exit-invalid-args
                             "logs requires ID as first argument (use -- for IDs starting with -)"
                             (if json-p 'json 'human)))
     ;; Reject extra IDs
     (extra-ids
      (elinit--cli-error elinit-cli-exit-invalid-args
                             (format "logs takes exactly one ID, got extra: %s"
                                     (mapconcat #'identity extra-ids " "))
                             (if json-p 'json 'human)))
     ;; Target units have no logs
     ((let ((entry (elinit--get-entry-for-id id)))
        (and entry (eq (elinit-entry-type entry) 'target)))
      (elinit--cli-error elinit-cli-exit-failure
                             (format "Cannot show logs for target unit: %s" id)
                             (if json-p 'json 'human)))
     (t
      (let ((log-file (elinit--log-file id)))
        (if (file-exists-p log-file)
            (let ((content (with-temp-buffer
                             (insert-file-contents log-file)
                             (let ((lines (split-string (buffer-string) "\n" t)))
                               (if (> (length lines) tail-n)
                                   (mapconcat #'identity (last lines tail-n) "\n")
                                 (buffer-string))))))
              (elinit--cli-success
               (if json-p
                   (json-encode `((id . ,id) (log_file . ,log-file) (content . ,content)))
                 (concat (format "=== Log for %s (%s) ===\n" id log-file)
                         content "\n"))
               (if json-p 'json 'human)))
          (elinit--cli-error elinit-cli-exit-failure
                                 (format "No log file for '%s'" id)
                                 (if json-p 'json 'human))))))))

(defun elinit--cli-parse-journal-args (args)
  "Parse journal command ARGS into a plist.
Recognized flags: -u/--unit ID, -n N, -p PRIORITY, --since TS,
--until TS, -f/--follow.  Combined -fu ID is supported.
Return plist with :unit, :lines, :priority, :since, :until,
:follow, :unknown."
  (let (unit lines priority since until follow unknown
        (rest args))
    (while rest
      (let ((arg (car rest)))
        (cond
         ;; -fu ID (combined short form, exact match only)
         ((equal arg "-fu")
          (setq follow t)
          (let ((next (cadr rest)))
            (if (or (null next) (string-prefix-p "-" next))
                (setq unknown (or unknown (format "-fu requires a unit ID")))
              (setq unit next)
              (setq rest (cdr rest)))))
         ;; -u or --unit
         ((or (equal arg "-u") (equal arg "--unit"))
          (let ((next (cadr rest)))
            (if (or (null next) (string-prefix-p "-" next))
                (setq unknown (or unknown "--unit requires an argument"))
              (setq unit next)
              (setq rest (cdr rest)))))
         ;; -n (line limit)
         ((equal arg "-n")
          (let ((next (cadr rest)))
            (if (or (null next) (not (string-match-p "\\`[0-9]+\\'" next)))
                (setq unknown (or unknown "-n requires a numeric value"))
              (setq lines (string-to-number next))
              (setq rest (cdr rest)))))
         ;; -p (priority filter)
         ((equal arg "-p")
          (let ((next (cadr rest)))
            (if (or (null next)
                    (not (member next '("err" "info"))))
                (setq unknown (or unknown "-p must be err or info"))
              (setq priority (intern next))
              (setq rest (cdr rest)))))
         ;; --since
         ((equal arg "--since")
          (let ((next (cadr rest)))
            (if (null next)
                (setq unknown (or unknown "--since requires a timestamp"))
              (setq since next)
              (setq rest (cdr rest)))))
         ;; --until
         ((equal arg "--until")
          (let ((next (cadr rest)))
            (if (null next)
                (setq unknown (or unknown "--until requires a timestamp"))
              (setq until next)
              (setq rest (cdr rest)))))
         ;; -f or --follow
         ((member arg '("-f" "--follow"))
          (setq follow t))
         ;; Unknown flags
         ((string-prefix-p "-" arg)
          (setq unknown (or unknown (format "Unknown option: %s" arg))))
         ;; Bare positional args are not accepted
         (t
          (setq unknown (or unknown
                             (format "Unexpected argument: %s" arg)))))
        (setq rest (cdr rest))))
    (list :unit unit :lines lines :priority priority
          :since since :until until :follow follow :unknown unknown)))

(defun elinit--cli-journal-json-envelope
    (unit decoded since-str until-str priority n follow records)
  "Build JSON envelope for journal response.
UNIT is the service ID.  DECODED is the decode-file result plist.
SINCE-STR and UNTIL-STR are the raw timestamp strings or nil.
PRIORITY is the filter symbol or nil.  N is the line limit or nil.
FOLLOW is non-nil when follow mode is active.  RECORDS is the
filtered record list."
  (json-encode
   `((unit . ,unit)
     (format . ,(symbol-name (plist-get decoded :format)))
     (since . ,since-str)
     (until . ,until-str)
     (priority . ,(when priority (symbol-name priority)))
     (limit . ,n)
     (follow . ,(if follow t :json-false))
     (records .
      ,(vconcat
        (mapcar
         (lambda (r)
           `((ts . ,(plist-get r :ts))
             (unit . ,(plist-get r :unit))
             (pid . ,(plist-get r :pid))
             (stream . ,(symbol-name (plist-get r :stream)))
             (event . ,(symbol-name (plist-get r :event)))
             (status . ,(when (plist-get r :status)
                          (symbol-name (plist-get r :status))))
             (code . ,(plist-get r :code))
             (payload . ,(plist-get r :payload))
             (priority . ,(symbol-name
                           (elinit--log-record-priority r)))))
         records))))))

;;;; Follow session management

(defun elinit--cli-follow-start (unit log-file offset since-ts
                                          until-ts priority json-p)
  "Start a follow session for UNIT reading LOG-FILE from OFFSET.
SINCE-TS, UNTIL-TS, PRIORITY, and JSON-P configure record filtering
and formatting.  Return a plist (:session-id STRING :follow-file STRING)."
  (let* ((session-id (format "follow-%s-%.6f" unit (float-time)))
         (log-dir (file-name-directory log-file))
         (follow-file (expand-file-name
                       (concat ".follow-" session-id) log-dir))
         (session (list :unit unit
                        :log-file log-file
                        :offset offset
                        :since-ts since-ts
                        :until-ts until-ts
                        :priority priority
                        :json-p json-p
                        :follow-file follow-file
                        :timer nil
                        :last-activity (float-time))))
    ;; Create follow file
    (with-temp-file follow-file
      (insert ""))
    (puthash session-id session elinit--cli-follow-sessions)
    (elinit--cli-follow-schedule-poll session-id)
    (list :session-id session-id :follow-file follow-file)))

(defun elinit--cli-follow-schedule-poll (session-id)
  "Schedule next poll for SESSION-ID."
  (let ((session (gethash session-id elinit--cli-follow-sessions)))
    (when session
      (let ((timer (run-at-time elinit-log-follow-interval nil
                                #'elinit--cli-follow-poll session-id)))
        (plist-put session :timer timer)))))

(defun elinit--cli-follow-poll (session-id)
  "Poll for new log records in follow SESSION-ID.
Append formatted records to the follow file and reschedule."
  (let ((session (gethash session-id elinit--cli-follow-sessions)))
    (when session
      (let ((follow-file (plist-get session :follow-file))
            (log-file (plist-get session :log-file))
            (offset (plist-get session :offset))
            (since-ts (plist-get session :since-ts))
            (until-ts (plist-get session :until-ts))
            (priority (plist-get session :priority))
            (json-p (plist-get session :json-p))
            (last-activity (plist-get session :last-activity)))
        (cond
         ;; Guard: follow file deleted externally
         ((not (file-exists-p follow-file))
          (elinit--cli-follow-stop session-id))
         ;; Guard: session inactive too long
         ((> (- (float-time) last-activity)
             elinit-cli-follow-max-age)
          (elinit--cli-follow-stop session-id))
         (t
          (let* ((decoded (elinit--log-decode-file log-file nil offset))
                 (new-records (plist-get decoded :records))
                 (new-offset (plist-get decoded :offset)))
            (when new-records
              (let* ((filtered (elinit--log-filter-records
                                new-records since-ts until-ts priority))
                     (text (mapconcat
                            (if json-p
                                #'elinit--log-record-to-json
                              #'elinit--log-format-record-human)
                            filtered "\n")))
                (when (and filtered (> (length text) 0))
                  (append-to-file (concat text "\n") nil follow-file)))
              ;; Reset activity timer only when log produced new records
              (plist-put session :last-activity (float-time)))
            (plist-put session :offset new-offset)
            (elinit--cli-follow-schedule-poll session-id))))))))

(defun elinit--cli-follow-stop (session-id)
  "Stop follow SESSION-ID.
Cancel timer, delete follow file, remove from sessions table.
Return t if session existed, nil otherwise."
  (let ((session (gethash session-id elinit--cli-follow-sessions)))
    (when session
      (let ((timer (plist-get session :timer))
            (follow-file (plist-get session :follow-file)))
        (when timer (cancel-timer timer))
        (when (and follow-file (file-exists-p follow-file))
          (delete-file follow-file)))
      (remhash session-id elinit--cli-follow-sessions)
      t)))

(defun elinit--cli-journal-follow-stop (session-id)
  "Stop follow session SESSION-ID from external wrapper cleanup.
Return t if session existed."
  (elinit--cli-follow-stop session-id))

(defun elinit--cli-cmd-journal (args json-p)
  "Handle `journal' command with ARGS.  JSON-P for JSON output.
Display structured log records for a unit.  Requires -u/--unit."
  (let* ((parsed (elinit--cli-parse-journal-args args))
         (unit (plist-get parsed :unit))
         (n (plist-get parsed :lines))
         (priority (plist-get parsed :priority))
         (since-str (plist-get parsed :since))
         (until-str (plist-get parsed :until))
         (follow (plist-get parsed :follow))
         (unknown (plist-get parsed :unknown)))
    (cond
     (unknown
      (elinit--cli-error elinit-cli-exit-invalid-args
                             unknown
                             (if json-p 'json 'human)))
     ((null unit)
      (elinit--cli-error elinit-cli-exit-invalid-args
                             "journal requires -u/--unit ID"
                             (if json-p 'json 'human)))
     (t
      (let* ((since-ts (when since-str
                          (elinit--log-parse-timestamp since-str)))
             (until-ts (when until-str
                          (elinit--log-parse-timestamp until-str))))
        (cond
         ((and since-str (null since-ts))
          (elinit--cli-error
           elinit-cli-exit-invalid-args
           (format "Invalid --since timestamp: %s" since-str)
           (if json-p 'json 'human)))
         ((and until-str (null until-ts))
          (elinit--cli-error
           elinit-cli-exit-invalid-args
           (format "Invalid --until timestamp: %s" until-str)
           (if json-p 'json 'human)))
         (t
          (let ((log-file (elinit--log-file unit)))
            (if (not (file-exists-p log-file))
                (elinit--cli-error elinit-cli-exit-failure
                                       (format "No log file for '%s'" unit)
                                       (if json-p 'json 'human))
              (let* ((tail-bytes (when n (* n 512)))
                     (tail-decoded (elinit--log-decode-file
                                    log-file nil nil tail-bytes))
                     (tail-records (plist-get tail-decoded :records))
                     ;; If tail heuristic returned fewer records than
                     ;; requested, retry with full file to guarantee
                     ;; correctness for large payloads.
                     (decoded (if (and tail-bytes
                                       (< (length tail-records) n))
                                  (elinit--log-decode-file
                                   log-file nil nil nil)
                                tail-decoded))
                     (records (plist-get decoded :records))
                     (filtered (elinit--log-filter-records
                                records since-ts until-ts priority))
                     (limited (if (and n (> (length filtered) n))
                                  (last filtered n)
                                filtered)))
                (if (not follow)
                    ;; Non-follow: return result
                    (elinit--cli-success
                     (if json-p
                         (elinit--cli-journal-json-envelope
                          unit decoded since-str until-str
                          priority n follow limited)
                       (if limited
                           (mapconcat #'elinit--log-format-record-human
                                      limited "\n")
                         (format "No records for %s" unit)))
                     (if json-p 'json 'human))
                  ;; Follow mode: start session, return FOLLOW protocol
                  (let* ((finfo (elinit--cli-follow-start
                                 unit log-file
                                 (plist-get decoded :offset)
                                 since-ts until-ts priority json-p))
                         (initial-output
                          (if limited
                              (mapconcat
                               (if json-p
                                   #'elinit--log-record-to-json
                                 #'elinit--log-format-record-human)
                               limited "\n")
                            "")))
                    (elinit--cli-make-result
                     elinit-cli-exit-success 'follow
                     (format "FOLLOW:%s\t%s\t%s"
                             (base64-encode-string
                              (encode-coding-string
                               initial-output 'utf-8)
                              t)
                             (plist-get finfo :follow-file)
                             (plist-get finfo
                                        :session-id)))))))))))))))

(defun elinit--cli-cmd-ping (args json-p)
  "Handle `ping' command with ARGS.  JSON-P enables JSON output."
  (let ((extra-err (elinit--cli-reject-extra-args args json-p)))
    (if extra-err extra-err
      (elinit--cli-success
       (if json-p
           (json-encode `((status . "ok") (server . "elinit")))
         "pong\n")
       (if json-p 'json 'human)))))

(defun elinit--cli-cmd-version (args json-p)
  "Handle `version' command with ARGS.  JSON-P enables JSON output."
  (let ((extra-err (elinit--cli-reject-extra-args args json-p)))
    (if extra-err extra-err
      (elinit--cli-success
       (if json-p
           (json-encode `((version . ,elinit-cli-version)
                          (emacs . ,emacs-version)))
         (format "elinitctl %s (Emacs %s)\n" elinit-cli-version emacs-version))
       (if json-p 'json 'human)))))

(defconst elinit--valid-signals
  '(SIGHUP SIGINT SIGQUIT SIGILL SIGTRAP SIGABRT SIGBUS SIGFPE
    SIGKILL SIGUSR1 SIGSEGV SIGUSR2 SIGPIPE SIGALRM SIGTERM
    SIGCHLD SIGCONT SIGSTOP SIGTSTP SIGTTIN SIGTTOU SIGURG
    SIGXCPU SIGXFSZ SIGVTALRM SIGPROF SIGWINCH SIGIO SIGSYS)
  "List of valid POSIX signal names for CLI kill command.")

(defun elinit--cli-cmd-kill (args json-p)
  "Handle `kill [--signal SIG] [--] ID' command with ARGS.  JSON-P for JSON.
Options must come before --.  Use -- before IDs that start with hyphen."
  ;; Split at -- separator: options before, ID after
  (let* ((split (elinit--cli-split-at-separator args))
         (before (car split))
         (after (cdr split))
         ;; Check for unknown flags in before-part only
         (unknown (elinit--cli-has-unknown-flags before '("--signal")))
         ;; Parse --signal by position (not value) to avoid collision bugs
         (parsed (elinit--cli-parse-option before "--signal"))
         (sig-val (plist-get parsed :value))
         (sig-missing (plist-get parsed :missing))
         (sig-duplicate (plist-get parsed :duplicate))
         (before-positional (plist-get parsed :positional))
         ;; Normalize signal name: accept both TERM and SIGTERM forms
         (sig-name (when sig-val
                     (let ((upper (upcase sig-val)))
                       (if (string-prefix-p "SIG" upper) upper (concat "SIG" upper)))))
         (sig-sym (when sig-name (intern sig-name)))
         (sig-invalid (and sig-val (not (memq sig-sym elinit--valid-signals))))
         (sig (if sig-sym sig-sym 'SIGTERM))
         ;; ID is first positional from before or first from after
         (id (or (car after) (car before-positional)))
         ;; Extra args check: should have at most 1 ID total
         (extra-ids (append (cdr before-positional) (cdr after))))
    (cond
     (unknown
      (elinit--cli-error elinit-cli-exit-invalid-args
                             (format "Unknown option: %s" unknown)
                             (if json-p 'json 'human)))
     (sig-duplicate
      (elinit--cli-error elinit-cli-exit-invalid-args
                             "--signal specified multiple times"
                             (if json-p 'json 'human)))
     (sig-missing
      (elinit--cli-error elinit-cli-exit-invalid-args
                             "--signal requires a signal name"
                             (if json-p 'json 'human)))
     (sig-invalid
      (elinit--cli-error elinit-cli-exit-invalid-args
                             (format "Invalid signal name: %s" sig-val)
                             (if json-p 'json 'human)))
     ((null id)
      (elinit--cli-error elinit-cli-exit-invalid-args
                             "kill requires an ID argument"
                             (if json-p 'json 'human)))
     ;; Reject if first positional in before-part looks like an option
     ((and (null after) before-positional (string-prefix-p "-" (car before-positional)))
      (elinit--cli-error elinit-cli-exit-invalid-args
                             "kill requires ID as first argument (use -- for IDs starting with -)"
                             (if json-p 'json 'human)))
     ;; Reject extra IDs
     (extra-ids
      (elinit--cli-error elinit-cli-exit-invalid-args
                             (format "kill takes exactly one ID, got extra: %s"
                                     (mapconcat #'identity extra-ids " "))
                             (if json-p 'json 'human)))
     (t
      (let ((result (elinit--manual-kill id sig)))
        (pcase (plist-get result :status)
          ('signaled
           (elinit--cli-success
            (if json-p
                (json-encode `((id . ,id) (signal . ,(symbol-name sig))))
              (format "Sent %s to %s\n" sig id))
            (if json-p 'json 'human)))
          (_
           (elinit--cli-error elinit-cli-exit-failure
                                  (format "Process '%s' not running" id)
                                  (if json-p 'json 'human)))))))))

;;; Timer Status Output

(defun elinit--cli-gather-timer-info (&optional timers)
  "Gather timer info from elinit state.
TIMERS defaults to `elinit--timer-list' when nil.
Return a list of alists, one per timer."
  (let ((result nil))
    (dolist (timer (or timers elinit--timer-list))
      (let* ((id (elinit-timer-id timer))
             (target (elinit-timer-target timer))
             (enabled (elinit-timer-enabled timer))
             (persistent (elinit-timer-persistent timer))
             (state (gethash id elinit--timer-state))
             (last-run (plist-get state :last-run-at))
             (last-success (plist-get state :last-success-at))
             (last-failure (plist-get state :last-failure-at))
             (last-exit (plist-get state :last-exit))
             (next-run (plist-get state :next-run-at))
             (last-miss (plist-get state :last-missed-at))
             (miss-reason (plist-get state :last-miss-reason))
             (retry-at (plist-get state :retry-next-at))
             (last-result (plist-get state :last-result))
             (last-result-reason (plist-get state :last-result-reason))
             ;; Resolve target type from current config, not runtime
             ;; state, so it is always current even for fresh timers.
             (target-entry (elinit--get-entry-for-id target))
             (target-type (when target-entry
                            (elinit-entry-type target-entry))))
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
                (retry-at . ,retry-at)
                (last-result . ,last-result)
                (last-result-reason . ,last-result-reason)
                (target-type . ,target-type))
              result)))
    (nreverse result)))

(defun elinit--cli-build-timer-list ()
  "Build timer list from current effective config."
  (if (fboundp 'elinit-timer-build-list)
      (let* ((programs (elinit--effective-programs))
             (plan (elinit--build-plan programs)))
        (elinit-timer-build-list plan))
    nil))

(defun elinit--cli-format-relative-time (timestamp)
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

(defun elinit--cli-format-timer-line (info)
  "Format single timer INFO alist as status line."
  (let ((id (alist-get 'id info))
        (target (alist-get 'target info))
        (enabled (alist-get 'enabled info))
        (target-type (alist-get 'target-type info))
        (last-run (alist-get 'last-run info))
        (next-run (alist-get 'next-run info))
        (last-exit (alist-get 'last-exit info))
        (last-result (alist-get 'last-result info))
        (result-reason (alist-get 'last-result-reason info)))
    (format "%-16s %-16s %-8s %-8s %-12s %-12s %-8s %-8s %s\n"
            (or id "-")
            (or target "-")
            (if enabled "yes" "no")
            (if target-type (symbol-name target-type) "-")
            (elinit--cli-format-relative-time last-run)
            (elinit--cli-format-relative-time next-run)
            (if (null last-exit) "-" (number-to-string last-exit))
            (if last-result (symbol-name last-result) "-")
            (if result-reason (symbol-name result-reason) "-"))))

(defun elinit--cli-timers-human (timers invalid)
  "Format TIMERS and INVALID as human-readable timer status table."
  (if (and (null timers) (null invalid))
      "No timers configured.\n"
    (let ((header (format "%-16s %-16s %-8s %-8s %-12s %-12s %-8s %-8s %s\n"
                          "ID" "TARGET" "ENABLED" "TYPE"
                          "LAST-RUN" "NEXT-RUN" "EXIT" "RESULT" "REASON"))
          (sep (make-string 106 ?-)))
      (concat header sep "\n"
              (mapconcat #'elinit--cli-format-timer-line timers "")
              (when invalid
                (concat "\nInvalid timers:\n"
                        (mapconcat (lambda (inv)
                                     (format "  %s: %s\n"
                                             (plist-get inv :id)
                                             (plist-get inv :reason)))
                                   invalid "")))))))

(defun elinit--cli-timer-to-json-obj (info)
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
    (retry_at . ,(alist-get 'retry-at info))
    (last_result . ,(if (alist-get 'last-result info)
                        (symbol-name (alist-get 'last-result info))
                      :json-null))
    (last_result_reason . ,(if (alist-get 'last-result-reason info)
                               (symbol-name (alist-get 'last-result-reason info))
                             :json-null))
    (target_type . ,(if (alist-get 'target-type info)
                        (symbol-name (alist-get 'target-type info))
                      :json-null))))

(defun elinit--cli-invalid-timer-to-json-obj (info)
  "Convert invalid timer INFO plist to JSON-compatible alist."
  `((id . ,(plist-get info :id))
    (reason . ,(plist-get info :reason))))

(defun elinit--cli-timers-json (timers invalid)
  "Format TIMERS and INVALID as JSON timer status object."
  (let ((obj `((timers . ,(elinit--cli-ensure-array
                           (mapcar #'elinit--cli-timer-to-json-obj timers)))
               (invalid . ,(elinit--cli-ensure-array
                            (mapcar #'elinit--cli-invalid-timer-to-json-obj invalid))))))
    (json-encode obj)))

(defun elinit--cli-cmd-list-timers (args json-p)
  "Handle `list-timers' command with ARGS.  JSON-P enables JSON output."
  (let ((unknown (elinit--cli-has-unknown-flags args)))
    (cond
     (unknown
      (elinit--cli-error elinit-cli-exit-invalid-args
                             (format "Unknown option: %s" unknown)
                             (if json-p 'json 'human)))
     (args
      (elinit--cli-error elinit-cli-exit-invalid-args
                             "list-timers command does not accept arguments"
                             (if json-p 'json 'human)))
     ;; Check if timer subsystem gate is enabled.
     ;; Listing timers does not require `elinit-mode' to be active.
     ((not (bound-and-true-p elinit-timer-subsystem-mode))
      (let ((msg "Timer subsystem is disabled.\nEnable with: (elinit-timer-subsystem-mode 1)"))
        (elinit--cli-success
         (if json-p
             (json-encode `((status . "disabled")
                            (message . "Timer subsystem is disabled")))
           (concat msg "\n"))
         (if json-p 'json 'human))))
     (t
      (let* ((built-timers (elinit--cli-build-timer-list))
             ;; Prefer scheduler runtime list when present; otherwise build
             ;; from current config so listing works even when mode is off.
             (timer-list (or elinit--timer-list built-timers))
             (timers (elinit--cli-gather-timer-info timer-list))
             ;; Convert hash table entries (id -> reason) to plists,
             ;; sorted by ID for deterministic output ordering.
             (invalid (let (result)
                          (maphash (lambda (id reason)
                                     (push (list :id id :reason reason) result))
                                   elinit--invalid-timers)
                          (sort result
                                (lambda (a b)
                                  (string< (plist-get a :id)
                                           (plist-get b :id))))))
               (output (if json-p
                           (elinit--cli-timers-json timers invalid)
                         (elinit--cli-timers-human timers invalid))))
          (elinit--cli-success output (if json-p 'json 'human)))))))

;;; CLI Dispatcher

(defun elinit--cli-parse-args (argv)
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

(defun elinit--cli-cmd-cat (args json-p)
  "Handle `cat ID' command with ARGS.  JSON-P enables JSON output.
Output literal raw content of a unit file."
  (cond
   ((null args)
    (elinit--cli-error elinit-cli-exit-invalid-args
                           "cat requires an ID argument"
                           (if json-p 'json 'human)))
   ((cdr args)
    (elinit--cli-error elinit-cli-exit-invalid-args
                           (format "cat takes exactly one ID, got: %s"
                                   (mapconcat #'identity args " "))
                           (if json-p 'json 'human)))
   (t
    (let* ((id (car args))
           (entry (elinit--get-entry-for-id id)))
      (if (and entry (eq (elinit-entry-type entry) 'target))
          (elinit--cli-error
           elinit-cli-exit-failure
           (format "Cannot cat a target unit: %s" id)
           (if json-p 'json 'human))
        (let ((path (elinit--unit-file-path id)))
          (if (and path (file-exists-p path))
              (let ((content (with-temp-buffer
                               (insert-file-contents path)
                               (buffer-string))))
                (if json-p
                    (elinit--cli-success
                     (json-encode `((path . ,path)
                                    (content . ,content)))
                     'json)
                  (elinit--cli-success content 'human)))
            (if (and path entry)
                (elinit--cli-error
                 elinit-cli-exit-failure
                 (format "No unit file on disk for '%s' (use 'edit %s' to create an override)"
                         id id)
                 (if json-p 'json 'human))
              (elinit--cli-error
               elinit-cli-exit-failure
               (format "Unit file not found: %s" (or path id))
               (if json-p 'json 'human))))))))))

(defun elinit--cli-edit-launch-editor (editor path)
  "Launch EDITOR on PATH synchronously.
Returns the exit status of the editor process."
  (let* ((parts (split-string editor))
         (program (car parts))
         (extra-args (cdr parts)))
    (apply #'call-process program nil nil nil
           (append extra-args (list path)))))

(defun elinit--cli-cmd-edit (args json-p)
  "Handle `edit ID' command with ARGS.  JSON-P enables JSON output.
Open unit file for editing.  Create scaffold if file does not exist.
In non-interactive context, launch $VISUAL or $EDITOR."
  (cond
   ((null args)
    (elinit--cli-error elinit-cli-exit-invalid-args
                           "edit requires an ID argument"
                           (if json-p 'json 'human)))
   ((cdr args)
    (elinit--cli-error elinit-cli-exit-invalid-args
                           (format "edit takes exactly one ID, got: %s"
                                   (mapconcat #'identity args " "))
                           (if json-p 'json 'human)))
   (t
    (let* ((id (car args))
           (entry (elinit--get-entry-for-id id)))
      (if (and entry (eq (elinit-entry-type entry) 'target))
          (elinit--cli-error
           elinit-cli-exit-failure
           (format "Cannot edit a target unit: %s" id)
           (if json-p 'json 'human))
    (let* ((path (elinit--unit-file-path id))
           (root (or (when (fboundp 'elinit--authority-root-for-id)
                       (elinit--authority-root-for-id id))
                     (when path (file-name-directory path))))
           (tier (when (fboundp 'elinit--authority-tier-for-id)
                   (elinit--authority-tier-for-id id))))
      (if (not path)
          (elinit--cli-error
           elinit-cli-exit-failure
           "No active authority roots configured"
           (if json-p 'json 'human))
        (let ((created (not (file-exists-p path))))
          ;; Ensure directory exists
          (when created
            (make-directory (file-name-directory path) t)
            (write-region (elinit--unit-file-scaffold id) nil path))
          (if json-p
              (elinit--cli-success
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
                         (elinit--cli-edit-launch-editor
                          editor path)))
                    (if (= 0 exit-status)
                        (elinit--cli-success
                         (concat preamble
                                 "\nNext steps after editing:\n"
                                 "  elinitctl daemon-reload"
                                 "    Reload unit definitions")
                         'human)
                      (elinit--cli-error
                       elinit-cli-exit-failure
                       (format "%sEditor exited with status %d"
                               preamble exit-status)
                       'human)))
                (elinit--cli-error
                 elinit-cli-exit-failure
                 (format "%sNo $VISUAL or $EDITOR set.  Edit the file manually:\n  %s\n\nNext steps after editing:\n  elinitctl daemon-reload    Reload unit definitions"
                         preamble path)
                 'human))))))))))))

;;; CLI Target Commands

(defun elinit--cli-require-running (json-p)
  "Return an error result if the elinit has no runtime context.
Return nil when the elinit is running (current plan is available).
JSON-P controls the output format."
  (unless elinit--current-plan
    (elinit--cli-error
     elinit-cli-exit-failure
     "Elinit is not running (no current plan)"
     (if json-p 'json 'human))))

(defun elinit--cli-target-status-string (target-id)
  "Return runtime target status string for TARGET-ID."
  (car (elinit--compute-entry-status target-id 'target)))

(defun elinit--cli-cmd-get-default (args json-p)
  "Handle `get-default' command with ARGS.  JSON-P enables JSON output.
Print the effective default-target-link (what default.target resolves to)."
  (let ((extra-err (elinit--cli-reject-extra-args args json-p)))
    (if extra-err extra-err
      (let ((resolved (elinit--resolve-default-target-link)))
        (elinit--cli-success
         (if json-p
             (json-encode `((default-target . ,resolved)))
           (format "%s\n" resolved))
         (if json-p 'json 'human))))))

(defun elinit--cli-cmd-set-default (args json-p)
  "Handle `set-default TARGET' command with ARGS.  JSON-P enables JSON output.
Persist default-target-link override.  Reject \"default.target\"."
  (cond
   ((null args)
    (elinit--cli-error elinit-cli-exit-invalid-args
                           "set-default requires a TARGET argument"
                           (if json-p 'json 'human)))
   ((cdr args)
    (elinit--cli-error elinit-cli-exit-invalid-args
                           (format "set-default takes exactly one argument, got: %s"
                                   (mapconcat #'identity args " "))
                           (if json-p 'json 'human)))
   (t
    (let ((target (car args)))
      (cond
       ((equal target "default.target")
        (elinit--cli-error
         elinit-cli-exit-invalid-args
         "Cannot set default to \"default.target\" (circular alias); use a concrete target"
         (if json-p 'json 'human)))
       ((not (string-suffix-p ".target" target))
        (elinit--cli-error
         elinit-cli-exit-invalid-args
         (format "'%s' is not a target (must end in .target)" target)
         (if json-p 'json 'human)))
       (t
        ;; Validate target exists in current plan
        (let* ((plan (elinit--build-plan (elinit--effective-programs)))
               (entries (elinit-plan-entries plan))
               (found (cl-find target entries
                               :key #'elinit-entry-id
                               :test #'equal)))
          (cond
           ((not found)
            (elinit--cli-error
             elinit-cli-exit-no-such-unit
             (format "Target '%s' does not exist" target)
             (if json-p 'json 'human)))
           ((not (eq (elinit-entry-type found) 'target))
            (elinit--cli-error
             elinit-cli-exit-invalid-args
             (format "'%s' exists but is not a target unit" target)
             (if json-p 'json 'human)))
           (t
            ;; Resolve alias to canonical before persisting
            (let ((canonical (elinit--resolve-target-alias target)))
              (setq elinit--default-target-link-override canonical)
              (elinit--save-overrides)
              (elinit--cli-success
               (if json-p
                   (json-encode `((status . "applied")
                                  (target . ,canonical)))
                 (if (equal target canonical)
                     (format "Default target set to %s\n" canonical)
                   (format "Default target set to %s (resolved from %s)\n"
                           canonical target)))
               (if json-p 'json 'human))))))))))))

(defun elinit--cli-cmd-list-targets (args json-p)
  "Handle `list-targets' command with ARGS.  JSON-P enables JSON output.
List all target units with convergence state and member counts."
  (let ((extra-err (elinit--cli-reject-extra-args args json-p)))
    (if extra-err extra-err
      (let ((running-err (elinit--cli-require-running json-p)))
        (if running-err running-err
      (let* ((plan (elinit--build-plan (elinit--effective-programs)))
             (entries (elinit-plan-entries plan))
             (target-entries
              (cl-remove-if-not
               (lambda (e) (eq (elinit-entry-type e) 'target))
               entries))
             (members (elinit--materialize-target-members entries))
             (rows nil))
        ;; Sort by ID for deterministic output
        (setq target-entries
              (sort (copy-sequence target-entries)
                    (lambda (a b)
                      (string< (elinit-entry-id a)
                               (elinit-entry-id b)))))
        (dolist (entry target-entries)
          (let* ((id (elinit-entry-id entry))
                 (canonical (elinit--resolve-target-alias id))
                 (mem (gethash canonical members))
                 (req-count (length (plist-get mem :requires)))
                 (wants-count (length (plist-get mem :wants)))
                 (status (elinit--cli-target-status-string id))
                 (alias-target (elinit--target-alias-p id))
                 (kind (if alias-target "alias" "canonical"))
                 (resolved-link
                  (cond (alias-target
                         (elinit--resolve-target-alias id))
                        ((equal id "default.target")
                         (elinit--resolve-default-target-link)))))
            (push `((id . ,id)
                    (status . ,status)
                    (kind . ,kind)
                    (requires-count . ,req-count)
                    (wants-count . ,wants-count)
                    (resolved-link . ,resolved-link))
                  rows)))
        (setq rows (nreverse rows))
        (elinit--cli-success
         (if json-p
             (json-encode (elinit--cli-ensure-array rows))
           (concat
            (format "%-25s %-12s %-10s %s\n" "ID" "STATUS" "KIND" "MEMBERS")
            (make-string 70 ?-)
            "\n"
            (mapconcat
             (lambda (row)
               (let ((id (alist-get 'id row))
                     (status (alist-get 'status row))
                     (kind (alist-get 'kind row))
                     (req (alist-get 'requires-count row))
                     (wants (alist-get 'wants-count row))
                     (link (alist-get 'resolved-link row)))
                 (format "%-25s %-12s %-10s %d requires, %d wants%s\n"
                         id status kind req wants
                         (if link (format "  (-> %s)" link) ""))))
             rows "")))
         (if json-p 'json 'human))))))))

(defun elinit--cli-cmd-target-status (args json-p)
  "Handle `target-status TARGET' command with ARGS.  JSON-P for JSON.
Show convergence state, reasons, and member lists for a target."
  (cond
   ((null args)
    (elinit--cli-error elinit-cli-exit-invalid-args
                           "target-status requires a TARGET argument"
                           (if json-p 'json 'human)))
   ((cdr args)
    (elinit--cli-error elinit-cli-exit-invalid-args
                           (format "target-status takes exactly one argument, got: %s"
                                   (mapconcat #'identity args " "))
                           (if json-p 'json 'human)))
   (t
    (let ((running-err (elinit--cli-require-running json-p)))
      (or running-err
    (let* ((target (car args))
           (plan (elinit--build-plan (elinit--effective-programs)))
           (entries (elinit-plan-entries plan))
           (found (cl-find target entries
                           :key #'elinit-entry-id
                           :test #'equal)))
      (cond
       ((not found)
        (elinit--cli-error
         elinit-cli-exit-no-such-unit
         (format "Target '%s' does not exist" target)
         (if json-p 'json 'human)))
       ((not (eq (elinit-entry-type found) 'target))
        (elinit--cli-error
         elinit-cli-exit-invalid-args
         (format "'%s' exists but is not a target unit" target)
         (if json-p 'json 'human)))
       (t
        (let* ((members (elinit--materialize-target-members entries))
               (canonical (elinit--resolve-target-alias target))
               (mem (gethash canonical members))
               (req-ids (plist-get mem :requires))
               (want-ids (plist-get mem :wants))
               (status (elinit--cli-target-status-string target))
               (reasons (when (hash-table-p
                               elinit--target-convergence-reasons)
                          (gethash canonical
                                   elinit--target-convergence-reasons)))
               (alias-target (elinit--target-alias-p target))
               (kind (if alias-target "alias" "canonical"))
               (resolved-link
                (cond (alias-target
                       (elinit--resolve-target-alias target))
                      ((equal target "default.target")
                       (elinit--resolve-default-target-link))))
               (desc (elinit-entry-description found))
               ;; Build member status lists
               (req-info
                (mapcar (lambda (mid)
                          (let ((st (car (elinit--compute-entry-status
                                         mid (elinit--cli-type-for-id mid entries)))))
                            (cons mid st)))
                        req-ids))
               (want-info
                (mapcar (lambda (mid)
                          (let ((st (car (elinit--compute-entry-status
                                         mid (elinit--cli-type-for-id mid entries)))))
                            (cons mid st)))
                        want-ids)))
          (elinit--cli-success
           (if json-p
               (json-encode
                `((id . ,target)
                  (description . ,desc)
                  (status . ,status)
                  (kind . ,kind)
                  (reasons . ,(or reasons []))
                  (resolved-link . ,resolved-link)
                  (requires
                   . ,(elinit--cli-ensure-array
                       (mapcar (lambda (p)
                                 `((id . ,(car p))
                                   (status . ,(cdr p))))
                               req-info)))
                  (wants
                   . ,(elinit--cli-ensure-array
                       (mapcar (lambda (p)
                                 `((id . ,(car p))
                                   (status . ,(cdr p))))
                               want-info)))))
             (concat
              (format "%s" target)
              (if desc (format " - %s" desc) "")
              "\n"
              (format "  Kind: %s\n" kind)
              (format "  Status: %s\n" status)
              (when reasons
                (format "  Reason: %s\n"
                        (mapconcat #'identity reasons "; ")))
              (when resolved-link
                (format "  Resolved link: %s\n" resolved-link))
              (format "  Requires: %s\n"
                      (if req-info
                          (mapconcat
                           (lambda (p)
                             (format "%s (%s)" (car p) (cdr p)))
                           req-info ", ")
                        "none"))
              (format "  Wants: %s\n"
                      (if want-info
                          (mapconcat
                           (lambda (p)
                             (format "%s (%s)" (car p) (cdr p)))
                           want-info ", ")
                        "none"))))
           (if json-p 'json 'human)))))))))))

(defun elinit--cli-type-for-id (id entries)
  "Return the entry type symbol for ID in ENTRIES list, or nil."
  (let ((found (cl-find id entries
                        :key #'elinit-entry-id
                        :test #'equal)))
    (when found (elinit-entry-type found))))

(defun elinit--cli-cmd-explain-target (args json-p)
  "Handle `explain-target TARGET' command with ARGS.  JSON-P for JSON.
Show root-cause chain: why is this target in its current state?"
  (cond
   ((null args)
    (elinit--cli-error elinit-cli-exit-invalid-args
                           "explain-target requires a TARGET argument"
                           (if json-p 'json 'human)))
   ((cdr args)
    (elinit--cli-error elinit-cli-exit-invalid-args
                           (format "explain-target takes exactly one argument, got: %s"
                                   (mapconcat #'identity args " "))
                           (if json-p 'json 'human)))
   (t
    (let ((running-err (elinit--cli-require-running json-p)))
      (or running-err
    (let* ((target (car args))
           (plan (elinit--build-plan (elinit--effective-programs)))
           (entries (elinit-plan-entries plan))
           (found (cl-find target entries
                           :key #'elinit-entry-id
                           :test #'equal)))
      (cond
       ((not found)
        (elinit--cli-error
         elinit-cli-exit-no-such-unit
         (format "Target '%s' does not exist" target)
         (if json-p 'json 'human)))
       ((not (eq (elinit-entry-type found) 'target))
        (elinit--cli-error
         elinit-cli-exit-invalid-args
         (format "'%s' exists but is not a target unit" target)
         (if json-p 'json 'human)))
       (t
        (let* ((members (elinit--materialize-target-members entries))
               (canonical (elinit--resolve-target-alias target))
               (mem (gethash canonical members))
               (req-ids (plist-get mem :requires))
               (want-ids (plist-get mem :wants))
               (status (elinit--cli-target-status-string target))
               (reasons (when (hash-table-p
                               elinit--target-convergence-reasons)
                          (gethash canonical
                                   elinit--target-convergence-reasons)))
               (all-ids (append req-ids want-ids))
               (member-details nil))
          ;; Build member detail list based on convergence state
          (pcase status
            ("degraded"
             (dolist (mid all-ids)
               (let* ((st (car (elinit--compute-entry-status
                                mid (elinit--cli-type-for-id mid entries))))
                      (failed (member st '("dead" "failed"))))
                 (when failed
                   (push `((id . ,mid) (status . ,st)) member-details))))
             (setq member-details (nreverse member-details)))
            ("converging"
             (dolist (mid all-ids)
               (let ((st (car (elinit--compute-entry-status
                               mid (elinit--cli-type-for-id mid entries)))))
                 (unless (member st '("running" "done" "active" "reached"))
                   (push `((id . ,mid) (status . ,st)) member-details))))
             (setq member-details (nreverse member-details)))
            (_ nil))
          (elinit--cli-success
           (if json-p
               (let ((alias-info (elinit--target-alias-p target)))
                 (json-encode
                  `((id . ,target)
                    (kind . ,(if alias-info "alias" "canonical"))
                    ,@(when alias-info
                        `((resolved-link . ,(elinit--resolve-target-alias target))))
                    (status . ,status)
                    (reasons . ,(or reasons []))
                    (members . ,(elinit--cli-ensure-array
                                 member-details)))))
             (concat
              (format "%s: %s%s\n" target status
                      (if (elinit--target-alias-p target)
                          (format " (alias -> %s)"
                                  (elinit--resolve-target-alias target))
                        ""))
              (pcase status
                ("degraded"
                 (concat
                  (mapconcat
                   (lambda (m)
                     (format "  %s: %s\n"
                             (alist-get 'id m) (alist-get 'status m)))
                   member-details "")
                  (when reasons
                    (format "  --> %s\n"
                            (mapconcat #'identity reasons "; ")))))
                ("converging"
                 (concat
                  (mapconcat
                   (lambda (m)
                     (format "  %s: %s\n"
                             (alist-get 'id m) (alist-get 'status m)))
                   member-details "")
                  (format "  --> %d member%s not yet terminal\n"
                          (length member-details)
                          (if (= 1 (length member-details)) "" "s"))))
                ("reached"
                 (format "  All %d required member%s healthy\n"
                         (length req-ids)
                         (if (= 1 (length req-ids)) "" "s")))
                ("unreachable"
                 "  Target is outside the current activation closure\n")
                ("pending"
                 "  Target convergence has not started yet\n"))))
           (if json-p 'json 'human)))))))))))

(defun elinit--cli-cmd-isolate (args json-p)
  "Handle `isolate [--yes] TARGET' command with ARGS.  JSON-P for JSON.
Switch to TARGET: stop entries not in closure, start entries in closure.
Requires --yes flag.  Transaction-scoped (does not persist default change)."
  (let* ((split (elinit--cli-split-at-separator args))
         (before (car split))
         (after (cdr split))
         (unknown (elinit--cli-has-unknown-flags before '("--yes")))
         (yes-flag (member "--yes" before))
         (positional (cl-remove "--yes" (append before after) :test #'equal)))
    (cond
     (unknown
      (elinit--cli-error elinit-cli-exit-invalid-args
                             (format "Unknown option: %s" unknown)
                             (if json-p 'json 'human)))
     ((null positional)
      (elinit--cli-error elinit-cli-exit-invalid-args
                             "isolate requires a TARGET argument"
                             (if json-p 'json 'human)))
     ((cdr positional)
      (elinit--cli-error elinit-cli-exit-invalid-args
                             (format "isolate takes exactly one TARGET, got: %s"
                                     (mapconcat #'identity positional " "))
                             (if json-p 'json 'human)))
     ((not yes-flag)
      (elinit--cli-error elinit-cli-exit-invalid-args
                             "isolate requires --yes to confirm"
                             (if json-p 'json 'human)))
     (t
      (let ((running-err (elinit--cli-require-running json-p)))
        (or running-err
      (let* ((raw-target (car positional))
             ;; Resolve alias before lookup (e.g., runlevelN.target -> canonical)
             (target (elinit--resolve-target-alias raw-target)))
        (cond
         ((not (string-suffix-p ".target" raw-target))
          (elinit--cli-error
           elinit-cli-exit-invalid-args
           (format "'%s' is not a target (must end in .target)" raw-target)
           (if json-p 'json 'human)))
         (t
          (let* ((plan (elinit--build-plan (elinit--effective-programs)))
                 (entries (elinit-plan-entries plan))
                 (found (cl-find target entries
                                 :key #'elinit-entry-id
                                 :test #'equal)))
            (cond
             ((not found)
              (elinit--cli-error
               elinit-cli-exit-no-such-unit
               (format "Target '%s' does not exist" target)
               (if json-p 'json 'human)))
             ((not (eq (elinit-entry-type found) 'target))
              (elinit--cli-error
               elinit-cli-exit-invalid-args
               (format "'%s' exists but is not a target unit" target)
               (if json-p 'json 'human)))
             (t
              ;; Compute new closure
              (let* ((entries-by-id (make-hash-table :test 'equal))
                     (_ (dolist (e entries)
                          (puthash (elinit-entry-id e) e entries-by-id)))
                     (members (elinit--materialize-target-members entries))
                     (closure (elinit--expand-transaction
                               target entries-by-id members
                               (elinit-plan-order-index plan)))
                     ;; Determine current running IDs
                     (running-ids nil)
                     (_ (maphash
                         (lambda (id proc)
                           (when (and proc (process-live-p proc))
                             (push id running-ids)))
                         elinit--processes))
                     ;; Stop entries running but not in new closure
                     (stop-count 0)
                     (start-count 0))
                (dolist (id running-ids)
                  (unless (gethash id closure)
                    (elinit--manual-stop id)
                    (cl-incf stop-count)))
                ;; Collect entries that need DAG-ordered starting
                (let ((to-start
                       (cl-remove-if
                        (lambda (e)
                          (let* ((id (elinit-entry-id e))
                                 (proc (gethash id elinit--processes)))
                            (or (not (gethash id closure))
                                (eq (elinit-entry-type e) 'target)
                                (and proc (process-live-p proc)))))
                        (elinit-plan-by-target plan))))
                  (setq start-count (length to-start))
                  ;; Start via DAG scheduler (respects oneshot-blocking)
                  (elinit--dag-start-with-deps
                   to-start
                   (lambda ()
                     (elinit--log 'info "isolate startup complete"))))
                ;; Update convergence state for new target membership
                (when (hash-table-p elinit--target-members)
                  (maphash (lambda (k v) (puthash k v elinit--target-members))
                           members))
                (elinit--maybe-refresh-dashboard)
                (elinit--cli-success
                 (if json-p
                     (json-encode `((status . "applied")
                                    (target . ,target)
                                    (stopped . ,stop-count)
                                    (started . ,start-count)))
                   (format "Isolated to %s: stopped %d, started %d\n"
                           target stop-count start-count))
                 (if json-p 'json 'human)))))))))))))))

(defconst elinit--runlevel-map
  '((0 . "poweroff.target")
    (1 . "rescue.target")
    (2 . "multi-user.target")
    (3 . "multi-user.target")
    (4 . "multi-user.target")
    (5 . "graphical.target")
    (6 . "reboot.target"))
  "Fixed runlevel-to-target mapping using systemd semantics.")

(defun elinit--cli-cmd-init (args json-p)
  "Handle `init N' command with ARGS.  JSON-P for JSON output.
Map numeric runlevel N (0-6) to a target and isolate to it.
Runlevels 0 and 6 are destructive transitions: in interactive
Emacs sessions they prompt for confirmation via `y-or-n-p'; in
batch mode (or JSON mode) the --yes flag is required.
Does not persist default-target changes.
Result summary includes both numeric runlevel and resolved target."
  (let* ((split (elinit--cli-split-at-separator args))
         (before (car split))
         (after (cdr split))
         (unknown (elinit--cli-has-unknown-flags before '("--yes")))
         (yes-flag (member "--yes" before))
         (positional (cl-remove "--yes" (append before after)
                                :test #'equal)))
    (cond
     (unknown
      (elinit--cli-error elinit-cli-exit-invalid-args
                             (format "Unknown option: %s" unknown)
                             (if json-p 'json 'human)))
     ((null positional)
      (elinit--cli-error elinit-cli-exit-invalid-args
                             "init requires a runlevel argument (0-6)"
                             (if json-p 'json 'human)))
     ((cdr positional)
      (elinit--cli-error elinit-cli-exit-invalid-args
                             (format "init takes exactly one argument, got: %s"
                                     (mapconcat #'identity positional " "))
                             (if json-p 'json 'human)))
     (t
      (let* ((arg (car positional))
             (n (and (string-match-p "\\`[0-9]+\\'" arg)
                     (string-to-number arg)))
             (mapping (when n (assq n elinit--runlevel-map))))
        (cond
         ((null n)
          (elinit--cli-error
           elinit-cli-exit-invalid-args
           (format "'%s' is not a valid runlevel (must be 0-6)" arg)
           (if json-p 'json 'human)))
         ((null mapping)
          (elinit--cli-error
           elinit-cli-exit-invalid-args
           (format "Runlevel %d is out of range (must be 0-6)" n)
           (if json-p 'json 'human)))
         ;; Destructive transitions (0=poweroff, 6=reboot) need confirmation.
         ;; Interactive Emacs: prompt via y-or-n-p.
         ;; Batch/JSON: require --yes flag.
         ((and (memq n '(0 6)) (not yes-flag))
          (if (and (not noninteractive) (not json-p)
                   (y-or-n-p
                    (format "Init %d (%s) is destructive.  Proceed? "
                            n (cdr mapping))))
              (elinit--cli-init-execute n mapping json-p)
            (elinit--cli-error
             elinit-cli-exit-invalid-args
             (format "init %d (%s) is destructive; use --yes to confirm"
                     n (cdr mapping))
             (if json-p 'json 'human))))
         (t
          (elinit--cli-init-execute n mapping json-p))))))))

(defun elinit--cli-init-execute (runlevel mapping json-p)
  "Execute init transition for RUNLEVEL using MAPPING.
MAPPING is a cons cell from `elinit--runlevel-map'.
JSON-P selects output format.
Route through isolate and wrap the result to include both numeric
runlevel and resolved target identity per plan requirement."
  (let* ((target (cdr mapping))
         (result (elinit--cli-cmd-isolate
                  (list "--yes" target) json-p)))
    (if (= elinit-cli-exit-success
           (elinit-cli-result-exitcode result))
        (let ((inner (elinit-cli-result-output result)))
          (elinit--cli-success
           (if json-p
               (let ((obj (json-read-from-string inner)))
                 (json-encode
                  (append `((runlevel . ,runlevel)) obj)))
             (format "init %d (runlevel %d -> %s): %s"
                     runlevel runlevel target inner))
           (if json-p 'json 'human)))
      result)))

(defun elinit--cli-dispatch (argv)
  "Dispatch CLI command from ARGV.
ARGV is a list of strings (command and arguments).
Returns a `elinit-cli-result' struct."
  (let* ((parsed (elinit--cli-parse-args argv))
         (command (nth 0 parsed))
         (args (nth 1 parsed))
         (json-p (nth 2 parsed)))
    (condition-case err
        (cond
         ((null command)
          (elinit--cli-success
           (concat "Usage: elinitctl COMMAND [ARGS...]\n\n"
                   "Systemctl-compatible commands:\n"
                   "  status [ID...]             Show unit status (detail with IDs, overview without)\n"
                   "  list-units [ID...]         List units (overview table)\n"
                   "  show ID                    Show all properties of a unit\n"
                   "  start [--target T] [-- ID...]  Start units (or full startup with --target)\n"
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
                   "Elinit-specific commands:\n"
                   "  verify                     Verify config\n"
                   "  reset-failed [--] [ID...]  Reset failed state\n"
                   "  restart-policy POLICY ID...    Set restart policy (no|on-success|on-failure|always)\n"
                   "  logging (on|off) ID...     Set logging policy\n"
                   "  blame                      Show startup timing\n"
                   "  logs [--tail N] [--] ID    View unit log file\n"
                   "  journal -u ID [-n N] [-p err] [-f] [--since TS] [--until TS]  Structured log records\n"
                   "  ping                       Check elinit liveness\n"
                   "  version                    Show version\n\n"
                   "Target commands:\n"
                   "  list-targets               List all target units\n"
                   "  target-status TARGET       Show target status and members\n"
                   "  explain-target TARGET      Show root-cause chain for target state\n"
                   "  isolate --yes TARGET       Switch to target (transaction-scoped)\n"
                   "  get-default                Show effective default target\n"
                   "  set-default TARGET         Set persistent default target\n"
                   "  init [--yes] N             Switch to runlevel (0-6)\n"
                   "  telinit [--yes] N          Alias for init\n\n"
                   "Options: --json (output as JSON)\n")
           (if json-p 'json 'human)))
         ((equal command "status")
          (elinit--cli-cmd-status args json-p))
         ((equal command "list-units")
          (elinit--cli-cmd-list-units args json-p))
         ((equal command "show")
          (elinit--cli-cmd-show args json-p))
         ((equal command "verify")
          (elinit--cli-cmd-verify args json-p))
         ((equal command "start")
          (elinit--cli-cmd-start args json-p))
         ((equal command "stop")
          (elinit--cli-cmd-stop args json-p))
         ((equal command "restart")
          (elinit--cli-cmd-restart args json-p))
         ((equal command "daemon-reload")
          (elinit--cli-cmd-daemon-reload args json-p))
         ((equal command "reload")
          (elinit--cli-cmd-reload args json-p))
         ((equal command "enable")
          (elinit--cli-cmd-enable args json-p))
         ((equal command "disable")
          (elinit--cli-cmd-disable args json-p))
         ((equal command "mask")
          (elinit--cli-cmd-mask args json-p))
         ((equal command "unmask")
          (elinit--cli-cmd-unmask args json-p))
         ((equal command "restart-policy")
          (elinit--cli-cmd-restart-policy args json-p))
         ((equal command "logging")
          (elinit--cli-cmd-logging args json-p))
         ((equal command "blame")
          (elinit--cli-cmd-blame args json-p))
         ((equal command "list-dependencies")
          (elinit--cli-cmd-list-dependencies args json-p))
         ((equal command "logs")
          (elinit--cli-cmd-logs args json-p))
         ((equal command "journal")
          (elinit--cli-cmd-journal args json-p))
         ((equal command "kill")
          (elinit--cli-cmd-kill args json-p))
         ((equal command "ping")
          (elinit--cli-cmd-ping args json-p))
         ((equal command "version")
          (elinit--cli-cmd-version args json-p))
         ((equal command "list-timers")
          (elinit--cli-cmd-list-timers args json-p))
         ((equal command "cat")
          (elinit--cli-cmd-cat args json-p))
         ((equal command "edit")
          (elinit--cli-cmd-edit args json-p))
         ((equal command "is-active")
          (elinit--cli-cmd-is-active args json-p))
         ((equal command "is-enabled")
          (elinit--cli-cmd-is-enabled args json-p))
         ((equal command "is-failed")
          (elinit--cli-cmd-is-failed args json-p))
         ((equal command "reset-failed")
          (elinit--cli-cmd-reset-failed args json-p))
         ((equal command "list-targets")
          (elinit--cli-cmd-list-targets args json-p))
         ((equal command "target-status")
          (elinit--cli-cmd-target-status args json-p))
         ((equal command "explain-target")
          (elinit--cli-cmd-explain-target args json-p))
         ((equal command "isolate")
          (elinit--cli-cmd-isolate args json-p))
         ((equal command "get-default")
          (elinit--cli-cmd-get-default args json-p))
         ((equal command "set-default")
          (elinit--cli-cmd-set-default args json-p))
         ((equal command "init")
          (elinit--cli-cmd-init args json-p))
         ((equal command "telinit")
          (elinit--cli-cmd-init args json-p))
         (t
          (elinit--cli-error elinit-cli-exit-invalid-args
                                 (format "Unknown command: %s" command)
                                 (if json-p 'json 'human))))
      (error
       (elinit--cli-error elinit-cli-exit-failure
                              (format "Internal error: %s" (error-message-string err))
                              (if json-p 'json 'human))))))

(defun elinit--cli-dispatch-for-wrapper (argv-list)
  "Dispatch CLI command for external wrapper.
ARGV-LIST is a list of strings passed from the shell wrapper.
Returns a string in format \"EXITCODE:BASE64OUTPUT\" for the wrapper to parse.
Exit code and base64-encoded output are separated by colon.
Base64 encoding avoids escaping issues with newlines and special characters."
  (let ((result (elinit--cli-dispatch argv-list)))
    (if (eq (elinit-cli-result-format result) 'follow)
        (elinit-cli-result-output result)
      (format "%d:%s"
              (elinit-cli-result-exitcode result)
              (base64-encode-string
               (encode-coding-string
                (elinit-cli-result-output result) 'utf-8)
               t)))))


(provide 'elinit-cli)

;;; elinit-cli.el ends here
