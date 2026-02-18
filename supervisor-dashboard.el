;;; supervisor-dashboard.el --- Supervisor dashboard UI -*- lexical-binding: t -*-

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

;; Dashboard UI for supervisor.el.
;; Run M-x supervisor-handbook for full documentation.

;;; Code:

(require 'cl-lib)
(require 'supervisor-core)

;; Forward declaration for transient menu (defined at runtime via eval)
(declare-function supervisor-dashboard-menu "supervisor-dashboard" ())

;; Forward declaration for proced function
(declare-function proced-toggle-auto-update "proced" (arg))

;; Forward declarations for timer subsystem (defined in supervisor-timer.el)
(declare-function supervisor-timer-subsystem-active-p "supervisor-timer" ())
(declare-function supervisor-timer-id "supervisor-timer" (timer))
(declare-function supervisor-timer-target "supervisor-timer" (timer))
(declare-function supervisor-timer-enabled "supervisor-timer" (timer))
(declare-function supervisor-timer-persistent "supervisor-timer" (timer))
(declare-function supervisor-timer-on-calendar "supervisor-timer" (timer))
(declare-function supervisor-timer-on-startup-sec "supervisor-timer" (timer))
(declare-function supervisor-timer-on-unit-active-sec "supervisor-timer" (timer))
(declare-function supervisor-timer--target-active-p "supervisor-timer" (timer))
(declare-function supervisor-timer--trigger "supervisor-timer" (timer reason))
(declare-function supervisor-timer--update-next-run "supervisor-timer" (timer-id))
(declare-function supervisor-timer--save-state "supervisor-timer" ())

;; Forward declarations for unit-file module (optional)
(declare-function supervisor--unit-file-path "supervisor-units" (id))
(declare-function supervisor--unit-file-existing-path "supervisor-units" (id))
(declare-function supervisor--unit-file-scaffold "supervisor-units" (id))
(declare-function supervisor--validate-unit-file-buffer "supervisor-units" ())
(declare-function supervisor--authority-root-for-id "supervisor-units" (id))
(declare-function supervisor--authority-tier-for-id "supervisor-units" (id))

;; Forward declarations for logging module (defined in supervisor-log.el)
(declare-function supervisor--log-file "supervisor-log" (prog))
(declare-function supervisor--log-format-record-human "supervisor-log" (record))
(declare-function supervisor--log-decode-file "supervisor-log"
                  (file &optional limit offset max-bytes))
(declare-function supervisor--log-filter-records "supervisor-log"
                  (records &optional since until priority))
(declare-function supervisor--log-record-to-json "supervisor-log" (record))
(declare-function supervisor--log-record-priority "supervisor-log" (record))

;; Forward declarations for timer state variables (defined in supervisor-timer.el)
(defvar supervisor--timer-state)
(defvar supervisor--timer-list)
(defvar supervisor--invalid-timers)
(defvar supervisor-timer-retry-intervals)
;; Forward declaration for tabulated-list internals
(defvar tabulated-list-header-string nil)

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

(defface supervisor-status-unreachable
  '((t :foreground "#666666"))
  "Face for unreachable (not in activation closure) status in dashboard."
  :group 'supervisor)


(defface supervisor-type-simple
  '((t :foreground "#87ceeb"))
  "Face for simple process type in dashboard."
  :group 'supervisor)

(defface supervisor-type-oneshot
  '((t :foreground "#dda0dd"))
  "Face for oneshot process type in dashboard."
  :group 'supervisor)

(defface supervisor-type-timer
  '((t :foreground "#98d8c8"))
  "Face for timer type in dashboard."
  :group 'supervisor)

(defface supervisor-type-target
  '((t :foreground "#ffd700"))
  "Face for target type in dashboard."
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

(defface supervisor-section-separator
  '((t :foreground "#5f87af" :weight bold :underline t))
  "Face for section separator rows in dashboard."
  :group 'supervisor)

(defcustom supervisor-dashboard-show-header-hints nil
  "Reserved compatibility option for dashboard header hints.
This option is currently inert; the header line shows only service
counters.  Press `h' in the dashboard for full keybinding help."
  :type 'boolean
  :group 'supervisor)

(defcustom supervisor-dashboard-show-timers t
  "When non-nil, show timers section in dashboard after services."
  :type 'boolean
  :group 'supervisor)

(defcustom supervisor-dashboard-log-view-record-limit 1000
  "Maximum records decoded for `supervisor-dashboard-view-log'.
Bounds both the number of records returned and the bytes read from
disk (heuristic: LIMIT * 512 bytes tail read).  Must be a positive
integer; the dashboard decode path is always bounded."
  :type 'integer
  :group 'supervisor)

;;; Dashboard

(defvar-local supervisor--dashboard-tag-filter nil
  "Current tag filter for dashboard.
nil means show all entries, otherwise a tag symbol or string.")

(defvar-local supervisor--dashboard-target-filter nil
  "Current target filter for dashboard.
nil means show all entries, otherwise a target ID string.")

(defvar-local supervisor--dashboard-show-targets nil
  "Non-nil means show target-type entries in the dashboard.
Default is nil, hiding targets for a service-first view.")

(defvar-local supervisor--dashboard-show-init-targets nil
  "Non-nil means include init-transition targets when targets are shown.
Init-transition targets are rescue, shutdown, poweroff, reboot,
runlevel0, runlevel1, and runlevel6 targets.")

(defconst supervisor--init-transition-target-ids
  '("rescue.target" "shutdown.target" "poweroff.target" "reboot.target"
    "runlevel0.target" "runlevel1.target" "runlevel6.target")
  "Target IDs that represent init-transition targets.
These are hidden by default even when regular targets are shown.")

(defvar supervisor-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    ;; Navigation / filter
    (define-key map "f" #'supervisor-dashboard-cycle-filter)
    (define-key map "F" #'supervisor-dashboard-cycle-tag-filter)
    ;; Visibility
    (define-key map "v" #'supervisor-dashboard-toggle-targets)
    (define-key map "V" #'supervisor-dashboard-toggle-init-targets)
    ;; Refresh
    (define-key map "g" #'supervisor-dashboard-refresh)
    (define-key map "G" #'supervisor-dashboard-toggle-auto-refresh)
    ;; System (proced)
    (define-key map "t" #'proced)
    (define-key map "T" #'supervisor-dashboard-toggle-proced-auto-update)
    ;; Nested action groups
    (define-key map "l" #'supervisor-dashboard-lifecycle)
    (define-key map "p" #'supervisor-dashboard-policy)
    (define-key map "i" #'supervisor-dashboard-inspect)
    (define-key map "y" #'supervisor-dashboard-timer-actions)
    ;; Help / quit
    (define-key map "h" #'supervisor-dashboard-help)
    (define-key map "?" #'supervisor-dashboard-menu-open)
    (define-key map "q" #'supervisor-dashboard-quit)
    map)
  "Keymap for `supervisor-dashboard-mode'.")

;;; Submenu Dispatchers

(defun supervisor-dashboard-lifecycle ()
  "Open lifecycle action submenu.
Lifecycle actions control the runtime state of individual entries."
  (interactive)
  (pcase (read-char
          "Lifecycle: [s]tart [t]stop [r]estart [k]ill [u]reload [f]reset-failed ")
    (?s (call-interactively #'supervisor-dashboard-start))
    (?t (call-interactively #'supervisor-dashboard-stop))
    (?r (call-interactively #'supervisor-dashboard-restart))
    (?k (call-interactively #'supervisor-dashboard-kill))
    (?u (call-interactively #'supervisor-dashboard-reload-unit))
    (?f (call-interactively #'supervisor-dashboard-reset-failed))
    (_ (message "Lifecycle: cancelled"))))

(defun supervisor-dashboard-policy ()
  "Open policy submenu.
Policy actions modify persistent configuration overrides."
  (interactive)
  (pcase (read-char
          "Policy: [e]nable [d]isable [m]ask [u]nmask [r]estart-policy [l]ogging ")
    (?e (call-interactively #'supervisor-dashboard-enable))
    (?d (call-interactively #'supervisor-dashboard-disable))
    (?m (call-interactively #'supervisor-dashboard-mask))
    (?u (call-interactively #'supervisor-dashboard-unmask))
    (?r (call-interactively #'supervisor-dashboard-set-restart-policy))
    (?l (call-interactively #'supervisor-dashboard-set-logging))
    (_ (message "Policy: cancelled"))))

(defun supervisor-dashboard-inspect ()
  "Open inspect submenu.
Inspect actions are read-only presentation workflows."
  (interactive)
  (pcase (read-char
          "Inspect: [i]nfo [d]eps [g]raph [b]lame [l]og [c]at [e]dit [m]embers ")
    (?i (call-interactively #'supervisor-dashboard-describe-entry))
    (?d (call-interactively #'supervisor-dashboard-show-deps))
    (?g (call-interactively #'supervisor-dashboard-show-graph))
    (?b (call-interactively #'supervisor-dashboard-blame))
    (?l (call-interactively #'supervisor-dashboard-view-log))
    (?c (call-interactively #'supervisor-dashboard-cat))
    (?e (call-interactively #'supervisor-dashboard-edit))
    (?m (call-interactively #'supervisor-dashboard-target-members))
    (_ (message "Inspect: cancelled"))))

(defun supervisor-dashboard-timer-actions ()
  "Open timer actions submenu.
Timer actions operate on timer rows in the dashboard."
  (interactive)
  (pcase (read-char
          "Timer: [t]rigger [i]nfo [j]ump [r]eset [g]refresh ")
    (?t (call-interactively #'supervisor-dashboard-timer-trigger))
    (?i (call-interactively #'supervisor-dashboard-timer-info))
    (?j (call-interactively #'supervisor-dashboard-timer-jump))
    (?r (call-interactively #'supervisor-dashboard-timer-reset))
    (?g (call-interactively #'supervisor-dashboard-timer-refresh))
    (_ (message "Timer: cancelled"))))

;;; Transient Menu

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
         ["Service Actions"
          ("l" "Lifecycle..." supervisor-dashboard-lifecycle)
          ("p" "Policy..." supervisor-dashboard-policy)
          ("i" "Inspect..." supervisor-dashboard-inspect)]
         ["Timers"
          ("y t" "Trigger" supervisor-dashboard-timer-trigger)
          ("y i" "Info" supervisor-dashboard-timer-info)
          ("y j" "Jump to target" supervisor-dashboard-timer-jump)
          ("y r" "Reset state" supervisor-dashboard-timer-reset)
          ("y g" "Refresh timers" supervisor-dashboard-timer-refresh)]
         ["Navigation"
          ("f" "Target filter" supervisor-dashboard-cycle-filter)
          ("F" "Tag filter" supervisor-dashboard-cycle-tag-filter)
          ("v" "Show targets" supervisor-dashboard-toggle-targets)
          ("V" "Show init targets" supervisor-dashboard-toggle-init-targets)
          ("g" "Refresh" supervisor-dashboard-refresh)
          ("G" "Auto-refresh" supervisor-dashboard-toggle-auto-refresh)]
         ["System"
          ("t" "Proced" proced)
          ("T" "Proced auto" supervisor-dashboard-toggle-proced-auto-update)
          ("X" "Daemon-reload" supervisor-dashboard-daemon-reload)]
         ["Help"
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
  (when-let* ((raw-id (tabulated-list-get-id)))
    (unless (equal raw-id supervisor--dashboard-last-id)
      (setq supervisor--dashboard-last-id raw-id)
      ;; Only echo typed row IDs longer than column width (15)
      (when-let* ((id (supervisor--row-id raw-id)))
        (when (> (length id) 15)
          (message "%s" id))))))

(define-derived-mode supervisor-dashboard-mode tabulated-list-mode "Supervisor"
  "Major mode for the supervisor dashboard."
  (setq tabulated-list-format [("ID" 16 t)
                               ("TYPE" 8 t)
                               ("TARGET" 10 t)
                               ("ENABLED" 8 t)
                               ("STATUS" 10 t)
                               ("RESTART" 11 t)
                               ("LOG" 5 t)
                               ("PID" 7 t)
                               ("REASON" 30 t)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header)
  ;; Echo full ID in minibuffer when point moves between rows
  (add-hook 'post-command-hook #'supervisor--dashboard-echo-id nil t)
  ;; Clean up auto-refresh timer when buffer is killed
  (add-hook 'kill-buffer-hook #'supervisor--cleanup-auto-refresh nil t))

(defun supervisor--status-face (status)
  "Return the face for STATUS string."
  (pcase status
    ("running" 'supervisor-status-running)
    ("active" 'supervisor-status-running)
    ("done" 'supervisor-status-done)
    ("failed" 'supervisor-status-failed)
    ("dead" 'supervisor-status-dead)
    ("invalid" 'supervisor-status-invalid)
    ("pending" 'supervisor-status-pending)
    ("stopped" 'supervisor-status-stopped)
    ("masked" 'supervisor-status-dead)
    ("reached" 'supervisor-status-running)
    ("degraded" 'supervisor-status-failed)
    ("converging" 'supervisor-status-pending)
    ("disabled" 'supervisor-status-stopped)
    ("unreachable" 'supervisor-status-unreachable)
    (_ nil)))

(defun supervisor--propertize-status (status)
  "Return STATUS string with appropriate face applied."
  (let ((face (supervisor--status-face status)))
    (if face
        (propertize status 'face face)
      status)))

(defun supervisor--make-blank-row (n)
  "Create a blank row for visual spacing.
N makes the symbol ID unique."
  (list (intern (format "--blank-%d--" n))
        (vector "" "" "" "" "" "" "" "" "")))

(defun supervisor--make-health-summary-row (&optional snapshot programs)
  "Return a non-interactive row that summarizes service health.
Counts are service-scoped and do not include timer rows.
SNAPSHOT and PROGRAMS are forwarded to `supervisor--health-counts'."
  (let* ((counts (supervisor--health-counts snapshot programs))
         (running (plist-get counts :running))
         (active (plist-get counts :active))
         (done (plist-get counts :done))
         (pending (plist-get counts :pending))
         (failed (plist-get counts :failed))
         (invalid (plist-get counts :invalid))
         (active-text (if (> active 0)
                          (concat (propertize (format "%d" active)
                                              'face 'supervisor-status-running)
                                  " active")
                        "-")))
    (list '--health--
          (vector (propertize "services" 'face 'supervisor-section-separator)
                  (concat (propertize (format "%d" running)
                                      'face 'supervisor-status-running)
                          " run")
                  (concat (propertize (format "%d" done)
                                      'face 'supervisor-status-done)
                          " done")
                  (concat (propertize (format "%d" pending)
                                      'face 'supervisor-status-pending)
                          " pend")
                  (concat (propertize (format "%d" failed)
                                      'face 'supervisor-status-failed)
                          " fail")
                  (concat (propertize (format "%d" invalid)
                                      'face 'supervisor-status-invalid)
                          " inv")
                  "-"
                  "-"
                  active-text))))

(defun supervisor--separator-row-p (id)
  "Return non-nil if ID represents a separator or summary row."
  (and id (symbolp id)))

(defun supervisor--timer-row-p (id)
  "Return non-nil if ID represents a timer row."
  (and (consp id) (eq (car id) :timer)))

(defun supervisor--service-row-p (id)
  "Return non-nil if ID represents a service row."
  (and (consp id) (eq (car id) :service)))

(defun supervisor--target-type-p (type)
  "Return non-nil if TYPE is the target entry type."
  (eq type 'target))

(defun supervisor--init-transition-target-p (id)
  "Return non-nil if ID is an init-transition target.
Init-transition targets are rescue, shutdown, poweroff, reboot,
runlevel0, runlevel1, and runlevel6."
  (member id supervisor--init-transition-target-ids))

(defun supervisor--row-kind (id)
  "Return the kind of dashboard row for ID.
Return `:service', `:timer', or `:separator'."
  (cond
   ((supervisor--service-row-p id) :service)
   ((supervisor--timer-row-p id) :timer)
   ((supervisor--separator-row-p id) :separator)
   (t nil)))

(defun supervisor--row-id (id)
  "Extract the string ID from a typed row ID.
For cons cell IDs like (:service . \"foo\"), return \"foo\".
For symbol IDs (separators), return nil."
  (when (consp id)
    (cdr id)))

(defconst supervisor--timer-row-rejection
  "Not available for timer rows: use timer actions (y or ? -> Timers)"
  "Stable error message for service-only commands on timer rows.")

(defun supervisor--require-service-row ()
  "Return the string ID of the service row at point.
Signal `user-error' if point is on a separator, timer, or empty row."
  (let ((raw-id (tabulated-list-get-id)))
    (cond
     ((null raw-id)
      (user-error "No entry at point"))
     ((supervisor--separator-row-p raw-id)
      (user-error "Not available on separator row"))
     ((supervisor--timer-row-p raw-id)
      (user-error "%s" supervisor--timer-row-rejection))
     (t
      (or (supervisor--row-id raw-id)
          (user-error "No entry at point"))))))

(defun supervisor--timer-projection (timer)
  "Return projected TIMER fields as a plist for display.
Provides rendering parity with `list-timers' CLI output.
Fields: :id, :target, :enabled, :last-run, :next-run, :last-exit,
:result-reason, :target-type, :last-result.
The :target-type is resolved from current config, not runtime state,
so it is always current even for never-triggered timers."
  (let* ((id (supervisor-timer-id timer))
         (target-id (supervisor-timer-target timer))
         (state (gethash id supervisor--timer-state))
         (target-entry (supervisor--get-entry-for-id target-id))
         (target-type (when target-entry
                        (supervisor-entry-type target-entry))))
    (list :id id
          :target target-id
          :enabled (supervisor-timer-enabled timer)
          :last-run (plist-get state :last-run-at)
          :next-run (plist-get state :next-run-at)
          :last-exit (plist-get state :last-exit)
          :result-reason (plist-get state :last-result-reason)
          :target-type target-type
          :last-result (plist-get state :last-result))))

(defun supervisor--make-dashboard-entry (id type parent-target enabled-p
                                            restart-policy logging-p
                                            &optional snapshot)
  "Create a dashboard entry vector for ID.
TYPE, PARENT-TARGET, ENABLED-P, RESTART-POLICY, LOGGING-P are entry fields.
PARENT-TARGET is the first target this entry belongs to (via wanted-by or
required-by), shown in the TARGET column for non-target entries.
If SNAPSHOT is provided, read runtime state from it."
  (let* ((status-pid (supervisor--compute-entry-status id type snapshot))
         (status (car status-pid))
         (pid-raw (cdr status-pid))
         (pid (if (and (eq type 'oneshot)
                       (string-prefix-p "exit:" pid-raw))
                  "-"
                pid-raw))
         (reason (supervisor--compute-entry-reason id type snapshot))
         ;; For overrides, use snapshot if provided, otherwise globals
         (mask-hash (if snapshot
                       (or (supervisor-snapshot-mask-override snapshot)
                           (make-hash-table :test 'equal))
                     supervisor--mask-override))
         (mask-val (gethash id mask-hash))
         (is-masked (eq mask-val 'masked))
         (enabled-override (if snapshot
                               (gethash id (supervisor-snapshot-enabled-override snapshot))
                             (gethash id supervisor--enabled-override)))
         (effective-enabled (cond (is-masked nil)
                                  ((eq enabled-override 'enabled) t)
                                  ((eq enabled-override 'disabled) nil)
                                  (t enabled-p)))
         (restart-override (if snapshot
                               (gethash id (supervisor-snapshot-restart-override snapshot))
                             (gethash id supervisor--restart-override)))
         (effective-restart (cond ((eq restart-override 'enabled) 'always)
                                  ((eq restart-override 'disabled) 'no)
                                  ((memq restart-override
                                         supervisor--valid-restart-policies)
                                   restart-override)
                                  (t (supervisor--normalize-restart-policy
                                      restart-policy))))
         (restart-str (if (memq type '(oneshot target))
                          "n/a"
                        (symbol-name effective-restart)))
         (log-override (if snapshot
                           (gethash id (supervisor-snapshot-logging-override snapshot))
                         (gethash id supervisor--logging)))
         (effective-logging (cond ((eq log-override 'enabled) t)
                                  ((eq log-override 'disabled) nil)
                                  (t logging-p))))
    (vector id
            (propertize (symbol-name type)
                        'face (pcase type
                                ('oneshot 'supervisor-type-oneshot)
                                ('target 'supervisor-type-target)
                                (_ 'supervisor-type-simple)))
            (if (eq type 'target)
                (let* ((effective-id
                        (cond ((equal id "default.target")
                               (supervisor--resolve-default-target-link))
                              ((supervisor--target-alias-p id)
                               (supervisor--resolve-target-alias id))
                              (t id)))
                       (conv (when (hash-table-p supervisor--target-convergence)
                               (gethash effective-id
                                        supervisor--target-convergence)))
                       (closure (and (supervisor-plan-p supervisor--current-plan)
                                     (supervisor-plan-activation-closure
                                      supervisor--current-plan)))
                       (conv-str (cond
                                  ((and (hash-table-p closure)
                                        (not (gethash effective-id closure)))
                                   "unreachable")
                                  ((eq conv 'reached) "reached")
                                  ((eq conv 'degraded) "degraded")
                                  ((eq conv 'converging) "pending")
                                  (t "pending"))))
                  (supervisor--propertize-status conv-str))
              (or parent-target "-"))
            (propertize (if effective-enabled "yes" "no")
                        'face (if effective-enabled
                                  'supervisor-enabled-yes
                                'supervisor-enabled-no))
            (supervisor--propertize-status status)
            restart-str
            (if (eq type 'target) "-" (if effective-logging "yes" "no"))
            pid
            (if (or (null reason) (string-empty-p reason))
                ""
              (propertize reason 'face 'supervisor-reason)))))

(defun supervisor--format-timer-relative-time (timestamp)
  "Format TIMESTAMP as relative time for dashboard display."
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

(defun supervisor--make-timer-separator ()
  "Create a header row for the Timers section with column labels."
  (list '--timers--
        (vector (propertize "── Timers" 'face 'supervisor-section-separator)
                (propertize "TARGET" 'face 'supervisor-section-separator)
                (propertize "ENABLED" 'face 'supervisor-section-separator)
                (propertize "LAST-RUN" 'face 'supervisor-section-separator)
                (propertize "NEXT-RUN" 'face 'supervisor-section-separator)
                (propertize "EXIT" 'face 'supervisor-section-separator)
                (propertize "REASON" 'face 'supervisor-section-separator)
                (propertize "TYPE" 'face 'supervisor-section-separator)
                (propertize "RESULT" 'face 'supervisor-section-separator))))

(defun supervisor--make-services-separator ()
  "Create a header row for the Services section with column labels."
  (list '--services--
        (vector (propertize "── Services" 'face 'supervisor-section-separator)
                (propertize "TYPE" 'face 'supervisor-section-separator)
                (propertize "TARGET" 'face 'supervisor-section-separator)
                (propertize "ENABLED" 'face 'supervisor-section-separator)
                (propertize "STATUS" 'face 'supervisor-section-separator)
                (propertize "RESTART" 'face 'supervisor-section-separator)
                (propertize "LOG" 'face 'supervisor-section-separator)
                (propertize "PID" 'face 'supervisor-section-separator)
                (propertize "REASON" 'face 'supervisor-section-separator))))

(defun supervisor--make-timer-separator-disabled ()
  "Create a timer section header showing disabled state."
  (list '--timers--
        (vector (propertize "── Timers (disabled)" 'face 'supervisor-section-separator)
                "" "" "" "" "" "" "" "")))

(defun supervisor--make-timer-separator-empty ()
  "Create a timer section header showing no timers configured."
  (list '--timers--
        (vector (propertize "── Timers" 'face 'supervisor-section-separator)
                (propertize "no timers configured" 'face 'supervisor-status-stopped)
                "" "" "" "" "" "" "")))

(defun supervisor--make-timer-dashboard-entry (timer)
  "Create a dashboard entry vector for TIMER.
Columns are mapped to match `list-timers' semantics:
ID, TARGET, ENABLED, LAST-RUN, NEXT-RUN, EXIT, REASON, TYPE, RESULT."
  (let* ((proj (supervisor--timer-projection timer))
         (id (plist-get proj :id))
         (target (plist-get proj :target))
         (enabled (plist-get proj :enabled))
         (last-run (plist-get proj :last-run))
         (next-run (plist-get proj :next-run))
         (last-exit (plist-get proj :last-exit))
         (result-reason (plist-get proj :result-reason))
         (target-type (plist-get proj :target-type))
         (last-result (plist-get proj :last-result)))
    (vector (propertize id 'face 'supervisor-type-timer)
            (propertize (or target "-") 'face 'supervisor-type-oneshot)
            (propertize (if enabled "yes" "no")
                        'face (if enabled
                                  'supervisor-enabled-yes
                                'supervisor-enabled-no))
            (supervisor--format-timer-relative-time last-run)
            (supervisor--format-timer-relative-time next-run)
            (if (null last-exit) "-" (number-to-string last-exit))
            (if result-reason (symbol-name result-reason) "-")
            (if target-type (symbol-name target-type) "-")
            (if last-result
                (supervisor--propertize-status
                 (symbol-name last-result))
              "-"))))

(defun supervisor--get-timer-entries ()
  "Generate timer entries for the dashboard.
Returns list of (typed-id vector) pairs with (:timer . ID) keys."
  (let ((entries nil))
    (dolist (timer supervisor--timer-list)
      (let ((id (supervisor-timer-id timer)))
        (push (list (cons :timer id)
                    (supervisor--make-timer-dashboard-entry timer))
              entries)))
    ;; Also show invalid timers with matching column layout.
    ;; Collect and sort by ID for deterministic ordering.
    (let ((invalid-ids nil))
      (maphash (lambda (id _reason) (push id invalid-ids))
               supervisor--invalid-timers)
      (dolist (id (sort invalid-ids #'string<))
        (let ((reason (gethash id supervisor--invalid-timers)))
          (push (list (cons :timer id)
                      (vector (propertize id 'face 'supervisor-status-invalid)
                              "-"
                              "-"
                              "-"
                              (supervisor--propertize-status "invalid")
                              "-"
                              (propertize reason 'face 'supervisor-reason)
                              "" ""))
                entries))))
    (nreverse entries)))

(defun supervisor--get-entries (&optional snapshot programs)
  "Generate entries for the dashboard (deduplicates on the fly).
Respects tag filter when set.
If SNAPSHOT is provided, read runtime state from it.
If PROGRAMS is provided, use it instead of calling
`supervisor--effective-programs'.

Layout order: services section header, service rows, then timer section
if `supervisor-dashboard-show-timers' is non-nil."
  (let* ((snapshot (or snapshot (supervisor--build-snapshot)))
         (programs (or programs (supervisor--effective-programs)))
         (entries nil)
         (seen (make-hash-table :test 'equal))
         (tag-filter supervisor--dashboard-tag-filter)
         (target-filter supervisor--dashboard-target-filter)
         (show-targets supervisor--dashboard-show-targets)
         (show-init-targets supervisor--dashboard-show-init-targets)
         (target-member-hash
          (when (and supervisor--current-plan target-filter)
            (supervisor-plan-target-members supervisor--current-plan)))
         (invalid-hash (supervisor-snapshot-invalid snapshot))
         (idx 0))
    (dolist (entry programs)
      (let* ((raw-id (supervisor--extract-id entry idx))
             (invalid-reason (gethash raw-id invalid-hash)))
        (cl-incf idx)
        (unless (gethash raw-id seen)
          (puthash raw-id t seen)
          (if invalid-reason
              (push (list (cons :service raw-id)
                          (vector raw-id "-" "-" "-"
                                  (supervisor--propertize-status "invalid")
                                  "-" "-" "-"
                                  invalid-reason))
                    entries)
            (let* ((parsed (supervisor--parse-entry entry))
                   (id (supervisor-entry-id parsed))
                   (enabled-p (supervisor-entry-enabled-p parsed))
                   (restart-policy (supervisor-entry-restart-policy parsed))
                   (logging-p (supervisor-entry-logging-p parsed))
                   (type (supervisor-entry-type parsed))
                   (tags (supervisor-entry-tags parsed))
                   (parent-target
                    (or (car (supervisor-entry-required-by parsed))
                        (car (supervisor-entry-wanted-by parsed)))))
              (when (and (or (null tag-filter) (member tag-filter tags))
                        (or (null target-filter)
                            (string= id target-filter)
                            (member target-filter
                                    (supervisor-entry-wanted-by parsed))
                            (member target-filter
                                    (supervisor-entry-required-by parsed))
                            (when (hash-table-p target-member-hash)
                              (let ((members (gethash target-filter
                                                      target-member-hash)))
                                (or (member id (plist-get members :requires))
                                    (member id (plist-get members :wants))))))
                        ;; Target visibility: hide targets by default
                        ;; unless toggled or a target-filter is active.
                        (or (not (supervisor--target-type-p type))
                            target-filter
                            (and show-targets
                                 (or show-init-targets
                                     (not (supervisor--init-transition-target-p
                                           id))))))
                (push (list (cons :service id)
                            (supervisor--make-dashboard-entry
                             id type parent-target enabled-p restart-policy
                             logging-p snapshot))
                      entries)))))))
    ;; Include invalid authority units not already seen via programs
    (when (boundp 'supervisor--unit-file-invalid)
      (maphash (lambda (id reason)
                 (unless (gethash id seen)
                   (puthash id t seen)
                   (push (list (cons :service id)
                               (vector id "-" "-" "-"
                                       (supervisor--propertize-status "invalid")
                                       "-" "-" "-"
                                       reason))
                         entries)))
               (symbol-value 'supervisor--unit-file-invalid)))
    (setq entries (nreverse entries))
    (let ((final-entries (mapcar (lambda (e) (list (car e) (cadr e))) entries)))
      ;; Services section header is always first row in buffer body.
      (setq final-entries
            (cons (supervisor--make-services-separator) final-entries))
      ;; Timer section (tag filter does not hide timers)
      (when supervisor-dashboard-show-timers
        (setq final-entries
              (append final-entries
                      (cond
                       ;; Timer mode gate off
                       ((not supervisor-timer-subsystem-mode)
                        (list (supervisor--make-timer-separator-disabled)))
                       ;; No timers configured
                       ((and (null supervisor--timer-list)
                             (= 0 (hash-table-count supervisor--invalid-timers)))
                        (list (supervisor--make-timer-separator-empty)))
                       ;; Timers present
                       (t
                        (cons (supervisor--make-timer-separator)
                              (supervisor--get-timer-entries)))))))
      final-entries)))

(defun supervisor--health-counts (&optional snapshot programs)
  "Return dashboard health counters as a plist.
If SNAPSHOT is provided, read state from it; otherwise read from globals.
If PROGRAMS is provided, use it instead of calling
`supervisor--effective-programs'."
  (let ((running 0) (active 0) (done 0) (failed 0) (invalid 0) (pending 0)
        (disabled 0)
        (programs (or programs (supervisor--effective-programs)))
        (seen (make-hash-table :test 'equal))
        (invalid-hash (if snapshot
                          (supervisor-snapshot-invalid snapshot)
                        supervisor--invalid))
        (entry-state-hash (if snapshot
                              (supervisor-snapshot-entry-state snapshot)
                            supervisor--entry-state))
        (process-alive (when snapshot (supervisor-snapshot-process-alive snapshot)))
        (failed-hash (if snapshot
                         (supervisor-snapshot-failed snapshot)
                       supervisor--failed))
        (oneshot-hash (if snapshot
                          (supervisor-snapshot-oneshot-exit snapshot)
                        supervisor--oneshot-completed))
        (remain-hash (if snapshot
                         (or (supervisor-snapshot-remain-active snapshot)
                             (make-hash-table :test 'equal))
                       supervisor--remain-active))
        (idx 0))
    (dolist (entry programs)
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
                       (type (supervisor-entry-type parsed))
                       (e-state (gethash id entry-state-hash))
                       (alive (if snapshot
                                  (gethash id process-alive)
                                (let ((proc (gethash id supervisor--processes)))
                                  (and proc (process-live-p proc)))))
                       (is-failed (gethash id failed-hash))
                       (oneshot-p (eq type 'oneshot))
                       (oneshot-exit (gethash id oneshot-hash)))
                  (cond
                   ;; Skip target entries from health counts (no process)
                   ((eq type 'target))
                   ;; Disabled entries are counted separately
                   ((eq e-state 'disabled) (cl-incf disabled))
                   (alive (cl-incf running))
                   (is-failed (cl-incf failed))
                   ((and oneshot-p oneshot-exit (/= oneshot-exit 0)) (cl-incf failed))
                   ((and oneshot-p (gethash id remain-hash)) (cl-incf active))
                   ((and oneshot-p oneshot-exit) (cl-incf done))
                   (oneshot-p (cl-incf pending))
                   (t (cl-incf pending))))))))))
    ;; Count invalid authority units not already seen via programs
    (when (boundp 'supervisor--unit-file-invalid)
      (maphash (lambda (id _reason)
                 (unless (gethash id seen)
                   (puthash id t seen)
                   (cl-incf invalid)))
               (symbol-value 'supervisor--unit-file-invalid)))
    (list :running running
          :active active
          :done done
          :pending pending
          :failed failed
          :invalid invalid
          :disabled disabled)))

(defun supervisor--health-summary (&optional snapshot programs)
  "Return compact health summary string.
If SNAPSHOT is provided, read state from it; otherwise read from globals.
If PROGRAMS is provided, use it instead of calling
`supervisor--effective-programs'."
  (let* ((counts (supervisor--health-counts snapshot programs))
         (running (plist-get counts :running))
         (active (plist-get counts :active))
         (done (plist-get counts :done))
         (pending (plist-get counts :pending))
         (failed (plist-get counts :failed))
         (invalid (plist-get counts :invalid))
         (disabled (plist-get counts :disabled)))
    (concat (propertize (format "%d" running) 'face 'supervisor-status-running)
            " run | "
            (when (> active 0)
              (concat (propertize (format "%d" active) 'face 'supervisor-status-running)
                      " act | "))
            (propertize (format "%d" done) 'face 'supervisor-status-done)
            " done | "
            (propertize (format "%d" pending) 'face 'supervisor-status-pending)
            " pend | "
            (when (> disabled 0)
              (concat (propertize (format "%d" disabled) 'face 'supervisor-status-stopped)
                      " off | "))
            (propertize (format "%d" failed) 'face 'supervisor-status-failed)
            " fail | "
            (propertize (format "%d" invalid) 'face 'supervisor-status-invalid)
            " inv")))

(defvar supervisor--help-text
  (concat "[f]ilter [v]targets [g]refresh [G]live [t]proced [T]auto "
          "[l]ifecycle [p]olicy [i]nspect [y]timers [?]menu [h]elp [q]uit")
  "Key hints displayed in dashboard header.")

(defun supervisor--dashboard-column-header ()
  "Return dashboard column header text.
Use `tabulated-list-header-string' when available.  Fall back to
rendering from `tabulated-list-format' when running in batch or
other contexts where the tabulated header string is empty."
  (let ((header (and (boundp 'tabulated-list-header-string)
                     tabulated-list-header-string)))
    (if (and (stringp header) (> (length header) 0))
        header
      (let ((format-spec (and (boundp 'tabulated-list-format)
                              tabulated-list-format)))
        (if (vectorp format-spec)
            (mapconcat
             (lambda (column)
               (let* ((name (nth 0 column))
                      (width (max 1 (or (nth 1 column) 1)))
                      (text (format "%s" name)))
                 (truncate-string-to-width
                  (format (format "%%-%ds" width) text)
                  width nil ?\s)))
             (append format-spec nil)
             " ")
          "")))))

(defun supervisor--dashboard-header-line (&optional snapshot programs)
  "Return dashboard header line text with service health counters.
SNAPSHOT and PROGRAMS are forwarded to `supervisor--health-counts'."
  (let* ((counts (supervisor--health-counts snapshot programs))
         (running (plist-get counts :running))
         (done (plist-get counts :done))
         (pending (plist-get counts :pending))
         (failed (plist-get counts :failed))
         (invalid (plist-get counts :invalid))
         (base (concat (propertize (format "%d" running)
                                   'face 'supervisor-status-running)
                       " run    "
                       (propertize (format "%d" done)
                                   'face 'supervisor-status-done)
                       " done   "
                       (propertize (format "%d" pending)
                                   'face 'supervisor-status-pending)
                       " pend  "
                       (propertize (format "%d" failed)
                                   'face 'supervisor-status-failed)
                       " fail     "
                       (propertize (format "%d" invalid)
                                   'face 'supervisor-status-invalid)
                       " inv"))
         (root (when supervisor--current-plan
                 (supervisor-plan-activation-root supervisor--current-plan)))
         (view-parts (list "services")))
    (when supervisor--dashboard-show-targets
      (setq view-parts (append view-parts (list "targets")))
      (when supervisor--dashboard-show-init-targets
        (setq view-parts (append view-parts (list "init")))))
    (let* ((view-tag (propertize
                      (concat "[" (mapconcat #'identity view-parts "+") "]")
                      'face 'supervisor-section-separator))
           (result (if root
                       (concat base "    "
                               (propertize "root" 'face 'bold) ": " root)
                     base)))
      (concat result "    " view-tag))))

(defun supervisor--refresh-dashboard ()
  "Refresh the dashboard buffer if it exists.
Loads programs once and builds a single runtime snapshot, passing both
to entries and health summary for consistency within a single refresh."
  (when-let* ((buf (get-buffer "*supervisor*")))
    (with-current-buffer buf
      (let* ((programs (supervisor--effective-programs))
             (snapshot (supervisor--build-snapshot))
             (pos (point)))
        (setq tabulated-list-entries
              (supervisor--get-entries snapshot programs))
        (tabulated-list-print t)
        (setq header-line-format
              (supervisor--dashboard-header-line snapshot programs))
        (goto-char (min pos (point-max)))))))

(defun supervisor-dashboard-refresh ()
  "Refresh the dashboard."
  (interactive)
  (supervisor--refresh-dashboard))

(defun supervisor--all-target-ids ()
  "Return sorted list of all target IDs from current plan.
Includes all target-type entries, even those with no members."
  (let ((ids nil))
    (when supervisor--current-plan
      ;; Collect targets from membership hash (targets with members)
      (when-let* ((members (supervisor-plan-target-members
                            supervisor--current-plan)))
        (when (hash-table-p members)
          (maphash (lambda (id _) (cl-pushnew id ids :test #'equal))
                   members)))
      ;; Collect target-type entries from plan (includes empty targets)
      (dolist (entry (supervisor-plan-entries supervisor--current-plan))
        (when (eq (supervisor-entry-type entry) 'target)
          (cl-pushnew (supervisor-entry-id entry) ids :test #'equal))))
    (sort ids #'string<)))

(defun supervisor-dashboard-cycle-filter ()
  "Cycle dashboard target filter through all defined targets."
  (interactive)
  (let* ((all-targets (supervisor--all-target-ids))
         (current supervisor--dashboard-target-filter)
         (idx (cl-position current all-targets :test #'equal)))
    (setq supervisor--dashboard-target-filter
          (cond
           ((null all-targets) nil)
           ((null idx) (car all-targets))
           ((= idx (1- (length all-targets))) nil)
           (t (nth (1+ idx) all-targets))))
    (message "Target filter: %s"
             (or supervisor--dashboard-target-filter "all"))
    (supervisor--refresh-dashboard)))

(defun supervisor--all-tags ()
  "Return list of all unique tags used in entries."
  (let ((tags nil))
    (dolist (entry (supervisor--effective-programs))
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

(defun supervisor-dashboard-toggle-targets ()
  "Toggle visibility of target-type entries in the dashboard.
When targets are hidden (the default), only service and oneshot rows
are shown.  Init-transition targets remain hidden unless separately
toggled with \\[supervisor-dashboard-toggle-init-targets]."
  (interactive)
  (setq supervisor--dashboard-show-targets
        (not supervisor--dashboard-show-targets))
  (message "Targets: %s"
           (if supervisor--dashboard-show-targets "shown" "hidden"))
  (supervisor--refresh-dashboard))

(defun supervisor-dashboard-toggle-init-targets ()
  "Toggle visibility of init-transition targets in the dashboard.
Init-transition targets (rescue, shutdown, poweroff, reboot, runlevel0,
runlevel1, runlevel6) are hidden by default even when regular targets are shown.
This toggle has no effect when targets are hidden entirely."
  (interactive)
  (setq supervisor--dashboard-show-init-targets
        (not supervisor--dashboard-show-init-targets))
  (if supervisor--dashboard-show-targets
      (message "Init targets: %s"
               (if supervisor--dashboard-show-init-targets "shown" "hidden"))
    (message "Init targets: %s (targets are hidden, toggle with v first)"
             (if supervisor--dashboard-show-init-targets "shown" "hidden")))
  (supervisor--refresh-dashboard))

(defun supervisor-dashboard-target-members ()
  "Show members of target at point."
  (interactive)
  (let* ((id (supervisor--require-service-row))
         (entry (supervisor--get-entry-for-id id)))
    (if (or (not entry) (not (eq (supervisor-entry-type entry) 'target)))
        (message "Not a target entry: %s" id)
      (let ((members (when (hash-table-p supervisor--target-members)
                       (gethash id supervisor--target-members))))
        (if (not members)
            (message "%s: no members" id)
          (let ((req (plist-get members :requires))
                (wants (plist-get members :wants)))
            (message "%s members: requires=[%s] wants=[%s]"
                     id
                     (if req (mapconcat #'identity req ", ") "none")
                     (if wants (mapconcat #'identity wants ", ") "none"))))))))

(defvar supervisor--status-legend
  "Status: running=active process | active=oneshot remain-after-exit | done=oneshot completed ok | failed=crashed/exit>0 | dead=crash-loop | pending=not yet started | unreachable=outside activation closure | stopped=terminated | invalid=config error"
  "Legend explaining status column values.")

(defun supervisor--describe-format-duration (seconds)
  "Format SECONDS as a human-readable duration string."
  (let ((s (round seconds)))
    (cond
     ((< s 60) (format "%ds" s))
     ((< s 3600) (format "%dm%ds" (/ s 60) (% s 60)))
     ((< s 86400) (format "%dh%dm" (/ s 3600) (% (/ s 60) 60)))
     (t (format "%dd%dh" (/ s 86400) (% (/ s 3600) 24))))))

(defun supervisor-dashboard-describe-entry ()
  "Show detailed information about entry at point.
With prefix argument, show status legend instead."
  (interactive)
  (if current-prefix-arg
      (message "%s" supervisor--status-legend)
    (let* ((id (supervisor--require-service-row))
           (invalid-reason (gethash id supervisor--invalid)))
      (if invalid-reason
          (message "INVALID: %s" invalid-reason)
        (let ((entry (supervisor--get-entry-for-id id)))
          (if (not entry)
              (message "Entry not found: %s" id)
            (supervisor--describe-entry-detail id entry)))))))

(defun supervisor--describe-entry-detail (id entry)
  "Show detailed telemetry for ID with parsed ENTRY in a help window."
  (let* ((type (supervisor-entry-type entry))
         (enabled-p (supervisor-entry-enabled-p entry))
         (restart-policy (supervisor-entry-restart-policy entry))
         (logging-p (supervisor-entry-logging-p entry))
         (delay (supervisor-entry-delay entry))
         (after (supervisor-entry-after entry))
         (requires (supervisor-entry-requires entry))
         (description (supervisor-entry-description entry))
         (documentation (supervisor-entry-documentation entry))
         (status-result (supervisor--compute-entry-status id type))
         (status (car status-result))
         (reason (supervisor--compute-entry-reason id type))
         (proc (gethash id supervisor--processes))
         (pid (when (and proc (process-live-p proc)) (process-id proc)))
         (eff-enabled (supervisor--get-effective-enabled id enabled-p))
         (eff-restart (unless (eq type 'oneshot)
                        (supervisor--get-effective-restart id restart-policy)))
         (eff-logging (supervisor--get-effective-logging id logging-p))
         (uptime (supervisor--telemetry-uptime id))
         (restart-count (supervisor--telemetry-restart-count id))
         (last-exit (supervisor--telemetry-last-exit id))
         (last-exit-info (supervisor--telemetry-last-exit-info id))
         (next-eta (supervisor--telemetry-next-restart-eta id))
         (metrics (when pid (supervisor--telemetry-process-metrics pid)))
         (unit-file
          (cond
           ((fboundp 'supervisor--unit-file-existing-path)
            (supervisor--unit-file-existing-path id))
           ((fboundp 'supervisor--unit-file-path)
            (when-let* ((path (supervisor--unit-file-path id)))
              (when (file-exists-p path)
                path)))))
         (log-tail (supervisor--telemetry-log-tail id 5)))
    (with-help-window "*supervisor-info*"
      ;; Header
      (princ (format "%s - %s\n" id (or description (symbol-name type))))
      (princ (make-string (+ (length id) 3
                             (length (or description (symbol-name type))))
                          ?-))
      (princ "\n\n")
      ;; Status block
      (princ (format "   Status: %s%s\n" status
                     (if reason (format " (%s)" reason) "")))
      (when pid
        (princ (format "      PID: %d\n" pid))
        (when-let* ((tree (supervisor--telemetry-process-tree pid)))
          (let ((count (plist-get tree :count))
                (pids (plist-get tree :pids)))
            (princ (format "     Tree: %d descendant%s [%s]\n"
                           count (if (= count 1) "" "s")
                           (mapconcat (lambda (p) (format "%d" p))
                                      pids ", "))))))
      (when uptime
        (princ (format "   Uptime: %s\n"
                       (supervisor--describe-format-duration uptime))))
      (princ "\n")
      ;; Config block
      (princ (format "     Type: %s\n" type))
      (princ (format "  Enabled: %s%s\n"
                     (if eff-enabled "yes" "no")
                     (if (not (eq eff-enabled enabled-p)) " (override)" "")))
      (when eff-restart
        (princ (format "  Restart: %s%s\n"
                       eff-restart
                       (if (not (eq eff-restart
                                    (supervisor--normalize-restart-policy
                                     restart-policy)))
                           " (override)" ""))))
      (princ (format "  Logging: %s%s\n"
                     (if eff-logging "yes" "no")
                     (if (not (eq eff-logging logging-p)) " (override)" "")))
      (princ (format "    Delay: %s\n" delay))
      (princ (format "    After: %s\n"
                     (if after (mapconcat #'identity after ", ") "none")))
      (when requires
        (princ (format " Requires: %s\n"
                       (mapconcat #'identity requires ", "))))
      ;; Target-specific convergence info
      (when (eq type 'target)
        (let* ((effective-id (supervisor--resolve-target-alias id))
               (conv (when (hash-table-p supervisor--target-convergence)
                       (gethash effective-id supervisor--target-convergence)))
               (reasons (when (hash-table-p supervisor--target-convergence-reasons)
                          (gethash effective-id supervisor--target-convergence-reasons)))
               (members (when (hash-table-p supervisor--target-members)
                          (gethash effective-id supervisor--target-members))))
          (princ (format "Converge: %s\n" (or conv "not started")))
          (when reasons
            (princ (format " Reasons: %s\n"
                           (mapconcat #'identity reasons "; "))))
          (when members
            (let ((req (plist-get members :requires))
                  (wants (plist-get members :wants)))
              (when req
                (princ (format " Req-mem: %s\n"
                               (mapconcat #'identity req ", "))))
              (when wants
                (princ (format "Want-mem: %s\n"
                               (mapconcat #'identity wants ", "))))))))
      ;; Oneshot-specific
      (when (eq type 'oneshot)
        (princ (format " Blocking: %s\n"
                       (if (supervisor-entry-oneshot-blocking entry)
                           "yes" "no")))
        (let ((timeout (supervisor-entry-oneshot-timeout entry)))
          (when timeout
            (princ (format "  Timeout: %ss\n" timeout)))))
      ;; Identity (privilege drop)
      (let ((u (supervisor-entry-user entry))
            (g (supervisor-entry-group entry)))
        (when (or u g)
          (when u (princ (format "     User: %s\n" u)))
          (when g (princ (format "    Group: %s\n" g)))))
      ;; Sandbox indicator
      (when (supervisor--sandbox-requesting-p entry)
        (let* ((profile (or (supervisor-entry-sandbox-profile entry) 'none))
               (effective-network
                (or (supervisor-entry-sandbox-network entry)
                    (supervisor--sandbox-profile-default-network profile))))
          (princ (format "  Sandbox: %s (network %s)\n"
                         profile effective-network))))
      ;; Resource limits
      (when (supervisor--limits-requesting-p entry)
        (let ((parts nil))
          (dolist (pair (list (cons #'supervisor-entry-limit-nofile "nofile")
                              (cons #'supervisor-entry-limit-nproc "nproc")
                              (cons #'supervisor-entry-limit-core "core")
                              (cons #'supervisor-entry-limit-fsize "fsize")
                              (cons #'supervisor-entry-limit-as "as")))
            (when-let* ((v (funcall (car pair) entry)))
              (push (format "%s=%s" (cdr pair) v) parts)))
          (when parts
            (princ (format "   Limits: %s\n"
                           (mapconcat #'identity (nreverse parts) ", "))))))
      ;; Runtime telemetry
      (when (or last-exit (> restart-count 0) next-eta metrics)
        (princ "\n")
        (when last-exit
          (let ((ts (when last-exit-info
                      (plist-get last-exit-info :timestamp))))
            (princ (format "Last exit: %s%s\n" last-exit
                           (if ts (format " (%.1fs ago)"
                                          (- (float-time) ts))
                             "")))))
        (when (> restart-count 0)
          (princ (format " Restarts: %d (in window)\n" restart-count)))
        (when next-eta
          (let ((remaining (- next-eta (float-time))))
            (when (> remaining 0)
              (princ (format "Next restart: in %.1fs\n" remaining)))))
        (when metrics
          (let ((parts nil))
            (when-let* ((rss (plist-get metrics :rss)))
              (push (format "RSS=%dKB" rss) parts))
            (when-let* ((pcpu (plist-get metrics :pcpu)))
              (push (format "CPU=%.1f%%" pcpu) parts))
            (when-let* ((pmem (plist-get metrics :pmem)))
              (push (format "MEM=%.1f%%" pmem) parts))
            (when-let* ((etime (plist-get metrics :etime)))
              (push (format "ETIME=%s"
                            (supervisor--describe-format-duration etime))
                    parts))
            (when-let* ((thcount (plist-get metrics :thcount)))
              (push (format "TASKS=%d" thcount) parts))
            (when parts
              (princ (format "  Metrics: %s\n"
                             (mapconcat #'identity
                                        (nreverse parts) " ")))))))
      ;; Unit file and docs
      (when (or unit-file documentation)
        (princ "\n")
        (when unit-file
          (let ((tier (when (fboundp 'supervisor--authority-tier-for-id)
                        (supervisor--authority-tier-for-id id))))
            (princ (format "Unit file: %s%s\n" unit-file
                           (if tier (format " (tier %d)" tier) "")))))
        (when documentation
          (princ (format "     Docs: %s\n"
                         (mapconcat #'identity documentation ", ")))))
      ;; Log tail
      (when log-tail
        (princ "\nRecent log:\n")
        (princ log-tail)))))

(defun supervisor-dashboard-help ()
  "Show full help for supervisor dashboard."
  (interactive)
  (with-help-window "*Supervisor Help*"
    (princ "Supervisor Dashboard Help\n")
    (princ "=========================\n\n")
    (princ "TOP-LEVEL KEYS\n")
    (princ "--------------\n")
    (princ "  f     Cycle target filter (all -> target1 -> target2 -> ...)\n")
    (princ "  F     Cycle tag filter\n")
    (princ "  v     Toggle target visibility (service-first default)\n")
    (princ "  V     Toggle init-transition targets (rescue/shutdown/poweroff/reboot/rl0/1/6)\n")
    (princ "  g     Refresh dashboard\n")
    (princ "  G     Toggle auto-refresh (live monitoring)\n")
    (princ "  t     Open proced (system process list)\n")
    (princ "  T     Toggle proced auto-update mode\n")
    (princ "  l     Lifecycle submenu (service rows only)\n")
    (princ "  p     Policy submenu (service rows only)\n")
    (princ "  i     Inspect submenu (service rows only)\n")
    (princ "  y     Timer actions submenu (timer rows only)\n")
    (princ "  ?     Open action menu (transient)\n")
    (princ "  h     Show this help\n")
    (princ "  q     Quit dashboard\n\n")
    (princ "LIFECYCLE (l) - service rows only\n")
    (princ "---------------------------------\n")
    (princ "  s     Start process\n")
    (princ "  t     Stop process (graceful, suppresses restart)\n")
    (princ "  r     Restart process (stop + start)\n")
    (princ "  k     Kill process (send signal, restart policy unchanged)\n")
    (princ "  u     Reload unit (re-read config and restart)\n")
    (princ "  f     Reset failed state\n\n")
    (princ "POLICY (p) - service rows only\n")
    (princ "------------------------------\n")
    (princ "  e     Enable entry\n")
    (princ "  d     Disable entry\n")
    (princ "  m     Mask entry (always disabled)\n")
    (princ "  u     Unmask entry\n")
    (princ "  r     Set restart policy (selection)\n")
    (princ "  l     Set logging (selection)\n\n")
    (princ "INSPECT (i) - service rows only\n")
    (princ "-------------------------------\n")
    (princ "  i     Show entry details (C-u for status legend)\n")
    (princ "  d     Show dependencies for entry\n")
    (princ "  g     Show dependency graph\n")
    (princ "  b     Blame: startup timing sorted by duration\n")
    (princ "  l     View log file\n")
    (princ "  c     View unit file (read-only)\n")
    (princ "  e     Edit unit file (create scaffold if missing)\n")
    (princ "  m     Show target members (target rows only)\n\n")
    (princ "TIMERS (y) - timer rows only\n")
    (princ "----------------------------\n")
    (princ "  t     Trigger timer now (manual)\n")
    (princ "  i     Show timer details\n")
    (princ "  j     Jump to target service row\n")
    (princ "  r     Reset timer runtime state\n")
    (princ "  g     Refresh timer section\n\n")
    (princ "TIMER LIMITATIONS\n")
    (princ "-----------------\n")
    (princ "Timer rows are not unit files.  Lifecycle, policy, cat, and edit\n")
    (princ "commands are not available for timer rows.  Use `y' for timer\n")
    (princ "actions or `?' -> Timers in the transient menu.\n\n")
    (princ "STATUS VALUES\n")
    (princ "-------------\n")
    (princ "  running   Process is alive and running\n")
    (princ "  active    Oneshot with remain-after-exit (exited successfully)\n")
    (princ "  done      Oneshot completed successfully (exit 0)\n")
    (princ "  failed    Process crashed or oneshot exited non-zero\n")
    (princ "  dead      Process crash-looped (exceeded restart limit)\n")
    (princ "  pending   Not yet started (inside activation closure)\n")
    (princ "  unreachable Outside current activation closure\n")
    (princ "  stopped   Process terminated or active oneshot explicitly stopped\n")
    (princ "  masked    Entry is masked (always disabled)\n")
    (princ "  invalid   Config entry has errors\n")
    (princ "  reached   Target converged successfully (all members ok)\n")
    (princ "  degraded  Target converged with failures\n")
    (princ "  converging Target is still converging\n\n")
    (princ "SERVICE COLUMNS\n")
    (princ "---------------\n")
    (princ "  ID        Process identifier (from :id or command)\n")
    (princ "  Type      simple (daemon), oneshot (run-once), or target (group)\n")
    (princ "  Target    Convergence/unreachable for targets, - for services\n")
    (princ "  Enabled   Whether process will start (yes/no)\n")
    (princ "  Status    Current state (see above)\n")
    (princ "  Restart   Restart policy: no, on-success, on-failure, always (simple only)\n")
    (princ "  Log       Whether output is logged to file\n")
    (princ "  PID       Process ID or exit code (exit:N)\n")
    (princ "  Reason    Why process is in current state\n\n")
    (princ "TIMER COLUMNS\n")
    (princ "-------------\n")
    (princ "  ID        Timer identifier\n")
    (princ "  Target    Target oneshot service ID\n")
    (princ "  Enabled   Whether timer is active (yes/no)\n")
    (princ "  Last-Run  Time since last trigger\n")
    (princ "  Next-Run  Time until next trigger\n")
    (princ "  Exit      Last exit code (0 = success)\n")
    (princ "  Miss      Last miss reason (overlap, downtime, disabled)\n")))

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

(defun supervisor-dashboard-stop ()
  "Stop process at point with confirmation.
Gracefully stops the process and suppresses auto-restart.
Rejects separator rows, timer rows, and non-active oneshot entries.
Oneshot entries with `:remain-after-exit' in active state can be stopped.
Use `s' to start the process again later."
  (interactive)
  (let ((id (supervisor--require-service-row)))
    (let ((entry (supervisor--get-entry-for-id id)))
      (when (and entry (eq (supervisor-entry-type entry) 'target))
        (user-error "Cannot stop target unit '%s'" id))
      (when (and entry (eq (supervisor-entry-type entry) 'oneshot)
                 (not (gethash id supervisor--remain-active)))
        (user-error "Cannot stop oneshot entry '%s'" id)))
    (when (yes-or-no-p (format "Stop process '%s'? " id))
      (let ((result (supervisor--manual-stop id)))
        (pcase (plist-get result :status)
          ('stopped (supervisor--refresh-dashboard))
          ('skipped (message "Entry %s is %s" id (plist-get result :reason)))
          ('error (message "Cannot stop %s: %s"
                           id (plist-get result :reason))))))))

(defun supervisor-dashboard-restart ()
  "Restart process at point with confirmation.
Stop the process gracefully, then start it again.
If the entry is not running, this is equivalent to start."
  (interactive)
  (let ((id (supervisor--require-service-row)))
    (let ((entry (supervisor--get-entry-for-id id)))
      (when (and entry (eq (supervisor-entry-type entry) 'target))
        (user-error "Cannot restart target unit '%s'" id)))
    (when (yes-or-no-p (format "Restart process '%s'? " id))
      ;; Stop first (ignore result — entry may not be running)
      (supervisor--manual-stop id)
      ;; Then start unconditionally, matching CLI restart semantics
      (let ((start-result (supervisor--manual-start id)))
        (pcase (plist-get start-result :status)
          ('started (supervisor--refresh-dashboard))
          ('skipped
           (message "Cannot start %s: %s"
                    id (plist-get start-result :reason)))
          ('error
           (message "Failed to start %s: %s"
                    id (plist-get start-result :reason))))))))

(defun supervisor-dashboard-kill (&optional force)
  "Kill process at point with confirmation.
Sends kill signal and leaves restart policy unchanged.
With prefix argument FORCE, skip confirmation."
  (interactive "P")
  (let ((id (supervisor--require-service-row)))
    (when (or force
              (yes-or-no-p (format "Kill process '%s'? " id)))
      (let ((result (supervisor--manual-kill id)))
        (pcase (plist-get result :status)
          ('signaled (supervisor--refresh-dashboard))
          ('skipped (message "Entry %s is %s" id (plist-get result :reason))))))))

(defun supervisor-dashboard-kill-force ()
  "Kill process at point without confirmation.
Sends kill signal immediately and leaves restart policy unchanged."
  (interactive)
  (supervisor-dashboard-kill t))

(defun supervisor-dashboard-reset-failed ()
  "Reset failed state for entry at point.
Clears crash-loop tracking so the entry can be restarted."
  (interactive)
  (let ((id (supervisor--require-service-row)))
    (let ((result (supervisor--reset-failed id)))
      (pcase (plist-get result :status)
        ('reset
         (message "Reset failed state for %s" id)
         (supervisor--refresh-dashboard))
        ('skipped (message "%s is %s" id (plist-get result :reason)))))))

(defun supervisor-dashboard-start ()
  "Start process at point if stopped.
Disabled units can be started (session-only); only mask blocks."
  (interactive)
  (let ((id (supervisor--require-service-row)))
    (let ((result (supervisor--manual-start id)))
      (pcase (plist-get result :status)
        ('started (supervisor--refresh-dashboard))
        ('skipped (message "Entry %s is %s" id (plist-get result :reason)))
        ('error (message "Cannot start %s: %s" id (plist-get result :reason)))))))

;;; Explicit Policy Commands

(defun supervisor-dashboard-enable ()
  "Enable the entry at point.
Set an enabled override if the config default is disabled."
  (interactive)
  (let ((id (supervisor--require-service-row)))
    (let ((result (supervisor--policy-enable id)))
      (message "%s" (plist-get result :message))
      (when (eq (plist-get result :status) 'applied)
        (supervisor--save-overrides)
        (supervisor--refresh-dashboard)))))

(defun supervisor-dashboard-disable ()
  "Disable the entry at point.
Set a disabled override if the config default is enabled."
  (interactive)
  (let ((id (supervisor--require-service-row)))
    (let ((result (supervisor--policy-disable id)))
      (message "%s" (plist-get result :message))
      (when (eq (plist-get result :status) 'applied)
        (supervisor--save-overrides)
        (supervisor--refresh-dashboard)))))

(defun supervisor-dashboard-mask ()
  "Mask the entry at point.
Masked entries are always disabled regardless of enabled overrides."
  (interactive)
  (let ((id (supervisor--require-service-row)))
    (let ((result (supervisor--policy-mask id)))
      (message "%s" (plist-get result :message))
      (when (eq (plist-get result :status) 'applied)
        (supervisor--save-overrides)
        (supervisor--refresh-dashboard)))))

(defun supervisor-dashboard-unmask ()
  "Unmask the entry at point.
Remove the mask override so the enabled state takes effect again."
  (interactive)
  (let ((id (supervisor--require-service-row)))
    (let ((result (supervisor--policy-unmask id)))
      (message "%s" (plist-get result :message))
      (when (eq (plist-get result :status) 'applied)
        (supervisor--save-overrides)
        (supervisor--refresh-dashboard)))))

(defun supervisor-dashboard-set-restart-policy ()
  "Set restart policy for entry at point via selection.
Prompt with `completing-read' to choose the target policy explicitly."
  (interactive)
  (let ((id (supervisor--require-service-row)))
    (let* ((choices '("always" "on-success" "on-failure" "no"))
           (choice (completing-read "Restart policy: " choices nil t))
           (policy (intern choice))
           (result (supervisor--policy-set-restart id policy)))
      (message "%s" (plist-get result :message))
      (when (eq (plist-get result :status) 'applied)
        (supervisor--save-overrides)
        (supervisor--refresh-dashboard)))))

(defun supervisor-dashboard-set-logging ()
  "Set logging for entry at point via selection.
Prompt with `completing-read' to choose on or off explicitly."
  (interactive)
  (let ((id (supervisor--require-service-row)))
    (let* ((choice (completing-read "Logging: " '("on" "off") nil t))
           (enabled-p (string= choice "on"))
           (result (supervisor--policy-set-logging id enabled-p)))
      (message "%s" (plist-get result :message))
      (when (eq (plist-get result :status) 'applied)
        (supervisor--save-overrides)
        (supervisor--refresh-dashboard)))))

(defun supervisor-dashboard-view-log ()
  "Open the log file for process at point.
Decode all formats through the structured decoder and display in a
read-only buffer with formatted timestamps and stream labels.
The view is bounded to `supervisor-dashboard-log-view-record-limit'
records for interactive responsiveness."
  (interactive)
  (let ((id (supervisor--require-service-row)))
    (let ((log-file (supervisor--log-file id)))
      (if (not (file-exists-p log-file))
          (message "No log file for %s" id)
        (let* ((limit (if (and (integerp supervisor-dashboard-log-view-record-limit)
                              (> supervisor-dashboard-log-view-record-limit 0))
                         supervisor-dashboard-log-view-record-limit
                       1000))
               (decoded (supervisor--log-decode-file
                         log-file limit nil (* limit 512)))
               (records (plist-get decoded :records))
               (buf-name (format "*supervisor-log-%s*" id)))
          (with-current-buffer (get-buffer-create buf-name)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (if records
                  (insert (mapconcat
                           #'supervisor--log-format-record-human
                           records "\n")
                          "\n")
                (insert (format "No records in %s\n" log-file))))
            (goto-char (point-min))
            (special-mode))
          (pop-to-buffer buf-name))))))

(defun supervisor-dashboard-cat ()
  "View unit file for entry at point in read-only mode.
Opens the unit file with `view-mode' so `q' returns to the dashboard."
  (interactive)
  (let ((id (supervisor--require-service-row)))
    (let ((path (supervisor--unit-file-path id)))
      (cond
       ((not path)
        (user-error "No active authority roots configured"))
       ((file-exists-p path)
        (view-file path))
       ((supervisor--get-entry-for-id id)
        (user-error "No unit file on disk for %s; use edit to create an override" id))
       (t
        (user-error "Unit file not found: %s" path))))))

(defvar supervisor-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-q") #'supervisor-edit-finish)
    (define-key map (kbd "q") #'supervisor-edit-quit)
    map)
  "Keymap for `supervisor-edit-mode'.")

(define-minor-mode supervisor-edit-mode
  "Minor mode for editing supervisor unit files.
Provides `q' to return to dashboard (prompts to save if modified),
and \\[supervisor-edit-finish] to save and return unconditionally.
Validates the unit file on save."
  :lighter " SvEdit"
  :keymap supervisor-edit-mode-map
  (if supervisor-edit-mode
      (progn
        (add-hook 'after-save-hook
                  #'supervisor--validate-unit-file-buffer nil t)
        (add-hook 'kill-buffer-hook
                  #'supervisor--return-to-dashboard nil t))
    (remove-hook 'after-save-hook
                 #'supervisor--validate-unit-file-buffer t)
    (remove-hook 'kill-buffer-hook
                 #'supervisor--return-to-dashboard t)))

(defun supervisor-edit-finish ()
  "Save current unit file buffer and return to the dashboard.
If modified, save first.  Then kill the buffer and return."
  (interactive)
  (when (and (buffer-modified-p) (buffer-file-name))
    (save-buffer))
  (let ((buf (current-buffer)))
    (supervisor--return-to-dashboard)
    (kill-buffer buf)))

(defun supervisor-edit-quit ()
  "Save if needed and return to the dashboard.
If the buffer is modified, offer to save first.  Then kill the
buffer and switch to the `*supervisor*' dashboard."
  (interactive)
  (when (and (buffer-modified-p) (buffer-file-name))
    (if (y-or-n-p "Unit file modified; save before returning? ")
        (save-buffer)
      (set-buffer-modified-p nil)))
  (let ((buf (current-buffer)))
    (supervisor--return-to-dashboard)
    (kill-buffer buf)))

(defun supervisor-dashboard-edit ()
  "Edit unit file for entry at point.
If the unit file does not exist, prompt to create a scaffold template.
On save, validate the unit file and report errors.
Press `q' or \\[supervisor-edit-finish] to return to the dashboard."
  (interactive)
  (let ((id (supervisor--require-service-row)))
    (let* ((path (supervisor--unit-file-path id))
           (root (or (when (fboundp 'supervisor--authority-root-for-id)
                       (supervisor--authority-root-for-id id))
                     (when path (file-name-directory path))))
           (tier (when (fboundp 'supervisor--authority-tier-for-id)
                   (supervisor--authority-tier-for-id id))))
      (unless path
        (user-error "No active authority roots configured"))
      (let ((created (not (file-exists-p path))))
        (when created
          (unless (y-or-n-p (format "No unit file for %s.  Create override? " id))
            (user-error "Cancelled"))
          (make-directory (file-name-directory path) t)
          (write-region (supervisor--unit-file-scaffold id) nil path))
        ;; Open the file and activate edit mode
        (find-file path)
        (supervisor-edit-mode 1)
        (message "%s %s in %s (tier %s)"
                 (if created "Created" "Editing")
                 id (or root "unknown root")
                 (or tier "?"))))))

(defun supervisor--return-to-dashboard ()
  "Return to the *supervisor* dashboard buffer if it exists.
Intended as `kill-buffer-hook' for edited unit files."
  (when-let* ((buf (get-buffer "*supervisor*")))
    (when (buffer-live-p buf)
      (pop-to-buffer buf))))

(defvar proced-auto-update-flag)  ; from proced.el

(defun supervisor-dashboard-toggle-proced-auto-update ()
  "Toggle proced auto-update in the Proced buffer, or globally if no buffer."
  (interactive)
  (require 'proced)
  (if-let* ((proced-buf (get-buffer "*Proced*")))
      (with-current-buffer proced-buf
        (call-interactively #'proced-toggle-auto-update)
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

(defun supervisor-dashboard-reload-unit ()
  "Hot-reload the unit at point.
Re-reads config and restarts if running."
  (interactive)
  (let ((id (supervisor--require-service-row)))
    (let ((result (supervisor--reload-unit id)))
      (supervisor-dashboard-refresh)
      (message "Reload %s: %s" id (plist-get result :action)))))

(defun supervisor-dashboard-daemon-reload ()
  "Reload unit definitions from disk without affecting runtime.
Calls `supervisor-daemon-reload' and refreshes the dashboard."
  (interactive)
  (let ((result (supervisor-daemon-reload)))
    (supervisor-dashboard-refresh)
    (message "Daemon-reload: %d entries, %d invalid"
             (plist-get result :entries)
             (plist-get result :invalid))))

(defun supervisor-dashboard-show-deps ()
  "Show computed dependencies for entry at point.
Shows post-validation edges: after cycle fallback and dep filtering.
Run `supervisor-start' first to populate computed dependency data."
  (interactive)
  (let ((id (supervisor--require-service-row)))
    (if (gethash id supervisor--invalid)
        (message "Cannot show dependencies for invalid entry: %s" id)
      (let ((entry (supervisor--get-entry-for-id id)))
        (if entry
            (let* ((cycle-fallback (gethash id supervisor--cycle-fallback-ids))
                   ;; Use computed deps if available, else show raw (pre-start)
                   (computed (gethash id supervisor--computed-deps))
                   (effective-deps (if cycle-fallback
                                       nil  ; cycle fallback clears all deps
                                     (or computed (supervisor-entry-after entry))))
                   ;; Find entries that depend on this one (computed)
                   (dependents nil))
              ;; Scan computed-deps for entries that list this ID
              (maphash (lambda (e-id e-deps)
                         (when (and (not (string= e-id id))
                                    (not (gethash e-id supervisor--cycle-fallback-ids))
                                    (member id e-deps))
                           (push e-id dependents)))
                       supervisor--computed-deps)
              (message "%s%s: depends-on=%s blocks=%s"
                       id
                       (if cycle-fallback " (cycle fallback)" "")
                       (if effective-deps (mapconcat #'identity effective-deps ", ") "none")
                       (if dependents (mapconcat #'identity dependents ", ") "none")))
          (message "Entry not found: %s" id))))))

(defun supervisor-dashboard-blame ()
  "Show startup timing blame view sorted by duration."
  (interactive)
  (supervisor--require-service-row)
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
  (supervisor--require-service-row)
  (if (= 0 (hash-table-count supervisor--computed-deps))
      (message "No dependency data available (run supervisor-start first)")
    (with-help-window "*supervisor-deps*"
      (princ "Supervisor Dependency Graph\n")
      (princ (make-string 50 ?=))
      (princ "\n\n")
      ;; Flat listing of all entries
      (let ((all-ids nil))
        (let ((idx 0))
          (dolist (entry (supervisor--effective-programs))
            (let ((id (supervisor--extract-id entry idx)))
              (cl-incf idx)
              (unless (gethash id supervisor--invalid)
                (push id all-ids)))))
        (dolist (id (nreverse all-ids))
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
              (princ (format "    -> %s\n" (mapconcat #'identity dependents ", "))))))))))

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
                    (supervisor--dashboard-header-line snapshot))))
    (pop-to-buffer buf)))



;;; Timer Dashboard Commands

(defun supervisor-dashboard-timer-trigger ()
  "Trigger the timer at point manually.
Reject invalid timers and when timer mode gate is off."
  (interactive)
  (let ((raw-id (tabulated-list-get-id)))
    (cond
     ((null raw-id)
      (user-error "No entry at point"))
     ((not (supervisor--timer-row-p raw-id))
      (user-error "Not a timer row; use on timer rows only"))
     (t
      (let ((timer-id (supervisor--row-id raw-id)))
        (unless (supervisor-timer-subsystem-active-p)
          (user-error "Timer subsystem is disabled"))
        (when (gethash timer-id supervisor--invalid-timers)
          (user-error "Cannot trigger invalid timer '%s'" timer-id))
        (let ((timer (cl-find timer-id supervisor--timer-list
                              :key #'supervisor-timer-id :test #'equal)))
          (unless timer
            (user-error "Timer not found: %s" timer-id))
          (if (supervisor-timer--trigger timer 'manual)
              (progn
                (message "Triggered timer '%s'" timer-id)
                (supervisor--refresh-dashboard))
            (message "Timer '%s' skipped (check logs)" timer-id))))))))

(defun supervisor-dashboard-timer-info ()
  "Show detailed information about the timer at point."
  (interactive)
  (let ((raw-id (tabulated-list-get-id)))
    (cond
     ((null raw-id) (user-error "No entry at point"))
     ((not (supervisor--timer-row-p raw-id)) (user-error "Not a timer row"))
     (t
      (let* ((timer-id (supervisor--row-id raw-id))
             (timer (cl-find timer-id supervisor--timer-list
                             :key #'supervisor-timer-id :test #'equal))
             (state (gethash timer-id supervisor--timer-state)))
        (with-help-window "*supervisor-timer-info*"
          (if (null timer)
              ;; Invalid timer
              (let ((reason (gethash timer-id supervisor--invalid-timers)))
                (princ (format "Timer: %s (INVALID)\n" timer-id))
                (princ (make-string 40 ?-))
                (princ (format "\n\nReason: %s\n" (or reason "unknown"))))
            ;; Valid timer detail
            (princ (format "Timer: %s\n" timer-id))
            (princ (make-string (+ 8 (length timer-id)) ?-))
            (princ "\n\n")
            (princ (format "     Target: %s\n" (supervisor-timer-target timer)))
            (princ (format "    Enabled: %s\n"
                           (if (supervisor-timer-enabled timer) "yes" "no")))
            (princ (format " Persistent: %s\n"
                           (if (supervisor-timer-persistent timer) "yes" "no")))
            (when-let* ((cal (supervisor-timer-on-calendar timer)))
              (princ (format "   Calendar: %S\n" cal)))
            (when-let* ((startup (supervisor-timer-on-startup-sec timer)))
              (princ (format "    Startup: %ds\n" startup)))
            (when-let* ((active (supervisor-timer-on-unit-active-sec timer)))
              (princ (format " UnitActive: %ds\n" active)))
            ;; Runtime state
            (when state
              (princ "\n")
              (when-let* ((last-run (plist-get state :last-run-at)))
                (princ (format "   Last run: %s (%s)\n"
                               (format-time-string "%Y-%m-%d %H:%M:%S" last-run)
                               (supervisor--format-timer-relative-time last-run))))
              (when-let* ((next-run (plist-get state :next-run-at)))
                (princ (format "   Next run: %s (%s)\n"
                               (format-time-string "%Y-%m-%d %H:%M:%S" next-run)
                               (supervisor--format-timer-relative-time next-run))))
              (when-let* ((last-exit (plist-get state :last-exit)))
                (princ (format "  Last exit: %s\n" last-exit)))
              (when-let* ((retry (plist-get state :retry-attempt)))
                (when (> retry 0)
                  (princ (format "      Retry: %d/%d\n" retry
                                 (length supervisor-timer-retry-intervals)))))
              (when-let* ((retry-at (plist-get state :retry-next-at)))
                (princ (format "   Retry at: %s\n"
                               (supervisor--format-timer-relative-time retry-at))))
              (when-let* ((miss (plist-get state :last-miss-reason)))
                (princ (format "  Last miss: %s\n" (symbol-name miss))))
              (when-let* ((target-type (plist-get state :last-target-type)))
                (princ (format "Target type: %s\n"
                               (symbol-name target-type))))
              (when-let* ((result (plist-get state :last-result)))
                (princ (format "Last result: %s\n"
                               (symbol-name result))))
              (when (plist-member state :last-result-reason)
                (let ((reason (plist-get state :last-result-reason)))
                  (when reason
                    (princ (format "     Reason: %s\n"
                                   (symbol-name reason))))))))))))))

(defun supervisor-dashboard-timer-jump ()
  "Jump to the target service row for the timer at point."
  (interactive)
  (let ((raw-id (tabulated-list-get-id)))
    (cond
     ((null raw-id) (user-error "No entry at point"))
     ((not (supervisor--timer-row-p raw-id)) (user-error "Not a timer row"))
     (t
      (let* ((timer-id (supervisor--row-id raw-id))
             (timer (cl-find timer-id supervisor--timer-list
                             :key #'supervisor-timer-id :test #'equal)))
        (if (null timer)
            (user-error "Timer not found: %s" timer-id)
          (let ((target-id (supervisor-timer-target timer))
                (found nil))
            (save-excursion
              (goto-char (point-min))
              (while (and (not found) (not (eobp)))
                (when-let* ((row-id (tabulated-list-get-id)))
                  (when (and (supervisor--service-row-p row-id)
                             (string= (supervisor--row-id row-id) target-id))
                    (setq found (point))))
                (forward-line 1)))
            (if found
                (progn
                  (goto-char found)
                  (message "Target: %s" target-id))
              (message "Target service '%s' not visible in current view"
                       target-id)))))))))

(defun supervisor-dashboard-timer-reset ()
  "Reset runtime state for the timer at point.
Clear runtime fields and recompute next run time."
  (interactive)
  (let ((raw-id (tabulated-list-get-id)))
    (cond
     ((null raw-id) (user-error "No entry at point"))
     ((not (supervisor--timer-row-p raw-id)) (user-error "Not a timer row"))
     (t
      (let ((timer-id (supervisor--row-id raw-id)))
        (when (yes-or-no-p (format "Reset runtime state for timer '%s'? " timer-id))
          (let ((state (or (gethash timer-id supervisor--timer-state) nil)))
            (dolist (key '(:last-run-at :last-success-at :last-failure-at :last-exit
                           :retry-attempt :retry-next-at :last-missed-at
                           :last-miss-reason :next-run-at :startup-triggered))
              (setq state (plist-put state key nil)))
            (puthash timer-id state supervisor--timer-state)
            ;; Recompute next-run
            (supervisor-timer--update-next-run timer-id)
            ;; Persist
            (when (supervisor-timer-subsystem-active-p)
              (supervisor-timer--save-state))
            (message "Reset timer state for '%s'" timer-id)
            (supervisor--refresh-dashboard))))))))

(defun supervisor-dashboard-timer-refresh ()
  "Refresh the timer section of the dashboard."
  (interactive)
  (supervisor--refresh-dashboard)
  (message "Timer section refreshed"))

(provide 'supervisor-dashboard)

;;; supervisor-dashboard.el ends here
