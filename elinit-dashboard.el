;;; elinit-dashboard.el --- Elinit dashboard UI -*- lexical-binding: t -*-

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

;; Dashboard UI for elinit.el.
;; Run M-x elinit-handbook for full documentation.

;;; Code:

(require 'cl-lib)
(require 'elinit-core)

;; Forward declaration for transient menu (defined at runtime via eval)
(declare-function elinit-dashboard-menu "elinit-dashboard" ())

;; Forward declaration for proced function

;; Forward declarations for timer subsystem (defined in elinit-timer.el)
(declare-function elinit-timer-subsystem-active-p "elinit-timer" ())
(declare-function elinit-timer-id "elinit-timer" (timer))
(declare-function elinit-timer-target "elinit-timer" (timer))
(declare-function elinit-timer-enabled "elinit-timer" (timer))
(declare-function elinit-timer-persistent "elinit-timer" (timer))
(declare-function elinit-timer-on-calendar "elinit-timer" (timer))
(declare-function elinit-timer-on-startup-sec "elinit-timer" (timer))
(declare-function elinit-timer-on-unit-active-sec "elinit-timer" (timer))
(declare-function elinit-timer--target-active-p "elinit-timer" (timer))
(declare-function elinit-timer--trigger "elinit-timer" (timer reason))
(declare-function elinit-timer--update-next-run "elinit-timer" (timer-id))
(declare-function elinit-timer--save-state "elinit-timer" ())

;; Forward declarations for unit-file module (optional)
(declare-function elinit--unit-file-path "elinit-units" (id))
(declare-function elinit--unit-file-existing-path "elinit-units" (id))
(declare-function elinit--unit-file-scaffold "elinit-units" (id))
(declare-function elinit--validate-unit-file-buffer "elinit-units" ())
(declare-function elinit--authority-root-for-id "elinit-units" (id))
(declare-function elinit--authority-tier-for-id "elinit-units" (id))

;; Forward declarations for logging module (defined in elinit-log.el)
(declare-function elinit--log-file "elinit-log" (prog))
(declare-function elinit--log-format-record-human "elinit-log" (record))
(declare-function elinit--log-decode-file "elinit-log"
                  (file &optional limit offset max-bytes))
(declare-function elinit--log-filter-records "elinit-log"
                  (records &optional since until priority))
(declare-function elinit--log-record-to-json "elinit-log" (record))
(declare-function elinit--log-record-priority "elinit-log" (record))

;; Forward declarations for timer state variables (defined in elinit-timer.el)
(defvar elinit--timer-state)
(defvar elinit--timer-list)
(defvar elinit--invalid-timers)
(defvar elinit-timer-retry-intervals)
;; Forward declaration for tabulated-list internals
(defvar tabulated-list-header-string nil)

;;; Dashboard Faces

(defface elinit-status-running
  '((t :foreground "#00ff00" :weight bold))
  "Face for running status in dashboard."
  :group 'elinit)

(defface elinit-status-done
  '((t :foreground "#00bfff"))
  "Face for done/completed oneshot status in dashboard."
  :group 'elinit)

(defface elinit-status-failed
  '((t :foreground "#ff4444" :weight bold))
  "Face for failed status in dashboard."
  :group 'elinit)

(defface elinit-status-dead
  '((t :foreground "#ff8c00" :weight bold))
  "Face for dead/crash-loop status in dashboard."
  :group 'elinit)

(defface elinit-status-invalid
  '((t :foreground "#ff00ff" :weight bold))
  "Face for invalid config entry in dashboard."
  :group 'elinit)

(defface elinit-status-pending
  '((t :foreground "#ffd700"))
  "Face for pending/not-yet-started status in dashboard."
  :group 'elinit)

(defface elinit-status-stopped
  '((t :foreground "#888888"))
  "Face for stopped status in dashboard."
  :group 'elinit)

(defface elinit-status-unreachable
  '((t :foreground "#666666"))
  "Face for unreachable (not in activation closure) status in dashboard."
  :group 'elinit)


(defface elinit-type-simple
  '((t :foreground "#87ceeb"))
  "Face for simple process type in dashboard."
  :group 'elinit)

(defface elinit-type-oneshot
  '((t :foreground "#dda0dd"))
  "Face for oneshot process type in dashboard."
  :group 'elinit)

(defface elinit-type-timer
  '((t :foreground "#98d8c8"))
  "Face for timer type in dashboard."
  :group 'elinit)

(defface elinit-type-target
  '((t :foreground "#ffd700"))
  "Face for target type in dashboard."
  :group 'elinit)

(defface elinit-enabled-yes
  '((t :foreground "#00ff00"))
  "Face for enabled=yes in dashboard."
  :group 'elinit)

(defface elinit-enabled-no
  '((t :foreground "#ff6b6b"))
  "Face for enabled=no in dashboard."
  :group 'elinit)

(defface elinit-reason
  '((t :foreground "#ffa500" :slant italic))
  "Face for reason column in dashboard."
  :group 'elinit)

(defface elinit-section-separator
  '((t :foreground "#5f87af" :weight bold :underline t))
  "Face for section separator rows in dashboard."
  :group 'elinit)

(defcustom elinit-dashboard-show-header-hints nil
  "Reserved compatibility option for dashboard header hints.
This option is currently inert; the header line shows only service
counters.  Press `h' in the dashboard for full keybinding help."
  :type 'boolean
  :group 'elinit)

(defcustom elinit-dashboard-show-timers t
  "When non-nil, show timers section in dashboard after services."
  :type 'boolean
  :group 'elinit)

(defcustom elinit-dashboard-log-view-record-limit 1000
  "Maximum records decoded for `elinit-dashboard-view-log'.
Bounds both the number of records returned and the bytes read from
disk (heuristic: LIMIT * 512 bytes tail read).  Must be a positive
integer; the dashboard decode path is always bounded."
  :type 'integer
  :group 'elinit)

;;; Dashboard

(defvar-local elinit--dashboard-tag-filter nil
  "Current tag filter for dashboard.
nil means show all entries, otherwise a tag symbol or string.")

(defvar-local elinit--dashboard-target-filter nil
  "Current target filter for dashboard.
nil means show all entries, otherwise a target ID string.")

(defvar-local elinit--dashboard-show-targets nil
  "Non-nil means show target-type entries in the dashboard.
Default is nil, hiding targets for a service-first view.")

(defvar-local elinit--dashboard-show-init-targets nil
  "Non-nil means include init-transition targets when targets are shown.
Init-transition targets are rescue, shutdown, poweroff, reboot,
runlevel0, runlevel1, and runlevel6 targets.")

(defconst elinit--init-transition-target-ids
  '("rescue.target" "shutdown.target" "poweroff.target" "reboot.target"
    "runlevel0.target" "runlevel1.target" "runlevel6.target")
  "Target IDs that represent init-transition targets.
These are hidden by default even when regular targets are shown.")

(defvar elinit-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    ;; Navigation / filter
    (define-key map "f" #'elinit-dashboard-cycle-filter)
    (define-key map "F" #'elinit-dashboard-cycle-tag-filter)
    ;; Visibility
    (define-key map "v" #'elinit-dashboard-toggle-targets)
    (define-key map "V" #'elinit-dashboard-toggle-init-targets)
    ;; Refresh
    (define-key map "g" #'elinit-dashboard-refresh)
    (define-key map "G" #'elinit-dashboard-toggle-auto-refresh)
    ;; System (proced)
    (define-key map "P" #'proced)
    ;; Nested action groups
    (define-key map "l" #'elinit-dashboard-lifecycle)
    (define-key map "p" #'elinit-dashboard-policy)
    (define-key map "i" #'elinit-dashboard-inspect)
    (define-key map "t" #'elinit-dashboard-timer-actions)
    ;; Help / quit
    (define-key map "h" #'elinit-dashboard-help)
    (define-key map "?" #'elinit-dashboard-menu-open)
    (define-key map "q" #'elinit-dashboard-quit)
    map)
  "Keymap for `elinit-dashboard-mode'.")

;;; Submenu Dispatchers

(defun elinit-dashboard-lifecycle ()
  "Open lifecycle action submenu.
Lifecycle actions control the runtime state of individual entries."
  (interactive)
  (pcase (read-char
          "Lifecycle: [s]tart [t]stop [r]estart [k]ill [u]reload [f]reset-failed ")
    (?s (call-interactively #'elinit-dashboard-start))
    (?t (call-interactively #'elinit-dashboard-stop))
    (?r (call-interactively #'elinit-dashboard-restart))
    (?k (call-interactively #'elinit-dashboard-kill))
    (?u (call-interactively #'elinit-dashboard-reload-unit))
    (?f (call-interactively #'elinit-dashboard-reset-failed))
    (_ (message "Lifecycle: cancelled"))))

(defun elinit-dashboard-policy ()
  "Open policy submenu.
Policy actions modify persistent configuration overrides."
  (interactive)
  (pcase (read-char
          "Policy: [e]nable [d]isable [m]ask [u]nmask [r]estart-policy [l]ogging ")
    (?e (call-interactively #'elinit-dashboard-enable))
    (?d (call-interactively #'elinit-dashboard-disable))
    (?m (call-interactively #'elinit-dashboard-mask))
    (?u (call-interactively #'elinit-dashboard-unmask))
    (?r (call-interactively #'elinit-dashboard-set-restart-policy))
    (?l (call-interactively #'elinit-dashboard-set-logging))
    (_ (message "Policy: cancelled"))))

(defun elinit-dashboard-inspect ()
  "Open inspect submenu.
Inspect actions are read-only presentation workflows."
  (interactive)
  (pcase (read-char
          "Inspect: [i]nfo [d]eps [g]raph [b]lame [l]og [c]at [e]dit [m]embers ")
    (?i (call-interactively #'elinit-dashboard-describe-entry))
    (?d (call-interactively #'elinit-dashboard-show-deps))
    (?g (call-interactively #'elinit-dashboard-show-graph))
    (?b (call-interactively #'elinit-dashboard-blame))
    (?l (call-interactively #'elinit-dashboard-view-log))
    (?c (call-interactively #'elinit-dashboard-cat))
    (?e (call-interactively #'elinit-dashboard-edit))
    (?m (call-interactively #'elinit-dashboard-target-members))
    (_ (message "Inspect: cancelled"))))

(defun elinit-dashboard-timer-actions ()
  "Open timer actions submenu.
Timer actions operate on timer rows in the dashboard."
  (interactive)
  (pcase (read-char
          "Timer: [t]rigger [i]nfo [j]ump [r]eset [g]refresh ")
    (?t (call-interactively #'elinit-dashboard-timer-trigger))
    (?i (call-interactively #'elinit-dashboard-timer-info))
    (?j (call-interactively #'elinit-dashboard-timer-jump))
    (?r (call-interactively #'elinit-dashboard-timer-reset))
    (?g (call-interactively #'elinit-dashboard-timer-refresh))
    (_ (message "Timer: cancelled"))))

;;; Transient Menu

(defvar elinit--dashboard-menu-defined nil
  "Non-nil if the transient menu has been defined.")

(defun elinit--define-dashboard-menu ()
  "Define the transient menu for the dashboard.
This is called on first use to avoid loading transient at package load time."
  (unless elinit--dashboard-menu-defined
    (require 'transient)
    (eval
     '(transient-define-prefix elinit-dashboard-menu ()
        "Elinit dashboard actions."
        [:description
         (lambda ()
           (elinit--health-summary (elinit--build-snapshot)))
         ["Service Actions"
          ("l" "Lifecycle..." elinit-dashboard-lifecycle)
          ("p" "Policy..." elinit-dashboard-policy)
          ("i" "Inspect..." elinit-dashboard-inspect)]
         ["Timers"
          ("t" "Timers..." elinit-dashboard-timer-actions)]
         ["Navigation"
          ("f" "Target filter" elinit-dashboard-cycle-filter)
          ("F" "Tag filter" elinit-dashboard-cycle-tag-filter)
          ("v" "Show targets" elinit-dashboard-toggle-targets)
          ("V" "Show init targets" elinit-dashboard-toggle-init-targets)
          ("g" "Refresh" elinit-dashboard-refresh)
          ("G" "Auto-refresh" elinit-dashboard-toggle-auto-refresh)]
         ["System"
          ("P" "Proced" proced)
          ("X" "Daemon-reload" elinit-dashboard-daemon-reload)]
         ["Help"
          ("h" "Full help" elinit-dashboard-help)
          ("q" "Quit" elinit-dashboard-quit)]]))
    (setq elinit--dashboard-menu-defined t)))

(defun elinit-dashboard-menu-open ()
  "Open the elinit dashboard transient menu.
Requires the `transient' package to be installed."
  (interactive)
  (unless (require 'transient nil t)
    (user-error "The `transient' package is required for this feature"))
  (elinit--define-dashboard-menu)
  (call-interactively #'elinit-dashboard-menu))

(defvar-local elinit--dashboard-last-id nil
  "Last echoed entry ID, to avoid repeated messages.")

(defun elinit--dashboard-echo-id ()
  "Echo full entry ID in minibuffer if point moved to new row."
  (when-let* ((raw-id (tabulated-list-get-id)))
    (unless (equal raw-id elinit--dashboard-last-id)
      (setq elinit--dashboard-last-id raw-id)
      ;; Only echo typed row IDs longer than column width (15)
      (when-let* ((id (elinit--row-id raw-id)))
        (when (> (length id) 15)
          (message "%s" id))))))

(define-derived-mode elinit-dashboard-mode tabulated-list-mode "Elinit"
  "Major mode for the elinit dashboard."
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
  (add-hook 'post-command-hook #'elinit--dashboard-echo-id nil t)
  ;; Clean up auto-refresh timer when buffer is killed
  (add-hook 'kill-buffer-hook #'elinit--cleanup-auto-refresh nil t))

(defun elinit--status-face (status)
  "Return the face for STATUS string."
  (pcase status
    ("running" 'elinit-status-running)
    ("active" 'elinit-status-running)
    ("done" 'elinit-status-done)
    ("failed" 'elinit-status-failed)
    ("dead" 'elinit-status-dead)
    ("invalid" 'elinit-status-invalid)
    ("pending" 'elinit-status-pending)
    ("stopped" 'elinit-status-stopped)
    ("masked" 'elinit-status-dead)
    ("reached" 'elinit-status-running)
    ("degraded" 'elinit-status-failed)
    ("converging" 'elinit-status-pending)
    ("disabled" 'elinit-status-stopped)
    ("unreachable" 'elinit-status-unreachable)
    (_ nil)))

(defun elinit--propertize-status (status)
  "Return STATUS string with appropriate face applied."
  (let ((face (elinit--status-face status)))
    (if face
        (propertize status 'face face)
      status)))

(defun elinit--make-blank-row (n)
  "Create a blank row for visual spacing.
N makes the symbol ID unique."
  (list (intern (format "--blank-%d--" n))
        (vector "" "" "" "" "" "" "" "" "")))

(defun elinit--make-health-summary-row (&optional snapshot programs)
  "Return a non-interactive row that summarizes service health.
Counts are service-scoped and do not include timer rows.
SNAPSHOT and PROGRAMS are forwarded to `elinit--health-counts'."
  (let* ((counts (elinit--health-counts snapshot programs))
         (running (plist-get counts :running))
         (active (plist-get counts :active))
         (done (plist-get counts :done))
         (pending (plist-get counts :pending))
         (failed (plist-get counts :failed))
         (invalid (plist-get counts :invalid))
         (active-text (if (> active 0)
                          (concat (propertize (format "%d" active)
                                              'face 'elinit-status-running)
                                  " active")
                        "-")))
    (list '--health--
          (vector (propertize "services" 'face 'elinit-section-separator)
                  (concat (propertize (format "%d" running)
                                      'face 'elinit-status-running)
                          " run")
                  (concat (propertize (format "%d" done)
                                      'face 'elinit-status-done)
                          " done")
                  (concat (propertize (format "%d" pending)
                                      'face 'elinit-status-pending)
                          " pend")
                  (concat (propertize (format "%d" failed)
                                      'face 'elinit-status-failed)
                          " fail")
                  (concat (propertize (format "%d" invalid)
                                      'face 'elinit-status-invalid)
                          " inv")
                  "-"
                  "-"
                  active-text))))

(defun elinit--separator-row-p (id)
  "Return non-nil if ID represents a separator or summary row."
  (and id (symbolp id)))

(defun elinit--timer-row-p (id)
  "Return non-nil if ID represents a timer row."
  (and (consp id) (eq (car id) :timer)))

(defun elinit--service-row-p (id)
  "Return non-nil if ID represents a service row."
  (and (consp id) (eq (car id) :service)))

(defun elinit--target-type-p (type)
  "Return non-nil if TYPE is the target entry type."
  (eq type 'target))

(defun elinit--init-transition-target-p (id)
  "Return non-nil if ID is an init-transition target.
Init-transition targets are rescue, shutdown, poweroff, reboot,
runlevel0, runlevel1, and runlevel6."
  (member id elinit--init-transition-target-ids))

(defun elinit--row-kind (id)
  "Return the kind of dashboard row for ID.
Return `:service', `:timer', or `:separator'."
  (cond
   ((elinit--service-row-p id) :service)
   ((elinit--timer-row-p id) :timer)
   ((elinit--separator-row-p id) :separator)
   (t nil)))

(defun elinit--row-id (id)
  "Extract the string ID from a typed row ID.
For cons cell IDs like (:service . \"foo\"), return \"foo\".
For symbol IDs (separators), return nil."
  (when (consp id)
    (cdr id)))

(defconst elinit--timer-row-rejection
  "Not available for timer rows: use timer actions (t or ? -> Timers)"
  "Stable error message for service-only commands on timer rows.")

(defun elinit--require-service-row ()
  "Return the string ID of the service row at point.
Signal `user-error' if point is on a separator, timer, or empty row."
  (let ((raw-id (tabulated-list-get-id)))
    (cond
     ((null raw-id)
      (user-error "No entry at point"))
     ((elinit--separator-row-p raw-id)
      (user-error "Not available on separator row"))
     ((elinit--timer-row-p raw-id)
      (user-error "%s" elinit--timer-row-rejection))
     (t
      (or (elinit--row-id raw-id)
          (user-error "No entry at point"))))))

(defun elinit--timer-projection (timer)
  "Return projected TIMER fields as a plist for display.
Provides rendering parity with `list-timers' CLI output.
Fields: :id, :target, :enabled, :last-run, :next-run, :last-exit,
:result-reason, :target-type, :last-result.
The :target-type is resolved from current config, not runtime state,
so it is always current even for never-triggered timers."
  (let* ((id (elinit-timer-id timer))
         (target-id (elinit-timer-target timer))
         (state (gethash id elinit--timer-state))
         (target-entry (elinit--get-entry-for-id target-id))
         (target-type (when target-entry
                        (elinit-entry-type target-entry))))
    (list :id id
          :target target-id
          :enabled (elinit-timer-enabled timer)
          :last-run (plist-get state :last-run-at)
          :next-run (plist-get state :next-run-at)
          :last-exit (plist-get state :last-exit)
          :result-reason (plist-get state :last-result-reason)
          :target-type target-type
          :last-result (plist-get state :last-result))))

(defun elinit--make-dashboard-entry (id type parent-target enabled-p
                                            restart-policy logging-p
                                            &optional snapshot)
  "Create a dashboard entry vector for ID.
TYPE, PARENT-TARGET, ENABLED-P, RESTART-POLICY, LOGGING-P are entry fields.
PARENT-TARGET is the first target this entry belongs to (via wanted-by or
required-by), shown in the TARGET column for non-target entries.
If SNAPSHOT is provided, read runtime state from it."
  (let* ((status-pid (elinit--compute-entry-status id type snapshot))
         (status (car status-pid))
         (pid-raw (cdr status-pid))
         (pid (if (and (eq type 'oneshot)
                       (string-prefix-p "exit:" pid-raw))
                  "-"
                pid-raw))
         (reason (elinit--compute-entry-reason id type snapshot))
         ;; For overrides, use snapshot if provided, otherwise globals
         (mask-hash (if snapshot
                       (or (elinit-snapshot-mask-override snapshot)
                           (make-hash-table :test 'equal))
                     elinit--mask-override))
         (mask-val (gethash id mask-hash))
         (is-masked (eq mask-val 'masked))
         (enabled-override (if snapshot
                               (gethash id (elinit-snapshot-enabled-override snapshot))
                             (gethash id elinit--enabled-override)))
         (effective-enabled (cond (is-masked nil)
                                  ((eq enabled-override 'enabled) t)
                                  ((eq enabled-override 'disabled) nil)
                                  (t enabled-p)))
         (restart-override (if snapshot
                               (gethash id (elinit-snapshot-restart-override snapshot))
                             (gethash id elinit--restart-override)))
         (effective-restart (cond ((eq restart-override 'enabled) 'always)
                                  ((eq restart-override 'disabled) 'no)
                                  ((memq restart-override
                                         elinit--valid-restart-policies)
                                   restart-override)
                                  (t (elinit--normalize-restart-policy
                                      restart-policy))))
         (restart-str (if (memq type '(oneshot target))
                          "n/a"
                        (symbol-name effective-restart)))
         (log-override (if snapshot
                           (gethash id (elinit-snapshot-logging-override snapshot))
                         (gethash id elinit--logging)))
         (effective-logging (cond ((eq log-override 'enabled) t)
                                  ((eq log-override 'disabled) nil)
                                  (t logging-p))))
    (vector id
            (propertize (symbol-name type)
                        'face (pcase type
                                ('oneshot 'elinit-type-oneshot)
                                ('target 'elinit-type-target)
                                (_ 'elinit-type-simple)))
            (if (eq type 'target)
                (let* ((effective-id
                        (cond ((equal id "default.target")
                               (elinit--resolve-default-target-link))
                              ((elinit--target-alias-p id)
                               (elinit--resolve-target-alias id))
                              (t id)))
                       (conv (when (hash-table-p elinit--target-convergence)
                               (gethash effective-id
                                        elinit--target-convergence)))
                       (closure (and (elinit-plan-p elinit--current-plan)
                                     (elinit-plan-activation-closure
                                      elinit--current-plan)))
                       (conv-str (cond
                                  ((and (hash-table-p closure)
                                        (not (gethash effective-id closure)))
                                   "unreachable")
                                  ((eq conv 'reached) "reached")
                                  ((eq conv 'degraded) "degraded")
                                  ((eq conv 'converging) "pending")
                                  (t "pending"))))
                  (elinit--propertize-status conv-str))
              (or parent-target "-"))
            (propertize (if effective-enabled "yes" "no")
                        'face (if effective-enabled
                                  'elinit-enabled-yes
                                'elinit-enabled-no))
            (elinit--propertize-status status)
            restart-str
            (if (eq type 'target) "-" (if effective-logging "yes" "no"))
            pid
            (if (or (null reason) (string-empty-p reason))
                ""
              (propertize reason 'face 'elinit-reason)))))

(defun elinit--format-timer-relative-time (timestamp)
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

(defun elinit--make-timer-separator ()
  "Create a header row for the Timers section with column labels."
  (list '--timers--
        (vector (propertize "── Timers" 'face 'elinit-section-separator)
                (propertize "TARGET" 'face 'elinit-section-separator)
                (propertize "ENABLED" 'face 'elinit-section-separator)
                (propertize "LAST-RUN" 'face 'elinit-section-separator)
                (propertize "NEXT-RUN" 'face 'elinit-section-separator)
                (propertize "EXIT" 'face 'elinit-section-separator)
                (propertize "REASON" 'face 'elinit-section-separator)
                (propertize "TYPE" 'face 'elinit-section-separator)
                (propertize "RESULT" 'face 'elinit-section-separator))))

(defun elinit--make-services-separator ()
  "Create a header row for the Services section with column labels."
  (list '--services--
        (vector (propertize "── Services" 'face 'elinit-section-separator)
                (propertize "TYPE" 'face 'elinit-section-separator)
                (propertize "TARGET" 'face 'elinit-section-separator)
                (propertize "ENABLED" 'face 'elinit-section-separator)
                (propertize "STATUS" 'face 'elinit-section-separator)
                (propertize "RESTART" 'face 'elinit-section-separator)
                (propertize "LOG" 'face 'elinit-section-separator)
                (propertize "PID" 'face 'elinit-section-separator)
                (propertize "REASON" 'face 'elinit-section-separator))))

(defun elinit--make-timer-separator-disabled ()
  "Create a timer section header showing disabled state."
  (list '--timers--
        (vector (propertize "── Timers (disabled)" 'face 'elinit-section-separator)
                "" "" "" "" "" "" "" "")))

(defun elinit--make-timer-separator-empty ()
  "Create a timer section header showing no timers configured."
  (list '--timers--
        (vector (propertize "── Timers" 'face 'elinit-section-separator)
                (propertize "no timers configured" 'face 'elinit-status-stopped)
                "" "" "" "" "" "" "")))

(defun elinit--make-timer-dashboard-entry (timer)
  "Create a dashboard entry vector for TIMER.
Columns are mapped to match `list-timers' semantics:
ID, TARGET, ENABLED, LAST-RUN, NEXT-RUN, EXIT, REASON, TYPE, RESULT."
  (let* ((proj (elinit--timer-projection timer))
         (id (plist-get proj :id))
         (target (plist-get proj :target))
         (enabled (plist-get proj :enabled))
         (last-run (plist-get proj :last-run))
         (next-run (plist-get proj :next-run))
         (last-exit (plist-get proj :last-exit))
         (result-reason (plist-get proj :result-reason))
         (target-type (plist-get proj :target-type))
         (last-result (plist-get proj :last-result)))
    (vector (propertize id 'face 'elinit-type-timer)
            (propertize (or target "-") 'face 'elinit-type-oneshot)
            (propertize (if enabled "yes" "no")
                        'face (if enabled
                                  'elinit-enabled-yes
                                'elinit-enabled-no))
            (elinit--format-timer-relative-time last-run)
            (elinit--format-timer-relative-time next-run)
            (if (null last-exit) "-" (number-to-string last-exit))
            (if result-reason (symbol-name result-reason) "-")
            (if target-type (symbol-name target-type) "-")
            (if last-result
                (elinit--propertize-status
                 (symbol-name last-result))
              "-"))))

(defun elinit--get-timer-entries ()
  "Generate timer entries for the dashboard.
Returns list of (typed-id vector) pairs with (:timer . ID) keys."
  (let ((entries nil))
    (dolist (timer elinit--timer-list)
      (let ((id (elinit-timer-id timer)))
        (push (list (cons :timer id)
                    (elinit--make-timer-dashboard-entry timer))
              entries)))
    ;; Also show invalid timers with matching column layout.
    ;; Collect and sort by ID for deterministic ordering.
    (let ((invalid-ids nil))
      (maphash (lambda (id _reason) (push id invalid-ids))
               elinit--invalid-timers)
      (dolist (id (sort invalid-ids #'string<))
        (let ((reason (gethash id elinit--invalid-timers)))
          (push (list (cons :timer id)
                      (vector (propertize id 'face 'elinit-status-invalid)
                              "-"
                              "-"
                              "-"
                              (elinit--propertize-status "invalid")
                              "-"
                              (propertize reason 'face 'elinit-reason)
                              "" ""))
                entries))))
    (nreverse entries)))

(defun elinit--get-entries (&optional snapshot programs)
  "Generate entries for the dashboard (deduplicates on the fly).
Respects tag filter when set.
If SNAPSHOT is provided, read runtime state from it.
If PROGRAMS is provided, use it instead of calling
`elinit--effective-programs'.

Layout order: services section header, service rows, then timer section
if `elinit-dashboard-show-timers' is non-nil."
  (let* ((snapshot (or snapshot (elinit--build-snapshot)))
         (programs (or programs (elinit--effective-programs)))
         (entries nil)
         (seen (make-hash-table :test 'equal))
         (tag-filter elinit--dashboard-tag-filter)
         (target-filter elinit--dashboard-target-filter)
         (show-targets elinit--dashboard-show-targets)
         (show-init-targets elinit--dashboard-show-init-targets)
         (target-member-hash
          (when (and elinit--current-plan target-filter)
            (elinit-plan-target-members elinit--current-plan)))
         (invalid-hash (elinit-snapshot-invalid snapshot))
         (idx 0))
    (dolist (entry programs)
      (let* ((raw-id (elinit--extract-id entry idx))
             (invalid-reason (gethash raw-id invalid-hash)))
        (cl-incf idx)
        (unless (gethash raw-id seen)
          (puthash raw-id t seen)
          (if invalid-reason
              (push (list (cons :service raw-id)
                          (vector raw-id "-" "-" "-"
                                  (elinit--propertize-status "invalid")
                                  "-" "-" "-"
                                  invalid-reason))
                    entries)
            (let* ((parsed (elinit--parse-entry entry))
                   (id (elinit-entry-id parsed))
                   (enabled-p (elinit-entry-enabled-p parsed))
                   (restart-policy (elinit-entry-restart-policy parsed))
                   (logging-p (elinit-entry-logging-p parsed))
                   (type (elinit-entry-type parsed))
                   (tags (elinit-entry-tags parsed))
                   (parent-target
                    (or (car (elinit-entry-required-by parsed))
                        (car (elinit-entry-wanted-by parsed)))))
              (when (and (or (null tag-filter) (member tag-filter tags))
                        (or (null target-filter)
                            (string= id target-filter)
                            (member target-filter
                                    (elinit-entry-wanted-by parsed))
                            (member target-filter
                                    (elinit-entry-required-by parsed))
                            (when (hash-table-p target-member-hash)
                              (let ((members (gethash target-filter
                                                      target-member-hash)))
                                (or (member id (plist-get members :requires))
                                    (member id (plist-get members :wants))))))
                        ;; Target visibility: hide targets by default
                        ;; unless toggled or a target-filter is active.
                        (or (not (elinit--target-type-p type))
                            target-filter
                            (and show-targets
                                 (or show-init-targets
                                     (not (elinit--init-transition-target-p
                                           id))))))
                (push (list (cons :service id)
                            (elinit--make-dashboard-entry
                             id type parent-target enabled-p restart-policy
                             logging-p snapshot))
                      entries)))))))
    ;; Include invalid authority units not already seen via programs
    (when (boundp 'elinit--unit-file-invalid)
      (maphash (lambda (id reason)
                 (unless (gethash id seen)
                   (puthash id t seen)
                   (push (list (cons :service id)
                               (vector id "-" "-" "-"
                                       (elinit--propertize-status "invalid")
                                       "-" "-" "-"
                                       reason))
                         entries)))
               (symbol-value 'elinit--unit-file-invalid)))
    (setq entries (nreverse entries))
    (let ((final-entries (mapcar (lambda (e) (list (car e) (cadr e))) entries)))
      ;; Services section header is always first row in buffer body.
      (setq final-entries
            (cons (elinit--make-services-separator) final-entries))
      ;; Timer section (tag filter does not hide timers)
      (when elinit-dashboard-show-timers
        (setq final-entries
              (append final-entries
                      (cond
                       ;; Timer mode gate off
                       ((not elinit-timer-subsystem-mode)
                        (list (elinit--make-timer-separator-disabled)))
                       ;; No timers configured
                       ((and (null elinit--timer-list)
                             (= 0 (hash-table-count elinit--invalid-timers)))
                        (list (elinit--make-timer-separator-empty)))
                       ;; Timers present
                       (t
                        (cons (elinit--make-timer-separator)
                              (elinit--get-timer-entries)))))))
      final-entries)))

(defun elinit--health-counts (&optional snapshot programs)
  "Return dashboard health counters as a plist.
If SNAPSHOT is provided, read state from it; otherwise read from globals.
If PROGRAMS is provided, use it instead of calling
`elinit--effective-programs'."
  (let ((running 0) (active 0) (done 0) (failed 0) (invalid 0) (pending 0)
        (disabled 0)
        (programs (or programs (elinit--effective-programs)))
        (seen (make-hash-table :test 'equal))
        (invalid-hash (if snapshot
                          (elinit-snapshot-invalid snapshot)
                        elinit--invalid))
        (entry-state-hash (if snapshot
                              (elinit-snapshot-entry-state snapshot)
                            elinit--entry-state))
        (process-alive (when snapshot (elinit-snapshot-process-alive snapshot)))
        (failed-hash (if snapshot
                         (elinit-snapshot-failed snapshot)
                       elinit--failed))
        (oneshot-hash (if snapshot
                          (elinit-snapshot-oneshot-exit snapshot)
                        elinit--oneshot-completed))
        (remain-hash (if snapshot
                         (or (elinit-snapshot-remain-active snapshot)
                             (make-hash-table :test 'equal))
                       elinit--remain-active))
        (idx 0))
    (dolist (entry programs)
      (let ((raw-id (elinit--extract-id entry idx)))
        (cl-incf idx)
        ;; Skip duplicates to match runtime behavior
        (unless (gethash raw-id seen)
          (puthash raw-id t seen)
          (if (gethash raw-id invalid-hash)
              (cl-incf invalid)
            (let ((parsed (ignore-errors (elinit--parse-entry entry))))
              (if (null parsed)
                  (cl-incf invalid)
                (let* ((id (car parsed))
                       (type (elinit-entry-type parsed))
                       (e-state (gethash id entry-state-hash))
                       (alive (if snapshot
                                  (gethash id process-alive)
                                (let ((proc (gethash id elinit--processes)))
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
    (when (boundp 'elinit--unit-file-invalid)
      (maphash (lambda (id _reason)
                 (unless (gethash id seen)
                   (puthash id t seen)
                   (cl-incf invalid)))
               (symbol-value 'elinit--unit-file-invalid)))
    (list :running running
          :active active
          :done done
          :pending pending
          :failed failed
          :invalid invalid
          :disabled disabled)))

(defun elinit--health-summary (&optional snapshot programs)
  "Return compact health summary string.
If SNAPSHOT is provided, read state from it; otherwise read from globals.
If PROGRAMS is provided, use it instead of calling
`elinit--effective-programs'."
  (let* ((counts (elinit--health-counts snapshot programs))
         (running (plist-get counts :running))
         (active (plist-get counts :active))
         (done (plist-get counts :done))
         (pending (plist-get counts :pending))
         (failed (plist-get counts :failed))
         (invalid (plist-get counts :invalid))
         (disabled (plist-get counts :disabled)))
    (concat (propertize (format "%d" running) 'face 'elinit-status-running)
            " run | "
            (when (> active 0)
              (concat (propertize (format "%d" active) 'face 'elinit-status-running)
                      " act | "))
            (propertize (format "%d" done) 'face 'elinit-status-done)
            " done | "
            (propertize (format "%d" pending) 'face 'elinit-status-pending)
            " pend | "
            (when (> disabled 0)
              (concat (propertize (format "%d" disabled) 'face 'elinit-status-stopped)
                      " off | "))
            (propertize (format "%d" failed) 'face 'elinit-status-failed)
            " fail | "
            (propertize (format "%d" invalid) 'face 'elinit-status-invalid)
            " inv")))

(defvar elinit--help-text
  (concat "[f]ilter [v]targets [g]refresh [G]live [P]roced "
          "[l]ifecycle [p]olicy [i]nspect [t]imers [?]menu [h]elp [q]uit")
  "Key hints displayed in dashboard header.")

(defun elinit--dashboard-column-header ()
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

(defun elinit--dashboard-header-line (&optional snapshot programs)
  "Return dashboard header line text with service health counters.
SNAPSHOT and PROGRAMS are forwarded to `elinit--health-counts'."
  (let* ((counts (elinit--health-counts snapshot programs))
         (running (plist-get counts :running))
         (done (plist-get counts :done))
         (pending (plist-get counts :pending))
         (failed (plist-get counts :failed))
         (invalid (plist-get counts :invalid))
         (base (concat (propertize (format "%d" running)
                                   'face 'elinit-status-running)
                       " run    "
                       (propertize (format "%d" done)
                                   'face 'elinit-status-done)
                       " done   "
                       (propertize (format "%d" pending)
                                   'face 'elinit-status-pending)
                       " pend  "
                       (propertize (format "%d" failed)
                                   'face 'elinit-status-failed)
                       " fail     "
                       (propertize (format "%d" invalid)
                                   'face 'elinit-status-invalid)
                       " inv"))
         (root (when elinit--current-plan
                 (elinit-plan-activation-root elinit--current-plan)))
         (view-parts (list "services")))
    (when elinit--dashboard-show-targets
      (setq view-parts (append view-parts (list "targets")))
      (when elinit--dashboard-show-init-targets
        (setq view-parts (append view-parts (list "init")))))
    (let* ((view-tag (propertize
                      (concat "[" (mapconcat #'identity view-parts "+") "]")
                      'face 'elinit-section-separator))
           (result (if root
                       (concat base "    "
                               (propertize "root" 'face 'bold) ": " root)
                     base)))
      (concat result "    " view-tag))))

(defun elinit--refresh-dashboard ()
  "Refresh the dashboard buffer if it exists.
Loads programs once and builds a single runtime snapshot, passing both
to entries and health summary for consistency within a single refresh."
  (when-let* ((buf (get-buffer "*elinit*")))
    (with-current-buffer buf
      (let* ((programs (elinit--effective-programs))
             (snapshot (elinit--build-snapshot))
             (pos (point)))
        (setq tabulated-list-entries
              (elinit--get-entries snapshot programs))
        (tabulated-list-print t)
        (setq header-line-format
              (elinit--dashboard-header-line snapshot programs))
        (goto-char (min pos (point-max)))))))

(defun elinit-dashboard-refresh ()
  "Refresh the dashboard."
  (interactive)
  (elinit--refresh-dashboard))

(defun elinit--all-target-ids ()
  "Return sorted list of all target IDs from current plan.
Includes all target-type entries, even those with no members."
  (let ((ids nil))
    (when elinit--current-plan
      ;; Collect targets from membership hash (targets with members)
      (when-let* ((members (elinit-plan-target-members
                            elinit--current-plan)))
        (when (hash-table-p members)
          (maphash (lambda (id _) (cl-pushnew id ids :test #'equal))
                   members)))
      ;; Collect target-type entries from plan (includes empty targets)
      (dolist (entry (elinit-plan-entries elinit--current-plan))
        (when (eq (elinit-entry-type entry) 'target)
          (cl-pushnew (elinit-entry-id entry) ids :test #'equal))))
    (sort ids #'string<)))

(defun elinit-dashboard-cycle-filter ()
  "Cycle dashboard target filter through all defined targets."
  (interactive)
  (let* ((all-targets (elinit--all-target-ids))
         (current elinit--dashboard-target-filter)
         (idx (cl-position current all-targets :test #'equal)))
    (setq elinit--dashboard-target-filter
          (cond
           ((null all-targets) nil)
           ((null idx) (car all-targets))
           ((= idx (1- (length all-targets))) nil)
           (t (nth (1+ idx) all-targets))))
    (message "Target filter: %s"
             (or elinit--dashboard-target-filter "all"))
    (elinit--refresh-dashboard)))

(defun elinit--all-tags ()
  "Return list of all unique tags used in entries."
  (let ((tags nil))
    (dolist (entry (elinit--effective-programs))
      (unless (stringp entry)
        (let ((entry-tags (plist-get (cdr entry) :tags)))
          (dolist (tag (if (listp entry-tags) entry-tags (list entry-tags)))
            (when tag
              (cl-pushnew tag tags :test #'equal))))))
    (sort tags (lambda (a b)
                 (string< (format "%s" a) (format "%s" b))))))

(defun elinit-dashboard-cycle-tag-filter ()
  "Cycle dashboard tag filter through all available tags."
  (interactive)
  (let* ((all-tags (elinit--all-tags))
         (current elinit--dashboard-tag-filter)
         (idx (cl-position current all-tags :test #'equal)))
    (setq elinit--dashboard-tag-filter
          (cond
           ((null all-tags) nil)
           ((null idx) (car all-tags))
           ((= idx (1- (length all-tags))) nil)
           (t (nth (1+ idx) all-tags))))
    (message "Tag filter: %s"
             (if elinit--dashboard-tag-filter
                 (format "%s" elinit--dashboard-tag-filter)
               "all"))
    (elinit--refresh-dashboard)))

(defun elinit-dashboard-toggle-targets ()
  "Toggle visibility of target-type entries in the dashboard.
When targets are hidden (the default), only service and oneshot rows
are shown.  Init-transition targets remain hidden unless separately
toggled with \\[elinit-dashboard-toggle-init-targets]."
  (interactive)
  (setq elinit--dashboard-show-targets
        (not elinit--dashboard-show-targets))
  (message "Targets: %s"
           (if elinit--dashboard-show-targets "shown" "hidden"))
  (elinit--refresh-dashboard))

(defun elinit-dashboard-toggle-init-targets ()
  "Toggle visibility of init-transition targets in the dashboard.
Init-transition targets (rescue, shutdown, poweroff, reboot, runlevel0,
runlevel1, runlevel6) are hidden by default even when regular targets are shown.
This toggle has no effect when targets are hidden entirely."
  (interactive)
  (setq elinit--dashboard-show-init-targets
        (not elinit--dashboard-show-init-targets))
  (if elinit--dashboard-show-targets
      (message "Init targets: %s"
               (if elinit--dashboard-show-init-targets "shown" "hidden"))
    (message "Init targets: %s (targets are hidden, toggle with v first)"
             (if elinit--dashboard-show-init-targets "shown" "hidden")))
  (elinit--refresh-dashboard))

(defun elinit-dashboard-target-members ()
  "Show members of target at point."
  (interactive)
  (let* ((id (elinit--require-service-row))
         (entry (elinit--get-entry-for-id id)))
    (if (or (not entry) (not (eq (elinit-entry-type entry) 'target)))
        (message "Not a target entry: %s" id)
      (let ((members (when (hash-table-p elinit--target-members)
                       (gethash id elinit--target-members))))
        (if (not members)
            (message "%s: no members" id)
          (let ((req (plist-get members :requires))
                (wants (plist-get members :wants)))
            (message "%s members: requires=[%s] wants=[%s]"
                     id
                     (if req (mapconcat #'identity req ", ") "none")
                     (if wants (mapconcat #'identity wants ", ") "none"))))))))

(defvar elinit--status-legend
  "Status: running=active process | active=oneshot remain-after-exit | done=oneshot completed ok | failed=crashed/exit>0 | dead=crash-loop | pending=not yet started | unreachable=outside activation closure | stopped=terminated | invalid=config error"
  "Legend explaining status column values.")

(defun elinit--describe-format-duration (seconds)
  "Format SECONDS as a human-readable duration string."
  (let ((s (round seconds)))
    (cond
     ((< s 60) (format "%ds" s))
     ((< s 3600) (format "%dm%ds" (/ s 60) (% s 60)))
     ((< s 86400) (format "%dh%dm" (/ s 3600) (% (/ s 60) 60)))
     (t (format "%dd%dh" (/ s 86400) (% (/ s 3600) 24))))))

(defun elinit-dashboard-describe-entry ()
  "Show detailed information about entry at point.
With prefix argument, show status legend instead."
  (interactive)
  (if current-prefix-arg
      (message "%s" elinit--status-legend)
    (let* ((id (elinit--require-service-row))
           (invalid-reason (gethash id elinit--invalid)))
      (if invalid-reason
          (message "INVALID: %s" invalid-reason)
        (let ((entry (elinit--get-entry-for-id id)))
          (if (not entry)
              (message "Entry not found: %s" id)
            (elinit--describe-entry-detail id entry)))))))

(defun elinit--describe-entry-detail (id entry)
  "Show detailed telemetry for ID with parsed ENTRY in a help window."
  (let* ((type (elinit-entry-type entry))
         (enabled-p (elinit-entry-enabled-p entry))
         (restart-policy (elinit-entry-restart-policy entry))
         (logging-p (elinit-entry-logging-p entry))
         (delay (elinit-entry-delay entry))
         (after (elinit-entry-after entry))
         (requires (elinit-entry-requires entry))
         (description (elinit-entry-description entry))
         (documentation (elinit-entry-documentation entry))
         (status-result (elinit--compute-entry-status id type))
         (status (car status-result))
         (reason (elinit--compute-entry-reason id type))
         (proc (gethash id elinit--processes))
         (pid (when (and proc (process-live-p proc)) (process-id proc)))
         (eff-enabled (elinit--get-effective-enabled id enabled-p))
         (eff-restart (unless (eq type 'oneshot)
                        (elinit--get-effective-restart id restart-policy)))
         (eff-logging (elinit--get-effective-logging id logging-p))
         (uptime (elinit--telemetry-uptime id))
         (restart-count (elinit--telemetry-restart-count id))
         (last-exit (elinit--telemetry-last-exit id))
         (last-exit-info (elinit--telemetry-last-exit-info id))
         (next-eta (elinit--telemetry-next-restart-eta id))
         (metrics (when pid (elinit--telemetry-process-metrics pid)))
         (unit-file
          (cond
           ((fboundp 'elinit--unit-file-existing-path)
            (elinit--unit-file-existing-path id))
           ((fboundp 'elinit--unit-file-path)
            (when-let* ((path (elinit--unit-file-path id)))
              (when (file-exists-p path)
                path)))))
         (log-tail (elinit--telemetry-log-tail id 5)))
    (with-help-window "*elinit-info*"
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
        (when-let* ((tree (elinit--telemetry-process-tree pid)))
          (let ((count (plist-get tree :count))
                (pids (plist-get tree :pids)))
            (princ (format "     Tree: %d descendant%s [%s]\n"
                           count (if (= count 1) "" "s")
                           (mapconcat (lambda (p) (format "%d" p))
                                      pids ", "))))))
      (when uptime
        (princ (format "   Uptime: %s\n"
                       (elinit--describe-format-duration uptime))))
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
                                    (elinit--normalize-restart-policy
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
      (let ((conflicts (elinit-entry-conflicts entry)))
        (princ (format "Conflicts: %s\n"
                       (if conflicts
                           (mapconcat #'identity conflicts ", ")
                         "none"))))
      ;; Target-specific convergence info
      (when (eq type 'target)
        (let* ((effective-id (elinit--resolve-target-alias id))
               (conv (when (hash-table-p elinit--target-convergence)
                       (gethash effective-id elinit--target-convergence)))
               (reasons (when (hash-table-p elinit--target-convergence-reasons)
                          (gethash effective-id elinit--target-convergence-reasons)))
               (members (when (hash-table-p elinit--target-members)
                          (gethash effective-id elinit--target-members))))
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
                       (if (elinit-entry-oneshot-blocking entry)
                           "yes" "no")))
        (let ((timeout (elinit-entry-oneshot-timeout entry)))
          (when timeout
            (princ (format "  Timeout: %ss\n" timeout)))))
      ;; Identity (privilege drop)
      (let ((u (elinit-entry-user entry))
            (g (elinit-entry-group entry)))
        (when (or u g)
          (when u (princ (format "     User: %s\n" u)))
          (when g (princ (format "    Group: %s\n" g)))))
      ;; Sandbox indicator
      (when (elinit--sandbox-requesting-p entry)
        (let* ((profile (or (elinit-entry-sandbox-profile entry) 'none))
               (effective-network
                (or (elinit-entry-sandbox-network entry)
                    (elinit--sandbox-profile-default-network profile))))
          (princ (format "  Sandbox: %s (network %s)\n"
                         profile effective-network))))
      ;; Resource limits
      (when (elinit--limits-requesting-p entry)
        (let ((parts nil))
          (dolist (pair (list (cons #'elinit-entry-limit-nofile "nofile")
                              (cons #'elinit-entry-limit-nproc "nproc")
                              (cons #'elinit-entry-limit-core "core")
                              (cons #'elinit-entry-limit-fsize "fsize")
                              (cons #'elinit-entry-limit-as "as")))
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
                            (elinit--describe-format-duration etime))
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
          (let ((tier (when (fboundp 'elinit--authority-tier-for-id)
                        (elinit--authority-tier-for-id id))))
            (princ (format "Unit file: %s%s\n" unit-file
                           (if tier (format " (tier %d)" tier) "")))))
        (when documentation
          (princ (format "     Docs: %s\n"
                         (mapconcat #'identity documentation ", ")))))
      ;; Log tail
      (when log-tail
        (princ "\nRecent log:\n")
        (princ log-tail)))))

(defun elinit-dashboard-help ()
  "Show full help for elinit dashboard."
  (interactive)
  (with-help-window "*Elinit Help*"
    (princ "Elinit Dashboard Help\n")
    (princ "=========================\n\n")
    (princ "TOP-LEVEL KEYS\n")
    (princ "--------------\n")
    (princ "  f     Cycle target filter (all -> target1 -> target2 -> ...)\n")
    (princ "  F     Cycle tag filter\n")
    (princ "  v     Toggle target visibility (service-first default)\n")
    (princ "  V     Toggle init-transition targets (rescue/shutdown/poweroff/reboot/rl0/1/6)\n")
    (princ "  g     Refresh dashboard\n")
    (princ "  G     Toggle auto-refresh (live monitoring)\n")
    (princ "  P     Open proced (system process list)\n")
    (princ "  l     Lifecycle submenu (service rows only)\n")
    (princ "  p     Policy submenu (service rows only)\n")
    (princ "  i     Inspect submenu (service rows only)\n")
    (princ "  t     Timer actions submenu (timer rows only)\n")
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
    (princ "TIMERS (t) - timer rows only\n")
    (princ "----------------------------\n")
    (princ "  t     Trigger timer now (manual)\n")
    (princ "  i     Show timer details\n")
    (princ "  j     Jump to target service row\n")
    (princ "  r     Reset timer runtime state\n")
    (princ "  g     Refresh timer section\n\n")
    (princ "TIMER LIMITATIONS\n")
    (princ "-----------------\n")
    (princ "Timer rows are not unit files.  Lifecycle, policy, cat, and edit\n")
    (princ "commands are not available for timer rows.  Use `t' for timer\n")
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

(defvar-local elinit--auto-refresh-timer nil
  "Timer for auto-refresh in dashboard buffer.")

(defcustom elinit-auto-refresh-interval 2
  "Interval in seconds for dashboard auto-refresh when enabled."
  :type 'integer
  :group 'elinit)

(defun elinit--cleanup-auto-refresh ()
  "Clean up auto-refresh timer when dashboard buffer is killed."
  (when elinit--auto-refresh-timer
    (cancel-timer elinit--auto-refresh-timer)
    (setq elinit--auto-refresh-timer nil)))

(defun elinit-dashboard-toggle-auto-refresh ()
  "Toggle automatic dashboard refresh for live monitoring."
  (interactive)
  (if elinit--auto-refresh-timer
      (progn
        (cancel-timer elinit--auto-refresh-timer)
        (setq elinit--auto-refresh-timer nil)
        (message "Auto-refresh disabled"))
    (setq elinit--auto-refresh-timer
          (run-with-timer elinit-auto-refresh-interval
                          elinit-auto-refresh-interval
                          (lambda ()
                            (when (buffer-live-p (get-buffer "*elinit*"))
                              (elinit--refresh-dashboard)))))
    (message "Auto-refresh enabled (every %ds)" elinit-auto-refresh-interval)))

(defun elinit-dashboard-quit ()
  "Quit dashboard and clean up auto-refresh timer."
  (interactive)
  (elinit--cleanup-auto-refresh)
  (quit-window))

(defun elinit-dashboard-stop ()
  "Stop process at point with confirmation.
Gracefully stops the process and suppresses auto-restart.
Rejects separator rows, timer rows, and non-active oneshot entries.
Oneshot entries with `:remain-after-exit' in active state can be stopped.
Use `s' to start the process again later."
  (interactive)
  (let ((id (elinit--require-service-row)))
    (let ((entry (elinit--get-entry-for-id id)))
      (when (and entry (eq (elinit-entry-type entry) 'target))
        (user-error "Cannot stop target unit '%s'" id))
      (when (and entry (eq (elinit-entry-type entry) 'oneshot)
                 (not (gethash id elinit--remain-active)))
        (user-error "Cannot stop oneshot entry '%s'" id)))
    (when (yes-or-no-p (format "Stop process '%s'? " id))
      (let ((result (elinit--manual-stop id)))
        (pcase (plist-get result :status)
          ('stopped (elinit--refresh-dashboard))
          ('skipped (message "Entry %s is %s" id (plist-get result :reason)))
          ('error (message "Cannot stop %s: %s"
                           id (plist-get result :reason))))))))

(defun elinit-dashboard-restart ()
  "Restart process at point with confirmation.
Stop the process gracefully, then start it again.
If the entry is not running, this is equivalent to start."
  (interactive)
  (let ((id (elinit--require-service-row)))
    (let ((entry (elinit--get-entry-for-id id)))
      (when (and entry (eq (elinit-entry-type entry) 'target))
        (user-error "Cannot restart target unit '%s'" id)))
    (when (yes-or-no-p (format "Restart process '%s'? " id))
      ;; Stop first (ignore result — entry may not be running)
      (elinit--manual-stop id)
      ;; Then start unconditionally, matching CLI restart semantics
      (let ((start-result (elinit--manual-start id)))
        (pcase (plist-get start-result :status)
          ('started (elinit--refresh-dashboard))
          ('skipped
           (message "Cannot start %s: %s"
                    id (plist-get start-result :reason)))
          ('error
           (message "Failed to start %s: %s"
                    id (plist-get start-result :reason))))))))

(defun elinit-dashboard-kill (&optional force)
  "Kill process at point with confirmation.
Sends kill signal and leaves restart policy unchanged.
With prefix argument FORCE, skip confirmation."
  (interactive "P")
  (let ((id (elinit--require-service-row)))
    (when (or force
              (yes-or-no-p (format "Kill process '%s'? " id)))
      (let ((result (elinit--manual-kill id)))
        (pcase (plist-get result :status)
          ('signaled (elinit--refresh-dashboard))
          ('skipped (message "Entry %s is %s" id (plist-get result :reason))))))))

(defun elinit-dashboard-kill-force ()
  "Kill process at point without confirmation.
Sends kill signal immediately and leaves restart policy unchanged."
  (interactive)
  (elinit-dashboard-kill t))

(defun elinit-dashboard-reset-failed ()
  "Reset failed state for entry at point.
Clears crash-loop tracking so the entry can be restarted."
  (interactive)
  (let ((id (elinit--require-service-row)))
    (let ((result (elinit--reset-failed id)))
      (pcase (plist-get result :status)
        ('reset
         (message "Reset failed state for %s" id)
         (elinit--refresh-dashboard))
        ('skipped (message "%s is %s" id (plist-get result :reason)))))))

(defun elinit-dashboard-start ()
  "Start process at point if stopped.
Disabled units can be started (session-only); only mask blocks."
  (interactive)
  (let ((id (elinit--require-service-row)))
    (let ((result (elinit--manual-start id)))
      (pcase (plist-get result :status)
        ('started (elinit--refresh-dashboard))
        ('skipped (message "Entry %s is %s" id (plist-get result :reason)))
        ('error (message "Cannot start %s: %s" id (plist-get result :reason)))))))

;;; Explicit Policy Commands

(defun elinit-dashboard-enable ()
  "Enable the entry at point.
Set an enabled override if the config default is disabled."
  (interactive)
  (let ((id (elinit--require-service-row)))
    (let ((result (elinit--policy-enable id)))
      (message "%s" (plist-get result :message))
      (when (eq (plist-get result :status) 'applied)
        (elinit--save-overrides)
        (elinit--refresh-dashboard)))))

(defun elinit-dashboard-disable ()
  "Disable the entry at point.
Set a disabled override if the config default is enabled."
  (interactive)
  (let ((id (elinit--require-service-row)))
    (let ((result (elinit--policy-disable id)))
      (message "%s" (plist-get result :message))
      (when (eq (plist-get result :status) 'applied)
        (elinit--save-overrides)
        (elinit--refresh-dashboard)))))

(defun elinit-dashboard-mask ()
  "Mask the entry at point.
Masked entries are always disabled regardless of enabled overrides."
  (interactive)
  (let ((id (elinit--require-service-row)))
    (let ((result (elinit--policy-mask id)))
      (message "%s" (plist-get result :message))
      (when (eq (plist-get result :status) 'applied)
        (elinit--save-overrides)
        (elinit--refresh-dashboard)))))

(defun elinit-dashboard-unmask ()
  "Unmask the entry at point.
Remove the mask override so the enabled state takes effect again."
  (interactive)
  (let ((id (elinit--require-service-row)))
    (let ((result (elinit--policy-unmask id)))
      (message "%s" (plist-get result :message))
      (when (eq (plist-get result :status) 'applied)
        (elinit--save-overrides)
        (elinit--refresh-dashboard)))))

(defun elinit-dashboard-set-restart-policy ()
  "Set restart policy for entry at point via selection.
Prompt with `completing-read' to choose the target policy explicitly."
  (interactive)
  (let ((id (elinit--require-service-row)))
    (let* ((choices '("always" "on-success" "on-failure" "no"))
           (choice (completing-read "Restart policy: " choices nil t))
           (policy (intern choice))
           (result (elinit--policy-set-restart id policy)))
      (message "%s" (plist-get result :message))
      (when (eq (plist-get result :status) 'applied)
        (elinit--save-overrides)
        (elinit--refresh-dashboard)))))

(defun elinit-dashboard-set-logging ()
  "Set logging for entry at point via selection.
Prompt with `completing-read' to choose on or off explicitly."
  (interactive)
  (let ((id (elinit--require-service-row)))
    (let* ((choice (completing-read "Logging: " '("on" "off") nil t))
           (enabled-p (string= choice "on"))
           (result (elinit--policy-set-logging id enabled-p)))
      (message "%s" (plist-get result :message))
      (when (eq (plist-get result :status) 'applied)
        (elinit--save-overrides)
        (elinit--refresh-dashboard)))))

(defun elinit-dashboard-view-log ()
  "Open the log file for process at point.
Decode all formats through the structured decoder and display in a
read-only buffer with formatted timestamps and stream labels.
The view is bounded to `elinit-dashboard-log-view-record-limit'
records for interactive responsiveness."
  (interactive)
  (let ((id (elinit--require-service-row)))
    (let ((log-file (elinit--log-file id)))
      (if (not (file-exists-p log-file))
          (message "No log file for %s" id)
        (let* ((limit (if (and (integerp elinit-dashboard-log-view-record-limit)
                              (> elinit-dashboard-log-view-record-limit 0))
                         elinit-dashboard-log-view-record-limit
                       1000))
               (decoded (elinit--log-decode-file
                         log-file limit nil (* limit 512)))
               (records (plist-get decoded :records))
               (buf-name (format "*elinit-log-%s*" id)))
          (with-current-buffer (get-buffer-create buf-name)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (if records
                  (insert (mapconcat
                           #'elinit--log-format-record-human
                           records "\n")
                          "\n")
                (insert (format "No records in %s\n" log-file))))
            (goto-char (point-min))
            (special-mode))
          (pop-to-buffer buf-name))))))

(defun elinit-dashboard-cat ()
  "View unit file for entry at point in read-only mode.
Opens the unit file with `view-mode' so `q' returns to the dashboard."
  (interactive)
  (let ((id (elinit--require-service-row)))
    (let ((path (elinit--unit-file-path id)))
      (cond
       ((not path)
        (user-error "No active authority roots configured"))
       ((file-exists-p path)
        (view-file path))
       ((elinit--get-entry-for-id id)
        (user-error "No unit file on disk for %s; use edit to create an override" id))
       (t
        (user-error "Unit file not found: %s" path))))))

(defvar elinit-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-q") #'elinit-edit-finish)
    (define-key map (kbd "q") #'elinit-edit-quit)
    map)
  "Keymap for `elinit-edit-mode'.")

(define-minor-mode elinit-edit-mode
  "Minor mode for editing elinit unit files.
Provides `q' to return to dashboard (prompts to save if modified),
and \\[elinit-edit-finish] to save and return unconditionally.
Validates the unit file on save."
  :lighter " SvEdit"
  :keymap elinit-edit-mode-map
  (if elinit-edit-mode
      (progn
        (add-hook 'after-save-hook
                  #'elinit--validate-unit-file-buffer nil t)
        (add-hook 'kill-buffer-hook
                  #'elinit--return-to-dashboard nil t))
    (remove-hook 'after-save-hook
                 #'elinit--validate-unit-file-buffer t)
    (remove-hook 'kill-buffer-hook
                 #'elinit--return-to-dashboard t)))

(defun elinit-edit-finish ()
  "Save current unit file buffer and return to the dashboard.
If modified, save first.  Then kill the buffer and return."
  (interactive)
  (when (and (buffer-modified-p) (buffer-file-name))
    (save-buffer))
  (let ((buf (current-buffer)))
    (elinit--return-to-dashboard)
    (kill-buffer buf)))

(defun elinit-edit-quit ()
  "Save if needed and return to the dashboard.
If the buffer is modified, offer to save first.  Then kill the
buffer and switch to the `*elinit*' dashboard."
  (interactive)
  (when (and (buffer-modified-p) (buffer-file-name))
    (if (y-or-n-p "Unit file modified; save before returning? ")
        (save-buffer)
      (set-buffer-modified-p nil)))
  (let ((buf (current-buffer)))
    (elinit--return-to-dashboard)
    (kill-buffer buf)))

(defun elinit-dashboard-edit ()
  "Edit unit file for entry at point.
If the unit file does not exist, prompt to create a scaffold template.
On save, validate the unit file and report errors.
Press `q' or \\[elinit-edit-finish] to return to the dashboard."
  (interactive)
  (let ((id (elinit--require-service-row)))
    (let* ((path (elinit--unit-file-path id))
           (root (or (when (fboundp 'elinit--authority-root-for-id)
                       (elinit--authority-root-for-id id))
                     (when path (file-name-directory path))))
           (tier (when (fboundp 'elinit--authority-tier-for-id)
                   (elinit--authority-tier-for-id id))))
      (unless path
        (user-error "No active authority roots configured"))
      (let ((created (not (file-exists-p path))))
        (when created
          (unless (y-or-n-p (format "No unit file for %s.  Create override? " id))
            (user-error "Cancelled"))
          (make-directory (file-name-directory path) t)
          (write-region (elinit--unit-file-scaffold id) nil path))
        ;; Open the file and activate edit mode
        (find-file path)
        (elinit-edit-mode 1)
        (message "%s %s in %s (tier %s)"
                 (if created "Created" "Editing")
                 id (or root "unknown root")
                 (or tier "?"))))))

(defun elinit--return-to-dashboard ()
  "Return to the *elinit* dashboard buffer if it exists.
Intended as `kill-buffer-hook' for edited unit files."
  (when-let* ((buf (get-buffer "*elinit*")))
    (when (buffer-live-p buf)
      (pop-to-buffer buf))))


(defun elinit-dashboard-reload-unit ()
  "Hot-reload the unit at point.
Re-reads config and restarts if running."
  (interactive)
  (let ((id (elinit--require-service-row)))
    (let ((result (elinit--reload-unit id)))
      (elinit-dashboard-refresh)
      (message "Reload %s: %s" id (plist-get result :action)))))

(defun elinit-dashboard-daemon-reload ()
  "Reload unit definitions from disk without affecting runtime.
Calls `elinit-daemon-reload' and refreshes the dashboard."
  (interactive)
  (let ((result (elinit-daemon-reload)))
    (elinit-dashboard-refresh)
    (message "Daemon-reload: %d entries, %d invalid"
             (plist-get result :entries)
             (plist-get result :invalid))))

(defun elinit-dashboard-show-deps ()
  "Show computed dependencies for entry at point.
Shows post-validation edges: after cycle fallback and dep filtering.
Run `elinit-start' first to populate computed dependency data."
  (interactive)
  (let ((id (elinit--require-service-row)))
    (if (gethash id elinit--invalid)
        (message "Cannot show dependencies for invalid entry: %s" id)
      (let ((entry (elinit--get-entry-for-id id)))
        (if entry
            (let* ((cycle-fallback (gethash id elinit--cycle-fallback-ids))
                   ;; Use computed deps if available, else show raw (pre-start)
                   (computed (gethash id elinit--computed-deps))
                   (effective-deps (if cycle-fallback
                                       nil  ; cycle fallback clears all deps
                                     (or computed (elinit-entry-after entry))))
                   ;; Find entries that depend on this one (computed)
                   (dependents nil))
              ;; Scan computed-deps for entries that list this ID
              (maphash (lambda (e-id e-deps)
                         (when (and (not (string= e-id id))
                                    (not (gethash e-id elinit--cycle-fallback-ids))
                                    (member id e-deps))
                           (push e-id dependents)))
                       elinit--computed-deps)
              (message "%s%s: depends-on=%s blocks=%s"
                       id
                       (if cycle-fallback " (cycle fallback)" "")
                       (if effective-deps (mapconcat #'identity effective-deps ", ") "none")
                       (if dependents (mapconcat #'identity dependents ", ") "none")))
          (message "Entry not found: %s" id))))))

(defun elinit-dashboard-blame ()
  "Show startup timing blame view sorted by duration."
  (interactive)
  (elinit--require-service-row)
  (if (= 0 (hash-table-count elinit--start-times))
      (message "No timing data available (run elinit-start first)")
    (let ((entries nil))
      (maphash (lambda (id start-time)
                 (let* ((ready-time (gethash id elinit--ready-times))
                        (duration (if ready-time
                                      (- ready-time start-time)
                                    nil)))
                   (push (list id start-time ready-time duration) entries)))
               elinit--start-times)
      ;; Sort by duration descending (nil durations at end)
      (setq entries (sort entries
                          (lambda (a b)
                            (let ((da (nth 3 a))
                                  (db (nth 3 b)))
                              (cond ((and da db) (> da db))
                                    (da t)
                                    (t nil))))))
      ;; Display in a help buffer
      (with-help-window "*elinit-blame*"
        (princ "Elinit Startup Blame (sorted by duration)\n")
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

(defun elinit-dashboard-show-graph ()
  "Show full dependency graph for all entries.
Displays computed dependencies after validation and cycle fallback."
  (interactive)
  (elinit--require-service-row)
  (if (= 0 (hash-table-count elinit--computed-deps))
      (message "No dependency data available (run elinit-start first)")
    (with-help-window "*elinit-deps*"
      (princ "Elinit Dependency Graph\n")
      (princ (make-string 50 ?=))
      (princ "\n\n")
      ;; Flat listing of all entries
      (let ((all-ids nil))
        (let ((idx 0))
          (dolist (entry (elinit--effective-programs))
            (let ((id (elinit--extract-id entry idx)))
              (cl-incf idx)
              (unless (gethash id elinit--invalid)
                (push id all-ids)))))
        (dolist (id (nreverse all-ids))
          (let* ((deps (gethash id elinit--computed-deps))
                 (cycle-fallback (gethash id elinit--cycle-fallback-ids))
                 (dependents nil))
            ;; Find entries that depend on this one
            (maphash (lambda (e-id e-deps)
                       (when (member id e-deps)
                         (push e-id dependents)))
                     elinit--computed-deps)
            (princ (format "  %s%s\n"
                           id
                           (if cycle-fallback " [CYCLE FALLBACK]" "")))
            (when deps
              (princ (format "    <- %s\n" (mapconcat #'identity deps ", "))))
            (when dependents
              (princ (format "    -> %s\n" (mapconcat #'identity dependents ", "))))))))))

;;;###autoload
(defun elinit ()
  "Open the elinit dashboard.
Builds a single snapshot for both entries and header to ensure consistency."
  (interactive)
  (let ((buf (get-buffer-create "*elinit*")))
    (with-current-buffer buf
      (elinit-dashboard-mode)
      ;; Build snapshot once and use for both entries and header
      (let ((snapshot (elinit--build-snapshot)))
        (setq tabulated-list-entries (elinit--get-entries snapshot))
        (tabulated-list-print)
        (setq-local header-line-format
                    (elinit--dashboard-header-line snapshot))))
    (pop-to-buffer buf)))



;;; Timer Dashboard Commands

(defun elinit-dashboard-timer-trigger ()
  "Trigger the timer at point manually.
Reject invalid timers and when timer mode gate is off."
  (interactive)
  (let ((raw-id (tabulated-list-get-id)))
    (cond
     ((null raw-id)
      (user-error "No entry at point"))
     ((not (elinit--timer-row-p raw-id))
      (user-error "Not a timer row; use on timer rows only"))
     (t
      (let ((timer-id (elinit--row-id raw-id)))
        (unless (elinit-timer-subsystem-active-p)
          (user-error "Timer subsystem is disabled"))
        (when (gethash timer-id elinit--invalid-timers)
          (user-error "Cannot trigger invalid timer '%s'" timer-id))
        (let ((timer (cl-find timer-id elinit--timer-list
                              :key #'elinit-timer-id :test #'equal)))
          (unless timer
            (user-error "Timer not found: %s" timer-id))
          (if (elinit-timer--trigger timer 'manual)
              (progn
                (message "Triggered timer '%s'" timer-id)
                (elinit--refresh-dashboard))
            (message "Timer '%s' skipped (check logs)" timer-id))))))))

(defun elinit-dashboard-timer-info ()
  "Show detailed information about the timer at point."
  (interactive)
  (let ((raw-id (tabulated-list-get-id)))
    (cond
     ((null raw-id) (user-error "No entry at point"))
     ((not (elinit--timer-row-p raw-id)) (user-error "Not a timer row"))
     (t
      (let* ((timer-id (elinit--row-id raw-id))
             (timer (cl-find timer-id elinit--timer-list
                             :key #'elinit-timer-id :test #'equal))
             (state (gethash timer-id elinit--timer-state)))
        (with-help-window "*elinit-timer-info*"
          (if (null timer)
              ;; Invalid timer
              (let ((reason (gethash timer-id elinit--invalid-timers)))
                (princ (format "Timer: %s (INVALID)\n" timer-id))
                (princ (make-string 40 ?-))
                (princ (format "\n\nReason: %s\n" (or reason "unknown"))))
            ;; Valid timer detail
            (princ (format "Timer: %s\n" timer-id))
            (princ (make-string (+ 8 (length timer-id)) ?-))
            (princ "\n\n")
            (princ (format "     Target: %s\n" (elinit-timer-target timer)))
            (princ (format "    Enabled: %s\n"
                           (if (elinit-timer-enabled timer) "yes" "no")))
            (princ (format " Persistent: %s\n"
                           (if (elinit-timer-persistent timer) "yes" "no")))
            (when-let* ((cal (elinit-timer-on-calendar timer)))
              (princ (format "   Calendar: %S\n" cal)))
            (when-let* ((startup (elinit-timer-on-startup-sec timer)))
              (princ (format "    Startup: %ds\n" startup)))
            (when-let* ((active (elinit-timer-on-unit-active-sec timer)))
              (princ (format " UnitActive: %ds\n" active)))
            ;; Runtime state
            (when state
              (princ "\n")
              (when-let* ((last-run (plist-get state :last-run-at)))
                (princ (format "   Last run: %s (%s)\n"
                               (format-time-string "%Y-%m-%d %H:%M:%S" last-run)
                               (elinit--format-timer-relative-time last-run))))
              (when-let* ((next-run (plist-get state :next-run-at)))
                (princ (format "   Next run: %s (%s)\n"
                               (format-time-string "%Y-%m-%d %H:%M:%S" next-run)
                               (elinit--format-timer-relative-time next-run))))
              (when-let* ((last-exit (plist-get state :last-exit)))
                (princ (format "  Last exit: %s\n" last-exit)))
              (when-let* ((retry (plist-get state :retry-attempt)))
                (when (> retry 0)
                  (princ (format "      Retry: %d/%d\n" retry
                                 (length elinit-timer-retry-intervals)))))
              (when-let* ((retry-at (plist-get state :retry-next-at)))
                (princ (format "   Retry at: %s\n"
                               (elinit--format-timer-relative-time retry-at))))
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

(defun elinit-dashboard-timer-jump ()
  "Jump to the target service row for the timer at point."
  (interactive)
  (let ((raw-id (tabulated-list-get-id)))
    (cond
     ((null raw-id) (user-error "No entry at point"))
     ((not (elinit--timer-row-p raw-id)) (user-error "Not a timer row"))
     (t
      (let* ((timer-id (elinit--row-id raw-id))
             (timer (cl-find timer-id elinit--timer-list
                             :key #'elinit-timer-id :test #'equal)))
        (if (null timer)
            (user-error "Timer not found: %s" timer-id)
          (let ((target-id (elinit-timer-target timer))
                (found nil))
            (save-excursion
              (goto-char (point-min))
              (while (and (not found) (not (eobp)))
                (when-let* ((row-id (tabulated-list-get-id)))
                  (when (and (elinit--service-row-p row-id)
                             (string= (elinit--row-id row-id) target-id))
                    (setq found (point))))
                (forward-line 1)))
            (if found
                (progn
                  (goto-char found)
                  (message "Target: %s" target-id))
              (message "Target service '%s' not visible in current view"
                       target-id)))))))))

(defun elinit-dashboard-timer-reset ()
  "Reset runtime state for the timer at point.
Clear runtime fields and recompute next run time."
  (interactive)
  (let ((raw-id (tabulated-list-get-id)))
    (cond
     ((null raw-id) (user-error "No entry at point"))
     ((not (elinit--timer-row-p raw-id)) (user-error "Not a timer row"))
     (t
      (let ((timer-id (elinit--row-id raw-id)))
        (when (yes-or-no-p (format "Reset runtime state for timer '%s'? " timer-id))
          (let ((state (or (gethash timer-id elinit--timer-state) nil)))
            (dolist (key '(:last-run-at :last-success-at :last-failure-at :last-exit
                           :retry-attempt :retry-next-at :last-missed-at
                           :last-miss-reason :next-run-at :startup-triggered))
              (setq state (plist-put state key nil)))
            (puthash timer-id state elinit--timer-state)
            ;; Recompute next-run
            (elinit-timer--update-next-run timer-id)
            ;; Persist
            (when (elinit-timer-subsystem-active-p)
              (elinit-timer--save-state))
            (message "Reset timer state for '%s'" timer-id)
            (elinit--refresh-dashboard))))))))

(defun elinit-dashboard-timer-refresh ()
  "Refresh the timer section of the dashboard."
  (interactive)
  (elinit--refresh-dashboard)
  (message "Timer section refreshed"))

(provide 'elinit-dashboard)

;;; elinit-dashboard.el ends here
