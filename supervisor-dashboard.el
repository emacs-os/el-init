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
(declare-function supervisor-timer--target-active-p "supervisor-timer" (timer))

;; Forward declarations for unit-file module (optional)
(declare-function supervisor--unit-file-path "supervisor-units" (id))
(declare-function supervisor--unit-file-scaffold "supervisor-units" (id))
(declare-function supervisor--validate-unit-file-buffer "supervisor-units" ())
(declare-function supervisor--authority-root-for-id "supervisor-units" (id))
(declare-function supervisor--authority-tier-for-id "supervisor-units" (id))

;; Forward declarations for timer state variables (defined in supervisor-timer.el)
(defvar supervisor--timer-state)
(defvar supervisor--timer-list)
(defvar supervisor--invalid-timers)

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

(defface supervisor-type-timer
  '((t :foreground "#98d8c8"))
  "Face for timer type in dashboard."
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

(defcustom supervisor-dashboard-show-timers t
  "When non-nil, show timers section in dashboard after services."
  :type 'boolean
  :group 'supervisor)

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
    ;; Navigation / filter
    (define-key map "f" #'supervisor-dashboard-cycle-filter)
    (define-key map "F" #'supervisor-dashboard-cycle-tag-filter)
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
          "Inspect: [i]nfo [d]eps [g]raph [b]lame [l]og [c]at [e]dit ")
    (?i (call-interactively #'supervisor-dashboard-describe-entry))
    (?d (call-interactively #'supervisor-dashboard-show-deps))
    (?g (call-interactively #'supervisor-dashboard-show-graph))
    (?b (call-interactively #'supervisor-dashboard-blame))
    (?l (call-interactively #'supervisor-dashboard-view-log))
    (?c (call-interactively #'supervisor-dashboard-cat))
    (?e (call-interactively #'supervisor-dashboard-edit))
    (_ (message "Inspect: cancelled"))))

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
         ["Actions"
          ("l" "Lifecycle..." supervisor-dashboard-lifecycle)
          ("p" "Policy..." supervisor-dashboard-policy)
          ("i" "Inspect..." supervisor-dashboard-inspect)]
         ["Navigation"
          ("f" "Stage filter" supervisor-dashboard-cycle-filter)
          ("F" "Tag filter" supervisor-dashboard-cycle-tag-filter)
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
                               ("Restart" 10 t)
                               ("Log" 3 t)
                               ("PID" 7 t)
                               ("Reason" 30 t)])
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

(defun supervisor--timer-row-p (_id)
  "Return non-nil if the dashboard row at point is a timer row.
Checks the Type column of the current `tabulated-list' entry.
_ID is accepted for signature consistency but ignored."
  (when-let* ((entry (tabulated-list-get-entry)))
    (string= "timer" (substring-no-properties (aref entry 1)))))

(defun supervisor--make-dashboard-entry (id type stage enabled-p restart-policy logging-p
                                            &optional snapshot)
  "Create a dashboard entry vector for ID.
TYPE, STAGE, ENABLED-P, RESTART-POLICY, LOGGING-P are parsed entry fields.
If SNAPSHOT is provided, read runtime state from it."
  (let* ((status-pid (supervisor--compute-entry-status id type snapshot))
         (status (car status-pid))
         (pid (cdr status-pid))
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
         (restart-str (if (eq type 'oneshot)
                          "-"
                        (symbol-name effective-restart)))
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
  "Create a separator row for the Timers section."
  (let ((label (propertize "── Timers "
                           'face 'supervisor-stage-separator)))
    (list '--timers--
          (vector label "" "" "" "" "" "" "" ""))))

(defun supervisor--make-timer-dashboard-entry (timer)
  "Create a dashboard entry vector for TIMER."
  (let* ((id (supervisor-timer-id timer))
         (target (supervisor-timer-target timer))
         (enabled (supervisor-timer-enabled timer))
         (state (gethash id supervisor--timer-state))
         (next-run (plist-get state :next-run-at))
         (last-exit (plist-get state :last-exit))
         (miss-reason (plist-get state :last-miss-reason))
         ;; Compute status: pending, active (target running), or based on last result
         (target-active (supervisor-timer--target-active-p timer))
         (status (cond
                  (target-active "active")
                  ((and last-exit (= last-exit 0)) "done")
                  ((and last-exit (/= last-exit 0)) "failed")
                  (t "pending")))
         ;; Reason shows next run time or last miss
         (reason (cond
                  (miss-reason (format "missed: %s" miss-reason))
                  (next-run (format "next: %s"
                                    (supervisor--format-timer-relative-time next-run)))
                  (t "-"))))
    (vector (propertize id 'face 'supervisor-type-timer)
            (propertize "timer" 'face 'supervisor-type-timer)
            (propertize target 'face 'supervisor-type-oneshot)
            (propertize (if enabled "yes" "no")
                        'face (if enabled
                                  'supervisor-enabled-yes
                                'supervisor-enabled-no))
            (supervisor--propertize-status status)
            "-"  ; restart (N/A for timers)
            "-"  ; logging (N/A for timers)
            "-"  ; PID (N/A for timers)
            (if (string-empty-p reason)
                ""
              (propertize reason 'face 'supervisor-reason)))))

(defun supervisor--get-timer-entries ()
  "Generate timer entries for the dashboard.
Returns list of (id vector) pairs."
  (let ((entries nil))
    (dolist (timer supervisor--timer-list)
      (let ((id (supervisor-timer-id timer)))
        (push (list id (supervisor--make-timer-dashboard-entry timer)) entries)))
    ;; Also show invalid timers
    (maphash (lambda (id reason)
               (push (list id
                           (vector (propertize id 'face 'supervisor-status-invalid)
                                   (propertize "timer" 'face 'supervisor-type-timer)
                                   "-" "-"
                                   (supervisor--propertize-status "invalid")
                                   "-" "-" "-"
                                   (propertize reason 'face 'supervisor-reason)))
                     entries))
             supervisor--invalid-timers)
    (nreverse entries)))

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

(defun supervisor--get-entries (&optional snapshot programs)
  "Generate entries for the dashboard (deduplicates on the fly).
Respects `supervisor--dashboard-stage-filter' and tag filter when set.
When `supervisor-dashboard-group-by-stage' is non-nil and no stage filter
is active, entries are grouped by stage with separator rows.
If SNAPSHOT is provided, read runtime state from it.
If PROGRAMS is provided, use it instead of calling
`supervisor--effective-programs'."
  (let* ((snapshot (or snapshot (supervisor--build-snapshot)))
         (programs (or programs (supervisor--effective-programs)))
         (entries nil)
         (seen (make-hash-table :test 'equal))
         (stage-filter supervisor--dashboard-stage-filter)
         (tag-filter supervisor--dashboard-tag-filter)
         (invalid-hash (supervisor-snapshot-invalid snapshot))
         (idx 0))
    (dolist (entry programs)
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
            (pcase-let ((`(,id ,_cmd ,_delay ,enabled-p ,restart-policy ,logging-p
                               ,type ,stage ,_after ,_owait ,_otimeout ,tags)
                         (supervisor--parse-entry entry)))
              (when (and (or (null stage-filter) (eq stage stage-filter))
                         (or (null tag-filter) (member tag-filter tags)))
                (push (list id
                            (supervisor--make-dashboard-entry
                             id type stage enabled-p restart-policy logging-p snapshot)
                            stage)
                      entries)))))))
    ;; Include invalid authority units not already seen via programs
    (when (and (boundp 'supervisor--unit-file-invalid)
               (not stage-filter))
      (maphash (lambda (id reason)
                 (unless (gethash id seen)
                   (puthash id t seen)
                   (push (list id
                               (vector id "-" "-" "-"
                                       (supervisor--propertize-status "invalid")
                                       "-" "-" "-"
                                       reason)
                               nil)
                         entries)))
               (symbol-value 'supervisor--unit-file-invalid)))
    (setq entries (nreverse entries))
    (let ((final-entries
           (if (and supervisor-dashboard-group-by-stage (null stage-filter))
               (supervisor--group-entries-by-stage entries)
             (mapcar (lambda (e) (list (car e) (cadr e))) entries))))
      ;; Append timers section if enabled, subsystem active, and no stage filter
      (when (and supervisor-dashboard-show-timers
                 (supervisor-timer-subsystem-active-p)
                 (null stage-filter)
                 (or supervisor--timer-list
                     (> (hash-table-count supervisor--invalid-timers) 0)))
        (setq final-entries
              (append final-entries
                      (list (supervisor--make-timer-separator))
                      (supervisor--get-timer-entries))))
      final-entries)))

(defun supervisor--health-summary (&optional snapshot programs)
  "Return compact health summary string.
If SNAPSHOT is provided, read state from it; otherwise read from globals.
If PROGRAMS is provided, use it instead of calling
`supervisor--effective-programs'."
  (let ((running 0) (active 0) (done 0) (failed 0) (invalid 0) (pending 0)
        (programs (or programs (supervisor--effective-programs)))
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
    (concat (propertize (format "%d" running) 'face 'supervisor-status-running)
            " run | "
            (when (> active 0)
              (concat (propertize (format "%d" active) 'face 'supervisor-status-running)
                      " act | "))
            (propertize (format "%d" done) 'face 'supervisor-status-done)
            " done | "
            (propertize (format "%d" pending) 'face 'supervisor-status-pending)
            " pend | "
            (propertize (format "%d" failed) 'face 'supervisor-status-failed)
            " fail | "
            (propertize (format "%d" invalid) 'face 'supervisor-status-invalid)
            " inv")))

(defvar supervisor--help-text
  (concat "[f]ilter [g]refresh [G]live [t]proced [T]auto "
          "[l]ifecycle [p]olicy [i]nspect [?]menu [h]elp [q]uit")
  "Key hints displayed in dashboard header.")

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
              (concat (supervisor--health-summary snapshot programs)
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

(defvar supervisor--status-legend
  "Status: running=active process | active=oneshot remain-after-exit | done=oneshot completed ok | failed=crashed/exit>0 | dead=crash-loop | pending=not yet started | stopped=terminated | invalid=config error"
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
    (let ((id (tabulated-list-get-id)))
      (cond
       ((null id)
        (message "%s" supervisor--status-legend))
       ((supervisor--separator-row-p id)
        (message "Stage separator row"))
       (t
        (let ((invalid-reason (gethash id supervisor--invalid)))
          (if invalid-reason
              (message "INVALID: %s" invalid-reason)
            (let ((entry (supervisor--get-entry-for-id id)))
              (if (not entry)
                  (message "Entry not found: %s" id)
                (supervisor--describe-entry-detail id entry))))))))))

(defun supervisor--describe-entry-detail (id entry)
  "Show detailed telemetry for ID with parsed ENTRY in a help window."
  (let* ((type (supervisor-entry-type entry))
         (stage (supervisor-entry-stage entry))
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
         (unit-file (when (fboundp 'supervisor--unit-file-path)
                      (supervisor--unit-file-path id)))
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
        (princ (format "      PID: %d\n" pid)))
      (when uptime
        (princ (format "   Uptime: %s\n"
                       (supervisor--describe-format-duration uptime))))
      (princ "\n")
      ;; Config block
      (princ (format "     Type: %s\n" type))
      (princ (format "    Stage: %s\n" stage))
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
      ;; Oneshot-specific
      (when (eq type 'oneshot)
        (princ (format " Blocking: %s\n"
                       (if (supervisor-entry-oneshot-blocking entry)
                           "yes" "no")))
        (let ((timeout (supervisor-entry-oneshot-timeout entry)))
          (when timeout
            (princ (format "  Timeout: %ss\n" timeout)))))
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
            (when parts
              (princ (format "  Metrics: %s\n"
                             (mapconcat #'identity
                                        (nreverse parts) " ")))))))
      ;; Unit file and docs
      (when (or unit-file documentation)
        (princ "\n")
        (when unit-file
          (princ (format "Unit file: %s\n" unit-file)))
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
    (princ "  f     Cycle stage filter (all -> stage1 -> stage2 -> ...)\n")
    (princ "  F     Cycle tag filter\n")
    (princ "  g     Refresh dashboard\n")
    (princ "  G     Toggle auto-refresh (live monitoring)\n")
    (princ "  t     Open proced (system process list)\n")
    (princ "  T     Toggle proced auto-update mode\n")
    (princ "  l     Lifecycle submenu\n")
    (princ "  p     Policy submenu\n")
    (princ "  i     Inspect submenu\n")
    (princ "  ?     Open action menu (transient)\n")
    (princ "  h     Show this help\n")
    (princ "  q     Quit dashboard\n\n")
    (princ "LIFECYCLE (l)\n")
    (princ "-------------\n")
    (princ "  s     Start process\n")
    (princ "  t     Stop process (graceful, suppresses restart)\n")
    (princ "  r     Restart process (stop + start)\n")
    (princ "  k     Kill process (send signal, restart policy unchanged)\n")
    (princ "  u     Reload unit (re-read config and restart)\n")
    (princ "  f     Reset failed state\n\n")
    (princ "POLICY (p)\n")
    (princ "----------\n")
    (princ "  e     Enable entry\n")
    (princ "  d     Disable entry\n")
    (princ "  m     Mask entry (always disabled)\n")
    (princ "  u     Unmask entry\n")
    (princ "  r     Set restart policy (selection)\n")
    (princ "  l     Set logging (selection)\n\n")
    (princ "INSPECT (i)\n")
    (princ "-----------\n")
    (princ "  i     Show entry details (C-u for status legend)\n")
    (princ "  d     Show dependencies for entry\n")
    (princ "  g     Show dependency graph\n")
    (princ "  b     Blame: startup timing sorted by duration\n")
    (princ "  l     View log file\n")
    (princ "  c     View unit file (read-only)\n")
    (princ "  e     Edit unit file (create scaffold if missing)\n\n")
    (princ "STATUS VALUES\n")
    (princ "-------------\n")
    (princ "  running   Process is alive and running\n")
    (princ "  active    Oneshot with remain-after-exit (exited successfully)\n")
    (princ "  done      Oneshot completed successfully (exit 0)\n")
    (princ "  failed    Process crashed or oneshot exited non-zero\n")
    (princ "  dead      Process crash-looped (exceeded restart limit)\n")
    (princ "  pending   Not yet started (waiting for stage/deps)\n")
    (princ "  stopped   Process terminated or active oneshot explicitly stopped\n")
    (princ "  masked    Entry is masked (always disabled)\n")
    (princ "  invalid   Config entry has errors\n\n")
    (princ "COLUMN MEANINGS\n")
    (princ "---------------\n")
    (princ "  ID        Process identifier (from :id or command)\n")
    (princ "  Type      simple (daemon) or oneshot (run-once)\n")
    (princ "  Stage     Startup stage (stage1-4, lower runs first)\n")
    (princ "  Enabled   Whether process will start (yes/no)\n")
    (princ "  Status    Current state (see above)\n")
    (princ "  Restart   Restart policy: no, on-success, on-failure, always (simple only)\n")
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

(defun supervisor-dashboard-stop ()
  "Stop process at point with confirmation.
Gracefully stops the process and suppresses auto-restart.
Rejects separator rows, timer rows, and non-active oneshot entries.
Oneshot entries with `:remain-after-exit' in active state can be stopped.
Use `s' to start the process again later."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (when (supervisor--separator-row-p id)
      (user-error "Cannot stop separator row"))
    (when (supervisor--timer-row-p id)
      (user-error "Cannot stop timer '%s'" id))
    (let ((entry (supervisor--get-entry-for-id id)))
      (when (and entry (eq (supervisor-entry-type entry) 'oneshot)
                 (not (gethash id supervisor--remain-active)))
        (user-error "Cannot stop oneshot entry '%s'" id)))
    (when (yes-or-no-p (format "Stop process '%s'? " id))
      (let ((result (supervisor--manual-stop id)))
        (pcase (plist-get result :status)
          ('stopped (supervisor--refresh-dashboard))
          ('skipped (message "Entry %s is %s" id (plist-get result :reason))))))))

(defun supervisor-dashboard-restart ()
  "Restart process at point with confirmation.
Stop the process gracefully, then start it again.
If the entry is not running, this is equivalent to start."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (when (supervisor--separator-row-p id)
      (user-error "Cannot restart separator row"))
    (when (supervisor--timer-row-p id)
      (user-error "Cannot restart timer '%s'" id))
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
  (when-let* ((id (tabulated-list-get-id)))
    (when (supervisor--separator-row-p id)
      (user-error "Cannot kill separator row"))
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
  (when-let* ((id (tabulated-list-get-id)))
    (when (supervisor--separator-row-p id)
      (user-error "Cannot reset failed on separator row"))
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
  (when-let* ((id (tabulated-list-get-id)))
    (when (supervisor--separator-row-p id)
      (user-error "Cannot start separator row"))
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
  (when-let* ((id (tabulated-list-get-id)))
    (when (supervisor--separator-row-p id)
      (user-error "Cannot enable separator row"))
    (let ((result (supervisor--policy-enable id)))
      (message "%s" (plist-get result :message))
      (when (eq (plist-get result :status) 'applied)
        (supervisor--save-overrides)
        (supervisor--refresh-dashboard)))))

(defun supervisor-dashboard-disable ()
  "Disable the entry at point.
Set a disabled override if the config default is enabled."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (when (supervisor--separator-row-p id)
      (user-error "Cannot disable separator row"))
    (let ((result (supervisor--policy-disable id)))
      (message "%s" (plist-get result :message))
      (when (eq (plist-get result :status) 'applied)
        (supervisor--save-overrides)
        (supervisor--refresh-dashboard)))))

(defun supervisor-dashboard-mask ()
  "Mask the entry at point.
Masked entries are always disabled regardless of enabled overrides."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (when (supervisor--separator-row-p id)
      (user-error "Cannot mask separator row"))
    (when (supervisor--timer-row-p id)
      (user-error "Cannot mask timer '%s'" id))
    (let ((result (supervisor--policy-mask id)))
      (message "%s" (plist-get result :message))
      (when (eq (plist-get result :status) 'applied)
        (supervisor--save-overrides)
        (supervisor--refresh-dashboard)))))

(defun supervisor-dashboard-unmask ()
  "Unmask the entry at point.
Remove the mask override so the enabled state takes effect again."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (when (supervisor--separator-row-p id)
      (user-error "Cannot unmask separator row"))
    (when (supervisor--timer-row-p id)
      (user-error "Cannot unmask timer '%s'" id))
    (let ((result (supervisor--policy-unmask id)))
      (message "%s" (plist-get result :message))
      (when (eq (plist-get result :status) 'applied)
        (supervisor--save-overrides)
        (supervisor--refresh-dashboard)))))

(defun supervisor-dashboard-set-restart-policy ()
  "Set restart policy for entry at point via selection.
Prompt with `completing-read' to choose the target policy explicitly."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (when (supervisor--separator-row-p id)
      (user-error "Cannot set restart on separator row"))
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
  (when-let* ((id (tabulated-list-get-id)))
    (when (supervisor--separator-row-p id)
      (user-error "Cannot set logging on separator row"))
    (let* ((choice (completing-read "Logging: " '("on" "off") nil t))
           (enabled-p (string= choice "on"))
           (result (supervisor--policy-set-logging id enabled-p)))
      (message "%s" (plist-get result :message))
      (supervisor--save-overrides)
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

(defun supervisor-dashboard-cat ()
  "View unit file for entry at point in read-only mode.
Opens the unit file with `view-mode' so `q' returns to the dashboard."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (when (supervisor--separator-row-p id)
      (user-error "No unit file for separator row"))
    (when (supervisor--timer-row-p id)
      (user-error "No unit file for timer row"))
    (let ((path (supervisor--unit-file-path id)))
      (cond
       ((not path)
        (user-error "No active authority roots configured"))
       ((file-exists-p path)
        (view-file path))
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
If the unit file does not exist, create a scaffold template.
On save, validate the unit file and report errors.
Press `q' or \\[supervisor-edit-finish] to return to the dashboard."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (when (supervisor--separator-row-p id)
      (user-error "Cannot edit separator row"))
    (when (supervisor--timer-row-p id)
      (user-error "Cannot edit timer row"))
    (let* ((path (supervisor--unit-file-path id))
           (root (or (when (fboundp 'supervisor--authority-root-for-id)
                       (supervisor--authority-root-for-id id))
                     (when path (file-name-directory path))))
           (tier (when (fboundp 'supervisor--authority-tier-for-id)
                   (supervisor--authority-tier-for-id id))))
      (unless path
        (user-error "No active authority roots configured"))
      (let ((created (not (file-exists-p path))))
        ;; Create scaffold if file doesn't exist
        (when created
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
  (when-let* ((id (tabulated-list-get-id)))
    (when (supervisor--separator-row-p id)
      (user-error "Cannot reload separator row"))
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
            (dolist (entry (supervisor--effective-programs))
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



(provide 'supervisor-dashboard)

;;; supervisor-dashboard.el ends here
