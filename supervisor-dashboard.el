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

;; Dashboard UI for supervisor.el.  This module provides:
;; - `supervisor-dashboard-mode' based on `tabulated-list-mode'
;; - Dashboard keymap and interactive commands
;; - Entry rendering and status display
;; - Faces for status, type, and stage visualization
;; - Stage grouping and filtering
;; - Auto-refresh functionality
;; - Transient menu integration (optional)
;;
;; This module requires supervisor-core for state access.
;; It does not depend on supervisor-cli.

;;; Code:

(require 'cl-lib)
(require 'supervisor-core)

;; Forward declaration for transient menu (defined at runtime via eval)
(declare-function supervisor-dashboard-menu "supervisor-dashboard" ())

;; Forward declaration for proced function
(declare-function proced-toggle-auto-update "proced" (&optional arg))

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
         (target-active (supervisor--timer-target-active-p timer))
         (status (cond
                  (target-active "active")
                  ((and last-exit (= last-exit 0)) "done")
                  ((and last-exit (> last-exit 0)) "failed")
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
    (let ((final-entries
           (if (and supervisor-dashboard-group-by-stage (null stage-filter))
               (supervisor--group-entries-by-stage entries)
             (mapcar (lambda (e) (list (car e) (cadr e))) entries))))
      ;; Append timers section if enabled and no stage filter active
      (when (and supervisor-dashboard-show-timers
                 (null stage-filter)
                 (or supervisor--timer-list
                     (> (hash-table-count supervisor--invalid-timers) 0)))
        (setq final-entries
              (append final-entries
                      (list (supervisor--make-timer-separator))
                      (supervisor--get-timer-entries))))
      final-entries)))

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
                   ((and oneshot-p oneshot-exit (/= oneshot-exit 0)) (cl-incf failed))
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
Prevents auto-restart and sends kill signal.
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
        ;; Mark as manually stopped so sentinel doesn't restart it
        ;; This is temporary - cleared when entry is started again
        (puthash id t supervisor--manually-stopped)
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



(provide 'supervisor-dashboard)

;;; supervisor-dashboard.el ends here
