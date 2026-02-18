;;; supervisor-overrides.el --- Overrides persistence for supervisor.el -*- lexical-binding: t -*-

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

;; Overrides persistence for supervisor.el: file path resolution,
;; load/migration/save, effective state getters, and policy mutator
;; functions.
;; Run M-x supervisor-handbook for full documentation.

;;; Code:

(require 'cl-lib)

;; External variables defined in supervisor-core.el
(defvar supervisor--enabled-override)
(defvar supervisor--restart-override)
(defvar supervisor--mask-override)
(defvar supervisor--logging)
(defvar supervisor--invalid)
(defvar supervisor--restart-timers)
(defvar supervisor--valid-restart-policies)

;; Forward declarations for core functions
(declare-function supervisor--log "supervisor-core"
                  (level format-string &rest args))
(declare-function supervisor--maybe-refresh-dashboard "supervisor-core" ())
(declare-function supervisor--get-entry-for-id "supervisor-core" (id))
(declare-function supervisor--normalize-restart-policy "supervisor-core"
                  (value))
(declare-function supervisor-entry-enabled-p "supervisor-core" (entry))
(declare-function supervisor-entry-type "supervisor-core" (entry))
(declare-function supervisor-entry-restart-policy "supervisor-core" (entry))
(declare-function supervisor-entry-logging-p "supervisor-core" (entry))


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


;;; Effective State Getters

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

(provide 'supervisor-overrides)

;;; supervisor-overrides.el ends here
