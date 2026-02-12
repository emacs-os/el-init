;;; supervisor-units.el --- Unit-file support for supervisor.el -*- lexical-binding: t -*-

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

;; Modular unit-file support for supervisor.el.
;; Each unit is a single `.el' file containing a plist expression.
;; Run M-x supervisor-handbook for full documentation.

;;; Code:

(require 'cl-lib)

;; Forward declarations for supervisor-core functions
(declare-function supervisor--log "supervisor-core" (level format-string &rest args))
(declare-function supervisor--extract-id "supervisor-core" (entry idx))

(defvar supervisor-programs)

;;; Customization

(defcustom supervisor-unit-directory
  (expand-file-name "supervisor/units/"
                    (or (getenv "XDG_CONFIG_HOME")
                        (expand-file-name "~/.config")))
  "Directory containing unit files.
Each unit is a single `.el' file with a plist expression."
  :type 'directory
  :group 'supervisor)

(defcustom supervisor-use-unit-files nil
  "When non-nil, load unit files and merge with `supervisor-programs'.
Unit-file entries take precedence on ID collision."
  :type 'boolean
  :group 'supervisor)

;;; Valid unit-file keywords

(defconst supervisor--unit-file-keywords
  '(:id :command :type :stage :delay :after :requires :enabled :disabled
    :restart :no-restart :logging :oneshot-wait :async :oneshot-timeout :tags)
  "Valid keywords in a unit-file plist.
Includes `:command' which is unit-file specific.")

;;; Unit-File Path Resolution

(defun supervisor--unit-file-path (id)
  "Return the path to the unit file for ID.
Resolves to `supervisor-unit-directory'/ID.el."
  (expand-file-name (concat id ".el") supervisor-unit-directory))

;;; Invalid unit-file tracking

(defvar supervisor--unit-file-invalid (make-hash-table :test 'equal)
  "Hash table mapping unit-file ID (or filename) to reason string.
Populated during `supervisor--load-all-unit-files' for entries that
fail loading or validation.  Merged into the plan's invalid hash
so they are visible in the dashboard and CLI.")

;;; Unit-File Loading

(defun supervisor--load-unit-file (path)
  "Read and parse a single unit file at PATH.
The file must contain exactly one plist expression.
Returns (LINE . PLIST) where LINE is the line number of the plist,
or signals an error with file:line context."
  (condition-case _
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (skip-chars-forward " \t\n\r")
        (let ((line (line-number-at-pos (point))))
          (condition-case read-err
              (let ((form (read (current-buffer))))
                ;; Validate it is a plist (list starting with keyword)
                (unless (and (listp form) (keywordp (car form)))
                  (error "%s:%d: file must contain a plist starting with a keyword"
                         path line))
                ;; Check for trailing content
                (skip-chars-forward " \t\n\r")
                (unless (eobp)
                  (error "%s:%d: file contains extra content after plist"
                         path (line-number-at-pos (point))))
                (cons line form))
            (end-of-file
             (error "%s:%d: unit file is empty or has incomplete expression"
                    path (line-number-at-pos (point))))
            (invalid-read-syntax
             (error "%s:%d: invalid syntax: %s"
                    path (line-number-at-pos (point))
                    (error-message-string read-err))))))
    (file-missing
     (error "%s:1: unit file not found" path))))

(defun supervisor--validate-unit-file-plist (plist path line)
  "Validate unit-file PLIST loaded from PATH at LINE.
Return nil if valid, or a reason string with file:line context if invalid."
  (cond
   ((not (plist-get plist :id))
    (format "%s:%d: missing :id" path line))
   ((not (stringp (plist-get plist :id)))
    (format "%s:%d: :id must be a string" path line))
   ((string-empty-p (plist-get plist :id))
    (format "%s:%d: :id must be non-empty" path line))
   ((not (plist-get plist :command))
    (format "%s:%d: missing :command" path line))
   ((not (stringp (plist-get plist :command)))
    (format "%s:%d: :command must be a string" path line))
   (t
    ;; Check for unknown keywords
    (let ((keys plist)
          (bad nil))
      (while keys
        (let ((key (car keys)))
          (unless (memq key supervisor--unit-file-keywords)
            (setq bad (format "%s:%d: unknown keyword %s"
                              path line key))))
        (setq keys (cddr keys)))
      bad))))

(defun supervisor--unit-file-to-program-entry (plist)
  "Convert unit-file PLIST to `supervisor-programs' entry format.
Input: (:id \"x\" :command \"cmd\" :type simple ...)
Output: (\"cmd\" :id \"x\" :type simple ...)
The `:command' key is consumed and becomes the car of the entry."
  (let ((cmd (plist-get plist :command))
        (rest nil))
    ;; Copy all keys except :command
    (let ((keys plist))
      (while keys
        (let ((key (car keys))
              (val (cadr keys)))
          (unless (eq key :command)
            (setq rest (append rest (list key val)))))
        (setq keys (cddr keys))))
    (cons cmd rest)))

(defun supervisor--load-all-unit-files ()
  "Scan `supervisor-unit-directory' and load all unit files.
Returns a list of plists in deterministic alphabetical order.
Invalid unit files are recorded in `supervisor--unit-file-invalid'
with file:line context so they appear in the dashboard and CLI.
Missing directory returns nil with a log message."
  (clrhash supervisor--unit-file-invalid)
  (let ((dir supervisor-unit-directory))
    (cond
     ((not (file-directory-p dir))
      (supervisor--log 'info "Unit directory does not exist: %s" dir)
      nil)
     (t
      (let ((files (sort (directory-files dir t "\\.el\\'" t) #'string<))
            (results nil))
        (dolist (path files)
          (condition-case err
              (let* ((line-and-plist (supervisor--load-unit-file path))
                     (line (car line-and-plist))
                     (plist (cdr line-and-plist))
                     (reason (supervisor--validate-unit-file-plist
                              plist path line)))
                (if reason
                    (let ((id (or (plist-get plist :id)
                                  (file-name-sans-extension
                                   (file-name-nondirectory path)))))
                      (supervisor--log 'warning "INVALID unit file %s - %s"
                                       (file-name-nondirectory path) reason)
                      (puthash id reason supervisor--unit-file-invalid))
                  (push plist results)))
            (error
             (let ((id (file-name-sans-extension
                        (file-name-nondirectory path))))
               (supervisor--log 'warning "Failed to load unit file %s: %s"
                                (file-name-nondirectory path)
                                (error-message-string err))
               (puthash id (error-message-string err)
                        supervisor--unit-file-invalid)))))
        (nreverse results))))))

(defun supervisor--load-programs ()
  "Return the unified program list for plan building.
When `supervisor-use-unit-files' is non-nil, loads unit files from
`supervisor-unit-directory' and merges them with `supervisor-programs'.
Unit-file entries take precedence on ID collision.
Invalid unit files are recorded in `supervisor--unit-file-invalid'.

When `supervisor-use-unit-files' is nil, returns `supervisor-programs'
unchanged (legacy path)."
  (if (not supervisor-use-unit-files)
      supervisor-programs
    (let ((unit-plists (supervisor--load-all-unit-files))
          (unit-entries nil)
          (unit-ids (make-hash-table :test 'equal)))
      ;; Convert valid plists to program entries
      (dolist (plist unit-plists)
        (let* ((id (plist-get plist :id))
               (entry (supervisor--unit-file-to-program-entry plist)))
          (if (gethash id unit-ids)
              (supervisor--log 'warning
                               "Duplicate unit-file ID '%s', skipping" id)
            (puthash id t unit-ids)
            (push entry unit-entries))))
      (setq unit-entries (nreverse unit-entries))
      ;; Filter legacy programs: remove entries whose ID collides with unit files
      (let ((legacy-filtered nil)
            (idx 0))
        (dolist (entry supervisor-programs)
          (let ((id (supervisor--extract-id entry idx)))
            (if (gethash id unit-ids)
                (supervisor--log 'info
                                 "Unit file overrides legacy entry '%s'" id)
              (push entry legacy-filtered)))
          (cl-incf idx))
        ;; Unit-file entries first, then surviving legacy entries
        (append unit-entries (nreverse legacy-filtered))))))

;;; Unit-File Template

(defun supervisor--unit-file-scaffold (id)
  "Return a scaffold template string for a new unit file with ID."
  (concat "(:id \"" id "\"\n"
          " :command \"\"  ; command to run\n"
          " ;; :type simple        ; simple (daemon) or oneshot (run-once)\n"
          " ;; :stage stage3       ; stage1-4, lower runs first\n"
          " ;; :restart t          ; auto-restart on crash (simple only)\n"
          " ;; :enabled t          ; start on supervisor-start\n"
          " ;; :delay 0            ; seconds to wait before starting\n"
          " ;; :after (\"dep-id\")   ; start after these IDs (same stage)\n"
          " ;; :requires (\"id\")   ; pull-in + ordering dependency\n"
          " ;; :logging t          ; log output to file\n"
          " ;; :tags (tag1 tag2)   ; tags for filtering\n"
          " )\n"))

;;; Unit-File Validate-on-Save

(defun supervisor--validate-unit-file-buffer ()
  "Validate the unit-file plist in the current buffer.
Reports errors via `message'.  Intended as `after-save-hook'."
  (condition-case err
      (let* ((result (supervisor--load-unit-file (buffer-file-name)))
             (line (car result))
             (plist (cdr result))
             (reason (supervisor--validate-unit-file-plist
                      plist (buffer-file-name) line)))
        (if reason
            (message "Unit file validation: %s" reason)
          (message "Unit file OK: %s" (plist-get plist :id))))
    (error
     (message "Unit file validation error: %s"
              (error-message-string err)))))

(provide 'supervisor-units)

;;; supervisor-units.el ends here
