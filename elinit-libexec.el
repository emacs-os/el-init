;;; elinit-libexec.el --- Libexec build helpers for elinit.el -*- lexical-binding: t -*-

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

;; Libexec build helpers for elinit.el: build target enumeration,
;; stale detection, compiler selection, and build invocation for
;; bundled C helpers (elinit-logd, elinit-runas, elinit-rlimits).
;; Run M-x elinit-handbook for full documentation.

;;; Code:

(require 'cl-lib)

;; External variables defined in elinit-core.el
(defvar elinit-logd-command)
(defvar elinit-libexec-build-on-startup)
(defvar elinit-libexec-compiler-candidates)
(defvar elinit-libexec-cflags)

;; Forward declarations for core functions
(declare-function elinit--log "elinit-core"
                  (level format-string &rest args))


(defvar elinit-runas-command
  (expand-file-name "libexec/elinit-runas"
                    (file-name-directory (or load-file-name
                                             buffer-file-name "")))
  "Path to the elinit-runas privilege-drop helper.
Used when `:user' or `:group' is set on a unit entry.")

(defvar elinit-rlimits-command
  (expand-file-name "libexec/elinit-rlimits"
                    (file-name-directory (or load-file-name
                                             buffer-file-name "")))
  "Path to the elinit-rlimits resource limit helper.
Used when any `:limit-*' key is set on a unit entry.")

(defun elinit--libexec-build-targets ()
  "Return build target specs for bundled libexec helpers.
Each spec is a plist with keys `:name', `:binary-file', and `:source-file'."
  (list (list :name "elinit-logd"
              :binary-file elinit-logd-command
              :source-file (concat elinit-logd-command ".c"))
        (list :name "elinit-runas"
              :binary-file elinit-runas-command
              :source-file (concat elinit-runas-command ".c"))
        (list :name "elinit-rlimits"
              :binary-file elinit-rlimits-command
              :source-file (concat elinit-rlimits-command ".c"))))

(defun elinit--libexec-target-needs-build-p (target)
  "Return non-nil when TARGET needs compilation.
TARGET is a plist from `elinit--libexec-build-targets'."
  (let ((source-file (plist-get target :source-file))
        (binary-file (plist-get target :binary-file)))
    (or (not (file-exists-p binary-file))
        (not (file-executable-p binary-file))
        (and (file-exists-p source-file)
             (file-newer-than-file-p source-file binary-file)))))

(defun elinit--libexec-pending-build-targets ()
  "Return libexec helper targets that are missing, stale, or non-executable."
  (let ((pending nil))
    (dolist (target (elinit--libexec-build-targets))
      (when (elinit--libexec-target-needs-build-p target)
        (push target pending)))
    (nreverse pending)))

(defun elinit--libexec-target-names (targets)
  "Return comma-separated helper names for TARGETS."
  (mapconcat (lambda (target) (plist-get target :name)) targets ", "))

(defun elinit--find-libexec-compiler ()
  "Return the first available C compiler command, or nil."
  (cl-loop for candidate in elinit-libexec-compiler-candidates
           for compiler = (executable-find candidate)
           when compiler return compiler))

(defun elinit--compile-libexec-target (compiler target)
  "Compile TARGET with COMPILER.
COMPILER is a command path.  TARGET is a target plist.
Return nil on success, or an error string on failure."
  (let* ((name (plist-get target :name))
         (source-file (plist-get target :source-file))
         (binary-file (plist-get target :binary-file))
         (args (append elinit-libexec-cflags
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
(defun elinit-build-libexec-helpers (&optional build-all)
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
    (dolist (target (elinit--libexec-build-targets))
      (let ((source-file (plist-get target :source-file))
            (binary-file (plist-get target :binary-file))
            (name (plist-get target :name)))
        (cond
         ((file-exists-p source-file)
          (when (or build-all
                    (elinit--libexec-target-needs-build-p target))
            (push target targets)))
         ((or build-all
              (not (file-exists-p binary-file))
              (not (file-executable-p binary-file)))
          (push (format "%s source file missing: %s" name source-file)
                missing-source)))))
    (setq targets (nreverse targets))
    (let ((compiler (and targets (elinit--find-libexec-compiler))))
      (when (and targets (not compiler))
        (push (format "No C compiler found in `elinit-libexec-compiler-candidates': %S"
                      elinit-libexec-compiler-candidates)
              failed))
      (when compiler
        (dolist (target targets)
          (let ((err (elinit--compile-libexec-target compiler target)))
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
          (message "elinit: libexec helpers are up to date"))
         ((and failed (null missing-source))
          (message "elinit: libexec build failures: %d"
                   (length failed)))
         ((and (null failed) missing-source)
          (message "elinit: helper sources missing: %d"
                   (length missing-source)))
         (t
          (message "elinit: libexec built=%d failed=%d missing-source=%d"
                   built (length failed) (length missing-source)))))
      result)))

(defun elinit--log-libexec-build-result (result)
  "Log warnings for libexec helper build RESULT."
  (dolist (failure (plist-get result :failed))
    (elinit--log 'warning "%s" failure))
  (dolist (missing (plist-get result :missing-source))
    (elinit--log 'warning "%s" missing)))

(defun elinit--maybe-build-libexec-helpers ()
  "Maybe compile libexec helpers during startup.
When `elinit-libexec-build-on-startup' is `prompt', show a
`y-or-n-p' prompt in graphical Emacs sessions.  In batch mode the
prompt is skipped and an info message is logged instead."
  (let ((pending (elinit--libexec-pending-build-targets)))
    (when pending
      (pcase elinit-libexec-build-on-startup
        ('never nil)
        ('automatic
         (elinit--log-libexec-build-result
          (elinit-build-libexec-helpers)))
        ('prompt
         (if (not noninteractive)
             (when (y-or-n-p
                    (format "Elinit requires compiled C helpers (%s).  Build now? "
                            (elinit--libexec-target-names pending)))
               (elinit--log-libexec-build-result
                (elinit-build-libexec-helpers)))
           (elinit--log
            'info
            "libexec helpers need build (%s); run M-x elinit-build-libexec-helpers"
            (elinit--libexec-target-names pending))))
        (_ nil)))))

(provide 'elinit-libexec)

;;; elinit-libexec.el ends here
