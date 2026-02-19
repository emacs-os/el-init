;;; elinit-pid1.el --- PID1 integration for elinit.el -*- lexical-binding: t -*-

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

;; PID1 integration for elinit.el.
;; Run M-x elinit-handbook for full documentation.

;;; Code:

;; Forward declarations for elinit-core functions we depend on
(declare-function elinit--log "elinit-core" (level format-string &rest args))
(declare-function elinit-start "elinit-core" ())
(declare-function elinit-stop-now "elinit-core" ())

;;;; Customization group

(defgroup elinit-pid1 nil
  "PID1 integration for elinit."
  :group 'elinit)

;;;; Auto-detection

(defun elinit--pid1-detect-mode ()
  "Return non-nil when running as PID 1 with `pid1-mode' enabled.
Checks both `pid1-mode' (set by the --pid1 flag) and that the
current process is actually PID 1."
  (and (bound-and-true-p pid1-mode)
       (= (emacs-pid) 1)))

;;;; User options

(defcustom elinit-pid1-mode-enabled (elinit--pid1-detect-mode)
  "Non-nil means PID1 boot/shutdown actions are active.
When non-nil, `elinit--pid1-boot' and `elinit--pid1-shutdown'
load rc scripts and call `elinit-start'/`elinit-stop-now'.
When nil, those functions are no-ops.  Hook registration is
separately gated by `pid1-mode' at module load time; setting
this variable to nil does not remove hooks.  Default is
auto-detected from `pid1-mode' and the process PID."
  :type 'boolean
  :group 'elinit-pid1)

(defcustom elinit-pid1-boot-script "/lib/init/rc.boot.el"
  "File name of the boot script loaded during PID1 startup."
  :type 'string
  :group 'elinit-pid1)

(defcustom elinit-pid1-shutdown-script "/lib/init/rc.shutdown.el"
  "File name of the shutdown script loaded during PID1 shutdown."
  :type 'string
  :group 'elinit-pid1)

(defcustom elinit-pid1-boot-policy 'if-present
  "Policy for loading the boot script.
`never' skips the script unconditionally.  `if-present' loads
the script when it is a readable regular file, silently skipping
otherwise.
`require' loads the script and signals an error if missing
or unreadable."
  :type '(choice (const :tag "Never load" never)
                 (const :tag "Load if present" if-present)
                 (const :tag "Require (error if missing)" require))
  :group 'elinit-pid1)

(defcustom elinit-pid1-shutdown-policy 'if-present
  "Policy for loading the shutdown script.
`never' skips the script unconditionally.  `if-present' loads
the script when it is a readable regular file, silently skipping
otherwise.
`require' loads the script and signals an error if missing
or unreadable."
  :type '(choice (const :tag "Never load" never)
                 (const :tag "Load if present" if-present)
                 (const :tag "Require (error if missing)" require))
  :group 'elinit-pid1)

;;;; Script loading

(defun elinit--pid1-load-script (script-file-name policy)
  "Load SCRIPT-FILE-NAME according to POLICY.
POLICY is one of `never', `if-present', or `require'.
With `never', the script is unconditionally skipped.
With `if-present', the script is loaded when it is a readable
regular file, silently skipped otherwise (including directories
and non-existent paths).  With `require', the script is loaded
and an error is signaled if the file is missing or unreadable."
  (pcase policy
    ('never nil)
    ('if-present
     (when (and (file-regular-p script-file-name)
                (file-readable-p script-file-name))
       (elinit--log 'info "Loading PID1 script: %s" script-file-name)
       (load script-file-name nil t)))
    ('require
     (elinit--log 'info "Loading PID1 script (required): %s"
                  script-file-name)
     (load script-file-name nil nil))
    (_
     (error "elinit-pid1: Unknown script policy `%S'" policy))))

;;;; Boot and shutdown functions

(defun elinit--pid1-boot ()
  "Initialize elinit PID1 mode.
Load the boot script per `elinit-pid1-boot-policy', then start
service management via `elinit-start'."
  (when elinit-pid1-mode-enabled
    (elinit--pid1-load-script elinit-pid1-boot-script
                              elinit-pid1-boot-policy)
    (elinit-start)))

(defun elinit--pid1-shutdown ()
  "Shut down elinit PID1 mode.
Load the shutdown script per `elinit-pid1-shutdown-policy', then
stop all managed services via `elinit-stop-now'."
  (when elinit-pid1-mode-enabled
    (elinit--pid1-load-script elinit-pid1-shutdown-script
                              elinit-pid1-shutdown-policy)
    (elinit-stop-now)))

;;;; Hook registration

(defun elinit--pid1-register-hooks ()
  "Register elinit functions on PID1 hooks when `pid1-mode' is active.
Add `elinit--pid1-boot' to `pid1-boot-hook' and
`elinit--pid1-shutdown' to both `pid1-poweroff-hook' and
`pid1-reboot-hook'.  Called at load time when `pid1-mode' is
non-nil."
  (add-hook 'pid1-boot-hook #'elinit--pid1-boot)
  (add-hook 'pid1-poweroff-hook #'elinit--pid1-shutdown)
  (add-hook 'pid1-reboot-hook #'elinit--pid1-shutdown))

;; Register on PID1 hooks when pid1-mode is active at load time.
(when (bound-and-true-p pid1-mode)
  (elinit--pid1-register-hooks))

(provide 'elinit-pid1)

;;; elinit-pid1.el ends here
