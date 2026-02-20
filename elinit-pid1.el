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
call `elinit-start'/`elinit-stop-now'.
When nil, those functions are no-ops.  Hook registration is
separately gated by `pid1-mode' at module load time; setting
this variable to nil does not remove hooks.  Default is
auto-detected from `pid1-mode' and the process PID."
  :type 'boolean
  :group 'elinit-pid1)

;;;; Boot and shutdown functions

(defun elinit--pid1-boot ()
  "Initialize elinit PID1 mode.
Start service management via `elinit-start'."
  (when elinit-pid1-mode-enabled
    (elinit-start)))

(defun elinit--pid1-shutdown ()
  "Shut down elinit PID1 mode.
Stop all managed services via `elinit-stop-now'."
  (when elinit-pid1-mode-enabled
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
