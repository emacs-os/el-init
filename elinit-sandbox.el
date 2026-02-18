;;; elinit-sandbox.el --- Sandbox profile subsystem for elinit.el -*- lexical-binding: t -*-

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

;; Sandbox profile subsystem for elinit.el: bwrap profile
;; argument construction and launch argv wrapping.
;; Run M-x elinit-handbook for full documentation.

;;; Code:

(require 'cl-lib)

;; Forward declarations for entry accessor functions in elinit-core.el
(declare-function elinit-entry-sandbox-profile "elinit-core" (entry))
(declare-function elinit-entry-sandbox-network "elinit-core" (entry))
(declare-function elinit-entry-sandbox-ro-bind "elinit-core" (entry))
(declare-function elinit-entry-sandbox-rw-bind "elinit-core" (entry))
(declare-function elinit-entry-sandbox-tmpfs "elinit-core" (entry))
(declare-function elinit-entry-sandbox-raw-args "elinit-core" (entry))


;;; Sandbox Profile Registry

(defun elinit--sandbox-profile-args (profile)
  "Return the bwrap argument list for sandbox PROFILE.
PROFILE is a symbol: `none', `strict', `service', or `desktop'.
Returns nil for `none' (no sandbox).  The returned list does not
include the `bwrap' executable itself -- callers prepend that."
  (pcase profile
    ('none nil)
    ('strict
     (list "--unshare-all"
           "--die-with-parent"
           "--ro-bind" "/" "/"
           "--tmpfs" "/tmp"
           "--proc" "/proc"
           "--dev" "/dev"))
    ('service
     (list "--unshare-pid"
           "--unshare-ipc"
           "--unshare-uts"
           "--die-with-parent"
           "--ro-bind" "/" "/"
           "--tmpfs" "/tmp"
           "--proc" "/proc"
           "--dev" "/dev"
           "--dev-bind" "/dev/null" "/dev/null"
           "--dev-bind" "/dev/urandom" "/dev/urandom"))
    ('desktop
     (list "--unshare-pid"
           "--unshare-ipc"
           "--unshare-uts"
           "--die-with-parent"
           "--ro-bind" "/" "/"
           "--tmpfs" "/tmp"
           "--proc" "/proc"
           "--dev" "/dev"
           "--dev-bind" "/dev/null" "/dev/null"
           "--dev-bind" "/dev/urandom" "/dev/urandom"
           "--bind" (or (getenv "XDG_RUNTIME_DIR")
                        (format "/run/user/%d" (user-uid)))
           (or (getenv "XDG_RUNTIME_DIR")
               (format "/run/user/%d" (user-uid)))
           "--ro-bind" "/tmp/.X11-unix" "/tmp/.X11-unix"))
    (_ nil)))

(defun elinit--sandbox-profile-default-network (profile)
  "Return the default network mode symbol for PROFILE.
`strict' defaults to `isolated'; others default to `shared'."
  (if (eq profile 'strict) 'isolated 'shared))

(defun elinit--sandbox-build-argv (entry)
  "Build the bwrap wrapper argv for a sandbox-requesting ENTRY.
Return a list of strings starting with the bwrap executable path,
or nil if the entry does not request sandbox.  The returned argv
does not include the service command itself -- callers append that."
  (let ((profile (or (elinit-entry-sandbox-profile entry) 'none)))
    (when (or (not (eq profile 'none))
              (elinit-entry-sandbox-network entry)
              (elinit-entry-sandbox-ro-bind entry)
              (elinit-entry-sandbox-rw-bind entry)
              (elinit-entry-sandbox-tmpfs entry)
              (elinit-entry-sandbox-raw-args entry))
      (let* ((bwrap (or (executable-find "bwrap") "bwrap"))
             (base-args (or (elinit--sandbox-profile-args profile)
                            ;; Minimal base when profile is none but knobs set
                            (list "--die-with-parent"
                                  "--ro-bind" "/" "/"
                                  "--proc" "/proc"
                                  "--dev" "/dev")))
             (effective-network
              (or (elinit-entry-sandbox-network entry)
                  (elinit--sandbox-profile-default-network profile)))
             (argv (list bwrap)))
        ;; Append profile base args
        (setq argv (append argv base-args))
        ;; Apply network override.  When profile uses --unshare-all
        ;; (which includes network), --share-net is needed to restore
        ;; shared networking.
        (cond
         ((eq effective-network 'isolated)
          (unless (member "--unshare-net" argv)
            (setq argv (append argv (list "--unshare-net")))))
         ((and (eq effective-network 'shared)
               (member "--unshare-all" argv))
          (setq argv (append argv (list "--share-net")))))
        ;; Apply read-only bind overrides
        (dolist (path (elinit-entry-sandbox-ro-bind entry))
          (setq argv (append argv (list "--ro-bind" path path))))
        ;; Apply read-write bind overrides
        (dolist (path (elinit-entry-sandbox-rw-bind entry))
          (setq argv (append argv (list "--bind" path path))))
        ;; Apply tmpfs overrides
        (dolist (path (elinit-entry-sandbox-tmpfs entry))
          (setq argv (append argv (list "--tmpfs" path))))
        ;; Apply raw args (expert mode, already validated)
        (when (elinit-entry-sandbox-raw-args entry)
          (setq argv (append argv (elinit-entry-sandbox-raw-args entry))))
        ;; Separator before service command
        (append argv (list "--"))))))

(provide 'elinit-sandbox)

;;; elinit-sandbox.el ends here
