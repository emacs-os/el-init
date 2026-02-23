;;; elinit.el --- Emacs Lisp service manager -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026 telecommuter <telecommuter@riseup.net>
;;
;; Author: telecommuter <telecommuter@riseup.net>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: processes, unix
;; URL: https://github.com/emacs-os/el-init

;; This file is not part of GNU Emacs.

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

;; Emacs Lisp service manager.
;; Run M-x elinit-handbook for full documentation.

;;; Code:

(require 'elinit-core)
(require 'elinit-log)
(require 'elinit-overrides)
(require 'elinit-sandbox)
(require 'elinit-libexec)
(require 'elinit-units)
(require 'elinit-timer)
(require 'elinit-pid1)
(require 'elinit-dashboard)
(require 'elinit-cli)

(provide 'elinit)

;;; elinit.el ends here
