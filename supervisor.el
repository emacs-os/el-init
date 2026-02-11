;;; supervisor.el --- Emacs Lisp process supervisor -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>
;;
;; Author: telecommuter <telecommuter@riseup.net>
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: processes, unix
;; URL: https://github.com/cypherpunk2001/supervisor.el

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

;; See README.org for full documentation.
;;
;; Module structure:
;; - supervisor-core.el: engine, parsing, scheduling, process lifecycle, state
;; - supervisor-dashboard.el: UI rendering, keymaps, interactive commands
;; - supervisor-cli.el: CLI dispatcher, formatters, command handlers
;; - supervisor.el: facade that loads all modules (this file)
;;
;; Load order: core -> dashboard -> cli -> facade.
;; No circular require chains exist between modules.

;;; Code:

(require 'supervisor-core)
(require 'supervisor-dashboard)
(require 'supervisor-cli)

(provide 'supervisor)

;;; supervisor.el ends here
