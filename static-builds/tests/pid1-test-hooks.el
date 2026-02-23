;;; pid1-test-hooks.el --- PID1 boot-hook test helper  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Writes marker files to PID1_TEST_MARKER_DIR when hooks fire.
;; Intended for use with --fg-daemon inside a PID namespace.
;;
;; Markers written:
;;   startup-hook   -- emacs-startup-hook fired
;;   pid1-boot-hook -- pid1-boot-hook fired (only in --pid1 mode)
;;   ready          -- all hooks complete, safe to inspect
;;
;; After writing markers, calls (kill-emacs 0) to exit cleanly.

;;; Code:

(defvar pid1-test-marker-dir (getenv "PID1_TEST_MARKER_DIR")
  "Directory for writing test marker files.")

(defun pid1-test--write-marker (name)
  "Write marker file NAME to `pid1-test-marker-dir'."
  (when pid1-test-marker-dir
    (let ((file (expand-file-name name pid1-test-marker-dir)))
      (with-temp-file file
        (insert name "\n")))))

;; Always fires on startup.
(add-hook 'emacs-startup-hook
          (lambda ()
            (pid1-test--write-marker "startup-hook")))

;; Fires only in --pid1 mode.
(when (boundp 'pid1-boot-hook)
  (add-hook 'pid1-boot-hook
            (lambda ()
              (pid1-test--write-marker "pid1-boot-hook"))))

;; After all hooks, write ready marker and exit.
;; Use a timer to ensure this runs after pid1-boot-hook (which fires
;; after emacs-startup-hook in startup.el).
(run-with-timer
 0.5 nil
 (lambda ()
   (pid1-test--write-marker "ready")
   (kill-emacs 0)))

;;; pid1-test-hooks.el ends here
