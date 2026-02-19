;;; pid1-test-signals.el --- PID1 signal-hook test helper  -*- lexical-binding: t; -*-

;; Writes marker files to PID1_TEST_MARKER_DIR when PID1 hooks fire
;; in response to signals (SIGTERM, SIGUSR1, SIGUSR2).
;;
;; Markers written:
;;   ready              -- pid1-boot-hook fired, Emacs is ready for signals
;;   pid1-poweroff-hook -- pid1-poweroff-hook fired (SIGTERM or SIGUSR1)
;;   pid1-reboot-hook   -- pid1-reboot-hook fired (SIGINT or SIGUSR2)
;;   kill-emacs-hook    -- kill-emacs-hook fired (always on exit)
;;
;; Does NOT call kill-emacs -- stays running in daemon loop waiting
;; for signals from the test harness.

;;; Code:

(defvar pid1-test-marker-dir (getenv "PID1_TEST_MARKER_DIR")
  "Directory for writing test marker files.")

(defun pid1-test--write-marker (name)
  "Write marker file NAME to `pid1-test-marker-dir'."
  (when pid1-test-marker-dir
    (let ((file (expand-file-name name pid1-test-marker-dir)))
      (with-temp-file file
        (insert name "\n")))))

;; Signal readiness on pid1-boot-hook.
(when (boundp 'pid1-boot-hook)
  (add-hook 'pid1-boot-hook
            (lambda ()
              (pid1-test--write-marker "ready"))))

;; Poweroff hook (SIGTERM, SIGUSR1).
(when (boundp 'pid1-poweroff-hook)
  (add-hook 'pid1-poweroff-hook
            (lambda ()
              (pid1-test--write-marker "pid1-poweroff-hook"))))

;; Reboot hook (SIGINT, SIGUSR2).
(when (boundp 'pid1-reboot-hook)
  (add-hook 'pid1-reboot-hook
            (lambda ()
              (pid1-test--write-marker "pid1-reboot-hook"))))

;; kill-emacs-hook fires on any exit.
(add-hook 'kill-emacs-hook
          (lambda ()
            (pid1-test--write-marker "kill-emacs-hook")))

;;; pid1-test-signals.el ends here
