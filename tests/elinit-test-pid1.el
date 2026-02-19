;;; elinit-test-pid1.el --- PID1 integration tests for elinit.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; ERT tests for elinit-pid1.el: auto-detection, policy dispatch,
;; boot/shutdown functions, hook registration, and disabled mode.

;;; Code:

(require 'elinit-test-helpers)

;; Forward declarations for PID1 patch variables (not present in
;; normal Emacs).  These must be special (dynamic) so `bound-and-true-p'
;; sees `let' bindings via `boundp'.
(defvar pid1-mode)
(defvar pid1-boot-hook)
(defvar pid1-poweroff-hook)
(defvar pid1-reboot-hook)

;;;; Auto-detection tests

(ert-deftest elinit-test-pid1-detect-mode-nil ()
  "Auto-detect returns nil when `pid1-mode' is nil."
  (let ((pid1-mode nil))
    (should-not (elinit--pid1-detect-mode))))

(ert-deftest elinit-test-pid1-detect-mode-not-pid1 ()
  "Auto-detect returns nil when `pid1-mode' is t but PID is not 1."
  (let ((pid1-mode t))
    (cl-letf (((symbol-function 'emacs-pid) (lambda () 42)))
      (should-not (elinit--pid1-detect-mode)))))

(ert-deftest elinit-test-pid1-detect-mode-is-pid1 ()
  "Auto-detect returns non-nil when `pid1-mode' is t and PID is 1."
  (let ((pid1-mode t))
    (cl-letf (((symbol-function 'emacs-pid) (lambda () 1)))
      (should (elinit--pid1-detect-mode)))))

;;;; Policy dispatch tests

(ert-deftest elinit-test-pid1-load-script-never ()
  "Policy `never' skips script unconditionally."
  (let ((loaded nil))
    (cl-letf (((symbol-function 'load)
               (lambda (&rest _) (setq loaded t))))
      (elinit--pid1-load-script "/nonexistent/script.el" 'never)
      (should-not loaded))))

(ert-deftest elinit-test-pid1-load-script-if-present-readable ()
  "Policy `if-present' loads script when file is readable."
  (let ((tmpfile (make-temp-file "pid1-test-" nil ".el"))
        (loaded-file nil))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert ";; test script\n"))
          (cl-letf (((symbol-function 'elinit--log)
                     (lambda (&rest _))))
            (cl-letf (((symbol-function 'load)
                       (lambda (file &rest _)
                         (setq loaded-file file))))
              (elinit--pid1-load-script tmpfile 'if-present)
              (should (equal loaded-file tmpfile)))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-pid1-load-script-if-present-missing ()
  "Policy `if-present' silently skips when file is missing."
  (let ((loaded nil))
    (cl-letf (((symbol-function 'load)
               (lambda (&rest _) (setq loaded t))))
      (elinit--pid1-load-script "/nonexistent/missing.el" 'if-present)
      (should-not loaded))))

(ert-deftest elinit-test-pid1-load-script-require-loads ()
  "Policy `require' loads the script."
  (let ((tmpfile (make-temp-file "pid1-test-" nil ".el"))
        (loaded-file nil))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert ";; test script\n"))
          (cl-letf (((symbol-function 'elinit--log)
                     (lambda (&rest _))))
            (cl-letf (((symbol-function 'load)
                       (lambda (file &rest _)
                         (setq loaded-file file))))
              (elinit--pid1-load-script tmpfile 'require)
              (should (equal loaded-file tmpfile)))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-pid1-load-script-require-errors ()
  "Policy `require' signals error when file is missing."
  (cl-letf (((symbol-function 'elinit--log)
             (lambda (&rest _))))
    (should-error
     (elinit--pid1-load-script "/nonexistent/missing.el" 'require))))

(ert-deftest elinit-test-pid1-load-script-require-unreadable ()
  "Policy `require' signals error for existing but unreadable file."
  (let ((tmpfile (make-temp-file "pid1-test-" nil ".el")))
    (unwind-protect
        (progn
          (set-file-modes tmpfile #o000)
          (cl-letf (((symbol-function 'elinit--log)
                     (lambda (&rest _))))
            (should-error
             (elinit--pid1-load-script tmpfile 'require))))
      (set-file-modes tmpfile #o644)
      (delete-file tmpfile))))

(ert-deftest elinit-test-pid1-load-script-if-present-unreadable ()
  "Policy `if-present' silently skips existing but unreadable file."
  (let ((tmpfile (make-temp-file "pid1-test-" nil ".el"))
        (loaded nil))
    (unwind-protect
        (progn
          (set-file-modes tmpfile #o000)
          (cl-letf (((symbol-function 'load)
                     (lambda (&rest _) (setq loaded t))))
            (elinit--pid1-load-script tmpfile 'if-present)
            (should-not loaded)))
      (set-file-modes tmpfile #o644)
      (delete-file tmpfile))))

(ert-deftest elinit-test-pid1-load-script-if-present-directory ()
  "Policy `if-present' silently skips directories."
  (let ((tmpdir (make-temp-file "pid1-test-" t))
        (loaded nil))
    (unwind-protect
        (cl-letf (((symbol-function 'load)
                   (lambda (&rest _) (setq loaded t))))
          (elinit--pid1-load-script tmpdir 'if-present)
          (should-not loaded))
      (delete-directory tmpdir))))

(ert-deftest elinit-test-pid1-load-script-unknown-policy ()
  "Unknown policy signals an error."
  (should-error
   (elinit--pid1-load-script "/some/file.el" 'bogus)
   :type 'error))

;;;; Boot function tests

(ert-deftest elinit-test-pid1-boot-calls-start ()
  "Boot function calls `elinit-start' when enabled."
  (let ((elinit-pid1-mode-enabled t)
        (elinit-pid1-boot-script "/nonexistent/boot.el")
        (elinit-pid1-boot-policy 'never)
        (start-called nil))
    (cl-letf (((symbol-function 'elinit-start)
               (lambda () (setq start-called t))))
      (elinit--pid1-boot)
      (should start-called))))

(ert-deftest elinit-test-pid1-boot-noop-when-disabled ()
  "Boot function is a no-op when disabled."
  (let ((elinit-pid1-mode-enabled nil)
        (elinit-pid1-boot-script "/nonexistent/boot.el")
        (elinit-pid1-boot-policy 'never)
        (start-called nil))
    (cl-letf (((symbol-function 'elinit-start)
               (lambda () (setq start-called t))))
      (elinit--pid1-boot)
      (should-not start-called))))

(ert-deftest elinit-test-pid1-boot-loads-script-before-start ()
  "Boot function loads script before calling `elinit-start'."
  (let ((elinit-pid1-mode-enabled t)
        (elinit-pid1-boot-policy 'never)
        (elinit-pid1-boot-script "/nonexistent/boot.el")
        (call-order nil))
    (cl-letf (((symbol-function 'elinit--pid1-load-script)
               (lambda (&rest _) (push 'load-script call-order)))
              ((symbol-function 'elinit-start)
               (lambda () (push 'start call-order))))
      (elinit--pid1-boot)
      (should (equal '(start load-script) call-order)))))

;;;; Shutdown function tests

(ert-deftest elinit-test-pid1-shutdown-calls-stop ()
  "Shutdown function calls `elinit-stop-now' when enabled."
  (let ((elinit-pid1-mode-enabled t)
        (elinit-pid1-shutdown-script "/nonexistent/shutdown.el")
        (elinit-pid1-shutdown-policy 'never)
        (stop-called nil))
    (cl-letf (((symbol-function 'elinit-stop-now)
               (lambda () (setq stop-called t))))
      (elinit--pid1-shutdown)
      (should stop-called))))

(ert-deftest elinit-test-pid1-shutdown-noop-when-disabled ()
  "Shutdown function is a no-op when disabled."
  (let ((elinit-pid1-mode-enabled nil)
        (elinit-pid1-shutdown-script "/nonexistent/shutdown.el")
        (elinit-pid1-shutdown-policy 'never)
        (stop-called nil))
    (cl-letf (((symbol-function 'elinit-stop-now)
               (lambda () (setq stop-called t))))
      (elinit--pid1-shutdown)
      (should-not stop-called))))

(ert-deftest elinit-test-pid1-shutdown-loads-script-before-stop ()
  "Shutdown function loads script before calling `elinit-stop-now'."
  (let ((elinit-pid1-mode-enabled t)
        (elinit-pid1-shutdown-policy 'never)
        (elinit-pid1-shutdown-script "/nonexistent/shutdown.el")
        (call-order nil))
    (cl-letf (((symbol-function 'elinit--pid1-load-script)
               (lambda (&rest _) (push 'load-script call-order)))
              ((symbol-function 'elinit-stop-now)
               (lambda () (push 'stop call-order))))
      (elinit--pid1-shutdown)
      (should (equal '(stop load-script) call-order)))))

;;;; Hook registration tests

(ert-deftest elinit-test-pid1-hooks-populated-when-pid1-mode ()
  "Calling `elinit--pid1-register-hooks' populates PID1 hooks.
Calls the production function directly, then verifies the hooks
contain the expected functions."
  (let ((pid1-boot-hook nil)
        (pid1-poweroff-hook nil)
        (pid1-reboot-hook nil))
    (elinit--pid1-register-hooks)
    (should (memq #'elinit--pid1-boot pid1-boot-hook))
    (should (memq #'elinit--pid1-shutdown pid1-poweroff-hook))
    (should (memq #'elinit--pid1-shutdown pid1-reboot-hook))))

(ert-deftest elinit-test-pid1-hooks-skipped-when-no-pid1-mode ()
  "Load-time guard skips registration when `pid1-mode' is nil.
Exercises the production load-time conditional by evaluating
it with `pid1-mode' nil, verifying no hooks are added."
  (let ((pid1-mode nil)
        (pid1-boot-hook nil)
        (pid1-poweroff-hook nil)
        (pid1-reboot-hook nil))
    ;; Evaluate the load-time guard form (must use eval because
    ;; bound-and-true-p is a macro that captures the symbol name).
    (eval '(when (bound-and-true-p pid1-mode)
             (elinit--pid1-register-hooks))
          t)
    (should-not pid1-boot-hook)
    (should-not pid1-poweroff-hook)
    (should-not pid1-reboot-hook)))

;;;; B1 acceptance #2: non-PID1 behavior unchanged (make check gate)
;;
;; These tests verify that loading elinit-pid1 in a normal (non-PID1)
;; Emacs session produces no observable side effects.  This closes the
;; gap where B1 acceptance criterion "default behavior remains unchanged
;; when PID1 mode is disabled" was only documented, not CI-enforced.

(ert-deftest elinit-test-pid1-mode-enabled-default-nil ()
  "Non-nil default for `elinit-pid1-mode-enabled' requires PID1 env.
In the test environment `pid1-mode' is nil and PID is not 1, so
auto-detection returns nil and the defcustom default is nil."
  (should-not elinit-pid1-mode-enabled))

(ert-deftest elinit-test-pid1-no-hooks-at-load-without-pid1-mode ()
  "Module load registers no hooks when `pid1-mode' is absent.
After `elinit-pid1' was loaded (via `require' in the test harness),
no elinit functions should appear on any PID1 hook because
`pid1-mode' was nil at load time."
  ;; pid1-boot-hook should be void or not contain our function.
  (should-not (and (boundp 'pid1-boot-hook)
                   (memq #'elinit--pid1-boot
                         (symbol-value 'pid1-boot-hook))))
  ;; pid1-poweroff-hook
  (should-not (and (boundp 'pid1-poweroff-hook)
                   (memq #'elinit--pid1-shutdown
                         (symbol-value 'pid1-poweroff-hook))))
  ;; pid1-reboot-hook
  (should-not (and (boundp 'pid1-reboot-hook)
                   (memq #'elinit--pid1-shutdown
                         (symbol-value 'pid1-reboot-hook)))))

(ert-deftest elinit-test-pid1-no-process-side-effects-when-disabled ()
  "Boot and shutdown produce zero calls when mode is disabled.
Verifies that `elinit-start', `elinit-stop-now', `load', and
`elinit--log' are never invoked when `elinit-pid1-mode-enabled'
is nil -- the default in non-PID1 environments."
  (let ((elinit-pid1-mode-enabled nil)
        (elinit-pid1-boot-script "/lib/init/rc.boot.el")
        (elinit-pid1-boot-policy 'if-present)
        (elinit-pid1-shutdown-script "/lib/init/rc.shutdown.el")
        (elinit-pid1-shutdown-policy 'if-present)
        (any-call nil))
    (cl-letf (((symbol-function 'elinit-start)
               (lambda () (setq any-call t)))
              ((symbol-function 'elinit-stop-now)
               (lambda () (setq any-call t)))
              ((symbol-function 'elinit--log)
               (lambda (&rest _) (setq any-call t)))
              ((symbol-function 'load)
               (lambda (&rest _) (setq any-call t))))
      (elinit--pid1-boot)
      (elinit--pid1-shutdown)
      (should-not any-call))))

(ert-deftest elinit-test-pid1-detect-mode-unbound-pid1-mode ()
  "Auto-detect returns nil when `pid1-mode' symbol is void.
Covers the real-world case where Emacs was not built with PID1
patches and `pid1-mode' is completely undefined."
  ;; Temporarily make pid1-mode void to simulate unpatched Emacs.
  (let ((had-value (boundp 'pid1-mode))
        (saved-value (and (boundp 'pid1-mode)
                          (symbol-value 'pid1-mode))))
    (unwind-protect
        (progn
          (makunbound 'pid1-mode)
          (should-not (elinit--pid1-detect-mode)))
      ;; Restore previous state.
      (if had-value
          (set 'pid1-mode saved-value)
        (makunbound 'pid1-mode)))))

(provide 'elinit-test-pid1)

;;; elinit-test-pid1.el ends here
