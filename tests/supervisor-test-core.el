;;; supervisor-test-core.el --- Core feature loading, parsing, and types tests for supervisor.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Core feature loading, parsing, and types ERT tests for supervisor.el.

;;; Code:

(require 'supervisor-test-helpers)

;;; Package structure tests

(ert-deftest supervisor-test-feature-provided ()
  "Verify the supervisor feature is provided."
  (should (featurep 'supervisor)))

(ert-deftest supervisor-test-module-load-core ()
  "Verify supervisor-core module loads and provides its feature."
  (should (featurep 'supervisor-core)))

(ert-deftest supervisor-test-module-load-dashboard ()
  "Verify supervisor-dashboard module loads and provides its feature."
  (should (featurep 'supervisor-dashboard)))

(ert-deftest supervisor-test-module-load-cli ()
  "Verify supervisor-cli module loads and provides its feature."
  (should (featurep 'supervisor-cli)))

(ert-deftest supervisor-test-module-core-standalone ()
  "Verify supervisor-core loads and works without dashboard, CLI, or timer.
Spawns a subprocess to test true standalone behavior.
Core guards timer calls with fboundp, so verify works without timer module."
  (let* ((default-directory (file-name-directory (locate-library "supervisor")))
         (result (call-process
                  "emacs" nil nil nil
                  "-Q" "--batch" "-L" "."
                  "--eval" "(require 'supervisor-core)"
                  "--eval" "(setq supervisor-unit-directory (make-temp-file \"units-\" t))"
                  "--eval" "(supervisor-verify)")))
    (should (= result 0))))

(ert-deftest supervisor-test-module-core-standalone-stop ()
  "Verify supervisor-stop works standalone without timer module.
Tests the stop path which was previously missing fboundp guard."
  (let* ((default-directory (file-name-directory (locate-library "supervisor")))
         (result (call-process
                  "emacs" nil nil nil
                  "-Q" "--batch" "-L" "."
                  "--eval" "(require 'supervisor-core)"
                  "--eval" "(setq supervisor-unit-directory (make-temp-file \"units-\" t))"
                  "--eval" "(supervisor-stop)")))
    (should (= result 0))))

(ert-deftest supervisor-test-module-core-standalone-start ()
  "Verify supervisor-start works standalone without timer module."
  (let* ((default-directory (file-name-directory (locate-library "supervisor")))
         (result (call-process
                  "emacs" nil nil nil
                  "-Q" "--batch" "-L" "."
                  "--eval" "(require 'supervisor-core)"
                  "--eval" "(setq supervisor-unit-directory (make-temp-file \"units-\" t))"
                  "--eval" "(supervisor-start)")))
    (should (= result 0))))

(ert-deftest supervisor-test-module-cli-standalone ()
  "Verify supervisor-cli loads and works without dashboard or timer.
Spawns a subprocess to test CLI dispatch without dashboard or timer module."
  (let* ((default-directory (file-name-directory (locate-library "supervisor")))
         (result (call-process
                  "emacs" nil nil nil
                  "-Q" "--batch" "-L" "."
                  "--eval" "(require 'supervisor-core)"
                  "--eval" "(require 'supervisor-cli)"
                  "--eval" "(setq supervisor-unit-directory (make-temp-file \"units-\" t))"
                  "--eval" "(supervisor--cli-dispatch '(\"status\"))")))
    (should (= result 0))))

(ert-deftest supervisor-test-module-no-cycles ()
  "Verify modules have no circular require chains.
Core must not require dashboard or cli.  This is checked by verifying
core symbols exist without dashboard/cli-specific dependencies."
  ;; Core should work standalone - key symbols should exist
  (should (fboundp 'supervisor--parse-entry))
  (should (fboundp 'supervisor--build-plan))
  (should (fboundp 'supervisor-start))
  ;; Dashboard-specific symbols should be in dashboard
  (should (fboundp 'supervisor-dashboard-mode))
  ;; CLI-specific symbols should be in cli
  (should (fboundp 'supervisor--cli-dispatch)))


;;; Entry parsing tests

(ert-deftest supervisor-test-parse-string-entry ()
  "Parse a simple string entry."
  (let ((parsed (supervisor--parse-entry "nm-applet")))
    (should (equal (supervisor-entry-id parsed) "nm-applet"))
    (should (equal (supervisor-entry-command parsed) "nm-applet"))
    (should (= (supervisor-entry-delay parsed) 0))
    (should (eq (supervisor-entry-enabled-p parsed) t))
    (should (eq (supervisor-entry-restart-policy parsed) 'always))
    (should (eq (supervisor-entry-logging-p parsed) t))
    (should-not (supervisor-entry-stdout-log-file parsed))
    (should-not (supervisor-entry-stderr-log-file parsed))
    (should (eq (supervisor-entry-type parsed) 'simple))
    (should (eq (supervisor-entry-type parsed) 'simple))))

(ert-deftest supervisor-test-parse-plist-entry ()
  "Parse a plist-style entry with options."
  (let ((parsed (supervisor--parse-entry
                 '("nm-applet" :type simple :delay 3 :restart nil))))
    (should (equal (supervisor-entry-id parsed) "nm-applet"))
    (should (= (supervisor-entry-delay parsed) 3))
    (should (eq (supervisor-entry-restart-policy parsed) 'no))
    (should (eq (supervisor-entry-type parsed) 'simple))))

(ert-deftest supervisor-test-parse-explicit-id ()
  "Parse entry with explicit :id."
  (let ((parsed (supervisor--parse-entry
                 '("/usr/bin/nm-applet" :id "network"))))
    (should (equal (supervisor-entry-id parsed) "network"))))

(ert-deftest supervisor-test-parse-oneshot ()
  "Parse oneshot entry."
  (let ((parsed (supervisor--parse-entry
                 '("xrdb ~/.Xresources" :type oneshot))))
    (should (eq (supervisor-entry-type parsed) 'oneshot))))

(ert-deftest supervisor-test-parse-enabled-disabled ()
  "Parse :enabled and :disabled flags."
  (let ((enabled (supervisor--parse-entry '("foo" :enabled t)))
        (disabled (supervisor--parse-entry '("foo" :disabled t)))
        (explicit-nil (supervisor--parse-entry '("foo" :enabled nil))))
    (should (eq (nth 3 enabled) t))
    (should (eq (nth 3 disabled) nil))
    (should (eq (nth 3 explicit-nil) nil))))

(ert-deftest supervisor-test-parse-restart-no-restart ()
  "Parse :restart and :no-restart flags."
  (let ((restart (supervisor--parse-entry '("foo" :restart t)))
        (no-restart (supervisor--parse-entry '("foo" :no-restart t)))
        (explicit-nil (supervisor--parse-entry '("foo" :restart nil))))
    (should (eq (nth 4 restart) 'always))
    (should (eq (nth 4 no-restart) 'no))
    (should (eq (nth 4 explicit-nil) 'no))))

(ert-deftest supervisor-test-parse-after-string ()
  "Parse :after as string."
  (let ((parsed (supervisor--parse-entry '("bar" :after "foo"))))
    (should (equal (supervisor-entry-after parsed) '("foo")))))

(ert-deftest supervisor-test-parse-after-list ()
  "Parse :after as list."
  (let ((parsed (supervisor--parse-entry '("baz" :after ("foo" "bar")))))
    (should (equal (supervisor-entry-after parsed) '("foo" "bar")))))

(ert-deftest supervisor-test-parse-stream-log-files ()
  "Parse per-stream log file options."
  (let ((parsed (supervisor--parse-entry
                 '("svc-cmd"
                   :stdout-log-file "/tmp/svc.out.log"
                   :stderr-log-file "/tmp/svc.err.log"))))
    (should (equal (supervisor-entry-stdout-log-file parsed)
                   "/tmp/svc.out.log"))
    (should (equal (supervisor-entry-stderr-log-file parsed)
                   "/tmp/svc.err.log"))))

(ert-deftest supervisor-test-validate-stream-log-files ()
  "Validate per-stream log file options."
  (should-not (supervisor--validate-entry
               '("svc-cmd"
                 :stdout-log-file "/tmp/svc.out.log"
                 :stderr-log-file "/tmp/svc.err.log")))
  (should (string-match-p ":stdout-log-file"
                          (supervisor--validate-entry
                           '("svc-cmd" :stdout-log-file ""))))
  (should (string-match-p ":stderr-log-file"
                          (supervisor--validate-entry
                           '("svc-cmd" :stderr-log-file 123)))))


;;; Type conversion tests

;;; Normalize :after tests

(ert-deftest supervisor-test-normalize-after ()
  "Normalize :after values to lists."
  (should (equal (supervisor--normalize-after nil) nil))
  (should (equal (supervisor--normalize-after "foo") '("foo")))
  (should (equal (supervisor--normalize-after '("foo" "bar")) '("foo" "bar"))))

;;; Oneshot wait/timeout tests

(ert-deftest supervisor-test-oneshot-blocking-p-default ()
  "Default oneshot blocking behavior."
  (should (eq (supervisor--oneshot-blocking-p '()) supervisor-oneshot-default-blocking)))

(ert-deftest supervisor-test-oneshot-blocking-p-explicit ()
  "Explicit :oneshot-blocking overrides default."
  (should (eq (supervisor--oneshot-blocking-p '(:oneshot-blocking t)) t))
  (should (eq (supervisor--oneshot-blocking-p '(:oneshot-blocking nil)) nil)))

(ert-deftest supervisor-test-oneshot-blocking-p-oneshot-async ()
  "The :oneshot-async flag is inverse of :oneshot-blocking."
  (should (eq (supervisor--oneshot-blocking-p '(:oneshot-async t)) nil))
  (should (eq (supervisor--oneshot-blocking-p '(:oneshot-async nil)) t)))

(ert-deftest supervisor-test-oneshot-timeout-default ()
  "Default oneshot timeout."
  (should (eq (supervisor--oneshot-timeout-value '()) supervisor-oneshot-timeout)))

(ert-deftest supervisor-test-oneshot-timeout-explicit ()
  "Explicit :oneshot-timeout."
  (should (= (supervisor--oneshot-timeout-value '(:oneshot-timeout 60)) 60))
  (should (eq (supervisor--oneshot-timeout-value '(:oneshot-timeout nil)) nil)))


;;; Global minor mode tests

(ert-deftest supervisor-test-mode-defined ()
  "Verify supervisor-mode is defined as a global minor mode."
  (should (fboundp 'supervisor-mode))
  (should (custom-variable-p 'supervisor-mode)))

;;; Verbose logging tests

(ert-deftest supervisor-test-log-warning-always-shows ()
  "Warning messages always show regardless of verbose setting."
  (let ((supervisor-verbose nil)
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (supervisor--log 'warning "test warning"))
    (should (= (length messages) 1))
    (should (string-match "WARNING" (car messages)))))

(ert-deftest supervisor-test-log-info-hidden-when-not-verbose ()
  "Info messages hidden when supervisor-verbose is nil."
  (let ((supervisor-verbose nil)
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (supervisor--log 'info "test info"))
    (should (= (length messages) 0))))

(ert-deftest supervisor-test-log-info-shown-when-verbose ()
  "Info messages shown when supervisor-verbose is non-nil."
  (let ((supervisor-verbose t)
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (supervisor--log 'info "test info"))
    (should (= (length messages) 1))
    (should (string-match "test info" (car messages)))))


(provide 'supervisor-test-core)
;;; supervisor-test-core.el ends here
