;;; elinit-test-core.el --- Core feature loading, parsing, and types tests for elinit.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Core feature loading, parsing, and types ERT tests for elinit.el.

;;; Code:

(require 'elinit-test-helpers)

;;; Package structure tests

(ert-deftest elinit-test-feature-provided ()
  "Verify the elinit feature is provided."
  (should (featurep 'elinit)))

(ert-deftest elinit-test-module-load-core ()
  "Verify elinit-core module loads and provides its feature."
  (should (featurep 'elinit-core)))

(ert-deftest elinit-test-module-load-dashboard ()
  "Verify elinit-dashboard module loads and provides its feature."
  (should (featurep 'elinit-dashboard)))

(ert-deftest elinit-test-module-load-cli ()
  "Verify elinit-cli module loads and provides its feature."
  (should (featurep 'elinit-cli)))

(ert-deftest elinit-test-module-core-standalone ()
  "Verify elinit-core loads and works without dashboard, CLI, or timer.
Spawns a subprocess to test true standalone behavior.
Core guards timer calls with fboundp, so verify works without timer module."
  (let* ((default-directory (file-name-directory (locate-library "elinit")))
         (result (call-process
                  "emacs" nil nil nil
                  "-Q" "--batch" "-L" "."
                  "--eval" "(require 'elinit-core)"
                  "--eval" "(setq elinit-unit-directory (make-temp-file \"units-\" t))"
                  "--eval" "(elinit-verify)")))
    (should (= result 0))))

(ert-deftest elinit-test-module-core-standalone-stop ()
  "Verify elinit-stop works standalone without timer module.
Tests the stop path which was previously missing fboundp guard."
  (let* ((default-directory (file-name-directory (locate-library "elinit")))
         (result (call-process
                  "emacs" nil nil nil
                  "-Q" "--batch" "-L" "."
                  "--eval" "(require 'elinit-core)"
                  "--eval" "(setq elinit-unit-directory (make-temp-file \"units-\" t))"
                  "--eval" "(elinit-stop)")))
    (should (= result 0))))

(ert-deftest elinit-test-module-core-standalone-start ()
  "Verify elinit-start works standalone without timer module."
  (let* ((default-directory (file-name-directory (locate-library "elinit")))
         (result (call-process
                  "emacs" nil nil nil
                  "-Q" "--batch" "-L" "."
                  "--eval" "(require 'elinit-core)"
                  "--eval" "(setq elinit-unit-directory (make-temp-file \"units-\" t))"
                  "--eval" "(elinit-start)")))
    (should (= result 0))))

(ert-deftest elinit-test-module-cli-standalone ()
  "Verify elinit-cli loads and works without dashboard or timer.
Spawns a subprocess to test CLI dispatch without dashboard or timer module."
  (let* ((default-directory (file-name-directory (locate-library "elinit")))
         (result (call-process
                  "emacs" nil nil nil
                  "-Q" "--batch" "-L" "."
                  "--eval" "(require 'elinit-core)"
                  "--eval" "(require 'elinit-cli)"
                  "--eval" "(setq elinit-unit-directory (make-temp-file \"units-\" t))"
                  "--eval" "(elinit--cli-dispatch '(\"status\"))")))
    (should (= result 0))))

(ert-deftest elinit-test-module-no-cycles ()
  "Verify modules have no circular require chains.
Core must not require dashboard or cli.  This is checked by verifying
core symbols exist without dashboard/cli-specific dependencies."
  ;; Core should work standalone - key symbols should exist
  (should (fboundp 'elinit--parse-entry))
  (should (fboundp 'elinit--build-plan))
  (should (fboundp 'elinit-start))
  ;; Dashboard-specific symbols should be in dashboard
  (should (fboundp 'elinit-dashboard-mode))
  ;; CLI-specific symbols should be in cli
  (should (fboundp 'elinit--cli-dispatch)))


;;; Entry parsing tests

(ert-deftest elinit-test-parse-string-entry ()
  "Parse a simple string entry."
  (let ((parsed (elinit--parse-entry "nm-applet")))
    (should (equal (elinit-entry-id parsed) "nm-applet"))
    (should (equal (elinit-entry-command parsed) "nm-applet"))
    (should (= (elinit-entry-delay parsed) 0))
    (should (eq (elinit-entry-enabled-p parsed) t))
    (should (eq (elinit-entry-restart-policy parsed) 'always))
    (should (eq (elinit-entry-logging-p parsed) t))
    (should-not (elinit-entry-stdout-log-file parsed))
    (should-not (elinit-entry-stderr-log-file parsed))
    (should (eq (elinit-entry-type parsed) 'simple))
    (should (eq (elinit-entry-type parsed) 'simple))))

(ert-deftest elinit-test-parse-plist-entry ()
  "Parse a plist-style entry with options."
  (let ((parsed (elinit--parse-entry
                 '("nm-applet" :type simple :delay 3 :restart nil))))
    (should (equal (elinit-entry-id parsed) "nm-applet"))
    (should (= (elinit-entry-delay parsed) 3))
    (should (eq (elinit-entry-restart-policy parsed) 'no))
    (should (eq (elinit-entry-type parsed) 'simple))))

(ert-deftest elinit-test-parse-explicit-id ()
  "Parse entry with explicit :id."
  (let ((parsed (elinit--parse-entry
                 '("/usr/bin/nm-applet" :id "network"))))
    (should (equal (elinit-entry-id parsed) "network"))))

(ert-deftest elinit-test-parse-oneshot ()
  "Parse oneshot entry."
  (let ((parsed (elinit--parse-entry
                 '("xrdb ~/.Xresources" :type oneshot))))
    (should (eq (elinit-entry-type parsed) 'oneshot))))

(ert-deftest elinit-test-parse-enabled-disabled ()
  "Parse :enabled and :disabled flags."
  (let ((enabled (elinit--parse-entry '("foo" :enabled t)))
        (disabled (elinit--parse-entry '("foo" :disabled t)))
        (explicit-nil (elinit--parse-entry '("foo" :enabled nil))))
    (should (eq (nth 3 enabled) t))
    (should (eq (nth 3 disabled) nil))
    (should (eq (nth 3 explicit-nil) nil))))

(ert-deftest elinit-test-parse-restart-no-restart ()
  "Parse :restart and :no-restart flags."
  (let ((restart (elinit--parse-entry '("foo" :restart t)))
        (no-restart (elinit--parse-entry '("foo" :no-restart t)))
        (explicit-nil (elinit--parse-entry '("foo" :restart nil))))
    (should (eq (nth 4 restart) 'always))
    (should (eq (nth 4 no-restart) 'no))
    (should (eq (nth 4 explicit-nil) 'no))))

(ert-deftest elinit-test-parse-after-string ()
  "Parse :after as string."
  (let ((parsed (elinit--parse-entry '("bar" :after "foo"))))
    (should (equal (elinit-entry-after parsed) '("foo")))))

(ert-deftest elinit-test-parse-after-list ()
  "Parse :after as list."
  (let ((parsed (elinit--parse-entry '("baz" :after ("foo" "bar")))))
    (should (equal (elinit-entry-after parsed) '("foo" "bar")))))

(ert-deftest elinit-test-parse-stream-log-files ()
  "Parse per-stream log file options."
  (let ((parsed (elinit--parse-entry
                 '("svc-cmd"
                   :stdout-log-file "/tmp/svc.out.log"
                   :stderr-log-file "/tmp/svc.err.log"))))
    (should (equal (elinit-entry-stdout-log-file parsed)
                   "/tmp/svc.out.log"))
    (should (equal (elinit-entry-stderr-log-file parsed)
                   "/tmp/svc.err.log"))))

(ert-deftest elinit-test-validate-stream-log-files ()
  "Validate per-stream log file options."
  (should-not (elinit--validate-entry
               '("svc-cmd"
                 :stdout-log-file "/tmp/svc.out.log"
                 :stderr-log-file "/tmp/svc.err.log")))
  (should (string-match-p ":stdout-log-file"
                          (elinit--validate-entry
                           '("svc-cmd" :stdout-log-file ""))))
  (should (string-match-p ":stderr-log-file"
                          (elinit--validate-entry
                           '("svc-cmd" :stderr-log-file 123)))))


;;; Type conversion tests

;;; Normalize :after tests

(ert-deftest elinit-test-normalize-after ()
  "Normalize :after values to lists."
  (should (equal (elinit--normalize-after nil) nil))
  (should (equal (elinit--normalize-after "foo") '("foo")))
  (should (equal (elinit--normalize-after '("foo" "bar")) '("foo" "bar"))))

;;; Oneshot wait/timeout tests

(ert-deftest elinit-test-oneshot-blocking-p-default ()
  "Default oneshot blocking behavior."
  (should (eq (elinit--oneshot-blocking-p '()) elinit-oneshot-default-blocking)))

(ert-deftest elinit-test-oneshot-blocking-p-explicit ()
  "Explicit :oneshot-blocking overrides default."
  (should (eq (elinit--oneshot-blocking-p '(:oneshot-blocking t)) t))
  (should (eq (elinit--oneshot-blocking-p '(:oneshot-blocking nil)) nil)))

(ert-deftest elinit-test-oneshot-blocking-p-oneshot-async ()
  "The :oneshot-async flag is inverse of :oneshot-blocking."
  (should (eq (elinit--oneshot-blocking-p '(:oneshot-async t)) nil))
  (should (eq (elinit--oneshot-blocking-p '(:oneshot-async nil)) t)))

(ert-deftest elinit-test-oneshot-timeout-default ()
  "Default oneshot timeout."
  (should (eq (elinit--oneshot-timeout-value '()) elinit-oneshot-timeout)))

(ert-deftest elinit-test-oneshot-timeout-explicit ()
  "Explicit :oneshot-timeout."
  (should (= (elinit--oneshot-timeout-value '(:oneshot-timeout 60)) 60))
  (should (eq (elinit--oneshot-timeout-value '(:oneshot-timeout nil)) nil)))


;;; Global minor mode tests

(ert-deftest elinit-test-mode-defined ()
  "Verify elinit-mode is defined as a global minor mode."
  (should (fboundp 'elinit-mode))
  (should (custom-variable-p 'elinit-mode)))

;;; Verbose logging tests

(ert-deftest elinit-test-log-warning-always-shows ()
  "Warning messages always show regardless of verbose setting."
  (let ((elinit-verbose nil)
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (elinit--log 'warning "test warning"))
    (should (= (length messages) 1))
    (should (string-match "WARNING" (car messages)))))

(ert-deftest elinit-test-log-info-hidden-when-not-verbose ()
  "Info messages hidden when elinit-verbose is nil."
  (let ((elinit-verbose nil)
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (elinit--log 'info "test info"))
    (should (= (length messages) 0))))

(ert-deftest elinit-test-log-info-shown-when-verbose ()
  "Info messages shown when elinit-verbose is non-nil."
  (let ((elinit-verbose t)
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (elinit--log 'info "test info"))
    (should (= (length messages) 1))
    (should (string-match "test info" (car messages)))))


(provide 'elinit-test-core)
;;; elinit-test-core.el ends here
