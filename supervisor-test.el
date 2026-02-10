;;; supervisor-test.el --- Tests for supervisor.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; ERT tests for supervisor.el.
;; Run with: make test
;; Run single test: make test-one TEST=test-name

;;; Code:

(require 'ert)
(require 'supervisor)

;;; Package structure tests

(ert-deftest supervisor-test-feature-provided ()
  "Verify the supervisor feature is provided."
  (should (featurep 'supervisor)))

(ert-deftest supervisor-test-stages-defined ()
  "Verify stages are properly defined."
  (should (equal supervisor-stage-names '(early services session ui)))
  (should (= (alist-get 'early supervisor-stages) 0))
  (should (= (alist-get 'services supervisor-stages) 1))
  (should (= (alist-get 'session supervisor-stages) 2))
  (should (= (alist-get 'ui supervisor-stages) 3)))

;;; Entry parsing tests

(ert-deftest supervisor-test-parse-string-entry ()
  "Parse a simple string entry."
  (let ((parsed (supervisor--parse-entry "nm-applet")))
    (should (equal (nth 0 parsed) "nm-applet"))  ; id
    (should (equal (nth 1 parsed) "nm-applet"))  ; cmd
    (should (= (nth 2 parsed) 0))                ; delay
    (should (eq (nth 3 parsed) t))               ; enabled-p
    (should (eq (nth 4 parsed) t))               ; restart-p
    (should (eq (nth 5 parsed) t))               ; logging-p
    (should (eq (nth 6 parsed) 'simple))         ; type
    (should (eq (nth 7 parsed) 'session))))      ; stage (default)

(ert-deftest supervisor-test-parse-plist-entry ()
  "Parse a plist-style entry with options."
  (let ((parsed (supervisor--parse-entry
                 '("nm-applet" :type simple :stage services :delay 3 :restart nil))))
    (should (equal (nth 0 parsed) "nm-applet"))
    (should (= (nth 2 parsed) 3))                ; delay
    (should (eq (nth 4 parsed) nil))             ; restart-p
    (should (eq (nth 6 parsed) 'simple))
    (should (eq (nth 7 parsed) 'services))))

(ert-deftest supervisor-test-parse-explicit-id ()
  "Parse entry with explicit :id."
  (let ((parsed (supervisor--parse-entry
                 '("/usr/bin/nm-applet" :id "network"))))
    (should (equal (nth 0 parsed) "network"))))

(ert-deftest supervisor-test-parse-oneshot ()
  "Parse oneshot entry."
  (let ((parsed (supervisor--parse-entry
                 '("xrdb ~/.Xresources" :type oneshot :stage early))))
    (should (eq (nth 6 parsed) 'oneshot))
    (should (eq (nth 7 parsed) 'early))))

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
    (should (eq (nth 4 restart) t))
    (should (eq (nth 4 no-restart) nil))
    (should (eq (nth 4 explicit-nil) nil))))

(ert-deftest supervisor-test-parse-after-string ()
  "Parse :after as string."
  (let ((parsed (supervisor--parse-entry '("bar" :after "foo"))))
    (should (equal (nth 8 parsed) '("foo")))))

(ert-deftest supervisor-test-parse-after-list ()
  "Parse :after as list."
  (let ((parsed (supervisor--parse-entry '("baz" :after ("foo" "bar")))))
    (should (equal (nth 8 parsed) '("foo" "bar")))))

;;; Stage conversion tests

(ert-deftest supervisor-test-stage-to-int ()
  "Convert stage symbols to integers."
  (should (= (supervisor--stage-to-int 'early) 0))
  (should (= (supervisor--stage-to-int 'services) 1))
  (should (= (supervisor--stage-to-int 'session) 2))
  (should (= (supervisor--stage-to-int 'ui) 3))
  ;; Unknown defaults to session (2)
  (should (= (supervisor--stage-to-int 'unknown) 2)))

(ert-deftest supervisor-test-int-to-stage ()
  "Convert integers to stage symbols."
  (should (eq (supervisor--int-to-stage 0) 'early))
  (should (eq (supervisor--int-to-stage 1) 'services))
  (should (eq (supervisor--int-to-stage 2) 'session))
  (should (eq (supervisor--int-to-stage 3) 'ui)))

;;; Normalize :after tests

(ert-deftest supervisor-test-normalize-after ()
  "Normalize :after values to lists."
  (should (equal (supervisor--normalize-after nil) nil))
  (should (equal (supervisor--normalize-after "foo") '("foo")))
  (should (equal (supervisor--normalize-after '("foo" "bar")) '("foo" "bar"))))

;;; Oneshot wait/timeout tests

(ert-deftest supervisor-test-oneshot-wait-p-default ()
  "Default oneshot wait behavior."
  (should (eq (supervisor--oneshot-wait-p '()) supervisor-oneshot-default-wait)))

(ert-deftest supervisor-test-oneshot-wait-p-explicit ()
  "Explicit :oneshot-wait overrides default."
  (should (eq (supervisor--oneshot-wait-p '(:oneshot-wait t)) t))
  (should (eq (supervisor--oneshot-wait-p '(:oneshot-wait nil)) nil)))

(ert-deftest supervisor-test-oneshot-wait-p-async ()
  "The :async flag is inverse of :oneshot-wait."
  (should (eq (supervisor--oneshot-wait-p '(:async t)) nil))
  (should (eq (supervisor--oneshot-wait-p '(:async nil)) t)))

(ert-deftest supervisor-test-oneshot-timeout-default ()
  "Default oneshot timeout."
  (should (eq (supervisor--oneshot-timeout-value '()) supervisor-oneshot-timeout)))

(ert-deftest supervisor-test-oneshot-timeout-explicit ()
  "Explicit :oneshot-timeout."
  (should (= (supervisor--oneshot-timeout-value '(:oneshot-timeout 60)) 60))
  (should (eq (supervisor--oneshot-timeout-value '(:oneshot-timeout nil)) nil)))

;;; Entry validation tests

(ert-deftest supervisor-test-validate-string-entry ()
  "String entries are always valid."
  (should (null (supervisor--validate-entry "nm-applet")))
  (should (null (supervisor--validate-entry "/usr/bin/foo"))))

(ert-deftest supervisor-test-validate-valid-simple ()
  "Valid simple entry passes validation."
  (should (null (supervisor--validate-entry
                 '("nm-applet" :type simple :stage session :restart t)))))

(ert-deftest supervisor-test-validate-valid-oneshot ()
  "Valid oneshot entry passes validation."
  (should (null (supervisor--validate-entry
                 '("xrdb" :type oneshot :stage early :oneshot-timeout 30)))))

(ert-deftest supervisor-test-validate-unknown-keyword ()
  "Unknown keywords are rejected."
  (should (string-match "unknown keyword"
                        (supervisor--validate-entry
                         '("foo" :bogus t)))))

(ert-deftest supervisor-test-validate-invalid-type ()
  "Invalid :type values are rejected."
  (should (string-match ":type must be"
                        (supervisor--validate-entry
                         '("foo" :type daemon)))))

(ert-deftest supervisor-test-validate-invalid-stage ()
  "Invalid :stage values are rejected."
  (should (string-match ":stage must be"
                        (supervisor--validate-entry
                         '("foo" :stage boot)))))

(ert-deftest supervisor-test-validate-invalid-delay ()
  "Invalid :delay values are rejected."
  (should (string-match ":delay must be"
                        (supervisor--validate-entry
                         '("foo" :delay "slow"))))
  (should (string-match ":delay must be"
                        (supervisor--validate-entry
                         '("foo" :delay -1)))))

(ert-deftest supervisor-test-validate-invalid-oneshot-timeout ()
  "Invalid :oneshot-timeout values are rejected."
  (should (string-match ":oneshot-timeout must be"
                        (supervisor--validate-entry
                         '("foo" :type oneshot :oneshot-timeout "slow")))))

(ert-deftest supervisor-test-validate-mutually-exclusive-enabled ()
  ":enabled and :disabled are mutually exclusive."
  (should (string-match "mutually exclusive"
                        (supervisor--validate-entry
                         '("foo" :enabled t :disabled t)))))

(ert-deftest supervisor-test-validate-mutually-exclusive-restart ()
  ":restart and :no-restart are mutually exclusive."
  (should (string-match "mutually exclusive"
                        (supervisor--validate-entry
                         '("foo" :restart t :no-restart t)))))

(ert-deftest supervisor-test-validate-simple-rejects-oneshot-keywords ()
  "Simple type rejects oneshot-specific keywords."
  (should (string-match ":oneshot-wait is invalid for :type simple"
                        (supervisor--validate-entry
                         '("foo" :type simple :oneshot-wait t))))
  (should (string-match ":async is invalid for :type simple"
                        (supervisor--validate-entry
                         '("foo" :type simple :async t))))
  (should (string-match ":oneshot-timeout is invalid for :type simple"
                        (supervisor--validate-entry
                         '("foo" :type simple :oneshot-timeout 30)))))

(ert-deftest supervisor-test-validate-oneshot-rejects-restart-keywords ()
  "Oneshot type rejects restart-specific keywords."
  (should (string-match ":restart is invalid for :type oneshot"
                        (supervisor--validate-entry
                         '("foo" :type oneshot :restart t))))
  (should (string-match ":no-restart is invalid for :type oneshot"
                        (supervisor--validate-entry
                         '("foo" :type oneshot :no-restart t)))))

(ert-deftest supervisor-test-validate-multiple-errors ()
  "Multiple validation errors are collected."
  (let ((result (supervisor--validate-entry
                 '("foo" :enabled t :disabled t :restart t :no-restart t))))
    (should (string-match "mutually exclusive" result))
    ;; Should contain both errors separated by semicolon
    (should (> (length (split-string result ";")) 1))))

(ert-deftest supervisor-test-validate-mutually-exclusive-oneshot-wait-async ()
  ":oneshot-wait and :async are mutually exclusive."
  (should (string-match "mutually exclusive"
                        (supervisor--validate-entry
                         '("foo" :type oneshot :oneshot-wait t :async t)))))

(ert-deftest supervisor-test-validate-type-must-be-symbol ()
  ":type must be a symbol, not a string."
  (should (string-match "must be a symbol"
                        (supervisor--validate-entry
                         '("foo" :type "oneshot")))))

(ert-deftest supervisor-test-validate-stage-must-be-symbol ()
  ":stage must be a symbol, not a string."
  (should (string-match "must be a symbol"
                        (supervisor--validate-entry
                         '("foo" :stage "early")))))

;;; Integration tests

(ert-deftest supervisor-test-validate-populates-invalid-hash ()
  "supervisor-validate populates supervisor--invalid hash table."
  (let ((supervisor-programs '(("valid-entry" :type simple)
                               ("invalid-entry" :type "bad")))
        (supervisor--invalid (make-hash-table :test 'equal)))
    (with-temp-buffer
      (let ((standard-output (current-buffer)))
        (supervisor-validate)))
    (should (null (gethash "valid-entry" supervisor--invalid)))
    (should (gethash "invalid-entry" supervisor--invalid))))

(ert-deftest supervisor-test-all-parsed-entries-skips-invalid ()
  "supervisor--all-parsed-entries skips invalid entries."
  (let ((supervisor-programs '(("valid" :type simple)
                               ("invalid" :type "bad")
                               ("also-valid" :type oneshot)))
        (supervisor--invalid (make-hash-table :test 'equal)))
    (let ((entries (supervisor--all-parsed-entries)))
      ;; Should have 2 valid entries
      (should (= (length entries) 2))
      ;; Invalid should be tracked
      (should (gethash "invalid" supervisor--invalid))
      ;; Valid entries should be in result
      (should (cl-find "valid" entries :key #'car :test #'equal))
      (should (cl-find "also-valid" entries :key #'car :test #'equal)))))

(ert-deftest supervisor-test-validate-handles-malformed-entry ()
  "supervisor-validate handles non-list, non-string entries gracefully."
  (let ((supervisor-programs '(42 ("valid" :type simple)))
        (supervisor--invalid (make-hash-table :test 'equal)))
    (with-temp-buffer
      (let ((standard-output (current-buffer)))
        (supervisor-validate)))
    ;; Should have recorded the malformed entry with index-based ID
    (should (gethash "malformed#0" supervisor--invalid))
    ;; Valid entry should not be in invalid
    (should (null (gethash "valid" supervisor--invalid)))))

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

(provide 'supervisor-test)
;;; supervisor-test.el ends here
