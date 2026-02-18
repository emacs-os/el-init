;;; supervisor-test-validation.el --- Entry validation and hardening tests for supervisor.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Entry validation and hardening ERT tests for supervisor.el.

;;; Code:

(require 'supervisor-test-helpers)

;;; Entry validation tests

(ert-deftest supervisor-test-validate-string-entry ()
  "String entries are always valid."
  (should (null (supervisor--validate-entry "nm-applet")))
  (should (null (supervisor--validate-entry "/usr/bin/foo"))))

(ert-deftest supervisor-test-validate-valid-simple ()
  "Valid simple entry passes validation."
  (should (null (supervisor--validate-entry
                 '("nm-applet" :type simple :restart t)))))

(ert-deftest supervisor-test-validate-valid-oneshot ()
  "Valid oneshot entry passes validation."
  (should (null (supervisor--validate-entry
                 '("xrdb" :type oneshot :oneshot-timeout 30)))))

(ert-deftest supervisor-test-validate-unknown-keyword ()
  "Unknown keywords are rejected."
  (should (string-match "unknown keyword"
                        (supervisor--validate-entry
                         '("foo" :bogus t)))))

(ert-deftest supervisor-test-validate-rejects-legacy-oneshot-wait ()
  "Legacy :oneshot-wait keyword is rejected as unknown."
  (should (string-match "unknown keyword.*:oneshot-wait"
                        (supervisor--validate-entry
                         '("foo" :type oneshot :oneshot-wait t)))))

(ert-deftest supervisor-test-validate-rejects-legacy-async ()
  "Legacy :async keyword is rejected as unknown."
  (should (string-match "unknown keyword.*:async"
                        (supervisor--validate-entry
                         '("foo" :type oneshot :async t)))))

(ert-deftest supervisor-test-validate-invalid-type ()
  "Invalid :type values are rejected."
  (should (string-match ":type must be"
                        (supervisor--validate-entry
                         '("foo" :type daemon)))))

(ert-deftest supervisor-test-validate-invalid-stage ()
  ":stage produces deprecation error."
  (should (string-match ":stage is removed"
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

(ert-deftest supervisor-test-validate-id-must-be-string ()
  "Non-string :id values are rejected."
  (should (string-match ":id must be a string"
                        (supervisor--validate-entry
                         '("foo" :id 42))))
  (should (string-match ":id must be a string"
                        (supervisor--validate-entry
                         '("foo" :id foo-symbol))))
  ;; String :id is valid
  (should-not (supervisor--validate-entry
               '("foo" :id "valid-string"))))

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
  (should (string-match ":oneshot-blocking is invalid for :type simple"
                        (supervisor--validate-entry
                         '("foo" :type simple :oneshot-blocking t))))
  (should (string-match ":oneshot-async is invalid for :type simple"
                        (supervisor--validate-entry
                         '("foo" :type simple :oneshot-async t))))
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

(ert-deftest supervisor-test-validate-mutually-exclusive-oneshot-blocking-async ()
  ":oneshot-blocking and :oneshot-async are mutually exclusive."
  (should (string-match "mutually exclusive"
                        (supervisor--validate-entry
                         '("foo" :type oneshot :oneshot-blocking t :oneshot-async t)))))

(ert-deftest supervisor-test-validate-type-must-be-symbol ()
  ":type must be a symbol, not a string."
  (should (string-match "must be a symbol"
                        (supervisor--validate-entry
                         '("foo" :type "oneshot")))))

(ert-deftest supervisor-test-validate-stage-must-be-symbol ()
  ":stage as string also produces deprecation error."
  (should (string-match ":stage is removed"
                        (supervisor--validate-entry
                         '("foo" :stage "stage1")))))


;;; V1: Plist shape guard tests

(ert-deftest supervisor-test-validate-entry-odd-plist ()
  "Odd-length plist is rejected by entry validation."
  (let ((result (supervisor--validate-entry '("cmd" :id "svc" :enabled))))
    (should (stringp result))
    (should (string-match-p "odd number of elements" result))))

(ert-deftest supervisor-test-validate-entry-dotted-plist ()
  "Dotted plist is rejected by entry validation."
  (let ((result (supervisor--validate-entry '("cmd" :enabled . t))))
    (should (stringp result))
    (should (string-match-p "must be a proper key/value list" result))))

(ert-deftest supervisor-test-validate-unit-file-odd-plist ()
  "Odd-length plist is rejected by unit-file validation."
  (let ((result (supervisor--validate-unit-file-plist
                 '(:id "svc" :command "cmd" :enabled) "test.el" 1)))
    (should (stringp result))
    (should (string-match-p "odd number of elements" result))))

(ert-deftest supervisor-test-validate-unit-file-dotted-plist ()
  "Dotted plist is rejected by unit-file validation."
  (let ((result (supervisor--validate-unit-file-plist
                 '(:id "svc" :command . "cmd") "test.el" 1)))
    (should (stringp result))
    (should (string-match-p "must be a proper key/value list" result))))

;;; V2: Command non-empty guard tests

(ert-deftest supervisor-test-validate-entry-empty-string-command ()
  "Empty string command is rejected by entry validation."
  (let ((result (supervisor--validate-entry "")))
    (should (stringp result))
    (should (string-match-p "empty or whitespace-only" result))))

(ert-deftest supervisor-test-validate-entry-whitespace-string-command ()
  "Whitespace-only string command is rejected by entry validation."
  (let ((result (supervisor--validate-entry "   ")))
    (should (stringp result))
    (should (string-match-p "empty or whitespace-only" result))))

(ert-deftest supervisor-test-validate-entry-empty-list-command ()
  "Empty command in list entry is rejected by entry validation."
  (let ((result (supervisor--validate-entry '("" :id "svc"))))
    (should (stringp result))
    (should (string-match-p "empty or whitespace-only" result))))

(ert-deftest supervisor-test-validate-unit-file-empty-command ()
  "Empty :command is rejected by unit-file validation."
  (let ((result (supervisor--validate-unit-file-plist
                 '(:id "svc" :command "") "test.el" 1)))
    (should (stringp result))
    (should (string-match-p "empty or whitespace-only" result))))

(ert-deftest supervisor-test-validate-unit-file-whitespace-command ()
  "Whitespace-only :command is rejected by unit-file validation."
  (let ((result (supervisor--validate-unit-file-plist
                 '(:id "svc" :command "   ") "test.el" 1)))
    (should (stringp result))
    (should (string-match-p "empty or whitespace-only" result))))

;;; V3: ID hardening tests

(ert-deftest supervisor-test-validate-entry-empty-id ()
  "Empty :id is rejected by entry validation."
  (let ((result (supervisor--validate-entry '("cmd" :id ""))))
    (should (stringp result))
    (should (string-match-p ":id must not be empty" result))))

(ert-deftest supervisor-test-validate-entry-id-with-slash ()
  "ID containing slash is rejected by entry validation."
  (let ((result (supervisor--validate-entry '("cmd" :id "../etc/passwd"))))
    (should (stringp result))
    (should (string-match-p "invalid characters" result))))

(ert-deftest supervisor-test-validate-entry-id-with-control-char ()
  "ID containing control characters is rejected by entry validation."
  (let ((result (supervisor--validate-entry '("cmd" :id "foo\nbar"))))
    (should (stringp result))
    (should (string-match-p "invalid characters" result))))

(ert-deftest supervisor-test-validate-entry-id-valid-chars ()
  "ID with valid characters passes entry validation."
  (let ((result (supervisor--validate-entry '("cmd" :id "my-svc_2.0:main@host"))))
    (should-not result)))

(ert-deftest supervisor-test-validate-unit-file-id-invalid-chars ()
  "ID with invalid characters is rejected by unit-file validation."
  (let ((result (supervisor--validate-unit-file-plist
                 '(:id "foo/bar" :command "cmd") "test.el" 1)))
    (should (stringp result))
    (should (string-match-p "invalid characters" result))))

;;; V4: Strict boolean flag tests

(ert-deftest supervisor-test-validate-boolean-enabled-string ()
  "String value for :enabled is rejected."
  (let ((result (supervisor--validate-entry '("cmd" :enabled "yes"))))
    (should (stringp result))
    (should (string-match-p ":enabled must be t or nil" result))))

(ert-deftest supervisor-test-validate-boolean-disabled-string ()
  "String value for :disabled is rejected."
  (let ((result (supervisor--validate-entry '("cmd" :disabled "no"))))
    (should (stringp result))
    (should (string-match-p ":disabled must be t or nil" result))))

(ert-deftest supervisor-test-validate-boolean-logging-number ()
  "Numeric value for :logging is rejected."
  (let ((result (supervisor--validate-entry '("cmd" :logging 1))))
    (should (stringp result))
    (should (string-match-p ":logging must be t or nil" result))))

(ert-deftest supervisor-test-validate-boolean-no-restart-string ()
  "String value for :no-restart is rejected."
  (let ((result (supervisor--validate-entry '("cmd" :no-restart "no"))))
    (should (stringp result))
    (should (string-match-p ":no-restart must be t or nil" result))))

(ert-deftest supervisor-test-validate-boolean-oneshot-blocking-string ()
  "String value for :oneshot-blocking is rejected."
  (let ((result (supervisor--validate-entry
                 '("cmd" :type oneshot :oneshot-blocking "no"))))
    (should (stringp result))
    (should (string-match-p ":oneshot-blocking must be t or nil" result))))

(ert-deftest supervisor-test-validate-boolean-oneshot-async-string ()
  "String value for :oneshot-async is rejected."
  (let ((result (supervisor--validate-entry
                 '("cmd" :type oneshot :oneshot-async "yes"))))
    (should (stringp result))
    (should (string-match-p ":oneshot-async must be t or nil" result))))

;;; V5: Tags validation tests

(ert-deftest supervisor-test-validate-tags-number ()
  "Numeric :tags value is rejected."
  (let ((result (supervisor--validate-entry '("cmd" :tags 42))))
    (should (stringp result))
    (should (string-match-p ":tags must be a symbol, string, or list" result))))

(ert-deftest supervisor-test-validate-tags-valid-symbol-list ()
  "List of symbols is valid for :tags."
  (should-not (supervisor--validate-entry '("cmd" :tags (web api)))))

(ert-deftest supervisor-test-validate-tags-valid-string-list ()
  "List of strings is valid for :tags."
  (should-not (supervisor--validate-entry '("cmd" :tags ("web" "api")))))

(ert-deftest supervisor-test-validate-tags-empty-string ()
  "Empty string in :tags is rejected."
  (let ((result (supervisor--validate-entry '("cmd" :tags ""))))
    (should (stringp result))
    (should (string-match-p "empty strings" result))))

(ert-deftest supervisor-test-validate-tags-nil-element ()
  "Nil element in :tags list is rejected."
  (let ((result (supervisor--validate-entry '("cmd" :tags (web nil)))))
    (should (stringp result))
    (should (string-match-p "nil values" result))))

;;; V6: Dependency and timeout hardening tests

(ert-deftest supervisor-test-validate-oneshot-timeout-negative ()
  "Negative :oneshot-timeout is rejected."
  (let ((result (supervisor--validate-entry
                 '("cmd" :type oneshot :oneshot-timeout -5))))
    (should (stringp result))
    (should (string-match-p ":oneshot-timeout must be a positive number" result))))

(ert-deftest supervisor-test-validate-oneshot-timeout-zero ()
  "Zero :oneshot-timeout is rejected."
  (let ((result (supervisor--validate-entry
                 '("cmd" :type oneshot :oneshot-timeout 0))))
    (should (stringp result))
    (should (string-match-p ":oneshot-timeout must be a positive number" result))))

(ert-deftest supervisor-test-validate-oneshot-timeout-valid ()
  "Positive :oneshot-timeout passes validation."
  (should-not (supervisor--validate-entry
               '("cmd" :type oneshot :oneshot-timeout 5))))

(ert-deftest supervisor-test-validate-after-empty-string ()
  "Empty string in :after is rejected."
  (let ((result (supervisor--validate-entry '("cmd" :after ""))))
    (should (stringp result))
    (should (string-match-p ":after must not contain empty" result))))

(ert-deftest supervisor-test-validate-requires-empty-string ()
  "Empty string in :requires is rejected."
  (let ((result (supervisor--validate-entry '("cmd" :requires ""))))
    (should (stringp result))
    (should (string-match-p ":requires must not contain empty" result))))

(ert-deftest supervisor-test-validate-before-empty-string ()
  "Empty string in :before is rejected."
  (let ((result (supervisor--validate-entry '("cmd" :before ""))))
    (should (stringp result))
    (should (string-match-p ":before must not contain empty" result))))

(ert-deftest supervisor-test-validate-wants-empty-string ()
  "Empty string in :wants is rejected."
  (let ((result (supervisor--validate-entry '("cmd" :wants ""))))
    (should (stringp result))
    (should (string-match-p ":wants must not contain empty" result))))

(ert-deftest supervisor-test-validate-after-self-dependency ()
  "Self-reference in :after is rejected."
  (let ((result (supervisor--validate-entry '("cmd" :id "a" :after "a"))))
    (should (stringp result))
    (should (string-match-p ":after must not reference the entry's own ID" result))))

(ert-deftest supervisor-test-validate-requires-self-dependency ()
  "Self-reference in :requires is rejected."
  (let ((result (supervisor--validate-entry '("cmd" :id "a" :requires "a"))))
    (should (stringp result))
    (should (string-match-p ":requires must not reference the entry's own ID" result))))

(ert-deftest supervisor-test-validate-before-self-dependency ()
  "Self-reference in :before is rejected."
  (let ((result (supervisor--validate-entry '("cmd" :id "a" :before "a"))))
    (should (stringp result))
    (should (string-match-p ":before must not reference the entry's own ID" result))))

(ert-deftest supervisor-test-validate-wants-self-dependency ()
  "Self-reference in :wants is rejected."
  (let ((result (supervisor--validate-entry '("cmd" :id "a" :wants "a"))))
    (should (stringp result))
    (should (string-match-p ":wants must not reference the entry's own ID" result))))

;;; V7: Cross-keyword contradiction tests

(ert-deftest supervisor-test-validate-restart-sec-with-no-restart-true ()
  "Restart-sec with :no-restart t is contradictory."
  (let ((result (supervisor--validate-entry
                 '("cmd" :no-restart t :restart-sec 5))))
    (should (stringp result))
    (should (string-match-p "contradictory" result))))

(ert-deftest supervisor-test-validate-restart-sec-with-restart-no ()
  "Restart-sec with :restart no is contradictory."
  (let ((result (supervisor--validate-entry
                 '("cmd" :restart no :restart-sec 5))))
    (should (stringp result))
    (should (string-match-p "contradictory" result))))

(ert-deftest supervisor-test-validate-restart-sec-with-restart-nil ()
  "Restart-sec with :restart nil is contradictory."
  (let ((result (supervisor--validate-entry
                 '("cmd" :restart nil :restart-sec 5))))
    (should (stringp result))
    (should (string-match-p "contradictory" result))))

(ert-deftest supervisor-test-validate-restart-sec-with-restart-always ()
  "Restart-sec with :restart always is valid."
  (should-not (supervisor--validate-entry
               '("cmd" :restart always :restart-sec 5))))

(ert-deftest supervisor-test-validate-restart-sec-nil-with-no-restart ()
  "Explicit :restart-sec nil with disabled restart is accepted."
  (should-not (supervisor--validate-entry
               '("cmd" :no-restart t :restart-sec nil))))

;;; V6 supplement: derived-ID self-reference regression

(ert-deftest supervisor-test-validate-after-self-dependency-derived-id ()
  "Self-reference in :after detected via derived command ID."
  (let ((result (supervisor--validate-entry '("svc" :after "svc"))))
    (should (stringp result))
    (should (string-match-p ":after must not reference the entry's own ID" result))))

(ert-deftest supervisor-test-validate-requires-self-dependency-derived-id ()
  "Self-reference in :requires detected via derived command ID."
  (let ((result (supervisor--validate-entry '("svc" :requires "svc"))))
    (should (stringp result))
    (should (string-match-p ":requires must not reference the entry's own ID" result))))

(ert-deftest supervisor-test-validate-before-self-dependency-derived-id ()
  "Self-reference in :before detected via derived command ID."
  (let ((result (supervisor--validate-entry '("svc" :before "svc"))))
    (should (stringp result))
    (should (string-match-p ":before must not reference the entry's own ID" result))))

(ert-deftest supervisor-test-validate-wants-self-dependency-derived-id ()
  "Self-reference in :wants detected via derived command ID."
  (let ((result (supervisor--validate-entry '("svc" :wants "svc"))))
    (should (stringp result))
    (should (string-match-p ":wants must not reference the entry's own ID" result))))

;;; V8: Environment validation tests

(ert-deftest supervisor-test-validate-environment-empty-key ()
  "Empty environment key is rejected."
  (let ((result (supervisor--validate-entry
                 '("cmd" :environment (("" . "val"))))))
    (should (stringp result))
    (should (string-match-p ":environment key .* is not a valid variable name"
                            result))))

(ert-deftest supervisor-test-validate-environment-key-with-space ()
  "Environment key with space is rejected."
  (let ((result (supervisor--validate-entry
                 '("cmd" :environment (("FOO BAR" . "val"))))))
    (should (stringp result))
    (should (string-match-p ":environment key .* is not a valid variable name"
                            result))))

(ert-deftest supervisor-test-validate-environment-key-with-equals ()
  "Environment key with equals sign is rejected."
  (let ((result (supervisor--validate-entry
                 '("cmd" :environment (("FOO=BAR" . "val"))))))
    (should (stringp result))
    (should (string-match-p ":environment key .* is not a valid variable name"
                            result))))

(ert-deftest supervisor-test-validate-environment-valid-key ()
  "Valid environment key is accepted."
  (should-not (supervisor--validate-entry
               '("cmd" :environment (("MY_VAR_1" . "val"))))))

(ert-deftest supervisor-test-validate-environment-duplicate-key ()
  "Duplicate environment key is rejected."
  (let ((result (supervisor--validate-entry
                 '("cmd" :environment (("FOO" . "a") ("FOO" . "b"))))))
    (should (stringp result))
    (should (string-match-p ":environment contains duplicate key" result))))

;;; V9: Exec and exit-status hardening tests

(ert-deftest supervisor-test-validate-exec-stop-empty-string ()
  "Empty :exec-stop string is rejected."
  (let ((result (supervisor--validate-entry '("cmd" :exec-stop ""))))
    (should (stringp result))
    (should (string-match-p ":exec-stop must not contain empty commands" result))))

(ert-deftest supervisor-test-validate-exec-stop-list-with-empty ()
  "List :exec-stop containing empty string is rejected."
  (let ((result (supervisor--validate-entry
                 '("cmd" :exec-stop ("valid" "  ")))))
    (should (stringp result))
    (should (string-match-p ":exec-stop must not contain empty commands" result))))

(ert-deftest supervisor-test-validate-exec-reload-empty-string ()
  "Empty :exec-reload string is rejected."
  (let ((result (supervisor--validate-entry '("cmd" :exec-reload ""))))
    (should (stringp result))
    (should (string-match-p ":exec-reload must not contain empty commands" result))))

(ert-deftest supervisor-test-validate-success-exit-status-negative ()
  "Negative :success-exit-status code is rejected."
  (let ((result (supervisor--validate-entry '("cmd" :success-exit-status (-1)))))
    (should (stringp result))
    (should (string-match-p ":success-exit-status code .* is outside valid range"
                            result))))

(ert-deftest supervisor-test-validate-success-exit-status-over-255 ()
  "Exit code over 255 in :success-exit-status is rejected."
  (let ((result (supervisor--validate-entry '("cmd" :success-exit-status (256)))))
    (should (stringp result))
    (should (string-match-p ":success-exit-status code .* is outside valid range"
                            result))))

(ert-deftest supervisor-test-validate-success-exit-status-valid-boundaries ()
  "Exit codes 0 and 255 in :success-exit-status are accepted."
  (should-not (supervisor--validate-entry
               '("cmd" :success-exit-status (0 255)))))

;;; V10: Unit-file delegation to entry validation tests

(ert-deftest supervisor-test-validate-unit-file-delegates-boolean ()
  "Unit-file validation delegates strict boolean check to entry validator."
  (let ((result (supervisor--validate-unit-file-plist
                 '(:id "svc" :command "cmd" :enabled "yes")
                 "/tmp/test.unit" 1)))
    (should (stringp result))
    (should (string-match-p ":enabled must be t or nil" result))))

(ert-deftest supervisor-test-validate-unit-file-delegates-type-gate ()
  "Unit-file validation delegates type-specific restriction check."
  (let ((result (supervisor--validate-unit-file-plist
                 '(:id "svc" :command "cmd" :type oneshot :restart always)
                 "/tmp/test.unit" 1)))
    (should (stringp result))
    (should (string-match-p ":restart is invalid for :type oneshot" result))))

(ert-deftest supervisor-test-validate-unit-file-delegates-self-dependency ()
  "Unit-file validation delegates self-dependency check."
  (let ((result (supervisor--validate-unit-file-plist
                 '(:id "svc" :command "cmd" :after "svc")
                 "/tmp/test.unit" 1)))
    (should (stringp result))
    (should (string-match-p ":after must not reference the entry's own ID" result))))

(ert-deftest supervisor-test-validate-unit-file-delegates-empty-command ()
  "Unit-file validation delegates empty exec-stop check."
  (let ((result (supervisor--validate-unit-file-plist
                 '(:id "svc" :command "cmd" :exec-stop "")
                 "/tmp/test.unit" 1)))
    (should (stringp result))
    (should (string-match-p ":exec-stop must not contain empty commands" result))))

;;; V11: Coverage sweep gap tests

(ert-deftest supervisor-test-validate-entry-invalid-entry-type ()
  "Non-string non-list entries are rejected."
  (should (string-match-p "entry must be a string or list"
                          (supervisor--validate-entry 123)))
  (should (string-match-p "entry must be a string or list"
                          (supervisor--validate-entry '(123 :id "x"))))
  (should (string-match-p "entry must be a string or list"
                          (supervisor--validate-entry nil))))


(provide 'supervisor-test-validation)
;;; supervisor-test-validation.el ends here
