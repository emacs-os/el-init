;;; supervisor-test-policy.el --- Core policy mutators tests for supervisor.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Core policy mutators ERT tests for supervisor.el.

;;; Code:

(require 'supervisor-test-helpers)

;;; Core Policy Mutator Tests

(ert-deftest supervisor-test-policy-enable-sets-override ()
  "Core enable mutator sets override for disabled entry."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple :enabled nil))
    (let ((supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor--mask-override (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      (let ((result (supervisor--policy-enable "svc")))
        (should (eq 'applied (plist-get result :status)))
        (should (eq 'enabled (gethash "svc" supervisor--enabled-override)))))))

(ert-deftest supervisor-test-policy-enable-skips-already-enabled ()
  "Core enable mutator returns skipped for already-enabled entry."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple))
    (let ((supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor--mask-override (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      (let ((result (supervisor--policy-enable "svc")))
        (should (eq 'skipped (plist-get result :status)))))))

(ert-deftest supervisor-test-policy-enable-clears-stale-override ()
  "Core enable mutator clears override when config default is enabled."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple))
    (let ((supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor--mask-override (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      ;; Disable via override, then re-enable
      (puthash "svc" 'disabled supervisor--enabled-override)
      (let ((result (supervisor--policy-enable "svc")))
        (should (eq 'applied (plist-get result :status)))
        ;; Override cleared (config default is enabled)
        (should-not (gethash "svc" supervisor--enabled-override))))))

(ert-deftest supervisor-test-policy-disable-normalization ()
  "Core disable mutator clears override when config default is disabled."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple :enabled nil))
    (let ((supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor--mask-override (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      ;; Enable via override, then disable
      (puthash "svc" 'enabled supervisor--enabled-override)
      (let ((result (supervisor--policy-disable "svc")))
        (should (eq 'applied (plist-get result :status)))
        ;; Override cleared (config default is disabled)
        (should-not (gethash "svc" supervisor--enabled-override))))))

(ert-deftest supervisor-test-policy-enable-rejects-invalid ()
  "Core enable mutator rejects invalid entries."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple))
    (let ((supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor--mask-override (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      (puthash "svc" "bad type" supervisor--invalid)
      (let ((result (supervisor--policy-enable "svc")))
        (should (eq 'error (plist-get result :status)))))))

(ert-deftest supervisor-test-policy-enable-rejects-unknown ()
  "Core enable mutator rejects unknown entry ID."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple))
    (let ((supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor--mask-override (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      (let ((result (supervisor--policy-enable "nonexistent")))
        (should (eq 'error (plist-get result :status)))
        (should (string-match-p "Unknown" (plist-get result :message)))))))

(ert-deftest supervisor-test-policy-mask-idempotent ()
  "Core mask mutator returns skipped when already masked."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple))
    (let ((supervisor--mask-override (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      (puthash "svc" 'masked supervisor--mask-override)
      (let ((result (supervisor--policy-mask "svc")))
        (should (eq 'skipped (plist-get result :status)))))))

(ert-deftest supervisor-test-policy-unmask-idempotent ()
  "Core unmask mutator returns skipped when not masked."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple))
    (let ((supervisor--mask-override (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      (let ((result (supervisor--policy-unmask "svc")))
        (should (eq 'skipped (plist-get result :status)))))))

(ert-deftest supervisor-test-policy-restart-rejects-oneshot ()
  "Core restart-policy mutator rejects oneshot entries."
  (supervisor-test-with-unit-files
      '(("true" :id "svc" :type oneshot))
    (let ((supervisor--restart-override (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      (let ((result (supervisor--policy-set-restart "svc" 'always)))
        (should (eq 'error (plist-get result :status)))
        (should (string-match-p "oneshot" (plist-get result :message)))))))

(ert-deftest supervisor-test-policy-restart-normalizes-config ()
  "Core restart-policy mutator clears override matching config."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple :restart t))
    (let ((supervisor--restart-override (make-hash-table :test 'equal))
          (supervisor--restart-timers (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      ;; Setting to 'always matches config default (t normalizes to always)
      (let ((result (supervisor--policy-set-restart "svc" 'always)))
        (should (eq 'applied (plist-get result :status)))
        ;; Override cleared since it matches config
        (should-not (gethash "svc" supervisor--restart-override))))))

(ert-deftest supervisor-test-policy-logging-normalizes-config ()
  "Core logging mutator clears override matching config default."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple))
    (let ((supervisor--logging (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      ;; Config default is logging=t; setting on matches
      (let ((result (supervisor--policy-set-logging "svc" t)))
        (should (eq 'applied (plist-get result :status)))
        (should-not (gethash "svc" supervisor--logging))))))

(ert-deftest supervisor-test-cli-policy-batch-reports-errors ()
  "CLI policy batch reports errors for unknown entries."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple :enabled nil))
    (let ((supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      (let ((result (supervisor--cli-dispatch
                     '("enable" "svc" "bogus" "--json"))))
        ;; Should fail because of unknown entry
        (should (= supervisor-cli-exit-failure
                    (supervisor-cli-result-exitcode result)))
        ;; But svc should still be enabled
        (should (eq 'enabled
                    (gethash "svc" supervisor--enabled-override)))))))


(provide 'supervisor-test-policy)
;;; supervisor-test-policy.el ends here
