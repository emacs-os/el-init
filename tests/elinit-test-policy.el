;;; elinit-test-policy.el --- Core policy mutators tests for elinit.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Core policy mutators ERT tests for elinit.el.

;;; Code:

(require 'elinit-test-helpers)

;;; Core Policy Mutator Tests

(ert-deftest elinit-test-policy-enable-sets-override ()
  "Core enable mutator sets override for disabled entry."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple :enabled nil))
    (let ((elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      (let ((result (elinit--policy-enable "svc")))
        (should (eq 'applied (plist-get result :status)))
        (should (eq 'enabled (gethash "svc" elinit--enabled-override)))))))

(ert-deftest elinit-test-policy-enable-skips-already-enabled ()
  "Core enable mutator returns skipped for already-enabled entry."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple))
    (let ((elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      (let ((result (elinit--policy-enable "svc")))
        (should (eq 'skipped (plist-get result :status)))))))

(ert-deftest elinit-test-policy-enable-clears-stale-override ()
  "Core enable mutator clears override when config default is enabled."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple))
    (let ((elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      ;; Disable via override, then re-enable
      (puthash "svc" 'disabled elinit--enabled-override)
      (let ((result (elinit--policy-enable "svc")))
        (should (eq 'applied (plist-get result :status)))
        ;; Override cleared (config default is enabled)
        (should-not (gethash "svc" elinit--enabled-override))))))

(ert-deftest elinit-test-policy-disable-normalization ()
  "Core disable mutator clears override when config default is disabled."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple :enabled nil))
    (let ((elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      ;; Enable via override, then disable
      (puthash "svc" 'enabled elinit--enabled-override)
      (let ((result (elinit--policy-disable "svc")))
        (should (eq 'applied (plist-get result :status)))
        ;; Override cleared (config default is disabled)
        (should-not (gethash "svc" elinit--enabled-override))))))

(ert-deftest elinit-test-policy-enable-rejects-invalid ()
  "Core enable mutator rejects invalid entries."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple))
    (let ((elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      (puthash "svc" "bad type" elinit--invalid)
      (let ((result (elinit--policy-enable "svc")))
        (should (eq 'error (plist-get result :status)))))))

(ert-deftest elinit-test-policy-enable-rejects-unknown ()
  "Core enable mutator rejects unknown entry ID."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple))
    (let ((elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      (let ((result (elinit--policy-enable "nonexistent")))
        (should (eq 'error (plist-get result :status)))
        (should (string-match-p "Unknown" (plist-get result :message)))))))

(ert-deftest elinit-test-policy-mask-idempotent ()
  "Core mask mutator returns skipped when already masked."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple))
    (let ((elinit--mask-override (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      (puthash "svc" 'masked elinit--mask-override)
      (let ((result (elinit--policy-mask "svc")))
        (should (eq 'skipped (plist-get result :status)))))))

(ert-deftest elinit-test-policy-unmask-idempotent ()
  "Core unmask mutator returns skipped when not masked."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple))
    (let ((elinit--mask-override (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      (let ((result (elinit--policy-unmask "svc")))
        (should (eq 'skipped (plist-get result :status)))))))

(ert-deftest elinit-test-policy-restart-rejects-oneshot ()
  "Core restart-policy mutator rejects oneshot entries."
  (elinit-test-with-unit-files
      '(("true" :id "svc" :type oneshot))
    (let ((elinit--restart-override (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      (let ((result (elinit--policy-set-restart "svc" 'always)))
        (should (eq 'error (plist-get result :status)))
        (should (string-match-p "oneshot" (plist-get result :message)))))))

(ert-deftest elinit-test-policy-restart-normalizes-config ()
  "Core restart-policy mutator clears override matching config."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple :restart t))
    (let ((elinit--restart-override (make-hash-table :test 'equal))
          (elinit--restart-timers (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      ;; Setting to 'always matches config default (t normalizes to always)
      (let ((result (elinit--policy-set-restart "svc" 'always)))
        (should (eq 'applied (plist-get result :status)))
        ;; Override cleared since it matches config
        (should-not (gethash "svc" elinit--restart-override))))))

(ert-deftest elinit-test-policy-logging-normalizes-config ()
  "Core logging mutator clears override matching config default."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple))
    (let ((elinit--logging (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      ;; Config default is logging=t; setting on matches
      (let ((result (elinit--policy-set-logging "svc" t)))
        (should (eq 'applied (plist-get result :status)))
        (should-not (gethash "svc" elinit--logging))))))

(ert-deftest elinit-test-cli-policy-batch-reports-errors ()
  "CLI policy batch reports errors for unknown entries."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple :enabled nil))
    (let ((elinit--enabled-override (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      (let ((result (elinit--cli-dispatch
                     '("enable" "svc" "bogus" "--json"))))
        ;; Should fail because of unknown entry
        (should (= elinit-cli-exit-failure
                    (elinit-cli-result-exitcode result)))
        ;; But svc should still be enabled
        (should (eq 'enabled
                    (gethash "svc" elinit--enabled-override)))))))


(provide 'elinit-test-policy)
;;; elinit-test-policy.el ends here
