;;; elinit-test-overrides.el --- Overrides persistence and policy tests for elinit.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the elinit-overrides module: override persistence,
;; effective state getters, and policy mutators.

;;; Code:

(require 'elinit-test-helpers)

;;; Enable/Disable Policy Tests

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
  "Core enable mutator clears override when config default is enabled.
When the config already says enabled, the override is redundant
and must be cleared to keep the override file minimal."
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
  "Core disable mutator clears override when config default is disabled.
Mirror of enable normalization: redundant overrides are removed."
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

;;; Mask Policy Tests

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

;;; Restart/Logging Policy Tests

(ert-deftest elinit-test-policy-restart-rejects-oneshot ()
  "Core restart-policy mutator rejects oneshot entries.
Oneshots run once and exit -- restart policy is meaningless for them."
  (elinit-test-with-unit-files
      '(("true" :id "svc" :type oneshot))
    (let ((elinit--restart-override (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      (let ((result (elinit--policy-set-restart "svc" 'always)))
        (should (eq 'error (plist-get result :status)))
        (should (string-match-p "oneshot" (plist-get result :message)))))))

(ert-deftest elinit-test-policy-restart-normalizes-config ()
  "Core restart-policy mutator clears override matching config.
When the requested policy matches the config default, the override
is redundant and must be cleared."
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
  "Core logging mutator clears override matching config default.
Mirrors restart normalization: redundant overrides are removed."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple))
    (let ((elinit--logging (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      ;; Config default is logging=t; setting on matches
      (let ((result (elinit--policy-set-logging "svc" t)))
        (should (eq 'applied (plist-get result :status)))
        (should-not (gethash "svc" elinit--logging))))))

;;; Restart Override Tests

(ert-deftest elinit-test-overrides-load-migrates-legacy-restart ()
  "Loading overrides with legacy enabled/disabled migrates to policy symbols."
  (let* ((temp-file (make-temp-file "elinit-test-migrate-" nil ".eld"))
         (elinit-overrides-file temp-file)
         (elinit--enabled-override (make-hash-table :test 'equal))
         (elinit--restart-override (make-hash-table :test 'equal))
         (elinit--logging (make-hash-table :test 'equal))
         (elinit--mask-override (make-hash-table :test 'equal))
         (elinit--overrides-loaded nil))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert ";; test overrides\n")
            (prin1 `((version . 1)
                     (timestamp . "2025-01-01T00:00:00+0000")
                     (overrides . (("svc-a" :restart enabled)
                                   ("svc-b" :restart disabled))))
                   (current-buffer)))
          (should (elinit--load-overrides))
          (should (eq 'always (gethash "svc-a" elinit--restart-override)))
          (should (eq 'no (gethash "svc-b" elinit--restart-override))))
      (delete-file temp-file))))

(ert-deftest elinit-test-get-effective-restart-policy ()
  "Effective restart returns policy symbols with override migration."
  (let ((elinit--restart-override (make-hash-table :test 'equal)))
    ;; No override: return config value
    (should (eq 'always (elinit--get-effective-restart "svc" 'always)))
    (should (eq 'on-failure (elinit--get-effective-restart "svc" 'on-failure)))
    ;; Legacy boolean config: normalized
    (should (eq 'always (elinit--get-effective-restart "svc" t)))
    (should (eq 'no (elinit--get-effective-restart "svc" nil)))
    ;; Override with policy symbol
    (puthash "svc" 'no elinit--restart-override)
    (should (eq 'no (elinit--get-effective-restart "svc" 'always)))
    ;; Override with legacy enabled/disabled
    (puthash "svc" 'enabled elinit--restart-override)
    (should (eq 'always (elinit--get-effective-restart "svc" 'no)))
    (puthash "svc" 'disabled elinit--restart-override)
    (should (eq 'no (elinit--get-effective-restart "svc" 'always)))))

(provide 'elinit-test-overrides)
;;; elinit-test-overrides.el ends here
