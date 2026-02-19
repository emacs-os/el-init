;;; elinit-test-dag.el --- DAG scheduler and integration tests for elinit.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; DAG scheduler and integration ERT tests for elinit.el.

;;; Code:

(require 'elinit-test-helpers)

;;; Integration tests

(ert-deftest elinit-test-verify-populates-invalid-hash ()
  "elinit-verify populates elinit--invalid hash table."
  (elinit-test-with-unit-files
      '(("valid-entry" :id "valid-entry" :type simple)
        ("invalid-entry" :id "invalid-entry" :type "bad"))
    (let ((elinit--invalid (make-hash-table :test 'equal)))
      (with-temp-buffer
        (let ((standard-output (current-buffer)))
          (elinit-verify)))
      (should (null (gethash "valid-entry" elinit--invalid)))
      (should (gethash "invalid-entry" elinit--invalid)))))

(ert-deftest elinit-test-all-parsed-entries-skips-invalid ()
  "elinit--all-parsed-entries skips invalid entries."
  (elinit-test-with-unit-files
      '(("valid" :id "valid" :type simple)
        ("invalid" :id "invalid" :type "bad")
        ("also-valid" :id "also-valid" :type oneshot))
    (let ((elinit--invalid (make-hash-table :test 'equal)))
      (let ((entries (elinit--all-parsed-entries)))
        ;; Should have 2 valid entries
        (should (= (length entries) 2))
        ;; Invalid should be tracked
        (should (gethash "invalid" elinit--invalid))
        ;; Valid entries should be in result
        (should (cl-find "valid" entries :key #'car :test #'equal))
        (should (cl-find "also-valid" entries :key #'car :test #'equal))))))

(ert-deftest elinit-test-verify-handles-malformed-entry ()
  "elinit-verify handles malformed unit files gracefully."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit--programs-cache :not-yet-loaded)
         (elinit--unit-file-invalid (make-hash-table :test 'equal))
         (elinit--invalid (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; Write a valid unit file and a malformed (non-plist) file
          (with-temp-file (expand-file-name "valid.el" dir)
            (insert "(:id \"valid\" :command \"true\" :type simple)"))
          (with-temp-file (expand-file-name "broken.el" dir)
            (insert "42"))
          (with-temp-buffer
            (let ((standard-output (current-buffer)))
              (elinit-verify)))
          ;; Malformed file should be tracked in invalid
          (should (gethash "broken" elinit--invalid))
          ;; Valid entry should not be in invalid
          (should (null (gethash "valid" elinit--invalid))))
      (delete-directory dir t))))

;;; DAG scheduler tests

(ert-deftest elinit-test-topo-sort-stable-ordering ()
  "Unconstrained nodes maintain original list order."
  (let* ((entries '(("a" "cmd" 0 t always t simple nil t 30)
                    ("b" "cmd" 0 t always t simple nil t 30)
                    ("c" "cmd" 0 t always t simple nil t 30)))
         (sorted (elinit--stable-topo-sort entries)))
    ;; With no dependencies, order should be preserved
    (should (equal (mapcar #'car sorted) '("a" "b" "c")))))

(ert-deftest elinit-test-topo-sort-respects-after ()
  "Entries with :after come after their dependencies."
  (let* ((entries '(("c" "cmd" 0 t always t simple ("a") t 30)
                    ("a" "cmd" 0 t always t simple nil t 30)
                    ("b" "cmd" 0 t always t simple nil t 30)))
         (sorted (elinit--stable-topo-sort entries))
         (order (mapcar #'car sorted)))
    ;; "a" must come before "c" (dependency constraint)
    (should (< (cl-position "a" order :test #'equal)
               (cl-position "c" order :test #'equal)))
    ;; Stable sort: after "a" emits, both "c" (idx 0) and "b" (idx 2) are ready
    ;; "c" comes first because it has lower original index
    (should (equal order '("a" "c" "b")))))

(ert-deftest elinit-test-topo-sort-cycle-fallback ()
  "Cycle detection returns list order with :after cleared."
  (let* ((entries '(("a" "cmd" 0 t always t simple ("b") t 30)
                    ("b" "cmd" 0 t always t simple ("a") t 30)))
         (sorted (elinit--stable-topo-sort entries)))
    ;; Should return in original order
    (should (equal (mapcar #'car sorted) '("a" "b")))
    ;; :after (index 7) should be nil for all entries
    (should (null (nth 7 (car sorted))))
    (should (null (nth 7 (cadr sorted))))))

(ert-deftest elinit-test-topo-sort-complex-dag ()
  "Complex DAG with multiple dependencies."
  ;; d depends on b and c, b depends on a
  (let* ((entries '(("a" "cmd" 0 t always t simple nil t 30)
                    ("b" "cmd" 0 t always t simple ("a") t 30)
                    ("c" "cmd" 0 t always t simple nil t 30)
                    ("d" "cmd" 0 t always t simple ("b" "c") t 30)))
         (sorted (elinit--stable-topo-sort entries))
         (order (mapcar #'car sorted)))
    ;; a must come before b
    (should (< (cl-position "a" order :test #'equal)
               (cl-position "b" order :test #'equal)))
    ;; b and c must come before d
    (should (< (cl-position "b" order :test #'equal)
               (cl-position "d" order :test #'equal)))
    (should (< (cl-position "c" order :test #'equal)
               (cl-position "d" order :test #'equal)))))

(ert-deftest elinit-test-dag-init-blocking-oneshot ()
  "Blocking oneshots are tracked in elinit--dag-blocking."
  (let ((elinit--dag-blocking nil)
        (elinit--dag-in-degree nil)
        (elinit--dag-dependents nil)
        (elinit--dag-entries nil)
        (elinit--dag-started nil)
        (elinit--dag-ready nil)
        (elinit--dag-timeout-timers nil)
        (elinit--dag-delay-timers nil)
        (elinit--dag-id-to-index nil))
    ;; Entry: (id cmd delay enabled-p restart-policy logging-p type after oneshot-blocking oneshot-timeout)
    (let ((entries '(("blocking" "cmd" 0 t always t oneshot nil t 30)
                     ("non-blocking" "cmd" 0 t always t oneshot nil nil 30)
                     ("simple" "cmd" 0 t always t simple nil t 30))))
      (elinit--dag-init entries)
      ;; Blocking oneshot should be in blocking set
      (should (gethash "blocking" elinit--dag-blocking))
      ;; Non-blocking oneshot should NOT be in blocking set
      (should-not (gethash "non-blocking" elinit--dag-blocking))
      ;; Simple process should NOT be in blocking set
      (should-not (gethash "simple" elinit--dag-blocking)))))

(ert-deftest elinit-test-dag-init-in-degree ()
  "DAG init correctly calculates in-degree from :after."
  (let ((elinit--dag-blocking nil)
        (elinit--dag-in-degree nil)
        (elinit--dag-dependents nil)
        (elinit--dag-entries nil)
        (elinit--dag-started nil)
        (elinit--dag-ready nil)
        (elinit--dag-timeout-timers nil)
        (elinit--dag-delay-timers nil)
        (elinit--dag-id-to-index nil))
    (let ((entries '(("a" "cmd" 0 t always t simple nil t 30)
                     ("b" "cmd" 0 t always t simple ("a") t 30)
                     ("c" "cmd" 0 t always t simple ("a" "b") t 30))))
      (elinit--dag-init entries)
      ;; a has no dependencies
      (should (= (gethash "a" elinit--dag-in-degree) 0))
      ;; b depends on a
      (should (= (gethash "b" elinit--dag-in-degree) 1))
      ;; c depends on a and b
      (should (= (gethash "c" elinit--dag-in-degree) 2)))))

(ert-deftest elinit-test-dag-init-dependents ()
  "DAG init correctly builds dependents graph."
  (let ((elinit--dag-blocking nil)
        (elinit--dag-in-degree nil)
        (elinit--dag-dependents nil)
        (elinit--dag-entries nil)
        (elinit--dag-started nil)
        (elinit--dag-ready nil)
        (elinit--dag-timeout-timers nil)
        (elinit--dag-delay-timers nil)
        (elinit--dag-id-to-index nil))
    (let ((entries '(("a" "cmd" 0 t always t simple nil t 30)
                     ("b" "cmd" 0 t always t simple ("a") t 30)
                     ("c" "cmd" 0 t always t simple ("a") t 30))))
      (elinit--dag-init entries)
      ;; a should have b and c as dependents
      (let ((deps (gethash "a" elinit--dag-dependents)))
        (should (member "b" deps))
        (should (member "c" deps))))))

(ert-deftest elinit-test-dag-disabled-entry-ready-immediately ()
  "Disabled entries are ready immediately and don't block dependents."
  (let ((elinit--dag-blocking (make-hash-table :test 'equal))
        (elinit--dag-in-degree (make-hash-table :test 'equal))
        (elinit--dag-dependents (make-hash-table :test 'equal))
        (elinit--dag-entries (make-hash-table :test 'equal))
        (elinit--dag-started (make-hash-table :test 'equal))
        (elinit--dag-ready (make-hash-table :test 'equal))
        (elinit--dag-timeout-timers (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (elinit--dag-id-to-index (make-hash-table :test 'equal))
        (elinit--entry-state (make-hash-table :test 'equal))
        (elinit--ready-times (make-hash-table :test 'equal)))
    ;; "a" is disabled, "b" depends on "a"
    ;;           (id   cmd   delay enabled-p restart-policy logging-p type   after oneshot-blocking timeout)
    (let ((entries '(("a" "cmd" 0 nil always t simple nil t 30)
                     ("b" "cmd" 0 t   always t simple ("a") t 30))))
      (elinit--dag-init entries)
      ;; Disabled entry should be marked ready immediately
      (should (gethash "a" elinit--dag-ready))
      (should (gethash "a" elinit--dag-started))
      ;; Entry state should be 'disabled
      (should (eq (gethash "a" elinit--entry-state) 'disabled))
      ;; Dependent "b" should have in-degree 0 (not blocked by disabled "a")
      (should (= 0 (gethash "b" elinit--dag-in-degree))))))

(ert-deftest elinit-test-async-oneshot-not-blocking ()
  "Async oneshots (oneshot-blocking nil) do not block convergence."
  (let ((elinit--dag-blocking nil)
        (elinit--dag-in-degree nil)
        (elinit--dag-dependents nil)
        (elinit--dag-entries nil)
        (elinit--dag-started nil)
        (elinit--dag-ready nil)
        (elinit--dag-timeout-timers nil)
        (elinit--dag-delay-timers nil)
        (elinit--dag-id-to-index nil))
    ;; Entry with :oneshot-async t means oneshot-blocking = nil
    (let ((entries '(("async-oneshot" "cmd" 0 t always t oneshot nil nil 30))))
      (elinit--dag-init entries)
      ;; Async oneshot should NOT be in blocking set
      (should-not (gethash "async-oneshot" elinit--dag-blocking)))))

(ert-deftest elinit-test-startup-complete-blocked-by-delay ()
  "Delayed entries prevent startup completion until they start."
  (let ((elinit--dag-blocking (make-hash-table :test 'equal))
        (elinit--dag-in-degree (make-hash-table :test 'equal))
        (elinit--dag-dependents (make-hash-table :test 'equal))
        (elinit--dag-entries (make-hash-table :test 'equal))
        (elinit--dag-started (make-hash-table :test 'equal))
        (elinit--dag-ready (make-hash-table :test 'equal))
        (elinit--dag-timeout-timers (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (elinit--dag-id-to-index (make-hash-table :test 'equal))
        (elinit--dag-complete-callback nil)
        (elinit--shutting-down nil)
        (callback-called nil))
    ;; Entry with delay
    (puthash "delayed" '("delayed" "echo hi" 5 t t t simple nil t 30) elinit--dag-entries)
    (puthash "delayed" 0 elinit--dag-in-degree)
    (puthash "delayed" 0 elinit--dag-id-to-index)
    (puthash "delayed" nil elinit--dag-dependents)
    (setq elinit--dag-complete-callback (lambda () (setq callback-called t)))
    ;; Simulate starting the delayed entry - adds to delay-timers
    (puthash "delayed" 'mock-timer elinit--dag-delay-timers)
    ;; Mark as "started" from scheduler's perspective
    (puthash "delayed" t elinit--dag-started)
    ;; Try to complete startup - should NOT call callback because delay timer exists
    (elinit--dag-check-complete)
    (should-not callback-called)))

(ert-deftest elinit-test-startup-complete-blocked-by-blocking-oneshot ()
  "Blocking oneshots prevent startup completion until they exit."
  (let ((elinit--dag-blocking (make-hash-table :test 'equal))
        (elinit--dag-in-degree (make-hash-table :test 'equal))
        (elinit--dag-dependents (make-hash-table :test 'equal))
        (elinit--dag-entries (make-hash-table :test 'equal))
        (elinit--dag-started (make-hash-table :test 'equal))
        (elinit--dag-ready (make-hash-table :test 'equal))
        (elinit--dag-timeout-timers (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (elinit--dag-id-to-index (make-hash-table :test 'equal))
        (elinit--dag-complete-callback nil)
        (callback-called nil))
    ;; Blocking oneshot entry
    (puthash "blocking" '("blocking" "sleep 10" 0 t always t oneshot nil t 30) elinit--dag-entries)
    (puthash "blocking" 0 elinit--dag-in-degree)
    (puthash "blocking" t elinit--dag-started)
    ;; Oneshot is blocking
    (puthash "blocking" t elinit--dag-blocking)
    (setq elinit--dag-complete-callback (lambda () (setq callback-called t)))
    ;; Try to complete startup - should NOT call callback because blocking oneshot exists
    (elinit--dag-check-complete)
    (should-not callback-called)))

(ert-deftest elinit-test-mark-ready-removes-from-blocking ()
  "elinit--dag-mark-ready removes entry from blocking set."
  (let ((elinit--dag-blocking (make-hash-table :test 'equal))
        (elinit--dag-in-degree (make-hash-table :test 'equal))
        (elinit--dag-dependents (make-hash-table :test 'equal))
        (elinit--dag-entries (make-hash-table :test 'equal))
        (elinit--dag-started (make-hash-table :test 'equal))
        (elinit--dag-ready (make-hash-table :test 'equal))
        (elinit--dag-timeout-timers (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (elinit--dag-id-to-index (make-hash-table :test 'equal))
        (elinit--dag-complete-callback nil)
        (elinit-verbose nil))
    ;; Set up blocking oneshot
    (puthash "oneshot" '("oneshot" "cmd" 0 t always t oneshot nil t 30) elinit--dag-entries)
    (puthash "oneshot" 0 elinit--dag-in-degree)
    (puthash "oneshot" t elinit--dag-started)
    (puthash "oneshot" t elinit--dag-blocking)
    (puthash "oneshot" nil elinit--dag-dependents)
    ;; Mark ready
    (elinit--dag-mark-ready "oneshot")
    ;; Should be removed from blocking
    (should-not (gethash "oneshot" elinit--dag-blocking))
    ;; Should be in ready set
    (should (gethash "oneshot" elinit--dag-ready))))

(ert-deftest elinit-test-mark-ready-unlocks-dependents ()
  "elinit--dag-mark-ready decrements in-degree for dependents."
  (let ((elinit--dag-blocking (make-hash-table :test 'equal))
        (elinit--dag-in-degree (make-hash-table :test 'equal))
        (elinit--dag-dependents (make-hash-table :test 'equal))
        (elinit--dag-entries (make-hash-table :test 'equal))
        (elinit--dag-started (make-hash-table :test 'equal))
        (elinit--dag-ready (make-hash-table :test 'equal))
        (elinit--dag-timeout-timers (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (elinit--dag-id-to-index (make-hash-table :test 'equal))
        (elinit--dag-complete-callback nil)
        (elinit--shutting-down nil)
        (elinit-verbose nil)
        (started-ids nil))
    ;; Stub start function to prevent actual process spawning
    (cl-letf (((symbol-function 'elinit--dag-start-entry-async)
               (lambda (entry) (push (car entry) started-ids))))
      ;; Set up: b depends on a
      (puthash "a" '("a" "cmd" 0 t always t simple nil t 30) elinit--dag-entries)
      (puthash "b" '("b" "cmd" 0 t always t simple ("a") t 30) elinit--dag-entries)
      (puthash "a" 0 elinit--dag-in-degree)
      (puthash "b" 1 elinit--dag-in-degree)
      (puthash "a" t elinit--dag-started)
      (puthash "b" nil elinit--dag-started)
      (puthash "a" '("b") elinit--dag-dependents)
      (puthash "b" nil elinit--dag-dependents)
      (puthash "a" 0 elinit--dag-id-to-index)
      (puthash "b" 1 elinit--dag-id-to-index)
      ;; Mark a as ready
      (elinit--dag-mark-ready "a")
      ;; b's in-degree should now be 0
      (should (= 0 (gethash "b" elinit--dag-in-degree)))
      ;; b should have been triggered to start
      (should (member "b" started-ids)))))

(ert-deftest elinit-test-oneshot-timeout-unlocks-dependents ()
  "Oneshot timeout calls mark-ready which unlocks dependents and convergence."
  (let ((elinit--dag-blocking (make-hash-table :test 'equal))
        (elinit--dag-in-degree (make-hash-table :test 'equal))
        (elinit--dag-dependents (make-hash-table :test 'equal))
        (elinit--dag-entries (make-hash-table :test 'equal))
        (elinit--dag-started (make-hash-table :test 'equal))
        (elinit--dag-ready (make-hash-table :test 'equal))
        (elinit--dag-timeout-timers (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (elinit--dag-id-to-index (make-hash-table :test 'equal))
        (elinit--dag-complete-callback nil)
        (elinit--shutting-down nil)
        (elinit-verbose nil)
        (startup-complete nil)
        (started-ids nil))
    ;; Stub start function
    (cl-letf (((symbol-function 'elinit--dag-start-entry-async)
               (lambda (entry) (push (car entry) started-ids))))
      ;; Set up: blocking oneshot "slow" with dependent "after-slow"
      (puthash "slow" '("slow" "sleep 999" 0 t always t oneshot nil t 5) elinit--dag-entries)
      (puthash "after-slow" '("after-slow" "echo done" 0 t always t simple ("slow") t 30) elinit--dag-entries)
      (puthash "slow" 0 elinit--dag-in-degree)
      (puthash "after-slow" 1 elinit--dag-in-degree)
      (puthash "slow" t elinit--dag-started)
      (puthash "after-slow" nil elinit--dag-started)
      (puthash "slow" t elinit--dag-blocking)  ; blocking oneshot
      (puthash "slow" '("after-slow") elinit--dag-dependents)
      (puthash "after-slow" nil elinit--dag-dependents)
      (puthash "slow" 0 elinit--dag-id-to-index)
      (puthash "after-slow" 1 elinit--dag-id-to-index)
      ;; Set up a mock timeout timer
      (puthash "slow" 'mock-timer elinit--dag-timeout-timers)
      ;; Startup completion callback
      (setq elinit--dag-complete-callback
            (lambda () (setq startup-complete t)))
      ;; Simulate timeout firing: this is what the timeout timer does
      (elinit--dag-mark-ready "slow")
      ;; Blocking oneshot should be removed from blocking set
      (should-not (gethash "slow" elinit--dag-blocking))
      ;; Dependent should have been unlocked and triggered
      (should (= 0 (gethash "after-slow" elinit--dag-in-degree)))
      (should (member "after-slow" started-ids))
      ;; Timeout timer should be removed
      (should-not (gethash "slow" elinit--dag-timeout-timers)))))


;;; P1 behavior tests

(ert-deftest elinit-test-max-concurrent-starts-active-count-no-leak ()
  "Active count must not leak when entries are processed from queue."
  ;; The fix ensures elinit--dag-process-pending-starts does NOT
  ;; increment the count - only elinit--dag-do-start does.
  (let ((elinit--dag-pending-starts nil)
        (elinit--dag-active-starts 0)
        (elinit--dag-entries (make-hash-table :test 'equal))
        (elinit--dag-started (make-hash-table :test 'equal))
        (elinit--dag-ready (make-hash-table :test 'equal))
        (elinit--dag-blocking (make-hash-table :test 'equal))
        (elinit--dag-in-degree (make-hash-table :test 'equal))
        (elinit--dag-dependents (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (elinit--dag-timeout-timers (make-hash-table :test 'equal))
        (elinit--entry-state (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--ready-times (make-hash-table :test 'equal))
        (elinit--shutting-down nil)
        (elinit-max-concurrent-starts 2)
        (started-ids nil))
    ;; Queue entries
    (puthash "a" '("a" "true" 0 t always t simple nil t 30) elinit--dag-entries)
    (puthash "b" '("b" "true" 0 t always t simple nil t 30) elinit--dag-entries)
    (setq elinit--dag-pending-starts '("a" "b"))
    ;; Before processing, count should be 0
    (should (= elinit--dag-active-starts 0))
    ;; Stub elinit--dag-start-entry-async to track calls without spawning
    (cl-letf (((symbol-function 'elinit--dag-start-entry-async)
               (lambda (entry) (push (car entry) started-ids))))
      ;; Call the real function
      (elinit--dag-process-pending-starts)
      ;; Queue should be drained
      (should (null elinit--dag-pending-starts))
      ;; Both entries should have been passed to start-entry-async
      (should (member "a" started-ids))
      (should (member "b" started-ids))
      ;; Active count should still be 0 (no increment in process-pending-starts)
      (should (= elinit--dag-active-starts 0)))))

(ert-deftest elinit-test-enabled-override-affects-effective-enabled ()
  "Runtime enable override affects effective enabled state."
  (let ((elinit--enabled-override (make-hash-table :test 'equal)))
    ;; No override: config enabled-p applies
    (should (elinit--get-effective-enabled "a" t))
    (should-not (elinit--get-effective-enabled "b" nil))
    ;; Override to disabled: entry is disabled regardless of config
    (puthash "a" 'disabled elinit--enabled-override)
    (should-not (elinit--get-effective-enabled "a" t))
    ;; Override to enabled: entry is enabled regardless of config
    (puthash "b" 'enabled elinit--enabled-override)
    (should (elinit--get-effective-enabled "b" nil))))

(ert-deftest elinit-test-startup-timeout-sets-entry-state ()
  "Startup timeout marks unstarted entries with startup-timeout state."
  (let ((elinit--dag-entries (make-hash-table :test 'equal))
        (elinit--dag-started (make-hash-table :test 'equal))
        (elinit--dag-blocking (make-hash-table :test 'equal))
        (elinit--dag-in-degree (make-hash-table :test 'equal))
        (elinit--dag-dependents (make-hash-table :test 'equal))
        (elinit--dag-ready (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (elinit--dag-timeout-timers (make-hash-table :test 'equal))
        (elinit--dag-id-to-index (make-hash-table :test 'equal))
        (elinit--dag-complete-callback nil)
        (elinit--dag-timeout-timer nil)
        (elinit--dag-pending-starts nil)
        (elinit--dag-active-starts 0)
        (elinit--entry-state (make-hash-table :test 'equal))
        (callback-called nil))
    ;; Set up entry that hasn't started
    (puthash "delayed" '("delayed" "cmd" 5 t t t simple nil t 30)
             elinit--dag-entries)
    (setq elinit--dag-complete-callback (lambda () (setq callback-called t)))
    ;; Force complete should mark unstarted as startup-timeout
    (elinit--dag-force-complete)
    (should (eq (gethash "delayed" elinit--entry-state) 'startup-timeout))
    (should callback-called)))

(ert-deftest elinit-test-format-exit-status-signal ()
  "Exit status formatting distinguishes signal from exit."
  ;; Signal case
  (should (string-match "killed by signal 15"
                        (elinit--format-exit-status 'signal 15)))
  ;; Exit with code 0
  (should (string-match "exited successfully"
                        (elinit--format-exit-status 'exit 0)))
  ;; Exit with non-zero code
  (should (string-match "exited with code 1"
                        (elinit--format-exit-status 'exit 1))))

(ert-deftest elinit-test-reconcile-respects-enabled-override ()
  "Reconcile uses effective enabled state for start decisions."
  (elinit-test-with-unit-files
      '(("true" :id "new-entry" :type simple))
    (let ((elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--restart-override (make-hash-table :test 'equal))
          (elinit--logging (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (started-ids nil))
      ;; Mark entry as runtime-disabled
      (puthash "new-entry" 'disabled elinit--enabled-override)
      ;; Mock elinit--start-process to track what gets started
      (cl-letf (((symbol-function 'elinit--start-process)
                 (lambda (id _cmd _log _type _restart &rest _args)
                   (push id started-ids)))
                ((symbol-function 'elinit--refresh-dashboard) #'ignore)
                ((symbol-function 'executable-find) (lambda (_) t)))
        (elinit--reconcile)
        ;; Entry should NOT have been started due to override
        (should-not (member "new-entry" started-ids))))))

(ert-deftest elinit-test-reconcile-stops-disabled-entries ()
  "Reconcile stops running entries that are now disabled.
Only auto-started (not manually-started) disabled units are stopped."
  (elinit-test-with-unit-files
      '(("sleep 100" :id "running" :type simple :disabled t))
    (let ((elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--restart-override (make-hash-table :test 'equal))
          (elinit--logging (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--manually-started (make-hash-table :test 'equal))
          (killed-ids nil))
      ;; Create a fake live process (NOT manually started)
      (let ((fake-proc (start-process "test-proc" nil "sleep" "100")))
        (puthash "running" fake-proc elinit--processes)
        ;; Mock kill-process to track kills
        (cl-letf (((symbol-function 'kill-process)
                   (lambda (proc)
                     (push (process-name proc) killed-ids)
                     (delete-process proc)))
                  ((symbol-function 'elinit--refresh-dashboard) #'ignore))
          (elinit--reconcile)
          ;; Entry should have been killed due to :disabled (not manually started)
          (should (member "test-proc" killed-ids)))))))

(ert-deftest elinit-test-computed-deps-populated ()
  "Topo-sort populates computed-deps with validated dependencies."
  (let ((elinit--computed-deps (make-hash-table :test 'equal))
        (elinit--cycle-fallback-ids (make-hash-table :test 'equal)))
    ;; Entry c depends on a and b, but b doesn't exist
    (let ((entries '(("a" "cmd" 0 t always t simple nil t 30)
                     ("c" "cmd" 0 t always t simple ("a" "b") t 30))))
      (elinit--stable-topo-sort entries)
      ;; c's computed deps should only include "a" (b doesn't exist)
      (should (equal (gethash "c" elinit--computed-deps) '("a")))
      ;; a has no deps
      (should (equal (gethash "a" elinit--computed-deps) nil)))))

(ert-deftest elinit-test-cycle-fallback-clears-computed-deps ()
  "Cycle fallback marks entries and clears their computed deps."
  (let ((elinit--computed-deps (make-hash-table :test 'equal))
        (elinit--cycle-fallback-ids (make-hash-table :test 'equal)))
    ;; Create a cycle: a -> b -> a
    (let ((entries '(("a" "cmd" 0 t always t simple ("b") t 30)
                     ("b" "cmd" 0 t always t simple ("a") t 30))))
      (elinit--stable-topo-sort entries)
      ;; Both should be marked as cycle fallback
      (should (gethash "a" elinit--cycle-fallback-ids))
      (should (gethash "b" elinit--cycle-fallback-ids))
      ;; Both should have nil computed deps (edges cleared)
      (should (null (gethash "a" elinit--computed-deps)))
      (should (null (gethash "b" elinit--computed-deps))))))

;;; Tags parsing tests

(ert-deftest elinit-test-parse-tags ()
  "Parse :tags keyword."
  (let ((parsed (elinit--parse-entry '("foo" :tags (x-setup network)))))
    (should (equal (elinit-entry-tags parsed) '(x-setup network)))))

(ert-deftest elinit-test-parse-tags-default-nil ()
  "Tags default to nil when not specified."
  (let ((parsed (elinit--parse-entry "foo")))
    (should-not (elinit-entry-tags parsed))))

(ert-deftest elinit-test-parse-tags-single ()
  "Parse single tag (not in list)."
  (let ((parsed (elinit--parse-entry '("foo" :tags myapp))))
    ;; Single symbol should be wrapped in list
    (should (equal (elinit-entry-tags parsed) '(myapp)))))

;;; Dry-run tests

(ert-deftest elinit-test-dry-run-output ()
  "Dry-run produces expected output with entries."
  (elinit-test-with-unit-files
      '(("true" :id "a" :type oneshot)
        ("true" :id "b")
        ("true" :id "c"))
    (let ((elinit--invalid (make-hash-table :test 'equal))
          (elinit--cycle-fallback-ids (make-hash-table :test 'equal))
          (elinit--computed-deps (make-hash-table :test 'equal)))
      ;; elinit-dry-run uses with-output-to-temp-buffer
      (elinit-dry-run)
      (let ((output (with-current-buffer "*elinit-dry-run*"
                      (buffer-string))))
        (kill-buffer "*elinit-dry-run*")
        ;; All entries in activation order
        (should (string-match-p "Activation order" output))
        (should (string-match-p "\\ba\\b" output))
        (should (string-match-p "\\bb\\b" output))
        (should (string-match-p "\\bc\\b" output))))))

(ert-deftest elinit-test-dry-run-shows-invalid ()
  "Dry-run shows invalid entries."
  (elinit-test-with-unit-files
      '(("true" :id "valid" :type simple)
        ("true" :id "invalid" :type "bad"))
    (let ((elinit-timers nil)
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--invalid-timers (make-hash-table :test 'equal))
          (elinit--cycle-fallback-ids (make-hash-table :test 'equal))
          (elinit--computed-deps (make-hash-table :test 'equal)))
      (elinit-dry-run)
      (let ((output (with-current-buffer "*elinit-dry-run*"
                      (buffer-string))))
        (kill-buffer "*elinit-dry-run*")
        (should (string-match-p "Invalid Services" output))
        (should (string-match-p "invalid" output))))))

(ert-deftest elinit-test-dry-run-validates-after ()
  "Dry-run validates :after references using same path as startup."
  (elinit-test-with-unit-files
      '(("true" :id "a")
        ("true" :id "b" :after ("nonexistent")))
    (let ((elinit--invalid (make-hash-table :test 'equal))
          (elinit--cycle-fallback-ids (make-hash-table :test 'equal))
          (elinit--computed-deps (make-hash-table :test 'equal)))
      ;; This should populate computed-deps via plan building
      (elinit-dry-run)
      (kill-buffer "*elinit-dry-run*")
      ;; b's computed deps should be empty since nonexistent doesn't exist
      (should (null (gethash "b" elinit--computed-deps)))
      ;; a should have nil deps (no :after)
      (should (null (gethash "a" elinit--computed-deps))))))


;;;; Conflict primitive tests

(ert-deftest elinit-test-conflict-targets-pure ()
  "Pure conflict-targets returns deduplicated symmetric conflicts."
  (let* ((programs '(("sleep 1" :id "a" :conflicts "b")
                     ("sleep 2" :id "b" :conflicts "a")
                     ("sleep 3" :id "c")))
         (plan (elinit--build-plan programs)))
    ;; Forward + reverse for a -> ("b") deduped
    (should (equal (elinit--conflict-targets "a" plan) '("b")))
    ;; c has no conflicts
    (should (null (elinit--conflict-targets "c" plan)))
    ;; nil plan returns nil
    (should (null (elinit--conflict-targets "a" nil)))))

(ert-deftest elinit-test-conflict-preflight-stops-active ()
  "Conflict preflight stops an active conflicting process."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--conflict-suppressed (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (signals-sent nil))
    ;; Mock a running process for "b"
    (let ((fake-proc (start-process "b" nil "sleep" "300")))
      (puthash "b" fake-proc elinit--processes)
      ;; Build a plan with a conflicts b
      (let* ((programs '(("sleep 1" :id "a" :conflicts "b")
                         ("sleep 2" :id "b")))
             (plan (elinit--build-plan programs))
             (stopped (elinit--conflict-preflight "a" plan)))
        ;; b should be in stopped list
        (should (member "b" stopped))
        ;; b should be conflict-suppressed
        (should (equal (gethash "b" elinit--conflict-suppressed) "a"))
        ;; b should be manually stopped
        (should (gethash "b" elinit--manually-stopped)))
      ;; Clean up
      (when (process-live-p fake-proc)
        (delete-process fake-proc)))))

(ert-deftest elinit-test-conflict-preflight-skips-inactive ()
  "Conflict preflight does not stop inactive units."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--conflict-suppressed (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal)))
    ;; No process for "b", no latched state - not active at all
    (let* ((programs '(("sleep 1" :id "a" :conflicts "b")
                       ("sleep 2" :id "b")))
           (plan (elinit--build-plan programs))
           (stopped (elinit--conflict-preflight "a" plan)))
      ;; Nothing should be stopped
      (should (null stopped))
      ;; No suppression set
      (should (null (gethash "b" elinit--conflict-suppressed))))))

(ert-deftest elinit-test-conflict-preflight-cancels-restart-timer ()
  "Conflict preflight cancels pending restart timer."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--conflict-suppressed (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (timer (run-at-time 999 nil #'ignore)))
    ;; Set up a pending restart timer for "b"
    (puthash "b" timer elinit--restart-timers)
    ;; Mock a running process for "b"
    (let ((fake-proc (start-process "b" nil "sleep" "300")))
      (puthash "b" fake-proc elinit--processes)
      (let* ((programs '(("sleep 1" :id "a" :conflicts "b")
                         ("sleep 2" :id "b")))
             (plan (elinit--build-plan programs)))
        (elinit--conflict-preflight "a" plan)
        ;; Restart timer should be cancelled and removed
        (should (null (gethash "b" elinit--restart-timers))))
      (when (process-live-p fake-proc)
        (delete-process fake-proc)))))

(ert-deftest elinit-test-conflict-clear-suppression ()
  "Clear-suppression removes entry from conflict-suppressed."
  (let ((elinit--conflict-suppressed (make-hash-table :test 'equal)))
    (puthash "b" "a" elinit--conflict-suppressed)
    (elinit--conflict-clear-suppression "b")
    (should (null (gethash "b" elinit--conflict-suppressed)))))

(ert-deftest elinit-test-conflict-preflight-nil-plan ()
  "Conflict preflight with nil plan does nothing."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--conflict-suppressed (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal)))
    (should (null (elinit--conflict-preflight "a" nil)))))

(ert-deftest elinit-test-conflict-preflight-symmetric ()
  "Conflict preflight sees reverse conflicts too."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--conflict-suppressed (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal)))
    ;; b declares conflict with a, but we start a
    (let ((fake-proc (start-process "b" nil "sleep" "300")))
      (puthash "b" fake-proc elinit--processes)
      (let* ((programs '(("sleep 1" :id "a")
                         ("sleep 2" :id "b" :conflicts "a")))
             (plan (elinit--build-plan programs))
             (stopped (elinit--conflict-preflight "a" plan)))
        ;; b should be stopped via reverse conflict
        (should (member "b" stopped)))
      (when (process-live-p fake-proc)
        (delete-process fake-proc)))))

(ert-deftest elinit-test-conflict-preflight-latched-oneshot ()
  "Conflict preflight deactivates latched remain-after-exit oneshot.
Clears remain-active and oneshot-completed so status shows
conflict-stopped rather than done."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--conflict-suppressed (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (elinit--mask-override (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--entry-state (make-hash-table :test 'equal))
        (elinit--spawn-failure-reason (make-hash-table :test 'equal))
        (elinit--target-convergence-reasons nil))
    ;; b is a latched oneshot (not alive, but remain-active + completed)
    (puthash "b" t elinit--remain-active)
    (puthash "b" 0 elinit--oneshot-completed)
    (let* ((programs '(("true" :id "a" :conflicts "b")
                       ("true" :id "b" :type oneshot :remain-after-exit t)))
           (plan (elinit--build-plan programs))
           (stopped (elinit--conflict-preflight "a" plan)))
      ;; b should be in stopped list
      (should (member "b" stopped))
      ;; b should be conflict-suppressed
      (should (equal (gethash "b" elinit--conflict-suppressed) "a"))
      ;; remain-active should be cleared
      (should (null (gethash "b" elinit--remain-active)))
      ;; oneshot-completed should be cleared so status is not "done"
      (should (null (gethash "b" elinit--oneshot-completed)))
      ;; Reason should be conflict-stopped, not nil
      (let ((reason (elinit--compute-entry-reason "b" 'oneshot)))
        (should (stringp reason))
        (should (string-match-p "conflict-stopped" reason))))))

(ert-deftest elinit-test-conflict-preflight-cancels-delay-timer ()
  "Conflict preflight cancels pending delay timer."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--conflict-suppressed (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (timer (run-at-time 999 nil #'ignore)))
    ;; b has a pending delay timer (not yet started)
    (puthash "b" timer elinit--dag-delay-timers)
    (let* ((programs '(("sleep 1" :id "a" :conflicts "b")
                       ("sleep 2" :id "b")))
           (plan (elinit--build-plan programs)))
      (elinit--conflict-preflight "a" plan)
      ;; Delay timer should be cancelled and removed
      (should (null (gethash "b" elinit--dag-delay-timers))))))

(ert-deftest elinit-test-conflict-preflight-restart-timer-no-process ()
  "Conflict preflight cancels restart timer even without live process."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--conflict-suppressed (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (timer (run-at-time 999 nil #'ignore)))
    ;; b has a pending restart timer but no live process
    (puthash "b" timer elinit--restart-timers)
    (let* ((programs '(("sleep 1" :id "a" :conflicts "b")
                       ("sleep 2" :id "b")))
           (plan (elinit--build-plan programs)))
      (elinit--conflict-preflight "a" plan)
      ;; Restart timer should be cancelled and removed
      (should (null (gethash "b" elinit--restart-timers))))))

(ert-deftest elinit-test-reconcile-publishes-current-plan ()
  "Reconcile publishes elinit--current-plan for conflict lookups."
  (elinit-test-with-unit-files
      '(("true" :id "svc" :type simple))
    (let ((elinit--current-plan nil)
          (elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--restart-override (make-hash-table :test 'equal))
          (elinit--logging (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal)))
      (cl-letf (((symbol-function 'elinit--start-process)
                 (lambda (_id _cmd _log _type _restart &rest _args) t))
                ((symbol-function 'elinit--refresh-dashboard) #'ignore)
                ((symbol-function 'executable-find) (lambda (_) t)))
        (elinit--reconcile)
        ;; Plan should now be published
        (should (elinit-plan-p elinit--current-plan))))))

(ert-deftest elinit-test-conflict-preflight-sigkill-escalation ()
  "Conflict preflight sets up SIGKILL escalation timer for stubborn processes."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--conflict-suppressed (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (escalation-fired nil))
    ;; Mock a running process for "b"
    (let ((fake-proc (start-process "b" nil "sleep" "300")))
      (puthash "b" fake-proc elinit--processes)
      (let* ((programs '(("sleep 1" :id "a" :conflicts "b")
                         ("sleep 2" :id "b")))
             (plan (elinit--build-plan programs)))
        ;; Mock run-at-time to capture escalation timer setup
        (let ((original-run-at-time (symbol-function 'run-at-time)))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (secs repeat fn &rest args)
                       ;; Detect the escalation timer (non-nil secs, nil repeat)
                       (when (and (numberp secs) (null repeat)
                                  (> secs 0))
                         (setq escalation-fired t))
                       ;; Don't actually schedule
                       nil)))
            (elinit--conflict-preflight "a" plan))))
      ;; SIGKILL escalation timer should have been set up
      (should escalation-fired)
      (when (process-live-p fake-proc)
        (delete-process fake-proc)))))

(ert-deftest elinit-test-restart-callback-suppression-guard ()
  "Restart callback skips start when unit is conflict-suppressed at fire time."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--conflict-suppressed (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (elinit--current-plan nil)
        (elinit--failed (make-hash-table :test 'equal))
        (start-called nil))
    ;; Schedule a restart via elinit--schedule-restart
    (cl-letf (((symbol-function 'elinit--start-process)
               (lambda (&rest _args)
                 (setq start-called t)
                 t))
              ((symbol-function 'elinit--format-exit-status)
               (lambda (&rest _) "exited 0")))
      ;; Schedule with 0-delay so timer fires immediately on sit-for
      (let ((elinit-restart-delay 0))
        (elinit--schedule-restart
         "svc" "sleep 300" t 'simple 'always "exited" 0)
        ;; Set conflict-suppressed AFTER scheduling (simulates race)
        (puthash "svc" "other" elinit--conflict-suppressed)
        ;; Let the timer fire
        (sit-for 0.1)
        ;; Start should NOT have been called
        (should-not start-called)))
    ;; Clean up timer if still pending
    (let ((timer (gethash "svc" elinit--restart-timers)))
      (when (and timer (timerp timer))
        (cancel-timer timer)))))

(provide 'elinit-test-dag)
;;; elinit-test-dag.el ends here
