;;; supervisor-test-dag.el --- DAG scheduler and integration tests for supervisor.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; DAG scheduler and integration ERT tests for supervisor.el.

;;; Code:

(require 'supervisor-test-helpers)

;;; Integration tests

(ert-deftest supervisor-test-verify-populates-invalid-hash ()
  "supervisor-verify populates supervisor--invalid hash table."
  (supervisor-test-with-unit-files
      '(("valid-entry" :id "valid-entry" :type simple)
        ("invalid-entry" :id "invalid-entry" :type "bad"))
    (let ((supervisor--invalid (make-hash-table :test 'equal)))
      (with-temp-buffer
        (let ((standard-output (current-buffer)))
          (supervisor-verify)))
      (should (null (gethash "valid-entry" supervisor--invalid)))
      (should (gethash "invalid-entry" supervisor--invalid)))))

(ert-deftest supervisor-test-all-parsed-entries-skips-invalid ()
  "supervisor--all-parsed-entries skips invalid entries."
  (supervisor-test-with-unit-files
      '(("valid" :id "valid" :type simple)
        ("invalid" :id "invalid" :type "bad")
        ("also-valid" :id "also-valid" :type oneshot))
    (let ((supervisor--invalid (make-hash-table :test 'equal)))
      (let ((entries (supervisor--all-parsed-entries)))
        ;; Should have 2 valid entries
        (should (= (length entries) 2))
        ;; Invalid should be tracked
        (should (gethash "invalid" supervisor--invalid))
        ;; Valid entries should be in result
        (should (cl-find "valid" entries :key #'car :test #'equal))
        (should (cl-find "also-valid" entries :key #'car :test #'equal))))))

(ert-deftest supervisor-test-verify-handles-malformed-entry ()
  "supervisor-verify handles malformed unit files gracefully."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-unit-directory dir)
         (supervisor--programs-cache :not-yet-loaded)
         (supervisor--unit-file-invalid (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; Write a valid unit file and a malformed (non-plist) file
          (with-temp-file (expand-file-name "valid.el" dir)
            (insert "(:id \"valid\" :command \"true\" :type simple)"))
          (with-temp-file (expand-file-name "broken.el" dir)
            (insert "42"))
          (with-temp-buffer
            (let ((standard-output (current-buffer)))
              (supervisor-verify)))
          ;; Malformed file should be tracked in invalid
          (should (gethash "broken" supervisor--invalid))
          ;; Valid entry should not be in invalid
          (should (null (gethash "valid" supervisor--invalid))))
      (delete-directory dir t))))

;;; DAG scheduler tests

(ert-deftest supervisor-test-topo-sort-stable-ordering ()
  "Unconstrained nodes maintain original list order."
  (let* ((entries '(("a" "cmd" 0 t always t simple nil t 30)
                    ("b" "cmd" 0 t always t simple nil t 30)
                    ("c" "cmd" 0 t always t simple nil t 30)))
         (sorted (supervisor--stable-topo-sort entries)))
    ;; With no dependencies, order should be preserved
    (should (equal (mapcar #'car sorted) '("a" "b" "c")))))

(ert-deftest supervisor-test-topo-sort-respects-after ()
  "Entries with :after come after their dependencies."
  (let* ((entries '(("c" "cmd" 0 t always t simple ("a") t 30)
                    ("a" "cmd" 0 t always t simple nil t 30)
                    ("b" "cmd" 0 t always t simple nil t 30)))
         (sorted (supervisor--stable-topo-sort entries))
         (order (mapcar #'car sorted)))
    ;; "a" must come before "c" (dependency constraint)
    (should (< (cl-position "a" order :test #'equal)
               (cl-position "c" order :test #'equal)))
    ;; Stable sort: after "a" emits, both "c" (idx 0) and "b" (idx 2) are ready
    ;; "c" comes first because it has lower original index
    (should (equal order '("a" "c" "b")))))

(ert-deftest supervisor-test-topo-sort-cycle-fallback ()
  "Cycle detection returns list order with :after cleared."
  (let* ((entries '(("a" "cmd" 0 t always t simple ("b") t 30)
                    ("b" "cmd" 0 t always t simple ("a") t 30)))
         (sorted (supervisor--stable-topo-sort entries)))
    ;; Should return in original order
    (should (equal (mapcar #'car sorted) '("a" "b")))
    ;; :after (index 7) should be nil for all entries
    (should (null (nth 7 (car sorted))))
    (should (null (nth 7 (cadr sorted))))))

(ert-deftest supervisor-test-topo-sort-complex-dag ()
  "Complex DAG with multiple dependencies."
  ;; d depends on b and c, b depends on a
  (let* ((entries '(("a" "cmd" 0 t always t simple nil t 30)
                    ("b" "cmd" 0 t always t simple ("a") t 30)
                    ("c" "cmd" 0 t always t simple nil t 30)
                    ("d" "cmd" 0 t always t simple ("b" "c") t 30)))
         (sorted (supervisor--stable-topo-sort entries))
         (order (mapcar #'car sorted)))
    ;; a must come before b
    (should (< (cl-position "a" order :test #'equal)
               (cl-position "b" order :test #'equal)))
    ;; b and c must come before d
    (should (< (cl-position "b" order :test #'equal)
               (cl-position "d" order :test #'equal)))
    (should (< (cl-position "c" order :test #'equal)
               (cl-position "d" order :test #'equal)))))

(ert-deftest supervisor-test-dag-init-blocking-oneshot ()
  "Blocking oneshots are tracked in supervisor--dag-blocking."
  (let ((supervisor--dag-blocking nil)
        (supervisor--dag-in-degree nil)
        (supervisor--dag-dependents nil)
        (supervisor--dag-entries nil)
        (supervisor--dag-started nil)
        (supervisor--dag-ready nil)
        (supervisor--dag-timeout-timers nil)
        (supervisor--dag-delay-timers nil)
        (supervisor--dag-id-to-index nil))
    ;; Entry: (id cmd delay enabled-p restart-policy logging-p type after oneshot-blocking oneshot-timeout)
    (let ((entries '(("blocking" "cmd" 0 t always t oneshot nil t 30)
                     ("non-blocking" "cmd" 0 t always t oneshot nil nil 30)
                     ("simple" "cmd" 0 t always t simple nil t 30))))
      (supervisor--dag-init entries)
      ;; Blocking oneshot should be in blocking set
      (should (gethash "blocking" supervisor--dag-blocking))
      ;; Non-blocking oneshot should NOT be in blocking set
      (should-not (gethash "non-blocking" supervisor--dag-blocking))
      ;; Simple process should NOT be in blocking set
      (should-not (gethash "simple" supervisor--dag-blocking)))))

(ert-deftest supervisor-test-dag-init-in-degree ()
  "DAG init correctly calculates in-degree from :after."
  (let ((supervisor--dag-blocking nil)
        (supervisor--dag-in-degree nil)
        (supervisor--dag-dependents nil)
        (supervisor--dag-entries nil)
        (supervisor--dag-started nil)
        (supervisor--dag-ready nil)
        (supervisor--dag-timeout-timers nil)
        (supervisor--dag-delay-timers nil)
        (supervisor--dag-id-to-index nil))
    (let ((entries '(("a" "cmd" 0 t always t simple nil t 30)
                     ("b" "cmd" 0 t always t simple ("a") t 30)
                     ("c" "cmd" 0 t always t simple ("a" "b") t 30))))
      (supervisor--dag-init entries)
      ;; a has no dependencies
      (should (= (gethash "a" supervisor--dag-in-degree) 0))
      ;; b depends on a
      (should (= (gethash "b" supervisor--dag-in-degree) 1))
      ;; c depends on a and b
      (should (= (gethash "c" supervisor--dag-in-degree) 2)))))

(ert-deftest supervisor-test-dag-init-dependents ()
  "DAG init correctly builds dependents graph."
  (let ((supervisor--dag-blocking nil)
        (supervisor--dag-in-degree nil)
        (supervisor--dag-dependents nil)
        (supervisor--dag-entries nil)
        (supervisor--dag-started nil)
        (supervisor--dag-ready nil)
        (supervisor--dag-timeout-timers nil)
        (supervisor--dag-delay-timers nil)
        (supervisor--dag-id-to-index nil))
    (let ((entries '(("a" "cmd" 0 t always t simple nil t 30)
                     ("b" "cmd" 0 t always t simple ("a") t 30)
                     ("c" "cmd" 0 t always t simple ("a") t 30))))
      (supervisor--dag-init entries)
      ;; a should have b and c as dependents
      (let ((deps (gethash "a" supervisor--dag-dependents)))
        (should (member "b" deps))
        (should (member "c" deps))))))

(ert-deftest supervisor-test-dag-disabled-entry-ready-immediately ()
  "Disabled entries are ready immediately and don't block dependents."
  (let ((supervisor--dag-blocking (make-hash-table :test 'equal))
        (supervisor--dag-in-degree (make-hash-table :test 'equal))
        (supervisor--dag-dependents (make-hash-table :test 'equal))
        (supervisor--dag-entries (make-hash-table :test 'equal))
        (supervisor--dag-started (make-hash-table :test 'equal))
        (supervisor--dag-ready (make-hash-table :test 'equal))
        (supervisor--dag-timeout-timers (make-hash-table :test 'equal))
        (supervisor--dag-delay-timers (make-hash-table :test 'equal))
        (supervisor--dag-id-to-index (make-hash-table :test 'equal))
        (supervisor--entry-state (make-hash-table :test 'equal))
        (supervisor--ready-times (make-hash-table :test 'equal)))
    ;; "a" is disabled, "b" depends on "a"
    ;;           (id   cmd   delay enabled-p restart-policy logging-p type   after oneshot-blocking timeout)
    (let ((entries '(("a" "cmd" 0 nil always t simple nil t 30)
                     ("b" "cmd" 0 t   always t simple ("a") t 30))))
      (supervisor--dag-init entries)
      ;; Disabled entry should be marked ready immediately
      (should (gethash "a" supervisor--dag-ready))
      (should (gethash "a" supervisor--dag-started))
      ;; Entry state should be 'disabled
      (should (eq (gethash "a" supervisor--entry-state) 'disabled))
      ;; Dependent "b" should have in-degree 0 (not blocked by disabled "a")
      (should (= 0 (gethash "b" supervisor--dag-in-degree))))))

(ert-deftest supervisor-test-async-oneshot-not-blocking ()
  "Async oneshots (oneshot-blocking nil) do not block convergence."
  (let ((supervisor--dag-blocking nil)
        (supervisor--dag-in-degree nil)
        (supervisor--dag-dependents nil)
        (supervisor--dag-entries nil)
        (supervisor--dag-started nil)
        (supervisor--dag-ready nil)
        (supervisor--dag-timeout-timers nil)
        (supervisor--dag-delay-timers nil)
        (supervisor--dag-id-to-index nil))
    ;; Entry with :oneshot-async t means oneshot-blocking = nil
    (let ((entries '(("async-oneshot" "cmd" 0 t always t oneshot nil nil 30))))
      (supervisor--dag-init entries)
      ;; Async oneshot should NOT be in blocking set
      (should-not (gethash "async-oneshot" supervisor--dag-blocking)))))

(ert-deftest supervisor-test-startup-complete-blocked-by-delay ()
  "Delayed entries prevent startup completion until they start."
  (let ((supervisor--dag-blocking (make-hash-table :test 'equal))
        (supervisor--dag-in-degree (make-hash-table :test 'equal))
        (supervisor--dag-dependents (make-hash-table :test 'equal))
        (supervisor--dag-entries (make-hash-table :test 'equal))
        (supervisor--dag-started (make-hash-table :test 'equal))
        (supervisor--dag-ready (make-hash-table :test 'equal))
        (supervisor--dag-timeout-timers (make-hash-table :test 'equal))
        (supervisor--dag-delay-timers (make-hash-table :test 'equal))
        (supervisor--dag-id-to-index (make-hash-table :test 'equal))
        (supervisor--dag-complete-callback nil)
        (supervisor--shutting-down nil)
        (callback-called nil))
    ;; Entry with delay
    (puthash "delayed" '("delayed" "echo hi" 5 t t t simple nil t 30) supervisor--dag-entries)
    (puthash "delayed" 0 supervisor--dag-in-degree)
    (puthash "delayed" 0 supervisor--dag-id-to-index)
    (puthash "delayed" nil supervisor--dag-dependents)
    (setq supervisor--dag-complete-callback (lambda () (setq callback-called t)))
    ;; Simulate starting the delayed entry - adds to delay-timers
    (puthash "delayed" 'mock-timer supervisor--dag-delay-timers)
    ;; Mark as "started" from scheduler's perspective
    (puthash "delayed" t supervisor--dag-started)
    ;; Try to complete startup - should NOT call callback because delay timer exists
    (supervisor--dag-check-complete)
    (should-not callback-called)))

(ert-deftest supervisor-test-startup-complete-blocked-by-blocking-oneshot ()
  "Blocking oneshots prevent startup completion until they exit."
  (let ((supervisor--dag-blocking (make-hash-table :test 'equal))
        (supervisor--dag-in-degree (make-hash-table :test 'equal))
        (supervisor--dag-dependents (make-hash-table :test 'equal))
        (supervisor--dag-entries (make-hash-table :test 'equal))
        (supervisor--dag-started (make-hash-table :test 'equal))
        (supervisor--dag-ready (make-hash-table :test 'equal))
        (supervisor--dag-timeout-timers (make-hash-table :test 'equal))
        (supervisor--dag-delay-timers (make-hash-table :test 'equal))
        (supervisor--dag-id-to-index (make-hash-table :test 'equal))
        (supervisor--dag-complete-callback nil)
        (callback-called nil))
    ;; Blocking oneshot entry
    (puthash "blocking" '("blocking" "sleep 10" 0 t always t oneshot nil t 30) supervisor--dag-entries)
    (puthash "blocking" 0 supervisor--dag-in-degree)
    (puthash "blocking" t supervisor--dag-started)
    ;; Oneshot is blocking
    (puthash "blocking" t supervisor--dag-blocking)
    (setq supervisor--dag-complete-callback (lambda () (setq callback-called t)))
    ;; Try to complete startup - should NOT call callback because blocking oneshot exists
    (supervisor--dag-check-complete)
    (should-not callback-called)))

(ert-deftest supervisor-test-mark-ready-removes-from-blocking ()
  "supervisor--dag-mark-ready removes entry from blocking set."
  (let ((supervisor--dag-blocking (make-hash-table :test 'equal))
        (supervisor--dag-in-degree (make-hash-table :test 'equal))
        (supervisor--dag-dependents (make-hash-table :test 'equal))
        (supervisor--dag-entries (make-hash-table :test 'equal))
        (supervisor--dag-started (make-hash-table :test 'equal))
        (supervisor--dag-ready (make-hash-table :test 'equal))
        (supervisor--dag-timeout-timers (make-hash-table :test 'equal))
        (supervisor--dag-delay-timers (make-hash-table :test 'equal))
        (supervisor--dag-id-to-index (make-hash-table :test 'equal))
        (supervisor--dag-complete-callback nil)
        (supervisor-verbose nil))
    ;; Set up blocking oneshot
    (puthash "oneshot" '("oneshot" "cmd" 0 t always t oneshot nil t 30) supervisor--dag-entries)
    (puthash "oneshot" 0 supervisor--dag-in-degree)
    (puthash "oneshot" t supervisor--dag-started)
    (puthash "oneshot" t supervisor--dag-blocking)
    (puthash "oneshot" nil supervisor--dag-dependents)
    ;; Mark ready
    (supervisor--dag-mark-ready "oneshot")
    ;; Should be removed from blocking
    (should-not (gethash "oneshot" supervisor--dag-blocking))
    ;; Should be in ready set
    (should (gethash "oneshot" supervisor--dag-ready))))

(ert-deftest supervisor-test-mark-ready-unlocks-dependents ()
  "supervisor--dag-mark-ready decrements in-degree for dependents."
  (let ((supervisor--dag-blocking (make-hash-table :test 'equal))
        (supervisor--dag-in-degree (make-hash-table :test 'equal))
        (supervisor--dag-dependents (make-hash-table :test 'equal))
        (supervisor--dag-entries (make-hash-table :test 'equal))
        (supervisor--dag-started (make-hash-table :test 'equal))
        (supervisor--dag-ready (make-hash-table :test 'equal))
        (supervisor--dag-timeout-timers (make-hash-table :test 'equal))
        (supervisor--dag-delay-timers (make-hash-table :test 'equal))
        (supervisor--dag-id-to-index (make-hash-table :test 'equal))
        (supervisor--dag-complete-callback nil)
        (supervisor--shutting-down nil)
        (supervisor-verbose nil)
        (started-ids nil))
    ;; Stub start function to prevent actual process spawning
    (cl-letf (((symbol-function 'supervisor--dag-start-entry-async)
               (lambda (entry) (push (car entry) started-ids))))
      ;; Set up: b depends on a
      (puthash "a" '("a" "cmd" 0 t always t simple nil t 30) supervisor--dag-entries)
      (puthash "b" '("b" "cmd" 0 t always t simple ("a") t 30) supervisor--dag-entries)
      (puthash "a" 0 supervisor--dag-in-degree)
      (puthash "b" 1 supervisor--dag-in-degree)
      (puthash "a" t supervisor--dag-started)
      (puthash "b" nil supervisor--dag-started)
      (puthash "a" '("b") supervisor--dag-dependents)
      (puthash "b" nil supervisor--dag-dependents)
      (puthash "a" 0 supervisor--dag-id-to-index)
      (puthash "b" 1 supervisor--dag-id-to-index)
      ;; Mark a as ready
      (supervisor--dag-mark-ready "a")
      ;; b's in-degree should now be 0
      (should (= 0 (gethash "b" supervisor--dag-in-degree)))
      ;; b should have been triggered to start
      (should (member "b" started-ids)))))

(ert-deftest supervisor-test-oneshot-timeout-unlocks-dependents ()
  "Oneshot timeout calls mark-ready which unlocks dependents and convergence."
  (let ((supervisor--dag-blocking (make-hash-table :test 'equal))
        (supervisor--dag-in-degree (make-hash-table :test 'equal))
        (supervisor--dag-dependents (make-hash-table :test 'equal))
        (supervisor--dag-entries (make-hash-table :test 'equal))
        (supervisor--dag-started (make-hash-table :test 'equal))
        (supervisor--dag-ready (make-hash-table :test 'equal))
        (supervisor--dag-timeout-timers (make-hash-table :test 'equal))
        (supervisor--dag-delay-timers (make-hash-table :test 'equal))
        (supervisor--dag-id-to-index (make-hash-table :test 'equal))
        (supervisor--dag-complete-callback nil)
        (supervisor--shutting-down nil)
        (supervisor-verbose nil)
        (startup-complete nil)
        (started-ids nil))
    ;; Stub start function
    (cl-letf (((symbol-function 'supervisor--dag-start-entry-async)
               (lambda (entry) (push (car entry) started-ids))))
      ;; Set up: blocking oneshot "slow" with dependent "after-slow"
      (puthash "slow" '("slow" "sleep 999" 0 t always t oneshot nil t 5) supervisor--dag-entries)
      (puthash "after-slow" '("after-slow" "echo done" 0 t always t simple ("slow") t 30) supervisor--dag-entries)
      (puthash "slow" 0 supervisor--dag-in-degree)
      (puthash "after-slow" 1 supervisor--dag-in-degree)
      (puthash "slow" t supervisor--dag-started)
      (puthash "after-slow" nil supervisor--dag-started)
      (puthash "slow" t supervisor--dag-blocking)  ; blocking oneshot
      (puthash "slow" '("after-slow") supervisor--dag-dependents)
      (puthash "after-slow" nil supervisor--dag-dependents)
      (puthash "slow" 0 supervisor--dag-id-to-index)
      (puthash "after-slow" 1 supervisor--dag-id-to-index)
      ;; Set up a mock timeout timer
      (puthash "slow" 'mock-timer supervisor--dag-timeout-timers)
      ;; Startup completion callback
      (setq supervisor--dag-complete-callback
            (lambda () (setq startup-complete t)))
      ;; Simulate timeout firing: this is what the timeout timer does
      (supervisor--dag-mark-ready "slow")
      ;; Blocking oneshot should be removed from blocking set
      (should-not (gethash "slow" supervisor--dag-blocking))
      ;; Dependent should have been unlocked and triggered
      (should (= 0 (gethash "after-slow" supervisor--dag-in-degree)))
      (should (member "after-slow" started-ids))
      ;; Timeout timer should be removed
      (should-not (gethash "slow" supervisor--dag-timeout-timers)))))


;;; P1 behavior tests

(ert-deftest supervisor-test-max-concurrent-starts-active-count-no-leak ()
  "Active count must not leak when entries are processed from queue."
  ;; The fix ensures supervisor--dag-process-pending-starts does NOT
  ;; increment the count - only supervisor--dag-do-start does.
  (let ((supervisor--dag-pending-starts nil)
        (supervisor--dag-active-starts 0)
        (supervisor--dag-entries (make-hash-table :test 'equal))
        (supervisor--dag-started (make-hash-table :test 'equal))
        (supervisor--dag-ready (make-hash-table :test 'equal))
        (supervisor--dag-blocking (make-hash-table :test 'equal))
        (supervisor--dag-in-degree (make-hash-table :test 'equal))
        (supervisor--dag-dependents (make-hash-table :test 'equal))
        (supervisor--dag-delay-timers (make-hash-table :test 'equal))
        (supervisor--dag-timeout-timers (make-hash-table :test 'equal))
        (supervisor--entry-state (make-hash-table :test 'equal))
        (supervisor--enabled-override (make-hash-table :test 'equal))
        (supervisor--ready-times (make-hash-table :test 'equal))
        (supervisor--shutting-down nil)
        (supervisor-max-concurrent-starts 2)
        (started-ids nil))
    ;; Queue entries
    (puthash "a" '("a" "true" 0 t always t simple nil t 30) supervisor--dag-entries)
    (puthash "b" '("b" "true" 0 t always t simple nil t 30) supervisor--dag-entries)
    (setq supervisor--dag-pending-starts '("a" "b"))
    ;; Before processing, count should be 0
    (should (= supervisor--dag-active-starts 0))
    ;; Stub supervisor--dag-start-entry-async to track calls without spawning
    (cl-letf (((symbol-function 'supervisor--dag-start-entry-async)
               (lambda (entry) (push (car entry) started-ids))))
      ;; Call the real function
      (supervisor--dag-process-pending-starts)
      ;; Queue should be drained
      (should (null supervisor--dag-pending-starts))
      ;; Both entries should have been passed to start-entry-async
      (should (member "a" started-ids))
      (should (member "b" started-ids))
      ;; Active count should still be 0 (no increment in process-pending-starts)
      (should (= supervisor--dag-active-starts 0)))))

(ert-deftest supervisor-test-enabled-override-affects-effective-enabled ()
  "Runtime enable override affects effective enabled state."
  (let ((supervisor--enabled-override (make-hash-table :test 'equal)))
    ;; No override: config enabled-p applies
    (should (supervisor--get-effective-enabled "a" t))
    (should-not (supervisor--get-effective-enabled "b" nil))
    ;; Override to disabled: entry is disabled regardless of config
    (puthash "a" 'disabled supervisor--enabled-override)
    (should-not (supervisor--get-effective-enabled "a" t))
    ;; Override to enabled: entry is enabled regardless of config
    (puthash "b" 'enabled supervisor--enabled-override)
    (should (supervisor--get-effective-enabled "b" nil))))

(ert-deftest supervisor-test-startup-timeout-sets-entry-state ()
  "Startup timeout marks unstarted entries with startup-timeout state."
  (let ((supervisor--dag-entries (make-hash-table :test 'equal))
        (supervisor--dag-started (make-hash-table :test 'equal))
        (supervisor--dag-blocking (make-hash-table :test 'equal))
        (supervisor--dag-in-degree (make-hash-table :test 'equal))
        (supervisor--dag-dependents (make-hash-table :test 'equal))
        (supervisor--dag-ready (make-hash-table :test 'equal))
        (supervisor--dag-delay-timers (make-hash-table :test 'equal))
        (supervisor--dag-timeout-timers (make-hash-table :test 'equal))
        (supervisor--dag-id-to-index (make-hash-table :test 'equal))
        (supervisor--dag-complete-callback nil)
        (supervisor--dag-timeout-timer nil)
        (supervisor--dag-pending-starts nil)
        (supervisor--dag-active-starts 0)
        (supervisor--entry-state (make-hash-table :test 'equal))
        (callback-called nil))
    ;; Set up entry that hasn't started
    (puthash "delayed" '("delayed" "cmd" 5 t t t simple nil t 30)
             supervisor--dag-entries)
    (setq supervisor--dag-complete-callback (lambda () (setq callback-called t)))
    ;; Force complete should mark unstarted as startup-timeout
    (supervisor--dag-force-complete)
    (should (eq (gethash "delayed" supervisor--entry-state) 'startup-timeout))
    (should callback-called)))

(ert-deftest supervisor-test-format-exit-status-signal ()
  "Exit status formatting distinguishes signal from exit."
  ;; Signal case
  (should (string-match "killed by signal 15"
                        (supervisor--format-exit-status 'signal 15)))
  ;; Exit with code 0
  (should (string-match "exited successfully"
                        (supervisor--format-exit-status 'exit 0)))
  ;; Exit with non-zero code
  (should (string-match "exited with code 1"
                        (supervisor--format-exit-status 'exit 1))))

(ert-deftest supervisor-test-reconcile-respects-enabled-override ()
  "Reconcile uses effective enabled state for start decisions."
  (supervisor-test-with-unit-files
      '(("true" :id "new-entry" :type simple))
    (let ((supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor--processes (make-hash-table :test 'equal))
          (supervisor--failed (make-hash-table :test 'equal))
          (supervisor--oneshot-completed (make-hash-table :test 'equal))
          (supervisor--restart-override (make-hash-table :test 'equal))
          (supervisor--logging (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (started-ids nil))
      ;; Mark entry as runtime-disabled
      (puthash "new-entry" 'disabled supervisor--enabled-override)
      ;; Mock supervisor--start-process to track what gets started
      (cl-letf (((symbol-function 'supervisor--start-process)
                 (lambda (id _cmd _log _type _restart &rest _args)
                   (push id started-ids)))
                ((symbol-function 'supervisor--refresh-dashboard) #'ignore)
                ((symbol-function 'executable-find) (lambda (_) t)))
        (supervisor--reconcile)
        ;; Entry should NOT have been started due to override
        (should-not (member "new-entry" started-ids))))))

(ert-deftest supervisor-test-reconcile-stops-disabled-entries ()
  "Reconcile stops running entries that are now disabled.
Only auto-started (not manually-started) disabled units are stopped."
  (supervisor-test-with-unit-files
      '(("sleep 100" :id "running" :type simple :disabled t))
    (let ((supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor--processes (make-hash-table :test 'equal))
          (supervisor--failed (make-hash-table :test 'equal))
          (supervisor--oneshot-completed (make-hash-table :test 'equal))
          (supervisor--restart-override (make-hash-table :test 'equal))
          (supervisor--logging (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--mask-override (make-hash-table :test 'equal))
          (supervisor--manually-started (make-hash-table :test 'equal))
          (killed-ids nil))
      ;; Create a fake live process (NOT manually started)
      (let ((fake-proc (start-process "test-proc" nil "sleep" "100")))
        (puthash "running" fake-proc supervisor--processes)
        ;; Mock kill-process to track kills
        (cl-letf (((symbol-function 'kill-process)
                   (lambda (proc)
                     (push (process-name proc) killed-ids)
                     (delete-process proc)))
                  ((symbol-function 'supervisor--refresh-dashboard) #'ignore))
          (supervisor--reconcile)
          ;; Entry should have been killed due to :disabled (not manually started)
          (should (member "test-proc" killed-ids)))))))

(ert-deftest supervisor-test-computed-deps-populated ()
  "Topo-sort populates computed-deps with validated dependencies."
  (let ((supervisor--computed-deps (make-hash-table :test 'equal))
        (supervisor--cycle-fallback-ids (make-hash-table :test 'equal)))
    ;; Entry c depends on a and b, but b doesn't exist
    (let ((entries '(("a" "cmd" 0 t always t simple nil t 30)
                     ("c" "cmd" 0 t always t simple ("a" "b") t 30))))
      (supervisor--stable-topo-sort entries)
      ;; c's computed deps should only include "a" (b doesn't exist)
      (should (equal (gethash "c" supervisor--computed-deps) '("a")))
      ;; a has no deps
      (should (equal (gethash "a" supervisor--computed-deps) nil)))))

(ert-deftest supervisor-test-cycle-fallback-clears-computed-deps ()
  "Cycle fallback marks entries and clears their computed deps."
  (let ((supervisor--computed-deps (make-hash-table :test 'equal))
        (supervisor--cycle-fallback-ids (make-hash-table :test 'equal)))
    ;; Create a cycle: a -> b -> a
    (let ((entries '(("a" "cmd" 0 t always t simple ("b") t 30)
                     ("b" "cmd" 0 t always t simple ("a") t 30))))
      (supervisor--stable-topo-sort entries)
      ;; Both should be marked as cycle fallback
      (should (gethash "a" supervisor--cycle-fallback-ids))
      (should (gethash "b" supervisor--cycle-fallback-ids))
      ;; Both should have nil computed deps (edges cleared)
      (should (null (gethash "a" supervisor--computed-deps)))
      (should (null (gethash "b" supervisor--computed-deps))))))

;;; Tags parsing tests

(ert-deftest supervisor-test-parse-tags ()
  "Parse :tags keyword."
  (let ((parsed (supervisor--parse-entry '("foo" :tags (x-setup network)))))
    (should (equal (supervisor-entry-tags parsed) '(x-setup network)))))

(ert-deftest supervisor-test-parse-tags-default-nil ()
  "Tags default to nil when not specified."
  (let ((parsed (supervisor--parse-entry "foo")))
    (should-not (supervisor-entry-tags parsed))))

(ert-deftest supervisor-test-parse-tags-single ()
  "Parse single tag (not in list)."
  (let ((parsed (supervisor--parse-entry '("foo" :tags myapp))))
    ;; Single symbol should be wrapped in list
    (should (equal (supervisor-entry-tags parsed) '(myapp)))))

;;; Dry-run tests

(ert-deftest supervisor-test-dry-run-output ()
  "Dry-run produces expected output with entries."
  (supervisor-test-with-unit-files
      '(("true" :id "a" :type oneshot)
        ("true" :id "b")
        ("true" :id "c"))
    (let ((supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
          (supervisor--computed-deps (make-hash-table :test 'equal)))
      ;; supervisor-dry-run uses with-output-to-temp-buffer
      (supervisor-dry-run)
      (let ((output (with-current-buffer "*supervisor-dry-run*"
                      (buffer-string))))
        (kill-buffer "*supervisor-dry-run*")
        ;; All entries in activation order
        (should (string-match-p "Activation order" output))
        (should (string-match-p "\\ba\\b" output))
        (should (string-match-p "\\bb\\b" output))
        (should (string-match-p "\\bc\\b" output))))))

(ert-deftest supervisor-test-dry-run-shows-invalid ()
  "Dry-run shows invalid entries."
  (supervisor-test-with-unit-files
      '(("true" :id "valid" :type simple)
        ("true" :id "invalid" :type "bad"))
    (let ((supervisor-timers nil)
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--invalid-timers (make-hash-table :test 'equal))
          (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
          (supervisor--computed-deps (make-hash-table :test 'equal)))
      (supervisor-dry-run)
      (let ((output (with-current-buffer "*supervisor-dry-run*"
                      (buffer-string))))
        (kill-buffer "*supervisor-dry-run*")
        (should (string-match-p "Invalid Services" output))
        (should (string-match-p "invalid" output))))))

(ert-deftest supervisor-test-dry-run-validates-after ()
  "Dry-run validates :after references using same path as startup."
  (supervisor-test-with-unit-files
      '(("true" :id "a")
        ("true" :id "b" :after ("nonexistent")))
    (let ((supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
          (supervisor--computed-deps (make-hash-table :test 'equal)))
      ;; This should populate computed-deps via plan building
      (supervisor-dry-run)
      (kill-buffer "*supervisor-dry-run*")
      ;; b's computed deps should be empty since nonexistent doesn't exist
      (should (null (gethash "b" supervisor--computed-deps)))
      ;; a should have nil deps (no :after)
      (should (null (gethash "a" supervisor--computed-deps))))))


(provide 'supervisor-test-dag)
;;; supervisor-test-dag.el ends here
