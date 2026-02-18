;;; supervisor-test-plan.el --- Plan builder and FSM tests for supervisor.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Plan builder and FSM ERT tests for supervisor.el.

;;; Code:

(require 'supervisor-test-helpers)

;;; Phase 1: Plan Builder Tests

(ert-deftest supervisor-test-plan-shape ()
  "Plan struct has all required fields with correct types."
  (let* ((programs '(("sleep 100" :id "a")
                     ("sleep 200" :id "b" :after "a")
                     ("invalid-entry" :unknown-keyword t)))
         (plan (supervisor--build-plan programs)))
    ;; Plan is a struct
    (should (supervisor-plan-p plan))
    ;; entries is a list of valid parsed entries
    (should (listp (supervisor-plan-entries plan)))
    (should (= 2 (length (supervisor-plan-entries plan))))
    ;; invalid is a hash table with reasons
    (should (hash-table-p (supervisor-plan-invalid plan)))
    (should (= 1 (hash-table-count (supervisor-plan-invalid plan))))
    ;; by-target is a flat list of entries (globally sorted)
    (should (listp (supervisor-plan-by-target plan)))
    (should (= 2 (length (supervisor-plan-by-target plan))))
    ;; deps is a hash table
    (should (hash-table-p (supervisor-plan-deps plan)))
    ;; dependents is a hash table
    (should (hash-table-p (supervisor-plan-dependents plan)))
    ;; cycle-fallback-ids is a hash table
    (should (hash-table-p (supervisor-plan-cycle-fallback-ids plan)))
    ;; order-index is a hash table
    (should (hash-table-p (supervisor-plan-order-index plan)))
    ;; New Phase 3 fields are nil when no activation root
    (should-not (supervisor-plan-target-members plan))
    (should-not (supervisor-plan-activation-root plan))
    (should-not (supervisor-plan-activation-closure plan))
    ;; meta is a plist with version and timestamp
    (should (listp (supervisor-plan-meta plan)))
    (should (plist-get (supervisor-plan-meta plan) :version))
    (should (plist-get (supervisor-plan-meta plan) :timestamp))))

(ert-deftest supervisor-test-plan-determinism ()
  "Identical config produces identical plan data."
  (let* ((programs '(("sleep 100" :id "a")
                     ("sleep 200" :id "b" :after "a")
                     ("sleep 300" :id "c")))
         (plan1 (supervisor--build-plan programs))
         (plan2 (supervisor--build-plan programs)))
    ;; Entries should be equal
    (should (equal (supervisor-plan-entries plan1)
                   (supervisor-plan-entries plan2)))
    ;; By-target should be equal
    (should (equal (supervisor-plan-by-target plan1)
                   (supervisor-plan-by-target plan2)))
    ;; Deps should be equal
    (should (= (hash-table-count (supervisor-plan-deps plan1))
               (hash-table-count (supervisor-plan-deps plan2))))
    (maphash (lambda (k v)
               (should (equal v (gethash k (supervisor-plan-deps plan2)))))
             (supervisor-plan-deps plan1))))

(ert-deftest supervisor-test-plan-no-global-mutation ()
  "Plan building does not mutate global state."
  (let ((programs '(("sleep 100" :id "a")
                    ("sleep 200" :id "b" :after "a"))))
    ;; Set up globals with known values
    (clrhash supervisor--invalid)
    (clrhash supervisor--cycle-fallback-ids)
    (clrhash supervisor--computed-deps)
    (puthash "sentinel" "should-remain" supervisor--invalid)
    (puthash "sentinel" t supervisor--cycle-fallback-ids)
    (puthash "sentinel" '("test") supervisor--computed-deps)
    ;; Build plan
    (supervisor--build-plan programs)
    ;; Globals should be unchanged
    (should (equal "should-remain" (gethash "sentinel" supervisor--invalid)))
    (should (eq t (gethash "sentinel" supervisor--cycle-fallback-ids)))
    (should (equal '("test") (gethash "sentinel" supervisor--computed-deps)))
    ;; Globals should NOT have plan's computed data
    (should-not (gethash "a" supervisor--computed-deps))
    (should-not (gethash "b" supervisor--computed-deps))))

(ert-deftest supervisor-test-plan-cycle-detection ()
  "Plan correctly detects cycles and falls back."
  (let* ((programs '(("sleep 100" :id "a" :after "b")
                     ("sleep 200" :id "b" :after "a")))
         (plan (supervisor--build-plan programs)))
    ;; Both entries should be marked as cycle fallback
    (should (gethash "a" (supervisor-plan-cycle-fallback-ids plan)))
    (should (gethash "b" (supervisor-plan-cycle-fallback-ids plan)))
    ;; Deps should be cleared after fallback
    (should (null (gethash "a" (supervisor-plan-deps plan))))
    (should (null (gethash "b" (supervisor-plan-deps plan))))))

(ert-deftest supervisor-test-plan-dependency-validation ()
  "Plan validates :after references correctly."
  (let* ((programs '(("sleep 100" :id "a")
                     ("sleep 200" :id "b" :after "a")
                     ("sleep 300" :id "c" :after "nonexistent")
                     ("sleep 400" :id "d" :after "a")))
         (plan (supervisor--build-plan programs)))
    ;; b's dep on a should be preserved
    (should (equal '("a") (gethash "b" (supervisor-plan-deps plan))))
    ;; c's dep on nonexistent should be removed
    (should (null (gethash "c" (supervisor-plan-deps plan))))
    ;; d's dep on a should be preserved
    (should (equal '("a") (gethash "d" (supervisor-plan-deps plan))))))

(ert-deftest supervisor-test-plan-duplicate-id-first-occurrence-order ()
  "Duplicate IDs use first-occurrence index for ordering.
Regression test: duplicates must not overwrite order-index of kept entry."
  (let* ((programs '(("sleep 100" :id "a")
                     ("sleep 200" :id "b")
                     ("sleep 300" :id "a")))  ; duplicate of a
         (plan (supervisor--build-plan programs)))
    ;; Only 2 entries should be in plan (a and b, duplicate skipped)
    (should (= 2 (length (supervisor-plan-entries plan))))
    ;; Order index should reflect first occurrence: a=0, b=1
    (should (= 0 (gethash "a" (supervisor-plan-order-index plan))))
    (should (= 1 (gethash "b" (supervisor-plan-order-index plan))))
    ;; Sorted order should be a, b (not b, a)
    (let* ((sorted (supervisor-plan-by-target plan))
           (ids (mapcar #'car sorted)))
      (should (equal '("a" "b") ids)))))

;;; Phase 2: Startup Uses Plan Tests

(ert-deftest supervisor-test-start-sets-current-plan-and-closure ()
  "Supervisor-start stores active plan metadata for status surfaces."
  (supervisor-test-with-unit-files
      '((nil :id "graphical.target" :type target)
        (nil :id "rescue.target" :type target)
        (nil :id "default.target" :type target))
    (let ((supervisor--processes (make-hash-table :test 'equal))
          (supervisor--entry-state (make-hash-table :test 'equal))
          (supervisor--failed (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--restart-override (make-hash-table :test 'equal))
          (supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor--mask-override (make-hash-table :test 'equal))
          (supervisor--logging (make-hash-table :test 'equal))
          (supervisor--writers (make-hash-table :test 'equal))
          (supervisor--stderr-writers (make-hash-table :test 'equal))
          (supervisor--stderr-pipes (make-hash-table :test 'equal))
          (supervisor--restart-times (make-hash-table :test 'equal))
          (supervisor--restart-timers (make-hash-table :test 'equal))
          (supervisor--last-exit-info (make-hash-table :test 'equal))
          (supervisor--oneshot-completed (make-hash-table :test 'equal))
          (supervisor--remain-active (make-hash-table :test 'equal))
          (supervisor--start-times (make-hash-table :test 'equal))
          (supervisor--ready-times (make-hash-table :test 'equal))
          (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
          (supervisor--computed-deps (make-hash-table :test 'equal))
          (supervisor--spawn-failure-reason (make-hash-table :test 'equal))
          (supervisor--manually-stopped (make-hash-table :test 'equal))
          (supervisor--manually-started (make-hash-table :test 'equal))
          (supervisor--current-plan nil)
          (supervisor-default-target "default.target")
          (supervisor-default-target-link "graphical.target")
          (supervisor--default-target-link-override nil)
          (supervisor--shutting-down t)
          (started-ids nil))
      (cl-letf (((symbol-function 'supervisor--stop-all-processes)
                 (lambda (callback) (funcall callback)))
                ((symbol-function 'supervisor--load-overrides) #'ignore)
                ((symbol-function 'supervisor--start-entries-async)
                 (lambda (entries callback &optional _target-members)
                   (setq started-ids (mapcar #'supervisor-entry-id entries))
                   (funcall callback))))
        (supervisor-start))
      (should (supervisor-plan-p supervisor--current-plan))
      (let ((closure (supervisor-plan-activation-closure supervisor--current-plan)))
        (should (hash-table-p closure))
        (should (gethash "graphical.target" closure))
        (should-not (gethash "rescue.target" closure)))
      (should (member "graphical.target" started-ids))
      (should-not (member "rescue.target" started-ids)))))

(ert-deftest supervisor-test-startup-populates-globals-from-plan ()
  "Plan->global copy mechanism populates legacy globals correctly.
Unit test for the plan data extraction used by supervisor-start.
Note: Does not call supervisor-start directly to avoid process spawning."
  (supervisor-test-with-unit-files
      '(("sleep 100" :id "a")
        ("sleep 200" :id "b" :after "a")
        ("invalid-cmd" :id "invalid" :restart t :no-restart t))
    (let ((supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
          (supervisor--computed-deps (make-hash-table :test 'equal))
          (supervisor--entry-state (make-hash-table :test 'equal))
          (supervisor--processes (make-hash-table :test 'equal))
          (supervisor--failed (make-hash-table :test 'equal))
          (supervisor--restart-override (make-hash-table :test 'equal))
          (supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor--logging (make-hash-table :test 'equal))
          (supervisor--restart-times (make-hash-table :test 'equal))
          (supervisor--restart-timers (make-hash-table :test 'equal))
          (supervisor--oneshot-completed (make-hash-table :test 'equal))
          (supervisor--start-times (make-hash-table :test 'equal))
          (supervisor--ready-times (make-hash-table :test 'equal))
          (supervisor--timers nil)
          (supervisor--shutting-down t))  ; Prevent actual process starts
      ;; Build plan and populate globals (same as supervisor-start)
      (let* ((progs (supervisor--effective-programs))
             (plan (supervisor--build-plan progs)))
        (supervisor--merge-unit-file-invalid)
        (maphash (lambda (k v) (puthash k v supervisor--invalid))
                 (supervisor-plan-invalid plan))
        (maphash (lambda (k v) (puthash k v supervisor--cycle-fallback-ids))
                 (supervisor-plan-cycle-fallback-ids plan))
        (maphash (lambda (k v) (puthash k v supervisor--computed-deps))
                 (supervisor-plan-deps plan)))
      ;; Verify globals were populated from plan
      (should (gethash "invalid" supervisor--invalid))  ; Invalid entry recorded
      (should (equal '("a") (gethash "b" supervisor--computed-deps)))  ; Deps computed
      (should (= 1 (hash-table-count supervisor--invalid))))))

(ert-deftest supervisor-test-plan-build-warns-on-duplicates ()
  "Plan building emits warnings for duplicate IDs.
Regression test for warning parity with legacy startup path."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-unit-directory dir)
         (supervisor--programs-cache :not-yet-loaded)
         (supervisor--unit-file-invalid (make-hash-table :test 'equal))
         (messages nil))
    (with-temp-file (expand-file-name "dup1.el" dir)
      (insert "(:id \"dup\" :command \"sleep 100\")"))
    (with-temp-file (expand-file-name "dup2.el" dir)
      (insert "(:id \"dup\" :command \"sleep 200\")"))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'supervisor--log)
                     (lambda (_level fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (supervisor--build-plan (supervisor--effective-programs)))
          ;; Should have warned about duplicate
          (should (cl-some (lambda (m) (string-match-p "Duplicate.*ID 'dup'" m))
                           messages)))
      (delete-directory dir t))))

(ert-deftest supervisor-test-plan-build-warns-on-invalid-after ()
  "Plan building emits warnings for invalid :after references.
Regression test for warning parity with legacy startup path."
  (supervisor-test-with-unit-files
      '(("sleep 100" :id "a")
        ("sleep 200" :id "b" :after "nonexistent"))
    (let ((messages nil))
      (cl-letf (((symbol-function 'supervisor--log)
                 (lambda (_level fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (supervisor--build-plan (supervisor--effective-programs)))
      ;; Should have warned about nonexistent dep
      (should (cl-some (lambda (m) (string-match-p "does not exist" m))
                       messages)))))

;;; Phase 3: Snapshot-Based Read Model Tests

(ert-deftest supervisor-test-snapshot-shape ()
  "Snapshot struct has all required fields."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--oneshot-completed (make-hash-table :test 'equal))
        (supervisor--entry-state (make-hash-table :test 'equal))
        (supervisor--invalid (make-hash-table :test 'equal))
        (supervisor--enabled-override (make-hash-table :test 'equal))
        (supervisor--restart-override (make-hash-table :test 'equal))
        (supervisor--logging (make-hash-table :test 'equal)))
    (let ((snapshot (supervisor--build-snapshot)))
      (should (supervisor-snapshot-p snapshot))
      (should (hash-table-p (supervisor-snapshot-process-alive snapshot)))
      (should (hash-table-p (supervisor-snapshot-process-pids snapshot)))
      (should (hash-table-p (supervisor-snapshot-failed snapshot)))
      (should (hash-table-p (supervisor-snapshot-oneshot-exit snapshot)))
      (should (hash-table-p (supervisor-snapshot-entry-state snapshot)))
      (should (hash-table-p (supervisor-snapshot-invalid snapshot)))
      (should (hash-table-p (supervisor-snapshot-enabled-override snapshot)))
      (should (hash-table-p (supervisor-snapshot-restart-override snapshot)))
      (should (hash-table-p (supervisor-snapshot-logging-override snapshot)))
      (should (numberp (supervisor-snapshot-timestamp snapshot))))))

(ert-deftest supervisor-test-snapshot-captures-state ()
  "Snapshot captures current runtime state correctly."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--oneshot-completed (make-hash-table :test 'equal))
        (supervisor--entry-state (make-hash-table :test 'equal))
        (supervisor--invalid (make-hash-table :test 'equal))
        (supervisor--enabled-override (make-hash-table :test 'equal))
        (supervisor--restart-override (make-hash-table :test 'equal))
        (supervisor--logging (make-hash-table :test 'equal)))
    ;; Set up some state
    (puthash "test-failed" t supervisor--failed)
    (puthash "test-oneshot" 0 supervisor--oneshot-completed)
    (puthash "test-entry" 'running supervisor--entry-state)
    (puthash "test-invalid" "bad config" supervisor--invalid)
    (puthash "test-enabled" 'disabled supervisor--enabled-override)
    ;; Build snapshot
    (let ((snapshot (supervisor--build-snapshot)))
      ;; Verify state was captured
      (should (gethash "test-failed" (supervisor-snapshot-failed snapshot)))
      (should (= 0 (gethash "test-oneshot" (supervisor-snapshot-oneshot-exit snapshot))))
      (should (eq 'running (gethash "test-entry" (supervisor-snapshot-entry-state snapshot))))
      (should (equal "bad config" (gethash "test-invalid" (supervisor-snapshot-invalid snapshot))))
      (should (eq 'disabled (gethash "test-enabled" (supervisor-snapshot-enabled-override snapshot)))))))

(ert-deftest supervisor-test-status-from-snapshot-parity ()
  "Status computation from snapshot matches direct global access."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--oneshot-completed (make-hash-table :test 'equal))
        (supervisor--entry-state (make-hash-table :test 'equal))
        (supervisor--invalid (make-hash-table :test 'equal))
        (supervisor--enabled-override (make-hash-table :test 'equal))
        (supervisor--restart-override (make-hash-table :test 'equal))
        (supervisor--logging (make-hash-table :test 'equal)))
    ;; Set up state for a failed entry
    (puthash "test" t supervisor--failed)
    (puthash "oneshot-done" 0 supervisor--oneshot-completed)
    ;; Build snapshot
    (let ((snapshot (supervisor--build-snapshot)))
      ;; Status from globals
      (let ((status-globals (supervisor--compute-entry-status "test" 'simple)))
        ;; Status from snapshot
        (let ((status-snapshot (supervisor--compute-entry-status "test" 'simple snapshot)))
          (should (equal status-globals status-snapshot))))
      ;; Test oneshot status
      (let ((status-globals (supervisor--compute-entry-status "oneshot-done" 'oneshot)))
        (let ((status-snapshot (supervisor--compute-entry-status "oneshot-done" 'oneshot snapshot)))
          (should (equal status-globals status-snapshot)))))))

(ert-deftest supervisor-test-health-summary-from-snapshot-parity ()
  "Health summary from snapshot matches direct global access."
  (supervisor-test-with-unit-files
      '(("sleep 100" :id "a")
        ("sleep 200" :id "b"))
    (let ((supervisor--processes (make-hash-table :test 'equal))
          (supervisor--failed (make-hash-table :test 'equal))
          (supervisor--oneshot-completed (make-hash-table :test 'equal))
          (supervisor--entry-state (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor--restart-override (make-hash-table :test 'equal))
          (supervisor--logging (make-hash-table :test 'equal)))
      ;; Mark one as failed
      (puthash "a" t supervisor--failed)
      ;; Build snapshot
      (let ((snapshot (supervisor--build-snapshot)))
        ;; Health from globals
        (let ((health-globals (supervisor--health-summary)))
          ;; Health from snapshot
          (let ((health-snapshot (supervisor--health-summary snapshot)))
            (should (equal health-globals health-snapshot))))))))

(ert-deftest supervisor-test-reason-from-snapshot-parity ()
  "Reason computation from snapshot matches direct global access."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--oneshot-completed (make-hash-table :test 'equal))
        (supervisor--entry-state (make-hash-table :test 'equal))
        (supervisor--invalid (make-hash-table :test 'equal))
        (supervisor--enabled-override (make-hash-table :test 'equal))
        (supervisor--restart-override (make-hash-table :test 'equal))
        (supervisor--logging (make-hash-table :test 'equal)))
    ;; Set up various states
    (puthash "disabled" 'disabled supervisor--entry-state)
    (puthash "delayed" 'delayed supervisor--entry-state)
    (puthash "waiting" 'waiting-on-deps supervisor--entry-state)
    (puthash "crashed" t supervisor--failed)
    ;; Build snapshot
    (let ((snapshot (supervisor--build-snapshot)))
      ;; Test each case
      (dolist (test-case '(("disabled" . simple)
                           ("delayed" . simple)
                           ("waiting" . simple)
                           ("crashed" . simple)))
        (let ((id (car test-case))
              (type (cdr test-case)))
          (should (equal (supervisor--compute-entry-reason id type)
                         (supervisor--compute-entry-reason id type snapshot))))))))

(ert-deftest supervisor-test-initial-dashboard-uses-shared-snapshot ()
  "Initial dashboard render uses same snapshot for entries and header.
Regression test: M-x supervisor must use shared snapshot like refresh does."
  (supervisor-test-with-unit-files
      '(("sleep 100" :id "a"))
    (let ((supervisor--processes (make-hash-table :test 'equal))
          (supervisor--failed (make-hash-table :test 'equal))
          (supervisor--oneshot-completed (make-hash-table :test 'equal))
          (supervisor--entry-state (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor--restart-override (make-hash-table :test 'equal))
          (supervisor--logging (make-hash-table :test 'equal))
          (snapshots-built 0))
      ;; Track how many snapshots are built during initial render
      (cl-letf (((symbol-function 'supervisor--build-snapshot)
                 (lambda ()
                   (cl-incf snapshots-built)
                   (supervisor-snapshot--create
                    :process-alive (make-hash-table :test 'equal)
                    :process-pids (make-hash-table :test 'equal)
                    :failed (copy-hash-table supervisor--failed)
                    :oneshot-exit (copy-hash-table supervisor--oneshot-completed)
                    :entry-state (copy-hash-table supervisor--entry-state)
                    :invalid (copy-hash-table supervisor--invalid)
                    :enabled-override (copy-hash-table supervisor--enabled-override)
                    :restart-override (copy-hash-table supervisor--restart-override)
                    :logging-override (copy-hash-table supervisor--logging)
                    :timestamp (float-time)))))
        (unwind-protect
            (progn
              (supervisor)
              ;; Should build exactly one snapshot for initial render
              (should (= 1 snapshots-built)))
          ;; Cleanup
          (when-let* ((buf (get-buffer "*supervisor*")))
            (kill-buffer buf)))))))

(ert-deftest supervisor-test-dashboard-header-and-services-row ()
  "Dashboard shows counters in header and services columns in body row 1."
  (supervisor-test-with-unit-files
      '(("true" :id "svc" :type oneshot))
    (let ((supervisor--processes (make-hash-table :test 'equal))
          (supervisor--failed (make-hash-table :test 'equal))
          (supervisor--oneshot-completed (make-hash-table :test 'equal))
          (supervisor--entry-state (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor--restart-override (make-hash-table :test 'equal))
          (supervisor--logging (make-hash-table :test 'equal)))
      (unwind-protect
          (progn
            (supervisor)
            (with-current-buffer "*supervisor*"
              (let* ((raw header-line-format)
                     (header (if (stringp raw)
                                 (substring-no-properties raw)
                               (format "%s" raw))))
                (should (string-match-p "\\brun\\b" header))
                (should (string-match-p "\\bdone\\b" header))
                (should (string-match-p "\\bpend\\b" header))
                (should (string-match-p "\\bfail\\b" header))
                (should (string-match-p "\\binv\\b" header))
                (should-not (string-match-p "\\bID\\b" header)))
              (let* ((first-row (car tabulated-list-entries))
                     (first-id (car first-row))
                     (first-vec (cadr first-row)))
                (should (eq '--services-- first-id))
                (should (string-match-p "TYPE"
                                        (substring-no-properties (aref first-vec 1))))
                (should (string-match-p "TARGET"
                                        (substring-no-properties (aref first-vec 2))))
                (should (string-match-p "PID"
                                        (substring-no-properties (aref first-vec 7)))))))
        (when-let* ((buf (get-buffer "*supervisor*")))
          (kill-buffer buf))))))

;;; Phase 4: Declarative Reconciler Tests

(ert-deftest supervisor-test-compute-actions-start ()
  "Compute-actions returns start action for entries not running."
  (supervisor-test-with-unit-files
      '(("sleep 100" :id "a"))
    (let* ((plan (supervisor--build-plan (supervisor--effective-programs)))
           ;; Empty snapshot - nothing running
           (snapshot (supervisor-snapshot--create
                      :process-alive (make-hash-table :test 'equal)
                      :process-pids (make-hash-table :test 'equal)
                      :failed (make-hash-table :test 'equal)
                      :oneshot-exit (make-hash-table :test 'equal)
                      :entry-state (make-hash-table :test 'equal)
                      :invalid (make-hash-table :test 'equal)
                      :enabled-override (make-hash-table :test 'equal)
                      :restart-override (make-hash-table :test 'equal)
                      :logging-override (make-hash-table :test 'equal)
                      :timestamp (float-time)))
           (actions (supervisor--compute-actions plan snapshot)))
      (should (= 1 (length actions)))
      (should (eq 'start (plist-get (car actions) :op)))
      (should (equal "a" (plist-get (car actions) :id)))
      (should (eq 'new-entry (plist-get (car actions) :reason))))))

(ert-deftest supervisor-test-compute-actions-stop-removed ()
  "Compute-actions returns stop action for running entries not in plan."
  (supervisor-test-with-unit-files nil
    (let* ((plan (supervisor--build-plan (supervisor--effective-programs)))
           ;; Snapshot shows a running process
           (process-alive (make-hash-table :test 'equal)))
      (puthash "orphan" t process-alive)
      (let* ((snapshot (supervisor-snapshot--create
                        :process-alive process-alive
                        :process-pids (make-hash-table :test 'equal)
                        :failed (make-hash-table :test 'equal)
                        :oneshot-exit (make-hash-table :test 'equal)
                        :entry-state (make-hash-table :test 'equal)
                        :invalid (make-hash-table :test 'equal)
                        :enabled-override (make-hash-table :test 'equal)
                        :restart-override (make-hash-table :test 'equal)
                        :logging-override (make-hash-table :test 'equal)
                        :timestamp (float-time)))
             (actions (supervisor--compute-actions plan snapshot)))
        (should (= 1 (length actions)))
        (should (eq 'stop (plist-get (car actions) :op)))
        (should (equal "orphan" (plist-get (car actions) :id)))
        (should (eq 'removed (plist-get (car actions) :reason)))))))

(ert-deftest supervisor-test-compute-actions-stop-disabled ()
  "Compute-actions returns stop action for running entries now disabled."
  (supervisor-test-with-unit-files
      '(("sleep 100" :id "a" :disabled t))
    (let* ((plan (supervisor--build-plan (supervisor--effective-programs)))
           ;; Snapshot shows process is running
           (process-alive (make-hash-table :test 'equal)))
      (puthash "a" t process-alive)
      (let* ((snapshot (supervisor-snapshot--create
                        :process-alive process-alive
                        :process-pids (make-hash-table :test 'equal)
                        :failed (make-hash-table :test 'equal)
                        :oneshot-exit (make-hash-table :test 'equal)
                        :entry-state (make-hash-table :test 'equal)
                        :invalid (make-hash-table :test 'equal)
                        :enabled-override (make-hash-table :test 'equal)
                        :restart-override (make-hash-table :test 'equal)
                        :logging-override (make-hash-table :test 'equal)
                        :timestamp (float-time)))
             (actions (supervisor--compute-actions plan snapshot)))
        ;; Should have stop action for disabled entry
        (let ((stop-action (cl-find 'stop actions :key (lambda (a) (plist-get a :op)))))
          (should stop-action)
          (should (equal "a" (plist-get stop-action :id)))
          (should (eq 'disabled (plist-get stop-action :reason))))))))

(ert-deftest supervisor-test-compute-actions-noop-already-running ()
  "Compute-actions returns noop for entries already running."
  (supervisor-test-with-unit-files
      '(("sleep 100" :id "a"))
    (let* ((plan (supervisor--build-plan (supervisor--effective-programs)))
           ;; Snapshot shows process is running
           (process-alive (make-hash-table :test 'equal)))
      (puthash "a" t process-alive)
      (let* ((snapshot (supervisor-snapshot--create
                        :process-alive process-alive
                        :process-pids (make-hash-table :test 'equal)
                        :failed (make-hash-table :test 'equal)
                        :oneshot-exit (make-hash-table :test 'equal)
                        :entry-state (make-hash-table :test 'equal)
                        :invalid (make-hash-table :test 'equal)
                        :enabled-override (make-hash-table :test 'equal)
                        :restart-override (make-hash-table :test 'equal)
                        :logging-override (make-hash-table :test 'equal)
                        :timestamp (float-time)))
             (actions (supervisor--compute-actions plan snapshot)))
        (should (= 1 (length actions)))
        (should (eq 'noop (plist-get (car actions) :op)))
        (should (equal "a" (plist-get (car actions) :id)))
        (should (eq 'already-running (plist-get (car actions) :reason)))))))

(ert-deftest supervisor-test-compute-actions-skip-failed ()
  "Compute-actions returns skip for failed entries."
  (supervisor-test-with-unit-files
      '(("sleep 100" :id "a"))
    (let* ((plan (supervisor--build-plan (supervisor--effective-programs)))
           ;; Snapshot shows entry is failed
           (failed (make-hash-table :test 'equal)))
      (puthash "a" t failed)
      (let* ((snapshot (supervisor-snapshot--create
                        :process-alive (make-hash-table :test 'equal)
                        :process-pids (make-hash-table :test 'equal)
                        :failed failed
                        :oneshot-exit (make-hash-table :test 'equal)
                        :entry-state (make-hash-table :test 'equal)
                        :invalid (make-hash-table :test 'equal)
                        :enabled-override (make-hash-table :test 'equal)
                        :restart-override (make-hash-table :test 'equal)
                        :logging-override (make-hash-table :test 'equal)
                        :timestamp (float-time)))
             (actions (supervisor--compute-actions plan snapshot)))
        (should (= 1 (length actions)))
        (should (eq 'skip (plist-get (car actions) :op)))
        (should (equal "a" (plist-get (car actions) :id)))
        (should (eq 'failed (plist-get (car actions) :reason)))))))

(ert-deftest supervisor-test-compute-actions-skip-oneshot-completed ()
  "Compute-actions returns skip for completed oneshots."
  (supervisor-test-with-unit-files
      '(("echo done" :id "a" :type oneshot))
    (let* ((plan (supervisor--build-plan (supervisor--effective-programs)))
           ;; Snapshot shows oneshot completed
           (oneshot-exit (make-hash-table :test 'equal)))
      (puthash "a" 0 oneshot-exit)
      (let* ((snapshot (supervisor-snapshot--create
                        :process-alive (make-hash-table :test 'equal)
                        :process-pids (make-hash-table :test 'equal)
                        :failed (make-hash-table :test 'equal)
                        :oneshot-exit oneshot-exit
                        :entry-state (make-hash-table :test 'equal)
                        :invalid (make-hash-table :test 'equal)
                        :enabled-override (make-hash-table :test 'equal)
                        :restart-override (make-hash-table :test 'equal)
                        :logging-override (make-hash-table :test 'equal)
                        :timestamp (float-time)))
             (actions (supervisor--compute-actions plan snapshot)))
        (should (= 1 (length actions)))
        (should (eq 'skip (plist-get (car actions) :op)))
        (should (equal "a" (plist-get (car actions) :id)))
        (should (eq 'oneshot-completed (plist-get (car actions) :reason)))))))

(ert-deftest supervisor-test-compute-actions-skip-disabled ()
  "Compute-actions returns skip for disabled entries not running."
  (supervisor-test-with-unit-files
      '(("sleep 100" :id "a" :disabled t))
    (let* ((plan (supervisor--build-plan (supervisor--effective-programs)))
           ;; Empty snapshot - nothing running
           (snapshot (supervisor-snapshot--create
                      :process-alive (make-hash-table :test 'equal)
                      :process-pids (make-hash-table :test 'equal)
                      :failed (make-hash-table :test 'equal)
                      :oneshot-exit (make-hash-table :test 'equal)
                      :entry-state (make-hash-table :test 'equal)
                      :invalid (make-hash-table :test 'equal)
                      :enabled-override (make-hash-table :test 'equal)
                      :restart-override (make-hash-table :test 'equal)
                      :logging-override (make-hash-table :test 'equal)
                      :timestamp (float-time)))
           (actions (supervisor--compute-actions plan snapshot)))
      (should (= 1 (length actions)))
      (should (eq 'skip (plist-get (car actions) :op)))
      (should (equal "a" (plist-get (car actions) :id)))
      (should (eq 'disabled (plist-get (car actions) :reason))))))

(ert-deftest supervisor-test-compute-actions-is-pure ()
  "Compute-actions does not modify any global state."
  (supervisor-test-with-unit-files
      '(("sleep 100" :id "a")
        ("sleep 200" :id "b" :disabled t))
    (let* ((plan (supervisor--build-plan (supervisor--effective-programs)))
           ;; Set up some mutable globals
           (supervisor--processes (make-hash-table :test 'equal))
           (supervisor--failed (make-hash-table :test 'equal))
           (supervisor--oneshot-completed (make-hash-table :test 'equal))
           (supervisor--restart-override (make-hash-table :test 'equal)))
      (puthash "x" t supervisor--failed)
      (let* ((failed-before (copy-hash-table supervisor--failed))
             (process-alive (make-hash-table :test 'equal))
             (snapshot (supervisor-snapshot--create
                        :process-alive process-alive
                        :process-pids (make-hash-table :test 'equal)
                        :failed (copy-hash-table supervisor--failed)
                        :oneshot-exit (make-hash-table :test 'equal)
                        :entry-state (make-hash-table :test 'equal)
                        :invalid (make-hash-table :test 'equal)
                        :enabled-override (make-hash-table :test 'equal)
                        :restart-override (make-hash-table :test 'equal)
                        :logging-override (make-hash-table :test 'equal)
                        :timestamp (float-time))))
        ;; Call compute-actions multiple times
        (supervisor--compute-actions plan snapshot)
        (supervisor--compute-actions plan snapshot)
        ;; Globals should be unchanged
        (should (equal (hash-table-count supervisor--failed)
                       (hash-table-count failed-before)))
        (should (gethash "x" supervisor--failed))
        (should (= 0 (hash-table-count supervisor--processes)))
        (should (= 0 (hash-table-count supervisor--restart-override)))))))

(ert-deftest supervisor-test-compute-actions-idempotent ()
  "Idempotence: second call on converged state produces only noops/skips.
When actual state matches desired state, no start/stop actions are needed."
  (supervisor-test-with-unit-files
      '(("sleep 100" :id "a")
        ("sleep 200" :id "b")
        ("echo done" :id "c" :type oneshot)
        ("sleep 300" :id "d" :disabled t))
    (let* ((plan (supervisor--build-plan (supervisor--effective-programs)))
           ;; Simulate converged state: a and b running, c completed, d disabled
           (process-alive (make-hash-table :test 'equal))
           (oneshot-exit (make-hash-table :test 'equal)))
      (puthash "a" t process-alive)
      (puthash "b" t process-alive)
      (puthash "c" 0 oneshot-exit)
      (let* ((snapshot (supervisor-snapshot--create
                        :process-alive process-alive
                        :process-pids (make-hash-table :test 'equal)
                        :failed (make-hash-table :test 'equal)
                        :oneshot-exit oneshot-exit
                        :entry-state (make-hash-table :test 'equal)
                        :invalid (make-hash-table :test 'equal)
                        :enabled-override (make-hash-table :test 'equal)
                        :restart-override (make-hash-table :test 'equal)
                        :logging-override (make-hash-table :test 'equal)
                        :timestamp (float-time)))
             (actions (supervisor--compute-actions plan snapshot)))
        ;; Should have 4 actions, none of which are start or stop
        (should (= 4 (length actions)))
        (should-not (cl-find 'start actions :key (lambda (a) (plist-get a :op))))
        (should-not (cl-find 'stop actions :key (lambda (a) (plist-get a :op))))
        ;; All should be noop or skip
        (dolist (action actions)
          (should (memq (plist-get action :op) '(noop skip))))))))

(ert-deftest supervisor-test-compute-actions-action-matrix ()
  "Comprehensive test of action matrix with mixed scenarios.
Covers add, remove, disable (running), already-running, failed."
  (supervisor-test-with-unit-files
      '(("sleep 100" :id "a")
        ("sleep 200" :id "b")
        ("sleep 300" :id "c" :disabled t)
        ("sleep 400" :id "d")
        ("sleep 500" :id "e" :disabled t))
    ;; Plan: a (new), b (already running), c (running but now disabled),
    ;;       d (failed), e (disabled from start)
    (let* ((plan (supervisor--build-plan (supervisor--effective-programs)))
           ;; Runtime: b and c running, orphan running (removed), d failed
           (process-alive (make-hash-table :test 'equal))
           (failed (make-hash-table :test 'equal)))
      (puthash "b" t process-alive)
      (puthash "c" t process-alive)
      (puthash "orphan" t process-alive)
      (puthash "d" t failed)
      (let* ((snapshot (supervisor-snapshot--create
                        :process-alive process-alive
                        :process-pids (make-hash-table :test 'equal)
                        :failed failed
                        :oneshot-exit (make-hash-table :test 'equal)
                        :entry-state (make-hash-table :test 'equal)
                        :invalid (make-hash-table :test 'equal)
                        :enabled-override (make-hash-table :test 'equal)
                        :restart-override (make-hash-table :test 'equal)
                        :logging-override (make-hash-table :test 'equal)
                        :timestamp (float-time)))
             (actions (supervisor--compute-actions plan snapshot))
             (by-id (make-hash-table :test 'equal)))
        ;; Index actions by id for easy lookup
        (dolist (a actions)
          (puthash (plist-get a :id) a by-id))
        ;; a: should start (added)
        (should (eq 'start (plist-get (gethash "a" by-id) :op)))
        ;; b: should noop (already-running)
        (should (eq 'noop (plist-get (gethash "b" by-id) :op)))
        ;; c: should stop (disabled)
        (should (eq 'stop (plist-get (gethash "c" by-id) :op)))
        ;; orphan: should stop (removed)
        (should (eq 'stop (plist-get (gethash "orphan" by-id) :op)))
        ;; d: should skip (failed)
        (should (eq 'skip (plist-get (gethash "d" by-id) :op)))
        ;; e: should skip (disabled)
        (should (eq 'skip (plist-get (gethash "e" by-id) :op)))))))

(ert-deftest supervisor-test-apply-actions-atomic-on-invalid-plan ()
  "Apply-actions does not mutate state when plan has no matching entries.
This tests the atomic boundary: invalid/empty plans cause no side effects."
  (let* ((supervisor--processes (make-hash-table :test 'equal))
         (supervisor--restart-override (make-hash-table :test 'equal))
         (supervisor--failed (make-hash-table :test 'equal))
         ;; Empty plan (no entries)
         (plan (supervisor--build-plan nil))
         ;; Actions that reference non-existent entries
         (actions (list (list :op 'start :id "nonexistent" :reason 'added)
                        (list :op 'stop :id "also-nonexistent" :reason 'removed))))
    ;; Apply should not crash and should not modify state
    (let ((result (supervisor--apply-actions actions plan)))
      ;; No processes were started or stopped
      (should (= 0 (plist-get result :started)))
      (should (= 0 (plist-get result :stopped)))
      ;; Globals remain empty
      (should (= 0 (hash-table-count supervisor--processes)))
      (should (= 0 (hash-table-count supervisor--restart-override))))))

;;; Phase 5: Explicit FSM Adoption Tests

(ert-deftest supervisor-test-fsm-valid-states-defined ()
  "All valid states are defined in the state list."
  (should (memq 'pending supervisor--valid-states))
  (should (memq 'waiting-on-deps supervisor--valid-states))
  (should (memq 'delayed supervisor--valid-states))
  (should (memq 'disabled supervisor--valid-states))
  (should (memq 'started supervisor--valid-states))
  (should (memq 'failed-to-spawn supervisor--valid-states))
  (should (memq 'startup-timeout supervisor--valid-states)))

(ert-deftest supervisor-test-fsm-invalid-state-rejected ()
  "Transitioning to an invalid state signals an error."
  (let ((supervisor--entry-state (make-hash-table :test 'equal)))
    (should-error (supervisor--transition-state "test" 'invalid-state)
                  :type 'error)))

(ert-deftest supervisor-test-fsm-invalid-transition-rejected ()
  "Invalid state transitions signal an error."
  (let ((supervisor--entry-state (make-hash-table :test 'equal)))
    ;; Set up valid path to terminal state
    (supervisor--transition-state "test" 'pending)
    (supervisor--transition-state "test" 'started)
    ;; Transition from terminal state should error
    (should-error (supervisor--transition-state "test" 'delayed)
                  :type 'error)))

(ert-deftest supervisor-test-fsm-valid-transitions ()
  "Valid state transitions succeed."
  (let ((supervisor--entry-state (make-hash-table :test 'equal)))
    ;; nil -> pending
    (should (supervisor--transition-state "a" 'pending))
    (should (eq 'pending (gethash "a" supervisor--entry-state)))
    ;; pending -> waiting-on-deps
    (should (supervisor--transition-state "a" 'waiting-on-deps))
    (should (eq 'waiting-on-deps (gethash "a" supervisor--entry-state)))
    ;; waiting-on-deps -> delayed
    (should (supervisor--transition-state "a" 'delayed))
    (should (eq 'delayed (gethash "a" supervisor--entry-state)))
    ;; delayed -> started
    (should (supervisor--transition-state "a" 'started))
    (should (eq 'started (gethash "a" supervisor--entry-state)))))

(ert-deftest supervisor-test-fsm-self-transitions-allowed ()
  "Self-transitions are allowed for idempotent operations."
  (let ((supervisor--entry-state (make-hash-table :test 'equal)))
    (supervisor--transition-state "a" 'pending)
    ;; Self-transition should succeed
    (should (supervisor--transition-state "a" 'pending))
    (should (eq 'pending (gethash "a" supervisor--entry-state)))))

(ert-deftest supervisor-test-fsm-force-invalid-transition ()
  "Force parameter allows invalid transitions with warning.
Returns nil (not t) and emits a warning for forced invalid transitions."
  (let ((supervisor--entry-state (make-hash-table :test 'equal))
        (warnings nil))
    ;; Set up valid path to terminal state
    (supervisor--transition-state "a" 'pending)
    (supervisor--transition-state "a" 'started)
    (cl-letf (((symbol-function 'supervisor--log)
               (lambda (_level fmt &rest args)
                 (push (apply #'format fmt args) warnings))))
      ;; Force invalid transition from terminal state - should return nil
      (let ((result (supervisor--transition-state "a" 'delayed t)))
        ;; Return value should be nil for forced invalid transition
        (should (null result))
        ;; State should change despite being invalid
        (should (eq 'delayed (gethash "a" supervisor--entry-state)))
        ;; Warning should have been emitted
        (should (= 1 (length warnings)))
        (should (string-match-p "Forced invalid transition" (car warnings)))))))

(ert-deftest supervisor-test-fsm-lifecycle-simple-success ()
  "Test lifecycle path for successful simple process start."
  (let ((supervisor--entry-state (make-hash-table :test 'equal)))
    ;; Initial state
    (supervisor--transition-state "proc" 'pending)
    ;; After delay
    (supervisor--transition-state "proc" 'delayed)
    ;; After successful spawn
    (supervisor--transition-state "proc" 'started)
    (should (eq 'started (gethash "proc" supervisor--entry-state)))))

(ert-deftest supervisor-test-fsm-lifecycle-spawn-failure ()
  "Test lifecycle path for spawn failure."
  (let ((supervisor--entry-state (make-hash-table :test 'equal)))
    ;; Initial state
    (supervisor--transition-state "proc" 'pending)
    ;; Spawn fails
    (supervisor--transition-state "proc" 'failed-to-spawn)
    (should (eq 'failed-to-spawn (gethash "proc" supervisor--entry-state)))))

(ert-deftest supervisor-test-fsm-lifecycle-startup-timeout ()
  "Test lifecycle path for startup timeout."
  (let ((supervisor--entry-state (make-hash-table :test 'equal)))
    ;; Initial state with deps
    (supervisor--transition-state "proc" 'waiting-on-deps)
    ;; Startup times out before deps complete
    (supervisor--transition-state "proc" 'startup-timeout)
    (should (eq 'startup-timeout (gethash "proc" supervisor--entry-state)))))

(ert-deftest supervisor-test-fsm-lifecycle-disabled ()
  "Test lifecycle path for disabled entry."
  (let ((supervisor--entry-state (make-hash-table :test 'equal)))
    ;; Entry is disabled from the start
    (supervisor--transition-state "proc" 'disabled)
    (should (eq 'disabled (gethash "proc" supervisor--entry-state)))))

(ert-deftest supervisor-test-fsm-lifecycle-oneshot ()
  "Test lifecycle path for oneshot process."
  (let ((supervisor--entry-state (make-hash-table :test 'equal)))
    ;; Initial state
    (supervisor--transition-state "oneshot" 'pending)
    ;; Started (oneshot still uses 'started state)
    (supervisor--transition-state "oneshot" 'started)
    (should (eq 'started (gethash "oneshot" supervisor--entry-state)))))

;;; Phase 6: Structured Event Dispatcher Tests

(ert-deftest supervisor-test-event-types-defined ()
  "All event types are defined in the event type list."
  (should (memq 'startup-begin supervisor--event-types))
  (should (memq 'startup-complete supervisor--event-types))
  (should (memq 'process-started supervisor--event-types))
  (should (memq 'process-ready supervisor--event-types))
  (should (memq 'process-exit supervisor--event-types))
  (should (memq 'process-failed supervisor--event-types))
  (should (memq 'cleanup supervisor--event-types))
  ;; Timer event types
  (should (memq 'timer-trigger supervisor--event-types))
  (should (memq 'timer-overlap supervisor--event-types))
  (should (memq 'timer-success supervisor--event-types))
  (should (memq 'timer-failure supervisor--event-types)))

(ert-deftest supervisor-test-emit-event-invalid-type-rejected ()
  "Emitting an invalid event type signals an error."
  (should-error (supervisor--emit-event 'invalid-event-type nil nil)
                :type 'error))

(ert-deftest supervisor-test-emit-event-schema ()
  "Emitted events have the correct schema."
  (let ((events nil))
    (cl-letf (((symbol-function 'run-hook-with-args)
               (lambda (hook &rest args) (when (eq hook 'supervisor-event-hook)
                                           (push (car args) events))))
              ((symbol-function 'run-hooks) #'ignore))
      (supervisor--emit-event 'startup-begin)
      (let ((event (car events)))
        (should (eq 'startup-begin (plist-get event :type)))
        (should (numberp (plist-get event :ts)))
        (should (null (plist-get event :id)))))))

(ert-deftest supervisor-test-emit-event-startup-begin ()
  "Startup-begin event is emitted with correct data."
  (let ((events nil))
    (cl-letf (((symbol-function 'run-hook-with-args)
               (lambda (hook &rest args)
                 (when (eq hook 'supervisor-event-hook)
                   (push (car args) events))))
              ((symbol-function 'run-hooks) #'ignore))
      (supervisor--emit-event 'startup-begin)
      (should (= 1 (length events)))
      (let ((event (car events)))
        (should (eq 'startup-begin (plist-get event :type)))))))

(ert-deftest supervisor-test-emit-event-process-exit ()
  "Process-exit event carries status and code in data."
  (let ((events nil))
    (cl-letf (((symbol-function 'run-hook-with-args)
               (lambda (hook &rest args)
                 (when (eq hook 'supervisor-event-hook)
                   (push (car args) events))))
              ((symbol-function 'run-hooks) #'ignore))
      (supervisor--emit-event 'process-exit "myproc"
                              (list :status 'exited :code 0))
      (should (= 1 (length events)))
      (let ((event (car events)))
        (should (eq 'process-exit (plist-get event :type)))
        (should (equal "myproc" (plist-get event :id)))
        (should (eq 'exited (plist-get (plist-get event :data) :status)))
        (should (= 0 (plist-get (plist-get event :data) :code)))))))

(ert-deftest supervisor-test-emit-event-process-started ()
  "Process-started event includes type in data."
  (let ((events nil))
    (cl-letf (((symbol-function 'run-hook-with-args)
               (lambda (hook &rest args)
                 (when (eq hook 'supervisor-event-hook)
                   (push (car args) events))))
              ((symbol-function 'run-hooks) #'ignore))
      (supervisor--emit-event 'process-started "daemon"
                              (list :type 'simple))
      (should (= 1 (length events)))
      (let ((event (car events)))
        (should (eq 'process-started (plist-get event :type)))
        (should (equal "daemon" (plist-get event :id)))
        (should (eq 'simple (plist-get (plist-get event :data) :type)))))))

(ert-deftest supervisor-test-spawn-failure-no-process-ready ()
  "Spawn failure emits process-failed but NOT process-ready.
Regression test: process-ready was incorrectly emitted for failures."
  (let ((events nil)
        (supervisor--entry-state (make-hash-table :test 'equal))
        (supervisor--dag-ready (make-hash-table :test 'equal))
        (supervisor--ready-times (make-hash-table :test 'equal))
        (supervisor--dag-dependents (make-hash-table :test 'equal))
        (supervisor--dag-in-degree (make-hash-table :test 'equal))
        (supervisor--dag-blocking (make-hash-table :test 'equal))
        (supervisor--dag-timeout-timers (make-hash-table :test 'equal))
        (supervisor--dag-started (make-hash-table :test 'equal))
        (supervisor--dag-active-starts 1)
        (supervisor--dag-pending-starts nil)
        (supervisor--dag-complete-callback nil)
        (supervisor--dag-entries (make-hash-table :test 'equal)))
    ;; Set up initial state
    (supervisor--transition-state "test" 'pending)
    (puthash "test" '("test" "cmd" 0 t always t simple nil t 30 nil)
             supervisor--dag-entries)
    ;; Capture events
    (cl-letf (((symbol-function 'run-hook-with-args)
               (lambda (hook &rest args)
                 (when (eq hook 'supervisor-event-hook)
                   (push (car args) events))))
              ((symbol-function 'run-hooks) #'ignore))
      (supervisor--dag-handle-spawn-failure "test"))
    ;; Should have process-failed but NOT process-ready
    (should (cl-find 'process-failed events :key (lambda (e) (plist-get e :type))))
    (should-not (cl-find 'process-ready events :key (lambda (e) (plist-get e :type))))))

(ert-deftest supervisor-test-disabled-entry-no-process-ready ()
  "Disabled entries do NOT emit process-ready.
Regression test: process-ready was incorrectly emitted for disabled entries."
  (let ((events nil)
        (supervisor--entry-state (make-hash-table :test 'equal))
        (supervisor--dag-ready (make-hash-table :test 'equal))
        (supervisor--ready-times (make-hash-table :test 'equal))
        (supervisor--dag-dependents (make-hash-table :test 'equal))
        (supervisor--dag-in-degree (make-hash-table :test 'equal))
        (supervisor--dag-blocking (make-hash-table :test 'equal))
        (supervisor--dag-timeout-timers (make-hash-table :test 'equal))
        (supervisor--dag-started (make-hash-table :test 'equal))
        (supervisor--dag-complete-callback nil)
        (supervisor--dag-entries (make-hash-table :test 'equal))
        (supervisor--enabled-override (make-hash-table :test 'equal)))
    ;; Set up as disabled entry
    (puthash "test" '("test" "cmd" 0 nil always t simple nil t 30 nil)
             supervisor--dag-entries)
    ;; Capture events
    (cl-letf (((symbol-function 'run-hook-with-args)
               (lambda (hook &rest args)
                 (when (eq hook 'supervisor-event-hook)
                   (push (car args) events))))
              ((symbol-function 'run-hooks) #'ignore))
      (supervisor--dag-start-entry-async
       '("test" "cmd" 0 nil always t simple nil t 30 nil)))
    ;; Should NOT have process-ready
    (should-not (cl-find 'process-ready events :key (lambda (e) (plist-get e :type))))))

(ert-deftest supervisor-test-simple-spawn-emits-process-ready ()
  "Successful simple process spawn emits process-started AND process-ready."
  (let ((events nil)
        (supervisor--entry-state (make-hash-table :test 'equal))
        (supervisor--dag-ready (make-hash-table :test 'equal))
        (supervisor--ready-times (make-hash-table :test 'equal))
        (supervisor--dag-dependents (make-hash-table :test 'equal))
        (supervisor--dag-in-degree (make-hash-table :test 'equal))
        (supervisor--dag-blocking (make-hash-table :test 'equal))
        (supervisor--dag-timeout-timers (make-hash-table :test 'equal))
        (supervisor--dag-started (make-hash-table :test 'equal))
        (supervisor--dag-active-starts 1)
        (supervisor--dag-pending-starts nil)
        (supervisor--dag-complete-callback nil)
        (supervisor--dag-entries (make-hash-table :test 'equal)))
    ;; Set up initial state
    (supervisor--transition-state "test" 'pending)
    (puthash "test" '("test" "sleep" 0 t always t simple nil t 30 nil)
             supervisor--dag-entries)
    ;; Capture events
    (cl-letf (((symbol-function 'run-hook-with-args)
               (lambda (hook &rest args)
                 (when (eq hook 'supervisor-event-hook)
                   (push (car args) events))))
              ((symbol-function 'run-hooks) #'ignore))
      (supervisor--dag-handle-spawn-success "test" 'simple nil))
    ;; Should have both process-started and process-ready
    (should (cl-find 'process-started events :key (lambda (e) (plist-get e :type))))
    (should (cl-find 'process-ready events :key (lambda (e) (plist-get e :type))))))

(ert-deftest supervisor-test-no-stderr-process-created ()
  "Starting a process does not create a separate stderr process.
Regression test: stderr pipe processes used to pollute the process list."
  (supervisor-test-with-unit-files
      '(("true" :id "test-no-stderr" :type oneshot))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--failed (make-hash-table :test 'equal))
           (supervisor--enabled-override (make-hash-table :test 'equal))
           (supervisor--logging (make-hash-table :test 'equal))
           (supervisor--oneshot-completed (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (supervisor--restart-timestamps (make-hash-table :test 'equal))
           (supervisor-log-directory (make-temp-file "supervisor-test-" t)))
      (unwind-protect
          (progn
            (supervisor--start-process "test-no-stderr" "true" t 'oneshot nil nil)
            ;; Main process should exist
            (should (gethash "test-no-stderr" supervisor--processes))
            ;; No stderr process should exist (with or without space prefix)
            (should-not (get-process "test-no-stderr-stderr"))
            (should-not (get-process " test-no-stderr-stderr")))
        (when-let* ((proc (gethash "test-no-stderr" supervisor--processes)))
          (delete-process proc))
        (delete-directory supervisor-log-directory t)))))

(ert-deftest supervisor-test-dashboard-menu-keybinding ()
  "Dashboard ? key is bound to transient menu wrapper."
  (should (eq (lookup-key supervisor-dashboard-mode-map "?")
              'supervisor-dashboard-menu-open)))

(ert-deftest supervisor-test-dashboard-info-keybinding ()
  "Dashboard i key is bound to inspect submenu."
  (should (eq (lookup-key supervisor-dashboard-mode-map "i")
              'supervisor-dashboard-inspect)))

(ert-deftest supervisor-test-header-hints-default-hidden ()
  "Header hints are hidden by default."
  (should-not supervisor-dashboard-show-header-hints))

(ert-deftest supervisor-test-transient-menu-defined ()
  "Transient menu wrapper is defined and helper function exists."
  ;; The wrapper function should always be defined
  (should (fboundp 'supervisor-dashboard-menu-open))
  ;; The helper that defines the menu should exist
  (should (fboundp 'supervisor--define-dashboard-menu))
  ;; After calling the helper with transient loaded, the menu should exist
  (when (require 'transient nil t)
    (supervisor--define-dashboard-menu)
    (should (fboundp 'supervisor-dashboard-menu))
    (should (get 'supervisor-dashboard-menu 'transient--prefix))))


(provide 'supervisor-test-plan)
;;; supervisor-test-plan.el ends here
