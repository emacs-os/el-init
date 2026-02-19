;;; elinit-test-plan.el --- Plan builder and FSM tests for elinit.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Plan builder and FSM ERT tests for elinit.el.

;;; Code:

(require 'elinit-test-helpers)

;;; Phase 1: Plan Builder Tests

(ert-deftest elinit-test-plan-shape ()
  "Plan struct has all required fields with correct types."
  (let* ((programs '(("sleep 100" :id "a")
                     ("sleep 200" :id "b" :after "a")
                     ("invalid-entry" :unknown-keyword t)))
         (plan (elinit--build-plan programs)))
    ;; Plan is a struct
    (should (elinit-plan-p plan))
    ;; entries is a list of valid parsed entries
    (should (listp (elinit-plan-entries plan)))
    (should (= 2 (length (elinit-plan-entries plan))))
    ;; invalid is a hash table with reasons
    (should (hash-table-p (elinit-plan-invalid plan)))
    (should (= 1 (hash-table-count (elinit-plan-invalid plan))))
    ;; by-target is a flat list of entries (globally sorted)
    (should (listp (elinit-plan-by-target plan)))
    (should (= 2 (length (elinit-plan-by-target plan))))
    ;; deps is a hash table
    (should (hash-table-p (elinit-plan-deps plan)))
    ;; dependents is a hash table
    (should (hash-table-p (elinit-plan-dependents plan)))
    ;; cycle-fallback-ids is a hash table
    (should (hash-table-p (elinit-plan-cycle-fallback-ids plan)))
    ;; order-index is a hash table
    (should (hash-table-p (elinit-plan-order-index plan)))
    ;; conflicts-deps and conflict-reverse are hash tables
    (should (hash-table-p (elinit-plan-conflicts-deps plan)))
    (should (hash-table-p (elinit-plan-conflict-reverse plan)))
    ;; New Phase 3 fields are nil when no activation root
    (should-not (elinit-plan-target-members plan))
    (should-not (elinit-plan-activation-root plan))
    (should-not (elinit-plan-activation-closure plan))
    ;; meta is a plist with version and timestamp
    (should (listp (elinit-plan-meta plan)))
    (should (plist-get (elinit-plan-meta plan) :version))
    (should (plist-get (elinit-plan-meta plan) :timestamp))))

(ert-deftest elinit-test-plan-determinism ()
  "Identical config produces identical plan data."
  (let* ((programs '(("sleep 100" :id "a")
                     ("sleep 200" :id "b" :after "a")
                     ("sleep 300" :id "c")))
         (plan1 (elinit--build-plan programs))
         (plan2 (elinit--build-plan programs)))
    ;; Entries should be equal
    (should (equal (elinit-plan-entries plan1)
                   (elinit-plan-entries plan2)))
    ;; By-target should be equal
    (should (equal (elinit-plan-by-target plan1)
                   (elinit-plan-by-target plan2)))
    ;; Deps should be equal
    (should (= (hash-table-count (elinit-plan-deps plan1))
               (hash-table-count (elinit-plan-deps plan2))))
    (maphash (lambda (k v)
               (should (equal v (gethash k (elinit-plan-deps plan2)))))
             (elinit-plan-deps plan1))))

(ert-deftest elinit-test-plan-no-global-mutation ()
  "Plan building does not mutate global state."
  (let ((programs '(("sleep 100" :id "a")
                    ("sleep 200" :id "b" :after "a"))))
    ;; Set up globals with known values
    (clrhash elinit--invalid)
    (clrhash elinit--cycle-fallback-ids)
    (clrhash elinit--computed-deps)
    (puthash "sentinel" "should-remain" elinit--invalid)
    (puthash "sentinel" t elinit--cycle-fallback-ids)
    (puthash "sentinel" '("test") elinit--computed-deps)
    ;; Build plan
    (elinit--build-plan programs)
    ;; Globals should be unchanged
    (should (equal "should-remain" (gethash "sentinel" elinit--invalid)))
    (should (eq t (gethash "sentinel" elinit--cycle-fallback-ids)))
    (should (equal '("test") (gethash "sentinel" elinit--computed-deps)))
    ;; Globals should NOT have plan's computed data
    (should-not (gethash "a" elinit--computed-deps))
    (should-not (gethash "b" elinit--computed-deps))))

(ert-deftest elinit-test-plan-cycle-detection ()
  "Plan correctly detects cycles and falls back."
  (let* ((programs '(("sleep 100" :id "a" :after "b")
                     ("sleep 200" :id "b" :after "a")))
         (plan (elinit--build-plan programs)))
    ;; Both entries should be marked as cycle fallback
    (should (gethash "a" (elinit-plan-cycle-fallback-ids plan)))
    (should (gethash "b" (elinit-plan-cycle-fallback-ids plan)))
    ;; Deps should be cleared after fallback
    (should (null (gethash "a" (elinit-plan-deps plan))))
    (should (null (gethash "b" (elinit-plan-deps plan))))))

(ert-deftest elinit-test-plan-dependency-validation ()
  "Plan validates :after references correctly."
  (let* ((programs '(("sleep 100" :id "a")
                     ("sleep 200" :id "b" :after "a")
                     ("sleep 300" :id "c" :after "nonexistent")
                     ("sleep 400" :id "d" :after "a")))
         (plan (elinit--build-plan programs)))
    ;; b's dep on a should be preserved
    (should (equal '("a") (gethash "b" (elinit-plan-deps plan))))
    ;; c's dep on nonexistent should be removed
    (should (null (gethash "c" (elinit-plan-deps plan))))
    ;; d's dep on a should be preserved
    (should (equal '("a") (gethash "d" (elinit-plan-deps plan))))))

(ert-deftest elinit-test-plan-duplicate-id-first-occurrence-order ()
  "Duplicate IDs use first-occurrence index for ordering.
Regression test: duplicates must not overwrite order-index of kept entry."
  (let* ((programs '(("sleep 100" :id "a")
                     ("sleep 200" :id "b")
                     ("sleep 300" :id "a")))  ; duplicate of a
         (plan (elinit--build-plan programs)))
    ;; Only 2 entries should be in plan (a and b, duplicate skipped)
    (should (= 2 (length (elinit-plan-entries plan))))
    ;; Order index should reflect first occurrence: a=0, b=1
    (should (= 0 (gethash "a" (elinit-plan-order-index plan))))
    (should (= 1 (gethash "b" (elinit-plan-order-index plan))))
    ;; Sorted order should be a, b (not b, a)
    (let* ((sorted (elinit-plan-by-target plan))
           (ids (mapcar #'car sorted)))
      (should (equal '("a" "b") ids)))))

;;; Phase 2: Startup Uses Plan Tests

(ert-deftest elinit-test-start-sets-current-plan-and-closure ()
  "Elinit-start stores active plan metadata for status surfaces."
  (elinit-test-with-unit-files
      '((nil :id "graphical.target" :type target)
        (nil :id "rescue.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--restart-override (make-hash-table :test 'equal))
          (elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--logging (make-hash-table :test 'equal))
          (elinit--writers (make-hash-table :test 'equal))
          (elinit--stderr-writers (make-hash-table :test 'equal))
          (elinit--stderr-pipes (make-hash-table :test 'equal))
          (elinit--restart-times (make-hash-table :test 'equal))
          (elinit--restart-timers (make-hash-table :test 'equal))
          (elinit--last-exit-info (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--remain-active (make-hash-table :test 'equal))
          (elinit--start-times (make-hash-table :test 'equal))
          (elinit--ready-times (make-hash-table :test 'equal))
          (elinit--cycle-fallback-ids (make-hash-table :test 'equal))
          (elinit--computed-deps (make-hash-table :test 'equal))
          (elinit--spawn-failure-reason (make-hash-table :test 'equal))
          (elinit--manually-stopped (make-hash-table :test 'equal))
          (elinit--manually-started (make-hash-table :test 'equal))
          (elinit--current-plan nil)
          (elinit-default-target "default.target")
          (elinit-default-target-link "graphical.target")
          (elinit--default-target-link-override nil)
          (elinit--shutting-down t)
          (started-ids nil))
      (cl-letf (((symbol-function 'elinit--stop-all-processes)
                 (lambda (callback) (funcall callback)))
                ((symbol-function 'elinit--load-overrides) #'ignore)
                ((symbol-function 'elinit--start-entries-async)
                 (lambda (entries callback &optional _target-members)
                   (setq started-ids (mapcar #'elinit-entry-id entries))
                   (funcall callback))))
        (elinit-start))
      (should (elinit-plan-p elinit--current-plan))
      (let ((closure (elinit-plan-activation-closure elinit--current-plan)))
        (should (hash-table-p closure))
        (should (gethash "graphical.target" closure))
        (should-not (gethash "rescue.target" closure)))
      (should (member "graphical.target" started-ids))
      (should-not (member "rescue.target" started-ids)))))

(ert-deftest elinit-test-startup-populates-globals-from-plan ()
  "Plan->global copy mechanism populates legacy globals correctly.
Unit test for the plan data extraction used by elinit-start.
Note: Does not call elinit-start directly to avoid process spawning."
  (elinit-test-with-unit-files
      '(("sleep 100" :id "a")
        ("sleep 200" :id "b" :after "a")
        ("invalid-cmd" :id "invalid" :restart t :no-restart t))
    (let ((elinit--invalid (make-hash-table :test 'equal))
          (elinit--cycle-fallback-ids (make-hash-table :test 'equal))
          (elinit--computed-deps (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--restart-override (make-hash-table :test 'equal))
          (elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--logging (make-hash-table :test 'equal))
          (elinit--restart-times (make-hash-table :test 'equal))
          (elinit--restart-timers (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--start-times (make-hash-table :test 'equal))
          (elinit--ready-times (make-hash-table :test 'equal))
          (elinit--timers nil)
          (elinit--shutting-down t))  ; Prevent actual process starts
      ;; Build plan and populate globals (same as elinit-start)
      (let* ((progs (elinit--effective-programs))
             (plan (elinit--build-plan progs)))
        (elinit--merge-unit-file-invalid)
        (maphash (lambda (k v) (puthash k v elinit--invalid))
                 (elinit-plan-invalid plan))
        (maphash (lambda (k v) (puthash k v elinit--cycle-fallback-ids))
                 (elinit-plan-cycle-fallback-ids plan))
        (maphash (lambda (k v) (puthash k v elinit--computed-deps))
                 (elinit-plan-deps plan)))
      ;; Verify globals were populated from plan
      (should (gethash "invalid" elinit--invalid))  ; Invalid entry recorded
      (should (equal '("a") (gethash "b" elinit--computed-deps)))  ; Deps computed
      (should (= 1 (hash-table-count elinit--invalid))))))

(ert-deftest elinit-test-plan-build-warns-on-duplicates ()
  "Plan building emits warnings for duplicate IDs.
Regression test for warning parity with legacy startup path."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit--programs-cache :not-yet-loaded)
         (elinit--unit-file-invalid (make-hash-table :test 'equal))
         (messages nil))
    (with-temp-file (expand-file-name "dup1.el" dir)
      (insert "(:id \"dup\" :command \"sleep 100\")"))
    (with-temp-file (expand-file-name "dup2.el" dir)
      (insert "(:id \"dup\" :command \"sleep 200\")"))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'elinit--log)
                     (lambda (_level fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (elinit--build-plan (elinit--effective-programs)))
          ;; Should have warned about duplicate
          (should (cl-some (lambda (m) (string-match-p "Duplicate.*ID 'dup'" m))
                           messages)))
      (delete-directory dir t))))

(ert-deftest elinit-test-plan-build-warns-on-invalid-after ()
  "Plan building emits warnings for invalid :after references.
Regression test for warning parity with legacy startup path."
  (elinit-test-with-unit-files
      '(("sleep 100" :id "a")
        ("sleep 200" :id "b" :after "nonexistent"))
    (let ((messages nil))
      (cl-letf (((symbol-function 'elinit--log)
                 (lambda (_level fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (elinit--build-plan (elinit--effective-programs)))
      ;; Should have warned about nonexistent dep
      (should (cl-some (lambda (m) (string-match-p "does not exist" m))
                       messages)))))

;;; Phase 3: Snapshot-Based Read Model Tests

(ert-deftest elinit-test-snapshot-shape ()
  "Snapshot struct has all required fields."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--entry-state (make-hash-table :test 'equal))
        (elinit--invalid (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--restart-override (make-hash-table :test 'equal))
        (elinit--logging (make-hash-table :test 'equal)))
    (let ((snapshot (elinit--build-snapshot)))
      (should (elinit-snapshot-p snapshot))
      (should (hash-table-p (elinit-snapshot-process-alive snapshot)))
      (should (hash-table-p (elinit-snapshot-process-pids snapshot)))
      (should (hash-table-p (elinit-snapshot-failed snapshot)))
      (should (hash-table-p (elinit-snapshot-oneshot-exit snapshot)))
      (should (hash-table-p (elinit-snapshot-entry-state snapshot)))
      (should (hash-table-p (elinit-snapshot-invalid snapshot)))
      (should (hash-table-p (elinit-snapshot-enabled-override snapshot)))
      (should (hash-table-p (elinit-snapshot-restart-override snapshot)))
      (should (hash-table-p (elinit-snapshot-logging-override snapshot)))
      (should (numberp (elinit-snapshot-timestamp snapshot))))))

(ert-deftest elinit-test-snapshot-captures-state ()
  "Snapshot captures current runtime state correctly."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--entry-state (make-hash-table :test 'equal))
        (elinit--invalid (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--restart-override (make-hash-table :test 'equal))
        (elinit--logging (make-hash-table :test 'equal)))
    ;; Set up some state
    (puthash "test-failed" t elinit--failed)
    (puthash "test-oneshot" 0 elinit--oneshot-completed)
    (puthash "test-entry" 'running elinit--entry-state)
    (puthash "test-invalid" "bad config" elinit--invalid)
    (puthash "test-enabled" 'disabled elinit--enabled-override)
    ;; Build snapshot
    (let ((snapshot (elinit--build-snapshot)))
      ;; Verify state was captured
      (should (gethash "test-failed" (elinit-snapshot-failed snapshot)))
      (should (= 0 (gethash "test-oneshot" (elinit-snapshot-oneshot-exit snapshot))))
      (should (eq 'running (gethash "test-entry" (elinit-snapshot-entry-state snapshot))))
      (should (equal "bad config" (gethash "test-invalid" (elinit-snapshot-invalid snapshot))))
      (should (eq 'disabled (gethash "test-enabled" (elinit-snapshot-enabled-override snapshot)))))))

(ert-deftest elinit-test-status-from-snapshot-parity ()
  "Status computation from snapshot matches direct global access."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--entry-state (make-hash-table :test 'equal))
        (elinit--invalid (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--restart-override (make-hash-table :test 'equal))
        (elinit--logging (make-hash-table :test 'equal)))
    ;; Set up state for a failed entry
    (puthash "test" t elinit--failed)
    (puthash "oneshot-done" 0 elinit--oneshot-completed)
    ;; Build snapshot
    (let ((snapshot (elinit--build-snapshot)))
      ;; Status from globals
      (let ((status-globals (elinit--compute-entry-status "test" 'simple)))
        ;; Status from snapshot
        (let ((status-snapshot (elinit--compute-entry-status "test" 'simple snapshot)))
          (should (equal status-globals status-snapshot))))
      ;; Test oneshot status
      (let ((status-globals (elinit--compute-entry-status "oneshot-done" 'oneshot)))
        (let ((status-snapshot (elinit--compute-entry-status "oneshot-done" 'oneshot snapshot)))
          (should (equal status-globals status-snapshot)))))))

(ert-deftest elinit-test-health-summary-from-snapshot-parity ()
  "Health summary from snapshot matches direct global access."
  (elinit-test-with-unit-files
      '(("sleep 100" :id "a")
        ("sleep 200" :id "b"))
    (let ((elinit--processes (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--restart-override (make-hash-table :test 'equal))
          (elinit--logging (make-hash-table :test 'equal)))
      ;; Mark one as failed
      (puthash "a" t elinit--failed)
      ;; Build snapshot
      (let ((snapshot (elinit--build-snapshot)))
        ;; Health from globals
        (let ((health-globals (elinit--health-summary)))
          ;; Health from snapshot
          (let ((health-snapshot (elinit--health-summary snapshot)))
            (should (equal health-globals health-snapshot))))))))

(ert-deftest elinit-test-reason-from-snapshot-parity ()
  "Reason computation from snapshot matches direct global access."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--entry-state (make-hash-table :test 'equal))
        (elinit--invalid (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--restart-override (make-hash-table :test 'equal))
        (elinit--logging (make-hash-table :test 'equal)))
    ;; Set up various states
    (puthash "disabled" 'disabled elinit--entry-state)
    (puthash "delayed" 'delayed elinit--entry-state)
    (puthash "waiting" 'waiting-on-deps elinit--entry-state)
    (puthash "crashed" t elinit--failed)
    ;; Build snapshot
    (let ((snapshot (elinit--build-snapshot)))
      ;; Test each case
      (dolist (test-case '(("disabled" . simple)
                           ("delayed" . simple)
                           ("waiting" . simple)
                           ("crashed" . simple)))
        (let ((id (car test-case))
              (type (cdr test-case)))
          (should (equal (elinit--compute-entry-reason id type)
                         (elinit--compute-entry-reason id type snapshot))))))))

(ert-deftest elinit-test-initial-dashboard-uses-shared-snapshot ()
  "Initial dashboard render uses same snapshot for entries and header.
Regression test: M-x elinit must use shared snapshot like refresh does."
  (elinit-test-with-unit-files
      '(("sleep 100" :id "a"))
    (let ((elinit--processes (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--restart-override (make-hash-table :test 'equal))
          (elinit--logging (make-hash-table :test 'equal))
          (snapshots-built 0))
      ;; Track how many snapshots are built during initial render
      (cl-letf (((symbol-function 'elinit--build-snapshot)
                 (lambda ()
                   (cl-incf snapshots-built)
                   (elinit-snapshot--create
                    :process-alive (make-hash-table :test 'equal)
                    :process-pids (make-hash-table :test 'equal)
                    :failed (copy-hash-table elinit--failed)
                    :oneshot-exit (copy-hash-table elinit--oneshot-completed)
                    :entry-state (copy-hash-table elinit--entry-state)
                    :invalid (copy-hash-table elinit--invalid)
                    :enabled-override (copy-hash-table elinit--enabled-override)
                    :restart-override (copy-hash-table elinit--restart-override)
                    :logging-override (copy-hash-table elinit--logging)
                    :timestamp (float-time)))))
        (unwind-protect
            (progn
              (elinit)
              ;; Should build exactly one snapshot for initial render
              (should (= 1 snapshots-built)))
          ;; Cleanup
          (when-let* ((buf (get-buffer "*elinit*")))
            (kill-buffer buf)))))))

(ert-deftest elinit-test-dashboard-header-and-services-row ()
  "Dashboard shows counters in header and services columns in body row 1."
  (elinit-test-with-unit-files
      '(("true" :id "svc" :type oneshot))
    (let ((elinit--processes (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--restart-override (make-hash-table :test 'equal))
          (elinit--logging (make-hash-table :test 'equal)))
      (unwind-protect
          (progn
            (elinit)
            (with-current-buffer "*elinit*"
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
        (when-let* ((buf (get-buffer "*elinit*")))
          (kill-buffer buf))))))

;;; Phase 4: Declarative Reconciler Tests

(ert-deftest elinit-test-compute-actions-start ()
  "Compute-actions returns start action for entries not running."
  (elinit-test-with-unit-files
      '(("sleep 100" :id "a"))
    (let* ((plan (elinit--build-plan (elinit--effective-programs)))
           ;; Empty snapshot - nothing running
           (snapshot (elinit-snapshot--create
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
           (actions (elinit--compute-actions plan snapshot)))
      (should (= 1 (length actions)))
      (should (eq 'start (plist-get (car actions) :op)))
      (should (equal "a" (plist-get (car actions) :id)))
      (should (eq 'new-entry (plist-get (car actions) :reason))))))

(ert-deftest elinit-test-compute-actions-stop-removed ()
  "Compute-actions returns stop action for running entries not in plan."
  (elinit-test-with-unit-files nil
    (let* ((plan (elinit--build-plan (elinit--effective-programs)))
           ;; Snapshot shows a running process
           (process-alive (make-hash-table :test 'equal)))
      (puthash "orphan" t process-alive)
      (let* ((snapshot (elinit-snapshot--create
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
             (actions (elinit--compute-actions plan snapshot)))
        (should (= 1 (length actions)))
        (should (eq 'stop (plist-get (car actions) :op)))
        (should (equal "orphan" (plist-get (car actions) :id)))
        (should (eq 'removed (plist-get (car actions) :reason)))))))

(ert-deftest elinit-test-compute-actions-stop-disabled ()
  "Compute-actions returns stop action for running entries now disabled."
  (elinit-test-with-unit-files
      '(("sleep 100" :id "a" :disabled t))
    (let* ((plan (elinit--build-plan (elinit--effective-programs)))
           ;; Snapshot shows process is running
           (process-alive (make-hash-table :test 'equal)))
      (puthash "a" t process-alive)
      (let* ((snapshot (elinit-snapshot--create
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
             (actions (elinit--compute-actions plan snapshot)))
        ;; Should have stop action for disabled entry
        (let ((stop-action (cl-find 'stop actions :key (lambda (a) (plist-get a :op)))))
          (should stop-action)
          (should (equal "a" (plist-get stop-action :id)))
          (should (eq 'disabled (plist-get stop-action :reason))))))))

(ert-deftest elinit-test-compute-actions-noop-already-running ()
  "Compute-actions returns noop for entries already running."
  (elinit-test-with-unit-files
      '(("sleep 100" :id "a"))
    (let* ((plan (elinit--build-plan (elinit--effective-programs)))
           ;; Snapshot shows process is running
           (process-alive (make-hash-table :test 'equal)))
      (puthash "a" t process-alive)
      (let* ((snapshot (elinit-snapshot--create
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
             (actions (elinit--compute-actions plan snapshot)))
        (should (= 1 (length actions)))
        (should (eq 'noop (plist-get (car actions) :op)))
        (should (equal "a" (plist-get (car actions) :id)))
        (should (eq 'already-running (plist-get (car actions) :reason)))))))

(ert-deftest elinit-test-compute-actions-skip-failed ()
  "Compute-actions returns skip for failed entries."
  (elinit-test-with-unit-files
      '(("sleep 100" :id "a"))
    (let* ((plan (elinit--build-plan (elinit--effective-programs)))
           ;; Snapshot shows entry is failed
           (failed (make-hash-table :test 'equal)))
      (puthash "a" t failed)
      (let* ((snapshot (elinit-snapshot--create
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
             (actions (elinit--compute-actions plan snapshot)))
        (should (= 1 (length actions)))
        (should (eq 'skip (plist-get (car actions) :op)))
        (should (equal "a" (plist-get (car actions) :id)))
        (should (eq 'failed (plist-get (car actions) :reason)))))))

(ert-deftest elinit-test-compute-actions-skip-oneshot-completed ()
  "Compute-actions returns skip for completed oneshots."
  (elinit-test-with-unit-files
      '(("echo done" :id "a" :type oneshot))
    (let* ((plan (elinit--build-plan (elinit--effective-programs)))
           ;; Snapshot shows oneshot completed
           (oneshot-exit (make-hash-table :test 'equal)))
      (puthash "a" 0 oneshot-exit)
      (let* ((snapshot (elinit-snapshot--create
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
             (actions (elinit--compute-actions plan snapshot)))
        (should (= 1 (length actions)))
        (should (eq 'skip (plist-get (car actions) :op)))
        (should (equal "a" (plist-get (car actions) :id)))
        (should (eq 'oneshot-completed (plist-get (car actions) :reason)))))))

(ert-deftest elinit-test-compute-actions-skip-disabled ()
  "Compute-actions returns skip for disabled entries not running."
  (elinit-test-with-unit-files
      '(("sleep 100" :id "a" :disabled t))
    (let* ((plan (elinit--build-plan (elinit--effective-programs)))
           ;; Empty snapshot - nothing running
           (snapshot (elinit-snapshot--create
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
           (actions (elinit--compute-actions plan snapshot)))
      (should (= 1 (length actions)))
      (should (eq 'skip (plist-get (car actions) :op)))
      (should (equal "a" (plist-get (car actions) :id)))
      (should (eq 'disabled (plist-get (car actions) :reason))))))

(ert-deftest elinit-test-compute-actions-is-pure ()
  "Compute-actions does not modify any global state."
  (elinit-test-with-unit-files
      '(("sleep 100" :id "a")
        ("sleep 200" :id "b" :disabled t))
    (let* ((plan (elinit--build-plan (elinit--effective-programs)))
           ;; Set up some mutable globals
           (elinit--processes (make-hash-table :test 'equal))
           (elinit--failed (make-hash-table :test 'equal))
           (elinit--oneshot-completed (make-hash-table :test 'equal))
           (elinit--restart-override (make-hash-table :test 'equal)))
      (puthash "x" t elinit--failed)
      (let* ((failed-before (copy-hash-table elinit--failed))
             (process-alive (make-hash-table :test 'equal))
             (snapshot (elinit-snapshot--create
                        :process-alive process-alive
                        :process-pids (make-hash-table :test 'equal)
                        :failed (copy-hash-table elinit--failed)
                        :oneshot-exit (make-hash-table :test 'equal)
                        :entry-state (make-hash-table :test 'equal)
                        :invalid (make-hash-table :test 'equal)
                        :enabled-override (make-hash-table :test 'equal)
                        :restart-override (make-hash-table :test 'equal)
                        :logging-override (make-hash-table :test 'equal)
                        :timestamp (float-time))))
        ;; Call compute-actions multiple times
        (elinit--compute-actions plan snapshot)
        (elinit--compute-actions plan snapshot)
        ;; Globals should be unchanged
        (should (equal (hash-table-count elinit--failed)
                       (hash-table-count failed-before)))
        (should (gethash "x" elinit--failed))
        (should (= 0 (hash-table-count elinit--processes)))
        (should (= 0 (hash-table-count elinit--restart-override)))))))

(ert-deftest elinit-test-compute-actions-idempotent ()
  "Idempotence: second call on converged state produces only noops/skips.
When actual state matches desired state, no start/stop actions are needed."
  (elinit-test-with-unit-files
      '(("sleep 100" :id "a")
        ("sleep 200" :id "b")
        ("echo done" :id "c" :type oneshot)
        ("sleep 300" :id "d" :disabled t))
    (let* ((plan (elinit--build-plan (elinit--effective-programs)))
           ;; Simulate converged state: a and b running, c completed, d disabled
           (process-alive (make-hash-table :test 'equal))
           (oneshot-exit (make-hash-table :test 'equal)))
      (puthash "a" t process-alive)
      (puthash "b" t process-alive)
      (puthash "c" 0 oneshot-exit)
      (let* ((snapshot (elinit-snapshot--create
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
             (actions (elinit--compute-actions plan snapshot)))
        ;; Should have 4 actions, none of which are start or stop
        (should (= 4 (length actions)))
        (should-not (cl-find 'start actions :key (lambda (a) (plist-get a :op))))
        (should-not (cl-find 'stop actions :key (lambda (a) (plist-get a :op))))
        ;; All should be noop or skip
        (dolist (action actions)
          (should (memq (plist-get action :op) '(noop skip))))))))

(ert-deftest elinit-test-compute-actions-action-matrix ()
  "Comprehensive test of action matrix with mixed scenarios.
Covers add, remove, disable (running), already-running, failed."
  (elinit-test-with-unit-files
      '(("sleep 100" :id "a")
        ("sleep 200" :id "b")
        ("sleep 300" :id "c" :disabled t)
        ("sleep 400" :id "d")
        ("sleep 500" :id "e" :disabled t))
    ;; Plan: a (new), b (already running), c (running but now disabled),
    ;;       d (failed), e (disabled from start)
    (let* ((plan (elinit--build-plan (elinit--effective-programs)))
           ;; Runtime: b and c running, orphan running (removed), d failed
           (process-alive (make-hash-table :test 'equal))
           (failed (make-hash-table :test 'equal)))
      (puthash "b" t process-alive)
      (puthash "c" t process-alive)
      (puthash "orphan" t process-alive)
      (puthash "d" t failed)
      (let* ((snapshot (elinit-snapshot--create
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
             (actions (elinit--compute-actions plan snapshot))
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

(ert-deftest elinit-test-apply-actions-atomic-on-invalid-plan ()
  "Apply-actions does not mutate state when plan has no matching entries.
This tests the atomic boundary: invalid/empty plans cause no side effects."
  (let* ((elinit--processes (make-hash-table :test 'equal))
         (elinit--restart-override (make-hash-table :test 'equal))
         (elinit--failed (make-hash-table :test 'equal))
         ;; Empty plan (no entries)
         (plan (elinit--build-plan nil))
         ;; Actions that reference non-existent entries
         (actions (list (list :op 'start :id "nonexistent" :reason 'added)
                        (list :op 'stop :id "also-nonexistent" :reason 'removed))))
    ;; Apply should not crash and should not modify state
    (let ((result (elinit--apply-actions actions plan)))
      ;; No processes were started or stopped
      (should (= 0 (plist-get result :started)))
      (should (= 0 (plist-get result :stopped)))
      ;; Globals remain empty
      (should (= 0 (hash-table-count elinit--processes)))
      (should (= 0 (hash-table-count elinit--restart-override))))))

;;; Phase 5: Explicit FSM Adoption Tests

(ert-deftest elinit-test-fsm-valid-states-defined ()
  "All valid states are defined in the state list."
  (should (memq 'pending elinit--valid-states))
  (should (memq 'waiting-on-deps elinit--valid-states))
  (should (memq 'delayed elinit--valid-states))
  (should (memq 'disabled elinit--valid-states))
  (should (memq 'started elinit--valid-states))
  (should (memq 'failed-to-spawn elinit--valid-states))
  (should (memq 'startup-timeout elinit--valid-states)))

(ert-deftest elinit-test-fsm-invalid-state-rejected ()
  "Transitioning to an invalid state signals an error."
  (let ((elinit--entry-state (make-hash-table :test 'equal)))
    (should-error (elinit--transition-state "test" 'invalid-state)
                  :type 'error)))

(ert-deftest elinit-test-fsm-invalid-transition-rejected ()
  "Invalid state transitions signal an error."
  (let ((elinit--entry-state (make-hash-table :test 'equal)))
    ;; Set up valid path to terminal state
    (elinit--transition-state "test" 'pending)
    (elinit--transition-state "test" 'started)
    ;; Transition from terminal state should error
    (should-error (elinit--transition-state "test" 'delayed)
                  :type 'error)))

(ert-deftest elinit-test-fsm-valid-transitions ()
  "Valid state transitions succeed."
  (let ((elinit--entry-state (make-hash-table :test 'equal)))
    ;; nil -> pending
    (should (elinit--transition-state "a" 'pending))
    (should (eq 'pending (gethash "a" elinit--entry-state)))
    ;; pending -> waiting-on-deps
    (should (elinit--transition-state "a" 'waiting-on-deps))
    (should (eq 'waiting-on-deps (gethash "a" elinit--entry-state)))
    ;; waiting-on-deps -> delayed
    (should (elinit--transition-state "a" 'delayed))
    (should (eq 'delayed (gethash "a" elinit--entry-state)))
    ;; delayed -> started
    (should (elinit--transition-state "a" 'started))
    (should (eq 'started (gethash "a" elinit--entry-state)))))

(ert-deftest elinit-test-fsm-self-transitions-allowed ()
  "Self-transitions are allowed for idempotent operations."
  (let ((elinit--entry-state (make-hash-table :test 'equal)))
    (elinit--transition-state "a" 'pending)
    ;; Self-transition should succeed
    (should (elinit--transition-state "a" 'pending))
    (should (eq 'pending (gethash "a" elinit--entry-state)))))

(ert-deftest elinit-test-fsm-force-invalid-transition ()
  "Force parameter allows invalid transitions with warning.
Returns nil (not t) and emits a warning for forced invalid transitions."
  (let ((elinit--entry-state (make-hash-table :test 'equal))
        (warnings nil))
    ;; Set up valid path to terminal state
    (elinit--transition-state "a" 'pending)
    (elinit--transition-state "a" 'started)
    (cl-letf (((symbol-function 'elinit--log)
               (lambda (_level fmt &rest args)
                 (push (apply #'format fmt args) warnings))))
      ;; Force invalid transition from terminal state - should return nil
      (let ((result (elinit--transition-state "a" 'delayed t)))
        ;; Return value should be nil for forced invalid transition
        (should (null result))
        ;; State should change despite being invalid
        (should (eq 'delayed (gethash "a" elinit--entry-state)))
        ;; Warning should have been emitted
        (should (= 1 (length warnings)))
        (should (string-match-p "Forced invalid transition" (car warnings)))))))

(ert-deftest elinit-test-fsm-lifecycle-simple-success ()
  "Test lifecycle path for successful simple process start."
  (let ((elinit--entry-state (make-hash-table :test 'equal)))
    ;; Initial state
    (elinit--transition-state "proc" 'pending)
    ;; After delay
    (elinit--transition-state "proc" 'delayed)
    ;; After successful spawn
    (elinit--transition-state "proc" 'started)
    (should (eq 'started (gethash "proc" elinit--entry-state)))))

(ert-deftest elinit-test-fsm-lifecycle-spawn-failure ()
  "Test lifecycle path for spawn failure."
  (let ((elinit--entry-state (make-hash-table :test 'equal)))
    ;; Initial state
    (elinit--transition-state "proc" 'pending)
    ;; Spawn fails
    (elinit--transition-state "proc" 'failed-to-spawn)
    (should (eq 'failed-to-spawn (gethash "proc" elinit--entry-state)))))

(ert-deftest elinit-test-fsm-lifecycle-startup-timeout ()
  "Test lifecycle path for startup timeout."
  (let ((elinit--entry-state (make-hash-table :test 'equal)))
    ;; Initial state with deps
    (elinit--transition-state "proc" 'waiting-on-deps)
    ;; Startup times out before deps complete
    (elinit--transition-state "proc" 'startup-timeout)
    (should (eq 'startup-timeout (gethash "proc" elinit--entry-state)))))

(ert-deftest elinit-test-fsm-lifecycle-disabled ()
  "Test lifecycle path for disabled entry."
  (let ((elinit--entry-state (make-hash-table :test 'equal)))
    ;; Entry is disabled from the start
    (elinit--transition-state "proc" 'disabled)
    (should (eq 'disabled (gethash "proc" elinit--entry-state)))))

(ert-deftest elinit-test-fsm-lifecycle-oneshot ()
  "Test lifecycle path for oneshot process."
  (let ((elinit--entry-state (make-hash-table :test 'equal)))
    ;; Initial state
    (elinit--transition-state "oneshot" 'pending)
    ;; Started (oneshot still uses 'started state)
    (elinit--transition-state "oneshot" 'started)
    (should (eq 'started (gethash "oneshot" elinit--entry-state)))))

;;; Phase 6: Structured Event Dispatcher Tests

(ert-deftest elinit-test-event-types-defined ()
  "All event types are defined in the event type list."
  (should (memq 'startup-begin elinit--event-types))
  (should (memq 'startup-complete elinit--event-types))
  (should (memq 'process-started elinit--event-types))
  (should (memq 'process-ready elinit--event-types))
  (should (memq 'process-exit elinit--event-types))
  (should (memq 'process-failed elinit--event-types))
  (should (memq 'cleanup elinit--event-types))
  ;; Timer event types
  (should (memq 'timer-trigger elinit--event-types))
  (should (memq 'timer-overlap elinit--event-types))
  (should (memq 'timer-success elinit--event-types))
  (should (memq 'timer-failure elinit--event-types)))

(ert-deftest elinit-test-emit-event-invalid-type-rejected ()
  "Emitting an invalid event type signals an error."
  (should-error (elinit--emit-event 'invalid-event-type nil nil)
                :type 'error))

(ert-deftest elinit-test-emit-event-schema ()
  "Emitted events have the correct schema."
  (let ((events nil))
    (cl-letf (((symbol-function 'run-hook-with-args)
               (lambda (hook &rest args) (when (eq hook 'elinit-event-hook)
                                           (push (car args) events))))
              ((symbol-function 'run-hooks) #'ignore))
      (elinit--emit-event 'startup-begin)
      (let ((event (car events)))
        (should (eq 'startup-begin (plist-get event :type)))
        (should (numberp (plist-get event :ts)))
        (should (null (plist-get event :id)))))))

(ert-deftest elinit-test-emit-event-startup-begin ()
  "Startup-begin event is emitted with correct data."
  (let ((events nil))
    (cl-letf (((symbol-function 'run-hook-with-args)
               (lambda (hook &rest args)
                 (when (eq hook 'elinit-event-hook)
                   (push (car args) events))))
              ((symbol-function 'run-hooks) #'ignore))
      (elinit--emit-event 'startup-begin)
      (should (= 1 (length events)))
      (let ((event (car events)))
        (should (eq 'startup-begin (plist-get event :type)))))))

(ert-deftest elinit-test-emit-event-process-exit ()
  "Process-exit event carries status and code in data."
  (let ((events nil))
    (cl-letf (((symbol-function 'run-hook-with-args)
               (lambda (hook &rest args)
                 (when (eq hook 'elinit-event-hook)
                   (push (car args) events))))
              ((symbol-function 'run-hooks) #'ignore))
      (elinit--emit-event 'process-exit "myproc"
                              (list :status 'exited :code 0))
      (should (= 1 (length events)))
      (let ((event (car events)))
        (should (eq 'process-exit (plist-get event :type)))
        (should (equal "myproc" (plist-get event :id)))
        (should (eq 'exited (plist-get (plist-get event :data) :status)))
        (should (= 0 (plist-get (plist-get event :data) :code)))))))

(ert-deftest elinit-test-emit-event-process-started ()
  "Process-started event includes type in data."
  (let ((events nil))
    (cl-letf (((symbol-function 'run-hook-with-args)
               (lambda (hook &rest args)
                 (when (eq hook 'elinit-event-hook)
                   (push (car args) events))))
              ((symbol-function 'run-hooks) #'ignore))
      (elinit--emit-event 'process-started "daemon"
                              (list :type 'simple))
      (should (= 1 (length events)))
      (let ((event (car events)))
        (should (eq 'process-started (plist-get event :type)))
        (should (equal "daemon" (plist-get event :id)))
        (should (eq 'simple (plist-get (plist-get event :data) :type)))))))

(ert-deftest elinit-test-spawn-failure-no-process-ready ()
  "Spawn failure emits process-failed but NOT process-ready.
Regression test: process-ready was incorrectly emitted for failures."
  (let ((events nil)
        (elinit--entry-state (make-hash-table :test 'equal))
        (elinit--dag-ready (make-hash-table :test 'equal))
        (elinit--ready-times (make-hash-table :test 'equal))
        (elinit--dag-dependents (make-hash-table :test 'equal))
        (elinit--dag-in-degree (make-hash-table :test 'equal))
        (elinit--dag-blocking (make-hash-table :test 'equal))
        (elinit--dag-timeout-timers (make-hash-table :test 'equal))
        (elinit--dag-started (make-hash-table :test 'equal))
        (elinit--dag-active-starts 1)
        (elinit--dag-pending-starts nil)
        (elinit--dag-complete-callback nil)
        (elinit--dag-entries (make-hash-table :test 'equal)))
    ;; Set up initial state
    (elinit--transition-state "test" 'pending)
    (puthash "test" '("test" "cmd" 0 t always t simple nil t 30 nil)
             elinit--dag-entries)
    ;; Capture events
    (cl-letf (((symbol-function 'run-hook-with-args)
               (lambda (hook &rest args)
                 (when (eq hook 'elinit-event-hook)
                   (push (car args) events))))
              ((symbol-function 'run-hooks) #'ignore))
      (elinit--dag-handle-spawn-failure "test"))
    ;; Should have process-failed but NOT process-ready
    (should (cl-find 'process-failed events :key (lambda (e) (plist-get e :type))))
    (should-not (cl-find 'process-ready events :key (lambda (e) (plist-get e :type))))))

(ert-deftest elinit-test-disabled-entry-no-process-ready ()
  "Disabled entries do NOT emit process-ready.
Regression test: process-ready was incorrectly emitted for disabled entries."
  (let ((events nil)
        (elinit--entry-state (make-hash-table :test 'equal))
        (elinit--dag-ready (make-hash-table :test 'equal))
        (elinit--ready-times (make-hash-table :test 'equal))
        (elinit--dag-dependents (make-hash-table :test 'equal))
        (elinit--dag-in-degree (make-hash-table :test 'equal))
        (elinit--dag-blocking (make-hash-table :test 'equal))
        (elinit--dag-timeout-timers (make-hash-table :test 'equal))
        (elinit--dag-started (make-hash-table :test 'equal))
        (elinit--dag-complete-callback nil)
        (elinit--dag-entries (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal)))
    ;; Set up as disabled entry
    (puthash "test" '("test" "cmd" 0 nil always t simple nil t 30 nil)
             elinit--dag-entries)
    ;; Capture events
    (cl-letf (((symbol-function 'run-hook-with-args)
               (lambda (hook &rest args)
                 (when (eq hook 'elinit-event-hook)
                   (push (car args) events))))
              ((symbol-function 'run-hooks) #'ignore))
      (elinit--dag-start-entry-async
       '("test" "cmd" 0 nil always t simple nil t 30 nil)))
    ;; Should NOT have process-ready
    (should-not (cl-find 'process-ready events :key (lambda (e) (plist-get e :type))))))

(ert-deftest elinit-test-simple-spawn-emits-process-ready ()
  "Successful simple process spawn emits process-started AND process-ready."
  (let ((events nil)
        (elinit--entry-state (make-hash-table :test 'equal))
        (elinit--dag-ready (make-hash-table :test 'equal))
        (elinit--ready-times (make-hash-table :test 'equal))
        (elinit--dag-dependents (make-hash-table :test 'equal))
        (elinit--dag-in-degree (make-hash-table :test 'equal))
        (elinit--dag-blocking (make-hash-table :test 'equal))
        (elinit--dag-timeout-timers (make-hash-table :test 'equal))
        (elinit--dag-started (make-hash-table :test 'equal))
        (elinit--dag-active-starts 1)
        (elinit--dag-pending-starts nil)
        (elinit--dag-complete-callback nil)
        (elinit--dag-entries (make-hash-table :test 'equal)))
    ;; Set up initial state
    (elinit--transition-state "test" 'pending)
    (puthash "test" '("test" "sleep" 0 t always t simple nil t 30 nil)
             elinit--dag-entries)
    ;; Capture events
    (cl-letf (((symbol-function 'run-hook-with-args)
               (lambda (hook &rest args)
                 (when (eq hook 'elinit-event-hook)
                   (push (car args) events))))
              ((symbol-function 'run-hooks) #'ignore))
      (elinit--dag-handle-spawn-success "test" 'simple nil))
    ;; Should have both process-started and process-ready
    (should (cl-find 'process-started events :key (lambda (e) (plist-get e :type))))
    (should (cl-find 'process-ready events :key (lambda (e) (plist-get e :type))))))

(ert-deftest elinit-test-no-stderr-process-created ()
  "Starting a process does not create a separate stderr process.
Regression test: stderr pipe processes used to pollute the process list."
  (elinit-test-with-unit-files
      '(("true" :id "test-no-stderr" :type oneshot))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--failed (make-hash-table :test 'equal))
           (elinit--enabled-override (make-hash-table :test 'equal))
           (elinit--logging (make-hash-table :test 'equal))
           (elinit--oneshot-completed (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (elinit--restart-timestamps (make-hash-table :test 'equal))
           (elinit-log-directory (make-temp-file "elinit-test-" t)))
      (unwind-protect
          (progn
            (elinit--start-process "test-no-stderr" "true" t 'oneshot nil nil)
            ;; Main process should exist
            (should (gethash "test-no-stderr" elinit--processes))
            ;; No stderr process should exist (with or without space prefix)
            (should-not (get-process "test-no-stderr-stderr"))
            (should-not (get-process " test-no-stderr-stderr")))
        (when-let* ((proc (gethash "test-no-stderr" elinit--processes)))
          (delete-process proc))
        (delete-directory elinit-log-directory t)))))

(ert-deftest elinit-test-dashboard-menu-keybinding ()
  "Dashboard ? key is bound to transient menu wrapper."
  (should (eq (lookup-key elinit-dashboard-mode-map "?")
              'elinit-dashboard-menu-open)))

(ert-deftest elinit-test-dashboard-info-keybinding ()
  "Dashboard i key is bound to inspect submenu."
  (should (eq (lookup-key elinit-dashboard-mode-map "i")
              'elinit-dashboard-inspect)))

(ert-deftest elinit-test-header-hints-default-hidden ()
  "Header hints are hidden by default."
  (should-not elinit-dashboard-show-header-hints))

(ert-deftest elinit-test-transient-menu-defined ()
  "Transient menu wrapper is defined and helper function exists."
  ;; The wrapper function should always be defined
  (should (fboundp 'elinit-dashboard-menu-open))
  ;; The helper that defines the menu should exist
  (should (fboundp 'elinit--define-dashboard-menu))
  ;; After calling the helper with transient loaded, the menu should exist
  (when (require 'transient nil t)
    (elinit--define-dashboard-menu)
    (should (fboundp 'elinit-dashboard-menu))
    (should (get 'elinit-dashboard-menu 'transient--prefix))))


;;;; Conflicts planner tests

(ert-deftest elinit-test-plan-conflicts-metadata ()
  "Plan with :conflicts builds conflicts-deps hash."
  (let* ((programs '(("sleep 1" :id "a" :conflicts "b")
                     ("sleep 2" :id "b")))
         (plan (elinit--build-plan programs)))
    (should (hash-table-p (elinit-plan-conflicts-deps plan)))
    (should (equal (gethash "a" (elinit-plan-conflicts-deps plan))
                   '("b")))))

(ert-deftest elinit-test-plan-conflicts-reverse ()
  "Plan reverse map is symmetric: A conflicts B means B has A in reverse."
  (let* ((programs '(("sleep 1" :id "a" :conflicts "b")
                     ("sleep 2" :id "b")))
         (plan (elinit--build-plan programs)))
    (should (member "a" (gethash "b" (elinit-plan-conflict-reverse plan))))))

(ert-deftest elinit-test-plan-conflicts-missing-dropped ()
  "Missing conflict target is warned and dropped from plan."
  (let* ((programs '(("sleep 1" :id "a" :conflicts "nonexistent")))
         (plan (elinit--build-plan programs)))
    ;; Missing target should be dropped
    (should (null (gethash "a" (elinit-plan-conflicts-deps plan))))))

(ert-deftest elinit-test-plan-conflicts-fingerprint ()
  "Plan with conflicts has different fingerprint than without."
  (let* ((programs-a '(("sleep 1" :id "a")
                       ("sleep 2" :id "b")))
         (programs-b '(("sleep 1" :id "a" :conflicts "b")
                       ("sleep 2" :id "b")))
         (plan-a (elinit--build-plan programs-a))
         (plan-b (elinit--build-plan programs-b)))
    (should-not (equal (plist-get (elinit-plan-meta plan-a) :fingerprint)
                       (plist-get (elinit-plan-meta plan-b) :fingerprint)))))

(ert-deftest elinit-test-plan-conflicts-self-rejected ()
  "Self-reference in :conflicts is caught by validation."
  (let* ((programs '(("sleep 1" :id "a" :conflicts "a")))
         (plan (elinit--build-plan programs)))
    ;; Entry with self-conflict is invalid
    (should (gethash "a" (elinit-plan-invalid plan)))))

(ert-deftest elinit-test-plan-conflicts-deterministic ()
  "Same input produces same conflicts metadata."
  (let* ((programs '(("sleep 1" :id "a" :conflicts ("b" "c"))
                     ("sleep 2" :id "b")
                     ("sleep 3" :id "c")))
         (plan1 (elinit--build-plan programs))
         (plan2 (elinit--build-plan programs)))
    (should (equal (gethash "a" (elinit-plan-conflicts-deps plan1))
                   (gethash "a" (elinit-plan-conflicts-deps plan2))))
    (should (equal (plist-get (elinit-plan-meta plan1) :fingerprint)
                   (plist-get (elinit-plan-meta plan2) :fingerprint)))))

(ert-deftest elinit-test-plan-conflicts-both-directions ()
  "Both A and B declaring :conflicts produces both forward and reverse."
  (let* ((programs '(("sleep 1" :id "a" :conflicts "b")
                     ("sleep 2" :id "b" :conflicts "a")))
         (plan (elinit--build-plan programs)))
    (should (equal (gethash "a" (elinit-plan-conflicts-deps plan))
                   '("b")))
    (should (equal (gethash "b" (elinit-plan-conflicts-deps plan))
                   '("a")))
    (should (member "b" (gethash "a" (elinit-plan-conflict-reverse plan))))
    (should (member "a" (gethash "b" (elinit-plan-conflict-reverse plan))))))

(provide 'elinit-test-plan)
;;; elinit-test-plan.el ends here
