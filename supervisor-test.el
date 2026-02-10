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
  (should (equal supervisor-stage-names '(stage1 stage2 stage3 stage4)))
  (should (= (alist-get 'stage1 supervisor-stages) 0))
  (should (= (alist-get 'stage2 supervisor-stages) 1))
  (should (= (alist-get 'stage3 supervisor-stages) 2))
  (should (= (alist-get 'stage4 supervisor-stages) 3)))

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
    (should (eq (nth 7 parsed) 'stage3))))      ; stage (default)

(ert-deftest supervisor-test-parse-plist-entry ()
  "Parse a plist-style entry with options."
  (let ((parsed (supervisor--parse-entry
                 '("nm-applet" :type simple :stage stage2 :delay 3 :restart nil))))
    (should (equal (nth 0 parsed) "nm-applet"))
    (should (= (nth 2 parsed) 3))                ; delay
    (should (eq (nth 4 parsed) nil))             ; restart-p
    (should (eq (nth 6 parsed) 'simple))
    (should (eq (nth 7 parsed) 'stage2))))

(ert-deftest supervisor-test-parse-explicit-id ()
  "Parse entry with explicit :id."
  (let ((parsed (supervisor--parse-entry
                 '("/usr/bin/nm-applet" :id "network"))))
    (should (equal (nth 0 parsed) "network"))))

(ert-deftest supervisor-test-parse-oneshot ()
  "Parse oneshot entry."
  (let ((parsed (supervisor--parse-entry
                 '("xrdb ~/.Xresources" :type oneshot :stage stage1))))
    (should (eq (nth 6 parsed) 'oneshot))
    (should (eq (nth 7 parsed) 'stage1))))

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
  (should (= (supervisor--stage-to-int 'stage1) 0))
  (should (= (supervisor--stage-to-int 'stage2) 1))
  (should (= (supervisor--stage-to-int 'stage3) 2))
  (should (= (supervisor--stage-to-int 'stage4) 3))
  ;; Unknown defaults to stage3 (2)
  (should (= (supervisor--stage-to-int 'unknown) 2)))

(ert-deftest supervisor-test-int-to-stage ()
  "Convert integers to stage symbols."
  (should (eq (supervisor--int-to-stage 0) 'stage1))
  (should (eq (supervisor--int-to-stage 1) 'stage2))
  (should (eq (supervisor--int-to-stage 2) 'stage3))
  (should (eq (supervisor--int-to-stage 3) 'stage4)))

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
                 '("nm-applet" :type simple :stage stage3 :restart t)))))

(ert-deftest supervisor-test-validate-valid-oneshot ()
  "Valid oneshot entry passes validation."
  (should (null (supervisor--validate-entry
                 '("xrdb" :type oneshot :stage stage1 :oneshot-timeout 30)))))

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
                         '("foo" :stage "stage1")))))

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

;;; DAG scheduler tests

(ert-deftest supervisor-test-topo-sort-stable-ordering ()
  "Unconstrained nodes maintain original list order."
  (let* ((entries '(("a" "cmd" 0 t t t simple stage3 nil t 30)
                    ("b" "cmd" 0 t t t simple stage3 nil t 30)
                    ("c" "cmd" 0 t t t simple stage3 nil t 30)))
         (sorted (supervisor--stable-topo-sort entries)))
    ;; With no dependencies, order should be preserved
    (should (equal (mapcar #'car sorted) '("a" "b" "c")))))

(ert-deftest supervisor-test-topo-sort-respects-after ()
  "Entries with :after come after their dependencies."
  (let* ((entries '(("c" "cmd" 0 t t t simple stage3 ("a") t 30)
                    ("a" "cmd" 0 t t t simple stage3 nil t 30)
                    ("b" "cmd" 0 t t t simple stage3 nil t 30)))
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
  (let* ((entries '(("a" "cmd" 0 t t t simple stage3 ("b") t 30)
                    ("b" "cmd" 0 t t t simple stage3 ("a") t 30)))
         (sorted (supervisor--stable-topo-sort entries)))
    ;; Should return in original order
    (should (equal (mapcar #'car sorted) '("a" "b")))
    ;; :after (index 8) should be nil for all entries
    (should (null (nth 8 (car sorted))))
    (should (null (nth 8 (cadr sorted))))))

(ert-deftest supervisor-test-topo-sort-complex-dag ()
  "Complex DAG with multiple dependencies."
  ;; d depends on b and c, b depends on a
  (let* ((entries '(("a" "cmd" 0 t t t simple stage3 nil t 30)
                    ("b" "cmd" 0 t t t simple stage3 ("a") t 30)
                    ("c" "cmd" 0 t t t simple stage3 nil t 30)
                    ("d" "cmd" 0 t t t simple stage3 ("b" "c") t 30)))
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
    ;; Entry: (id cmd delay enabled-p restart-p logging-p type stage after oneshot-wait oneshot-timeout)
    (let ((entries '(("blocking" "cmd" 0 t t t oneshot stage3 nil t 30)
                     ("non-blocking" "cmd" 0 t t t oneshot stage3 nil nil 30)
                     ("simple" "cmd" 0 t t t simple stage3 nil t 30))))
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
    (let ((entries '(("a" "cmd" 0 t t t simple stage3 nil t 30)
                     ("b" "cmd" 0 t t t simple stage3 ("a") t 30)
                     ("c" "cmd" 0 t t t simple stage3 ("a" "b") t 30))))
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
    (let ((entries '(("a" "cmd" 0 t t t simple stage3 nil t 30)
                     ("b" "cmd" 0 t t t simple stage3 ("a") t 30)
                     ("c" "cmd" 0 t t t simple stage3 ("a") t 30))))
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
    ;;           (id   cmd   delay enabled-p restart-p logging-p type   stage   after oneshot-wait timeout)
    (let ((entries '(("a" "cmd" 0 nil t t simple stage3 nil t 30)
                     ("b" "cmd" 0 t   t t simple stage3 ("a") t 30))))
      (supervisor--dag-init entries)
      ;; Disabled entry should be marked ready immediately
      (should (gethash "a" supervisor--dag-ready))
      (should (gethash "a" supervisor--dag-started))
      ;; Entry state should be 'disabled
      (should (eq (gethash "a" supervisor--entry-state) 'disabled))
      ;; Dependent "b" should have in-degree 0 (not blocked by disabled "a")
      (should (= 0 (gethash "b" supervisor--dag-in-degree))))))

(ert-deftest supervisor-test-async-oneshot-not-blocking ()
  "Async oneshots (oneshot-wait nil) do not block stage completion."
  (let ((supervisor--dag-blocking nil)
        (supervisor--dag-in-degree nil)
        (supervisor--dag-dependents nil)
        (supervisor--dag-entries nil)
        (supervisor--dag-started nil)
        (supervisor--dag-ready nil)
        (supervisor--dag-timeout-timers nil)
        (supervisor--dag-delay-timers nil)
        (supervisor--dag-id-to-index nil))
    ;; Entry with :async t means oneshot-wait = nil
    (let ((entries '(("async-oneshot" "cmd" 0 t t t oneshot stage3 nil nil 30))))
      (supervisor--dag-init entries)
      ;; Async oneshot should NOT be in blocking set
      (should-not (gethash "async-oneshot" supervisor--dag-blocking)))))

(ert-deftest supervisor-test-stage-complete-blocked-by-delay ()
  "Delayed entries prevent stage completion until they start."
  (let ((supervisor--dag-blocking (make-hash-table :test 'equal))
        (supervisor--dag-in-degree (make-hash-table :test 'equal))
        (supervisor--dag-dependents (make-hash-table :test 'equal))
        (supervisor--dag-entries (make-hash-table :test 'equal))
        (supervisor--dag-started (make-hash-table :test 'equal))
        (supervisor--dag-ready (make-hash-table :test 'equal))
        (supervisor--dag-timeout-timers (make-hash-table :test 'equal))
        (supervisor--dag-delay-timers (make-hash-table :test 'equal))
        (supervisor--dag-id-to-index (make-hash-table :test 'equal))
        (supervisor--dag-stage-complete-callback nil)
        (supervisor--shutting-down nil)
        (callback-called nil))
    ;; Entry with delay
    (puthash "delayed" '("delayed" "echo hi" 5 t t t simple stage3 nil t 30) supervisor--dag-entries)
    (puthash "delayed" 0 supervisor--dag-in-degree)
    (puthash "delayed" 0 supervisor--dag-id-to-index)
    (puthash "delayed" nil supervisor--dag-dependents)
    (setq supervisor--dag-stage-complete-callback (lambda () (setq callback-called t)))
    ;; Simulate starting the delayed entry - adds to delay-timers
    (puthash "delayed" 'mock-timer supervisor--dag-delay-timers)
    ;; Mark as "started" from scheduler's perspective
    (puthash "delayed" t supervisor--dag-started)
    ;; Try to complete stage - should NOT call callback because delay timer exists
    (supervisor--dag-check-stage-complete)
    (should-not callback-called)))

(ert-deftest supervisor-test-stage-complete-blocked-by-blocking-oneshot ()
  "Blocking oneshots prevent stage completion until they exit."
  (let ((supervisor--dag-blocking (make-hash-table :test 'equal))
        (supervisor--dag-in-degree (make-hash-table :test 'equal))
        (supervisor--dag-dependents (make-hash-table :test 'equal))
        (supervisor--dag-entries (make-hash-table :test 'equal))
        (supervisor--dag-started (make-hash-table :test 'equal))
        (supervisor--dag-ready (make-hash-table :test 'equal))
        (supervisor--dag-timeout-timers (make-hash-table :test 'equal))
        (supervisor--dag-delay-timers (make-hash-table :test 'equal))
        (supervisor--dag-id-to-index (make-hash-table :test 'equal))
        (supervisor--dag-stage-complete-callback nil)
        (callback-called nil))
    ;; Blocking oneshot entry
    (puthash "blocking" '("blocking" "sleep 10" 0 t t t oneshot stage3 nil t 30) supervisor--dag-entries)
    (puthash "blocking" 0 supervisor--dag-in-degree)
    (puthash "blocking" t supervisor--dag-started)
    ;; Oneshot is blocking
    (puthash "blocking" t supervisor--dag-blocking)
    (setq supervisor--dag-stage-complete-callback (lambda () (setq callback-called t)))
    ;; Try to complete stage - should NOT call callback because blocking oneshot exists
    (supervisor--dag-check-stage-complete)
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
        (supervisor--dag-stage-complete-callback nil)
        (supervisor-verbose nil))
    ;; Set up blocking oneshot
    (puthash "oneshot" '("oneshot" "cmd" 0 t t t oneshot stage3 nil t 30) supervisor--dag-entries)
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
        (supervisor--dag-stage-complete-callback nil)
        (supervisor--shutting-down nil)
        (supervisor-verbose nil)
        (started-ids nil))
    ;; Stub start function to prevent actual process spawning
    (cl-letf (((symbol-function 'supervisor--dag-start-entry-async)
               (lambda (entry) (push (car entry) started-ids))))
      ;; Set up: b depends on a
      (puthash "a" '("a" "cmd" 0 t t t simple stage3 nil t 30) supervisor--dag-entries)
      (puthash "b" '("b" "cmd" 0 t t t simple stage3 ("a") t 30) supervisor--dag-entries)
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
  "Oneshot timeout calls mark-ready which unlocks dependents and stage."
  (let ((supervisor--dag-blocking (make-hash-table :test 'equal))
        (supervisor--dag-in-degree (make-hash-table :test 'equal))
        (supervisor--dag-dependents (make-hash-table :test 'equal))
        (supervisor--dag-entries (make-hash-table :test 'equal))
        (supervisor--dag-started (make-hash-table :test 'equal))
        (supervisor--dag-ready (make-hash-table :test 'equal))
        (supervisor--dag-timeout-timers (make-hash-table :test 'equal))
        (supervisor--dag-delay-timers (make-hash-table :test 'equal))
        (supervisor--dag-id-to-index (make-hash-table :test 'equal))
        (supervisor--dag-stage-complete-callback nil)
        (supervisor--shutting-down nil)
        (supervisor-verbose nil)
        (stage-complete nil)
        (started-ids nil))
    ;; Stub start function
    (cl-letf (((symbol-function 'supervisor--dag-start-entry-async)
               (lambda (entry) (push (car entry) started-ids))))
      ;; Set up: blocking oneshot "slow" with dependent "after-slow"
      (puthash "slow" '("slow" "sleep 999" 0 t t t oneshot stage3 nil t 5) supervisor--dag-entries)
      (puthash "after-slow" '("after-slow" "echo done" 0 t t t simple stage3 ("slow") t 30) supervisor--dag-entries)
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
      ;; Stage completion callback
      (setq supervisor--dag-stage-complete-callback
            (lambda () (setq stage-complete t)))
      ;; Simulate timeout firing: this is what the timeout timer does
      (supervisor--dag-mark-ready "slow")
      ;; Blocking oneshot should be removed from blocking set
      (should-not (gethash "slow" supervisor--dag-blocking))
      ;; Dependent should have been unlocked and triggered
      (should (= 0 (gethash "after-slow" supervisor--dag-in-degree)))
      (should (member "after-slow" started-ids))
      ;; Timeout timer should be removed
      (should-not (gethash "slow" supervisor--dag-timeout-timers)))))

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
    (puthash "a" '("a" "true" 0 t t t simple stage3 nil t 30) supervisor--dag-entries)
    (puthash "b" '("b" "true" 0 t t t simple stage3 nil t 30) supervisor--dag-entries)
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

(ert-deftest supervisor-test-stage-timeout-sets-entry-state ()
  "Stage timeout marks unstarted entries with stage-timeout state."
  (let ((supervisor--dag-entries (make-hash-table :test 'equal))
        (supervisor--dag-started (make-hash-table :test 'equal))
        (supervisor--dag-blocking (make-hash-table :test 'equal))
        (supervisor--dag-in-degree (make-hash-table :test 'equal))
        (supervisor--dag-dependents (make-hash-table :test 'equal))
        (supervisor--dag-ready (make-hash-table :test 'equal))
        (supervisor--dag-delay-timers (make-hash-table :test 'equal))
        (supervisor--dag-timeout-timers (make-hash-table :test 'equal))
        (supervisor--dag-id-to-index (make-hash-table :test 'equal))
        (supervisor--dag-stage-complete-callback nil)
        (supervisor--dag-stage-timeout-timer nil)
        (supervisor--dag-pending-starts nil)
        (supervisor--dag-active-starts 0)
        (supervisor--entry-state (make-hash-table :test 'equal))
        (callback-called nil))
    ;; Set up entry that hasn't started
    (puthash "delayed" '("delayed" "cmd" 5 t t t simple stage3 nil t 30)
             supervisor--dag-entries)
    (setq supervisor--dag-stage-complete-callback (lambda () (setq callback-called t)))
    ;; Force stage complete should mark unstarted as stage-timeout
    (supervisor--dag-force-stage-complete)
    (should (eq (gethash "delayed" supervisor--entry-state) 'stage-timeout))
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

(ert-deftest supervisor-test-reload-respects-enabled-override ()
  "Reload uses effective enabled state for start decisions."
  (let ((supervisor--enabled-override (make-hash-table :test 'equal))
        (supervisor--processes (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--oneshot-completed (make-hash-table :test 'equal))
        (supervisor--restart-override (make-hash-table :test 'equal))
        (supervisor--logging (make-hash-table :test 'equal))
        (supervisor--invalid (make-hash-table :test 'equal))
        (supervisor-programs '(("new-entry" :type simple)))
        (started-ids nil))
    ;; Mark entry as runtime-disabled
    (puthash "new-entry" 'disabled supervisor--enabled-override)
    ;; Mock supervisor--start-process to track what gets started
    (cl-letf (((symbol-function 'supervisor--start-process)
               (lambda (id _cmd _log _type _restart)
                 (push id started-ids)))
              ((symbol-function 'supervisor--refresh-dashboard) #'ignore)
              ((symbol-function 'executable-find) (lambda (_) t)))
      (supervisor-reload)
      ;; Entry should NOT have been started due to override
      (should-not (member "new-entry" started-ids)))))

(ert-deftest supervisor-test-reload-stops-disabled-entries ()
  "Reload stops running entries that are now disabled."
  (let ((supervisor--enabled-override (make-hash-table :test 'equal))
        (supervisor--processes (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--oneshot-completed (make-hash-table :test 'equal))
        (supervisor--restart-override (make-hash-table :test 'equal))
        (supervisor--logging (make-hash-table :test 'equal))
        (supervisor--invalid (make-hash-table :test 'equal))
        (supervisor-programs '(("running" :type simple :disabled t)))
        (killed-ids nil))
    ;; Create a fake live process
    (let ((fake-proc (start-process "test-proc" nil "sleep" "100")))
      (puthash "running" fake-proc supervisor--processes)
      ;; Mock kill-process to track kills
      (cl-letf (((symbol-function 'kill-process)
                 (lambda (proc)
                   (push (process-name proc) killed-ids)
                   (delete-process proc)))
                ((symbol-function 'supervisor--refresh-dashboard) #'ignore))
        (supervisor-reload)
        ;; Entry should have been killed due to :disabled
        (should (member "test-proc" killed-ids))))))

(ert-deftest supervisor-test-computed-deps-populated ()
  "Topo-sort populates computed-deps with validated dependencies."
  (let ((supervisor--computed-deps (make-hash-table :test 'equal))
        (supervisor--cycle-fallback-ids (make-hash-table :test 'equal)))
    ;; Entry c depends on a and b, but b doesn't exist in this stage
    (let ((entries '(("a" "cmd" 0 t t t simple stage3 nil t 30)
                     ("c" "cmd" 0 t t t simple stage3 ("a" "b") t 30))))
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
    (let ((entries '(("a" "cmd" 0 t t t simple stage3 ("b") t 30)
                     ("b" "cmd" 0 t t t simple stage3 ("a") t 30))))
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
    ;; tags is the 12th field (index 11)
    (should (equal (nth 11 parsed) '(x-setup network)))))

(ert-deftest supervisor-test-parse-tags-default-nil ()
  "Tags default to nil when not specified."
  (let ((parsed (supervisor--parse-entry "foo")))
    (should (null (nth 11 parsed)))))

(ert-deftest supervisor-test-parse-tags-single ()
  "Parse single tag (not in list)."
  (let ((parsed (supervisor--parse-entry '("foo" :tags myapp))))
    ;; Single symbol should be wrapped in list
    (should (equal (nth 11 parsed) '(myapp)))))

;;; Dry-run tests

(ert-deftest supervisor-test-dry-run-output ()
  "Dry-run produces expected output with stages."
  (let ((supervisor-programs '(("a" :stage stage1 :type oneshot)
                               ("b" :stage stage2)
                               ("c" :stage stage3)))
        (supervisor--invalid (make-hash-table :test 'equal))
        (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
        (supervisor--computed-deps (make-hash-table :test 'equal)))
    ;; supervisor-dry-run uses with-output-to-temp-buffer
    (supervisor-dry-run)
    (let ((output (with-current-buffer "*supervisor-dry-run*"
                    (buffer-string))))
      (kill-buffer "*supervisor-dry-run*")
      (should (string-match-p "Stage: stage1" output))
      (should (string-match-p "Stage: stage2" output))
      (should (string-match-p "Stage: stage3" output))
      (should (string-match-p "\\ba\\b" output))
      (should (string-match-p "\\bb\\b" output))
      (should (string-match-p "\\bc\\b" output)))))

(ert-deftest supervisor-test-dry-run-shows-invalid ()
  "Dry-run shows invalid entries."
  (let ((supervisor-programs '(("valid" :type simple)
                               ("invalid" :type "bad")))
        (supervisor--invalid (make-hash-table :test 'equal))
        (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
        (supervisor--computed-deps (make-hash-table :test 'equal)))
    (supervisor-dry-run)
    (let ((output (with-current-buffer "*supervisor-dry-run*"
                    (buffer-string))))
      (kill-buffer "*supervisor-dry-run*")
      (should (string-match-p "Invalid Entries" output))
      (should (string-match-p "invalid" output)))))

(ert-deftest supervisor-test-dry-run-validates-after ()
  "Dry-run validates :after references using same path as startup."
  (let ((supervisor-programs '(("a" :stage stage3)
                               ("b" :stage stage3 :after "nonexistent")))
        (supervisor--invalid (make-hash-table :test 'equal))
        (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
        (supervisor--computed-deps (make-hash-table :test 'equal)))
    ;; This should call supervisor--validate-after which populates computed-deps
    (supervisor-dry-run)
    (kill-buffer "*supervisor-dry-run*")
    ;; b's computed deps should be empty since nonexistent doesn't exist
    (should (null (gethash "b" supervisor--computed-deps)))
    ;; a should have nil deps (no :after)
    (should (null (gethash "a" supervisor--computed-deps)))))

;;; Dashboard UI tests

(ert-deftest supervisor-test-separator-row-detection ()
  "Separator rows are correctly identified."
  (should (supervisor--separator-row-p '--stage1--))
  (should (supervisor--separator-row-p '--stage4--))
  (should-not (supervisor--separator-row-p "nm-applet"))
  (should-not (supervisor--separator-row-p nil))
  (should-not (supervisor--separator-row-p 'some-symbol)))

(ert-deftest supervisor-test-health-summary-format ()
  "Health summary includes all required counts."
  (let ((supervisor-programs nil)
        (supervisor--invalid (make-hash-table :test 'equal))
        (supervisor--processes (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--oneshot-completed (make-hash-table :test 'equal)))
    (let ((summary (supervisor--health-summary)))
      ;; Should contain all five metrics
      (should (string-match-p "run" summary))
      (should (string-match-p "done" summary))
      (should (string-match-p "pend" summary))
      (should (string-match-p "fail" summary))
      (should (string-match-p "inv" summary)))))

(ert-deftest supervisor-test-help-text-key-parity ()
  "Help text includes all bound keys."
  ;; Keys that must be discoverable on-screen
  (should (string-match-p "\\[e\\]" supervisor--help-text))
  (should (string-match-p "\\[f\\]" supervisor--help-text))
  (should (string-match-p "\\[t\\]" supervisor--help-text))
  (should (string-match-p "\\[s\\]" supervisor--help-text))
  (should (string-match-p "\\[k\\]" supervisor--help-text))
  (should (string-match-p "\\[K\\]" supervisor--help-text))
  (should (string-match-p "\\[r\\]" supervisor--help-text))
  (should (string-match-p "\\[l\\]" supervisor--help-text))
  (should (string-match-p "\\[L\\]" supervisor--help-text))
  (should (string-match-p "\\[p\\]" supervisor--help-text))
  (should (string-match-p "\\[P\\]" supervisor--help-text))
  (should (string-match-p "\\[d\\]" supervisor--help-text))
  (should (string-match-p "\\[D\\]" supervisor--help-text))
  (should (string-match-p "\\[B\\]" supervisor--help-text))
  (should (string-match-p "\\[g\\]" supervisor--help-text))
  (should (string-match-p "\\[G\\]" supervisor--help-text))
  (should (string-match-p "\\[h\\]" supervisor--help-text))
  (should (string-match-p "\\[?\\]" supervisor--help-text))
  (should (string-match-p "\\[q\\]" supervisor--help-text)))

(ert-deftest supervisor-test-stage-separator-creation ()
  "Stage separators have correct structure."
  (let ((sep (supervisor--make-stage-separator 'stage1)))
    ;; ID should be a symbol starting with --
    (should (symbolp (car sep)))
    (should (string-prefix-p "--" (symbol-name (car sep))))
    ;; Vector should have 9 elements
    (should (= 9 (length (cadr sep))))))

(provide 'supervisor-test)
;;; supervisor-test.el ends here
