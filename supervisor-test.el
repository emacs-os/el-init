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

(ert-deftest supervisor-test-health-summary-deduplication ()
  "Health summary deduplicates entries with same ID."
  (let ((supervisor-programs '(("sleep 100" :id "dup")
                               ("sleep 100" :id "dup")))
        (supervisor--invalid (make-hash-table :test 'equal))
        (supervisor--processes (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--oneshot-completed (make-hash-table :test 'equal)))
    (let ((summary (supervisor--health-summary)))
      ;; Should count only 1 pending, not 2
      (should (string-match-p "1 pend" summary)))))

(ert-deftest supervisor-test-disabled-only-stage-completes ()
  "Stage with only disabled entries completes immediately."
  (let* ((supervisor-programs '(("sleep 100" :id "a" :disabled t :stage stage1)))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--failed (make-hash-table :test 'equal))
         (supervisor--oneshot-completed (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
         (supervisor--computed-deps (make-hash-table :test 'equal))
         (supervisor--current-stage nil)
         (supervisor--completed-stages nil)
         (supervisor--shutting-down nil)
         (completed nil))
    ;; Parse entries
    (let ((entries (supervisor--all-parsed-entries)))
      ;; Start stage with callback that sets completed flag
      (supervisor--start-stage-async
       'stage1 entries
       (lambda () (setq completed t)))
      ;; Stage should complete immediately since all entries are disabled
      (should completed))))

(ert-deftest supervisor-test-config-watch-timer-cleanup ()
  "Config watch stop cleans up debounce timer."
  ;; Set up a fake timer on the symbol property
  (let ((fake-timer (run-at-time 100 nil #'ignore)))
    (put 'supervisor--config-watch-callback 'timer fake-timer)
    ;; Stop should cancel the timer
    (supervisor--stop-config-watch)
    ;; Timer property should be nil
    (should (null (get 'supervisor--config-watch-callback 'timer)))
    ;; Timer should be cancelled (no longer in timer-list)
    (should-not (memq fake-timer timer-list))))

(ert-deftest supervisor-test-log-dir-not-created-when-logging-disabled ()
  "Log directory is not created when logging is disabled for entry."
  (let* ((temp-dir (make-temp-file "supervisor-test" t))
         (nonexistent-subdir (expand-file-name "should-not-exist" temp-dir))
         (supervisor-log-directory nonexistent-subdir)
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal))
         (supervisor--restart-timers (make-hash-table :test 'equal))
         (supervisor--failed (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--restart-override (make-hash-table :test 'equal))
         (supervisor--shutting-down nil))
    (unwind-protect
        (progn
          ;; Start with logging disabled - should NOT create log directory
          (supervisor--start-process "test" "/bin/true" nil 'oneshot nil)
          ;; Log directory should not have been created
          (should-not (file-directory-p nonexistent-subdir)))
      ;; Cleanup
      (delete-directory temp-dir t))))

(ert-deftest supervisor-test-shutdown-complete-flag-without-callback ()
  "Shutdown sets complete flag even when no callback is provided."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--shutting-down nil)
        (supervisor--shutdown-complete-flag nil)
        (supervisor--shutdown-callback nil)
        (supervisor--shutdown-remaining 0)
        (supervisor--shutdown-timer nil))
    ;; Simulate a process exit during shutdown with no callback
    (setq supervisor--shutting-down t)
    (setq supervisor--shutdown-remaining 1)
    ;; Call the exit handler (simulates sentinel calling this)
    (supervisor--handle-shutdown-exit)
    ;; Flag should be set even without callback
    (should (eq supervisor--shutdown-complete-flag t))
    (should (= supervisor--shutdown-remaining 0))))

;;; Phase 1: Plan Builder Tests

(ert-deftest supervisor-test-plan-shape ()
  "Plan struct has all required fields with correct types."
  (let* ((programs '(("sleep 100" :id "a" :stage stage1)
                     ("sleep 200" :id "b" :stage stage1 :after "a")
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
    ;; by-stage is an alist of (stage-int . entries)
    (should (listp (supervisor-plan-by-stage plan)))
    (should (assq 0 (supervisor-plan-by-stage plan)))  ; stage1 = 0
    ;; deps is a hash table
    (should (hash-table-p (supervisor-plan-deps plan)))
    ;; dependents is a hash table
    (should (hash-table-p (supervisor-plan-dependents plan)))
    ;; cycle-fallback-ids is a hash table
    (should (hash-table-p (supervisor-plan-cycle-fallback-ids plan)))
    ;; order-index is a hash table
    (should (hash-table-p (supervisor-plan-order-index plan)))
    ;; meta is a plist with version and timestamp
    (should (listp (supervisor-plan-meta plan)))
    (should (plist-get (supervisor-plan-meta plan) :version))
    (should (plist-get (supervisor-plan-meta plan) :timestamp))))

(ert-deftest supervisor-test-plan-determinism ()
  "Identical config produces identical plan data."
  (let* ((programs '(("sleep 100" :id "a" :stage stage1)
                     ("sleep 200" :id "b" :stage stage1 :after "a")
                     ("sleep 300" :id "c" :stage stage2)))
         (plan1 (supervisor--build-plan programs))
         (plan2 (supervisor--build-plan programs)))
    ;; Entries should be equal
    (should (equal (supervisor-plan-entries plan1)
                   (supervisor-plan-entries plan2)))
    ;; By-stage should have same structure
    (should (equal (length (supervisor-plan-by-stage plan1))
                   (length (supervisor-plan-by-stage plan2))))
    ;; Same stage should have same entries in same order
    (dolist (stage-pair (supervisor-plan-by-stage plan1))
      (let* ((stage-int (car stage-pair))
             (entries1 (cdr stage-pair))
             (entries2 (cdr (assq stage-int (supervisor-plan-by-stage plan2)))))
        (should (equal entries1 entries2))))
    ;; Deps should be equal
    (should (= (hash-table-count (supervisor-plan-deps plan1))
               (hash-table-count (supervisor-plan-deps plan2))))
    (maphash (lambda (k v)
               (should (equal v (gethash k (supervisor-plan-deps plan2)))))
             (supervisor-plan-deps plan1))))

(ert-deftest supervisor-test-plan-no-global-mutation ()
  "Plan building does not mutate global state."
  (let ((programs '(("sleep 100" :id "a" :stage stage1)
                    ("sleep 200" :id "b" :stage stage1 :after "a"))))
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
  (let* ((programs '(("sleep 100" :id "a" :stage stage1 :after "b")
                     ("sleep 200" :id "b" :stage stage1 :after "a")))
         (plan (supervisor--build-plan programs)))
    ;; Both entries should be marked as cycle fallback
    (should (gethash "a" (supervisor-plan-cycle-fallback-ids plan)))
    (should (gethash "b" (supervisor-plan-cycle-fallback-ids plan)))
    ;; Deps should be cleared after fallback
    (should (null (gethash "a" (supervisor-plan-deps plan))))
    (should (null (gethash "b" (supervisor-plan-deps plan))))))

(ert-deftest supervisor-test-plan-dependency-validation ()
  "Plan validates :after references correctly."
  (let* ((programs '(("sleep 100" :id "a" :stage stage1)
                     ("sleep 200" :id "b" :stage stage1 :after "a")
                     ("sleep 300" :id "c" :stage stage1 :after "nonexistent")
                     ("sleep 400" :id "d" :stage stage2 :after "a")))
         (plan (supervisor--build-plan programs)))
    ;; b's dep on a (same stage) should be preserved
    (should (equal '("a") (gethash "b" (supervisor-plan-deps plan))))
    ;; c's dep on nonexistent should be removed
    (should (null (gethash "c" (supervisor-plan-deps plan))))
    ;; d's dep on a (cross-stage) should be removed
    (should (null (gethash "d" (supervisor-plan-deps plan))))))

(ert-deftest supervisor-test-plan-duplicate-id-first-occurrence-order ()
  "Duplicate IDs use first-occurrence index for ordering.
Regression test: duplicates must not overwrite order-index of kept entry."
  (let* ((programs '(("sleep 100" :id "a" :stage stage1)
                     ("sleep 200" :id "b" :stage stage1)
                     ("sleep 300" :id "a" :stage stage1)))  ; duplicate of a
         (plan (supervisor--build-plan programs)))
    ;; Only 2 entries should be in plan (a and b, duplicate skipped)
    (should (= 2 (length (supervisor-plan-entries plan))))
    ;; Order index should reflect first occurrence: a=0, b=1
    (should (= 0 (gethash "a" (supervisor-plan-order-index plan))))
    (should (= 1 (gethash "b" (supervisor-plan-order-index plan))))
    ;; Stage order should be a, b (not b, a)
    (let* ((stage-entries (cdr (assq 0 (supervisor-plan-by-stage plan))))
           (ids (mapcar #'car stage-entries)))
      (should (equal '("a" "b") ids)))))

;;; Phase 2: Startup Uses Plan Tests

(ert-deftest supervisor-test-startup-populates-globals-from-plan ()
  "Plan->global copy mechanism populates legacy globals correctly.
Unit test for the plan data extraction used by supervisor-start.
Note: Does not call supervisor-start directly to avoid process spawning."
  (let ((supervisor-programs '(("sleep 100" :id "a" :stage stage1)
                               ("sleep 200" :id "b" :stage stage1 :after "a")
                               ("invalid" :unknown-key t)))
        (supervisor--invalid (make-hash-table :test 'equal))
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
        (supervisor--current-stage nil)
        (supervisor--completed-stages nil)
        (supervisor--shutting-down t))  ; Prevent actual process starts
    ;; Build plan and populate globals (same as supervisor-start)
    (let ((plan (supervisor--build-plan supervisor-programs)))
      (maphash (lambda (k v) (puthash k v supervisor--invalid))
               (supervisor-plan-invalid plan))
      (maphash (lambda (k v) (puthash k v supervisor--cycle-fallback-ids))
               (supervisor-plan-cycle-fallback-ids plan))
      (maphash (lambda (k v) (puthash k v supervisor--computed-deps))
               (supervisor-plan-deps plan)))
    ;; Verify globals were populated from plan
    (should (gethash "invalid" supervisor--invalid))  ; Invalid entry recorded
    (should (equal '("a") (gethash "b" supervisor--computed-deps)))  ; Deps computed
    (should (= 1 (hash-table-count supervisor--invalid)))))

(ert-deftest supervisor-test-plan-build-warns-on-duplicates ()
  "Plan building emits warnings for duplicate IDs.
Regression test for warning parity with legacy startup path."
  (let ((supervisor-programs '(("sleep 100" :id "dup")
                               ("sleep 200" :id "dup")))
        (messages nil))
    (cl-letf (((symbol-function 'supervisor--log)
               (lambda (_level fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (supervisor--build-plan supervisor-programs))
    ;; Should have warned about duplicate
    (should (cl-some (lambda (m) (string-match-p "duplicate ID 'dup'" m))
                     messages))))

(ert-deftest supervisor-test-plan-build-warns-on-invalid-after ()
  "Plan building emits warnings for invalid :after references.
Regression test for warning parity with legacy startup path."
  (let ((supervisor-programs '(("sleep 100" :id "a" :stage stage1)
                               ("sleep 200" :id "b" :stage stage1 :after "nonexistent")
                               ("sleep 300" :id "c" :stage stage2 :after "a")))
        (messages nil))
    (cl-letf (((symbol-function 'supervisor--log)
               (lambda (_level fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (supervisor--build-plan supervisor-programs))
    ;; Should have warned about nonexistent dep
    (should (cl-some (lambda (m) (string-match-p "does not exist" m))
                     messages))
    ;; Should have warned about cross-stage dep
    (should (cl-some (lambda (m) (string-match-p "different stage" m))
                     messages))))

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
  (let ((supervisor-programs '(("sleep 100" :id "a")
                               ("sleep 200" :id "b")))
        (supervisor--processes (make-hash-table :test 'equal))
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
          (should (equal health-globals health-snapshot)))))))

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
  (let ((supervisor-programs '(("sleep 100" :id "a")))
        (supervisor--processes (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--oneshot-completed (make-hash-table :test 'equal))
        (supervisor--entry-state (make-hash-table :test 'equal))
        (supervisor--invalid (make-hash-table :test 'equal))
        (supervisor--enabled-override (make-hash-table :test 'equal))
        (supervisor--restart-override (make-hash-table :test 'equal))
        (supervisor--logging (make-hash-table :test 'equal))
        (supervisor--current-stage nil)
        (supervisor--completed-stages nil)
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
          (kill-buffer buf))))))

;;; Phase 4: Declarative Reconciler Tests

(ert-deftest supervisor-test-compute-actions-start ()
  "Compute-actions returns start action for entries not running."
  (let* ((supervisor-programs '(("sleep 100" :id "a")))
         (plan (supervisor--build-plan supervisor-programs))
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
    (should (eq 'new-entry (plist-get (car actions) :reason)))))

(ert-deftest supervisor-test-compute-actions-stop-removed ()
  "Compute-actions returns stop action for running entries not in plan."
  (let* ((supervisor-programs nil)  ; Empty plan
         (plan (supervisor--build-plan supervisor-programs))
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
      (should (eq 'removed (plist-get (car actions) :reason))))))

(ert-deftest supervisor-test-compute-actions-stop-disabled ()
  "Compute-actions returns stop action for running entries now disabled."
  (let* ((supervisor-programs '(("sleep 100" :id "a" :disabled t)))
         (plan (supervisor--build-plan supervisor-programs))
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
        (should (eq 'disabled (plist-get stop-action :reason)))))))

(ert-deftest supervisor-test-compute-actions-noop-already-running ()
  "Compute-actions returns noop for entries already running."
  (let* ((supervisor-programs '(("sleep 100" :id "a")))
         (plan (supervisor--build-plan supervisor-programs))
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
      (should (eq 'already-running (plist-get (car actions) :reason))))))

(ert-deftest supervisor-test-compute-actions-skip-failed ()
  "Compute-actions returns skip for failed entries."
  (let* ((supervisor-programs '(("sleep 100" :id "a")))
         (plan (supervisor--build-plan supervisor-programs))
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
      (should (eq 'failed (plist-get (car actions) :reason))))))

(ert-deftest supervisor-test-compute-actions-skip-oneshot-completed ()
  "Compute-actions returns skip for completed oneshots."
  (let* ((supervisor-programs '(("echo done" :id "a" :type oneshot)))
         (plan (supervisor--build-plan supervisor-programs))
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
      (should (eq 'oneshot-completed (plist-get (car actions) :reason))))))

(ert-deftest supervisor-test-compute-actions-skip-disabled ()
  "Compute-actions returns skip for disabled entries not running."
  (let* ((supervisor-programs '(("sleep 100" :id "a" :disabled t)))
         (plan (supervisor--build-plan supervisor-programs))
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
    (should (eq 'disabled (plist-get (car actions) :reason)))))

(ert-deftest supervisor-test-compute-actions-is-pure ()
  "Compute-actions does not modify any global state."
  (let* ((supervisor-programs '(("sleep 100" :id "a")
                                ("sleep 200" :id "b" :disabled t)))
         (plan (supervisor--build-plan supervisor-programs))
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
      (should (= 0 (hash-table-count supervisor--restart-override))))))

(ert-deftest supervisor-test-compute-actions-idempotent ()
  "Idempotence: second call on converged state produces only noops/skips.
When actual state matches desired state, no start/stop actions are needed."
  (let* ((supervisor-programs '(("sleep 100" :id "a")
                                ("sleep 200" :id "b")
                                ("echo done" :id "c" :type oneshot)
                                ("sleep 300" :id "d" :disabled t)))
         (plan (supervisor--build-plan supervisor-programs))
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
        (should (memq (plist-get action :op) '(noop skip)))))))

(ert-deftest supervisor-test-compute-actions-action-matrix ()
  "Comprehensive test of action matrix with mixed scenarios.
Covers add, remove, disable (running), already-running, failed."
  (let* (;; Plan: a (new), b (already running), c (running but now disabled),
         ;;       d (failed), e (disabled from start)
         (supervisor-programs '(("sleep 100" :id "a")
                                ("sleep 200" :id "b")
                                ("sleep 300" :id "c" :disabled t)
                                ("sleep 400" :id "d")
                                ("sleep 500" :id "e" :disabled t)))
         (plan (supervisor--build-plan supervisor-programs))
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
      (should (eq 'skip (plist-get (gethash "e" by-id) :op))))))

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
  (should (memq 'stage-not-started supervisor--valid-states))
  (should (memq 'waiting-on-deps supervisor--valid-states))
  (should (memq 'delayed supervisor--valid-states))
  (should (memq 'disabled supervisor--valid-states))
  (should (memq 'started supervisor--valid-states))
  (should (memq 'failed-to-spawn supervisor--valid-states))
  (should (memq 'stage-timeout supervisor--valid-states)))

(ert-deftest supervisor-test-fsm-invalid-state-rejected ()
  "Transitioning to an invalid state signals an error."
  (let ((supervisor--entry-state (make-hash-table :test 'equal)))
    (should-error (supervisor--transition-state "test" 'invalid-state)
                  :type 'error)))

(ert-deftest supervisor-test-fsm-invalid-transition-rejected ()
  "Invalid state transitions signal an error."
  (let ((supervisor--entry-state (make-hash-table :test 'equal)))
    ;; Set up valid path to terminal state
    (supervisor--transition-state "test" 'stage-not-started)
    (supervisor--transition-state "test" 'started)
    ;; Transition from terminal state should error
    (should-error (supervisor--transition-state "test" 'delayed)
                  :type 'error)))

(ert-deftest supervisor-test-fsm-valid-transitions ()
  "Valid state transitions succeed."
  (let ((supervisor--entry-state (make-hash-table :test 'equal)))
    ;; nil -> stage-not-started
    (should (supervisor--transition-state "a" 'stage-not-started))
    (should (eq 'stage-not-started (gethash "a" supervisor--entry-state)))
    ;; stage-not-started -> waiting-on-deps
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
    (supervisor--transition-state "a" 'stage-not-started)
    ;; Self-transition should succeed
    (should (supervisor--transition-state "a" 'stage-not-started))
    (should (eq 'stage-not-started (gethash "a" supervisor--entry-state)))))

(ert-deftest supervisor-test-fsm-force-invalid-transition ()
  "Force parameter allows invalid transitions with warning.
Returns nil (not t) and emits a warning for forced invalid transitions."
  (let ((supervisor--entry-state (make-hash-table :test 'equal))
        (warnings nil))
    ;; Set up valid path to terminal state
    (supervisor--transition-state "a" 'stage-not-started)
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
    (supervisor--transition-state "proc" 'stage-not-started)
    ;; After delay
    (supervisor--transition-state "proc" 'delayed)
    ;; After successful spawn
    (supervisor--transition-state "proc" 'started)
    (should (eq 'started (gethash "proc" supervisor--entry-state)))))

(ert-deftest supervisor-test-fsm-lifecycle-spawn-failure ()
  "Test lifecycle path for spawn failure."
  (let ((supervisor--entry-state (make-hash-table :test 'equal)))
    ;; Initial state
    (supervisor--transition-state "proc" 'stage-not-started)
    ;; Spawn fails
    (supervisor--transition-state "proc" 'failed-to-spawn)
    (should (eq 'failed-to-spawn (gethash "proc" supervisor--entry-state)))))

(ert-deftest supervisor-test-fsm-lifecycle-stage-timeout ()
  "Test lifecycle path for stage timeout."
  (let ((supervisor--entry-state (make-hash-table :test 'equal)))
    ;; Initial state with deps
    (supervisor--transition-state "proc" 'waiting-on-deps)
    ;; Stage times out before deps complete
    (supervisor--transition-state "proc" 'stage-timeout)
    (should (eq 'stage-timeout (gethash "proc" supervisor--entry-state)))))

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
    (supervisor--transition-state "oneshot" 'stage-not-started)
    ;; Started (oneshot still uses 'started state)
    (supervisor--transition-state "oneshot" 'started)
    (should (eq 'started (gethash "oneshot" supervisor--entry-state)))))

;;; Phase 6: Structured Event Dispatcher Tests

(ert-deftest supervisor-test-event-types-defined ()
  "All event types are defined in the event type list."
  (should (memq 'stage-start supervisor--event-types))
  (should (memq 'stage-complete supervisor--event-types))
  (should (memq 'process-started supervisor--event-types))
  (should (memq 'process-ready supervisor--event-types))
  (should (memq 'process-exit supervisor--event-types))
  (should (memq 'process-failed supervisor--event-types))
  (should (memq 'cleanup supervisor--event-types)))

(ert-deftest supervisor-test-emit-event-invalid-type-rejected ()
  "Emitting an invalid event type signals an error."
  (should-error (supervisor--emit-event 'invalid-event-type nil nil nil)
                :type 'error))

(ert-deftest supervisor-test-emit-event-schema ()
  "Emitted events have the correct schema."
  (let ((events nil))
    (cl-letf (((symbol-function 'run-hook-with-args)
               (lambda (hook &rest args) (when (eq hook 'supervisor-event-hook)
                                           (push (car args) events))))
              ((symbol-function 'run-hooks) #'ignore))
      (supervisor--emit-event 'stage-start nil 'stage1 nil)
      (let ((event (car events)))
        (should (eq 'stage-start (plist-get event :type)))
        (should (numberp (plist-get event :ts)))
        (should (null (plist-get event :id)))
        (should (eq 'stage1 (plist-get event :stage)))))))

(ert-deftest supervisor-test-emit-event-stage-start ()
  "Stage-start event is emitted with correct data."
  (let ((events nil))
    (cl-letf (((symbol-function 'run-hook-with-args)
               (lambda (hook &rest args)
                 (when (eq hook 'supervisor-event-hook)
                   (push (car args) events))))
              ((symbol-function 'run-hooks) #'ignore))
      (supervisor--emit-event 'stage-start nil 'stage2 nil)
      (should (= 1 (length events)))
      (let ((event (car events)))
        (should (eq 'stage-start (plist-get event :type)))
        (should (eq 'stage2 (plist-get event :stage)))))))

(ert-deftest supervisor-test-emit-event-process-exit ()
  "Process-exit event carries status and code in data."
  (let ((events nil))
    (cl-letf (((symbol-function 'run-hook-with-args)
               (lambda (hook &rest args)
                 (when (eq hook 'supervisor-event-hook)
                   (push (car args) events))))
              ((symbol-function 'run-hooks) #'ignore))
      (supervisor--emit-event 'process-exit "myproc" nil
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
      (supervisor--emit-event 'process-started "daemon" nil
                              (list :type 'simple))
      (should (= 1 (length events)))
      (let ((event (car events)))
        (should (eq 'process-started (plist-get event :type)))
        (should (equal "daemon" (plist-get event :id)))
        (should (eq 'simple (plist-get (plist-get event :data) :type)))))))

(ert-deftest supervisor-test-legacy-hook-stage-start-compatibility ()
  "Stage-start event dispatches to legacy supervisor-stage-start-hook."
  (let ((legacy-calls nil))
    (cl-letf (((symbol-function 'run-hook-with-args)
               (lambda (hook &rest args)
                 (when (eq hook 'supervisor-stage-start-hook)
                   (push args legacy-calls)))))
      (supervisor--emit-event 'stage-start nil 'stage3 nil)
      (should (= 1 (length legacy-calls)))
      (should (eq 'stage3 (caar legacy-calls))))))

(ert-deftest supervisor-test-legacy-hook-stage-complete-compatibility ()
  "Stage-complete event dispatches to legacy supervisor-stage-complete-hook."
  (let ((legacy-calls nil))
    (cl-letf (((symbol-function 'run-hook-with-args)
               (lambda (hook &rest args)
                 (when (eq hook 'supervisor-stage-complete-hook)
                   (push args legacy-calls)))))
      (supervisor--emit-event 'stage-complete nil 'stage4 nil)
      (should (= 1 (length legacy-calls)))
      (should (eq 'stage4 (caar legacy-calls))))))

(ert-deftest supervisor-test-legacy-hook-process-exit-compatibility ()
  "Process-exit event dispatches to legacy supervisor-process-exit-hook."
  (let ((legacy-calls nil))
    (cl-letf (((symbol-function 'run-hook-with-args)
               (lambda (hook &rest args)
                 (when (eq hook 'supervisor-process-exit-hook)
                   (push args legacy-calls)))))
      (supervisor--emit-event 'process-exit "proc1" nil
                              (list :status 'signal :code 15))
      (should (= 1 (length legacy-calls)))
      (let ((call (car legacy-calls)))
        (should (equal "proc1" (nth 0 call)))
        (should (eq 'signal (nth 1 call)))
        (should (= 15 (nth 2 call)))))))

(ert-deftest supervisor-test-legacy-hook-cleanup-compatibility ()
  "Cleanup event dispatches to legacy supervisor-cleanup-hook."
  (let ((cleanup-called nil))
    (cl-letf (((symbol-function 'run-hooks)
               (lambda (hook)
                 (when (eq hook 'supervisor-cleanup-hook)
                   (setq cleanup-called t))))
              ((symbol-function 'run-hook-with-args) #'ignore))
      (supervisor--emit-event 'cleanup nil nil nil)
      (should cleanup-called))))

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
        (supervisor--dag-stage-complete-callback nil)
        (supervisor--dag-entries (make-hash-table :test 'equal)))
    ;; Set up initial state
    (supervisor--transition-state "test" 'stage-not-started)
    (puthash "test" '("test" "cmd" 0 t t t simple stage3 nil t 30 nil)
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
        (supervisor--dag-stage-complete-callback nil)
        (supervisor--dag-entries (make-hash-table :test 'equal))
        (supervisor--enabled-override (make-hash-table :test 'equal)))
    ;; Set up as disabled entry
    (puthash "test" '("test" "cmd" 0 nil t t simple stage3 nil t 30 nil)
             supervisor--dag-entries)
    ;; Capture events
    (cl-letf (((symbol-function 'run-hook-with-args)
               (lambda (hook &rest args)
                 (when (eq hook 'supervisor-event-hook)
                   (push (car args) events))))
              ((symbol-function 'run-hooks) #'ignore))
      (supervisor--dag-start-entry-async
       '("test" "cmd" 0 nil t t simple stage3 nil t 30 nil)))
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
        (supervisor--dag-stage-complete-callback nil)
        (supervisor--dag-entries (make-hash-table :test 'equal)))
    ;; Set up initial state
    (supervisor--transition-state "test" 'stage-not-started)
    (puthash "test" '("test" "sleep" 0 t t t simple stage3 nil t 30 nil)
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

(provide 'supervisor-test)
;;; supervisor-test.el ends here
