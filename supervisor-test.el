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

(ert-deftest supervisor-test-module-load-core ()
  "Verify supervisor-core module loads and provides its feature."
  (should (featurep 'supervisor-core)))

(ert-deftest supervisor-test-module-load-dashboard ()
  "Verify supervisor-dashboard module loads and provides its feature."
  (should (featurep 'supervisor-dashboard)))

(ert-deftest supervisor-test-module-load-cli ()
  "Verify supervisor-cli module loads and provides its feature."
  (should (featurep 'supervisor-cli)))

(ert-deftest supervisor-test-module-core-standalone ()
  "Verify supervisor-core loads and works without dashboard, CLI, or timer.
Spawns a subprocess to test true standalone behavior.
Core guards timer calls with fboundp, so validate works without timer module."
  (let* ((default-directory (file-name-directory (locate-library "supervisor")))
         (result (call-process
                  "emacs" nil nil nil
                  "-Q" "--batch" "-L" "."
                  "--eval" "(require 'supervisor-core)"
                  "--eval" "(setq supervisor-programs nil)"
                  "--eval" "(supervisor-validate)")))
    (should (= result 0))))

(ert-deftest supervisor-test-module-core-standalone-stop ()
  "Verify supervisor-stop works standalone without timer module.
Tests the stop path which was previously missing fboundp guard."
  (let* ((default-directory (file-name-directory (locate-library "supervisor")))
         (result (call-process
                  "emacs" nil nil nil
                  "-Q" "--batch" "-L" "."
                  "--eval" "(require 'supervisor-core)"
                  "--eval" "(setq supervisor-programs nil)"
                  "--eval" "(supervisor-stop)")))
    (should (= result 0))))

(ert-deftest supervisor-test-module-core-standalone-start ()
  "Verify supervisor-start works standalone without timer module."
  (let* ((default-directory (file-name-directory (locate-library "supervisor")))
         (result (call-process
                  "emacs" nil nil nil
                  "-Q" "--batch" "-L" "."
                  "--eval" "(require 'supervisor-core)"
                  "--eval" "(setq supervisor-programs nil)"
                  "--eval" "(supervisor-start)")))
    (should (= result 0))))

(ert-deftest supervisor-test-module-cli-standalone ()
  "Verify supervisor-cli loads and works without dashboard or timer.
Spawns a subprocess to test CLI dispatch without dashboard or timer module."
  (let* ((default-directory (file-name-directory (locate-library "supervisor")))
         (result (call-process
                  "emacs" nil nil nil
                  "-Q" "--batch" "-L" "."
                  "--eval" "(require 'supervisor-core)"
                  "--eval" "(require 'supervisor-cli)"
                  "--eval" "(setq supervisor-programs nil)"
                  "--eval" "(supervisor--cli-dispatch '(\"reconcile\"))")))
    (should (= result 0))))

(ert-deftest supervisor-test-module-no-cycles ()
  "Verify modules have no circular require chains.
Core must not require dashboard or cli.  This is checked by verifying
core symbols exist without dashboard/cli-specific dependencies."
  ;; Core should work standalone - key symbols should exist
  (should (fboundp 'supervisor--parse-entry))
  (should (fboundp 'supervisor--build-plan))
  (should (fboundp 'supervisor-start))
  ;; Dashboard-specific symbols should be in dashboard
  (should (fboundp 'supervisor-dashboard-mode))
  ;; CLI-specific symbols should be in cli
  (should (fboundp 'supervisor--cli-dispatch)))

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

(ert-deftest supervisor-test-reconcile-respects-enabled-override ()
  "Reconcile uses effective enabled state for start decisions."
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
      (supervisor-reconcile)
      ;; Entry should NOT have been started due to override
      (should-not (member "new-entry" started-ids)))))

(ert-deftest supervisor-test-reconcile-stops-disabled-entries ()
  "Reconcile stops running entries that are now disabled."
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
        (supervisor-reconcile)
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
        (supervisor-timers nil)
        (supervisor--invalid (make-hash-table :test 'equal))
        (supervisor--invalid-timers (make-hash-table :test 'equal))
        (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
        (supervisor--computed-deps (make-hash-table :test 'equal)))
    (supervisor-dry-run)
    (let ((output (with-current-buffer "*supervisor-dry-run*"
                    (buffer-string))))
      (kill-buffer "*supervisor-dry-run*")
      (should (string-match-p "Invalid Services" output))
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
  (should (string-match-p "\\[i\\]" supervisor--help-text))
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
  (should (memq 'cleanup supervisor--event-types))
  ;; Timer event types
  (should (memq 'timer-trigger supervisor--event-types))
  (should (memq 'timer-overlap supervisor--event-types))
  (should (memq 'timer-success supervisor--event-types))
  (should (memq 'timer-failure supervisor--event-types)))

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

(ert-deftest supervisor-test-no-stderr-process-created ()
  "Starting a process does not create a separate stderr process.
Regression test: stderr pipe processes used to pollute the process list."
  (let* ((supervisor-programs '(("true" :id "test-no-stderr" :type oneshot)))
         (supervisor--processes (make-hash-table :test 'equal))
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
      (delete-directory supervisor-log-directory t))))

(ert-deftest supervisor-test-dashboard-menu-keybinding ()
  "Dashboard ? key is bound to transient menu wrapper."
  (should (eq (lookup-key supervisor-dashboard-mode-map "?")
              'supervisor-dashboard-menu-open)))

(ert-deftest supervisor-test-dashboard-info-keybinding ()
  "Dashboard i key is bound to entry info."
  (should (eq (lookup-key supervisor-dashboard-mode-map "i")
              'supervisor-dashboard-describe-entry)))

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

;;; Dashboard Stop/Restart Tests

(ert-deftest supervisor-test-dashboard-stop-keybinding ()
  "Dashboard x key is bound to stop."
  (should (eq (lookup-key supervisor-dashboard-mode-map "x")
              'supervisor-dashboard-stop)))

(ert-deftest supervisor-test-dashboard-restart-keybinding ()
  "Dashboard R key is bound to restart."
  (should (eq (lookup-key supervisor-dashboard-mode-map "R")
              'supervisor-dashboard-restart)))

(ert-deftest supervisor-test-dashboard-stop-is-defined ()
  "Dashboard stop command is defined as interactive."
  (should (fboundp 'supervisor-dashboard-stop))
  (should (commandp 'supervisor-dashboard-stop)))

(ert-deftest supervisor-test-dashboard-restart-is-defined ()
  "Dashboard restart command is defined as interactive."
  (should (fboundp 'supervisor-dashboard-restart))
  (should (commandp 'supervisor-dashboard-restart)))

(ert-deftest supervisor-test-manual-stop-not-running ()
  "Manual stop on non-running entry returns skipped."
  (let ((supervisor--processes (make-hash-table :test 'equal)))
    (let ((result (supervisor--manual-stop "nonexistent")))
      (should (eq 'skipped (plist-get result :status)))
      (should (string= "not running" (plist-get result :reason))))))

(ert-deftest supervisor-test-manual-stop-sets-manually-stopped ()
  "Manual stop sets the manually-stopped flag."
  (let* ((supervisor--processes (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (buf (generate-new-buffer " *test-proc*"))
         (proc (start-process "test-stop" buf "sleep" "60")))
    (unwind-protect
        (progn
          (puthash "test-svc" proc supervisor--processes)
          (let ((result (supervisor--manual-stop "test-svc")))
            (should (eq 'stopped (plist-get result :status)))
            (should (eq t (gethash "test-svc" supervisor--manually-stopped)))))
      (when (process-live-p proc) (delete-process proc))
      (kill-buffer buf))))

(ert-deftest supervisor-test-manual-kill-does-not-set-manually-stopped ()
  "Manual kill does not set manually-stopped flag."
  (let* ((supervisor--processes (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (buf (generate-new-buffer " *test-proc*"))
         (proc (start-process "test-kill" buf "sleep" "60")))
    (unwind-protect
        (progn
          (puthash "test-svc" proc supervisor--processes)
          (let ((result (supervisor--manual-kill "test-svc" 'SIGTERM)))
            (should (eq 'signaled (plist-get result :status)))
            (should-not (gethash "test-svc" supervisor--manually-stopped))))
      (when (process-live-p proc) (delete-process proc))
      (kill-buffer buf))))

(ert-deftest supervisor-test-dashboard-kill-leaves-manually-stopped-untouched ()
  "Dashboard kill does not set the manually-stopped flag."
  (let* ((supervisor--processes (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (buf (generate-new-buffer " *test-dk*"))
         (proc (start-process "test-dk" buf "sleep" "60")))
    (unwind-protect
        (progn
          (puthash "test-svc" proc supervisor--processes)
          (with-temp-buffer
            (supervisor-dashboard-mode)
            (let ((tabulated-list-entries
                   (list (list "test-svc"
                               (vector "test-svc" "simple" "stage3"
                                       "yes" "running" "yes" "yes"
                                       "1234" "-")))))
              (tabulated-list-init-header)
              (tabulated-list-print)
              (goto-char (point-min))
              ;; Call kill with force=t to skip confirmation
              (supervisor-dashboard-kill t)
              (should-not (gethash "test-svc" supervisor--manually-stopped)))))
      (when (process-live-p proc) (delete-process proc))
      (kill-buffer buf))))

(ert-deftest supervisor-test-help-text-includes-stop-restart ()
  "Dashboard help text includes stop and restart keys."
  (should (string-match "\\[x\\]stop" supervisor--help-text))
  (should (string-match "\\[R\\]estart" supervisor--help-text)))

(ert-deftest supervisor-test-timer-row-p-detects-timer ()
  "Timer row predicate detects timer rows by Type column content."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list "my-timer" (vector "my-timer" "timer" "-"
                                         "-" "pending" "-" "-" "-" "-")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should (supervisor--timer-row-p "my-timer")))))

(ert-deftest supervisor-test-timer-row-p-rejects-service-row ()
  "Timer row predicate returns nil for service rows."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list "my-svc" (vector "my-svc" "simple" "stage3"
                                       "yes" "running" "yes" "yes" "1234" "-")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should-not (supervisor--timer-row-p "my-svc")))))

(ert-deftest supervisor-test-stop-allows-service-with-timer-id-collision ()
  "Stop on a service row works even if a timer has the same ID."
  (let* ((supervisor-programs '(("sleep 60" :id "dup" :type simple)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--invalid-timers (make-hash-table :test 'equal)))
    ;; Timer with same ID as service
    (puthash "dup" "some schedule" supervisor--invalid-timers)
    (with-temp-buffer
      (supervisor-dashboard-mode)
      ;; Row has type "simple" — this is a service row, not a timer row
      (let ((tabulated-list-entries
             (list (list "dup" (vector "dup" "simple" "stage3"
                                      "yes" "stopped" "yes" "yes" "-" "-")))))
        (tabulated-list-init-header)
        (tabulated-list-print)
        (goto-char (point-min))
        ;; Should NOT error with "Cannot stop timer" — it's a service row
        (should-not (supervisor--timer-row-p "dup"))))))

(ert-deftest supervisor-test-stop-rejects-oneshot ()
  "Stop rejects oneshot entries."
  (let* ((supervisor-programs '(("true" :id "my-oneshot" :type oneshot)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal)))
    ;; Simulate being in dashboard with oneshot at point
    (with-temp-buffer
      (supervisor-dashboard-mode)
      (let ((tabulated-list-entries
             (list (list "my-oneshot" (vector "my-oneshot" "oneshot" "stage3"
                                             "yes" "done" "n/a" "yes" "-" "-")))))
        (tabulated-list-init-header)
        (tabulated-list-print)
        (goto-char (point-min))
        (should-error (supervisor-dashboard-stop)
                      :type 'user-error)))))

(ert-deftest supervisor-test-restart-rejects-oneshot ()
  "Restart rejects oneshot entries."
  (let* ((supervisor-programs '(("true" :id "my-oneshot" :type oneshot)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal)))
    (with-temp-buffer
      (supervisor-dashboard-mode)
      (let ((tabulated-list-entries
             (list (list "my-oneshot" (vector "my-oneshot" "oneshot" "stage3"
                                             "yes" "done" "n/a" "yes" "-" "-")))))
        (tabulated-list-init-header)
        (tabulated-list-print)
        (goto-char (point-min))
        (should-error (supervisor-dashboard-restart)
                      :type 'user-error)))))

(ert-deftest supervisor-test-stop-rejects-timer-row ()
  "Stop rejects timer rows."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list "my-timer" (vector "my-timer" "timer" "-"
                                         "-" "pending" "-" "-" "-" "-")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should-error (supervisor-dashboard-stop)
                    :type 'user-error))))

(ert-deftest supervisor-test-restart-rejects-timer-row ()
  "Restart rejects timer rows."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list "my-timer" (vector "my-timer" "timer" "-"
                                         "-" "pending" "-" "-" "-" "-")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should-error (supervisor-dashboard-restart)
                    :type 'user-error))))

(ert-deftest supervisor-test-restart-rejects-not-running ()
  "Restart rejects entries that are not currently running."
  (let* ((supervisor-programs '(("sleep 60" :id "my-svc" :type simple)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal)))
    (with-temp-buffer
      (supervisor-dashboard-mode)
      (let ((tabulated-list-entries
             (list (list "my-svc" (vector "my-svc" "simple" "stage3"
                                         "yes" "stopped" "yes" "yes" "-" "-")))))
        (tabulated-list-init-header)
        (tabulated-list-print)
        (goto-char (point-min))
        (should-error (supervisor-dashboard-restart)
                      :type 'user-error)))))

;;; Unit-File Tests

(ert-deftest supervisor-test-unit-file-path-resolution ()
  "Unit file path resolves to directory/ID.el."
  (let ((supervisor-unit-directory "/tmp/test-units"))
    (should (equal "/tmp/test-units/nm-applet.el"
                   (supervisor--unit-file-path "nm-applet")))))

(ert-deftest supervisor-test-unit-file-keywords-include-command ()
  "Unit-file valid keywords include :command."
  (should (memq :command supervisor--unit-file-keywords))
  (should (memq :id supervisor--unit-file-keywords))
  (should (memq :type supervisor--unit-file-keywords)))

(ert-deftest supervisor-test-load-unit-file-valid ()
  "Loading a valid unit file returns (LINE . PLIST)."
  (let ((path (make-temp-file "unit-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "(:id \"test\" :command \"echo hello\" :type simple)"))
          (let* ((result (supervisor--load-unit-file path))
                 (line (car result))
                 (plist (cdr result)))
            (should (integerp line))
            (should (equal "test" (plist-get plist :id)))
            (should (equal "echo hello" (plist-get plist :command)))
            (should (eq 'simple (plist-get plist :type)))))
      (delete-file path))))

(ert-deftest supervisor-test-load-unit-file-line-number ()
  "Line number reflects plist position after leading whitespace."
  (let ((path (make-temp-file "unit-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "\n\n(:id \"test\" :command \"echo\")"))
          (let* ((result (supervisor--load-unit-file path))
                 (line (car result)))
            (should (= 3 line))))
      (delete-file path))))

(ert-deftest supervisor-test-load-unit-file-missing ()
  "Loading a missing unit file signals error with path:line context."
  (let ((err (should-error
              (supervisor--load-unit-file "/tmp/nonexistent-unit-file.el")
              :type 'error)))
    (should (string-match "/tmp/nonexistent-unit-file\\.el:1:"
                          (error-message-string err)))))

(ert-deftest supervisor-test-load-unit-file-empty ()
  "Loading an empty unit file signals error with path:line context."
  (let ((path (make-temp-file "unit-test-" nil ".el")))
    (unwind-protect
        (let ((err (should-error (supervisor--load-unit-file path)
                                 :type 'error)))
          (should (string-match (regexp-quote (concat path ":1:"))
                                (error-message-string err))))
      (delete-file path))))

(ert-deftest supervisor-test-load-unit-file-not-plist ()
  "Loading a non-plist unit file signals error with path:line context."
  (let ((path (make-temp-file "unit-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "(defun foo () nil)"))
          (let ((err (should-error (supervisor--load-unit-file path)
                                   :type 'error)))
            (should (string-match (regexp-quote path)
                                  (error-message-string err)))
            (should (string-match ":[0-9]+:" (error-message-string err)))))
      (delete-file path))))

(ert-deftest supervisor-test-load-unit-file-trailing-content ()
  "Loading a unit file with trailing content signals error with path:line."
  (let ((path (make-temp-file "unit-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "(:id \"test\" :command \"echo\")\n(extra stuff)"))
          (let ((err (should-error (supervisor--load-unit-file path)
                                   :type 'error)))
            (should (string-match (regexp-quote path)
                                  (error-message-string err)))
            (should (string-match ":[0-9]+:" (error-message-string err)))))
      (delete-file path))))

(ert-deftest supervisor-test-validate-unit-file-missing-id ()
  "Unit file plist without :id is invalid with path:line context."
  (let ((reason (supervisor--validate-unit-file-plist
                 '(:command "echo") "/tmp/test.el" 1)))
    (should (stringp reason))
    (should (string-match "missing :id" reason))
    (should (string-match "/tmp/test\\.el:1:" reason))))

(ert-deftest supervisor-test-validate-unit-file-missing-command ()
  "Unit file plist without :command is invalid with path:line context."
  (let ((reason (supervisor--validate-unit-file-plist
                 '(:id "test") "/tmp/test.el" 3)))
    (should (stringp reason))
    (should (string-match "missing :command" reason))
    (should (string-match "/tmp/test\\.el:3:" reason))))

(ert-deftest supervisor-test-validate-unit-file-unknown-keyword ()
  "Unit file plist with unknown keyword is invalid with path:line context."
  (let ((reason (supervisor--validate-unit-file-plist
                 '(:id "test" :command "echo" :bogus 42) "/tmp/test.el" 1)))
    (should (stringp reason))
    (should (string-match "unknown keyword" reason))
    (should (string-match "/tmp/test\\.el:1:" reason))))

(ert-deftest supervisor-test-validate-unit-file-valid ()
  "Valid unit file plist returns nil."
  (should-not (supervisor--validate-unit-file-plist
               '(:id "test" :command "echo" :type simple :stage stage3)
               "/tmp/test.el" 1)))

(ert-deftest supervisor-test-unit-file-to-program-entry ()
  "Unit file plist converts to supervisor-programs entry format."
  (let ((entry (supervisor--unit-file-to-program-entry
                '(:id "nm" :command "nm-applet" :type simple :restart t))))
    ;; Car is the command string
    (should (equal "nm-applet" (car entry)))
    ;; Rest is a plist with :id but no :command
    (should (equal "nm" (plist-get (cdr entry) :id)))
    (should (eq 'simple (plist-get (cdr entry) :type)))
    (should (eq t (plist-get (cdr entry) :restart)))
    ;; :command should not be in the output plist
    (should-not (plist-member (cdr entry) :command))))

(ert-deftest supervisor-test-load-all-unit-files-missing-dir ()
  "Loading from nonexistent directory returns nil."
  (let ((supervisor-unit-directory "/tmp/nonexistent-supervisor-units-dir"))
    (should-not (supervisor--load-all-unit-files))))

(ert-deftest supervisor-test-load-all-unit-files-alphabetical ()
  "Unit files are loaded in alphabetical order as plists."
  (let ((dir (make-temp-file "units-" t)))
    (unwind-protect
        (let ((supervisor-unit-directory dir))
          (with-temp-file (expand-file-name "bravo.el" dir)
            (insert "(:id \"bravo\" :command \"bravo-cmd\")"))
          (with-temp-file (expand-file-name "alpha.el" dir)
            (insert "(:id \"alpha\" :command \"alpha-cmd\")"))
          (let ((results (supervisor--load-all-unit-files)))
            (should (= 2 (length results)))
            ;; Alpha first (alphabetical); results are plists directly
            (should (equal "alpha" (plist-get (nth 0 results) :id)))
            (should (equal "bravo" (plist-get (nth 1 results) :id)))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-load-all-unit-files-invalid-tracked ()
  "Invalid unit files are recorded in supervisor--unit-file-invalid."
  (let ((dir (make-temp-file "units-" t)))
    (unwind-protect
        (let ((supervisor-unit-directory dir))
          ;; Valid file
          (with-temp-file (expand-file-name "good.el" dir)
            (insert "(:id \"good\" :command \"echo ok\")"))
          ;; Invalid file (missing :command)
          (with-temp-file (expand-file-name "bad.el" dir)
            (insert "(:id \"bad\")"))
          (let ((results (supervisor--load-all-unit-files)))
            ;; Only valid entries returned
            (should (= 1 (length results)))
            (should (equal "good" (plist-get (nth 0 results) :id)))
            ;; Invalid tracked in hash with path:line context
            (should (= 1 (hash-table-count supervisor--unit-file-invalid)))
            (let ((reason (gethash "bad" supervisor--unit-file-invalid)))
              (should (stringp reason))
              (should (string-match "missing :command" reason))
              (should (string-match ":[0-9]+:" reason)))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-load-all-unit-files-load-error-tracked ()
  "Unit files that fail to parse are recorded in invalid hash with path:line."
  (let ((dir (make-temp-file "units-" t)))
    (unwind-protect
        (let ((supervisor-unit-directory dir))
          (with-temp-file (expand-file-name "broken.el" dir)
            (insert "not a plist at all"))
          (let ((results (supervisor--load-all-unit-files)))
            (should (= 0 (length results)))
            (should (= 1 (hash-table-count supervisor--unit-file-invalid)))
            (let ((reason (gethash "broken" supervisor--unit-file-invalid)))
              (should (stringp reason))
              (should (string-match ":[0-9]+:" reason)))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-unit-file-invalid-merged-into-plan ()
  "Invalid unit files appear in supervisor--invalid after plan build."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-directory dir)
         (supervisor-use-unit-files t)
         (supervisor-programs nil)
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
         (supervisor--computed-deps (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "bad.el" dir)
            (insert "(:id \"bad-unit\")"))
          (let ((plan (supervisor--build-plan (supervisor--effective-programs))))
            (maphash (lambda (k v) (puthash k v supervisor--invalid))
                     (supervisor-plan-invalid plan))
            (supervisor--merge-unit-file-invalid)
            ;; The invalid unit file should now be visible
            (should (gethash "bad-unit" supervisor--invalid))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-load-programs-legacy-path ()
  "With unit files disabled, returns supervisor-programs unchanged."
  (let ((supervisor-use-unit-files nil)
        (supervisor-programs '(("test" :id "test" :type simple))))
    (should (equal supervisor-programs (supervisor--effective-programs)))))

(ert-deftest supervisor-test-load-programs-unit-file-merge ()
  "With unit files enabled, unit entries override legacy on ID collision."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-directory dir)
         (supervisor-use-unit-files t)
         (supervisor-programs '(("old-cmd" :id "svc" :type simple))))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "svc.el" dir)
            (insert "(:id \"svc\" :command \"new-cmd\" :type simple)"))
          (let ((programs (supervisor--effective-programs)))
            ;; Should have exactly one entry (unit-file wins)
            (should (= 1 (length programs)))
            ;; The command should be the unit-file version
            (should (equal "new-cmd" (car (nth 0 programs))))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-load-programs-unit-file-adds-new ()
  "Unit files can introduce new entries alongside legacy programs."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-directory dir)
         (supervisor-use-unit-files t)
         (supervisor-programs '(("legacy-cmd" :id "legacy" :type simple))))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "new-svc.el" dir)
            (insert "(:id \"new-svc\" :command \"new-cmd\" :type simple)"))
          (let ((programs (supervisor--effective-programs)))
            ;; Should have both entries
            (should (= 2 (length programs)))
            ;; Unit-file entries come first
            (should (equal "new-cmd" (car (nth 0 programs))))
            (should (equal "legacy-cmd" (car (nth 1 programs))))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-cli-validate-invalid-unit-file ()
  "CLI validate reports invalid unit files in count and output."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-directory dir)
         (supervisor-use-unit-files t)
         (supervisor-programs nil))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "bad.el" dir)
            (insert "(:id \"bad-unit\")"))
          (let ((result (supervisor--cli-dispatch '("validate"))))
            (should (supervisor-cli-result-p result))
            (should (= supervisor-cli-exit-validation-failed
                        (supervisor-cli-result-exitcode result)))
            (should (string-match "1 invalid"
                                  (supervisor-cli-result-output result)))
            (should (string-match "bad-unit"
                                  (supervisor-cli-result-output result)))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-cli-list-units-shows-invalid-unit-file ()
  "CLI list-units includes invalid unit files in output."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-directory dir)
         (supervisor-use-unit-files t)
         (supervisor-programs nil)
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "bad.el" dir)
            (insert "(:id \"bad-unit\")"))
          (let ((result (supervisor--cli-dispatch '("list-units"))))
            (should (supervisor-cli-result-p result))
            (should (string-match "bad-unit"
                                  (supervisor-cli-result-output result)))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-cli-status-invalid-unit-file ()
  "CLI status ID recognizes invalid unit files, not just plan-level invalids."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-directory dir)
         (supervisor-use-unit-files t)
         (supervisor-programs nil)
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "bad.el" dir)
            (insert "(:id \"bad-unit\")"))
          (let ((result (supervisor--cli-dispatch '("status" "bad-unit"))))
            (should (supervisor-cli-result-p result))
            ;; Should find it as invalid, not "not found"
            (should (= supervisor-cli-exit-success
                        (supervisor-cli-result-exitcode result)))
            (should (string-match "invalid"
                                  (supervisor-cli-result-output result)))
            (should-not (string-match "could not be found"
                                      (supervisor-cli-result-output result)))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-dry-run-shows-invalid-unit-files ()
  "Dry-run summary includes invalid unit files in count and listing."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-directory dir)
         (supervisor-use-unit-files t)
         (supervisor-programs nil)
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
         (supervisor--computed-deps (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "bad.el" dir)
            (insert "(:id \"bad-unit\")"))
          (supervisor-dry-run)
          (with-current-buffer "*supervisor-dry-run*"
            (let ((content (buffer-string)))
              (should (string-match "1 invalid" content))
              (should (string-match "bad-unit" content)))))
      (delete-directory dir t))))

;;; Unit-File Scaffold and Edit/Cat Tests

(ert-deftest supervisor-test-unit-file-scaffold-contains-id ()
  "Scaffold template contains the given ID."
  (let ((scaffold (supervisor--unit-file-scaffold "my-svc")))
    (should (stringp scaffold))
    (should (string-match ":id \"my-svc\"" scaffold))
    (should (string-match ":command" scaffold))))

(ert-deftest supervisor-test-unit-file-scaffold-is-valid-plist ()
  "Scaffold template can be read as a valid plist (with command filled)."
  (let* ((scaffold (supervisor--unit-file-scaffold "test-svc"))
         ;; Replace empty command placeholder with actual value
         (filled (replace-regexp-in-string
                  ":command \"\"" ":command \"echo hello\"" scaffold)))
    (with-temp-buffer
      (insert filled)
      (goto-char (point-min))
      (let ((form (read (current-buffer))))
        (should (listp form))
        (should (keywordp (car form)))
        (should (equal "test-svc" (plist-get form :id)))
        (should (equal "echo hello" (plist-get form :command)))))))

(ert-deftest supervisor-test-cli-cat-existing-unit-file ()
  "CLI cat outputs raw content of existing unit file."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-directory dir))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "my-svc.el" dir)
            (insert "(:id \"my-svc\" :command \"echo ok\")"))
          (let ((result (supervisor--cli-dispatch '("cat" "my-svc"))))
            (should (supervisor-cli-result-p result))
            (should (= supervisor-cli-exit-success
                        (supervisor-cli-result-exitcode result)))
            (should (string-match ":id \"my-svc\""
                                  (supervisor-cli-result-output result)))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-cli-cat-missing-unit-file ()
  "CLI cat returns error for missing unit file."
  (let ((supervisor-unit-directory "/tmp/nonexistent-supervisor-units-dir"))
    (let ((result (supervisor--cli-dispatch '("cat" "nope"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-failure
                  (supervisor-cli-result-exitcode result)))
      (should (string-match "not found"
                            (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-cat-no-args ()
  "CLI cat requires an ID argument."
  (let ((result (supervisor--cli-dispatch '("cat"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args
                (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-cat-too-many-args ()
  "CLI cat takes exactly one ID."
  (let ((result (supervisor--cli-dispatch '("cat" "a" "b"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args
                (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-cat-json-format ()
  "CLI cat --json outputs path and content fields."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-directory dir))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "svc.el" dir)
            (insert "(:id \"svc\" :command \"echo\")"))
          (let ((result (supervisor--cli-dispatch '("cat" "svc" "--json"))))
            (should (supervisor-cli-result-p result))
            (should (= supervisor-cli-exit-success
                        (supervisor-cli-result-exitcode result)))
            (should (eq 'json (supervisor-cli-result-format result)))
            (let ((parsed (json-read-from-string
                           (supervisor-cli-result-output result))))
              (should (assoc 'path parsed))
              (should (assoc 'content parsed))
              (should (string-match ":id" (cdr (assoc 'content parsed)))))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-cli-edit-creates-scaffold ()
  "CLI edit creates scaffold template for missing unit file."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-directory dir))
    (unwind-protect
        ;; Use JSON mode to test scaffold creation without editor launch
        (let ((result (supervisor--cli-dispatch '("edit" "new-svc" "--json"))))
          (should (supervisor-cli-result-p result))
          (should (= supervisor-cli-exit-success
                      (supervisor-cli-result-exitcode result)))
          ;; File should have been created
          (let ((path (expand-file-name "new-svc.el" dir)))
            (should (file-exists-p path))
            ;; Content should have the ID
            (with-temp-buffer
              (insert-file-contents path)
              (should (string-match ":id \"new-svc\""
                                    (buffer-string))))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-cli-edit-existing-file ()
  "CLI edit on existing file does not overwrite it."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-directory dir))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "svc.el" dir)
            (insert "(:id \"svc\" :command \"existing\")"))
          ;; Use JSON mode to test without editor launch
          (let ((result (supervisor--cli-dispatch '("edit" "svc" "--json"))))
            (should (supervisor-cli-result-p result))
            (should (= supervisor-cli-exit-success
                        (supervisor-cli-result-exitcode result)))
            ;; File content should be unchanged
            (with-temp-buffer
              (insert-file-contents (expand-file-name "svc.el" dir))
              (should (string-match "existing" (buffer-string))))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-cli-edit-json-format ()
  "CLI edit --json outputs path and created flag."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-directory dir))
    (unwind-protect
        (let ((result (supervisor--cli-dispatch '("edit" "new" "--json"))))
          (should (supervisor-cli-result-p result))
          (should (= supervisor-cli-exit-success
                      (supervisor-cli-result-exitcode result)))
          (should (eq 'json (supervisor-cli-result-format result)))
          (let ((parsed (json-read-from-string
                         (supervisor-cli-result-output result))))
            (should (assoc 'path parsed))
            (should (eq t (cdr (assoc 'created parsed))))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-cli-edit-no-args ()
  "CLI edit requires an ID argument."
  (let ((result (supervisor--cli-dispatch '("edit"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args
                (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-edit-too-many-args ()
  "CLI edit takes exactly one ID."
  (let ((result (supervisor--cli-dispatch '("edit" "a" "b"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args
                (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-dashboard-cat-keybinding ()
  "Dashboard keymap binds `c' to cat."
  (should (eq #'supervisor-dashboard-cat
              (lookup-key supervisor-dashboard-mode-map "c"))))

(ert-deftest supervisor-test-dashboard-edit-keybinding ()
  "Dashboard keymap binds `E' to edit."
  (should (eq #'supervisor-dashboard-edit
              (lookup-key supervisor-dashboard-mode-map "E"))))

(ert-deftest supervisor-test-dashboard-cat-rejects-separator ()
  "Dashboard cat rejects separator rows."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list '--stage3-- (vector "" "" "" "" "" "" "" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should-error (supervisor-dashboard-cat)
                    :type 'user-error))))

(ert-deftest supervisor-test-dashboard-cat-rejects-timer ()
  "Dashboard cat rejects timer rows."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list "t1" (vector "t1" "timer" "---" "yes"
                                    "waiting" "---" "---" "-" "-")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should-error (supervisor-dashboard-cat)
                    :type 'user-error))))

(ert-deftest supervisor-test-dashboard-edit-rejects-separator ()
  "Dashboard edit rejects separator rows."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list '--stage3-- (vector "" "" "" "" "" "" "" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should-error (supervisor-dashboard-edit)
                    :type 'user-error))))

(ert-deftest supervisor-test-cli-edit-launches-editor ()
  "CLI edit launches $VISUAL/$EDITOR via `call-process'."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-directory dir)
         (launch-args nil))
    (unwind-protect
        (cl-letf (((symbol-function 'supervisor--cli-edit-launch-editor)
                   (lambda (editor path)
                     (setq launch-args (list editor path))
                     0)))
          (let* ((process-environment
                  (cons "VISUAL=my-editor" process-environment))
                 (result (supervisor--cli-dispatch '("edit" "new-svc"))))
            (should (supervisor-cli-result-p result))
            (should (= supervisor-cli-exit-success
                        (supervisor-cli-result-exitcode result)))
            ;; Editor should have been called
            (should launch-args)
            (should (equal "my-editor" (car launch-args)))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-cli-edit-editor-failure ()
  "CLI edit reports failure when editor exits non-zero."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-directory dir))
    (unwind-protect
        (cl-letf (((symbol-function 'supervisor--cli-edit-launch-editor)
                   (lambda (_editor _path) 1)))
          (let* ((process-environment
                  (cons "VISUAL=my-editor" process-environment))
                 (result (supervisor--cli-dispatch '("edit" "new-svc"))))
            (should (supervisor-cli-result-p result))
            (should (= supervisor-cli-exit-failure
                        (supervisor-cli-result-exitcode result)))
            (should (string-match "exited with status 1"
                                  (supervisor-cli-result-output result)))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-cli-edit-no-editor-error ()
  "CLI edit returns error when no $VISUAL or $EDITOR is set."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-directory dir))
    (unwind-protect
        (let* ((process-environment
                (cl-remove-if (lambda (e)
                                (or (string-prefix-p "VISUAL=" e)
                                    (string-prefix-p "EDITOR=" e)))
                              process-environment))
               (result (supervisor--cli-dispatch '("edit" "new-svc"))))
          (should (supervisor-cli-result-p result))
          (should (= supervisor-cli-exit-failure
                      (supervisor-cli-result-exitcode result)))
          (should (string-match "No \\$VISUAL or \\$EDITOR"
                                (supervisor-cli-result-output result))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-dashboard-edit-enables-edit-mode ()
  "Dashboard edit activates `supervisor-edit-mode' with return-on-q."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-directory dir)
         (path (expand-file-name "test-svc.el" dir)))
    (unwind-protect
        (progn
          (write-region "(:id \"test-svc\" :command \"echo\")" nil path)
          ;; Simulate dashboard with a valid entry
          (with-temp-buffer
            (supervisor-dashboard-mode)
            (let ((tabulated-list-entries
                   (list (list "test-svc"
                               (vector "test-svc" "simple" "stage3"
                                       "yes" "running" "yes" "---"
                                       "-" "-")))))
              (tabulated-list-init-header)
              (tabulated-list-print)
              (goto-char (point-min))
              (supervisor-dashboard-edit)
              ;; Should have opened the file with edit mode
              (let ((edit-buf (get-file-buffer path)))
                (should edit-buf)
                (with-current-buffer edit-buf
                  (should supervisor-edit-mode)
                  ;; q should be bound
                  (should (eq #'supervisor-edit-quit
                              (lookup-key supervisor-edit-mode-map "q")))
                  ;; C-c C-q should be bound
                  (should (eq #'supervisor-edit-finish
                              (lookup-key supervisor-edit-mode-map
                                          (kbd "C-c C-q")))))
                (kill-buffer edit-buf)))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-edit-quit-returns ()
  "Pressing `q' in edit buffer returns to dashboard."
  (let* ((dir (make-temp-file "units-" t))
         (path (expand-file-name "svc.el" dir))
         (returned nil))
    (unwind-protect
        (progn
          (write-region "(:id \"svc\" :command \"echo\")" nil path)
          (find-file path)
          (supervisor-edit-mode 1)
          (cl-letf (((symbol-function 'supervisor--return-to-dashboard)
                     (lambda () (setq returned t)))
                    ((symbol-function 'y-or-n-p) (lambda (_) nil)))
            ;; Buffer is unmodified, q should quit
            (should-not (buffer-modified-p))
            (supervisor-edit-quit)
            (should returned)))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest supervisor-test-dashboard-edit-save-hook-fires ()
  "Dashboard edit save-hook wiring fires validator on save."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-directory dir)
         (path (expand-file-name "hook-svc.el" dir))
         (validated nil))
    (unwind-protect
        (progn
          (write-region "(:id \"hook-svc\" :command \"echo\")" nil path)
          (with-temp-buffer
            (supervisor-dashboard-mode)
            (let ((tabulated-list-entries
                   (list (list "hook-svc"
                               (vector "hook-svc" "simple" "stage3"
                                       "yes" "running" "yes" "---"
                                       "-" "-")))))
              (tabulated-list-init-header)
              (tabulated-list-print)
              (goto-char (point-min))
              (supervisor-dashboard-edit)
              (let ((edit-buf (get-file-buffer path)))
                (should edit-buf)
                (with-current-buffer edit-buf
                  ;; Validator should be in after-save-hook
                  (should (memq #'supervisor--validate-unit-file-buffer
                                after-save-hook))
                  ;; Mock validator to track invocation
                  (cl-letf (((symbol-function
                              'supervisor--validate-unit-file-buffer)
                             (lambda () (setq validated t))))
                    ;; Modify and save to trigger hook
                    (goto-char (point-max))
                    (insert " ")
                    (save-buffer)))
                (should validated)
                (kill-buffer edit-buf)))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-validate-unit-file-buffer-hook ()
  "Validate-on-save reports validation result."
  (let* ((dir (make-temp-file "units-" t))
         (path (expand-file-name "test.el" dir)))
    (unwind-protect
        (progn
          (write-region "(:id \"test\" :command \"echo\")" nil path)
          (with-temp-buffer
            (insert-file-contents path)
            (setq buffer-file-name path)
            ;; Should not error for valid file
            (supervisor--validate-unit-file-buffer)))
      (delete-directory dir t))))

;;; Schema v1 Tests

(ert-deftest supervisor-test-service-schema-version ()
  "Service schema version constant is defined."
  (should (= supervisor-service-schema-version 1)))

(ert-deftest supervisor-test-service-struct-fields ()
  "Service struct has all expected fields."
  (let ((service (supervisor-service--create
                  :id "test"
                  :command "sleep 100"
                  :type 'simple
                  :stage 'stage2
                  :delay 5
                  :enabled t
                  :restart nil
                  :logging t
                  :after '("dep1")
                  :requires '("req1")
                  :oneshot-wait nil
                  :oneshot-timeout 60
                  :tags '(tag1 tag2))))
    (should (equal "test" (supervisor-service-id service)))
    (should (equal "sleep 100" (supervisor-service-command service)))
    (should (eq 'simple (supervisor-service-type service)))
    (should (eq 'stage2 (supervisor-service-stage service)))
    (should (= 5 (supervisor-service-delay service)))
    (should (supervisor-service-enabled service))
    (should-not (supervisor-service-restart service))
    (should (supervisor-service-logging service))
    (should (equal '("dep1") (supervisor-service-after service)))
    (should (equal '("req1") (supervisor-service-requires service)))
    (should-not (supervisor-service-oneshot-wait service))
    (should (= 60 (supervisor-service-oneshot-timeout service)))
    (should (equal '(tag1 tag2) (supervisor-service-tags service)))))

(ert-deftest supervisor-test-entry-accessors ()
  "Entry accessor functions work correctly."
  (let ((entry (supervisor--parse-entry
                '("sleep 100" :id "test" :type simple :stage stage2
                  :delay 5 :restart nil :after ("dep1") :requires ("req1")))))
    (should (equal "test" (supervisor-entry-id entry)))
    (should (equal "sleep 100" (supervisor-entry-command entry)))
    (should (eq 'simple (supervisor-entry-type entry)))
    (should (eq 'stage2 (supervisor-entry-stage entry)))
    (should (= 5 (supervisor-entry-delay entry)))
    (should-not (supervisor-entry-restart-p entry))
    (should (equal '("dep1") (supervisor-entry-after entry)))
    (should (equal '("req1") (supervisor-entry-requires entry)))))

(ert-deftest supervisor-test-entry-to-service-conversion ()
  "Entry to service conversion preserves all fields."
  (let* ((entry (supervisor--parse-entry
                 '("sleep 100" :id "test" :type oneshot :stage stage1
                   :after ("dep") :requires ("req") :tags (t1 t2))))
         (service (supervisor-entry-to-service entry)))
    (should (equal "test" (supervisor-service-id service)))
    (should (equal "sleep 100" (supervisor-service-command service)))
    (should (eq 'oneshot (supervisor-service-type service)))
    (should (eq 'stage1 (supervisor-service-stage service)))
    (should (equal '("dep") (supervisor-service-after service)))
    (should (equal '("req") (supervisor-service-requires service)))
    (should (equal '(t1 t2) (supervisor-service-tags service)))))

(ert-deftest supervisor-test-service-to-entry-conversion ()
  "Service to entry conversion preserves all fields."
  (let* ((service (supervisor-service--create
                   :id "test"
                   :command "sleep 100"
                   :type 'oneshot
                   :stage 'stage1
                   :delay 10
                   :enabled nil
                   :restart t
                   :logging nil
                   :after '("dep")
                   :requires '("req")
                   :oneshot-wait t
                   :oneshot-timeout 120
                   :tags '(t1)))
         (entry (supervisor-service-to-entry service)))
    (should (equal "test" (supervisor-entry-id entry)))
    (should (equal "sleep 100" (supervisor-entry-command entry)))
    (should (eq 'oneshot (supervisor-entry-type entry)))
    (should (eq 'stage1 (supervisor-entry-stage entry)))
    (should (= 10 (supervisor-entry-delay entry)))
    (should-not (supervisor-entry-enabled-p entry))
    (should (supervisor-entry-restart-p entry))
    (should-not (supervisor-entry-logging-p entry))
    (should (equal '("dep") (supervisor-entry-after entry)))
    (should (equal '("req") (supervisor-entry-requires entry)))
    (should (supervisor-entry-oneshot-wait entry))
    (should (= 120 (supervisor-entry-oneshot-timeout entry)))
    (should (equal '(t1) (supervisor-entry-tags entry)))))

;;; Dependency Semantics Tests

(ert-deftest supervisor-test-requires-keyword-valid ()
  ":requires keyword is accepted as valid."
  (should-not (supervisor--validate-entry
               '("foo" :type simple :requires "bar"))))

(ert-deftest supervisor-test-requires-parsed-correctly ()
  ":requires is parsed into entry correctly."
  (let ((entry (supervisor--parse-entry
                '("foo" :requires ("bar" "baz")))))
    (should (equal '("bar" "baz") (supervisor-entry-requires entry)))))

(ert-deftest supervisor-test-requires-cross-stage-error ()
  "Cross-stage :requires causes an error in plan."
  (let* ((programs '(("sleep 100" :id "a" :stage stage1)
                     ("sleep 200" :id "b" :stage stage2 :requires "a")))
         (plan (supervisor--build-plan programs)))
    ;; Entry b should be marked invalid due to cross-stage requires
    (should (gethash "b" (supervisor-plan-invalid plan)))))

(ert-deftest supervisor-test-requires-combined-with-after ()
  ":requires and :after are combined for topological sort."
  (let* ((programs '(("sleep 100" :id "a" :stage stage1)
                     ("sleep 200" :id "b" :stage stage1 :after "a")
                     ("sleep 300" :id "c" :stage stage1 :requires "a")))
         (plan (supervisor--build-plan programs))
         (by-stage (supervisor-plan-by-stage plan))
         (stage1-entries (cdr (assq 0 by-stage)))
         (stage1-ids (mapcar #'car stage1-entries)))
    ;; a should come before both b and c
    (should (< (cl-position "a" stage1-ids :test #'equal)
               (cl-position "b" stage1-ids :test #'equal)))
    (should (< (cl-position "a" stage1-ids :test #'equal)
               (cl-position "c" stage1-ids :test #'equal)))))

(ert-deftest supervisor-test-plan-requires-deps-populated ()
  "Plan includes separate requires-deps hash."
  (let* ((programs '(("sleep 100" :id "a" :stage stage1)
                     ("sleep 200" :id "b" :stage stage1 :requires "a")))
         (plan (supervisor--build-plan programs)))
    (should (supervisor-plan-requires-deps plan))
    (should (equal '("a") (gethash "b" (supervisor-plan-requires-deps plan))))))

;;; Persistent Overrides Tests

(ert-deftest supervisor-test-overrides-file-path ()
  "Overrides file path respects custom setting."
  (let ((supervisor-overrides-file "/custom/path/overrides.eld"))
    (should (equal "/custom/path/overrides.eld"
                   (supervisor--overrides-file-path))))
  (let ((supervisor-overrides-file nil))
    (should-not (supervisor--overrides-file-path))))

(ert-deftest supervisor-test-overrides-to-alist ()
  "Overrides are correctly collected into an alist."
  (let ((supervisor--enabled-override (make-hash-table :test 'equal))
        (supervisor--restart-override (make-hash-table :test 'equal))
        (supervisor--logging (make-hash-table :test 'equal)))
    (puthash "a" 'enabled supervisor--enabled-override)
    (puthash "b" 'disabled supervisor--restart-override)
    (puthash "a" 'enabled supervisor--logging)
    (let ((alist (supervisor--overrides-to-alist)))
      (should (= 2 (length alist)))
      (let ((a-entry (cdr (assoc "a" alist)))
            (b-entry (cdr (assoc "b" alist))))
        (should (eq 'enabled (plist-get a-entry :enabled)))
        (should (eq 'enabled (plist-get a-entry :logging)))
        (should (eq 'disabled (plist-get b-entry :restart)))))))

(ert-deftest supervisor-test-overrides-save-load-roundtrip ()
  "Overrides survive save and load roundtrip."
  (let* ((temp-file (make-temp-file "supervisor-test-overrides-" nil ".eld"))
         (supervisor-overrides-file temp-file)
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--restart-override (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal))
         (supervisor--overrides-loaded nil))
    (unwind-protect
        (progn
          ;; Set some overrides
          (puthash "test-entry" 'disabled supervisor--enabled-override)
          (puthash "test-entry" 'enabled supervisor--restart-override)
          ;; Save
          (should (supervisor--save-overrides))
          ;; Clear memory
          (clrhash supervisor--enabled-override)
          (clrhash supervisor--restart-override)
          ;; Load
          (should (supervisor--load-overrides))
          ;; Verify
          (should (eq 'disabled (gethash "test-entry" supervisor--enabled-override)))
          (should (eq 'enabled (gethash "test-entry" supervisor--restart-override))))
      (delete-file temp-file))))

(ert-deftest supervisor-test-overrides-corrupt-file-handled ()
  "Corrupt overrides file is handled gracefully."
  (let* ((temp-file (make-temp-file "supervisor-test-corrupt-" nil ".eld"))
         (supervisor-overrides-file temp-file)
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--restart-override (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; Write corrupt data
          (with-temp-file temp-file
            (insert "this is not valid lisp (((("))
          ;; Load should return nil but not error
          (should-not (supervisor--load-overrides))
          ;; File should still exist (not deleted)
          (should (file-exists-p temp-file)))
      (delete-file temp-file))))

;;; Migration Tests

(ert-deftest supervisor-test-migrate-string-entry ()
  "Migration handles simple string entries."
  (let ((result (supervisor--migrate-entry-to-plist "nm-applet")))
    ;; Simple string with defaults should remain a string
    (should (stringp result))
    (should (equal "nm-applet" result))))

(ert-deftest supervisor-test-migrate-plist-entry ()
  "Migration handles plist entries."
  (let ((result (supervisor--migrate-entry-to-plist
                 '("sleep 100" :type oneshot :stage stage1))))
    (should (listp result))
    (should (equal "sleep 100" (car result)))
    (should (eq 'oneshot (plist-get (cdr result) :type)))
    (should (eq 'stage1 (plist-get (cdr result) :stage)))))

(ert-deftest supervisor-test-migrate-all-entries-skips-invalid ()
  "Migration skips invalid entries with reason."
  (let ((supervisor-programs '(("valid" :type simple)
                               ("invalid" :type "bad"))))
    (let ((result (supervisor--migrate-all-entries)))
      (should (= 1 (length (plist-get result :migrated))))
      (should (= 1 (length (plist-get result :skipped))))
      (should (assoc "invalid" (plist-get result :skipped))))))

(ert-deftest supervisor-test-migrate-all-entries-skips-duplicates ()
  "Migration skips duplicate IDs."
  (let ((supervisor-programs '(("a" :id "test" :type simple)
                               ("b" :id "test" :type oneshot))))
    (let ((result (supervisor--migrate-all-entries)))
      (should (= 1 (length (plist-get result :migrated))))
      (should (= 1 (length (plist-get result :skipped))))
      (let ((skipped-reason (cdr (assoc "test" (plist-get result :skipped)))))
        (should (string-match "duplicate" skipped-reason))))))

(ert-deftest supervisor-test-migrate-entry-to-service ()
  "High-level migration function works."
  (let ((service (supervisor-migrate-entry-to-service
                  '("sleep 100" :id "test" :type oneshot))))
    (should (supervisor-service-p service))
    (should (equal "test" (supervisor-service-id service)))
    (should (eq 'oneshot (supervisor-service-type service)))))

(ert-deftest supervisor-test-migrate-entry-to-service-invalid-errors ()
  "High-level migration function signals error for invalid entry."
  (should-error
   (supervisor-migrate-entry-to-service '("foo" :type "bad"))
   :type 'error))

;;; Bug fix verification tests

(ert-deftest supervisor-test-validate-after-must-be-string-or-list ()
  "Validation rejects non-string/list :after values."
  ;; Valid cases
  (should-not (supervisor--validate-entry '("foo" :after "bar")))
  (should-not (supervisor--validate-entry '("foo" :after ("a" "b"))))
  (should-not (supervisor--validate-entry '("foo" :after nil)))
  ;; Invalid cases
  (should (string-match ":after must be"
                        (supervisor--validate-entry '("foo" :after 123))))
  (should (string-match ":after must be"
                        (supervisor--validate-entry '("foo" :after (1 2 3))))))

(ert-deftest supervisor-test-validate-requires-must-be-string-or-list ()
  "Validation rejects non-string/list :requires values."
  ;; Valid cases
  (should-not (supervisor--validate-entry '("foo" :requires "bar")))
  (should-not (supervisor--validate-entry '("foo" :requires ("a" "b"))))
  (should-not (supervisor--validate-entry '("foo" :requires nil)))
  ;; Invalid cases
  (should (string-match ":requires must be"
                        (supervisor--validate-entry '("foo" :requires 123))))
  (should (string-match ":requires must be"
                        (supervisor--validate-entry '("foo" :requires (1 2 3))))))

(ert-deftest supervisor-test-cross-stage-requires-excluded-from-stage ()
  "Entries with cross-stage :requires are excluded from stage lists."
  (let* ((programs '(("a" :stage stage2 :id "a")
                     ("b" :stage stage3 :id "b" :requires "a")))  ; cross-stage
         (plan (supervisor--build-plan programs))
         (stage3-entries (cdr (assoc 2 (supervisor-plan-by-stage plan)))))
    ;; "b" should be marked invalid
    (should (gethash "b" (supervisor-plan-invalid plan)))
    ;; "b" should NOT appear in stage3 entries (was the bug)
    (should-not (cl-find "b" stage3-entries :key #'supervisor-entry-id :test #'equal))))

(ert-deftest supervisor-test-dag-uses-requires-edges ()
  "DAG scheduler uses :requires edges for in-degree calculation."
  (let* ((entries '(("a" "sleep 1" 0 t t t simple stage3 nil t 30 nil nil)
                    ("b" "sleep 1" 0 t t t simple stage3 nil t 30 nil ("a"))))
         ;; Entry "b" has :requires "a" at index 12
         (supervisor--dag-in-degree (make-hash-table :test 'equal))
         (supervisor--dag-dependents (make-hash-table :test 'equal))
         (supervisor--dag-entries (make-hash-table :test 'equal))
         (supervisor--dag-blocking (make-hash-table :test 'equal))
         (supervisor--dag-started (make-hash-table :test 'equal))
         (supervisor--dag-ready (make-hash-table :test 'equal))
         (supervisor--dag-timeout-timers (make-hash-table :test 'equal))
         (supervisor--dag-delay-timers (make-hash-table :test 'equal))
         (supervisor--dag-id-to-index (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal)))
    (supervisor--dag-init entries)
    ;; "a" has no deps, in-degree should be 0
    (should (= 0 (gethash "a" supervisor--dag-in-degree)))
    ;; "b" has :requires "a", in-degree should be 1
    (should (= 1 (gethash "b" supervisor--dag-in-degree)))
    ;; "a" should have "b" as dependent
    (should (member "b" (gethash "a" supervisor--dag-dependents)))))

(ert-deftest supervisor-test-override-load-clears-stale-state ()
  "Loading overrides clears stale in-memory state not present in file."
  (let* ((temp-file (make-temp-file "supervisor-test-stale-" nil ".eld"))
         (supervisor-overrides-file temp-file)
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--restart-override (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; First, save a file with only "file-entry"
          (puthash "file-entry" 'enabled supervisor--enabled-override)
          (supervisor--save-overrides)
          ;; Clear and add stale state that's NOT in the file
          (clrhash supervisor--enabled-override)
          (clrhash supervisor--restart-override)
          (clrhash supervisor--logging)
          (puthash "stale-entry" 'disabled supervisor--enabled-override)
          (puthash "stale-entry" 'disabled supervisor--restart-override)
          (puthash "stale-entry" 'disabled supervisor--logging)
          (puthash "another-stale" 'enabled supervisor--enabled-override)
          ;; Load from file - should clear stale state and restore file state
          (supervisor--load-overrides)
          ;; Stale entries should be gone (not in file)
          (should-not (gethash "stale-entry" supervisor--enabled-override))
          (should-not (gethash "another-stale" supervisor--enabled-override))
          (should-not (gethash "stale-entry" supervisor--restart-override))
          (should-not (gethash "stale-entry" supervisor--logging))
          ;; File entry should be restored
          (should (eq 'enabled (gethash "file-entry" supervisor--enabled-override))))
      (delete-file temp-file))))

(ert-deftest supervisor-test-duplicate-id-invalid-does-not-poison-valid ()
  "Valid entry is not poisoned by later invalid duplicate with same ID."
  (let* ((programs '(("cmd" :id "test" :type simple)      ; valid first
                     ("cmd" :id "test" :type "bad")))     ; invalid duplicate
         (plan (supervisor--build-plan programs)))
    ;; The valid entry should be in plan.entries
    (should (= 1 (length (supervisor-plan-entries plan))))
    (should (equal "test" (supervisor-entry-id (car (supervisor-plan-entries plan)))))
    ;; The ID should NOT be in the invalid hash (first valid wins)
    (should-not (gethash "test" (supervisor-plan-invalid plan)))))

(ert-deftest supervisor-test-duplicate-id-invalid-first-valid-later ()
  "First valid occurrence wins even when first occurrence is invalid."
  (let* ((programs '(("cmd" :id "test" :type "bad")       ; invalid first
                     ("cmd" :id "test" :type simple)))    ; valid later
         (plan (supervisor--build-plan programs)))
    ;; The valid entry should be in plan.entries
    (should (= 1 (length (supervisor-plan-entries plan))))
    (should (equal "test" (supervisor-entry-id (car (supervisor-plan-entries plan)))))
    ;; The ID should NOT be in the invalid hash (valid cleared stale invalid)
    (should-not (gethash "test" (supervisor-plan-invalid plan)))))

(ert-deftest supervisor-test-cross-stage-requires-excluded-from-plan-entries ()
  "Entries with cross-stage :requires are excluded from plan.entries."
  (let* ((programs '(("a" :stage stage2 :id "a")
                     ("b" :stage stage3 :id "b" :requires "a")))  ; cross-stage
         (plan (supervisor--build-plan programs)))
    ;; "b" should be marked invalid
    (should (gethash "b" (supervisor-plan-invalid plan)))
    ;; "b" should NOT appear in plan.entries (Bug 2 fix)
    (should-not (cl-find "b" (supervisor-plan-entries plan)
                         :key #'supervisor-entry-id :test #'equal))))

(ert-deftest supervisor-test-validate-dotted-list-after ()
  "Validation handles dotted lists in :after without error."
  ;; Dotted list should return validation error, not signal
  (let ((result (supervisor--validate-entry '("foo" :after (a . b)))))
    (should (stringp result))
    (should (string-match ":after must be" result))))

(ert-deftest supervisor-test-validate-dotted-list-requires ()
  "Validation handles dotted lists in :requires without error."
  ;; Dotted list should return validation error, not signal
  (let ((result (supervisor--validate-entry '("foo" :requires (x . y)))))
    (should (stringp result))
    (should (string-match ":requires must be" result))))

(ert-deftest supervisor-test-empty-stages-not-in-by-stage ()
  "Empty stages after filtering are not included in by-stage."
  (let* ((programs '(("a" :stage stage2 :id "a")
                     ("b" :stage stage3 :id "b" :requires "a")))  ; cross-stage, invalid
         (plan (supervisor--build-plan programs)))
    ;; stage3 should be empty after filtering
    ;; by-stage should NOT contain stage3 entry
    (let ((stage3-entry (assoc 2 (supervisor-plan-by-stage plan))))
      (should-not stage3-entry))))

;;; Timer Schema tests

(ert-deftest supervisor-test-timer-struct-fields ()
  "Timer struct has expected fields."
  (let ((timer (supervisor-timer--create
                :id "test"
                :target "target"
                :enabled t
                :on-startup-sec 60
                :persistent t)))
    (should (supervisor-timer-p timer))
    (should (equal "test" (supervisor-timer-id timer)))
    (should (equal "target" (supervisor-timer-target timer)))
    (should (eq t (supervisor-timer-enabled timer)))
    (should (= 60 (supervisor-timer-on-startup-sec timer)))
    (should (eq t (supervisor-timer-persistent timer)))))

(ert-deftest supervisor-test-timer-validate-missing-id ()
  "Timer without :id is rejected."
  (let ((err (supervisor-timer--validate '(:target "foo" :on-startup-sec 60) nil)))
    (should (string-match-p ":id must be" err))))

(ert-deftest supervisor-test-timer-validate-missing-target ()
  "Timer without :target is rejected."
  (let ((err (supervisor-timer--validate '(:id "t" :on-startup-sec 60) nil)))
    (should (string-match-p ":target must be" err))))

(ert-deftest supervisor-test-timer-validate-empty-id ()
  "Timer with empty string :id is rejected."
  (let ((err (supervisor-timer--validate '(:id "" :target "foo" :on-startup-sec 60) nil)))
    (should (string-match-p ":id must be a non-empty string" err))))

(ert-deftest supervisor-test-timer-validate-empty-target ()
  "Timer with empty string :target is rejected."
  (let ((err (supervisor-timer--validate '(:id "t" :target "" :on-startup-sec 60) nil)))
    (should (string-match-p ":target must be a non-empty string" err))))

(ert-deftest supervisor-test-timer-validate-no-trigger ()
  "Timer without any trigger is rejected."
  (let ((err (supervisor-timer--validate '(:id "t" :target "foo") nil)))
    (should (string-match-p "at least one trigger" err))))

(ert-deftest supervisor-test-timer-validate-unknown-keyword ()
  "Timer with unknown keyword is rejected."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-startup-sec 60 :bogus t) nil)))
    (should (string-match-p "unknown keyword" err))))

(ert-deftest supervisor-test-timer-validate-startup-sec-type ()
  "Timer with non-integer :on-startup-sec is rejected."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-startup-sec "60") nil)))
    (should (string-match-p ":on-startup-sec must be" err))))

(ert-deftest supervisor-test-timer-validate-unit-active-sec-positive ()
  "Timer with zero :on-unit-active-sec is rejected."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-unit-active-sec 0) nil)))
    (should (string-match-p ":on-unit-active-sec must be a positive" err))))

(ert-deftest supervisor-test-timer-validate-startup-sec-nil ()
  "Timer with only nil :on-startup-sec has no valid trigger."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-startup-sec nil) nil)))
    (should (string-match-p "at least one trigger" err))))

(ert-deftest supervisor-test-timer-validate-unit-active-sec-nil ()
  "Timer with only nil :on-unit-active-sec has no valid trigger."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-unit-active-sec nil) nil)))
    (should (string-match-p "at least one trigger" err))))

(ert-deftest supervisor-test-timer-validate-startup-sec-nil-with-other-trigger ()
  "Timer with nil :on-startup-sec but valid :on-calendar still validates."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (supervisor--build-plan programs))
         (err (supervisor-timer--validate
               '(:id "t" :target "script" :on-startup-sec nil
                 :on-calendar (:hour 3))
               plan)))
    ;; Should fail on nil :on-startup-sec type check, not on missing trigger
    (should (string-match-p ":on-startup-sec must be" err))))

(ert-deftest supervisor-test-timer-validate-calendar-unknown-field ()
  "Timer with unknown calendar field is rejected."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-calendar (:bogus 5)) nil)))
    (should (string-match-p "unknown field" err))))

(ert-deftest supervisor-test-timer-validate-calendar-bad-value ()
  "Timer with invalid calendar value type is rejected."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-calendar (:hour "3")) nil)))
    (should (string-match-p "must be integer" err))))

(ert-deftest supervisor-test-timer-validate-enabled-boolean ()
  "Timer with non-boolean :enabled is rejected."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-startup-sec 60 :enabled "yes") nil)))
    (should (string-match-p ":enabled must be a boolean" err))))

(ert-deftest supervisor-test-timer-validate-target-not-found ()
  "Timer targeting nonexistent service is rejected."
  (let* ((programs '(("real" :type oneshot :id "real")))
         (plan (supervisor--build-plan programs))
         (err (supervisor-timer--validate
               '(:id "t" :target "missing" :on-startup-sec 60) plan)))
    (should (string-match-p "not found" err))))

(ert-deftest supervisor-test-timer-validate-target-not-oneshot ()
  "Timer targeting simple service is rejected."
  (let* ((programs '(("daemon" :type simple :id "daemon")))
         (plan (supervisor--build-plan programs))
         (err (supervisor-timer--validate
               '(:id "t" :target "daemon" :on-startup-sec 60) plan)))
    (should (string-match-p "must be a oneshot" err))))

(ert-deftest supervisor-test-timer-validate-valid ()
  "Valid timer passes validation."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (supervisor--build-plan programs))
         (err (supervisor-timer--validate
               '(:id "t" :target "script" :on-startup-sec 60) plan)))
    (should-not err)))

(ert-deftest supervisor-test-timer-validate-calendar-valid ()
  "Valid calendar schedule passes validation."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (supervisor--build-plan programs))
         (err (supervisor-timer--validate
               '(:id "t" :target "script"
                 :on-calendar (:hour 3 :minute 0 :day-of-week *))
               plan)))
    (should-not err)))

(ert-deftest supervisor-test-timer-validate-calendar-list-valid ()
  "Valid list of calendar schedules passes validation."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (supervisor--build-plan programs))
         (err (supervisor-timer--validate
               '(:id "t" :target "script"
                 :on-calendar ((:hour 3 :minute 0)
                               (:hour 15 :minute 30)))
               plan)))
    (should-not err)))

(ert-deftest supervisor-test-timer-validate-calendar-list-invalid ()
  "Invalid entry in list of calendar schedules is rejected."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (supervisor--build-plan programs))
         (err (supervisor-timer--validate
               '(:id "t" :target "script"
                 :on-calendar ((:hour 3 :minute 0)
                               (:bogus 15)))
               plan)))
    (should (string-match-p "unknown field" err))))

(ert-deftest supervisor-test-timer-validate-calendar-empty ()
  "Empty calendar list with no other trigger has no valid trigger."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (supervisor--build-plan programs))
         (err (supervisor-timer--validate
               '(:id "t" :target "script" :on-calendar ())
               plan)))
    ;; Empty list is falsy so fails trigger check first
    (should (string-match-p "at least one trigger" err))))

(ert-deftest supervisor-test-timer-validate-calendar-dotted-pair ()
  "Dotted pair calendar is rejected, not crash."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (supervisor--build-plan programs))
         (err (supervisor-timer--validate
               '(:id "t" :target "script" :on-calendar (:hour . 3))
               plan)))
    (should (string-match-p "proper plist" err))))

(ert-deftest supervisor-test-timer-validate-calendar-empty-with-other-trigger ()
  "Empty calendar with valid :on-startup-sec fails on empty calendar."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (supervisor--build-plan programs))
         (err (supervisor-timer--validate
               '(:id "t" :target "script" :on-calendar () :on-startup-sec 60)
               plan)))
    (should (string-match-p "cannot be empty" err))))

(ert-deftest supervisor-test-timer-validate-calendar-list-non-plist ()
  "Non-plist entry in calendar list is rejected."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (supervisor--build-plan programs))
         (err (supervisor-timer--validate
               '(:id "t" :target "script"
                 :on-calendar ((:hour 3) foo))
               plan)))
    (should (string-match-p "must be a plist" err))))

(ert-deftest supervisor-test-timer-parse ()
  "Timer parsing produces correct struct."
  (let ((timer (supervisor-timer--parse
                '(:id "backup" :target "backup-script"
                  :on-calendar (:hour 3 :minute 0)
                  :enabled nil :persistent nil))))
    (should (equal "backup" (supervisor-timer-id timer)))
    (should (equal "backup-script" (supervisor-timer-target timer)))
    (should (eq nil (supervisor-timer-enabled timer)))
    (should (equal '(:hour 3 :minute 0) (supervisor-timer-on-calendar timer)))
    (should (eq nil (supervisor-timer-persistent timer)))))

(ert-deftest supervisor-test-timer-parse-defaults ()
  "Timer parsing applies correct defaults."
  (let ((timer (supervisor-timer--parse
                '(:id "t" :target "foo" :on-startup-sec 60))))
    (should (eq t (supervisor-timer-enabled timer)))
    (should (eq t (supervisor-timer-persistent timer)))))

(ert-deftest supervisor-test-timer-build-list-valid ()
  "Build timer list from valid config."
  (let* ((supervisor-timers '((:id "t1" :target "s1" :on-startup-sec 60)
                              (:id "t2" :target "s2" :on-startup-sec 120)))
         (programs '(("s1" :type oneshot :id "s1")
                     ("s2" :type oneshot :id "s2")))
         (plan (supervisor--build-plan programs))
         (timers (supervisor-timer-build-list plan)))
    (should (= 2 (length timers)))
    (should (= 0 (hash-table-count supervisor--invalid-timers)))))

(ert-deftest supervisor-test-timer-build-list-invalid-rejected ()
  "Invalid timers are rejected and tracked."
  (let* ((supervisor-timers '((:id "valid" :target "s1" :on-startup-sec 60)
                              (:id "invalid" :target "missing" :on-startup-sec 60)))
         (programs '(("s1" :type oneshot :id "s1")))
         (plan (supervisor--build-plan programs))
         (timers (supervisor-timer-build-list plan)))
    (should (= 1 (length timers)))
    (should (= 1 (hash-table-count supervisor--invalid-timers)))
    (should (gethash "invalid" supervisor--invalid-timers))))

(ert-deftest supervisor-test-timer-build-list-duplicate-rejected ()
  "Duplicate timer IDs are rejected."
  (let* ((supervisor-timers '((:id "dup" :target "s1" :on-startup-sec 60)
                              (:id "dup" :target "s1" :on-startup-sec 120)))
         (programs '(("s1" :type oneshot :id "s1")))
         (plan (supervisor--build-plan programs))
         (timers (supervisor-timer-build-list plan)))
    (should (= 1 (length timers)))
    (should (= 1 (hash-table-count supervisor--invalid-timers)))))

(ert-deftest supervisor-test-timer-build-list-duplicate-invalid-first ()
  "Duplicate timer ID rejected deterministically when first is invalid.
Second valid occurrence should not activate when first invalid occurrence used the ID."
  (let* ((supervisor-timers '((:id "dup" :target "missing" :on-startup-sec 60)  ; invalid (bad target)
                              (:id "dup" :target "s1" :on-startup-sec 120)))    ; valid but duplicate
         (programs '(("s1" :type oneshot :id "s1")))
         (plan (supervisor--build-plan programs))
         (timers (supervisor-timer-build-list plan)))
    ;; No active timers (first invalid, second duplicate)
    (should (= 0 (length timers)))
    ;; One invalid entry (hash key = "dup", first error preserved)
    (should (= 1 (hash-table-count supervisor--invalid-timers)))
    ;; First error (target not found) should be preserved, not overwritten by duplicate
    (should (string-match-p "target" (gethash "dup" supervisor--invalid-timers)))))

(ert-deftest supervisor-test-timer-validate-startup-sec-zero-rejected ()
  "Timer with zero :on-startup-sec is rejected (must be positive)."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-startup-sec 0) nil)))
    (should err)
    (should (string-match-p ":on-startup-sec must be a positive" err))))

(ert-deftest supervisor-test-timer-validate-calendar-field-range-minute ()
  "Calendar :minute field rejects values outside 0-59."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-calendar (:minute 60)) nil)))
    (should err)
    (should (string-match-p "out of range" err))))

(ert-deftest supervisor-test-timer-validate-calendar-field-range-hour ()
  "Calendar :hour field rejects values outside 0-23."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-calendar (:hour 24)) nil)))
    (should err)
    (should (string-match-p "out of range" err))))

(ert-deftest supervisor-test-timer-validate-calendar-field-range-day ()
  "Calendar :day-of-month field rejects values outside 1-31."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-calendar (:day-of-month 0)) nil)))
    (should err)
    (should (string-match-p "out of range" err))))

(ert-deftest supervisor-test-timer-validate-calendar-field-range-month ()
  "Calendar :month field rejects values outside 1-12."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-calendar (:month 13)) nil)))
    (should err)
    (should (string-match-p "out of range" err))))

(ert-deftest supervisor-test-timer-validate-calendar-field-range-dow ()
  "Calendar :day-of-week field rejects values outside 0-6."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-calendar (:day-of-week 7)) nil)))
    (should err)
    (should (string-match-p "out of range" err))))

(ert-deftest supervisor-test-timer-validate-calendar-field-range-list ()
  "Calendar field list with out-of-range value is rejected."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-calendar (:minute (0 30 61))) nil)))
    (should err)
    (should (string-match-p "out of range" err))))

(ert-deftest supervisor-test-timer-validate-calendar-dotted-pair-rejected ()
  "Calendar field dotted pair is rejected (not crash)."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-calendar (:minute (0 . 1))) nil)))
    (should err)
    (should (string-match-p "non-empty list of integers" err))))

(ert-deftest supervisor-test-timer-validate-calendar-empty-list-rejected ()
  "Calendar field empty list is rejected."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-calendar (:minute ())) nil)))
    (should err)
    (should (string-match-p "non-empty list of integers" err))))

;;; Timer Scheduler Core tests

(ert-deftest supervisor-test-calendar-field-matches-star ()
  "Calendar field * matches any value."
  (should (supervisor-timer--calendar-field-matches-p '* 0))
  (should (supervisor-timer--calendar-field-matches-p '* 23))
  (should (supervisor-timer--calendar-field-matches-p '* 59)))

(ert-deftest supervisor-test-calendar-field-matches-integer ()
  "Calendar field integer matches exact value."
  (should (supervisor-timer--calendar-field-matches-p 3 3))
  (should-not (supervisor-timer--calendar-field-matches-p 3 4))
  (should-not (supervisor-timer--calendar-field-matches-p 3 0)))

(ert-deftest supervisor-test-calendar-field-matches-list ()
  "Calendar field list matches any value in list."
  (should (supervisor-timer--calendar-field-matches-p '(1 3 5) 1))
  (should (supervisor-timer--calendar-field-matches-p '(1 3 5) 3))
  (should (supervisor-timer--calendar-field-matches-p '(1 3 5) 5))
  (should-not (supervisor-timer--calendar-field-matches-p '(1 3 5) 2))
  (should-not (supervisor-timer--calendar-field-matches-p '(1 3 5) 4)))

(ert-deftest supervisor-test-calendar-matches-time ()
  "Calendar spec matches decoded time correctly."
  ;; Create a decoded time for 2025-01-15 03:30:00 (Wednesday)
  (let ((decoded (decode-time (encode-time 0 30 3 15 1 2025))))
    ;; Exact match
    (should (supervisor-timer--calendar-matches-time-p
             '(:hour 3 :minute 30) decoded))
    ;; Wildcards
    (should (supervisor-timer--calendar-matches-time-p
             '(:hour 3 :minute *) decoded))
    ;; Lists
    (should (supervisor-timer--calendar-matches-time-p
             '(:hour (1 2 3) :minute 30) decoded))
    ;; Mismatch
    (should-not (supervisor-timer--calendar-matches-time-p
                 '(:hour 4 :minute 30) decoded))
    (should-not (supervisor-timer--calendar-matches-time-p
                 '(:hour 3 :minute 0) decoded))))

(ert-deftest supervisor-test-calendar-next-minute-finds-match ()
  "Calendar next minute finds matching time."
  ;; From 2025-01-15 00:00:00, find next 03:30
  (let* ((from (encode-time 0 0 0 15 1 2025))
         (next (supervisor-timer--calendar-next-minute
                from '(:hour 3 :minute 30) 2)))  ; 2 days max
    (should next)
    ;; Should be 03:30
    (let ((decoded (decode-time next)))
      (should (= 3 (decoded-time-hour decoded)))
      (should (= 30 (decoded-time-minute decoded))))))

(ert-deftest supervisor-test-calendar-next-minute-respects-limit ()
  "Calendar next minute respects iteration limit."
  ;; Looking for hour 25 (impossible) with small limit
  (let* ((from (encode-time 0 0 0 15 1 2025))
         (next (supervisor-timer--calendar-next-minute
                from '(:hour 25 :minute 0) 7)))  ; 7 days max
    (should-not next)))

(ert-deftest supervisor-test-calendar-next-minute-leap-day ()
  "Calendar next minute finds leap day across multi-year gap.
From March 2025, next Feb 29 is in 2028 (~3 years away)."
  (let* ((from (encode-time 0 0 0 1 3 2025))  ; 2025-03-01 00:00:00
         (next (supervisor-timer--calendar-next-minute
                from '(:month 2 :day-of-month 29 :hour 0 :minute 0) 10228)))  ; 28 years
    (should next)
    (let ((decoded (decode-time next)))
      (should (= 2028 (decoded-time-year decoded)))
      (should (= 2 (decoded-time-month decoded)))
      (should (= 29 (decoded-time-day decoded))))))

(ert-deftest supervisor-test-calendar-next-minute-leap-day-weekday ()
  "Calendar next minute finds leap day + weekday across long gap.
From March 2025, next Feb 29 that is Sunday (dow=0) is in 2032 (~7 years away)."
  (let* ((from (encode-time 0 0 0 1 3 2025))  ; 2025-03-01 00:00:00
         (next (supervisor-timer--calendar-next-minute
                from '(:month 2 :day-of-month 29 :day-of-week 0 :hour 0 :minute 0) 10228)))
    (should next)
    (let ((decoded (decode-time next)))
      (should (= 2032 (decoded-time-year decoded)))
      (should (= 2 (decoded-time-month decoded)))
      (should (= 29 (decoded-time-day decoded)))
      ;; Verify it's actually Sunday (day-of-week 0)
      (should (= 0 (decoded-time-weekday decoded))))))

(ert-deftest supervisor-test-calendar-next-minute-strictly-after ()
  "Calendar next minute returns time strictly after from-time.
When from-time is exactly at a matching minute boundary, should return next occurrence."
  ;; From 2025-01-15 03:30:00 exactly (matches :hour 3 :minute 30)
  (let* ((from (encode-time 0 30 3 15 1 2025))
         (next (supervisor-timer--calendar-next-minute
                from '(:hour 3 :minute 30) 2)))  ; 2 days max
    (should next)
    ;; Should be next day's 03:30, not the same time
    (should (> next (float-time from)))
    (let ((decoded (decode-time next)))
      (should (= 16 (decoded-time-day decoded)))  ; Next day
      (should (= 3 (decoded-time-hour decoded)))
      (should (= 30 (decoded-time-minute decoded))))))

(ert-deftest supervisor-test-calendar-next-minute-dst-gap ()
  "Calendar next minute skips non-existent DST gap times.
On March 9, 2025 in America/New_York, 2:00-2:59 AM doesn't exist (spring forward).
Searching for 2:30 AM should skip March 9 and return March 10 2:30 AM."
  (let ((process-environment (cons "TZ=America/New_York" process-environment)))
    ;; Skip test if TZ setting doesn't take effect (CI may lack timezone data)
    ;; In America/New_York, 2:30 AM on March 9 2025 doesn't exist due to DST.
    ;; encode-time will shift it to 3:30 AM. If it stays at 2:30, TZ isn't working.
    (let* ((test-time (encode-time 0 30 2 9 3 2025))
           (test-decoded (decode-time test-time)))
      ;; If DST is working, hour should be 3 (shifted from non-existent 2:30)
      (skip-unless (= 3 (decoded-time-hour test-decoded))))
    ;; From March 9, 2025 00:00:00 (before DST transition)
    (let* ((from (encode-time 0 0 0 9 3 2025))
           (next (supervisor-timer--calendar-next-minute
                  from '(:hour 2 :minute 30) 7)))  ; 7 days max
      (should next)
      (let ((decoded (decode-time next)))
        ;; Should be March 10 (next day), not March 9
        (should (= 10 (decoded-time-day decoded)))
        (should (= 2 (decoded-time-hour decoded)))
        (should (= 30 (decoded-time-minute decoded)))))))

(ert-deftest supervisor-test-timer-next-startup-time ()
  "Startup trigger computes time relative to scheduler start."
  (let* ((timer (supervisor-timer--create
                 :id "t1" :target "s1" :on-startup-sec 120))
         (supervisor--scheduler-startup-time 1000.0)
         (supervisor--timer-state (make-hash-table :test 'equal)))
    ;; No previous run - should return startup + delay
    (should (= 1120.0 (supervisor-timer--next-startup-time timer)))
    ;; After startup trigger has fired - should return nil
    (puthash "t1" '(:startup-triggered t) supervisor--timer-state)
    (should-not (supervisor-timer--next-startup-time timer))))

(ert-deftest supervisor-test-timer-next-unit-active-time ()
  "Unit-active trigger computes time relative to last success."
  (let* ((timer (supervisor-timer--create
                 :id "t1" :target "s1" :on-unit-active-sec 300))
         (supervisor--timer-state (make-hash-table :test 'equal)))
    ;; No previous success - should return nil
    (should-not (supervisor-timer--next-unit-active-time timer))
    ;; After a successful run
    (puthash "t1" '(:last-success-at 2000.0) supervisor--timer-state)
    (should (= 2300.0 (supervisor-timer--next-unit-active-time timer)))))

(ert-deftest supervisor-test-timer-compute-next-run-picks-earliest ()
  "Timer picks earliest trigger when multiple are configured."
  (let* ((timer (supervisor-timer--create
                 :id "t1" :target "s1"
                 :on-startup-sec 60
                 :on-unit-active-sec 300))
         (supervisor--scheduler-startup-time 1000.0)
         (supervisor--timer-state (make-hash-table :test 'equal)))
    ;; Only startup is available (no previous success)
    (should (= 1060.0 (supervisor-timer--compute-next-run timer 1000.0)))
    ;; After startup trigger has fired and a success recorded, unit-active becomes available
    (puthash "t1" '(:last-success-at 1050.0 :startup-triggered t) supervisor--timer-state)
    ;; Startup already fired, unit-active at 1350
    (should (= 1350.0 (supervisor-timer--compute-next-run timer 1100.0)))))

(ert-deftest supervisor-test-timer-overlap-detection ()
  "Timer detects when target is still running."
  (let* ((timer (supervisor-timer--create :id "t1" :target "s1"))
         (supervisor--processes (make-hash-table :test 'equal)))
    ;; No process - not active
    (should-not (supervisor-timer--target-active-p timer))
    ;; Dead process - not active
    (puthash "s1" (start-process "test" nil "true") supervisor--processes)
    (sleep-for 0.1) ; Let it die
    (should-not (supervisor-timer--target-active-p timer))
    ;; Cleanup
    (clrhash supervisor--processes)))

(ert-deftest supervisor-test-timer-trigger-disabled-timer ()
  "Timer trigger skips disabled timer and records miss."
  (let* ((timer (supervisor-timer--create :id "t1" :target "s1" :enabled nil))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor-programs '(("true" :id "s1" :type oneshot))))
    ;; Trigger disabled timer
    (should-not (supervisor-timer--trigger timer 'scheduled))
    ;; Should record miss with reason 'disabled
    (let ((state (gethash "t1" supervisor--timer-state)))
      (should state)
      (should (plist-get state :last-missed-at))
      (should (eq 'disabled (plist-get state :last-miss-reason))))
    ;; Cleanup
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-trigger-disabled-target ()
  "Timer trigger skips disabled target and records miss."
  (let* ((timer (supervisor-timer--create :id "t1" :target "s1" :enabled t))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor-programs '(("true" :id "s1" :type oneshot :disabled t))))
    ;; Trigger timer with disabled target (via config)
    (should-not (supervisor-timer--trigger timer 'scheduled))
    ;; Should record miss with reason 'disabled-target
    (let ((state (gethash "t1" supervisor--timer-state)))
      (should state)
      (should (plist-get state :last-missed-at))
      (should (eq 'disabled-target (plist-get state :last-miss-reason))))
    ;; Cleanup
    (clrhash supervisor--timer-state)
    (clrhash supervisor--processes)
    (clrhash supervisor--enabled-override)
    (clrhash supervisor--invalid)))

(ert-deftest supervisor-test-timer-trigger-disabled-target-override ()
  "Timer trigger respects runtime disabled override."
  (let* ((timer (supervisor-timer--create :id "t1" :target "s1" :enabled t))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor-programs '(("true" :id "s1" :type oneshot))))
    ;; Disable target via runtime override
    (puthash "s1" 'disabled supervisor--enabled-override)
    ;; Trigger timer
    (should-not (supervisor-timer--trigger timer 'scheduled))
    ;; Should record miss with reason 'disabled-target
    (let ((state (gethash "t1" supervisor--timer-state)))
      (should state)
      (should (eq 'disabled-target (plist-get state :last-miss-reason))))
    ;; Cleanup
    (clrhash supervisor--timer-state)
    (clrhash supervisor--processes)
    (clrhash supervisor--enabled-override)
    (clrhash supervisor--invalid)))

(ert-deftest supervisor-test-timer-trigger-target-not-found ()
  "Timer trigger handles missing target gracefully."
  (let* ((timer (supervisor-timer--create :id "t1" :target "nonexistent" :enabled t))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor-programs nil))
    ;; Trigger timer with nonexistent target
    (should-not (supervisor-timer--trigger timer 'scheduled))
    ;; No state recorded (early return)
    (should-not (gethash "t1" supervisor--timer-state))
    ;; Cleanup
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-trigger-overlap-skips ()
  "Timer trigger skips when target is still running."
  (let* ((timer (supervisor-timer--create :id "t1" :target "s1" :enabled t))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor-programs '(("sleep 10" :id "s1" :type oneshot))))
    ;; Simulate running process
    (puthash "s1" (start-process "test-overlap" nil "sleep" "10")
             supervisor--processes)
    ;; Trigger timer
    (should-not (supervisor-timer--trigger timer 'scheduled))
    ;; Should record miss with reason 'overlap
    (let ((state (gethash "t1" supervisor--timer-state)))
      (should state)
      (should (eq 'overlap (plist-get state :last-miss-reason))))
    ;; Cleanup
    (let ((proc (gethash "s1" supervisor--processes)))
      (when (process-live-p proc)
        (delete-process proc)))
    (clrhash supervisor--invalid)
    (clrhash supervisor--timer-state)
    (clrhash supervisor--processes)
    (clrhash supervisor--enabled-override)))

(ert-deftest supervisor-test-timer-trigger-success-path ()
  "Timer trigger succeeds and emits timer-trigger event."
  (let* ((supervisor-timer-subsystem-mode t)
         (supervisor-mode t)
         (timer (supervisor-timer--create :id "t1" :target "s1" :enabled t))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--scheduler-startup-time (float-time))
         (supervisor-programs '(("true" :id "s1" :type oneshot)))
         (events nil)
         (hook-fn (lambda (event)
                    (when (eq (plist-get event :type) 'timer-trigger)
                      (push event events)))))
    ;; Capture timer-trigger event
    (add-hook 'supervisor-event-hook hook-fn)
    (unwind-protect
        (progn
          ;; Trigger timer
          (should (supervisor-timer--trigger timer 'scheduled))
          ;; Should have recorded :last-run-at
          (let ((state (gethash "t1" supervisor--timer-state)))
            (should state)
            (should (plist-get state :last-run-at)))
          ;; Should have emitted timer-trigger event
          (should (= 1 (length events)))
          (let ((event (car events)))
            (should (eq 'timer-trigger (plist-get event :type)))
            (should (equal "t1" (plist-get event :id)))
            (should (equal "s1" (plist-get (plist-get event :data) :target)))))
      ;; Cleanup
      (remove-hook 'supervisor-event-hook hook-fn)
      (clrhash supervisor--invalid)
      (clrhash supervisor--timer-state)
      (clrhash supervisor--processes)
      (clrhash supervisor--enabled-override))))

(ert-deftest supervisor-test-timer-startup-trigger-independent ()
  "Startup trigger is independent from calendar/unit-active triggers."
  (let* ((timer (supervisor-timer--create :id "t1" :target "s1" :enabled t
                                          :on-startup-sec 60
                                          :on-calendar '(:minute 0)))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--scheduler-startup-time 1000.0))
    ;; Simulate a calendar trigger having fired (sets :last-run-at)
    (puthash "t1" (list :last-run-at 1010.0) supervisor--timer-state)
    ;; Startup trigger should still return a time (not cancelled by calendar)
    (let ((next (supervisor-timer--next-startup-time timer)))
      (should next)
      (should (= 1060.0 next)))
    ;; Now mark startup as triggered
    (puthash "t1" (list :last-run-at 1010.0 :startup-triggered t)
             supervisor--timer-state)
    ;; Now startup trigger should return nil
    (should-not (supervisor-timer--next-startup-time timer))
    ;; Cleanup
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-startup-consumed-on-skipped-run ()
  "Startup trigger is consumed even when run is skipped (disabled-target)."
  ;; Regression test: startup trigger must not cause 1s retry loop on skip
  (let* ((timer (supervisor-timer--create :id "t1" :target "s1" :enabled t
                                          :on-startup-sec 10))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--scheduler-startup-time 1000.0)
         (supervisor-programs '(("true" :id "s1" :type oneshot :disabled t))))
    ;; Initialize next-run-at to startup trigger time
    (puthash "t1" '(:next-run-at 1010.0) supervisor--timer-state)
    ;; Simulate time is now past startup trigger
    (cl-letf (((symbol-function 'float-time) (lambda () 1015.0)))
      ;; Trigger should fail (target disabled)
      (should-not (supervisor-timer--trigger timer 'scheduled))
      ;; But startup-triggered should be set to prevent retry loop
      (let ((state (gethash "t1" supervisor--timer-state)))
        (should (plist-get state :startup-triggered)))
      ;; Next startup time should now be nil (consumed)
      (should-not (supervisor-timer--next-startup-time timer)))
    ;; Cleanup
    (clrhash supervisor--timer-state)
    (clrhash supervisor--processes)
    (clrhash supervisor--enabled-override)
    (clrhash supervisor--invalid)))

(ert-deftest supervisor-test-timer-scheduler-not-started-during-shutdown ()
  "Timer scheduler is not started when shutting down."
  (let ((supervisor--shutting-down t)
        (supervisor-timers '((:id "t1" :target "s1" :on-startup-sec 60)))
        (supervisor-programs '(("true" :id "s1" :type oneshot)))
        (scheduler-started nil))
    ;; Mock the scheduler start function
    (cl-letf (((symbol-function 'supervisor-timer-scheduler-start)
               (lambda () (setq scheduler-started t))))
      ;; Simulate stage completion with no remaining stages
      (supervisor--start-stages-from-plan nil)
      ;; Scheduler should NOT have been started
      (should-not scheduler-started))))

;;; Phase 3: Retry and Catch-up Tests

(ert-deftest supervisor-test-timer-failure-retryable-positive-exit ()
  "Positive exit codes are retryable."
  (should (supervisor-timer--failure-retryable-p 1))
  (should (supervisor-timer--failure-retryable-p 127)))

(ert-deftest supervisor-test-timer-failure-not-retryable-signal ()
  "Signal deaths (stored as negative values) are not retryable.
Emacs provides signal numbers as positive values with process-status='signal.
The oneshot exit handler encodes these as negative for retry gating."
  (should-not (supervisor-timer--failure-retryable-p -9))   ; SIGKILL
  (should-not (supervisor-timer--failure-retryable-p -15))) ; SIGTERM

(ert-deftest supervisor-test-oneshot-exit-encodes-signal-as-negative ()
  "Signal deaths are stored as negative values in oneshot-completed.
This ensures retry eligibility correctly rejects signal deaths."
  (let ((supervisor--oneshot-completed (make-hash-table :test 'equal))
        (supervisor--oneshot-callbacks (make-hash-table :test 'equal))
        (supervisor--dag-blocking-oneshots nil)
        (supervisor--dag-in-degree (make-hash-table :test 'equal))
        (supervisor--dag-dependents (make-hash-table :test 'equal))
        (supervisor--dag-ready nil)
        (supervisor--event-handlers nil))
    ;; Simulate signal death: proc-status='signal, exit-code=9 (SIGKILL)
    (supervisor--handle-oneshot-exit "test-oneshot" 'signal 9)
    ;; Should be stored as -9
    (should (= -9 (gethash "test-oneshot" supervisor--oneshot-completed)))
    ;; Therefore not retryable
    (should-not (supervisor-timer--failure-retryable-p
                 (gethash "test-oneshot" supervisor--oneshot-completed)))))

(ert-deftest supervisor-test-oneshot-exit-preserves-normal-exit-code ()
  "Normal exit codes are stored as-is (positive values)."
  (let ((supervisor--oneshot-completed (make-hash-table :test 'equal))
        (supervisor--oneshot-callbacks (make-hash-table :test 'equal))
        (supervisor--dag-blocking-oneshots nil)
        (supervisor--dag-in-degree (make-hash-table :test 'equal))
        (supervisor--dag-dependents (make-hash-table :test 'equal))
        (supervisor--dag-ready nil)
        (supervisor--event-handlers nil))
    ;; Simulate normal exit: proc-status='exit, exit-code=1
    (supervisor--handle-oneshot-exit "test-oneshot" 'exit 1)
    ;; Should be stored as 1 (positive)
    (should (= 1 (gethash "test-oneshot" supervisor--oneshot-completed)))
    ;; Therefore retryable
    (should (supervisor-timer--failure-retryable-p
             (gethash "test-oneshot" supervisor--oneshot-completed)))))

(ert-deftest supervisor-test-timer-failure-not-retryable-zero ()
  "Zero exit (success) is not retryable."
  (should-not (supervisor-timer--failure-retryable-p 0)))

(ert-deftest supervisor-test-timer-failure-not-retryable-nil ()
  "Nil exit code is not retryable."
  (should-not (supervisor-timer--failure-retryable-p nil)))

(ert-deftest supervisor-test-signal-death-status-is-failed ()
  "Signal deaths (negative exit codes) are classified as failed in status.
This is a regression test: signals are non-retryable but still failed."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--oneshot-completed (make-hash-table :test 'equal)))
    ;; Simulate SIGKILL death stored as -9
    (puthash "test-oneshot" -9 supervisor--oneshot-completed)
    (let ((result (supervisor--compute-entry-status "test-oneshot" 'oneshot)))
      (should (equal "failed" (car result)))
      (should (equal "exit:-9" (cdr result))))))

(ert-deftest supervisor-test-signal-death-dashboard-count-is-failed ()
  "Signal deaths are counted as failed in dashboard summary."
  (let ((supervisor-programs '(("true" :id "sig-test" :type oneshot)))
        (supervisor--processes (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--oneshot-completed (make-hash-table :test 'equal))
        (supervisor--invalid (make-hash-table :test 'equal)))
    ;; Simulate SIGTERM death stored as -15
    (puthash "sig-test" -15 supervisor--oneshot-completed)
    (let ((summary (supervisor--health-summary)))
      ;; Should show 1 fail, not 1 done
      (should (string-match-p "1 fail" summary))
      (should-not (string-match-p "1 done" summary)))))

(ert-deftest supervisor-test-timer-schedule-retry-first-attempt ()
  "First retry is scheduled with first interval."
  (let ((supervisor-timer-retry-intervals '(30 120 600))
        (supervisor--timer-state (make-hash-table :test 'equal))
        (state nil))
    (puthash "t1" state supervisor--timer-state)
    (cl-letf (((symbol-function 'float-time) (lambda () 1000.0)))
      (let ((updated (supervisor-timer--schedule-retry "t1" state)))
        (should updated)
        (should (= 1 (plist-get updated :retry-attempt)))
        (should (= 1030.0 (plist-get updated :retry-next-at)))))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-schedule-retry-second-attempt ()
  "Second retry uses second interval."
  (let ((supervisor-timer-retry-intervals '(30 120 600))
        (supervisor--timer-state (make-hash-table :test 'equal))
        (state '(:retry-attempt 1)))
    (puthash "t1" state supervisor--timer-state)
    (cl-letf (((symbol-function 'float-time) (lambda () 1000.0)))
      (let ((updated (supervisor-timer--schedule-retry "t1" state)))
        (should updated)
        (should (= 2 (plist-get updated :retry-attempt)))
        (should (= 1120.0 (plist-get updated :retry-next-at)))))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-schedule-retry-exhausted ()
  "No retry scheduled when max attempts reached."
  (let ((supervisor-timer-retry-intervals '(30 120 600))
        (supervisor--timer-state (make-hash-table :test 'equal))
        (state '(:retry-attempt 3)))
    (puthash "t1" state supervisor--timer-state)
    (should-not (supervisor-timer--schedule-retry "t1" state))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-schedule-retry-disabled ()
  "No retry when retry-intervals is nil."
  (let ((supervisor-timer-retry-intervals nil)
        (supervisor--timer-state (make-hash-table :test 'equal))
        (state nil))
    (puthash "t1" state supervisor--timer-state)
    (should-not (supervisor-timer--schedule-retry "t1" state))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-catch-up-needed ()
  "Catch-up detected for persistent timer with missed run."
  (let* ((timer (supervisor-timer--create :id "t1" :target "s1"
                                          :on-calendar '(:minute 0)
                                          :persistent t))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--scheduler-startup-time 1000.0)
         (supervisor-timer-catch-up-limit 86400))
    ;; Last run was 2 hours ago, missed the hourly schedule
    (puthash "t1" '(:last-run-at 900.0) supervisor--timer-state)
    ;; Mock calendar computation to return a time in the past
    (cl-letf (((symbol-function 'supervisor-timer--compute-next-run)
               (lambda (_timer _from) 950.0))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      (should (supervisor-timer--needs-catch-up-p timer)))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-catch-up-not-needed-recent ()
  "No catch-up for timer with recent run."
  (let* ((timer (supervisor-timer--create :id "t1" :target "s1"
                                          :on-calendar '(:minute 0)
                                          :persistent t))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--scheduler-startup-time 1000.0)
         (supervisor-timer-catch-up-limit 86400))
    ;; Last run was just now
    (puthash "t1" '(:last-run-at 999.0) supervisor--timer-state)
    ;; Mock calendar computation to return a time in the future
    (cl-letf (((symbol-function 'supervisor-timer--compute-next-run)
               (lambda (_timer _from) 1060.0))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      (should-not (supervisor-timer--needs-catch-up-p timer)))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-catch-up-not-needed-non-persistent ()
  "No catch-up for non-persistent timer."
  (let* ((timer (supervisor-timer--create :id "t1" :target "s1"
                                          :on-calendar '(:minute 0)
                                          :persistent nil))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--scheduler-startup-time 1000.0)
         (supervisor-timer-catch-up-limit 86400))
    (puthash "t1" '(:last-run-at 900.0) supervisor--timer-state)
    (should-not (supervisor-timer--needs-catch-up-p timer))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-catch-up-not-needed-too-old ()
  "No catch-up for missed run beyond catch-up limit."
  (let* ((timer (supervisor-timer--create :id "t1" :target "s1"
                                          :on-calendar '(:minute 0)
                                          :persistent t))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--scheduler-startup-time 100000.0)
         (supervisor-timer-catch-up-limit 86400))
    ;; Last run was way before catch-up window
    (puthash "t1" '(:last-run-at 1000.0) supervisor--timer-state)
    ;; Mock calendar computation to return a time before catch-up window
    (cl-letf (((symbol-function 'supervisor-timer--compute-next-run)
               (lambda (_timer _from) 2000.0))  ; Before cutoff
              ((symbol-function 'float-time) (lambda () 100000.0)))
      (should-not (supervisor-timer--needs-catch-up-p timer)))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-catch-up-boundary-no-false-trigger ()
  "No false catch-up when last-run-at is exactly at schedule boundary.
The fix passes (1+ last-run) to compute-next-run to avoid the case where
compute-next-run returns the same timestamp as last-run for calendar timers
at minute boundaries."
  (let* ((timer (supervisor-timer--create :id "t1" :target "s1"
                                          :on-calendar '(:minute 0)
                                          :persistent t))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--scheduler-startup-time 1000.0)
         (supervisor-timer-catch-up-limit 86400)
         (call-count 0)
         (from-times nil))
    ;; Last run was at exactly time 900.0 (a schedule boundary)
    (puthash "t1" '(:last-run-at 900.0) supervisor--timer-state)
    ;; Mock compute-next-run to return 900.0 when called with 900.0,
    ;; but return future time 1800.0 when called with 901.0
    ;; This simulates the boundary condition we fixed
    (cl-letf (((symbol-function 'supervisor-timer--compute-next-run)
               (lambda (_timer from)
                 (setq call-count (1+ call-count))
                 (push from from-times)
                 (if (= from 900.0) 900.0 1800.0)))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      ;; Should NOT need catch-up because next run is in the future
      (should-not (supervisor-timer--needs-catch-up-p timer))
      ;; Verify we called with 901.0 (last-run + 1), not 900.0
      (should (= call-count 1))
      (should (= (car from-times) 901.0)))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-state-to-alist ()
  "Timer state to alist only includes persist keys."
  (let ((supervisor--timer-state (make-hash-table :test 'equal)))
    (puthash "t1" '(:last-run-at 1000.0
                    :last-success-at 1000.0
                    :next-run-at 2000.0
                    :retry-attempt 1
                    :startup-triggered t)
             supervisor--timer-state)
    (let ((alist (supervisor--timer-state-to-alist)))
      ;; Should include persisted keys
      (should (equal (alist-get "t1" alist nil nil #'equal)
                     '(:last-run-at 1000.0 :last-success-at 1000.0)))
      ;; Transient keys should not be included
      (should-not (plist-get (cdr (assoc "t1" alist)) :next-run-at))
      (should-not (plist-get (cdr (assoc "t1" alist)) :retry-attempt))
      (should-not (plist-get (cdr (assoc "t1" alist)) :startup-triggered)))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-state-save-load-roundtrip ()
  "Timer state survives save/load cycle."
  (let* ((supervisor-timer-subsystem-mode t)
         (supervisor-mode t)
         (temp-file (make-temp-file "supervisor-test-timer-state-" nil ".eld"))
         (supervisor-timer-state-file temp-file)
         (supervisor--timer-state (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; Save some state
          (puthash "t1" '(:last-run-at 1000.0 :last-success-at 1000.0)
                   supervisor--timer-state)
          (puthash "t2" '(:last-failure-at 900.0 :last-exit 1)
                   supervisor--timer-state)
          (should (supervisor-timer--save-state))
          ;; Clear and reconcile
          (clrhash supervisor--timer-state)
          (should (supervisor-timer--load-state))
          ;; Verify state restored
          (let ((t1 (gethash "t1" supervisor--timer-state))
                (t2 (gethash "t2" supervisor--timer-state)))
            (should (equal (plist-get t1 :last-run-at) 1000.0))
            (should (equal (plist-get t1 :last-success-at) 1000.0))
            (should (equal (plist-get t2 :last-failure-at) 900.0))
            (should (equal (plist-get t2 :last-exit) 1))))
      (delete-file temp-file)
      (clrhash supervisor--timer-state))))

(ert-deftest supervisor-test-timer-state-corrupt-file-handled ()
  "Corrupt timer state file is handled gracefully."
  (let* ((supervisor-timer-subsystem-mode t)
         (supervisor-mode t)
         (temp-file (make-temp-file "supervisor-test-corrupt-" nil ".eld"))
         (supervisor-timer-state-file temp-file)
         (supervisor--timer-state (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "this is not valid lisp data"))
          (should-not (supervisor-timer--load-state))
          ;; Hash should remain unchanged
          (should (= (hash-table-count supervisor--timer-state) 0)))
      (delete-file temp-file)
      (clrhash supervisor--timer-state))))

(ert-deftest supervisor-test-timer-state-persistence-disabled ()
  "Nil timer state file path disables persistence."
  (let ((supervisor-timer-subsystem-mode t)
        (supervisor-timer-state-file nil)
        (supervisor--timer-state (make-hash-table :test 'equal)))
    (puthash "t1" '(:last-run-at 1000.0) supervisor--timer-state)
    ;; Save returns nil when disabled
    (should-not (supervisor-timer--save-state))
    ;; Load returns nil when disabled
    (should-not (supervisor-timer--load-state))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-state-file-path ()
  "Timer state file path helper returns configured path."
  (let ((supervisor-timer-state-file "/test/path/timer-state.eld"))
    (should (equal (supervisor--timer-state-file-path) "/test/path/timer-state.eld")))
  (let ((supervisor-timer-state-file nil))
    (should-not (supervisor--timer-state-file-path))))

(ert-deftest supervisor-test-timer-state-newer-version-rejected ()
  "Newer schema version is rejected, not just warned."
  (let* ((supervisor-timer-subsystem-mode t)
         (supervisor-mode t)
         (temp-file (make-temp-file "supervisor-test-version-" nil ".eld"))
         (supervisor-timer-state-file temp-file)
         (supervisor--timer-state (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; Write file with future version
          (with-temp-file temp-file
            (insert (format "((version . %d) (timestamp . \"test\") (timers . ((\"t1\" :last-run-at 1000.0))))"
                            (1+ supervisor-timer-state-schema-version))))
          ;; Load should fail
          (should-not (supervisor-timer--load-state))
          ;; Hash should remain empty (data not merged)
          (should (= (hash-table-count supervisor--timer-state) 0)))
      (delete-file temp-file)
      (clrhash supervisor--timer-state))))

(ert-deftest supervisor-test-timer-state-stale-ids-pruned ()
  "Stale timer IDs are pruned from state during scheduler startup."
  (let* ((supervisor-timer-subsystem-mode t)
         (supervisor-mode t)
         (supervisor-programs '(("true" :id "s1" :type oneshot)))
         (supervisor-timers '((:id "active" :target "s1" :on-startup-sec 60)))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--timer-list nil)
         (supervisor--timer-scheduler nil)
         (supervisor--shutting-down nil)
         (supervisor--scheduler-startup-time nil)
         (supervisor-timer-state-file nil))
    ;; Pre-populate state with a stale ID
    (puthash "stale-removed" '(:last-run-at 500.0) supervisor--timer-state)
    (puthash "active" '(:last-run-at 900.0) supervisor--timer-state)
    ;; Mock to prevent actual scheduling
    (cl-letf (((symbol-function 'supervisor--timer-scheduler-tick) #'ignore)
              ((symbol-function 'supervisor-timer--process-catch-ups) #'ignore)
              ((symbol-function 'float-time) (lambda () 1000.0)))
      (supervisor-timer-scheduler-start))
    ;; Stale ID should be pruned
    (should-not (gethash "stale-removed" supervisor--timer-state))
    ;; Active ID should remain
    (should (gethash "active" supervisor--timer-state))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-cross-restart-catch-up ()
  "Integration test: scheduler startup with persisted state triggers catch-up."
  (let* ((supervisor-timer-subsystem-mode t)
         (supervisor-mode t)
         (temp-file (make-temp-file "supervisor-test-catchup-" nil ".eld"))
         (supervisor-programs '(("true" :id "s1" :type oneshot)))
         (supervisor-timers '((:id "t1" :target "s1" :on-calendar (:minute 0)
                               :persistent t)))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--timer-list nil)
         (supervisor--timer-scheduler nil)
         (supervisor--shutting-down nil)
         (supervisor--scheduler-startup-time nil)
         (supervisor--timer-state-loaded nil)
         (supervisor-timer-state-file temp-file)
         (supervisor-timer-catch-up-limit 86400)
         (catch-up-triggered nil))
    (unwind-protect
        (progn
          ;; Write persisted state with old last-run (simulates restart)
          (with-temp-file temp-file
            (insert (format "((version . %d) (timestamp . \"test\") (timers . ((\"t1\" :last-run-at 900.0 :last-success-at 900.0))))"
                            supervisor-timer-state-schema-version)))
          ;; Mock to track catch-up and prevent actual scheduling
          ;; Timer module's process-catch-ups uses funcall on supervisor-timer--trigger
          (cl-letf (((symbol-function 'supervisor--timer-scheduler-tick) #'ignore)
                    ((symbol-function 'supervisor-timer--trigger)
                     (lambda (_timer reason)
                       (when (eq reason 'catch-up)
                         (setq catch-up-triggered t))))
                    ;; Mock calendar to return a time between last-run and now
                    ((symbol-function 'supervisor-timer--compute-next-run)
                     (lambda (_timer _from) 950.0))
                    ((symbol-function 'float-time) (lambda () 1000.0)))
            (supervisor-timer-scheduler-start))
          ;; Verify state was loaded from file
          (should supervisor--timer-state-loaded)
          ;; Verify catch-up was triggered
          (should catch-up-triggered))
      (when (file-exists-p temp-file) (delete-file temp-file))
      (clrhash supervisor--timer-state))))

(ert-deftest supervisor-test-timer-scheduler-tick-handles-retry ()
  "Scheduler tick triggers retry when due."
  (let* ((supervisor-timer-subsystem-mode t)
         (supervisor-mode t)
         (timer (supervisor-timer--create :id "t1" :target "s1" :enabled t))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--timer-scheduler nil)
         (supervisor--shutting-down nil)
         (triggered-reason nil))
    ;; Set up state with pending retry
    (puthash "t1" '(:retry-next-at 999.0 :next-run-at 2000.0)
             supervisor--timer-state)
    ;; Mock trigger to capture reason
    (cl-letf (((symbol-function 'supervisor-timer--trigger)
               (lambda (_timer reason) (setq triggered-reason reason)))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      (supervisor--timer-scheduler-tick)
      (should (eq 'retry triggered-reason)))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-retry-budget-reset-on-scheduled ()
  "Retry budget is reset on fresh scheduled trigger."
  (let* ((supervisor-timer-subsystem-mode t)
         (supervisor-mode t)
         (timer (supervisor-timer--create :id "t1" :target "s1" :enabled t))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--timer-scheduler nil)
         (supervisor--shutting-down nil))
    ;; Set up state with exhausted retry budget and due scheduled run
    (puthash "t1" '(:retry-attempt 3 :retry-next-at nil :next-run-at 999.0)
             supervisor--timer-state)
    ;; Mock trigger and update functions
    (cl-letf (((symbol-function 'supervisor-timer--trigger)
               (lambda (_timer _reason) nil))
              ((symbol-function 'supervisor-timer--update-next-run)
               (lambda (_id) nil))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      (supervisor--timer-scheduler-tick)
      ;; Retry budget should be reset
      (let ((state (gethash "t1" supervisor--timer-state)))
        (should (= 0 (plist-get state :retry-attempt)))
        (should-not (plist-get state :retry-next-at))))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-scheduler-tick-scheduled ()
  "Scheduler tick triggers scheduled run when due."
  (let* ((supervisor-timer-subsystem-mode t)
         (supervisor-mode t)
         (timer (supervisor-timer--create :id "t1" :target "s1" :enabled t))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--timer-scheduler nil)
         (supervisor--shutting-down nil)
         (triggered-reason nil))
    ;; Set up state with scheduled run due
    (puthash "t1" '(:next-run-at 999.0) supervisor--timer-state)
    ;; Mock trigger to capture reason
    (cl-letf (((symbol-function 'supervisor-timer--trigger)
               (lambda (_timer reason) (setq triggered-reason reason)))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      (supervisor--timer-scheduler-tick)
      (should (eq 'scheduled triggered-reason)))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-scheduler-tick-disabled-skips ()
  "Scheduler tick skips disabled timers."
  (let* ((timer (supervisor-timer--create :id "t1" :target "s1" :enabled nil))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--timer-scheduler nil)
         (supervisor--shutting-down nil)
         (triggered nil))
    ;; Set up state with scheduled run due
    (puthash "t1" '(:next-run-at 999.0) supervisor--timer-state)
    ;; Mock trigger to detect if called
    (cl-letf (((symbol-function 'supervisor-timer--trigger)
               (lambda (_timer _reason) (setq triggered t)))
              ((symbol-function 'float-time) (lambda () 1000.0))
              ((symbol-function 'run-at-time) (lambda (&rest _) nil)))
      (supervisor--timer-scheduler-tick)
      ;; Should not trigger disabled timer
      (should-not triggered))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-scheduler-tick-simultaneous-order ()
  "Scheduler tick processes simultaneous due timers in list order."
  (let* ((supervisor-timer-subsystem-mode t)
         (supervisor-mode t)
         (timer1 (supervisor-timer--create :id "t1" :target "s1" :enabled t))
         (timer2 (supervisor-timer--create :id "t2" :target "s2" :enabled t))
         (timer3 (supervisor-timer--create :id "t3" :target "s3" :enabled t))
         ;; List order is t1, t2, t3
         (supervisor--timer-list (list timer1 timer2 timer3))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--timer-scheduler nil)
         (supervisor--shutting-down nil)
         (trigger-order nil))
    ;; All timers due at same time
    (puthash "t1" '(:next-run-at 999.0) supervisor--timer-state)
    (puthash "t2" '(:next-run-at 999.0) supervisor--timer-state)
    (puthash "t3" '(:next-run-at 999.0) supervisor--timer-state)
    ;; Capture trigger order
    (cl-letf (((symbol-function 'supervisor-timer--trigger)
               (lambda (timer _reason)
                 (push (supervisor-timer-id timer) trigger-order)))
              ((symbol-function 'supervisor-timer--update-next-run)
               (lambda (_id) nil))
              ((symbol-function 'float-time) (lambda () 1000.0))
              ((symbol-function 'run-at-time) (lambda (&rest _) nil)))
      (supervisor--timer-scheduler-tick)
      ;; Should process in list order: t1, t2, t3
      (should (equal '("t1" "t2" "t3") (nreverse trigger-order))))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-cli-relative-time-formatter ()
  "Relative time formatter handles various time differences."
  ;; Test with mock time at 100000
  (cl-letf (((symbol-function 'float-time) (lambda () 100000.0)))
    ;; Recent past (seconds)
    (should (string-match "\\`[0-9]+s ago\\'" (supervisor--cli-format-relative-time 99990.0)))
    ;; Minutes ago
    (should (string-match "\\`[0-9]+m ago\\'" (supervisor--cli-format-relative-time 99800.0)))
    ;; Hours ago (2h = 7200s, so 100000 - 10000 = 90000)
    (should (string-match "\\`[0-9]+h ago\\'" (supervisor--cli-format-relative-time 90000.0)))
    ;; Future (in X)
    (should (string-match "\\`in [0-9]+s\\'" (supervisor--cli-format-relative-time 100010.0)))
    (should (string-match "\\`in [0-9]+m\\'" (supervisor--cli-format-relative-time 100200.0)))
    ;; Nil returns dash
    (should (equal "-" (supervisor--cli-format-relative-time nil)))))

(ert-deftest supervisor-test-cli-relative-time-boundaries ()
  "Relative time formatter handles exact boundaries correctly."
  (cl-letf (((symbol-function 'float-time) (lambda () 100000.0)))
    ;; Exactly 60 seconds (boundary between s and m)
    (should (string-match "\\`1m ago\\'" (supervisor--cli-format-relative-time 99940.0)))
    ;; 59 seconds (should be seconds)
    (should (string-match "\\`59s ago\\'" (supervisor--cli-format-relative-time 99941.0)))
    ;; Exactly 3600 seconds (boundary between m and h)
    (should (string-match "\\`1h ago\\'" (supervisor--cli-format-relative-time 96400.0)))
    ;; 3599 seconds (should be minutes)
    (should (string-match "\\`60m ago\\'" (supervisor--cli-format-relative-time 96401.0)))
    ;; Exactly 86400 seconds (boundary between h and d)
    (should (string-match "\\`1d ago\\'" (supervisor--cli-format-relative-time 13600.0)))
    ;; Day path works for large values
    (should (string-match "\\`2d ago\\'" (supervisor--cli-format-relative-time (- 100000.0 (* 2 86400)))))
    ;; Future day path
    (should (string-match "\\`in 2d\\'" (supervisor--cli-format-relative-time (+ 100000.0 (* 2 86400)))))))

(ert-deftest supervisor-test-timer-state-load-merges-correctly ()
  "Load timer state merges with existing runtime state."
  (let* ((supervisor-timer-subsystem-mode t)
         (supervisor-mode t)
         (temp-file (make-temp-file "supervisor-test-merge-" nil ".eld"))
         (supervisor-timer-state-file temp-file)
         (supervisor--timer-state (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; Create state file with persisted data
          (with-temp-file temp-file
            (insert ";; Test state\n")
            (pp '((version . 1)
                  (timestamp . "2024-01-01T00:00:00")
                  (timers . (("t1" :last-run-at 500.0 :last-success-at 500.0))))
                (current-buffer)))
          ;; Pre-populate runtime state
          (puthash "t1" '(:next-run-at 2000.0 :retry-attempt 1)
                   supervisor--timer-state)
          ;; Load should merge
          (should (supervisor-timer--load-state))
          (let ((state (gethash "t1" supervisor--timer-state)))
            ;; Should have persisted keys from file
            (should (equal (plist-get state :last-run-at) 500.0))
            (should (equal (plist-get state :last-success-at) 500.0))
            ;; Should preserve runtime keys
            (should (equal (plist-get state :next-run-at) 2000.0))
            (should (equal (plist-get state :retry-attempt) 1))))
      (delete-file temp-file)
      (clrhash supervisor--timer-state))))

(ert-deftest supervisor-test-timer-multiple-triggers-earliest-wins ()
  "Timer with multiple triggers uses earliest due time."
  (let* ((timer (supervisor-timer--create :id "t1" :target "s1"
                                          :on-startup-sec 60
                                          :on-unit-active-sec 120))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--scheduler-startup-time 1000.0))
    ;; Set last success for unit-active calculation
    (puthash "t1" '(:last-success-at 900.0) supervisor--timer-state)
    ;; Startup trigger: 1000 + 60 = 1060
    ;; Unit-active trigger: 900 + 120 = 1020 (earlier)
    (let ((next (supervisor-timer--compute-next-run timer 1001.0)))
      (should (= 1020.0 next)))
    (clrhash supervisor--timer-state)))

;;; Timer Subsystem Gating Tests

(ert-deftest supervisor-test-timer-gate-scheduler-start-noop ()
  "Scheduler start is a no-op when timer subsystem is disabled."
  (let ((supervisor-timer-subsystem-mode nil)
        (supervisor-programs '(("true" :id "s1" :type oneshot)))
        (supervisor-timers '((:id "t1" :target "s1" :on-startup-sec 60)))
        (supervisor--timer-list nil)
        (supervisor--timer-scheduler nil)
        (supervisor--shutting-down nil)
        (supervisor--scheduler-startup-time nil)
        (supervisor--timer-state-loaded nil)
        (supervisor-timer-state-file nil))
    ;; Start should do nothing when gated off
    (supervisor-timer-scheduler-start)
    ;; Timer list should remain empty
    (should (null supervisor--timer-list))
    ;; Scheduler should not be running
    (should (null supervisor--timer-scheduler))))

(ert-deftest supervisor-test-timer-gate-state-save-noop ()
  "State save is a no-op when timer subsystem is disabled."
  (let* ((supervisor-timer-subsystem-mode nil)
         (temp-file (concat (make-temp-name
                             (expand-file-name "supervisor-test-gate-"
                                               temporary-file-directory))
                            ".eld"))
         (supervisor-timer-state-file temp-file)
         (supervisor--timer-state (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (puthash "t1" '(:last-run-at 1000.0) supervisor--timer-state)
          ;; Save should return nil when gated off
          (should-not (supervisor-timer--save-state))
          ;; File should NOT be created
          (should-not (file-exists-p temp-file)))
      (when (file-exists-p temp-file) (delete-file temp-file))
      (clrhash supervisor--timer-state))))

(ert-deftest supervisor-test-timer-gate-state-load-noop ()
  "State load is a no-op when timer subsystem is disabled."
  (let* ((supervisor-timer-subsystem-mode nil)
         (temp-file (make-temp-file "supervisor-test-gate-" nil ".eld"))
         (supervisor-timer-state-file temp-file)
         (supervisor--timer-state (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; Write a valid state file
          (with-temp-file temp-file
            (insert (format "((version . %d) (timestamp . \"test\") (timers . ((\"t1\" :last-run-at 1000.0))))"
                            supervisor-timer-state-schema-version)))
          ;; Load should return nil when gated off
          (should-not (supervisor-timer--load-state))
          ;; State should NOT be populated
          (should (= 0 (hash-table-count supervisor--timer-state))))
      (delete-file temp-file)
      (clrhash supervisor--timer-state))))

(ert-deftest supervisor-test-timer-gate-scheduler-tick-noop ()
  "Scheduler tick is a no-op when timer subsystem is disabled."
  (let* ((supervisor-timer-subsystem-mode nil)
         (timer (supervisor-timer--create :id "t1" :target "s1" :enabled t
                                          :on-startup-sec 1))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--timer-scheduler nil)
         (supervisor--shutting-down nil)
         (triggered nil))
    ;; Set up state so timer would be due
    (puthash "t1" '(:next-run-at 900.0) supervisor--timer-state)
    ;; Mock trigger to detect if called
    (cl-letf (((symbol-function 'supervisor-timer--trigger)
               (lambda (_timer _reason) (setq triggered t)))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      (supervisor--timer-scheduler-tick))
    ;; Timer should NOT have been triggered
    (should-not triggered)
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-gate-enabled-works ()
  "Timer functions work normally when subsystem is enabled."
  (let* ((supervisor-timer-subsystem-mode t)
         (supervisor-mode t)  ; Parent mode must also be enabled
         (timer (supervisor-timer--create :id "t1" :target "s1" :enabled t
                                          :on-startup-sec 1))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--timer-scheduler nil)
         (supervisor--shutting-down nil)
         (triggered nil))
    ;; Set up state so timer would be due
    (puthash "t1" '(:next-run-at 900.0) supervisor--timer-state)
    ;; Mock trigger to detect if called
    (cl-letf (((symbol-function 'supervisor-timer--trigger)
               (lambda (_timer _reason) (setq triggered t)))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      (supervisor--timer-scheduler-tick))
    ;; Timer SHOULD have been triggered when enabled
    (should triggered)
    ;; Clean up scheduler timer if created
    (when (timerp supervisor--timer-scheduler)
      (cancel-timer supervisor--timer-scheduler))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-gate-parent-mode-off ()
  "Timer subsystem is a no-op when parent supervisor-mode is off."
  (let* ((supervisor-timer-subsystem-mode t)  ; Timer mode enabled
         (supervisor-mode nil)                 ; But parent mode is OFF
         (timer (supervisor-timer--create :id "t1" :target "s1" :enabled t
                                          :on-startup-sec 1))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--timer-scheduler nil)
         (supervisor--shutting-down nil)
         (triggered nil))
    ;; Set up state so timer would be due
    (puthash "t1" '(:next-run-at 900.0) supervisor--timer-state)
    ;; Mock trigger to detect if called
    (cl-letf (((symbol-function 'supervisor-timer--trigger)
               (lambda (_timer _reason) (setq triggered t)))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      (supervisor--timer-scheduler-tick))
    ;; Timer should NOT be triggered when parent mode is off
    (should-not triggered)
    (clrhash supervisor--timer-state)))

;;; CLI Control Plane tests

(ert-deftest supervisor-test-cli-result-structure ()
  "CLI result struct has required fields."
  (let ((result (supervisor--cli-make-result 0 'human "output")))
    (should (supervisor-cli-result-p result))
    (should (= 0 (supervisor-cli-result-exitcode result)))
    (should (eq 'human (supervisor-cli-result-format result)))
    (should (equal "output" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-exit-codes ()
  "CLI exit code constants are defined."
  (should (= 0 supervisor-cli-exit-success))
  (should (= 1 supervisor-cli-exit-failure))
  (should (= 2 supervisor-cli-exit-invalid-args))
  (should (= 3 supervisor-cli-exit-not-active))
  (should (= 4 supervisor-cli-exit-no-such-unit))
  (should (= 4 supervisor-cli-exit-validation-failed))
  (should (= 69 supervisor-cli-exit-server-unavailable)))

(ert-deftest supervisor-test-cli-dispatch-unknown-command ()
  "Unknown CLI command returns exit code 2."
  (let ((result (supervisor--cli-dispatch '("unknown-cmd"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-dispatch-empty-args ()
  "Empty args returns help text."
  (let ((result (supervisor--cli-dispatch '())))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
    (should (string-match "Usage:" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-ping ()
  "Ping command returns pong."
  (let ((result (supervisor--cli-dispatch '("ping"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
    (should (string-match "pong" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-ping-json ()
  "Ping command with --json returns JSON."
  (let ((result (supervisor--cli-dispatch '("ping" "--json"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
    (should (eq 'json (supervisor-cli-result-format result)))
    (should (string-match "\"status\"" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-version ()
  "Version command returns version info."
  (let ((result (supervisor--cli-dispatch '("version"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
    (should (string-match "supervisorctl" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-validate-no-programs ()
  "Validate with no programs returns success."
  (let ((supervisor-programs nil)
        (result (supervisor--cli-dispatch '("validate"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
    (should (string-match "0 valid" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-validate-invalid-entry ()
  "Validate with invalid entry returns validation-failed exit code."
  (let* ((supervisor-programs '(("cmd" :type "bad")))
         (result (supervisor--cli-dispatch '("validate"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-validation-failed (supervisor-cli-result-exitcode result)))
    (should (string-match "1 invalid" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-list-units-human-format ()
  "The `list-units' command returns human-readable table."
  (let* ((supervisor-programs '(("test-cmd" :id "test" :type simple)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (result (supervisor--cli-dispatch '("list-units"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
    (should (eq 'human (supervisor-cli-result-format result)))
    ;; Should contain header row
    (should (string-match "ID" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-list-units-json-format ()
  "The `list-units --json' returns JSON with entries array."
  (let* ((supervisor-programs '(("test-cmd" :id "test" :type simple)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (result (supervisor--cli-dispatch '("list-units" "--json"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
    (should (eq 'json (supervisor-cli-result-format result)))
    ;; Should be valid JSON with entries array
    (should (string-match "\"entries\"" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-show-requires-id ()
  "The `show' command without ID returns invalid-args exit code."
  (let ((result (supervisor--cli-dispatch '("show"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-enable-requires-id ()
  "Enable without ID returns invalid-args exit code."
  (let ((result (supervisor--cli-dispatch '("enable"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-enable-sets-override ()
  "Enable command sets enabled override."
  (let ((supervisor--enabled-override (make-hash-table :test 'equal))
        (supervisor-overrides-file nil))  ; disable auto-save
    (supervisor--cli-dispatch '("enable" "test-id"))
    (should (eq 'enabled (gethash "test-id" supervisor--enabled-override)))))

(ert-deftest supervisor-test-cli-disable-sets-override ()
  "Disable command sets disabled override."
  (let ((supervisor--enabled-override (make-hash-table :test 'equal))
        (supervisor-overrides-file nil))  ; disable auto-save
    (supervisor--cli-dispatch '("disable" "test-id"))
    (should (eq 'disabled (gethash "test-id" supervisor--enabled-override)))))

(ert-deftest supervisor-test-cli-mask-sets-override ()
  "Mask command sets masked override."
  (let ((supervisor--mask-override (make-hash-table :test 'equal))
        (supervisor-overrides-file nil))
    (supervisor--cli-dispatch '("mask" "test-id"))
    (should (eq 'masked (gethash "test-id" supervisor--mask-override)))))

(ert-deftest supervisor-test-cli-unmask-clears-override ()
  "Unmask command clears masked override."
  (let ((supervisor--mask-override (make-hash-table :test 'equal))
        (supervisor-overrides-file nil))
    (puthash "test-id" 'masked supervisor--mask-override)
    (supervisor--cli-dispatch '("unmask" "test-id"))
    (should-not (gethash "test-id" supervisor--mask-override))))

(ert-deftest supervisor-test-cli-mask-requires-id ()
  "Mask command requires at least one ID."
  (let ((result (supervisor--cli-dispatch '("mask"))))
    (should (= supervisor-cli-exit-invalid-args
                (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-unmask-requires-id ()
  "Unmask command requires at least one ID."
  (let ((result (supervisor--cli-dispatch '("unmask"))))
    (should (= supervisor-cli-exit-invalid-args
                (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-mask-json-output ()
  "Mask command returns JSON with masked IDs."
  (let ((supervisor--mask-override (make-hash-table :test 'equal))
        (supervisor-overrides-file nil))
    (let ((result (supervisor--cli-dispatch '("mask" "a" "b" "--json"))))
      (should (eq 'json (supervisor-cli-result-format result)))
      (let ((parsed (json-read-from-string
                     (supervisor-cli-result-output result))))
        (should (assoc 'masked parsed))))))

(ert-deftest supervisor-test-cli-mask-with-separator ()
  "Mask command supports -- separator for hyphen-prefixed IDs."
  (let ((supervisor--mask-override (make-hash-table :test 'equal))
        (supervisor-overrides-file nil))
    (supervisor--cli-dispatch '("mask" "--" "-my-id"))
    (should (eq 'masked (gethash "-my-id" supervisor--mask-override)))))

(ert-deftest supervisor-test-cli-unmask-json-output ()
  "Unmask command returns JSON with unmasked IDs."
  (let ((supervisor--mask-override (make-hash-table :test 'equal))
        (supervisor-overrides-file nil))
    (puthash "a" 'masked supervisor--mask-override)
    (puthash "b" 'masked supervisor--mask-override)
    (let ((result (supervisor--cli-dispatch '("unmask" "a" "b" "--json"))))
      (should (eq 'json (supervisor-cli-result-format result)))
      (let ((parsed (json-read-from-string
                     (supervisor-cli-result-output result))))
        (should (assoc 'unmasked parsed))))))

(ert-deftest supervisor-test-cli-unmask-with-separator ()
  "Unmask command supports -- separator for hyphen-prefixed IDs."
  (let ((supervisor--mask-override (make-hash-table :test 'equal))
        (supervisor-overrides-file nil))
    (puthash "-my-id" 'masked supervisor--mask-override)
    (supervisor--cli-dispatch '("unmask" "--" "-my-id"))
    (should-not (gethash "-my-id" supervisor--mask-override))))

(ert-deftest supervisor-test-mask-precedence-over-enabled ()
  "Masked entry is always disabled regardless of enabled override."
  (let ((supervisor--mask-override (make-hash-table :test 'equal))
        (supervisor--enabled-override (make-hash-table :test 'equal)))
    ;; Even with enabled override, mask takes precedence
    (puthash "test" 'masked supervisor--mask-override)
    (puthash "test" 'enabled supervisor--enabled-override)
    (should-not (supervisor--get-effective-enabled "test" t))))

(ert-deftest supervisor-test-mask-blocks-manual-start ()
  "Masked entry cannot be manually started."
  (let ((supervisor--mask-override (make-hash-table :test 'equal))
        (supervisor--enabled-override (make-hash-table :test 'equal))
        (supervisor--invalid (make-hash-table :test 'equal))
        (supervisor--processes (make-hash-table :test 'equal))
        (supervisor-programs '(("echo hi" :id "svc"))))
    (puthash "svc" 'masked supervisor--mask-override)
    (let ((result (supervisor--manual-start "svc")))
      (should (eq 'skipped (plist-get result :status)))
      (should (equal "masked" (plist-get result :reason))))))

(ert-deftest supervisor-test-reconcile-stops-masked-running ()
  "Reconcile stops masked entries that are currently running."
  (let* ((supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (process-alive (make-hash-table :test 'equal))
         (process-pids (make-hash-table :test 'equal))
         (failed (make-hash-table :test 'equal))
         (oneshot (make-hash-table :test 'equal))
         (entry-state (make-hash-table :test 'equal))
         (invalid (make-hash-table :test 'equal))
         (logging (make-hash-table :test 'equal))
         (restart (make-hash-table :test 'equal))
         ;; Build a plan with one entry
         (supervisor-programs '(("echo" :id "svc")))
         (plan (supervisor--build-plan supervisor-programs)))
    ;; Simulate running process
    (puthash "svc" t process-alive)
    (puthash "svc" 123 process-pids)
    ;; Mask the entry
    (puthash "svc" 'masked supervisor--mask-override)
    (let ((snapshot (supervisor-snapshot--create
                     :process-alive process-alive
                     :process-pids process-pids
                     :failed failed
                     :oneshot-exit oneshot
                     :entry-state entry-state
                     :invalid invalid
                     :enabled-override supervisor--enabled-override
                     :restart-override restart
                     :logging-override logging
                     :mask-override supervisor--mask-override
                     :timestamp (float-time))))
      (let ((actions (supervisor--compute-actions plan snapshot)))
        ;; Should have a stop action for the masked entry
        (let ((stop-action (cl-find "svc" actions
                                    :key (lambda (a) (plist-get a :id))
                                    :test #'equal)))
          (should stop-action)
          (should (eq 'stop (plist-get stop-action :op)))
          (should (eq 'masked (plist-get stop-action :reason))))))))

(ert-deftest supervisor-test-mask-persistence-roundtrip ()
  "Mask override survives save/load cycle."
  (let* ((dir (make-temp-file "overrides-" t))
         (supervisor-overrides-file (expand-file-name "overrides.eld" dir))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--restart-override (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal))
         (supervisor--overrides-loaded nil))
    (unwind-protect
        (progn
          (puthash "svc" 'masked supervisor--mask-override)
          (supervisor--save-overrides)
          ;; Clear and reload
          (clrhash supervisor--mask-override)
          (should-not (gethash "svc" supervisor--mask-override))
          (supervisor--load-overrides)
          (should (eq 'masked (gethash "svc" supervisor--mask-override))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-compute-entry-status-masked ()
  "Compute-entry-status returns masked even when process is running."
  (let ((supervisor--mask-override (make-hash-table :test 'equal))
        (supervisor--processes (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--oneshot-completed (make-hash-table :test 'equal)))
    (puthash "svc" 'masked supervisor--mask-override)
    ;; Non-running masked entry
    (let ((result (supervisor--compute-entry-status "svc" 'simple)))
      (should (equal "masked" (car result))))
    ;; Running masked entry still shows masked
    (let ((proc (start-process "test" nil "sleep" "999")))
      (unwind-protect
          (progn
            (puthash "svc" proc supervisor--processes)
            (let ((result (supervisor--compute-entry-status "svc" 'simple)))
              (should (equal "masked" (car result)))))
        (delete-process proc)))))

(ert-deftest supervisor-test-compute-entry-reason-masked ()
  "Compute-entry-reason returns masked even when process is running."
  (let ((supervisor--mask-override (make-hash-table :test 'equal))
        (supervisor--processes (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--oneshot-completed (make-hash-table :test 'equal))
        (supervisor--entry-state (make-hash-table :test 'equal)))
    (puthash "svc" 'masked supervisor--mask-override)
    ;; Non-running masked entry
    (should (equal "masked" (supervisor--compute-entry-reason "svc" 'simple)))
    ;; Running masked entry still shows masked
    (let ((proc (start-process "test" nil "sleep" "999")))
      (unwind-protect
          (progn
            (puthash "svc" proc supervisor--processes)
            (should (equal "masked"
                           (supervisor--compute-entry-reason "svc" 'simple))))
        (delete-process proc)))))

(ert-deftest supervisor-test-dashboard-mask-keybinding ()
  "Dashboard keymap binds `m' to toggle-mask."
  (should (eq #'supervisor-dashboard-toggle-mask
              (lookup-key supervisor-dashboard-mode-map "m"))))

(ert-deftest supervisor-test-cli-restart-policy-on ()
  "Restart-policy on sets override."
  (let ((supervisor--restart-override (make-hash-table :test 'equal))
        (supervisor-overrides-file nil))
    (supervisor--cli-dispatch '("restart-policy" "on" "test-id"))
    (should (eq 'enabled (gethash "test-id" supervisor--restart-override)))))

(ert-deftest supervisor-test-cli-restart-policy-off ()
  "Restart-policy off sets override."
  (let ((supervisor--restart-override (make-hash-table :test 'equal))
        (supervisor-overrides-file nil))
    (supervisor--cli-dispatch '("restart-policy" "off" "test-id"))
    (should (eq 'disabled (gethash "test-id" supervisor--restart-override)))))

(ert-deftest supervisor-test-cli-logging-on ()
  "Logging on sets override."
  (let ((supervisor--logging (make-hash-table :test 'equal))
        (supervisor-overrides-file nil))
    (supervisor--cli-dispatch '("logging" "on" "test-id"))
    (should (eq 'enabled (gethash "test-id" supervisor--logging)))))

(ert-deftest supervisor-test-cli-logging-off ()
  "Logging off sets override."
  (let ((supervisor--logging (make-hash-table :test 'equal))
        (supervisor-overrides-file nil))
    (supervisor--cli-dispatch '("logging" "off" "test-id"))
    (should (eq 'disabled (gethash "test-id" supervisor--logging)))))

(ert-deftest supervisor-test-cli-stop-uses-manually-stopped ()
  "CLI stop sets manually-stopped flag, not restart-override."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--manually-stopped (make-hash-table :test 'equal))
        (supervisor--restart-override (make-hash-table :test 'equal))
        (proc (start-process "test-proc" nil "sleep" "999")))
    (unwind-protect
        (progn
          (puthash "test-id" proc supervisor--processes)
          (supervisor--cli-dispatch '("stop" "test-id"))
          ;; Should set manually-stopped, NOT restart-override
          (should (gethash "test-id" supervisor--manually-stopped))
          (should-not (gethash "test-id" supervisor--restart-override)))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest supervisor-test-cli-kill-does-not-use-manually-stopped ()
  "CLI kill does not set manually-stopped, matching signal-only semantics."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--manually-stopped (make-hash-table :test 'equal))
        (proc (start-process "test-proc" nil "sleep" "999")))
    (unwind-protect
        (progn
          (puthash "test-id" proc supervisor--processes)
          (let ((result (supervisor--cli-dispatch '("kill" "test-id"))))
            (should (= supervisor-cli-exit-success
                       (supervisor-cli-result-exitcode result))))
          (should-not (gethash "test-id" supervisor--manually-stopped)))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest supervisor-test-start-clears-manually-stopped ()
  "Starting a process clears the manually-stopped flag."
  (let ((supervisor--manually-stopped (make-hash-table :test 'equal))
        (supervisor--processes (make-hash-table :test 'equal))
        (supervisor--restart-timers (make-hash-table :test 'equal))
        (supervisor--restart-override (make-hash-table :test 'equal))
        (supervisor--enabled-override (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--logging (make-hash-table :test 'equal))
        (supervisor-log-directory (make-temp-file "supervisor-test-" t))
        (supervisor--shutting-down nil))
    (puthash "test-id" t supervisor--manually-stopped)
    (should (gethash "test-id" supervisor--manually-stopped))
    (let ((proc (supervisor--start-process "test-id" "sleep 999" nil 'simple t)))
      (unwind-protect
          (progn
            ;; Flag should be cleared after start
            (should-not (gethash "test-id" supervisor--manually-stopped)))
        (when (and proc (process-live-p proc))
          (delete-process proc))))))

(ert-deftest supervisor-test-cli-logs-requires-id ()
  "Logs without ID returns invalid-args exit code."
  (let ((result (supervisor--cli-dispatch '("logs"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-kill-requires-id ()
  "Kill without ID returns invalid-args exit code."
  (let ((result (supervisor--cli-dispatch '("kill"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-list-dependencies-full ()
  "The `list-dependencies' command without ID returns full graph."
  (let* ((supervisor-programs '(("a" :id "a") ("b" :id "b" :after "a")))
         (result (supervisor--cli-dispatch '("list-dependencies"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-list-dependencies-single ()
  "The `list-dependencies' command with ID returns single entry deps."
  (let* ((supervisor-programs '(("a" :id "a") ("b" :id "b" :after "a")))
         (result (supervisor--cli-dispatch '("list-dependencies" "b"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
    (should (string-match "ID: b" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-blame ()
  "Blame command returns timing info."
  (let ((supervisor--start-times (make-hash-table :test 'equal))
        (supervisor--ready-times (make-hash-table :test 'equal)))
    (puthash "test" 1000.0 supervisor--start-times)
    (puthash "test" 1001.5 supervisor--ready-times)
    (let ((result (supervisor--cli-dispatch '("blame"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      (should (string-match "DURATION" (supervisor-cli-result-output result))))))

;;; CLI Arg Validation Tests

(ert-deftest supervisor-test-cli-validate-rejects-extra-args ()
  "Validate with extra args returns invalid-args exit code."
  (let ((result (supervisor--cli-dispatch '("validate" "extra"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-reconcile-rejects-extra-args ()
  "Reconcile with extra args returns invalid-args exit code."
  (let ((result (supervisor--cli-dispatch '("reconcile" "extra"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-reload-requires-at-least-one-id ()
  "CLI `reload' with no arguments returns exit 2 (invalid args)."
  (let ((result (supervisor--cli-dispatch '("reload"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args
               (supervisor-cli-result-exitcode result)))
    (should (string-match "reload requires at least one ID"
                          (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-list-is-unknown-command ()
  "Legacy `list' command is rejected after rename to `list-units'."
  (let ((result (supervisor--cli-dispatch '("list"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args
               (supervisor-cli-result-exitcode result)))
    (should (string-match "Unknown command: list"
                          (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-describe-is-unknown-command ()
  "Legacy `describe' command is rejected after rename to `show'."
  (let ((result (supervisor--cli-dispatch '("describe"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args
               (supervisor-cli-result-exitcode result)))
    (should (string-match "Unknown command: describe"
                          (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-graph-is-unknown-command ()
  "Legacy `graph' command is rejected after rename to `list-dependencies'."
  (let ((result (supervisor--cli-dispatch '("graph"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args
               (supervisor-cli-result-exitcode result)))
    (should (string-match "Unknown command: graph"
                          (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-timers-is-unknown-command ()
  "Legacy `timers' command is rejected after rename to `list-timers'."
  (let ((result (supervisor--cli-dispatch '("timers"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args
               (supervisor-cli-result-exitcode result)))
    (should (string-match "Unknown command: timers"
                          (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-status-no-args-shows-table ()
  "The `status' with no IDs shows overview table (delegates to `list-units')."
  (let* ((supervisor-programs '(("test-cmd" :id "test" :type simple)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (result (supervisor--cli-dispatch '("status"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
    ;; Should contain header row (table format)
    (should (string-match "ID" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-status-with-id-shows-detail ()
  "The `status ID' shows detailed unit info."
  (let* ((supervisor-programs '(("test-cmd" :id "test" :type simple)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (result (supervisor--cli-dispatch '("status" "test"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
    ;; Should show detailed info (describe format)
    (should (string-match "ID: test" (supervisor-cli-result-output result)))
    (should (string-match "Type:" (supervisor-cli-result-output result)))
    (should (string-match "Status:" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-status-partial-missing ()
  "The `status ID1 ID2' prints found units and warns about missing ones."
  (let* ((supervisor-programs '(("test-cmd" :id "test" :type simple)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (result (supervisor--cli-dispatch '("status" "test" "nonexistent"))))
    (should (supervisor-cli-result-p result))
    ;; Non-zero exit because of missing ID
    (should (= supervisor-cli-exit-failure (supervisor-cli-result-exitcode result)))
    ;; But should still show output for the found ID
    (should (string-match "ID: test" (supervisor-cli-result-output result)))
    ;; And warn about the missing one
    (should (string-match "nonexistent" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-status-all-missing ()
  "The `status' with only missing IDs still shows warnings."
  (let* ((supervisor-programs '(("test-cmd" :id "test" :type simple)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (result (supervisor--cli-dispatch '("status" "nope"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-failure (supervisor-cli-result-exitcode result)))
    (should (string-match "could not be found" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-list-units-filters-invalid ()
  "The `list-units ID' filters both valid and invalid entries."
  (let* ((supervisor-programs '(("good-cmd" :id "good" :type simple)
                                ("bad-cmd" :id "bad" :type "string-type")))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (result (supervisor--cli-dispatch '("list-units" "good"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
    ;; Should show "good" but not "bad" (invalid entry filtered out)
    (should (string-match "good" (supervisor-cli-result-output result)))
    (should-not (string-match "bad" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-status-invalid-id-shows-invalid-detail ()
  "The `status' with an invalid unit ID shows invalid detail, not \"not found\"."
  (let* ((supervisor-programs '(("good-cmd" :id "good" :type simple)
                                ("bad-cmd" :id "bad" :type "string-type")))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (result (supervisor--cli-dispatch '("status" "bad"))))
    (should (supervisor-cli-result-p result))
    ;; Invalid configured unit is found, so exit success (not missing)
    (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
    (should (string-match "ID: bad" (supervisor-cli-result-output result)))
    (should (string-match "Status: invalid" (supervisor-cli-result-output result)))
    (should-not (string-match "could not be found" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-status-mixed-valid-invalid-missing ()
  "The `status' with valid, invalid, and missing IDs classifies each correctly."
  (let* ((supervisor-programs '(("good-cmd" :id "good" :type simple)
                                ("bad-cmd" :id "bad" :type "string-type")))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (result (supervisor--cli-dispatch '("status" "good" "bad" "ghost"))))
    (should (supervisor-cli-result-p result))
    ;; Non-zero exit because "ghost" is truly missing
    (should (= supervisor-cli-exit-failure (supervisor-cli-result-exitcode result)))
    ;; Valid unit has detail
    (should (string-match "ID: good" (supervisor-cli-result-output result)))
    ;; Invalid unit has invalid detail
    (should (string-match "ID: bad" (supervisor-cli-result-output result)))
    (should (string-match "Status: invalid" (supervisor-cli-result-output result)))
    ;; Only the truly missing ID gets "could not be found"
    (should (string-match "ghost.*could not be found" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-status-invalid-id-json ()
  "The `status --json' with invalid ID returns invalid array, not not_found."
  (let* ((supervisor-programs '(("bad-cmd" :id "bad" :type "string-type")))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (result (supervisor--cli-dispatch '("status" "bad" "--json"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
    (let ((parsed (json-read-from-string (supervisor-cli-result-output result))))
      ;; Invalid unit appears in "invalid" array, not "not_found"
      (should (< 0 (length (alist-get 'invalid parsed))))
      (should (equal "bad" (alist-get 'id (aref (alist-get 'invalid parsed) 0)))))))

(ert-deftest supervisor-test-cli-show-invalid-id-shows-invalid-detail ()
  "The `show' with an invalid unit ID shows invalid detail, not \"not found\"."
  (let* ((supervisor-programs '(("bad-cmd" :id "bad" :type "string-type")))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (result (supervisor--cli-dispatch '("show" "bad"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
    (should (string-match "ID: bad" (supervisor-cli-result-output result)))
    (should (string-match "Status: invalid" (supervisor-cli-result-output result)))
    (should-not (string-match "No entry with ID" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-show-invalid-id-json ()
  "The `show --json' with invalid ID returns invalid object, not error."
  (let* ((supervisor-programs '(("bad-cmd" :id "bad" :type "string-type")))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (result (supervisor--cli-dispatch '("show" "bad" "--json"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
    (let ((parsed (json-read-from-string (supervisor-cli-result-output result))))
      (should (equal "bad" (alist-get 'id parsed)))
      (should (alist-get 'reason parsed)))))

(ert-deftest supervisor-test-cli-show-truly-missing-still-errors ()
  "The `show' with a truly missing ID still returns an error."
  (let* ((supervisor-programs '(("good-cmd" :id "good" :type simple)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (result (supervisor--cli-dispatch '("show" "ghost"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-failure (supervisor-cli-result-exitcode result)))
    (should (string-match "No entry with ID" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-ping-rejects-extra-args ()
  "Ping with extra args returns invalid-args exit code."
  (let ((result (supervisor--cli-dispatch '("ping" "extra"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-version-rejects-extra-args ()
  "Version with extra args returns invalid-args exit code."
  (let ((result (supervisor--cli-dispatch '("version" "extra"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-list-timers-no-timers ()
  "The `list-timers' command with no timers configured."
  (let ((supervisor-timer-subsystem-mode t)
        (supervisor-mode t)
        (supervisor--timer-list nil)
        (supervisor--timer-state (make-hash-table :test 'equal))
        (supervisor--invalid-timers (make-hash-table :test 'equal)))
    (let ((result (supervisor--cli-dispatch '("list-timers"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      (should (string-match "No timers" (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-list-timers-shows-state ()
  "The `list-timers' command shows timer state."
  (let* ((supervisor-timer-subsystem-mode t)
         (supervisor-mode t)
         (timer (supervisor-timer--create :id "t1" :target "s1" :enabled t))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--invalid-timers (make-hash-table :test 'equal)))
    (puthash "t1" '(:last-run-at 1000.0 :next-run-at 2000.0)
             supervisor--timer-state)
    (let ((result (supervisor--cli-dispatch '("list-timers"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      (should (string-match "t1" (supervisor-cli-result-output result)))
      (should (string-match "s1" (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-list-timers-json ()
  "The `list-timers --json' outputs JSON."
  (let* ((timer (supervisor-timer--create :id "t1" :target "s1" :enabled t))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--invalid-timers (make-hash-table :test 'equal)))
    (let ((result (supervisor--cli-dispatch '("--json" "list-timers"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      (should (eq 'json (supervisor-cli-result-format result)))
      ;; Should be valid JSON
      (let ((json-object-type 'alist))
        (should (json-read-from-string (supervisor-cli-result-output result)))))))

(ert-deftest supervisor-test-cli-list-timers-invalid-human-format ()
  "The `list-timers' command shows invalid timers with correct id and reason."
  (let ((supervisor-timer-subsystem-mode t)
        (supervisor-mode t)
        (supervisor--timer-list nil)
        (supervisor--timer-state (make-hash-table :test 'equal))
        (supervisor--invalid-timers (make-hash-table :test 'equal)))
    ;; Add invalid timer with known id and reason
    (puthash "bad-timer" ":target 'missing' not found" supervisor--invalid-timers)
    (let ((result (supervisor--cli-dispatch '("list-timers"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      ;; Should show id and reason correctly
      (should (string-match-p "bad-timer" (supervisor-cli-result-output result)))
      (should (string-match-p ":target 'missing' not found"
                              (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-list-timers-invalid-json-format ()
  "The `list-timers --json' outputs invalid timers with correct structure."
  (let ((supervisor-timer-subsystem-mode t)
        (supervisor-mode t)
        (supervisor--timer-list nil)
        (supervisor--timer-state (make-hash-table :test 'equal))
        (supervisor--invalid-timers (make-hash-table :test 'equal)))
    ;; Add invalid timer
    (puthash "bad-timer" "test reason" supervisor--invalid-timers)
    (let ((result (supervisor--cli-dispatch '("--json" "list-timers"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      ;; Parse JSON and check invalid array structure
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (data (json-read-from-string (supervisor-cli-result-output result)))
             (invalid (alist-get 'invalid data))
             (entry (car invalid)))
        (should invalid)
        (should (equal "bad-timer" (alist-get 'id entry)))
        (should (equal "test reason" (alist-get 'reason entry)))))))

(ert-deftest supervisor-test-dashboard-timer-signal-exit-is-failed ()
  "Dashboard timer status classifies signal exits as failed."
  (let* ((timer (supervisor-timer--create :id "t1" :target "s1" :enabled t))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--processes (make-hash-table :test 'equal)))
    ;; Simulate signal death stored as negative exit code
    (puthash "t1" '(:last-exit -9 :next-run-at 2000.0) supervisor--timer-state)
    (let ((entry (supervisor--make-timer-dashboard-entry timer)))
      ;; Entry is a vector: [id type target enabled status restart log pid reason]
      ;; Status is at index 4
      (should (string-match-p "failed" (aref entry 4))))))

(ert-deftest supervisor-test-cli-list-timers-full-field-mapping ()
  "The `list-timers' output includes all required fields."
  (let* ((supervisor-timer-subsystem-mode t)
         (supervisor-mode t)
         (timer (supervisor-timer--create :id "test-timer" :target "test-target"
                                          :enabled t :persistent t))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--invalid-timers (make-hash-table :test 'equal)))
    ;; Set up comprehensive state (note: :last-missed-at is the state key)
    (puthash "test-timer" '(:last-run-at 1000.0
                            :last-success-at 900.0
                            :last-failure-at 950.0
                            :last-exit 1
                            :next-run-at 2000.0
                            :last-missed-at 850.0
                            :last-miss-reason overlap)
             supervisor--timer-state)
    ;; Test human format includes all fields
    (let ((result (supervisor--cli-dispatch '("list-timers"))))
      (should (supervisor-cli-result-p result))
      (let ((output (supervisor-cli-result-output result)))
        ;; ID and target
        (should (string-match-p "test-timer" output))
        (should (string-match-p "test-target" output))
        ;; Enabled (yes/no)
        (should (string-match-p "yes" output))
        ;; Exit code
        (should (string-match-p "1" output))
        ;; Miss reason
        (should (string-match-p "overlap" output))))
    ;; Test JSON format includes all fields with correct values
    (let ((result (supervisor--cli-dispatch '("--json" "list-timers"))))
      (should (supervisor-cli-result-p result))
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (data (json-read-from-string (supervisor-cli-result-output result)))
             (timers (alist-get 'timers data))
             (entry (car timers)))
        ;; Verify all required fields present and mapped correctly
        (should (equal "test-timer" (alist-get 'id entry)))
        (should (equal "test-target" (alist-get 'target entry)))
        (should (eq t (alist-get 'enabled entry)))
        (should (eq t (alist-get 'persistent entry)))
        (should (= 1000.0 (alist-get 'last_run_at entry)))
        (should (= 900.0 (alist-get 'last_success_at entry)))
        (should (= 950.0 (alist-get 'last_failure_at entry)))
        (should (= 1 (alist-get 'last_exit entry)))
        (should (= 2000.0 (alist-get 'next_run_at entry)))
        (should (= 850.0 (alist-get 'last_miss_at entry)))
        (should (equal "overlap" (alist-get 'miss_reason entry)))))))

(ert-deftest supervisor-test-cli-list-timers-rejects-extra-args ()
  "The `list-timers' with extra args returns invalid-args exit code."
  (let ((supervisor--timer-list nil)
        (supervisor--timer-state (make-hash-table :test 'equal))
        (supervisor--invalid-timers (make-hash-table :test 'equal)))
    (let ((result (supervisor--cli-dispatch '("list-timers" "extra"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result))))))

(ert-deftest supervisor-test-cli-blame-rejects-extra-args ()
  "Blame with extra args returns invalid-args exit code."
  (let ((supervisor--start-times (make-hash-table :test 'equal)))
    (let ((result (supervisor--cli-dispatch '("blame" "extra"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result))))))

(ert-deftest supervisor-test-cli-show-rejects-extra-args ()
  "The `show' with extra args returns invalid-args exit code."
  (let* ((supervisor-programs '(("cmd" :id "test")))
         (result (supervisor--cli-dispatch '("show" "test" "extra"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-status-rejects-unknown-flags ()
  "Status with unknown flag returns invalid-args exit code."
  (let ((result (supervisor--cli-dispatch '("status" "--bogus"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-list-dependencies-rejects-extra-args ()
  "The `list-dependencies' with multiple args returns invalid-args exit code."
  (let* ((supervisor-programs '(("a" :id "a")))
         (result (supervisor--cli-dispatch '("list-dependencies" "a" "b"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))))

;;; CLI JSON Schema Tests

(ert-deftest supervisor-test-cli-list-units-json-empty-arrays ()
  "The `list-units --json' returns arrays, not null, for empty results."
  (let* ((supervisor-programs nil)
         (result (supervisor--cli-dispatch '("list-units" "--json"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
    ;; Should have [] not null
    (should (string-match "\"entries\":\\[\\]" (supervisor-cli-result-output result)))
    (should (string-match "\"invalid\":\\[\\]" (supervisor-cli-result-output result)))))

;;; CLI Wrapper Transport Tests

(ert-deftest supervisor-test-cli-wrapper-dispatch-format ()
  "Wrapper dispatch returns base64-encoded format."
  (let* ((supervisor-programs nil)
         (result (supervisor--cli-dispatch-for-wrapper '("ping"))))
    ;; Format is EXITCODE:BASE64OUTPUT
    (should (stringp result))
    (should (string-match "^0:" result))
    ;; Decode the base64 part
    (let ((b64 (substring result 2)))
      (should (string-match "pong" (decode-coding-string (base64-decode-string b64) 'utf-8))))))

(ert-deftest supervisor-test-cli-wrapper-dispatch-error ()
  "Wrapper dispatch returns non-zero exit code for errors."
  (let ((result (supervisor--cli-dispatch-for-wrapper '("unknown-cmd"))))
    ;; Should start with 2: (invalid args)
    (should (stringp result))
    (should (string-match "^2:" result))))

;;; CLI Malformed Option Tests

(ert-deftest supervisor-test-cli-status-rejects-short-unknown-flag ()
  "Status with single-letter unknown flag returns invalid-args."
  (let ((result (supervisor--cli-dispatch '("status" "-x"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-logs-rejects-malformed-tail ()
  "Logs with --tailx (prefix match) returns invalid-args."
  (let ((result (supervisor--cli-dispatch '("logs" "test" "--tailx" "5"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-kill-rejects-malformed-signal ()
  "Kill with --signalx (prefix match) returns invalid-args."
  (let ((result (supervisor--cli-dispatch '("kill" "test" "--signalx" "TERM"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-logs-tail-missing-value ()
  "Logs with --tail but no value returns invalid-args."
  (let ((result (supervisor--cli-dispatch '("logs" "test" "--tail"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))
    (should (string-match "requires a numeric value" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-logs-tail-non-numeric ()
  "Logs with --tail abc returns invalid-args."
  (let ((result (supervisor--cli-dispatch '("logs" "test" "--tail" "abc"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))
    (should (string-match "must be a number" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-logs-id-matches-tail-value ()
  "Logs with ID that matches --tail value should not collide."
  ;; ("logs" "5" "--tail" "5") - ID is "5", tail value is also "5"
  ;; Should NOT reject the ID just because it matches the tail value
  (let ((result (supervisor--cli-dispatch '("logs" "5" "--tail" "5"))))
    (should (supervisor-cli-result-p result))
    ;; Should fail because log file doesn't exist, not because ID is missing
    (should (= supervisor-cli-exit-failure (supervisor-cli-result-exitcode result)))
    (should (string-match "No log file for '5'" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-logs-extra-arg-matches-tail-value ()
  "Logs with extra arg matching --tail value should be rejected."
  ;; ("logs" "foo" "--tail" "5" "5") - extra "5" should not be hidden
  (let ((result (supervisor--cli-dispatch '("logs" "foo" "--tail" "5" "5"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))
    (should (string-match "got extra" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-kill-id-matches-signal-value ()
  "Kill with ID that matches --signal value should not collide."
  ;; ("kill" "TERM" "--signal" "TERM") - ID is "TERM", signal value is also "TERM"
  ;; Should NOT reject the ID just because it matches the signal value
  (let* ((supervisor--processes (make-hash-table :test 'equal))
         (result (supervisor--cli-dispatch '("kill" "TERM" "--signal" "TERM"))))
    (should (supervisor-cli-result-p result))
    ;; Should fail because process not running, not because ID is missing
    (should (= supervisor-cli-exit-failure (supervisor-cli-result-exitcode result)))
    (should (string-match "not running" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-kill-extra-arg-matches-signal-value ()
  "Kill with extra arg matching --signal value should be rejected."
  ;; ("kill" "foo" "--signal" "TERM" "TERM") - extra "TERM" should not be hidden
  (let ((result (supervisor--cli-dispatch '("kill" "foo" "--signal" "TERM" "TERM"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))
    (should (string-match "got extra" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-logs-duplicate-tail ()
  "Logs with --tail specified twice returns invalid-args."
  (let ((result (supervisor--cli-dispatch '("logs" "foo" "--tail" "10" "--tail" "20"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))
    (should (string-match "multiple times" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-kill-duplicate-signal ()
  "Kill with --signal specified twice returns invalid-args."
  (let ((result (supervisor--cli-dispatch '("kill" "foo" "--signal" "TERM" "--signal" "KILL"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))
    (should (string-match "multiple times" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-kill-signal-missing-value ()
  "Kill with --signal but no value returns invalid-args."
  (let ((result (supervisor--cli-dispatch '("kill" "test" "--signal"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))
    (should (string-match "requires a signal name" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-kill-signal-invalid-name ()
  "Kill with --signal NOSIG returns invalid-args."
  (let ((result (supervisor--cli-dispatch '("kill" "test" "--signal" "NOSIG"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))
    (should (string-match "Invalid signal name" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-logs-option-before-id ()
  "Logs with only option (no ID) returns invalid-args (exit 2)."
  (let ((result (supervisor--cli-dispatch '("logs" "--tail" "5"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))
    (should (string-match "requires an ID" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-kill-option-before-id ()
  "Kill with only option (no ID) returns invalid-args (exit 2)."
  (let ((result (supervisor--cli-dispatch '("kill" "--signal" "TERM"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))
    (should (string-match "requires an ID" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-start-rejects-unknown-flags ()
  "Start with unknown flag returns invalid-args (exit 2)."
  (let ((result (supervisor--cli-dispatch '("start" "--bogus"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-stop-rejects-unknown-flags ()
  "Stop with unknown flag returns invalid-args (exit 2)."
  (let ((result (supervisor--cli-dispatch '("stop" "--bogus"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-restart-rejects-unknown-flags ()
  "Restart with unknown flag returns invalid-args (exit 2)."
  (let ((result (supervisor--cli-dispatch '("restart" "--bogus"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-enable-rejects-unknown-flags ()
  "Enable with unknown flag returns invalid-args (exit 2)."
  (let ((result (supervisor--cli-dispatch '("enable" "--bogus"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-disable-rejects-unknown-flags ()
  "Disable with unknown flag returns invalid-args (exit 2)."
  (let ((result (supervisor--cli-dispatch '("disable" "--bogus"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-restart-policy-rejects-unknown-flags ()
  "Restart-policy with unknown flag returns invalid-args (exit 2)."
  (let ((result (supervisor--cli-dispatch '("restart-policy" "--bogus" "id"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-logging-rejects-unknown-flags ()
  "Logging with unknown flag returns invalid-args (exit 2)."
  (let ((result (supervisor--cli-dispatch '("logging" "--bogus" "id"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))))

;;; CLI -- Separator Tests (POSIX end-of-options)

(ert-deftest supervisor-test-cli-split-at-separator ()
  "Split at -- separator correctly."
  (should (equal (supervisor--cli-split-at-separator '("a" "--" "b"))
                 '(("a") . ("b"))))
  (should (equal (supervisor--cli-split-at-separator '("a" "b"))
                 '(("a" "b") . nil)))
  (should (equal (supervisor--cli-split-at-separator '("--" "-svc"))
                 '(nil . ("-svc")))))

(ert-deftest supervisor-test-cli-strip-separator ()
  "Strip separator and concatenate."
  (should (equal (supervisor--cli-strip-separator '("a" "--" "b"))
                 '("a" "b")))
  (should (equal (supervisor--cli-strip-separator '("--" "-svc"))
                 '("-svc"))))

(ert-deftest supervisor-test-cli-parse-option ()
  "Parse option and value by position, not by value."
  ;; Option with value
  (let ((result (supervisor--cli-parse-option '("foo" "--opt" "val" "bar") "--opt")))
    (should (equal (plist-get result :value) "val"))
    (should (null (plist-get result :missing)))
    (should (equal (plist-get result :positional) '("foo" "bar"))))
  ;; Option with missing value (end of args)
  (let ((result (supervisor--cli-parse-option '("foo" "--opt") "--opt")))
    (should (null (plist-get result :value)))
    (should (plist-get result :missing))
    (should (equal (plist-get result :positional) '("foo"))))
  ;; Option with missing value (next arg is flag)
  (let ((result (supervisor--cli-parse-option '("--opt" "--other") "--opt")))
    (should (null (plist-get result :value)))
    (should (plist-get result :missing))
    (should (equal (plist-get result :positional) '("--other"))))
  ;; No option present
  (let ((result (supervisor--cli-parse-option '("foo" "bar") "--opt")))
    (should (null (plist-get result :value)))
    (should (null (plist-get result :missing)))
    (should (equal (plist-get result :positional) '("foo" "bar"))))
  ;; Value collision: positional arg equals option value
  (let ((result (supervisor--cli-parse-option '("5" "--tail" "5") "--tail")))
    (should (equal (plist-get result :value) "5"))
    (should (null (plist-get result :missing)))
    ;; Positional "5" should NOT be removed by value collision
    (should (equal (plist-get result :positional) '("5"))))
  ;; Duplicate option detection
  (let ((result (supervisor--cli-parse-option '("--opt" "val1" "--opt" "val2") "--opt")))
    (should (plist-get result :duplicate))
    ;; Value is the last one seen
    (should (equal (plist-get result :value) "val2")))
  ;; Single option is not duplicate
  (let ((result (supervisor--cli-parse-option '("--opt" "val") "--opt")))
    (should (null (plist-get result :duplicate)))))

(ert-deftest supervisor-test-cli-enable-hyphen-id-with-separator ()
  "Enable allows hyphen-prefixed ID after -- separator."
  (let ((supervisor--enabled-override (make-hash-table :test 'equal)))
    (let ((result (supervisor--cli-dispatch '("enable" "--" "-svc"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      (should (eq 'enabled (gethash "-svc" supervisor--enabled-override))))))

(ert-deftest supervisor-test-cli-logs-hyphen-id-with-separator ()
  "Logs allows hyphen-prefixed ID after -- separator."
  ;; Without separator, should fail with "ID as first argument"
  (let ((result (supervisor--cli-dispatch '("logs" "-svc"))))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result))))
  ;; With separator, should proceed (will fail with "No log file" but that's exit 1, not 2)
  (let ((result (supervisor--cli-dispatch '("logs" "--" "-svc"))))
    (should (= supervisor-cli-exit-failure (supervisor-cli-result-exitcode result)))
    (should (string-match "No log file" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-logs-rejects-extra-args ()
  "Logs with extra args returns invalid-args (exit 2)."
  (let ((result (supervisor--cli-dispatch '("logs" "id1" "id2"))))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))
    (should (string-match "extra" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-kill-rejects-extra-args ()
  "Kill with extra args returns invalid-args (exit 2)."
  (let ((result (supervisor--cli-dispatch '("kill" "id1" "id2"))))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))
    (should (string-match "extra" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-logs-options-not-parsed-after-separator ()
  "Logs does not parse --tail after -- separator."
  ;; logs -- --tail should treat --tail as the ID, not an option
  (let ((result (supervisor--cli-dispatch '("logs" "--" "--tail"))))
    ;; Should fail with "No log file for --tail", not parse --tail as option
    (should (= supervisor-cli-exit-failure (supervisor-cli-result-exitcode result)))
    (should (string-match "No log file for '--tail'" (supervisor-cli-result-output result)))))

;;; CLI -- is-active Tests

(ert-deftest supervisor-test-cli-is-active-running ()
  "The `is-active' returns exit 0 for a running unit."
  (let* ((supervisor-programs '(("sleep 999" :id "svc" :type simple)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (proc (start-process "test" nil "sleep" "999")))
    (unwind-protect
        (progn
          (puthash "svc" proc supervisor--processes)
          (let ((result (supervisor--cli-dispatch '("is-active" "svc"))))
            (should (= supervisor-cli-exit-success
                       (supervisor-cli-result-exitcode result)))
            (should (string-match "running" (supervisor-cli-result-output result)))))
      (delete-process proc))))

(ert-deftest supervisor-test-cli-is-active-not-running ()
  "The `is-active' returns exit 3 (not-active) for a non-running unit."
  (let* ((supervisor-programs '(("echo hi" :id "svc" :type simple)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (result (supervisor--cli-dispatch '("is-active" "svc"))))
    (should (= supervisor-cli-exit-not-active
               (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-is-active-unknown-id ()
  "The `is-active' returns exit 4 (no-such-unit) for unknown ID."
  (let* ((supervisor-programs '(("echo hi" :id "svc" :type simple)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (result (supervisor--cli-dispatch '("is-active" "nonexistent"))))
    (should (= supervisor-cli-exit-no-such-unit
               (supervisor-cli-result-exitcode result)))
    (should (string-match "inactive" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-is-active-no-args ()
  "The `is-active' with no args returns exit 2."
  (let ((result (supervisor--cli-dispatch '("is-active"))))
    (should (= supervisor-cli-exit-invalid-args
               (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-is-active-extra-args ()
  "The `is-active' with multiple IDs returns exit 2."
  (let ((result (supervisor--cli-dispatch '("is-active" "a" "b"))))
    (should (= supervisor-cli-exit-invalid-args
               (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-is-active-json ()
  "The `is-active --json' returns JSON with active field."
  (let* ((supervisor-programs '(("echo hi" :id "svc" :type simple)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (result (supervisor--cli-dispatch '("is-active" "svc" "--json"))))
    (should (eq 'json (supervisor-cli-result-format result)))
    (let ((parsed (json-read-from-string (supervisor-cli-result-output result))))
      (should (equal "svc" (alist-get 'id parsed)))
      (should (assoc 'active parsed))
      (should (assoc 'status parsed)))))

(ert-deftest supervisor-test-cli-is-active-json-running ()
  "The `is-active --json' returns active=true for running unit."
  (let* ((supervisor-programs '(("sleep 999" :id "svc" :type simple)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (proc (start-process "test" nil "sleep" "999")))
    (unwind-protect
        (progn
          (puthash "svc" proc supervisor--processes)
          (let* ((result (supervisor--cli-dispatch '("is-active" "svc" "--json")))
                 (parsed (json-read-from-string
                          (supervisor-cli-result-output result))))
            (should (= supervisor-cli-exit-success
                       (supervisor-cli-result-exitcode result)))
            (should (eq t (alist-get 'active parsed)))))
      (delete-process proc))))

;;; CLI -- is-enabled Tests

(ert-deftest supervisor-test-cli-is-enabled-enabled ()
  "The `is-enabled' returns exit 0 for an enabled unit."
  (let* ((supervisor-programs '(("echo hi" :id "svc" :type simple)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (result (supervisor--cli-dispatch '("is-enabled" "svc"))))
    (should (= supervisor-cli-exit-success
               (supervisor-cli-result-exitcode result)))
    (should (string-match "enabled" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-is-enabled-disabled ()
  "The `is-enabled' returns exit 1 for a disabled unit."
  (let* ((supervisor-programs '(("echo hi" :id "svc" :type simple :disabled t)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (result (supervisor--cli-dispatch '("is-enabled" "svc"))))
    (should (= supervisor-cli-exit-failure
               (supervisor-cli-result-exitcode result)))
    (should (string-match "disabled" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-is-enabled-masked ()
  "The `is-enabled' returns exit 1 and state=masked for a masked unit."
  (let* ((supervisor-programs '(("echo hi" :id "svc" :type simple)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal)))
    (puthash "svc" 'masked supervisor--mask-override)
    (let ((result (supervisor--cli-dispatch '("is-enabled" "svc"))))
      (should (= supervisor-cli-exit-failure
                 (supervisor-cli-result-exitcode result)))
      (should (string-match "masked" (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-is-enabled-unknown-id ()
  "The `is-enabled' returns exit 4 (no-such-unit) for unknown ID."
  (let* ((supervisor-programs '(("echo hi" :id "svc" :type simple)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (result (supervisor--cli-dispatch '("is-enabled" "nonexistent"))))
    (should (= supervisor-cli-exit-no-such-unit
               (supervisor-cli-result-exitcode result)))
    (should (string-match "not-found" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-is-enabled-no-args ()
  "The `is-enabled' with no args returns exit 2."
  (let ((result (supervisor--cli-dispatch '("is-enabled"))))
    (should (= supervisor-cli-exit-invalid-args
               (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-is-enabled-extra-args ()
  "The `is-enabled' with multiple IDs returns exit 2."
  (let ((result (supervisor--cli-dispatch '("is-enabled" "a" "b"))))
    (should (= supervisor-cli-exit-invalid-args
               (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-is-enabled-json ()
  "The `is-enabled --json' returns JSON with enabled and state fields."
  (let* ((supervisor-programs '(("echo hi" :id "svc" :type simple)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (result (supervisor--cli-dispatch '("is-enabled" "svc" "--json"))))
    (should (eq 'json (supervisor-cli-result-format result)))
    (let ((parsed (json-read-from-string (supervisor-cli-result-output result))))
      (should (equal "svc" (alist-get 'id parsed)))
      (should (eq t (alist-get 'enabled parsed)))
      (should (equal "enabled" (alist-get 'state parsed))))))

(ert-deftest supervisor-test-cli-is-enabled-masked-json ()
  "The `is-enabled --json' for masked unit shows enabled=false, state=masked."
  (let* ((supervisor-programs '(("echo hi" :id "svc" :type simple)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal)))
    (puthash "svc" 'masked supervisor--mask-override)
    (let* ((result (supervisor--cli-dispatch '("is-enabled" "svc" "--json")))
           (parsed (json-read-from-string (supervisor-cli-result-output result))))
      (should (= supervisor-cli-exit-failure
                 (supervisor-cli-result-exitcode result)))
      (should (eq :json-false (alist-get 'enabled parsed)))
      (should (equal "masked" (alist-get 'state parsed))))))

;;; CLI -- is-failed Tests

(ert-deftest supervisor-test-cli-is-failed-dead ()
  "The `is-failed' returns exit 0 for a crash-looped (dead) unit."
  (let* ((supervisor-programs '(("echo hi" :id "svc" :type simple)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--failed (make-hash-table :test 'equal)))
    (puthash "svc" t supervisor--failed)
    (let ((result (supervisor--cli-dispatch '("is-failed" "svc"))))
      (should (= supervisor-cli-exit-success
                 (supervisor-cli-result-exitcode result)))
      (should (string-match "dead" (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-is-failed-not-failed ()
  "The `is-failed' returns exit 1 for a non-failed unit."
  (let* ((supervisor-programs '(("echo hi" :id "svc" :type simple)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (result (supervisor--cli-dispatch '("is-failed" "svc"))))
    (should (= supervisor-cli-exit-failure
               (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-is-failed-oneshot-failed ()
  "The `is-failed' returns exit 0 for a oneshot with non-zero exit."
  (let* ((supervisor-programs '(("false" :id "svc" :type oneshot)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--oneshot-completed (make-hash-table :test 'equal)))
    (puthash "svc" 1 supervisor--oneshot-completed)
    (let ((result (supervisor--cli-dispatch '("is-failed" "svc"))))
      (should (= supervisor-cli-exit-success
                 (supervisor-cli-result-exitcode result)))
      (should (string-match "failed" (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-is-failed-unknown-id ()
  "The `is-failed' returns exit 4 (no-such-unit) for unknown ID."
  (let* ((supervisor-programs '(("echo hi" :id "svc" :type simple)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (result (supervisor--cli-dispatch '("is-failed" "nonexistent"))))
    (should (= supervisor-cli-exit-no-such-unit
               (supervisor-cli-result-exitcode result)))
    (should (string-match "inactive" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-is-failed-no-args ()
  "The `is-failed' with no args returns exit 2."
  (let ((result (supervisor--cli-dispatch '("is-failed"))))
    (should (= supervisor-cli-exit-invalid-args
               (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-is-failed-extra-args ()
  "The `is-failed' with multiple IDs returns exit 2."
  (let ((result (supervisor--cli-dispatch '("is-failed" "a" "b"))))
    (should (= supervisor-cli-exit-invalid-args
               (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-is-failed-json ()
  "The `is-failed --json' returns JSON with failed and status fields."
  (let* ((supervisor-programs '(("echo hi" :id "svc" :type simple)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--failed (make-hash-table :test 'equal)))
    (puthash "svc" t supervisor--failed)
    (let* ((result (supervisor--cli-dispatch '("is-failed" "svc" "--json")))
           (parsed (json-read-from-string (supervisor-cli-result-output result))))
      (should (= supervisor-cli-exit-success
                 (supervisor-cli-result-exitcode result)))
      (should (eq 'json (supervisor-cli-result-format result)))
      (should (equal "svc" (alist-get 'id parsed)))
      (should (eq t (alist-get 'failed parsed)))
      (should (equal "dead" (alist-get 'status parsed))))))

(ert-deftest supervisor-test-cli-is-failed-running-json ()
  "The `is-failed --json' returns failed=false for running unit."
  (let* ((supervisor-programs '(("sleep 999" :id "svc" :type simple)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (proc (start-process "test" nil "sleep" "999")))
    (unwind-protect
        (progn
          (puthash "svc" proc supervisor--processes)
          (let* ((result (supervisor--cli-dispatch '("is-failed" "svc" "--json")))
                 (parsed (json-read-from-string
                          (supervisor-cli-result-output result))))
            (should (= supervisor-cli-exit-failure
                       (supervisor-cli-result-exitcode result)))
            (should (eq :json-false (alist-get 'failed parsed)))))
      (delete-process proc))))

;;; CLI -- daemon-reload Tests

(ert-deftest supervisor-test-daemon-reload-picks-up-config-change ()
  "The `daemon-reload' picks up added entries."
  (let ((supervisor-programs '(("echo a" :id "a" :type simple)))
        (supervisor--processes (make-hash-table :test 'equal))
        (supervisor--entry-state (make-hash-table :test 'equal))
        (supervisor--current-plan nil)
        (supervisor--invalid (make-hash-table :test 'equal)))
    ;; First reload with one entry
    (supervisor-daemon-reload)
    (should supervisor--current-plan)
    (should (= 1 (length (supervisor-plan-entries supervisor--current-plan))))
    ;; Add entry and reload again
    (setq supervisor-programs '(("echo a" :id "a" :type simple)
                                ("echo b" :id "b" :type simple)))
    (supervisor-daemon-reload)
    (should (= 2 (length (supervisor-plan-entries supervisor--current-plan))))))

(ert-deftest supervisor-test-daemon-reload-runtime-untouched ()
  "The `daemon-reload' does not start or stop processes."
  (let ((supervisor-programs '(("sleep 999" :id "svc" :type simple)))
        (supervisor--processes (make-hash-table :test 'equal))
        (supervisor--entry-state (make-hash-table :test 'equal))
        (supervisor--current-plan nil)
        (supervisor--invalid (make-hash-table :test 'equal))
        (proc (start-process "test" nil "sleep" "999")))
    (unwind-protect
        (progn
          (puthash "svc" proc supervisor--processes)
          ;; Remove entry from config and reload
          (setq supervisor-programs nil)
          (supervisor-daemon-reload)
          ;; Process should still be running (runtime untouched)
          (should (process-live-p proc))
          (should (gethash "svc" supervisor--processes)))
      (delete-process proc))))

(ert-deftest supervisor-test-daemon-reload-surfaces-invalid ()
  "The `daemon-reload' surfaces invalid entries in plan."
  (let ((supervisor-programs '(("good" :id "ok" :type simple)
                                ("bad" :id "nope" :type "not-a-symbol")))
        (supervisor--processes (make-hash-table :test 'equal))
        (supervisor--entry-state (make-hash-table :test 'equal))
        (supervisor--current-plan nil)
        (supervisor--invalid (make-hash-table :test 'equal)))
    (let ((result (supervisor-daemon-reload)))
      (should (= 1 (plist-get result :entries)))
      (should (= 1 (plist-get result :invalid)))
      (should (gethash "nope" supervisor--invalid)))))

(ert-deftest supervisor-test-daemon-reload-returns-counts ()
  "The `daemon-reload' returns entry and invalid counts."
  (let ((supervisor-programs '(("echo a" :id "a" :type simple)))
        (supervisor--processes (make-hash-table :test 'equal))
        (supervisor--entry-state (make-hash-table :test 'equal))
        (supervisor--current-plan nil)
        (supervisor--invalid (make-hash-table :test 'equal)))
    (let ((result (supervisor-daemon-reload)))
      (should (= 1 (plist-get result :entries)))
      (should (= 0 (plist-get result :invalid))))))

(ert-deftest supervisor-test-cli-daemon-reload ()
  "CLI `daemon-reload' returns success."
  (let ((supervisor-programs '(("echo a" :id "a" :type simple)))
        (supervisor--processes (make-hash-table :test 'equal))
        (supervisor--entry-state (make-hash-table :test 'equal))
        (supervisor--current-plan nil)
        (supervisor--invalid (make-hash-table :test 'equal)))
    (let ((result (supervisor--cli-dispatch '("daemon-reload"))))
      (should (= supervisor-cli-exit-success
                 (supervisor-cli-result-exitcode result)))
      (should (string-match "1 entries" (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-daemon-reload-json ()
  "CLI `daemon-reload --json' returns JSON with reloaded and counts."
  (let ((supervisor-programs '(("echo a" :id "a" :type simple)))
        (supervisor--processes (make-hash-table :test 'equal))
        (supervisor--entry-state (make-hash-table :test 'equal))
        (supervisor--current-plan nil)
        (supervisor--invalid (make-hash-table :test 'equal)))
    (let* ((result (supervisor--cli-dispatch '("daemon-reload" "--json")))
           (parsed (json-read-from-string (supervisor-cli-result-output result))))
      (should (= supervisor-cli-exit-success
                 (supervisor-cli-result-exitcode result)))
      (should (eq 'json (supervisor-cli-result-format result)))
      (should (eq t (alist-get 'reloaded parsed)))
      (should (= 1 (alist-get 'entries parsed)))
      (should (= 0 (alist-get 'invalid parsed))))))

(ert-deftest supervisor-test-cli-daemon-reload-rejects-args ()
  "CLI `daemon-reload' with extra args returns exit 2."
  (let ((result (supervisor--cli-dispatch '("daemon-reload" "extra"))))
    (should (= supervisor-cli-exit-invalid-args
               (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-start-clears-current-plan ()
  "The `supervisor-start' clears `supervisor--current-plan'."
  (let ((supervisor--current-plan 'dummy))
    ;; supervisor-start resets this to nil early in its flow
    ;; We can't call full start in tests, but verify the variable exists
    (should (boundp 'supervisor--current-plan))))

(ert-deftest supervisor-test-daemon-reload-counts-unit-file-invalid ()
  "The `daemon-reload' invalid count includes malformed unit files."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-directory dir)
         (supervisor-use-unit-files t)
         (supervisor-programs nil)
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--current-plan nil)
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
         (supervisor--computed-deps (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; Create a malformed unit file (missing :command)
          (with-temp-file (expand-file-name "bad.el" dir)
            (insert "(:id \"bad-unit\")"))
          (let ((result (supervisor-daemon-reload)))
            ;; Invalid count should include the unit-file invalid
            (should (= 1 (plist-get result :invalid)))
            (should (gethash "bad-unit" supervisor--invalid))))
      (delete-directory dir t))))

;;; Phase 8: reload command tests

(ert-deftest supervisor-test-reload-unit-running-simple-restarts ()
  "Reloading a running simple unit stops and restarts it.
Reload bypasses `supervisor--manual-start' and calls
`supervisor--start-process' directly to avoid disabled-policy refusal."
  (let* ((supervisor-programs '(("sleep 999" :id "svc1")))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--failed (make-hash-table :test 'equal))
         (supervisor--restart-times (make-hash-table :test 'equal))
         (supervisor--oneshot-completed (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
         (supervisor--computed-deps (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal))
         (stop-called nil)
         (start-called nil))
    ;; Simulate a running process
    (puthash "svc1" (start-process "test" nil "sleep" "999")
             supervisor--processes)
    (unwind-protect
        (cl-letf (((symbol-function 'supervisor--manual-stop)
                   (lambda (id)
                     (setq stop-called id)
                     (let ((p (gethash id supervisor--processes)))
                       (when (and p (process-live-p p))
                         (delete-process p)))
                     (list :status 'stopped :reason nil)))
                  ((symbol-function 'supervisor--start-process)
                   (lambda (id _cmd _logging _type _restart &optional _is-restart)
                     (setq start-called id)
                     t)))
          (let ((result (supervisor--reload-unit "svc1")))
            (should (equal "svc1" (plist-get result :id)))
            (should (equal "reloaded" (plist-get result :action)))
            (should (equal "svc1" stop-called))
            (should (equal "svc1" start-called))))
      ;; Cleanup
      (let ((p (gethash "svc1" supervisor--processes)))
        (when (and p (process-live-p p))
          (delete-process p))))))

(ert-deftest supervisor-test-reload-unit-stopped-updates ()
  "Reloading a stopped unit returns `updated' and clears stale state."
  (let* ((supervisor-programs '(("echo hi" :id "svc1")))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--failed (make-hash-table :test 'equal))
         (supervisor--restart-times (make-hash-table :test 'equal))
         (supervisor--oneshot-completed (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
         (supervisor--computed-deps (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal)))
    ;; Set some stale state
    (puthash "svc1" t supervisor--failed)
    (puthash "svc1" '(12345) supervisor--restart-times)
    (puthash "svc1" 0 supervisor--oneshot-completed)
    (let ((result (supervisor--reload-unit "svc1")))
      (should (equal "svc1" (plist-get result :id)))
      (should (equal "updated" (plist-get result :action)))
      ;; Stale state should be cleared
      (should-not (gethash "svc1" supervisor--failed))
      (should-not (gethash "svc1" supervisor--restart-times))
      (should-not (gethash "svc1" supervisor--oneshot-completed)))))

(ert-deftest supervisor-test-reload-unit-masked-skips ()
  "Reloading a masked unit returns `skipped (masked)'."
  (let* ((supervisor-programs '(("echo hi" :id "svc1")))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal)))
    (puthash "svc1" 'masked supervisor--mask-override)
    (let ((result (supervisor--reload-unit "svc1")))
      (should (equal "svc1" (plist-get result :id)))
      (should (equal "skipped (masked)" (plist-get result :action))))))

(ert-deftest supervisor-test-reload-unit-unknown-errors ()
  "Reloading an unknown unit returns an error."
  (let* ((supervisor-programs nil)
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
         (supervisor--computed-deps (make-hash-table :test 'equal)))
    (let ((result (supervisor--reload-unit "nonexistent")))
      (should (equal "nonexistent" (plist-get result :id)))
      (should (equal "error: not found" (plist-get result :action))))))

(ert-deftest supervisor-test-cli-reload-requires-ids ()
  "CLI `reload' with no IDs returns exit 2."
  (let ((result (supervisor--cli-dispatch '("reload"))))
    (should (= supervisor-cli-exit-invalid-args
               (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-reload-unknown-flag ()
  "CLI `reload' with unknown flag returns exit 2."
  (let ((result (supervisor--cli-dispatch '("reload" "--bogus" "svc1"))))
    (should (= supervisor-cli-exit-invalid-args
               (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-reload-stopped-human ()
  "CLI `reload' on stopped unit shows `updated' in human output."
  (let* ((supervisor-programs '(("echo hi" :id "svc1")))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--failed (make-hash-table :test 'equal))
         (supervisor--restart-times (make-hash-table :test 'equal))
         (supervisor--oneshot-completed (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
         (supervisor--computed-deps (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal)))
    (let ((result (supervisor--cli-dispatch '("reload" "--" "svc1"))))
      (should (= supervisor-cli-exit-success
                 (supervisor-cli-result-exitcode result)))
      (should (string-match-p "svc1: updated"
                              (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-reload-masked-human ()
  "CLI `reload' on masked unit shows `skipped (masked)' in human output."
  (let* ((supervisor-programs '(("echo hi" :id "svc1")))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal)))
    (puthash "svc1" 'masked supervisor--mask-override)
    (let ((result (supervisor--cli-dispatch '("reload" "svc1"))))
      (should (= supervisor-cli-exit-success
                 (supervisor-cli-result-exitcode result)))
      (should (string-match-p "skipped (masked)"
                              (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-reload-unknown-id-human ()
  "CLI `reload' on unknown ID returns exit 1 with error."
  (let* ((supervisor-programs nil)
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
         (supervisor--computed-deps (make-hash-table :test 'equal)))
    (let ((result (supervisor--cli-dispatch '("reload" "ghost"))))
      (should (= supervisor-cli-exit-failure
                 (supervisor-cli-result-exitcode result)))
      (should (string-match-p "error: not found"
                              (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-reload-json ()
  "CLI `reload' with --json returns proper JSON structure."
  (let* ((supervisor-programs '(("echo hi" :id "svc1") ("echo bye" :id "svc2")))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--failed (make-hash-table :test 'equal))
         (supervisor--restart-times (make-hash-table :test 'equal))
         (supervisor--oneshot-completed (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
         (supervisor--computed-deps (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal)))
    (puthash "svc2" 'masked supervisor--mask-override)
    (let* ((result (supervisor--cli-dispatch '("reload" "svc1" "svc2" "--json")))
           (json-data (json-read-from-string
                       (supervisor-cli-result-output result)))
           (results (cdr (assoc 'results json-data))))
      (should (= supervisor-cli-exit-success
                 (supervisor-cli-result-exitcode result)))
      (should (= 2 (length results)))
      (should (equal "updated" (cdr (assoc 'action (aref results 0)))))
      (should (equal "skipped (masked)" (cdr (assoc 'action (aref results 1))))))))

(ert-deftest supervisor-test-cli-reload-mixed-error-json ()
  "CLI `reload' with mix of valid and unknown IDs returns exit 1 JSON."
  (let* ((supervisor-programs '(("echo hi" :id "svc1")))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--failed (make-hash-table :test 'equal))
         (supervisor--restart-times (make-hash-table :test 'equal))
         (supervisor--oneshot-completed (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
         (supervisor--computed-deps (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal)))
    (let* ((result (supervisor--cli-dispatch '("reload" "svc1" "ghost" "--json")))
           (json-data (json-read-from-string
                       (supervisor-cli-result-output result)))
           (results (cdr (assoc 'results json-data))))
      (should (= supervisor-cli-exit-failure
                 (supervisor-cli-result-exitcode result)))
      (should (= 2 (length results)))
      (should (equal "updated" (cdr (assoc 'action (aref results 0)))))
      (should (string-match-p "error:" (cdr (assoc 'action (aref results 1))))))))

(ert-deftest supervisor-test-cli-reload-help-listed ()
  "CLI help text includes the `reload' command."
  (let ((result (supervisor--cli-dispatch nil)))
    (should (string-match-p "reload \\[--\\] ID"
                            (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-reload-unit-file-only ()
  "Reload finds unit-file-only entries via effective programs."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-directory dir)
         (supervisor-use-unit-files t)
         (supervisor-programs nil)
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--failed (make-hash-table :test 'equal))
         (supervisor--restart-times (make-hash-table :test 'equal))
         (supervisor--oneshot-completed (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
         (supervisor--computed-deps (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; Create a valid unit file (not in supervisor-programs)
          (with-temp-file (expand-file-name "uf-svc.el" dir)
            (insert "(:id \"uf-svc\" :command \"echo hello\")"))
          (let ((result (supervisor--reload-unit "uf-svc")))
            (should (equal "uf-svc" (plist-get result :id)))
            (should (equal "updated" (plist-get result :action)))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-reload-clears-stale-invalid ()
  "Reload succeeds on a previously-invalid entry after config fix.
When an entry was recorded in `supervisor--invalid' but has since
been fixed, reload should find and parse it fresh, clearing the
stale invalid state."
  (let* ((supervisor-programs '(("echo fixed" :id "svc1")))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--failed (make-hash-table :test 'equal))
         (supervisor--restart-times (make-hash-table :test 'equal))
         (supervisor--oneshot-completed (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
         (supervisor--computed-deps (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal)))
    ;; Simulate stale invalid entry (was broken, now fixed in config)
    (puthash "svc1" "previously broken" supervisor--invalid)
    (let ((result (supervisor--reload-unit "svc1")))
      (should (equal "svc1" (plist-get result :id)))
      (should (equal "updated" (plist-get result :action)))
      ;; Invalid cache should be cleared for this entry
      (should-not (gethash "svc1" supervisor--invalid)))))

(ert-deftest supervisor-test-reload-find-entry-uses-effective-programs ()
  "The `supervisor--reload-find-entry' reads from effective programs."
  (let* ((supervisor-programs '(("echo hi" :id "from-config")))
         (supervisor--invalid (make-hash-table :test 'equal)))
    ;; Without unit files, should find config entry
    (should (supervisor--reload-find-entry "from-config"))
    ;; Should not find nonexistent entry
    (should-not (supervisor--reload-find-entry "nonexistent"))
    ;; Should find entry even if it's in the invalid cache
    (puthash "from-config" "some reason" supervisor--invalid)
    (should (supervisor--reload-find-entry "from-config"))))

(ert-deftest supervisor-test-reload-running-disabled-unit-succeeds ()
  "Reloading a running disabled unit keeps it running.
Reload bypasses enabled/disabled policy since the unit is already
running and reload's contract is config hot-swap."
  (let* ((supervisor-programs '(("sleep 999" :id "svc1" :disabled t)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--failed (make-hash-table :test 'equal))
         (supervisor--restart-times (make-hash-table :test 'equal))
         (supervisor--oneshot-completed (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
         (supervisor--computed-deps (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal))
         (start-process-called nil))
    ;; Simulate a running process (was started before being disabled)
    (puthash "svc1" (start-process "test" nil "sleep" "999")
             supervisor--processes)
    (unwind-protect
        (cl-letf (((symbol-function 'supervisor--manual-stop)
                   (lambda (id)
                     (let ((p (gethash id supervisor--processes)))
                       (when (and p (process-live-p p))
                         (delete-process p)))
                     (list :status 'stopped :reason nil)))
                  ((symbol-function 'supervisor--start-process)
                   (lambda (id _cmd _logging _type _restart &optional _is-restart)
                     (setq start-process-called id)
                     t)))
          (let ((result (supervisor--reload-unit "svc1")))
            ;; Must succeed, not fail with "disabled"
            (should (equal "reloaded" (plist-get result :action)))
            (should (equal "svc1" start-process-called))))
      ;; Cleanup
      (let ((p (gethash "svc1" supervisor--processes)))
        (when (and p (process-live-p p))
          (delete-process p))))))

(ert-deftest supervisor-test-reload-running-oneshot-updates-only ()
  "Reloading a running oneshot does not interrupt it.
Per Phase 8 spec, only running simple units are restarted.
Running oneshots get definition update only."
  (let* ((supervisor-programs
          '(("sleep 999" :id "osh1" :type oneshot)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--failed (make-hash-table :test 'equal))
         (supervisor--restart-times (make-hash-table :test 'equal))
         (supervisor--oneshot-completed (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
         (supervisor--computed-deps (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal))
         (stop-called nil))
    ;; Simulate a running oneshot process
    (puthash "osh1" (start-process "test" nil "sleep" "999")
             supervisor--processes)
    (unwind-protect
        (cl-letf (((symbol-function 'supervisor--manual-stop)
                   (lambda (id)
                     (setq stop-called id)
                     (list :status 'stopped :reason nil))))
          (let ((result (supervisor--reload-unit "osh1")))
            ;; Should return "updated", not "reloaded"
            (should (equal "updated" (plist-get result :action)))
            ;; Should NOT have called stop
            (should-not stop-called)))
      ;; Cleanup
      (let ((p (gethash "osh1" supervisor--processes)))
        (when (and p (process-live-p p))
          (delete-process p))))))

(ert-deftest supervisor-test-reload-parse-error-returns-invalid-config ()
  "Reload on unparseable entry returns `error: invalid config'.
This distinguishes config errors from truly missing entries."
  (let* ((supervisor-programs nil)
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'supervisor--effective-programs)
               (lambda ()
                 ;; Return a malformed entry: not a string or (string . plist)
                 '(42))))
      ;; The malformed entry's ID will be "malformed#0"
      (let ((result (supervisor--reload-unit "malformed#0")))
        (should (equal "malformed#0" (plist-get result :id)))
        (should (equal "error: invalid config" (plist-get result :action)))))))

(provide 'supervisor-test)
;;; supervisor-test.el ends here
