;;; elinit-test-core.el --- Core feature loading, parsing, and types tests for elinit.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Core feature loading, parsing, and types ERT tests for elinit.el.

;;; Code:

(require 'elinit-test-helpers)

;;; Standalone module loading tests

(ert-deftest elinit-test-module-core-standalone ()
  "Verify elinit-core loads and works without dashboard, CLI, or timer.
Spawns a subprocess to test true standalone behavior.
Core guards timer calls with fboundp, so verify works without timer module."
  (let* ((default-directory (file-name-directory (locate-library "elinit")))
         (result (call-process
                  "emacs" nil nil nil
                  "-Q" "--batch" "-L" "."
                  "--eval" "(require 'elinit-core)"
                  "--eval" "(setq elinit-unit-directory (make-temp-file \"units-\" t))"
                  "--eval" "(elinit-verify)")))
    (should (= result 0))))

(ert-deftest elinit-test-module-core-standalone-stop ()
  "Verify elinit-stop works standalone without timer module.
Tests the stop path which was previously missing fboundp guard."
  (let* ((default-directory (file-name-directory (locate-library "elinit")))
         (result (call-process
                  "emacs" nil nil nil
                  "-Q" "--batch" "-L" "."
                  "--eval" "(require 'elinit-core)"
                  "--eval" "(setq elinit-unit-directory (make-temp-file \"units-\" t))"
                  "--eval" "(elinit-stop)")))
    (should (= result 0))))

(ert-deftest elinit-test-module-core-standalone-start ()
  "Verify elinit-start works standalone without timer module."
  (let* ((default-directory (file-name-directory (locate-library "elinit")))
         (result (call-process
                  "emacs" nil nil nil
                  "-Q" "--batch" "-L" "."
                  "--eval" "(require 'elinit-core)"
                  "--eval" "(setq elinit-unit-directory (make-temp-file \"units-\" t))"
                  "--eval" "(elinit-start)")))
    (should (= result 0))))

(ert-deftest elinit-test-module-cli-standalone ()
  "Verify elinit-cli loads and works without dashboard or timer.
Spawns a subprocess to test CLI dispatch without dashboard or timer module."
  (let* ((default-directory (file-name-directory (locate-library "elinit")))
         (result (call-process
                  "emacs" nil nil nil
                  "-Q" "--batch" "-L" "."
                  "--eval" "(require 'elinit-core)"
                  "--eval" "(require 'elinit-cli)"
                  "--eval" "(setq elinit-unit-directory (make-temp-file \"units-\" t))"
                  "--eval" "(elinit--cli-dispatch '(\"status\"))")))
    (should (= result 0))))

;;; Entry parsing tests

(ert-deftest elinit-test-parse-string-entry ()
  "Parse a bare string entry and verify all defaults.
A bare string is the simplest configuration form; the parser
must derive the ID from the command and apply all defaults."
  (let ((parsed (elinit--parse-entry "nm-applet")))
    (should (equal (elinit-entry-id parsed) "nm-applet"))
    (should (equal (elinit-entry-command parsed) "nm-applet"))
    (should (= (elinit-entry-delay parsed) 0))
    (should (eq (elinit-entry-enabled-p parsed) t))
    (should (eq (elinit-entry-restart-policy parsed) 'always))
    (should (eq (elinit-entry-logging-p parsed) t))
    (should-not (elinit-entry-stdout-log-file parsed))
    (should-not (elinit-entry-stderr-log-file parsed))
    (should (eq (elinit-entry-type parsed) 'simple))))

(ert-deftest elinit-test-parse-plist-entry ()
  "Parse a plist-style entry with multiple options overriding defaults."
  (let ((parsed (elinit--parse-entry
                 '("nm-applet" :type simple :delay 3 :restart nil))))
    (should (equal (elinit-entry-id parsed) "nm-applet"))
    (should (= (elinit-entry-delay parsed) 3))
    (should (eq (elinit-entry-restart-policy parsed) 'no))
    (should (eq (elinit-entry-type parsed) 'simple))))

(ert-deftest elinit-test-parse-enabled-disabled ()
  "Parse :enabled and :disabled flags."
  (let ((enabled (elinit--parse-entry '("foo" :enabled t)))
        (disabled (elinit--parse-entry '("foo" :disabled t)))
        (explicit-nil (elinit--parse-entry '("foo" :enabled nil))))
    (should (eq (nth 3 enabled) t))
    (should (eq (nth 3 disabled) nil))
    (should (eq (nth 3 explicit-nil) nil))))

(ert-deftest elinit-test-parse-restart-no-restart ()
  "Parse :restart and :no-restart flags."
  (let ((restart (elinit--parse-entry '("foo" :restart t)))
        (no-restart (elinit--parse-entry '("foo" :no-restart t)))
        (explicit-nil (elinit--parse-entry '("foo" :restart nil))))
    (should (eq (nth 4 restart) 'always))
    (should (eq (nth 4 no-restart) 'no))
    (should (eq (nth 4 explicit-nil) 'no))))

(ert-deftest elinit-test-parse-after-string ()
  "Parse :after as string."
  (let ((parsed (elinit--parse-entry '("bar" :after "foo"))))
    (should (equal (elinit-entry-after parsed) '("foo")))))

(ert-deftest elinit-test-parse-after-list ()
  "Parse :after as list."
  (let ((parsed (elinit--parse-entry '("baz" :after ("foo" "bar")))))
    (should (equal (elinit-entry-after parsed) '("foo" "bar")))))

(ert-deftest elinit-test-parse-stream-log-files ()
  "Parse per-stream log file options."
  (let ((parsed (elinit--parse-entry
                 '("svc-cmd"
                   :stdout-log-file "/tmp/svc.out.log"
                   :stderr-log-file "/tmp/svc.err.log"))))
    (should (equal (elinit-entry-stdout-log-file parsed)
                   "/tmp/svc.out.log"))
    (should (equal (elinit-entry-stderr-log-file parsed)
                   "/tmp/svc.err.log"))))

(ert-deftest elinit-test-validate-stream-log-files ()
  "Validate per-stream log file options."
  (should-not (elinit--validate-entry
               '("svc-cmd"
                 :stdout-log-file "/tmp/svc.out.log"
                 :stderr-log-file "/tmp/svc.err.log")))
  (should (string-match-p ":stdout-log-file"
                          (elinit--validate-entry
                           '("svc-cmd" :stdout-log-file ""))))
  (should (string-match-p ":stderr-log-file"
                          (elinit--validate-entry
                           '("svc-cmd" :stderr-log-file 123)))))


;;; Oneshot blocking semantics
;;
;; Blocking vs async is a core scheduling decision: blocking oneshots
;; gate stage completion, async ones run in parallel.

(ert-deftest elinit-test-oneshot-blocking-semantics ()
  "Oneshot blocking semantics: explicit overrides default, async inverts."
  ;; Default uses defcustom
  (should (eq (elinit--oneshot-blocking-p '()) elinit-oneshot-default-blocking))
  ;; Explicit :oneshot-blocking overrides
  (should (eq (elinit--oneshot-blocking-p '(:oneshot-blocking t)) t))
  (should (eq (elinit--oneshot-blocking-p '(:oneshot-blocking nil)) nil))
  ;; :oneshot-async is inverse sugar
  (should (eq (elinit--oneshot-blocking-p '(:oneshot-async t)) nil))
  (should (eq (elinit--oneshot-blocking-p '(:oneshot-async nil)) t)))

;;; Verbose logging tests

(ert-deftest elinit-test-log-warning-always-shows ()
  "Warning messages always show regardless of verbose setting."
  (let ((elinit-verbose nil)
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (elinit--log 'warning "test warning"))
    (should (= (length messages) 1))
    (should (string-match "WARNING" (car messages)))))

(ert-deftest elinit-test-log-info-hidden-when-not-verbose ()
  "Info messages hidden when elinit-verbose is nil."
  (let ((elinit-verbose nil)
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (elinit--log 'info "test info"))
    (should (= (length messages) 0))))

(ert-deftest elinit-test-log-info-shown-when-verbose ()
  "Info messages shown when elinit-verbose is non-nil."
  (let ((elinit-verbose t)
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (elinit--log 'info "test info"))
    (should (= (length messages) 1))
    (should (string-match "test info" (car messages)))))



;;; DAG Scheduler Tests
;;
;; The DAG scheduler is the heart of startup orchestration.  These tests
;; verify the invariants that make ordered, dependency-aware startup correct.

(ert-deftest elinit-test-topo-sort-stable-ordering ()
  "Unconstrained nodes maintain original list order.
Stability guarantees predictable startup for entries without
explicit dependencies."
  (let* ((entries '(("a" "cmd" 0 t always t simple nil t 30)
                    ("b" "cmd" 0 t always t simple nil t 30)
                    ("c" "cmd" 0 t always t simple nil t 30)))
         (sorted (elinit--stable-topo-sort entries)))
    (should (equal (mapcar #'car sorted) '("a" "b" "c")))))

(ert-deftest elinit-test-topo-sort-respects-after ()
  "Entries with :after come after their dependencies."
  (let* ((entries '(("c" "cmd" 0 t always t simple ("a") t 30)
                    ("a" "cmd" 0 t always t simple nil t 30)
                    ("b" "cmd" 0 t always t simple nil t 30)))
         (sorted (elinit--stable-topo-sort entries))
         (order (mapcar #'car sorted)))
    (should (< (cl-position "a" order :test #'equal)
               (cl-position "c" order :test #'equal)))
    ;; Stable sort: after "a" emits, both "c" (idx 0) and "b" (idx 2) are ready
    ;; "c" comes first because it has lower original index
    (should (equal order '("a" "c" "b")))))

(ert-deftest elinit-test-topo-sort-cycle-fallback ()
  "Cycle detection returns list order with :after cleared.
Cycles are unresolvable; the fallback preserves startup order and
clears dependency edges to prevent deadlock."
  (let* ((entries '(("a" "cmd" 0 t always t simple ("b") t 30)
                    ("b" "cmd" 0 t always t simple ("a") t 30)))
         (sorted (elinit--stable-topo-sort entries)))
    (should (equal (mapcar #'car sorted) '("a" "b")))
    ;; :after (index 7) should be nil for all entries
    (should (null (nth 7 (car sorted))))
    (should (null (nth 7 (cadr sorted))))))

(ert-deftest elinit-test-topo-sort-complex-dag ()
  "Complex DAG with multiple dependencies resolves correctly."
  ;; d depends on b and c, b depends on a
  (let* ((entries '(("a" "cmd" 0 t always t simple nil t 30)
                    ("b" "cmd" 0 t always t simple ("a") t 30)
                    ("c" "cmd" 0 t always t simple nil t 30)
                    ("d" "cmd" 0 t always t simple ("b" "c") t 30)))
         (sorted (elinit--stable-topo-sort entries))
         (order (mapcar #'car sorted)))
    (should (< (cl-position "a" order :test #'equal)
               (cl-position "b" order :test #'equal)))
    (should (< (cl-position "b" order :test #'equal)
               (cl-position "d" order :test #'equal)))
    (should (< (cl-position "c" order :test #'equal)
               (cl-position "d" order :test #'equal)))))

(ert-deftest elinit-test-dag-init-blocking-oneshot ()
  "Blocking oneshots are tracked in the blocking set.
Only oneshot entries with oneshot-blocking=t gate stage completion."
  (let ((elinit--dag-blocking nil)
        (elinit--dag-in-degree nil)
        (elinit--dag-dependents nil)
        (elinit--dag-entries nil)
        (elinit--dag-started nil)
        (elinit--dag-ready nil)
        (elinit--dag-timeout-timers nil)
        (elinit--dag-delay-timers nil)
        (elinit--dag-id-to-index nil))
    (let ((entries '(("blocking" "cmd" 0 t always t oneshot nil t 30)
                     ("non-blocking" "cmd" 0 t always t oneshot nil nil 30)
                     ("simple" "cmd" 0 t always t simple nil t 30))))
      (elinit--dag-init entries)
      (should (gethash "blocking" elinit--dag-blocking))
      (should-not (gethash "non-blocking" elinit--dag-blocking))
      (should-not (gethash "simple" elinit--dag-blocking)))))

(ert-deftest elinit-test-dag-disabled-entry-ready-immediately ()
  "Disabled entries are ready immediately and don't block dependents.
This is a critical scheduler invariant: disabled entries must not
cause deadlocks in the dependency graph."
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
    (let ((entries '(("a" "cmd" 0 nil always t simple nil t 30)
                     ("b" "cmd" 0 t   always t simple ("a") t 30))))
      (elinit--dag-init entries)
      (should (gethash "a" elinit--dag-ready))
      (should (gethash "a" elinit--dag-started))
      (should (eq (gethash "a" elinit--entry-state) 'disabled))
      ;; Dependent "b" should have in-degree 0 (not blocked by disabled "a")
      (should (= 0 (gethash "b" elinit--dag-in-degree))))))

(ert-deftest elinit-test-async-oneshot-not-blocking ()
  "Async oneshots (oneshot-blocking nil) do not block convergence.
This is the semantic difference between blocking and async oneshots."
  (let ((elinit--dag-blocking nil)
        (elinit--dag-in-degree nil)
        (elinit--dag-dependents nil)
        (elinit--dag-entries nil)
        (elinit--dag-started nil)
        (elinit--dag-ready nil)
        (elinit--dag-timeout-timers nil)
        (elinit--dag-delay-timers nil)
        (elinit--dag-id-to-index nil))
    (let ((entries '(("async-oneshot" "cmd" 0 t always t oneshot nil nil 30))))
      (elinit--dag-init entries)
      (should-not (gethash "async-oneshot" elinit--dag-blocking)))))

(ert-deftest elinit-test-startup-complete-blocked-by-delay ()
  "Delayed entries prevent startup completion until they start.
A stage is not complete until all delayed entries have actually started."
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
    (puthash "delayed" '("delayed" "echo hi" 5 t t t simple nil t 30) elinit--dag-entries)
    (puthash "delayed" 0 elinit--dag-in-degree)
    (puthash "delayed" 0 elinit--dag-id-to-index)
    (puthash "delayed" nil elinit--dag-dependents)
    (setq elinit--dag-complete-callback (lambda () (setq callback-called t)))
    (puthash "delayed" 'mock-timer elinit--dag-delay-timers)
    (puthash "delayed" t elinit--dag-started)
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
    (puthash "blocking" '("blocking" "sleep 10" 0 t always t oneshot nil t 30) elinit--dag-entries)
    (puthash "blocking" 0 elinit--dag-in-degree)
    (puthash "blocking" t elinit--dag-started)
    (puthash "blocking" t elinit--dag-blocking)
    (setq elinit--dag-complete-callback (lambda () (setq callback-called t)))
    (elinit--dag-check-complete)
    (should-not callback-called)))

(ert-deftest elinit-test-mark-ready-removes-from-blocking ()
  "Mark-ready removes entry from blocking set, enabling convergence."
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
    (puthash "oneshot" '("oneshot" "cmd" 0 t always t oneshot nil t 30) elinit--dag-entries)
    (puthash "oneshot" 0 elinit--dag-in-degree)
    (puthash "oneshot" t elinit--dag-started)
    (puthash "oneshot" t elinit--dag-blocking)
    (puthash "oneshot" nil elinit--dag-dependents)
    (elinit--dag-mark-ready "oneshot")
    (should-not (gethash "oneshot" elinit--dag-blocking))
    (should (gethash "oneshot" elinit--dag-ready))))

(ert-deftest elinit-test-mark-ready-unlocks-dependents ()
  "Mark-ready decrements in-degree and triggers dependent starts."
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
    (cl-letf (((symbol-function 'elinit--dag-start-entry-async)
               (lambda (entry) (push (car entry) started-ids))))
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
      (elinit--dag-mark-ready "a")
      (should (= 0 (gethash "b" elinit--dag-in-degree)))
      (should (member "b" started-ids)))))

(ert-deftest elinit-test-oneshot-timeout-unlocks-dependents ()
  "Oneshot timeout calls mark-ready which unlocks dependents.
This ensures a stuck oneshot does not permanently block the startup."
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
    (cl-letf (((symbol-function 'elinit--dag-start-entry-async)
               (lambda (entry) (push (car entry) started-ids))))
      (puthash "slow" '("slow" "sleep 999" 0 t always t oneshot nil t 5) elinit--dag-entries)
      (puthash "after-slow" '("after-slow" "echo done" 0 t always t simple ("slow") t 30) elinit--dag-entries)
      (puthash "slow" 0 elinit--dag-in-degree)
      (puthash "after-slow" 1 elinit--dag-in-degree)
      (puthash "slow" t elinit--dag-started)
      (puthash "after-slow" nil elinit--dag-started)
      (puthash "slow" t elinit--dag-blocking)
      (puthash "slow" '("after-slow") elinit--dag-dependents)
      (puthash "after-slow" nil elinit--dag-dependents)
      (puthash "slow" 0 elinit--dag-id-to-index)
      (puthash "after-slow" 1 elinit--dag-id-to-index)
      (puthash "slow" 'mock-timer elinit--dag-timeout-timers)
      (setq elinit--dag-complete-callback
            (lambda () (setq startup-complete t)))
      (elinit--dag-mark-ready "slow")
      (should-not (gethash "slow" elinit--dag-blocking))
      (should (= 0 (gethash "after-slow" elinit--dag-in-degree)))
      (should (member "after-slow" started-ids))
      (should-not (gethash "slow" elinit--dag-timeout-timers)))))

(ert-deftest elinit-test-max-concurrent-starts-active-count-no-leak ()
  "Active count must not leak when entries are processed from queue.
Regression: process-pending-starts must NOT increment the counter;
only do-start does."
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
    (puthash "a" '("a" "true" 0 t always t simple nil t 30) elinit--dag-entries)
    (puthash "b" '("b" "true" 0 t always t simple nil t 30) elinit--dag-entries)
    (setq elinit--dag-pending-starts '("a" "b"))
    (should (= elinit--dag-active-starts 0))
    (cl-letf (((symbol-function 'elinit--dag-start-entry-async)
               (lambda (entry) (push (car entry) started-ids))))
      (elinit--dag-process-pending-starts)
      (should (null elinit--dag-pending-starts))
      (should (member "a" started-ids))
      (should (member "b" started-ids))
      (should (= elinit--dag-active-starts 0)))))

(ert-deftest elinit-test-enabled-override-affects-effective-enabled ()
  "Runtime enable override affects effective enabled state.
Overrides take precedence over config for enable/disable decisions."
  (let ((elinit--enabled-override (make-hash-table :test 'equal)))
    ;; No override: config enabled-p applies
    (should (elinit--get-effective-enabled "a" t))
    (should-not (elinit--get-effective-enabled "b" nil))
    ;; Override to disabled
    (puthash "a" 'disabled elinit--enabled-override)
    (should-not (elinit--get-effective-enabled "a" t))
    ;; Override to enabled
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
    (puthash "delayed" '("delayed" "cmd" 5 t t t simple nil t 30)
             elinit--dag-entries)
    (setq elinit--dag-complete-callback (lambda () (setq callback-called t)))
    (elinit--dag-force-complete)
    (should (eq (gethash "delayed" elinit--entry-state) 'startup-timeout))
    (should callback-called)))

(ert-deftest elinit-test-format-exit-status-signal ()
  "Exit status formatting distinguishes signal from exit."
  (should (string-match "killed by signal 15"
                        (elinit--format-exit-status 'signal 15)))
  (should (string-match "exited successfully"
                        (elinit--format-exit-status 'exit 0)))
  (should (string-match "exited with code 1"
                        (elinit--format-exit-status 'exit 1))))

;;; Verification and integration tests

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
  "All-parsed-entries skips invalid entries and tracks them."
  (elinit-test-with-unit-files
      '(("valid" :id "valid" :type simple)
        ("invalid" :id "invalid" :type "bad")
        ("also-valid" :id "also-valid" :type oneshot))
    (let ((elinit--invalid (make-hash-table :test 'equal)))
      (let ((entries (elinit--all-parsed-entries)))
        (should (= (length entries) 2))
        (should (gethash "invalid" elinit--invalid))
        (should (cl-find "valid" entries :key #'car :test #'equal))
        (should (cl-find "also-valid" entries :key #'car :test #'equal))))))

(ert-deftest elinit-test-verify-handles-malformed-entry ()
  "Verify handles malformed unit files gracefully."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit--programs-cache :not-yet-loaded)
         (elinit--unit-file-invalid (make-hash-table :test 'equal))
         (elinit--invalid (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "valid.el" dir)
            (insert "(:id \"valid\" :command \"true\" :type simple)"))
          (with-temp-file (expand-file-name "broken.el" dir)
            (insert "42"))
          (with-temp-buffer
            (let ((standard-output (current-buffer)))
              (elinit-verify)))
          (should (gethash "broken" elinit--invalid))
          (should (null (gethash "valid" elinit--invalid))))
      (delete-directory dir t))))

;;; Reconcile tests

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
      (puthash "new-entry" 'disabled elinit--enabled-override)
      (cl-letf (((symbol-function 'elinit--start-process)
                 (lambda (id _cmd _log _type _restart &rest _args)
                   (push id started-ids)))
                ((symbol-function 'elinit--refresh-dashboard) #'ignore)
                ((symbol-function 'executable-find) (lambda (_) t)))
        (elinit--reconcile)
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
      (let ((fake-proc (start-process "test-proc" nil "sleep" "100")))
        (puthash "running" fake-proc elinit--processes)
        (cl-letf (((symbol-function 'kill-process)
                   (lambda (proc)
                     (push (process-name proc) killed-ids)
                     (delete-process proc)))
                  ((symbol-function 'elinit--refresh-dashboard) #'ignore))
          (elinit--reconcile)
          (should (member "test-proc" killed-ids)))))))

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
        (should (elinit-plan-p elinit--current-plan))))))

;;; Computed dependencies tests

(ert-deftest elinit-test-computed-deps-populated ()
  "Topo-sort populates computed-deps with validated dependencies.
Missing dependencies are silently dropped."
  (let ((elinit--computed-deps (make-hash-table :test 'equal))
        (elinit--cycle-fallback-ids (make-hash-table :test 'equal)))
    (let ((entries '(("a" "cmd" 0 t always t simple nil t 30)
                     ("c" "cmd" 0 t always t simple ("a" "b") t 30))))
      (elinit--stable-topo-sort entries)
      ;; c's computed deps should only include "a" (b doesn't exist)
      (should (equal (gethash "c" elinit--computed-deps) '("a")))
      (should (equal (gethash "a" elinit--computed-deps) nil)))))

(ert-deftest elinit-test-cycle-fallback-clears-computed-deps ()
  "Cycle fallback marks entries and clears their computed deps."
  (let ((elinit--computed-deps (make-hash-table :test 'equal))
        (elinit--cycle-fallback-ids (make-hash-table :test 'equal)))
    (let ((entries '(("a" "cmd" 0 t always t simple ("b") t 30)
                     ("b" "cmd" 0 t always t simple ("a") t 30))))
      (elinit--stable-topo-sort entries)
      (should (gethash "a" elinit--cycle-fallback-ids))
      (should (gethash "b" elinit--cycle-fallback-ids))
      (should (null (gethash "a" elinit--computed-deps)))
      (should (null (gethash "b" elinit--computed-deps))))))

;;; Tags parsing

(ert-deftest elinit-test-parse-tags ()
  "Parse :tags keyword to symbol list."
  (let ((parsed (elinit--parse-entry '("foo" :tags (x-setup network)))))
    (should (equal (elinit-entry-tags parsed) '(x-setup network))))
  ;; Single symbol should be wrapped in list
  (let ((parsed (elinit--parse-entry '("foo" :tags myapp))))
    (should (equal (elinit-entry-tags parsed) '(myapp)))))

;;; Dry-run tests

(ert-deftest elinit-test-dry-run-output ()
  "Dry-run produces activation order and invalid entry sections."
  (elinit-test-with-unit-files
      '(("true" :id "a" :type oneshot)
        ("true" :id "b")
        ("true" :id "c"))
    (let ((elinit--invalid (make-hash-table :test 'equal))
          (elinit--cycle-fallback-ids (make-hash-table :test 'equal))
          (elinit--computed-deps (make-hash-table :test 'equal)))
      (elinit-dry-run)
      (let ((output (with-current-buffer "*elinit-dry-run*"
                      (buffer-string))))
        (kill-buffer "*elinit-dry-run*")
        (should (string-match-p "Activation order" output))
        (should (string-match-p "\\ba\\b" output))
        (should (string-match-p "\\bb\\b" output))
        (should (string-match-p "\\bc\\b" output))))))

(ert-deftest elinit-test-dry-run-shows-invalid ()
  "Dry-run shows invalid entries in a separate section."
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
      (elinit-dry-run)
      (kill-buffer "*elinit-dry-run*")
      ;; b's computed deps should be empty since nonexistent doesn't exist
      (should (null (gethash "b" elinit--computed-deps))))))

;;; Conflict Subsystem Tests
;;
;; Conflicts are mutual exclusion constraints between units.  These tests
;; verify the core conflict resolution mechanics.

(ert-deftest elinit-test-conflict-targets-pure ()
  "Pure conflict-targets returns deduplicated symmetric conflicts."
  (let* ((programs '(("sleep 1" :id "a" :conflicts "b")
                     ("sleep 2" :id "b" :conflicts "a")
                     ("sleep 3" :id "c")))
         (plan (elinit--build-plan programs)))
    (should (equal (elinit--conflict-targets "a" plan) '("b")))
    (should (null (elinit--conflict-targets "c" plan)))
    (should (null (elinit--conflict-targets "a" nil)))))

(ert-deftest elinit-test-conflict-preflight-stops-active ()
  "Conflict preflight stops an active conflicting process."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--conflict-suppressed (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal)))
    (let ((fake-proc (start-process "b" nil "sleep" "300")))
      (puthash "b" fake-proc elinit--processes)
      (let* ((programs '(("sleep 1" :id "a" :conflicts "b")
                         ("sleep 2" :id "b")))
             (plan (elinit--build-plan programs))
             (stopped (elinit--conflict-preflight "a" plan)))
        (should (member "b" stopped))
        (should (equal (gethash "b" elinit--conflict-suppressed) "a"))
        (should (gethash "b" elinit--manually-stopped))
        (should-not (process-live-p fake-proc)))
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
    (let* ((programs '(("sleep 1" :id "a" :conflicts "b")
                       ("sleep 2" :id "b")))
           (plan (elinit--build-plan programs))
           (stopped (elinit--conflict-preflight "a" plan)))
      (should (null stopped)))))

(ert-deftest elinit-test-conflict-preflight-cancels-restart-timer ()
  "Conflict preflight cancels pending restart timer."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--conflict-suppressed (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (timer (run-at-time 999 nil #'ignore)))
    (puthash "b" timer elinit--restart-timers)
    (let ((fake-proc (start-process "b" nil "sleep" "300")))
      (puthash "b" fake-proc elinit--processes)
      (let* ((programs '(("sleep 1" :id "a" :conflicts "b")
                         ("sleep 2" :id "b")))
             (plan (elinit--build-plan programs)))
        (elinit--conflict-preflight "a" plan)
        (should (null (gethash "b" elinit--restart-timers))))
      (when (process-live-p fake-proc)
        (delete-process fake-proc)))))

(ert-deftest elinit-test-conflict-preflight-symmetric ()
  "Conflict preflight sees reverse conflicts too.
B declares conflict with A, but we start A -- B should still be stopped."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--conflict-suppressed (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal)))
    (let ((fake-proc (start-process "b" nil "sleep" "300")))
      (puthash "b" fake-proc elinit--processes)
      (let* ((programs '(("sleep 1" :id "a")
                         ("sleep 2" :id "b" :conflicts "a")))
             (plan (elinit--build-plan programs))
             (stopped (elinit--conflict-preflight "a" plan)))
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
    (puthash "b" t elinit--remain-active)
    (puthash "b" 0 elinit--oneshot-completed)
    (let* ((programs '(("true" :id "a" :conflicts "b")
                       ("true" :id "b" :type oneshot :remain-after-exit t)))
           (plan (elinit--build-plan programs))
           (stopped (elinit--conflict-preflight "a" plan)))
      (should (member "b" stopped))
      (should (equal (gethash "b" elinit--conflict-suppressed) "a"))
      (should (null (gethash "b" elinit--remain-active)))
      (should (null (gethash "b" elinit--oneshot-completed)))
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
    (puthash "b" timer elinit--dag-delay-timers)
    (let* ((programs '(("sleep 1" :id "a" :conflicts "b")
                       ("sleep 2" :id "b")))
           (plan (elinit--build-plan programs)))
      (elinit--conflict-preflight "a" plan)
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
    (puthash "b" timer elinit--restart-timers)
    (let* ((programs '(("sleep 1" :id "a" :conflicts "b")
                       ("sleep 2" :id "b")))
           (plan (elinit--build-plan programs)))
      (elinit--conflict-preflight "a" plan)
      (should (null (gethash "b" elinit--restart-timers))))))

(ert-deftest elinit-test-conflict-preflight-sigkill-escalation ()
  "Conflict preflight escalates to SIGKILL for SIGTERM-resistant processes."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--conflict-suppressed (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (elinit-shutdown-timeout 0.2))
    (let ((fake-proc (start-process "b" nil "sh" "-c"
                                    "trap '' TERM; sleep 300")))
      (sleep-for 0.1)
      (puthash "b" fake-proc elinit--processes)
      (let* ((programs '(("sleep 1" :id "a" :conflicts "b")
                         ("sleep 2" :id "b")))
             (plan (elinit--build-plan programs))
             (stopped (elinit--conflict-preflight "a" plan)))
        (should (member "b" stopped))
        (should-not (process-live-p fake-proc)))
      (when (process-live-p fake-proc)
        (delete-process fake-proc)))))

(ert-deftest elinit-test-restart-callback-suppression-guard ()
  "Restart callback skips start when unit is conflict-suppressed at fire time.
Race condition guard: suppression can be set after restart is scheduled."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--conflict-suppressed (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (elinit--current-plan nil)
        (elinit--failed (make-hash-table :test 'equal))
        (start-called nil))
    (cl-letf (((symbol-function 'elinit--start-process)
               (lambda (&rest _args) (setq start-called t) t))
              ((symbol-function 'elinit--format-exit-status)
               (lambda (&rest _) "exited 0")))
      (let ((elinit-restart-delay 0))
        (elinit--schedule-restart
         "svc" "sleep 300" t 'simple 'always "exited" 0)
        (puthash "svc" "other" elinit--conflict-suppressed)
        (sit-for 0.1)
        (should-not start-called)
        (should-not (gethash "svc" elinit--restart-timers))))))

;;; Manual start/stop conflict integration

(ert-deftest elinit-test-manual-start-stops-conflicting-unit ()
  "Manual start of A stops running conflicting unit B."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--conflict-suppressed (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--manually-started (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (elinit--invalid (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--restart-times (make-hash-table :test 'equal))
        (elinit--mask-override (make-hash-table :test 'equal))
        (elinit--spawn-failure-reason (make-hash-table :test 'equal))
        (elinit--programs-cache
         (list '("sleep 1" :id "a" :conflicts "b")
               '("sleep 2" :id "b")))
        (elinit--current-plan nil)
        (a-started nil))
    (setq elinit--current-plan
          (elinit--build-plan elinit--programs-cache))
    (let ((b-proc (start-process "b" nil "sleep" "300")))
      (puthash "b" b-proc elinit--processes)
      (cl-letf (((symbol-function 'elinit--start-process)
                 (lambda (id &rest _args)
                   (when (equal id "a")
                     (setq a-started t))
                   (start-process "mock-a" nil "true")))
                ((symbol-function 'elinit--unit-file-directory-for-id)
                 (lambda (_) nil))
                ((symbol-function 'executable-find) (lambda (_) t)))
        (let ((result (elinit--manual-start "a")))
          (should (eq 'started (plist-get result :status)))
          (should a-started)
          (should (equal "a" (gethash "b" elinit--conflict-suppressed)))
          (should-not (process-live-p b-proc))))
      (when (process-live-p b-proc)
        (delete-process b-proc)))))

(ert-deftest elinit-test-reconcile-stops-conflicting-unit ()
  "Reconcile starting A stops running conflicting unit B."
  (elinit-test-with-unit-files
      '(("sleep 1" :id "a" :type simple :conflicts "b")
        ("sleep 2" :id "b" :type simple))
    (let ((elinit--processes (make-hash-table :test 'equal))
          (elinit--restart-timers (make-hash-table :test 'equal))
          (elinit--conflict-suppressed (make-hash-table :test 'equal))
          (elinit--manually-stopped (make-hash-table :test 'equal))
          (elinit--manually-started (make-hash-table :test 'equal))
          (elinit--remain-active (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--dag-delay-timers (make-hash-table :test 'equal))
          (elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--restart-override (make-hash-table :test 'equal))
          (elinit--logging (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--current-plan nil)
          (started-ids nil))
      (let ((b-proc (start-process "b" nil "sleep" "300")))
        (puthash "b" b-proc elinit--processes)
        (cl-letf (((symbol-function 'elinit--start-process)
                   (lambda (id _cmd _log _type _restart &rest _args)
                     (push id started-ids)
                     t))
                  ((symbol-function 'elinit--refresh-dashboard) #'ignore)
                  ((symbol-function 'executable-find) (lambda (_) t)))
          (elinit--reconcile)
          (should (member "a" started-ids))
          (should (equal "a" (gethash "b" elinit--conflict-suppressed)))
          (should-not (process-live-p b-proc)))
        (when (process-live-p b-proc)
          (delete-process b-proc))))))


;;; Plan Builder Tests
;;
;; The plan builder compiles program config into an immutable activation
;; plan.  These tests verify its structural contracts and safety properties.

(ert-deftest elinit-test-plan-shape ()
  "Plan struct has all required fields with correct types."
  (let* ((programs '(("sleep 100" :id "a")
                     ("sleep 200" :id "b" :after "a")
                     ("invalid-entry" :unknown-keyword t)))
         (plan (elinit--build-plan programs)))
    (should (elinit-plan-p plan))
    (should (= 2 (length (elinit-plan-entries plan))))
    (should (= 1 (hash-table-count (elinit-plan-invalid plan))))
    (should (= 2 (length (elinit-plan-by-target plan))))
    (should (hash-table-p (elinit-plan-deps plan)))
    (should (hash-table-p (elinit-plan-dependents plan)))
    (should (hash-table-p (elinit-plan-cycle-fallback-ids plan)))
    (should (hash-table-p (elinit-plan-order-index plan)))
    (should (hash-table-p (elinit-plan-conflicts-deps plan)))
    (should (hash-table-p (elinit-plan-conflict-reverse plan)))
    (should (plist-get (elinit-plan-meta plan) :version))
    (should (plist-get (elinit-plan-meta plan) :timestamp))))

(ert-deftest elinit-test-plan-determinism ()
  "Identical config produces identical plan data.
Determinism is critical for idempotent reconcile operations."
  (let* ((programs '(("sleep 100" :id "a")
                     ("sleep 200" :id "b" :after "a")
                     ("sleep 300" :id "c")))
         (plan1 (elinit--build-plan programs))
         (plan2 (elinit--build-plan programs)))
    (should (equal (elinit-plan-entries plan1)
                   (elinit-plan-entries plan2)))
    (should (equal (elinit-plan-by-target plan1)
                   (elinit-plan-by-target plan2)))))

(ert-deftest elinit-test-plan-no-global-mutation ()
  "Plan building does not mutate global state.
The plan builder must be pure so it can be called repeatedly
without side effects on the running system."
  (let ((programs '(("sleep 100" :id "a")
                    ("sleep 200" :id "b" :after "a"))))
    (clrhash elinit--invalid)
    (clrhash elinit--cycle-fallback-ids)
    (clrhash elinit--computed-deps)
    (puthash "sentinel" "should-remain" elinit--invalid)
    (puthash "sentinel" t elinit--cycle-fallback-ids)
    (puthash "sentinel" '("test") elinit--computed-deps)
    (elinit--build-plan programs)
    (should (equal "should-remain" (gethash "sentinel" elinit--invalid)))
    (should (eq t (gethash "sentinel" elinit--cycle-fallback-ids)))
    (should (equal '("test") (gethash "sentinel" elinit--computed-deps)))))

(ert-deftest elinit-test-plan-dependency-validation ()
  "Plan validates :after references and drops nonexistent ones."
  (let* ((programs '(("sleep 100" :id "a")
                     ("sleep 200" :id "b" :after "a")
                     ("sleep 300" :id "c" :after "nonexistent")
                     ("sleep 400" :id "d" :after "a")))
         (plan (elinit--build-plan programs)))
    (should (equal '("a") (gethash "b" (elinit-plan-deps plan))))
    (should (null (gethash "c" (elinit-plan-deps plan))))
    (should (equal '("a") (gethash "d" (elinit-plan-deps plan))))))

(ert-deftest elinit-test-plan-duplicate-id-first-occurrence-order ()
  "Duplicate IDs use first-occurrence index for ordering.
Regression: duplicates must not overwrite order-index of kept entry."
  (let* ((programs '(("sleep 100" :id "a")
                     ("sleep 200" :id "b")
                     ("sleep 300" :id "a")))
         (plan (elinit--build-plan programs)))
    (should (= 2 (length (elinit-plan-entries plan))))
    (should (= 0 (gethash "a" (elinit-plan-order-index plan))))
    (should (= 1 (gethash "b" (elinit-plan-order-index plan))))))

(ert-deftest elinit-test-plan-build-warns-on-duplicates ()
  "Plan building emits warnings for duplicate IDs."
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
          (should (cl-some (lambda (m) (string-match-p "Duplicate.*ID 'dup'" m))
                           messages)))
      (delete-directory dir t))))

(ert-deftest elinit-test-plan-build-warns-on-invalid-after ()
  "Plan building emits warnings for invalid :after references."
  (elinit-test-with-unit-files
      '(("sleep 100" :id "a")
        ("sleep 200" :id "b" :after "nonexistent"))
    (let ((messages nil))
      (cl-letf (((symbol-function 'elinit--log)
                 (lambda (_level fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (elinit--build-plan (elinit--effective-programs)))
      (should (cl-some (lambda (m) (string-match-p "does not exist" m))
                       messages)))))

;;; Startup Uses Plan Tests

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
  "Plan->global copy mechanism populates legacy globals correctly."
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
          (elinit--shutting-down t))
      (let* ((progs (elinit--effective-programs))
             (plan (elinit--build-plan progs)))
        (elinit--merge-unit-file-invalid)
        (maphash (lambda (k v) (puthash k v elinit--invalid))
                 (elinit-plan-invalid plan))
        (maphash (lambda (k v) (puthash k v elinit--cycle-fallback-ids))
                 (elinit-plan-cycle-fallback-ids plan))
        (maphash (lambda (k v) (puthash k v elinit--computed-deps))
                 (elinit-plan-deps plan)))
      (should (gethash "invalid" elinit--invalid))
      (should (equal '("a") (gethash "b" elinit--computed-deps)))
      (should (= 1 (hash-table-count elinit--invalid))))))

;;; Snapshot Tests

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
    (puthash "test-failed" t elinit--failed)
    (puthash "test-oneshot" 0 elinit--oneshot-completed)
    (puthash "test-entry" 'running elinit--entry-state)
    (puthash "test-invalid" "bad config" elinit--invalid)
    (puthash "test-enabled" 'disabled elinit--enabled-override)
    (let ((snapshot (elinit--build-snapshot)))
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
    (puthash "test" t elinit--failed)
    (puthash "oneshot-done" 0 elinit--oneshot-completed)
    (let ((snapshot (elinit--build-snapshot)))
      (should (equal (elinit--compute-entry-status "test" 'simple)
                     (elinit--compute-entry-status "test" 'simple snapshot)))
      (should (equal (elinit--compute-entry-status "oneshot-done" 'oneshot)
                     (elinit--compute-entry-status "oneshot-done" 'oneshot snapshot))))))

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
      (puthash "a" t elinit--failed)
      (let ((snapshot (elinit--build-snapshot)))
        (should (equal (elinit--health-summary)
                       (elinit--health-summary snapshot)))))))

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
    (puthash "disabled" 'disabled elinit--entry-state)
    (puthash "delayed" 'delayed elinit--entry-state)
    (puthash "waiting" 'waiting-on-deps elinit--entry-state)
    (puthash "crashed" t elinit--failed)
    (let ((snapshot (elinit--build-snapshot)))
      (dolist (test-case '(("disabled" . simple)
                           ("delayed" . simple)
                           ("waiting" . simple)
                           ("crashed" . simple)))
        (should (equal (elinit--compute-entry-reason (car test-case) (cdr test-case))
                       (elinit--compute-entry-reason (car test-case) (cdr test-case) snapshot)))))))

;;; Declarative Reconciler Tests
;;
;; The reconciler computes actions needed to converge actual state to
;; desired state.  The action matrix test is the comprehensive test;
;; the others verify specific safety properties.

(ert-deftest elinit-test-compute-actions-action-matrix ()
  "Comprehensive test of action matrix with mixed scenarios.
Covers add, remove, disable (running), already-running, failed."
  (elinit-test-with-unit-files
      '(("sleep 100" :id "a")
        ("sleep 200" :id "b")
        ("sleep 300" :id "c" :disabled t)
        ("sleep 400" :id "d")
        ("sleep 500" :id "e" :disabled t))
    (let* ((plan (elinit--build-plan (elinit--effective-programs)))
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
        (dolist (a actions)
          (puthash (plist-get a :id) a by-id))
        ;; a: new -> start
        (should (eq 'start (plist-get (gethash "a" by-id) :op)))
        ;; b: running -> noop
        (should (eq 'noop (plist-get (gethash "b" by-id) :op)))
        ;; c: running but disabled -> stop
        (should (eq 'stop (plist-get (gethash "c" by-id) :op)))
        ;; orphan: not in plan -> stop
        (should (eq 'stop (plist-get (gethash "orphan" by-id) :op)))
        ;; d: failed -> skip
        (should (eq 'skip (plist-get (gethash "d" by-id) :op)))
        ;; e: disabled (not running) -> skip
        (should (eq 'skip (plist-get (gethash "e" by-id) :op)))))))

(ert-deftest elinit-test-compute-actions-is-pure ()
  "Compute-actions does not modify any global state."
  (elinit-test-with-unit-files
      '(("sleep 100" :id "a")
        ("sleep 200" :id "b" :disabled t))
    (let* ((plan (elinit--build-plan (elinit--effective-programs)))
           (elinit--processes (make-hash-table :test 'equal))
           (elinit--failed (make-hash-table :test 'equal))
           (elinit--oneshot-completed (make-hash-table :test 'equal))
           (elinit--restart-override (make-hash-table :test 'equal)))
      (puthash "x" t elinit--failed)
      (let* ((failed-before (copy-hash-table elinit--failed))
             (snapshot (elinit-snapshot--create
                        :process-alive (make-hash-table :test 'equal)
                        :process-pids (make-hash-table :test 'equal)
                        :failed (copy-hash-table elinit--failed)
                        :oneshot-exit (make-hash-table :test 'equal)
                        :entry-state (make-hash-table :test 'equal)
                        :invalid (make-hash-table :test 'equal)
                        :enabled-override (make-hash-table :test 'equal)
                        :restart-override (make-hash-table :test 'equal)
                        :logging-override (make-hash-table :test 'equal)
                        :timestamp (float-time))))
        (elinit--compute-actions plan snapshot)
        (elinit--compute-actions plan snapshot)
        (should (equal (hash-table-count elinit--failed)
                       (hash-table-count failed-before)))
        (should (gethash "x" elinit--failed))))))

(ert-deftest elinit-test-compute-actions-idempotent ()
  "Idempotence: second call on converged state produces only noops/skips."
  (elinit-test-with-unit-files
      '(("sleep 100" :id "a")
        ("sleep 200" :id "b")
        ("echo done" :id "c" :type oneshot)
        ("sleep 300" :id "d" :disabled t))
    (let* ((plan (elinit--build-plan (elinit--effective-programs)))
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
        (should (= 4 (length actions)))
        (should-not (cl-find 'start actions :key (lambda (a) (plist-get a :op))))
        (should-not (cl-find 'stop actions :key (lambda (a) (plist-get a :op))))))))

(ert-deftest elinit-test-apply-actions-atomic-on-invalid-plan ()
  "Apply-actions does not mutate state when plan has no matching entries."
  (let* ((elinit--processes (make-hash-table :test 'equal))
         (elinit--restart-override (make-hash-table :test 'equal))
         (elinit--failed (make-hash-table :test 'equal))
         (plan (elinit--build-plan nil))
         (actions (list (list :op 'start :id "nonexistent" :reason 'added)
                        (list :op 'stop :id "also-nonexistent" :reason 'removed))))
    (let ((result (elinit--apply-actions actions plan)))
      (should (= 0 (plist-get result :started)))
      (should (= 0 (plist-get result :stopped))))))

;;; FSM Tests
;;
;; The entry state machine guards against invalid state transitions.

(ert-deftest elinit-test-fsm-invalid-state-rejected ()
  "Transitioning to an invalid state signals an error."
  (let ((elinit--entry-state (make-hash-table :test 'equal)))
    (should-error (elinit--transition-state "test" 'invalid-state)
                  :type 'error)))

(ert-deftest elinit-test-fsm-invalid-transition-rejected ()
  "Invalid state transitions signal an error."
  (let ((elinit--entry-state (make-hash-table :test 'equal)))
    (elinit--transition-state "test" 'pending)
    (elinit--transition-state "test" 'started)
    ;; Terminal -> non-terminal is invalid
    (should-error (elinit--transition-state "test" 'delayed)
                  :type 'error)))

(ert-deftest elinit-test-fsm-valid-transitions ()
  "Valid state transitions succeed through the full lifecycle."
  (let ((elinit--entry-state (make-hash-table :test 'equal)))
    (should (elinit--transition-state "a" 'pending))
    (should (eq 'pending (gethash "a" elinit--entry-state)))
    (should (elinit--transition-state "a" 'waiting-on-deps))
    (should (elinit--transition-state "a" 'delayed))
    (should (elinit--transition-state "a" 'started))
    (should (eq 'started (gethash "a" elinit--entry-state)))))

(ert-deftest elinit-test-fsm-force-invalid-transition ()
  "Force parameter allows invalid transitions with warning.
Returns nil (not t) and emits a warning for forced invalid transitions."
  (let ((elinit--entry-state (make-hash-table :test 'equal))
        (warnings nil))
    (elinit--transition-state "a" 'pending)
    (elinit--transition-state "a" 'started)
    (cl-letf (((symbol-function 'elinit--log)
               (lambda (_level fmt &rest args)
                 (push (apply #'format fmt args) warnings))))
      (let ((result (elinit--transition-state "a" 'delayed t)))
        (should (null result))
        (should (eq 'delayed (gethash "a" elinit--entry-state)))
        (should (= 1 (length warnings)))
        (should (string-match-p "Forced invalid transition" (car warnings)))))))

;;; Event Dispatcher Tests

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
      (let ((event (car events)))
        (should (eq 'process-exit (plist-get event :type)))
        (should (equal "myproc" (plist-get event :id)))
        (should (eq 'exited (plist-get (plist-get event :data) :status)))
        (should (= 0 (plist-get (plist-get event :data) :code)))))))

(ert-deftest elinit-test-spawn-failure-no-process-ready ()
  "Spawn failure emits process-failed but NOT process-ready.
Regression: process-ready was incorrectly emitted for failures."
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
    (elinit--transition-state "test" 'pending)
    (puthash "test" '("test" "cmd" 0 t always t simple nil t 30 nil)
             elinit--dag-entries)
    (cl-letf (((symbol-function 'run-hook-with-args)
               (lambda (hook &rest args)
                 (when (eq hook 'elinit-event-hook)
                   (push (car args) events))))
              ((symbol-function 'run-hooks) #'ignore))
      (elinit--dag-handle-spawn-failure "test"))
    (should (cl-find 'process-failed events :key (lambda (e) (plist-get e :type))))
    (should-not (cl-find 'process-ready events :key (lambda (e) (plist-get e :type))))))

(ert-deftest elinit-test-disabled-entry-no-process-ready ()
  "Disabled entries do NOT emit process-ready.
Regression: process-ready was incorrectly emitted for disabled entries."
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
    (puthash "test" '("test" "cmd" 0 nil always t simple nil t 30 nil)
             elinit--dag-entries)
    (cl-letf (((symbol-function 'run-hook-with-args)
               (lambda (hook &rest args)
                 (when (eq hook 'elinit-event-hook)
                   (push (car args) events))))
              ((symbol-function 'run-hooks) #'ignore))
      (elinit--dag-start-entry-async
       '("test" "cmd" 0 nil always t simple nil t 30 nil)))
    (should-not (cl-find 'process-ready events :key (lambda (e) (plist-get e :type))))))

(ert-deftest elinit-test-simple-spawn-emits-process-ready ()
  "Successful simple process spawn emits both process-started and process-ready."
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
    (elinit--transition-state "test" 'pending)
    (puthash "test" '("test" "sleep" 0 t always t simple nil t 30 nil)
             elinit--dag-entries)
    (cl-letf (((symbol-function 'run-hook-with-args)
               (lambda (hook &rest args)
                 (when (eq hook 'elinit-event-hook)
                   (push (car args) events))))
              ((symbol-function 'run-hooks) #'ignore))
      (elinit--dag-handle-spawn-success "test" 'simple nil))
    (should (cl-find 'process-started events :key (lambda (e) (plist-get e :type))))
    (should (cl-find 'process-ready events :key (lambda (e) (plist-get e :type))))))

(ert-deftest elinit-test-no-stderr-process-created ()
  "Starting a process does not create a separate stderr process.
Regression: stderr pipe processes used to pollute the process list."
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
            (should (gethash "test-no-stderr" elinit--processes))
            (should-not (get-process "test-no-stderr-stderr"))
            (should-not (get-process " test-no-stderr-stderr")))
        (when-let* ((proc (gethash "test-no-stderr" elinit--processes)))
          (delete-process proc))
        (delete-directory elinit-log-directory t)))))

;;; Plan Conflicts Tests

(ert-deftest elinit-test-plan-conflicts-metadata ()
  "Plan with :conflicts builds conflicts-deps hash."
  (let* ((programs '(("sleep 1" :id "a" :conflicts "b")
                     ("sleep 2" :id "b")))
         (plan (elinit--build-plan programs)))
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
    (should (null (gethash "a" (elinit-plan-conflicts-deps plan))))))

(ert-deftest elinit-test-plan-conflicts-fingerprint ()
  "Plan with conflicts has different fingerprint than without."
  (let* ((programs-a '(("sleep 1" :id "a") ("sleep 2" :id "b")))
         (programs-b '(("sleep 1" :id "a" :conflicts "b") ("sleep 2" :id "b")))
         (plan-a (elinit--build-plan programs-a))
         (plan-b (elinit--build-plan programs-b)))
    (should-not (equal (plist-get (elinit-plan-meta plan-a) :fingerprint)
                       (plist-get (elinit-plan-meta plan-b) :fingerprint)))))

(ert-deftest elinit-test-plan-conflicts-self-rejected ()
  "Self-reference in :conflicts is caught by validation."
  (let* ((programs '(("sleep 1" :id "a" :conflicts "a")))
         (plan (elinit--build-plan programs)))
    (should (gethash "a" (elinit-plan-invalid plan)))))

(ert-deftest elinit-test-plan-conflicts-both-directions ()
  "Both A and B declaring :conflicts produces symmetric metadata."
  (let* ((programs '(("sleep 1" :id "a" :conflicts "b")
                     ("sleep 2" :id "b" :conflicts "a")))
         (plan (elinit--build-plan programs)))
    (should (equal (gethash "a" (elinit-plan-conflicts-deps plan)) '("b")))
    (should (equal (gethash "b" (elinit-plan-conflicts-deps plan)) '("a")))))

(ert-deftest elinit-test-plan-entries-conflicts-normalized ()
  "Plan entries have normalized conflicts matching conflicts-deps.
Dropped missing targets must not appear in plan.entries conflicts."
  (let* ((programs '(("sleep 1" :id "a" :conflicts ("b" "ghost"))
                     ("sleep 2" :id "b")))
         (plan (elinit--build-plan programs))
         (entry-a (cl-find "a" (elinit-plan-entries plan)
                           :key #'elinit-entry-id :test #'equal)))
    (should (equal (elinit-entry-conflicts entry-a) '("b")))
    (should (equal (elinit-entry-conflicts entry-a)
                   (gethash "a" (elinit-plan-conflicts-deps plan))))))

(ert-deftest elinit-test-plan-cycle-fallback-preserves-conflicts ()
  "Cycle fallback clears dep edges but preserves conflict metadata."
  (let* ((programs '(("sleep 1" :id "a" :after "b" :conflicts "c")
                     ("sleep 2" :id "b" :after "a")
                     ("sleep 3" :id "c")))
         (plan (elinit--build-plan programs)))
    (should (gethash "a" (elinit-plan-cycle-fallback-ids plan)))
    (should (equal (gethash "a" (elinit-plan-conflicts-deps plan)) '("c")))
    (should (member "a" (gethash "c" (elinit-plan-conflict-reverse plan))))))

;;; Entry Validation Tests
;;
;; The entry validator enforces the keyword whitelist, type gates,
;; mutual exclusion rules, and structural guards.

(ert-deftest elinit-test-validate-unknown-keyword ()
  "Unknown keywords are rejected."
  (should (string-match "unknown keyword"
                        (elinit--validate-entry
                         '("foo" :bogus t)))))

(ert-deftest elinit-test-validate-rejects-legacy-oneshot-wait ()
  "Legacy :oneshot-wait keyword is rejected as unknown."
  (should (string-match "unknown keyword.*:oneshot-wait"
                        (elinit--validate-entry
                         '("foo" :type oneshot :oneshot-wait t)))))

(ert-deftest elinit-test-validate-invalid-type ()
  "Invalid :type values are rejected."
  (should (string-match ":type must be"
                        (elinit--validate-entry
                         '("foo" :type daemon)))))

(ert-deftest elinit-test-validate-invalid-stage ()
  ":stage produces deprecation error."
  (should (string-match ":stage is removed"
                        (elinit--validate-entry
                         '("foo" :stage boot)))))

(ert-deftest elinit-test-validate-invalid-delay ()
  "Invalid :delay values are rejected."
  (should (string-match ":delay must be"
                        (elinit--validate-entry
                         '("foo" :delay "slow"))))
  (should (string-match ":delay must be"
                        (elinit--validate-entry
                         '("foo" :delay -1)))))

(ert-deftest elinit-test-validate-id-must-be-string ()
  "Non-string :id values are rejected."
  (should (string-match ":id must be a string"
                        (elinit--validate-entry
                         '("foo" :id 42))))
  (should (string-match ":id must be a string"
                        (elinit--validate-entry
                         '("foo" :id foo-symbol))))
  ;; String :id is valid
  (should-not (elinit--validate-entry
               '("foo" :id "valid-string"))))

(ert-deftest elinit-test-validate-mutually-exclusive-enabled ()
  ":enabled and :disabled are mutually exclusive."
  (should (string-match "mutually exclusive"
                        (elinit--validate-entry
                         '("foo" :enabled t :disabled t)))))

(ert-deftest elinit-test-validate-mutually-exclusive-restart ()
  ":restart and :no-restart are mutually exclusive."
  (should (string-match "mutually exclusive"
                        (elinit--validate-entry
                         '("foo" :restart t :no-restart t)))))

(ert-deftest elinit-test-validate-simple-rejects-oneshot-keywords ()
  "Simple type rejects oneshot-specific keywords."
  (should (string-match ":oneshot-blocking is invalid for :type simple"
                        (elinit--validate-entry
                         '("foo" :type simple :oneshot-blocking t))))
  (should (string-match ":oneshot-async is invalid for :type simple"
                        (elinit--validate-entry
                         '("foo" :type simple :oneshot-async t))))
  (should (string-match ":oneshot-timeout is invalid for :type simple"
                        (elinit--validate-entry
                         '("foo" :type simple :oneshot-timeout 30)))))

(ert-deftest elinit-test-validate-oneshot-rejects-restart-keywords ()
  "Oneshot type rejects restart-specific keywords."
  (should (string-match ":restart is invalid for :type oneshot"
                        (elinit--validate-entry
                         '("foo" :type oneshot :restart t))))
  (should (string-match ":no-restart is invalid for :type oneshot"
                        (elinit--validate-entry
                         '("foo" :type oneshot :no-restart t)))))

(ert-deftest elinit-test-validate-multiple-errors ()
  "Multiple validation errors are collected."
  (let ((result (elinit--validate-entry
                 '("foo" :enabled t :disabled t :restart t :no-restart t))))
    (should (string-match "mutually exclusive" result))
    ;; Should contain both errors separated by semicolon
    (should (> (length (split-string result ";")) 1))))

(ert-deftest elinit-test-validate-mutually-exclusive-oneshot-blocking-async ()
  ":oneshot-blocking and :oneshot-async are mutually exclusive."
  (should (string-match "mutually exclusive"
                        (elinit--validate-entry
                         '("foo" :type oneshot :oneshot-blocking t :oneshot-async t)))))

(ert-deftest elinit-test-validate-type-must-be-symbol ()
  ":type must be a symbol, not a string."
  (should (string-match "must be a symbol"
                        (elinit--validate-entry
                         '("foo" :type "oneshot")))))

(ert-deftest elinit-test-validate-entry-odd-plist ()
  "Odd-length plist is rejected by entry validation."
  (let ((result (elinit--validate-entry '("cmd" :id "svc" :enabled))))
    (should (stringp result))
    (should (string-match-p "odd number of elements" result))))

(ert-deftest elinit-test-validate-entry-dotted-plist ()
  "Dotted plist is rejected by entry validation."
  (let ((result (elinit--validate-entry '("cmd" :enabled . t))))
    (should (stringp result))
    (should (string-match-p "must be a proper key/value list" result))))

(ert-deftest elinit-test-validate-entry-empty-string-command ()
  "Empty string command is rejected by entry validation."
  (let ((result (elinit--validate-entry "")))
    (should (stringp result))
    (should (string-match-p "empty or whitespace-only" result))))

(ert-deftest elinit-test-validate-entry-empty-id ()
  "Empty :id is rejected by entry validation."
  (let ((result (elinit--validate-entry '("cmd" :id ""))))
    (should (stringp result))
    (should (string-match-p ":id must not be empty" result))))

(ert-deftest elinit-test-validate-entry-id-with-slash ()
  "ID containing slash is rejected by entry validation."
  (let ((result (elinit--validate-entry '("cmd" :id "../etc/passwd"))))
    (should (stringp result))
    (should (string-match-p "invalid characters" result))))

(ert-deftest elinit-test-validate-boolean-enabled-string ()
  "String value for :enabled is rejected.
Representative test for strict boolean flag checking; all boolean
flags (:enabled, :disabled, :logging, :no-restart, :oneshot-blocking,
:oneshot-async) use the same validation path."
  (let ((result (elinit--validate-entry '("cmd" :enabled "yes"))))
    (should (stringp result))
    (should (string-match-p ":enabled must be t or nil" result))))

(ert-deftest elinit-test-validate-after-self-dependency ()
  "Self-reference in :after is rejected."
  (let ((result (elinit--validate-entry '("cmd" :id "a" :after "a"))))
    (should (stringp result))
    (should (string-match-p ":after must not reference the entry's own ID" result))))

(ert-deftest elinit-test-validate-after-self-dependency-derived-id ()
  "Self-reference in :after detected via derived command ID."
  (let ((result (elinit--validate-entry '("svc" :after "svc"))))
    (should (stringp result))
    (should (string-match-p ":after must not reference the entry's own ID" result))))

(ert-deftest elinit-test-validate-restart-sec-with-no-restart-true ()
  "Restart-sec with :no-restart t is contradictory."
  (let ((result (elinit--validate-entry
                 '("cmd" :no-restart t :restart-sec 5))))
    (should (stringp result))
    (should (string-match-p "contradictory" result))))

(ert-deftest elinit-test-validate-environment-empty-key ()
  "Empty environment key is rejected."
  (let ((result (elinit--validate-entry
                 '("cmd" :environment (("" . "val"))))))
    (should (stringp result))
    (should (string-match-p ":environment key .* is not a valid variable name"
                            result))))

(ert-deftest elinit-test-validate-environment-duplicate-key ()
  "Duplicate environment key is rejected."
  (let ((result (elinit--validate-entry
                 '("cmd" :environment (("FOO" . "a") ("FOO" . "b"))))))
    (should (stringp result))
    (should (string-match-p ":environment contains duplicate key" result))))

(ert-deftest elinit-test-validate-exec-stop-empty-string ()
  "Empty :exec-stop string is rejected."
  (let ((result (elinit--validate-entry '("cmd" :exec-stop ""))))
    (should (stringp result))
    (should (string-match-p ":exec-stop must not contain empty commands" result))))

(ert-deftest elinit-test-validate-success-exit-status-negative ()
  "Negative :success-exit-status code is rejected."
  (let ((result (elinit--validate-entry '("cmd" :success-exit-status (-1)))))
    (should (stringp result))
    (should (string-match-p ":success-exit-status code .* is outside valid range"
                            result))))

(ert-deftest elinit-test-validate-entry-invalid-entry-type ()
  "Non-string non-list entries are rejected."
  (should (string-match-p "entry must be a string or list"
                          (elinit--validate-entry 123)))
  (should (string-match-p "entry must be a string or list"
                          (elinit--validate-entry '(123 :id "x"))))
  (should (string-match-p "entry must be a string or list"
                          (elinit--validate-entry nil))))

;;; Identity and Spawn Abstraction Tests
;;
;; User/group identity threading, trust enforcement, runas helper
;; integration, and shell metachar handling for process spawning.

(ert-deftest elinit-test-validate-user-invalid-symbol ()
  "Validation rejects :user as symbol.
Representative test for user/group type checking."
  (should (string-match-p ":user must be"
                          (elinit--validate-entry
                           '("cmd" :user postgres)))))

(ert-deftest elinit-test-validate-user-empty-string ()
  "Validation rejects :user as empty string."
  (should (string-match-p ":user must not be an empty string"
                          (elinit--validate-entry
                           '("cmd" :user "")))))

(ert-deftest elinit-test-validate-user-negative-integer ()
  "Validation rejects :user as negative integer."
  (should (string-match-p ":user must be a non-negative integer"
                          (elinit--validate-entry
                           '("cmd" :user -1)))))

(ert-deftest elinit-test-build-launch-command-user-only ()
  "Build launch command prepends helper with --user when user is set."
  (let ((elinit-runas-command "/usr/libexec/elinit-runas"))
    (let ((result (elinit--build-launch-command "sleep 300" "alice" nil)))
      (should (equal (car result) "/usr/libexec/elinit-runas"))
      (should (member "--user" result))
      (should (equal (nth (1+ (cl-position "--user" result :test #'equal))
                          result)
                     "alice"))
      (should (member "--" result))
      ;; Program after "--" is resolved to absolute path
      (should (string-suffix-p "sleep" (nth (1+ (cl-position "--" result
                                                             :test #'equal))
                                            result)))
      (should (equal (car (last result)) "300")))))

(ert-deftest elinit-test-build-launch-command-integer-uid ()
  "Build launch command converts integer uid to string for helper."
  (let ((elinit-runas-command "/usr/libexec/elinit-runas"))
    (let ((result (elinit--build-launch-command "sleep 300" 1000 33)))
      (should (equal (nth (1+ (cl-position "--user" result :test #'equal))
                          result)
                     "1000"))
      (should (equal (nth (1+ (cl-position "--group" result :test #'equal))
                          result)
                     "33")))))

(ert-deftest elinit-test-build-launch-command-resolves-path ()
  "Build launch command resolves program to absolute path when wrapping."
  (let ((elinit-runas-command "/usr/libexec/elinit-runas"))
    (let ((result (elinit--build-launch-command "sleep 300" "alice" nil)))
      ;; The program after "--" should be an absolute path
      (let ((cmd-after-sep (cdr (member "--" result))))
        (should cmd-after-sep)
        (should (file-name-absolute-p (car cmd-after-sep)))))))

(ert-deftest elinit-test-shell-metachar-p ()
  "Detect shell metacharacters in command strings."
  (should (elinit--shell-metachar-p "cmd1 && cmd2"))
  (should (elinit--shell-metachar-p "cmd1 || cmd2"))
  (should (elinit--shell-metachar-p "cmd1 | cmd2"))
  (should (elinit--shell-metachar-p "cmd1 ; cmd2"))
  (should (elinit--shell-metachar-p "cmd > file"))
  (should (elinit--shell-metachar-p "cmd < file"))
  (should (elinit--shell-metachar-p "echo $HOME"))
  (should (elinit--shell-metachar-p "echo `date`"))
  (should-not (elinit--shell-metachar-p "sleep 300"))
  (should-not (elinit--shell-metachar-p "/usr/bin/logrotate --log-dir /tmp")))

(ert-deftest elinit-test-build-launch-command-shell-metachar ()
  "Build launch command wraps in sh -c when shell operators present."
  (let ((result (elinit--build-launch-command "cmd1 && cmd2")))
    (should (equal (car result) shell-file-name))
    (should (equal (nth 1 result) shell-command-switch))
    (should (equal (nth 2 result) "cmd1 && cmd2"))))

;;; elinit-runas Helper Tests
;;
;; These tests exercise the helper binary's error paths.
;; Full privilege-drop tests require root and are gated by
;; ELINIT_TEST_ROOT env var (not run in normal CI).

(ert-deftest elinit-test-runas-missing-command ()
  "Helper exits 111 when no command is given after \"--\"."
  (skip-unless (file-executable-p elinit-test-runas-binary))
  (with-temp-buffer
    (let ((code (call-process elinit-test-runas-binary nil t nil
                              "--user" "nobody")))
      (should (= code 111)))))

(ert-deftest elinit-test-runas-unknown-user ()
  "Helper exits 112 for unknown user name."
  (skip-unless (file-executable-p elinit-test-runas-binary))
  (with-temp-buffer
    (let ((code (call-process elinit-test-runas-binary nil t nil
                              "--user" "nonexistent_user_xyz_sv"
                              "--" "echo" "hi")))
      (should (= code 112))
      (should (string-match-p "unknown user"
                              (buffer-string))))))

(ert-deftest elinit-test-runas-privdrop-fails-non-root ()
  "Helper exits 113 when non-root tries to drop to another user."
  (skip-unless (file-executable-p elinit-test-runas-binary))
  (skip-unless (not (= (user-uid) 0)))
  (with-temp-buffer
    (let ((code (call-process elinit-test-runas-binary nil t nil
                              "--user" "root" "--" "echo" "hi")))
      (should (= code 113)))))

(ert-deftest elinit-test-runas-exec-fails-bare-name ()
  "Helper exits 114 when target is a bare name (execv has no PATH search)."
  (skip-unless (file-executable-p elinit-test-runas-binary))
  (skip-unless (= (user-uid) 0))
  (skip-unless (getenv "ELINIT_TEST_ROOT"))
  (with-temp-buffer
    (let ((code (call-process elinit-test-runas-binary nil t nil
                              "--user" (number-to-string (user-uid))
                              "--" "nonexistent_binary_xyz")))
      (should (= code 114))
      (should (string-match-p "exec nonexistent_binary_xyz"
                              (buffer-string))))))

(ert-deftest elinit-test-runas-root-success ()
  "Helper runs command as target user when invoked by root."
  (skip-unless (file-executable-p elinit-test-runas-binary))
  (skip-unless (= (user-uid) 0))
  (skip-unless (getenv "ELINIT_TEST_ROOT"))
  (with-temp-buffer
    (let ((code (call-process elinit-test-runas-binary nil t nil
                              "--user" "nobody"
                              "--" "/usr/bin/id" "-u")))
      (should (= code 0))
      (should (string-match-p "^[0-9]+$"
                              (string-trim (buffer-string)))))))

;;; Identity Integration Tests

(ert-deftest elinit-test-start-process-nonroot-rejects-identity ()
  "Non-root elinit returns nil when user/group identity is requested.
Security guard: identity change is a privileged operation."
  (skip-unless (not (zerop (user-uid))))
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--shutting-down nil))
    (should-not (elinit--start-process
                 "svc" "sleep 300" nil 'simple 'yes nil
                 nil nil nil nil nil "alice" nil))))

(ert-deftest elinit-test-dag-start-threads-user-group ()
  "DAG start passes user/group from entry through to start-process."
  (let* ((entry (elinit--parse-entry
                 '("sleep 300" :id "svc1" :user "www-data" :group "www-data")))
         (elinit--dag-started (make-hash-table :test 'equal))
         (elinit--start-times (make-hash-table :test 'equal))
         (elinit--dag-active-starts 0)
         (captured-user nil)
         (captured-group nil))
    (cl-letf (((symbol-function 'elinit--start-process)
               (lambda (_id _cmd _log _type _restart &rest args)
                 (setq captured-user (nth 6 args))
                 (setq captured-group (nth 7 args))
                 t))
              ((symbol-function 'elinit--dag-handle-spawn-success) #'ignore)
              ((symbol-function 'executable-find) (lambda (_) t)))
      (elinit--dag-do-start
       (elinit-entry-id entry)
       (elinit-entry-command entry)
       (elinit-entry-logging-p entry)
       (elinit-entry-stdout-log-file entry)
       (elinit-entry-stderr-log-file entry)
       (elinit-entry-type entry)
       (elinit-entry-restart-policy entry)
       (elinit-entry-oneshot-blocking entry)
       (elinit-entry-oneshot-timeout entry)
       (elinit-entry-working-directory entry)
       (elinit-entry-environment entry)
       (elinit-entry-environment-file entry)
       (elinit-entry-restart-sec entry)
       nil  ; unit-file-directory
       "www-data"
       "www-data"))
    (should (equal captured-user "www-data"))
    (should (equal captured-group "www-data"))))

(ert-deftest elinit-test-sentinel-preserves-identity-for-restart ()
  "Process sentinel forwards user/group to schedule-restart."
  (let* ((captured-user nil)
         (captured-group nil)
         (sentinel (elinit--make-process-sentinel
                    "svc-rs" "sleep 300" nil 'simple 'yes
                    nil nil nil nil nil
                    "alice" "staff")))
    (cl-letf (((symbol-function 'elinit--schedule-restart)
               (lambda (_id _cmd _log _type _restart _status _code &rest args)
                 (setq captured-user (nth 5 args))
                 (setq captured-group (nth 6 args))))
              ((symbol-function 'elinit--should-restart-p) (lambda (&rest _) t))
              ((symbol-function 'elinit--check-crash-loop) (lambda (_) nil))
              ((symbol-function 'elinit--get-effective-restart)
               (lambda (_id _cfg) 'yes))
              ((symbol-function 'elinit--maybe-refresh-dashboard) #'ignore)
              ((symbol-function 'elinit--emit-event) #'ignore)
              ((symbol-function 'elinit--get-entry-for-id) (lambda (_) nil)))
      (let* ((elinit--processes (make-hash-table :test 'equal))
             (elinit--last-exit-info (make-hash-table :test 'equal))
             (elinit--shutting-down nil)
             (elinit--manually-stopped (make-hash-table :test 'equal))
             (elinit--enabled-override (make-hash-table :test 'equal))
             (elinit--failed (make-hash-table :test 'equal))
             (proc (start-process "svc-rs" nil "sleep" "300")))
        (puthash "svc-rs" proc elinit--processes)
        (unwind-protect
            (progn
              (delete-process proc)
              (sleep-for 0.1)
              (funcall sentinel proc "finished\n")
              (should (equal captured-user "alice"))
              (should (equal captured-group "staff")))
          (when (process-live-p proc)
            (delete-process proc)))))))

;;; Trust Enforcement Tests

(ert-deftest elinit-test-identity-trust-no-units-module ()
  "Trust check fails closed when units module is not loaded."
  (cl-letf (((symbol-function 'elinit--unit-file-path)
             nil))
    (should-not (elinit--identity-source-trusted-p "svc1"))))

(ert-deftest elinit-test-identity-trust-non-root-owned ()
  "Trust check rejects unit file not owned by root."
  (skip-unless (not (zerop (user-uid))))
  (let ((temp-file (make-temp-file "trust-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "(:id \"svc1\" :command \"echo hi\" :user \"alice\")\n"))
          (cl-letf (((symbol-function 'elinit--unit-file-path)
                     (lambda (_id) temp-file)))
            (should-not (elinit--identity-source-trusted-p "svc1"))))
      (delete-file temp-file))))

(ert-deftest elinit-test-identity-trust-world-writable ()
  "Trust check rejects world-writable unit file even if root-owned."
  (skip-unless (= (user-uid) 0))
  (skip-unless (getenv "ELINIT_TEST_ROOT"))
  (let ((temp-file (make-temp-file "trust-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "(:id \"svc1\" :command \"echo hi\" :user \"alice\")\n"))
          (set-file-modes temp-file #o666)
          (cl-letf (((symbol-function 'elinit--unit-file-path)
                     (lambda (_id) temp-file)))
            (should-not (elinit--identity-source-trusted-p "svc1"))))
      (delete-file temp-file))))

(ert-deftest elinit-test-identity-trust-root-owned-ok ()
  "Trust check accepts root-owned, non-world-writable unit file."
  (skip-unless (= (user-uid) 0))
  (skip-unless (getenv "ELINIT_TEST_ROOT"))
  (let ((temp-file (make-temp-file "trust-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "(:id \"svc1\" :command \"echo hi\" :user \"alice\")\n"))
          (set-file-modes temp-file #o644)
          (cl-letf (((symbol-function 'elinit--unit-file-path)
                     (lambda (_id) temp-file)))
            (should (elinit--identity-source-trusted-p "svc1"))))
      (delete-file temp-file))))

(ert-deftest elinit-test-manual-start-root-untrusted-source ()
  "Manual start returns error when root but unit source is untrusted."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc1" :user "alice"))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--restart-timers (make-hash-table :test 'equal))
           (elinit--manually-stopped (make-hash-table :test 'equal))
           (elinit--manually-started (make-hash-table :test 'equal))
           (elinit--failed (make-hash-table :test 'equal))
           (elinit--restart-times (make-hash-table :test 'equal))
           (elinit--enabled-override (make-hash-table :test 'equal))
           (elinit--mask-override (make-hash-table :test 'equal))
           (elinit--oneshot-completed (make-hash-table :test 'equal))
           (elinit--remain-active (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal))
           (elinit--cycle-fallback-ids (make-hash-table :test 'equal))
           (elinit--computed-deps (make-hash-table :test 'equal))
           (elinit--logging (make-hash-table :test 'equal)))
      (cl-letf (((symbol-function 'user-uid) (lambda () 0))
                ((symbol-function 'elinit--identity-source-trusted-p)
                 (lambda (_id) nil)))
        (let ((result (elinit--manual-start "svc1")))
          (should (eq (plist-get result :status) 'error))
          (should (string-match-p "not trusted"
                                  (plist-get result :reason))))))))

;;; Spawn Failure Reason Tests

(ert-deftest elinit-test-spawn-failure-reason-identity-non-root ()
  "Spawn failure reason records identity context for non-root."
  (let ((elinit--spawn-failure-reason (make-hash-table :test 'equal))
        (elinit--processes (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--restart-times (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--start-times (make-hash-table :test 'equal))
        (elinit--ready-times (make-hash-table :test 'equal))
        (elinit--shutting-down nil))
    (unless (zerop (user-uid))
      (elinit--start-process
       "svc" "sleep 300" nil 'simple 'yes nil
       nil nil nil nil nil "alice" "staff")
      (let ((reason (gethash "svc" elinit--spawn-failure-reason)))
        (should reason)
        (should (string-match "identity change requires root" reason))
        (should (string-match "user=alice" reason))
        (should (string-match "group=staff" reason))))))

(ert-deftest elinit-test-compute-entry-reason-identity-context ()
  "Entry reason returns specific identity context instead of generic."
  (let ((elinit--entry-state (make-hash-table :test 'equal))
        (elinit--spawn-failure-reason (make-hash-table :test 'equal))
        (elinit--mask-override (make-hash-table :test 'equal))
        (elinit--processes (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal)))
    (puthash "svc" 'failed-to-spawn elinit--entry-state)
    (puthash "svc" "identity change requires root (user=alice group=staff)"
             elinit--spawn-failure-reason)
    (let ((reason (elinit--compute-entry-reason "svc" 'simple)))
      (should (string-match "identity change requires root" reason))
      (should (string-match "user=alice" reason)))))

(ert-deftest elinit-test-reset-failed-clears-spawn-failure-reason ()
  "Reset-failed clears specific spawn failure reason."
  (let ((elinit--failed (make-hash-table :test 'equal))
        (elinit--restart-times (make-hash-table :test 'equal))
        (elinit--spawn-failure-reason (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal)))
    (puthash "svc" t elinit--failed)
    (puthash "svc" "identity change requires root (user=alice group=nil)"
             elinit--spawn-failure-reason)
    (elinit--reset-failed "svc")
    (should-not (gethash "svc" elinit--spawn-failure-reason))))

(ert-deftest elinit-test-reload-running-simple-passes-user-group ()
  "Reload of a running simple unit threads user and group to start-process."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--manually-started (make-hash-table :test 'equal))
        (elinit--mask-override (make-hash-table :test 'equal))
        (elinit--invalid (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--restart-times (make-hash-table :test 'equal))
        (captured-user nil)
        (captured-group nil)
        (proc (start-process "test-reload-id" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "test-reload-id" proc elinit--processes)
          (cl-letf (((symbol-function 'elinit--reload-find-entry)
                     (lambda (_id)
                       (list "test-reload-id" "sleep 300" 0 t 'always t
                             nil nil 'simple nil nil 30 nil nil
                             nil nil nil nil nil nil
                             nil nil nil nil nil nil nil nil
                             "webuser" "webgrp"
                             nil nil nil nil nil nil nil nil
                             nil nil nil nil nil nil)))
                    ((symbol-function 'elinit--manual-stop)
                     (lambda (_id)
                       (list :status 'stopped :reason nil)))
                    ((symbol-function 'elinit--start-process)
                     (lambda (_id _cmd _logging _type _restart
                              &optional _is-restart _wd _env _ef _rs _ufd
                              user group _stdout-log-file _stderr-log-file
                              _sandbox-entry _log-format _limits-entry)
                       (setq captured-user user)
                       (setq captured-group group)
                       t))
                    ((symbol-function 'elinit--unit-file-directory-for-id)
                     (lambda (_id) nil))
                    ((symbol-function 'executable-find)
                     (lambda (_cmd) "/usr/bin/sleep")))
            (let ((result (elinit--reload-unit "test-reload-id")))
              (should (equal "reloaded" (plist-get result :action)))
              (should (equal "webuser" captured-user))
              (should (equal "webgrp" captured-group)))))
      (when (process-live-p proc)
        (delete-process proc)))))

;;; Restart Policy Tests
;;
;; The restart policy is a four-way enum: always, no, on-failure,
;; on-success.  The policy matrix test is the comprehensive test.

(ert-deftest elinit-test-parse-restart-policy-symbols ()
  "All four restart policy symbols parse correctly."
  (let ((always (elinit--parse-entry '("foo" :restart always)))
        (no (elinit--parse-entry '("foo" :restart no)))
        (on-failure (elinit--parse-entry '("foo" :restart on-failure)))
        (on-success (elinit--parse-entry '("foo" :restart on-success))))
    (should (eq (nth 4 always) 'always))
    (should (eq (nth 4 no) 'no))
    (should (eq (nth 4 on-failure) 'on-failure))
    (should (eq (nth 4 on-success) 'on-success))))

(ert-deftest elinit-test-parse-restart-policy-boolean-compat ()
  "Boolean :restart values are normalized to policy symbols.
Backward compatibility: t -> always, nil -> no."
  (let ((bool-t (elinit--parse-entry '("foo" :restart t)))
        (bool-nil (elinit--parse-entry '("foo" :restart nil)))
        (no-restart (elinit--parse-entry '("foo" :no-restart t))))
    (should (eq (nth 4 bool-t) 'always))
    (should (eq (nth 4 bool-nil) 'no))
    (should (eq (nth 4 no-restart) 'no))))

(ert-deftest elinit-test-validate-restart-invalid-symbol ()
  "Invalid :restart symbol is rejected by validation."
  (let ((reason (elinit--validate-entry '("foo" :restart bogus))))
    (should reason)
    (should (string-match-p ":restart" reason))))

(ert-deftest elinit-test-clean-exit-p ()
  "Clean exit predicate handles all cases.
Exit 0 is clean, non-zero is dirty, signals 1/2/13/15 are clean."
  (should (elinit--clean-exit-p 'exit 0))
  (should-not (elinit--clean-exit-p 'exit 1))
  (should-not (elinit--clean-exit-p 'exit 127))
  (should (elinit--clean-exit-p 'signal 1))
  (should (elinit--clean-exit-p 'signal 2))
  (should (elinit--clean-exit-p 'signal 13))
  (should (elinit--clean-exit-p 'signal 15))
  (should-not (elinit--clean-exit-p 'signal 9))
  (should-not (elinit--clean-exit-p 'signal 11)))

(ert-deftest elinit-test-should-restart-p ()
  "Full policy x exit-type matrix for restart decisions."
  ;; always: restart on any exit
  (should (elinit--should-restart-p 'always 'exit 0))
  (should (elinit--should-restart-p 'always 'exit 1))
  (should (elinit--should-restart-p 'always 'signal 9))
  (should (elinit--should-restart-p 'always 'signal 15))
  ;; no: never restart
  (should-not (elinit--should-restart-p 'no 'exit 0))
  (should-not (elinit--should-restart-p 'no 'exit 1))
  (should-not (elinit--should-restart-p 'no 'signal 9))
  (should-not (elinit--should-restart-p 'no 'signal 15))
  ;; on-failure: restart only on non-clean exit
  (should-not (elinit--should-restart-p 'on-failure 'exit 0))
  (should (elinit--should-restart-p 'on-failure 'exit 1))
  (should (elinit--should-restart-p 'on-failure 'signal 9))
  (should-not (elinit--should-restart-p 'on-failure 'signal 15))
  ;; on-success: restart only on clean exit
  (should (elinit--should-restart-p 'on-success 'exit 0))
  (should-not (elinit--should-restart-p 'on-success 'exit 1))
  (should-not (elinit--should-restart-p 'on-success 'signal 9))
  (should (elinit--should-restart-p 'on-success 'signal 15))
  ;; Legacy boolean compat
  (should (elinit--should-restart-p t 'exit 1))
  (should-not (elinit--should-restart-p nil 'exit 1)))

(ert-deftest elinit-test-clean-exit-p-extra-codes ()
  "Extra exit codes from :success-exit-status are treated as clean."
  (let ((ses '(:codes (42 77) :signals nil)))
    (should-not (elinit--clean-exit-p 'exit 42))
    (should (elinit--clean-exit-p 'exit 42 ses))
    (should (elinit--clean-exit-p 'exit 77 ses))
    (should-not (elinit--clean-exit-p 'exit 99 ses))
    (should (elinit--clean-exit-p 'exit 0 ses))))

(ert-deftest elinit-test-clean-exit-p-extra-signals ()
  "Extra signals from :success-exit-status are treated as clean."
  (let ((ses '(:codes nil :signals (SIGUSR1 SIGUSR2))))
    (should-not (elinit--clean-exit-p 'signal 10))
    (should (elinit--clean-exit-p 'signal 10 ses))
    (should (elinit--clean-exit-p 'signal 12 ses))
    (should-not (elinit--clean-exit-p 'signal 9 ses))
    (should (elinit--clean-exit-p 'signal 15 ses))))

(ert-deftest elinit-test-should-restart-p-on-failure-extra-code ()
  "On-failure policy: extra success code suppresses restart."
  (let ((ses '(:codes (42) :signals nil)))
    (should (elinit--should-restart-p 'on-failure 'exit 42))
    (should-not (elinit--should-restart-p 'on-failure 'exit 42 ses))
    (should (elinit--should-restart-p 'on-failure 'exit 1 ses))))

(ert-deftest elinit-test-should-restart-p-always-no-unaffected ()
  "Always/no policies unaffected by :success-exit-status."
  (let ((ses '(:codes (42) :signals (SIGUSR1))))
    (should (elinit--should-restart-p 'always 'exit 42 ses))
    (should-not (elinit--should-restart-p 'no 'exit 42 ses))))

(ert-deftest elinit-test-cycle-restart-policy ()
  "Cycle restart policy follows no -> on-success -> on-failure -> always -> no."
  (should (eq 'on-success (elinit--cycle-restart-policy 'no)))
  (should (eq 'on-failure (elinit--cycle-restart-policy 'on-success)))
  (should (eq 'always (elinit--cycle-restart-policy 'on-failure)))
  (should (eq 'no (elinit--cycle-restart-policy 'always)))
  (should (eq 'no (elinit--cycle-restart-policy 'bogus))))

;;; Conflict Suppression Tests
;;
;; Conflict suppression prevents restart oscillation between
;; mutually-exclusive units.

(ert-deftest elinit-test-conflict-suppressed-blocks-restart ()
  "Conflict-suppressed entry does not auto-restart via sentinel."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--conflict-suppressed (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--logging (make-hash-table :test 'equal))
        (elinit--writers (make-hash-table :test 'equal))
        (elinit--stderr-writers (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--oneshot-callbacks (make-hash-table :test 'equal))
        (elinit--crash-log (make-hash-table :test 'equal))
        (elinit--last-exit-info (make-hash-table :test 'equal))
        (elinit--spawn-failure-reason (make-hash-table :test 'equal))
        (elinit--shutting-down nil)
        (restart-scheduled nil))
    (puthash "svc" "other" elinit--conflict-suppressed)
    (puthash "svc" t elinit--manually-stopped)
    (let ((fake-proc (start-process "svc" nil "true")))
      (puthash "svc" fake-proc elinit--processes)
      (unwind-protect
          (cl-letf (((symbol-function 'elinit--schedule-restart)
                     (lambda (&rest _args)
                       (setq restart-scheduled t)))
                    ((symbol-function 'elinit--stop-writer-if-same) #'ignore)
                    ((symbol-function 'elinit--emit-event) #'ignore)
                    ((symbol-function 'elinit--maybe-refresh-dashboard) #'ignore)
                    ((symbol-function 'elinit--should-restart-p)
                     (lambda (&rest _) t)))
            (let ((sentinel (elinit--make-process-sentinel
                             "svc" '("sleep" "300") t 'simple 'always)))
              (while (process-live-p fake-proc) (sit-for 0.05))
              (funcall sentinel fake-proc "finished\n")
              (should-not restart-scheduled)))
        (when (process-live-p fake-proc)
          (delete-process fake-proc))))))

(ert-deftest elinit-test-conflict-suppression-reason ()
  "Conflict-suppressed entry shows reason in compute-entry-reason."
  (let ((elinit--conflict-suppressed (make-hash-table :test 'equal))
        (elinit--mask-override (make-hash-table :test 'equal))
        (elinit--processes (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--entry-state (make-hash-table :test 'equal))
        (elinit--spawn-failure-reason (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--target-convergence-reasons nil))
    (puthash "b" "a" elinit--conflict-suppressed)
    (let ((reason (elinit--compute-entry-reason "b" 'simple)))
      (should (stringp reason))
      (should (string-match-p "conflict-stopped" reason))
      (should (string-match-p "by a" reason)))))

(ert-deftest elinit-test-conflict-no-oscillation ()
  "Mutual conflict does not cause oscillation via suppression."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--conflict-suppressed (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal)))
    (let* ((programs '(("sleep 1" :id "a" :conflicts "b")
                       ("sleep 2" :id "b" :conflicts "a")))
           (plan (elinit--build-plan programs)))
      (let ((proc-b (start-process "b" nil "sleep" "300")))
        (puthash "b" proc-b elinit--processes)
        (elinit--conflict-preflight "a" plan)
        (should (equal (gethash "b" elinit--conflict-suppressed) "a"))
        (should (gethash "b" elinit--manually-stopped))
        (when (process-live-p proc-b)
          (delete-process proc-b))))))

(ert-deftest elinit-test-conflict-stopped-oneshot-reason ()
  "Conflict-stopped oneshot shows reason even after signal exit.
When a live oneshot is killed by conflict preflight, the sentinel
records the exit.  The reason function must still report conflict-stopped."
  (let ((elinit--conflict-suppressed (make-hash-table :test 'equal))
        (elinit--mask-override (make-hash-table :test 'equal))
        (elinit--processes (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--entry-state (make-hash-table :test 'equal))
        (elinit--spawn-failure-reason (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--target-convergence-reasons nil))
    (puthash "b" "a" elinit--conflict-suppressed)
    (puthash "b" -15 elinit--oneshot-completed)
    (let ((reason (elinit--compute-entry-reason "b" 'oneshot)))
      (should (stringp reason))
      (should (string-match-p "conflict-stopped" reason))
      (should (string-match-p "by a" reason)))))

(ert-deftest elinit-test-suppressed-exit-skips-crash-loop ()
  "Conflict-suppressed exit does not mutate crash-loop state.
Safety invariant: suppressed exits must not push timestamps into
restart-times or mark the unit as failed."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--conflict-suppressed (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--restart-times (make-hash-table :test 'equal))
        (elinit--entry-state (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--last-exit-info (make-hash-table :test 'equal))
        (elinit--current-plan nil)
        (elinit--shutting-down nil)
        (restart-scheduled nil))
    (let* ((programs '(("sleep 1" :id "a" :conflicts "b")
                       ("sleep 2" :id "b")))
           (plan (elinit--build-plan programs)))
      (setq elinit--current-plan plan)
      (let ((b-proc (start-process "b" nil "sleep" "300")))
        (puthash "b" b-proc elinit--processes)
        (elinit--conflict-preflight "a" plan)
        (should (gethash "b" elinit--conflict-suppressed))
        (cl-letf (((symbol-function 'elinit--schedule-restart)
                   (lambda (&rest _) (setq restart-scheduled t)))
                  ((symbol-function 'elinit--maybe-refresh-dashboard) #'ignore)
                  ((symbol-function 'elinit--emit-event) #'ignore)
                  ((symbol-function 'elinit--stop-writer-if-same) #'ignore))
          (let ((sentinel (elinit--make-process-sentinel
                           "b" "sleep 300" nil 'simple 'always)))
            (funcall sentinel b-proc "killed: 15\n")))
        (should-not (gethash "b" elinit--restart-times))
        (should-not (gethash "b" elinit--failed))
        (should-not restart-scheduled)
        (when (process-live-p b-proc)
          (delete-process b-proc))))))

;;; P2 Keyword Parsing and Validation Tests

(ert-deftest elinit-test-parse-entry-environment-file-string ()
  "Parsed entry normalizes :environment-file string to list."
  (let ((entry (elinit--parse-entry
                '("echo hi" :id "svc" :environment-file "/etc/env"))))
    (should (equal (elinit-entry-environment-file entry)
                   '("/etc/env")))))

(ert-deftest elinit-test-parse-entry-restart-sec-zero ()
  "Parsed entry accepts :restart-sec 0."
  (let ((entry (elinit--parse-entry
                '("my-daemon" :id "svc" :restart-sec 0))))
    (should (equal (elinit-entry-restart-sec entry) 0))))

(ert-deftest elinit-test-validate-duplicate-key-entry ()
  "Duplicate plist keys are rejected in entry validation."
  (let ((reason (elinit--validate-entry
                 '("cmd" :id "svc" :delay 1 :delay 2))))
    (should reason)
    (should (string-match-p "duplicate key :delay" reason))))

(ert-deftest elinit-test-validate-restart-sec-negative ()
  "Negative :restart-sec is rejected."
  (let ((reason (elinit--validate-entry
                 '("cmd" :id "svc" :restart-sec -1))))
    (should reason)
    (should (string-match-p ":restart-sec must be" reason))))

(ert-deftest elinit-test-validate-exec-stop-oneshot-rejected ()
  "The :exec-stop keyword is rejected for oneshot type."
  (let ((reason (elinit--validate-entry
                 '("cmd" :id "svc" :type oneshot
                   :exec-stop "stop-cmd"))))
    (should reason)
    (should (string-match-p ":exec-stop is invalid for :type oneshot" reason))))

(ert-deftest elinit-test-entry-to-service-new-fields ()
  "Entry-to-service conversion carries all six new fields."
  (let* ((entry (elinit--parse-entry
                 '("my-daemon" :id "svc"
                   :working-directory "/opt"
                   :environment (("K" . "V"))
                   :environment-file ("/etc/env")
                   :exec-stop ("stop1" "stop2")
                   :exec-reload "reload1"
                   :restart-sec 3)))
         (svc (elinit-entry-to-service entry)))
    (should (equal (elinit-service-working-directory svc) "/opt"))
    (should (equal (elinit-service-environment svc) '(("K" . "V"))))
    (should (equal (elinit-service-environment-file svc) '("/etc/env")))
    (should (equal (elinit-service-exec-stop svc) '("stop1" "stop2")))
    (should (equal (elinit-service-exec-reload svc) '("reload1")))
    (should (equal (elinit-service-restart-sec svc) 3))))

(ert-deftest elinit-test-build-plan-preserves-new-fields ()
  "Build-plan :requires normalization preserves fields 13-26."
  (let* ((elinit--authority-snapshot nil)
         (programs '(("svc-a" :id "svc-a"
                      :working-directory "/opt"
                      :environment (("K" . "V"))
                      :exec-stop "stop-cmd"
                      :description "Service A")
                     ("svc-b" :id "svc-b"
                      :requires "svc-a"
                      :restart-sec 5
                      :exec-reload "reload-cmd"
                      :kill-signal SIGTERM)))
         (plan (elinit--build-plan programs))
         (entries (elinit-plan-entries plan)))
    ;; Both entries must be full parsed tuples.
    (dolist (entry entries)
      (should (= (length entry) 45)))
    ;; svc-a new fields preserved
    (let ((a (cl-find "svc-a" entries :key #'car :test #'equal)))
      (should (equal (elinit-entry-working-directory a) "/opt"))
      (should (equal (elinit-entry-environment a) '(("K" . "V"))))
      (should (equal (elinit-entry-exec-stop a) '("stop-cmd")))
      (should (equal (elinit-entry-description a) "Service A")))
    ;; svc-b new fields preserved (has :requires which triggers rewrite)
    (let ((b (cl-find "svc-b" entries :key #'car :test #'equal)))
      (should (equal (elinit-entry-restart-sec b) 5))
      (should (equal (elinit-entry-exec-reload b) '("reload-cmd")))
      (should (eq (elinit-entry-kill-signal b) 'SIGTERM)))))

(ert-deftest elinit-test-stable-topo-cycle-preserves-new-fields ()
  "Cycle fallback in stable-topo-sort preserves fields 13-26."
  (let ((elinit--computed-deps (make-hash-table :test 'equal))
        (elinit--cycle-fallback-ids (make-hash-table :test 'equal)))
    ;; 26-element entries with cycle: a -> b -> a
    (let* ((entries (list (list "a" "cmd" 0 t 'always t 'simple '("b")
                                t 30 nil nil "/opt" '(("K" . "V")) nil
                                '("stop") nil 3
                                "desc-a" nil nil nil nil nil nil nil)
                          (list "b" "cmd" 0 t 'always t 'simple '("a")
                                t 30 nil nil nil nil '("/env") nil
                                '("reload") nil
                                nil nil nil nil 'SIGTERM nil nil nil)))
           (sorted (elinit--stable-topo-sort entries)))
      ;; All entries must remain 26 fields
      (dolist (entry sorted)
        (should (= (length entry) 26)))
      ;; :after (7) and :requires (11) cleared
      (dolist (entry sorted)
        (should (null (nth 7 entry)))
        (should (null (nth 11 entry))))
      ;; P2 fields preserved
      (let ((a (cl-find "a" sorted :key #'car :test #'equal)))
        (should (equal (elinit-entry-working-directory a) "/opt"))
        (should (equal (elinit-entry-environment a) '(("K" . "V"))))
        (should (equal (elinit-entry-exec-stop a) '("stop")))
        (should (equal (elinit-entry-restart-sec a) 3))
        (should (equal (elinit-entry-description a) "desc-a")))
      (let ((b (cl-find "b" sorted :key #'car :test #'equal)))
        (should (equal (elinit-entry-environment-file b) '("/env")))
        (should (equal (elinit-entry-exec-reload b) '("reload")))
        (should (eq (elinit-entry-kill-signal b) 'SIGTERM))))))

;;; Environment File and Working Directory Tests

(ert-deftest elinit-test-parse-env-file ()
  "Parse a well-formed environment file."
  (let ((tmpfile (make-temp-file "env-test")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "# comment\n")
            (insert "; another comment\n")
            (insert "\n")
            (insert "FOO=bar\n")
            (insert "BAZ=qux value\n")
            (insert "export EXPORTED=yes\n")
            (insert "bad line no equals\n")
            (insert "EMPTY=\n"))
          (let ((result (elinit--parse-env-file tmpfile)))
            (should (equal result '(("FOO" . "bar")
                                    ("BAZ" . "qux value")
                                    ("EXPORTED" . "yes")
                                    ("EMPTY" . ""))))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-parse-env-file-key-validation ()
  "Env file rejects keys that don't match [A-Za-z_][A-Za-z0-9_]*."
  (let ((tmpfile (make-temp-file "env-test")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "VALID_KEY=ok\n")
            (insert "123BAD=no\n")
            (insert "_ok=yes\n"))
          (let ((result (elinit--parse-env-file tmpfile)))
            (should (= (length result) 2))
            (should (equal (car result) '("VALID_KEY" . "ok")))
            (should (equal (cadr result) '("_ok" . "yes")))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-resolve-env-file-path ()
  "Resolve env-file path with and without optional - prefix."
  (let ((result (elinit--resolve-env-file-path "/etc/env" "/units/")))
    (should-not (car result))
    (should (equal (cdr result) "/etc/env")))
  (let ((result (elinit--resolve-env-file-path "-/etc/env.local" "/units/")))
    (should (car result))
    (should (equal (cdr result) "/etc/env.local")))
  ;; Relative path resolved against unit dir
  (let ((result (elinit--resolve-env-file-path "local.env" "/opt/units/")))
    (should-not (car result))
    (should (equal (cdr result) "/opt/units/local.env")))
  ;; Optional relative
  (let ((result (elinit--resolve-env-file-path "-local.env" "/opt/units/")))
    (should (car result))
    (should (equal (cdr result) "/opt/units/local.env"))))

(ert-deftest elinit-test-build-process-environment ()
  "Build effective process-environment from env-files and alist."
  (let* ((tmpfile (make-temp-file "env-test"))
         (process-environment '("INHERITED=yes" "OVERRIDE=old")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "FILE_VAR=from-file\n")
            (insert "OVERRIDE=from-file\n"))
          (let ((result (elinit--build-process-environment
                         (list tmpfile)
                         '(("ALIST_VAR" . "from-alist")
                           ("OVERRIDE" . "from-alist"))
                         "/tmp/")))
            ;; Alist wins over file (applied later)
            (should (member "OVERRIDE=from-alist" result))
            (should (member "FILE_VAR=from-file" result))
            (should (member "ALIST_VAR=from-alist" result))
            (should (member "INHERITED=yes" result))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-build-process-environment-optional-missing ()
  "Optional missing env-file (leading -) is silently skipped."
  (let ((process-environment '("KEEP=yes")))
    (let ((result (elinit--build-process-environment
                   '("-/nonexistent/file.env")
                   nil "/tmp/")))
      ;; Should not error, inherited env preserved
      (should (member "KEEP=yes" result)))))

(ert-deftest elinit-test-build-process-environment-required-missing ()
  "Required missing env-file (no leading -) signals an error."
  (let ((process-environment '("KEEP=yes")))
    (should-error (elinit--build-process-environment
                   '("/nonexistent/required.env")
                   nil "/tmp/"))))

(ert-deftest elinit-test-resolve-working-directory-relative ()
  "Resolve relative working directory against unit-file directory."
  (let ((tmp-dir (make-temp-file "wdir-" t)))
    (unwind-protect
        (let ((sub (expand-file-name "subdir" tmp-dir)))
          (make-directory sub)
          (should (equal (elinit--resolve-working-directory
                          "subdir" (file-name-as-directory tmp-dir))
                         sub)))
      (delete-directory tmp-dir t))))

(ert-deftest elinit-test-resolve-working-directory-nonexistent ()
  "Non-existent working directory signals error."
  (should-error (elinit--resolve-working-directory
                 "/nonexistent/dir/xyz" "/tmp/")))

(ert-deftest elinit-test-restart-sec-overrides-global ()
  "Per-unit :restart-sec overrides `elinit-restart-delay' in schedule-restart."
  (let* ((elinit-restart-delay 10)
         (elinit--restart-timers (make-hash-table :test 'equal))
         (scheduled-delay nil))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (delay _repeat &rest _args)
                 (setq scheduled-delay delay)
                 'mock-timer))
              ((symbol-function 'elinit--log) #'ignore)
              ((symbol-function 'elinit--format-exit-status)
               (lambda (&rest _) "exited")))
      ;; With restart-sec = 3, should use 3 not 10
      (elinit--schedule-restart "svc" "cmd" t 'simple 'always
                                    'exit 1 nil nil nil 3)
      (should (= scheduled-delay 3)))))

(ert-deftest elinit-test-restart-sec-zero-immediate ()
  "Restart-sec 0 means immediate retry."
  (let* ((elinit-restart-delay 10)
         (elinit--restart-timers (make-hash-table :test 'equal))
         (scheduled-delay nil))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (delay _repeat &rest _args)
                 (setq scheduled-delay delay)
                 'mock-timer))
              ((symbol-function 'elinit--log) #'ignore)
              ((symbol-function 'elinit--format-exit-status)
               (lambda (&rest _) "exited")))
      (elinit--schedule-restart "svc" "cmd" t 'simple 'always
                                    'exit 1 nil nil nil 0)
      (should (= scheduled-delay 0)))))

;;; Stop and Reload Command Execution Tests

(ert-deftest elinit-test-run-command-with-timeout-timeout ()
  "Command that exceeds timeout returns 124."
  (should (= 124 (elinit--run-command-with-timeout
                  "sleep 60" 0.2 nil nil nil))))

(ert-deftest elinit-test-exec-command-chain-partial-failure ()
  "One failure returns nil but all commands still run."
  (let ((log-file (make-temp-file "sv-test-log-")))
    (unwind-protect
        (progn
          (should (eq nil (elinit--exec-command-chain
                           '("echo before" "false" "echo after") "test-id"
                           nil nil log-file 5)))
          ;; Verify "after" command ran despite "false" failure
          (let ((content (with-temp-buffer
                           (insert-file-contents log-file)
                           (buffer-string))))
            (should (string-match-p "before" content))
            (should (string-match-p "after" content))))
      (delete-file log-file))))

(ert-deftest elinit-test-manual-stop-with-exec-stop ()
  "Exec-stop commands run before kill-signal with escalation timer."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--manually-started (make-hash-table :test 'equal))
        (chain-called nil)
        (signal-sent nil)
        (timer-set nil)
        (proc (start-process "test-stop" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "test-stop" proc elinit--processes)
          (cl-letf (((symbol-function 'elinit--get-entry-for-id)
                     (lambda (_id)
                       (list "test-stop" "sleep 300" 0 t 'no t 'simple
                             nil nil 30 nil nil
                             nil nil nil
                             '("my-stop-cmd") nil nil)))
                    ((symbol-function 'elinit--exec-command-chain)
                     (lambda (cmds _id _dir _env _log _timeout)
                       (setq chain-called cmds)
                       ;; Process should still be alive at this point
                       (should (process-live-p proc))
                       t))
                    ((symbol-function 'elinit--unit-file-directory-for-id)
                     (lambda (_id) nil))
                    ((symbol-function 'signal-process)
                     (lambda (_p sig) (setq signal-sent sig)))
                    ((symbol-function 'run-at-time)
                     (lambda (&rest _args) (setq timer-set t))))
            (let ((result (elinit--manual-stop "test-stop")))
              (should (eq (plist-get result :status) 'stopped))
              (should (equal chain-called '("my-stop-cmd")))
              ;; Kill-signal sent after exec-stop
              (should (eq 'SIGTERM signal-sent))
              ;; Escalation timer set up
              (should timer-set))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest elinit-test-manual-stop-exec-stop-failure-still-terminates ()
  "Even if exec-stop fails, kill-signal is still sent."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--manually-started (make-hash-table :test 'equal))
        (signal-sent nil)
        (proc (start-process "test-stop3" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "test-stop3" proc elinit--processes)
          (cl-letf (((symbol-function 'elinit--get-entry-for-id)
                     (lambda (_id)
                       (list "test-stop3" "sleep 300" 0 t 'no t 'simple
                             nil nil 30 nil nil
                             nil nil nil
                             '("false") nil nil)))
                    ((symbol-function 'elinit--exec-command-chain)
                     (lambda (_cmds _id _dir _env _log _timeout)
                       nil))  ; Simulate failure
                    ((symbol-function 'elinit--unit-file-directory-for-id)
                     (lambda (_id) nil))
                    ((symbol-function 'signal-process)
                     (lambda (_p sig) (setq signal-sent sig)))
                    ((symbol-function 'run-at-time)
                     (lambda (&rest _args))))
            (let ((result (elinit--manual-stop "test-stop3")))
              (should (eq (plist-get result :status) 'stopped))
              ;; Kill-signal sent regardless of exec-stop failure
              (should (eq 'SIGTERM signal-sent)))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest elinit-test-reload-unit-with-exec-reload ()
  "Reload with exec-reload runs commands without stop/start."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--manually-started (make-hash-table :test 'equal))
        (elinit--mask-override (make-hash-table :test 'equal))
        (elinit--invalid (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--restart-times (make-hash-table :test 'equal))
        (chain-called nil)
        (stop-called nil)
        (proc (start-process "test-reload" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "test-reload" proc elinit--processes)
          (cl-letf (((symbol-function 'elinit--reload-find-entry)
                     (lambda (_id)
                       (list "test-reload" "sleep 300" 0 t 'no t 'simple
                             nil nil 30 nil nil
                             nil nil nil
                             nil '("my-reload-cmd") nil)))
                    ((symbol-function 'elinit--exec-command-chain)
                     (lambda (cmds _id _dir _env _log _timeout)
                       (setq chain-called cmds)
                       t))
                    ((symbol-function 'elinit--manual-stop)
                     (lambda (_id)
                       (setq stop-called t)
                       (list :status 'stopped :reason nil)))
                    ((symbol-function 'elinit--unit-file-directory-for-id)
                     (lambda (_id) nil)))
            (let ((result (elinit--reload-unit "test-reload")))
              (should (equal (plist-get result :action) "reloaded"))
              (should (equal chain-called '("my-reload-cmd")))
              ;; Stop should NOT have been called
              (should (null stop-called))
              ;; Process should still be running
              (should (process-live-p proc)))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest elinit-test-reload-unit-without-exec-reload ()
  "Reload without exec-reload does stop + start."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--manually-started (make-hash-table :test 'equal))
        (elinit--mask-override (make-hash-table :test 'equal))
        (elinit--invalid (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--restart-times (make-hash-table :test 'equal))
        (stop-called nil)
        (start-called nil)
        (proc (start-process "test-reload2" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "test-reload2" proc elinit--processes)
          (cl-letf (((symbol-function 'elinit--reload-find-entry)
                     (lambda (_id)
                       ;; No exec-reload (nil at index 17)
                       (list "test-reload2" "sleep 300" 0 t 'no t 'simple
                             nil nil 30 nil nil
                             nil nil nil
                             nil nil nil)))
                    ((symbol-function 'elinit--manual-stop)
                     (lambda (_id)
                       (setq stop-called t)
                       (list :status 'stopped :reason nil)))
                    ((symbol-function 'elinit--start-process)
                     (lambda (&rest _args)
                       (setq start-called t)
                       proc))
                    ((symbol-function 'elinit--unit-file-directory-for-id)
                     (lambda (_id) nil)))
            (let ((result (elinit--reload-unit "test-reload2")))
              (should (equal (plist-get result :action) "reloaded"))
              (should stop-called)
              (should start-called))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest elinit-test-reload-unit-exec-reload-failure ()
  "Reload command failure reports error, keeps process running."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--manually-started (make-hash-table :test 'equal))
        (elinit--mask-override (make-hash-table :test 'equal))
        (elinit--invalid (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--restart-times (make-hash-table :test 'equal))
        (proc (start-process "test-reload3" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "test-reload3" proc elinit--processes)
          (cl-letf (((symbol-function 'elinit--reload-find-entry)
                     (lambda (_id)
                       (list "test-reload3" "sleep 300" 0 t 'no t 'simple
                             nil nil 30 nil nil
                             nil nil nil
                             nil '("false") nil)))
                    ((symbol-function 'elinit--exec-command-chain)
                     (lambda (_cmds _id _dir _env _log _timeout)
                       nil))  ; Simulate failure
                    ((symbol-function 'elinit--unit-file-directory-for-id)
                     (lambda (_id) nil)))
            (let ((result (elinit--reload-unit "test-reload3")))
              (should (string-match-p "reload command failed"
                                      (plist-get result :action)))
              ;; Process should still be running
              (should (process-live-p proc)))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest elinit-test-reload-stopped-unit-with-exec-reload ()
  "Stopped simple unit with exec-reload does not run reload chain."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--manually-started (make-hash-table :test 'equal))
        (elinit--mask-override (make-hash-table :test 'equal))
        (elinit--invalid (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--restart-times (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (chain-called nil))
    ;; No process in elinit--processes — unit is stopped
    (cl-letf (((symbol-function 'elinit--reload-find-entry)
               (lambda (_id)
                 (list "stopped-svc" "echo hi" 0 t 'no t 'simple
                       nil nil 30 nil nil
                       nil nil nil
                       nil '("reload-cmd") nil)))
              ((symbol-function 'elinit--exec-command-chain)
               (lambda (cmds _id _dir _env _log _timeout)
                 (setq chain-called cmds)
                 t))
              ((symbol-function 'elinit--unit-file-directory-for-id)
               (lambda (_id) nil)))
      (let ((result (elinit--reload-unit "stopped-svc")))
        ;; Should update definition, not run reload chain
        (should (equal (plist-get result :action) "updated"))
        (should (null chain-called))))))

(ert-deftest elinit-test-stop-all-runs-exec-stop ()
  "The stop-all flow runs exec-stop for applicable units."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--manually-started (make-hash-table :test 'equal))
        (elinit--timers nil)
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--shutting-down nil)
        (elinit--shutdown-complete-flag nil)
        (elinit--shutdown-remaining 0)
        (elinit--shutdown-timer nil)
        (elinit--shutdown-callback nil)
        (exec-stop-ids nil)
        (proc (start-process "test-stop-all" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "test-stop-all" proc elinit--processes)
          (cl-letf (((symbol-function 'elinit--run-exec-stop-for-id)
                     (lambda (id)
                       (push id exec-stop-ids)))
                    ((symbol-function 'elinit--dag-cleanup)
                     #'ignore)
                    ((symbol-function 'elinit--emit-event)
                     #'ignore))
            (elinit-stop)
            ;; exec-stop should have been called for the unit
            (should (member "test-stop-all" exec-stop-ids))))
      (when (process-live-p proc)
        (delete-process proc)))))

;;; Signal and Kill Mode Normalization Tests

(ert-deftest elinit-test-normalize-success-exit-status-signal ()
  "Normalize single signal."
  (let ((result (elinit--normalize-success-exit-status 'TERM)))
    (should (equal (plist-get result :codes) nil))
    (should (equal (plist-get result :signals) '(SIGTERM)))))

(ert-deftest elinit-test-normalize-success-exit-status-list ()
  "Normalize mixed list of ints and signals."
  (let ((result (elinit--normalize-success-exit-status '(0 42 SIGHUP "term"))))
    (should (equal (plist-get result :codes) '(0 42)))
    (should (equal (plist-get result :signals) '(SIGHUP SIGTERM)))))

(ert-deftest elinit-test-validate-remain-after-exit-simple-invalid ()
  "Remain-after-exit on simple type fails."
  (should (string-match-p ":remain-after-exit is invalid for :type simple"
                          (elinit--validate-entry
                           '("cmd" :type simple :remain-after-exit t)))))

(ert-deftest elinit-test-entry-service-roundtrip-pt3 ()
  "Round-trip entry->service->entry preserves PT3 fields."
  (let* ((entry (elinit--parse-entry
                 '("cmd" :description "test svc"
                   :documentation ("man:foo(1)")
                   :before ("b")
                   :wants ("w")
                   :kill-signal SIGTERM
                   :kill-mode mixed
                   :success-exit-status (42 SIGHUP))))
         (service (elinit-entry-to-service entry))
         (roundtripped (elinit-service-to-entry service)))
    (should (equal (elinit-entry-description roundtripped) "test svc"))
    (should (equal (elinit-entry-documentation roundtripped) '("man:foo(1)")))
    (should (equal (elinit-entry-before roundtripped) '("b")))
    (should (equal (elinit-entry-wants roundtripped) '("w")))
    (should (eq (elinit-entry-kill-signal roundtripped) 'SIGTERM))
    (should (eq (elinit-entry-kill-mode roundtripped) 'mixed))
    (should (equal (plist-get (elinit-entry-success-exit-status roundtripped) :codes)
                   '(42)))
    (should (equal (plist-get (elinit-entry-success-exit-status roundtripped) :signals)
                   '(SIGHUP)))))

;;; Before and Wants Ordering Tests

(ert-deftest elinit-test-before-inversion-basic ()
  "A :before B produces same ordering as B :after A."
  (let* ((elinit--authority-snapshot nil)
         ;; A should start before B
         (programs-before '(("cmd-a" :id "a" :before ("b"))
                            ("cmd-b" :id "b")))
         (programs-after '(("cmd-a" :id "a")
                           ("cmd-b" :id "b" :after ("a"))))
         (plan-before (elinit--build-plan programs-before))
         (plan-after (elinit--build-plan programs-after))
         (ids-before (mapcar #'elinit-entry-id
                             (elinit-plan-entries plan-before)))
         (ids-after (mapcar #'elinit-entry-id
                            (elinit-plan-entries plan-after))))
    (should (equal ids-before ids-after))))

(ert-deftest elinit-test-before-missing-target-ignored ()
  "Missing :before target is warned and ignored."
  (let* ((elinit--authority-snapshot nil)
         (logged nil)
         (programs '(("cmd-a" :id "a" :before ("nonexistent")))))
    (cl-letf (((symbol-function 'elinit--log)
               (lambda (_level fmt &rest args)
                 (push (apply #'format fmt args) logged))))
      (elinit--build-plan programs)
      ;; Should have logged a warning about nonexistent
      (should (cl-some (lambda (msg)
                         (string-match-p ":before.*does not exist" msg))
                       logged)))))

(ert-deftest elinit-test-before-after-cycle-fallback ()
  "Cycle involving :before falls back correctly."
  (let* ((elinit--authority-snapshot nil)
         ;; a :before b, b :before a → cycle
         (programs '(("cmd-a" :id "a" :before ("b"))
                     ("cmd-b" :id "b" :before ("a"))))
         (plan (elinit--build-plan programs)))
    ;; Both should be in cycle-fallback
    (should (gethash "a" (elinit-plan-cycle-fallback-ids plan)))
    (should (gethash "b" (elinit-plan-cycle-fallback-ids plan)))))

(ert-deftest elinit-test-wants-ordering ()
  "Wants creates ordering preference when target exists."
  (let* ((elinit--authority-snapshot nil)
         (programs '(("cmd-a" :id "a")
                     ("cmd-b" :id "b" :wants ("a"))))
         (plan (elinit--build-plan programs))
         (sorted (elinit-plan-by-target plan))
         (ids (mapcar #'elinit-entry-id sorted)))
    ;; a should come before b
    (should (< (cl-position "a" ids :test #'equal)
               (cl-position "b" ids :test #'equal)))))

(ert-deftest elinit-test-wants-missing-silently-dropped ()
  "Missing :wants target is silently dropped (no warning)."
  (let* ((elinit--authority-snapshot nil)
         (logged nil)
         (programs '(("cmd-a" :id "a" :wants ("nonexistent")))))
    (cl-letf (((symbol-function 'elinit--log)
               (lambda (_level fmt &rest args)
                 (push (apply #'format fmt args) logged))))
      (let ((plan (elinit--build-plan programs)))
        ;; Entry should still be valid
        (should (= 1 (length (elinit-plan-entries plan))))
        ;; No warning about the missing wanted unit
        (should-not (cl-some (lambda (msg)
                               (string-match-p "nonexistent" msg))
                             logged))))))

(ert-deftest elinit-test-wants-disabled-not-blocking ()
  "Disabled :wants target does not block the wanting unit."
  (let* ((elinit--authority-snapshot nil)
         (programs '(("cmd-a" :id "a" :disabled t)
                     ("cmd-b" :id "b" :wants ("a"))))
         (plan (elinit--build-plan programs))
         (sorted (elinit-plan-by-target plan)))
    ;; Both entries should be in the plan
    (should (= 2 (length sorted)))))

(ert-deftest elinit-test-dag-init-masked-wants-ignored ()
  "DAG init ignores wants for masked entries."
  (let* ((entries (list (elinit--parse-entry '("cmd-a" :id "a"))
                        (elinit--parse-entry '("cmd-b" :id "b" :wants ("a")))))
         (elinit--entry-state (make-hash-table :test 'equal))
         (elinit--mask-override (make-hash-table :test 'equal)))
    ;; Mask entry a
    (puthash "a" 'masked elinit--mask-override)
    (elinit--dag-init entries)
    ;; b should have in-degree 0 (masked wants ignored)
    (should (= 0 (gethash "b" elinit--dag-in-degree)))))

(ert-deftest elinit-test-wants-cycle-fallback-clears-wants ()
  "Cycle fallback clears :wants edges so runtime DAG has zero in-degree."
  (let* ((elinit--authority-snapshot nil)
         (programs '(("cmd-a" :id "a" :wants ("b"))
                     ("cmd-b" :id "b" :wants ("a"))))
         (plan (elinit--build-plan programs))
         (sorted (elinit-plan-by-target plan)))
    ;; Cycle fallback should have cleared :wants
    (dolist (entry sorted)
      (should (null (elinit-entry-wants entry))))
    ;; DAG init should produce zero in-degree for both
    (let ((elinit--entry-state (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal)))
      (elinit--dag-init sorted)
      (should (= 0 (gethash "a" elinit--dag-in-degree)))
      (should (= 0 (gethash "b" elinit--dag-in-degree))))))

(ert-deftest elinit-test-wants-failed-spawn-not-blocking ()
  "A wanted unit that fails to start does not block the wanting unit.
Spawn failure calls `elinit--dag-mark-ready', which decrements
in-degree of dependents including :wants edges."
  (let* ((entries (list (elinit--parse-entry '("cmd-a" :id "a"))
                        (elinit--parse-entry '("cmd-b" :id "b" :wants ("a")))))
         (elinit--entry-state (make-hash-table :test 'equal))
         (elinit--mask-override (make-hash-table :test 'equal))
         (elinit--dag-active-starts 0)
         (elinit--dag-pending-starts nil)
         (elinit--dag-complete-callback nil))
    (elinit--dag-init entries)
    ;; b depends on a (in-degree 1)
    (should (= 1 (gethash "b" elinit--dag-in-degree)))
    ;; Simulate a failing to start
    (elinit--dag-handle-spawn-failure "a")
    ;; b should now be unblocked (in-degree 0)
    (should (= 0 (gethash "b" elinit--dag-in-degree)))))

;;; Kill Controls Tests

(ert-deftest elinit-test-process-descendants-returns-children ()
  "Descendant discovery finds child PIDs via process-attributes."
  (cl-letf (((symbol-function 'list-system-processes)
             (lambda () '(1 10 20 30 40)))
            ((symbol-function 'process-attributes)
             (lambda (pid)
               (pcase pid
                 (10 '((ppid . 1)))
                 (20 '((ppid . 10)))
                 (30 '((ppid . 10)))
                 (40 '((ppid . 99)))
                 (_ nil)))))
    (let ((desc (elinit--process-descendants 10)))
      (should (member 20 desc))
      (should (member 30 desc))
      (should-not (member 40 desc))
      (should-not (member 1 desc)))))

(ert-deftest elinit-test-process-descendants-fallback-on-error ()
  "Descendant discovery returns nil and logs warning on error."
  (let ((warnings nil))
    (cl-letf (((symbol-function 'list-system-processes)
               (lambda () (error "Not supported")))
              ((symbol-function 'elinit--log)
               (lambda (level fmt &rest args)
                 (when (eq level 'warning)
                   (push (apply #'format fmt args) warnings)))))
      (should-not (elinit--process-descendants 1234))
      (should (= 1 (length warnings)))
      (should (string-match-p "descendant discovery failed" (car warnings))))))

(ert-deftest elinit-test-kill-with-descendants-mixed ()
  "Kill with descendants sends SIGKILL to main and descendant PIDs."
  (let* ((killed nil)
         (proc (start-process "test-kill-mixed" nil "sleep" "300")))
    (unwind-protect
        (cl-letf (((symbol-function 'elinit--process-descendants)
                   (lambda (_pid) '(9991 9992)))
                  ((symbol-function 'signal-process)
                   (lambda (p sig)
                     (push (list p sig) killed)))
                  ((symbol-function 'elinit--log)
                   (lambda (&rest _args))))
          (elinit--kill-with-descendants proc)
          ;; Main process + 2 descendants = 3 SIGKILL signals
          (should (= 3 (length killed)))
          (should (cl-every (lambda (entry) (eq 'SIGKILL (cadr entry)))
                            killed)))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest elinit-test-manual-stop-uses-kill-signal ()
  "Manual stop sends configured :kill-signal instead of default."
  (let* ((elinit--processes (make-hash-table :test 'equal))
         (elinit--manually-stopped (make-hash-table :test 'equal))
         (elinit--manually-started (make-hash-table :test 'equal))
         (signaled-with nil)
         (proc (start-process "test-stop-sig" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "svc1" proc elinit--processes)
          (cl-letf (((symbol-function 'elinit--run-exec-stop-for-id)
                     (lambda (_id)))
                    ((symbol-function 'elinit--kill-signal-for-id)
                     (lambda (_id) 'SIGUSR1))
                    ((symbol-function 'elinit--kill-mode-for-id)
                     (lambda (_id) 'process))
                    ((symbol-function 'signal-process)
                     (lambda (_p sig)
                       (push sig signaled-with)))
                    ((symbol-function 'run-at-time)
                     (lambda (&rest _args))))
            (let ((result (elinit--manual-stop "svc1")))
              (should (eq 'stopped (plist-get result :status)))
              ;; Should have sent SIGUSR1, not SIGTERM
              (should (memq 'SIGUSR1 signaled-with))
              (should-not (memq 'SIGTERM signaled-with)))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest elinit-test-manual-stop-escalation-timer ()
  "Manual stop sets up SIGKILL escalation timer."
  (let* ((elinit--processes (make-hash-table :test 'equal))
         (elinit--manually-stopped (make-hash-table :test 'equal))
         (elinit--manually-started (make-hash-table :test 'equal))
         (timer-set nil)
         (proc (start-process "test-esc" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "svc1" proc elinit--processes)
          (cl-letf (((symbol-function 'elinit--run-exec-stop-for-id)
                     (lambda (_id)))
                    ((symbol-function 'elinit--kill-signal-for-id)
                     (lambda (_id) 'SIGTERM))
                    ((symbol-function 'elinit--kill-mode-for-id)
                     (lambda (_id) 'process))
                    ((symbol-function 'signal-process)
                     (lambda (_p _sig)))
                    ((symbol-function 'run-at-time)
                     (lambda (timeout _repeat fn)
                       (setq timer-set
                             (list :timeout timeout :fn fn)))))
            (elinit--manual-stop "svc1")
            ;; Escalation timer should be set
            (should timer-set)
            (should (= elinit-shutdown-timeout
                       (plist-get timer-set :timeout)))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest elinit-test-manual-stop-mixed-escalation ()
  "Manual stop with :kill-mode mixed uses kill-with-descendants on escalation."
  (let* ((elinit--processes (make-hash-table :test 'equal))
         (elinit--manually-stopped (make-hash-table :test 'equal))
         (elinit--manually-started (make-hash-table :test 'equal))
         (escalation-fn nil)
         (kill-descendants-called nil)
         (proc (start-process "test-mixed" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "svc1" proc elinit--processes)
          (cl-letf (((symbol-function 'elinit--run-exec-stop-for-id)
                     (lambda (_id)))
                    ((symbol-function 'elinit--kill-signal-for-id)
                     (lambda (_id) 'SIGTERM))
                    ((symbol-function 'elinit--kill-mode-for-id)
                     (lambda (_id) 'mixed))
                    ((symbol-function 'signal-process)
                     (lambda (_p _sig)))
                    ((symbol-function 'run-at-time)
                     (lambda (_timeout _repeat fn)
                       (setq escalation-fn fn))))
            (elinit--manual-stop "svc1")
            ;; Fire the escalation timer while process is still live
            (cl-letf (((symbol-function 'elinit--kill-with-descendants)
                       (lambda (_proc)
                         (setq kill-descendants-called t))))
              (funcall escalation-fn)
              (should kill-descendants-called))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest elinit-test-manual-kill-explicit-signal-overrides ()
  "Manual kill with explicit signal overrides unit's :kill-signal."
  (let* ((elinit--processes (make-hash-table :test 'equal))
         (signaled-with nil)
         (proc (start-process "test-kill-exp" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "svc1" proc elinit--processes)
          (cl-letf (((symbol-function 'elinit--kill-signal-for-id)
                     (lambda (_id) 'SIGINT))
                    ((symbol-function 'signal-process)
                     (lambda (_p sig)
                       (setq signaled-with sig))))
            (elinit--manual-kill "svc1" 'SIGUSR2)
            (should (eq 'SIGUSR2 signaled-with))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest elinit-test-shutdown-uses-per-unit-kill-signal ()
  "Shutdown sends per-unit :kill-signal instead of blanket SIGTERM."
  (let* ((elinit--processes (make-hash-table :test 'equal))
         (elinit--timers nil)
         (elinit--restart-timers (make-hash-table :test 'equal))
         (elinit--shutting-down nil)
         (elinit--shutdown-complete-flag nil)
         (elinit--shutdown-remaining 0)
         (elinit--shutdown-callback nil)
         (elinit--shutdown-timer nil)
         (signals-sent nil)
         (proc1 (start-process "svc1" nil "sleep" "300"))
         (proc2 (start-process "svc2" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "svc1" proc1 elinit--processes)
          (puthash "svc2" proc2 elinit--processes)
          (cl-letf (((symbol-function 'elinit--run-exec-stop-for-id)
                     (lambda (_id)))
                    ((symbol-function 'elinit--dag-cleanup)
                     (lambda ()))
                    ((symbol-function 'elinit--emit-event)
                     (lambda (&rest _args)))
                    ((symbol-function 'elinit--kill-signal-for-id)
                     (lambda (id)
                       (if (string= id "svc1") 'SIGUSR1 'SIGINT)))
                    ((symbol-function 'signal-process)
                     (lambda (p sig)
                       (push (list (process-name p) sig) signals-sent)))
                    ((symbol-function 'run-at-time)
                     (lambda (&rest _args))))
            (elinit-stop)
            ;; Each unit should get its own kill-signal
            (should (cl-find-if (lambda (e)
                                  (and (string= "svc1" (car e))
                                       (eq 'SIGUSR1 (cadr e))))
                                signals-sent))
            (should (cl-find-if (lambda (e)
                                  (and (string= "svc2" (car e))
                                       (eq 'SIGINT (cadr e))))
                                signals-sent))))
      (when (process-live-p proc1) (delete-process proc1))
      (when (process-live-p proc2) (delete-process proc2)))))

(ert-deftest elinit-test-shutdown-escalation-respects-kill-mode ()
  "Shutdown SIGKILL escalation uses kill-with-descendants for mixed mode."
  (let* ((elinit--processes (make-hash-table :test 'equal))
         (elinit--timers nil)
         (elinit--restart-timers (make-hash-table :test 'equal))
         (elinit--shutting-down nil)
         (elinit--shutdown-complete-flag nil)
         (elinit--shutdown-remaining 0)
         (elinit--shutdown-callback nil)
         (elinit--shutdown-timer nil)
         (escalation-fn nil)
         (kill-desc-called nil)
         (plain-kill-called nil)
         (proc1 (start-process "mixed-svc" nil "sleep" "300"))
         (proc2 (start-process "normal-svc" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "mixed-svc" proc1 elinit--processes)
          (puthash "normal-svc" proc2 elinit--processes)
          (cl-letf (((symbol-function 'elinit--run-exec-stop-for-id)
                     (lambda (_id)))
                    ((symbol-function 'elinit--dag-cleanup)
                     (lambda ()))
                    ((symbol-function 'elinit--emit-event)
                     (lambda (&rest _args)))
                    ((symbol-function 'elinit--kill-signal-for-id)
                     (lambda (_id) 'SIGTERM))
                    ((symbol-function 'elinit--kill-mode-for-id)
                     (lambda (id)
                       (if (string= id "mixed-svc") 'mixed 'process)))
                    ((symbol-function 'signal-process)
                     (lambda (_p _sig)))
                    ((symbol-function 'run-at-time)
                     (lambda (_timeout _repeat fn)
                       (setq escalation-fn fn))))
            (elinit-stop)
            ;; Fire the escalation timer
            (cl-letf (((symbol-function 'elinit--kill-with-descendants)
                       (lambda (_proc)
                         (setq kill-desc-called t)))
                      ((symbol-function 'signal-process)
                       (lambda (_p sig)
                         (when (eq sig 'SIGKILL)
                           (setq plain-kill-called t)))))
              (funcall escalation-fn)
              ;; mixed-svc should use kill-with-descendants
              (should kill-desc-called)
              ;; normal-svc should use plain SIGKILL
              (should plain-kill-called))))
      (when (process-live-p proc1) (delete-process proc1))
      (when (process-live-p proc2) (delete-process proc2)))))

;;; Remain-After-Exit (Oneshot Active-Latch) Tests

(ert-deftest elinit-test-remain-after-exit-success-active ()
  "Oneshot with :remain-after-exit t shows `active' status on success."
  (let ((elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--processes (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--mask-override (make-hash-table :test 'equal)))
    ;; Simulate successful oneshot exit with remain-after-exit latch
    (puthash "svc1" 0 elinit--oneshot-completed)
    (puthash "svc1" t elinit--remain-active)
    (let ((result (elinit--compute-entry-status "svc1" 'oneshot)))
      (should (string= "active" (car result)))
      (should (string= "exit:0" (cdr result))))))

(ert-deftest elinit-test-remain-after-exit-failure-not-active ()
  "Oneshot with :remain-after-exit t shows `failed' on non-zero exit."
  (let ((elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--processes (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--mask-override (make-hash-table :test 'equal)))
    ;; Failure: remain-active should NOT be set
    (puthash "svc1" 1 elinit--oneshot-completed)
    (let ((result (elinit--compute-entry-status "svc1" 'oneshot)))
      (should (string= "failed" (car result))))))

(ert-deftest elinit-test-remain-after-exit-regular-oneshot-done ()
  "Regular oneshot without remain-after-exit still shows `done'."
  (let ((elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--processes (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--mask-override (make-hash-table :test 'equal)))
    (puthash "svc1" 0 elinit--oneshot-completed)
    ;; No remain-active entry
    (let ((result (elinit--compute-entry-status "svc1" 'oneshot)))
      (should (string= "done" (car result))))))

(ert-deftest elinit-test-remain-after-exit-handle-oneshot-exit ()
  "Handle-oneshot-exit sets remain-active on successful exit."
  (let ((elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--oneshot-callbacks (make-hash-table :test 'equal))
        (elinit--programs-cache
         (list (list "true" :id "svc1" :type 'oneshot
                     :remain-after-exit t)))
        (elinit--invalid (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'elinit--dag-mark-ready)
               (lambda (_id)))
              ((symbol-function 'elinit--emit-event)
               (lambda (&rest _args)))
              ((symbol-function 'elinit--log)
               (lambda (&rest _args))))
      (elinit--handle-oneshot-exit "svc1" 'exit 0)
      (should (gethash "svc1" elinit--remain-active))
      (should (= 0 (gethash "svc1" elinit--oneshot-completed))))))

(ert-deftest elinit-test-remain-after-exit-handle-signal ()
  "Handle-oneshot-exit does NOT set remain-active on signal death."
  (let ((elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--oneshot-callbacks (make-hash-table :test 'equal))
        (elinit--programs-cache
         (list (list "true" :id "svc1" :type 'oneshot
                     :remain-after-exit t)))
        (elinit--invalid (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'elinit--dag-mark-ready)
               (lambda (_id)))
              ((symbol-function 'elinit--emit-event)
               (lambda (&rest _args)))
              ((symbol-function 'elinit--log)
               (lambda (&rest _args))))
      (elinit--handle-oneshot-exit "svc1" 'signal 9)
      (should-not (gethash "svc1" elinit--remain-active)))))

(ert-deftest elinit-test-remain-after-exit-start-noop ()
  "Start on active remain-after-exit unit is a no-op."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--invalid (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--manually-started (make-hash-table :test 'equal))
        (elinit--restart-times (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal)))
    (puthash "svc1" t elinit--remain-active)
    (let ((result (elinit--manual-start "svc1")))
      (should (eq 'skipped (plist-get result :status)))
      (should (string= "already active" (plist-get result :reason)))
      ;; Latch should still be set
      (should (gethash "svc1" elinit--remain-active)))))

(ert-deftest elinit-test-remain-after-exit-stop-clears-latch ()
  "Stop on active remain-after-exit unit clears the latch."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--manually-started (make-hash-table :test 'equal)))
    (puthash "svc1" t elinit--remain-active)
    (let ((result (elinit--manual-stop "svc1")))
      (should (eq 'stopped (plist-get result :status)))
      ;; Latch should be cleared
      (should-not (gethash "svc1" elinit--remain-active))
      ;; Manually-stopped should be set
      (should (gethash "svc1" elinit--manually-stopped)))))

(ert-deftest elinit-test-remain-after-exit-restart-reruns ()
  "Restart on active remain-after-exit unit clears latch and re-runs."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--manually-started (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--invalid (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--restart-times (make-hash-table :test 'equal))
        (elinit--mask-override (make-hash-table :test 'equal))
        (elinit--programs-cache
         (list (list "true" :id "svc1" :type 'oneshot
                     :remain-after-exit t)))
        (started nil))
    (puthash "svc1" t elinit--remain-active)
    (puthash "svc1" 0 elinit--oneshot-completed)
    ;; Stop clears the latch
    (elinit--manual-stop "svc1")
    (should-not (gethash "svc1" elinit--remain-active))
    ;; Start re-runs (mock the actual process creation)
    (cl-letf (((symbol-function 'elinit--start-process)
               (lambda (&rest _args)
                 (setq started t)
                 (start-process "mock" nil "true")))
              ((symbol-function 'elinit--unit-file-directory-for-id)
               (lambda (_id) nil)))
      (let ((result (elinit--manual-start "svc1")))
        (should (eq 'started (plist-get result :status)))
        (should started)
        ;; Oneshot-completed should have been cleared
        (should-not (gethash "svc1" elinit--oneshot-completed))
        ;; Remain-active should have been cleared
        (should-not (gethash "svc1" elinit--remain-active))))))

;;; Conflicts Validation Tests

(ert-deftest elinit-test-parse-entry-conflicts-string ()
  "Parse entry with :conflicts as string."
  (let ((entry (elinit--parse-entry '("cmd" :conflicts "foo"))))
    (should (equal (elinit-entry-conflicts entry) '("foo")))))

(ert-deftest elinit-test-validate-conflicts-invalid ()
  "Validation rejects :conflicts with invalid type."
  (let ((result (elinit--validate-entry '("cmd" :conflicts 42))))
    (should (stringp result))
    (should (string-match-p ":conflicts" result))))

(ert-deftest elinit-test-validate-conflicts-empty-string ()
  "Validation rejects empty string in :conflicts."
  (let ((result (elinit--validate-entry '("cmd" :id "svc" :conflicts ("" "b")))))
    (should (stringp result))
    (should (string-match-p ":conflicts.*empty" result))))

(ert-deftest elinit-test-validate-conflicts-self-reference ()
  "Validation rejects self-reference in :conflicts."
  (let ((result (elinit--validate-entry '("cmd" :id "svc" :conflicts "svc"))))
    (should (stringp result))
    (should (string-match-p ":conflicts.*own ID" result))))

;;; Resource Limits Validation and Launch Tests

(ert-deftest elinit-test-rlimits-reject-on-target ()
  "Limit keys on target type are rejected."
  (should (string-match "invalid for :type target"
                        (elinit--validate-entry
                         '(nil :id "foo.target" :type target
                           :limit-nofile 1024)))))

(ert-deftest elinit-test-rlimits-validate-limit-value-helper ()
  "Validate-limit-value helper accepts and rejects correctly."
  ;; Valid
  (should (null (elinit--validate-limit-value :limit-nofile 0)))
  (should (null (elinit--validate-limit-value :limit-nofile 1024)))
  (should (null (elinit--validate-limit-value :limit-nofile 'infinity)))
  (should (null (elinit--validate-limit-value :limit-nofile "100:200")))
  (should (null (elinit--validate-limit-value :limit-nofile "infinity:infinity")))
  (should (null (elinit--validate-limit-value :limit-nofile "0:infinity")))
  ;; Valid: soft equals hard
  (should (null (elinit--validate-limit-value :limit-nofile "200:200")))
  ;; Valid: soft < hard
  (should (null (elinit--validate-limit-value :limit-nofile "1:999")))
  ;; Valid: hard is infinity, soft is finite
  (should (null (elinit--validate-limit-value :limit-nofile "1024:infinity")))
  ;; Invalid
  (should (elinit--validate-limit-value :limit-nofile -1))
  (should (elinit--validate-limit-value :limit-nofile "bad"))
  (should (elinit--validate-limit-value :limit-nofile "100:abc"))
  (should (elinit--validate-limit-value :limit-nofile ":100"))
  (should (elinit--validate-limit-value :limit-nofile "100:"))
  (should (elinit--validate-limit-value :limit-nofile 1.5))
  (should (elinit--validate-limit-value :limit-nofile '(1 2)))
  ;; Invalid: soft > hard (kernel rejects this)
  (should (elinit--validate-limit-value :limit-nofile "9999:1"))
  (should (elinit--validate-limit-value :limit-nofile "200:100"))
  ;; Invalid: infinity soft with finite hard
  (should (elinit--validate-limit-value :limit-nofile "infinity:100"))
  ;; Invalid: value too large for C helper (>= 2^64-1)
  (should (elinit--validate-limit-value
           :limit-nofile (expt 2 64)))
  (should (elinit--validate-limit-value
           :limit-nofile "999999999999999999999999999999:1")))

(ert-deftest elinit-test-rlimits-build-launch-no-limits ()
  "Build-launch-command without limits produces normal argv."
  (let ((args (elinit--build-launch-command "sleep 300")))
    (should (equal args '("sleep" "300")))))

(ert-deftest elinit-test-rlimits-build-launch-limits-only ()
  "Build-launch-command with limits prepends rlimits helper."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc" :limit-nofile 1024))))
    (let ((args (elinit--build-launch-command "sleep 300" nil nil
                                                  nil entry)))
      ;; First element should be rlimits helper
      (should (equal (car args) elinit-rlimits-command))
      ;; Should contain --nofile
      (should (member "--nofile" args))
      (should (member "1024:1024" args))
      ;; Should end with -- sleep 300
      (let ((sep-pos (cl-position "--" args :test #'equal :from-end t)))
        (should sep-pos)
        (should (equal (nth (+ sep-pos 1) args) "sleep"))
        (should (equal (nth (+ sep-pos 2) args) "300"))))))

(ert-deftest elinit-test-rlimits-build-launch-limits-runas ()
  "Build-launch-command with limits + identity has correct order."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc" :limit-nproc "128:256"))))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_name) "/usr/bin/sleep")))
      (let ((args (elinit--build-launch-command "sleep 300" "alice" nil
                                                    nil entry)))
        ;; Outermost: rlimits helper
        (should (equal (car args) elinit-rlimits-command))
        ;; Should contain --nproc
        (should (member "--nproc" args))
        (should (member "128:256" args))
        ;; After rlimits --, runas should appear
        (let* ((first-sep (cl-position "--" args :test #'equal))
               (after-sep (nth (1+ first-sep) args)))
          (should (equal after-sep elinit-runas-command)))))))

(ert-deftest elinit-test-rlimits-build-launch-limits-sandbox ()
  "Build-launch-command with limits + sandbox has correct order."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc" :sandbox-profile service
                  :limit-core 0))))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_name) "/usr/bin/bwrap")))
      (let ((args (elinit--build-launch-command "sleep 300" nil nil
                                                    entry entry)))
        ;; Outermost: rlimits helper
        (should (equal (car args) elinit-rlimits-command))
        ;; Should contain --core
        (should (member "--core" args))
        ;; After rlimits --, bwrap should appear
        (let* ((first-sep (cl-position "--" args :test #'equal))
               (after-sep (nth (1+ first-sep) args)))
          (should (equal after-sep "/usr/bin/bwrap")))))))

(ert-deftest elinit-test-rlimits-build-launch-limits-runas-sandbox ()
  "Build-launch-command with limits + runas + sandbox has correct order."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc" :sandbox-profile service
                  :limit-fsize infinity))))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_name) "/usr/bin/bwrap")))
      (let ((args (elinit--build-launch-command "sleep 300" "alice" nil
                                                    entry entry)))
        ;; Outermost: rlimits
        (should (equal (car args) elinit-rlimits-command))
        ;; Should contain --fsize infinity:infinity
        (should (member "--fsize" args))
        (should (member "infinity:infinity" args))
        ;; After rlimits --, runas should appear
        (let* ((first-sep (cl-position "--" args :test #'equal))
               (after-sep (nth (1+ first-sep) args)))
          (should (equal after-sep elinit-runas-command)))
        ;; Runas -- should be followed by bwrap
        (let* ((seps (cl-loop for i from 0 below (length args)
                              when (equal (nth i args) "--")
                              collect i))
               (second-sep (cadr seps))
               (after-second (nth (1+ second-sep) args)))
          (should (equal after-second "/usr/bin/bwrap")))))))

(ert-deftest elinit-test-rlimits-plan-invalid-negative ()
  "Build-plan marks entry with negative limit as invalid."
  (let* ((elinit--authority-snapshot nil)
         (programs '(("sleep 300" :id "bad-svc" :limit-nofile -1)
                     ("true" :id "good-svc")))
         (plan (elinit--build-plan programs)))
    (should (gethash "bad-svc" (elinit-plan-invalid plan)))
    (should (string-match "non-negative"
                          (gethash "bad-svc" (elinit-plan-invalid plan))))
    (should-not (gethash "good-svc" (elinit-plan-invalid plan)))))

(ert-deftest elinit-test-rlimits-plan-invalid-soft-exceeds-hard ()
  "Build-plan marks entry with soft > hard limit as invalid."
  (let* ((elinit--authority-snapshot nil)
         (programs '(("sleep 300" :id "bad-svc" :limit-nofile "9999:1")))
         (plan (elinit--build-plan programs)))
    (should (gethash "bad-svc" (elinit-plan-invalid plan)))
    (should (string-match "soft limit.*exceeds hard"
                          (gethash "bad-svc" (elinit-plan-invalid plan))))))

(ert-deftest elinit-test-rlimits-plan-invalid-target-limit ()
  "Build-plan marks target with limit keys as invalid."
  (let* ((elinit--authority-snapshot nil)
         (programs '((nil :id "app.target" :type target :limit-nofile 1024)))
         (plan (elinit--build-plan programs)))
    (should (gethash "app.target" (elinit-plan-invalid plan)))
    (should (string-match "invalid for :type target"
                          (gethash "app.target"
                                   (elinit-plan-invalid plan))))))

;;; Target Plan Building and Structure Tests

(ert-deftest elinit-test-plan-version-2 ()
  "Plan struct version is 2 after stage removal."
  (should (= 2 elinit-plan-version)))

(ert-deftest elinit-test-plan-by-target-global-sort ()
  "Plan by-target is a flat list, not an alist keyed by stage."
  (let* ((programs '(("true" :id "a")
                     ("true" :id "b" :after "a")))
         (plan (elinit--build-plan programs)))
    ;; by-target should be a flat list of entries
    (let ((sorted (elinit-plan-by-target plan)))
      (should (listp sorted))
      ;; Not an alist -- first element should be an entry, not a cons of (int . list)
      (should (stringp (elinit-entry-id (car sorted))))
      ;; a before b (b depends on a)
      (should (equal (mapcar #'elinit-entry-id sorted) '("a" "b"))))))

(ert-deftest elinit-test-builtin-target-ordering-in-plan ()
  "Built-in targets appear in dependency order in plan."
  (let* ((programs
          '((nil :id "basic.target" :type target)
            (nil :id "multi-user.target" :type target
                 :requires ("basic.target") :after ("basic.target"))
            (nil :id "graphical.target" :type target
                 :requires ("multi-user.target") :after ("multi-user.target"))
            (nil :id "default.target" :type target)))
         (plan (elinit--build-plan programs)))
    (let* ((sorted (elinit-plan-by-target plan))
           (ids (mapcar #'elinit-entry-id sorted)))
      ;; basic.target before multi-user.target before graphical.target
      (should (< (cl-position "basic.target" ids :test #'equal)
                 (cl-position "multi-user.target" ids :test #'equal)))
      (should (< (cl-position "multi-user.target" ids :test #'equal)
                 (cl-position "graphical.target" ids :test #'equal))))))

(ert-deftest elinit-test-no-cross-stage-concept ()
  "Entries with different former stages can depend on each other."
  (let* ((programs '(("true" :id "a")
                     ("true" :id "b" :after "a")))
         (plan (elinit--build-plan programs)))
    ;; Both entries are in a single flat list (no stage separation)
    (let ((sorted (elinit-plan-by-target plan)))
      (should (= 2 (length sorted)))
      (should (equal (mapcar #'elinit-entry-id sorted) '("a" "b"))))))

(ert-deftest elinit-test-builtin-programs-overridden-by-disk ()
  "Disk unit file with same ID overrides the built-in entry."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit--programs-cache :not-yet-loaded)
         (elinit--unit-file-invalid (make-hash-table :test 'equal)))
    (elinit-test--write-unit-files dir '(("echo hi" :id "logrotate" :type oneshot)))
    (unwind-protect
        (progn
          (elinit--load-programs)
          (let ((ids (mapcar (lambda (e) (plist-get (cdr e) :id))
                             elinit--programs-cache)))
            ;; Should appear exactly once (disk version, not builtin)
            (should (= 1 (cl-count "logrotate" ids :test #'equal)))
            ;; The command should be the disk version, not the builtin
            (let ((entry (cl-find "logrotate" elinit--programs-cache
                                  :key (lambda (e) (plist-get (cdr e) :id))
                                  :test #'equal)))
              (should (string-match-p "echo hi" (car entry))))))
      (delete-directory dir t))))

;;; Logging and Writer Integration Tests

(ert-deftest elinit-test-logging-toggle-deferred-until-restart ()
  "Policy toggle changes the override hash but does not affect running writer."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple))
    (let ((elinit--logging (make-hash-table :test 'equal))
          (elinit--writers (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit-overrides-file nil)
          (fake-writer (start-process "fake-writer" nil "sleep" "300")))
      (unwind-protect
          (progn
            ;; Simulate a running writer for "svc"
            (puthash "svc" fake-writer elinit--writers)
            ;; Toggle logging off via policy mutator
            (let ((result (elinit--policy-set-logging "svc" nil)))
              (should (eq 'applied (plist-get result :status)))
              ;; Override hash should record disabled
              (should (eq 'disabled (gethash "svc" elinit--logging)))
              ;; Writer should still be in the hash (not stopped mid-flight)
              (should (eq fake-writer (gethash "svc" elinit--writers)))
              ;; Writer process should still be live
              (should (process-live-p fake-writer))))
        (when (process-live-p fake-writer)
          (delete-process fake-writer))))))

(ert-deftest elinit-test-logging-enabled-on-restart-spawns-writer ()
  "Start-process spawns a writer when effective logging is enabled."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (elinit--processes (make-hash-table :test 'equal))
        (elinit--shutting-down nil)
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--logging (make-hash-table :test 'equal))
        (elinit--spawn-failure-reason (make-hash-table :test 'equal))
        (elinit-log-directory "/tmp/test-logs")
        (writer-started nil)
        (fake-proc (start-process "fake-svc" nil "sleep" "300")))
    (unwind-protect
        (cl-letf (((symbol-function 'elinit--get-effective-logging)
                   (lambda (_id _default) t))
                  ((symbol-function 'elinit--ensure-log-directory)
                   #'ignore)
                  ((symbol-function 'elinit--log-file)
                   (lambda (id) (format "/tmp/test-logs/log-%s.log" id)))
                  ((symbol-function 'elinit--start-writer)
                   (lambda (_id _file &optional _log-format)
                     (setq writer-started t)
                     fake-proc))
                  ((symbol-function 'make-process)
                   (lambda (&rest _args) fake-proc))
                  ((symbol-function 'elinit--make-process-sentinel)
                   (lambda (&rest _args) #'ignore))
                  ((symbol-function 'elinit--build-launch-command)
                   (lambda (_cmd &rest _args) (list "sleep" "300"))))
          (let ((proc (elinit--start-process
                       "svc" "sleep 300" nil 'simple 'always)))
            (should proc)
            (should writer-started)))
      (when (process-live-p fake-proc)
        (delete-process fake-proc)))))

(ert-deftest elinit-test-start-process-split-stderr-wiring ()
  "Start-process uses split stderr wiring when stream targets differ."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (elinit--stderr-writers (make-hash-table :test 'equal))
        (elinit--stderr-pipes (make-hash-table :test 'equal))
        (elinit--processes (make-hash-table :test 'equal))
        (elinit--shutting-down nil)
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--logging (make-hash-table :test 'equal))
        (elinit--spawn-failure-reason (make-hash-table :test 'equal))
        (stdout-file nil)
        (stderr-file nil)
        (captured-stderr nil)
        (captured-filter nil)
        (fake-stdout (start-process "fake-stdout-writer" nil "sleep" "300"))
        (fake-stderr (start-process "fake-stderr-writer" nil "sleep" "300"))
        (fake-proc (start-process "fake-svc" nil "sleep" "300"))
        (fake-stderr-pipe 'fake-stderr-pipe))
    (unwind-protect
        (cl-letf (((symbol-function 'elinit--get-effective-logging)
                   (lambda (_id _default) t))
                  ((symbol-function 'elinit--start-writer)
                   (lambda (_id file &optional _log-format)
                     (setq stdout-file file)
                     fake-stdout))
                  ((symbol-function 'elinit--start-stderr-writer)
                   (lambda (_id file &optional _log-format)
                     (setq stderr-file file)
                     fake-stderr))
                  ((symbol-function 'elinit--start-stderr-pipe)
                   (lambda (_id _writer) fake-stderr-pipe))
                  ((symbol-function 'make-process)
                   (lambda (&rest args)
                     (setq captured-stderr (plist-get args :stderr))
                     (setq captured-filter (plist-get args :filter))
                     fake-proc))
                  ((symbol-function 'elinit--make-process-sentinel)
                   (lambda (&rest _args) #'ignore))
                  ((symbol-function 'elinit--build-launch-command)
                   (lambda (_cmd &rest _args) (list "sleep" "300"))))
          (let ((proc (elinit--start-process
                       "svc" "sleep 300" t 'simple 'always
                       nil nil nil nil nil nil nil nil
                       "/tmp/svc.out.log" "/tmp/svc.err.log")))
            (should proc)
            (should (equal stdout-file "/tmp/svc.out.log"))
            (should (equal stderr-file "/tmp/svc.err.log"))
            (should (eq captured-stderr fake-stderr-pipe))
            (should captured-filter)))
      (when (process-live-p fake-proc) (delete-process fake-proc))
      (when (process-live-p fake-stdout) (delete-process fake-stdout))
      (when (process-live-p fake-stderr) (delete-process fake-stderr)))))

(ert-deftest elinit-test-start-process-merged-stderr-when-targets-equal ()
  "Start-process uses merged stderr pipe (not separate writer) when targets match.
No separate stderr writer is spawned, but a merged stderr pipe routes
stderr through the stdout writer for correct stream=2 tagging."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (elinit--stderr-writers (make-hash-table :test 'equal))
        (elinit--stderr-pipes (make-hash-table :test 'equal))
        (elinit--processes (make-hash-table :test 'equal))
        (elinit--shutting-down nil)
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--logging (make-hash-table :test 'equal))
        (elinit--spawn-failure-reason (make-hash-table :test 'equal))
        (stderr-writer-called nil)
        (captured-stderr 'unset)
        (fake-writer (start-process "fake-writer" nil "sleep" "300"))
        (fake-proc (start-process "fake-svc" nil "sleep" "300")))
    (unwind-protect
        (cl-letf (((symbol-function 'elinit--get-effective-logging)
                   (lambda (_id _default) t))
                  ((symbol-function 'elinit--start-writer)
                   (lambda (_id _file &optional _log-format) fake-writer))
                  ((symbol-function 'elinit--start-stderr-writer)
                   (lambda (&rest _args)
                     (setq stderr-writer-called t)
                     nil))
                  ((symbol-function 'make-process)
                   (lambda (&rest args)
                     (setq captured-stderr (plist-get args :stderr))
                     fake-proc))
                  ((symbol-function 'elinit--make-process-sentinel)
                   (lambda (&rest _args) #'ignore))
                  ((symbol-function 'elinit--build-launch-command)
                   (lambda (_cmd &rest _args) (list "sleep" "300"))))
          (let ((proc (elinit--start-process
                       "svc" "sleep 300" t 'simple 'always
                       nil nil nil nil nil nil nil nil
                       "/tmp/svc.shared.log" "/tmp/svc.shared.log")))
            (should proc)
            ;; No separate stderr writer (targets are same)
            (should-not stderr-writer-called)
            ;; But merged stderr pipe IS created for stream identity
            (should captured-stderr)))
      (when (process-live-p fake-proc) (delete-process fake-proc))
      (when (process-live-p fake-writer) (delete-process fake-writer))
      (when-let* ((pipe (gethash "svc" elinit--stderr-pipes)))
        (when (process-live-p pipe) (delete-process pipe))))))

(ert-deftest elinit-test-writer-reopen-sighup-real-process ()
  "Signal-writers-reopen sends SIGHUP to live writer processes."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (elinit--stderr-writers (make-hash-table :test 'equal))
        (proc (start-process "test-sleep" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "svc1" proc elinit--writers)
          (should (process-live-p proc))
          (elinit--signal-writers-reopen)
          ;; sleep does not handle SIGHUP, so it dies
          (sleep-for 0.1)
          (should-not (process-live-p proc)))
      (when (process-live-p proc)
        (delete-process proc)))))

;;; Target Entry Validation and Parsing Tests

(ert-deftest elinit-test-target-entry-without-command-valid ()
  "Target entry with empty or nil command and valid :id passes validation."
  ;; Empty string sentinel form
  (should-not (elinit--validate-entry
               '("" :type target :id "multi.target")))
  ;; Nil car form (first-class inline target)
  (should-not (elinit--validate-entry
               '(nil :type target :id "multi.target"))))

(ert-deftest elinit-test-target-entry-with-command-invalid ()
  "Target entry with a non-empty command is rejected."
  ;; Even with a valid .target ID, a non-empty command is rejected
  (should (string-match-p "target entry must not have a command"
                          (elinit--validate-entry
                           '("my-app" :type target :id "foo.target"))))
  ;; Without explicit :id, the suffix rule also catches it
  (should (elinit--validate-entry '("my-app" :type target))))

(ert-deftest elinit-test-target-invalid-keywords-rejected ()
  "Keywords invalid for :type target produce errors."
  (dolist (kw '(:delay :restart :logging :working-directory
                :exec-stop :kill-signal :user :group))
    (let* ((entry (list "" :type 'target :id "test.target" kw t))
           (result (elinit--validate-entry entry)))
      (should (string-match-p (format "%s is invalid for :type target" kw)
                              result)))))

(ert-deftest elinit-test-entry-parses-wanted-by-required-by ()
  "Parsed entry contains :wanted-by and :required-by at correct indices."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "myapp"
                  :wanted-by "multi.target"
                  :required-by ("net.target" "gui.target")))))
    (should (equal (elinit-entry-wanted-by entry) '("multi.target")))
    (should (equal (elinit-entry-required-by entry)
                   '("net.target" "gui.target")))
    ;; Verify index positions
    (should (equal (nth 30 entry) '("multi.target")))
    (should (equal (nth 31 entry) '("net.target" "gui.target")))))

(ert-deftest elinit-test-target-id-without-suffix-invalid ()
  "Target entry ID not ending in .target is rejected."
  (should (string-match-p ":type target requires ID ending in .target"
                          (elinit--validate-entry
                           '("" :type target :id "myservice")))))

(ert-deftest elinit-test-non-target-id-with-suffix-invalid ()
  "Non-target entry with ID ending in .target is rejected."
  (should (string-match-p "non-target ID must not end in .target"
                          (elinit--validate-entry
                           '("sleep 300" :id "oops.target")))))

(ert-deftest elinit-test-target-parses-to-nil-command ()
  "Target entry parses with nil command from both entry forms."
  ;; Empty string sentinel form
  (let ((entry (elinit--parse-entry
                '("" :type target :id "multi.target"))))
    (should (equal (elinit-entry-id entry) "multi.target"))
    (should (null (elinit-entry-command entry)))
    (should (eq (elinit-entry-type entry) 'target))
    (should (= (length entry) 45)))
  ;; Nil car form
  (let ((entry (elinit--parse-entry
                '(nil :type target :id "multi.target"))))
    (should (equal (elinit-entry-id entry) "multi.target"))
    (should (null (elinit-entry-command entry)))
    (should (eq (elinit-entry-type entry) 'target))
    (should (= (length entry) 45))))

(ert-deftest elinit-test-wanted-by-shape-invalid ()
  ":wanted-by as a non-string, non-list is rejected."
  (should (string-match-p ":wanted-by must be a string or list of strings"
                          (elinit--validate-entry
                           '("sleep 300" :wanted-by 42)))))

(ert-deftest elinit-test-required-by-shape-invalid ()
  ":required-by with non-string element is rejected."
  (should (string-match-p ":required-by must be a string or list of strings"
                          (elinit--validate-entry
                           '("sleep 300" :required-by (42))))))

(ert-deftest elinit-test-wanted-by-missing-target-invalidates-owner ()
  ":wanted-by referencing non-existent target invalidates the entry."
  (let* ((programs '(("" :type target :id "multi.target")
                     ("sleep 300" :id "myapp"
                      :wanted-by "missing.target")))
         (plan (elinit--build-plan programs)))
    (should (gethash "myapp" (elinit-plan-invalid plan)))
    (should (string-match-p "non-existent target"
                            (gethash "myapp" (elinit-plan-invalid plan))))))

(ert-deftest elinit-test-wanted-by-non-target-invalidates-owner ()
  ":wanted-by referencing a non-target entry invalidates the entry."
  (let* ((programs '(("sleep 100" :id "svc-a")
                     ("sleep 300" :id "myapp"
                      :wanted-by "svc-a")))
         (plan (elinit--build-plan programs)))
    (should (gethash "myapp" (elinit-plan-invalid plan)))
    (should (string-match-p "which is not a target"
                            (gethash "myapp" (elinit-plan-invalid plan))))))

(ert-deftest elinit-test-required-by-non-target-invalidates-owner ()
  ":required-by referencing a non-target entry invalidates the entry."
  (let* ((programs '(("sleep 100" :id "svc-a")
                     ("sleep 300" :id "myapp"
                      :required-by "svc-a")))
         (plan (elinit--build-plan programs)))
    (should (gethash "myapp" (elinit-plan-invalid plan)))
    (should (string-match-p "which is not a target"
                            (gethash "myapp" (elinit-plan-invalid plan))))))

(ert-deftest elinit-test-wanted-by-valid-target-accepted ()
  ":wanted-by referencing an existing target passes reference validation."
  (let* ((programs '(("" :type target :id "multi.target")
                     ("sleep 300" :id "myapp"
                      :wanted-by "multi.target")))
         (plan (elinit--build-plan programs)))
    (should-not (gethash "myapp" (elinit-plan-invalid plan)))
    (should (cl-find "myapp" (elinit-plan-entries plan)
                     :key #'elinit-entry-id :test #'equal))))

(ert-deftest elinit-test-target-requires-missing-ref-invalid ()
  "Target with :requires referencing non-existent ID is invalid."
  (let* ((programs '(("" :type target :id "multi.target"
                      :requires "missing-service")))
         (plan (elinit--build-plan programs)))
    (should (gethash "multi.target" (elinit-plan-invalid plan)))
    (should (string-match-p ":requires 'missing-service' does not exist"
                            (gethash "multi.target"
                                     (elinit-plan-invalid plan))))))

(ert-deftest elinit-test-target-requires-valid-ref-accepted ()
  "Target with :requires referencing an existing entry is valid."
  (let* ((programs '(("sleep 300" :id "myapp")
                     ("" :type target :id "multi.target"
                      :requires "myapp")))
         (plan (elinit--build-plan programs)))
    (should-not (gethash "multi.target" (elinit-plan-invalid plan)))))

(ert-deftest elinit-test-target-missing-wants-drops-with-warning ()
  "Target with :wants referencing non-existent ID drops it with warning."
  (let ((warnings nil))
    (cl-letf (((symbol-function 'elinit--log)
               (lambda (_level fmt &rest args)
                 (push (apply #'format fmt args) warnings))))
      (let* ((programs '(("" :type target :id "multi.target"
                          :wants "missing-svc")))
             (plan (elinit--build-plan programs)))
        ;; Target should still be valid (soft dep dropped, not error)
        (should-not (gethash "multi.target" (elinit-plan-invalid plan)))
        ;; A warning should have been emitted
        (should (cl-some (lambda (w) (string-match-p "missing-svc.*does not exist" w))
                         warnings))))))

(ert-deftest elinit-test-target-requires-cycle-invalid ()
  "Cycle in target :requires graph marks participants invalid."
  (let* ((programs '(("" :type target :id "a.target"
                      :requires "b.target")
                     ("" :type target :id "b.target"
                      :requires "a.target")))
         (plan (elinit--build-plan programs)))
    (should (gethash "a.target" (elinit-plan-invalid plan)))
    (should (gethash "b.target" (elinit-plan-invalid plan)))
    (should (string-match-p "cycle detected"
                            (gethash "a.target"
                                     (elinit-plan-invalid plan))))))

(ert-deftest elinit-test-target-requires-three-way-cycle ()
  "Three-way cycle in target :requires marks all participants invalid."
  (let* ((programs '(("" :type target :id "a.target"
                      :requires "b.target")
                     ("" :type target :id "b.target"
                      :requires "c.target")
                     ("" :type target :id "c.target"
                      :requires "a.target")))
         (plan (elinit--build-plan programs)))
    (should (gethash "a.target" (elinit-plan-invalid plan)))
    (should (gethash "b.target" (elinit-plan-invalid plan)))
    (should (gethash "c.target" (elinit-plan-invalid plan)))))

(ert-deftest elinit-test-target-with-after-valid ()
  "Target entry with :after passes validation."
  (should-not (elinit--validate-entry
               '("" :type target :id "multi.target"
                 :after "some-service"))))

(ert-deftest elinit-test-target-self-wanted-by-rejected ()
  ":wanted-by referencing own ID is rejected."
  (should (string-match-p ":wanted-by must not reference the entry's own ID"
                          (elinit--validate-entry
                           '("" :type target :id "multi.target"
                             :wanted-by "multi.target")))))

(ert-deftest elinit-test-target-required-by-self-rejected ()
  ":required-by referencing own ID is rejected."
  (should (string-match-p ":required-by must not reference the entry's own ID"
                          (elinit--validate-entry
                           '("sleep 300" :id "svc"
                             :required-by "svc")))))

(ert-deftest elinit-test-nil-car-target-duplicate-id-detected ()
  "Duplicate nil-car target entries are correctly deduplicated by ID."
  (let* ((programs '((nil :type target :id "a.target")
                     (nil :type target :id "a.target")))
         (plan (elinit--build-plan programs)))
    ;; Only one should survive (first wins)
    (should (= 1 (length (elinit-plan-entries plan))))))

;;; Transaction Expansion Tests

(ert-deftest elinit-test-materialize-target-members-basic ()
  "Materialize target members from :wanted-by declarations."
  (let* ((programs '(("sleep 1" :id "svc-a" :wanted-by ("multi-user.target"))
                     ("sleep 2" :id "svc-b" :wanted-by ("multi-user.target"))
                     (nil :id "multi-user.target" :type target)))
         (plan (elinit--build-plan programs))
         (members (elinit--materialize-target-members
                   (elinit-plan-entries plan))))
    (should (hash-table-p members))
    (let ((mu (gethash "multi-user.target" members)))
      (should mu)
      ;; :wanted-by produces :wants membership
      (should (equal '("svc-a" "svc-b") (plist-get mu :wants)))
      ;; No :requires members
      (should-not (plist-get mu :requires)))))

(ert-deftest elinit-test-materialize-required-by-members ()
  "Services with :required-by produce :requires membership in target."
  (let* ((programs '(("sleep 1" :id "svc-a" :required-by ("basic.target"))
                     (nil :id "basic.target" :type target)))
         (plan (elinit--build-plan programs))
         (members (elinit--materialize-target-members
                   (elinit-plan-entries plan))))
    (let ((bt (gethash "basic.target" members)))
      (should bt)
      (should (equal '("svc-a") (plist-get bt :requires)))
      (should-not (plist-get bt :wants)))))

(ert-deftest elinit-test-expand-transaction-single-target ()
  "Expansion of a single target with wanted-by services."
  (let* ((programs '(("sleep 1" :id "svc-a" :wanted-by ("basic.target"))
                     ("sleep 2" :id "svc-b" :wanted-by ("basic.target"))
                     ("sleep 3" :id "svc-c")
                     (nil :id "basic.target" :type target)))
         (plan (elinit--build-plan programs))
         (entries-by-id (make-hash-table :test 'equal))
         (_ (dolist (e (elinit-plan-entries plan))
              (puthash (elinit-entry-id e) e entries-by-id)))
         (members (elinit--materialize-target-members
                   (elinit-plan-entries plan)))
         (closure (elinit--expand-transaction
                   "basic.target" entries-by-id members
                   (elinit-plan-order-index plan))))
    ;; basic.target and its two wanted services are in closure
    (should (gethash "basic.target" closure))
    (should (gethash "svc-a" closure))
    (should (gethash "svc-b" closure))
    ;; svc-c has no membership, not activated
    (should-not (gethash "svc-c" closure))))

(ert-deftest elinit-test-expand-transaction-target-chain ()
  "Expansion follows target :requires chain."
  (let* ((programs '(("sleep 1" :id "svc-a" :wanted-by ("basic.target"))
                     (nil :id "basic.target" :type target)
                     (nil :id "multi-user.target" :type target
                          :requires ("basic.target")
                          :after ("basic.target"))))
         (plan (elinit--build-plan programs))
         (entries-by-id (make-hash-table :test 'equal))
         (_ (dolist (e (elinit-plan-entries plan))
              (puthash (elinit-entry-id e) e entries-by-id)))
         (members (elinit--materialize-target-members
                   (elinit-plan-entries plan)))
         (closure (elinit--expand-transaction
                   "multi-user.target" entries-by-id members
                   (elinit-plan-order-index plan))))
    ;; multi-user pulls in basic via :requires
    (should (gethash "multi-user.target" closure))
    (should (gethash "basic.target" closure))
    ;; basic pulls in svc-a via membership
    (should (gethash "svc-a" closure))))

(ert-deftest elinit-test-expand-transaction-service-pull-in ()
  "Expansion follows service :requires for pull-in."
  (let* ((programs '(("sleep 1" :id "svc-a"
                                :wanted-by ("basic.target")
                                :requires ("svc-dep"))
                     ("sleep 2" :id "svc-dep")
                     (nil :id "basic.target" :type target)))
         (plan (elinit--build-plan programs))
         (entries-by-id (make-hash-table :test 'equal))
         (_ (dolist (e (elinit-plan-entries plan))
              (puthash (elinit-entry-id e) e entries-by-id)))
         (members (elinit--materialize-target-members
                   (elinit-plan-entries plan)))
         (closure (elinit--expand-transaction
                   "basic.target" entries-by-id members
                   (elinit-plan-order-index plan))))
    ;; svc-a is pulled in by basic.target, and svc-dep by svc-a's :requires
    (should (gethash "basic.target" closure))
    (should (gethash "svc-a" closure))
    (should (gethash "svc-dep" closure))))

(ert-deftest elinit-test-expand-transaction-transitive-service-requires ()
  "Expansion follows transitive service :requires chains."
  (let* ((programs '(("sleep 1" :id "svc-a"
                                :wanted-by ("basic.target")
                                :requires ("svc-b"))
                     ("sleep 2" :id "svc-b" :requires ("svc-c"))
                     ("sleep 3" :id "svc-c")
                     (nil :id "basic.target" :type target)))
         (plan (elinit--build-plan programs))
         (entries-by-id (make-hash-table :test 'equal))
         (_ (dolist (e (elinit-plan-entries plan))
              (puthash (elinit-entry-id e) e entries-by-id)))
         (members (elinit--materialize-target-members
                   (elinit-plan-entries plan)))
         (closure (elinit--expand-transaction
                   "basic.target" entries-by-id members
                   (elinit-plan-order-index plan))))
    (should (gethash "basic.target" closure))
    (should (gethash "svc-a" closure))
    (should (gethash "svc-b" closure))
    (should (gethash "svc-c" closure))))

(ert-deftest elinit-test-expand-transaction-deterministic ()
  "Expansion output is deterministic across identical input."
  (let* ((programs '(("sleep 1" :id "svc-a" :wanted-by ("basic.target"))
                     ("sleep 2" :id "svc-b" :wanted-by ("basic.target"))
                     ("sleep 3" :id "svc-c" :wanted-by ("basic.target"))
                     (nil :id "basic.target" :type target)))
         (plan (elinit--build-plan programs))
         (entries-by-id (make-hash-table :test 'equal))
         (_ (dolist (e (elinit-plan-entries plan))
              (puthash (elinit-entry-id e) e entries-by-id)))
         (members (elinit--materialize-target-members
                   (elinit-plan-entries plan)))
         (c1 (elinit--expand-transaction
              "basic.target" entries-by-id members
              (elinit-plan-order-index plan)))
         (c2 (elinit--expand-transaction
              "basic.target" entries-by-id members
              (elinit-plan-order-index plan))))
    ;; Same number of entries
    (should (= (hash-table-count c1) (hash-table-count c2)))
    ;; Same entries
    (maphash (lambda (k _v) (should (gethash k c2))) c1)))

(ert-deftest elinit-test-expand-transaction-missing-dep-ignored ()
  "Expansion ignores :requires deps that don't exist in entries-by-id."
  (let* ((programs '(("sleep 1" :id "svc-a"
                                :wanted-by ("basic.target")
                                :requires ("nonexistent"))
                     (nil :id "basic.target" :type target)))
         (plan (elinit--build-plan programs)))
    ;; svc-a has :requires "nonexistent" which is invalid and removed
    ;; during validation, so the plan entries should not reference it.
    ;; Build expansion from plan entries:
    (let ((entries-by-id (make-hash-table :test 'equal)))
      (dolist (e (elinit-plan-entries plan))
        (puthash (elinit-entry-id e) e entries-by-id))
      (let* ((members (elinit--materialize-target-members
                       (elinit-plan-entries plan)))
             (closure (elinit--expand-transaction
                       "basic.target" entries-by-id members
                       (elinit-plan-order-index plan))))
        (should (gethash "basic.target" closure))
        ;; svc-a still activated (its wanted-by is valid)
        (should (gethash "svc-a" closure))
        ;; nonexistent is not in closure
        (should-not (gethash "nonexistent" closure))))))

(ert-deftest elinit-test-expand-transaction-no-membership-not-activated ()
  "Services without target membership are not activated."
  (let* ((programs '(("sleep 1" :id "svc-member" :wanted-by ("basic.target"))
                     ("sleep 2" :id "svc-orphan")
                     (nil :id "basic.target" :type target)))
         (plan (elinit--build-plan programs))
         (entries-by-id (make-hash-table :test 'equal))
         (_ (dolist (e (elinit-plan-entries plan))
              (puthash (elinit-entry-id e) e entries-by-id)))
         (members (elinit--materialize-target-members
                   (elinit-plan-entries plan)))
         (closure (elinit--expand-transaction
                   "basic.target" entries-by-id members
                   (elinit-plan-order-index plan))))
    (should (gethash "svc-member" closure))
    (should-not (gethash "svc-orphan" closure))))

(ert-deftest elinit-test-plan-with-root-filters-by-target ()
  "When elinit-start resolves a root, by-target is filtered to closure."
  ;; Use build-plan directly and compute expansion post-hoc (as start does)
  (let* ((programs '(("sleep 1" :id "svc-a" :wanted-by ("basic.target"))
                     ("sleep 2" :id "svc-orphan")
                     (nil :id "basic.target" :type target)))
         (plan (elinit--build-plan programs)))
    ;; by-target before expansion has all entries
    (let ((all-ids (mapcar #'elinit-entry-id
                           (elinit-plan-by-target plan))))
      (should (member "svc-a" all-ids))
      (should (member "svc-orphan" all-ids))
      (should (member "basic.target" all-ids)))
    ;; Now compute expansion and filter (as elinit-start would)
    (let ((entries-by-id (make-hash-table :test 'equal)))
      (dolist (e (elinit-plan-entries plan))
        (puthash (elinit-entry-id e) e entries-by-id))
      (let* ((members (elinit--materialize-target-members
                       (elinit-plan-entries plan)))
             (closure (elinit--expand-transaction
                       "basic.target" entries-by-id members
                       (elinit-plan-order-index plan))))
        (setf (elinit-plan-by-target plan)
              (cl-remove-if-not
               (lambda (e) (gethash (elinit-entry-id e) closure))
               (elinit-plan-by-target plan)))
        ;; Now by-target only has closure entries
        (let ((filtered-ids (mapcar #'elinit-entry-id
                                    (elinit-plan-by-target plan))))
          (should (member "svc-a" filtered-ids))
          (should (member "basic.target" filtered-ids))
          (should-not (member "svc-orphan" filtered-ids)))))))

(ert-deftest elinit-test-plan-without-root-full-list ()
  "Without activation root, by-target contains full sorted list."
  (let* ((programs '(("sleep 1" :id "a")
                     ("sleep 2" :id "b" :after "a")))
         (plan (elinit--build-plan programs)))
    ;; No root computed -- by-target is full list
    (should-not (elinit-plan-activation-root plan))
    (should (= 2 (length (elinit-plan-by-target plan))))))

(ert-deftest elinit-test-passive-target-in-dag ()
  "Target entries are started as passive nodes in the DAG."
  (let* ((programs '((nil :id "basic.target" :type target)
                     ("sleep 1" :id "svc-a"
                                :after ("basic.target")
                                :wanted-by ("basic.target"))))
         (plan (elinit--build-plan programs))
         (entries-by-id (make-hash-table :test 'equal))
         (_ (dolist (e (elinit-plan-entries plan))
              (puthash (elinit-entry-id e) e entries-by-id)))
         (members (elinit--materialize-target-members
                   (elinit-plan-entries plan)))
         (closure (elinit--expand-transaction
                   "basic.target" entries-by-id members
                   (elinit-plan-order-index plan)))
         (activated (cl-remove-if-not
                     (lambda (e) (gethash (elinit-entry-id e) closure))
                     (elinit-plan-by-target plan))))
    ;; Set up DAG globals
    (let ((elinit--dag-blocking (make-hash-table :test 'equal))
          (elinit--dag-in-degree (make-hash-table :test 'equal))
          (elinit--dag-dependents (make-hash-table :test 'equal))
          (elinit--dag-entries (make-hash-table :test 'equal))
          (elinit--dag-started (make-hash-table :test 'equal))
          (elinit--dag-ready (make-hash-table :test 'equal))
          (elinit--dag-timeout-timers (make-hash-table :test 'equal))
          (elinit--dag-delay-timers (make-hash-table :test 'equal))
          (elinit--dag-id-to-index (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--target-members members)
          (elinit--target-convergence (make-hash-table :test 'equal))
          (elinit--target-convergence-reasons (make-hash-table :test 'equal))
          (elinit--target-converging (make-hash-table :test 'equal))
          (elinit--target-member-reverse (make-hash-table :test 'equal)))
      (elinit--dag-init activated)
      ;; Target should have 0 in-degree (no deps)
      (should (= 0 (gethash "basic.target" elinit--dag-in-degree)))
      ;; Now start the target entry
      (let ((target-entry (gethash "basic.target" entries-by-id)))
        (elinit--dag-start-entry-async target-entry))
      ;; Target should be started and ready immediately
      (should (gethash "basic.target" elinit--dag-started))
      (should (gethash "basic.target" elinit--dag-ready))
      ;; State should be 'started
      (should (eq 'started (gethash "basic.target"
                                    elinit--entry-state))))))

(ert-deftest elinit-test-dag-deps-filtered-to-entry-set ()
  "DAG init filters :after and :requires deps to entries in the DAG."
  (let* ((programs '(("sleep 1" :id "svc-a" :after ("missing-dep"))
                     ("sleep 2" :id "svc-b")))
         (plan (elinit--build-plan programs)))
    (let ((elinit--dag-blocking (make-hash-table :test 'equal))
          (elinit--dag-in-degree (make-hash-table :test 'equal))
          (elinit--dag-dependents (make-hash-table :test 'equal))
          (elinit--dag-entries (make-hash-table :test 'equal))
          (elinit--dag-started (make-hash-table :test 'equal))
          (elinit--dag-ready (make-hash-table :test 'equal))
          (elinit--dag-timeout-timers (make-hash-table :test 'equal))
          (elinit--dag-delay-timers (make-hash-table :test 'equal))
          (elinit--dag-id-to-index (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--enabled-override (make-hash-table :test 'equal)))
      ;; Initialize with plan entries (no "missing-dep" entry)
      (elinit--dag-init (elinit-plan-by-target plan))
      ;; svc-a should have 0 in-degree (missing-dep filtered out)
      (should (= 0 (gethash "svc-a" elinit--dag-in-degree))))))

(ert-deftest elinit-test-builtin-maintenance-no-wanted-by ()
  "Built-in logrotate and log-prune do not declare wanted-by.
They are inert fallback definitions activated only by timers."
  (let* ((builtins (elinit--builtin-programs))
         (rotate (cl-find "logrotate" builtins
                          :key (lambda (e) (plist-get (cdr e) :id))
                          :test #'equal))
         (prune (cl-find "log-prune" builtins
                         :key (lambda (e) (plist-get (cdr e) :id))
                         :test #'equal)))
    (should-not (plist-member (cdr rotate) :wanted-by))
    (should-not (plist-member (cdr prune) :wanted-by))))

(ert-deftest elinit-test-startup-activates-only-closure ()
  "Full expansion from graphical.target includes the standard chain."
  (let* ((programs '(("sleep 1" :id "svc-basic" :wanted-by ("basic.target"))
                     ("sleep 2" :id "svc-gui" :wanted-by ("graphical.target"))
                     ("sleep 3" :id "svc-orphan")
                     (nil :id "basic.target" :type target)
                     (nil :id "multi-user.target" :type target
                          :requires ("basic.target")
                          :after ("basic.target"))
                     (nil :id "graphical.target" :type target
                          :requires ("multi-user.target")
                          :after ("multi-user.target"))))
         (plan (elinit--build-plan programs))
         (entries-by-id (make-hash-table :test 'equal))
         (_ (dolist (e (elinit-plan-entries plan))
              (puthash (elinit-entry-id e) e entries-by-id)))
         (members (elinit--materialize-target-members
                   (elinit-plan-entries plan)))
         (closure (elinit--expand-transaction
                   "graphical.target" entries-by-id members
                   (elinit-plan-order-index plan))))
    ;; All three targets reachable via :requires chain
    (should (gethash "graphical.target" closure))
    (should (gethash "multi-user.target" closure))
    (should (gethash "basic.target" closure))
    ;; Services with membership are activated
    (should (gethash "svc-basic" closure))
    (should (gethash "svc-gui" closure))
    ;; Orphan service is not activated
    (should-not (gethash "svc-orphan" closure))))

(ert-deftest elinit-test-expand-transaction-wants-soft-pull-in ()
  "Expansion follows :wants edges on both targets and services."
  (let* ((programs '(("sleep 1" :id "svc-a"
                                :wanted-by ("basic.target")
                                :wants ("svc-opt"))
                     ("sleep 2" :id "svc-opt")
                     (nil :id "basic.target" :type target)))
         (plan (elinit--build-plan programs))
         (entries-by-id (make-hash-table :test 'equal))
         (_ (dolist (e (elinit-plan-entries plan))
              (puthash (elinit-entry-id e) e entries-by-id)))
         (members (elinit--materialize-target-members
                   (elinit-plan-entries plan)))
         (closure (elinit--expand-transaction
                   "basic.target" entries-by-id members
                   (elinit-plan-order-index plan))))
    ;; svc-opt pulled in via svc-a's :wants
    (should (gethash "basic.target" closure))
    (should (gethash "svc-a" closure))
    (should (gethash "svc-opt" closure))))

;;; Target-Aware Plan Builder Tests

(ert-deftest elinit-test-target-auto-ordering-requires-implies-after ()
  "Target :requires automatically implies :after ordering edge."
  (let* ((programs '((nil :id "basic.target" :type target)
                     ("true" :id "svc-a")
                     (nil :id "multi.target" :type target
                          :requires ("basic.target" "svc-a"))))
         (plan (elinit--build-plan programs))
         (after-deps (gethash "multi.target"
                              (elinit-plan-deps plan)))
         (multi-entry (cl-find "multi.target"
                               (elinit-plan-entries plan)
                               :key #'elinit-entry-id :test #'equal)))
    ;; Target's :requires should appear in :after deps
    (should (member "basic.target" after-deps))
    (should (member "svc-a" after-deps))
    ;; Structural integrity: :type preserved, :requires preserved
    (should (eq 'target (elinit-entry-type multi-entry)))
    (should (member "basic.target" (elinit-entry-requires multi-entry)))
    (should (member "svc-a" (elinit-entry-requires multi-entry)))
    ;; Tuple length unchanged
    (should (= 45 (length multi-entry)))))

(ert-deftest elinit-test-target-auto-ordering-wants-implies-after ()
  "Target :wants automatically implies :after ordering edge."
  (let* ((programs '((nil :id "basic.target" :type target)
                     ("true" :id "svc-opt")
                     (nil :id "multi.target" :type target
                          :wants ("basic.target" "svc-opt"))))
         (plan (elinit--build-plan programs))
         (after-deps (gethash "multi.target"
                              (elinit-plan-deps plan)))
         (multi-entry (cl-find "multi.target"
                               (elinit-plan-entries plan)
                               :key #'elinit-entry-id :test #'equal)))
    ;; Target's :wants should appear in :after deps
    (should (member "basic.target" after-deps))
    (should (member "svc-opt" after-deps))
    ;; Structural integrity: :type preserved, :wants preserved
    (should (eq 'target (elinit-entry-type multi-entry)))
    (should (member "basic.target" (elinit-entry-wants multi-entry)))
    (should (member "svc-opt" (elinit-entry-wants multi-entry)))
    ;; Tuple length unchanged
    (should (= 45 (length multi-entry)))))

(ert-deftest elinit-test-service-requires-no-auto-after ()
  "Service :requires does NOT auto-inject :after (only targets do)."
  (let* ((programs '(("true" :id "svc-a")
                     ("true" :id "svc-b" :requires ("svc-a"))))
         (plan (elinit--build-plan programs))
         (after-deps (gethash "svc-b"
                              (elinit-plan-deps plan)))
         (svc-b (cl-find "svc-b" (elinit-plan-entries plan)
                         :key #'elinit-entry-id :test #'equal)))
    ;; Service :requires should NOT appear in :after deps
    (should-not (member "svc-a" after-deps))
    ;; Structural integrity: :type and :requires preserved
    (should (eq 'simple (elinit-entry-type svc-b)))
    (should (equal '("svc-a") (elinit-entry-requires svc-b)))
    (should (= 45 (length svc-b)))))

(ert-deftest elinit-test-builtin-targets-no-redundant-after ()
  "Built-in targets rely on auto-ordering, no explicit :after."
  (let* ((builtins (elinit--builtin-programs)))
    (dolist (entry builtins)
      (when (eq (plist-get (cdr entry) :type) 'target)
        (should-not (plist-get (cdr entry) :after))))))

(ert-deftest elinit-test-plan-fingerprint-deterministic ()
  "Plan fingerprint is deterministic for same input."
  (let* ((programs '(("cmd-a" :id "a")
                     ("cmd-b" :id "b" :after ("a"))))
         (plan1 (elinit--build-plan programs))
         (plan2 (elinit--build-plan programs)))
    (should (equal (plist-get (elinit-plan-meta plan1) :fingerprint)
                   (plist-get (elinit-plan-meta plan2) :fingerprint)))))

(ert-deftest elinit-test-plan-fingerprint-changes-with-input ()
  "Plan fingerprint changes when input changes."
  (let* ((programs1 '(("cmd-a" :id "a")
                      ("cmd-b" :id "b" :after ("a"))))
         (programs2 '(("cmd-a" :id "a")
                      ("cmd-c" :id "c" :after ("a"))))
         (plan1 (elinit--build-plan programs1))
         (plan2 (elinit--build-plan programs2)))
    (should-not (equal (plist-get (elinit-plan-meta plan1) :fingerprint)
                       (plist-get (elinit-plan-meta plan2) :fingerprint)))))

(ert-deftest elinit-test-default-target-alias-and-closure-combined ()
  "Default target alias resolution and closure exactness combined."
  (let* ((programs '((nil :id "basic.target" :type target)
                     (nil :id "graphical.target" :type target
                          :requires ("basic.target"))
                     (nil :id "default.target" :type target)
                     ("cmd-a" :id "svc-a" :wanted-by ("basic.target"))
                     ("cmd-b" :id "svc-b" :wanted-by ("graphical.target"))
                     ("cmd-c" :id "svc-orphan")))
         (plan (elinit--build-plan programs))
         (entries-by-id (make-hash-table :test 'equal))
         (valid-ids (make-hash-table :test 'equal)))
    (dolist (e (elinit-plan-entries plan))
      (puthash (elinit-entry-id e) e entries-by-id)
      (puthash (elinit-entry-id e) t valid-ids))
    ;; Resolve via default.target -> graphical.target alias
    (let* ((elinit-default-target "default.target")
           (elinit-default-target-link "graphical.target")
           (elinit--default-target-link-override nil)
           (root (elinit--resolve-startup-root valid-ids))
           (members (elinit--materialize-target-members
                     (elinit-plan-entries plan)))
           (closure (elinit--expand-transaction
                     root entries-by-id members
                     (elinit-plan-order-index plan))))
      ;; Root resolved to graphical.target
      (should (equal "graphical.target" root))
      ;; Closure includes graphical.target and its transitive deps
      (should (gethash "graphical.target" closure))
      (should (gethash "basic.target" closure))
      (should (gethash "svc-a" closure))
      (should (gethash "svc-b" closure))
      ;; Orphan service not in any target's closure
      (should-not (gethash "svc-orphan" closure)))))

(ert-deftest elinit-test-dep-normalization-preserves-tuple-integrity ()
  "Dep normalization in build-plan preserves all tuple fields."
  (let* ((programs '(("true" :id "svc-a")
                     ("true" :id "svc-b" :after ("svc-a" "nonexistent")
                             :requires ("svc-a" "also-missing"))))
         (plan (elinit--build-plan programs))
         ;; by-target has the dep-normalized entries
         (svc-b (cl-find "svc-b" (elinit-plan-by-target plan)
                         :key #'elinit-entry-id :test #'equal)))
    ;; After invalid dep removal, entry must retain correct structure
    (should (= 45 (length svc-b)))
    (should (equal "svc-b" (elinit-entry-id svc-b)))
    (should (equal "true" (elinit-entry-command svc-b)))
    (should (eq 'simple (elinit-entry-type svc-b)))
    ;; :after normalized to only valid dep
    (should (equal '("svc-a") (elinit-entry-after svc-b)))
    ;; :requires normalized to only valid dep
    (should (equal '("svc-a") (elinit-entry-requires svc-b)))))

(ert-deftest elinit-test-target-dep-normalization-preserves-type ()
  "Target auto-ordering does not corrupt :type during entry rewrite."
  (let* ((programs '((nil :id "base.target" :type target)
                     ("true" :id "svc-x")
                     (nil :id "upper.target" :type target
                          :requires ("base.target")
                          :after ("nonexistent"))))
         (plan (elinit--build-plan programs))
         ;; by-target has the dep-normalized entries
         (upper (cl-find "upper.target" (elinit-plan-by-target plan)
                         :key #'elinit-entry-id :test #'equal)))
    ;; After removing invalid :after "nonexistent", auto-ordering
    ;; injects :requires into :after.  :type must survive.
    (should (eq 'target (elinit-entry-type upper)))
    (should (member "base.target" (elinit-entry-after upper)))
    (should (equal '("base.target") (elinit-entry-requires upper)))
    (should (= 45 (length upper)))))

(ert-deftest elinit-test-mixed-target-service-cycle-fallback ()
  "Cycle involving both target and service entries triggers fallback."
  (let* ((programs '((nil :id "app.target" :type target
                          :requires ("svc-a"))
                     ("true" :id "svc-a" :after ("app.target"))))
         (plan (elinit--build-plan programs)))
    ;; Both should be in cycle-fallback (target's auto-ordering creates
    ;; app.target -> svc-a edge, plus svc-a -> app.target explicit edge)
    (should (gethash "app.target" (elinit-plan-cycle-fallback-ids plan)))
    (should (gethash "svc-a" (elinit-plan-cycle-fallback-ids plan)))
    ;; Deps cleared after fallback
    (should (null (gethash "app.target" (elinit-plan-deps plan))))
    (should (null (gethash "svc-a" (elinit-plan-deps plan))))
    ;; Both entries still present and structurally intact
    (should (= 2 (length (elinit-plan-by-target plan))))
    (let ((app (cl-find "app.target" (elinit-plan-by-target plan)
                        :key #'elinit-entry-id :test #'equal)))
      (should (eq 'target (elinit-entry-type app))))))

;;; Target Convergence Tests

(ert-deftest elinit-test-target-convergence-reached ()
  "Target with all required members ready reaches `reached' state."
  (let ((elinit--dag-ready (make-hash-table :test 'equal))
        (elinit--dag-started (make-hash-table :test 'equal))
        (elinit--dag-entries (make-hash-table :test 'equal))
        (elinit--dag-in-degree (make-hash-table :test 'equal))
        (elinit--dag-dependents (make-hash-table :test 'equal))
        (elinit--dag-id-to-index (make-hash-table :test 'equal))
        (elinit--dag-blocking (make-hash-table :test 'equal))
        (elinit--dag-timeout-timers (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (elinit--dag-complete-callback nil)
        (elinit--entry-state (make-hash-table :test 'equal))
        (elinit--target-convergence (make-hash-table :test 'equal))
        (elinit--target-convergence-reasons (make-hash-table :test 'equal))
        (elinit--target-converging (make-hash-table :test 'equal))
        (elinit--target-member-reverse (make-hash-table :test 'equal))
        (elinit--target-members (make-hash-table :test 'equal)))
    ;; Set up: two required members, both already ready
    (puthash "app.target" '(:requires ("svc-a" "svc-b") :wants nil)
             elinit--target-members)
    (puthash "svc-a" t elinit--dag-ready)
    (puthash "svc-b" t elinit--dag-ready)
    (puthash "svc-a" 'started elinit--entry-state)
    (puthash "svc-b" 'started elinit--entry-state)
    ;; Need target in DAG for mark-ready to work
    (puthash "app.target" t elinit--dag-entries)
    (puthash "app.target" 0 elinit--dag-in-degree)
    (puthash "app.target" nil elinit--dag-dependents)
    ;; Begin convergence
    (elinit--target-begin-convergence "app.target")
    ;; Should resolve immediately as reached
    (should (eq 'reached (gethash "app.target"
                                  elinit--target-convergence)))
    (should-not (gethash "app.target" elinit--target-converging))
    (should (gethash "app.target" elinit--dag-ready))))

(ert-deftest elinit-test-target-convergence-degraded-on-failure ()
  "Target with a failed required member resolves as degraded."
  (let ((elinit--dag-ready (make-hash-table :test 'equal))
        (elinit--dag-started (make-hash-table :test 'equal))
        (elinit--dag-entries (make-hash-table :test 'equal))
        (elinit--dag-in-degree (make-hash-table :test 'equal))
        (elinit--dag-dependents (make-hash-table :test 'equal))
        (elinit--dag-id-to-index (make-hash-table :test 'equal))
        (elinit--dag-blocking (make-hash-table :test 'equal))
        (elinit--dag-timeout-timers (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (elinit--dag-complete-callback nil)
        (elinit--entry-state (make-hash-table :test 'equal))
        (elinit--target-convergence (make-hash-table :test 'equal))
        (elinit--target-convergence-reasons (make-hash-table :test 'equal))
        (elinit--target-converging (make-hash-table :test 'equal))
        (elinit--target-member-reverse (make-hash-table :test 'equal))
        (elinit--target-members (make-hash-table :test 'equal)))
    (puthash "app.target" '(:requires ("svc-a") :wants nil)
             elinit--target-members)
    (puthash "svc-a" t elinit--dag-ready)
    (puthash "svc-a" 'failed-to-spawn elinit--entry-state)
    (puthash "app.target" t elinit--dag-entries)
    (puthash "app.target" 0 elinit--dag-in-degree)
    (puthash "app.target" nil elinit--dag-dependents)
    (elinit--target-begin-convergence "app.target")
    (should (eq 'degraded (gethash "app.target"
                                   elinit--target-convergence)))
    (should (gethash "app.target" elinit--target-convergence-reasons))
    (should (gethash "app.target" elinit--dag-ready))))

(ert-deftest elinit-test-target-convergence-wanted-failure-no-block ()
  "Wanted member failure does not block target convergence."
  (let ((elinit--dag-ready (make-hash-table :test 'equal))
        (elinit--dag-started (make-hash-table :test 'equal))
        (elinit--dag-entries (make-hash-table :test 'equal))
        (elinit--dag-in-degree (make-hash-table :test 'equal))
        (elinit--dag-dependents (make-hash-table :test 'equal))
        (elinit--dag-id-to-index (make-hash-table :test 'equal))
        (elinit--dag-blocking (make-hash-table :test 'equal))
        (elinit--dag-timeout-timers (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (elinit--dag-complete-callback nil)
        (elinit--entry-state (make-hash-table :test 'equal))
        (elinit--target-convergence (make-hash-table :test 'equal))
        (elinit--target-convergence-reasons (make-hash-table :test 'equal))
        (elinit--target-converging (make-hash-table :test 'equal))
        (elinit--target-member-reverse (make-hash-table :test 'equal))
        (elinit--target-members (make-hash-table :test 'equal)))
    ;; No required members, only wanted (wanted failures do not affect state)
    (puthash "app.target" '(:requires nil :wants ("svc-w"))
             elinit--target-members)
    (puthash "app.target" t elinit--dag-entries)
    (puthash "app.target" 0 elinit--dag-in-degree)
    (puthash "app.target" nil elinit--dag-dependents)
    (elinit--target-begin-convergence "app.target")
    ;; Empty required -> immediate reached
    (should (eq 'reached (gethash "app.target"
                                  elinit--target-convergence)))
    (should (gethash "app.target" elinit--dag-ready))))

(ert-deftest elinit-test-target-convergence-empty-members ()
  "Target with no members resolves immediately as reached."
  (let ((elinit--dag-ready (make-hash-table :test 'equal))
        (elinit--dag-started (make-hash-table :test 'equal))
        (elinit--dag-entries (make-hash-table :test 'equal))
        (elinit--dag-in-degree (make-hash-table :test 'equal))
        (elinit--dag-dependents (make-hash-table :test 'equal))
        (elinit--dag-id-to-index (make-hash-table :test 'equal))
        (elinit--dag-blocking (make-hash-table :test 'equal))
        (elinit--dag-timeout-timers (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (elinit--dag-complete-callback nil)
        (elinit--entry-state (make-hash-table :test 'equal))
        (elinit--target-convergence (make-hash-table :test 'equal))
        (elinit--target-convergence-reasons (make-hash-table :test 'equal))
        (elinit--target-converging (make-hash-table :test 'equal))
        (elinit--target-member-reverse (make-hash-table :test 'equal))
        (elinit--target-members (make-hash-table :test 'equal)))
    ;; Target not even in target-members hash -> nil members -> reached
    (puthash "empty.target" t elinit--dag-entries)
    (puthash "empty.target" 0 elinit--dag-in-degree)
    (puthash "empty.target" nil elinit--dag-dependents)
    (elinit--target-begin-convergence "empty.target")
    (should (eq 'reached (gethash "empty.target"
                                  elinit--target-convergence)))
    (should (gethash "empty.target" elinit--dag-ready))))

(ert-deftest elinit-test-target-convergence-blocks-check-complete ()
  "Converging target prevents startup completion callback."
  (let ((elinit--dag-ready (make-hash-table :test 'equal))
        (elinit--dag-started (make-hash-table :test 'equal))
        (elinit--dag-entries (make-hash-table :test 'equal))
        (elinit--dag-in-degree (make-hash-table :test 'equal))
        (elinit--dag-dependents (make-hash-table :test 'equal))
        (elinit--dag-id-to-index (make-hash-table :test 'equal))
        (elinit--dag-blocking (make-hash-table :test 'equal))
        (elinit--dag-timeout-timers (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (elinit--entry-state (make-hash-table :test 'equal))
        (elinit--target-convergence (make-hash-table :test 'equal))
        (elinit--target-convergence-reasons (make-hash-table :test 'equal))
        (elinit--target-converging (make-hash-table :test 'equal))
        (elinit--target-member-reverse (make-hash-table :test 'equal))
        (elinit--target-members (make-hash-table :test 'equal))
        (completed nil))
    ;; Single target entry, still converging
    (puthash "app.target" t elinit--dag-entries)
    (puthash "app.target" t elinit--dag-started)
    (puthash "app.target" t elinit--target-converging)
    (setq elinit--dag-complete-callback (lambda () (setq completed t)))
    ;; check-complete should NOT fire because target is converging
    (elinit--dag-check-complete)
    (should-not completed)))

(ert-deftest elinit-test-target-convergence-force-complete-resolves ()
  "Force-complete resolves converging targets as degraded."
  (let ((elinit--dag-ready (make-hash-table :test 'equal))
        (elinit--dag-started (make-hash-table :test 'equal))
        (elinit--dag-entries (make-hash-table :test 'equal))
        (elinit--dag-in-degree (make-hash-table :test 'equal))
        (elinit--dag-dependents (make-hash-table :test 'equal))
        (elinit--dag-id-to-index (make-hash-table :test 'equal))
        (elinit--dag-blocking (make-hash-table :test 'equal))
        (elinit--dag-timeout-timers (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (elinit--entry-state (make-hash-table :test 'equal))
        (elinit--target-convergence (make-hash-table :test 'equal))
        (elinit--target-convergence-reasons (make-hash-table :test 'equal))
        (elinit--target-converging (make-hash-table :test 'equal))
        (elinit--target-member-reverse (make-hash-table :test 'equal))
        (elinit--target-members (make-hash-table :test 'equal))
        (completed nil))
    (puthash "app.target" t elinit--dag-entries)
    (puthash "app.target" t elinit--dag-started)
    (puthash "app.target" t elinit--target-converging)
    (setq elinit--dag-complete-callback (lambda () (setq completed t)))
    (elinit--dag-force-complete)
    ;; Target should be degraded
    (should (eq 'degraded (gethash "app.target"
                                   elinit--target-convergence)))
    (should (equal '("startup timeout")
                   (gethash "app.target"
                            elinit--target-convergence-reasons)))
    ;; Converging hash should be empty
    (should (= 0 (hash-table-count elinit--target-converging)))
    ;; Callback should have fired
    (should completed)))

(ert-deftest elinit-test-target-convergence-delayed-member ()
  "Required member not yet ready keeps target converging until member ready."
  (let ((elinit--dag-ready (make-hash-table :test 'equal))
        (elinit--dag-started (make-hash-table :test 'equal))
        (elinit--dag-entries (make-hash-table :test 'equal))
        (elinit--dag-in-degree (make-hash-table :test 'equal))
        (elinit--dag-dependents (make-hash-table :test 'equal))
        (elinit--dag-id-to-index (make-hash-table :test 'equal))
        (elinit--dag-blocking (make-hash-table :test 'equal))
        (elinit--dag-timeout-timers (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (elinit--dag-complete-callback nil)
        (elinit--entry-state (make-hash-table :test 'equal))
        (elinit--target-convergence (make-hash-table :test 'equal))
        (elinit--target-convergence-reasons (make-hash-table :test 'equal))
        (elinit--target-converging (make-hash-table :test 'equal))
        (elinit--target-member-reverse (make-hash-table :test 'equal))
        (elinit--target-members (make-hash-table :test 'equal)))
    ;; Set up target with one required member not yet ready
    (puthash "app.target" '(:requires ("svc-a") :wants nil)
             elinit--target-members)
    (puthash "app.target" t elinit--dag-entries)
    (puthash "app.target" 0 elinit--dag-in-degree)
    (puthash "app.target" nil elinit--dag-dependents)
    ;; Build reverse index
    (puthash "svc-a" '("app.target") elinit--target-member-reverse)
    ;; svc-a not yet in dag-ready
    (elinit--target-begin-convergence "app.target")
    ;; Should still be converging
    (should (eq 'converging (gethash "app.target"
                                     elinit--target-convergence)))
    (should (gethash "app.target" elinit--target-converging))
    (should-not (gethash "app.target" elinit--dag-ready))
    ;; Now svc-a becomes ready -- simulate via mark-ready
    (puthash "svc-a" t elinit--dag-entries)
    (puthash "svc-a" 0 elinit--dag-in-degree)
    (puthash "svc-a" nil elinit--dag-dependents)
    (puthash "svc-a" 'started elinit--entry-state)
    (elinit--dag-mark-ready "svc-a")
    ;; Now target should be reached
    (should (eq 'reached (gethash "app.target"
                                  elinit--target-convergence)))
    (should-not (gethash "app.target" elinit--target-converging))
    (should (gethash "app.target" elinit--dag-ready))))

(ert-deftest elinit-test-target-convergence-disabled-member-ok ()
  "Disabled required member in dag-ready results in reached, not degraded."
  (let ((elinit--dag-ready (make-hash-table :test 'equal))
        (elinit--dag-started (make-hash-table :test 'equal))
        (elinit--dag-entries (make-hash-table :test 'equal))
        (elinit--dag-in-degree (make-hash-table :test 'equal))
        (elinit--dag-dependents (make-hash-table :test 'equal))
        (elinit--dag-id-to-index (make-hash-table :test 'equal))
        (elinit--dag-blocking (make-hash-table :test 'equal))
        (elinit--dag-timeout-timers (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (elinit--dag-complete-callback nil)
        (elinit--entry-state (make-hash-table :test 'equal))
        (elinit--target-convergence (make-hash-table :test 'equal))
        (elinit--target-convergence-reasons (make-hash-table :test 'equal))
        (elinit--target-converging (make-hash-table :test 'equal))
        (elinit--target-member-reverse (make-hash-table :test 'equal))
        (elinit--target-members (make-hash-table :test 'equal)))
    (puthash "app.target" '(:requires ("svc-a") :wants nil)
             elinit--target-members)
    (puthash "svc-a" t elinit--dag-ready)
    (puthash "svc-a" 'disabled elinit--entry-state)
    (puthash "app.target" t elinit--dag-entries)
    (puthash "app.target" 0 elinit--dag-in-degree)
    (puthash "app.target" nil elinit--dag-dependents)
    (elinit--target-begin-convergence "app.target")
    ;; Disabled is not a failure state, so should be reached
    (should (eq 'reached (gethash "app.target"
                                  elinit--target-convergence)))
    (should (gethash "app.target" elinit--dag-ready))))

(ert-deftest elinit-test-materialize-target-members-includes-target-requires ()
  "Target with :requires produces :requires membership from target itself."
  (let* ((programs '(("sleep 1" :id "svc-a")
                     (nil :id "app.target" :type target
                          :requires ("svc-a"))))
         (plan (elinit--build-plan programs))
         (members (elinit--materialize-target-members
                   (elinit-plan-entries plan))))
    (let ((app (gethash "app.target" members)))
      (should app)
      (should (equal '("svc-a") (plist-get app :requires))))))

(ert-deftest elinit-test-materialize-target-members-deduplicates ()
  "Service :required-by and target :requires for same service deduplicates."
  (let* ((programs '(("sleep 1" :id "svc-a"
                       :required-by ("app.target"))
                     (nil :id "app.target" :type target
                          :requires ("svc-a"))))
         (plan (elinit--build-plan programs))
         (members (elinit--materialize-target-members
                   (elinit-plan-entries plan))))
    (let ((app (gethash "app.target" members)))
      (should app)
      ;; Should have svc-a exactly once, not duplicated
      (should (equal '("svc-a") (plist-get app :requires))))))

(ert-deftest elinit-test-target-requires-failure-produces-degraded ()
  "Target with own :requires, member fails -> convergence is degraded."
  (let ((elinit--dag-ready (make-hash-table :test 'equal))
        (elinit--dag-started (make-hash-table :test 'equal))
        (elinit--dag-entries (make-hash-table :test 'equal))
        (elinit--dag-in-degree (make-hash-table :test 'equal))
        (elinit--dag-dependents (make-hash-table :test 'equal))
        (elinit--dag-id-to-index (make-hash-table :test 'equal))
        (elinit--dag-blocking (make-hash-table :test 'equal))
        (elinit--dag-timeout-timers (make-hash-table :test 'equal))
        (elinit--dag-delay-timers (make-hash-table :test 'equal))
        (elinit--dag-complete-callback nil)
        (elinit--entry-state (make-hash-table :test 'equal))
        (elinit--target-convergence (make-hash-table :test 'equal))
        (elinit--target-convergence-reasons (make-hash-table :test 'equal))
        (elinit--target-converging (make-hash-table :test 'equal))
        (elinit--target-member-reverse (make-hash-table :test 'equal))
        (elinit--target-members (make-hash-table :test 'equal)))
    (let* ((programs '(("sleep 1" :id "svc-a")
                       (nil :id "app.target" :type target
                            :requires ("svc-a"))))
           (plan (elinit--build-plan programs))
           (members (elinit--materialize-target-members
                     (elinit-plan-entries plan))))
      (setq elinit--target-members members)
      (puthash "svc-a" t elinit--dag-ready)
      (puthash "svc-a" 'failed-to-spawn elinit--entry-state)
      (puthash "app.target" t elinit--dag-entries)
      (puthash "app.target" 0 elinit--dag-in-degree)
      (puthash "app.target" nil elinit--dag-dependents)
      (elinit--target-begin-convergence "app.target")
      (should (eq 'degraded (gethash "app.target"
                                     elinit--target-convergence))))))

(ert-deftest elinit-test-target-requires-invalid-ref-invalidates-target ()
  "Target :requires ref invalidated during validation -> target invalid."
  (let* ((programs '(("sleep 1" :id "svc-a"
                       :required-by ("nonexistent.target"))
                     (nil :id "app.target" :type target
                          :requires ("svc-a"))))
         (plan (elinit--build-plan programs)))
    (should (gethash "app.target" (elinit-plan-invalid plan)))))

;;; DAG Oneshot Blocking Tests

(ert-deftest elinit-test-dag-start-with-deps-oneshot-blocking ()
  "DAG start respects blocking oneshot: dependent waits for oneshot exit."
  (elinit-test-with-unit-files
      '(("true" :id "setup" :type oneshot :oneshot-blocking t)
        ("sleep 300" :id "svc" :type simple :after ("setup")))
    (let ((elinit--dag-in-degree (make-hash-table :test 'equal))
          (elinit--dag-dependents (make-hash-table :test 'equal))
          (elinit--dag-entries (make-hash-table :test 'equal))
          (elinit--dag-blocking (make-hash-table :test 'equal))
          (elinit--dag-started (make-hash-table :test 'equal))
          (elinit--dag-ready (make-hash-table :test 'equal))
          (elinit--dag-timeout-timers (make-hash-table :test 'equal))
          (elinit--dag-delay-timers (make-hash-table :test 'equal))
          (elinit--dag-id-to-index (make-hash-table :test 'equal))
          (elinit--dag-complete-callback nil)
          (elinit--dag-pending-starts nil)
          (elinit--dag-active-starts 0)
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--ready-times (make-hash-table :test 'equal))
          (elinit--start-times (make-hash-table :test 'equal))
          (elinit--target-member-reverse nil)
          (elinit--target-converging nil)
          (elinit--shutting-down nil)
          (elinit-max-concurrent-starts nil)
          (spawned nil)
          (complete nil))
      (let* ((plan (elinit--build-plan (elinit--effective-programs)))
             (entries (cl-remove-if
                       (lambda (e) (eq (elinit-entry-type e) 'target))
                       (elinit-plan-by-target plan))))
        ;; Mock start-process to track spawns without real processes
        (cl-letf (((symbol-function 'elinit--start-process)
                   (lambda (id _cmd _log _type _restart &rest _args)
                     (push id spawned)
                     ;; Return a fake process object
                     (start-process (concat "fake-" id) nil "sleep" "300")))
                  ((symbol-function 'executable-find) (lambda (_) t))
                  ((symbol-function 'elinit--emit-event) #'ignore)
                  ((symbol-function 'elinit--maybe-refresh-dashboard)
                   #'ignore))
          (unwind-protect
              (progn
                ;; Start entries via DAG
                (elinit--dag-start-with-deps
                 entries
                 (lambda () (setq complete t)))
                ;; Oneshot should be spawned first
                (should (member "setup" spawned))
                ;; Dependent should NOT be spawned yet (waiting for oneshot)
                (should-not (member "svc" spawned))
                ;; Simulate oneshot exit by marking ready
                (elinit--dag-mark-ready "setup")
                ;; Now dependent should be spawned
                (should (member "svc" spawned)))
            ;; Cleanup fake processes
            (maphash (lambda (_id proc)
                       (when (process-live-p proc)
                         (delete-process proc)))
                     elinit--processes)))))))

;;; SysV Alias and Runlevel Compatibility Tests

(ert-deftest elinit-test-runlevel-map-complete ()
  "Runlevel map covers all 7 runlevels 0-6."
  (dolist (n '(0 1 2 3 4 5 6))
    (should (assq n elinit--runlevel-map))))

(ert-deftest elinit-test-runlevel-map-values ()
  "Runlevel map matches systemd semantics exactly."
  (should (equal "poweroff.target" (cdr (assq 0 elinit--runlevel-map))))
  (should (equal "rescue.target" (cdr (assq 1 elinit--runlevel-map))))
  (should (equal "multi-user.target" (cdr (assq 2 elinit--runlevel-map))))
  (should (equal "multi-user.target" (cdr (assq 3 elinit--runlevel-map))))
  (should (equal "multi-user.target" (cdr (assq 4 elinit--runlevel-map))))
  (should (equal "graphical.target" (cdr (assq 5 elinit--runlevel-map))))
  (should (equal "reboot.target" (cdr (assq 6 elinit--runlevel-map)))))

(ert-deftest elinit-test-alias-resolution ()
  "Alias targets resolve to canonical targets."
  (should (equal "poweroff.target"
                 (elinit--resolve-target-alias "runlevel0.target")))
  (should (equal "rescue.target"
                 (elinit--resolve-target-alias "runlevel1.target")))
  (should (equal "multi-user.target"
                 (elinit--resolve-target-alias "runlevel3.target")))
  (should (equal "graphical.target"
                 (elinit--resolve-target-alias "runlevel5.target")))
  (should (equal "reboot.target"
                 (elinit--resolve-target-alias "runlevel6.target"))))

(ert-deftest elinit-test-alias-resolution-passthrough ()
  "Non-alias targets pass through unchanged."
  (should (equal "graphical.target"
                 (elinit--resolve-target-alias "graphical.target")))
  (should (equal "basic.target"
                 (elinit--resolve-target-alias "basic.target")))
  (should (equal "my-service"
                 (elinit--resolve-target-alias "my-service"))))

(ert-deftest elinit-test-alias-predicate ()
  "Alias predicate identifies aliases and non-aliases."
  (should (elinit--target-alias-p "runlevel0.target"))
  (should (elinit--target-alias-p "runlevel5.target"))
  (should-not (elinit--target-alias-p "graphical.target"))
  (should-not (elinit--target-alias-p "basic.target")))

(ert-deftest elinit-test-builtin-init-targets-present ()
  "Built-in programs include init-transition canonical targets."
  (let* ((builtins (elinit--builtin-programs))
         (ids (mapcar (lambda (e) (plist-get (cdr e) :id)) builtins)))
    (should (member "rescue.target" ids))
    (should (member "shutdown.target" ids))
    (should (member "poweroff.target" ids))
    (should (member "reboot.target" ids))))

(ert-deftest elinit-test-builtin-alias-targets-present ()
  "Built-in programs include all runlevel alias targets."
  (let* ((builtins (elinit--builtin-programs))
         (ids (mapcar (lambda (e) (plist-get (cdr e) :id)) builtins)))
    (dolist (n '(0 1 2 3 4 5 6))
      (should (member (format "runlevel%d.target" n) ids)))))

(ert-deftest elinit-test-builtin-init-target-topology ()
  "Init-transition targets have correct dependency chain."
  (let* ((builtins (elinit--builtin-programs))
         (rescue (cl-find "rescue.target" builtins
                          :key (lambda (e) (plist-get (cdr e) :id))
                          :test #'equal))
         (shutdown (cl-find "shutdown.target" builtins
                            :key (lambda (e) (plist-get (cdr e) :id))
                            :test #'equal))
         (poweroff (cl-find "poweroff.target" builtins
                            :key (lambda (e) (plist-get (cdr e) :id))
                            :test #'equal))
         (reboot (cl-find "reboot.target" builtins
                          :key (lambda (e) (plist-get (cdr e) :id))
                          :test #'equal)))
    ;; rescue.target requires basic.target
    (should (equal '("basic.target") (plist-get (cdr rescue) :requires)))
    ;; shutdown.target has no requires
    (should-not (plist-get (cdr shutdown) :requires))
    ;; poweroff.target requires shutdown.target
    (should (equal '("shutdown.target") (plist-get (cdr poweroff) :requires)))
    ;; reboot.target requires shutdown.target
    (should (equal '("shutdown.target") (plist-get (cdr reboot) :requires)))))

(ert-deftest elinit-test-alias-target-with-deps-rejected-by-validation ()
  "Disk alias target with dependency edges is caught by validation."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit--programs-cache :not-yet-loaded)
         (elinit--unit-file-invalid (make-hash-table :test 'equal))
         (warnings nil))
    (elinit-test--write-unit-files
     dir '((nil :id "runlevel5.target" :type target
                :requires ("basic.target"))))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'elinit--log)
                     (lambda (_level fmt &rest args)
                       (push (apply #'format fmt args) warnings))))
            (elinit--load-programs))
          ;; The alias target should still be the built-in (no :requires)
          (let ((entry (cl-find "runlevel5.target"
                                elinit--programs-cache
                                :key (lambda (e) (plist-get (cdr e) :id))
                                :test #'equal)))
            (should entry)
            (should-not (plist-get (cdr entry) :requires)))
          ;; Validation rejected with "passive" reason
          (should (cl-some (lambda (w)
                             (string-match-p "runlevel5\\.target.*passive" w))
                           warnings))
          (should-not (gethash "runlevel5.target"
                               elinit--unit-file-invalid)))
      (delete-directory dir t))))

(ert-deftest elinit-test-alias-target-no-deps-rejected-by-merge ()
  "Disk alias target without deps passes validation but is rejected at merge."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit--programs-cache :not-yet-loaded)
         (elinit--unit-file-invalid (make-hash-table :test 'equal))
         (warnings nil))
    (elinit-test--write-unit-files
     dir '((nil :id "runlevel3.target" :type target)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'elinit--log)
                     (lambda (_level fmt &rest args)
                       (push (apply #'format fmt args) warnings))))
            (elinit--load-programs))
          (let ((entry (cl-find "runlevel3.target"
                                elinit--programs-cache
                                :key (lambda (e) (plist-get (cdr e) :id))
                                :test #'equal)))
            (should entry))
          (should (cl-some (lambda (w)
                             (string-match-p "runlevel3\\.target.*immutable" w))
                           warnings))
          (should-not (gethash "runlevel3.target"
                               elinit--unit-file-invalid)))
      (delete-directory dir t))))

(ert-deftest elinit-test-alias-invalid-winner-does-not-suppress-builtin ()
  "Invalid disk alias target does not suppress the valid builtin in merged set."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit--programs-cache :not-yet-loaded)
         (elinit--unit-file-invalid (make-hash-table :test 'equal)))
    (elinit-test--write-unit-files
     dir '((nil :id "runlevel0.target" :type target
                :after ("basic.target"))))
    (unwind-protect
        (progn
          (elinit--load-programs)
          (let ((entry (cl-find "runlevel0.target"
                                elinit--programs-cache
                                :key (lambda (e) (plist-get (cdr e) :id))
                                :test #'equal)))
            (should entry)
            (should-not (plist-get (cdr entry) :after))
            (should (string-match-p "poweroff"
                                    (plist-get (cdr entry) :description))))
          (should-not (gethash "runlevel0.target"
                               elinit--unit-file-invalid))
          (should (cl-find "runlevel5.target"
                           elinit--programs-cache
                           :key (lambda (e) (plist-get (cdr e) :id))
                           :test #'equal)))
      (delete-directory dir t))))

(ert-deftest elinit-test-alias-target-override-non-alias-still-works ()
  "Disk unit can still override non-alias built-in targets."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit--programs-cache :not-yet-loaded)
         (elinit--unit-file-invalid (make-hash-table :test 'equal)))
    (elinit-test--write-unit-files
     dir '((nil :id "basic.target" :type target
                :description "User-custom basic target")))
    (unwind-protect
        (progn
          (elinit--load-programs)
          (let ((entry (cl-find "basic.target"
                                elinit--programs-cache
                                :key (lambda (e) (plist-get (cdr e) :id))
                                :test #'equal)))
            (should entry)
            (should (equal "User-custom basic target"
                           (plist-get (cdr entry) :description)))
            (should (= 1 (cl-count "basic.target"
                                   elinit--programs-cache
                                   :key (lambda (e) (plist-get (cdr e) :id))
                                   :test #'equal)))))
      (delete-directory dir t))))

(ert-deftest elinit-test-merged-targets-include-canonical-and-alias ()
  "Merged runtime load path includes both canonical and alias targets."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit--programs-cache :not-yet-loaded)
         (elinit--unit-file-invalid (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (elinit--load-programs)
          (let ((ids (mapcar (lambda (e) (plist-get (cdr e) :id))
                             elinit--programs-cache)))
            (should (member "basic.target" ids))
            (should (member "multi-user.target" ids))
            (should (member "graphical.target" ids))
            (should (member "default.target" ids))
            (should (member "rescue.target" ids))
            (should (member "shutdown.target" ids))
            (should (member "poweroff.target" ids))
            (should (member "reboot.target" ids))
            (dolist (n '(0 1 2 3 4 5 6))
              (should (member (format "runlevel%d.target" n) ids)))))
      (delete-directory dir t))))

(ert-deftest elinit-test-alias-target-validation-rejects-deps ()
  "Validation rejects alias target entries that carry dependency edges."
  (dolist (kw '(:requires :wants :after :before :wanted-by :required-by))
    (let* ((entry (list nil :id "runlevel3.target" :type 'target
                        kw '("basic.target")))
           (reason (elinit--validate-entry entry)))
      (should reason)
      (should (string-match-p "alias target" reason))
      (should (string-match-p "passive" reason))
      (should (string-match-p (symbol-name kw) reason)))))

(ert-deftest elinit-test-alias-target-validation-accepts-passive ()
  "Validation accepts alias target entries without dependency edges."
  (let* ((entry (list nil :id "runlevel5.target" :type 'target
                      :description "Alias for graphical.target"))
         (reason (elinit--validate-entry entry)))
    (should-not reason)))

(ert-deftest elinit-test-canonical-target-validation-allows-deps ()
  "Validation allows canonical (non-alias) targets to carry dependencies."
  (let* ((entry (list nil :id "multi-user.target" :type 'target
                      :requires '("basic.target")))
         (reason (elinit--validate-entry entry)))
    (should-not reason)))

;;; Target Regression Tests

(ert-deftest elinit-test-convergence-survives-dag-cleanup ()
  "Target convergence state persists after DAG cleanup."
  (let ((elinit--target-convergence (make-hash-table :test 'equal))
        (elinit--target-convergence-reasons (make-hash-table :test 'equal))
        (elinit--target-converging nil)
        (elinit--target-member-reverse nil)
        (elinit--target-members (make-hash-table :test 'equal))
        (elinit--dag-in-degree nil)
        (elinit--dag-dependents nil)
        (elinit--dag-entries nil)
        (elinit--dag-blocking nil)
        (elinit--dag-started nil)
        (elinit--dag-ready nil)
        (elinit--dag-timeout-timers nil)
        (elinit--dag-delay-timers nil)
        (elinit--dag-id-to-index nil)
        (elinit--dag-complete-callback nil)
        (elinit--dag-timeout-timer nil)
        (elinit--dag-pending-starts nil)
        (elinit--dag-active-starts 0))
    (puthash "basic.target" 'reached elinit--target-convergence)
    (puthash "multi.target" 'degraded elinit--target-convergence)
    (puthash "multi.target" '("svc: failed") elinit--target-convergence-reasons)
    (elinit--dag-cleanup)
    ;; Convergence state must survive
    (should (eq 'reached (gethash "basic.target" elinit--target-convergence)))
    (should (eq 'degraded (gethash "multi.target" elinit--target-convergence)))
    (should (equal '("svc: failed")
                   (gethash "multi.target" elinit--target-convergence-reasons)))
    ;; DAG temporaries must be cleared
    (should (null elinit--target-converging))
    (should (null elinit--target-member-reverse))))

(ert-deftest elinit-test-disabled-oneshot-status ()
  "Disabled oneshot entries show status=disabled, not pending."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--mask-override (make-hash-table :test 'equal))
        (elinit--entry-state (make-hash-table :test 'equal))
        (elinit--target-convergence nil)
        (elinit--current-plan nil))
    (puthash "setup-x" 'disabled elinit--entry-state)
    (let ((result (elinit--compute-entry-status "setup-x" 'oneshot)))
      (should (equal "disabled" (car result))))))

(ert-deftest elinit-test-disabled-simple-status ()
  "Disabled simple entries show status=disabled, not stopped."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--mask-override (make-hash-table :test 'equal))
        (elinit--entry-state (make-hash-table :test 'equal))
        (elinit--target-convergence nil)
        (elinit--current-plan nil))
    (puthash "svc" 'disabled elinit--entry-state)
    (let ((result (elinit--compute-entry-status "svc" 'simple)))
      (should (equal "disabled" (car result))))))

(ert-deftest elinit-test-default-target-alias-mirrors-link ()
  "Status of default.target mirrors its resolved link convergence."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--mask-override (make-hash-table :test 'equal))
        (elinit--entry-state (make-hash-table :test 'equal))
        (elinit--target-convergence (make-hash-table :test 'equal))
        (elinit--current-plan nil)
        (elinit-default-target-link "graphical.target")
        (elinit--default-target-link-override nil))
    (puthash "graphical.target" 'reached elinit--target-convergence)
    (let ((result (elinit--compute-entry-status "default.target" 'target)))
      (should (equal "reached" (car result))))))

(ert-deftest elinit-test-unreachable-status-outside-closure ()
  "Entries outside the activation closure show unreachable status."
  (let* ((elinit--processes (make-hash-table :test 'equal))
         (elinit--failed (make-hash-table :test 'equal))
         (elinit--oneshot-completed (make-hash-table :test 'equal))
         (elinit--remain-active (make-hash-table :test 'equal))
         (elinit--manually-stopped (make-hash-table :test 'equal))
         (elinit--mask-override (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal))
         (elinit--target-convergence nil)
         (closure (make-hash-table :test 'equal))
         (elinit--current-plan
          (elinit-plan--create
           :entries nil :by-target nil
           :deps (make-hash-table :test 'equal)
           :requires-deps (make-hash-table :test 'equal)
           :dependents (make-hash-table :test 'equal)
           :invalid (make-hash-table :test 'equal)
           :cycle-fallback-ids (make-hash-table :test 'equal)
           :order-index (make-hash-table :test 'equal)
           :meta nil
           :activation-closure closure)))
    ;; "inside" is in the closure
    (puthash "inside" t closure)
    ;; "outside" is not in the closure
    (let ((result (elinit--compute-entry-status "outside" 'simple)))
      (should (equal "unreachable" (car result))))
    ;; "inside" should show normal status (stopped since not running)
    (let ((result (elinit--compute-entry-status "inside" 'simple)))
      (should (equal "stopped" (car result))))))

(ert-deftest elinit-test-alias-target-status-mirrors-canonical ()
  "Alias target status mirrors convergence stored under canonical ID."
  (let* ((elinit--processes (make-hash-table :test 'equal))
         (elinit--failed (make-hash-table :test 'equal))
         (elinit--oneshot-completed (make-hash-table :test 'equal))
         (elinit--remain-active (make-hash-table :test 'equal))
         (elinit--manually-stopped (make-hash-table :test 'equal))
         (elinit--mask-override (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal))
         (elinit--target-convergence (make-hash-table :test 'equal))
         (elinit--current-plan nil))
    (puthash "multi-user.target" 'reached elinit--target-convergence)
    (let ((result (elinit--compute-entry-status
                   "runlevel3.target" 'target)))
      (should (equal "reached" (car result))))))

(ert-deftest elinit-test-alias-target-reason-mirrors-canonical ()
  "Alias target reason mirrors degraded reasons stored under canonical ID."
  (let* ((elinit--processes (make-hash-table :test 'equal))
         (elinit--failed (make-hash-table :test 'equal))
         (elinit--oneshot-completed (make-hash-table :test 'equal))
         (elinit--mask-override (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal))
         (elinit--target-convergence (make-hash-table :test 'equal))
         (elinit--target-convergence-reasons (make-hash-table :test 'equal))
         (elinit--current-plan nil))
    (puthash "multi-user.target" 'degraded elinit--target-convergence)
    (puthash "multi-user.target" '("svc failed")
             elinit--target-convergence-reasons)
    (let ((reason (elinit--compute-entry-reason
                   "runlevel3.target" 'target)))
      (should (equal "svc failed" reason)))))

(ert-deftest elinit-test-target-outside-closure-shows-unreachable ()
  "Target units outside activation closure show unreachable, not pending."
  (let* ((elinit--processes (make-hash-table :test 'equal))
         (elinit--failed (make-hash-table :test 'equal))
         (elinit--oneshot-completed (make-hash-table :test 'equal))
         (elinit--remain-active (make-hash-table :test 'equal))
         (elinit--manually-stopped (make-hash-table :test 'equal))
         (elinit--mask-override (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal))
         (elinit--target-convergence (make-hash-table :test 'equal))
         (closure (make-hash-table :test 'equal))
         (elinit--current-plan
          (elinit-plan--create
           :entries nil :by-target nil
           :deps (make-hash-table :test 'equal)
           :requires-deps (make-hash-table :test 'equal)
           :dependents (make-hash-table :test 'equal)
           :invalid (make-hash-table :test 'equal)
           :cycle-fallback-ids (make-hash-table :test 'equal)
           :order-index (make-hash-table :test 'equal)
           :meta nil
           :activation-closure closure)))
    (puthash "graphical.target" t closure)
    (let ((result (elinit--compute-entry-status
                   "rescue.target" 'target)))
      (should (equal "unreachable" (car result))))
    (let ((result (elinit--compute-entry-status
                   "graphical.target" 'target)))
      (should (equal "pending" (car result))))))

(ert-deftest elinit-test-alias-target-outside-closure-shows-unreachable ()
  "Alias targets outside closure show unreachable via canonical resolution."
  (let* ((elinit--processes (make-hash-table :test 'equal))
         (elinit--failed (make-hash-table :test 'equal))
         (elinit--oneshot-completed (make-hash-table :test 'equal))
         (elinit--remain-active (make-hash-table :test 'equal))
         (elinit--manually-stopped (make-hash-table :test 'equal))
         (elinit--mask-override (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal))
         (elinit--target-convergence (make-hash-table :test 'equal))
         (closure (make-hash-table :test 'equal))
         (elinit--current-plan
          (elinit-plan--create
           :entries nil :by-target nil
           :deps (make-hash-table :test 'equal)
           :requires-deps (make-hash-table :test 'equal)
           :dependents (make-hash-table :test 'equal)
           :invalid (make-hash-table :test 'equal)
           :cycle-fallback-ids (make-hash-table :test 'equal)
           :order-index (make-hash-table :test 'equal)
           :meta nil
           :activation-closure closure)))
    (puthash "multi-user.target" t closure)
    (puthash "multi-user.target" 'reached elinit--target-convergence)
    (let ((result (elinit--compute-entry-status
                   "runlevel3.target" 'target)))
      (should (equal "reached" (car result))))
    (let ((result (elinit--compute-entry-status
                   "runlevel1.target" 'target)))
      (should (equal "unreachable" (car result))))))

;;; Target Health and Guard Tests

(ert-deftest elinit-test-health-counts-disabled-not-pending ()
  "Health counts put disabled entries in :disabled, not :pending."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--mask-override (make-hash-table :test 'equal))
        (elinit--invalid (make-hash-table :test 'equal))
        (elinit--entry-state (make-hash-table :test 'equal)))
    (puthash "setup-a" 'disabled elinit--entry-state)
    (let ((counts (elinit--health-counts
                   nil
                   '(("true" :id "setup-a" :type oneshot)))))
      (should (= 0 (plist-get counts :pending)))
      (should (= 1 (plist-get counts :disabled))))))

(ert-deftest elinit-test-health-counts-skips-targets ()
  "Health counts do not count target entries."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--mask-override (make-hash-table :test 'equal))
        (elinit--invalid (make-hash-table :test 'equal))
        (elinit--entry-state (make-hash-table :test 'equal)))
    (let ((counts (elinit--health-counts
                   nil
                   '((nil :id "basic.target" :type target)
                     ("sleep 300" :id "svc" :type simple)))))
      ;; Only svc should be counted (as pending since not running)
      (should (= 1 (plist-get counts :pending)))
      (should (= 0 (plist-get counts :running))))))

(ert-deftest elinit-test-manual-start-rejects-target ()
  "Manual start of a target returns error."
  (elinit-test-with-unit-files
      '((nil :id "basic.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit-default-target-link "basic.target")
          (elinit--default-target-link-override nil)
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--remain-active (make-hash-table :test 'equal)))
      (let ((result (elinit--manual-start "basic.target")))
        (should (eq 'error (plist-get result :status)))
        (should (string-match "target" (plist-get result :reason)))))))

(ert-deftest elinit-test-manual-stop-rejects-target ()
  "Manual stop of a target returns error."
  (elinit-test-with-unit-files
      '((nil :id "basic.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit-default-target-link "basic.target")
          (elinit--default-target-link-override nil)
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--remain-active (make-hash-table :test 'equal)))
      (let ((result (elinit--manual-stop "basic.target")))
        (should (eq 'error (plist-get result :status)))
        (should (string-match "target" (plist-get result :reason)))))))

(ert-deftest elinit-test-policy-set-restart-rejects-target ()
  "Restart policy not applicable to target entries."
  (elinit-test-with-unit-files
      '((nil :id "basic.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit-default-target-link "basic.target")
          (elinit--default-target-link-override nil)
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--restart-override (make-hash-table :test 'equal))
          (elinit--logging (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--overrides-loaded-p t))
      (let ((result (elinit--policy-set-restart "basic.target" 'always)))
        (should (eq 'error (plist-get result :status)))
        (should (string-match "target" (plist-get result :message)))))))

(ert-deftest elinit-test-policy-set-logging-rejects-target ()
  "Logging not applicable to target entries."
  (elinit-test-with-unit-files
      '((nil :id "basic.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit-default-target-link "basic.target")
          (elinit--default-target-link-override nil)
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--restart-override (make-hash-table :test 'equal))
          (elinit--logging (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--overrides-loaded-p t))
      (let ((result (elinit--policy-set-logging "basic.target" t)))
        (should (eq 'error (plist-get result :status)))
        (should (string-match "target" (plist-get result :message)))))))

;;; Log Integration Tests

(ert-deftest elinit-test-log-format-service-round-trip ()
  "Entry->service->entry preserves :log-format."
  (let ((elinit-log-format-binary-enable t))
    (let* ((entry (elinit--parse-entry
                   '("sleep 300" :id "svc" :log-format binary)))
           (service (elinit-entry-to-service entry))
           (entry2 (elinit-service-to-entry service)))
      (should (eq (elinit-entry-log-format entry2) 'binary))
      (should (eq (elinit-service-log-format service) 'binary)))))

(ert-deftest elinit-test-log-format-oneshot-round-trip ()
  "Entry->service->entry round-trip preserves :log-format for oneshot."
  (let ((elinit-log-format-binary-enable t))
    (let* ((entry (elinit--parse-entry
                   '("sleep 1" :id "job" :type oneshot :log-format binary)))
           (service (elinit-entry-to-service entry))
           (entry2 (elinit-service-to-entry service)))
      (should (eq (elinit-entry-type entry2) 'oneshot))
      (should (eq (elinit-entry-log-format entry2) 'binary)))))

(ert-deftest elinit-test-log-format-unit-file-keyword ()
  ":log-format is in unit-file keyword allowlist."
  (should (memq :log-format elinit--unit-file-keywords)))

(ert-deftest elinit-test-manual-start-passes-log-format ()
  "Manual start threads :log-format through to start-process."
  (elinit-test-with-unit-files
      '(("echo hi" :id "svc1" :log-format text))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--failed (make-hash-table :test 'equal))
           (elinit--restart-times (make-hash-table :test 'equal))
           (elinit--oneshot-completed (make-hash-table :test 'equal))
           (elinit--manually-stopped (make-hash-table :test 'equal))
           (elinit--manually-started (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (elinit--mask-override (make-hash-table :test 'equal))
           (elinit--enabled-override (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal))
           (elinit--cycle-fallback-ids (make-hash-table :test 'equal))
           (elinit--computed-deps (make-hash-table :test 'equal))
           (elinit--logging (make-hash-table :test 'equal))
           (elinit--remain-active (make-hash-table :test 'equal))
           (elinit--spawn-failure-reason (make-hash-table :test 'equal))
           (captured-log-format :not-set))
      (cl-letf (((symbol-function 'elinit--start-process)
                 (lambda (_id _cmd _logging _type _restart
                          &optional _is-restart _wd _env _envf _rsec
                          _ufd _user _group _sout _serr _sandbox
                          log-format _limits-entry)
                   (setq captured-log-format log-format)
                   t)))
        (let ((result (elinit--manual-start "svc1")))
          (should (eq 'started (plist-get result :status)))
          (should (eq 'text captured-log-format)))))))

(ert-deftest elinit-test-reconcile-passes-log-format ()
  "Reconcile start path threads :log-format through to start-process."
  (elinit-test-with-unit-files
      '(("echo hi" :id "new-svc" :log-format text))
    (let ((elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--restart-override (make-hash-table :test 'equal))
          (elinit--logging (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (captured-log-format :not-set))
      (cl-letf (((symbol-function 'elinit--start-process)
                 (lambda (_id _cmd _logging _type _restart
                          &optional _is-restart _wd _env _envf _rsec
                          _ufd _user _group _sout _serr _sandbox
                          log-format _limits-entry)
                   (setq captured-log-format log-format)
                   t))
                ((symbol-function 'elinit--refresh-dashboard) #'ignore)
                ((symbol-function 'executable-find) (lambda (_) t)))
        (elinit--reconcile)
        (should (eq 'text captured-log-format))))))

(ert-deftest elinit-test-reload-unit-passes-log-format ()
  "Reload restart path threads :log-format through to start-process."
  (elinit-test-with-unit-files
      '(("sleep 999" :id "svc1" :log-format text))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--failed (make-hash-table :test 'equal))
           (elinit--restart-times (make-hash-table :test 'equal))
           (elinit--oneshot-completed (make-hash-table :test 'equal))
           (elinit--manually-stopped (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (elinit--mask-override (make-hash-table :test 'equal))
           (elinit--enabled-override (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal))
           (elinit--cycle-fallback-ids (make-hash-table :test 'equal))
           (elinit--computed-deps (make-hash-table :test 'equal))
           (elinit--logging (make-hash-table :test 'equal))
           (captured-log-format :not-set))
      ;; Simulate a running process
      (puthash "svc1" (start-process "test" nil "sleep" "999")
               elinit--processes)
      (unwind-protect
          (cl-letf (((symbol-function 'elinit--manual-stop)
                     (lambda (id)
                       (let ((p (gethash id elinit--processes)))
                         (when (and p (process-live-p p))
                           (delete-process p)))
                       (list :status 'stopped :reason nil)))
                    ((symbol-function 'elinit--start-process)
                     (lambda (_id _cmd _logging _type _restart
                              &optional _is-restart _wd _env _envf _rsec
                              _ufd _user _group _sout _serr _sandbox
                              log-format _limits-entry)
                       (setq captured-log-format log-format)
                       t)))
            (let ((result (elinit--reload-unit "svc1")))
              (should (equal "reloaded" (plist-get result :action)))
              (should (eq 'text captured-log-format))))
        ;; Cleanup
        (let ((p (gethash "svc1" elinit--processes)))
          (when (and p (process-live-p p))
            (delete-process p)))))))

(ert-deftest elinit-test-manual-stop-triggers-exit-frame ()
  "Sentinel emits exit frame when process is killed by manual-stop."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (elinit--stderr-writers (make-hash-table :test 'equal))
        (elinit--processes (make-hash-table :test 'equal))
        (elinit--shutting-down nil)
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--logging (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--oneshot-callbacks (make-hash-table :test 'equal))
        (elinit--crash-log (make-hash-table :test 'equal))
        (elinit--last-exit-info (make-hash-table :test 'equal))
        (elinit--spawn-failure-reason (make-hash-table :test 'equal))
        (exit-frames nil))
    ;; Mark as manually stopped (as manual-stop would before killing)
    (puthash "manual-stop-svc" t elinit--manually-stopped)
    (let ((fake-writer (start-process "ms-fw" nil "sleep" "300")))
      (unwind-protect
          (progn
            (puthash "manual-stop-svc" fake-writer elinit--writers)
            (let ((fake-proc (start-process "manual-stop-svc" nil "true")))
              (puthash "manual-stop-svc" fake-proc elinit--processes)
              ;; Wait for "true" to exit (simulates kill-process sentinel)
              (while (process-live-p fake-proc) (sit-for 0.05))
              (cl-letf (((symbol-function 'elinit--log-send-frame)
                         (lambda (_w event _stream _pid unit-id &rest _args)
                           (push (list event unit-id) exit-frames)))
                        ((symbol-function 'process-send-eof)
                         (lambda (_proc) nil))
                        ((symbol-function 'elinit--emit-event) #'ignore)
                        ((symbol-function 'elinit--maybe-refresh-dashboard) #'ignore)
                        ((symbol-function 'elinit--should-restart-p) (lambda (&rest _) nil))
                        ((symbol-function 'run-at-time) (lambda (&rest _) nil)))
                (let ((sentinel (elinit--make-process-sentinel
                                 "manual-stop-svc" '("true") t 'simple t)))
                  (funcall sentinel fake-proc "exited abnormally with code 15\n")))))
        (when (process-live-p fake-writer) (delete-process fake-writer))))
    ;; Exit frame (event=2) should have been sent
    (should exit-frames)
    (should (= 2 (car (car exit-frames))))
    (should (equal "manual-stop-svc" (cadr (car exit-frames))))))

(ert-deftest elinit-test-oneshot-completion-triggers-exit-frame ()
  "Sentinel emits exit frame when oneshot process completes normally."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (elinit--stderr-writers (make-hash-table :test 'equal))
        (elinit--processes (make-hash-table :test 'equal))
        (elinit--shutting-down nil)
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--logging (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--oneshot-callbacks (make-hash-table :test 'equal))
        (elinit--crash-log (make-hash-table :test 'equal))
        (elinit--last-exit-info (make-hash-table :test 'equal))
        (elinit--spawn-failure-reason (make-hash-table :test 'equal))
        (exit-frames nil))
    (let ((fake-writer (start-process "os-fw" nil "sleep" "300")))
      (unwind-protect
          (progn
            (puthash "oneshot-svc" fake-writer elinit--writers)
            (let ((fake-proc (start-process "oneshot-svc" nil "true")))
              (puthash "oneshot-svc" fake-proc elinit--processes)
              (while (process-live-p fake-proc) (sit-for 0.05))
              (cl-letf (((symbol-function 'elinit--log-send-frame)
                         (lambda (_w event _stream _pid unit-id &rest _args)
                           (push (list event unit-id) exit-frames)))
                        ((symbol-function 'process-send-eof)
                         (lambda (_proc) nil))
                        ((symbol-function 'elinit--emit-event) #'ignore)
                        ((symbol-function 'elinit--maybe-refresh-dashboard) #'ignore)
                        ((symbol-function 'elinit--should-restart-p) (lambda (&rest _) nil))
                        ((symbol-function 'run-at-time) (lambda (&rest _) nil)))
                ;; Oneshot sentinel: type=oneshot, restart=nil
                (let ((sentinel (elinit--make-process-sentinel
                                 "oneshot-svc" '("true") t 'oneshot nil)))
                  (funcall sentinel fake-proc "finished\n")))))
        (when (process-live-p fake-writer) (delete-process fake-writer))))
    ;; Exit frame (event=2) should have been sent
    (should exit-frames)
    (should (= 2 (car (car exit-frames))))
    (should (equal "oneshot-svc" (cadr (car exit-frames))))))

(ert-deftest elinit-test-start-process-succeeds-despite-writer-failure ()
  "Service starts without logging when writer spawn fails."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (elinit--processes (make-hash-table :test 'equal))
        (elinit--shutting-down nil)
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--logging (make-hash-table :test 'equal))
        (elinit--spawn-failure-reason (make-hash-table :test 'equal))
        (captured-filter nil)
        (fake-proc (start-process "fake-svc" nil "sleep" "300")))
    (unwind-protect
        (cl-letf (((symbol-function 'elinit--get-effective-logging)
                   (lambda (_id _default) t))
                  ((symbol-function 'elinit--ensure-log-directory) #'ignore)
                  ((symbol-function 'elinit--log-file)
                   (lambda (_id) "/tmp/test.log"))
                  ((symbol-function 'elinit--start-writer)
                   (lambda (_id _file &optional _log-format) nil))
                  ((symbol-function 'make-process)
                   (lambda (&rest args)
                     (setq captured-filter (plist-get args :filter))
                     fake-proc))
                  ((symbol-function 'elinit--make-process-sentinel)
                   (lambda (&rest _args) #'ignore))
                  ((symbol-function 'elinit--build-launch-command)
                   (lambda (_cmd &rest _args) (list "sleep" "300"))))
          (let ((proc (elinit--start-process
                       "svc-nolog" "sleep 300" t 'simple 'always)))
            ;; Service should still start
            (should proc)
            ;; No filter when writer is nil
            (should-not captured-filter)))
      (when (process-live-p fake-proc)
        (delete-process fake-proc)))))

(ert-deftest elinit-test-graceful-stop-writers-after-services ()
  "Graceful stop keeps writers alive until services actually exit."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (elinit--processes (make-hash-table :test 'equal))
        (elinit--shutting-down nil)
        (elinit--shutdown-complete-flag nil)
        (elinit--shutdown-remaining 0)
        (elinit--shutdown-callback nil)
        (elinit--shutdown-timer nil)
        (elinit--timers nil)
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--last-exit-info (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--restart-times (make-hash-table :test 'equal))
        (writers-stopped-at nil))
    ;; Create real processes for service and writer
    (let ((svc (start-process "svc-gs" nil "sleep" "300"))
          (writer (start-process "logd-gs" nil "sleep" "300")))
      (puthash "svc-gs" svc elinit--processes)
      (puthash "svc-gs" writer elinit--writers)
      (unwind-protect
          (cl-letf (((symbol-function 'elinit--kill-signal-for-id)
                     (lambda (_id) 'SIGTERM))
                    ((symbol-function 'elinit--run-exec-stop-for-id) #'ignore)
                    ((symbol-function 'elinit--dag-cleanup) #'ignore)
                    ((symbol-function 'elinit--emit-event) #'ignore)
                    ((symbol-function 'elinit-timer-scheduler-stop) #'ignore)
                    ;; Mock signal-process so processes stay alive
                    ;; (on some Emacs versions SIGTERM kills synchronously)
                    ((symbol-function 'signal-process)
                     (lambda (_proc _sig) t))
                    ((symbol-function 'elinit--stop-all-writers)
                     (lambda ()
                       (setq writers-stopped-at
                             (hash-table-count elinit--processes))
                       (clrhash elinit--writers)))
                    ((symbol-function 'run-at-time)
                     (let ((real-run-at-time
                            (symbol-function 'run-at-time)))
                       (lambda (secs repeat fn &rest args)
                         (if (symbolp fn)
                             (apply real-run-at-time secs repeat fn args)
                           nil)))))
            ;; Call elinit-stop — writers should NOT be stopped eagerly
            (elinit-stop)
            ;; Writers should not have been stopped yet (services still live)
            ;; They will be stopped when handle-shutdown-exit completes
            (should-not writers-stopped-at))
        (when (process-live-p svc) (delete-process svc))
        (when (process-live-p writer) (delete-process writer))))))

(ert-deftest elinit-test-writer-cleaned-up-on-service-spawn-failure ()
  "Writer is stopped and removed when service make-process fails."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (elinit--processes (make-hash-table :test 'equal))
        (elinit--shutting-down nil)
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--logging (make-hash-table :test 'equal))
        (elinit--spawn-failure-reason (make-hash-table :test 'equal))
        (writer-stopped nil)
        (fake-writer (start-process "fake-writer" nil "sleep" "300"))
        (call-count 0))
    (unwind-protect
        (cl-letf (((symbol-function 'elinit--get-effective-logging)
                   (lambda (_id _default) t))
                  ((symbol-function 'elinit--ensure-log-directory) #'ignore)
                  ((symbol-function 'elinit--log-file)
                   (lambda (_id) "/tmp/test.log"))
                  ((symbol-function 'elinit--start-writer)
                   (lambda (id _file &optional _log-format)
                     (puthash id fake-writer elinit--writers)
                     fake-writer))
                  ((symbol-function 'elinit--stop-writer)
                   (lambda (id)
                     (setq writer-stopped id)
                     (remhash id elinit--writers)))
                  ((symbol-function 'make-process)
                   (lambda (&rest _args)
                     (cl-incf call-count)
                     ;; First call is from start-writer (mocked above),
                     ;; second call is the service spawn which fails
                     (error "Doing vfork: No such file or directory")))
                  ((symbol-function 'elinit--make-process-sentinel)
                   (lambda (&rest _args) #'ignore))
                  ((symbol-function 'elinit--build-launch-command)
                   (lambda (_cmd &rest _args) (list "/nonexistent/cmd")))
                  ((symbol-function 'elinit--log) #'ignore))
          (let ((proc (elinit--start-process
                       "svc-fail" "/nonexistent/cmd" t 'simple 'always)))
            ;; Service should return nil (spawn failed)
            (should-not proc)
            ;; Writer should have been cleaned up
            (should (equal writer-stopped "svc-fail"))
            (should (zerop (hash-table-count elinit--writers)))
            ;; Spawn failure reason should be recorded
            (should (gethash "svc-fail" elinit--spawn-failure-reason))))
      (when (process-live-p fake-writer)
        (delete-process fake-writer)))))

(ert-deftest elinit-test-filter-routes-to-writer ()
  "Process filter routes output to writer via process-send-string."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (elinit--processes (make-hash-table :test 'equal))
        (elinit--shutting-down nil)
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--logging (make-hash-table :test 'equal))
        (elinit--spawn-failure-reason (make-hash-table :test 'equal))
        (sent-data nil)
        (captured-filter nil)
        (fake-writer (start-process "fake-writer" nil "sleep" "300"))
        (fake-svc (start-process "fake-svc" nil "sleep" "300")))
    (unwind-protect
        (cl-letf (((symbol-function 'elinit--get-effective-logging)
                   (lambda (_id _default) t))
                  ((symbol-function 'elinit--ensure-log-directory) #'ignore)
                  ((symbol-function 'elinit--log-file)
                   (lambda (_id) "/tmp/test.log"))
                  ((symbol-function 'elinit--start-writer)
                   (lambda (id _file &optional _log-format)
                     (puthash id fake-writer elinit--writers)
                     fake-writer))
                  ((symbol-function 'make-process)
                   (lambda (&rest args)
                     (setq captured-filter (plist-get args :filter))
                     fake-svc))
                  ((symbol-function 'elinit--make-process-sentinel)
                   (lambda (&rest _args) #'ignore))
                  ((symbol-function 'elinit--build-launch-command)
                   (lambda (_cmd &rest _args) (list "sleep" "300")))
                  ((symbol-function 'process-send-string)
                   (lambda (_proc data)
                     (push data sent-data))))
          (let ((proc (elinit--start-process
                       "svc4" "sleep 300" t 'simple 'always)))
            (should proc)
            (should captured-filter)
            ;; Invoke the filter as Emacs would
            (funcall captured-filter proc "hello world\n")
            ;; Filter now sends framed transport data
            (should (= 1 (length sent-data)))
            (let ((frame (car sent-data)))
              ;; Frame should be a unibyte string
              (should (not (multibyte-string-p frame)))
              ;; Byte 4 = event (1=output)
              (should (= 1 (aref frame 4)))
              ;; Byte 5 = stream (1=stdout)
              (should (= 1 (aref frame 5)))
              ;; Payload is at end of frame, after unit-id
              (should (string-suffix-p "hello world\n" frame)))))
      (when (process-live-p fake-writer)
        (delete-process fake-writer))
      (when (process-live-p fake-svc)
        (delete-process fake-svc)))))

(provide 'elinit-test-core)
;;; elinit-test-core.el ends here
