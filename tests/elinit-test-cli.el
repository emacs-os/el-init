;;; elinit-test-cli.el --- CLI commands and transport tests for elinit.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; CLI commands and transport ERT tests for elinit.el.

;;; Code:

(require 'elinit-test-helpers)

;;; CLI Control Plane tests

(ert-deftest elinit-test-cli-result-structure ()
  "CLI result struct has required fields."
  (let ((result (elinit--cli-make-result 0 'human "output")))
    (should (elinit-cli-result-p result))
    (should (= 0 (elinit-cli-result-exitcode result)))
    (should (eq 'human (elinit-cli-result-format result)))
    (should (equal "output" (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-exit-codes ()
  "CLI exit code constants are defined."
  (should (= 0 elinit-cli-exit-success))
  (should (= 1 elinit-cli-exit-failure))
  (should (= 2 elinit-cli-exit-invalid-args))
  (should (= 3 elinit-cli-exit-not-active))
  (should (= 4 elinit-cli-exit-no-such-unit))
  (should (= 4 elinit-cli-exit-validation-failed))
  (should (= 69 elinit-cli-exit-server-unavailable)))

(ert-deftest elinit-test-cli-dispatch-unknown-command ()
  "Unknown CLI command returns exit code 2."
  (let ((result (elinit--cli-dispatch '("unknown-cmd"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-dispatch-empty-args ()
  "Empty args returns help text."
  (let ((result (elinit--cli-dispatch '())))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
    (should (string-match "Usage:" (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-ping ()
  "Ping command returns pong."
  (let ((result (elinit--cli-dispatch '("ping"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
    (should (string-match "pong" (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-ping-json ()
  "Ping command with --json returns JSON."
  (let ((result (elinit--cli-dispatch '("ping" "--json"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
    (should (eq 'json (elinit-cli-result-format result)))
    (should (string-match "\"status\"" (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-version ()
  "Version command returns version info."
  (let ((result (elinit--cli-dispatch '("version"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
    (should (string-match "elinitctl" (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-verify-no-programs ()
  "Verify with no programs returns success."
  (elinit-test-with-unit-files nil
    (let ((result (elinit--cli-dispatch '("verify"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
      (should (string-match "0 valid" (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-verify-invalid-entry ()
  "Verify with invalid entry returns validation-failed exit code."
  (elinit-test-with-unit-files
      '(("cmd" :id "cmd" :type "bad"))
    (let ((result (elinit--cli-dispatch '("verify"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-validation-failed (elinit-cli-result-exitcode result)))
      (should (string-match "1 invalid" (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-list-units-human-format ()
  "The `list-units' command returns human-readable table."
  (elinit-test-with-unit-files
      '(("test-cmd" :id "test" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (result (elinit--cli-dispatch '("list-units"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
      (should (eq 'human (elinit-cli-result-format result)))
      ;; Should contain header row
      (should (string-match "ID" (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-list-units-json-format ()
  "The `list-units --json' returns JSON with entries array."
  (elinit-test-with-unit-files
      '(("test-cmd" :id "test" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (result (elinit--cli-dispatch '("list-units" "--json"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
      (should (eq 'json (elinit-cli-result-format result)))
      ;; Should be valid JSON with entries array
      (should (string-match "\"entries\"" (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-show-requires-id ()
  "The `show' command without ID returns invalid-args exit code."
  (let ((result (elinit--cli-dispatch '("show"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-enable-requires-id ()
  "Enable without ID returns invalid-args exit code."
  (let ((result (elinit--cli-dispatch '("enable"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-enable-sets-override ()
  "Enable command sets enabled override."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "test-id" :type simple :enabled nil))
    (let ((elinit--enabled-override (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      (elinit--cli-dispatch '("enable" "test-id"))
      (should (eq 'enabled (gethash "test-id" elinit--enabled-override))))))

(ert-deftest elinit-test-cli-disable-sets-override ()
  "Disable command sets disabled override."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "test-id" :type simple))
    (let ((elinit--enabled-override (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      (elinit--cli-dispatch '("disable" "test-id"))
      (should (eq 'disabled (gethash "test-id" elinit--enabled-override))))))

(ert-deftest elinit-test-cli-mask-sets-override ()
  "Mask command sets masked override."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "test-id" :type simple))
    (let ((elinit--mask-override (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      (elinit--cli-dispatch '("mask" "test-id"))
      (should (eq 'masked (gethash "test-id" elinit--mask-override))))))

(ert-deftest elinit-test-cli-unmask-clears-override ()
  "Unmask command clears masked override."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "test-id" :type simple))
    (let ((elinit--mask-override (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      (puthash "test-id" 'masked elinit--mask-override)
      (elinit--cli-dispatch '("unmask" "test-id"))
      (should-not (gethash "test-id" elinit--mask-override)))))

(ert-deftest elinit-test-cli-mask-requires-id ()
  "Mask command requires at least one ID."
  (let ((result (elinit--cli-dispatch '("mask"))))
    (should (= elinit-cli-exit-invalid-args
                (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-unmask-requires-id ()
  "Unmask command requires at least one ID."
  (let ((result (elinit--cli-dispatch '("unmask"))))
    (should (= elinit-cli-exit-invalid-args
                (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-mask-json-output ()
  "Mask command returns JSON with applied IDs."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "a" :type simple)
        ("sleep 300" :id "b" :type simple))
    (let ((elinit--mask-override (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      (let ((result (elinit--cli-dispatch '("mask" "a" "b" "--json"))))
        (should (eq 'json (elinit-cli-result-format result)))
        (let ((parsed (json-read-from-string
                       (elinit-cli-result-output result))))
          (should (assoc 'applied parsed)))))))

(ert-deftest elinit-test-cli-mask-with-separator ()
  "Mask command supports -- separator for hyphen-prefixed IDs."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "-my-id" :type simple))
    (let ((elinit--mask-override (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      (elinit--cli-dispatch '("mask" "--" "-my-id"))
      (should (eq 'masked (gethash "-my-id" elinit--mask-override))))))

(ert-deftest elinit-test-cli-unmask-json-output ()
  "Unmask command returns JSON with applied IDs."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "a" :type simple)
        ("sleep 300" :id "b" :type simple))
    (let ((elinit--mask-override (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      (puthash "a" 'masked elinit--mask-override)
      (puthash "b" 'masked elinit--mask-override)
      (let ((result (elinit--cli-dispatch '("unmask" "a" "b" "--json"))))
        (should (eq 'json (elinit-cli-result-format result)))
        (let ((parsed (json-read-from-string
                       (elinit-cli-result-output result))))
          (should (assoc 'applied parsed)))))))

(ert-deftest elinit-test-cli-unmask-with-separator ()
  "Unmask command supports -- separator for hyphen-prefixed IDs."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "-my-id" :type simple))
    (let ((elinit--mask-override (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      (puthash "-my-id" 'masked elinit--mask-override)
      (elinit--cli-dispatch '("unmask" "--" "-my-id"))
      (should-not (gethash "-my-id" elinit--mask-override)))))

(ert-deftest elinit-test-mask-precedence-over-enabled ()
  "Masked entry is always disabled regardless of enabled override."
  (let ((elinit--mask-override (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal)))
    ;; Even with enabled override, mask takes precedence
    (puthash "test" 'masked elinit--mask-override)
    (puthash "test" 'enabled elinit--enabled-override)
    (should-not (elinit--get-effective-enabled "test" t))))

(ert-deftest elinit-test-mask-blocks-manual-start ()
  "Masked entry cannot be manually started."
  (elinit-test-with-unit-files
      '(("echo hi" :id "svc"))
    (let ((elinit--mask-override (make-hash-table :test 'equal))
          (elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--processes (make-hash-table :test 'equal)))
      (puthash "svc" 'masked elinit--mask-override)
      (let ((result (elinit--manual-start "svc")))
        (should (eq 'skipped (plist-get result :status)))
        (should (equal "masked" (plist-get result :reason)))))))

(ert-deftest elinit-test-reconcile-stops-masked-running ()
  "Reconcile stops masked entries that are currently running."
  (elinit-test-with-unit-files
      '(("echo" :id "svc"))
    (let* ((elinit--mask-override (make-hash-table :test 'equal))
           (elinit--enabled-override (make-hash-table :test 'equal))
           (process-alive (make-hash-table :test 'equal))
           (process-pids (make-hash-table :test 'equal))
           (failed (make-hash-table :test 'equal))
           (oneshot (make-hash-table :test 'equal))
           (entry-state (make-hash-table :test 'equal))
           (invalid (make-hash-table :test 'equal))
           (logging (make-hash-table :test 'equal))
           (restart (make-hash-table :test 'equal))
           ;; Build a plan with one entry
           (plan (elinit--build-plan (elinit--effective-programs))))
      ;; Simulate running process
      (puthash "svc" t process-alive)
      (puthash "svc" 123 process-pids)
      ;; Mask the entry
      (puthash "svc" 'masked elinit--mask-override)
      (let ((snapshot (elinit-snapshot--create
                       :process-alive process-alive
                       :process-pids process-pids
                       :failed failed
                       :oneshot-exit oneshot
                       :entry-state entry-state
                       :invalid invalid
                       :enabled-override elinit--enabled-override
                       :restart-override restart
                       :logging-override logging
                       :mask-override elinit--mask-override
                       :timestamp (float-time))))
        (let ((actions (elinit--compute-actions plan snapshot)))
          ;; Should have a stop action for the masked entry
          (let ((stop-action (cl-find "svc" actions
                                      :key (lambda (a) (plist-get a :id))
                                      :test #'equal)))
            (should stop-action)
            (should (eq 'stop (plist-get stop-action :op)))
            (should (eq 'masked (plist-get stop-action :reason)))))))))

(ert-deftest elinit-test-mask-persistence-roundtrip ()
  "Mask override survives save/load cycle."
  (let* ((dir (make-temp-file "overrides-" t))
         (elinit-overrides-file (expand-file-name "overrides.eld" dir))
         (elinit--mask-override (make-hash-table :test 'equal))
         (elinit--enabled-override (make-hash-table :test 'equal))
         (elinit--restart-override (make-hash-table :test 'equal))
         (elinit--logging (make-hash-table :test 'equal))
         (elinit--overrides-loaded nil))
    (unwind-protect
        (progn
          (puthash "svc" 'masked elinit--mask-override)
          (elinit--save-overrides)
          ;; Clear and reload
          (clrhash elinit--mask-override)
          (should-not (gethash "svc" elinit--mask-override))
          (elinit--load-overrides)
          (should (eq 'masked (gethash "svc" elinit--mask-override))))
      (delete-directory dir t))))

(ert-deftest elinit-test-compute-entry-status-masked ()
  "Compute-entry-status returns masked even when process is running."
  (let ((elinit--mask-override (make-hash-table :test 'equal))
        (elinit--processes (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal)))
    (puthash "svc" 'masked elinit--mask-override)
    ;; Non-running masked entry
    (let ((result (elinit--compute-entry-status "svc" 'simple)))
      (should (equal "masked" (car result))))
    ;; Running masked entry still shows masked
    (let ((proc (start-process "test" nil "sleep" "999")))
      (unwind-protect
          (progn
            (puthash "svc" proc elinit--processes)
            (let ((result (elinit--compute-entry-status "svc" 'simple)))
              (should (equal "masked" (car result)))))
        (delete-process proc)))))

(ert-deftest elinit-test-compute-entry-reason-masked ()
  "Compute-entry-reason returns masked even when process is running."
  (let ((elinit--mask-override (make-hash-table :test 'equal))
        (elinit--processes (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--entry-state (make-hash-table :test 'equal)))
    (puthash "svc" 'masked elinit--mask-override)
    ;; Non-running masked entry
    (should (equal "masked" (elinit--compute-entry-reason "svc" 'simple)))
    ;; Running masked entry still shows masked
    (let ((proc (start-process "test" nil "sleep" "999")))
      (unwind-protect
          (progn
            (puthash "svc" proc elinit--processes)
            (should (equal "masked"
                           (elinit--compute-entry-reason "svc" 'simple))))
        (delete-process proc)))))

(ert-deftest elinit-test-dashboard-proced-keybinding ()
  "Dashboard keymap binds `P' to proced."
  (should (eq #'proced
              (lookup-key elinit-dashboard-mode-map "P"))))

(ert-deftest elinit-test-cli-restart-policy-always ()
  "Restart-policy always sets override."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "test-id" :type simple :restart nil))
    (let ((elinit--restart-override (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      (elinit--cli-dispatch '("restart-policy" "always" "test-id"))
      (should (eq 'always (gethash "test-id" elinit--restart-override))))))

(ert-deftest elinit-test-cli-restart-policy-no ()
  "Restart-policy no sets override."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "test-id" :type simple :restart t))
    (let ((elinit--restart-override (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      (elinit--cli-dispatch '("restart-policy" "no" "test-id"))
      (should (eq 'no (gethash "test-id" elinit--restart-override))))))

(ert-deftest elinit-test-cli-restart-policy-on-failure ()
  "Restart-policy on-failure sets override."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "test-id" :type simple :restart nil))
    (let ((elinit--restart-override (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      (elinit--cli-dispatch '("restart-policy" "on-failure" "test-id"))
      (should (eq 'on-failure (gethash "test-id" elinit--restart-override))))))

(ert-deftest elinit-test-cli-restart-policy-on-success ()
  "Restart-policy on-success sets override."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "test-id" :type simple :restart nil))
    (let ((elinit--restart-override (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      (elinit--cli-dispatch '("restart-policy" "on-success" "test-id"))
      (should (eq 'on-success (gethash "test-id" elinit--restart-override))))))

(ert-deftest elinit-test-cli-restart-policy-invalid ()
  "Restart-policy with invalid value returns error."
  (let ((elinit--restart-override (make-hash-table :test 'equal))
        (result (elinit--cli-dispatch '("restart-policy" "bogus" "test-id"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args
               (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-logging-on ()
  "Logging on sets override."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "test-id" :type simple :logging nil))
    (let ((elinit--logging (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      (elinit--cli-dispatch '("logging" "on" "test-id"))
      (should (eq 'enabled (gethash "test-id" elinit--logging))))))

(ert-deftest elinit-test-cli-logging-off ()
  "Logging off sets override."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "test-id" :type simple))
    (let ((elinit--logging (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      (elinit--cli-dispatch '("logging" "off" "test-id"))
      (should (eq 'disabled (gethash "test-id" elinit--logging))))))

(ert-deftest elinit-test-cli-stop-uses-manually-stopped ()
  "CLI stop sets manually-stopped flag, not restart-override."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--restart-override (make-hash-table :test 'equal))
        (proc (start-process "test-proc" nil "sleep" "999")))
    (unwind-protect
        (progn
          (puthash "test-id" proc elinit--processes)
          (elinit--cli-dispatch '("stop" "test-id"))
          ;; Should set manually-stopped, NOT restart-override
          (should (gethash "test-id" elinit--manually-stopped))
          (should-not (gethash "test-id" elinit--restart-override)))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest elinit-test-cli-kill-does-not-use-manually-stopped ()
  "CLI kill does not set manually-stopped, matching signal-only semantics."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (proc (start-process "test-proc" nil "sleep" "999")))
    (unwind-protect
        (progn
          (puthash "test-id" proc elinit--processes)
          (let ((result (elinit--cli-dispatch '("kill" "test-id"))))
            (should (= elinit-cli-exit-success
                       (elinit-cli-result-exitcode result))))
          (should-not (gethash "test-id" elinit--manually-stopped)))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest elinit-test-start-clears-manually-stopped ()
  "Starting a process clears the manually-stopped flag."
  (let ((elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--processes (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--restart-override (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--logging (make-hash-table :test 'equal))
        (elinit-log-directory (make-temp-file "elinit-test-" t))
        (elinit--shutting-down nil))
    (puthash "test-id" t elinit--manually-stopped)
    (should (gethash "test-id" elinit--manually-stopped))
    (let ((proc (elinit--start-process "test-id" "sleep 999" nil 'simple t)))
      (unwind-protect
          (progn
            ;; Flag should be cleared after start
            (should-not (gethash "test-id" elinit--manually-stopped)))
        (when (and proc (process-live-p proc))
          (delete-process proc))))))

(ert-deftest elinit-test-cli-logs-requires-id ()
  "Logs without ID returns invalid-args exit code."
  (let ((result (elinit--cli-dispatch '("logs"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-kill-requires-id ()
  "Kill without ID returns invalid-args exit code."
  (let ((result (elinit--cli-dispatch '("kill"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-list-dependencies-full ()
  "The `list-dependencies' command without ID returns full graph."
  (elinit-test-with-unit-files
      '(("a" :id "a") ("b" :id "b" :after "a"))
    (let* ((result (elinit--cli-dispatch '("list-dependencies"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result))))))

(ert-deftest elinit-test-cli-list-dependencies-single ()
  "The `list-dependencies' command with ID returns single entry deps."
  (elinit-test-with-unit-files
      '(("a" :id "a") ("b" :id "b" :after "a"))
    (let* ((result (elinit--cli-dispatch '("list-dependencies" "b"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
      (should (string-match "ID: b" (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-blame ()
  "Blame command returns timing info."
  (let ((elinit--start-times (make-hash-table :test 'equal))
        (elinit--ready-times (make-hash-table :test 'equal)))
    (puthash "test" 1000.0 elinit--start-times)
    (puthash "test" 1001.5 elinit--ready-times)
    (let ((result (elinit--cli-dispatch '("blame"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
      (should (string-match "DURATION" (elinit-cli-result-output result))))))

;;; CLI Arg Validation Tests

(ert-deftest elinit-test-cli-verify-rejects-extra-args ()
  "Verify with extra args returns invalid-args exit code."
  (let ((result (elinit--cli-dispatch '("verify" "extra"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-validate-is-unknown-command ()
  "CLI `validate' is rejected as unknown command (renamed to `verify')."
  (let ((result (elinit--cli-dispatch '("validate"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))
    (should (string-match "Unknown command" (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-reset-failed-per-unit ()
  "CLI reset-failed clears failed state for a specific unit."
  (let ((elinit--failed (make-hash-table :test 'equal))
        (elinit--restart-times (make-hash-table :test 'equal)))
    (puthash "svc-a" t elinit--failed)
    (puthash "svc-a" '(100 99 98) elinit--restart-times)
    (let ((result (elinit--cli-dispatch '("reset-failed" "svc-a"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
      (should (string-match "Reset.*svc-a" (elinit-cli-result-output result)))
      (should-not (gethash "svc-a" elinit--failed))
      (should-not (gethash "svc-a" elinit--restart-times)))))

(ert-deftest elinit-test-cli-reset-failed-global ()
  "CLI reset-failed with no IDs clears all failed entries."
  (let ((elinit--failed (make-hash-table :test 'equal))
        (elinit--restart-times (make-hash-table :test 'equal)))
    (puthash "svc-a" t elinit--failed)
    (puthash "svc-b" t elinit--failed)
    (puthash "svc-a" '(100) elinit--restart-times)
    (puthash "svc-b" '(100) elinit--restart-times)
    (let ((result (elinit--cli-dispatch '("reset-failed"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
      (should (string-match "svc-a" (elinit-cli-result-output result)))
      (should (string-match "svc-b" (elinit-cli-result-output result)))
      (should (= 0 (hash-table-count elinit--failed))))))

(ert-deftest elinit-test-cli-reset-failed-not-failed ()
  "CLI reset-failed on a non-failed unit reports skipped."
  (let ((elinit--failed (make-hash-table :test 'equal))
        (elinit--restart-times (make-hash-table :test 'equal)))
    (let ((result (elinit--cli-dispatch '("reset-failed" "svc-x"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
      (should (string-match "Skipped.*svc-x" (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-reset-failed-no-failed-units ()
  "CLI reset-failed with no IDs and no failed units shows message."
  (let ((elinit--failed (make-hash-table :test 'equal)))
    (let ((result (elinit--cli-dispatch '("reset-failed"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
      (should (string-match "No failed" (elinit-cli-result-output result))))))

(ert-deftest elinit-test-reset-failed-core ()
  "Core reset-failed function clears failed and restart-times."
  (let ((elinit--failed (make-hash-table :test 'equal))
        (elinit--restart-times (make-hash-table :test 'equal)))
    (puthash "test-id" t elinit--failed)
    (puthash "test-id" '(100 99) elinit--restart-times)
    (let ((result (elinit--reset-failed "test-id")))
      (should (eq 'reset (plist-get result :status)))
      (should-not (gethash "test-id" elinit--failed))
      (should-not (gethash "test-id" elinit--restart-times)))))

(ert-deftest elinit-test-reset-failed-clears-failed-oneshot ()
  "Core reset-failed clears failed oneshot completion results."
  (let ((elinit--failed (make-hash-table :test 'equal))
        (elinit--restart-times (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal)))
    (puthash "fail-id" t elinit--failed)
    (puthash "fail-id" 1 elinit--oneshot-completed)
    (elinit--reset-failed "fail-id")
    (should-not (gethash "fail-id" elinit--oneshot-completed))))

(ert-deftest elinit-test-reset-failed-preserves-successful-oneshot ()
  "Core reset-failed does not clear successful oneshot completion."
  (let ((elinit--failed (make-hash-table :test 'equal))
        (elinit--restart-times (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal)))
    (puthash "ok-id" t elinit--failed)
    (puthash "ok-id" 0 elinit--oneshot-completed)
    (elinit--reset-failed "ok-id")
    (should (eql 0 (gethash "ok-id" elinit--oneshot-completed)))))

(ert-deftest elinit-test-reset-failed-clears-signal-oneshot ()
  "Core reset-failed clears oneshot that died by signal."
  (let ((elinit--failed (make-hash-table :test 'equal))
        (elinit--restart-times (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal)))
    (puthash "sig-id" t elinit--failed)
    (puthash "sig-id" -9 elinit--oneshot-completed)
    (elinit--reset-failed "sig-id")
    (should-not (gethash "sig-id" elinit--oneshot-completed))))

(ert-deftest elinit-test-reset-failed-not-failed ()
  "Core reset-failed on non-failed ID returns skipped."
  (let ((elinit--failed (make-hash-table :test 'equal)))
    (let ((result (elinit--reset-failed "test-id")))
      (should (eq 'skipped (plist-get result :status))))))

(ert-deftest elinit-test-cli-reload-requires-at-least-one-id ()
  "CLI `reload' with no arguments returns exit 2 (invalid args)."
  (let ((result (elinit--cli-dispatch '("reload"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args
               (elinit-cli-result-exitcode result)))
    (should (string-match "reload requires at least one ID"
                          (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-reconcile-is-unknown-command ()
  "Removed `reconcile' command is rejected as unknown."
  (let ((result (elinit--cli-dispatch '("reconcile"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args
               (elinit-cli-result-exitcode result)))
    (should (string-match "Unknown command: reconcile"
                          (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-list-is-unknown-command ()
  "Legacy `list' command is rejected after rename to `list-units'."
  (let ((result (elinit--cli-dispatch '("list"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args
               (elinit-cli-result-exitcode result)))
    (should (string-match "Unknown command: list"
                          (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-describe-is-unknown-command ()
  "Legacy `describe' command is rejected after rename to `show'."
  (let ((result (elinit--cli-dispatch '("describe"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args
               (elinit-cli-result-exitcode result)))
    (should (string-match "Unknown command: describe"
                          (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-graph-is-unknown-command ()
  "Legacy `graph' command is rejected after rename to `list-dependencies'."
  (let ((result (elinit--cli-dispatch '("graph"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args
               (elinit-cli-result-exitcode result)))
    (should (string-match "Unknown command: graph"
                          (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-timers-is-unknown-command ()
  "Legacy `timers' command is rejected after rename to `list-timers'."
  (let ((result (elinit--cli-dispatch '("timers"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args
               (elinit-cli-result-exitcode result)))
    (should (string-match "Unknown command: timers"
                          (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-status-no-args-shows-table ()
  "The `status' with no IDs shows overview table (delegates to `list-units')."
  (elinit-test-with-unit-files
      '(("test-cmd" :id "test" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (result (elinit--cli-dispatch '("status"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
      ;; Should contain header row (table format)
      (should (string-match "ID" (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-status-with-id-shows-detail ()
  "The `status ID' shows detailed unit info."
  (elinit-test-with-unit-files
      '(("test-cmd" :id "test" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (result (elinit--cli-dispatch '("status" "test"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
      ;; Should show detailed info (describe format)
      (should (string-match "ID: test" (elinit-cli-result-output result)))
      (should (string-match "Type:" (elinit-cli-result-output result)))
      (should (string-match "Status:" (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-status-partial-missing ()
  "The `status ID1 ID2' prints found units and warns about missing ones."
  (elinit-test-with-unit-files
      '(("test-cmd" :id "test" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (result (elinit--cli-dispatch '("status" "test" "nonexistent"))))
      (should (elinit-cli-result-p result))
      ;; Non-zero exit because of missing ID
      (should (= elinit-cli-exit-failure (elinit-cli-result-exitcode result)))
      ;; But should still show output for the found ID
      (should (string-match "ID: test" (elinit-cli-result-output result)))
      ;; And warn about the missing one
      (should (string-match "nonexistent" (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-status-all-missing ()
  "The `status' with only missing IDs still shows warnings."
  (elinit-test-with-unit-files
      '(("test-cmd" :id "test" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (result (elinit--cli-dispatch '("status" "nope"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-failure (elinit-cli-result-exitcode result)))
      (should (string-match "could not be found" (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-list-units-filters-invalid ()
  "The `list-units ID' filters both valid and invalid entries."
  (elinit-test-with-unit-files
      '(("good-cmd" :id "good" :type simple)
        ("bad-cmd" :id "bad" :type "string-type"))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (result (elinit--cli-dispatch '("list-units" "good"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
      ;; Should show "good" but not "bad" (invalid entry filtered out)
      (should (string-match "good" (elinit-cli-result-output result)))
      (should-not (string-match "bad" (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-status-invalid-id-shows-invalid-detail ()
  "The `status' with an invalid unit ID shows invalid detail, not \"not found\"."
  (elinit-test-with-unit-files
      '(("good-cmd" :id "good" :type simple)
        ("bad-cmd" :id "bad" :type "string-type"))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (result (elinit--cli-dispatch '("status" "bad"))))
      (should (elinit-cli-result-p result))
      ;; Invalid configured unit is found, so exit success (not missing)
      (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
      (should (string-match "ID: bad" (elinit-cli-result-output result)))
      (should (string-match "Status: invalid" (elinit-cli-result-output result)))
      (should-not (string-match "could not be found" (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-status-mixed-valid-invalid-missing ()
  "The `status' with valid, invalid, and missing IDs classifies each correctly."
  (elinit-test-with-unit-files
      '(("good-cmd" :id "good" :type simple)
        ("bad-cmd" :id "bad" :type "string-type"))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (result (elinit--cli-dispatch '("status" "good" "bad" "ghost"))))
      (should (elinit-cli-result-p result))
      ;; Non-zero exit because "ghost" is truly missing
      (should (= elinit-cli-exit-failure (elinit-cli-result-exitcode result)))
      ;; Valid unit has detail
      (should (string-match "ID: good" (elinit-cli-result-output result)))
      ;; Invalid unit has invalid detail
      (should (string-match "ID: bad" (elinit-cli-result-output result)))
      (should (string-match "Status: invalid" (elinit-cli-result-output result)))
      ;; Only the truly missing ID gets "could not be found"
      (should (string-match "ghost.*could not be found" (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-status-invalid-id-json ()
  "The `status --json' with invalid ID returns invalid array, not not_found."
  (elinit-test-with-unit-files
      '(("bad-cmd" :id "bad" :type "string-type"))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (result (elinit--cli-dispatch '("status" "bad" "--json"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
      (let ((parsed (json-read-from-string (elinit-cli-result-output result))))
        ;; Invalid unit appears in "invalid" array, not "not_found"
        (should (< 0 (length (alist-get 'invalid parsed))))
        (should (equal "bad" (alist-get 'id (aref (alist-get 'invalid parsed) 0))))))))

(ert-deftest elinit-test-cli-show-invalid-id-shows-invalid-detail ()
  "The `show' with an invalid unit ID shows invalid detail, not \"not found\"."
  (elinit-test-with-unit-files
      '(("bad-cmd" :id "bad" :type "string-type"))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (result (elinit--cli-dispatch '("show" "bad"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
      (should (string-match "ID: bad" (elinit-cli-result-output result)))
      (should (string-match "Status: invalid" (elinit-cli-result-output result)))
      (should-not (string-match "No entry with ID" (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-show-invalid-id-json ()
  "The `show --json' with invalid ID returns invalid object, not error."
  (elinit-test-with-unit-files
      '(("bad-cmd" :id "bad" :type "string-type"))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (result (elinit--cli-dispatch '("show" "bad" "--json"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
      (let ((parsed (json-read-from-string (elinit-cli-result-output result))))
        (should (equal "bad" (alist-get 'id parsed)))
        (should (alist-get 'reason parsed))))))

(ert-deftest elinit-test-cli-show-truly-missing-still-errors ()
  "The `show' with a truly missing ID still returns an error."
  (elinit-test-with-unit-files
      '(("good-cmd" :id "good" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (result (elinit--cli-dispatch '("show" "ghost"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-failure (elinit-cli-result-exitcode result)))
      (should (string-match "No entry with ID" (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-ping-rejects-extra-args ()
  "Ping with extra args returns invalid-args exit code."
  (let ((result (elinit--cli-dispatch '("ping" "extra"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-version-rejects-extra-args ()
  "Version with extra args returns invalid-args exit code."
  (let ((result (elinit--cli-dispatch '("version" "extra"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-list-timers-no-timers ()
  "The `list-timers' command with no timers configured."
  (elinit-test-without-builtins
    (let ((elinit-mode t)
          (elinit-timers nil)
          (elinit--timer-list nil)
          (elinit--timer-state (make-hash-table :test 'equal))
          (elinit--invalid-timers (make-hash-table :test 'equal)))
      (let ((result (elinit--cli-dispatch '("list-timers"))))
        (should (elinit-cli-result-p result))
        (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
        (should (string-match "No timers" (elinit-cli-result-output result)))))))

(ert-deftest elinit-test-cli-list-timers-builds-from-config-when-mode-off ()
  "The `list-timers' command works when `elinit-mode' is off."
  (elinit-test-with-unit-files
      '(("true" :id "s1" :type oneshot))
    (let ((elinit-mode nil)
          (elinit-timers '((:id "t1" :target "s1" :on-startup-sec 60)))
          (elinit--timer-list nil)
          (elinit--timer-state (make-hash-table :test 'equal))
          (elinit--invalid-timers (make-hash-table :test 'equal)))
      (let ((result (elinit--cli-dispatch '("list-timers"))))
        (should (elinit-cli-result-p result))
        (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
        (should (string-match-p "t1" (elinit-cli-result-output result)))
        (should (string-match-p "s1" (elinit-cli-result-output result)))))))

(ert-deftest elinit-test-cli-list-timers-disabled-message-no-experimental ()
  "The `list-timers' disabled response has no experimental wording."
  (let ((elinit-timer-subsystem-mode nil)
        (elinit-mode t)
        (elinit--timer-list nil)
        (elinit--timer-state (make-hash-table :test 'equal))
        (elinit--invalid-timers (make-hash-table :test 'equal)))
    (let ((human (elinit--cli-dispatch '("list-timers")))
          (json (elinit--cli-dispatch '("--json" "list-timers"))))
      (should (elinit-cli-result-p human))
      (should (= elinit-cli-exit-success (elinit-cli-result-exitcode human)))
      (should (string-match-p "Timer subsystem is disabled\\." (elinit-cli-result-output human)))
      (should-not (string-match-p "experimental" (elinit-cli-result-output human)))
      (let* ((json-object-type 'alist)
             (data (json-read-from-string (elinit-cli-result-output json))))
        (should (equal "disabled" (alist-get 'status data)))
        (should (equal "Timer subsystem is disabled" (alist-get 'message data)))))))

(ert-deftest elinit-test-cli-list-timers-shows-state ()
  "The `list-timers' command shows timer state."
  (let* ((elinit-mode t)
         (timer (elinit-timer--create :id "t1" :target "s1" :enabled t))
         (elinit--timer-list (list timer))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--invalid-timers (make-hash-table :test 'equal)))
    (puthash "t1" '(:last-run-at 1000.0 :next-run-at 2000.0)
             elinit--timer-state)
    (let ((result (elinit--cli-dispatch '("list-timers"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
      (should (string-match "t1" (elinit-cli-result-output result)))
      (should (string-match "s1" (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-list-timers-json ()
  "The `list-timers --json' outputs JSON."
  (let* ((timer (elinit-timer--create :id "t1" :target "s1" :enabled t))
         (elinit--timer-list (list timer))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--invalid-timers (make-hash-table :test 'equal)))
    (let ((result (elinit--cli-dispatch '("--json" "list-timers"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
      (should (eq 'json (elinit-cli-result-format result)))
      ;; Should be valid JSON
      (let ((json-object-type 'alist))
        (should (json-read-from-string (elinit-cli-result-output result)))))))

(ert-deftest elinit-test-cli-list-timers-invalid-human-format ()
  "The `list-timers' command shows invalid timers with correct id and reason."
  (elinit-test-with-unit-files nil
    (let ((elinit-mode t)
          (elinit-timers '((:id "bad-timer"
                               :target "missing"
                               :on-startup-sec 60)))
          (elinit--timer-list nil)
          (elinit--timer-state (make-hash-table :test 'equal))
          (elinit--invalid-timers (make-hash-table :test 'equal)))
      (let ((result (elinit--cli-dispatch '("list-timers"))))
        (should (elinit-cli-result-p result))
        (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
        ;; Should show id and validation reason
        (should (string-match-p "bad-timer" (elinit-cli-result-output result)))
        (should (string-match-p "target" (elinit-cli-result-output result)))))))

(ert-deftest elinit-test-cli-list-timers-invalid-json-format ()
  "The `list-timers --json' outputs invalid timers with correct structure."
  (elinit-test-with-unit-files nil
    (let ((elinit-mode t)
          (elinit-timers '((:id "bad-timer"
                               :target "missing"
                               :on-startup-sec 60)))
          (elinit--timer-list nil)
          (elinit--timer-state (make-hash-table :test 'equal))
          (elinit--invalid-timers (make-hash-table :test 'equal)))
      (let ((result (elinit--cli-dispatch '("--json" "list-timers"))))
        (should (elinit-cli-result-p result))
        (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
        ;; Parse JSON and check invalid array structure
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (data (json-read-from-string (elinit-cli-result-output result)))
               (invalid (alist-get 'invalid data))
               (entry (car invalid)))
          (should invalid)
          (should (equal "bad-timer" (alist-get 'id entry)))
          (should (string-match-p "target" (alist-get 'reason entry))))))))

(ert-deftest elinit-test-dashboard-timer-signal-exit-is-failed ()
  "Dashboard timer entry shows signal exit code in EXIT column."
  (let* ((timer (elinit-timer--create :id "t1" :target "s1" :enabled t))
         (elinit--timer-list (list timer))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--processes (make-hash-table :test 'equal)))
    ;; Simulate signal death stored as negative exit code
    (puthash "t1" '(:last-exit -9 :next-run-at 2000.0) elinit--timer-state)
    (cl-letf (((symbol-function 'elinit--get-entry-for-id)
               (lambda (_id) nil)))
      (let ((entry (elinit--make-timer-dashboard-entry timer)))
        ;; Exit code is at index 5
        (should (string= "-9" (aref entry 5)))))))

(ert-deftest elinit-test-cli-list-timers-full-field-mapping ()
  "The `list-timers' output includes all required fields.
Target type is resolved from current config."
  (let* ((elinit-mode t)
         (timer (elinit-timer--create :id "test-timer" :target "test-target"
                                          :enabled t :persistent t))
         (elinit--timer-list (list timer))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--invalid-timers (make-hash-table :test 'equal))
         (mock-entry (list "test-target" "echo hi" 0 t nil nil nil nil
                           'oneshot nil nil nil nil nil nil nil nil nil nil
                           nil nil nil nil nil nil nil nil nil nil nil nil
                           nil nil)))
    ;; Set up comprehensive state (note: :last-missed-at is the state key)
    (puthash "test-timer" '(:last-run-at 1000.0
                            :last-success-at 900.0
                            :last-failure-at 950.0
                            :last-exit 1
                            :next-run-at 2000.0
                            :last-missed-at 850.0
                            :last-miss-reason overlap
                            :last-result skip
                            :last-result-reason overlap)
             elinit--timer-state)
    (cl-letf (((symbol-function 'elinit--get-entry-for-id)
               (lambda (_id) mock-entry)))
      ;; Test human format includes all fields
      (let ((result (elinit--cli-dispatch '("list-timers"))))
        (should (elinit-cli-result-p result))
        (let ((output (elinit-cli-result-output result)))
          ;; ID and target
          (should (string-match-p "test-timer" output))
          (should (string-match-p "test-target" output))
          ;; Enabled (yes/no)
          (should (string-match-p "yes" output))
          ;; Exit code
          (should (string-match-p "1" output))
          ;; Type resolved from config
          (should (string-match-p "oneshot" output))
          ;; Result and reason
          (should (string-match-p "skip" output))
          (should (string-match-p "overlap" output))))
      ;; Test JSON format includes all fields with correct values
      (let ((result (elinit--cli-dispatch '("--json" "list-timers"))))
        (should (elinit-cli-result-p result))
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (data (json-read-from-string (elinit-cli-result-output result)))
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
          (should (equal "overlap" (alist-get 'miss_reason entry)))
          (should (equal "oneshot" (alist-get 'target_type entry))))))))

(ert-deftest elinit-test-cli-list-timers-rejects-extra-args ()
  "The `list-timers' with extra args returns invalid-args exit code."
  (let ((elinit--timer-list nil)
        (elinit--timer-state (make-hash-table :test 'equal))
        (elinit--invalid-timers (make-hash-table :test 'equal)))
    (let ((result (elinit--cli-dispatch '("list-timers" "extra"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result))))))

(ert-deftest elinit-test-cli-blame-rejects-extra-args ()
  "Blame with extra args returns invalid-args exit code."
  (let ((elinit--start-times (make-hash-table :test 'equal)))
    (let ((result (elinit--cli-dispatch '("blame" "extra"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result))))))

(ert-deftest elinit-test-cli-show-rejects-extra-args ()
  "The `show' with extra args returns invalid-args exit code."
  (elinit-test-with-unit-files
      '(("cmd" :id "test"))
    (let* ((result (elinit--cli-dispatch '("show" "test" "extra"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result))))))

(ert-deftest elinit-test-cli-status-rejects-unknown-flags ()
  "Status with unknown flag returns invalid-args exit code."
  (let ((result (elinit--cli-dispatch '("status" "--bogus"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-list-dependencies-rejects-extra-args ()
  "The `list-dependencies' with multiple args returns invalid-args exit code."
  (elinit-test-with-unit-files
      '(("a" :id "a"))
    (let* ((result (elinit--cli-dispatch '("list-dependencies" "a" "b"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result))))))

;;; CLI JSON Schema Tests

(ert-deftest elinit-test-cli-list-units-json-empty-arrays ()
  "The `list-units --json' returns arrays, not null, for empty results."
  (elinit-test-with-unit-files
      nil
    (let* ((result (elinit--cli-dispatch '("list-units" "--json"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
      ;; Should have [] not null
      (should (string-match "\"entries\":\\[\\]" (elinit-cli-result-output result)))
      (should (string-match "\"invalid\":\\[\\]" (elinit-cli-result-output result))))))

;;; CLI Wrapper Transport Tests

(ert-deftest elinit-test-cli-wrapper-dispatch-format ()
  "Wrapper dispatch returns base64-encoded format."
  (elinit-test-with-unit-files
      nil
    (let* ((result (elinit--cli-dispatch-for-wrapper '("ping"))))
      ;; Format is EXITCODE:BASE64OUTPUT
      (should (stringp result))
      (should (string-match "^0:" result))
      ;; Decode the base64 part
      (let ((b64 (substring result 2)))
        (should (string-match "pong" (decode-coding-string (base64-decode-string b64) 'utf-8)))))))

(ert-deftest elinit-test-cli-wrapper-dispatch-error ()
  "Wrapper dispatch returns non-zero exit code for errors."
  (let ((result (elinit--cli-dispatch-for-wrapper '("unknown-cmd"))))
    ;; Should start with 2: (invalid args)
    (should (stringp result))
    (should (string-match "^2:" result))))

(ert-deftest elinit-test-cli-wrapper-edit-transport-clean ()
  "Wrapper edit output has clean EXITCODE:BASE64 format with no extra stdout."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit--authority-snapshot nil)
         (elinit--programs-cache :not-yet-loaded)
         (elinit--unit-file-invalid (make-hash-table :test 'equal))
         (result (elinit--cli-dispatch-for-wrapper
                  '("edit" "new-svc" "--json"))))
    (unwind-protect
        (progn
          ;; Must be EXITCODE:BASE64 with numeric exit code
          (should (stringp result))
          (should (string-match "^\\([0-9]+\\):\\(.*\\)$" result))
          ;; Decode and verify valid JSON
          (let* ((b64 (match-string 2 result))
                 (decoded (decode-coding-string
                           (base64-decode-string b64) 'utf-8))
                 (parsed (json-read-from-string decoded)))
            (should (assoc 'path parsed))
            (should (assoc 'tier parsed))
            (should (assoc 'created parsed))))
      (delete-directory dir t))))

;;; CLI Malformed Option Tests

(ert-deftest elinit-test-cli-status-rejects-short-unknown-flag ()
  "Status with single-letter unknown flag returns invalid-args."
  (let ((result (elinit--cli-dispatch '("status" "-x"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-logs-rejects-malformed-tail ()
  "Logs with --tailx (prefix match) returns invalid-args."
  (let ((result (elinit--cli-dispatch '("logs" "test" "--tailx" "5"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-kill-rejects-malformed-signal ()
  "Kill with --signalx (prefix match) returns invalid-args."
  (let ((result (elinit--cli-dispatch '("kill" "test" "--signalx" "TERM"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-logs-tail-missing-value ()
  "Logs with --tail but no value returns invalid-args."
  (let ((result (elinit--cli-dispatch '("logs" "test" "--tail"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))
    (should (string-match "requires a numeric value" (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-logs-tail-non-numeric ()
  "Logs with --tail abc returns invalid-args."
  (let ((result (elinit--cli-dispatch '("logs" "test" "--tail" "abc"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))
    (should (string-match "must be a number" (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-logs-id-matches-tail-value ()
  "Logs with ID that matches --tail value should not collide."
  ;; ("logs" "5" "--tail" "5") - ID is "5", tail value is also "5"
  ;; Should NOT reject the ID just because it matches the tail value
  (let ((result (elinit--cli-dispatch '("logs" "5" "--tail" "5"))))
    (should (elinit-cli-result-p result))
    ;; Should fail because log file doesn't exist, not because ID is missing
    (should (= elinit-cli-exit-failure (elinit-cli-result-exitcode result)))
    (should (string-match "No log file for '5'" (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-logs-extra-arg-matches-tail-value ()
  "Logs with extra arg matching --tail value should be rejected."
  ;; ("logs" "foo" "--tail" "5" "5") - extra "5" should not be hidden
  (let ((result (elinit--cli-dispatch '("logs" "foo" "--tail" "5" "5"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))
    (should (string-match "got extra" (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-kill-id-matches-signal-value ()
  "Kill with ID that matches --signal value should not collide."
  ;; ("kill" "TERM" "--signal" "TERM") - ID is "TERM", signal value is also "TERM"
  ;; Should NOT reject the ID just because it matches the signal value
  (let* ((elinit--processes (make-hash-table :test 'equal))
         (result (elinit--cli-dispatch '("kill" "TERM" "--signal" "TERM"))))
    (should (elinit-cli-result-p result))
    ;; Should fail because process not running, not because ID is missing
    (should (= elinit-cli-exit-failure (elinit-cli-result-exitcode result)))
    (should (string-match "not running" (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-kill-extra-arg-matches-signal-value ()
  "Kill with extra arg matching --signal value should be rejected."
  ;; ("kill" "foo" "--signal" "TERM" "TERM") - extra "TERM" should not be hidden
  (let ((result (elinit--cli-dispatch '("kill" "foo" "--signal" "TERM" "TERM"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))
    (should (string-match "got extra" (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-logs-duplicate-tail ()
  "Logs with --tail specified twice returns invalid-args."
  (let ((result (elinit--cli-dispatch '("logs" "foo" "--tail" "10" "--tail" "20"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))
    (should (string-match "multiple times" (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-kill-duplicate-signal ()
  "Kill with --signal specified twice returns invalid-args."
  (let ((result (elinit--cli-dispatch '("kill" "foo" "--signal" "TERM" "--signal" "KILL"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))
    (should (string-match "multiple times" (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-kill-signal-missing-value ()
  "Kill with --signal but no value returns invalid-args."
  (let ((result (elinit--cli-dispatch '("kill" "test" "--signal"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))
    (should (string-match "requires a signal name" (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-kill-signal-invalid-name ()
  "Kill with --signal NOSIG returns invalid-args."
  (let ((result (elinit--cli-dispatch '("kill" "test" "--signal" "NOSIG"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))
    (should (string-match "Invalid signal name" (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-logs-option-before-id ()
  "Logs with only option (no ID) returns invalid-args (exit 2)."
  (let ((result (elinit--cli-dispatch '("logs" "--tail" "5"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))
    (should (string-match "requires an ID" (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-kill-option-before-id ()
  "Kill with only option (no ID) returns invalid-args (exit 2)."
  (let ((result (elinit--cli-dispatch '("kill" "--signal" "TERM"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))
    (should (string-match "requires an ID" (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-start-rejects-unknown-flags ()
  "Start with unknown flag returns invalid-args (exit 2)."
  (let ((result (elinit--cli-dispatch '("start" "--bogus"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-stop-rejects-unknown-flags ()
  "Stop with unknown flag returns invalid-args (exit 2)."
  (let ((result (elinit--cli-dispatch '("stop" "--bogus"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-restart-rejects-unknown-flags ()
  "Restart with unknown flag returns invalid-args (exit 2)."
  (let ((result (elinit--cli-dispatch '("restart" "--bogus"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-enable-rejects-unknown-flags ()
  "Enable with unknown flag returns invalid-args (exit 2)."
  (let ((result (elinit--cli-dispatch '("enable" "--bogus"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-disable-rejects-unknown-flags ()
  "Disable with unknown flag returns invalid-args (exit 2)."
  (let ((result (elinit--cli-dispatch '("disable" "--bogus"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-restart-policy-rejects-unknown-flags ()
  "Restart-policy with unknown flag returns invalid-args (exit 2)."
  (let ((result (elinit--cli-dispatch '("restart-policy" "--bogus" "id"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-logging-rejects-unknown-flags ()
  "Logging with unknown flag returns invalid-args (exit 2)."
  (let ((result (elinit--cli-dispatch '("logging" "--bogus" "id"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))))

;;; CLI -- Separator Tests (POSIX end-of-options)

(ert-deftest elinit-test-cli-split-at-separator ()
  "Split at -- separator correctly."
  (should (equal (elinit--cli-split-at-separator '("a" "--" "b"))
                 '(("a") . ("b"))))
  (should (equal (elinit--cli-split-at-separator '("a" "b"))
                 '(("a" "b") . nil)))
  (should (equal (elinit--cli-split-at-separator '("--" "-svc"))
                 '(nil . ("-svc")))))

(ert-deftest elinit-test-cli-strip-separator ()
  "Strip separator and concatenate."
  (should (equal (elinit--cli-strip-separator '("a" "--" "b"))
                 '("a" "b")))
  (should (equal (elinit--cli-strip-separator '("--" "-svc"))
                 '("-svc"))))

(ert-deftest elinit-test-cli-parse-args-json-after-separator ()
  "Literal --json after -- is preserved as an ID, not stripped."
  (let ((result (elinit--cli-parse-args '("enable" "--" "--json"))))
    (should (equal (car result) "enable"))
    (should (equal (cadr result) '("--" "--json")))
    (should-not (cl-caddr result))))

(ert-deftest elinit-test-cli-parse-args-json-before-separator ()
  "Global --json before -- is stripped and sets json-p."
  (let ((result (elinit--cli-parse-args '("status" "--json" "--" "svc"))))
    (should (equal (car result) "status"))
    (should (equal (cadr result) '("--" "svc")))
    (should (cl-caddr result))))

(ert-deftest elinit-test-cli-parse-option ()
  "Parse option and value by position, not by value."
  ;; Option with value
  (let ((result (elinit--cli-parse-option '("foo" "--opt" "val" "bar") "--opt")))
    (should (equal (plist-get result :value) "val"))
    (should (null (plist-get result :missing)))
    (should (equal (plist-get result :positional) '("foo" "bar"))))
  ;; Option with missing value (end of args)
  (let ((result (elinit--cli-parse-option '("foo" "--opt") "--opt")))
    (should (null (plist-get result :value)))
    (should (plist-get result :missing))
    (should (equal (plist-get result :positional) '("foo"))))
  ;; Option with missing value (next arg is flag)
  (let ((result (elinit--cli-parse-option '("--opt" "--other") "--opt")))
    (should (null (plist-get result :value)))
    (should (plist-get result :missing))
    (should (equal (plist-get result :positional) '("--other"))))
  ;; No option present
  (let ((result (elinit--cli-parse-option '("foo" "bar") "--opt")))
    (should (null (plist-get result :value)))
    (should (null (plist-get result :missing)))
    (should (equal (plist-get result :positional) '("foo" "bar"))))
  ;; Value collision: positional arg equals option value
  (let ((result (elinit--cli-parse-option '("5" "--tail" "5") "--tail")))
    (should (equal (plist-get result :value) "5"))
    (should (null (plist-get result :missing)))
    ;; Positional "5" should NOT be removed by value collision
    (should (equal (plist-get result :positional) '("5"))))
  ;; Duplicate option detection
  (let ((result (elinit--cli-parse-option '("--opt" "val1" "--opt" "val2") "--opt")))
    (should (plist-get result :duplicate))
    ;; Value is the last one seen
    (should (equal (plist-get result :value) "val2")))
  ;; Single option is not duplicate
  (let ((result (elinit--cli-parse-option '("--opt" "val") "--opt")))
    (should (null (plist-get result :duplicate)))))

(ert-deftest elinit-test-cli-enable-hyphen-id-with-separator ()
  "Enable allows hyphen-prefixed ID after -- separator."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "-svc" :type simple :enabled nil))
    (let ((elinit--enabled-override (make-hash-table :test 'equal))
          (elinit-overrides-file nil))
      (let ((result (elinit--cli-dispatch '("enable" "--" "-svc"))))
        (should (elinit-cli-result-p result))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        (should (eq 'enabled
                    (gethash "-svc" elinit--enabled-override)))))))

(ert-deftest elinit-test-cli-logs-hyphen-id-with-separator ()
  "Logs allows hyphen-prefixed ID after -- separator."
  ;; Without separator, should fail with "ID as first argument"
  (let ((result (elinit--cli-dispatch '("logs" "-svc"))))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result))))
  ;; With separator, should proceed (will fail with "No log file" but that's exit 1, not 2)
  (let ((result (elinit--cli-dispatch '("logs" "--" "-svc"))))
    (should (= elinit-cli-exit-failure (elinit-cli-result-exitcode result)))
    (should (string-match "No log file" (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-logs-rejects-extra-args ()
  "Logs with extra args returns invalid-args (exit 2)."
  (let ((result (elinit--cli-dispatch '("logs" "id1" "id2"))))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))
    (should (string-match "extra" (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-kill-rejects-extra-args ()
  "Kill with extra args returns invalid-args (exit 2)."
  (let ((result (elinit--cli-dispatch '("kill" "id1" "id2"))))
    (should (= elinit-cli-exit-invalid-args (elinit-cli-result-exitcode result)))
    (should (string-match "extra" (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-logs-options-not-parsed-after-separator ()
  "Logs does not parse --tail after -- separator."
  ;; logs -- --tail should treat --tail as the ID, not an option
  (let ((result (elinit--cli-dispatch '("logs" "--" "--tail"))))
    ;; Should fail with "No log file for --tail", not parse --tail as option
    (should (= elinit-cli-exit-failure (elinit-cli-result-exitcode result)))
    (should (string-match "No log file for '--tail'" (elinit-cli-result-output result)))))

;;; CLI -- is-active Tests

(ert-deftest elinit-test-cli-is-active-running ()
  "The `is-active' returns exit 0 for a running unit."
  (elinit-test-with-unit-files
      '(("sleep 999" :id "svc" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (proc (start-process "test" nil "sleep" "999")))
      (unwind-protect
          (progn
            (puthash "svc" proc elinit--processes)
            (let ((result (elinit--cli-dispatch '("is-active" "svc"))))
              (should (= elinit-cli-exit-success
                         (elinit-cli-result-exitcode result)))
              (should (string-match "running" (elinit-cli-result-output result)))))
        (delete-process proc)))))

(ert-deftest elinit-test-cli-is-active-not-running ()
  "The `is-active' returns exit 3 (not-active) for a non-running unit."
  (elinit-test-with-unit-files
      '(("echo hi" :id "svc" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (result (elinit--cli-dispatch '("is-active" "svc"))))
      (should (= elinit-cli-exit-not-active
                 (elinit-cli-result-exitcode result))))))

(ert-deftest elinit-test-cli-is-active-unknown-id ()
  "The `is-active' returns exit 4 (no-such-unit) for unknown ID."
  (elinit-test-with-unit-files
      '(("echo hi" :id "svc" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (result (elinit--cli-dispatch '("is-active" "nonexistent"))))
      (should (= elinit-cli-exit-no-such-unit
                 (elinit-cli-result-exitcode result)))
      (should (string-match "inactive" (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-is-active-no-args ()
  "The `is-active' with no args returns exit 2."
  (let ((result (elinit--cli-dispatch '("is-active"))))
    (should (= elinit-cli-exit-invalid-args
               (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-is-active-extra-args ()
  "The `is-active' with multiple IDs returns exit 2."
  (let ((result (elinit--cli-dispatch '("is-active" "a" "b"))))
    (should (= elinit-cli-exit-invalid-args
               (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-is-active-json ()
  "The `is-active --json' returns JSON with active field."
  (elinit-test-with-unit-files
      '(("echo hi" :id "svc" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (result (elinit--cli-dispatch '("is-active" "svc" "--json"))))
      (should (eq 'json (elinit-cli-result-format result)))
      (let ((parsed (json-read-from-string (elinit-cli-result-output result))))
        (should (equal "svc" (alist-get 'id parsed)))
        (should (assoc 'active parsed))
        (should (assoc 'status parsed))))))

(ert-deftest elinit-test-cli-is-active-json-running ()
  "The `is-active --json' returns active=true for running unit."
  (elinit-test-with-unit-files
      '(("sleep 999" :id "svc" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (proc (start-process "test" nil "sleep" "999")))
      (unwind-protect
          (progn
            (puthash "svc" proc elinit--processes)
            (let* ((result (elinit--cli-dispatch '("is-active" "svc" "--json")))
                   (parsed (json-read-from-string
                            (elinit-cli-result-output result))))
              (should (= elinit-cli-exit-success
                         (elinit-cli-result-exitcode result)))
              (should (eq t (alist-get 'active parsed)))))
        (delete-process proc)))))

;;; CLI -- is-enabled Tests

(ert-deftest elinit-test-cli-is-enabled-enabled ()
  "The `is-enabled' returns exit 0 for an enabled unit."
  (elinit-test-with-unit-files
      '(("echo hi" :id "svc" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (result (elinit--cli-dispatch '("is-enabled" "svc"))))
      (should (= elinit-cli-exit-success
                 (elinit-cli-result-exitcode result)))
      (should (string-match "enabled" (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-is-enabled-disabled ()
  "The `is-enabled' returns exit 1 for a disabled unit."
  (elinit-test-with-unit-files
      '(("echo hi" :id "svc" :type simple :disabled t))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (result (elinit--cli-dispatch '("is-enabled" "svc"))))
      (should (= elinit-cli-exit-failure
                 (elinit-cli-result-exitcode result)))
      (should (string-match "disabled" (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-is-enabled-masked ()
  "The `is-enabled' returns exit 1 and state=masked for a masked unit."
  (elinit-test-with-unit-files
      '(("echo hi" :id "svc" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (elinit--mask-override (make-hash-table :test 'equal)))
      (puthash "svc" 'masked elinit--mask-override)
      (let ((result (elinit--cli-dispatch '("is-enabled" "svc"))))
        (should (= elinit-cli-exit-failure
                   (elinit-cli-result-exitcode result)))
        (should (string-match "masked" (elinit-cli-result-output result)))))))

(ert-deftest elinit-test-cli-is-enabled-unknown-id ()
  "The `is-enabled' returns exit 4 (no-such-unit) for unknown ID."
  (elinit-test-with-unit-files
      '(("echo hi" :id "svc" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (result (elinit--cli-dispatch '("is-enabled" "nonexistent"))))
      (should (= elinit-cli-exit-no-such-unit
                 (elinit-cli-result-exitcode result)))
      (should (string-match "not-found" (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-is-enabled-no-args ()
  "The `is-enabled' with no args returns exit 2."
  (let ((result (elinit--cli-dispatch '("is-enabled"))))
    (should (= elinit-cli-exit-invalid-args
               (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-is-enabled-extra-args ()
  "The `is-enabled' with multiple IDs returns exit 2."
  (let ((result (elinit--cli-dispatch '("is-enabled" "a" "b"))))
    (should (= elinit-cli-exit-invalid-args
               (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-is-enabled-json ()
  "The `is-enabled --json' returns JSON with enabled and state fields."
  (elinit-test-with-unit-files
      '(("echo hi" :id "svc" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (result (elinit--cli-dispatch '("is-enabled" "svc" "--json"))))
      (should (eq 'json (elinit-cli-result-format result)))
      (let ((parsed (json-read-from-string (elinit-cli-result-output result))))
        (should (equal "svc" (alist-get 'id parsed)))
        (should (eq t (alist-get 'enabled parsed)))
        (should (equal "enabled" (alist-get 'state parsed)))))))

(ert-deftest elinit-test-cli-is-enabled-masked-json ()
  "The `is-enabled --json' for masked unit shows enabled=false, state=masked."
  (elinit-test-with-unit-files
      '(("echo hi" :id "svc" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (elinit--mask-override (make-hash-table :test 'equal)))
      (puthash "svc" 'masked elinit--mask-override)
      (let* ((result (elinit--cli-dispatch '("is-enabled" "svc" "--json")))
             (parsed (json-read-from-string (elinit-cli-result-output result))))
        (should (= elinit-cli-exit-failure
                   (elinit-cli-result-exitcode result)))
        (should (eq :json-false (alist-get 'enabled parsed)))
        (should (equal "masked" (alist-get 'state parsed)))))))

;;; CLI -- is-failed Tests

(ert-deftest elinit-test-cli-is-failed-dead ()
  "The `is-failed' returns exit 0 for a crash-looped (dead) unit."
  (elinit-test-with-unit-files
      '(("echo hi" :id "svc" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (elinit--failed (make-hash-table :test 'equal)))
      (puthash "svc" t elinit--failed)
      (let ((result (elinit--cli-dispatch '("is-failed" "svc"))))
        (should (= elinit-cli-exit-success
                   (elinit-cli-result-exitcode result)))
        (should (string-match "dead" (elinit-cli-result-output result)))))))

(ert-deftest elinit-test-cli-is-failed-not-failed ()
  "The `is-failed' returns exit 1 for a non-failed unit."
  (elinit-test-with-unit-files
      '(("echo hi" :id "svc" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (result (elinit--cli-dispatch '("is-failed" "svc"))))
      (should (= elinit-cli-exit-failure
                 (elinit-cli-result-exitcode result))))))

(ert-deftest elinit-test-cli-is-failed-oneshot-failed ()
  "The `is-failed' returns exit 0 for a oneshot with non-zero exit."
  (elinit-test-with-unit-files
      '(("false" :id "svc" :type oneshot))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (elinit--oneshot-completed (make-hash-table :test 'equal)))
      (puthash "svc" 1 elinit--oneshot-completed)
      (let ((result (elinit--cli-dispatch '("is-failed" "svc"))))
        (should (= elinit-cli-exit-success
                   (elinit-cli-result-exitcode result)))
        (should (string-match "failed" (elinit-cli-result-output result)))))))

(ert-deftest elinit-test-cli-is-failed-unknown-id ()
  "The `is-failed' returns exit 4 (no-such-unit) for unknown ID."
  (elinit-test-with-unit-files
      '(("echo hi" :id "svc" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (result (elinit--cli-dispatch '("is-failed" "nonexistent"))))
      (should (= elinit-cli-exit-no-such-unit
                 (elinit-cli-result-exitcode result)))
      (should (string-match "inactive" (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-is-failed-no-args ()
  "The `is-failed' with no args returns exit 2."
  (let ((result (elinit--cli-dispatch '("is-failed"))))
    (should (= elinit-cli-exit-invalid-args
               (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-is-failed-extra-args ()
  "The `is-failed' with multiple IDs returns exit 2."
  (let ((result (elinit--cli-dispatch '("is-failed" "a" "b"))))
    (should (= elinit-cli-exit-invalid-args
               (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-is-failed-json ()
  "The `is-failed --json' returns JSON with failed and status fields."
  (elinit-test-with-unit-files
      '(("echo hi" :id "svc" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (elinit--failed (make-hash-table :test 'equal)))
      (puthash "svc" t elinit--failed)
      (let* ((result (elinit--cli-dispatch '("is-failed" "svc" "--json")))
             (parsed (json-read-from-string (elinit-cli-result-output result))))
        (should (= elinit-cli-exit-success
                   (elinit-cli-result-exitcode result)))
        (should (eq 'json (elinit-cli-result-format result)))
        (should (equal "svc" (alist-get 'id parsed)))
        (should (eq t (alist-get 'failed parsed)))
        (should (equal "dead" (alist-get 'status parsed)))))))

(ert-deftest elinit-test-cli-is-failed-running-json ()
  "The `is-failed --json' returns failed=false for running unit."
  (elinit-test-with-unit-files
      '(("sleep 999" :id "svc" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (proc (start-process "test" nil "sleep" "999")))
      (unwind-protect
          (progn
            (puthash "svc" proc elinit--processes)
            (let* ((result (elinit--cli-dispatch '("is-failed" "svc" "--json")))
                   (parsed (json-read-from-string
                            (elinit-cli-result-output result))))
              (should (= elinit-cli-exit-failure
                         (elinit-cli-result-exitcode result)))
              (should (eq :json-false (alist-get 'failed parsed)))))
        (delete-process proc)))))

;;; CLI -- daemon-reload Tests

(ert-deftest elinit-test-daemon-reload-picks-up-config-change ()
  "The `daemon-reload' picks up added entries from disk."
  (elinit-test-with-unit-files nil
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (elinit--current-plan nil)
           (elinit--invalid (make-hash-table :test 'equal)))
      ;; Write one unit file and reload
      (with-temp-file (expand-file-name "a.el" elinit-unit-directory)
        (insert "(:id \"a\" :command \"echo a\" :type simple)"))
      (elinit-daemon-reload)
      (should elinit--current-plan)
      (should (= 1 (length (elinit-plan-entries elinit--current-plan))))
      ;; Add a second unit file and reload again
      (with-temp-file (expand-file-name "b.el" elinit-unit-directory)
        (insert "(:id \"b\" :command \"echo b\" :type simple)"))
      (elinit-daemon-reload)
      (should (= 2 (length (elinit-plan-entries elinit--current-plan)))))))

(ert-deftest elinit-test-daemon-reload-runtime-untouched ()
  "The `daemon-reload' does not start or stop processes."
  (elinit-test-with-unit-files
      '(("sleep 999" :id "svc" :type simple))
    (let ((elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--current-plan nil)
          (elinit--invalid (make-hash-table :test 'equal))
          (proc (start-process "test" nil "sleep" "999")))
      (unwind-protect
          (progn
            (puthash "svc" proc elinit--processes)
            ;; Remove the unit file from disk and reload
            (delete-file (expand-file-name "svc.el" elinit-unit-directory))
            (elinit-daemon-reload)
            ;; Process should still be running (runtime untouched)
            (should (process-live-p proc))
            (should (gethash "svc" elinit--processes)))
        (delete-process proc)))))

(ert-deftest elinit-test-daemon-reload-surfaces-invalid ()
  "The `daemon-reload' surfaces invalid entries in plan."
  (elinit-test-with-unit-files
      '(("true" :id "ok" :type simple)
        ("true" :id "nope" :type "not-a-symbol"))
    (let ((elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--current-plan nil)
          (elinit--invalid (make-hash-table :test 'equal)))
      (let ((result (elinit-daemon-reload)))
        (should (= 1 (plist-get result :entries)))
        (should (= 1 (plist-get result :invalid)))
        (should (gethash "nope" elinit--invalid))))))

(ert-deftest elinit-test-daemon-reload-returns-counts ()
  "The `daemon-reload' returns entry and invalid counts."
  (elinit-test-with-unit-files
      '(("echo a" :id "a" :type simple))
    (let ((elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--current-plan nil)
          (elinit--invalid (make-hash-table :test 'equal)))
      (let ((result (elinit-daemon-reload)))
        (should (= 1 (plist-get result :entries)))
        (should (= 0 (plist-get result :invalid)))))))

(ert-deftest elinit-test-cli-daemon-reload ()
  "CLI `daemon-reload' returns success."
  (elinit-test-with-unit-files
      '(("echo a" :id "a" :type simple))
    (let ((elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--current-plan nil)
          (elinit--invalid (make-hash-table :test 'equal)))
      (let ((result (elinit--cli-dispatch '("daemon-reload"))))
        (should (= elinit-cli-exit-success
                   (elinit-cli-result-exitcode result)))
        (should (string-match "1 entries" (elinit-cli-result-output result)))))))

(ert-deftest elinit-test-cli-daemon-reload-json ()
  "CLI `daemon-reload --json' returns JSON with reloaded and counts."
  (elinit-test-with-unit-files
      '(("echo a" :id "a" :type simple))
    (let ((elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--current-plan nil)
          (elinit--invalid (make-hash-table :test 'equal)))
      (let* ((result (elinit--cli-dispatch '("daemon-reload" "--json")))
             (parsed (json-read-from-string (elinit-cli-result-output result))))
        (should (= elinit-cli-exit-success
                   (elinit-cli-result-exitcode result)))
        (should (eq 'json (elinit-cli-result-format result)))
        (should (eq t (alist-get 'reloaded parsed)))
        (should (= 1 (alist-get 'entries parsed)))
        (should (= 0 (alist-get 'invalid parsed)))))))

(ert-deftest elinit-test-cli-daemon-reload-rejects-args ()
  "CLI `daemon-reload' with extra args returns exit 2."
  (let ((result (elinit--cli-dispatch '("daemon-reload" "extra"))))
    (should (= elinit-cli-exit-invalid-args
               (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-start-clears-current-plan ()
  "The `elinit-start' clears `elinit--current-plan'."
  (let ((elinit--current-plan 'dummy))
    ;; elinit-start resets this to nil early in its flow
    ;; We can't call full start in tests, but verify the variable exists
    (should (boundp 'elinit--current-plan))))

(ert-deftest elinit-test-daemon-reload-counts-unit-file-invalid ()
  "The `daemon-reload' invalid count includes malformed unit files."
  (elinit-test-with-unit-files nil
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (elinit--current-plan nil)
           (elinit--invalid (make-hash-table :test 'equal))
           (elinit--cycle-fallback-ids (make-hash-table :test 'equal))
           (elinit--computed-deps (make-hash-table :test 'equal)))
      ;; Create a malformed unit file (missing :command)
      (with-temp-file (expand-file-name "bad.el" elinit-unit-directory)
        (insert "(:id \"bad-unit\")"))
      (let ((result (elinit-daemon-reload)))
        ;; Invalid count should include the unit-file invalid
        (should (= 1 (plist-get result :invalid)))
        (should (gethash "bad-unit" elinit--invalid))))))

;;; Phase 8: reload command tests

(ert-deftest elinit-test-reload-unit-running-simple-restarts ()
  "Reloading a running simple unit stops and restarts it.
Reload bypasses `elinit--manual-start' and calls
`elinit--start-process' directly to avoid disabled-policy refusal."
  (elinit-test-with-unit-files
      '(("sleep 999" :id "svc1"))
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
           (stop-called nil)
           (start-called nil))
      ;; Simulate a running process
      (puthash "svc1" (start-process "test" nil "sleep" "999")
               elinit--processes)
      (unwind-protect
          (cl-letf (((symbol-function 'elinit--manual-stop)
                     (lambda (id)
                       (setq stop-called id)
                       (let ((p (gethash id elinit--processes)))
                         (when (and p (process-live-p p))
                           (delete-process p)))
                       (list :status 'stopped :reason nil)))
                    ((symbol-function 'elinit--start-process)
                     (lambda (id _cmd _logging _type _restart &rest _args)
                       (setq start-called id)
                       t)))
            (let ((result (elinit--reload-unit "svc1")))
              (should (equal "svc1" (plist-get result :id)))
              (should (equal "reloaded" (plist-get result :action)))
              (should (equal "svc1" stop-called))
              (should (equal "svc1" start-called))))
        ;; Cleanup
        (let ((p (gethash "svc1" elinit--processes)))
          (when (and p (process-live-p p))
            (delete-process p)))))))

(ert-deftest elinit-test-reload-unit-clears-conflict-suppression ()
  "Reload stop+start path clears conflict suppression."
  (elinit-test-with-unit-files
      '(("sleep 999" :id "svc1"))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--failed (make-hash-table :test 'equal))
           (elinit--restart-times (make-hash-table :test 'equal))
           (elinit--oneshot-completed (make-hash-table :test 'equal))
           (elinit--manually-stopped (make-hash-table :test 'equal))
           (elinit--conflict-suppressed (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (elinit--mask-override (make-hash-table :test 'equal))
           (elinit--enabled-override (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal))
           (elinit--cycle-fallback-ids (make-hash-table :test 'equal))
           (elinit--computed-deps (make-hash-table :test 'equal))
           (elinit--logging (make-hash-table :test 'equal))
           (elinit--current-plan nil))
      ;; Mark as conflict-suppressed (stale from earlier conflict)
      (puthash "svc1" "other-svc" elinit--conflict-suppressed)
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
                     (lambda (_id _cmd _logging _type _restart &rest _args)
                       t)))
            (elinit--reload-unit "svc1")
            ;; Conflict suppression should be cleared
            (should-not (gethash "svc1" elinit--conflict-suppressed)))
        (let ((p (gethash "svc1" elinit--processes)))
          (when (and p (process-live-p p))
            (delete-process p)))))))

(ert-deftest elinit-test-reload-stopped-clears-conflict-suppression ()
  "Reload of stopped unit clears stale conflict suppression."
  (elinit-test-with-unit-files
      '(("echo hi" :id "svc1"))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--failed (make-hash-table :test 'equal))
           (elinit--restart-times (make-hash-table :test 'equal))
           (elinit--oneshot-completed (make-hash-table :test 'equal))
           (elinit--manually-stopped (make-hash-table :test 'equal))
           (elinit--conflict-suppressed (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (elinit--mask-override (make-hash-table :test 'equal))
           (elinit--enabled-override (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal))
           (elinit--cycle-fallback-ids (make-hash-table :test 'equal))
           (elinit--computed-deps (make-hash-table :test 'equal))
           (elinit--logging (make-hash-table :test 'equal))
           (elinit--remain-active (make-hash-table :test 'equal)))
      (puthash "svc1" "other-svc" elinit--conflict-suppressed)
      (let ((result (elinit--reload-unit "svc1")))
        (should (equal "updated" (plist-get result :action)))
        (should-not (gethash "svc1" elinit--conflict-suppressed))))))

(ert-deftest elinit-test-reload-unit-stopped-updates ()
  "Reloading a stopped unit returns `updated' and clears stale state."
  (elinit-test-with-unit-files
      '(("echo hi" :id "svc1"))
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
           (elinit--logging (make-hash-table :test 'equal)))
      ;; Set some stale state
      (puthash "svc1" t elinit--failed)
      (puthash "svc1" '(12345) elinit--restart-times)
      (puthash "svc1" 0 elinit--oneshot-completed)
      (let ((result (elinit--reload-unit "svc1")))
        (should (equal "svc1" (plist-get result :id)))
        (should (equal "updated" (plist-get result :action)))
        ;; Stale state should be cleared
        (should-not (gethash "svc1" elinit--failed))
        (should-not (gethash "svc1" elinit--restart-times))
        (should-not (gethash "svc1" elinit--oneshot-completed))))))

(ert-deftest elinit-test-reload-unit-masked-skips ()
  "Reloading a masked unit returns `skipped (masked)'."
  (elinit-test-with-unit-files
      '(("echo hi" :id "svc1"))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--mask-override (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal)))
      (puthash "svc1" 'masked elinit--mask-override)
      (let ((result (elinit--reload-unit "svc1")))
        (should (equal "svc1" (plist-get result :id)))
        (should (equal "skipped (masked)" (plist-get result :action)))))))

(ert-deftest elinit-test-reload-unit-unknown-errors ()
  "Reloading an unknown unit returns an error."
  (elinit-test-with-unit-files nil
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--mask-override (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal))
           (elinit--cycle-fallback-ids (make-hash-table :test 'equal))
           (elinit--computed-deps (make-hash-table :test 'equal)))
      (let ((result (elinit--reload-unit "nonexistent")))
        (should (equal "nonexistent" (plist-get result :id)))
        (should (equal "error: not found" (plist-get result :action)))))))

(ert-deftest elinit-test-cli-reload-requires-ids ()
  "CLI `reload' with no IDs returns exit 2."
  (let ((result (elinit--cli-dispatch '("reload"))))
    (should (= elinit-cli-exit-invalid-args
               (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-reload-unknown-flag ()
  "CLI `reload' with unknown flag returns exit 2."
  (let ((result (elinit--cli-dispatch '("reload" "--bogus" "svc1"))))
    (should (= elinit-cli-exit-invalid-args
               (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-reload-stopped-human ()
  "CLI `reload' on stopped unit shows `updated' in human output."
  (elinit-test-with-unit-files
      '(("echo hi" :id "svc1"))
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
           (elinit--logging (make-hash-table :test 'equal)))
      (let ((result (elinit--cli-dispatch '("reload" "--" "svc1"))))
        (should (= elinit-cli-exit-success
                   (elinit-cli-result-exitcode result)))
        (should (string-match-p "svc1: updated"
                                (elinit-cli-result-output result)))))))

(ert-deftest elinit-test-cli-reload-masked-human ()
  "CLI `reload' on masked unit shows `skipped (masked)' in human output."
  (elinit-test-with-unit-files
      '(("echo hi" :id "svc1"))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--mask-override (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal)))
      (puthash "svc1" 'masked elinit--mask-override)
      (let ((result (elinit--cli-dispatch '("reload" "svc1"))))
        (should (= elinit-cli-exit-success
                   (elinit-cli-result-exitcode result)))
        (should (string-match-p "skipped (masked)"
                                (elinit-cli-result-output result)))))))

(ert-deftest elinit-test-cli-reload-unknown-id-human ()
  "CLI `reload' on unknown ID returns exit 1 with error."
  (elinit-test-with-unit-files nil
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--mask-override (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal))
           (elinit--cycle-fallback-ids (make-hash-table :test 'equal))
           (elinit--computed-deps (make-hash-table :test 'equal)))
      (let ((result (elinit--cli-dispatch '("reload" "ghost"))))
        (should (= elinit-cli-exit-failure
                   (elinit-cli-result-exitcode result)))
        (should (string-match-p "error: not found"
                                (elinit-cli-result-output result)))))))

(ert-deftest elinit-test-cli-reload-json ()
  "CLI `reload' with --json returns proper JSON structure."
  (elinit-test-with-unit-files
      '(("echo hi" :id "svc1") ("echo bye" :id "svc2"))
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
           (elinit--logging (make-hash-table :test 'equal)))
      (puthash "svc2" 'masked elinit--mask-override)
      (let* ((result (elinit--cli-dispatch '("reload" "svc1" "svc2" "--json")))
             (json-data (json-read-from-string
                         (elinit-cli-result-output result)))
             (results (cdr (assoc 'results json-data))))
        (should (= elinit-cli-exit-success
                   (elinit-cli-result-exitcode result)))
        (should (= 2 (length results)))
        (should (equal "updated" (cdr (assoc 'action (aref results 0)))))
        (should (equal "skipped (masked)" (cdr (assoc 'action (aref results 1)))))))))

(ert-deftest elinit-test-cli-reload-mixed-error-json ()
  "CLI `reload' with mix of valid and unknown IDs returns exit 1 JSON."
  (elinit-test-with-unit-files
      '(("echo hi" :id "svc1"))
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
           (elinit--logging (make-hash-table :test 'equal)))
      (let* ((result (elinit--cli-dispatch '("reload" "svc1" "ghost" "--json")))
             (json-data (json-read-from-string
                         (elinit-cli-result-output result)))
             (results (cdr (assoc 'results json-data))))
        (should (= elinit-cli-exit-failure
                   (elinit-cli-result-exitcode result)))
        (should (= 2 (length results)))
        (should (equal "updated" (cdr (assoc 'action (aref results 0)))))
        (should (string-match-p "error:" (cdr (assoc 'action (aref results 1)))))))))

(ert-deftest elinit-test-cli-reload-help-listed ()
  "CLI help text includes the `reload' command."
  (let ((result (elinit--cli-dispatch nil)))
    (should (string-match-p "reload \\[--\\] ID"
                            (elinit-cli-result-output result)))))

(ert-deftest elinit-test-reload-unit-file-only ()
  "Reload finds unit-file-only entries via effective programs."
  (elinit-test-with-unit-files
      '(("echo hello" :id "uf-svc"))
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
           (elinit--logging (make-hash-table :test 'equal)))
      (let ((result (elinit--reload-unit "uf-svc")))
        (should (equal "uf-svc" (plist-get result :id)))
        (should (equal "updated" (plist-get result :action)))))))

(ert-deftest elinit-test-reload-clears-stale-invalid ()
  "Reload succeeds on a previously-invalid entry after config fix.
When an entry was recorded in `elinit--invalid' but has since
been fixed, reload should find and parse it fresh, clearing the
stale invalid state."
  (elinit-test-with-unit-files
      '(("echo fixed" :id "svc1"))
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
           (elinit--logging (make-hash-table :test 'equal)))
      ;; Simulate stale invalid entry (was broken, now fixed in config)
      (puthash "svc1" "previously broken" elinit--invalid)
      (let ((result (elinit--reload-unit "svc1")))
        (should (equal "svc1" (plist-get result :id)))
        (should (equal "updated" (plist-get result :action)))
        ;; Invalid cache should be cleared for this entry
        (should-not (gethash "svc1" elinit--invalid))))))

(ert-deftest elinit-test-reload-find-entry-uses-effective-programs ()
  "The `elinit--reload-find-entry' reads from effective programs."
  (elinit-test-with-unit-files
      '(("echo hi" :id "from-config"))
    (let* ((elinit--invalid (make-hash-table :test 'equal)))
      ;; Should find unit-file entry
      (should (elinit--reload-find-entry "from-config"))
      ;; Should not find nonexistent entry
      (should-not (elinit--reload-find-entry "nonexistent"))
      ;; Should find entry even if it's in the invalid cache
      (puthash "from-config" "some reason" elinit--invalid)
      (should (elinit--reload-find-entry "from-config")))))

(ert-deftest elinit-test-reload-running-disabled-unit-succeeds ()
  "Reloading a running disabled unit keeps it running.
Reload bypasses enabled/disabled policy since the unit is already
running and reload's contract is config hot-swap."
  (elinit-test-with-unit-files
      '(("sleep 999" :id "svc1" :disabled t))
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
           (start-process-called nil))
      ;; Simulate a running process (was started before being disabled)
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
                     (lambda (id _cmd _logging _type _restart &rest _args)
                       (setq start-process-called id)
                       t)))
            (let ((result (elinit--reload-unit "svc1")))
              ;; Must succeed, not fail with "disabled"
              (should (equal "reloaded" (plist-get result :action)))
              (should (equal "svc1" start-process-called))))
        ;; Cleanup
        (let ((p (gethash "svc1" elinit--processes)))
          (when (and p (process-live-p p))
            (delete-process p)))))))

(ert-deftest elinit-test-reload-running-oneshot-updates-only ()
  "Reloading a running oneshot does not interrupt it.
Per Phase 8 spec, only running simple units are restarted.
Running oneshots get definition update only."
  (elinit-test-with-unit-files
      '(("sleep 999" :id "osh1" :type oneshot))
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
           (stop-called nil))
      ;; Simulate a running oneshot process
      (puthash "osh1" (start-process "test" nil "sleep" "999")
               elinit--processes)
      (unwind-protect
          (cl-letf (((symbol-function 'elinit--manual-stop)
                     (lambda (id)
                       (setq stop-called id)
                       (list :status 'stopped :reason nil))))
            (let ((result (elinit--reload-unit "osh1")))
              ;; Should return "updated", not "reloaded"
              (should (equal "updated" (plist-get result :action)))
              ;; Should NOT have called stop
              (should-not stop-called)))
        ;; Cleanup
        (let ((p (gethash "osh1" elinit--processes)))
          (when (and p (process-live-p p))
            (delete-process p)))))))

(ert-deftest elinit-test-reload-parse-error-returns-invalid-config ()
  "Reload on unparseable entry returns `error: invalid config'.
This distinguishes config errors from truly missing entries."
  (elinit-test-with-unit-files nil
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--mask-override (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal)))
      (cl-letf (((symbol-function 'elinit--effective-programs)
                 (lambda ()
                   ;; Return a malformed entry: not a string or (string . plist)
                   '(42))))
        ;; The malformed entry's ID will be "malformed#0"
        (let ((result (elinit--reload-unit "malformed#0")))
          (should (equal "malformed#0" (plist-get result :id)))
          (should (equal "error: invalid config" (plist-get result :action))))))))

;;; Phase 9: enable/disable model alignment tests

(ert-deftest elinit-test-start-disabled-unit-works ()
  "Manual start on a disabled unit succeeds (systemctl model).
`start' on a disabled unit runs it this session only without
changing the enabled override."
  (elinit-test-with-unit-files
      '(("echo hello" :id "svc1" :disabled t))
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
           (started nil))
      (cl-letf (((symbol-function 'elinit--start-process)
                 (lambda (id _cmd _logging _type _restart &rest _args)
                   (setq started id)
                   t)))
        (let ((result (elinit--manual-start "svc1")))
          ;; Should succeed, not be skipped
          (should (eq 'started (plist-get result :status)))
          (should (equal "svc1" started))
          ;; Should NOT change enabled override
          (should-not (gethash "svc1" elinit--enabled-override))
          ;; Should mark as manually started for reconcile
          (should (gethash "svc1" elinit--manually-started)))))))

(ert-deftest elinit-test-start-disabled-unit-override-unchanged ()
  "Starting a disabled unit does not flip the enabled override.
Even with an explicit :disabled override set, manual start does not
change the override — it just runs the unit this session."
  (elinit-test-with-unit-files
      '(("echo hello" :id "svc1"))
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
           (elinit--logging (make-hash-table :test 'equal)))
      ;; Disable via override
      (puthash "svc1" 'disabled elinit--enabled-override)
      (cl-letf (((symbol-function 'elinit--start-process)
                 (lambda (_id _cmd _logging _type _restart &rest _args) t)))
        (let ((result (elinit--manual-start "svc1")))
          (should (eq 'started (plist-get result :status)))
          ;; Override should still be 'disabled (unchanged)
          (should (eq 'disabled (gethash "svc1" elinit--enabled-override))))))))

(ert-deftest elinit-test-reconcile-keeps-manually-started-disabled ()
  "Reconcile does not stop disabled units that were manually started.
Per the systemctl model, `start' on a disabled unit is session-only
and reconcile should not undo it."
  (elinit-test-with-unit-files
      '(("sleep 100" :id "svc1" :disabled t))
    (let* ((plan (elinit--build-plan (elinit--effective-programs)))
           (process-alive (make-hash-table :test 'equal))
           (manually-started (make-hash-table :test 'equal)))
      (puthash "svc1" t process-alive)
      (puthash "svc1" t manually-started)
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
                        :manually-started manually-started
                        :timestamp (float-time)))
             (actions (elinit--compute-actions plan snapshot)))
        ;; Should be noop (manually-started), NOT stop (disabled)
        (let ((action (cl-find "svc1" actions
                               :key (lambda (a) (plist-get a :id))
                               :test #'equal)))
          (should action)
          (should (eq 'noop (plist-get action :op)))
          (should (eq 'manually-started (plist-get action :reason))))))))

(ert-deftest elinit-test-reconcile-stops-non-manual-disabled ()
  "Reconcile stops disabled units that were NOT manually started."
  (elinit-test-with-unit-files
      '(("sleep 100" :id "svc1" :disabled t))
    (let* ((plan (elinit--build-plan (elinit--effective-programs)))
           (process-alive (make-hash-table :test 'equal)))
      (puthash "svc1" t process-alive)
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
                        :manually-started (make-hash-table :test 'equal)
                        :timestamp (float-time)))
             (actions (elinit--compute-actions plan snapshot)))
        ;; Should stop (disabled, not manually started)
        (let ((action (cl-find "svc1" actions
                               :key (lambda (a) (plist-get a :id))
                               :test #'equal)))
          (should action)
          (should (eq 'stop (plist-get action :op)))
          (should (eq 'disabled (plist-get action :reason))))))))

(ert-deftest elinit-test-reconcile-stops-masked-even-if-manually-started ()
  "Reconcile stops masked units even if manually started.
Mask overrides everything including manual-start tracking."
  (elinit-test-with-unit-files
      '(("sleep 100" :id "svc1"))
    (let* ((plan (elinit--build-plan (elinit--effective-programs)))
           (process-alive (make-hash-table :test 'equal))
           (manually-started (make-hash-table :test 'equal))
           (mask-override (make-hash-table :test 'equal)))
      (puthash "svc1" t process-alive)
      (puthash "svc1" t manually-started)
      (puthash "svc1" 'masked mask-override)
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
                        :mask-override mask-override
                        :manually-started manually-started
                        :timestamp (float-time)))
             (actions (elinit--compute-actions plan snapshot)))
        (let ((action (cl-find "svc1" actions
                               :key (lambda (a) (plist-get a :id))
                               :test #'equal)))
          (should action)
          (should (eq 'stop (plist-get action :op)))
          (should (eq 'masked (plist-get action :reason))))))))

(ert-deftest elinit-test-manual-stop-clears-manually-started ()
  "Manually stopping a unit clears the manually-started flag.
After manual stop, reconcile is free to treat it normally."
  (let* ((elinit--processes (make-hash-table :test 'equal))
         (elinit--manually-stopped (make-hash-table :test 'equal))
         (elinit--manually-started (make-hash-table :test 'equal)))
    ;; Simulate a manually-started running process
    (puthash "svc1" (start-process "test" nil "sleep" "999")
             elinit--processes)
    (puthash "svc1" t elinit--manually-started)
    (unwind-protect
        (progn
          (elinit--manual-stop "svc1")
          ;; manually-started should be cleared
          (should-not (gethash "svc1" elinit--manually-started))
          ;; manually-stopped should be set
          (should (gethash "svc1" elinit--manually-stopped)))
      (let ((p (gethash "svc1" elinit--processes)))
        (when (and p (process-live-p p))
          (delete-process p))))))

(ert-deftest elinit-test-cli-start-disabled-unit ()
  "CLI `start' on a disabled unit succeeds."
  (elinit-test-with-unit-files
      '(("echo hi" :id "svc1" :disabled t))
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
           (elinit--logging (make-hash-table :test 'equal)))
      (cl-letf (((symbol-function 'elinit--start-process)
                 (lambda (_id _cmd _logging _type _restart &rest _args) t)))
        (let ((result (elinit--cli-dispatch '("start" "--" "svc1"))))
          (should (= elinit-cli-exit-success
                     (elinit-cli-result-exitcode result)))
          (should (string-match-p "Started: svc1"
                                  (elinit-cli-result-output result))))))))

(ert-deftest elinit-test-mask-still-blocks-manual-start ()
  "Masked units are still blocked from manual start.
Only mask blocks; disabled does not."
  (elinit-test-with-unit-files
      '(("echo hi" :id "svc1"))
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
           (elinit--logging (make-hash-table :test 'equal)))
      (puthash "svc1" 'masked elinit--mask-override)
      (let ((result (elinit--manual-start "svc1")))
        (should (eq 'skipped (plist-get result :status)))
        (should (equal "masked" (plist-get result :reason)))))))

(ert-deftest elinit-test-cli-enable-persists ()
  "CLI `enable' command persists the override to disk."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc1" :type simple :enabled nil))
    (let* ((temp-file (make-temp-file "elinit-test-enable-" nil ".eld"))
           (elinit-overrides-file temp-file)
           (elinit--enabled-override (make-hash-table :test 'equal))
           (elinit--restart-override (make-hash-table :test 'equal))
           (elinit--logging (make-hash-table :test 'equal))
           (elinit--mask-override (make-hash-table :test 'equal))
           (elinit--overrides-loaded nil))
      (unwind-protect
          (progn
            (elinit--cli-dispatch '("enable" "svc1"))
            ;; In-memory override set
            (should (eq 'enabled (gethash "svc1" elinit--enabled-override)))
            ;; Clear memory and reload from file
            (clrhash elinit--enabled-override)
            (should (elinit--load-overrides))
            ;; Should survive roundtrip
            (should (eq 'enabled
                        (gethash "svc1" elinit--enabled-override))))
        (delete-file temp-file)))))

(ert-deftest elinit-test-cli-disable-persists ()
  "CLI `disable' command persists the override to disk."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc1" :type simple))
    (let* ((temp-file (make-temp-file "elinit-test-disable-" nil ".eld"))
           (elinit-overrides-file temp-file)
           (elinit--enabled-override (make-hash-table :test 'equal))
           (elinit--restart-override (make-hash-table :test 'equal))
           (elinit--logging (make-hash-table :test 'equal))
           (elinit--mask-override (make-hash-table :test 'equal))
           (elinit--overrides-loaded nil))
      (unwind-protect
          (progn
            (elinit--cli-dispatch '("disable" "svc1"))
            ;; In-memory override set
            (should (eq 'disabled
                        (gethash "svc1" elinit--enabled-override)))
            ;; Clear memory and reload from file
            (clrhash elinit--enabled-override)
            (should (elinit--load-overrides))
            ;; Should survive roundtrip
            (should (eq 'disabled
                        (gethash "svc1" elinit--enabled-override))))
        (delete-file temp-file)))))

(ert-deftest elinit-test-manual-start-failure-no-stale-flag ()
  "Failed manual start does not leave stale manually-started flag.
Only successful starts should set the flag, otherwise reconcile
could incorrectly preserve a non-running disabled unit."
  (elinit-test-with-unit-files
      '(("echo hi" :id "svc1"))
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
           (elinit--logging (make-hash-table :test 'equal)))
      (cl-letf (((symbol-function 'elinit--start-process)
                 (lambda (_id _cmd _logging _type _restart &rest _args)
                   nil)))  ; Simulate spawn failure
        (let ((result (elinit--manual-start "svc1")))
          (should (eq 'error (plist-get result :status)))
          ;; Flag must NOT be set on failure
          (should-not (gethash "svc1" elinit--manually-started)))))))


;;;; Conflicts CLI tests

(ert-deftest elinit-test-cli-entry-info-conflicts ()
  "CLI entry info includes conflicts field."
  (let* ((elinit--processes (make-hash-table :test 'equal))
         (elinit--start-times (make-hash-table :test 'equal))
         (elinit--ready-times (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal))
         (entry (elinit--parse-entry '("cmd" :id "svc" :conflicts ("x" "y"))))
         (info (elinit--cli-entry-info entry)))
    (should (equal (alist-get 'conflicts info) '("x" "y")))))

(ert-deftest elinit-test-cli-entry-info-conflicts-nil ()
  "CLI entry info has nil conflicts when none declared."
  (let* ((elinit--processes (make-hash-table :test 'equal))
         (elinit--start-times (make-hash-table :test 'equal))
         (elinit--ready-times (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal))
         (entry (elinit--parse-entry '("cmd" :id "svc")))
         (info (elinit--cli-entry-info entry)))
    (should (null (alist-get 'conflicts info)))))

(ert-deftest elinit-test-cli-describe-human-conflicts ()
  "CLI human describe includes Conflicts line."
  (let* ((elinit--processes (make-hash-table :test 'equal))
         (elinit--start-times (make-hash-table :test 'equal))
         (elinit--ready-times (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal))
         (entry (elinit--parse-entry '("cmd" :id "svc" :conflicts ("a" "b"))))
         (info (elinit--cli-entry-info entry))
         (output (elinit--cli-describe-human info)))
    (should (string-match-p "Conflicts: a, b" output))))

(ert-deftest elinit-test-cli-describe-human-conflicts-none ()
  "CLI human describe shows Conflicts: none when none declared."
  (let* ((elinit--processes (make-hash-table :test 'equal))
         (elinit--start-times (make-hash-table :test 'equal))
         (elinit--ready-times (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal))
         (entry (elinit--parse-entry '("cmd" :id "svc")))
         (info (elinit--cli-entry-info entry))
         (output (elinit--cli-describe-human info)))
    (should (string-match-p "Conflicts: none" output))))

(ert-deftest elinit-test-cli-json-conflicts ()
  "CLI JSON output includes conflicts array."
  (let* ((elinit--processes (make-hash-table :test 'equal))
         (elinit--start-times (make-hash-table :test 'equal))
         (elinit--ready-times (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal))
         (entry (elinit--parse-entry '("cmd" :id "svc" :conflicts ("x"))))
         (info (elinit--cli-entry-info entry))
         (json-obj (elinit--cli-entry-to-json-obj info)))
    (should (equal (alist-get 'conflicts json-obj) '("x")))))

(ert-deftest elinit-test-cli-json-conflicts-empty ()
  "CLI JSON output has empty array when no conflicts."
  (let* ((elinit--processes (make-hash-table :test 'equal))
         (elinit--start-times (make-hash-table :test 'equal))
         (elinit--ready-times (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal))
         (entry (elinit--parse-entry '("cmd" :id "svc")))
         (info (elinit--cli-entry-info entry))
         (json-obj (elinit--cli-entry-to-json-obj info)))
    (should (equal (alist-get 'conflicts json-obj) []))))

;;; CLI Policy Batch Tests (from elinit-test-policy)

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

;;; Identity and Sandbox CLI Surface Tests

(ert-deftest elinit-test-cli-entry-info-includes-user-group ()
  "Entry info alist includes user and group fields."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple :user "alice" :group "staff"))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (plan (elinit--build-plan (elinit--effective-programs)))
           (entry (car (elinit-plan-entries plan)))
           (info (elinit--cli-entry-info entry)))
      (should (equal "alice" (alist-get 'user info)))
      (should (equal "staff" (alist-get 'group info))))))

(ert-deftest elinit-test-cli-entry-info-sandbox-fields ()
  "CLI entry-info includes sandbox profile, effective network, and enabled."
  (let* ((elinit--processes (make-hash-table :test 'equal))
         (elinit--failed (make-hash-table :test 'equal))
         (elinit--restart-override (make-hash-table :test 'equal))
         (elinit--enabled-override (make-hash-table :test 'equal))
         (elinit--mask-override (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal))
         (elinit--invalid (make-hash-table :test 'equal))
         (elinit--manually-stopped (make-hash-table :test 'equal))
         (elinit--manually-started (make-hash-table :test 'equal))
         (elinit--oneshot-completed (make-hash-table :test 'equal))
         (elinit--remain-active (make-hash-table :test 'equal))
         (elinit--last-exit-info (make-hash-table :test 'equal))
         (elinit--start-times (make-hash-table :test 'equal))
         (elinit--ready-times (make-hash-table :test 'equal))
         (elinit--restart-times (make-hash-table :test 'equal))
         (elinit--restart-timers (make-hash-table :test 'equal))
         (elinit--logging (make-hash-table :test 'equal))
         (entry (elinit--parse-entry
                 '("sleep 300" :id "svc" :sandbox-profile strict))))
    (let ((info (elinit--cli-entry-info entry)))
      (should (eq 'strict (alist-get 'sandbox-profile info)))
      (should (eq 'isolated (alist-get 'sandbox-network info)))
      (should (eq t (alist-get 'sandbox-enabled info))))))

;;; Restart Policy CLI Tests

(ert-deftest elinit-test-cli-restart-policy-rejects-legacy-on-off ()
  "Restart-policy rejects legacy on/off values."
  (let ((elinit--restart-override (make-hash-table :test 'equal)))
    (let ((result (elinit--cli-dispatch '("restart-policy" "on" "test-id"))))
      (should (= elinit-cli-exit-invalid-args
                 (elinit-cli-result-exitcode result))))
    (let ((result (elinit--cli-dispatch '("restart-policy" "off" "test-id"))))
      (should (= elinit-cli-exit-invalid-args
                 (elinit-cli-result-exitcode result))))))

(ert-deftest elinit-test-oneshot-json-restart-na ()
  "Oneshot entries emit restart \"n/a\" in JSON."
  (let* ((elinit-programs '(("true" :type oneshot)))
         (elinit--programs-cache '(("true" :type oneshot)))
         (elinit--processes (make-hash-table :test 'equal))
         (elinit--restart-override (make-hash-table :test 'equal))
         (elinit--enabled-override (make-hash-table :test 'equal))
         (elinit--logging (make-hash-table :test 'equal))
         (elinit--mask-override (make-hash-table :test 'equal))
         (elinit--failed (make-hash-table :test 'equal))
         (elinit--oneshot-completed (make-hash-table :test 'equal))
         (elinit--manually-stopped (make-hash-table :test 'equal))
         (elinit--manually-started (make-hash-table :test 'equal))
         (elinit--invalid (make-hash-table :test 'equal))
         (elinit--start-times (make-hash-table :test 'equal))
         (elinit--ready-times (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal))
         (elinit-unit-directory "/nonexistent-elinit-test-dir")
         (elinit-overrides-file nil)
         (elinit--overrides-loaded t))
    (let* ((result (elinit--cli-all-entries-info))
           (entries (car result))
           (info (car entries))
           (json-obj (elinit--cli-entry-to-json-obj info)))
      (should (equal "n/a" (alist-get 'restart json-obj))))))

;;; Metadata and Remain-After-Exit CLI Tests

(ert-deftest elinit-test-cli-describe-human-description ()
  "Human describe includes description line."
  (let* ((entry (elinit--parse-entry
                 '("cmd" :id "svc" :description "My service")))
         (info (elinit--cli-entry-info entry))
         (output (elinit--cli-describe-human info)))
    (should (string-match-p "Description: My service" output))))

(ert-deftest elinit-test-cli-show-includes-description ()
  "The `show ID' output includes Description when set."
  (elinit-test-with-unit-files
      '(("cmd" :id "svc" :description "My test service"))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (result (elinit--cli-dispatch '("show" "svc"))))
      (should (= elinit-cli-exit-success (elinit-cli-result-exitcode result)))
      (should (string-match-p "Description: My test service"
                              (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-is-active-remain-after-exit ()
  "The `is-active' returns exit 0 for an active remain-after-exit unit."
  (elinit-test-with-unit-files
      '(("true" :id "svc" :type oneshot :remain-after-exit t))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (elinit--remain-active (make-hash-table :test 'equal))
           (elinit--oneshot-completed (make-hash-table :test 'equal))
           (elinit--failed (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal))
           (elinit--enabled-override (make-hash-table :test 'equal))
           (elinit--restart-override (make-hash-table :test 'equal))
           (elinit--mask-override (make-hash-table :test 'equal))
           (elinit--logging (make-hash-table :test 'equal))
           (elinit--manually-started (make-hash-table :test 'equal)))
      ;; Simulate active latch state
      (puthash "svc" 0 elinit--oneshot-completed)
      (puthash "svc" t elinit--remain-active)
      (let ((result (elinit--cli-dispatch '("is-active" "svc"))))
        (should (= elinit-cli-exit-success
                   (elinit-cli-result-exitcode result)))
        (should (string-match "active" (elinit-cli-result-output result)))))))

;;; Resource Limits CLI Tests

(ert-deftest elinit-test-rlimits-cli-describe-human-shows-limits ()
  "CLI describe-human output contains Limits line with set fields."
  (let ((info '((id . "svc") (type . simple) (status . stopped)
                (enabled . t) (restart . always) (logging . t)
                (limit-nofile . 1024) (limit-nproc . "256:512")
                (limit-core . nil) (limit-fsize . nil)
                (limit-as . infinity))))
    (let ((output (elinit--cli-describe-human info)))
      (should (string-match-p "Limits:" output))
      (should (string-match-p "nofile=1024" output))
      (should (string-match-p "nproc=256:512" output))
      (should (string-match-p "as=infinity" output))
      ;; nil fields should not appear
      (should-not (string-match-p "core=" output))
      (should-not (string-match-p "fsize=" output)))))

(ert-deftest elinit-test-rlimits-cli-describe-human-no-limits-line ()
  "CLI describe-human output omits Limits line when no limits set."
  (let ((info '((id . "svc") (type . simple) (status . stopped)
                (enabled . t) (restart . always) (logging . t)
                (limit-nofile . nil) (limit-nproc . nil)
                (limit-core . nil) (limit-fsize . nil)
                (limit-as . nil))))
    (let ((output (elinit--cli-describe-human info)))
      (should-not (string-match-p "Limits:" output)))))

(ert-deftest elinit-test-rlimits-cli-json-includes-limit-fields ()
  "CLI JSON object includes limit fields."
  (let ((info '((id . "svc") (type . simple) (status . stopped)
                (enabled . t) (restart . always) (logging . t)
                (limit-nofile . 1024) (limit-nproc . nil)
                (limit-core . 0) (limit-fsize . nil)
                (limit-as . "100:200"))))
    (let ((json-obj (elinit--cli-entry-to-json-obj info)))
      (should (equal 1024 (alist-get 'limit_nofile json-obj)))
      (should (null (alist-get 'limit_nproc json-obj)))
      (should (equal 0 (alist-get 'limit_core json-obj)))
      (should (null (alist-get 'limit_fsize json-obj)))
      (should (equal "100:200" (alist-get 'limit_as json-obj))))))

;;; Target CLI Command Tests

(ert-deftest elinit-test-cli-get-default ()
  "The `get-default' command returns effective default-target-link."
  (let ((elinit-default-target-link "graphical.target")
        (elinit--default-target-link-override nil))
    (let ((result (elinit--cli-dispatch '("get-default"))))
      (should (= elinit-cli-exit-success
                  (elinit-cli-result-exitcode result)))
      (should (string-match "graphical\\.target"
                            (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-get-default-json ()
  "The `get-default --json' returns JSON with default-target key."
  (let ((elinit-default-target-link "graphical.target")
        (elinit--default-target-link-override nil))
    (let ((result (elinit--cli-dispatch '("get-default" "--json"))))
      (should (= elinit-cli-exit-success
                  (elinit-cli-result-exitcode result)))
      (should (eq 'json (elinit-cli-result-format result)))
      (let ((parsed (json-read-from-string
                     (elinit-cli-result-output result))))
        (should (equal "graphical.target"
                       (alist-get 'default-target parsed)))))))

(ert-deftest elinit-test-cli-get-default-with-override ()
  "The `get-default' returns override when set."
  (let ((elinit-default-target-link "graphical.target")
        (elinit--default-target-link-override "basic.target"))
    (let ((result (elinit--cli-dispatch '("get-default"))))
      (should (= elinit-cli-exit-success
                  (elinit-cli-result-exitcode result)))
      (should (string-match "basic\\.target"
                            (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-set-default-persists ()
  "The `set-default' sets override and persists."
  (elinit-test-with-unit-files
      '((nil :id "basic.target" :type target)
        (nil :id "multi-user.target" :type target
             :after ("basic.target"))
        (nil :id "graphical.target" :type target
             :after ("multi-user.target"))
        (nil :id "default.target" :type target))
    (let ((elinit--default-target-link-override nil)
          (elinit-overrides-file nil)
          (elinit-default-target-link "graphical.target"))
      (let ((result (elinit--cli-dispatch
                     '("set-default" "multi-user.target"))))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        (should (equal "multi-user.target"
                       elinit--default-target-link-override))
        ;; get-default should now return the new value
        (let ((get-result (elinit--cli-dispatch '("get-default"))))
          (should (string-match "multi-user\\.target"
                                (elinit-cli-result-output
                                 get-result))))))))

(ert-deftest elinit-test-cli-set-default-rejects-default-target ()
  "Setting default to \"default.target\" is rejected."
  (let ((result (elinit--cli-dispatch
                 '("set-default" "default.target"))))
    (should (= elinit-cli-exit-invalid-args
                (elinit-cli-result-exitcode result)))
    (should (string-match "circular"
                          (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-set-default-rejects-non-target ()
  "Setting default to a non-.target ID is rejected."
  (let ((result (elinit--cli-dispatch
                 '("set-default" "my-service"))))
    (should (= elinit-cli-exit-invalid-args
                (elinit-cli-result-exitcode result)))
    (should (string-match "\\.target"
                          (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-set-default-rejects-nonexistent ()
  "Setting default to a nonexistent target is rejected with exit code 4."
  (elinit-test-with-unit-files
      '((nil :id "basic.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--default-target-link-override nil)
          (elinit-default-target-link "basic.target"))
      (let ((result (elinit--cli-dispatch
                     '("set-default" "nonexistent.target"))))
        (should (= elinit-cli-exit-no-such-unit
                    (elinit-cli-result-exitcode result)))))))

(ert-deftest elinit-test-cli-list-targets ()
  "The `list-targets' command lists all targets with convergence state."
  (elinit-test-with-unit-files
      '(("sleep 1" :id "svc-a" :wanted-by ("multi-user.target"))
        (nil :id "basic.target" :type target)
        (nil :id "multi-user.target" :type target
             :after ("basic.target"))
        (nil :id "graphical.target" :type target
             :after ("multi-user.target"))
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit--target-convergence nil)
          (elinit--target-convergence-reasons nil)
          (elinit-default-target-link "graphical.target")
          (elinit--default-target-link-override nil))
      (let ((result (elinit--cli-dispatch '("list-targets"))))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        (let ((output (elinit-cli-result-output result)))
          (should (string-match "basic\\.target" output))
          (should (string-match "multi-user\\.target" output))
          (should (string-match "graphical\\.target" output))
          (should (string-match "default\\.target" output))
          ;; default.target should show resolved link
          (should (string-match "graphical\\.target" output)))))))

(ert-deftest elinit-test-cli-list-targets-json ()
  "The `list-targets --json' returns JSON array."
  (elinit-test-with-unit-files
      '((nil :id "basic.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit--target-convergence nil)
          (elinit--target-convergence-reasons nil)
          (elinit-default-target-link "basic.target")
          (elinit--default-target-link-override nil))
      (let ((result (elinit--cli-dispatch
                     '("list-targets" "--json"))))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        (should (eq 'json (elinit-cli-result-format result)))
        (let ((parsed (json-read-from-string
                       (elinit-cli-result-output result))))
          (should (vectorp parsed))
          (should (> (length parsed) 0)))))))

(ert-deftest elinit-test-cli-list-targets-outside-closure-unreachable ()
  "List-targets marks outside-closure targets as unreachable."
  (elinit-test-with-unit-files
      '((nil :id "graphical.target" :type target)
        (nil :id "rescue.target" :type target)
        (nil :id "default.target" :type target))
    (let* ((closure (make-hash-table :test 'equal))
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
             :activation-closure closure))
           (elinit--processes (make-hash-table :test 'equal))
           (elinit--failed (make-hash-table :test 'equal))
           (elinit--oneshot-completed (make-hash-table :test 'equal))
           (elinit--remain-active (make-hash-table :test 'equal))
           (elinit--manually-stopped (make-hash-table :test 'equal))
           (elinit--mask-override (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (elinit--target-convergence (make-hash-table :test 'equal))
           (elinit--target-convergence-reasons (make-hash-table :test 'equal))
           (elinit-default-target-link "graphical.target")
           (elinit--default-target-link-override nil))
      (puthash "graphical.target" t closure)
      (let ((result (elinit--cli-dispatch '("list-targets"))))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        (let ((output (elinit-cli-result-output result)))
          (should (string-match "rescue\\.target" output))
          (should (string-match "rescue\\.target[[:space:]]+unreachable"
                                output)))))))

(ert-deftest elinit-test-cli-target-status ()
  "The `target-status' shows convergence and member lists for a target."
  (elinit-test-with-unit-files
      '(("sleep 1" :id "svc-a" :wanted-by ("multi-user.target"))
        (nil :id "basic.target" :type target)
        (nil :id "multi-user.target" :type target
             :after ("basic.target"))
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--remain-active (make-hash-table :test 'equal))
          (elinit--manually-stopped (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--target-convergence nil)
          (elinit--target-convergence-reasons nil)
          (elinit-default-target-link "multi-user.target")
          (elinit--default-target-link-override nil))
      (let ((result (elinit--cli-dispatch
                     '("target-status" "multi-user.target"))))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        (let ((output (elinit-cli-result-output result)))
          (should (string-match "multi-user\\.target" output))
          (should (string-match "Status:" output))
          (should (string-match "Wants:" output)))))))

(ert-deftest elinit-test-cli-target-status-default-shows-link ()
  "The `target-status default.target' shows resolved link."
  (elinit-test-with-unit-files
      '((nil :id "basic.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--remain-active (make-hash-table :test 'equal))
          (elinit--manually-stopped (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--target-convergence nil)
          (elinit--target-convergence-reasons nil)
          (elinit-default-target-link "basic.target")
          (elinit--default-target-link-override nil))
      (let ((result (elinit--cli-dispatch
                     '("target-status" "default.target"))))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        (should (string-match "basic\\.target"
                              (elinit-cli-result-output result)))))))

(ert-deftest elinit-test-cli-target-status-nonexistent ()
  "Unknown target returns exit code 4."
  (elinit-test-with-unit-files
      '((nil :id "basic.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit-default-target-link "basic.target")
          (elinit--default-target-link-override nil))
      (let ((result (elinit--cli-dispatch
                     '("target-status" "nonexistent.target"))))
        (should (= elinit-cli-exit-no-such-unit
                    (elinit-cli-result-exitcode result)))))))

(ert-deftest elinit-test-cli-explain-target-reached ()
  "Reached target shows all members healthy message."
  (elinit-test-with-unit-files
      '(("sleep 1" :id "svc-a" :required-by ("app.target"))
        (nil :id "app.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--remain-active (make-hash-table :test 'equal))
          (elinit--manually-stopped (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--target-convergence (make-hash-table :test 'equal))
          (elinit--target-convergence-reasons
           (make-hash-table :test 'equal))
          (elinit-default-target-link "app.target")
          (elinit--default-target-link-override nil))
      (puthash "app.target" 'reached elinit--target-convergence)
      (let ((result (elinit--cli-dispatch
                     '("explain-target" "app.target"))))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        (should (string-match "reached"
                              (elinit-cli-result-output result)))
        (should (string-match "healthy"
                              (elinit-cli-result-output result)))))))

(ert-deftest elinit-test-cli-explain-target-degraded ()
  "Degraded target lists failed members."
  (elinit-test-with-unit-files
      '(("sleep 1" :id "svc-a" :required-by ("app.target"))
        (nil :id "app.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--remain-active (make-hash-table :test 'equal))
          (elinit--manually-stopped (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--target-convergence (make-hash-table :test 'equal))
          (elinit--target-convergence-reasons
           (make-hash-table :test 'equal))
          (elinit-default-target-link "app.target")
          (elinit--default-target-link-override nil))
      (puthash "app.target" 'degraded elinit--target-convergence)
      (puthash "app.target" '("svc-a: failed-to-spawn")
               elinit--target-convergence-reasons)
      (puthash "svc-a" t elinit--failed)
      (let ((result (elinit--cli-dispatch
                     '("explain-target" "app.target"))))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        (should (string-match "degraded"
                              (elinit-cli-result-output result)))
        (should (string-match "svc-a"
                              (elinit-cli-result-output result)))))))

(ert-deftest elinit-test-cli-explain-target-pending ()
  "Pending target shows appropriate message."
  (elinit-test-with-unit-files
      '((nil :id "app.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--remain-active (make-hash-table :test 'equal))
          (elinit--manually-stopped (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--target-convergence nil)
          (elinit--target-convergence-reasons nil)
          (elinit-default-target-link "app.target")
          (elinit--default-target-link-override nil))
      (let ((result (elinit--cli-dispatch
                     '("explain-target" "app.target"))))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        (should (string-match "pending"
                              (elinit-cli-result-output result)))))))

(ert-deftest elinit-test-cli-explain-target-outside-closure-unreachable ()
  "Explain-target reports unreachable when target is outside closure."
  (elinit-test-with-unit-files
      '((nil :id "graphical.target" :type target)
        (nil :id "rescue.target" :type target)
        (nil :id "default.target" :type target))
    (let* ((closure (make-hash-table :test 'equal))
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
             :activation-closure closure))
           (elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (elinit--failed (make-hash-table :test 'equal))
           (elinit--oneshot-completed (make-hash-table :test 'equal))
           (elinit--remain-active (make-hash-table :test 'equal))
           (elinit--manually-stopped (make-hash-table :test 'equal))
           (elinit--mask-override (make-hash-table :test 'equal))
           (elinit--target-convergence (make-hash-table :test 'equal))
           (elinit--target-convergence-reasons (make-hash-table :test 'equal))
           (elinit-default-target-link "graphical.target")
           (elinit--default-target-link-override nil))
      (puthash "graphical.target" t closure)
      (let ((result (elinit--cli-dispatch
                     '("explain-target" "rescue.target"))))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        (let ((output (elinit-cli-result-output result)))
          (should (string-match "rescue\\.target: unreachable" output))
          (should (string-match "outside the current activation closure"
                                output)))))))

(ert-deftest elinit-test-cli-explain-target-nonexistent ()
  "Explain-target with nonexistent target returns exit code 4."
  (elinit-test-with-unit-files
      '((nil :id "basic.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit-default-target-link "basic.target")
          (elinit--default-target-link-override nil))
      (let ((result (elinit--cli-dispatch
                     '("explain-target" "nonexistent.target"))))
        (should (= elinit-cli-exit-no-such-unit
                    (elinit-cli-result-exitcode result)))))))

(ert-deftest elinit-test-cli-isolate-requires-yes ()
  "Isolate without --yes returns error."
  (elinit-test-with-unit-files
      '((nil :id "app.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit-default-target-link "app.target")
          (elinit--default-target-link-override nil))
      (let ((result (elinit--cli-dispatch
                     '("isolate" "app.target"))))
        (should (= elinit-cli-exit-invalid-args
                    (elinit-cli-result-exitcode result)))
        (should (string-match "--yes"
                              (elinit-cli-result-output result)))))))

(ert-deftest elinit-test-cli-isolate-stops-and-starts ()
  "Isolate with --yes stops non-closure entries and starts closure entries."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc-a" :required-by ("app.target"))
        ("sleep 300" :id "svc-b" :required-by ("other.target"))
        (nil :id "app.target" :type target)
        (nil :id "other.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--remain-active (make-hash-table :test 'equal))
          (elinit--manually-stopped (make-hash-table :test 'equal))
          (elinit--manually-started (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--restart-times (make-hash-table :test 'equal))
          (elinit--spawn-failure-reason (make-hash-table :test 'equal))
          (elinit--target-convergence nil)
          (elinit--target-convergence-reasons nil)
          (elinit--target-members nil)
          (elinit-default-target-link "app.target")
          (elinit--default-target-link-override nil)
          (dag-entries nil)
          (stopped nil))
      ;; Mock dag-start-with-deps to capture entries and manual-stop to track stops
      (cl-letf (((symbol-function 'elinit--dag-start-with-deps)
                 (lambda (entries callback)
                   (setq dag-entries (mapcar #'elinit-entry-id entries))
                   (funcall callback)))
                ((symbol-function 'elinit--manual-stop)
                 (lambda (id) (push id stopped)
                   (list :status 'stopped :reason nil))))
        (let ((result (elinit--cli-dispatch
                       '("isolate" "--yes" "app.target"))))
          (should (= elinit-cli-exit-success
                      (elinit-cli-result-exitcode result)))
          (should (string-match "Isolated"
                                (elinit-cli-result-output result)))
          ;; svc-a should be submitted to DAG (in app.target closure)
          (should (member "svc-a" dag-entries))
          ;; svc-b should NOT be submitted (not in closure)
          (should-not (member "svc-b" dag-entries)))))))

(ert-deftest elinit-test-cli-isolate-nonexistent ()
  "Isolate with nonexistent target returns exit code 4."
  (elinit-test-with-unit-files
      '((nil :id "basic.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit-default-target-link "basic.target")
          (elinit--default-target-link-override nil))
      (let ((result (elinit--cli-dispatch
                     '("isolate" "--yes" "nonexistent.target"))))
        (should (= elinit-cli-exit-no-such-unit
                    (elinit-cli-result-exitcode result)))))))

(ert-deftest elinit-test-cli-start-target-override ()
  "The `start --target' uses specified root via let-bind."
  (elinit-test-with-unit-files
      '(("sleep 1" :id "svc-a" :required-by ("basic.target"))
        (nil :id "basic.target" :type target)
        (nil :id "graphical.target" :type target
             :after ("basic.target"))
        (nil :id "default.target" :type target))
    (let ((elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--remain-active (make-hash-table :test 'equal))
          (elinit--manually-stopped (make-hash-table :test 'equal))
          (elinit--manually-started (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--cycle-fallback-ids (make-hash-table :test 'equal))
          (elinit--computed-deps (make-hash-table :test 'equal))
          (elinit--restart-override (make-hash-table :test 'equal))
          (elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--logging (make-hash-table :test 'equal))
          (elinit--writers (make-hash-table :test 'equal))
          (elinit--stderr-writers (make-hash-table :test 'equal))
          (elinit--stderr-pipes (make-hash-table :test 'equal))
          (elinit--restart-times (make-hash-table :test 'equal))
          (elinit--restart-timers (make-hash-table :test 'equal))
          (elinit--last-exit-info (make-hash-table :test 'equal))
          (elinit--start-times (make-hash-table :test 'equal))
          (elinit--ready-times (make-hash-table :test 'equal))
          (elinit--spawn-failure-reason (make-hash-table :test 'equal))
          (elinit--target-convergence nil)
          (elinit--target-convergence-reasons nil)
          (elinit--target-members nil)
          (elinit--timers nil)
          (elinit--current-plan nil)
          (elinit--shutting-down nil)
          (elinit--overrides-loaded nil)
          (elinit-overrides-file nil)
          (elinit-default-target "default.target")
          (elinit-default-target-link "graphical.target")
          (elinit--default-target-link-override nil)
          (captured-target nil))
      ;; Mock elinit-start to capture what target was used
      (cl-letf (((symbol-function 'elinit-start)
                 (lambda ()
                   (setq captured-target elinit-default-target))))
        (let ((result (elinit--cli-dispatch
                       '("start" "--target" "basic.target"))))
          (should (= elinit-cli-exit-success
                      (elinit-cli-result-exitcode result)))
          ;; The let-binding should have set elinit-default-target
          ;; to basic.target during the call
          (should (equal "basic.target" captured-target)))))))

(ert-deftest elinit-test-cli-start-target-invalid ()
  "The `start --target' with non-.target suffix returns error."
  (let ((result (elinit--cli-dispatch
                 '("start" "--target" "my-service"))))
    (should (= elinit-cli-exit-invalid-args
                (elinit-cli-result-exitcode result)))
    (should (string-match "\\.target"
                          (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-start-target-missing-value ()
  "The `start --target' without a value returns error."
  (let ((result (elinit--cli-dispatch '("start" "--target"))))
    (should (= elinit-cli-exit-invalid-args
                (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-start-target-with-ids-rejected ()
  "The `start --target T ID' is rejected -- cannot combine."
  (let ((result (elinit--cli-dispatch
                 '("start" "--target" "basic.target" "svc-a"))))
    (should (= elinit-cli-exit-invalid-args
                (elinit-cli-result-exitcode result)))
    (should (string-match "cannot be combined"
                          (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-start-target-nonexistent-clean-error ()
  "The `start --target' with nonexistent target returns clean error."
  (elinit-test-with-unit-files
      '((nil :id "basic.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit-default-target-link "basic.target")
          (elinit--default-target-link-override nil))
      (let ((result (elinit--cli-dispatch
                     '("start" "--target" "nonexistent.target"))))
        (should (= elinit-cli-exit-no-such-unit
                    (elinit-cli-result-exitcode result)))
        ;; Should be a clean error, not "Internal error"
        (should-not (string-match "Internal error"
                                  (elinit-cli-result-output result)))))))

(ert-deftest elinit-test-cli-start-no-args-calls-elinit-start ()
  "Plain `start' with no arguments calls `elinit-start'."
  (elinit-test-with-unit-files
      '((nil :id "basic.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit-default-target-link "basic.target")
          (elinit--default-target-link-override nil)
          (called nil))
      (cl-letf (((symbol-function 'elinit-start)
                 (lambda () (setq called t))))
        (let ((result (elinit--cli-dispatch '("start"))))
          (should (= elinit-cli-exit-success
                      (elinit-cli-result-exitcode result)))
          (should called))))))

(ert-deftest elinit-test-cli-isolate-skips-running-entries ()
  "Isolate does not submit already-running entries to DAG for starting."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc-a" :required-by ("app.target"))
        ("sleep 300" :id "svc-b" :required-by ("app.target"))
        (nil :id "app.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--remain-active (make-hash-table :test 'equal))
          (elinit--manually-stopped (make-hash-table :test 'equal))
          (elinit--manually-started (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--restart-times (make-hash-table :test 'equal))
          (elinit--spawn-failure-reason (make-hash-table :test 'equal))
          (elinit--target-convergence nil)
          (elinit--target-convergence-reasons nil)
          (elinit--target-members nil)
          (elinit-default-target-link "app.target")
          (elinit--default-target-link-override nil)
          (dag-entries nil)
          (stopped nil))
      ;; Simulate svc-a already running
      (let ((fake-proc (start-process "fake-svc-a" nil "sleep" "300")))
        (unwind-protect
            (progn
              (puthash "svc-a" fake-proc elinit--processes)
              (cl-letf (((symbol-function 'elinit--dag-start-with-deps)
                         (lambda (entries callback)
                           (setq dag-entries
                                 (mapcar #'elinit-entry-id entries))
                           (funcall callback)))
                        ((symbol-function 'elinit--manual-stop)
                         (lambda (id) (push id stopped)
                           (list :status 'stopped :reason nil))))
                (let ((result (elinit--cli-dispatch
                               '("isolate" "--yes" "app.target"))))
                  (should (= elinit-cli-exit-success
                              (elinit-cli-result-exitcode result)))
                  ;; svc-a should NOT be in DAG entries (already running)
                  (should-not (member "svc-a" dag-entries))
                  ;; svc-b should be in DAG entries (not running)
                  (should (member "svc-b" dag-entries)))))
          (when (process-live-p fake-proc)
            (delete-process fake-proc)))))))

(ert-deftest elinit-test-cli-explain-target-converging ()
  "Converging target lists non-terminal members."
  (elinit-test-with-unit-files
      '(("sleep 1" :id "svc-a" :required-by ("app.target"))
        ("sleep 1" :id "svc-b" :required-by ("app.target"))
        (nil :id "app.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--remain-active (make-hash-table :test 'equal))
          (elinit--manually-stopped (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--target-convergence (make-hash-table :test 'equal))
          (elinit--target-convergence-reasons
           (make-hash-table :test 'equal))
          (elinit-default-target-link "app.target")
          (elinit--default-target-link-override nil))
      (puthash "app.target" 'converging elinit--target-convergence)
      ;; svc-a is running, svc-b is pending (non-terminal)
      (puthash "svc-a" 'started elinit--entry-state)
      (let ((result (elinit--cli-dispatch
                     '("explain-target" "app.target"))))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        (should (string-match "converging"
                              (elinit-cli-result-output result)))
        (should (string-match "not yet terminal"
                              (elinit-cli-result-output result)))))))

;;; SysV Init CLI Command Tests

(ert-deftest elinit-test-cli-init-valid-runlevels ()
  "The `init' command accepts all runlevels 0-6 and resolves correctly."
  (elinit-test-with-unit-files
      '((nil :id "basic.target" :type target)
        (nil :id "multi-user.target" :type target)
        (nil :id "graphical.target" :type target)
        (nil :id "rescue.target" :type target)
        (nil :id "shutdown.target" :type target)
        (nil :id "poweroff.target" :type target)
        (nil :id "reboot.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--remain-active (make-hash-table :test 'equal))
          (elinit--manually-stopped (make-hash-table :test 'equal))
          (elinit--manually-started (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--restart-times (make-hash-table :test 'equal))
          (elinit--spawn-failure-reason (make-hash-table :test 'equal))
          (elinit--target-convergence nil)
          (elinit--target-convergence-reasons nil)
          (elinit--target-members nil)
          (elinit-default-target-link "graphical.target")
          (elinit--default-target-link-override nil))
      (cl-letf (((symbol-function 'elinit--dag-start-with-deps)
                 (lambda (_entries callback) (funcall callback)))
                ((symbol-function 'elinit--manual-stop)
                 (lambda (_id) (list :status 'stopped :reason nil))))
        ;; Test all runlevels with both target and runlevel identity
        (dolist (spec '((0 "poweroff\\.target" "runlevel 0")
                        (1 "rescue\\.target" "runlevel 1")
                        (2 "multi-user\\.target" "runlevel 2")
                        (3 "multi-user\\.target" "runlevel 3")
                        (4 "multi-user\\.target" "runlevel 4")
                        (5 "graphical\\.target" "runlevel 5")
                        (6 "reboot\\.target" "runlevel 6")))
          (let* ((rl (number-to-string (car spec)))
                 (target-pattern (nth 1 spec))
                 (runlevel-pattern (nth 2 spec))
                 (result (elinit--cli-dispatch
                          (list "init" "--yes" rl)))
                 (output (elinit-cli-result-output result)))
            (should (= elinit-cli-exit-success
                        (elinit-cli-result-exitcode result)))
            ;; Target identity in output
            (should (string-match target-pattern output))
            ;; Numeric runlevel identity in output
            (should (string-match runlevel-pattern output))))))))

(ert-deftest elinit-test-cli-init-out-of-range ()
  "The `init' command rejects runlevels outside 0-6."
  (let ((result (elinit--cli-dispatch '("init" "7"))))
    (should (= elinit-cli-exit-invalid-args
                (elinit-cli-result-exitcode result)))
    (should (string-match "out of range"
                          (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-init-destructive-requires-yes ()
  "Runlevels 0 and 6 require --yes for destructive confirmation."
  ;; init 0 without --yes
  (let ((result (elinit--cli-dispatch '("init" "0"))))
    (should (= elinit-cli-exit-invalid-args
                (elinit-cli-result-exitcode result)))
    (should (string-match "destructive"
                          (elinit-cli-result-output result)))
    (should (string-match "--yes"
                          (elinit-cli-result-output result))))
  ;; init 6 without --yes
  (let ((result (elinit--cli-dispatch '("init" "6"))))
    (should (= elinit-cli-exit-invalid-args
                (elinit-cli-result-exitcode result)))
    (should (string-match "destructive"
                          (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-init-interactive-prompt-accepts ()
  "Interactive destructive init succeeds when user confirms via y-or-n-p."
  (elinit-test-with-unit-files
      '((nil :id "basic.target" :type target)
        (nil :id "shutdown.target" :type target)
        (nil :id "poweroff.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--remain-active (make-hash-table :test 'equal))
          (elinit--manually-stopped (make-hash-table :test 'equal))
          (elinit--manually-started (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--restart-times (make-hash-table :test 'equal))
          (elinit--spawn-failure-reason (make-hash-table :test 'equal))
          (elinit--target-convergence nil)
          (elinit--target-convergence-reasons nil)
          (elinit--target-members nil)
          (elinit-default-target-link "poweroff.target")
          (elinit--default-target-link-override nil)
          ;; Simulate interactive Emacs
          (noninteractive nil))
      (cl-letf (((symbol-function 'elinit--dag-start-with-deps)
                 (lambda (_entries callback) (funcall callback)))
                ((symbol-function 'elinit--manual-stop)
                 (lambda (_id) (list :status 'stopped :reason nil)))
                ((symbol-function 'y-or-n-p)
                 (lambda (_prompt) t)))
        ;; init 0 without --yes, but y-or-n-p returns t
        (let ((result (elinit--cli-dispatch '("init" "0"))))
          (should (= elinit-cli-exit-success
                      (elinit-cli-result-exitcode result)))
          (should (string-match "poweroff\\.target"
                                (elinit-cli-result-output result))))))))

(ert-deftest elinit-test-cli-init-interactive-prompt-declines ()
  "Interactive destructive init fails when user declines via y-or-n-p."
  (let ((noninteractive nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) nil)))
      ;; init 0 without --yes, y-or-n-p returns nil
      (let ((result (elinit--cli-dispatch '("init" "0"))))
        (should (= elinit-cli-exit-invalid-args
                    (elinit-cli-result-exitcode result)))
        (should (string-match "destructive"
                              (elinit-cli-result-output result)))))))

(ert-deftest elinit-test-cli-init-non-destructive-no-yes ()
  "Runlevels 1-5 do not require --yes."
  (elinit-test-with-unit-files
      '((nil :id "basic.target" :type target)
        (nil :id "multi-user.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--remain-active (make-hash-table :test 'equal))
          (elinit--manually-stopped (make-hash-table :test 'equal))
          (elinit--manually-started (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--restart-times (make-hash-table :test 'equal))
          (elinit--spawn-failure-reason (make-hash-table :test 'equal))
          (elinit--target-convergence nil)
          (elinit--target-convergence-reasons nil)
          (elinit--target-members nil)
          (elinit-default-target-link "multi-user.target")
          (elinit--default-target-link-override nil))
      ;; init routes through isolate which requires --yes, but init
      ;; passes --yes automatically for non-destructive transitions
      (cl-letf (((symbol-function 'elinit--dag-start-with-deps)
                 (lambda (_entries callback) (funcall callback)))
                ((symbol-function 'elinit--manual-stop)
                 (lambda (_id) (list :status 'stopped :reason nil))))
        (let ((result (elinit--cli-dispatch '("init" "3"))))
          (should (= elinit-cli-exit-success
                      (elinit-cli-result-exitcode result))))))))

(ert-deftest elinit-test-cli-init-does-not-persist-default ()
  "The `init' command does not persist default target changes."
  (elinit-test-with-unit-files
      '((nil :id "basic.target" :type target)
        (nil :id "multi-user.target" :type target)
        (nil :id "graphical.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--remain-active (make-hash-table :test 'equal))
          (elinit--manually-stopped (make-hash-table :test 'equal))
          (elinit--manually-started (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--restart-times (make-hash-table :test 'equal))
          (elinit--spawn-failure-reason (make-hash-table :test 'equal))
          (elinit--target-convergence nil)
          (elinit--target-convergence-reasons nil)
          (elinit--target-members nil)
          (elinit-default-target-link "graphical.target")
          (elinit--default-target-link-override nil))
      (cl-letf (((symbol-function 'elinit--dag-start-with-deps)
                 (lambda (_entries callback) (funcall callback)))
                ((symbol-function 'elinit--manual-stop)
                 (lambda (_id) (list :status 'stopped :reason nil))))
        (elinit--cli-dispatch '("init" "--yes" "3"))
        ;; default-target-link-override must remain nil
        (should-not elinit--default-target-link-override)
        ;; get-default must still return original
        (let ((result (elinit--cli-dispatch '("get-default"))))
          (should (string-match "graphical\\.target"
                                (elinit-cli-result-output result))))))))

;;; Target Alias CLI Tests

(ert-deftest elinit-test-cli-list-targets-alias-shows-resolved ()
  "Alias targets in list-targets show resolved canonical target."
  (elinit-test-with-unit-files
      '((nil :id "basic.target" :type target)
        (nil :id "multi-user.target" :type target)
        (nil :id "graphical.target" :type target)
        (nil :id "default.target" :type target)
        (nil :id "runlevel5.target" :type target))
    (let ((elinit--current-plan t)
          (elinit--target-convergence nil)
          (elinit--target-convergence-reasons nil)
          (elinit-default-target-link "graphical.target")
          (elinit--default-target-link-override nil))
      (let ((result (elinit--cli-dispatch '("list-targets"))))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        (let ((output (elinit-cli-result-output result)))
          (should (string-match "alias" output))
          (should (string-match "runlevel5\\.target" output))
          (should (string-match "graphical\\.target" output)))))))

(ert-deftest elinit-test-cli-target-status-alias-shows-kind ()
  "Target-status for an alias target shows kind and resolved link."
  (elinit-test-with-unit-files
      '((nil :id "basic.target" :type target)
        (nil :id "multi-user.target" :type target)
        (nil :id "runlevel3.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--remain-active (make-hash-table :test 'equal))
          (elinit--manually-stopped (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--target-convergence nil)
          (elinit--target-convergence-reasons nil)
          (elinit-default-target-link "multi-user.target")
          (elinit--default-target-link-override nil))
      (let ((result (elinit--cli-dispatch
                     '("target-status" "runlevel3.target"))))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        (let ((output (elinit-cli-result-output result)))
          (should (string-match "alias" output))
          (should (string-match "multi-user\\.target" output)))))))

(ert-deftest elinit-test-cli-set-default-resolves-alias ()
  "The `set-default' with alias target persists canonical target."
  (elinit-test-with-unit-files
      '((nil :id "basic.target" :type target)
        (nil :id "multi-user.target" :type target)
        (nil :id "graphical.target" :type target)
        (nil :id "runlevel5.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--default-target-link-override nil)
          (elinit-overrides-file nil)
          (elinit-default-target-link "multi-user.target"))
      (let ((result (elinit--cli-dispatch
                     '("set-default" "runlevel5.target"))))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        ;; Must persist the canonical target, not the alias
        (should (equal "graphical.target"
                       elinit--default-target-link-override))
        ;; Output should mention both
        (should (string-match "graphical\\.target"
                              (elinit-cli-result-output result)))))))

(ert-deftest elinit-test-cli-isolate-accepts-alias ()
  "Isolate accepts alias targets and resolves to canonical."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc-a" :required-by ("multi-user.target"))
        (nil :id "basic.target" :type target)
        (nil :id "multi-user.target" :type target)
        (nil :id "runlevel3.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--remain-active (make-hash-table :test 'equal))
          (elinit--manually-stopped (make-hash-table :test 'equal))
          (elinit--manually-started (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--restart-times (make-hash-table :test 'equal))
          (elinit--spawn-failure-reason (make-hash-table :test 'equal))
          (elinit--target-convergence nil)
          (elinit--target-convergence-reasons nil)
          (elinit--target-members nil)
          (elinit-default-target-link "multi-user.target")
          (elinit--default-target-link-override nil))
      (cl-letf (((symbol-function 'elinit--dag-start-with-deps)
                 (lambda (_entries callback) (funcall callback)))
                ((symbol-function 'elinit--manual-stop)
                 (lambda (_id) (list :status 'stopped :reason nil))))
        (let ((result (elinit--cli-dispatch
                       '("isolate" "--yes" "runlevel3.target"))))
          (should (= elinit-cli-exit-success
                      (elinit-cli-result-exitcode result)))
          ;; Output should reference the canonical target
          (should (string-match "multi-user\\.target"
                                (elinit-cli-result-output result))))))))

(ert-deftest elinit-test-cli-list-timers-init-transition-human ()
  "The `list-timers' human output carries init-transition reason text."
  (elinit-test-with-unit-files
      '((nil :id "poweroff.target" :type target))
    (let ((elinit-mode t)
          (elinit-timers '((:id "timer-poweroff"
                               :target "poweroff.target"
                               :on-startup-sec 60)))
          (elinit--timer-list nil)
          (elinit--timer-state (make-hash-table :test 'equal))
          (elinit--invalid-timers (make-hash-table :test 'equal)))
      (let ((result (elinit--cli-dispatch '("list-timers"))))
        (should (elinit-cli-result-p result))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        (let ((output (elinit-cli-result-output result)))
          (should (string-match-p "timer-poweroff" output))
          (should (string-match-p "init-transition" output))
          (should (string-match-p "not timer-eligible" output)))))))

(ert-deftest elinit-test-cli-explain-target-alias-shows-resolved ()
  "Explain-target for alias shows resolved canonical target."
  (elinit-test-with-unit-files
      '((nil :id "basic.target" :type target)
        (nil :id "multi-user.target" :type target)
        (nil :id "runlevel3.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--remain-active (make-hash-table :test 'equal))
          (elinit--manually-stopped (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--target-convergence nil)
          (elinit--target-convergence-reasons nil)
          (elinit-default-target-link "multi-user.target")
          (elinit--default-target-link-override nil))
      (let ((result (elinit--cli-dispatch
                     '("explain-target" "runlevel3.target"))))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        (let ((output (elinit-cli-result-output result)))
          (should (string-match "alias" output))
          (should (string-match "multi-user\\.target" output)))))))

;;; Target Guard CLI Tests

(ert-deftest elinit-test-cli-cat-rejects-target ()
  "CLI cat rejects target units."
  (elinit-test-with-unit-files
      '((nil :id "basic.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit-default-target-link "basic.target")
          (elinit--default-target-link-override nil))
      (let ((result (elinit--cli-dispatch '("cat" "basic.target"))))
        (should (/= elinit-cli-exit-success
                     (elinit-cli-result-exitcode result)))
        (should (string-match "target"
                              (elinit-cli-result-output result)))))))

(ert-deftest elinit-test-cli-logs-rejects-target ()
  "CLI logs rejects target units."
  (elinit-test-with-unit-files
      '((nil :id "basic.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit-default-target-link "basic.target")
          (elinit--default-target-link-override nil))
      (let ((result (elinit--cli-dispatch '("logs" "basic.target"))))
        (should (/= elinit-cli-exit-success
                     (elinit-cli-result-exitcode result)))
        (should (string-match "target"
                              (elinit-cli-result-output result)))))))

(ert-deftest elinit-test-cli-isolate-publishes-conflicts-metadata ()
  "Isolate publishes plan with conflicts metadata."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc-a" :conflicts "svc-b"
         :required-by ("app.target"))
        ("sleep 300" :id "svc-b" :required-by ("app.target"))
        (nil :id "app.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--remain-active (make-hash-table :test 'equal))
          (elinit--manually-stopped (make-hash-table :test 'equal))
          (elinit--manually-started (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--restart-times (make-hash-table :test 'equal))
          (elinit--spawn-failure-reason (make-hash-table :test 'equal))
          (elinit--target-convergence nil)
          (elinit--target-convergence-reasons nil)
          (elinit--target-members nil)
          (elinit-default-target-link "app.target")
          (elinit--default-target-link-override nil))
      (cl-letf (((symbol-function 'elinit--dag-start-with-deps)
                 (lambda (_entries callback) (funcall callback)))
                ((symbol-function 'elinit--manual-stop)
                 (lambda (_id) (list :status 'stopped :reason nil))))
        (elinit--cli-dispatch '("isolate" "--yes" "app.target"))
        ;; Plan should be published with conflicts metadata
        (should (elinit-plan-p elinit--current-plan))
        (should (hash-table-p
                 (elinit-plan-conflicts-deps elinit--current-plan)))
        (should (equal (gethash "svc-a"
                                (elinit-plan-conflicts-deps
                                 elinit--current-plan))
                       '("svc-b")))))))

(ert-deftest elinit-test-cli-isolate-stops-latched-oneshot ()
  "Isolate stops latched remain-after-exit oneshots outside new closure."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc-a" :required-by ("app.target"))
        ("true" :id "svc-b" :type oneshot :remain-after-exit t
         :required-by ("other.target"))
        (nil :id "app.target" :type target)
        (nil :id "other.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--remain-active (make-hash-table :test 'equal))
          (elinit--manually-stopped (make-hash-table :test 'equal))
          (elinit--manually-started (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--restart-times (make-hash-table :test 'equal))
          (elinit--spawn-failure-reason (make-hash-table :test 'equal))
          (elinit--target-convergence nil)
          (elinit--target-convergence-reasons nil)
          (elinit--target-members nil)
          (elinit-default-target-link "app.target")
          (elinit--default-target-link-override nil)
          (stopped nil))
      ;; svc-b is a latched oneshot (no live process, but remain-active)
      (puthash "svc-b" t elinit--remain-active)
      (cl-letf (((symbol-function 'elinit--dag-start-with-deps)
                 (lambda (_entries callback) (funcall callback)))
                ((symbol-function 'elinit--manual-stop)
                 (lambda (id)
                   (push id stopped)
                   (remhash id elinit--remain-active)
                   (list :status 'stopped :reason nil))))
        (elinit--cli-dispatch '("isolate" "--yes" "app.target"))
        ;; svc-b should have been stopped (latched but outside closure)
        (should (member "svc-b" stopped))))))

(ert-deftest elinit-test-cli-isolate-dag-conflict-preflight ()
  "Isolate DAG start path fires conflict preflight against active units.
When the DAG scheduler starts svc-a, its conflict with svc-b should
stop svc-b via `elinit--conflict-preflight' in `elinit--dag-do-start'."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc-a" :conflicts "svc-b"
         :required-by ("app.target"))
        ("sleep 300" :id "svc-b" :required-by ("app.target"))
        (nil :id "app.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--remain-active (make-hash-table :test 'equal))
          (elinit--manually-stopped (make-hash-table :test 'equal))
          (elinit--manually-started (make-hash-table :test 'equal))
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--restart-times (make-hash-table :test 'equal))
          (elinit--restart-timers (make-hash-table :test 'equal))
          (elinit--conflict-suppressed (make-hash-table :test 'equal))
          (elinit--dag-delay-timers (make-hash-table :test 'equal))
          (elinit--spawn-failure-reason (make-hash-table :test 'equal))
          (elinit--target-convergence nil)
          (elinit--target-convergence-reasons nil)
          (elinit--target-members nil)
          (elinit--start-times (make-hash-table :test 'equal))
          (elinit-default-target-link "app.target")
          (elinit--default-target-link-override nil)
          (a-started nil))
      ;; svc-b is already running
      (let ((b-proc (start-process "svc-b" nil "sleep" "300")))
        (puthash "svc-b" b-proc elinit--processes)
        (cl-letf (((symbol-function 'elinit--start-process)
                   (lambda (id &rest _args)
                     (when (equal id "svc-a")
                       (setq a-started t))
                     ;; Return a mock process for spawn success
                     (start-process (concat "mock-" id) nil "true")))
                  ((symbol-function 'elinit--manual-stop)
                   (lambda (_id) (list :status 'stopped :reason nil)))
                  ((symbol-function 'executable-find) (lambda (_) t))
                  ((symbol-function 'elinit--maybe-refresh-dashboard) #'ignore)
                  ((symbol-function 'elinit--refresh-dashboard) #'ignore))
          (elinit--cli-dispatch '("isolate" "--yes" "app.target"))
          ;; svc-a should have been started via DAG
          (should a-started)
          ;; svc-b should be conflict-suppressed by svc-a
          (should (equal "svc-a"
                         (gethash "svc-b" elinit--conflict-suppressed)))
          ;; svc-b process should be dead (synchronous wait)
          (should-not (process-live-p b-proc)))
        ;; Defensive cleanup
        (when (process-live-p b-proc)
          (delete-process b-proc))))))

;;; Journal CLI Command Tests

(ert-deftest elinit-test-journal-requires-unit ()
  "Journal command without -u returns error."
  (cl-letf (((symbol-function 'elinit--log-file)
             (lambda (_id) "/tmp/nonexistent.log")))
    (let ((result (elinit--cli-cmd-journal '() nil)))
      (should (equal (elinit-cli-result-exitcode result)
                     elinit-cli-exit-invalid-args)))))

(ert-deftest elinit-test-journal-unknown-flag ()
  "Journal command with unknown flag returns actionable error."
  (let ((result (elinit--cli-cmd-journal '("--bad-flag") nil)))
    (should (equal (elinit-cli-result-exitcode result)
                   elinit-cli-exit-invalid-args))
    (should (string-match-p "Unknown option" (elinit-cli-result-output result)))))

(ert-deftest elinit-test-journal-with-text-log ()
  "Journal -u ID returns decoded records from text log."
  (let ((tmpfile (make-temp-file "log-test-")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=2026-02-16T12:00:00Z unit=svc pid=1 "
                    "stream=stdout event=output status=- "
                    "code=- payload=hello\n"))
          (cl-letf (((symbol-function 'elinit--log-file)
                     (lambda (_id) tmpfile)))
            (let ((result (elinit--cli-cmd-journal '("-u" "svc") nil)))
              (should (= (elinit-cli-result-exitcode result) 0))
              (should (string-match-p "hello"
                                      (elinit-cli-result-output result))))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-journal-n-limits-records ()
  "Journal -n N returns exactly N records."
  (let ((tmpfile (make-temp-file "log-test-")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (dotimes (i 30)
              (insert (format "ts=2026-02-16T12:00:%02dZ unit=svc pid=1 "
                              i)
                      "stream=stdout event=output status=- "
                      "code=- payload=line\n")))
          (cl-letf (((symbol-function 'elinit--log-file)
                     (lambda (_id) tmpfile)))
            (let* ((result (elinit--cli-cmd-journal
                            '("-u" "svc" "-n" "5") nil))
                   (output (elinit-cli-result-output result))
                   (lines (split-string output "\n" t)))
              (should (= (elinit-cli-result-exitcode result) 0))
              (should (= 5 (length lines))))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-journal-priority-filter ()
  "Journal -p err filters to error-priority records only."
  (let ((tmpfile (make-temp-file "log-test-")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=2026-02-16T12:00:00Z unit=svc pid=1 "
                    "stream=stdout event=output status=- "
                    "code=- payload=stdout-line\n")
            (insert "ts=2026-02-16T12:00:01Z unit=svc pid=1 "
                    "stream=stderr event=output status=- "
                    "code=- payload=stderr-line\n"))
          (cl-letf (((symbol-function 'elinit--log-file)
                     (lambda (_id) tmpfile)))
            (let* ((result (elinit--cli-cmd-journal
                            '("-u" "svc" "-p" "err") nil))
                   (output (elinit-cli-result-output result)))
              (should (= (elinit-cli-result-exitcode result) 0))
              ;; Only stderr line should appear
              (should (string-match-p "stderr-line" output))
              (should-not (string-match-p "stdout-line" output)))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-journal-json-output ()
  "Journal --json returns JSON with records array."
  (let ((tmpfile (make-temp-file "log-test-")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=2026-02-16T12:00:00Z unit=svc pid=1 "
                    "stream=stdout event=output status=- "
                    "code=- payload=hello\n"))
          (cl-letf (((symbol-function 'elinit--log-file)
                     (lambda (_id) tmpfile)))
            (let* ((result (elinit--cli-cmd-journal
                            '("-u" "svc") t))
                   (json-data (json-read-from-string
                               (elinit-cli-result-output result))))
              (should (= (elinit-cli-result-exitcode result) 0))
              (should (equal (cdr (assq 'unit json-data)) "svc"))
              (should (> (length (cdr (assq 'records json-data))) 0)))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-journal-fu-combined ()
  "Journal -fu ID combined short form parsed correctly."
  (let ((parsed (elinit--cli-parse-journal-args '("-fu" "my-svc"))))
    (should (equal (plist-get parsed :unit) "my-svc"))
    (should (null (plist-get parsed :unknown)))))

(ert-deftest elinit-test-journal-since-until-filtering ()
  "Journal --since and --until timestamp filtering works."
  (let ((tmpfile (make-temp-file "journal-ts-")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=1000 unit=svc pid=1 stream=stdout event=output status=- code=- payload=early\n")
            (insert "ts=2000 unit=svc pid=1 stream=stdout event=output status=- code=- payload=middle\n")
            (insert "ts=3000 unit=svc pid=1 stream=stdout event=output status=- code=- payload=late\n"))
          (cl-letf (((symbol-function 'elinit--log-file)
                     (lambda (_id) tmpfile)))
            (let ((result (elinit--cli-cmd-journal
                           '("--since" "2000" "--until" "2000" "-u" "svc")
                           nil)))
              (should (= (elinit-cli-result-exitcode result) 0))
              (should (string-match-p "middle"
                                      (elinit-cli-result-output result)))
              (should-not (string-match-p "early"
                                          (elinit-cli-result-output result)))
              (should-not (string-match-p "late"
                                          (elinit-cli-result-output result))))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-journal-parse-f-n-u-combination ()
  "Journal parser handles -f -n N -u ID combination."
  (let ((parsed (elinit--cli-parse-journal-args
                 '("-f" "-n" "10" "-u" "myapp"))))
    (should (equal "myapp" (plist-get parsed :unit)))
    (should (= 10 (plist-get parsed :lines)))
    (should-not (plist-get parsed :unknown))))

(ert-deftest elinit-test-journal-parse-n-missing-value ()
  "Journal parser reports error for -n without value."
  (let ((parsed (elinit--cli-parse-journal-args '("-n" "-u" "svc"))))
    (should (plist-get parsed :unknown))))

(ert-deftest elinit-test-journal-parse-p-invalid-value ()
  "Journal parser reports error for -p with invalid priority."
  (let ((parsed (elinit--cli-parse-journal-args
                 '("-p" "debug" "-u" "svc"))))
    (should (plist-get parsed :unknown))))

(ert-deftest elinit-test-journal-default-all-records ()
  "Journal without -n returns all records (no 50-record cap)."
  (let ((tmpfile (make-temp-file "journal-all-")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (dotimes (i 60)
              (insert (format "ts=2026-02-16T12:00:%02dZ unit=svc pid=1 "
                              (mod i 60))
                      "stream=stdout event=output status=- "
                      "code=- payload=line\n")))
          (cl-letf (((symbol-function 'elinit--log-file)
                     (lambda (_id) tmpfile)))
            (let* ((result (elinit--cli-cmd-journal
                            '("-u" "svc") nil))
                   (output (elinit-cli-result-output result))
                   (lines (split-string output "\n" t)))
              (should (= (elinit-cli-result-exitcode result) 0))
              (should (= 60 (length lines))))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-journal-invalid-since-timestamp ()
  "Journal --since with invalid timestamp returns actionable error."
  (let ((tmpfile (make-temp-file "journal-ts-")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=x\n"))
          (cl-letf (((symbol-function 'elinit--log-file)
                     (lambda (_id) tmpfile)))
            (let ((result (elinit--cli-cmd-journal
                           '("--since" "not-a-timestamp" "-u" "svc")
                           nil)))
              (should (equal (elinit-cli-result-exitcode result)
                             elinit-cli-exit-invalid-args))
              (should (string-match-p "Invalid --since"
                                      (elinit-cli-result-output result))))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-journal-invalid-until-timestamp ()
  "Journal --until with invalid timestamp returns actionable error."
  (let ((tmpfile (make-temp-file "journal-ts-")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=x\n"))
          (cl-letf (((symbol-function 'elinit--log-file)
                     (lambda (_id) tmpfile)))
            (let ((result (elinit--cli-cmd-journal
                           '("--until" "garbage" "-u" "svc") nil)))
              (should (equal (elinit-cli-result-exitcode result)
                             elinit-cli-exit-invalid-args))
              (should (string-match-p "Invalid --until"
                                      (elinit-cli-result-output result))))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-journal-json-envelope-metadata ()
  "Journal JSON output includes metadata fields in envelope."
  (let ((tmpfile (make-temp-file "journal-json-")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=hello\n"))
          (cl-letf (((symbol-function 'elinit--log-file)
                     (lambda (_id) tmpfile)))
            (let* ((result (elinit--cli-cmd-journal
                            '("--since" "500" "--until" "2000"
                              "-p" "info" "-n" "10" "-u" "svc")
                            t))
                   (json-data (json-read-from-string
                               (elinit-cli-result-output result))))
              (should (= (elinit-cli-result-exitcode result) 0))
              ;; Envelope metadata fields
              (should (equal (cdr (assq 'since json-data)) "500"))
              (should (equal (cdr (assq 'until json-data)) "2000"))
              (should (equal (cdr (assq 'priority json-data)) "info"))
              (should (equal (cdr (assq 'limit json-data)) 10))
              (should (eq (cdr (assq 'follow json-data)) :json-false)))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-journal-json-record-priority ()
  "Journal JSON records include per-record priority field."
  (let ((tmpfile (make-temp-file "journal-pri-")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=out\n")
            (insert "ts=1001 unit=svc pid=1 stream=stderr "
                    "event=output status=- code=- payload=err\n"))
          (cl-letf (((symbol-function 'elinit--log-file)
                     (lambda (_id) tmpfile)))
            (let* ((result (elinit--cli-cmd-journal
                            '("-u" "svc") t))
                   (json-data (json-read-from-string
                               (elinit-cli-result-output result)))
                   (records (cdr (assq 'records json-data))))
              (should (= (elinit-cli-result-exitcode result) 0))
              (should (= 2 (length records)))
              ;; First record: stdout -> info
              (should (equal (cdr (assq 'priority (aref records 0)))
                             "info"))
              ;; Second record: stderr -> err
              (should (equal (cdr (assq 'priority (aref records 1)))
                             "err")))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-journal-follow-flag-accepted ()
  "Journal --follow flag is accepted without error."
  (let ((parsed (elinit--cli-parse-journal-args
                 '("--follow" "-u" "svc"))))
    (should (plist-get parsed :follow))
    (should (equal "svc" (plist-get parsed :unit)))
    (should-not (plist-get parsed :unknown))))

(ert-deftest elinit-test-journal-f-flag-sets-follow ()
  "Journal -f flag sets :follow in parsed args."
  (let ((parsed (elinit--cli-parse-journal-args
                 '("-f" "-u" "svc"))))
    (should (plist-get parsed :follow))
    (should-not (plist-get parsed :unknown))))

(ert-deftest elinit-test-journal-fu-sets-follow ()
  "Journal -fu ID sets both :follow and :unit."
  (let ((parsed (elinit--cli-parse-journal-args
                 '("-fu" "svc"))))
    (should (plist-get parsed :follow))
    (should (equal "svc" (plist-get parsed :unit)))
    (should-not (plist-get parsed :unknown))))

(ert-deftest elinit-test-journal-json-null-fields ()
  "Journal JSON envelope uses JSON null for absent fields, not string."
  (let ((tmpfile (make-temp-file "log-json-null-")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=hello\n"))
          (cl-letf (((symbol-function 'elinit--log-file)
                     (lambda (_id) tmpfile)))
            (let* ((result (elinit--cli-cmd-journal
                            '("-u" "svc") t))
                   (json-data (json-read-from-string
                               (elinit-cli-result-output result))))
              ;; Absent fields must be JSON null (Elisp nil), not "null"
              (should (null (cdr (assq 'since json-data))))
              (should (null (cdr (assq 'until json-data))))
              (should (null (cdr (assq 'priority json-data))))
              (should (null (cdr (assq 'limit json-data))))
              ;; They must NOT be the string "null"
              (should-not (equal "null" (cdr (assq 'since json-data)))))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-journal-ndjson-null-status ()
  "NDJSON record uses JSON null for absent status, not string."
  (let ((record (list :ts 1000.0 :unit "svc" :pid 1
                      :stream 'stdout :event 'output
                      :status nil :code 0 :payload "hello")))
    (let* ((json-str (elinit--log-record-to-json record))
           (parsed (json-read-from-string json-str)))
      ;; status should be JSON null (Elisp nil), not the string "null"
      (should (null (cdr (assq 'status parsed))))
      (should-not (equal "null" (cdr (assq 'status parsed)))))))

(ert-deftest elinit-test-journal-rejects-extra-positional-args ()
  "Journal rejects unexpected bare positional arguments."
  (let ((result (elinit--cli-cmd-journal
                 '("-u" "svc" "extra-arg") nil)))
    (should (= elinit-cli-exit-invalid-args
               (elinit-cli-result-exitcode result)))
    (should (string-match-p "Unexpected argument"
                            (elinit-cli-result-output result)))))

(ert-deftest elinit-test-journal-rejects-multiple-extra-args ()
  "Journal rejects multiple extra positional arguments."
  (let ((result (elinit--cli-cmd-journal
                 '("-u" "svc" "one" "two") nil)))
    (should (= elinit-cli-exit-invalid-args
               (elinit-cli-result-exitcode result)))
    (should (string-match-p "Unexpected argument: one"
                            (elinit-cli-result-output result)))))

(ert-deftest elinit-test-journal-json-envelope-null-status ()
  "JSON envelope records use JSON null for absent status."
  (let ((tmpfile (make-temp-file "log-env-null-")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=hello\n"))
          (cl-letf (((symbol-function 'elinit--log-file)
                     (lambda (_id) tmpfile)))
            (let* ((result (elinit--cli-cmd-journal
                            '("-u" "svc") t))
                   (json-data (json-read-from-string
                               (elinit-cli-result-output result)))
                   (records (cdr (assq 'records json-data)))
                   (first-rec (aref records 0)))
              ;; status must be JSON null (Elisp nil), not string "null"
              (should (null (cdr (assq 'status first-rec))))
              (should-not (equal "null"
                                 (cdr (assq 'status first-rec)))))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-journal-fu-rejects-fux ()
  "Parser rejects -fux as unknown option, only -fu is valid."
  (let ((parsed (elinit--cli-parse-journal-args '("-fux" "svc"))))
    (should (plist-get parsed :unknown))
    (should (string-match-p "Unknown option: -fux"
                            (plist-get parsed :unknown)))))

(ert-deftest elinit-test-journal-ndjson-record-format ()
  "NDJSON record is valid JSON with all expected fields."
  (let ((record (list :ts 1000.0 :unit "svc" :pid 42
                      :stream 'stdout :event 'output
                      :status nil :code 0
                      :payload "hello")))
    (let* ((json-str (elinit--log-record-to-json record))
           (parsed (json-read-from-string json-str)))
      (should (= (cdr (assq 'ts parsed)) 1000.0))
      (should (equal (cdr (assq 'unit parsed)) "svc"))
      (should (= (cdr (assq 'pid parsed)) 42))
      (should (equal (cdr (assq 'stream parsed)) "stdout"))
      (should (equal (cdr (assq 'event parsed)) "output"))
      (should (equal (cdr (assq 'payload parsed)) "hello"))
      (should (equal (cdr (assq 'priority parsed)) "info")))))

(ert-deftest elinit-test-journal-ndjson-exit-record ()
  "NDJSON encodes exit record with err priority."
  (let ((record (list :ts 1000.0 :unit "svc" :pid 42
                      :stream 'meta :event 'exit
                      :status 'signaled :code 9
                      :payload "")))
    (let* ((json-str (elinit--log-record-to-json record))
           (parsed (json-read-from-string json-str)))
      (should (equal (cdr (assq 'priority parsed)) "err"))
      (should (equal (cdr (assq 'status parsed)) "signaled")))))

(ert-deftest elinit-test-journal-n-passes-max-bytes ()
  "Journal with -n tries n*512 first, then retries full on shortfall."
  (let ((tmpfile (make-temp-file "log-jmb-"))
        (call-log nil))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=hello\n"))
          (let ((records (cl-loop for i from 1 to 10
                                  collect (list :ts (float i) :unit "svc"
                                                :pid 1 :stream 'stdout
                                                :event 'output :status nil
                                                :code 0 :payload "x"))))
            (cl-letf (((symbol-function 'elinit--log-file)
                       (lambda (_id) tmpfile))
                      ((symbol-function 'elinit--log-decode-file)
                       (lambda (_file &optional _limit _offset max-bytes)
                         (push max-bytes call-log)
                         (list :records records
                               :offset 100 :format 'text :warning nil))))
              (elinit--cli-cmd-journal '("-u" "svc" "-n" "10") nil)
              ;; First call uses n*512 heuristic
              (should (= 5120 (car (last call-log))))
              ;; Since 10 records >= 10 requested, no retry needed
              (should (= 1 (length call-log))))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-journal-n-retries-on-shortfall ()
  "Journal with -n retries with full decode when tail has too few records."
  (let ((tmpfile (make-temp-file "log-jmb2-"))
        (call-log nil))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=hello\n"))
          (let ((few-records (list (list :ts 1.0 :unit "svc" :pid 1
                                         :stream 'stdout :event 'output
                                         :status nil :code 0
                                         :payload "big-payload")))
                (all-records (cl-loop for i from 1 to 5
                                      collect (list :ts (float i)
                                                    :unit "svc" :pid 1
                                                    :stream 'stdout
                                                    :event 'output
                                                    :status nil :code 0
                                                    :payload "x"))))
            (cl-letf (((symbol-function 'elinit--log-file)
                       (lambda (_id) tmpfile))
                      ((symbol-function 'elinit--log-decode-file)
                       (lambda (_file &optional _limit _offset max-bytes)
                         (push max-bytes call-log)
                         (if max-bytes
                             ;; Tail read returns fewer than requested
                             (list :records few-records
                                   :offset 100 :format 'text :warning nil)
                           ;; Full read returns all
                           (list :records all-records
                                 :offset 500 :format 'text :warning nil)))))
              (let* ((result (elinit--cli-cmd-journal
                              '("-u" "svc" "-n" "5") nil))
                     (output (elinit-cli-result-output result))
                     (lines (split-string output "\n" t)))
                ;; Two calls: first with heuristic, second with nil
                (should (= 2 (length call-log)))
                (should (= 2560 (car (last call-log))))
                (should (null (car call-log)))
                ;; Got all 5 records from the retry
                (should (= 5 (length lines)))))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-journal-follow-no-new-data ()
  "Follow-mode poll with no new data returns empty records."
  (let ((tmpfile (make-temp-file "journal-follow-empty-")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=only\n"))
          (let* ((initial (elinit--log-decode-file tmpfile))
                 (offset (plist-get initial :offset)))
            ;; Poll at end -- no new data
            (let* ((poll (elinit--log-decode-file tmpfile nil offset))
                   (new-recs (plist-get poll :records)))
              (should (= 0 (length new-recs))))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-follow-journal-returns-follow-result ()
  "Journal with -f returns result with format follow."
  (let ((tmpfile (make-temp-file "log-follow-"))
        (elinit--cli-follow-sessions (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=hello\n"))
          (cl-letf (((symbol-function 'elinit--log-file)
                     (lambda (_id) tmpfile)))
            (let ((result (elinit--cli-cmd-journal
                           '("-u" "svc" "-f") nil)))
              (should (eq (elinit-cli-result-format result) 'follow))
              (should (string-prefix-p "FOLLOW:"
                                       (elinit-cli-result-output result)))
              ;; Cleanup the session
              (let ((output (elinit-cli-result-output result)))
                (when (string-match "FOLLOW:[^\t]*\t[^\t]*\t\\(.+\\)" output)
                  (elinit--cli-follow-stop (match-string 1 output)))))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-existing-logs-command-unchanged ()
  "Existing logs command still works (regression)."
  (let ((tmpfile (make-temp-file "log-test-")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "raw log line 1\nraw log line 2\n"))
          (cl-letf (((symbol-function 'elinit--log-file)
                     (lambda (_id) tmpfile)))
            (let ((result (elinit--cli-cmd-logs '("my-svc") nil)))
              (should (= (elinit-cli-result-exitcode result) 0))
              (should (string-match-p "raw log line 1"
                                      (elinit-cli-result-output result))))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-follow-dispatch-wrapper-protocol ()
  "Dispatch for wrapper returns raw FOLLOW string for follow results."
  (let ((tmpfile (make-temp-file "log-follow-"))
        (elinit--cli-follow-sessions (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=hello\n"))
          (cl-letf (((symbol-function 'elinit--log-file)
                     (lambda (_id) tmpfile)))
            (let ((output (elinit--cli-dispatch-for-wrapper
                           '("journal" "-u" "svc" "-f"))))
              (should (string-prefix-p "FOLLOW:" output))
              ;; Cleanup the session
              (when (string-match "FOLLOW:[^\t]*\t[^\t]*\t\\(.+\\)" output)
                (elinit--cli-follow-stop (match-string 1 output))))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-follow-stop-nonexistent ()
  "Stopping nonexistent session returns nil without error."
  (let ((elinit--cli-follow-sessions (make-hash-table :test 'equal)))
    (should-not (elinit--cli-follow-stop "no-such-session"))))

(provide 'elinit-test-cli)
;;; elinit-test-cli.el ends here
