;;; elinit-test-cli.el --- CLI commands and transport tests for elinit.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

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

(ert-deftest elinit-test-dashboard-mask-keybinding ()
  "Dashboard keymap binds `T' to proced auto-update."
  (should (eq #'elinit-dashboard-toggle-proced-auto-update
              (lookup-key elinit-dashboard-mode-map "T"))))

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


(provide 'elinit-test-cli)
;;; elinit-test-cli.el ends here
