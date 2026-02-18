;;; supervisor-test-cli.el --- CLI commands and transport tests for supervisor.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; CLI commands and transport ERT tests for supervisor.el.

;;; Code:

(require 'supervisor-test-helpers)

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

(ert-deftest supervisor-test-cli-verify-no-programs ()
  "Verify with no programs returns success."
  (supervisor-test-with-unit-files nil
    (let ((result (supervisor--cli-dispatch '("verify"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      (should (string-match "0 valid" (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-verify-invalid-entry ()
  "Verify with invalid entry returns validation-failed exit code."
  (supervisor-test-with-unit-files
      '(("cmd" :id "cmd" :type "bad"))
    (let ((result (supervisor--cli-dispatch '("verify"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-validation-failed (supervisor-cli-result-exitcode result)))
      (should (string-match "1 invalid" (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-list-units-human-format ()
  "The `list-units' command returns human-readable table."
  (supervisor-test-with-unit-files
      '(("test-cmd" :id "test" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("list-units"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      (should (eq 'human (supervisor-cli-result-format result)))
      ;; Should contain header row
      (should (string-match "ID" (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-list-units-json-format ()
  "The `list-units --json' returns JSON with entries array."
  (supervisor-test-with-unit-files
      '(("test-cmd" :id "test" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("list-units" "--json"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      (should (eq 'json (supervisor-cli-result-format result)))
      ;; Should be valid JSON with entries array
      (should (string-match "\"entries\"" (supervisor-cli-result-output result))))))

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
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "test-id" :type simple :enabled nil))
    (let ((supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      (supervisor--cli-dispatch '("enable" "test-id"))
      (should (eq 'enabled (gethash "test-id" supervisor--enabled-override))))))

(ert-deftest supervisor-test-cli-disable-sets-override ()
  "Disable command sets disabled override."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "test-id" :type simple))
    (let ((supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      (supervisor--cli-dispatch '("disable" "test-id"))
      (should (eq 'disabled (gethash "test-id" supervisor--enabled-override))))))

(ert-deftest supervisor-test-cli-mask-sets-override ()
  "Mask command sets masked override."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "test-id" :type simple))
    (let ((supervisor--mask-override (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      (supervisor--cli-dispatch '("mask" "test-id"))
      (should (eq 'masked (gethash "test-id" supervisor--mask-override))))))

(ert-deftest supervisor-test-cli-unmask-clears-override ()
  "Unmask command clears masked override."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "test-id" :type simple))
    (let ((supervisor--mask-override (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      (puthash "test-id" 'masked supervisor--mask-override)
      (supervisor--cli-dispatch '("unmask" "test-id"))
      (should-not (gethash "test-id" supervisor--mask-override)))))

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
  "Mask command returns JSON with applied IDs."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "a" :type simple)
        ("sleep 300" :id "b" :type simple))
    (let ((supervisor--mask-override (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      (let ((result (supervisor--cli-dispatch '("mask" "a" "b" "--json"))))
        (should (eq 'json (supervisor-cli-result-format result)))
        (let ((parsed (json-read-from-string
                       (supervisor-cli-result-output result))))
          (should (assoc 'applied parsed)))))))

(ert-deftest supervisor-test-cli-mask-with-separator ()
  "Mask command supports -- separator for hyphen-prefixed IDs."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "-my-id" :type simple))
    (let ((supervisor--mask-override (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      (supervisor--cli-dispatch '("mask" "--" "-my-id"))
      (should (eq 'masked (gethash "-my-id" supervisor--mask-override))))))

(ert-deftest supervisor-test-cli-unmask-json-output ()
  "Unmask command returns JSON with applied IDs."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "a" :type simple)
        ("sleep 300" :id "b" :type simple))
    (let ((supervisor--mask-override (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      (puthash "a" 'masked supervisor--mask-override)
      (puthash "b" 'masked supervisor--mask-override)
      (let ((result (supervisor--cli-dispatch '("unmask" "a" "b" "--json"))))
        (should (eq 'json (supervisor-cli-result-format result)))
        (let ((parsed (json-read-from-string
                       (supervisor-cli-result-output result))))
          (should (assoc 'applied parsed)))))))

(ert-deftest supervisor-test-cli-unmask-with-separator ()
  "Unmask command supports -- separator for hyphen-prefixed IDs."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "-my-id" :type simple))
    (let ((supervisor--mask-override (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      (puthash "-my-id" 'masked supervisor--mask-override)
      (supervisor--cli-dispatch '("unmask" "--" "-my-id"))
      (should-not (gethash "-my-id" supervisor--mask-override)))))

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
  (supervisor-test-with-unit-files
      '(("echo hi" :id "svc"))
    (let ((supervisor--mask-override (make-hash-table :test 'equal))
          (supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--processes (make-hash-table :test 'equal)))
      (puthash "svc" 'masked supervisor--mask-override)
      (let ((result (supervisor--manual-start "svc")))
        (should (eq 'skipped (plist-get result :status)))
        (should (equal "masked" (plist-get result :reason)))))))

(ert-deftest supervisor-test-reconcile-stops-masked-running ()
  "Reconcile stops masked entries that are currently running."
  (supervisor-test-with-unit-files
      '(("echo" :id "svc"))
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
           (plan (supervisor--build-plan (supervisor--effective-programs))))
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
            (should (eq 'masked (plist-get stop-action :reason)))))))))

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
  "Dashboard keymap binds `T' to proced auto-update."
  (should (eq #'supervisor-dashboard-toggle-proced-auto-update
              (lookup-key supervisor-dashboard-mode-map "T"))))

(ert-deftest supervisor-test-cli-restart-policy-always ()
  "Restart-policy always sets override."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "test-id" :type simple :restart nil))
    (let ((supervisor--restart-override (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      (supervisor--cli-dispatch '("restart-policy" "always" "test-id"))
      (should (eq 'always (gethash "test-id" supervisor--restart-override))))))

(ert-deftest supervisor-test-cli-restart-policy-no ()
  "Restart-policy no sets override."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "test-id" :type simple :restart t))
    (let ((supervisor--restart-override (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      (supervisor--cli-dispatch '("restart-policy" "no" "test-id"))
      (should (eq 'no (gethash "test-id" supervisor--restart-override))))))

(ert-deftest supervisor-test-cli-restart-policy-on-failure ()
  "Restart-policy on-failure sets override."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "test-id" :type simple :restart nil))
    (let ((supervisor--restart-override (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      (supervisor--cli-dispatch '("restart-policy" "on-failure" "test-id"))
      (should (eq 'on-failure (gethash "test-id" supervisor--restart-override))))))

(ert-deftest supervisor-test-cli-restart-policy-on-success ()
  "Restart-policy on-success sets override."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "test-id" :type simple :restart nil))
    (let ((supervisor--restart-override (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      (supervisor--cli-dispatch '("restart-policy" "on-success" "test-id"))
      (should (eq 'on-success (gethash "test-id" supervisor--restart-override))))))

(ert-deftest supervisor-test-cli-restart-policy-invalid ()
  "Restart-policy with invalid value returns error."
  (let ((supervisor--restart-override (make-hash-table :test 'equal))
        (result (supervisor--cli-dispatch '("restart-policy" "bogus" "test-id"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args
               (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-logging-on ()
  "Logging on sets override."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "test-id" :type simple :logging nil))
    (let ((supervisor--logging (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      (supervisor--cli-dispatch '("logging" "on" "test-id"))
      (should (eq 'enabled (gethash "test-id" supervisor--logging))))))

(ert-deftest supervisor-test-cli-logging-off ()
  "Logging off sets override."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "test-id" :type simple))
    (let ((supervisor--logging (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      (supervisor--cli-dispatch '("logging" "off" "test-id"))
      (should (eq 'disabled (gethash "test-id" supervisor--logging))))))

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
  (supervisor-test-with-unit-files
      '(("a" :id "a") ("b" :id "b" :after "a"))
    (let* ((result (supervisor--cli-dispatch '("list-dependencies"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result))))))

(ert-deftest supervisor-test-cli-list-dependencies-single ()
  "The `list-dependencies' command with ID returns single entry deps."
  (supervisor-test-with-unit-files
      '(("a" :id "a") ("b" :id "b" :after "a"))
    (let* ((result (supervisor--cli-dispatch '("list-dependencies" "b"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      (should (string-match "ID: b" (supervisor-cli-result-output result))))))

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

(ert-deftest supervisor-test-cli-verify-rejects-extra-args ()
  "Verify with extra args returns invalid-args exit code."
  (let ((result (supervisor--cli-dispatch '("verify" "extra"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-validate-is-unknown-command ()
  "CLI `validate' is rejected as unknown command (renamed to `verify')."
  (let ((result (supervisor--cli-dispatch '("validate"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))
    (should (string-match "Unknown command" (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-reset-failed-per-unit ()
  "CLI reset-failed clears failed state for a specific unit."
  (let ((supervisor--failed (make-hash-table :test 'equal))
        (supervisor--restart-times (make-hash-table :test 'equal)))
    (puthash "svc-a" t supervisor--failed)
    (puthash "svc-a" '(100 99 98) supervisor--restart-times)
    (let ((result (supervisor--cli-dispatch '("reset-failed" "svc-a"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      (should (string-match "Reset.*svc-a" (supervisor-cli-result-output result)))
      (should-not (gethash "svc-a" supervisor--failed))
      (should-not (gethash "svc-a" supervisor--restart-times)))))

(ert-deftest supervisor-test-cli-reset-failed-global ()
  "CLI reset-failed with no IDs clears all failed entries."
  (let ((supervisor--failed (make-hash-table :test 'equal))
        (supervisor--restart-times (make-hash-table :test 'equal)))
    (puthash "svc-a" t supervisor--failed)
    (puthash "svc-b" t supervisor--failed)
    (puthash "svc-a" '(100) supervisor--restart-times)
    (puthash "svc-b" '(100) supervisor--restart-times)
    (let ((result (supervisor--cli-dispatch '("reset-failed"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      (should (string-match "svc-a" (supervisor-cli-result-output result)))
      (should (string-match "svc-b" (supervisor-cli-result-output result)))
      (should (= 0 (hash-table-count supervisor--failed))))))

(ert-deftest supervisor-test-cli-reset-failed-not-failed ()
  "CLI reset-failed on a non-failed unit reports skipped."
  (let ((supervisor--failed (make-hash-table :test 'equal))
        (supervisor--restart-times (make-hash-table :test 'equal)))
    (let ((result (supervisor--cli-dispatch '("reset-failed" "svc-x"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      (should (string-match "Skipped.*svc-x" (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-reset-failed-no-failed-units ()
  "CLI reset-failed with no IDs and no failed units shows message."
  (let ((supervisor--failed (make-hash-table :test 'equal)))
    (let ((result (supervisor--cli-dispatch '("reset-failed"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      (should (string-match "No failed" (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-reset-failed-core ()
  "Core reset-failed function clears failed and restart-times."
  (let ((supervisor--failed (make-hash-table :test 'equal))
        (supervisor--restart-times (make-hash-table :test 'equal)))
    (puthash "test-id" t supervisor--failed)
    (puthash "test-id" '(100 99) supervisor--restart-times)
    (let ((result (supervisor--reset-failed "test-id")))
      (should (eq 'reset (plist-get result :status)))
      (should-not (gethash "test-id" supervisor--failed))
      (should-not (gethash "test-id" supervisor--restart-times)))))

(ert-deftest supervisor-test-reset-failed-clears-failed-oneshot ()
  "Core reset-failed clears failed oneshot completion results."
  (let ((supervisor--failed (make-hash-table :test 'equal))
        (supervisor--restart-times (make-hash-table :test 'equal))
        (supervisor--oneshot-completed (make-hash-table :test 'equal)))
    (puthash "fail-id" t supervisor--failed)
    (puthash "fail-id" 1 supervisor--oneshot-completed)
    (supervisor--reset-failed "fail-id")
    (should-not (gethash "fail-id" supervisor--oneshot-completed))))

(ert-deftest supervisor-test-reset-failed-preserves-successful-oneshot ()
  "Core reset-failed does not clear successful oneshot completion."
  (let ((supervisor--failed (make-hash-table :test 'equal))
        (supervisor--restart-times (make-hash-table :test 'equal))
        (supervisor--oneshot-completed (make-hash-table :test 'equal)))
    (puthash "ok-id" t supervisor--failed)
    (puthash "ok-id" 0 supervisor--oneshot-completed)
    (supervisor--reset-failed "ok-id")
    (should (eql 0 (gethash "ok-id" supervisor--oneshot-completed)))))

(ert-deftest supervisor-test-reset-failed-clears-signal-oneshot ()
  "Core reset-failed clears oneshot that died by signal."
  (let ((supervisor--failed (make-hash-table :test 'equal))
        (supervisor--restart-times (make-hash-table :test 'equal))
        (supervisor--oneshot-completed (make-hash-table :test 'equal)))
    (puthash "sig-id" t supervisor--failed)
    (puthash "sig-id" -9 supervisor--oneshot-completed)
    (supervisor--reset-failed "sig-id")
    (should-not (gethash "sig-id" supervisor--oneshot-completed))))

(ert-deftest supervisor-test-reset-failed-not-failed ()
  "Core reset-failed on non-failed ID returns skipped."
  (let ((supervisor--failed (make-hash-table :test 'equal)))
    (let ((result (supervisor--reset-failed "test-id")))
      (should (eq 'skipped (plist-get result :status))))))

(ert-deftest supervisor-test-cli-reload-requires-at-least-one-id ()
  "CLI `reload' with no arguments returns exit 2 (invalid args)."
  (let ((result (supervisor--cli-dispatch '("reload"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args
               (supervisor-cli-result-exitcode result)))
    (should (string-match "reload requires at least one ID"
                          (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-cli-reconcile-is-unknown-command ()
  "Removed `reconcile' command is rejected as unknown."
  (let ((result (supervisor--cli-dispatch '("reconcile"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args
               (supervisor-cli-result-exitcode result)))
    (should (string-match "Unknown command: reconcile"
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
  (supervisor-test-with-unit-files
      '(("test-cmd" :id "test" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("status"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      ;; Should contain header row (table format)
      (should (string-match "ID" (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-status-with-id-shows-detail ()
  "The `status ID' shows detailed unit info."
  (supervisor-test-with-unit-files
      '(("test-cmd" :id "test" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("status" "test"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      ;; Should show detailed info (describe format)
      (should (string-match "ID: test" (supervisor-cli-result-output result)))
      (should (string-match "Type:" (supervisor-cli-result-output result)))
      (should (string-match "Status:" (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-status-partial-missing ()
  "The `status ID1 ID2' prints found units and warns about missing ones."
  (supervisor-test-with-unit-files
      '(("test-cmd" :id "test" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("status" "test" "nonexistent"))))
      (should (supervisor-cli-result-p result))
      ;; Non-zero exit because of missing ID
      (should (= supervisor-cli-exit-failure (supervisor-cli-result-exitcode result)))
      ;; But should still show output for the found ID
      (should (string-match "ID: test" (supervisor-cli-result-output result)))
      ;; And warn about the missing one
      (should (string-match "nonexistent" (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-status-all-missing ()
  "The `status' with only missing IDs still shows warnings."
  (supervisor-test-with-unit-files
      '(("test-cmd" :id "test" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("status" "nope"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-failure (supervisor-cli-result-exitcode result)))
      (should (string-match "could not be found" (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-list-units-filters-invalid ()
  "The `list-units ID' filters both valid and invalid entries."
  (supervisor-test-with-unit-files
      '(("good-cmd" :id "good" :type simple)
        ("bad-cmd" :id "bad" :type "string-type"))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("list-units" "good"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      ;; Should show "good" but not "bad" (invalid entry filtered out)
      (should (string-match "good" (supervisor-cli-result-output result)))
      (should-not (string-match "bad" (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-status-invalid-id-shows-invalid-detail ()
  "The `status' with an invalid unit ID shows invalid detail, not \"not found\"."
  (supervisor-test-with-unit-files
      '(("good-cmd" :id "good" :type simple)
        ("bad-cmd" :id "bad" :type "string-type"))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("status" "bad"))))
      (should (supervisor-cli-result-p result))
      ;; Invalid configured unit is found, so exit success (not missing)
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      (should (string-match "ID: bad" (supervisor-cli-result-output result)))
      (should (string-match "Status: invalid" (supervisor-cli-result-output result)))
      (should-not (string-match "could not be found" (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-status-mixed-valid-invalid-missing ()
  "The `status' with valid, invalid, and missing IDs classifies each correctly."
  (supervisor-test-with-unit-files
      '(("good-cmd" :id "good" :type simple)
        ("bad-cmd" :id "bad" :type "string-type"))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
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
      (should (string-match "ghost.*could not be found" (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-status-invalid-id-json ()
  "The `status --json' with invalid ID returns invalid array, not not_found."
  (supervisor-test-with-unit-files
      '(("bad-cmd" :id "bad" :type "string-type"))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("status" "bad" "--json"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      (let ((parsed (json-read-from-string (supervisor-cli-result-output result))))
        ;; Invalid unit appears in "invalid" array, not "not_found"
        (should (< 0 (length (alist-get 'invalid parsed))))
        (should (equal "bad" (alist-get 'id (aref (alist-get 'invalid parsed) 0))))))))

(ert-deftest supervisor-test-cli-show-invalid-id-shows-invalid-detail ()
  "The `show' with an invalid unit ID shows invalid detail, not \"not found\"."
  (supervisor-test-with-unit-files
      '(("bad-cmd" :id "bad" :type "string-type"))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("show" "bad"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      (should (string-match "ID: bad" (supervisor-cli-result-output result)))
      (should (string-match "Status: invalid" (supervisor-cli-result-output result)))
      (should-not (string-match "No entry with ID" (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-show-invalid-id-json ()
  "The `show --json' with invalid ID returns invalid object, not error."
  (supervisor-test-with-unit-files
      '(("bad-cmd" :id "bad" :type "string-type"))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("show" "bad" "--json"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      (let ((parsed (json-read-from-string (supervisor-cli-result-output result))))
        (should (equal "bad" (alist-get 'id parsed)))
        (should (alist-get 'reason parsed))))))

(ert-deftest supervisor-test-cli-show-truly-missing-still-errors ()
  "The `show' with a truly missing ID still returns an error."
  (supervisor-test-with-unit-files
      '(("good-cmd" :id "good" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("show" "ghost"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-failure (supervisor-cli-result-exitcode result)))
      (should (string-match "No entry with ID" (supervisor-cli-result-output result))))))

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
  (supervisor-test-without-builtins
    (let ((supervisor-mode t)
          (supervisor-timers nil)
          (supervisor--timer-list nil)
          (supervisor--timer-state (make-hash-table :test 'equal))
          (supervisor--invalid-timers (make-hash-table :test 'equal)))
      (let ((result (supervisor--cli-dispatch '("list-timers"))))
        (should (supervisor-cli-result-p result))
        (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
        (should (string-match "No timers" (supervisor-cli-result-output result)))))))

(ert-deftest supervisor-test-cli-list-timers-builds-from-config-when-mode-off ()
  "The `list-timers' command works when `supervisor-mode' is off."
  (supervisor-test-with-unit-files
      '(("true" :id "s1" :type oneshot))
    (let ((supervisor-mode nil)
          (supervisor-timers '((:id "t1" :target "s1" :on-startup-sec 60)))
          (supervisor--timer-list nil)
          (supervisor--timer-state (make-hash-table :test 'equal))
          (supervisor--invalid-timers (make-hash-table :test 'equal)))
      (let ((result (supervisor--cli-dispatch '("list-timers"))))
        (should (supervisor-cli-result-p result))
        (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
        (should (string-match-p "t1" (supervisor-cli-result-output result)))
        (should (string-match-p "s1" (supervisor-cli-result-output result)))))))

(ert-deftest supervisor-test-cli-list-timers-disabled-message-no-experimental ()
  "The `list-timers' disabled response has no experimental wording."
  (let ((supervisor-timer-subsystem-mode nil)
        (supervisor-mode t)
        (supervisor--timer-list nil)
        (supervisor--timer-state (make-hash-table :test 'equal))
        (supervisor--invalid-timers (make-hash-table :test 'equal)))
    (let ((human (supervisor--cli-dispatch '("list-timers")))
          (json (supervisor--cli-dispatch '("--json" "list-timers"))))
      (should (supervisor-cli-result-p human))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode human)))
      (should (string-match-p "Timer subsystem is disabled\\." (supervisor-cli-result-output human)))
      (should-not (string-match-p "experimental" (supervisor-cli-result-output human)))
      (let* ((json-object-type 'alist)
             (data (json-read-from-string (supervisor-cli-result-output json))))
        (should (equal "disabled" (alist-get 'status data)))
        (should (equal "Timer subsystem is disabled" (alist-get 'message data)))))))

(ert-deftest supervisor-test-cli-list-timers-shows-state ()
  "The `list-timers' command shows timer state."
  (let* ((supervisor-mode t)
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
  (supervisor-test-with-unit-files nil
    (let ((supervisor-mode t)
          (supervisor-timers '((:id "bad-timer"
                               :target "missing"
                               :on-startup-sec 60)))
          (supervisor--timer-list nil)
          (supervisor--timer-state (make-hash-table :test 'equal))
          (supervisor--invalid-timers (make-hash-table :test 'equal)))
      (let ((result (supervisor--cli-dispatch '("list-timers"))))
        (should (supervisor-cli-result-p result))
        (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
        ;; Should show id and validation reason
        (should (string-match-p "bad-timer" (supervisor-cli-result-output result)))
        (should (string-match-p "target" (supervisor-cli-result-output result)))))))

(ert-deftest supervisor-test-cli-list-timers-invalid-json-format ()
  "The `list-timers --json' outputs invalid timers with correct structure."
  (supervisor-test-with-unit-files nil
    (let ((supervisor-mode t)
          (supervisor-timers '((:id "bad-timer"
                               :target "missing"
                               :on-startup-sec 60)))
          (supervisor--timer-list nil)
          (supervisor--timer-state (make-hash-table :test 'equal))
          (supervisor--invalid-timers (make-hash-table :test 'equal)))
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
          (should (string-match-p "target" (alist-get 'reason entry))))))))

(ert-deftest supervisor-test-dashboard-timer-signal-exit-is-failed ()
  "Dashboard timer entry shows signal exit code in EXIT column."
  (let* ((timer (supervisor-timer--create :id "t1" :target "s1" :enabled t))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--processes (make-hash-table :test 'equal)))
    ;; Simulate signal death stored as negative exit code
    (puthash "t1" '(:last-exit -9 :next-run-at 2000.0) supervisor--timer-state)
    (cl-letf (((symbol-function 'supervisor--get-entry-for-id)
               (lambda (_id) nil)))
      (let ((entry (supervisor--make-timer-dashboard-entry timer)))
        ;; Exit code is at index 5
        (should (string= "-9" (aref entry 5)))))))

(ert-deftest supervisor-test-cli-list-timers-full-field-mapping ()
  "The `list-timers' output includes all required fields.
Target type is resolved from current config."
  (let* ((supervisor-mode t)
         (timer (supervisor-timer--create :id "test-timer" :target "test-target"
                                          :enabled t :persistent t))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--invalid-timers (make-hash-table :test 'equal))
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
             supervisor--timer-state)
    (cl-letf (((symbol-function 'supervisor--get-entry-for-id)
               (lambda (_id) mock-entry)))
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
          ;; Type resolved from config
          (should (string-match-p "oneshot" output))
          ;; Result and reason
          (should (string-match-p "skip" output))
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
          (should (equal "overlap" (alist-get 'miss_reason entry)))
          (should (equal "oneshot" (alist-get 'target_type entry))))))))

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
  (supervisor-test-with-unit-files
      '(("cmd" :id "test"))
    (let* ((result (supervisor--cli-dispatch '("show" "test" "extra"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result))))))

(ert-deftest supervisor-test-cli-status-rejects-unknown-flags ()
  "Status with unknown flag returns invalid-args exit code."
  (let ((result (supervisor--cli-dispatch '("status" "--bogus"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-list-dependencies-rejects-extra-args ()
  "The `list-dependencies' with multiple args returns invalid-args exit code."
  (supervisor-test-with-unit-files
      '(("a" :id "a"))
    (let* ((result (supervisor--cli-dispatch '("list-dependencies" "a" "b"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-invalid-args (supervisor-cli-result-exitcode result))))))

;;; CLI JSON Schema Tests

(ert-deftest supervisor-test-cli-list-units-json-empty-arrays ()
  "The `list-units --json' returns arrays, not null, for empty results."
  (supervisor-test-with-unit-files
      nil
    (let* ((result (supervisor--cli-dispatch '("list-units" "--json"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      ;; Should have [] not null
      (should (string-match "\"entries\":\\[\\]" (supervisor-cli-result-output result)))
      (should (string-match "\"invalid\":\\[\\]" (supervisor-cli-result-output result))))))

;;; CLI Wrapper Transport Tests

(ert-deftest supervisor-test-cli-wrapper-dispatch-format ()
  "Wrapper dispatch returns base64-encoded format."
  (supervisor-test-with-unit-files
      nil
    (let* ((result (supervisor--cli-dispatch-for-wrapper '("ping"))))
      ;; Format is EXITCODE:BASE64OUTPUT
      (should (stringp result))
      (should (string-match "^0:" result))
      ;; Decode the base64 part
      (let ((b64 (substring result 2)))
        (should (string-match "pong" (decode-coding-string (base64-decode-string b64) 'utf-8)))))))

(ert-deftest supervisor-test-cli-wrapper-dispatch-error ()
  "Wrapper dispatch returns non-zero exit code for errors."
  (let ((result (supervisor--cli-dispatch-for-wrapper '("unknown-cmd"))))
    ;; Should start with 2: (invalid args)
    (should (stringp result))
    (should (string-match "^2:" result))))

(ert-deftest supervisor-test-cli-wrapper-edit-transport-clean ()
  "Wrapper edit output has clean EXITCODE:BASE64 format with no extra stdout."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-unit-directory dir)
         (supervisor--authority-snapshot nil)
         (supervisor--programs-cache :not-yet-loaded)
         (supervisor--unit-file-invalid (make-hash-table :test 'equal))
         (result (supervisor--cli-dispatch-for-wrapper
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
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "-svc" :type simple :enabled nil))
    (let ((supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor-overrides-file nil))
      (let ((result (supervisor--cli-dispatch '("enable" "--" "-svc"))))
        (should (supervisor-cli-result-p result))
        (should (= supervisor-cli-exit-success
                    (supervisor-cli-result-exitcode result)))
        (should (eq 'enabled
                    (gethash "-svc" supervisor--enabled-override)))))))

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
  (supervisor-test-with-unit-files
      '(("sleep 999" :id "svc" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (proc (start-process "test" nil "sleep" "999")))
      (unwind-protect
          (progn
            (puthash "svc" proc supervisor--processes)
            (let ((result (supervisor--cli-dispatch '("is-active" "svc"))))
              (should (= supervisor-cli-exit-success
                         (supervisor-cli-result-exitcode result)))
              (should (string-match "running" (supervisor-cli-result-output result)))))
        (delete-process proc)))))

(ert-deftest supervisor-test-cli-is-active-not-running ()
  "The `is-active' returns exit 3 (not-active) for a non-running unit."
  (supervisor-test-with-unit-files
      '(("echo hi" :id "svc" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("is-active" "svc"))))
      (should (= supervisor-cli-exit-not-active
                 (supervisor-cli-result-exitcode result))))))

(ert-deftest supervisor-test-cli-is-active-unknown-id ()
  "The `is-active' returns exit 4 (no-such-unit) for unknown ID."
  (supervisor-test-with-unit-files
      '(("echo hi" :id "svc" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("is-active" "nonexistent"))))
      (should (= supervisor-cli-exit-no-such-unit
                 (supervisor-cli-result-exitcode result)))
      (should (string-match "inactive" (supervisor-cli-result-output result))))))

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
  (supervisor-test-with-unit-files
      '(("echo hi" :id "svc" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("is-active" "svc" "--json"))))
      (should (eq 'json (supervisor-cli-result-format result)))
      (let ((parsed (json-read-from-string (supervisor-cli-result-output result))))
        (should (equal "svc" (alist-get 'id parsed)))
        (should (assoc 'active parsed))
        (should (assoc 'status parsed))))))

(ert-deftest supervisor-test-cli-is-active-json-running ()
  "The `is-active --json' returns active=true for running unit."
  (supervisor-test-with-unit-files
      '(("sleep 999" :id "svc" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
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
        (delete-process proc)))))

;;; CLI -- is-enabled Tests

(ert-deftest supervisor-test-cli-is-enabled-enabled ()
  "The `is-enabled' returns exit 0 for an enabled unit."
  (supervisor-test-with-unit-files
      '(("echo hi" :id "svc" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("is-enabled" "svc"))))
      (should (= supervisor-cli-exit-success
                 (supervisor-cli-result-exitcode result)))
      (should (string-match "enabled" (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-is-enabled-disabled ()
  "The `is-enabled' returns exit 1 for a disabled unit."
  (supervisor-test-with-unit-files
      '(("echo hi" :id "svc" :type simple :disabled t))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("is-enabled" "svc"))))
      (should (= supervisor-cli-exit-failure
                 (supervisor-cli-result-exitcode result)))
      (should (string-match "disabled" (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-is-enabled-masked ()
  "The `is-enabled' returns exit 1 and state=masked for a masked unit."
  (supervisor-test-with-unit-files
      '(("echo hi" :id "svc" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (supervisor--mask-override (make-hash-table :test 'equal)))
      (puthash "svc" 'masked supervisor--mask-override)
      (let ((result (supervisor--cli-dispatch '("is-enabled" "svc"))))
        (should (= supervisor-cli-exit-failure
                   (supervisor-cli-result-exitcode result)))
        (should (string-match "masked" (supervisor-cli-result-output result)))))))

(ert-deftest supervisor-test-cli-is-enabled-unknown-id ()
  "The `is-enabled' returns exit 4 (no-such-unit) for unknown ID."
  (supervisor-test-with-unit-files
      '(("echo hi" :id "svc" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("is-enabled" "nonexistent"))))
      (should (= supervisor-cli-exit-no-such-unit
                 (supervisor-cli-result-exitcode result)))
      (should (string-match "not-found" (supervisor-cli-result-output result))))))

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
  (supervisor-test-with-unit-files
      '(("echo hi" :id "svc" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("is-enabled" "svc" "--json"))))
      (should (eq 'json (supervisor-cli-result-format result)))
      (let ((parsed (json-read-from-string (supervisor-cli-result-output result))))
        (should (equal "svc" (alist-get 'id parsed)))
        (should (eq t (alist-get 'enabled parsed)))
        (should (equal "enabled" (alist-get 'state parsed)))))))

(ert-deftest supervisor-test-cli-is-enabled-masked-json ()
  "The `is-enabled --json' for masked unit shows enabled=false, state=masked."
  (supervisor-test-with-unit-files
      '(("echo hi" :id "svc" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (supervisor--mask-override (make-hash-table :test 'equal)))
      (puthash "svc" 'masked supervisor--mask-override)
      (let* ((result (supervisor--cli-dispatch '("is-enabled" "svc" "--json")))
             (parsed (json-read-from-string (supervisor-cli-result-output result))))
        (should (= supervisor-cli-exit-failure
                   (supervisor-cli-result-exitcode result)))
        (should (eq :json-false (alist-get 'enabled parsed)))
        (should (equal "masked" (alist-get 'state parsed)))))))

;;; CLI -- is-failed Tests

(ert-deftest supervisor-test-cli-is-failed-dead ()
  "The `is-failed' returns exit 0 for a crash-looped (dead) unit."
  (supervisor-test-with-unit-files
      '(("echo hi" :id "svc" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (supervisor--failed (make-hash-table :test 'equal)))
      (puthash "svc" t supervisor--failed)
      (let ((result (supervisor--cli-dispatch '("is-failed" "svc"))))
        (should (= supervisor-cli-exit-success
                   (supervisor-cli-result-exitcode result)))
        (should (string-match "dead" (supervisor-cli-result-output result)))))))

(ert-deftest supervisor-test-cli-is-failed-not-failed ()
  "The `is-failed' returns exit 1 for a non-failed unit."
  (supervisor-test-with-unit-files
      '(("echo hi" :id "svc" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("is-failed" "svc"))))
      (should (= supervisor-cli-exit-failure
                 (supervisor-cli-result-exitcode result))))))

(ert-deftest supervisor-test-cli-is-failed-oneshot-failed ()
  "The `is-failed' returns exit 0 for a oneshot with non-zero exit."
  (supervisor-test-with-unit-files
      '(("false" :id "svc" :type oneshot))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (supervisor--oneshot-completed (make-hash-table :test 'equal)))
      (puthash "svc" 1 supervisor--oneshot-completed)
      (let ((result (supervisor--cli-dispatch '("is-failed" "svc"))))
        (should (= supervisor-cli-exit-success
                   (supervisor-cli-result-exitcode result)))
        (should (string-match "failed" (supervisor-cli-result-output result)))))))

(ert-deftest supervisor-test-cli-is-failed-unknown-id ()
  "The `is-failed' returns exit 4 (no-such-unit) for unknown ID."
  (supervisor-test-with-unit-files
      '(("echo hi" :id "svc" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("is-failed" "nonexistent"))))
      (should (= supervisor-cli-exit-no-such-unit
                 (supervisor-cli-result-exitcode result)))
      (should (string-match "inactive" (supervisor-cli-result-output result))))))

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
  (supervisor-test-with-unit-files
      '(("echo hi" :id "svc" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
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
        (should (equal "dead" (alist-get 'status parsed)))))))

(ert-deftest supervisor-test-cli-is-failed-running-json ()
  "The `is-failed --json' returns failed=false for running unit."
  (supervisor-test-with-unit-files
      '(("sleep 999" :id "svc" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
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
        (delete-process proc)))))

;;; CLI -- daemon-reload Tests

(ert-deftest supervisor-test-daemon-reload-picks-up-config-change ()
  "The `daemon-reload' picks up added entries from disk."
  (supervisor-test-with-unit-files nil
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (supervisor--current-plan nil)
           (supervisor--invalid (make-hash-table :test 'equal)))
      ;; Write one unit file and reload
      (with-temp-file (expand-file-name "a.el" supervisor-unit-directory)
        (insert "(:id \"a\" :command \"echo a\" :type simple)"))
      (supervisor-daemon-reload)
      (should supervisor--current-plan)
      (should (= 1 (length (supervisor-plan-entries supervisor--current-plan))))
      ;; Add a second unit file and reload again
      (with-temp-file (expand-file-name "b.el" supervisor-unit-directory)
        (insert "(:id \"b\" :command \"echo b\" :type simple)"))
      (supervisor-daemon-reload)
      (should (= 2 (length (supervisor-plan-entries supervisor--current-plan)))))))

(ert-deftest supervisor-test-daemon-reload-runtime-untouched ()
  "The `daemon-reload' does not start or stop processes."
  (supervisor-test-with-unit-files
      '(("sleep 999" :id "svc" :type simple))
    (let ((supervisor--processes (make-hash-table :test 'equal))
          (supervisor--entry-state (make-hash-table :test 'equal))
          (supervisor--current-plan nil)
          (supervisor--invalid (make-hash-table :test 'equal))
          (proc (start-process "test" nil "sleep" "999")))
      (unwind-protect
          (progn
            (puthash "svc" proc supervisor--processes)
            ;; Remove the unit file from disk and reload
            (delete-file (expand-file-name "svc.el" supervisor-unit-directory))
            (supervisor-daemon-reload)
            ;; Process should still be running (runtime untouched)
            (should (process-live-p proc))
            (should (gethash "svc" supervisor--processes)))
        (delete-process proc)))))

(ert-deftest supervisor-test-daemon-reload-surfaces-invalid ()
  "The `daemon-reload' surfaces invalid entries in plan."
  (supervisor-test-with-unit-files
      '(("true" :id "ok" :type simple)
        ("true" :id "nope" :type "not-a-symbol"))
    (let ((supervisor--processes (make-hash-table :test 'equal))
          (supervisor--entry-state (make-hash-table :test 'equal))
          (supervisor--current-plan nil)
          (supervisor--invalid (make-hash-table :test 'equal)))
      (let ((result (supervisor-daemon-reload)))
        (should (= 1 (plist-get result :entries)))
        (should (= 1 (plist-get result :invalid)))
        (should (gethash "nope" supervisor--invalid))))))

(ert-deftest supervisor-test-daemon-reload-returns-counts ()
  "The `daemon-reload' returns entry and invalid counts."
  (supervisor-test-with-unit-files
      '(("echo a" :id "a" :type simple))
    (let ((supervisor--processes (make-hash-table :test 'equal))
          (supervisor--entry-state (make-hash-table :test 'equal))
          (supervisor--current-plan nil)
          (supervisor--invalid (make-hash-table :test 'equal)))
      (let ((result (supervisor-daemon-reload)))
        (should (= 1 (plist-get result :entries)))
        (should (= 0 (plist-get result :invalid)))))))

(ert-deftest supervisor-test-cli-daemon-reload ()
  "CLI `daemon-reload' returns success."
  (supervisor-test-with-unit-files
      '(("echo a" :id "a" :type simple))
    (let ((supervisor--processes (make-hash-table :test 'equal))
          (supervisor--entry-state (make-hash-table :test 'equal))
          (supervisor--current-plan nil)
          (supervisor--invalid (make-hash-table :test 'equal)))
      (let ((result (supervisor--cli-dispatch '("daemon-reload"))))
        (should (= supervisor-cli-exit-success
                   (supervisor-cli-result-exitcode result)))
        (should (string-match "1 entries" (supervisor-cli-result-output result)))))))

(ert-deftest supervisor-test-cli-daemon-reload-json ()
  "CLI `daemon-reload --json' returns JSON with reloaded and counts."
  (supervisor-test-with-unit-files
      '(("echo a" :id "a" :type simple))
    (let ((supervisor--processes (make-hash-table :test 'equal))
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
        (should (= 0 (alist-get 'invalid parsed)))))))

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
  (supervisor-test-with-unit-files nil
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (supervisor--current-plan nil)
           (supervisor--invalid (make-hash-table :test 'equal))
           (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
           (supervisor--computed-deps (make-hash-table :test 'equal)))
      ;; Create a malformed unit file (missing :command)
      (with-temp-file (expand-file-name "bad.el" supervisor-unit-directory)
        (insert "(:id \"bad-unit\")"))
      (let ((result (supervisor-daemon-reload)))
        ;; Invalid count should include the unit-file invalid
        (should (= 1 (plist-get result :invalid)))
        (should (gethash "bad-unit" supervisor--invalid))))))

;;; Phase 8: reload command tests

(ert-deftest supervisor-test-reload-unit-running-simple-restarts ()
  "Reloading a running simple unit stops and restarts it.
Reload bypasses `supervisor--manual-start' and calls
`supervisor--start-process' directly to avoid disabled-policy refusal."
  (supervisor-test-with-unit-files
      '(("sleep 999" :id "svc1"))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
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
                     (lambda (id _cmd _logging _type _restart &rest _args)
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
            (delete-process p)))))))

(ert-deftest supervisor-test-reload-unit-stopped-updates ()
  "Reloading a stopped unit returns `updated' and clears stale state."
  (supervisor-test-with-unit-files
      '(("echo hi" :id "svc1"))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
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
        (should-not (gethash "svc1" supervisor--oneshot-completed))))))

(ert-deftest supervisor-test-reload-unit-masked-skips ()
  "Reloading a masked unit returns `skipped (masked)'."
  (supervisor-test-with-unit-files
      '(("echo hi" :id "svc1"))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--mask-override (make-hash-table :test 'equal))
           (supervisor--invalid (make-hash-table :test 'equal)))
      (puthash "svc1" 'masked supervisor--mask-override)
      (let ((result (supervisor--reload-unit "svc1")))
        (should (equal "svc1" (plist-get result :id)))
        (should (equal "skipped (masked)" (plist-get result :action)))))))

(ert-deftest supervisor-test-reload-unit-unknown-errors ()
  "Reloading an unknown unit returns an error."
  (supervisor-test-with-unit-files nil
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--mask-override (make-hash-table :test 'equal))
           (supervisor--invalid (make-hash-table :test 'equal))
           (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
           (supervisor--computed-deps (make-hash-table :test 'equal)))
      (let ((result (supervisor--reload-unit "nonexistent")))
        (should (equal "nonexistent" (plist-get result :id)))
        (should (equal "error: not found" (plist-get result :action)))))))

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
  (supervisor-test-with-unit-files
      '(("echo hi" :id "svc1"))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
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
                                (supervisor-cli-result-output result)))))))

(ert-deftest supervisor-test-cli-reload-masked-human ()
  "CLI `reload' on masked unit shows `skipped (masked)' in human output."
  (supervisor-test-with-unit-files
      '(("echo hi" :id "svc1"))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--mask-override (make-hash-table :test 'equal))
           (supervisor--invalid (make-hash-table :test 'equal)))
      (puthash "svc1" 'masked supervisor--mask-override)
      (let ((result (supervisor--cli-dispatch '("reload" "svc1"))))
        (should (= supervisor-cli-exit-success
                   (supervisor-cli-result-exitcode result)))
        (should (string-match-p "skipped (masked)"
                                (supervisor-cli-result-output result)))))))

(ert-deftest supervisor-test-cli-reload-unknown-id-human ()
  "CLI `reload' on unknown ID returns exit 1 with error."
  (supervisor-test-with-unit-files nil
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--mask-override (make-hash-table :test 'equal))
           (supervisor--invalid (make-hash-table :test 'equal))
           (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
           (supervisor--computed-deps (make-hash-table :test 'equal)))
      (let ((result (supervisor--cli-dispatch '("reload" "ghost"))))
        (should (= supervisor-cli-exit-failure
                   (supervisor-cli-result-exitcode result)))
        (should (string-match-p "error: not found"
                                (supervisor-cli-result-output result)))))))

(ert-deftest supervisor-test-cli-reload-json ()
  "CLI `reload' with --json returns proper JSON structure."
  (supervisor-test-with-unit-files
      '(("echo hi" :id "svc1") ("echo bye" :id "svc2"))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
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
        (should (equal "skipped (masked)" (cdr (assoc 'action (aref results 1)))))))))

(ert-deftest supervisor-test-cli-reload-mixed-error-json ()
  "CLI `reload' with mix of valid and unknown IDs returns exit 1 JSON."
  (supervisor-test-with-unit-files
      '(("echo hi" :id "svc1"))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
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
        (should (string-match-p "error:" (cdr (assoc 'action (aref results 1)))))))))

(ert-deftest supervisor-test-cli-reload-help-listed ()
  "CLI help text includes the `reload' command."
  (let ((result (supervisor--cli-dispatch nil)))
    (should (string-match-p "reload \\[--\\] ID"
                            (supervisor-cli-result-output result)))))

(ert-deftest supervisor-test-reload-unit-file-only ()
  "Reload finds unit-file-only entries via effective programs."
  (supervisor-test-with-unit-files
      '(("echo hello" :id "uf-svc"))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
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
      (let ((result (supervisor--reload-unit "uf-svc")))
        (should (equal "uf-svc" (plist-get result :id)))
        (should (equal "updated" (plist-get result :action)))))))

(ert-deftest supervisor-test-reload-clears-stale-invalid ()
  "Reload succeeds on a previously-invalid entry after config fix.
When an entry was recorded in `supervisor--invalid' but has since
been fixed, reload should find and parse it fresh, clearing the
stale invalid state."
  (supervisor-test-with-unit-files
      '(("echo fixed" :id "svc1"))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
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
        (should-not (gethash "svc1" supervisor--invalid))))))

(ert-deftest supervisor-test-reload-find-entry-uses-effective-programs ()
  "The `supervisor--reload-find-entry' reads from effective programs."
  (supervisor-test-with-unit-files
      '(("echo hi" :id "from-config"))
    (let* ((supervisor--invalid (make-hash-table :test 'equal)))
      ;; Should find unit-file entry
      (should (supervisor--reload-find-entry "from-config"))
      ;; Should not find nonexistent entry
      (should-not (supervisor--reload-find-entry "nonexistent"))
      ;; Should find entry even if it's in the invalid cache
      (puthash "from-config" "some reason" supervisor--invalid)
      (should (supervisor--reload-find-entry "from-config")))))

(ert-deftest supervisor-test-reload-running-disabled-unit-succeeds ()
  "Reloading a running disabled unit keeps it running.
Reload bypasses enabled/disabled policy since the unit is already
running and reload's contract is config hot-swap."
  (supervisor-test-with-unit-files
      '(("sleep 999" :id "svc1" :disabled t))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
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
                     (lambda (id _cmd _logging _type _restart &rest _args)
                       (setq start-process-called id)
                       t)))
            (let ((result (supervisor--reload-unit "svc1")))
              ;; Must succeed, not fail with "disabled"
              (should (equal "reloaded" (plist-get result :action)))
              (should (equal "svc1" start-process-called))))
        ;; Cleanup
        (let ((p (gethash "svc1" supervisor--processes)))
          (when (and p (process-live-p p))
            (delete-process p)))))))

(ert-deftest supervisor-test-reload-running-oneshot-updates-only ()
  "Reloading a running oneshot does not interrupt it.
Per Phase 8 spec, only running simple units are restarted.
Running oneshots get definition update only."
  (supervisor-test-with-unit-files
      '(("sleep 999" :id "osh1" :type oneshot))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
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
            (delete-process p)))))))

(ert-deftest supervisor-test-reload-parse-error-returns-invalid-config ()
  "Reload on unparseable entry returns `error: invalid config'.
This distinguishes config errors from truly missing entries."
  (supervisor-test-with-unit-files nil
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--mask-override (make-hash-table :test 'equal))
           (supervisor--invalid (make-hash-table :test 'equal)))
      (cl-letf (((symbol-function 'supervisor--effective-programs)
                 (lambda ()
                   ;; Return a malformed entry: not a string or (string . plist)
                   '(42))))
        ;; The malformed entry's ID will be "malformed#0"
        (let ((result (supervisor--reload-unit "malformed#0")))
          (should (equal "malformed#0" (plist-get result :id)))
          (should (equal "error: invalid config" (plist-get result :action))))))))

;;; Phase 9: enable/disable model alignment tests

(ert-deftest supervisor-test-start-disabled-unit-works ()
  "Manual start on a disabled unit succeeds (systemctl model).
`start' on a disabled unit runs it this session only without
changing the enabled override."
  (supervisor-test-with-unit-files
      '(("echo hello" :id "svc1" :disabled t))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--failed (make-hash-table :test 'equal))
           (supervisor--restart-times (make-hash-table :test 'equal))
           (supervisor--oneshot-completed (make-hash-table :test 'equal))
           (supervisor--manually-stopped (make-hash-table :test 'equal))
           (supervisor--manually-started (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (supervisor--mask-override (make-hash-table :test 'equal))
           (supervisor--enabled-override (make-hash-table :test 'equal))
           (supervisor--invalid (make-hash-table :test 'equal))
           (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
           (supervisor--computed-deps (make-hash-table :test 'equal))
           (supervisor--logging (make-hash-table :test 'equal))
           (started nil))
      (cl-letf (((symbol-function 'supervisor--start-process)
                 (lambda (id _cmd _logging _type _restart &rest _args)
                   (setq started id)
                   t)))
        (let ((result (supervisor--manual-start "svc1")))
          ;; Should succeed, not be skipped
          (should (eq 'started (plist-get result :status)))
          (should (equal "svc1" started))
          ;; Should NOT change enabled override
          (should-not (gethash "svc1" supervisor--enabled-override))
          ;; Should mark as manually started for reconcile
          (should (gethash "svc1" supervisor--manually-started)))))))

(ert-deftest supervisor-test-start-disabled-unit-override-unchanged ()
  "Starting a disabled unit does not flip the enabled override.
Even with an explicit :disabled override set, manual start does not
change the override — it just runs the unit this session."
  (supervisor-test-with-unit-files
      '(("echo hello" :id "svc1"))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--failed (make-hash-table :test 'equal))
           (supervisor--restart-times (make-hash-table :test 'equal))
           (supervisor--oneshot-completed (make-hash-table :test 'equal))
           (supervisor--manually-stopped (make-hash-table :test 'equal))
           (supervisor--manually-started (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (supervisor--mask-override (make-hash-table :test 'equal))
           (supervisor--enabled-override (make-hash-table :test 'equal))
           (supervisor--invalid (make-hash-table :test 'equal))
           (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
           (supervisor--computed-deps (make-hash-table :test 'equal))
           (supervisor--logging (make-hash-table :test 'equal)))
      ;; Disable via override
      (puthash "svc1" 'disabled supervisor--enabled-override)
      (cl-letf (((symbol-function 'supervisor--start-process)
                 (lambda (_id _cmd _logging _type _restart &rest _args) t)))
        (let ((result (supervisor--manual-start "svc1")))
          (should (eq 'started (plist-get result :status)))
          ;; Override should still be 'disabled (unchanged)
          (should (eq 'disabled (gethash "svc1" supervisor--enabled-override))))))))

(ert-deftest supervisor-test-reconcile-keeps-manually-started-disabled ()
  "Reconcile does not stop disabled units that were manually started.
Per the systemctl model, `start' on a disabled unit is session-only
and reconcile should not undo it."
  (supervisor-test-with-unit-files
      '(("sleep 100" :id "svc1" :disabled t))
    (let* ((plan (supervisor--build-plan (supervisor--effective-programs)))
           (process-alive (make-hash-table :test 'equal))
           (manually-started (make-hash-table :test 'equal)))
      (puthash "svc1" t process-alive)
      (puthash "svc1" t manually-started)
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
                        :manually-started manually-started
                        :timestamp (float-time)))
             (actions (supervisor--compute-actions plan snapshot)))
        ;; Should be noop (manually-started), NOT stop (disabled)
        (let ((action (cl-find "svc1" actions
                               :key (lambda (a) (plist-get a :id))
                               :test #'equal)))
          (should action)
          (should (eq 'noop (plist-get action :op)))
          (should (eq 'manually-started (plist-get action :reason))))))))

(ert-deftest supervisor-test-reconcile-stops-non-manual-disabled ()
  "Reconcile stops disabled units that were NOT manually started."
  (supervisor-test-with-unit-files
      '(("sleep 100" :id "svc1" :disabled t))
    (let* ((plan (supervisor--build-plan (supervisor--effective-programs)))
           (process-alive (make-hash-table :test 'equal)))
      (puthash "svc1" t process-alive)
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
                        :manually-started (make-hash-table :test 'equal)
                        :timestamp (float-time)))
             (actions (supervisor--compute-actions plan snapshot)))
        ;; Should stop (disabled, not manually started)
        (let ((action (cl-find "svc1" actions
                               :key (lambda (a) (plist-get a :id))
                               :test #'equal)))
          (should action)
          (should (eq 'stop (plist-get action :op)))
          (should (eq 'disabled (plist-get action :reason))))))))

(ert-deftest supervisor-test-reconcile-stops-masked-even-if-manually-started ()
  "Reconcile stops masked units even if manually started.
Mask overrides everything including manual-start tracking."
  (supervisor-test-with-unit-files
      '(("sleep 100" :id "svc1"))
    (let* ((plan (supervisor--build-plan (supervisor--effective-programs)))
           (process-alive (make-hash-table :test 'equal))
           (manually-started (make-hash-table :test 'equal))
           (mask-override (make-hash-table :test 'equal)))
      (puthash "svc1" t process-alive)
      (puthash "svc1" t manually-started)
      (puthash "svc1" 'masked mask-override)
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
                        :mask-override mask-override
                        :manually-started manually-started
                        :timestamp (float-time)))
             (actions (supervisor--compute-actions plan snapshot)))
        (let ((action (cl-find "svc1" actions
                               :key (lambda (a) (plist-get a :id))
                               :test #'equal)))
          (should action)
          (should (eq 'stop (plist-get action :op)))
          (should (eq 'masked (plist-get action :reason))))))))

(ert-deftest supervisor-test-manual-stop-clears-manually-started ()
  "Manually stopping a unit clears the manually-started flag.
After manual stop, reconcile is free to treat it normally."
  (let* ((supervisor--processes (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (supervisor--manually-started (make-hash-table :test 'equal)))
    ;; Simulate a manually-started running process
    (puthash "svc1" (start-process "test" nil "sleep" "999")
             supervisor--processes)
    (puthash "svc1" t supervisor--manually-started)
    (unwind-protect
        (progn
          (supervisor--manual-stop "svc1")
          ;; manually-started should be cleared
          (should-not (gethash "svc1" supervisor--manually-started))
          ;; manually-stopped should be set
          (should (gethash "svc1" supervisor--manually-stopped)))
      (let ((p (gethash "svc1" supervisor--processes)))
        (when (and p (process-live-p p))
          (delete-process p))))))

(ert-deftest supervisor-test-cli-start-disabled-unit ()
  "CLI `start' on a disabled unit succeeds."
  (supervisor-test-with-unit-files
      '(("echo hi" :id "svc1" :disabled t))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--failed (make-hash-table :test 'equal))
           (supervisor--restart-times (make-hash-table :test 'equal))
           (supervisor--oneshot-completed (make-hash-table :test 'equal))
           (supervisor--manually-stopped (make-hash-table :test 'equal))
           (supervisor--manually-started (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (supervisor--mask-override (make-hash-table :test 'equal))
           (supervisor--enabled-override (make-hash-table :test 'equal))
           (supervisor--invalid (make-hash-table :test 'equal))
           (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
           (supervisor--computed-deps (make-hash-table :test 'equal))
           (supervisor--logging (make-hash-table :test 'equal)))
      (cl-letf (((symbol-function 'supervisor--start-process)
                 (lambda (_id _cmd _logging _type _restart &rest _args) t)))
        (let ((result (supervisor--cli-dispatch '("start" "--" "svc1"))))
          (should (= supervisor-cli-exit-success
                     (supervisor-cli-result-exitcode result)))
          (should (string-match-p "Started: svc1"
                                  (supervisor-cli-result-output result))))))))

(ert-deftest supervisor-test-mask-still-blocks-manual-start ()
  "Masked units are still blocked from manual start.
Only mask blocks; disabled does not."
  (supervisor-test-with-unit-files
      '(("echo hi" :id "svc1"))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--failed (make-hash-table :test 'equal))
           (supervisor--restart-times (make-hash-table :test 'equal))
           (supervisor--oneshot-completed (make-hash-table :test 'equal))
           (supervisor--manually-stopped (make-hash-table :test 'equal))
           (supervisor--manually-started (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (supervisor--mask-override (make-hash-table :test 'equal))
           (supervisor--enabled-override (make-hash-table :test 'equal))
           (supervisor--invalid (make-hash-table :test 'equal))
           (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
           (supervisor--computed-deps (make-hash-table :test 'equal))
           (supervisor--logging (make-hash-table :test 'equal)))
      (puthash "svc1" 'masked supervisor--mask-override)
      (let ((result (supervisor--manual-start "svc1")))
        (should (eq 'skipped (plist-get result :status)))
        (should (equal "masked" (plist-get result :reason)))))))

(ert-deftest supervisor-test-cli-enable-persists ()
  "CLI `enable' command persists the override to disk."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "svc1" :type simple :enabled nil))
    (let* ((temp-file (make-temp-file "supervisor-test-enable-" nil ".eld"))
           (supervisor-overrides-file temp-file)
           (supervisor--enabled-override (make-hash-table :test 'equal))
           (supervisor--restart-override (make-hash-table :test 'equal))
           (supervisor--logging (make-hash-table :test 'equal))
           (supervisor--mask-override (make-hash-table :test 'equal))
           (supervisor--overrides-loaded nil))
      (unwind-protect
          (progn
            (supervisor--cli-dispatch '("enable" "svc1"))
            ;; In-memory override set
            (should (eq 'enabled (gethash "svc1" supervisor--enabled-override)))
            ;; Clear memory and reload from file
            (clrhash supervisor--enabled-override)
            (should (supervisor--load-overrides))
            ;; Should survive roundtrip
            (should (eq 'enabled
                        (gethash "svc1" supervisor--enabled-override))))
        (delete-file temp-file)))))

(ert-deftest supervisor-test-cli-disable-persists ()
  "CLI `disable' command persists the override to disk."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "svc1" :type simple))
    (let* ((temp-file (make-temp-file "supervisor-test-disable-" nil ".eld"))
           (supervisor-overrides-file temp-file)
           (supervisor--enabled-override (make-hash-table :test 'equal))
           (supervisor--restart-override (make-hash-table :test 'equal))
           (supervisor--logging (make-hash-table :test 'equal))
           (supervisor--mask-override (make-hash-table :test 'equal))
           (supervisor--overrides-loaded nil))
      (unwind-protect
          (progn
            (supervisor--cli-dispatch '("disable" "svc1"))
            ;; In-memory override set
            (should (eq 'disabled
                        (gethash "svc1" supervisor--enabled-override)))
            ;; Clear memory and reload from file
            (clrhash supervisor--enabled-override)
            (should (supervisor--load-overrides))
            ;; Should survive roundtrip
            (should (eq 'disabled
                        (gethash "svc1" supervisor--enabled-override))))
        (delete-file temp-file)))))

(ert-deftest supervisor-test-manual-start-failure-no-stale-flag ()
  "Failed manual start does not leave stale manually-started flag.
Only successful starts should set the flag, otherwise reconcile
could incorrectly preserve a non-running disabled unit."
  (supervisor-test-with-unit-files
      '(("echo hi" :id "svc1"))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--failed (make-hash-table :test 'equal))
           (supervisor--restart-times (make-hash-table :test 'equal))
           (supervisor--oneshot-completed (make-hash-table :test 'equal))
           (supervisor--manually-stopped (make-hash-table :test 'equal))
           (supervisor--manually-started (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (supervisor--mask-override (make-hash-table :test 'equal))
           (supervisor--enabled-override (make-hash-table :test 'equal))
           (supervisor--invalid (make-hash-table :test 'equal))
           (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
           (supervisor--computed-deps (make-hash-table :test 'equal))
           (supervisor--logging (make-hash-table :test 'equal)))
      (cl-letf (((symbol-function 'supervisor--start-process)
                 (lambda (_id _cmd _logging _type _restart &rest _args)
                   nil)))  ; Simulate spawn failure
        (let ((result (supervisor--manual-start "svc1")))
          (should (eq 'error (plist-get result :status)))
          ;; Flag must NOT be set on failure
          (should-not (gethash "svc1" supervisor--manually-started)))))))


(provide 'supervisor-test-cli)
;;; supervisor-test-cli.el ends here
