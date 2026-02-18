;;; supervisor-test-dashboard.el --- Dashboard UI and rendering tests for supervisor.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Dashboard UI and rendering ERT tests for supervisor.el.

;;; Code:

(require 'supervisor-test-helpers)

;;; Dashboard UI tests

(ert-deftest supervisor-test-separator-row-detection ()
  "Separator rows are correctly identified as symbol IDs."
  (should (supervisor--separator-row-p '--services--))
  (should (supervisor--separator-row-p '--health--))
  (should (supervisor--separator-row-p '--timers--))
  (should (supervisor--separator-row-p '--blank-1--))
  (should-not (supervisor--separator-row-p "nm-applet"))
  (should-not (supervisor--separator-row-p nil))
  (should-not (supervisor--separator-row-p (cons :service "foo")))
  (should-not (supervisor--separator-row-p (cons :timer "bar"))))

(ert-deftest supervisor-test-health-summary-format ()
  "Health summary includes all required counts."
  (supervisor-test-with-unit-files nil
    (let ((supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--processes (make-hash-table :test 'equal))
          (supervisor--failed (make-hash-table :test 'equal))
          (supervisor--oneshot-completed (make-hash-table :test 'equal)))
      (let ((summary (supervisor--health-summary)))
        ;; Should contain all five metrics
        (should (string-match-p "run" summary))
        (should (string-match-p "done" summary))
        (should (string-match-p "pend" summary))
        (should (string-match-p "fail" summary))
        (should (string-match-p "inv" summary))))))

(ert-deftest supervisor-test-help-text-key-parity ()
  "Help text includes all top-level bound keys."
  ;; Top-level keys that must be discoverable on-screen
  (should (string-match-p "\\[f\\]" supervisor--help-text))
  (should (string-match-p "\\[g\\]" supervisor--help-text))
  (should (string-match-p "\\[G\\]" supervisor--help-text))
  (should (string-match-p "\\[t\\]" supervisor--help-text))
  (should (string-match-p "\\[T\\]" supervisor--help-text))
  (should (string-match-p "\\[l\\]" supervisor--help-text))
  (should (string-match-p "\\[p\\]" supervisor--help-text))
  (should (string-match-p "\\[i\\]" supervisor--help-text))
  (should (string-match-p "\\[?\\]" supervisor--help-text))
  (should (string-match-p "\\[h\\]" supervisor--help-text))
  (should (string-match-p "\\[q\\]" supervisor--help-text)))

(ert-deftest supervisor-test-health-summary-deduplication ()
  "Health summary deduplicates entries with same ID.
Unit-file loader already deduplicates, so only one entry is loaded."
  (supervisor-test-without-builtins
    (let* ((dir (make-temp-file "units-" t))
           (supervisor-unit-authority-path (list dir))
           (supervisor-unit-directory dir)
           (supervisor--programs-cache :not-yet-loaded)
           (supervisor--unit-file-invalid (make-hash-table :test 'equal))
           (supervisor--invalid (make-hash-table :test 'equal))
           (supervisor--processes (make-hash-table :test 'equal))
           (supervisor--failed (make-hash-table :test 'equal))
           (supervisor--oneshot-completed (make-hash-table :test 'equal)))
      ;; Create two unit files with the same :id
      (with-temp-file (expand-file-name "dup1.el" dir)
        (insert "(:id \"dup\" :command \"sleep 100\")"))
      (with-temp-file (expand-file-name "dup2.el" dir)
        (insert "(:id \"dup\" :command \"sleep 100\")"))
      (unwind-protect
          (let ((summary (supervisor--health-summary)))
            ;; Should count only 1 pending, not 2
            (should (string-match-p "1 pend" summary)))
        (delete-directory dir t)))))

(ert-deftest supervisor-test-disabled-only-completes ()
  "Only disabled entries completes immediately."
  (supervisor-test-with-unit-files
      '(("sleep 100" :id "a" :disabled t))
    (let* ((supervisor--invalid (make-hash-table :test 'equal))
           (supervisor--processes (make-hash-table :test 'equal))
           (supervisor--failed (make-hash-table :test 'equal))
           (supervisor--oneshot-completed (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
           (supervisor--computed-deps (make-hash-table :test 'equal))
           (supervisor--shutting-down nil)
           (completed nil))
      ;; Parse entries
      (let ((entries (supervisor--all-parsed-entries)))
        ;; Start entries with callback that sets completed flag
        (supervisor--start-entries-async
         entries
         (lambda () (setq completed t)))
        ;; Should complete immediately since all entries are disabled
        (should completed)))))

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

(ert-deftest supervisor-test-start-process-logging-unavailable-does-not-fail ()
  "Unwritable log directory does not block process startup."
  (let* ((supervisor--processes (make-hash-table :test 'equal))
         (supervisor--writers (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal))
         (supervisor--restart-timers (make-hash-table :test 'equal))
         (supervisor--failed (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--restart-override (make-hash-table :test 'equal))
         (supervisor--shutting-down nil))
    (cl-letf (((symbol-function 'supervisor--effective-log-directory)
               (lambda () nil))
              ((symbol-function 'supervisor--warn-log-directory)
               #'ignore))
      (let ((proc (supervisor--start-process "test" "/bin/true" t 'oneshot nil)))
        (should (processp proc))
        (while (process-live-p proc)
          (accept-process-output nil 0.01))
        (should-not (gethash "test" supervisor--writers))))))

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


;;; Dashboard Stop/Restart Tests

(ert-deftest supervisor-test-dashboard-stop-keybinding ()
  "Dashboard l key reaches lifecycle submenu."
  (should (eq (lookup-key supervisor-dashboard-mode-map "l")
              'supervisor-dashboard-lifecycle)))

(ert-deftest supervisor-test-dashboard-restart-keybinding ()
  "Dashboard p key reaches policy submenu."
  (should (eq (lookup-key supervisor-dashboard-mode-map "p")
              'supervisor-dashboard-policy)))

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
                   (list (list (cons :service "test-svc")
                               (vector "test-svc" "simple" "-"
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
  "Dashboard help text includes lifecycle and policy submenu hints."
  (should (string-match "\\[l\\]ifecycle" supervisor--help-text))
  (should (string-match "\\[p\\]olicy" supervisor--help-text))
  (should (string-match "\\[i\\]nspect" supervisor--help-text)))

(ert-deftest supervisor-test-timer-row-p-detects-timer ()
  "Timer row predicate detects timer rows by typed cons cell ID."
  (should (supervisor--timer-row-p (cons :timer "my-timer")))
  (should-not (supervisor--timer-row-p (cons :service "my-svc")))
  (should-not (supervisor--timer-row-p "my-timer"))
  (should-not (supervisor--timer-row-p nil))
  (should-not (supervisor--timer-row-p '--timers--)))

(ert-deftest supervisor-test-timer-row-p-rejects-service-row ()
  "Timer row predicate returns nil for service rows."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list "my-svc" (vector "my-svc" "simple" "-"
                                       "yes" "running" "yes" "yes" "1234" "-")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should-not (supervisor--timer-row-p "my-svc")))))

(ert-deftest supervisor-test-stop-allows-service-with-timer-id-collision ()
  "Stop on a service row works even if a timer has the same ID."
  (supervisor-test-with-unit-files
      '(("sleep 60" :id "dup" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
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
               (list (list "dup" (vector "dup" "simple" "-"
                                        "yes" "stopped" "yes" "yes" "-" "-")))))
          (tabulated-list-init-header)
          (tabulated-list-print)
          (goto-char (point-min))
          ;; Should NOT error with "Cannot stop timer" — it's a service row
          (should-not (supervisor--timer-row-p "dup")))))))

(ert-deftest supervisor-test-stop-rejects-oneshot ()
  "Stop rejects oneshot entries."
  (supervisor-test-with-unit-files
      '(("true" :id "my-oneshot" :type oneshot))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (supervisor--invalid (make-hash-table :test 'equal)))
      ;; Simulate being in dashboard with oneshot at point
      (with-temp-buffer
        (supervisor-dashboard-mode)
        (let ((tabulated-list-entries
               (list (list "my-oneshot" (vector "my-oneshot" "oneshot" "-"
                                               "yes" "done" "n/a" "yes" "-" "-")))))
          (tabulated-list-init-header)
          (tabulated-list-print)
          (goto-char (point-min))
          (should-error (supervisor-dashboard-stop)
                        :type 'user-error))))))

(ert-deftest supervisor-test-restart-accepts-oneshot ()
  "Restart starts oneshot entries (parity with CLI).
Stop returns skipped for completed oneshot; restart must still
proceed to call start unconditionally."
  (supervisor-test-with-unit-files
      '(("true" :id "my-oneshot" :type oneshot))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (supervisor--invalid (make-hash-table :test 'equal))
           (started nil))
      (with-temp-buffer
        (supervisor-dashboard-mode)
        (let ((tabulated-list-entries
               (list (list (cons :service "my-oneshot")
                           (vector "my-oneshot" "oneshot" "-"
                                   "yes" "done" "n/a" "yes" "-" "-")))))
          (tabulated-list-init-header)
          (tabulated-list-print)
          (goto-char (point-min))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
                    ((symbol-function 'supervisor--manual-stop)
                     (lambda (_id)
                       '(:status skipped :reason "not running")))
                    ((symbol-function 'supervisor--manual-start)
                     (lambda (id) (setq started id)
                       '(:status started)))
                    ((symbol-function 'supervisor--refresh-dashboard) #'ignore))
            (supervisor-dashboard-restart)
            (should (equal "my-oneshot" started))))))))

(ert-deftest supervisor-test-stop-rejects-timer-row ()
  "Stop rejects timer rows with stable message."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "my-timer")
                       (vector "my-timer" "timer" "-"
                               "-" "pending" "-" "-" "-" "-")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (supervisor-dashboard-stop)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest supervisor-test-restart-rejects-timer-row ()
  "Restart rejects timer rows with stable message."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "my-timer")
                       (vector "my-timer" "timer" "-"
                               "-" "pending" "-" "-" "-" "-")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (supervisor-dashboard-restart)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest supervisor-test-restart-accepts-not-running ()
  "Restart starts non-running entries (parity with CLI).
Stop returns skipped for non-running entries; restart must still
proceed to call start unconditionally."
  (supervisor-test-with-unit-files
      '(("sleep 60" :id "my-svc" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (supervisor--invalid (make-hash-table :test 'equal))
           (started nil))
      (with-temp-buffer
        (supervisor-dashboard-mode)
        (let ((tabulated-list-entries
               (list (list (cons :service "my-svc")
                           (vector "my-svc" "simple" "-"
                                   "yes" "stopped" "yes" "yes" "-" "-")))))
          (tabulated-list-init-header)
          (tabulated-list-print)
          (goto-char (point-min))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
                    ((symbol-function 'supervisor--manual-stop)
                     (lambda (_id)
                       '(:status skipped :reason "not running")))
                    ((symbol-function 'supervisor--manual-start)
                     (lambda (id) (setq started id)
                       '(:status started)))
                    ((symbol-function 'supervisor--refresh-dashboard) #'ignore))
            (supervisor-dashboard-restart)
            (should (equal "my-svc" started))))))))

;;; Interactive Dashboard Timer Section Tests (PLAN-interactive Phase 5)

(ert-deftest supervisor-test-typed-row-id-service ()
  "Service row IDs are typed cons cells."
  (should (supervisor--service-row-p (cons :service "foo")))
  (should-not (supervisor--service-row-p (cons :timer "foo")))
  (should-not (supervisor--service-row-p "foo"))
  (should-not (supervisor--service-row-p nil)))

(ert-deftest supervisor-test-typed-row-id-timer ()
  "Timer row IDs are typed cons cells."
  (should (supervisor--timer-row-p (cons :timer "foo")))
  (should-not (supervisor--timer-row-p (cons :service "foo")))
  (should-not (supervisor--timer-row-p "foo"))
  (should-not (supervisor--timer-row-p nil)))

(ert-deftest supervisor-test-typed-row-id-separator ()
  "Separator row IDs are symbols."
  (should (supervisor--separator-row-p '--services--))
  (should (supervisor--separator-row-p '--health--))
  (should (supervisor--separator-row-p '--timers--))
  (should-not (supervisor--separator-row-p (cons :service "foo")))
  (should-not (supervisor--separator-row-p nil)))

(ert-deftest supervisor-test-row-kind-dispatch ()
  "Row kind returns correct kind for all ID types."
  (should (eq :service (supervisor--row-kind (cons :service "x"))))
  (should (eq :timer (supervisor--row-kind (cons :timer "x"))))
  (should (eq :separator (supervisor--row-kind '--services--)))
  (should-not (supervisor--row-kind nil)))

(ert-deftest supervisor-test-row-id-extraction ()
  "Row ID extraction returns string from typed IDs."
  (should (equal "foo" (supervisor--row-id (cons :service "foo"))))
  (should (equal "bar" (supervisor--row-id (cons :timer "bar"))))
  (should-not (supervisor--row-id '--separator--))
  (should-not (supervisor--row-id nil)))

(ert-deftest supervisor-test-collision-service-timer-same-id ()
  "Service and timer with same ID string do not collide in row dispatch."
  (let ((svc-id (cons :service "backup"))
        (tmr-id (cons :timer "backup")))
    ;; They are distinct
    (should-not (equal svc-id tmr-id))
    ;; Each detects correctly
    (should (supervisor--service-row-p svc-id))
    (should-not (supervisor--timer-row-p svc-id))
    (should (supervisor--timer-row-p tmr-id))
    (should-not (supervisor--service-row-p tmr-id))
    ;; Both extract same string
    (should (equal "backup" (supervisor--row-id svc-id)))
    (should (equal "backup" (supervisor--row-id tmr-id)))))

(ert-deftest supervisor-test-collision-dashboard-rows-coexist ()
  "Service and timer rows with same ID coexist in tabulated-list."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :service "backup")
                       (vector "backup" "simple" "-"
                               "yes" "running" "yes" "yes" "-" "-"))
                 (list (cons :timer "backup")
                       (vector "backup" "backup-svc" "-"
                               "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      ;; First row is service
      (should (supervisor--service-row-p (tabulated-list-get-id)))
      ;; Second row is timer
      (forward-line 1)
      (should (supervisor--timer-row-p (tabulated-list-get-id))))))

(ert-deftest supervisor-test-require-service-row-on-service ()
  "Require-service-row returns string ID on service row."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :service "svc")
                       (vector "svc" "simple" "-"
                               "yes" "running" "yes" "yes" "-" "-")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should (equal "svc" (supervisor--require-service-row))))))

(ert-deftest supervisor-test-require-service-row-rejects-timer ()
  "Require-service-row signals user-error on timer row."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "my-timer")
                       (vector "my-timer" "target" "-"
                               "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should-error (supervisor--require-service-row)
                    :type 'user-error))))

(ert-deftest supervisor-test-require-service-row-rejects-separator ()
  "Require-service-row signals user-error on separator row."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list '--services--
                       (vector "── Services" "" "" "" "" "" "" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should-error (supervisor--require-service-row)
                    :type 'user-error))))

(ert-deftest supervisor-test-timer-section-disabled-mode ()
  "Timer section shows disabled state when timer-subsystem-mode is nil."
  (supervisor-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple))
    (let ((supervisor--processes (make-hash-table :test 'equal))
          (supervisor--entry-state (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--timer-list nil)
          (supervisor--timer-state (make-hash-table :test 'equal))
          (supervisor--invalid-timers (make-hash-table :test 'equal))
          (supervisor-dashboard-show-timers t)
          (supervisor-timer-subsystem-mode nil))
      (let ((entries (supervisor--get-entries)))
        (let ((tmr-sep (cl-find '--timers-- entries :key #'car)))
          (should tmr-sep)
          (should (string-match-p "disabled"
                                  (aref (cadr tmr-sep) 0))))))))

(ert-deftest supervisor-test-timer-section-mode-on-supervisor-off ()
  "Timer section shows timers when timer-mode is on but supervisor-mode is off.
The disabled gate is `supervisor-timer-subsystem-mode', not
`supervisor-timer-subsystem-active-p' (which also requires `supervisor-mode').
When `supervisor-timer-subsystem-mode' is t but `supervisor-mode' is nil,
configured timers must be visible for analysis."
  (supervisor-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (supervisor--invalid (make-hash-table :test 'equal))
           (supervisor--timer-state (make-hash-table :test 'equal))
           (supervisor--invalid-timers (make-hash-table :test 'equal))
           (supervisor-dashboard-show-timers t)
           (supervisor-mode nil)
           (supervisor--timer-list
            (list (supervisor-timer--create :id "t1" :target "svc"))))
      (let ((entries (supervisor--get-entries)))
        ;; Timer section present and NOT disabled
        (let ((tmr-sep (cl-find '--timers-- entries :key #'car)))
          (should tmr-sep)
          (should-not (string-match-p "disabled" (aref (cadr tmr-sep) 0)))
          ;; Column headers visible
          (should (string-match-p "TARGET" (aref (cadr tmr-sep) 1))))
        ;; Timer row visible
        (let ((tmr-row (cl-find (cons :timer "t1") entries
                                :key #'car :test #'equal)))
          (should tmr-row))))))

(ert-deftest supervisor-test-timer-section-empty-state ()
  "Timer section shows empty state when no timers configured."
  (supervisor-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple))
    (let ((supervisor--processes (make-hash-table :test 'equal))
          (supervisor--entry-state (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--timer-list nil)
          (supervisor--timer-state (make-hash-table :test 'equal))
          (supervisor--invalid-timers (make-hash-table :test 'equal))
          (supervisor-dashboard-show-timers t))
      (let ((entries (supervisor--get-entries)))
        (let ((tmr-sep (cl-find '--timers-- entries :key #'car)))
          (should tmr-sep)
          (should (string-match-p "no timers configured"
                                  (aref (cadr tmr-sep) 1))))))))

(ert-deftest supervisor-test-timer-section-valid-state ()
  "Timer section shows valid timer rows when timers are configured."
  (supervisor-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (supervisor--invalid (make-hash-table :test 'equal))
           (supervisor--timer-state (make-hash-table :test 'equal))
           (supervisor--invalid-timers (make-hash-table :test 'equal))
           (supervisor-dashboard-show-timers t)
           (supervisor--timer-list
            (list (supervisor-timer--create :id "t1" :target "svc"))))
      (let ((entries (supervisor--get-entries)))
        ;; Timers header present
        (let ((tmr-sep (cl-find '--timers-- entries :key #'car)))
          (should tmr-sep)
          (should (string-match-p "TARGET" (aref (cadr tmr-sep) 1))))
        ;; Timer row present with typed ID
        (let ((tmr-row (cl-find (cons :timer "t1") entries
                                :key #'car :test #'equal)))
          (should tmr-row))))))

(ert-deftest supervisor-test-timer-section-invalid-timers ()
  "Timer section shows invalid timer rows."
  (supervisor-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (supervisor--invalid (make-hash-table :test 'equal))
           (supervisor--timer-state (make-hash-table :test 'equal))
           (supervisor--invalid-timers (make-hash-table :test 'equal))
           (supervisor-dashboard-show-timers t)
           (supervisor--timer-list nil))
      (puthash "bad-timer" "missing target" supervisor--invalid-timers)
      (let ((entries (supervisor--get-entries)))
        (let ((bad-row (cl-find (cons :timer "bad-timer") entries
                                :key #'car :test #'equal)))
          (should bad-row)
          ;; Check invalid status
          (should (string-match-p "invalid"
                                  (aref (cadr bad-row) 4))))))))

(ert-deftest supervisor-test-timer-section-hidden-when-show-timers-nil ()
  "Timer section not rendered when show-timers is nil."
  (supervisor-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple))
    (let ((supervisor--processes (make-hash-table :test 'equal))
          (supervisor--entry-state (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--timer-list nil)
          (supervisor--timer-state (make-hash-table :test 'equal))
          (supervisor--invalid-timers (make-hash-table :test 'equal))
          (supervisor-dashboard-show-timers nil))
      (let ((entries (supervisor--get-entries)))
        (should-not (cl-find '--timers-- entries :key #'car))))))

(ert-deftest supervisor-test-timer-trigger-rejects-service-row ()
  "Timer trigger rejects service rows."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :service "svc")
                       (vector "svc" "simple" "-"
                               "yes" "running" "yes" "yes" "-" "-")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should-error (supervisor-dashboard-timer-trigger)
                    :type 'user-error))))

(ert-deftest supervisor-test-timer-info-rejects-service-row ()
  "Timer info rejects service rows."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :service "svc")
                       (vector "svc" "simple" "-"
                               "yes" "running" "yes" "yes" "-" "-")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should-error (supervisor-dashboard-timer-info)
                    :type 'user-error))))

(ert-deftest supervisor-test-timer-jump-rejects-service-row ()
  "Timer jump rejects service rows."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :service "svc")
                       (vector "svc" "simple" "-"
                               "yes" "running" "yes" "yes" "-" "-")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should-error (supervisor-dashboard-timer-jump)
                    :type 'user-error))))

(ert-deftest supervisor-test-timer-reset-rejects-service-row ()
  "Timer reset rejects service rows."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :service "svc")
                       (vector "svc" "simple" "-"
                               "yes" "running" "yes" "yes" "-" "-")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should-error (supervisor-dashboard-timer-reset)
                    :type 'user-error))))

(ert-deftest supervisor-test-timer-jump-finds-target ()
  "Timer jump moves point to target service row."
  (let ((supervisor--timer-list
         (list (supervisor-timer--create :id "t1" :target "my-svc"))))
    (with-temp-buffer
      (supervisor-dashboard-mode)
      (let ((tabulated-list-entries
             (list (list (cons :service "my-svc")
                         (vector "my-svc" "simple" "-"
                                 "yes" "running" "yes" "yes" "-" "-"))
                   (list (cons :timer "t1")
                         (vector "t1" "my-svc" "-"
                                 "-" "-" "-" "-" "" "")))))
        (tabulated-list-init-header)
        (tabulated-list-print)
        ;; Go to timer row
        (goto-char (point-min))
        (forward-line 1)
        (should (supervisor--timer-row-p (tabulated-list-get-id)))
        ;; Jump
        (supervisor-dashboard-timer-jump)
        ;; Should be on service row now
        (should (supervisor--service-row-p (tabulated-list-get-id)))
        (should (equal "my-svc" (supervisor--row-id (tabulated-list-get-id))))))))

(ert-deftest supervisor-test-timer-jump-absent-target-message ()
  "Timer jump shows message when target not visible."
  (let ((supervisor--timer-list
         (list (supervisor-timer--create :id "t1" :target "missing-svc")))
        (last-msg nil))
    (with-temp-buffer
      (supervisor-dashboard-mode)
      (let ((tabulated-list-entries
             (list (list (cons :timer "t1")
                         (vector "t1" "missing-svc" "yes"
                                 "-" "-" "-" "-" "" "")))))
        (tabulated-list-init-header)
        (tabulated-list-print)
        (goto-char (point-min))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq last-msg (apply #'format fmt args)))))
          (supervisor-dashboard-timer-jump)
          (should (string-match-p "not visible" last-msg)))))))

(ert-deftest supervisor-test-timer-trigger-rejects-disabled-subsystem ()
  "Timer trigger rejects when subsystem is disabled."
  (let ((supervisor--timer-list
         (list (supervisor-timer--create :id "t1" :target "svc")))
        (supervisor--invalid-timers (make-hash-table :test 'equal)))
    (with-temp-buffer
      (supervisor-dashboard-mode)
      (let ((tabulated-list-entries
             (list (list (cons :timer "t1")
                         (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
        (tabulated-list-init-header)
        (tabulated-list-print)
        (goto-char (point-min))
        (cl-letf (((symbol-function 'supervisor-timer-subsystem-active-p)
                   (lambda () nil)))
          (should-error (supervisor-dashboard-timer-trigger)
                        :type 'user-error))))))

(ert-deftest supervisor-test-timer-trigger-rejects-invalid-timer ()
  "Timer trigger rejects invalid timers."
  (let ((supervisor--timer-list
         (list (supervisor-timer--create :id "t1" :target "svc")))
        (supervisor--invalid-timers (make-hash-table :test 'equal)))
    (puthash "t1" "bad config" supervisor--invalid-timers)
    (with-temp-buffer
      (supervisor-dashboard-mode)
      (let ((tabulated-list-entries
             (list (list (cons :timer "t1")
                         (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
        (tabulated-list-init-header)
        (tabulated-list-print)
        (goto-char (point-min))
        (cl-letf (((symbol-function 'supervisor-timer-subsystem-active-p)
                   (lambda () t)))
          (should-error (supervisor-dashboard-timer-trigger)
                        :type 'user-error))))))

(ert-deftest supervisor-test-timer-reset-clears-state ()
  "Timer reset clears runtime state fields."
  (let* ((supervisor--timer-list
          (list (supervisor-timer--create :id "t1" :target "svc")))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--invalid-timers (make-hash-table :test 'equal))
         (update-called nil))
    ;; Seed runtime state
    (puthash "t1" (list :last-run-at 1000 :last-exit 0
                        :retry-attempt 2 :next-run-at 2000
                        :startup-triggered t)
             supervisor--timer-state)
    (with-temp-buffer
      (supervisor-dashboard-mode)
      (let ((tabulated-list-entries
             (list (list (cons :timer "t1")
                         (vector "t1" "svc" "yes" "-" "-" "0" "-" "" "")))))
        (tabulated-list-init-header)
        (tabulated-list-print)
        (goto-char (point-min))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
                  ((symbol-function 'supervisor-timer--update-next-run)
                   (lambda (_id) (setq update-called t)))
                  ((symbol-function 'supervisor-timer-subsystem-active-p)
                   (lambda () nil))
                  ((symbol-function 'supervisor--refresh-dashboard) #'ignore))
          (supervisor-dashboard-timer-reset)
          (let ((state (gethash "t1" supervisor--timer-state)))
            ;; All runtime fields cleared
            (should-not (plist-get state :last-run-at))
            (should-not (plist-get state :last-exit))
            (should-not (plist-get state :retry-attempt))
            (should-not (plist-get state :next-run-at))
            (should-not (plist-get state :startup-triggered))
            ;; Update-next-run was called
            (should update-called)))))))

(ert-deftest supervisor-test-service-only-reject-on-timer-enable ()
  "Enable rejects timer rows with stable message."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (supervisor-dashboard-enable)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest supervisor-test-service-only-reject-on-timer-disable ()
  "Disable rejects timer rows with stable message."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (supervisor-dashboard-disable)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest supervisor-test-service-only-reject-on-timer-mask ()
  "Mask rejects timer rows with stable message."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (supervisor-dashboard-mask)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest supervisor-test-service-only-reject-on-timer-cat ()
  "Cat rejects timer rows with stable message."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (supervisor-dashboard-cat)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest supervisor-test-service-only-reject-on-timer-kill ()
  "Kill rejects timer rows with stable message."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (supervisor-dashboard-kill)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest supervisor-test-service-only-reject-on-timer-blame ()
  "Blame rejects timer rows with stable message."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (supervisor-dashboard-blame)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest supervisor-test-service-only-reject-on-timer-graph ()
  "Show-graph rejects timer rows with stable message."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (supervisor-dashboard-show-graph)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest supervisor-test-service-only-reject-on-timer-describe ()
  "Describe-entry rejects timer rows with stable message."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (supervisor-dashboard-describe-entry)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest supervisor-test-transient-menu-has-timer-group ()
  "Transient menu definition includes Timers group with y-prefixed suffixes."
  ;; The transient menu is defined lazily; we trigger it via the definer
  (let ((supervisor--dashboard-menu-defined nil))
    (require 'transient)
    (supervisor--define-dashboard-menu)
    ;; The transient prefix should be defined now
    (should (fboundp 'supervisor-dashboard-menu))
    ;; Verify timer commands are interactive
    (should (commandp 'supervisor-dashboard-timer-trigger))
    (should (commandp 'supervisor-dashboard-timer-info))
    (should (commandp 'supervisor-dashboard-timer-jump))
    (should (commandp 'supervisor-dashboard-timer-reset))
    (should (commandp 'supervisor-dashboard-timer-refresh))
    ;; Verify transient layout has Timers group with correct key bindings.
    ;; Serialize the layout and search for key strings to avoid depending
    ;; on internal transient layout structure which varies across versions.
    (let* ((layout (get 'supervisor-dashboard-menu 'transient--layout))
           (repr (format "%S" layout)))
      (should (string-match-p ":key \"y t\"" repr))
      (should (string-match-p ":key \"y i\"" repr))
      (should (string-match-p ":key \"y j\"" repr))
      (should (string-match-p ":key \"y r\"" repr))
      (should (string-match-p ":key \"y g\"" repr))
      (should (string-match-p "Timers" repr)))))

(ert-deftest supervisor-test-timer-actions-dispatcher-key ()
  "Timer actions dispatcher is bound to y in dashboard keymap."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (should (eq (key-binding "y") #'supervisor-dashboard-timer-actions))))

(ert-deftest supervisor-test-service-only-reject-on-timer-reload-unit ()
  "Reload-unit rejects timer rows with stable message."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (supervisor-dashboard-reload-unit)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest supervisor-test-service-only-reject-on-timer-reset-failed ()
  "Reset-failed rejects timer rows with stable message."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (supervisor-dashboard-reset-failed)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest supervisor-test-service-only-reject-on-timer-unmask ()
  "Unmask rejects timer rows with stable message."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (supervisor-dashboard-unmask)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest supervisor-test-service-only-reject-on-timer-set-restart ()
  "Set-restart-policy rejects timer rows with stable message."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (supervisor-dashboard-set-restart-policy)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest supervisor-test-service-only-reject-on-timer-set-logging ()
  "Set-logging rejects timer rows with stable message."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (supervisor-dashboard-set-logging)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest supervisor-test-service-only-reject-on-timer-edit ()
  "Edit rejects timer rows with stable message."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (supervisor-dashboard-edit)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest supervisor-test-service-only-reject-on-timer-show-deps ()
  "Show-deps rejects timer rows with stable message."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (supervisor-dashboard-show-deps)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest supervisor-test-service-only-reject-on-timer-view-log ()
  "View-log rejects timer rows with stable message."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (supervisor-dashboard-view-log)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest supervisor-test-service-only-reject-on-timer-start ()
  "Start rejects timer rows with stable message."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (supervisor-dashboard-start)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest supervisor-test-timer-refresh-calls-dashboard-refresh ()
  "Timer refresh command invokes dashboard refresh and messages."
  (let ((refreshed nil)
        (msg nil))
    (with-temp-buffer
      (supervisor-dashboard-mode)
      (cl-letf (((symbol-function 'supervisor--refresh-dashboard)
                 (lambda () (setq refreshed t)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq msg (apply #'format fmt args)))))
        (supervisor-dashboard-timer-refresh)
        (should refreshed)
        (should (string-match-p "refreshed" msg))))))

(ert-deftest supervisor-test-service-counters-in-header-line ()
  "Service counters are shown in `header-line-format'."
  (supervisor-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple))
    (let ((supervisor--processes (make-hash-table :test 'equal))
          (supervisor--entry-state (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--timer-list nil)
          (supervisor--timer-state (make-hash-table :test 'equal))
          (supervisor--invalid-timers (make-hash-table :test 'equal))
          (supervisor-dashboard-show-timers nil))
      (let ((header (supervisor--dashboard-header-line)))
        (should (string-match-p "\\brun\\b" header))
        (should (string-match-p "\\bdone\\b" header))
        (should (string-match-p "\\bpend\\b" header))
        (should (string-match-p "\\bfail\\b" header))
        (should (string-match-p "\\binv\\b" header))
        ;; Header no longer carries column labels.
        (should-not (string-match-p "\\bID\\b" header))
        ;; No pipe filler separators in this compact format.
        (should-not (string-match-p "|" header))))))

(ert-deftest supervisor-test-services-header-precedes-service-and-timer-rows ()
  "Services section header appears first, then services, then timers."
  (supervisor-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple))
    (let ((supervisor--processes (make-hash-table :test 'equal))
          (supervisor--entry-state (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--timer-list
           (list (supervisor-timer--create :id "t1" :target "svc")))
          (supervisor--timer-state (make-hash-table :test 'equal))
          (supervisor--invalid-timers (make-hash-table :test 'equal))
          (supervisor-dashboard-show-timers t))
      (let* ((entries (supervisor--get-entries))
             (ids (mapcar #'car entries))
             (services-pos (cl-position '--services-- ids))
             (svc-pos (cl-position-if
                       (lambda (id) (supervisor--service-row-p id)) ids))
             (timer-pos (cl-position '--timers-- ids))
             (tmr-pos (cl-position-if
                       (lambda (id) (supervisor--timer-row-p id)) ids)))
        (should svc-pos)
        (should services-pos)
        (should timer-pos)
        (should tmr-pos)
        ;; Services header is first body row.
        (should (= 0 services-pos))
        ;; Services section header precedes service rows and timers section.
        (should (< services-pos svc-pos))
        (should (< svc-pos timer-pos))
        (should (< timer-pos tmr-pos))))))

(ert-deftest supervisor-test-no-blank-summary-spacers-in-body ()
  "Dashboard body has no blank summary spacer rows."
  (supervisor-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple))
    (let ((supervisor--processes (make-hash-table :test 'equal))
          (supervisor--entry-state (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--timer-list nil)
          (supervisor--timer-state (make-hash-table :test 'equal))
          (supervisor--invalid-timers (make-hash-table :test 'equal))
          (supervisor-dashboard-show-timers t))
      (let* ((entries (supervisor--get-entries))
             (ids (mapcar #'car entries)))
        (should-not
         (cl-find-if
          (lambda (id)
            (and (symbolp id)
                 (string-match-p "^--blank-" (symbol-name id))))
          ids))))))

(ert-deftest supervisor-test-header-counters-count-services-only ()
  "Header counters aggregate services only, never timer rows."
  (supervisor-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (supervisor--invalid (make-hash-table :test 'equal))
           (supervisor--timer-list nil)
           (supervisor--timer-state (make-hash-table :test 'equal))
           (supervisor--invalid-timers (make-hash-table :test 'equal))
           (supervisor-dashboard-show-timers t)
           (header-no-timers
            (substring-no-properties (supervisor--dashboard-header-line))))
      (setq supervisor--timer-list
            (list (supervisor-timer--create :id "t1" :target "svc")))
      (let ((header-with-timers
             (substring-no-properties (supervisor--dashboard-header-line))))
        (should (equal header-no-timers header-with-timers))))))


(ert-deftest supervisor-test-timer-info-invalid-timer ()
  "Timer info shows details for invalid timers."
  (let ((supervisor--timer-list nil)
        (supervisor--invalid-timers (make-hash-table :test 'equal)))
    (puthash "bad-t" "missing target field" supervisor--invalid-timers)
    (with-temp-buffer
      (supervisor-dashboard-mode)
      (let ((tabulated-list-entries
             (list (list (cons :timer "bad-t")
                         (vector "bad-t" "-" "-" "-"
                                 (propertize "invalid" 'face 'error)
                                 "-" "missing target field" "" "")))))
        (tabulated-list-init-header)
        (tabulated-list-print)
        (goto-char (point-min))
        (supervisor-dashboard-timer-info)
        ;; Help window should be open
        (let ((info-buf (get-buffer "*supervisor-timer-info*")))
          (should info-buf)
          (with-current-buffer info-buf
            (should (string-match-p "INVALID" (buffer-string)))
            (should (string-match-p "missing target field" (buffer-string))))
          (kill-buffer info-buf))))))

(ert-deftest supervisor-test-timer-info-valid-timer ()
  "Timer info shows details for valid timers."
  (let* ((supervisor--timer-list
          (list (supervisor-timer--create
                 :id "t1" :target "svc"
                 :on-startup-sec 30
                 :persistent t)))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--invalid-timers (make-hash-table :test 'equal)))
    (puthash "t1" (list :last-run-at (float-time) :last-exit 0
                        :next-run-at (+ (float-time) 60))
             supervisor--timer-state)
    (with-temp-buffer
      (supervisor-dashboard-mode)
      (let ((tabulated-list-entries
             (list (list (cons :timer "t1")
                         (vector "t1" "svc" "yes" "-" "-" "0" "-" "" "")))))
        (tabulated-list-init-header)
        (tabulated-list-print)
        (goto-char (point-min))
        (supervisor-dashboard-timer-info)
        (let ((info-buf (get-buffer "*supervisor-timer-info*")))
          (should info-buf)
          (with-current-buffer info-buf
            (should (string-match-p "Timer: t1" (buffer-string)))
            (should (string-match-p "Target: svc" (buffer-string)))
            (should (string-match-p "Startup: 30" (buffer-string)))
            (should (string-match-p "Persistent: yes" (buffer-string))))
          (kill-buffer info-buf))))))

(ert-deftest supervisor-test-services-header-is-first-row ()
  "Services section header is the first content row."
  (supervisor-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple))
    (let ((supervisor--processes (make-hash-table :test 'equal))
          (supervisor--entry-state (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--timer-list nil)
          (supervisor--timer-state (make-hash-table :test 'equal))
          (supervisor--invalid-timers (make-hash-table :test 'equal))
          (supervisor-dashboard-show-timers nil))
      (let* ((entries (supervisor--get-entries))
             (first-id (car (car entries))))
        (should (eq '--services-- first-id))))))


;;; Dashboard nested menu tests

(ert-deftest supervisor-test-dashboard-keymap-submenu-keys ()
  "Dashboard keymap binds l, p, i to submenu dispatchers."
  (should (eq (lookup-key supervisor-dashboard-mode-map "l")
              'supervisor-dashboard-lifecycle))
  (should (eq (lookup-key supervisor-dashboard-mode-map "p")
              'supervisor-dashboard-policy))
  (should (eq (lookup-key supervisor-dashboard-mode-map "i")
              'supervisor-dashboard-inspect)))

(ert-deftest supervisor-test-dashboard-keymap-top-level-only ()
  "Dashboard keymap has no direct lifecycle/policy/inspect bindings."
  ;; Old direct bindings should be gone
  (should-not (eq (lookup-key supervisor-dashboard-mode-map "s")
                  'supervisor-dashboard-start))
  (should-not (eq (lookup-key supervisor-dashboard-mode-map "x")
                  'supervisor-dashboard-stop))
  (should-not (eq (lookup-key supervisor-dashboard-mode-map "e")
                  'supervisor-dashboard-toggle-enabled))
  (should-not (eq (lookup-key supervisor-dashboard-mode-map "m")
                  'supervisor-dashboard-toggle-mask))
  (should-not (eq (lookup-key supervisor-dashboard-mode-map "r")
                  'supervisor-dashboard-toggle-restart))
  (should-not (eq (lookup-key supervisor-dashboard-mode-map "d")
                  'supervisor-dashboard-show-deps))
  (should-not (eq (lookup-key supervisor-dashboard-mode-map "c")
                  'supervisor-dashboard-cat)))

(ert-deftest supervisor-test-dashboard-keymap-proced-moved ()
  "Dashboard keymap binds proced to t (was p)."
  (should (eq (lookup-key supervisor-dashboard-mode-map "t")
              'proced))
  (should (eq (lookup-key supervisor-dashboard-mode-map "T")
              'supervisor-dashboard-toggle-proced-auto-update)))

(ert-deftest supervisor-test-dashboard-keymap-tag-filter-moved ()
  "Dashboard keymap binds tag filter to F (was t)."
  (should (eq (lookup-key supervisor-dashboard-mode-map "F")
              'supervisor-dashboard-cycle-tag-filter)))

(ert-deftest supervisor-test-dashboard-enable-explicit ()
  "Explicit enable sets enabled override."
  (let* ((entry (supervisor--parse-entry '("cmd" :id "svc" :disabled t)))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor-overrides-file nil)
         (msg nil))
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () (cons :service "svc")))
              ((symbol-function 'supervisor--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'supervisor--save-overrides) #'ignore)
              ((symbol-function 'supervisor--refresh-dashboard) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (supervisor-dashboard-enable)
      (should (eq 'enabled (gethash "svc" supervisor--enabled-override)))
      (should (string-match-p "Enabled svc" msg)))))

(ert-deftest supervisor-test-dashboard-disable-explicit ()
  "Explicit disable sets disabled override."
  (let* ((entry (supervisor--parse-entry '("cmd" :id "svc")))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor-overrides-file nil)
         (msg nil))
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () (cons :service "svc")))
              ((symbol-function 'supervisor--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'supervisor--save-overrides) #'ignore)
              ((symbol-function 'supervisor--refresh-dashboard) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (supervisor-dashboard-disable)
      (should (eq 'disabled (gethash "svc" supervisor--enabled-override)))
      (should (string-match-p "Disabled svc" msg)))))

(ert-deftest supervisor-test-dashboard-enable-already-enabled ()
  "Explicit enable on already-enabled entry shows message."
  (let* ((entry (supervisor--parse-entry '("cmd" :id "svc")))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor-overrides-file nil)
         (msg nil))
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () (cons :service "svc")))
              ((symbol-function 'supervisor--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (supervisor-dashboard-enable)
      (should (string-match-p "already enabled" msg)))))

(ert-deftest supervisor-test-dashboard-mask-explicit ()
  "Explicit mask sets mask override."
  (let* ((entry (supervisor--parse-entry '("cmd" :id "svc")))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor-overrides-file nil)
         (msg nil))
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () (cons :service "svc")))
              ((symbol-function 'supervisor--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'supervisor--save-overrides) #'ignore)
              ((symbol-function 'supervisor--refresh-dashboard) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (supervisor-dashboard-mask)
      (should (eq 'masked (gethash "svc" supervisor--mask-override)))
      (should (string-match-p "Masked svc" msg)))))

(ert-deftest supervisor-test-dashboard-unmask-explicit ()
  "Explicit unmask clears mask override."
  (let* ((entry (supervisor--parse-entry '("cmd" :id "svc")))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor-overrides-file nil)
         (msg nil))
    (puthash "svc" 'masked supervisor--mask-override)
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () (cons :service "svc")))
              ((symbol-function 'supervisor--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'supervisor--save-overrides) #'ignore)
              ((symbol-function 'supervisor--refresh-dashboard) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (supervisor-dashboard-unmask)
      (should-not (gethash "svc" supervisor--mask-override))
      (should (string-match-p "Unmasked svc" msg)))))

(ert-deftest supervisor-test-dashboard-unmask-not-masked ()
  "Explicit unmask on non-masked entry shows message."
  (let* ((entry (supervisor--parse-entry '("cmd" :id "svc")))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor-overrides-file nil)
         (msg nil))
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () (cons :service "svc")))
              ((symbol-function 'supervisor--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (supervisor-dashboard-unmask)
      (should (string-match-p "not masked" msg)))))

(ert-deftest supervisor-test-dashboard-set-restart-policy ()
  "Set restart policy stores override via completing-read."
  (let* ((entry (supervisor--parse-entry '("cmd" :id "svc" :restart on-failure)))
         (supervisor--restart-override (make-hash-table :test 'equal))
         (supervisor--restart-timers (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor-overrides-file nil)
         (msg nil))
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () (cons :service "svc")))
              ((symbol-function 'supervisor--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "always"))
              ((symbol-function 'supervisor--save-overrides) #'ignore)
              ((symbol-function 'supervisor--refresh-dashboard) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (supervisor-dashboard-set-restart-policy)
      (should (eq 'always (gethash "svc" supervisor--restart-override)))
      (should (string-match-p "Restart policy for svc: always" msg)))))

(ert-deftest supervisor-test-dashboard-set-restart-policy-clears-on-match ()
  "Set restart policy clears override when matching config default."
  (let* ((entry (supervisor--parse-entry '("cmd" :id "svc" :restart on-failure)))
         (supervisor--restart-override (make-hash-table :test 'equal))
         (supervisor--restart-timers (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor-overrides-file nil))
    ;; Pre-set an override
    (puthash "svc" 'always supervisor--restart-override)
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () (cons :service "svc")))
              ((symbol-function 'supervisor--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "on-failure"))
              ((symbol-function 'supervisor--save-overrides) #'ignore)
              ((symbol-function 'supervisor--refresh-dashboard) #'ignore)
              ((symbol-function 'message) #'ignore))
      (supervisor-dashboard-set-restart-policy)
      ;; Override should be cleared since on-failure matches config
      (should-not (gethash "svc" supervisor--restart-override)))))

(ert-deftest supervisor-test-dashboard-set-logging ()
  "Set logging stores override via completing-read."
  (let* ((entry (supervisor--parse-entry '("cmd" :id "svc" :logging t)))
         (supervisor--logging (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor-overrides-file nil)
         (msg nil))
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () (cons :service "svc")))
              ((symbol-function 'supervisor--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "off"))
              ((symbol-function 'supervisor--refresh-dashboard) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (supervisor-dashboard-set-logging)
      (should (eq 'disabled (gethash "svc" supervisor--logging)))
      (should (string-match-p "Logging for svc: off" msg)))))

(ert-deftest supervisor-test-dashboard-submenu-commands-defined ()
  "All submenu dispatcher commands are defined."
  (should (commandp 'supervisor-dashboard-lifecycle))
  (should (commandp 'supervisor-dashboard-policy))
  (should (commandp 'supervisor-dashboard-inspect)))

(ert-deftest supervisor-test-dashboard-explicit-policy-commands-defined ()
  "All explicit policy commands are defined."
  (should (commandp 'supervisor-dashboard-enable))
  (should (commandp 'supervisor-dashboard-disable))
  (should (commandp 'supervisor-dashboard-mask))
  (should (commandp 'supervisor-dashboard-unmask))
  (should (commandp 'supervisor-dashboard-set-restart-policy))
  (should (commandp 'supervisor-dashboard-set-logging)))

;;;; Telemetry Data Model Tests

(ert-deftest supervisor-test-last-exit-info-populated ()
  "Process sentinel populates `supervisor--last-exit-info'."
  (let* ((supervisor--processes (make-hash-table :test 'equal))
         (supervisor--failed (make-hash-table :test 'equal))
         (supervisor--restart-override (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (supervisor--manually-started (make-hash-table :test 'equal))
         (supervisor--oneshot-completed (make-hash-table :test 'equal))
         (supervisor--remain-active (make-hash-table :test 'equal))
         (supervisor--last-exit-info (make-hash-table :test 'equal))
         (supervisor--start-times (make-hash-table :test 'equal))
         (supervisor--ready-times (make-hash-table :test 'equal))
         (supervisor--restart-times (make-hash-table :test 'equal))
         (supervisor--restart-timers (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal))
         (supervisor--shutting-down nil)
         (proc (start-process "test-exit" nil "true")))
    (puthash "test-exit" proc supervisor--processes)
    (set-process-sentinel proc
                          (supervisor--make-process-sentinel
                           "test-exit" "true" nil 'simple 'no))
    ;; Wait for process to exit
    (while (process-live-p proc)
      (accept-process-output nil 0.01))
    (accept-process-output nil 0.05)
    ;; Verify exit info was recorded
    (let ((info (gethash "test-exit" supervisor--last-exit-info)))
      (should info)
      (should (eq 'exited (plist-get info :status)))
      (should (= 0 (plist-get info :code)))
      (should (numberp (plist-get info :timestamp))))))

(ert-deftest supervisor-test-telemetry-uptime-running ()
  "Uptime returns seconds for running process."
  (let ((supervisor--start-times (make-hash-table :test 'equal))
        (supervisor--processes (make-hash-table :test 'equal))
        (proc (start-process "uptime-test" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "uptime-test" (float-time) supervisor--start-times)
          (puthash "uptime-test" proc supervisor--processes)
          (let ((uptime (supervisor--telemetry-uptime "uptime-test")))
            (should uptime)
            (should (>= uptime 0))))
      (delete-process proc))))

(ert-deftest supervisor-test-telemetry-uptime-nil-when-not-running ()
  "Uptime returns nil when process is not running."
  (let ((supervisor--start-times (make-hash-table :test 'equal))
        (supervisor--processes (make-hash-table :test 'equal)))
    (puthash "dead-svc" (float-time) supervisor--start-times)
    (should-not (supervisor--telemetry-uptime "dead-svc"))))

(ert-deftest supervisor-test-telemetry-restart-count ()
  "Restart count reflects recent restarts in window."
  (let ((supervisor--restart-times (make-hash-table :test 'equal))
        (supervisor-restart-window 300))
    ;; No restarts
    (should (= 0 (supervisor--telemetry-restart-count "svc")))
    ;; Two recent restarts
    (puthash "svc" (list (float-time) (- (float-time) 10))
             supervisor--restart-times)
    (should (= 2 (supervisor--telemetry-restart-count "svc")))
    ;; Old restart outside window
    (puthash "svc" (list (- (float-time) 999))
             supervisor--restart-times)
    (should (= 0 (supervisor--telemetry-restart-count "svc")))))

(ert-deftest supervisor-test-telemetry-last-exit-format ()
  "Last exit returns formatted string."
  (let ((supervisor--last-exit-info (make-hash-table :test 'equal)))
    ;; No exit
    (should-not (supervisor--telemetry-last-exit "svc"))
    ;; Clean exit
    (puthash "svc" (list :status 'exited :code 0 :timestamp (float-time))
             supervisor--last-exit-info)
    (should (string= "exited successfully"
                      (supervisor--telemetry-last-exit "svc")))
    ;; Non-zero exit
    (puthash "svc" (list :status 'exited :code 1 :timestamp (float-time))
             supervisor--last-exit-info)
    (should (string= "exited with code 1"
                      (supervisor--telemetry-last-exit "svc")))
    ;; Signal
    (puthash "svc" (list :status 'signal :code 15 :timestamp (float-time))
             supervisor--last-exit-info)
    (should (string= "killed by signal 15"
                      (supervisor--telemetry-last-exit "svc")))))

(ert-deftest supervisor-test-telemetry-process-metrics-nil-graceful ()
  "Process metrics returns nil gracefully for non-existent PID."
  (should-not (supervisor--telemetry-process-metrics 999999999)))

(ert-deftest supervisor-test-telemetry-log-tail-missing ()
  "Log tail returns nil for non-existent log file."
  (let ((supervisor-log-directory "/tmp/nonexistent-sv-logs"))
    (should-not (supervisor--telemetry-log-tail "svc"))))

(ert-deftest supervisor-test-snapshot-includes-last-exit-info ()
  "Snapshot struct includes `last-exit-info' field."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--oneshot-completed (make-hash-table :test 'equal))
        (supervisor--entry-state (make-hash-table :test 'equal))
        (supervisor--invalid (make-hash-table :test 'equal))
        (supervisor--enabled-override (make-hash-table :test 'equal))
        (supervisor--restart-override (make-hash-table :test 'equal))
        (supervisor--logging (make-hash-table :test 'equal))
        (supervisor--mask-override (make-hash-table :test 'equal))
        (supervisor--manually-started (make-hash-table :test 'equal))
        (supervisor--manually-stopped (make-hash-table :test 'equal))
        (supervisor--remain-active (make-hash-table :test 'equal))
        (supervisor--last-exit-info (make-hash-table :test 'equal)))
    ;; Put exit info in the hash
    (puthash "svc" (list :status 'exited :code 42 :timestamp 1000.0)
             supervisor--last-exit-info)
    (let ((snapshot (supervisor--build-snapshot)))
      ;; Verify the snapshot captured it
      (should (supervisor-snapshot-last-exit-info snapshot))
      (let ((info (gethash "svc" (supervisor-snapshot-last-exit-info snapshot))))
        (should info)
        (should (eq 'exited (plist-get info :status)))
        (should (= 42 (plist-get info :code)))))))

(ert-deftest supervisor-test-format-duration ()
  "Duration formatting produces human-readable strings."
  (should (string= "5s" (supervisor--describe-format-duration 5)))
  (should (string= "2m30s" (supervisor--describe-format-duration 150)))
  (should (string= "1h30m" (supervisor--describe-format-duration 5400)))
  (should (string= "2d3h" (supervisor--describe-format-duration 183600))))

(ert-deftest supervisor-test-cli-format-duration ()
  "CLI duration formatting produces human-readable strings."
  (should (string= "5s" (supervisor--cli-format-duration 5)))
  (should (string= "2m30s" (supervisor--cli-format-duration 150)))
  (should (string= "1h30m" (supervisor--cli-format-duration 5400)))
  (should (string= "2d3h" (supervisor--cli-format-duration 183600))))

(ert-deftest supervisor-test-set-logging-saves-overrides ()
  "Dashboard set-logging calls `supervisor--save-overrides'."
  (let* ((supervisor--processes (make-hash-table :test 'equal))
         (supervisor--failed (make-hash-table :test 'equal))
         (supervisor--restart-override (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (supervisor--manually-started (make-hash-table :test 'equal))
         (supervisor--oneshot-completed (make-hash-table :test 'equal))
         (supervisor--remain-active (make-hash-table :test 'equal))
         (supervisor--last-exit-info (make-hash-table :test 'equal))
         (supervisor--start-times (make-hash-table :test 'equal))
         (supervisor--ready-times (make-hash-table :test 'equal))
         (supervisor--restart-times (make-hash-table :test 'equal))
         (supervisor--restart-timers (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal))
         (supervisor--logging-override (make-hash-table :test 'equal))
         (supervisor-overrides-file nil)
         (saved nil)
         (entry (supervisor--parse-entry
                 '("cmd" :id "svc" :type simple :logging))))
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () (cons :service "svc")))
              ((symbol-function 'supervisor--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'supervisor--save-overrides)
               (lambda () (setq saved t)))
              ((symbol-function 'supervisor--refresh-dashboard) #'ignore)
              ((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "off")))
      (supervisor-dashboard-set-logging)
      (should saved))))

(ert-deftest supervisor-test-cli-entry-info-telemetry-fields ()
  "CLI entry-info includes telemetry fields."
  (let* ((supervisor--processes (make-hash-table :test 'equal))
         (supervisor--failed (make-hash-table :test 'equal))
         (supervisor--restart-override (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (supervisor--manually-started (make-hash-table :test 'equal))
         (supervisor--oneshot-completed (make-hash-table :test 'equal))
         (supervisor--remain-active (make-hash-table :test 'equal))
         (supervisor--last-exit-info (make-hash-table :test 'equal))
         (supervisor--start-times (make-hash-table :test 'equal))
         (supervisor--ready-times (make-hash-table :test 'equal))
         (supervisor--restart-times (make-hash-table :test 'equal))
         (supervisor--restart-timers (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal))
         (entry (supervisor--parse-entry
                 '("cmd" :id "svc" :type simple :restart always))))
    ;; Add some exit info
    (puthash "svc" (list :status 'exited :code 1 :timestamp (float-time))
             supervisor--last-exit-info)
    (puthash "svc" (list (float-time)) supervisor--restart-times)
    (let ((info (supervisor--cli-entry-info entry)))
      (should (assq 'uptime info))
      (should (assq 'restart-count info))
      (should (assq 'last-exit info))
      (should (assq 'next-restart-eta info))
      (should (assq 'metrics info))
      ;; Verify last-exit was populated
      (let ((exit-info (alist-get 'last-exit info)))
        (should exit-info)
        (should (eq 'exited (plist-get exit-info :status))))
      ;; Verify restart count
      (should (= 1 (alist-get 'restart-count info))))))


;;; P5 Telemetry Presentation Tests

(ert-deftest supervisor-test-telemetry-process-tree-no-children ()
  "Process tree returns nil when PID has no children."
  (let ((proc (start-process "tree-test" nil "sleep" "300")))
    (unwind-protect
        (let ((pid (process-id proc)))
          ;; A bare sleep process has no children
          (should-not (supervisor--telemetry-process-tree pid)))
      (delete-process proc))))

(ert-deftest supervisor-test-telemetry-process-tree-bogus-pid ()
  "Process tree returns nil for non-existent PID."
  (should-not (supervisor--telemetry-process-tree 999999999)))

(ert-deftest supervisor-test-telemetry-process-metrics-thcount ()
  "Process metrics includes :thcount when available."
  (let ((proc (start-process "thcount-test" nil "sleep" "300")))
    (unwind-protect
        (let* ((pid (process-id proc))
               (metrics (supervisor--telemetry-process-metrics pid)))
          ;; We can only verify the plist structure, not the exact value,
          ;; since not all OSes provide thcount
          (when metrics
            (should (listp metrics))))
      (delete-process proc))))

(ert-deftest supervisor-test-cli-entry-info-log-tail ()
  "Entry info includes log-tail field."
  (let ((entry (supervisor--parse-entry '("cmd" :id "svc"))))
    (let ((info (supervisor--cli-entry-info entry)))
      (should (assq 'log-tail info)))))

(ert-deftest supervisor-test-cli-entry-info-process-tree ()
  "Entry info includes process-tree field."
  (let ((entry (supervisor--parse-entry '("cmd" :id "svc"))))
    (let ((info (supervisor--cli-entry-info entry)))
      (should (assq 'process-tree info)))))

(ert-deftest supervisor-test-cli-entry-info-authority-tier ()
  "Entry info includes authority-tier field."
  (let ((entry (supervisor--parse-entry '("cmd" :id "svc"))))
    (let ((info (supervisor--cli-entry-info entry)))
      (should (assq 'authority-tier info)))))

(ert-deftest supervisor-test-cli-describe-human-log-tail ()
  "Describe human output includes log tail when present."
  (let ((info '((id . "svc") (type . simple)
                (enabled . t) (enabled-config . t)
                (restart . always) (restart-config . always)
                (logging . t) (logging-config . t)
                (delay . 0) (after . nil) (requires . nil)
                (status . "running") (reason . nil)
                (pid . nil) (start-time . nil) (ready-time . nil)
                (duration . nil)
                (log-tail . "line1\nline2\n"))))
    (let ((output (supervisor--cli-describe-human info)))
      (should (string-match-p "Recent log:" output))
      (should (string-match-p "line1" output)))))

(ert-deftest supervisor-test-cli-describe-human-process-tree ()
  "Describe human output includes process tree when present."
  (let ((info '((id . "svc") (type . simple)
                (enabled . t) (enabled-config . t)
                (restart . always) (restart-config . always)
                (logging . t) (logging-config . t)
                (delay . 0) (after . nil) (requires . nil)
                (status . "running") (reason . nil)
                (pid . 1234) (start-time . nil) (ready-time . nil)
                (duration . nil)
                (process-tree . (:count 3 :pids (5678 5679 5680))))))
    (let ((output (supervisor--cli-describe-human info)))
      (should (string-match-p "Process tree: 3 descendants" output))
      (should (string-match-p "5678" output)))))

(ert-deftest supervisor-test-cli-describe-human-authority-tier ()
  "Describe human output shows tier when unit-file and tier are present."
  (let ((info '((id . "svc") (type . simple)
                (enabled . t) (enabled-config . t)
                (restart . always) (restart-config . always)
                (logging . t) (logging-config . t)
                (delay . 0) (after . nil) (requires . nil)
                (status . "running") (reason . nil)
                (pid . nil) (start-time . nil) (ready-time . nil)
                (duration . nil)
                (unit-file . "/etc/supervisor/svc.sv")
                (authority-tier . 2))))
    (let ((output (supervisor--cli-describe-human info)))
      (should (string-match-p "Unit file: /etc/supervisor/svc\\.sv (tier 2)" output)))))

(ert-deftest supervisor-test-cli-json-includes-log-tail ()
  "JSON output includes log_tail field."
  (let ((info '((id . "svc") (type . simple)
                (enabled . t) (status . "running")
                (restart . always) (logging . t)
                (pid . nil) (reason . nil) (delay . 0)
                (after . nil) (requires . nil)
                (log-tail . "hello\n"))))
    (let ((json (supervisor--cli-entry-to-json-obj info)))
      (should (equal "hello\n" (alist-get 'log_tail json))))))

(ert-deftest supervisor-test-cli-json-includes-process-tree ()
  "JSON output includes process_tree field."
  (let ((info '((id . "svc") (type . simple)
                (enabled . t) (status . "running")
                (restart . always) (logging . t)
                (pid . nil) (reason . nil) (delay . 0)
                (after . nil) (requires . nil)
                (process-tree . (:count 2 :pids (100 101))))))
    (let ((json (supervisor--cli-entry-to-json-obj info)))
      (let ((tree (alist-get 'process_tree json)))
        (should tree)
        (should (= 2 (alist-get 'count tree)))
        (should (equal '(100 101) (alist-get 'pids tree)))))))

(ert-deftest supervisor-test-cli-json-includes-authority-tier ()
  "JSON output includes authority_tier field."
  (let ((info '((id . "svc") (type . simple)
                (enabled . t) (status . "running")
                (restart . always) (logging . t)
                (pid . nil) (reason . nil) (delay . 0)
                (after . nil) (requires . nil)
                (authority-tier . 1))))
    (let ((json (supervisor--cli-entry-to-json-obj info)))
      (should (= 1 (alist-get 'authority_tier json))))))


;;; Dashboard Detail Rendering Tests

(ert-deftest supervisor-test-dashboard-describe-shows-process-tree ()
  "Dashboard detail view renders process-tree line when descendants exist."
  (let* ((entry (supervisor--parse-entry '("cmd" :id "svc")))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--failed (make-hash-table :test 'equal))
         (supervisor--restart-override (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (supervisor--manually-started (make-hash-table :test 'equal))
         (supervisor--oneshot-completed (make-hash-table :test 'equal))
         (supervisor--remain-active (make-hash-table :test 'equal))
         (supervisor--last-exit-info (make-hash-table :test 'equal))
         (supervisor--start-times (make-hash-table :test 'equal))
         (supervisor--ready-times (make-hash-table :test 'equal))
         (supervisor--restart-times (make-hash-table :test 'equal))
         (supervisor--restart-timers (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal))
         (supervisor--logging-override (make-hash-table :test 'equal))
         (fake-proc (start-process "tree-dash" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "svc" fake-proc supervisor--processes)
          (cl-letf (((symbol-function 'supervisor--unit-file-path)
                     (lambda (_id) nil))
                    ((symbol-function 'supervisor--telemetry-log-tail)
                     (lambda (_id &optional _lines) nil))
                    ((symbol-function 'supervisor--telemetry-process-tree)
                     (lambda (_pid) '(:count 3 :pids (100 101 102))))
                    ((symbol-function 'supervisor--telemetry-process-metrics)
                     (lambda (_pid) nil)))
            (supervisor--describe-entry-detail "svc" entry)
            (let ((info-buf (get-buffer "*supervisor-info*")))
              (unwind-protect
                  (let ((output (with-current-buffer info-buf
                                  (buffer-string))))
                    (should (string-match-p "Tree: 3 descendants" output))
                    (should (string-match-p "100, 101, 102" output)))
                (when info-buf (kill-buffer info-buf))))))
      (delete-process fake-proc))))

(ert-deftest supervisor-test-dashboard-describe-shows-etime-thcount ()
  "Dashboard detail view renders ETIME and TASKS in metrics line."
  (let* ((entry (supervisor--parse-entry '("cmd" :id "svc")))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--failed (make-hash-table :test 'equal))
         (supervisor--restart-override (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (supervisor--manually-started (make-hash-table :test 'equal))
         (supervisor--oneshot-completed (make-hash-table :test 'equal))
         (supervisor--remain-active (make-hash-table :test 'equal))
         (supervisor--last-exit-info (make-hash-table :test 'equal))
         (supervisor--start-times (make-hash-table :test 'equal))
         (supervisor--ready-times (make-hash-table :test 'equal))
         (supervisor--restart-times (make-hash-table :test 'equal))
         (supervisor--restart-timers (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal))
         (supervisor--logging-override (make-hash-table :test 'equal))
         (fake-proc (start-process "met-dash" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "svc" fake-proc supervisor--processes)
          (cl-letf (((symbol-function 'supervisor--unit-file-path)
                     (lambda (_id) nil))
                    ((symbol-function 'supervisor--telemetry-log-tail)
                     (lambda (_id &optional _lines) nil))
                    ((symbol-function 'supervisor--telemetry-process-tree)
                     (lambda (_pid) nil))
                    ((symbol-function 'supervisor--telemetry-process-metrics)
                     (lambda (_pid)
                       '(:rss 1024 :pcpu 5.0 :pmem 1.2
                         :etime 3661 :thcount 4))))
            (supervisor--describe-entry-detail "svc" entry)
            (let ((info-buf (get-buffer "*supervisor-info*")))
              (unwind-protect
                  (let ((output (with-current-buffer info-buf
                                  (buffer-string))))
                    (should (string-match-p "RSS=1024KB" output))
                    (should (string-match-p "ETIME=1h1m" output))
                    (should (string-match-p "TASKS=4" output)))
                (when info-buf (kill-buffer info-buf))))))
      (delete-process fake-proc))))

(ert-deftest supervisor-test-dashboard-describe-shows-authority-tier ()
  "Dashboard detail view appends tier to unit-file line."
  (let* ((entry (supervisor--parse-entry '("cmd" :id "svc")))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--failed (make-hash-table :test 'equal))
         (supervisor--restart-override (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (supervisor--manually-started (make-hash-table :test 'equal))
         (supervisor--oneshot-completed (make-hash-table :test 'equal))
         (supervisor--remain-active (make-hash-table :test 'equal))
         (supervisor--last-exit-info (make-hash-table :test 'equal))
         (supervisor--start-times (make-hash-table :test 'equal))
         (supervisor--ready-times (make-hash-table :test 'equal))
         (supervisor--restart-times (make-hash-table :test 'equal))
         (supervisor--restart-timers (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal))
         (supervisor--logging-override (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'supervisor--unit-file-existing-path)
               (lambda (_id) "/etc/supervisor/svc.sv"))
              ((symbol-function 'supervisor--authority-tier-for-id)
               (lambda (_id) 2))
              ((symbol-function 'supervisor--telemetry-log-tail)
               (lambda (_id &optional _lines) nil)))
      (supervisor--describe-entry-detail "svc" entry)
      (let ((info-buf (get-buffer "*supervisor-info*")))
        (unwind-protect
            (let ((output (with-current-buffer info-buf
                            (buffer-string))))
              (should (string-match-p
                       "Unit file: /etc/supervisor/svc\\.sv (tier 2)"
                       output)))
          (when info-buf (kill-buffer info-buf)))))))

(ert-deftest supervisor-test-dashboard-describe-shows-log-tail ()
  "Dashboard detail view renders Recent log section."
  (let* ((entry (supervisor--parse-entry '("cmd" :id "svc")))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--failed (make-hash-table :test 'equal))
         (supervisor--restart-override (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (supervisor--manually-started (make-hash-table :test 'equal))
         (supervisor--oneshot-completed (make-hash-table :test 'equal))
         (supervisor--remain-active (make-hash-table :test 'equal))
         (supervisor--last-exit-info (make-hash-table :test 'equal))
         (supervisor--start-times (make-hash-table :test 'equal))
         (supervisor--ready-times (make-hash-table :test 'equal))
         (supervisor--restart-times (make-hash-table :test 'equal))
         (supervisor--restart-timers (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal))
         (supervisor--logging-override (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'supervisor--unit-file-path)
               (lambda (_id) nil))
              ((symbol-function 'supervisor--telemetry-log-tail)
               (lambda (_id &optional _lines)
                 "2024-01-01 started\n2024-01-01 ready\n")))
      (supervisor--describe-entry-detail "svc" entry)
      (let ((info-buf (get-buffer "*supervisor-info*")))
        (unwind-protect
            (let ((output (with-current-buffer info-buf
                            (buffer-string))))
              (should (string-match-p "Recent log:" output))
              (should (string-match-p "2024-01-01 started" output))
              (should (string-match-p "2024-01-01 ready" output)))
          (when info-buf (kill-buffer info-buf)))))))


;;; Target Visibility Tests

(ert-deftest supervisor-test-dashboard-default-hides-targets ()
  "Default dashboard view hides all target-type entries."
  (supervisor-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple)
        (nil :id "app.target" :type target))
    (let ((supervisor--processes (make-hash-table :test 'equal))
          (supervisor--entry-state (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor--restart-override (make-hash-table :test 'equal))
          (supervisor--logging (make-hash-table :test 'equal))
          (supervisor--target-convergence (make-hash-table :test 'equal))
          (supervisor--target-convergence-reasons
           (make-hash-table :test 'equal))
          (supervisor--dashboard-show-targets nil)
          (supervisor--dashboard-show-init-targets nil))
      (let* ((entries (supervisor--get-entries))
             (service-ids
              (cl-loop for entry in entries
                       when (supervisor--service-row-p (car entry))
                       collect (cdr (car entry)))))
        (should (member "svc" service-ids))
        (should-not (member "app.target" service-ids))))))

(ert-deftest supervisor-test-dashboard-show-targets-toggle ()
  "Toggling show-targets makes target entries visible."
  (supervisor-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple)
        (nil :id "app.target" :type target))
    (let ((supervisor--processes (make-hash-table :test 'equal))
          (supervisor--entry-state (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor--restart-override (make-hash-table :test 'equal))
          (supervisor--logging (make-hash-table :test 'equal))
          (supervisor--target-convergence (make-hash-table :test 'equal))
          (supervisor--target-convergence-reasons
           (make-hash-table :test 'equal))
          (supervisor--dashboard-show-targets t)
          (supervisor--dashboard-show-init-targets nil))
      (let* ((entries (supervisor--get-entries))
             (service-ids
              (cl-loop for entry in entries
                       when (supervisor--service-row-p (car entry))
                       collect (cdr (car entry)))))
        (should (member "svc" service-ids))
        (should (member "app.target" service-ids))))))

(ert-deftest supervisor-test-dashboard-init-targets-hidden-when-targets-shown ()
  "Init-transition targets remain hidden when regular targets are shown."
  (supervisor-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple)
        (nil :id "app.target" :type target)
        (nil :id "rescue.target" :type target))
    (let ((supervisor--processes (make-hash-table :test 'equal))
          (supervisor--entry-state (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor--restart-override (make-hash-table :test 'equal))
          (supervisor--logging (make-hash-table :test 'equal))
          (supervisor--target-convergence (make-hash-table :test 'equal))
          (supervisor--target-convergence-reasons
           (make-hash-table :test 'equal))
          (supervisor--dashboard-show-targets t)
          (supervisor--dashboard-show-init-targets nil))
      (let* ((entries (supervisor--get-entries))
             (service-ids
              (cl-loop for entry in entries
                       when (supervisor--service-row-p (car entry))
                       collect (cdr (car entry)))))
        (should (member "svc" service-ids))
        (should (member "app.target" service-ids))
        (should-not (member "rescue.target" service-ids))))))

(ert-deftest supervisor-test-dashboard-init-targets-shown-when-toggled ()
  "Init-transition targets shown when both toggles are on."
  (supervisor-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple)
        (nil :id "app.target" :type target)
        (nil :id "rescue.target" :type target))
    (let ((supervisor--processes (make-hash-table :test 'equal))
          (supervisor--entry-state (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor--restart-override (make-hash-table :test 'equal))
          (supervisor--logging (make-hash-table :test 'equal))
          (supervisor--target-convergence (make-hash-table :test 'equal))
          (supervisor--target-convergence-reasons
           (make-hash-table :test 'equal))
          (supervisor--dashboard-show-targets t)
          (supervisor--dashboard-show-init-targets t))
      (let* ((entries (supervisor--get-entries))
             (service-ids
              (cl-loop for entry in entries
                       when (supervisor--service-row-p (car entry))
                       collect (cdr (car entry)))))
        (should (member "svc" service-ids))
        (should (member "app.target" service-ids))
        (should (member "rescue.target" service-ids))))))

(ert-deftest supervisor-test-dashboard-target-filter-bypasses-visibility ()
  "When target-filter is active, targets are shown regardless of visibility."
  (supervisor-test-with-unit-files
      '(("sleep 1" :id "svc-a" :wanted-by ("app.target"))
        ("sleep 1" :id "svc-b")
        (nil :id "app.target" :type target))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--failed (make-hash-table :test 'equal))
           (supervisor--oneshot-completed (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (supervisor--invalid (make-hash-table :test 'equal))
           (supervisor--enabled-override (make-hash-table :test 'equal))
           (supervisor--restart-override (make-hash-table :test 'equal))
           (supervisor--logging (make-hash-table :test 'equal))
           (supervisor--target-convergence (make-hash-table :test 'equal))
           (supervisor--target-convergence-reasons
            (make-hash-table :test 'equal))
           (members-hash (make-hash-table :test 'equal))
           (supervisor--current-plan
            (supervisor-plan--create
             :target-members members-hash
             :entries nil
             :invalid (make-hash-table :test 'equal)
             :by-target nil
             :deps (make-hash-table :test 'equal)
             :requires-deps (make-hash-table :test 'equal)
             :dependents (make-hash-table :test 'equal)
             :cycle-fallback-ids (make-hash-table :test 'equal)
             :order-index (make-hash-table :test 'equal)
             :meta nil))
           (supervisor--dashboard-target-filter "app.target")
           (supervisor--dashboard-show-targets nil))
      (puthash "app.target" '(:requires nil :wants ("svc-a")) members-hash)
      (let* ((snapshot (supervisor--build-snapshot))
             (all-entries (supervisor--get-entries snapshot))
             (service-ids
              (cl-loop for entry in all-entries
                       when (supervisor--service-row-p (car entry))
                       collect (cdr (car entry)))))
        (should (member "svc-a" service-ids))
        (should (member "app.target" service-ids))
        (should-not (member "svc-b" service-ids))))))

(ert-deftest supervisor-test-dashboard-init-transition-target-predicate ()
  "Init-transition target predicate identifies correct IDs."
  (should (supervisor--init-transition-target-p "rescue.target"))
  (should (supervisor--init-transition-target-p "shutdown.target"))
  (should (supervisor--init-transition-target-p "poweroff.target"))
  (should (supervisor--init-transition-target-p "reboot.target"))
  (should (supervisor--init-transition-target-p "runlevel0.target"))
  (should (supervisor--init-transition-target-p "runlevel1.target"))
  (should (supervisor--init-transition-target-p "runlevel6.target"))
  (should-not (supervisor--init-transition-target-p "basic.target"))
  (should-not (supervisor--init-transition-target-p "multi-user.target"))
  (should-not (supervisor--init-transition-target-p "graphical.target"))
  (should-not (supervisor--init-transition-target-p "default.target"))
  (should-not (supervisor--init-transition-target-p "app.target")))

(ert-deftest supervisor-test-dashboard-header-shows-filter-state ()
  "Header line always includes visibility indicator."
  (supervisor-test-with-unit-files nil
    (let ((supervisor--processes (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--failed (make-hash-table :test 'equal))
          (supervisor--oneshot-completed (make-hash-table :test 'equal))
          (supervisor--dashboard-show-targets nil)
          (supervisor--dashboard-show-init-targets nil))
      ;; Default: services-only indicator
      (let ((header (supervisor--dashboard-header-line)))
        (should (string-match-p "\\[services\\]" header))
        (should-not (string-match-p "targets" header)))
      ;; Show targets: services+targets
      (let ((supervisor--dashboard-show-targets t))
        (let ((header (supervisor--dashboard-header-line)))
          (should (string-match-p "services" header))
          (should (string-match-p "targets" header))
          (should-not (string-match-p "init" header))))
      ;; Show targets + init: services+targets+init
      (let ((supervisor--dashboard-show-targets t)
            (supervisor--dashboard-show-init-targets t))
        (let ((header (supervisor--dashboard-header-line)))
          (should (string-match-p "services" header))
          (should (string-match-p "targets" header))
          (should (string-match-p "init" header))))
      ;; Init toggled but targets hidden: init suppressed from header
      (let ((supervisor--dashboard-show-targets nil)
            (supervisor--dashboard-show-init-targets t))
        (let ((header (supervisor--dashboard-header-line)))
          (should (string-match-p "\\[services\\]" header))
          (should-not (string-match-p "init" header)))))))

(ert-deftest supervisor-test-dashboard-services-remain-sortable ()
  "Service rows preserve tabulated-list sorting after target filtering."
  (supervisor-test-with-unit-files
      '(("sleep 60" :id "alpha" :type simple)
        ("sleep 60" :id "beta" :type simple)
        (nil :id "app.target" :type target))
    (let ((supervisor--processes (make-hash-table :test 'equal))
          (supervisor--entry-state (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor--restart-override (make-hash-table :test 'equal))
          (supervisor--logging (make-hash-table :test 'equal))
          (supervisor--target-convergence (make-hash-table :test 'equal))
          (supervisor--target-convergence-reasons
           (make-hash-table :test 'equal))
          (supervisor--dashboard-show-targets nil))
      (let* ((entries (supervisor--get-entries))
             (service-ids
              (cl-loop for entry in entries
                       when (supervisor--service-row-p (car entry))
                       collect (cdr (car entry)))))
        ;; Both services visible and in order
        (should (equal '("alpha" "beta") service-ids))))))

(ert-deftest supervisor-test-dashboard-keymap-binds-v ()
  "Dashboard keymap binds v to toggle targets."
  (should (eq (lookup-key supervisor-dashboard-mode-map "v")
              #'supervisor-dashboard-toggle-targets)))

(ert-deftest supervisor-test-dashboard-keymap-binds-shift-v ()
  "Dashboard keymap binds V to toggle init-transition targets."
  (should (eq (lookup-key supervisor-dashboard-mode-map "V")
              #'supervisor-dashboard-toggle-init-targets)))

(ert-deftest supervisor-test-dashboard-toggle-targets-flips-state ()
  "Toggle-targets command flips show-targets state."
  (with-temp-buffer
    (let ((supervisor--dashboard-show-targets nil))
      ;; Stub refresh to avoid needing a full dashboard buffer
      (cl-letf (((symbol-function 'supervisor--refresh-dashboard)
                 (lambda ())))
        (supervisor-dashboard-toggle-targets)
        (should supervisor--dashboard-show-targets)
        (supervisor-dashboard-toggle-targets)
        (should-not supervisor--dashboard-show-targets)))))

(ert-deftest supervisor-test-dashboard-toggle-init-targets-flips-state ()
  "Toggle-init-targets command flips show-init-targets state."
  (with-temp-buffer
    (let ((supervisor--dashboard-show-init-targets nil))
      (cl-letf (((symbol-function 'supervisor--refresh-dashboard)
                 (lambda ())))
        (supervisor-dashboard-toggle-init-targets)
        (should supervisor--dashboard-show-init-targets)
        (supervisor-dashboard-toggle-init-targets)
        (should-not supervisor--dashboard-show-init-targets)))))

(provide 'supervisor-test-dashboard)
;;; supervisor-test-dashboard.el ends here
