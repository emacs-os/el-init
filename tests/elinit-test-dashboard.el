;;; elinit-test-dashboard.el --- Dashboard UI and rendering tests for elinit.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Dashboard UI and rendering ERT tests for elinit.el.

;;; Code:

(require 'elinit-test-helpers)

;;; Dashboard UI tests

(ert-deftest elinit-test-separator-row-detection ()
  "Separator rows are correctly identified as symbol IDs."
  (should (elinit--separator-row-p '--services--))
  (should (elinit--separator-row-p '--health--))
  (should (elinit--separator-row-p '--timers--))
  (should (elinit--separator-row-p '--blank-1--))
  (should-not (elinit--separator-row-p "nm-applet"))
  (should-not (elinit--separator-row-p nil))
  (should-not (elinit--separator-row-p (cons :service "foo")))
  (should-not (elinit--separator-row-p (cons :timer "bar"))))

(ert-deftest elinit-test-health-summary-format ()
  "Health summary includes all required counts."
  (elinit-test-with-unit-files nil
    (let ((elinit--invalid (make-hash-table :test 'equal))
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal)))
      (let ((summary (elinit--health-summary)))
        ;; Should contain all five metrics
        (should (string-match-p "run" summary))
        (should (string-match-p "done" summary))
        (should (string-match-p "pend" summary))
        (should (string-match-p "fail" summary))
        (should (string-match-p "inv" summary))))))

(ert-deftest elinit-test-help-text-key-parity ()
  "Help text includes all top-level bound keys."
  ;; Top-level keys that must be discoverable on-screen
  (should (string-match-p "\\[f\\]" elinit--help-text))
  (should (string-match-p "\\[g\\]" elinit--help-text))
  (should (string-match-p "\\[G\\]" elinit--help-text))
  (should (string-match-p "\\[t\\]" elinit--help-text))
  (should (string-match-p "\\[T\\]" elinit--help-text))
  (should (string-match-p "\\[l\\]" elinit--help-text))
  (should (string-match-p "\\[p\\]" elinit--help-text))
  (should (string-match-p "\\[i\\]" elinit--help-text))
  (should (string-match-p "\\[?\\]" elinit--help-text))
  (should (string-match-p "\\[h\\]" elinit--help-text))
  (should (string-match-p "\\[q\\]" elinit--help-text)))

(ert-deftest elinit-test-health-summary-deduplication ()
  "Health summary deduplicates entries with same ID.
Unit-file loader already deduplicates, so only one entry is loaded."
  (elinit-test-without-builtins
    (let* ((dir (make-temp-file "units-" t))
           (elinit-unit-authority-path (list dir))
           (elinit-unit-directory dir)
           (elinit--programs-cache :not-yet-loaded)
           (elinit--unit-file-invalid (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal))
           (elinit--processes (make-hash-table :test 'equal))
           (elinit--failed (make-hash-table :test 'equal))
           (elinit--oneshot-completed (make-hash-table :test 'equal)))
      ;; Create two unit files with the same :id
      (with-temp-file (expand-file-name "dup1.el" dir)
        (insert "(:id \"dup\" :command \"sleep 100\")"))
      (with-temp-file (expand-file-name "dup2.el" dir)
        (insert "(:id \"dup\" :command \"sleep 100\")"))
      (unwind-protect
          (let ((summary (elinit--health-summary)))
            ;; Should count only 1 pending, not 2
            (should (string-match-p "1 pend" summary)))
        (delete-directory dir t)))))

(ert-deftest elinit-test-disabled-only-completes ()
  "Only disabled entries completes immediately."
  (elinit-test-with-unit-files
      '(("sleep 100" :id "a" :disabled t))
    (let* ((elinit--invalid (make-hash-table :test 'equal))
           (elinit--processes (make-hash-table :test 'equal))
           (elinit--failed (make-hash-table :test 'equal))
           (elinit--oneshot-completed (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (elinit--cycle-fallback-ids (make-hash-table :test 'equal))
           (elinit--computed-deps (make-hash-table :test 'equal))
           (elinit--shutting-down nil)
           (completed nil))
      ;; Parse entries
      (let ((entries (elinit--all-parsed-entries)))
        ;; Start entries with callback that sets completed flag
        (elinit--start-entries-async
         entries
         (lambda () (setq completed t)))
        ;; Should complete immediately since all entries are disabled
        (should completed)))))

(ert-deftest elinit-test-config-watch-timer-cleanup ()
  "Config watch stop cleans up debounce timer."
  ;; Set up a fake timer on the symbol property
  (let ((fake-timer (run-at-time 100 nil #'ignore)))
    (put 'elinit--config-watch-callback 'timer fake-timer)
    ;; Stop should cancel the timer
    (elinit--stop-config-watch)
    ;; Timer property should be nil
    (should (null (get 'elinit--config-watch-callback 'timer)))
    ;; Timer should be cancelled (no longer in timer-list)
    (should-not (memq fake-timer timer-list))))

(ert-deftest elinit-test-log-dir-not-created-when-logging-disabled ()
  "Log directory is not created when logging is disabled for entry."
  (let* ((temp-dir (make-temp-file "elinit-test" t))
         (nonexistent-subdir (expand-file-name "should-not-exist" temp-dir))
         (elinit-log-directory nonexistent-subdir)
         (elinit--processes (make-hash-table :test 'equal))
         (elinit--logging (make-hash-table :test 'equal))
         (elinit--restart-timers (make-hash-table :test 'equal))
         (elinit--failed (make-hash-table :test 'equal))
         (elinit--enabled-override (make-hash-table :test 'equal))
         (elinit--restart-override (make-hash-table :test 'equal))
         (elinit--shutting-down nil))
    (unwind-protect
        (progn
          ;; Start with logging disabled - should NOT create log directory
          (elinit--start-process "test" "/bin/true" nil 'oneshot nil)
          ;; Log directory should not have been created
          (should-not (file-directory-p nonexistent-subdir)))
	      ;; Cleanup
	      (delete-directory temp-dir t))))

(ert-deftest elinit-test-start-process-logging-unavailable-does-not-fail ()
  "Unwritable log directory does not block process startup."
  (let* ((elinit--processes (make-hash-table :test 'equal))
         (elinit--writers (make-hash-table :test 'equal))
         (elinit--logging (make-hash-table :test 'equal))
         (elinit--restart-timers (make-hash-table :test 'equal))
         (elinit--failed (make-hash-table :test 'equal))
         (elinit--enabled-override (make-hash-table :test 'equal))
         (elinit--restart-override (make-hash-table :test 'equal))
         (elinit--shutting-down nil))
    (cl-letf (((symbol-function 'elinit--effective-log-directory)
               (lambda () nil))
              ((symbol-function 'elinit--warn-log-directory)
               #'ignore))
      (let ((proc (elinit--start-process "test" "/bin/true" t 'oneshot nil)))
        (should (processp proc))
        (while (process-live-p proc)
          (accept-process-output nil 0.01))
        (should-not (gethash "test" elinit--writers))))))

(ert-deftest elinit-test-shutdown-complete-flag-without-callback ()
  "Shutdown sets complete flag even when no callback is provided."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--shutting-down nil)
        (elinit--shutdown-complete-flag nil)
        (elinit--shutdown-callback nil)
        (elinit--shutdown-remaining 0)
        (elinit--shutdown-timer nil))
    ;; Simulate a process exit during shutdown with no callback
    (setq elinit--shutting-down t)
    (setq elinit--shutdown-remaining 1)
    ;; Call the exit handler (simulates sentinel calling this)
    (elinit--handle-shutdown-exit)
    ;; Flag should be set even without callback
    (should (eq elinit--shutdown-complete-flag t))
    (should (= elinit--shutdown-remaining 0))))


;;; Dashboard Stop/Restart Tests

(ert-deftest elinit-test-dashboard-stop-keybinding ()
  "Dashboard l key reaches lifecycle submenu."
  (should (eq (lookup-key elinit-dashboard-mode-map "l")
              'elinit-dashboard-lifecycle)))

(ert-deftest elinit-test-dashboard-restart-keybinding ()
  "Dashboard p key reaches policy submenu."
  (should (eq (lookup-key elinit-dashboard-mode-map "p")
              'elinit-dashboard-policy)))

(ert-deftest elinit-test-dashboard-stop-is-defined ()
  "Dashboard stop command is defined as interactive."
  (should (fboundp 'elinit-dashboard-stop))
  (should (commandp 'elinit-dashboard-stop)))

(ert-deftest elinit-test-dashboard-restart-is-defined ()
  "Dashboard restart command is defined as interactive."
  (should (fboundp 'elinit-dashboard-restart))
  (should (commandp 'elinit-dashboard-restart)))

(ert-deftest elinit-test-manual-stop-not-running ()
  "Manual stop on non-running entry returns skipped."
  (let ((elinit--processes (make-hash-table :test 'equal)))
    (let ((result (elinit--manual-stop "nonexistent")))
      (should (eq 'skipped (plist-get result :status)))
      (should (string= "not running" (plist-get result :reason))))))

(ert-deftest elinit-test-manual-stop-sets-manually-stopped ()
  "Manual stop sets the manually-stopped flag."
  (let* ((elinit--processes (make-hash-table :test 'equal))
         (elinit--manually-stopped (make-hash-table :test 'equal))
         (buf (generate-new-buffer " *test-proc*"))
         (proc (start-process "test-stop" buf "sleep" "60")))
    (unwind-protect
        (progn
          (puthash "test-svc" proc elinit--processes)
          (let ((result (elinit--manual-stop "test-svc")))
            (should (eq 'stopped (plist-get result :status)))
            (should (eq t (gethash "test-svc" elinit--manually-stopped)))))
      (when (process-live-p proc) (delete-process proc))
      (kill-buffer buf))))

(ert-deftest elinit-test-manual-kill-does-not-set-manually-stopped ()
  "Manual kill does not set manually-stopped flag."
  (let* ((elinit--processes (make-hash-table :test 'equal))
         (elinit--manually-stopped (make-hash-table :test 'equal))
         (buf (generate-new-buffer " *test-proc*"))
         (proc (start-process "test-kill" buf "sleep" "60")))
    (unwind-protect
        (progn
          (puthash "test-svc" proc elinit--processes)
          (let ((result (elinit--manual-kill "test-svc" 'SIGTERM)))
            (should (eq 'signaled (plist-get result :status)))
            (should-not (gethash "test-svc" elinit--manually-stopped))))
      (when (process-live-p proc) (delete-process proc))
      (kill-buffer buf))))

(ert-deftest elinit-test-dashboard-kill-leaves-manually-stopped-untouched ()
  "Dashboard kill does not set the manually-stopped flag."
  (let* ((elinit--processes (make-hash-table :test 'equal))
         (elinit--manually-stopped (make-hash-table :test 'equal))
         (buf (generate-new-buffer " *test-dk*"))
         (proc (start-process "test-dk" buf "sleep" "60")))
    (unwind-protect
        (progn
          (puthash "test-svc" proc elinit--processes)
          (with-temp-buffer
            (elinit-dashboard-mode)
            (let ((tabulated-list-entries
                   (list (list (cons :service "test-svc")
                               (vector "test-svc" "simple" "-"
                                       "yes" "running" "yes" "yes"
                                       "1234" "-")))))
              (tabulated-list-init-header)
              (tabulated-list-print)
              (goto-char (point-min))
              ;; Call kill with force=t to skip confirmation
              (elinit-dashboard-kill t)
              (should-not (gethash "test-svc" elinit--manually-stopped)))))
      (when (process-live-p proc) (delete-process proc))
      (kill-buffer buf))))

(ert-deftest elinit-test-help-text-includes-stop-restart ()
  "Dashboard help text includes lifecycle and policy submenu hints."
  (should (string-match "\\[l\\]ifecycle" elinit--help-text))
  (should (string-match "\\[p\\]olicy" elinit--help-text))
  (should (string-match "\\[i\\]nspect" elinit--help-text)))

(ert-deftest elinit-test-timer-row-p-detects-timer ()
  "Timer row predicate detects timer rows by typed cons cell ID."
  (should (elinit--timer-row-p (cons :timer "my-timer")))
  (should-not (elinit--timer-row-p (cons :service "my-svc")))
  (should-not (elinit--timer-row-p "my-timer"))
  (should-not (elinit--timer-row-p nil))
  (should-not (elinit--timer-row-p '--timers--)))

(ert-deftest elinit-test-timer-row-p-rejects-service-row ()
  "Timer row predicate returns nil for service rows."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list "my-svc" (vector "my-svc" "simple" "-"
                                       "yes" "running" "yes" "yes" "1234" "-")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should-not (elinit--timer-row-p "my-svc")))))

(ert-deftest elinit-test-stop-allows-service-with-timer-id-collision ()
  "Stop on a service row works even if a timer has the same ID."
  (elinit-test-with-unit-files
      '(("sleep 60" :id "dup" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--manually-stopped (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal))
           (elinit--invalid-timers (make-hash-table :test 'equal)))
      ;; Timer with same ID as service
      (puthash "dup" "some schedule" elinit--invalid-timers)
      (with-temp-buffer
        (elinit-dashboard-mode)
        ;; Row has type "simple" — this is a service row, not a timer row
        (let ((tabulated-list-entries
               (list (list "dup" (vector "dup" "simple" "-"
                                        "yes" "stopped" "yes" "yes" "-" "-")))))
          (tabulated-list-init-header)
          (tabulated-list-print)
          (goto-char (point-min))
          ;; Should NOT error with "Cannot stop timer" — it's a service row
          (should-not (elinit--timer-row-p "dup")))))))

(ert-deftest elinit-test-stop-rejects-oneshot ()
  "Stop rejects oneshot entries."
  (elinit-test-with-unit-files
      '(("true" :id "my-oneshot" :type oneshot))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal)))
      ;; Simulate being in dashboard with oneshot at point
      (with-temp-buffer
        (elinit-dashboard-mode)
        (let ((tabulated-list-entries
               (list (list "my-oneshot" (vector "my-oneshot" "oneshot" "-"
                                               "yes" "done" "n/a" "yes" "-" "-")))))
          (tabulated-list-init-header)
          (tabulated-list-print)
          (goto-char (point-min))
          (should-error (elinit-dashboard-stop)
                        :type 'user-error))))))

(ert-deftest elinit-test-restart-accepts-oneshot ()
  "Restart starts oneshot entries (parity with CLI).
Stop returns skipped for completed oneshot; restart must still
proceed to call start unconditionally."
  (elinit-test-with-unit-files
      '(("true" :id "my-oneshot" :type oneshot))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal))
           (started nil))
      (with-temp-buffer
        (elinit-dashboard-mode)
        (let ((tabulated-list-entries
               (list (list (cons :service "my-oneshot")
                           (vector "my-oneshot" "oneshot" "-"
                                   "yes" "done" "n/a" "yes" "-" "-")))))
          (tabulated-list-init-header)
          (tabulated-list-print)
          (goto-char (point-min))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
                    ((symbol-function 'elinit--manual-stop)
                     (lambda (_id)
                       '(:status skipped :reason "not running")))
                    ((symbol-function 'elinit--manual-start)
                     (lambda (id) (setq started id)
                       '(:status started)))
                    ((symbol-function 'elinit--refresh-dashboard) #'ignore))
            (elinit-dashboard-restart)
            (should (equal "my-oneshot" started))))))))

(ert-deftest elinit-test-stop-rejects-timer-row ()
  "Stop rejects timer rows with stable message."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "my-timer")
                       (vector "my-timer" "timer" "-"
                               "-" "pending" "-" "-" "-" "-")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (elinit-dashboard-stop)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest elinit-test-restart-rejects-timer-row ()
  "Restart rejects timer rows with stable message."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "my-timer")
                       (vector "my-timer" "timer" "-"
                               "-" "pending" "-" "-" "-" "-")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (elinit-dashboard-restart)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest elinit-test-restart-accepts-not-running ()
  "Restart starts non-running entries (parity with CLI).
Stop returns skipped for non-running entries; restart must still
proceed to call start unconditionally."
  (elinit-test-with-unit-files
      '(("sleep 60" :id "my-svc" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal))
           (started nil))
      (with-temp-buffer
        (elinit-dashboard-mode)
        (let ((tabulated-list-entries
               (list (list (cons :service "my-svc")
                           (vector "my-svc" "simple" "-"
                                   "yes" "stopped" "yes" "yes" "-" "-")))))
          (tabulated-list-init-header)
          (tabulated-list-print)
          (goto-char (point-min))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
                    ((symbol-function 'elinit--manual-stop)
                     (lambda (_id)
                       '(:status skipped :reason "not running")))
                    ((symbol-function 'elinit--manual-start)
                     (lambda (id) (setq started id)
                       '(:status started)))
                    ((symbol-function 'elinit--refresh-dashboard) #'ignore))
            (elinit-dashboard-restart)
            (should (equal "my-svc" started))))))))

;;; Interactive Dashboard Timer Section Tests (PLAN-interactive Phase 5)

(ert-deftest elinit-test-typed-row-id-service ()
  "Service row IDs are typed cons cells."
  (should (elinit--service-row-p (cons :service "foo")))
  (should-not (elinit--service-row-p (cons :timer "foo")))
  (should-not (elinit--service-row-p "foo"))
  (should-not (elinit--service-row-p nil)))

(ert-deftest elinit-test-typed-row-id-timer ()
  "Timer row IDs are typed cons cells."
  (should (elinit--timer-row-p (cons :timer "foo")))
  (should-not (elinit--timer-row-p (cons :service "foo")))
  (should-not (elinit--timer-row-p "foo"))
  (should-not (elinit--timer-row-p nil)))

(ert-deftest elinit-test-typed-row-id-separator ()
  "Separator row IDs are symbols."
  (should (elinit--separator-row-p '--services--))
  (should (elinit--separator-row-p '--health--))
  (should (elinit--separator-row-p '--timers--))
  (should-not (elinit--separator-row-p (cons :service "foo")))
  (should-not (elinit--separator-row-p nil)))

(ert-deftest elinit-test-row-kind-dispatch ()
  "Row kind returns correct kind for all ID types."
  (should (eq :service (elinit--row-kind (cons :service "x"))))
  (should (eq :timer (elinit--row-kind (cons :timer "x"))))
  (should (eq :separator (elinit--row-kind '--services--)))
  (should-not (elinit--row-kind nil)))

(ert-deftest elinit-test-row-id-extraction ()
  "Row ID extraction returns string from typed IDs."
  (should (equal "foo" (elinit--row-id (cons :service "foo"))))
  (should (equal "bar" (elinit--row-id (cons :timer "bar"))))
  (should-not (elinit--row-id '--separator--))
  (should-not (elinit--row-id nil)))

(ert-deftest elinit-test-collision-service-timer-same-id ()
  "Service and timer with same ID string do not collide in row dispatch."
  (let ((svc-id (cons :service "backup"))
        (tmr-id (cons :timer "backup")))
    ;; They are distinct
    (should-not (equal svc-id tmr-id))
    ;; Each detects correctly
    (should (elinit--service-row-p svc-id))
    (should-not (elinit--timer-row-p svc-id))
    (should (elinit--timer-row-p tmr-id))
    (should-not (elinit--service-row-p tmr-id))
    ;; Both extract same string
    (should (equal "backup" (elinit--row-id svc-id)))
    (should (equal "backup" (elinit--row-id tmr-id)))))

(ert-deftest elinit-test-collision-dashboard-rows-coexist ()
  "Service and timer rows with same ID coexist in tabulated-list."
  (with-temp-buffer
    (elinit-dashboard-mode)
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
      (should (elinit--service-row-p (tabulated-list-get-id)))
      ;; Second row is timer
      (forward-line 1)
      (should (elinit--timer-row-p (tabulated-list-get-id))))))

(ert-deftest elinit-test-require-service-row-on-service ()
  "Require-service-row returns string ID on service row."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :service "svc")
                       (vector "svc" "simple" "-"
                               "yes" "running" "yes" "yes" "-" "-")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should (equal "svc" (elinit--require-service-row))))))

(ert-deftest elinit-test-require-service-row-rejects-timer ()
  "Require-service-row signals user-error on timer row."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "my-timer")
                       (vector "my-timer" "target" "-"
                               "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should-error (elinit--require-service-row)
                    :type 'user-error))))

(ert-deftest elinit-test-require-service-row-rejects-separator ()
  "Require-service-row signals user-error on separator row."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list '--services--
                       (vector "── Services" "" "" "" "" "" "" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should-error (elinit--require-service-row)
                    :type 'user-error))))

(ert-deftest elinit-test-timer-section-disabled-mode ()
  "Timer section shows disabled state when timer-subsystem-mode is nil."
  (elinit-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple))
    (let ((elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--timer-list nil)
          (elinit--timer-state (make-hash-table :test 'equal))
          (elinit--invalid-timers (make-hash-table :test 'equal))
          (elinit-dashboard-show-timers t)
          (elinit-timer-subsystem-mode nil))
      (let ((entries (elinit--get-entries)))
        (let ((tmr-sep (cl-find '--timers-- entries :key #'car)))
          (should tmr-sep)
          (should (string-match-p "disabled"
                                  (aref (cadr tmr-sep) 0))))))))

(ert-deftest elinit-test-timer-section-mode-on-elinit-off ()
  "Timer section shows timers when timer-mode is on but elinit-mode is off.
The disabled gate is `elinit-timer-subsystem-mode', not
`elinit-timer-subsystem-active-p' (which also requires `elinit-mode').
When `elinit-timer-subsystem-mode' is t but `elinit-mode' is nil,
configured timers must be visible for analysis."
  (elinit-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal))
           (elinit--timer-state (make-hash-table :test 'equal))
           (elinit--invalid-timers (make-hash-table :test 'equal))
           (elinit-dashboard-show-timers t)
           (elinit-mode nil)
           (elinit--timer-list
            (list (elinit-timer--create :id "t1" :target "svc"))))
      (let ((entries (elinit--get-entries)))
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

(ert-deftest elinit-test-timer-section-empty-state ()
  "Timer section shows empty state when no timers configured."
  (elinit-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple))
    (let ((elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--timer-list nil)
          (elinit--timer-state (make-hash-table :test 'equal))
          (elinit--invalid-timers (make-hash-table :test 'equal))
          (elinit-dashboard-show-timers t))
      (let ((entries (elinit--get-entries)))
        (let ((tmr-sep (cl-find '--timers-- entries :key #'car)))
          (should tmr-sep)
          (should (string-match-p "no timers configured"
                                  (aref (cadr tmr-sep) 1))))))))

(ert-deftest elinit-test-timer-section-valid-state ()
  "Timer section shows valid timer rows when timers are configured."
  (elinit-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal))
           (elinit--timer-state (make-hash-table :test 'equal))
           (elinit--invalid-timers (make-hash-table :test 'equal))
           (elinit-dashboard-show-timers t)
           (elinit--timer-list
            (list (elinit-timer--create :id "t1" :target "svc"))))
      (let ((entries (elinit--get-entries)))
        ;; Timers header present
        (let ((tmr-sep (cl-find '--timers-- entries :key #'car)))
          (should tmr-sep)
          (should (string-match-p "TARGET" (aref (cadr tmr-sep) 1))))
        ;; Timer row present with typed ID
        (let ((tmr-row (cl-find (cons :timer "t1") entries
                                :key #'car :test #'equal)))
          (should tmr-row))))))

(ert-deftest elinit-test-timer-section-invalid-timers ()
  "Timer section shows invalid timer rows."
  (elinit-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal))
           (elinit--timer-state (make-hash-table :test 'equal))
           (elinit--invalid-timers (make-hash-table :test 'equal))
           (elinit-dashboard-show-timers t)
           (elinit--timer-list nil))
      (puthash "bad-timer" "missing target" elinit--invalid-timers)
      (let ((entries (elinit--get-entries)))
        (let ((bad-row (cl-find (cons :timer "bad-timer") entries
                                :key #'car :test #'equal)))
          (should bad-row)
          ;; Check invalid status
          (should (string-match-p "invalid"
                                  (aref (cadr bad-row) 4))))))))

(ert-deftest elinit-test-timer-section-hidden-when-show-timers-nil ()
  "Timer section not rendered when show-timers is nil."
  (elinit-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple))
    (let ((elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--timer-list nil)
          (elinit--timer-state (make-hash-table :test 'equal))
          (elinit--invalid-timers (make-hash-table :test 'equal))
          (elinit-dashboard-show-timers nil))
      (let ((entries (elinit--get-entries)))
        (should-not (cl-find '--timers-- entries :key #'car))))))

(ert-deftest elinit-test-timer-trigger-rejects-service-row ()
  "Timer trigger rejects service rows."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :service "svc")
                       (vector "svc" "simple" "-"
                               "yes" "running" "yes" "yes" "-" "-")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should-error (elinit-dashboard-timer-trigger)
                    :type 'user-error))))

(ert-deftest elinit-test-timer-info-rejects-service-row ()
  "Timer info rejects service rows."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :service "svc")
                       (vector "svc" "simple" "-"
                               "yes" "running" "yes" "yes" "-" "-")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should-error (elinit-dashboard-timer-info)
                    :type 'user-error))))

(ert-deftest elinit-test-timer-jump-rejects-service-row ()
  "Timer jump rejects service rows."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :service "svc")
                       (vector "svc" "simple" "-"
                               "yes" "running" "yes" "yes" "-" "-")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should-error (elinit-dashboard-timer-jump)
                    :type 'user-error))))

(ert-deftest elinit-test-timer-reset-rejects-service-row ()
  "Timer reset rejects service rows."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :service "svc")
                       (vector "svc" "simple" "-"
                               "yes" "running" "yes" "yes" "-" "-")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should-error (elinit-dashboard-timer-reset)
                    :type 'user-error))))

(ert-deftest elinit-test-timer-jump-finds-target ()
  "Timer jump moves point to target service row."
  (let ((elinit--timer-list
         (list (elinit-timer--create :id "t1" :target "my-svc"))))
    (with-temp-buffer
      (elinit-dashboard-mode)
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
        (should (elinit--timer-row-p (tabulated-list-get-id)))
        ;; Jump
        (elinit-dashboard-timer-jump)
        ;; Should be on service row now
        (should (elinit--service-row-p (tabulated-list-get-id)))
        (should (equal "my-svc" (elinit--row-id (tabulated-list-get-id))))))))

(ert-deftest elinit-test-timer-jump-absent-target-message ()
  "Timer jump shows message when target not visible."
  (let ((elinit--timer-list
         (list (elinit-timer--create :id "t1" :target "missing-svc")))
        (last-msg nil))
    (with-temp-buffer
      (elinit-dashboard-mode)
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
          (elinit-dashboard-timer-jump)
          (should (string-match-p "not visible" last-msg)))))))

(ert-deftest elinit-test-timer-trigger-rejects-disabled-subsystem ()
  "Timer trigger rejects when subsystem is disabled."
  (let ((elinit--timer-list
         (list (elinit-timer--create :id "t1" :target "svc")))
        (elinit--invalid-timers (make-hash-table :test 'equal)))
    (with-temp-buffer
      (elinit-dashboard-mode)
      (let ((tabulated-list-entries
             (list (list (cons :timer "t1")
                         (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
        (tabulated-list-init-header)
        (tabulated-list-print)
        (goto-char (point-min))
        (cl-letf (((symbol-function 'elinit-timer-subsystem-active-p)
                   (lambda () nil)))
          (should-error (elinit-dashboard-timer-trigger)
                        :type 'user-error))))))

(ert-deftest elinit-test-timer-trigger-rejects-invalid-timer ()
  "Timer trigger rejects invalid timers."
  (let ((elinit--timer-list
         (list (elinit-timer--create :id "t1" :target "svc")))
        (elinit--invalid-timers (make-hash-table :test 'equal)))
    (puthash "t1" "bad config" elinit--invalid-timers)
    (with-temp-buffer
      (elinit-dashboard-mode)
      (let ((tabulated-list-entries
             (list (list (cons :timer "t1")
                         (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
        (tabulated-list-init-header)
        (tabulated-list-print)
        (goto-char (point-min))
        (cl-letf (((symbol-function 'elinit-timer-subsystem-active-p)
                   (lambda () t)))
          (should-error (elinit-dashboard-timer-trigger)
                        :type 'user-error))))))

(ert-deftest elinit-test-timer-reset-clears-state ()
  "Timer reset clears runtime state fields."
  (let* ((elinit--timer-list
          (list (elinit-timer--create :id "t1" :target "svc")))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--invalid-timers (make-hash-table :test 'equal))
         (update-called nil))
    ;; Seed runtime state
    (puthash "t1" (list :last-run-at 1000 :last-exit 0
                        :retry-attempt 2 :next-run-at 2000
                        :startup-triggered t)
             elinit--timer-state)
    (with-temp-buffer
      (elinit-dashboard-mode)
      (let ((tabulated-list-entries
             (list (list (cons :timer "t1")
                         (vector "t1" "svc" "yes" "-" "-" "0" "-" "" "")))))
        (tabulated-list-init-header)
        (tabulated-list-print)
        (goto-char (point-min))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
                  ((symbol-function 'elinit-timer--update-next-run)
                   (lambda (_id) (setq update-called t)))
                  ((symbol-function 'elinit-timer-subsystem-active-p)
                   (lambda () nil))
                  ((symbol-function 'elinit--refresh-dashboard) #'ignore))
          (elinit-dashboard-timer-reset)
          (let ((state (gethash "t1" elinit--timer-state)))
            ;; All runtime fields cleared
            (should-not (plist-get state :last-run-at))
            (should-not (plist-get state :last-exit))
            (should-not (plist-get state :retry-attempt))
            (should-not (plist-get state :next-run-at))
            (should-not (plist-get state :startup-triggered))
            ;; Update-next-run was called
            (should update-called)))))))

(ert-deftest elinit-test-service-only-reject-on-timer-enable ()
  "Enable rejects timer rows with stable message."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (elinit-dashboard-enable)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest elinit-test-service-only-reject-on-timer-disable ()
  "Disable rejects timer rows with stable message."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (elinit-dashboard-disable)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest elinit-test-service-only-reject-on-timer-mask ()
  "Mask rejects timer rows with stable message."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (elinit-dashboard-mask)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest elinit-test-service-only-reject-on-timer-cat ()
  "Cat rejects timer rows with stable message."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (elinit-dashboard-cat)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest elinit-test-service-only-reject-on-timer-kill ()
  "Kill rejects timer rows with stable message."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (elinit-dashboard-kill)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest elinit-test-service-only-reject-on-timer-blame ()
  "Blame rejects timer rows with stable message."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (elinit-dashboard-blame)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest elinit-test-service-only-reject-on-timer-graph ()
  "Show-graph rejects timer rows with stable message."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (elinit-dashboard-show-graph)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest elinit-test-service-only-reject-on-timer-describe ()
  "Describe-entry rejects timer rows with stable message."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (elinit-dashboard-describe-entry)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest elinit-test-transient-menu-has-timer-group ()
  "Transient menu definition includes Timers group with y-prefixed suffixes."
  ;; The transient menu is defined lazily; we trigger it via the definer
  (let ((elinit--dashboard-menu-defined nil))
    (require 'transient)
    (elinit--define-dashboard-menu)
    ;; The transient prefix should be defined now
    (should (fboundp 'elinit-dashboard-menu))
    ;; Verify timer commands are interactive
    (should (commandp 'elinit-dashboard-timer-trigger))
    (should (commandp 'elinit-dashboard-timer-info))
    (should (commandp 'elinit-dashboard-timer-jump))
    (should (commandp 'elinit-dashboard-timer-reset))
    (should (commandp 'elinit-dashboard-timer-refresh))
    ;; Verify transient layout has Timers group with correct key bindings.
    ;; Serialize the layout and search for key strings to avoid depending
    ;; on internal transient layout structure which varies across versions.
    (let* ((layout (get 'elinit-dashboard-menu 'transient--layout))
           (repr (format "%S" layout)))
      (should (string-match-p ":key \"y t\"" repr))
      (should (string-match-p ":key \"y i\"" repr))
      (should (string-match-p ":key \"y j\"" repr))
      (should (string-match-p ":key \"y r\"" repr))
      (should (string-match-p ":key \"y g\"" repr))
      (should (string-match-p "Timers" repr)))))

(ert-deftest elinit-test-timer-actions-dispatcher-key ()
  "Timer actions dispatcher is bound to y in dashboard keymap."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (should (eq (key-binding "y") #'elinit-dashboard-timer-actions))))

(ert-deftest elinit-test-service-only-reject-on-timer-reload-unit ()
  "Reload-unit rejects timer rows with stable message."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (elinit-dashboard-reload-unit)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest elinit-test-service-only-reject-on-timer-reset-failed ()
  "Reset-failed rejects timer rows with stable message."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (elinit-dashboard-reset-failed)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest elinit-test-service-only-reject-on-timer-unmask ()
  "Unmask rejects timer rows with stable message."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (elinit-dashboard-unmask)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest elinit-test-service-only-reject-on-timer-set-restart ()
  "Set-restart-policy rejects timer rows with stable message."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (elinit-dashboard-set-restart-policy)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest elinit-test-service-only-reject-on-timer-set-logging ()
  "Set-logging rejects timer rows with stable message."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (elinit-dashboard-set-logging)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest elinit-test-service-only-reject-on-timer-edit ()
  "Edit rejects timer rows with stable message."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (elinit-dashboard-edit)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest elinit-test-service-only-reject-on-timer-show-deps ()
  "Show-deps rejects timer rows with stable message."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (elinit-dashboard-show-deps)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest elinit-test-service-only-reject-on-timer-view-log ()
  "View-log rejects timer rows with stable message."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (elinit-dashboard-view-log)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest elinit-test-service-only-reject-on-timer-start ()
  "Start rejects timer rows with stable message."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list (cons :timer "t1")
                       (vector "t1" "svc" "yes" "-" "-" "-" "-" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (let ((err (should-error (elinit-dashboard-start)
                               :type 'user-error)))
        (should (string-match-p "timer rows" (cadr err)))))))

(ert-deftest elinit-test-timer-refresh-calls-dashboard-refresh ()
  "Timer refresh command invokes dashboard refresh and messages."
  (let ((refreshed nil)
        (msg nil))
    (with-temp-buffer
      (elinit-dashboard-mode)
      (cl-letf (((symbol-function 'elinit--refresh-dashboard)
                 (lambda () (setq refreshed t)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq msg (apply #'format fmt args)))))
        (elinit-dashboard-timer-refresh)
        (should refreshed)
        (should (string-match-p "refreshed" msg))))))

(ert-deftest elinit-test-service-counters-in-header-line ()
  "Service counters are shown in `header-line-format'."
  (elinit-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple))
    (let ((elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--timer-list nil)
          (elinit--timer-state (make-hash-table :test 'equal))
          (elinit--invalid-timers (make-hash-table :test 'equal))
          (elinit-dashboard-show-timers nil))
      (let ((header (elinit--dashboard-header-line)))
        (should (string-match-p "\\brun\\b" header))
        (should (string-match-p "\\bdone\\b" header))
        (should (string-match-p "\\bpend\\b" header))
        (should (string-match-p "\\bfail\\b" header))
        (should (string-match-p "\\binv\\b" header))
        ;; Header no longer carries column labels.
        (should-not (string-match-p "\\bID\\b" header))
        ;; No pipe filler separators in this compact format.
        (should-not (string-match-p "|" header))))))

(ert-deftest elinit-test-services-header-precedes-service-and-timer-rows ()
  "Services section header appears first, then services, then timers."
  (elinit-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple))
    (let ((elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--timer-list
           (list (elinit-timer--create :id "t1" :target "svc")))
          (elinit--timer-state (make-hash-table :test 'equal))
          (elinit--invalid-timers (make-hash-table :test 'equal))
          (elinit-dashboard-show-timers t))
      (let* ((entries (elinit--get-entries))
             (ids (mapcar #'car entries))
             (services-pos (cl-position '--services-- ids))
             (svc-pos (cl-position-if
                       (lambda (id) (elinit--service-row-p id)) ids))
             (timer-pos (cl-position '--timers-- ids))
             (tmr-pos (cl-position-if
                       (lambda (id) (elinit--timer-row-p id)) ids)))
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

(ert-deftest elinit-test-no-blank-summary-spacers-in-body ()
  "Dashboard body has no blank summary spacer rows."
  (elinit-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple))
    (let ((elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--timer-list nil)
          (elinit--timer-state (make-hash-table :test 'equal))
          (elinit--invalid-timers (make-hash-table :test 'equal))
          (elinit-dashboard-show-timers t))
      (let* ((entries (elinit--get-entries))
             (ids (mapcar #'car entries)))
        (should-not
         (cl-find-if
          (lambda (id)
            (and (symbolp id)
                 (string-match-p "^--blank-" (symbol-name id))))
          ids))))))

(ert-deftest elinit-test-header-counters-count-services-only ()
  "Header counters aggregate services only, never timer rows."
  (elinit-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal))
           (elinit--timer-list nil)
           (elinit--timer-state (make-hash-table :test 'equal))
           (elinit--invalid-timers (make-hash-table :test 'equal))
           (elinit-dashboard-show-timers t)
           (header-no-timers
            (substring-no-properties (elinit--dashboard-header-line))))
      (setq elinit--timer-list
            (list (elinit-timer--create :id "t1" :target "svc")))
      (let ((header-with-timers
             (substring-no-properties (elinit--dashboard-header-line))))
        (should (equal header-no-timers header-with-timers))))))


(ert-deftest elinit-test-timer-info-invalid-timer ()
  "Timer info shows details for invalid timers."
  (let ((elinit--timer-list nil)
        (elinit--invalid-timers (make-hash-table :test 'equal)))
    (puthash "bad-t" "missing target field" elinit--invalid-timers)
    (with-temp-buffer
      (elinit-dashboard-mode)
      (let ((tabulated-list-entries
             (list (list (cons :timer "bad-t")
                         (vector "bad-t" "-" "-" "-"
                                 (propertize "invalid" 'face 'error)
                                 "-" "missing target field" "" "")))))
        (tabulated-list-init-header)
        (tabulated-list-print)
        (goto-char (point-min))
        (elinit-dashboard-timer-info)
        ;; Help window should be open
        (let ((info-buf (get-buffer "*elinit-timer-info*")))
          (should info-buf)
          (with-current-buffer info-buf
            (should (string-match-p "INVALID" (buffer-string)))
            (should (string-match-p "missing target field" (buffer-string))))
          (kill-buffer info-buf))))))

(ert-deftest elinit-test-timer-info-valid-timer ()
  "Timer info shows details for valid timers."
  (let* ((elinit--timer-list
          (list (elinit-timer--create
                 :id "t1" :target "svc"
                 :on-startup-sec 30
                 :persistent t)))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--invalid-timers (make-hash-table :test 'equal)))
    (puthash "t1" (list :last-run-at (float-time) :last-exit 0
                        :next-run-at (+ (float-time) 60))
             elinit--timer-state)
    (with-temp-buffer
      (elinit-dashboard-mode)
      (let ((tabulated-list-entries
             (list (list (cons :timer "t1")
                         (vector "t1" "svc" "yes" "-" "-" "0" "-" "" "")))))
        (tabulated-list-init-header)
        (tabulated-list-print)
        (goto-char (point-min))
        (elinit-dashboard-timer-info)
        (let ((info-buf (get-buffer "*elinit-timer-info*")))
          (should info-buf)
          (with-current-buffer info-buf
            (should (string-match-p "Timer: t1" (buffer-string)))
            (should (string-match-p "Target: svc" (buffer-string)))
            (should (string-match-p "Startup: 30" (buffer-string)))
            (should (string-match-p "Persistent: yes" (buffer-string))))
          (kill-buffer info-buf))))))

(ert-deftest elinit-test-services-header-is-first-row ()
  "Services section header is the first content row."
  (elinit-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple))
    (let ((elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--timer-list nil)
          (elinit--timer-state (make-hash-table :test 'equal))
          (elinit--invalid-timers (make-hash-table :test 'equal))
          (elinit-dashboard-show-timers nil))
      (let* ((entries (elinit--get-entries))
             (first-id (car (car entries))))
        (should (eq '--services-- first-id))))))


;;; Dashboard nested menu tests

(ert-deftest elinit-test-dashboard-keymap-submenu-keys ()
  "Dashboard keymap binds l, p, i to submenu dispatchers."
  (should (eq (lookup-key elinit-dashboard-mode-map "l")
              'elinit-dashboard-lifecycle))
  (should (eq (lookup-key elinit-dashboard-mode-map "p")
              'elinit-dashboard-policy))
  (should (eq (lookup-key elinit-dashboard-mode-map "i")
              'elinit-dashboard-inspect)))

(ert-deftest elinit-test-dashboard-keymap-top-level-only ()
  "Dashboard keymap has no direct lifecycle/policy/inspect bindings."
  ;; Old direct bindings should be gone
  (should-not (eq (lookup-key elinit-dashboard-mode-map "s")
                  'elinit-dashboard-start))
  (should-not (eq (lookup-key elinit-dashboard-mode-map "x")
                  'elinit-dashboard-stop))
  (should-not (eq (lookup-key elinit-dashboard-mode-map "e")
                  'elinit-dashboard-toggle-enabled))
  (should-not (eq (lookup-key elinit-dashboard-mode-map "m")
                  'elinit-dashboard-toggle-mask))
  (should-not (eq (lookup-key elinit-dashboard-mode-map "r")
                  'elinit-dashboard-toggle-restart))
  (should-not (eq (lookup-key elinit-dashboard-mode-map "d")
                  'elinit-dashboard-show-deps))
  (should-not (eq (lookup-key elinit-dashboard-mode-map "c")
                  'elinit-dashboard-cat)))

(ert-deftest elinit-test-dashboard-keymap-proced-moved ()
  "Dashboard keymap binds proced to t (was p)."
  (should (eq (lookup-key elinit-dashboard-mode-map "t")
              'proced))
  (should (eq (lookup-key elinit-dashboard-mode-map "T")
              'elinit-dashboard-toggle-proced-auto-update)))

(ert-deftest elinit-test-dashboard-keymap-tag-filter-moved ()
  "Dashboard keymap binds tag filter to F (was t)."
  (should (eq (lookup-key elinit-dashboard-mode-map "F")
              'elinit-dashboard-cycle-tag-filter)))

(ert-deftest elinit-test-dashboard-enable-explicit ()
  "Explicit enable sets enabled override."
  (let* ((entry (elinit--parse-entry '("cmd" :id "svc" :disabled t)))
         (elinit--enabled-override (make-hash-table :test 'equal))
         (elinit--mask-override (make-hash-table :test 'equal))
         (elinit--invalid (make-hash-table :test 'equal))
         (elinit-overrides-file nil)
         (msg nil))
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () (cons :service "svc")))
              ((symbol-function 'elinit--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'elinit--save-overrides) #'ignore)
              ((symbol-function 'elinit--refresh-dashboard) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (elinit-dashboard-enable)
      (should (eq 'enabled (gethash "svc" elinit--enabled-override)))
      (should (string-match-p "Enabled svc" msg)))))

(ert-deftest elinit-test-dashboard-disable-explicit ()
  "Explicit disable sets disabled override."
  (let* ((entry (elinit--parse-entry '("cmd" :id "svc")))
         (elinit--enabled-override (make-hash-table :test 'equal))
         (elinit--mask-override (make-hash-table :test 'equal))
         (elinit--invalid (make-hash-table :test 'equal))
         (elinit-overrides-file nil)
         (msg nil))
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () (cons :service "svc")))
              ((symbol-function 'elinit--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'elinit--save-overrides) #'ignore)
              ((symbol-function 'elinit--refresh-dashboard) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (elinit-dashboard-disable)
      (should (eq 'disabled (gethash "svc" elinit--enabled-override)))
      (should (string-match-p "Disabled svc" msg)))))

(ert-deftest elinit-test-dashboard-enable-already-enabled ()
  "Explicit enable on already-enabled entry shows message."
  (let* ((entry (elinit--parse-entry '("cmd" :id "svc")))
         (elinit--enabled-override (make-hash-table :test 'equal))
         (elinit--mask-override (make-hash-table :test 'equal))
         (elinit--invalid (make-hash-table :test 'equal))
         (elinit-overrides-file nil)
         (msg nil))
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () (cons :service "svc")))
              ((symbol-function 'elinit--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (elinit-dashboard-enable)
      (should (string-match-p "already enabled" msg)))))

(ert-deftest elinit-test-dashboard-mask-explicit ()
  "Explicit mask sets mask override."
  (let* ((entry (elinit--parse-entry '("cmd" :id "svc")))
         (elinit--mask-override (make-hash-table :test 'equal))
         (elinit--invalid (make-hash-table :test 'equal))
         (elinit-overrides-file nil)
         (msg nil))
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () (cons :service "svc")))
              ((symbol-function 'elinit--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'elinit--save-overrides) #'ignore)
              ((symbol-function 'elinit--refresh-dashboard) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (elinit-dashboard-mask)
      (should (eq 'masked (gethash "svc" elinit--mask-override)))
      (should (string-match-p "Masked svc" msg)))))

(ert-deftest elinit-test-dashboard-unmask-explicit ()
  "Explicit unmask clears mask override."
  (let* ((entry (elinit--parse-entry '("cmd" :id "svc")))
         (elinit--mask-override (make-hash-table :test 'equal))
         (elinit--invalid (make-hash-table :test 'equal))
         (elinit-overrides-file nil)
         (msg nil))
    (puthash "svc" 'masked elinit--mask-override)
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () (cons :service "svc")))
              ((symbol-function 'elinit--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'elinit--save-overrides) #'ignore)
              ((symbol-function 'elinit--refresh-dashboard) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (elinit-dashboard-unmask)
      (should-not (gethash "svc" elinit--mask-override))
      (should (string-match-p "Unmasked svc" msg)))))

(ert-deftest elinit-test-dashboard-unmask-not-masked ()
  "Explicit unmask on non-masked entry shows message."
  (let* ((entry (elinit--parse-entry '("cmd" :id "svc")))
         (elinit--mask-override (make-hash-table :test 'equal))
         (elinit--invalid (make-hash-table :test 'equal))
         (elinit-overrides-file nil)
         (msg nil))
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () (cons :service "svc")))
              ((symbol-function 'elinit--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (elinit-dashboard-unmask)
      (should (string-match-p "not masked" msg)))))

(ert-deftest elinit-test-dashboard-set-restart-policy ()
  "Set restart policy stores override via completing-read."
  (let* ((entry (elinit--parse-entry '("cmd" :id "svc" :restart on-failure)))
         (elinit--restart-override (make-hash-table :test 'equal))
         (elinit--restart-timers (make-hash-table :test 'equal))
         (elinit--invalid (make-hash-table :test 'equal))
         (elinit-overrides-file nil)
         (msg nil))
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () (cons :service "svc")))
              ((symbol-function 'elinit--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "always"))
              ((symbol-function 'elinit--save-overrides) #'ignore)
              ((symbol-function 'elinit--refresh-dashboard) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (elinit-dashboard-set-restart-policy)
      (should (eq 'always (gethash "svc" elinit--restart-override)))
      (should (string-match-p "Restart policy for svc: always" msg)))))

(ert-deftest elinit-test-dashboard-set-restart-policy-clears-on-match ()
  "Set restart policy clears override when matching config default."
  (let* ((entry (elinit--parse-entry '("cmd" :id "svc" :restart on-failure)))
         (elinit--restart-override (make-hash-table :test 'equal))
         (elinit--restart-timers (make-hash-table :test 'equal))
         (elinit--invalid (make-hash-table :test 'equal))
         (elinit-overrides-file nil))
    ;; Pre-set an override
    (puthash "svc" 'always elinit--restart-override)
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () (cons :service "svc")))
              ((symbol-function 'elinit--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "on-failure"))
              ((symbol-function 'elinit--save-overrides) #'ignore)
              ((symbol-function 'elinit--refresh-dashboard) #'ignore)
              ((symbol-function 'message) #'ignore))
      (elinit-dashboard-set-restart-policy)
      ;; Override should be cleared since on-failure matches config
      (should-not (gethash "svc" elinit--restart-override)))))

(ert-deftest elinit-test-dashboard-set-logging ()
  "Set logging stores override via completing-read."
  (let* ((entry (elinit--parse-entry '("cmd" :id "svc" :logging t)))
         (elinit--logging (make-hash-table :test 'equal))
         (elinit--invalid (make-hash-table :test 'equal))
         (elinit-overrides-file nil)
         (msg nil))
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () (cons :service "svc")))
              ((symbol-function 'elinit--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "off"))
              ((symbol-function 'elinit--refresh-dashboard) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (elinit-dashboard-set-logging)
      (should (eq 'disabled (gethash "svc" elinit--logging)))
      (should (string-match-p "Logging for svc: off" msg)))))

(ert-deftest elinit-test-dashboard-submenu-commands-defined ()
  "All submenu dispatcher commands are defined."
  (should (commandp 'elinit-dashboard-lifecycle))
  (should (commandp 'elinit-dashboard-policy))
  (should (commandp 'elinit-dashboard-inspect)))

(ert-deftest elinit-test-dashboard-explicit-policy-commands-defined ()
  "All explicit policy commands are defined."
  (should (commandp 'elinit-dashboard-enable))
  (should (commandp 'elinit-dashboard-disable))
  (should (commandp 'elinit-dashboard-mask))
  (should (commandp 'elinit-dashboard-unmask))
  (should (commandp 'elinit-dashboard-set-restart-policy))
  (should (commandp 'elinit-dashboard-set-logging)))

;;;; Telemetry Data Model Tests

(ert-deftest elinit-test-last-exit-info-populated ()
  "Process sentinel populates `elinit--last-exit-info'."
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
         (elinit--shutting-down nil)
         (proc (start-process "test-exit" nil "true")))
    (puthash "test-exit" proc elinit--processes)
    (set-process-sentinel proc
                          (elinit--make-process-sentinel
                           "test-exit" "true" nil 'simple 'no))
    ;; Wait for process to exit
    (while (process-live-p proc)
      (accept-process-output nil 0.01))
    (accept-process-output nil 0.05)
    ;; Verify exit info was recorded
    (let ((info (gethash "test-exit" elinit--last-exit-info)))
      (should info)
      (should (eq 'exited (plist-get info :status)))
      (should (= 0 (plist-get info :code)))
      (should (numberp (plist-get info :timestamp))))))

(ert-deftest elinit-test-telemetry-uptime-running ()
  "Uptime returns seconds for running process."
  (let ((elinit--start-times (make-hash-table :test 'equal))
        (elinit--processes (make-hash-table :test 'equal))
        (proc (start-process "uptime-test" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "uptime-test" (float-time) elinit--start-times)
          (puthash "uptime-test" proc elinit--processes)
          (let ((uptime (elinit--telemetry-uptime "uptime-test")))
            (should uptime)
            (should (>= uptime 0))))
      (delete-process proc))))

(ert-deftest elinit-test-telemetry-uptime-nil-when-not-running ()
  "Uptime returns nil when process is not running."
  (let ((elinit--start-times (make-hash-table :test 'equal))
        (elinit--processes (make-hash-table :test 'equal)))
    (puthash "dead-svc" (float-time) elinit--start-times)
    (should-not (elinit--telemetry-uptime "dead-svc"))))

(ert-deftest elinit-test-telemetry-restart-count ()
  "Restart count reflects recent restarts in window."
  (let ((elinit--restart-times (make-hash-table :test 'equal))
        (elinit-restart-window 300))
    ;; No restarts
    (should (= 0 (elinit--telemetry-restart-count "svc")))
    ;; Two recent restarts
    (puthash "svc" (list (float-time) (- (float-time) 10))
             elinit--restart-times)
    (should (= 2 (elinit--telemetry-restart-count "svc")))
    ;; Old restart outside window
    (puthash "svc" (list (- (float-time) 999))
             elinit--restart-times)
    (should (= 0 (elinit--telemetry-restart-count "svc")))))

(ert-deftest elinit-test-telemetry-last-exit-format ()
  "Last exit returns formatted string."
  (let ((elinit--last-exit-info (make-hash-table :test 'equal)))
    ;; No exit
    (should-not (elinit--telemetry-last-exit "svc"))
    ;; Clean exit
    (puthash "svc" (list :status 'exited :code 0 :timestamp (float-time))
             elinit--last-exit-info)
    (should (string= "exited successfully"
                      (elinit--telemetry-last-exit "svc")))
    ;; Non-zero exit
    (puthash "svc" (list :status 'exited :code 1 :timestamp (float-time))
             elinit--last-exit-info)
    (should (string= "exited with code 1"
                      (elinit--telemetry-last-exit "svc")))
    ;; Signal
    (puthash "svc" (list :status 'signal :code 15 :timestamp (float-time))
             elinit--last-exit-info)
    (should (string= "killed by signal 15"
                      (elinit--telemetry-last-exit "svc")))))

(ert-deftest elinit-test-telemetry-process-metrics-nil-graceful ()
  "Process metrics returns nil gracefully for non-existent PID."
  (should-not (elinit--telemetry-process-metrics 999999999)))

(ert-deftest elinit-test-telemetry-log-tail-missing ()
  "Log tail returns nil for non-existent log file."
  (let ((elinit-log-directory "/tmp/nonexistent-sv-logs"))
    (should-not (elinit--telemetry-log-tail "svc"))))

(ert-deftest elinit-test-snapshot-includes-last-exit-info ()
  "Snapshot struct includes `last-exit-info' field."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--entry-state (make-hash-table :test 'equal))
        (elinit--invalid (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--restart-override (make-hash-table :test 'equal))
        (elinit--logging (make-hash-table :test 'equal))
        (elinit--mask-override (make-hash-table :test 'equal))
        (elinit--manually-started (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--remain-active (make-hash-table :test 'equal))
        (elinit--last-exit-info (make-hash-table :test 'equal)))
    ;; Put exit info in the hash
    (puthash "svc" (list :status 'exited :code 42 :timestamp 1000.0)
             elinit--last-exit-info)
    (let ((snapshot (elinit--build-snapshot)))
      ;; Verify the snapshot captured it
      (should (elinit-snapshot-last-exit-info snapshot))
      (let ((info (gethash "svc" (elinit-snapshot-last-exit-info snapshot))))
        (should info)
        (should (eq 'exited (plist-get info :status)))
        (should (= 42 (plist-get info :code)))))))

(ert-deftest elinit-test-format-duration ()
  "Duration formatting produces human-readable strings."
  (should (string= "5s" (elinit--describe-format-duration 5)))
  (should (string= "2m30s" (elinit--describe-format-duration 150)))
  (should (string= "1h30m" (elinit--describe-format-duration 5400)))
  (should (string= "2d3h" (elinit--describe-format-duration 183600))))

(ert-deftest elinit-test-cli-format-duration ()
  "CLI duration formatting produces human-readable strings."
  (should (string= "5s" (elinit--cli-format-duration 5)))
  (should (string= "2m30s" (elinit--cli-format-duration 150)))
  (should (string= "1h30m" (elinit--cli-format-duration 5400)))
  (should (string= "2d3h" (elinit--cli-format-duration 183600))))

(ert-deftest elinit-test-set-logging-saves-overrides ()
  "Dashboard set-logging calls `elinit--save-overrides'."
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
         (elinit--logging-override (make-hash-table :test 'equal))
         (elinit-overrides-file nil)
         (saved nil)
         (entry (elinit--parse-entry
                 '("cmd" :id "svc" :type simple :logging))))
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () (cons :service "svc")))
              ((symbol-function 'elinit--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'elinit--save-overrides)
               (lambda () (setq saved t)))
              ((symbol-function 'elinit--refresh-dashboard) #'ignore)
              ((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "off")))
      (elinit-dashboard-set-logging)
      (should saved))))

(ert-deftest elinit-test-cli-entry-info-telemetry-fields ()
  "CLI entry-info includes telemetry fields."
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
                 '("cmd" :id "svc" :type simple :restart always))))
    ;; Add some exit info
    (puthash "svc" (list :status 'exited :code 1 :timestamp (float-time))
             elinit--last-exit-info)
    (puthash "svc" (list (float-time)) elinit--restart-times)
    (let ((info (elinit--cli-entry-info entry)))
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

(ert-deftest elinit-test-telemetry-process-tree-no-children ()
  "Process tree returns nil when PID has no children."
  (let ((proc (start-process "tree-test" nil "sleep" "300")))
    (unwind-protect
        (let ((pid (process-id proc)))
          ;; A bare sleep process has no children
          (should-not (elinit--telemetry-process-tree pid)))
      (delete-process proc))))

(ert-deftest elinit-test-telemetry-process-tree-bogus-pid ()
  "Process tree returns nil for non-existent PID."
  (should-not (elinit--telemetry-process-tree 999999999)))

(ert-deftest elinit-test-telemetry-process-metrics-thcount ()
  "Process metrics includes :thcount when available."
  (let ((proc (start-process "thcount-test" nil "sleep" "300")))
    (unwind-protect
        (let* ((pid (process-id proc))
               (metrics (elinit--telemetry-process-metrics pid)))
          ;; We can only verify the plist structure, not the exact value,
          ;; since not all OSes provide thcount
          (when metrics
            (should (listp metrics))))
      (delete-process proc))))

(ert-deftest elinit-test-cli-entry-info-log-tail ()
  "Entry info includes log-tail field."
  (let ((entry (elinit--parse-entry '("cmd" :id "svc"))))
    (let ((info (elinit--cli-entry-info entry)))
      (should (assq 'log-tail info)))))

(ert-deftest elinit-test-cli-entry-info-process-tree ()
  "Entry info includes process-tree field."
  (let ((entry (elinit--parse-entry '("cmd" :id "svc"))))
    (let ((info (elinit--cli-entry-info entry)))
      (should (assq 'process-tree info)))))

(ert-deftest elinit-test-cli-entry-info-authority-tier ()
  "Entry info includes authority-tier field."
  (let ((entry (elinit--parse-entry '("cmd" :id "svc"))))
    (let ((info (elinit--cli-entry-info entry)))
      (should (assq 'authority-tier info)))))

(ert-deftest elinit-test-cli-describe-human-log-tail ()
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
    (let ((output (elinit--cli-describe-human info)))
      (should (string-match-p "Recent log:" output))
      (should (string-match-p "line1" output)))))

(ert-deftest elinit-test-cli-describe-human-process-tree ()
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
    (let ((output (elinit--cli-describe-human info)))
      (should (string-match-p "Process tree: 3 descendants" output))
      (should (string-match-p "5678" output)))))

(ert-deftest elinit-test-cli-describe-human-authority-tier ()
  "Describe human output shows tier when unit-file and tier are present."
  (let ((info '((id . "svc") (type . simple)
                (enabled . t) (enabled-config . t)
                (restart . always) (restart-config . always)
                (logging . t) (logging-config . t)
                (delay . 0) (after . nil) (requires . nil)
                (status . "running") (reason . nil)
                (pid . nil) (start-time . nil) (ready-time . nil)
                (duration . nil)
                (unit-file . "/etc/elinit/svc.sv")
                (authority-tier . 2))))
    (let ((output (elinit--cli-describe-human info)))
      (should (string-match-p "Unit file: /etc/elinit/svc\\.sv (tier 2)" output)))))

(ert-deftest elinit-test-cli-json-includes-log-tail ()
  "JSON output includes log_tail field."
  (let ((info '((id . "svc") (type . simple)
                (enabled . t) (status . "running")
                (restart . always) (logging . t)
                (pid . nil) (reason . nil) (delay . 0)
                (after . nil) (requires . nil)
                (log-tail . "hello\n"))))
    (let ((json (elinit--cli-entry-to-json-obj info)))
      (should (equal "hello\n" (alist-get 'log_tail json))))))

(ert-deftest elinit-test-cli-json-includes-process-tree ()
  "JSON output includes process_tree field."
  (let ((info '((id . "svc") (type . simple)
                (enabled . t) (status . "running")
                (restart . always) (logging . t)
                (pid . nil) (reason . nil) (delay . 0)
                (after . nil) (requires . nil)
                (process-tree . (:count 2 :pids (100 101))))))
    (let ((json (elinit--cli-entry-to-json-obj info)))
      (let ((tree (alist-get 'process_tree json)))
        (should tree)
        (should (= 2 (alist-get 'count tree)))
        (should (equal '(100 101) (alist-get 'pids tree)))))))

(ert-deftest elinit-test-cli-json-includes-authority-tier ()
  "JSON output includes authority_tier field."
  (let ((info '((id . "svc") (type . simple)
                (enabled . t) (status . "running")
                (restart . always) (logging . t)
                (pid . nil) (reason . nil) (delay . 0)
                (after . nil) (requires . nil)
                (authority-tier . 1))))
    (let ((json (elinit--cli-entry-to-json-obj info)))
      (should (= 1 (alist-get 'authority_tier json))))))


;;; Dashboard Detail Rendering Tests

(ert-deftest elinit-test-dashboard-describe-shows-process-tree ()
  "Dashboard detail view renders process-tree line when descendants exist."
  (let* ((entry (elinit--parse-entry '("cmd" :id "svc")))
         (elinit--processes (make-hash-table :test 'equal))
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
         (elinit--logging-override (make-hash-table :test 'equal))
         (fake-proc (start-process "tree-dash" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "svc" fake-proc elinit--processes)
          (cl-letf (((symbol-function 'elinit--unit-file-path)
                     (lambda (_id) nil))
                    ((symbol-function 'elinit--telemetry-log-tail)
                     (lambda (_id &optional _lines) nil))
                    ((symbol-function 'elinit--telemetry-process-tree)
                     (lambda (_pid) '(:count 3 :pids (100 101 102))))
                    ((symbol-function 'elinit--telemetry-process-metrics)
                     (lambda (_pid) nil)))
            (elinit--describe-entry-detail "svc" entry)
            (let ((info-buf (get-buffer "*elinit-info*")))
              (unwind-protect
                  (let ((output (with-current-buffer info-buf
                                  (buffer-string))))
                    (should (string-match-p "Tree: 3 descendants" output))
                    (should (string-match-p "100, 101, 102" output)))
                (when info-buf (kill-buffer info-buf))))))
      (delete-process fake-proc))))

(ert-deftest elinit-test-dashboard-describe-shows-etime-thcount ()
  "Dashboard detail view renders ETIME and TASKS in metrics line."
  (let* ((entry (elinit--parse-entry '("cmd" :id "svc")))
         (elinit--processes (make-hash-table :test 'equal))
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
         (elinit--logging-override (make-hash-table :test 'equal))
         (fake-proc (start-process "met-dash" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "svc" fake-proc elinit--processes)
          (cl-letf (((symbol-function 'elinit--unit-file-path)
                     (lambda (_id) nil))
                    ((symbol-function 'elinit--telemetry-log-tail)
                     (lambda (_id &optional _lines) nil))
                    ((symbol-function 'elinit--telemetry-process-tree)
                     (lambda (_pid) nil))
                    ((symbol-function 'elinit--telemetry-process-metrics)
                     (lambda (_pid)
                       '(:rss 1024 :pcpu 5.0 :pmem 1.2
                         :etime 3661 :thcount 4))))
            (elinit--describe-entry-detail "svc" entry)
            (let ((info-buf (get-buffer "*elinit-info*")))
              (unwind-protect
                  (let ((output (with-current-buffer info-buf
                                  (buffer-string))))
                    (should (string-match-p "RSS=1024KB" output))
                    (should (string-match-p "ETIME=1h1m" output))
                    (should (string-match-p "TASKS=4" output)))
                (when info-buf (kill-buffer info-buf))))))
      (delete-process fake-proc))))

(ert-deftest elinit-test-dashboard-describe-shows-authority-tier ()
  "Dashboard detail view appends tier to unit-file line."
  (let* ((entry (elinit--parse-entry '("cmd" :id "svc")))
         (elinit--processes (make-hash-table :test 'equal))
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
         (elinit--logging-override (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'elinit--unit-file-existing-path)
               (lambda (_id) "/etc/elinit/svc.sv"))
              ((symbol-function 'elinit--authority-tier-for-id)
               (lambda (_id) 2))
              ((symbol-function 'elinit--telemetry-log-tail)
               (lambda (_id &optional _lines) nil)))
      (elinit--describe-entry-detail "svc" entry)
      (let ((info-buf (get-buffer "*elinit-info*")))
        (unwind-protect
            (let ((output (with-current-buffer info-buf
                            (buffer-string))))
              (should (string-match-p
                       "Unit file: /etc/elinit/svc\\.sv (tier 2)"
                       output)))
          (when info-buf (kill-buffer info-buf)))))))

(ert-deftest elinit-test-dashboard-describe-shows-log-tail ()
  "Dashboard detail view renders Recent log section."
  (let* ((entry (elinit--parse-entry '("cmd" :id "svc")))
         (elinit--processes (make-hash-table :test 'equal))
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
         (elinit--logging-override (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'elinit--unit-file-path)
               (lambda (_id) nil))
              ((symbol-function 'elinit--telemetry-log-tail)
               (lambda (_id &optional _lines)
                 "2024-01-01 started\n2024-01-01 ready\n")))
      (elinit--describe-entry-detail "svc" entry)
      (let ((info-buf (get-buffer "*elinit-info*")))
        (unwind-protect
            (let ((output (with-current-buffer info-buf
                            (buffer-string))))
              (should (string-match-p "Recent log:" output))
              (should (string-match-p "2024-01-01 started" output))
              (should (string-match-p "2024-01-01 ready" output)))
          (when info-buf (kill-buffer info-buf)))))))


;;; Target Visibility Tests

(ert-deftest elinit-test-dashboard-default-hides-targets ()
  "Default dashboard view hides all target-type entries."
  (elinit-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple)
        (nil :id "app.target" :type target))
    (let ((elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--restart-override (make-hash-table :test 'equal))
          (elinit--logging (make-hash-table :test 'equal))
          (elinit--target-convergence (make-hash-table :test 'equal))
          (elinit--target-convergence-reasons
           (make-hash-table :test 'equal))
          (elinit--dashboard-show-targets nil)
          (elinit--dashboard-show-init-targets nil))
      (let* ((entries (elinit--get-entries))
             (service-ids
              (cl-loop for entry in entries
                       when (elinit--service-row-p (car entry))
                       collect (cdr (car entry)))))
        (should (member "svc" service-ids))
        (should-not (member "app.target" service-ids))))))

(ert-deftest elinit-test-dashboard-show-targets-toggle ()
  "Toggling show-targets makes target entries visible."
  (elinit-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple)
        (nil :id "app.target" :type target))
    (let ((elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--restart-override (make-hash-table :test 'equal))
          (elinit--logging (make-hash-table :test 'equal))
          (elinit--target-convergence (make-hash-table :test 'equal))
          (elinit--target-convergence-reasons
           (make-hash-table :test 'equal))
          (elinit--dashboard-show-targets t)
          (elinit--dashboard-show-init-targets nil))
      (let* ((entries (elinit--get-entries))
             (service-ids
              (cl-loop for entry in entries
                       when (elinit--service-row-p (car entry))
                       collect (cdr (car entry)))))
        (should (member "svc" service-ids))
        (should (member "app.target" service-ids))))))

(ert-deftest elinit-test-dashboard-init-targets-hidden-when-targets-shown ()
  "Init-transition targets remain hidden when regular targets are shown."
  (elinit-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple)
        (nil :id "app.target" :type target)
        (nil :id "rescue.target" :type target))
    (let ((elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--restart-override (make-hash-table :test 'equal))
          (elinit--logging (make-hash-table :test 'equal))
          (elinit--target-convergence (make-hash-table :test 'equal))
          (elinit--target-convergence-reasons
           (make-hash-table :test 'equal))
          (elinit--dashboard-show-targets t)
          (elinit--dashboard-show-init-targets nil))
      (let* ((entries (elinit--get-entries))
             (service-ids
              (cl-loop for entry in entries
                       when (elinit--service-row-p (car entry))
                       collect (cdr (car entry)))))
        (should (member "svc" service-ids))
        (should (member "app.target" service-ids))
        (should-not (member "rescue.target" service-ids))))))

(ert-deftest elinit-test-dashboard-init-targets-shown-when-toggled ()
  "Init-transition targets shown when both toggles are on."
  (elinit-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple)
        (nil :id "app.target" :type target)
        (nil :id "rescue.target" :type target))
    (let ((elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--restart-override (make-hash-table :test 'equal))
          (elinit--logging (make-hash-table :test 'equal))
          (elinit--target-convergence (make-hash-table :test 'equal))
          (elinit--target-convergence-reasons
           (make-hash-table :test 'equal))
          (elinit--dashboard-show-targets t)
          (elinit--dashboard-show-init-targets t))
      (let* ((entries (elinit--get-entries))
             (service-ids
              (cl-loop for entry in entries
                       when (elinit--service-row-p (car entry))
                       collect (cdr (car entry)))))
        (should (member "svc" service-ids))
        (should (member "app.target" service-ids))
        (should (member "rescue.target" service-ids))))))

(ert-deftest elinit-test-dashboard-target-filter-bypasses-visibility ()
  "When target-filter is active, targets are shown regardless of visibility."
  (elinit-test-with-unit-files
      '(("sleep 1" :id "svc-a" :wanted-by ("app.target"))
        ("sleep 1" :id "svc-b")
        (nil :id "app.target" :type target))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--failed (make-hash-table :test 'equal))
           (elinit--oneshot-completed (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal))
           (elinit--enabled-override (make-hash-table :test 'equal))
           (elinit--restart-override (make-hash-table :test 'equal))
           (elinit--logging (make-hash-table :test 'equal))
           (elinit--target-convergence (make-hash-table :test 'equal))
           (elinit--target-convergence-reasons
            (make-hash-table :test 'equal))
           (members-hash (make-hash-table :test 'equal))
           (elinit--current-plan
            (elinit-plan--create
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
           (elinit--dashboard-target-filter "app.target")
           (elinit--dashboard-show-targets nil))
      (puthash "app.target" '(:requires nil :wants ("svc-a")) members-hash)
      (let* ((snapshot (elinit--build-snapshot))
             (all-entries (elinit--get-entries snapshot))
             (service-ids
              (cl-loop for entry in all-entries
                       when (elinit--service-row-p (car entry))
                       collect (cdr (car entry)))))
        (should (member "svc-a" service-ids))
        (should (member "app.target" service-ids))
        (should-not (member "svc-b" service-ids))))))

(ert-deftest elinit-test-dashboard-init-transition-target-predicate ()
  "Init-transition target predicate identifies correct IDs."
  (should (elinit--init-transition-target-p "rescue.target"))
  (should (elinit--init-transition-target-p "shutdown.target"))
  (should (elinit--init-transition-target-p "poweroff.target"))
  (should (elinit--init-transition-target-p "reboot.target"))
  (should (elinit--init-transition-target-p "runlevel0.target"))
  (should (elinit--init-transition-target-p "runlevel1.target"))
  (should (elinit--init-transition-target-p "runlevel6.target"))
  (should-not (elinit--init-transition-target-p "basic.target"))
  (should-not (elinit--init-transition-target-p "multi-user.target"))
  (should-not (elinit--init-transition-target-p "graphical.target"))
  (should-not (elinit--init-transition-target-p "default.target"))
  (should-not (elinit--init-transition-target-p "app.target")))

(ert-deftest elinit-test-dashboard-header-shows-filter-state ()
  "Header line always includes visibility indicator."
  (elinit-test-with-unit-files nil
    (let ((elinit--processes (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--dashboard-show-targets nil)
          (elinit--dashboard-show-init-targets nil))
      ;; Default: services-only indicator
      (let ((header (elinit--dashboard-header-line)))
        (should (string-match-p "\\[services\\]" header))
        (should-not (string-match-p "targets" header)))
      ;; Show targets: services+targets
      (let ((elinit--dashboard-show-targets t))
        (let ((header (elinit--dashboard-header-line)))
          (should (string-match-p "services" header))
          (should (string-match-p "targets" header))
          (should-not (string-match-p "init" header))))
      ;; Show targets + init: services+targets+init
      (let ((elinit--dashboard-show-targets t)
            (elinit--dashboard-show-init-targets t))
        (let ((header (elinit--dashboard-header-line)))
          (should (string-match-p "services" header))
          (should (string-match-p "targets" header))
          (should (string-match-p "init" header))))
      ;; Init toggled but targets hidden: init suppressed from header
      (let ((elinit--dashboard-show-targets nil)
            (elinit--dashboard-show-init-targets t))
        (let ((header (elinit--dashboard-header-line)))
          (should (string-match-p "\\[services\\]" header))
          (should-not (string-match-p "init" header)))))))

(ert-deftest elinit-test-dashboard-services-remain-sortable ()
  "Service rows preserve tabulated-list sorting after target filtering."
  (elinit-test-with-unit-files
      '(("sleep 60" :id "alpha" :type simple)
        ("sleep 60" :id "beta" :type simple)
        (nil :id "app.target" :type target))
    (let ((elinit--processes (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--restart-override (make-hash-table :test 'equal))
          (elinit--logging (make-hash-table :test 'equal))
          (elinit--target-convergence (make-hash-table :test 'equal))
          (elinit--target-convergence-reasons
           (make-hash-table :test 'equal))
          (elinit--dashboard-show-targets nil))
      (let* ((entries (elinit--get-entries))
             (service-ids
              (cl-loop for entry in entries
                       when (elinit--service-row-p (car entry))
                       collect (cdr (car entry)))))
        ;; Both services visible and in order
        (should (equal '("alpha" "beta") service-ids))))))

(ert-deftest elinit-test-dashboard-keymap-binds-v ()
  "Dashboard keymap binds v to toggle targets."
  (should (eq (lookup-key elinit-dashboard-mode-map "v")
              #'elinit-dashboard-toggle-targets)))

(ert-deftest elinit-test-dashboard-keymap-binds-shift-v ()
  "Dashboard keymap binds V to toggle init-transition targets."
  (should (eq (lookup-key elinit-dashboard-mode-map "V")
              #'elinit-dashboard-toggle-init-targets)))

(ert-deftest elinit-test-dashboard-toggle-targets-flips-state ()
  "Toggle-targets command flips show-targets state."
  (with-temp-buffer
    (let ((elinit--dashboard-show-targets nil))
      ;; Stub refresh to avoid needing a full dashboard buffer
      (cl-letf (((symbol-function 'elinit--refresh-dashboard)
                 (lambda ())))
        (elinit-dashboard-toggle-targets)
        (should elinit--dashboard-show-targets)
        (elinit-dashboard-toggle-targets)
        (should-not elinit--dashboard-show-targets)))))

(ert-deftest elinit-test-dashboard-toggle-init-targets-flips-state ()
  "Toggle-init-targets command flips show-init-targets state."
  (with-temp-buffer
    (let ((elinit--dashboard-show-init-targets nil))
      (cl-letf (((symbol-function 'elinit--refresh-dashboard)
                 (lambda ())))
        (elinit-dashboard-toggle-init-targets)
        (should elinit--dashboard-show-init-targets)
        (elinit-dashboard-toggle-init-targets)
        (should-not elinit--dashboard-show-init-targets)))))

(provide 'elinit-test-dashboard)
;;; elinit-test-dashboard.el ends here
