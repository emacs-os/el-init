;;; supervisor-test-restart.el --- Restart policy tests for supervisor.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Restart policy ERT tests for supervisor.el.

;;; Code:

(require 'supervisor-test-helpers)

;;; Restart policy tests

(ert-deftest supervisor-test-parse-restart-policy-symbols ()
  "All four restart policy symbols parse correctly."
  (let ((always (supervisor--parse-entry '("foo" :restart always)))
        (no (supervisor--parse-entry '("foo" :restart no)))
        (on-failure (supervisor--parse-entry '("foo" :restart on-failure)))
        (on-success (supervisor--parse-entry '("foo" :restart on-success))))
    (should (eq (nth 4 always) 'always))
    (should (eq (nth 4 no) 'no))
    (should (eq (nth 4 on-failure) 'on-failure))
    (should (eq (nth 4 on-success) 'on-success))))

(ert-deftest supervisor-test-parse-restart-policy-boolean-compat ()
  "Boolean :restart values are normalized to policy symbols."
  (let ((bool-t (supervisor--parse-entry '("foo" :restart t)))
        (bool-nil (supervisor--parse-entry '("foo" :restart nil)))
        (no-restart (supervisor--parse-entry '("foo" :no-restart t))))
    (should (eq (nth 4 bool-t) 'always))
    (should (eq (nth 4 bool-nil) 'no))
    (should (eq (nth 4 no-restart) 'no))))

(ert-deftest supervisor-test-validate-restart-invalid-symbol ()
  "Invalid :restart symbol is rejected by validation."
  (let ((reason (supervisor--validate-entry '("foo" :restart bogus))))
    (should reason)
    (should (string-match-p ":restart" reason))))

(ert-deftest supervisor-test-clean-exit-p ()
  "Clean exit predicate handles all cases."
  ;; Exit code 0 is always clean
  (should (supervisor--clean-exit-p 'exit 0))
  ;; Non-zero exit is not clean
  (should-not (supervisor--clean-exit-p 'exit 1))
  (should-not (supervisor--clean-exit-p 'exit 127))
  ;; Clean signals: HUP(1), INT(2), PIPE(13), TERM(15)
  (should (supervisor--clean-exit-p 'signal 1))
  (should (supervisor--clean-exit-p 'signal 2))
  (should (supervisor--clean-exit-p 'signal 13))
  (should (supervisor--clean-exit-p 'signal 15))
  ;; Non-clean signals: KILL(9), SEGV(11)
  (should-not (supervisor--clean-exit-p 'signal 9))
  (should-not (supervisor--clean-exit-p 'signal 11)))

(ert-deftest supervisor-test-should-restart-p ()
  "Full policy x exit-type matrix for restart decisions."
  ;; always: restart on any exit
  (should (supervisor--should-restart-p 'always 'exit 0))
  (should (supervisor--should-restart-p 'always 'exit 1))
  (should (supervisor--should-restart-p 'always 'signal 9))
  (should (supervisor--should-restart-p 'always 'signal 15))
  ;; no: never restart
  (should-not (supervisor--should-restart-p 'no 'exit 0))
  (should-not (supervisor--should-restart-p 'no 'exit 1))
  (should-not (supervisor--should-restart-p 'no 'signal 9))
  (should-not (supervisor--should-restart-p 'no 'signal 15))
  ;; on-failure: restart only on non-clean exit
  (should-not (supervisor--should-restart-p 'on-failure 'exit 0))
  (should (supervisor--should-restart-p 'on-failure 'exit 1))
  (should (supervisor--should-restart-p 'on-failure 'signal 9))
  (should-not (supervisor--should-restart-p 'on-failure 'signal 15))
  ;; on-success: restart only on clean exit
  (should (supervisor--should-restart-p 'on-success 'exit 0))
  (should-not (supervisor--should-restart-p 'on-success 'exit 1))
  (should-not (supervisor--should-restart-p 'on-success 'signal 9))
  (should (supervisor--should-restart-p 'on-success 'signal 15))
  ;; Legacy boolean compat
  (should (supervisor--should-restart-p t 'exit 1))
  (should-not (supervisor--should-restart-p nil 'exit 1)))

(ert-deftest supervisor-test-signal-to-number ()
  "Signal symbol to number lookup."
  (should (= 1 (supervisor--signal-to-number 'SIGHUP)))
  (should (= 9 (supervisor--signal-to-number 'SIGKILL)))
  (should (= 10 (supervisor--signal-to-number 'SIGUSR1)))
  (should (= 15 (supervisor--signal-to-number 'SIGTERM)))
  (should (= 17 (supervisor--signal-to-number 'SIGCHLD)))
  (should (= 31 (supervisor--signal-to-number 'SIGSYS)))
  (should-not (supervisor--signal-to-number 'SIGFAKE)))

(ert-deftest supervisor-test-signal-to-number-covers-known-signals ()
  "Every signal in `supervisor--known-signals' has a number mapping."
  (dolist (sig supervisor--known-signals)
    (should (integerp (supervisor--signal-to-number sig)))))

(ert-deftest supervisor-test-clean-exit-p-extra-codes ()
  "Extra exit codes from :success-exit-status are treated as clean."
  (let ((ses '(:codes (42 77) :signals nil)))
    ;; 42 is normally not clean
    (should-not (supervisor--clean-exit-p 'exit 42))
    ;; With extra codes, 42 is clean
    (should (supervisor--clean-exit-p 'exit 42 ses))
    (should (supervisor--clean-exit-p 'exit 77 ses))
    ;; Other codes still not clean
    (should-not (supervisor--clean-exit-p 'exit 99 ses))
    ;; Exit 0 still clean (baseline)
    (should (supervisor--clean-exit-p 'exit 0 ses))))

(ert-deftest supervisor-test-clean-exit-p-extra-signals ()
  "Extra signals from :success-exit-status are treated as clean."
  (let ((ses '(:codes nil :signals (SIGUSR1 SIGUSR2))))
    ;; SIGUSR1 (10) is normally not clean
    (should-not (supervisor--clean-exit-p 'signal 10))
    ;; With extra signals, SIGUSR1 is clean
    (should (supervisor--clean-exit-p 'signal 10 ses))
    ;; SIGUSR2 (12) is clean
    (should (supervisor--clean-exit-p 'signal 12 ses))
    ;; SIGKILL (9) still not clean
    (should-not (supervisor--clean-exit-p 'signal 9 ses))
    ;; Baseline clean signals still clean
    (should (supervisor--clean-exit-p 'signal 15 ses))))

(ert-deftest supervisor-test-should-restart-p-on-failure-extra-code ()
  "On-failure policy: extra success code suppresses restart."
  (let ((ses '(:codes (42) :signals nil)))
    ;; Without extra, exit 42 triggers restart under on-failure
    (should (supervisor--should-restart-p 'on-failure 'exit 42))
    ;; With extra, exit 42 is clean so no restart
    (should-not (supervisor--should-restart-p 'on-failure 'exit 42 ses))
    ;; Exit 1 still triggers restart
    (should (supervisor--should-restart-p 'on-failure 'exit 1 ses))))

(ert-deftest supervisor-test-should-restart-p-on-success-extra-code ()
  "On-success policy: extra success code triggers restart."
  (let ((ses '(:codes (42) :signals nil)))
    ;; Without extra, exit 42 does NOT trigger restart under on-success
    (should-not (supervisor--should-restart-p 'on-success 'exit 42))
    ;; With extra, exit 42 is clean so restart triggers
    (should (supervisor--should-restart-p 'on-success 'exit 42 ses))))

(ert-deftest supervisor-test-should-restart-p-on-failure-extra-signal ()
  "On-failure policy: extra success signal suppresses restart."
  (let ((ses '(:codes nil :signals (SIGUSR1))))
    ;; SIGUSR1 (10) normally triggers restart under on-failure
    (should (supervisor--should-restart-p 'on-failure 'signal 10))
    ;; With extra, SIGUSR1 is clean so no restart
    (should-not (supervisor--should-restart-p 'on-failure 'signal 10 ses))))

(ert-deftest supervisor-test-should-restart-p-always-no-unaffected ()
  "Always/no policies unaffected by :success-exit-status."
  (let ((ses '(:codes (42) :signals (SIGUSR1))))
    (should (supervisor--should-restart-p 'always 'exit 42 ses))
    (should-not (supervisor--should-restart-p 'no 'exit 42 ses))))

(ert-deftest supervisor-test-clean-exit-p-extra-signal-beyond-15 ()
  "Signals beyond the base 1-15 range work in :success-exit-status."
  (let ((ses '(:codes nil :signals (SIGCHLD))))
    ;; SIGCHLD (17) is not in the baseline clean set
    (should-not (supervisor--clean-exit-p 'signal 17))
    ;; With :success-exit-status, SIGCHLD is treated as clean
    (should (supervisor--clean-exit-p 'signal 17 ses))))

(ert-deftest supervisor-test-overrides-load-migrates-legacy-restart ()
  "Loading overrides with legacy enabled/disabled migrates to policy symbols."
  (let* ((temp-file (make-temp-file "supervisor-test-migrate-" nil ".eld"))
         (supervisor-overrides-file temp-file)
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--restart-override (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--overrides-loaded nil))
    (unwind-protect
        (progn
          ;; Write file with legacy enabled/disabled restart values
          (with-temp-file temp-file
            (insert ";; test overrides\n")
            (prin1 `((version . 1)
                     (timestamp . "2025-01-01T00:00:00+0000")
                     (overrides . (("svc-a" :restart enabled)
                                   ("svc-b" :restart disabled))))
                   (current-buffer)))
          ;; Load
          (should (supervisor--load-overrides))
          ;; Legacy values should be migrated
          (should (eq 'always (gethash "svc-a" supervisor--restart-override)))
          (should (eq 'no (gethash "svc-b" supervisor--restart-override))))
      (delete-file temp-file))))

(ert-deftest supervisor-test-entry-restart-p-accessor ()
  "The restart-p accessor returns boolean from policy symbol."
  (let ((always-entry (list "id" "cmd" 0 t 'always t 'simple nil t 30 nil nil))
        (no-entry (list "id" "cmd" 0 t 'no t 'simple nil t 30 nil nil))
        (on-failure-entry (list "id" "cmd" 0 t 'on-failure t 'simple nil t 30 nil nil)))
    (should (supervisor-entry-restart-p always-entry))
    (should-not (supervisor-entry-restart-p no-entry))
    (should (supervisor-entry-restart-p on-failure-entry))))

(ert-deftest supervisor-test-entry-restart-policy-accessor ()
  "The restart-policy accessor returns the raw policy symbol."
  (let ((entry (list "id" "cmd" 0 t 'on-failure t 'simple nil t 30 nil nil)))
    (should (eq 'on-failure (supervisor-entry-restart-policy entry)))))

(ert-deftest supervisor-test-get-effective-restart-policy ()
  "Effective restart returns policy symbols with override migration."
  (let ((supervisor--restart-override (make-hash-table :test 'equal)))
    ;; No override: return config value
    (should (eq 'always (supervisor--get-effective-restart "svc" 'always)))
    (should (eq 'on-failure (supervisor--get-effective-restart "svc" 'on-failure)))
    ;; Legacy boolean config: normalized
    (should (eq 'always (supervisor--get-effective-restart "svc" t)))
    (should (eq 'no (supervisor--get-effective-restart "svc" nil)))
    ;; Override with policy symbol
    (puthash "svc" 'no supervisor--restart-override)
    (should (eq 'no (supervisor--get-effective-restart "svc" 'always)))
    ;; Override with legacy enabled/disabled
    (puthash "svc" 'enabled supervisor--restart-override)
    (should (eq 'always (supervisor--get-effective-restart "svc" 'no)))
    (puthash "svc" 'disabled supervisor--restart-override)
    (should (eq 'no (supervisor--get-effective-restart "svc" 'always)))))

(ert-deftest supervisor-test-restart-policy-to-bool-nil ()
  "Nil restart policy (oneshot n/a) converts to false, not true."
  (should-not (supervisor--restart-policy-to-bool nil))
  (should-not (supervisor--restart-policy-to-bool 'no))
  (should (supervisor--restart-policy-to-bool 'always))
  (should (supervisor--restart-policy-to-bool 'on-failure))
  (should (supervisor--restart-policy-to-bool 'on-success)))

(ert-deftest supervisor-test-cycle-restart-policy ()
  "Cycle restart policy follows no -> on-success -> on-failure -> always -> no."
  (should (eq 'on-success (supervisor--cycle-restart-policy 'no)))
  (should (eq 'on-failure (supervisor--cycle-restart-policy 'on-success)))
  (should (eq 'always (supervisor--cycle-restart-policy 'on-failure)))
  (should (eq 'no (supervisor--cycle-restart-policy 'always)))
  ;; Unknown values default to no
  (should (eq 'no (supervisor--cycle-restart-policy 'bogus))))

(ert-deftest supervisor-test-dashboard-set-restart-policy-interactive ()
  "Dashboard set-restart-policy applies explicit selections end-to-end.
Exercises the interactive function with a real tabulated-list buffer,
verifying policy selection, override-clear on return to config, and
restart-timer cancellation on `no'."
  (supervisor-test-with-unit-files
      '(("sleep 999" :id "svc" :type simple :restart always))
    (let ((supervisor--restart-override (make-hash-table :test 'equal))
          (supervisor--restart-timers (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--processes (make-hash-table :test 'equal))
          (supervisor--failed (make-hash-table :test 'equal))
          (supervisor--oneshot-completed (make-hash-table :test 'equal))
          (supervisor--manually-stopped (make-hash-table :test 'equal))
          (supervisor--manually-started (make-hash-table :test 'equal))
          (supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor--logging (make-hash-table :test 'equal))
          (supervisor--mask-override (make-hash-table :test 'equal))
          (supervisor--entry-state (make-hash-table :test 'equal))
          (supervisor--start-times (make-hash-table :test 'equal))
          (supervisor--ready-times (make-hash-table :test 'equal))
          (supervisor-overrides-file nil)
          (fake-timer (run-at-time 9999 nil #'ignore)))
      (unwind-protect
          (let ((buf (get-buffer-create "*supervisor*")))
            (unwind-protect
                (with-current-buffer buf
                  (supervisor-dashboard-mode)
                  ;; Pre-seed a restart timer for cancellation test
                  (puthash "svc" fake-timer supervisor--restart-timers)
                  ;; Helper: move point to the "svc" row
                  (cl-flet ((goto-svc ()
                              (goto-char (point-min))
                              (while (and (not (eobp))
                                          (not (equal (cons :service "svc")
                                                      (tabulated-list-get-id))))
                                (forward-line 1))))
                    ;; Initial refresh to populate entries from unit file
                    (supervisor--refresh-dashboard)
                    (let ((choices '("no" "on-success" "on-failure" "always")))
                      (cl-letf (((symbol-function 'completing-read)
                                 (lambda (_prompt _collection &rest _)
                                   (prog1 (car choices)
                                     (setq choices (cdr choices))))))
                        ;; Selection 1: config=always -> no (override set, timer cancelled)
                        (goto-svc)
                        (supervisor-dashboard-set-restart-policy)
                        (should (eq 'no (gethash "svc" supervisor--restart-override)))
                        (should-not (gethash "svc" supervisor--restart-timers))
                        ;; Selection 2: no -> on-success
                        (goto-svc)
                        (supervisor-dashboard-set-restart-policy)
                        (should (eq 'on-success (gethash "svc" supervisor--restart-override)))
                        ;; Selection 3: on-success -> on-failure
                        (goto-svc)
                        (supervisor-dashboard-set-restart-policy)
                        (should (eq 'on-failure (gethash "svc" supervisor--restart-override)))
                        ;; Selection 4: on-failure -> always (matches config, override cleared)
                        (goto-svc)
                        (supervisor-dashboard-set-restart-policy)
                        (should-not (gethash "svc" supervisor--restart-override))))))
              (kill-buffer buf)))
        (when (timerp fake-timer) (cancel-timer fake-timer))))))

(ert-deftest supervisor-test-cli-restart-policy-rejects-legacy-on-off ()
  "Restart-policy rejects legacy on/off values."
  (let ((supervisor--restart-override (make-hash-table :test 'equal)))
    (let ((result (supervisor--cli-dispatch '("restart-policy" "on" "test-id"))))
      (should (= supervisor-cli-exit-invalid-args
                 (supervisor-cli-result-exitcode result))))
    (let ((result (supervisor--cli-dispatch '("restart-policy" "off" "test-id"))))
      (should (= supervisor-cli-exit-invalid-args
                 (supervisor-cli-result-exitcode result))))))

(ert-deftest supervisor-test-oneshot-json-restart-na ()
  "Oneshot entries emit restart \"n/a\" in JSON."
  (let* ((supervisor-programs '(("true" :type oneshot)))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--restart-override (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--failed (make-hash-table :test 'equal))
         (supervisor--oneshot-completed (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (supervisor--manually-started (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--start-times (make-hash-table :test 'equal))
         (supervisor--ready-times (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor-unit-directory "/nonexistent-supervisor-test-dir"))
    (let* ((result (supervisor--cli-all-entries-info))
           (entries (car result))
           (info (car entries))
           (json-obj (supervisor--cli-entry-to-json-obj info)))
      ;; restart field must be "n/a" for oneshot (not a policy symbol)
      (should (equal "n/a" (alist-get 'restart json-obj))))))

(ert-deftest supervisor-test-dashboard-restart-uses-snapshot ()
  "Dashboard restart resolution uses snapshot, not global state."
  (let* ((snapshot-restart (make-hash-table :test 'equal))
         (supervisor--restart-override (make-hash-table :test 'equal)))
    ;; Global says always, snapshot says no
    (puthash "svc" 'always supervisor--restart-override)
    (puthash "svc" 'no snapshot-restart)
    (let* ((snapshot (supervisor-snapshot--create
                      :process-alive (make-hash-table :test 'equal)
                      :process-pids (make-hash-table :test 'equal)
                      :failed (make-hash-table :test 'equal)
                      :oneshot-exit (make-hash-table :test 'equal)
                      :entry-state (make-hash-table :test 'equal)
                      :invalid (make-hash-table :test 'equal)
                      :enabled-override (make-hash-table :test 'equal)
                      :restart-override snapshot-restart
                      :logging-override (make-hash-table :test 'equal)
                      :mask-override (make-hash-table :test 'equal)
                      :manually-started (make-hash-table :test 'equal)
                      :timestamp (float-time)))
           (vec (supervisor--make-dashboard-entry
                 "svc" 'simple nil t 'always t snapshot)))
      ;; Restart column (index 5) should say "no" from snapshot, not "yes" from global
      (should (equal "no" (aref vec 5))))))

(ert-deftest supervisor-test-dashboard-oneshot-restart-renders-na ()
  "Dashboard renders oneshot restart column as n/a."
  (let* ((snapshot (supervisor-snapshot--create
                    :process-alive (make-hash-table :test 'equal)
                    :process-pids (make-hash-table :test 'equal)
                    :failed (make-hash-table :test 'equal)
                    :oneshot-exit (make-hash-table :test 'equal)
                    :entry-state (make-hash-table :test 'equal)
                    :invalid (make-hash-table :test 'equal)
                    :enabled-override (make-hash-table :test 'equal)
                    :restart-override (make-hash-table :test 'equal)
                    :logging-override (make-hash-table :test 'equal)
                    :mask-override (make-hash-table :test 'equal)
                    :manually-started (make-hash-table :test 'equal)
                    :timestamp (float-time)))
         (vec (supervisor--make-dashboard-entry
               "svc" 'oneshot nil t 'always t snapshot)))
    (should (equal "n/a" (aref vec 5)))))

(ert-deftest supervisor-test-dashboard-oneshot-done-pid-renders-dash ()
  "Dashboard hides oneshot exit-code pseudo-PIDs in the PID column."
  (let* ((oneshot-exit (make-hash-table :test 'equal))
         (snapshot nil)
         (vec nil))
    (puthash "svc" 0 oneshot-exit)
    (setq snapshot
          (supervisor-snapshot--create
           :process-alive (make-hash-table :test 'equal)
           :process-pids (make-hash-table :test 'equal)
           :failed (make-hash-table :test 'equal)
           :oneshot-exit oneshot-exit
           :entry-state (make-hash-table :test 'equal)
           :invalid (make-hash-table :test 'equal)
           :enabled-override (make-hash-table :test 'equal)
           :restart-override (make-hash-table :test 'equal)
           :logging-override (make-hash-table :test 'equal)
           :mask-override (make-hash-table :test 'equal)
           :manually-started (make-hash-table :test 'equal)
           :timestamp (float-time)))
    (setq vec (supervisor--make-dashboard-entry
               "svc" 'oneshot nil t 'always t snapshot))
    (should (equal "done" (substring-no-properties (aref vec 4))))
    (should (equal "-" (aref vec 7)))))


(provide 'supervisor-test-restart)
;;; supervisor-test-restart.el ends here
