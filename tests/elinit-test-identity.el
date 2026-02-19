;;; elinit-test-identity.el --- User/group identity and runas tests for elinit.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; User/group identity and runas ERT tests for elinit.el.

;;; Code:

(require 'elinit-test-helpers)

;;; User/Group Schema Tests (Phase 1)

(ert-deftest elinit-test-parse-user-string ()
  "Parse entry accepts :user as string."
  (let ((entry (elinit--parse-entry '("cmd" :id "svc" :user "postgres"))))
    (should (equal (elinit-entry-user entry) "postgres"))
    (should-not (elinit-entry-group entry))))

(ert-deftest elinit-test-parse-user-integer ()
  "Parse entry accepts :user as integer."
  (let ((entry (elinit--parse-entry '("cmd" :id "svc" :user 1000))))
    (should (= (elinit-entry-user entry) 1000))))

(ert-deftest elinit-test-parse-group-string ()
  "Parse entry accepts :group as string."
  (let ((entry (elinit--parse-entry '("cmd" :id "svc" :group "www-data"))))
    (should (equal (elinit-entry-group entry) "www-data"))
    (should-not (elinit-entry-user entry))))

(ert-deftest elinit-test-parse-group-integer ()
  "Parse entry accepts :group as integer."
  (let ((entry (elinit--parse-entry '("cmd" :id "svc" :group 33))))
    (should (= (elinit-entry-group entry) 33))))

(ert-deftest elinit-test-parse-user-group-both ()
  "Parse entry accepts both :user and :group."
  (let ((entry (elinit--parse-entry
                '("cmd" :id "svc" :user "postgres" :group "postgres"))))
    (should (equal (elinit-entry-user entry) "postgres"))
    (should (equal (elinit-entry-group entry) "postgres"))))

(ert-deftest elinit-test-parse-user-nil-default ()
  "Parse entry defaults :user to nil when absent."
  (let ((entry (elinit--parse-entry '("cmd" :id "svc"))))
    (should-not (elinit-entry-user entry))
    (should-not (elinit-entry-group entry))))

(ert-deftest elinit-test-validate-user-invalid-symbol ()
  "Validation rejects :user as symbol."
  (should (string-match-p ":user must be"
                          (elinit--validate-entry
                           '("cmd" :user postgres)))))

(ert-deftest elinit-test-validate-user-invalid-list ()
  "Validation rejects :user as list."
  (should (string-match-p ":user must be"
                          (elinit--validate-entry
                           '("cmd" :user ("a" "b"))))))

(ert-deftest elinit-test-validate-group-invalid-symbol ()
  "Validation rejects :group as symbol."
  (should (string-match-p ":group must be"
                          (elinit--validate-entry
                           '("cmd" :group www-data)))))

(ert-deftest elinit-test-validate-group-invalid-list ()
  "Validation rejects :group as list."
  (should (string-match-p ":group must be"
                          (elinit--validate-entry
                           '("cmd" :group (33 34))))))

(ert-deftest elinit-test-validate-user-string-valid ()
  "Validation accepts :user as string."
  (should-not (elinit--validate-entry '("cmd" :user "postgres"))))

(ert-deftest elinit-test-validate-user-integer-valid ()
  "Validation accepts :user as integer."
  (should-not (elinit--validate-entry '("cmd" :user 1000))))

(ert-deftest elinit-test-validate-group-string-valid ()
  "Validation accepts :group as string."
  (should-not (elinit--validate-entry '("cmd" :group "www-data"))))

(ert-deftest elinit-test-validate-group-integer-valid ()
  "Validation accepts :group as integer."
  (should-not (elinit--validate-entry '("cmd" :group 33))))

(ert-deftest elinit-test-service-roundtrip-user-group ()
  "Service struct roundtrip preserves :user and :group."
  (let* ((entry (elinit--parse-entry
                 '("cmd" :id "svc" :user "alice" :group 100)))
         (svc (elinit-entry-to-service entry))
         (back (elinit-service-to-entry svc)))
    (should (equal (elinit-entry-user back) "alice"))
    (should (= (elinit-entry-group back) 100))))

;;; Spawn Abstraction Tests (Phase 2)

(ert-deftest elinit-test-build-launch-command-simple ()
  "Build launch command splits command string into args."
  (should (equal (elinit--build-launch-command "sleep 300")
                 '("sleep" "300"))))

(ert-deftest elinit-test-build-launch-command-quoted ()
  "Build launch command handles quoted arguments."
  (should (equal (elinit--build-launch-command "echo \"hello world\"")
                 '("echo" "hello world"))))

(ert-deftest elinit-test-build-launch-command-single ()
  "Build launch command handles single-word command."
  (should (equal (elinit--build-launch-command "nginx")
                 '("nginx"))))

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

(ert-deftest elinit-test-build-launch-command-group-only ()
  "Build launch command prepends helper with --group when group is set."
  (let ((elinit-runas-command "/usr/libexec/elinit-runas"))
    (let ((result (elinit--build-launch-command "sleep 300" nil "staff")))
      (should (equal (car result) "/usr/libexec/elinit-runas"))
      (should (member "--group" result))
      (should (equal (nth (1+ (cl-position "--group" result :test #'equal))
                          result)
                     "staff"))
      (should (member "--" result)))))

(ert-deftest elinit-test-build-launch-command-user-and-group ()
  "Build launch command prepends helper with both --user and --group."
  (let ((elinit-runas-command "/usr/libexec/elinit-runas"))
    (let ((result (elinit--build-launch-command "echo hi" "postgres" "postgres")))
      (should (equal (car result) "/usr/libexec/elinit-runas"))
      (should (member "--user" result))
      (should (member "--group" result))
      (should (member "--" result)))))

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

(ert-deftest elinit-test-build-launch-command-no-wrapper-nil ()
  "Build launch command returns plain args when user and group are nil."
  (should (equal (elinit--build-launch-command "sleep 300" nil nil)
                 '("sleep" "300"))))

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

(ert-deftest elinit-test-build-launch-command-pipe ()
  "Build launch command wraps piped commands in sh -c."
  (let ((result (elinit--build-launch-command "ls | grep foo")))
    (should (equal (car result) shell-file-name))
    (should (equal (nth 2 result) "ls | grep foo"))))

(ert-deftest elinit-test-build-launch-command-no-shell-for-simple ()
  "Build launch command does not wrap simple commands in sh -c."
  (let ((result (elinit--build-launch-command "sleep 300")))
    (should (equal result '("sleep" "300")))))

;;; elinit-runas Helper Tests (Phase 3)
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

(ert-deftest elinit-test-runas-no-identity ()
  "Helper exits 111 when neither --user nor --group is specified."
  (skip-unless (file-executable-p elinit-test-runas-binary))
  (with-temp-buffer
    (let ((code (call-process elinit-test-runas-binary nil t nil
                              "--" "echo" "hi")))
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

(ert-deftest elinit-test-runas-unknown-group ()
  "Helper exits 112 for unknown group name."
  (skip-unless (file-executable-p elinit-test-runas-binary))
  (with-temp-buffer
    (let ((code (call-process elinit-test-runas-binary nil t nil
                              "--group" "nonexistent_group_xyz_sv"
                              "--" "echo" "hi")))
      (should (= code 112))
      (should (string-match-p "unknown group"
                              (buffer-string))))))

(ert-deftest elinit-test-runas-unknown-option ()
  "Helper exits 111 for unknown option."
  (skip-unless (file-executable-p elinit-test-runas-binary))
  (with-temp-buffer
    (let ((code (call-process elinit-test-runas-binary nil t nil
                              "--bogus" "--" "echo" "hi")))
      (should (= code 111)))))

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
  ;; Use numeric uid/gid of current user to avoid initgroups permission error
  ;; on non-root.  On non-root this still fails at setgid, but if we are root
  ;; (CI lane) it reaches execv.  Use skip-unless to gate on root.
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
      ;; nobody's uid is 65534 on most systems
      (should (string-match-p "^[0-9]+$"
                              (string-trim (buffer-string)))))))

(ert-deftest elinit-test-build-launch-command-resolves-path ()
  "Build launch command resolves program to absolute path when wrapping."
  (let ((elinit-runas-command "/usr/libexec/elinit-runas"))
    (let ((result (elinit--build-launch-command "sleep 300" "alice" nil)))
      ;; The program after "--" should be an absolute path
      (let ((cmd-after-sep (cdr (member "--" result))))
        (should cmd-after-sep)
        (should (file-name-absolute-p (car cmd-after-sep)))))))

;;;; Phase 4 – Core integration (user/group threading)

(ert-deftest elinit-test-start-process-nonroot-rejects-identity ()
  "Non-root elinit returns nil when user/group identity is requested."
  (skip-unless (not (zerop (user-uid))))
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--shutting-down nil))
    ;; Should return nil because non-root cannot change identity
    (should-not (elinit--start-process
                 "svc" "sleep 300" nil 'simple 'yes nil
                 nil nil nil nil nil "alice" nil))))

(ert-deftest elinit-test-start-process-nonroot-rejects-group-only ()
  "Non-root elinit returns nil when only group identity is requested."
  (skip-unless (not (zerop (user-uid))))
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--shutting-down nil))
    (should-not (elinit--start-process
                 "svc" "sleep 300" nil 'simple 'yes nil
                 nil nil nil nil nil nil "staff"))))

(ert-deftest elinit-test-start-process-nil-identity-ok-nonroot ()
  "Non-root elinit starts normally when user/group are nil."
  (skip-unless (not (zerop (user-uid))))
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--shutting-down nil)
        (proc nil))
    (unwind-protect
        (progn
          (setq proc (elinit--start-process
                      "svc-p4-test" "sleep 300" nil 'simple 'yes nil
                      nil nil nil nil nil nil nil))
          (should proc)
          (should (process-live-p proc)))
      (when (and proc (process-live-p proc))
        (delete-process proc)))))

(ert-deftest elinit-test-manual-start-nonroot-rejects-identity ()
  "Manual start returns error when non-root and user/group set."
  (skip-unless (not (zerop (user-uid))))
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
      (let ((result (elinit--manual-start "svc1")))
        (should (eq (plist-get result :status) 'error))
        (should (string-match-p "identity change requires root"
                                (plist-get result :reason)))))))

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
                 ;; args = (is-restart wd env env-file restart-sec ufd user group)
                 ;; user is at position 6, group at 7
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

(ert-deftest elinit-test-dag-start-entry-extracts-user-group ()
  "DAG start-entry-async extracts user/group from entry tuple."
  (let* ((entry (elinit--parse-entry
                 '("sleep 300" :id "svc-ug" :user "postgres" :group "postgres")))
         (elinit--dag-started (make-hash-table :test 'equal))
         (elinit--dag-entries (make-hash-table :test 'equal))
         (elinit--start-times (make-hash-table :test 'equal))
         (elinit--dag-active-starts 0)
         (elinit--enabled-override (make-hash-table :test 'equal))
         (elinit--mask-override (make-hash-table :test 'equal))
         (elinit--dag-delay-timers (make-hash-table :test 'equal))
         (elinit--unit-file-dirs (make-hash-table :test 'equal))
         (captured-user nil)
         (captured-group nil))
    (puthash "svc-ug" entry elinit--dag-entries)
    (cl-letf (((symbol-function 'elinit--start-process)
               (lambda (_id _cmd _log _type _restart &rest args)
                 (setq captured-user (nth 6 args))
                 (setq captured-group (nth 7 args))
                 t))
              ((symbol-function 'elinit--dag-handle-spawn-success) #'ignore)
              ((symbol-function 'executable-find) (lambda (_) t)))
      (elinit--dag-start-entry-async entry))
    (should (equal captured-user "postgres"))
    (should (equal captured-group "postgres"))))

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
                 ;; args = (wd env env-file restart-sec ufd user group)
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
              ;; Give sentinel time to fire
              (sleep-for 0.1)
              (funcall sentinel proc "finished\n")
              (should (equal captured-user "alice"))
              (should (equal captured-group "staff")))
          (when (process-live-p proc)
            (delete-process proc)))))))

(ert-deftest elinit-test-build-launch-command-threads-through-start-process ()
  "Start-process passes user/group to build-launch-command."
  (let ((captured-user nil)
        (captured-group nil)
        (elinit--processes (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--shutting-down nil)
        (proc nil))
    ;; Mock user-uid to 0 and trust check so guards do not reject
    (cl-letf (((symbol-function 'user-uid) (lambda () 0))
              ((symbol-function 'elinit--identity-source-trusted-p)
               (lambda (_id) t))
              ((symbol-function 'elinit--build-launch-command)
               (lambda (cmd &optional user group _sandbox-entry _limits-entry)
                 (setq captured-user user)
                 (setq captured-group group)
                 (split-string-and-unquote cmd))))
      (unwind-protect
          (progn
            (setq proc (elinit--start-process
                        "svc-blc" "sleep 300" nil 'simple 'yes nil
                        nil nil nil nil nil "bob" "users"))
            (should (equal captured-user "bob"))
            (should (equal captured-group "users")))
        (when (and proc (process-live-p proc))
          (delete-process proc))))))

;;;; Phase 5 – Trust enforcement for identity-changing units

(ert-deftest elinit-test-identity-trust-no-units-module ()
  "Trust check fails closed when units module is not loaded."
  (cl-letf (((symbol-function 'elinit--unit-file-path)
             nil))
    (should-not (elinit--identity-source-trusted-p "svc1"))))

(ert-deftest elinit-test-identity-trust-no-unit-file ()
  "Trust check fails closed when unit file path is nil."
  (cl-letf (((symbol-function 'elinit--unit-file-path)
             (lambda (_id) nil)))
    (should-not (elinit--identity-source-trusted-p "svc1"))))

(ert-deftest elinit-test-identity-trust-file-not-found ()
  "Trust check fails closed when unit file does not exist."
  (cl-letf (((symbol-function 'elinit--unit-file-path)
             (lambda (_id) "/nonexistent/path/svc1.el")))
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
            ;; File is owned by current (non-root) user
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
          ;; Make world-writable
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
          ;; Ensure proper mode (root-owned by default in root context)
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
      ;; Mock root euid, but trust check fails (file not root-owned)
      (cl-letf (((symbol-function 'user-uid) (lambda () 0))
                ((symbol-function 'elinit--identity-source-trusted-p)
                 (lambda (_id) nil)))
        (let ((result (elinit--manual-start "svc1")))
          (should (eq (plist-get result :status) 'error))
          (should (string-match-p "not trusted"
                                  (plist-get result :reason))))))))

(ert-deftest elinit-test-manual-start-root-trusted-source ()
  "Manual start succeeds when root and unit source is trusted."
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
           (elinit--logging (make-hash-table :test 'equal))
           (elinit--spawn-failure-reason (make-hash-table :test 'equal))
           (elinit--writers (make-hash-table :test 'equal))
           (fake-proc (start-process "svc1" nil "sleep" "300"))
           (proc nil))
      (unwind-protect
          ;; Mock root euid, trusted source, executable-find, and
          ;; make-process (runas binary is gitignored and absent in CI)
          (cl-letf (((symbol-function 'user-uid) (lambda () 0))
                    ((symbol-function 'elinit--identity-source-trusted-p)
                     (lambda (_id) t))
                    ((symbol-function 'executable-find)
                     (lambda (_cmd) "/usr/bin/sleep"))
                    ((symbol-function 'make-process)
                     (lambda (&rest _args) fake-proc)))
            (let ((result (elinit--manual-start "svc1")))
              (should (eq (plist-get result :status) 'started))
              (setq proc (gethash "svc1" elinit--processes))))
        (when (process-live-p fake-proc)
          (delete-process fake-proc))))))

(ert-deftest elinit-test-start-process-root-untrusted-blocked ()
  "Start-process returns nil when root but source untrusted."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--shutting-down nil))
    (cl-letf (((symbol-function 'user-uid) (lambda () 0))
              ((symbol-function 'elinit--identity-source-trusted-p)
               (lambda (_id) nil)))
      (should-not (elinit--start-process
                   "svc" "sleep 300" nil 'simple 'yes nil
                   nil nil nil nil nil "alice" nil)))))

;;; Phase 7: Identity fields in CLI/dashboard detail surfaces

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

(ert-deftest elinit-test-cli-entry-info-nil-user-group ()
  "Entry info alist has nil user and group when not set."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (plan (elinit--build-plan (elinit--effective-programs)))
           (entry (car (elinit-plan-entries plan)))
           (info (elinit--cli-entry-info entry)))
      (should-not (alist-get 'user info))
      (should-not (alist-get 'group info)))))

(ert-deftest elinit-test-cli-describe-human-shows-user-group ()
  "Human describe output includes User and Group lines."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple :user "alice" :group "staff"))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (plan (elinit--build-plan (elinit--effective-programs)))
           (entry (car (elinit-plan-entries plan)))
           (info (elinit--cli-entry-info entry))
           (output (elinit--cli-describe-human info)))
      (should (string-match "User: alice" output))
      (should (string-match "Group: staff" output)))))

(ert-deftest elinit-test-cli-describe-human-omits-user-when-nil ()
  "Human describe output omits User and Group lines when not set."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (plan (elinit--build-plan (elinit--effective-programs)))
           (entry (car (elinit-plan-entries plan)))
           (info (elinit--cli-entry-info entry))
           (output (elinit--cli-describe-human info)))
      (should-not (string-match "User:" output))
      (should-not (string-match "Group:" output)))))

(ert-deftest elinit-test-cli-json-includes-user-group ()
  "JSON output includes user and group fields."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple :user "bob" :group "wheel"))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (plan (elinit--build-plan (elinit--effective-programs)))
           (entry (car (elinit-plan-entries plan)))
           (info (elinit--cli-entry-info entry))
           (json-obj (elinit--cli-entry-to-json-obj info)))
      (should (equal "bob" (alist-get 'user json-obj)))
      (should (equal "wheel" (alist-get 'group json-obj))))))

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

(ert-deftest elinit-test-cli-entry-info-sandbox-disabled ()
  "CLI entry-info shows sandbox-enabled nil for non-sandbox entries."
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
         (entry (elinit--parse-entry '("sleep 300" :id "svc"))))
    (let ((info (elinit--cli-entry-info entry)))
      (should-not (alist-get 'sandbox-enabled info)))))

(ert-deftest elinit-test-cli-describe-human-sandbox-line ()
  "Human describe output includes sandbox line for sandbox entries."
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
                 '("sleep 300" :id "svc" :sandbox-profile service
                   :sandbox-network shared))))
    (let* ((info (elinit--cli-entry-info entry))
           (output (elinit--cli-describe-human info)))
      (should (string-match-p "Sandbox: service (network shared)" output)))))

(ert-deftest elinit-test-cli-describe-human-no-sandbox-line ()
  "Human describe output omits sandbox line for non-sandbox entries."
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
         (entry (elinit--parse-entry '("sleep 300" :id "svc"))))
    (let* ((info (elinit--cli-entry-info entry))
           (output (elinit--cli-describe-human info)))
      (should-not (string-match-p "Sandbox:" output)))))

(ert-deftest elinit-test-cli-json-sandbox-fields ()
  "JSON output includes sandbox fields."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (name) (when (equal name "bwrap") "/usr/bin/bwrap"))))
    (elinit-test-with-unit-files
        '(("sleep 300" :id "svc" :type simple :sandbox-profile strict))
      (let* ((elinit--processes (make-hash-table :test 'equal))
             (elinit--entry-state (make-hash-table :test 'equal))
             (plan (elinit--build-plan (elinit--effective-programs)))
             (entry (car (elinit-plan-entries plan)))
             (info (elinit--cli-entry-info entry))
             (json-obj (elinit--cli-entry-to-json-obj info)))
        (should (eq t (alist-get 'sandbox_enabled json-obj)))
        (should (equal "strict" (alist-get 'sandbox_profile json-obj)))
        (should (equal "isolated" (alist-get 'sandbox_network json-obj)))))))

(ert-deftest elinit-test-cli-json-sandbox-disabled ()
  "JSON output shows sandbox_enabled false for non-sandbox entries."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (plan (elinit--build-plan (elinit--effective-programs)))
           (entry (car (elinit-plan-entries plan)))
           (info (elinit--cli-entry-info entry))
           (json-obj (elinit--cli-entry-to-json-obj info)))
      (should (eq :json-false (alist-get 'sandbox_enabled json-obj))))))

(ert-deftest elinit-test-dashboard-detail-sandbox-indicator ()
  "Dashboard detail panel shows sandbox indicator for sandbox entries."
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
         (elinit--target-convergence (make-hash-table :test 'equal))
         (elinit--target-convergence-reasons (make-hash-table :test 'equal))
         (elinit--target-members (make-hash-table :test 'equal))
         (entry (elinit--parse-entry
                 '("sleep 300" :id "svc" :sandbox-profile desktop))))
    (elinit--describe-entry-detail "svc" entry)
    (unwind-protect
        (let ((output (with-current-buffer "*elinit-info*"
                        (buffer-string))))
          (should (string-match-p "Sandbox: desktop (network shared)" output)))
      (when (get-buffer "*elinit-info*")
        (kill-buffer "*elinit-info*")))))

(ert-deftest elinit-test-dashboard-detail-no-sandbox-for-plain ()
  "Dashboard detail panel omits sandbox indicator for plain entries."
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
         (elinit--target-convergence (make-hash-table :test 'equal))
         (elinit--target-convergence-reasons (make-hash-table :test 'equal))
         (elinit--target-members (make-hash-table :test 'equal))
         (entry (elinit--parse-entry '("sleep 300" :id "svc"))))
    (elinit--describe-entry-detail "svc" entry)
    (unwind-protect
        (let ((output (with-current-buffer "*elinit-info*"
                        (buffer-string))))
          (should-not (string-match-p "Sandbox:" output)))
      (when (get-buffer "*elinit-info*")
        (kill-buffer "*elinit-info*")))))

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
    ;; Non-root check: user-uid returns non-zero for test runner
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

(ert-deftest elinit-test-compute-entry-reason-generic-spawn-failure ()
  "Entry reason returns generic message when no specific reason stored."
  (let ((elinit--entry-state (make-hash-table :test 'equal))
        (elinit--spawn-failure-reason (make-hash-table :test 'equal))
        (elinit--mask-override (make-hash-table :test 'equal))
        (elinit--processes (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal)))
    (puthash "svc" 'failed-to-spawn elinit--entry-state)
    (should (equal "failed-to-spawn"
                   (elinit--compute-entry-reason "svc" 'simple)))))

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

;;; Phase 8: reload path preserves configured identity

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
          ;; Legacy entry with :user / :group at trailing positions.
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


(provide 'elinit-test-identity)
;;; elinit-test-identity.el ends here
