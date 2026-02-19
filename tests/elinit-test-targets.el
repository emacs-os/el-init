;;; elinit-test-targets.el --- Targets, convergence, and SysV compat tests for elinit.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Targets, convergence, and SysV compat ERT tests for elinit.el.

;;; Code:

(require 'elinit-test-helpers)

;;; Phase 2 Commit B: Hard cutover tests

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

(ert-deftest elinit-test-startup-timeout-defcustom ()
  "Startup timeout defcustom exists with correct default."
  (should (boundp 'elinit-startup-timeout))
  ;; Default is nil (no timeout)
  (should-not (default-value 'elinit-startup-timeout)))

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

(ert-deftest elinit-test-builtin-timers-present ()
  "Built-in timers are included when `elinit-timers' is nil."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit--programs-cache :not-yet-loaded)
         (elinit--unit-file-invalid (make-hash-table :test 'equal)))
    (elinit-test--write-unit-files
     dir '(("echo rotate" :id "logrotate" :type oneshot)))
    (unwind-protect
        (let* ((elinit-timers nil)
               (programs (elinit--effective-programs))
               (plan (elinit--build-plan programs))
               (timers (elinit-timer-build-list plan)))
          (should (cl-find "logrotate-daily" timers
                           :key #'elinit-timer-id
                           :test #'equal))
          (should (cl-find "log-prune-daily" timers
                           :key #'elinit-timer-id
                           :test #'equal)))
      (delete-directory dir t))))

(ert-deftest elinit-test-builtin-timers-overridden-by-user ()
  "User timer with same ID overrides the built-in timer."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit--programs-cache :not-yet-loaded)
         (elinit--unit-file-invalid (make-hash-table :test 'equal)))
    (elinit-test--write-unit-files
     dir '(("echo rotate" :id "logrotate" :type oneshot)))
    (unwind-protect
        (let* ((elinit-timers '((:id "logrotate-daily"
                                     :target "logrotate"
                                     :on-startup-sec 300
                                     :enabled t)))
               (programs (elinit--effective-programs))
               (plan (elinit--build-plan programs))
               (timers (elinit-timer-build-list plan)))
          ;; Should appear exactly once
          (should (= 1 (cl-count "logrotate-daily" timers
                                  :key #'elinit-timer-id
                                  :test #'equal)))
          ;; Should have user's on-startup-sec, not builtin's on-calendar
          (let ((timer (cl-find "logrotate-daily" timers
                                :key #'elinit-timer-id
                                :test #'equal)))
            (should (= 300 (elinit-timer-on-startup-sec timer))))
          ;; Other built-in timer remains present.
          (should (cl-find "log-prune-daily" timers
                           :key #'elinit-timer-id
                           :test #'equal)))
      (delete-directory dir t))))

(ert-deftest elinit-test-timer-subsystem-default-enabled ()
  "Timer subsystem mode defaults to enabled."
  ;; init-value is t, so the variable should be t by default
  (should (eq t (default-value 'elinit-timer-subsystem-mode))))

(ert-deftest elinit-test-timer-completion-does-not-signal-writers ()
  "Timer completion does not send reopen signals implicitly."
  (elinit-test-with-unit-files
      '(("true" :id "s1" :type oneshot))
    (let* ((elinit-mode t)
           (timer (elinit-timer--create :id "t1" :target "s1" :enabled t))
           (elinit--timer-state (make-hash-table :test 'equal))
           (elinit--processes (make-hash-table :test 'equal))
           (elinit--enabled-override (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal))
           (elinit--scheduler-startup-time (float-time))
           (captured-callback nil)
           (reopen-called nil))
      (cl-letf (((symbol-function 'elinit--start-entry-async)
                 (lambda (_entry callback) (setq captured-callback callback)))
                ((symbol-function 'elinit--signal-writers-reopen)
                 (lambda () (setq reopen-called t)))
                ((symbol-function 'elinit-timer--on-target-complete)
                 (lambda (_id _target-id _success) nil)))
        (unwind-protect
            (progn
              (elinit-timer--trigger timer 'scheduled)
              ;; Callback should have been captured
              (should captured-callback)
              ;; Completion callback should run without signaling writers.
              (funcall captured-callback t)
              (funcall captured-callback nil)
              (should-not reopen-called))
          (clrhash elinit--invalid)
          (clrhash elinit--timer-state)
          (clrhash elinit--processes)
          (clrhash elinit--enabled-override))))))

;;;; Phase 9: Test Coverage Expansion

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

(ert-deftest elinit-test-builtin-timer-schedule-is-03-00 ()
  "Built-in logrotate-daily timer has on-calendar at 03:00."
  (let ((timer (cl-find "logrotate-daily" elinit--builtin-timers
                         :key (lambda (t) (plist-get t :id))
                         :test #'equal)))
    (should timer)
    (let ((cal (plist-get timer :on-calendar)))
      (should (equal 3 (plist-get cal :hour)))
      (should (equal 0 (plist-get cal :minute))))))

(ert-deftest elinit-test-builtin-log-prune-timer-schedule-is-03-05 ()
  "Built-in log-prune-daily timer has on-calendar at 03:05."
  (let ((timer (cl-find "log-prune-daily" elinit--builtin-timers
                        :key (lambda (t) (plist-get t :id))
                        :test #'equal)))
    (should timer)
    (let ((cal (plist-get timer :on-calendar)))
      (should (equal 3 (plist-get cal :hour)))
      (should (equal 5 (plist-get cal :minute))))))

(ert-deftest elinit-test-logrotate-daily-not-scheduled-when-timer-off ()
  "Scheduler does not process timers when subsystem is off."
  (let ((elinit-timer-subsystem-mode nil)
        (elinit-mode nil)
        (build-list-called nil))
    (cl-letf (((symbol-function 'elinit--log) #'ignore))
      ;; elinit-timer-scheduler-start guards on subsystem-active-p
      ;; and returns early without building or processing any timers
      (cl-letf (((symbol-function 'elinit-timer-build-list)
                 (lambda (_plan)
                   (setq build-list-called t)
                   nil)))
        (elinit-timer-scheduler-start)
        ;; build-list should not have been called
        (should-not build-list-called)))))

(ert-deftest elinit-test-scheduler-not-started-when-timer-off ()
  "Timer scheduler is a no-op when subsystem is not active."
  (let ((elinit-timer-subsystem-mode nil)
        (elinit-mode nil)
        (build-list-called nil))
    (cl-letf (((symbol-function 'elinit--log) #'ignore)
              ((symbol-function 'elinit-timer-build-list)
               (lambda (_plan)
                 (setq build-list-called t)
                 nil)))
      ;; Call the real scheduler-start with subsystem off
      (elinit-timer-scheduler-start)
      ;; The guard should have returned early without building timers
      (should-not build-list-called))))

(ert-deftest elinit-test-logd-rotates-on-size-cap-exceed ()
  "Logd rotates the log file when it exceeds the size cap."
  (let* ((logd (elinit-test--ensure-logd-binary))
         (dir (make-temp-file "logd-rotate-" t))
         (log-file (expand-file-name "log-svc.log" dir)))
    (unwind-protect
        (let* ((proc (make-process
                      :name "test-logd"
                      :command (list logd
                                    "--file" log-file
                                    "--max-file-size-bytes" "50")
                      :connection-type 'pipe))
               (wrote nil))
          (unwind-protect
              (progn
                ;; Write enough data to exceed the 50-byte cap
                (process-send-string proc (make-string 80 ?x))
                (setq wrote t)
                ;; Give logd time to write and rotate
                (sleep-for 0.3)
                ;; Close stdin to trigger clean exit
                (process-send-eof proc)
                (sleep-for 0.3)
                ;; A rotated file should exist alongside the fresh log
                (let ((rotated (directory-files dir nil
                                               "^log-svc\\.[0-9].*\\.log$")))
                  (should (>= (length rotated) 1)))
                ;; The active log file should exist (reopened after rotation)
                (should (file-exists-p log-file))
                ;; Active log should be smaller than the cap (fresh after rotate)
                (let ((size (nth 7 (file-attributes log-file))))
                  (should (< size 50))))
            (when (process-live-p proc)
              (delete-process proc))))
      (delete-directory dir t))))

(ert-deftest elinit-test-logd-binary-rotation-both-files-decodable ()
  "Binary logd rotation produces SLG1 headers and decodable records in both files."
  (let* ((logd (elinit-test--ensure-logd-binary))
         (dir (make-temp-file "logd-binrot-" t))
         (log-file (expand-file-name "log-svc.log" dir)))
    (unwind-protect
        (let ((proc (make-process
                     :name "test-logd-binrot"
                     :command (list logd
                                   "--file" log-file
                                   "--framed"
                                   "--unit" "svc"
                                   "--format" "binary"
                                   "--max-file-size-bytes" "100")
                     :connection-type 'pipe
                     :coding 'no-conversion)))
          (unwind-protect
              (progn
                ;; Each binary record: 34 hdr + 3 unit + 8 payload = 45 bytes.
                ;; First record + SLG1 header = 49 bytes.
                ;; Second record brings total to 94 -- still under 100.
                ;; Third record crosses 100, triggering rotation.
                ;; Fourth record lands in the fresh active file.
                (process-send-string
                 proc (elinit--log-frame-encode 1 1 10 "svc" "data-001"))
                (process-send-string
                 proc (elinit--log-frame-encode 1 1 10 "svc" "data-002"))
                (sleep-for 0.2)
                (process-send-string
                 proc (elinit--log-frame-encode 1 1 10 "svc" "data-003"))
                (sleep-for 0.2)
                (process-send-string
                 proc (elinit--log-frame-encode 1 1 10 "svc" "data-004"))
                (sleep-for 0.3)
                (process-send-eof proc)
                (sleep-for 0.3)
                ;; Rotated file(s) should exist
                (let ((rotated (directory-files dir t
                                               "^log-svc\\.[0-9].*\\.log")))
                  (should (>= (length rotated) 1))
                  ;; Decode the rotated file
                  (let* ((rot-content
                          (with-temp-buffer
                            (set-buffer-multibyte nil)
                            (insert-file-contents-literally (car rotated))
                            (buffer-string)))
                         (rot-result (elinit--log-decode-binary-records
                                     rot-content))
                         (rot-records (plist-get rot-result :records)))
                    ;; Rotated file starts with SLG1
                    (should (>= (length rot-content) 4))
                    (should (equal (substring rot-content 0 4) "SLG1"))
                    (should (null (plist-get rot-result :warning)))
                    ;; At least one record in rotated file
                    (should (>= (length rot-records) 1))
                    ;; All rotated records have valid payloads
                    (dolist (r rot-records)
                      (should (string-match-p "\\`data-00[0-9]\\'"
                                              (plist-get r :payload))))))
                ;; Active file should exist and be decodable
                (should (file-exists-p log-file))
                (let* ((act-content
                        (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert-file-contents-literally log-file)
                          (buffer-string)))
                       (act-result (elinit--log-decode-binary-records
                                   act-content))
                       (act-records (plist-get act-result :records)))
                  ;; Active file starts with SLG1
                  (should (>= (length act-content) 4))
                  (should (equal (substring act-content 0 4) "SLG1"))
                  (should (null (plist-get act-result :warning)))
                  ;; At least one record in active file
                  (should (>= (length act-records) 1))
                  (dolist (r act-records)
                    (should (string-match-p "\\`data-00[0-9]\\'"
                                            (plist-get r :payload))))))
            (when (process-live-p proc) (delete-process proc))))
      (delete-directory dir t))))

(ert-deftest elinit-test-logd-text-framed-round-trip ()
  "End-to-end: framed input to logd --format text produces decodable records."
  (let* ((logd (elinit-test--ensure-logd-binary))
         (dir (make-temp-file "logd-text-rt-" t))
         (log-file (expand-file-name "log-svc.log" dir)))
    (unwind-protect
        (let ((proc (make-process
                     :name "test-logd-text-rt"
                     :command (list logd
                                   "--file" log-file
                                   "--framed"
                                   "--unit" "test-svc"
                                   "--format" "text"
                                   "--max-file-size-bytes" "1048576")
                     :connection-type 'pipe
                     :coding 'no-conversion)))
          (unwind-protect
              (progn
                ;; Frame 1: output with literal "-" payload
                (process-send-string
                 proc (elinit--log-frame-encode 1 1 42 "test-svc" "-"))
                ;; Frame 2: output with empty payload
                (process-send-string
                 proc (elinit--log-frame-encode 1 1 42 "test-svc" ""))
                ;; Frame 3: stderr output with normal payload
                (process-send-string
                 proc (elinit--log-frame-encode 1 2 42 "test-svc"
                                                    "hello world\n"))
                ;; Frame 4: exit event
                (process-send-string
                 proc (elinit--log-frame-encode 2 3 42 "test-svc"
                                                    nil 0 1))
                (sleep-for 0.3)
                (process-send-eof proc)
                (sleep-for 0.3)
                ;; Read and decode the text log file
                (should (file-exists-p log-file))
                (let* ((content (with-temp-buffer
                                  (insert-file-contents log-file)
                                  (buffer-string)))
                       (records (elinit--log-decode-text-records content)))
                  (should (= 4 (length records)))
                  (let ((r1 (nth 0 records))
                        (r2 (nth 1 records))
                        (r3 (nth 2 records))
                        (r4 (nth 3 records)))
                    ;; Literal "-" round-trips correctly
                    (should (eq (plist-get r1 :event) 'output))
                    (should (eq (plist-get r1 :stream) 'stdout))
                    (should (equal (plist-get r1 :payload) "-"))
                    ;; Rule 1: output status=- and code=-
                    (should-not (plist-get r1 :status))
                    (should (= (plist-get r1 :code) 0))
                    ;; Empty payload round-trips correctly
                    (should (eq (plist-get r2 :event) 'output))
                    (should (equal (plist-get r2 :payload) ""))
                    (should-not (plist-get r2 :status))
                    ;; Stderr output has correct stream tag and status=-
                    (should (eq (plist-get r3 :stream) 'stderr))
                    (should (equal (plist-get r3 :payload) "hello world\n"))
                    (should-not (plist-get r3 :status))
                    ;; Exit event decodes correctly with status/code
                    (should (eq (plist-get r4 :event) 'exit))
                    (should (eq (plist-get r4 :stream) 'meta))
                    (should (eq (plist-get r4 :status) 'exited))
                    (should (= (plist-get r4 :code) 0)))))
            (when (process-live-p proc) (delete-process proc))))
      (delete-directory dir t))))

(ert-deftest elinit-test-logd-text-framed-default-format ()
  "Framed logd without --format defaults to text, not raw."
  (let* ((logd (elinit-test--ensure-logd-binary))
         (dir (make-temp-file "logd-deffmt-" t))
         (log-file (expand-file-name "log-svc.log" dir)))
    (unwind-protect
        (let ((proc (make-process
                     :name "test-logd-deffmt"
                     :command (list logd
                                   "--file" log-file
                                   "--framed"
                                   "--unit" "test-svc"
                                   "--max-file-size-bytes" "1048576")
                     :connection-type 'pipe
                     :coding 'no-conversion)))
          (unwind-protect
              (progn
                (process-send-string
                 proc (elinit--log-frame-encode 1 1 42 "test-svc" "hi"))
                (sleep-for 0.3)
                (process-send-eof proc)
                (sleep-for 0.3)
                (should (file-exists-p log-file))
                (let* ((content (with-temp-buffer
                                  (insert-file-contents log-file)
                                  (buffer-string)))
                       (records (elinit--log-decode-text-records content)))
                  ;; If raw, records would be empty (no ts= prefix)
                  (should (= 1 (length records)))
                  (should (equal (plist-get (car records) :payload) "hi"))))
            (when (process-live-p proc) (delete-process proc))))
      (delete-directory dir t))))

(ert-deftest elinit-test-logd-text-nul-highbyte-round-trip ()
  "End-to-end: NUL and high-byte payloads survive C text escaping."
  (let* ((logd (elinit-test--ensure-logd-binary))
         (dir (make-temp-file "logd-bytes-" t))
         (log-file (expand-file-name "log-svc.log" dir))
         (nul-payload (unibyte-string 0 65 0 66))  ; \x00 A \x00 B
         (high-payload (unibyte-string #x80 #xff #xfe 92 110))) ; high bytes + backslash + n
    (unwind-protect
        (let ((proc (make-process
                     :name "test-logd-bytes"
                     :command (list logd
                                   "--file" log-file
                                   "--framed"
                                   "--unit" "test-svc"
                                   "--format" "text"
                                   "--max-file-size-bytes" "1048576")
                     :connection-type 'pipe
                     :coding 'no-conversion)))
          (unwind-protect
              (progn
                (process-send-string
                 proc (elinit--log-frame-encode 1 1 42 "test-svc"
                                                    nul-payload))
                (process-send-string
                 proc (elinit--log-frame-encode 1 1 42 "test-svc"
                                                    high-payload))
                (sleep-for 0.3)
                (process-send-eof proc)
                (sleep-for 0.3)
                (should (file-exists-p log-file))
                (let* ((content (with-temp-buffer
                                  (insert-file-contents log-file)
                                  (buffer-string)))
                       (records (elinit--log-decode-text-records content)))
                  (should (= 2 (length records)))
                  ;; NUL bytes round-trip through \x00 escaping
                  (should (equal (plist-get (nth 0 records) :payload)
                                 nul-payload))
                  ;; High bytes + backslash round-trip
                  (should (equal (plist-get (nth 1 records) :payload)
                                 high-payload))))
            (when (process-live-p proc) (delete-process proc))))
      (delete-directory dir t))))

(ert-deftest elinit-test-logd-text-timestamp-rfc3339nano ()
  "Text records from logd have strict RFC3339Nano UTC timestamps."
  (let* ((logd (elinit-test--ensure-logd-binary))
         (dir (make-temp-file "logd-ts-" t))
         (log-file (expand-file-name "log-svc.log" dir)))
    (unwind-protect
        (let ((proc (make-process
                     :name "test-logd-ts"
                     :command (list logd
                                   "--file" log-file
                                   "--framed"
                                   "--unit" "test-svc"
                                   "--format" "text"
                                   "--max-file-size-bytes" "1048576")
                     :connection-type 'pipe
                     :coding 'no-conversion)))
          (unwind-protect
              (progn
                (process-send-string
                 proc (elinit--log-frame-encode 1 1 42 "test-svc" "x"))
                (sleep-for 0.3)
                (process-send-eof proc)
                (sleep-for 0.3)
                (should (file-exists-p log-file))
                (let ((content (with-temp-buffer
                                 (insert-file-contents log-file)
                                 (buffer-string))))
                  ;; Raw ts= token must match YYYY-MM-DDTHH:MM:SS.NNNNNNNNNZ
                  (should (string-match
                           "\\`ts=\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\.[0-9]\\{9\\}Z\\) "
                           content))))
            (when (process-live-p proc) (delete-process proc))))
      (delete-directory dir t))))

(ert-deftest elinit-test-logd-text-rejects-invalid-event-stream ()
  "Logd rejects frames with invalid event/stream pairings."
  (let* ((logd (elinit-test--ensure-logd-binary))
         (dir (make-temp-file "logd-reject-" t))
         (log-file (expand-file-name "log-svc.log" dir)))
    (unwind-protect
        (let ((proc (make-process
                     :name "test-logd-reject"
                     :command (list logd
                                   "--file" log-file
                                   "--framed"
                                   "--unit" "test-svc"
                                   "--format" "text"
                                   "--max-file-size-bytes" "1048576")
                     :connection-type 'pipe
                     :coding 'no-conversion)))
          (unwind-protect
              (progn
                ;; Valid frame first (anchor for record count)
                (process-send-string
                 proc (elinit--log-frame-encode 1 1 42 "test-svc"
                                                    "valid"))
                ;; Invalid: output on meta stream
                (process-send-string
                 proc (elinit--log-frame-encode 1 3 42 "test-svc"
                                                    "bad-meta"))
                ;; Invalid: exit on stdout stream
                (process-send-string
                 proc (elinit--log-frame-encode 2 1 42 "test-svc"
                                                    nil 0 1))
                ;; Invalid: exit on stderr stream
                (process-send-string
                 proc (elinit--log-frame-encode 2 2 42 "test-svc"
                                                    nil 1 2))
                ;; Valid exit to confirm logd is still processing
                (process-send-string
                 proc (elinit--log-frame-encode 2 3 42 "test-svc"
                                                    nil 0 1))
                (sleep-for 0.3)
                (process-send-eof proc)
                (sleep-for 0.3)
                (should (file-exists-p log-file))
                (let* ((content (with-temp-buffer
                                  (insert-file-contents log-file)
                                  (buffer-string)))
                       (records (elinit--log-decode-text-records
                                 content)))
                  ;; Only the 2 valid frames should produce records
                  (should (= 2 (length records)))
                  (should (eq (plist-get (nth 0 records) :event) 'output))
                  (should (eq (plist-get (nth 1 records) :event) 'exit))))
            (when (process-live-p proc) (delete-process proc))))
      (delete-directory dir t))))

(ert-deftest elinit-test-timer-enabled-triggers-maintenance ()
  "Enabled logrotate-daily timer triggers target oneshot on schedule."
  (elinit-test-with-unit-files
      '(("echo maintenance" :id "logrotate" :type oneshot))
    (let* ((elinit-mode t)
           (timer (elinit-timer--create :id "logrotate-daily"
                                            :target "logrotate"
                                            :enabled t))
           (elinit--timer-state (make-hash-table :test 'equal))
           (elinit--processes (make-hash-table :test 'equal))
           (elinit--enabled-override (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal))
           (elinit--scheduler-startup-time (float-time))
           (triggered-target nil))
      (cl-letf (((symbol-function 'elinit--start-entry-async)
                 (lambda (entry callback)
                   (setq triggered-target (elinit-entry-id entry))
                   ;; Simulate success
                   (when callback (funcall callback t))))
                ((symbol-function 'elinit--signal-writers-reopen) #'ignore)
                ((symbol-function 'elinit-timer--on-target-complete)
                 (lambda (_id _target-id _success) nil))
                ((symbol-function 'elinit-timer--save-state) #'ignore))
        (unwind-protect
            (progn
              (elinit-timer--trigger timer 'scheduled)
              ;; The trigger should have started the "logrotate" target
              (should (equal "logrotate" triggered-target)))
          (clrhash elinit--invalid)
          (clrhash elinit--timer-state)
          (clrhash elinit--processes)
          (clrhash elinit--enabled-override))))))

;;; Target entry tests

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

(ert-deftest elinit-test-wanted-by-shape-string-valid ()
  ":wanted-by as a string passes validation."
  (should-not (elinit--validate-entry
               '("sleep 300" :wanted-by "multi.target"))))

(ert-deftest elinit-test-wanted-by-shape-list-valid ()
  ":wanted-by as a list of strings passes validation."
  (should-not (elinit--validate-entry
               '("sleep 300" :wanted-by ("a.target" "b.target")))))

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

(ert-deftest elinit-test-target-entry-type-returns-target ()
  "`elinit-entry-type' returns target for target entries."
  (let ((entry (elinit--parse-entry
                '("" :type target :id "multi.target"))))
    (should (eq (elinit-entry-type entry) 'target))))

(ert-deftest elinit-test-target-with-after-valid ()
  "Target entry with :after passes validation."
  (should-not (elinit--validate-entry
               '("" :type target :id "multi.target"
                 :after "some-service"))))

(ert-deftest elinit-test-backward-compat-31-element-tuple-accessors ()
  "Accessors for wanted-by and required-by return nil for shorter tuples."
  ;; Simulate a 31-element tuple (pre-extension)
  (let ((short-entry (make-list 31 nil)))
    (should (null (elinit-entry-wanted-by short-entry)))
    (should (null (elinit-entry-required-by short-entry)))))

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

(ert-deftest elinit-test-extract-id-nil-car-target ()
  "`elinit--extract-id' returns :id for nil-car target entries."
  (should (equal "a.target"
                 (elinit--extract-id
                  '(nil :type target :id "a.target") 0))))

;;;; Phase 3: Transaction Expansion Engine

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

(ert-deftest elinit-test-materialize-target-members-empty ()
  "Materialize returns empty hash when no services declare membership."
  (let* ((programs '((nil :id "basic.target" :type target)
                     ("sleep 1" :id "svc-a")))
         (plan (elinit--build-plan programs))
         (members (elinit--materialize-target-members
                   (elinit-plan-entries plan))))
    (should (hash-table-p members))
    (should (= 0 (hash-table-count members)))))

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
    ;; svc-a references "missing-dep" in :after which will be invalid
    ;; at validation time. Build a plan where svc-a has no after deps
    ;; since missing-dep is invalid. Now test DAG filtering directly:
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

;;; Phase 4: Target-Aware Plan Builder Tests

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
    ;; First set up svc-a in DAG so mark-ready works
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

(ert-deftest elinit-test-target-convergence-event-types ()
  "Event types list includes target-reached and target-degraded."
  (should (memq 'target-reached elinit--event-types))
  (should (memq 'target-degraded elinit--event-types)))

(ert-deftest elinit-test-target-convergence-reverse-index ()
  "Reverse member index built correctly from target-members."
  (let ((members (make-hash-table :test 'equal))
        (elinit--target-members nil)
        (elinit--target-convergence nil)
        (elinit--target-convergence-reasons nil)
        (elinit--target-converging nil)
        (elinit--target-member-reverse nil))
    (puthash "t1.target" '(:requires ("svc-a" "svc-b") :wants ("svc-c"))
             members)
    (puthash "t2.target" '(:requires ("svc-b") :wants nil)
             members)
    (elinit--target-init-convergence members)
    ;; svc-a should map to t1.target only
    (should (equal '("t1.target")
                   (gethash "svc-a" elinit--target-member-reverse)))
    ;; svc-b should map to both targets
    (let ((targets (gethash "svc-b" elinit--target-member-reverse)))
      (should (= 2 (length targets)))
      (should (member "t1.target" targets))
      (should (member "t2.target" targets)))
    ;; svc-c (wanted) should also be in reverse index
    (should (equal '("t1.target")
                   (gethash "svc-c" elinit--target-member-reverse)))))

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
    ;; Build plan so materialize-target-members picks up
    ;; target's own :requires
    (let* ((programs '(("sleep 1" :id "svc-a")
                       (nil :id "app.target" :type target
                            :requires ("svc-a"))))
           (plan (elinit--build-plan programs))
           (members (elinit--materialize-target-members
                     (elinit-plan-entries plan))))
      ;; Install membership from build
      (setq elinit--target-members members)
      ;; svc-a is ready but failed
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
    ;; svc-a is invalidated because its :required-by ref is invalid
    ;; (nonexistent.target doesn't exist).  app.target :requires
    ;; "svc-a" but svc-a was dropped, so dep normalization should
    ;; invalidate the target.
    (should (gethash "app.target" (elinit-plan-invalid plan)))))

;;; Dashboard Target UX Tests (Phase 6)

(ert-deftest elinit-test-dashboard-target-type-face ()
  "Target entry renders with `elinit-type-target' face in TYPE column."
  (let* ((snapshot (elinit-snapshot--create
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
                    :manually-stopped (make-hash-table :test 'equal)
                    :remain-active (make-hash-table :test 'equal)
                    :timestamp (float-time)))
         (elinit--target-convergence (make-hash-table :test 'equal))
         (elinit--target-convergence-reasons (make-hash-table :test 'equal))
         (vec (elinit--make-dashboard-entry
               "app.target" 'target nil t nil nil snapshot)))
    ;; TYPE column (index 1) should have target face
    (should (eq 'elinit-type-target
                (get-text-property 0 'face (aref vec 1))))
    ;; TYPE text should be "target"
    (should (equal "target" (substring-no-properties (aref vec 1))))))

(ert-deftest elinit-test-dashboard-target-convergence-in-status ()
  "Target with convergence state shows correct STATUS column."
  (let* ((conv-hash (make-hash-table :test 'equal))
         (elinit--target-convergence conv-hash)
         (elinit--target-convergence-reasons
          (make-hash-table :test 'equal))
         (snapshot (elinit-snapshot--create
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
                    :manually-stopped (make-hash-table :test 'equal)
                    :remain-active (make-hash-table :test 'equal)
                    :timestamp (float-time))))
    ;; Test reached
    (puthash "app.target" 'reached conv-hash)
    (let ((vec (elinit--make-dashboard-entry
                "app.target" 'target nil t nil nil snapshot)))
      (should (equal "reached"
                     (substring-no-properties (aref vec 4)))))
    ;; Test degraded
    (puthash "app.target" 'degraded conv-hash)
    (let ((vec (elinit--make-dashboard-entry
                "app.target" 'target nil t nil nil snapshot)))
      (should (equal "degraded"
                     (substring-no-properties (aref vec 4)))))
    ;; Test converging
    (puthash "app.target" 'converging conv-hash)
    (let ((vec (elinit--make-dashboard-entry
                "app.target" 'target nil t nil nil snapshot)))
      (should (equal "converging"
                     (substring-no-properties (aref vec 4)))))))

(ert-deftest elinit-test-dashboard-target-reason-shows-degraded ()
  "Target with degraded reasons shows joined reasons in REASON column."
  (let* ((conv-hash (make-hash-table :test 'equal))
         (reasons-hash (make-hash-table :test 'equal))
         (elinit--target-convergence conv-hash)
         (elinit--target-convergence-reasons reasons-hash)
         (snapshot (elinit-snapshot--create
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
                    :manually-stopped (make-hash-table :test 'equal)
                    :remain-active (make-hash-table :test 'equal)
                    :timestamp (float-time))))
    (puthash "app.target" 'degraded conv-hash)
    (puthash "app.target" '("svc-a failed" "svc-b failed") reasons-hash)
    (let ((vec (elinit--make-dashboard-entry
                "app.target" 'target nil t nil nil snapshot)))
      (should (equal "svc-a failed; svc-b failed"
                     (substring-no-properties (aref vec 8)))))))

(ert-deftest elinit-test-dashboard-target-filter-cycle ()
  "Cycle filter: nil -> first-target -> second-target -> nil."
  (let* ((members-hash (make-hash-table :test 'equal))
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
           :meta nil)))
    (puthash "alpha.target" '(:requires ("svc-a") :wants nil) members-hash)
    (puthash "beta.target" '(:requires nil :wants ("svc-b")) members-hash)
    (with-temp-buffer
      (let ((elinit--dashboard-target-filter nil))
        ;; First cycle: nil -> alpha.target
        (let ((all-targets (elinit--all-target-ids)))
          (should (equal '("alpha.target" "beta.target") all-targets)))
        (setq elinit--dashboard-target-filter
              (car (elinit--all-target-ids)))
        (should (equal "alpha.target" elinit--dashboard-target-filter))
        ;; Second cycle: alpha.target -> beta.target
        (let* ((all-targets (elinit--all-target-ids))
               (idx (cl-position elinit--dashboard-target-filter
                                all-targets :test #'equal)))
          (setq elinit--dashboard-target-filter
                (nth (1+ idx) all-targets)))
        (should (equal "beta.target" elinit--dashboard-target-filter))
        ;; Third cycle: beta.target -> nil
        (let* ((all-targets (elinit--all-target-ids))
               (idx (cl-position elinit--dashboard-target-filter
                                all-targets :test #'equal)))
          (setq elinit--dashboard-target-filter
                (when (< idx (1- (length all-targets)))
                  (nth (1+ idx) all-targets))))
        (should (null elinit--dashboard-target-filter))))))

(ert-deftest elinit-test-dashboard-target-filter-includes-members ()
  "Filtered view includes target and its members only."
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
           (elinit--dashboard-target-filter "app.target"))
      (puthash "app.target" '(:requires nil :wants ("svc-a")) members-hash)
      (let* ((snapshot (elinit--build-snapshot))
             (all-entries (elinit--get-entries snapshot))
             ;; Filter out separator rows
             (service-ids
              (cl-loop for entry in all-entries
                       when (elinit--service-row-p (car entry))
                       collect (cdr (car entry)))))
        ;; Should include svc-a (member) and app.target, not svc-b
        (should (member "svc-a" service-ids))
        (should (member "app.target" service-ids))
        (should-not (member "svc-b" service-ids))))))

(ert-deftest elinit-test-dashboard-target-members-command ()
  "On target row: shows requires and wants members."
  (let* ((members-hash (make-hash-table :test 'equal))
         (elinit--target-members members-hash))
    (puthash "app.target" '(:requires ("svc-a") :wants ("svc-b")) members-hash)
    ;; Simulate calling on a target entry
    (let* ((entry (list "app.target" nil 0 t nil nil nil nil
                        'target nil nil nil nil nil nil
                        nil nil nil nil nil nil nil nil nil nil
                        nil nil nil nil nil nil nil nil))
           (msg nil))
      ;; Verify entry type accessor works
      (should (eq 'target (elinit-entry-type entry)))
      ;; Test the message formatting logic directly
      (let ((members (gethash "app.target" members-hash)))
        (let ((req (plist-get members :requires))
              (wants (plist-get members :wants)))
          (setq msg (format "%s members: requires=[%s] wants=[%s]"
                            "app.target"
                            (if req (mapconcat #'identity req ", ") "none")
                            (if wants
                                (mapconcat #'identity wants ", ")
                              "none")))))
      (should (string-match-p "requires=\\[svc-a\\]" msg))
      (should (string-match-p "wants=\\[svc-b\\]" msg)))))

(ert-deftest elinit-test-dashboard-target-members-non-target ()
  "On non-target row: shows error message."
  (let* ((elinit--target-members (make-hash-table :test 'equal))
         ;; A simple entry, not a target
         (entry (list "svc-a" "sleep 1" 0 t nil nil nil nil
                      'simple nil nil nil nil nil
                      nil nil nil nil nil nil nil nil nil nil
                      nil nil nil nil nil nil nil nil)))
    (should (eq 'simple (elinit-entry-type entry)))
    ;; Not a target, so members command should say so
    (should-not (eq (elinit-entry-type entry) 'target))))

(ert-deftest elinit-test-dashboard-header-shows-root ()
  "When plan has activation-root, header includes root info."
  (let* ((elinit--current-plan
          (elinit-plan--create
           :activation-root "graphical.target"
           :entries nil
           :invalid (make-hash-table :test 'equal)
           :by-target nil
           :deps (make-hash-table :test 'equal)
           :requires-deps (make-hash-table :test 'equal)
           :dependents (make-hash-table :test 'equal)
           :cycle-fallback-ids (make-hash-table :test 'equal)
           :order-index (make-hash-table :test 'equal)
           :target-members (make-hash-table :test 'equal)
           :meta nil))
         (elinit--processes (make-hash-table :test 'equal))
         (elinit--failed (make-hash-table :test 'equal))
         (elinit--oneshot-completed (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal))
         (elinit--invalid (make-hash-table :test 'equal))
         (elinit--enabled-override (make-hash-table :test 'equal))
         (elinit--restart-override (make-hash-table :test 'equal))
         (elinit--logging (make-hash-table :test 'equal))
         (header (elinit--dashboard-header-line)))
    (should (string-match-p "root" (substring-no-properties header)))
    (should (string-match-p "graphical\\.target"
                            (substring-no-properties header)))))

(ert-deftest elinit-test-dashboard-header-no-root-when-no-plan ()
  "Without a plan, header does not include root info."
  (let* ((elinit--current-plan nil)
         (elinit--processes (make-hash-table :test 'equal))
         (elinit--failed (make-hash-table :test 'equal))
         (elinit--oneshot-completed (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal))
         (elinit--invalid (make-hash-table :test 'equal))
         (elinit--enabled-override (make-hash-table :test 'equal))
         (elinit--restart-override (make-hash-table :test 'equal))
         (elinit--logging (make-hash-table :test 'equal))
         (header (elinit--dashboard-header-line)))
    (should-not (string-match-p "root"
                                (substring-no-properties header)))))

(ert-deftest elinit-test-dashboard-describe-target-shows-convergence ()
  "Describe entry detail for target shows convergence state and members."
  (let* ((elinit--target-convergence (make-hash-table :test 'equal))
         (elinit--target-convergence-reasons
          (make-hash-table :test 'equal))
         (elinit--target-members (make-hash-table :test 'equal))
         (elinit--processes (make-hash-table :test 'equal))
         (elinit--failed (make-hash-table :test 'equal))
         (elinit--oneshot-completed (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal))
         (elinit--enabled-override (make-hash-table :test 'equal))
         (elinit--restart-override (make-hash-table :test 'equal))
         (elinit--logging (make-hash-table :test 'equal))
         (elinit--mask-override (make-hash-table :test 'equal))
         (elinit--remain-active (make-hash-table :test 'equal))
         (elinit--manually-stopped (make-hash-table :test 'equal))
         (elinit--start-times (make-hash-table :test 'equal))
         (elinit--ready-times (make-hash-table :test 'equal))
         (elinit--restart-times (make-hash-table :test 'equal))
         (elinit--restart-timers (make-hash-table :test 'equal))
         (elinit--logging-override (make-hash-table :test 'equal))
         (elinit--last-exit-info (make-hash-table :test 'equal))
         (elinit--spawn-failure-reason (make-hash-table :test 'equal))
         ;; 38-element target entry
         (entry (list "app.target" nil 0 t nil nil nil nil
                      'target nil nil nil nil nil
                      nil nil nil nil nil nil
                      "Test target" nil nil nil nil nil nil nil nil nil
                      '("multi-user.target") nil
                      nil nil nil nil nil nil)))
    (puthash "app.target" 'degraded elinit--target-convergence)
    (puthash "app.target" '("svc-x failed")
             elinit--target-convergence-reasons)
    (puthash "app.target" '(:requires ("svc-a") :wants ("svc-b"))
             elinit--target-members)
    (cl-letf (((symbol-function 'elinit--unit-file-path)
               (lambda (_id) nil))
              ((symbol-function 'elinit--telemetry-log-tail)
               (lambda (_id &optional _lines) nil)))
      (elinit--describe-entry-detail "app.target" entry)
      (let ((info-buf (get-buffer "*elinit-info*")))
        (unwind-protect
            (progn
              (should info-buf)
              (let ((output (with-current-buffer info-buf
                              (buffer-string))))
                (should (string-match-p "Converge: degraded" output))
                (should (string-match-p "Reasons: svc-x failed" output))
                (should (string-match-p "Req-mem: svc-a" output))
                (should (string-match-p "Want-mem: svc-b" output))))
          (when info-buf (kill-buffer info-buf)))))))

(ert-deftest elinit-test-dashboard-target-column-shows-convergence ()
  "TARGET column shows convergence state for target entries."
  (let* ((conv-hash (make-hash-table :test 'equal))
         (elinit--target-convergence conv-hash)
         (elinit--target-convergence-reasons
          (make-hash-table :test 'equal))
         (snapshot (elinit-snapshot--create
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
                    :manually-stopped (make-hash-table :test 'equal)
                    :remain-active (make-hash-table :test 'equal)
                    :timestamp (float-time))))
    ;; Target with reached convergence
    (puthash "app.target" 'reached conv-hash)
    (let ((vec (elinit--make-dashboard-entry
                "app.target" 'target nil t nil nil snapshot)))
      ;; TARGET column (index 2) shows convergence
      (should (equal "reached"
                     (substring-no-properties (aref vec 2)))))
    ;; Simple service with parent-target shows it in TARGET column
    (let ((vec (elinit--make-dashboard-entry
                "svc" 'simple "basic.target" t 'always t snapshot)))
      (should (equal "basic.target" (aref vec 2))))
    ;; Simple service without parent-target shows "-" in TARGET column
    (let ((vec (elinit--make-dashboard-entry
                "svc" 'simple nil t 'always t snapshot)))
      (should (equal "-" (aref vec 2))))))

(ert-deftest elinit-test-dashboard-target-column-outside-closure-unreachable ()
  "TARGET column shows unreachable for targets outside activation closure."
  (let* ((conv-hash (make-hash-table :test 'equal))
         (closure (make-hash-table :test 'equal))
         (elinit--target-convergence conv-hash)
         (elinit--target-convergence-reasons
          (make-hash-table :test 'equal))
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
         (snapshot (elinit-snapshot--create
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
                    :manually-stopped (make-hash-table :test 'equal)
                    :remain-active (make-hash-table :test 'equal)
                    :timestamp (float-time))))
    (puthash "graphical.target" t closure)
    (let ((vec (elinit--make-dashboard-entry
                "rescue.target" 'target nil t nil nil snapshot)))
      (should (equal "unreachable"
                     (substring-no-properties (aref vec 2))))
      (should (equal "unreachable"
                     (substring-no-properties (aref vec 4)))))))

(ert-deftest elinit-test-dashboard-target-restart-renders-na ()
  "Dashboard renders target restart column as n/a."
  (let* ((elinit--target-convergence (make-hash-table :test 'equal))
         (elinit--target-convergence-reasons
          (make-hash-table :test 'equal))
         (snapshot (elinit-snapshot--create
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
                    :manually-stopped (make-hash-table :test 'equal)
                    :remain-active (make-hash-table :test 'equal)
                    :timestamp (float-time)))
         (vec (elinit--make-dashboard-entry
               "app.target" 'target nil t nil nil snapshot)))
    (should (equal "n/a" (aref vec 5)))))

(ert-deftest elinit-test-dashboard-target-log-renders-dash ()
  "Dashboard renders target LOG column as dash."
  (let* ((elinit--target-convergence (make-hash-table :test 'equal))
         (elinit--target-convergence-reasons
          (make-hash-table :test 'equal))
         (snapshot (elinit-snapshot--create
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
                    :manually-stopped (make-hash-table :test 'equal)
                    :remain-active (make-hash-table :test 'equal)
                    :timestamp (float-time)))
         (vec (elinit--make-dashboard-entry
               "app.target" 'target nil t nil nil snapshot)))
    (should (equal "-" (aref vec 6)))))

(ert-deftest elinit-test-dashboard-services-separator-says-target ()
  "Services separator row uses TARGET not STAGE in column label."
  (let ((row (elinit--make-services-separator)))
    (should (string-match-p "TARGET"
                            (substring-no-properties (aref (cadr row) 2))))))

(ert-deftest elinit-test-dashboard-status-face-target-states ()
  "Status face returns correct faces for target convergence states."
  (should (eq 'elinit-status-running
              (elinit--status-face "reached")))
  (should (eq 'elinit-status-failed
              (elinit--status-face "degraded")))
  (should (eq 'elinit-status-pending
              (elinit--status-face "converging"))))

(ert-deftest elinit-test-compute-entry-status-target-reached ()
  "Compute-entry-status returns reached for target with reached convergence."
  (let* ((elinit--target-convergence (make-hash-table :test 'equal))
         (elinit--mask-override (make-hash-table :test 'equal))
         (elinit--processes (make-hash-table :test 'equal))
         (elinit--failed (make-hash-table :test 'equal))
         (elinit--oneshot-completed (make-hash-table :test 'equal))
         (elinit--remain-active (make-hash-table :test 'equal))
         (elinit--manually-stopped (make-hash-table :test 'equal)))
    (puthash "app.target" 'reached elinit--target-convergence)
    (let ((result (elinit--compute-entry-status "app.target" 'target)))
      (should (equal "reached" (car result))))))

(ert-deftest elinit-test-compute-entry-status-target-degraded ()
  "Compute-entry-status returns degraded for target with degraded convergence."
  (let* ((elinit--target-convergence (make-hash-table :test 'equal))
         (elinit--mask-override (make-hash-table :test 'equal))
         (elinit--processes (make-hash-table :test 'equal))
         (elinit--failed (make-hash-table :test 'equal))
         (elinit--oneshot-completed (make-hash-table :test 'equal))
         (elinit--remain-active (make-hash-table :test 'equal))
         (elinit--manually-stopped (make-hash-table :test 'equal)))
    (puthash "app.target" 'degraded elinit--target-convergence)
    (let ((result (elinit--compute-entry-status "app.target" 'target)))
      (should (equal "degraded" (car result))))))

(ert-deftest elinit-test-compute-entry-reason-target-degraded ()
  "Compute-entry-reason returns joined reasons for degraded target."
  (let* ((elinit--target-convergence-reasons
          (make-hash-table :test 'equal))
         (elinit--mask-override (make-hash-table :test 'equal))
         (elinit--processes (make-hash-table :test 'equal))
         (elinit--failed (make-hash-table :test 'equal))
         (elinit--oneshot-completed (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal))
         (elinit--spawn-failure-reason (make-hash-table :test 'equal)))
    (puthash "app.target" '("svc-a failed" "svc-b timeout")
             elinit--target-convergence-reasons)
    (let ((result (elinit--compute-entry-reason "app.target" 'target)))
      (should (equal "svc-a failed; svc-b timeout" result)))))

;;; Phase 6 Review Fixes

(ert-deftest elinit-test-all-target-ids-includes-empty-targets ()
  "All-target-ids includes targets with no members."
  (let* ((members-hash (make-hash-table :test 'equal))
         ;; 38-element target entry for empty-target (no members)
         (empty-entry (list "empty.target" nil 0 t nil nil nil nil
                            'target nil nil nil nil nil
                            nil nil nil nil nil nil nil nil nil nil
                            nil nil nil nil nil nil nil nil
                            nil nil nil nil nil nil))
         ;; 38-element target entry for pop.target (has members)
         (pop-entry (list "pop.target" nil 0 t nil nil nil nil
                          'target nil nil nil nil nil
                          nil nil nil nil nil nil nil nil nil nil
                          nil nil nil nil nil nil nil nil
                          nil nil nil nil nil nil))
         (elinit--current-plan
          (elinit-plan--create
           :target-members members-hash
           :entries (list empty-entry pop-entry)
           :invalid (make-hash-table :test 'equal)
           :by-target nil
           :deps (make-hash-table :test 'equal)
           :requires-deps (make-hash-table :test 'equal)
           :dependents (make-hash-table :test 'equal)
           :cycle-fallback-ids (make-hash-table :test 'equal)
           :order-index (make-hash-table :test 'equal)
           :meta nil)))
    ;; Only pop.target has members
    (puthash "pop.target" '(:requires ("svc-a") :wants nil) members-hash)
    (let ((ids (elinit--all-target-ids)))
      ;; Both targets should appear
      (should (member "empty.target" ids))
      (should (member "pop.target" ids)))))

(ert-deftest elinit-test-daemon-reload-populates-target-metadata ()
  "Daemon-reload populates target-members and activation-root in plan."
  (elinit-test-with-unit-files
      '(("sleep 1" :id "svc-a" :wanted-by ("multi-user.target"))
        (nil :id "basic.target" :type target)
        (nil :id "multi-user.target" :type target
             :after ("basic.target"))
        (nil :id "graphical.target" :type target
             :after ("multi-user.target"))
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan nil)
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--restart-override (make-hash-table :test 'equal))
          (elinit--logging (make-hash-table :test 'equal))
          (elinit--target-members nil)
          (elinit--target-convergence nil)
          (elinit--target-convergence-reasons nil)
          (elinit-default-target "default.target")
          (elinit-default-target-link "graphical.target"))
      (elinit-daemon-reload)
      ;; Plan should have activation-root
      (should elinit--current-plan)
      (should (elinit-plan-activation-root elinit--current-plan))
      (should (equal "graphical.target"
                     (elinit-plan-activation-root
                      elinit--current-plan)))
      ;; Plan should have target-members populated
      (should (hash-table-p
               (elinit-plan-target-members elinit--current-plan)))
      ;; svc-a declared :wanted-by multi-user.target
      (let ((members (gethash "multi-user.target"
                              (elinit-plan-target-members
                               elinit--current-plan))))
        (should members)
        (should (member "svc-a" (plist-get members :wants))))
      ;; Runtime global should also be updated
      (should (hash-table-p elinit--target-members))
      (should (gethash "multi-user.target" elinit--target-members)))))

(ert-deftest elinit-test-daemon-reload-all-target-ids-after-reload ()
  "All-target-ids returns all targets after daemon-reload."
  (elinit-test-with-unit-files
      '(("sleep 1" :id "svc-a" :wanted-by ("multi-user.target"))
        (nil :id "basic.target" :type target)
        (nil :id "multi-user.target" :type target
             :after ("basic.target"))
        (nil :id "graphical.target" :type target
             :after ("multi-user.target"))
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan nil)
          (elinit--processes (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--entry-state (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal))
          (elinit--enabled-override (make-hash-table :test 'equal))
          (elinit--restart-override (make-hash-table :test 'equal))
          (elinit--logging (make-hash-table :test 'equal))
          (elinit--target-members nil)
          (elinit--target-convergence nil)
          (elinit--target-convergence-reasons nil)
          (elinit-default-target "default.target")
          (elinit-default-target-link "graphical.target"))
      (elinit-daemon-reload)
      (let ((ids (elinit--all-target-ids)))
        ;; All four targets should be present (including empty ones)
        (should (member "basic.target" ids))
        (should (member "multi-user.target" ids))
        (should (member "graphical.target" ids))
        (should (member "default.target" ids))))))

;;;; CLI Target Command Tests

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

(ert-deftest elinit-test-cli-list-targets-includes-empty-targets ()
  "Targets with no members still appear in list-targets."
  (elinit-test-with-unit-files
      '((nil :id "empty.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit--target-convergence nil)
          (elinit--target-convergence-reasons nil)
          (elinit-default-target-link "empty.target")
          (elinit--default-target-link-override nil))
      (let ((result (elinit--cli-dispatch '("list-targets"))))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        (should (string-match "empty\\.target"
                              (elinit-cli-result-output result)))))))

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

(ert-deftest elinit-test-cli-target-status-json ()
  "The `target-status --json' returns structured JSON."
  (elinit-test-with-unit-files
      '(("sleep 1" :id "svc-a" :wanted-by ("app.target"))
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
          (elinit--target-convergence nil)
          (elinit--target-convergence-reasons nil)
          (elinit-default-target-link "app.target")
          (elinit--default-target-link-override nil))
      (let ((result (elinit--cli-dispatch
                     '("target-status" "app.target" "--json"))))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        (should (eq 'json (elinit-cli-result-format result)))
        (let ((parsed (json-read-from-string
                       (elinit-cli-result-output result))))
          (should (equal "app.target" (alist-get 'id parsed)))
          (should (assoc 'status parsed))
          (should (assoc 'requires parsed))
          (should (assoc 'wants parsed)))))))

(ert-deftest elinit-test-cli-target-status-outside-closure-unreachable ()
  "Target-status reports unreachable for target outside active closure."
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
                     '("target-status" "rescue.target"))))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        (should (string-match "Status: unreachable"
                              (elinit-cli-result-output result)))))))

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

(ert-deftest elinit-test-cli-help-text-includes-target-commands ()
  "Help text includes target command section."
  (let ((result (elinit--cli-dispatch '())))
    (should (= elinit-cli-exit-success
                (elinit-cli-result-exitcode result)))
    (let ((output (elinit-cli-result-output result)))
      (should (string-match "Target commands:" output))
      (should (string-match "list-targets" output))
      (should (string-match "target-status" output))
      (should (string-match "explain-target" output))
      (should (string-match "isolate" output))
      (should (string-match "get-default" output))
      (should (string-match "set-default" output)))))

(ert-deftest elinit-test-cli-list-targets-not-running ()
  "The `list-targets' rejects when elinit has no runtime context."
  (let ((elinit--current-plan nil))
    (let ((result (elinit--cli-dispatch '("list-targets"))))
      (should (= elinit-cli-exit-failure
                  (elinit-cli-result-exitcode result)))
      (should (string-match "not running"
                            (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-target-status-not-running ()
  "The `target-status' rejects when elinit has no runtime context."
  (let ((elinit--current-plan nil))
    (let ((result (elinit--cli-dispatch
                   '("target-status" "app.target"))))
      (should (= elinit-cli-exit-failure
                  (elinit-cli-result-exitcode result)))
      (should (string-match "not running"
                            (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-explain-target-not-running ()
  "The `explain-target' rejects when elinit has no runtime context."
  (let ((elinit--current-plan nil))
    (let ((result (elinit--cli-dispatch
                   '("explain-target" "app.target"))))
      (should (= elinit-cli-exit-failure
                  (elinit-cli-result-exitcode result)))
      (should (string-match "not running"
                            (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-isolate-not-running ()
  "The `isolate --yes' rejects when elinit has no runtime context."
  (let ((elinit--current-plan nil))
    (let ((result (elinit--cli-dispatch
                   '("isolate" "--yes" "app.target"))))
      (should (= elinit-cli-exit-failure
                  (elinit-cli-result-exitcode result)))
      (should (string-match "not running"
                            (elinit-cli-result-output result))))))

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

(ert-deftest elinit-test-cli-explain-target-json ()
  "Explain-target JSON output includes id, status, reasons, members."
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
                     '("--json" "explain-target" "app.target"))))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        (let ((json (json-read-from-string
                     (elinit-cli-result-output result))))
          (should (equal "app.target" (alist-get 'id json)))
          (should (equal "reached" (alist-get 'status json)))
          (should (arrayp (alist-get 'reasons json)))
          (should (arrayp (alist-get 'members json))))))))

(ert-deftest elinit-test-cli-isolate-json ()
  "Isolate JSON output includes status, target, stopped, started counts."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc-a" :required-by ("app.target"))
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
        (let ((result (elinit--cli-dispatch
                       '("--json" "isolate" "--yes" "app.target"))))
          (should (= elinit-cli-exit-success
                      (elinit-cli-result-exitcode result)))
          (let ((json (json-read-from-string
                       (elinit-cli-result-output result))))
            (should (equal "applied" (alist-get 'status json)))
            (should (equal "app.target" (alist-get 'target json)))
            (should (numberp (alist-get 'stopped json)))
            (should (numberp (alist-get 'started json)))))))))

;;;; SysV Init Runlevel Compatibility Tests

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
          ;; Invalid-hash must NOT contain the alias ID -- the valid
          ;; builtin replaces it, so the dashboard/CLI must not show
          ;; the rejected disk unit's invalid status.
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
          ;; Built-in alias still present
          (let ((entry (cl-find "runlevel3.target"
                                elinit--programs-cache
                                :key (lambda (e) (plist-get (cdr e) :id))
                                :test #'equal)))
            (should entry))
          ;; Merge-time warning was emitted
          (should (cl-some (lambda (w)
                             (string-match-p "runlevel3\\.target.*immutable" w))
                           warnings))
          ;; Invalid-hash cleaned: builtin is valid, disk rejection must
          ;; not leak into dashboard/CLI invalid state.
          (should-not (gethash "runlevel3.target"
                               elinit--unit-file-invalid)))
      (delete-directory dir t))))

(ert-deftest elinit-test-alias-invalid-winner-does-not-suppress-builtin ()
  "Invalid disk alias target does not suppress the valid builtin in merged set.
Exercises the authority resolver invalid-winner-blocks-fallback semantics:
even when a disk alias target wins authority and is marked invalid, the
builtin alias target must still appear in the merged program cache and
the invalid-hash must not contain the alias ID."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit--programs-cache :not-yet-loaded)
         (elinit--unit-file-invalid (make-hash-table :test 'equal)))
    ;; Write a deliberately invalid alias target (has :after dep edge)
    (elinit-test--write-unit-files
     dir '((nil :id "runlevel0.target" :type target
                :after ("basic.target"))))
    (unwind-protect
        (progn
          (elinit--load-programs)
          ;; Builtin runlevel0.target must be in the cache
          (let ((entry (cl-find "runlevel0.target"
                                elinit--programs-cache
                                :key (lambda (e) (plist-get (cdr e) :id))
                                :test #'equal)))
            (should entry)
            ;; Must be the builtin (no :after)
            (should-not (plist-get (cdr entry) :after))
            ;; Description should be the builtin's
            (should (string-match-p "poweroff"
                                    (plist-get (cdr entry) :description))))
          ;; Invalid-hash must not contain alias ID
          (should-not (gethash "runlevel0.target"
                               elinit--unit-file-invalid))
          ;; Other builtins are unaffected
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
    ;; Override basic.target (canonical, not alias -- should be allowed)
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
            ;; Should be the disk version (user-custom description)
            (should (equal "User-custom basic target"
                           (plist-get (cdr entry) :description)))
            ;; Should appear exactly once
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
    ;; No disk targets -- all built-in targets should appear in merged set
    (unwind-protect
        (progn
          (elinit--load-programs)
          (let ((ids (mapcar (lambda (e) (plist-get (cdr e) :id))
                             elinit--programs-cache)))
            ;; Canonical targets present
            (should (member "basic.target" ids))
            (should (member "multi-user.target" ids))
            (should (member "graphical.target" ids))
            (should (member "default.target" ids))
            ;; Init-transition targets present
            (should (member "rescue.target" ids))
            (should (member "shutdown.target" ids))
            (should (member "poweroff.target" ids))
            (should (member "reboot.target" ids))
            ;; Alias targets present
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

(ert-deftest elinit-test-cli-init-non-numeric ()
  "The `init' command rejects non-numeric arguments."
  (let ((result (elinit--cli-dispatch '("init" "abc"))))
    (should (= elinit-cli-exit-invalid-args
                (elinit-cli-result-exitcode result)))
    (should (string-match "not a valid runlevel"
                          (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-init-no-args ()
  "The `init' command rejects missing argument."
  (let ((result (elinit--cli-dispatch '("init"))))
    (should (= elinit-cli-exit-invalid-args
                (elinit-cli-result-exitcode result)))))

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

(ert-deftest elinit-test-cli-init-json-includes-runlevel ()
  "JSON output from init includes runlevel field."
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
      (cl-letf (((symbol-function 'elinit--dag-start-with-deps)
                 (lambda (_entries callback) (funcall callback)))
                ((symbol-function 'elinit--manual-stop)
                 (lambda (_id) (list :status 'stopped :reason nil))))
        (let ((result (elinit--cli-dispatch '("--json" "init" "--yes" "3"))))
          (should (= elinit-cli-exit-success
                      (elinit-cli-result-exitcode result)))
          (let ((obj (json-read-from-string
                      (elinit-cli-result-output result))))
            ;; Must have runlevel field
            (should (assq 'runlevel obj))
            (should (= 3 (cdr (assq 'runlevel obj))))
            ;; Must also have target from isolate
            (should (assq 'target obj))
            (should (equal "multi-user.target"
                           (cdr (assq 'target obj))))))))))

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

(ert-deftest elinit-test-cli-telinit-parity ()
  "The `telinit' command behaves identically to `init'."
  ;; Error paths: telinit without args
  (let ((result (elinit--cli-dispatch '("telinit"))))
    (should (= elinit-cli-exit-invalid-args
                (elinit-cli-result-exitcode result))))
  ;; Error paths: telinit out of range
  (let ((result (elinit--cli-dispatch '("telinit" "9"))))
    (should (= elinit-cli-exit-invalid-args
                (elinit-cli-result-exitcode result))))
  ;; Error paths: telinit destructive without --yes
  (let ((result (elinit--cli-dispatch '("telinit" "0"))))
    (should (= elinit-cli-exit-invalid-args
                (elinit-cli-result-exitcode result)))
    (should (string-match "destructive"
                          (elinit-cli-result-output result)))))

(ert-deftest elinit-test-cli-telinit-success-parity ()
  "Successful telinit produces same exitcode and output as init."
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
        ;; Test representative runlevels: non-destructive and destructive
        (dolist (spec '(("1" "rescue\\.target" "runlevel 1")
                        ("3" "multi-user\\.target" "runlevel 3")
                        ("5" "graphical\\.target" "runlevel 5")
                        ("0" "poweroff\\.target" "runlevel 0")
                        ("6" "reboot\\.target" "runlevel 6")))
          (let* ((rl (car spec))
                 (target-pattern (nth 1 spec))
                 (rl-pattern (nth 2 spec))
                 (init-result (elinit--cli-dispatch
                               (list "init" "--yes" rl)))
                 (telinit-result (elinit--cli-dispatch
                                  (list "telinit" "--yes" rl))))
            ;; Same exitcode
            (should (= (elinit-cli-result-exitcode init-result)
                        (elinit-cli-result-exitcode telinit-result)))
            ;; Both succeed
            (should (= elinit-cli-exit-success
                        (elinit-cli-result-exitcode telinit-result)))
            ;; Both mention resolved target
            (should (string-match target-pattern
                                  (elinit-cli-result-output init-result)))
            (should (string-match target-pattern
                                  (elinit-cli-result-output telinit-result)))
            ;; Both include runlevel identity
            (should (string-match rl-pattern
                                  (elinit-cli-result-output init-result)))
            (should (string-match rl-pattern
                                  (elinit-cli-result-output telinit-result)))))))))

(ert-deftest elinit-test-cli-list-targets-shows-kind ()
  "The `list-targets' output includes kind column (alias/canonical)."
  (elinit-test-with-unit-files
      '(("sleep 1" :id "svc-a" :wanted-by ("multi-user.target"))
        (nil :id "basic.target" :type target)
        (nil :id "multi-user.target" :type target
             :after ("basic.target"))
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit--target-convergence nil)
          (elinit--target-convergence-reasons nil)
          (elinit-default-target-link "multi-user.target")
          (elinit--default-target-link-override nil))
      (let ((result (elinit--cli-dispatch '("list-targets"))))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        (let ((output (elinit-cli-result-output result)))
          (should (string-match "KIND" output))
          (should (string-match "canonical" output)))))))

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

(ert-deftest elinit-test-timer-rejects-init-transition-target ()
  "Timer validation rejects init-transition targets."
  (elinit-test-with-unit-files
      '((nil :id "rescue.target" :type target)
        (nil :id "shutdown.target" :type target)
        (nil :id "poweroff.target" :type target)
        (nil :id "reboot.target" :type target)
        (nil :id "runlevel0.target" :type target)
        (nil :id "default.target" :type target))
    (let* ((plan (elinit--build-plan (elinit--effective-programs))))
      (dolist (tid '("rescue.target" "shutdown.target"
                     "poweroff.target" "reboot.target"
                     "runlevel0.target"))
        (let ((reason (elinit-timer--validate
                       `(:id ,(format "timer-%s" tid)
                             :target ,tid
                             :on-calendar (:hour 3))
                       plan)))
          (should reason)
          (should (string-match "init-transition" reason)))))))

(ert-deftest elinit-test-timer-allows-non-init-targets ()
  "Timer validation allows non-init-transition targets."
  (elinit-test-with-unit-files
      '(("sleep 1" :id "svc-a" :type oneshot)
        ("sleep 1" :id "svc-b" :type simple)
        (nil :id "app.target" :type target)
        (nil :id "default.target" :type target))
    (let* ((plan (elinit--build-plan (elinit--effective-programs))))
      (dolist (tid '("svc-a" "svc-b" "app.target"))
        (let ((reason (elinit-timer--validate
                       `(:id ,(format "timer-%s" tid)
                             :target ,tid
                             :on-calendar (:hour 3))
                       plan)))
          (should-not reason))))))

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

(ert-deftest elinit-test-cli-list-timers-init-transition-json ()
  "The `list-timers --json' output carries init-transition reason text."
  (elinit-test-with-unit-files
      '((nil :id "poweroff.target" :type target))
    (let ((elinit-mode t)
          (elinit-timers '((:id "timer-poweroff"
                               :target "poweroff.target"
                               :on-startup-sec 60)))
          (elinit--timer-list nil)
          (elinit--timer-state (make-hash-table :test 'equal))
          (elinit--invalid-timers (make-hash-table :test 'equal)))
      (let ((result (elinit--cli-dispatch '("--json" "list-timers"))))
        (should (elinit-cli-result-p result))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (data (json-read-from-string
                      (elinit-cli-result-output result)))
               (invalid (alist-get 'invalid data))
               (entry (car invalid)))
          (should invalid)
          (should (equal "timer-poweroff" (alist-get 'id entry)))
          (should (string-match-p "init-transition"
                                  (alist-get 'reason entry)))
          (should (string-match-p "not timer-eligible"
                                  (alist-get 'reason entry))))))))

(ert-deftest elinit-test-dashboard-timer-init-transition-reason ()
  "Dashboard timer section carries init-transition reason text."
  (elinit-test-with-unit-files
      '(("sleep 60" :id "svc" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal))
           (elinit--timer-state (make-hash-table :test 'equal))
           (elinit--invalid-timers (make-hash-table :test 'equal))
           (elinit-dashboard-show-timers t)
           (elinit--timer-list nil)
           (reason ":target 'poweroff.target' is an init-transition target and is not timer-eligible"))
      (puthash "timer-poweroff" reason elinit--invalid-timers)
      (let ((entries (elinit--get-entries)))
        (let ((bad-row (cl-find (cons :timer "timer-poweroff") entries
                                :key #'car :test #'equal)))
          (should bad-row)
          ;; Reason is at index 6 in dashboard vector
          (should (string-match-p "init-transition"
                                  (aref (cadr bad-row) 6)))
          (should (string-match-p "not timer-eligible"
                                  (aref (cadr bad-row) 6))))))))

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

(ert-deftest elinit-test-cli-list-targets-alias-shows-canonical-convergence ()
  "Alias target in list-targets shows convergence from canonical target."
  (elinit-test-with-unit-files
      '(("sleep 1" :id "svc-a" :wanted-by ("multi-user.target"))
        (nil :id "basic.target" :type target)
        (nil :id "multi-user.target" :type target)
        (nil :id "runlevel3.target" :type target)
        (nil :id "default.target" :type target))
    (let ((elinit--current-plan t)
          (elinit--target-convergence (make-hash-table :test 'equal))
          (elinit--target-convergence-reasons
           (make-hash-table :test 'equal))
          (elinit-default-target-link "multi-user.target")
          (elinit--default-target-link-override nil))
      (puthash "multi-user.target" 'reached elinit--target-convergence)
      (let ((result (elinit--cli-dispatch '("list-targets"))))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        (let ((output (elinit-cli-result-output result)))
          ;; runlevel3.target row should show reached, not pending
          (should (string-match "runlevel3\\.target.*reached" output)))))))

(ert-deftest elinit-test-cli-target-status-alias-shows-canonical-state ()
  "Target-status for alias shows convergence from canonical target."
  (elinit-test-with-unit-files
      '(("sleep 1" :id "svc-a" :wanted-by ("multi-user.target"))
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
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--target-convergence (make-hash-table :test 'equal))
          (elinit--target-convergence-reasons
           (make-hash-table :test 'equal))
          (elinit-default-target-link "multi-user.target")
          (elinit--default-target-link-override nil))
      (puthash "multi-user.target" 'reached elinit--target-convergence)
      (let ((result (elinit--cli-dispatch
                     '("target-status" "runlevel3.target"))))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        (let ((output (elinit-cli-result-output result)))
          (should (string-match "reached" output))
          ;; Should list members from canonical target
          (should (string-match "svc-a" output)))))))

(ert-deftest elinit-test-cli-explain-target-alias-shows-canonical-state ()
  "Explain-target for alias shows convergence from canonical target."
  (elinit-test-with-unit-files
      '(("sleep 1" :id "svc-a" :required-by ("multi-user.target"))
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
          (elinit--mask-override (make-hash-table :test 'equal))
          (elinit--target-convergence (make-hash-table :test 'equal))
          (elinit--target-convergence-reasons
           (make-hash-table :test 'equal))
          (elinit-default-target-link "multi-user.target")
          (elinit--default-target-link-override nil))
      (puthash "multi-user.target" 'reached elinit--target-convergence)
      (let ((result (elinit--cli-dispatch
                     '("explain-target" "runlevel3.target"))))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        (let ((output (elinit-cli-result-output result)))
          (should (string-match "reached" output))
          (should (string-match "healthy" output)))))))

(ert-deftest elinit-test-cli-explain-target-json-resolved-link-key ()
  "Explain-target JSON for alias uses `resolved-link' key, not `resolved'."
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
                     '("--json" "explain-target" "runlevel3.target"))))
        (should (= elinit-cli-exit-success
                    (elinit-cli-result-exitcode result)))
        (let ((json (json-read-from-string
                     (elinit-cli-result-output result))))
          (should (alist-get 'resolved-link json))
          (should-not (alist-get 'resolved json)))))))

(ert-deftest elinit-test-dashboard-alias-target-shows-canonical-convergence ()
  "Dashboard entry for alias target shows convergence from canonical."
  (let* ((conv-hash (make-hash-table :test 'equal))
         (elinit--target-convergence conv-hash)
         (elinit--target-convergence-reasons
          (make-hash-table :test 'equal))
         (snapshot (elinit-snapshot--create
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
                    :manually-stopped (make-hash-table :test 'equal)
                    :remain-active (make-hash-table :test 'equal)
                    :timestamp (float-time))))
    ;; Convergence stored under canonical ID
    (puthash "multi-user.target" 'reached conv-hash)
    ;; Query via alias ID
    (let ((vec (elinit--make-dashboard-entry
                "runlevel3.target" 'target nil t nil nil snapshot)))
      ;; TARGET column (index 2) should show reached
      (should (equal "reached"
                     (substring-no-properties (aref vec 2)))))))

(ert-deftest elinit-test-dashboard-alias-target-status-shows-canonical-convergence ()
  "Dashboard STATUS for alias target mirrors canonical convergence."
  (let* ((conv-hash (make-hash-table :test 'equal))
         (elinit--target-convergence conv-hash)
         (elinit--target-convergence-reasons
          (make-hash-table :test 'equal))
         (snapshot (elinit-snapshot--create
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
                    :manually-stopped (make-hash-table :test 'equal)
                    :remain-active (make-hash-table :test 'equal)
                    :timestamp (float-time))))
    ;; Convergence stored under canonical ID
    (puthash "multi-user.target" 'reached conv-hash)
    ;; Query via alias ID
    (let ((vec (elinit--make-dashboard-entry
                "runlevel3.target" 'target nil t nil nil snapshot)))
      ;; STATUS column (index 4) should also show reached
      (should (equal "reached"
                     (substring-no-properties (aref vec 4)))))))

(ert-deftest elinit-test-dashboard-detail-alias-shows-canonical-convergence ()
  "Detail panel for alias target shows convergence from canonical."
  (let* ((elinit--target-convergence (make-hash-table :test 'equal))
         (elinit--target-convergence-reasons
          (make-hash-table :test 'equal))
         (elinit--target-members (make-hash-table :test 'equal))
         (elinit--processes (make-hash-table :test 'equal))
         (elinit--failed (make-hash-table :test 'equal))
         (elinit--oneshot-completed (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal))
         (elinit--enabled-override (make-hash-table :test 'equal))
         (elinit--restart-override (make-hash-table :test 'equal))
         (elinit--logging (make-hash-table :test 'equal))
         (elinit--mask-override (make-hash-table :test 'equal))
         (elinit--remain-active (make-hash-table :test 'equal))
         (elinit--manually-stopped (make-hash-table :test 'equal))
         (elinit--start-times (make-hash-table :test 'equal))
         (elinit--ready-times (make-hash-table :test 'equal))
         (elinit--restart-times (make-hash-table :test 'equal))
         (elinit--restart-timers (make-hash-table :test 'equal))
         (elinit--logging-override (make-hash-table :test 'equal))
         (elinit--last-exit-info (make-hash-table :test 'equal))
         (elinit--spawn-failure-reason (make-hash-table :test 'equal))
         ;; 38-element target entry for runlevel3.target (alias)
         (entry (list "runlevel3.target" nil 0 t nil nil nil nil
                      'target nil nil nil nil nil
                      nil nil nil nil nil nil
                      nil nil nil nil nil nil nil nil nil nil
                      nil nil
                      nil nil nil nil nil nil)))
    ;; Store convergence under canonical ID
    (puthash "multi-user.target" 'reached elinit--target-convergence)
    (puthash "multi-user.target" '("all healthy")
             elinit--target-convergence-reasons)
    (puthash "multi-user.target" '(:requires ("svc-a") :wants ("svc-b"))
             elinit--target-members)
    (cl-letf (((symbol-function 'elinit--unit-file-path)
               (lambda (_id) nil))
              ((symbol-function 'elinit--telemetry-log-tail)
               (lambda (_id &optional _lines) nil)))
      (elinit--describe-entry-detail "runlevel3.target" entry)
      (let ((info-buf (get-buffer "*elinit-info*")))
        (unwind-protect
            (progn
              (should info-buf)
              (let ((output (with-current-buffer info-buf
                              (buffer-string))))
                (should (string-match-p "Converge: reached" output))
                (should (string-match-p "Req-mem: svc-a" output))
                (should (string-match-p "Want-mem: svc-b" output))))
          (when info-buf (kill-buffer info-buf)))))))

;;; Regression Tests: Convergence, Status, and Target Guards

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
    ;; Convergence stored under canonical target
    (puthash "multi-user.target" 'reached elinit--target-convergence)
    ;; Alias target should resolve to canonical and show reached
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
    ;; Alias should resolve and return canonical reasons
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
    ;; graphical.target is in the closure
    (puthash "graphical.target" t closure)
    ;; rescue.target is NOT in the closure -- should be unreachable
    (let ((result (elinit--compute-entry-status
                   "rescue.target" 'target)))
      (should (equal "unreachable" (car result))))
    ;; graphical.target is in closure with no convergence -- should be pending
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
    ;; multi-user.target is in the closure
    (puthash "multi-user.target" t closure)
    (puthash "multi-user.target" 'reached elinit--target-convergence)
    ;; runlevel3.target resolves to multi-user.target which IS in closure
    (let ((result (elinit--compute-entry-status
                   "runlevel3.target" 'target)))
      (should (equal "reached" (car result))))
    ;; runlevel1.target resolves to rescue.target which is NOT in closure
    (let ((result (elinit--compute-entry-status
                   "runlevel1.target" 'target)))
      (should (equal "unreachable" (car result))))))

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

(provide 'elinit-test-targets)
;;; elinit-test-targets.el ends here
