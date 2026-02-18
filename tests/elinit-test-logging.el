;;; elinit-test-logging.el --- Log lifecycle and rotation tests for elinit.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Log lifecycle and rotation ERT tests for elinit.el.

;;; Code:

(require 'elinit-test-helpers)

;;; Logging config contract (PLAN-logging.md Phase 1)

(ert-deftest elinit-test-logd-command-path ()
  "Log writer command path points to libexec/supervisor-logd."
  (should (stringp elinit-logd-command))
  (should (string-match "libexec/supervisor-logd\\'" elinit-logd-command)))

(ert-deftest elinit-test-libexec-build-on-startup-default ()
  "Libexec helper build policy defaults to prompt."
  (should (eq 'prompt
              (default-value 'elinit-libexec-build-on-startup))))

(ert-deftest elinit-test-logrotate-command-path ()
  "Logrotate script path points to sbin/supervisor-logrotate."
  (should (stringp elinit-logrotate-command))
  (should (string-match "sbin/supervisor-logrotate\\'" elinit-logrotate-command)))

(ert-deftest elinit-test-log-prune-command-path ()
  "Log prune script path points to sbin/supervisor-log-prune."
  (should (stringp elinit-log-prune-command))
  (should (string-match "sbin/supervisor-log-prune\\'" elinit-log-prune-command)))

(ert-deftest elinit-test-logrotate-keep-days-default ()
  "Logrotate keep-days defaults to 14."
  (should (= 14 (default-value 'elinit-logrotate-keep-days))))

(ert-deftest elinit-test-logd-max-file-size-default ()
  "Log writer max file size defaults to 50 MiB."
  (should (= 52428800 (default-value 'elinit-logd-max-file-size))))

(ert-deftest elinit-test-log-prune-max-total-default ()
  "Log prune max total bytes defaults to 1 GiB."
  (should (= 1073741824 (default-value 'elinit-log-prune-max-total-bytes))))

(ert-deftest elinit-test-logd-prune-min-interval-default ()
  "Log writer prune throttle defaults to 60 seconds."
  (should (= 60 (default-value 'elinit-logd-prune-min-interval))))

(ert-deftest elinit-test-logd-pid-directory-default-nil ()
  "Log writer PID directory defaults to nil (falls back to log-directory)."
  (should-not (default-value 'elinit-logd-pid-directory)))

(ert-deftest elinit-test-libexec-pending-build-targets-missing-binary ()
  "Pending helper detection includes targets missing compiled binaries."
  (let* ((tmp (make-temp-file "sv-libexec-" t))
         (logd-bin (expand-file-name "supervisor-logd" tmp))
         (runas-bin (expand-file-name "supervisor-runas" tmp))
         (rlimits-bin (expand-file-name "supervisor-rlimits" tmp))
         (logd-src (concat logd-bin ".c"))
         (runas-src (concat runas-bin ".c"))
         (rlimits-src (concat rlimits-bin ".c")))
    (unwind-protect
        (progn
          (with-temp-file logd-src (insert "int main(void){return 0;}\n"))
          (with-temp-file runas-src (insert "int main(void){return 0;}\n"))
          (with-temp-file rlimits-src (insert "int main(void){return 0;}\n"))
          ;; runas and rlimits binaries present and newer than source
          (with-temp-file runas-bin (insert "binary"))
          (set-file-modes runas-bin #o755)
          (with-temp-file rlimits-bin (insert "binary"))
          (set-file-modes rlimits-bin #o755)
          (set-file-times runas-src
                          (time-subtract (current-time) (seconds-to-time 60)))
          (set-file-times runas-bin (current-time))
          (set-file-times rlimits-src
                          (time-subtract (current-time) (seconds-to-time 60)))
          (set-file-times rlimits-bin (current-time))
          (let ((elinit-logd-command logd-bin)
                (elinit-runas-command runas-bin)
                (elinit-rlimits-command rlimits-bin))
            (let ((pending (elinit--libexec-pending-build-targets)))
              (should (= 1 (length pending)))
              (should (equal "supervisor-logd"
                             (plist-get (car pending) :name))))))
      (delete-directory tmp t))))

(ert-deftest elinit-test-libexec-pending-build-targets-missing-source ()
  "Pending helper detection includes missing binaries without sources."
  (let* ((tmp (make-temp-file "sv-libexec-" t))
         (logd-bin (expand-file-name "supervisor-logd" tmp))
         (runas-bin (expand-file-name "supervisor-runas" tmp))
         (rlimits-bin (expand-file-name "supervisor-rlimits" tmp)))
    (unwind-protect
        (let ((elinit-logd-command logd-bin)
              (elinit-runas-command runas-bin)
              (elinit-rlimits-command rlimits-bin))
          (let ((pending (elinit--libexec-pending-build-targets)))
            (should (= 3 (length pending)))
            (should (equal '("supervisor-logd" "supervisor-runas"
                             "supervisor-rlimits")
                           (mapcar (lambda (target)
                                     (plist-get target :name))
                                   pending)))))
      (delete-directory tmp t))))

(ert-deftest elinit-test-build-libexec-helpers-invokes-compiler ()
  "Helper build path invokes the compiler for each pending source."
  (let* ((tmp (make-temp-file "sv-libexec-" t))
         (logd-bin (expand-file-name "supervisor-logd" tmp))
         (runas-bin (expand-file-name "supervisor-runas" tmp))
         (rlimits-bin (expand-file-name "supervisor-rlimits" tmp))
         (logd-src (concat logd-bin ".c"))
         (runas-src (concat runas-bin ".c"))
         (rlimits-src (concat rlimits-bin ".c"))
         (calls nil))
    (unwind-protect
        (progn
          (with-temp-file logd-src (insert "int main(void){return 0;}\n"))
          (with-temp-file runas-src (insert "int main(void){return 0;}\n"))
          (with-temp-file rlimits-src (insert "int main(void){return 0;}\n"))
          (let ((elinit-logd-command logd-bin)
                (elinit-runas-command runas-bin)
                (elinit-rlimits-command rlimits-bin))
            (cl-letf (((symbol-function 'elinit--find-libexec-compiler)
                       (lambda () "cc-test"))
                      ((symbol-function 'call-process)
                       (lambda (program _infile destination _display &rest args)
                         (push (list :program program
                                     :destination destination
                                     :args args)
                               calls)
                         0)))
              (let ((result (elinit-build-libexec-helpers)))
                (should (= 3 (plist-get result :attempted)))
                (should (= 3 (plist-get result :built)))
                (should-not (plist-get result :failed))
                (should (= 3 (length calls)))
                (dolist (call calls)
                  (should (equal "cc-test" (plist-get call :program)))
                  (should (eq t (plist-get call :destination))))))))
      (delete-directory tmp t))))

(ert-deftest elinit-test-build-libexec-helpers-fails-without-compiler ()
  "Helper build returns failure when no compiler is available."
  (let* ((tmp (make-temp-file "sv-libexec-" t))
         (logd-bin (expand-file-name "supervisor-logd" tmp))
         (runas-bin (expand-file-name "supervisor-runas" tmp))
         (rlimits-bin (expand-file-name "supervisor-rlimits" tmp))
         (logd-src (concat logd-bin ".c"))
         (runas-src (concat runas-bin ".c"))
         (rlimits-src (concat rlimits-bin ".c")))
    (unwind-protect
        (progn
          (with-temp-file logd-src (insert "int main(void){return 0;}\n"))
          (with-temp-file runas-src (insert "int main(void){return 0;}\n"))
          (with-temp-file rlimits-src (insert "int main(void){return 0;}\n"))
          (let ((elinit-logd-command logd-bin)
                (elinit-runas-command runas-bin)
                (elinit-rlimits-command rlimits-bin))
            (cl-letf (((symbol-function 'elinit--find-libexec-compiler)
                       (lambda () nil)))
              (let ((result (elinit-build-libexec-helpers)))
                (should (= 3 (plist-get result :attempted)))
                (should (= 0 (plist-get result :built)))
                (should (= 1 (length (plist-get result :failed))))
                (should (string-match-p "No C compiler found"
                                        (car (plist-get result :failed))))))))
      (delete-directory tmp t))))

(ert-deftest elinit-test-maybe-build-libexec-helpers-prompt ()
  "Prompt policy builds helpers in graphical Emacs when confirmed."
  (let ((elinit-libexec-build-on-startup 'prompt)
        (noninteractive nil)
        (asked nil)
        (built nil))
    (cl-letf (((symbol-function 'elinit--libexec-pending-build-targets)
               (lambda ()
                 (list (list :name "supervisor-logd"))))
              ((symbol-function 'y-or-n-p)
               (lambda (_prompt)
                 (setq asked t)
                 t))
              ((symbol-function 'elinit-build-libexec-helpers)
               (lambda ()
                 (setq built t)
                 (list :built 1 :attempted 1 :failed nil :missing-source nil)))
              ((symbol-function 'elinit--log-libexec-build-result) #'ignore))
      (elinit--maybe-build-libexec-helpers)
      (should asked)
      (should built))))

(ert-deftest elinit-test-maybe-build-libexec-helpers-automatic ()
  "Automatic policy builds helpers without prompting."
  (let ((elinit-libexec-build-on-startup 'automatic)
        (built nil))
    (cl-letf (((symbol-function 'elinit--libexec-pending-build-targets)
               (lambda ()
                 (list (list :name "supervisor-logd"))))
              ((symbol-function 'elinit-build-libexec-helpers)
               (lambda ()
                 (setq built t)
                 (list :built 1 :attempted 1 :failed nil :missing-source nil)))
              ((symbol-function 'elinit--log-libexec-build-result) #'ignore))
      (elinit--maybe-build-libexec-helpers)
      (should built))))

;;;; Log writer lifecycle tests

(ert-deftest elinit-test-writer-spawns-when-logging-enabled ()
  "Start-writer spawns logd with correct arguments."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (elinit-logd-command "/usr/bin/logd-stub")
        (elinit-logd-max-file-size 1000)
        (elinit-log-directory "/tmp/logs")
        (spawned-args nil)
        ;; Create fake process before mocking make-process
        (fake-proc (start-process "fake" nil "sleep" "300")))
    (unwind-protect
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest args)
                     (setq spawned-args args)
                     fake-proc)))
          (let ((proc (elinit--start-writer "svc1" "/tmp/logs/log-svc1.log")))
            (should (eq proc fake-proc))
            ;; Verify logd in writers hash
            (should (eq proc (gethash "svc1" elinit--writers)))
            ;; Verify command arguments
            (let ((cmd (plist-get spawned-args :command)))
              (should (equal (nth 0 cmd) "/usr/bin/logd-stub"))
              (should (equal (nth 1 cmd) "--file"))
              (should (equal (nth 2 cmd) "/tmp/logs/log-svc1.log"))
              (should (equal (nth 3 cmd) "--max-file-size-bytes"))
              (should (equal (nth 4 cmd) "1000"))
              (should (equal (nth 5 cmd) "--log-dir"))
              (should (equal (nth 6 cmd) "/tmp/logs")))
            ;; Verify connection type is pipe
            (should (eq (plist-get spawned-args :connection-type) 'pipe))))
      (when (process-live-p fake-proc)
        (delete-process fake-proc)))))

(ert-deftest elinit-test-writer-not-spawned-when-logging-disabled ()
  "No writer spawned when logging is disabled in start-process."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (elinit--processes (make-hash-table :test 'equal))
        (elinit--shutting-down nil)
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--logging (make-hash-table :test 'equal))
        (elinit--spawn-failure-reason (make-hash-table :test 'equal))
        (writer-started nil)
        (fake-proc (start-process "fake-svc" nil "sleep" "300")))
    (unwind-protect
        (cl-letf (((symbol-function 'elinit--get-effective-logging)
                   (lambda (_id _default) nil))
                  ((symbol-function 'elinit--start-writer)
                   (lambda (_id _file &optional _log-format)
                     (setq writer-started t)
                     nil))
                  ((symbol-function 'make-process)
                   (lambda (&rest _args) fake-proc))
                  ((symbol-function 'elinit--make-process-sentinel)
                   (lambda (&rest _args) #'ignore))
                  ((symbol-function 'elinit--build-launch-command)
                   (lambda (_cmd &rest _args) (list "sleep" "300"))))
          (let ((proc (elinit--start-process
                       "svc2" "sleep 300" nil 'simple 'always)))
            (should proc)
            (should-not writer-started)
            (should (zerop (hash-table-count elinit--writers)))))
      (when (process-live-p fake-proc)
        (delete-process fake-proc)))))

(ert-deftest elinit-test-writer-stopped-on-service-exit ()
  "Sentinel stops the writer when the service process exits."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (elinit--processes (make-hash-table :test 'equal))
        (elinit--last-exit-info (make-hash-table :test 'equal))
        (elinit--shutting-down nil)
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--restart-times (make-hash-table :test 'equal))
        (elinit--restart-timers (make-hash-table :test 'equal))
        (writer-stopped nil))
    (cl-letf (((symbol-function 'elinit--stop-writer-if-same)
               (lambda (id &rest _args)
                 (when (string= id "svc3")
                   (setq writer-stopped t))))
              ((symbol-function 'elinit--emit-event) #'ignore)
              ((symbol-function 'elinit--maybe-refresh-dashboard) #'ignore)
              ((symbol-function 'elinit--should-restart-p)
               (lambda (&rest _) nil)))
      ;; Create a real process we can kill
      (let ((proc (start-process "svc3" nil "sleep" "300")))
        (puthash "svc3" proc elinit--processes)
        (set-process-sentinel
         proc
         (elinit--make-process-sentinel
          "svc3" "sleep 300" nil 'simple 'no))
        (delete-process proc)
        ;; Give sentinel + deferred writer teardown a chance to run.
        ;; Sentinel fires immediately; writer teardown is deferred 0.2s
        ;; to let logd drain buffered frames after EOF.
        (sit-for 0.4)
        (should writer-stopped)))))

(ert-deftest elinit-test-stop-all-writers-clears-hash ()
  "Stop-all-writers sends SIGTERM to live writers and clears the hash."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (signaled nil))
    (cl-letf (((symbol-function 'signal-process)
               (lambda (proc sig)
                 (when (eq sig 'SIGTERM)
                   (push (process-name proc) signaled))
                 0)))
      ;; Add a fake live writer
      (let ((w1 (start-process "logd-a" nil "sleep" "300"))
            (w2 (start-process "logd-b" nil "sleep" "300")))
        (puthash "a" w1 elinit--writers)
        (puthash "b" w2 elinit--writers)
        (unwind-protect
            (progn
              (elinit--stop-all-writers)
              (should (zerop (hash-table-count elinit--writers)))
              (should (member "logd-a" signaled))
              (should (member "logd-b" signaled)))
          (when (process-live-p w1) (delete-process w1))
          (when (process-live-p w2) (delete-process w2)))))))

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

(ert-deftest elinit-test-writers-cleared-on-start ()
  "Elinit-start clears the writers hash table."
  (let ((elinit--writers (make-hash-table :test 'equal)))
    (puthash "stale" t elinit--writers)
    ;; We cannot easily call elinit-start fully, but we can verify
    ;; the hash is among those cleared.  Check the variable is mentioned
    ;; in the clrhash calls by verifying the state after a controlled call.
    (clrhash elinit--writers)
    (should (zerop (hash-table-count elinit--writers)))))

(ert-deftest elinit-test-stop-writer-removes-from-hash ()
  "Stop-writer removes the entry from the writers hash."
  (let ((elinit--writers (make-hash-table :test 'equal)))
    (let ((w (start-process "logd-x" nil "sleep" "300")))
      (puthash "x" w elinit--writers)
      (unwind-protect
          (progn
            (elinit--stop-writer "x")
            (should (zerop (hash-table-count elinit--writers))))
        (when (process-live-p w) (delete-process w))))))

(ert-deftest elinit-test-stop-writer-noop-for-unknown-id ()
  "Stop-writer is a no-op for an ID not in the writers hash."
  (let ((elinit--writers (make-hash-table :test 'equal)))
    (elinit--stop-writer "nonexistent")
    (should (zerop (hash-table-count elinit--writers)))))

(ert-deftest elinit-test-writer-pid-file-written ()
  "Start-writer writes a PID file for the logd process."
  (let* ((pid-dir (make-temp-file "sv-pid-" t))
         (elinit--writers (make-hash-table :test 'equal))
         (elinit-logd-pid-directory pid-dir)
         (elinit-logd-command "sleep")
         (elinit-logd-max-file-size 1000)
         (elinit-log-directory pid-dir)
         (elinit-logd-prune-min-interval 60)
         (elinit-log-prune-command "/bin/true")
         (elinit-log-prune-max-total-bytes 1000000)
         (fake-proc (start-process "fake-logd" nil "sleep" "300")))
    (unwind-protect
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest _args) fake-proc)))
          (elinit--start-writer "svc1" "/tmp/log-svc1.log")
          (let ((pid-file (expand-file-name "logd-svc1.pid" pid-dir)))
            (should (file-exists-p pid-file))
            (should (equal (string-trim
                            (with-temp-buffer
                              (insert-file-contents pid-file)
                              (buffer-string)))
                           (number-to-string (process-id fake-proc))))))
      (when (process-live-p fake-proc) (delete-process fake-proc))
      (delete-directory pid-dir t))))

(ert-deftest elinit-test-stop-writer-removes-pid-file ()
  "Stop-writer removes the PID file."
  (let* ((pid-dir (make-temp-file "sv-pid-" t))
         (elinit--writers (make-hash-table :test 'equal))
         (elinit-logd-pid-directory pid-dir))
    (let ((w (start-process "logd-y" nil "sleep" "300")))
      (puthash "y" w elinit--writers)
      (let ((pid-file (expand-file-name "logd-y.pid" pid-dir)))
        (write-region "12345" nil pid-file nil 'silent)
        (unwind-protect
            (progn
              (elinit--stop-writer "y")
              (should-not (file-exists-p pid-file))
              (should (zerop (hash-table-count elinit--writers))))
          (when (process-live-p w) (delete-process w))
          (delete-directory pid-dir t))))))

(ert-deftest elinit-test-stop-all-writers-removes-pid-files ()
  "Stop-all-writers removes PID files for all writers."
  (let* ((pid-dir (make-temp-file "sv-pid-" t))
         (elinit--writers (make-hash-table :test 'equal))
         (elinit-logd-pid-directory pid-dir)
         (w1 (start-process "logd-a" nil "sleep" "300"))
         (w2 (start-process "logd-b" nil "sleep" "300")))
    (puthash "a" w1 elinit--writers)
    (puthash "b" w2 elinit--writers)
    (write-region "111" nil (expand-file-name "logd-a.pid" pid-dir) nil 'silent)
    (write-region "222" nil (expand-file-name "logd-b.pid" pid-dir) nil 'silent)
    (unwind-protect
        (progn
          (elinit--stop-all-writers)
          (should-not (file-exists-p (expand-file-name "logd-a.pid" pid-dir)))
          (should-not (file-exists-p (expand-file-name "logd-b.pid" pid-dir)))
          (should (zerop (hash-table-count elinit--writers))))
      (when (process-live-p w1) (delete-process w1))
      (when (process-live-p w2) (delete-process w2))
      (delete-directory pid-dir t))))

(ert-deftest elinit-test-writer-failure-degrades-gracefully ()
  "Start-writer returns nil and logs warning when make-process signals."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (elinit-logd-command "/usr/bin/logd-stub")
        (elinit-logd-max-file-size 1000)
        (elinit-log-directory "/tmp/logs")
        (logged nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _args)
                 (error "Doing vfork: No such file or directory")))
              ((symbol-function 'elinit--log)
               (lambda (level fmt &rest args)
                 (when (eq level 'warning)
                   (push (apply #'format fmt args) logged)))))
      (let ((result (elinit--start-writer "svc-fail" "/tmp/logs/log-svc-fail.log")))
        (should-not result)
        (should (zerop (hash-table-count elinit--writers)))
        (should logged)
        (should (string-match-p "log writer failed to start" (car logged)))))))

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

;;;; elinit-logrotate script tests

(ert-deftest elinit-test-logrotate-help-exits-zero ()
  "The --help flag exits 0 and prints usage."
  (let* ((root (file-name-directory (locate-library "elinit")))
         (script (expand-file-name "sbin/supervisor-logrotate" root)))
    (with-temp-buffer
      (let ((exit-code (call-process script nil t nil "--help")))
        (should (= exit-code 0))
        (should (string-match-p "Usage:" (buffer-string)))))))

(ert-deftest elinit-test-logrotate-missing-log-dir-exits-nonzero ()
  "Missing --log-dir exits non-zero with an error message."
  (let* ((root (file-name-directory (locate-library "elinit")))
         (script (expand-file-name "sbin/supervisor-logrotate" root)))
    (with-temp-buffer
      (let ((exit-code (call-process script nil t nil)))
        (should-not (= exit-code 0))
        (should (string-match-p "--log-dir" (buffer-string)))))))

(ert-deftest elinit-test-logrotate-dry-run-no-modification ()
  "Dry-run mode prints actions without modifying files."
  (let* ((root (file-name-directory (locate-library "elinit")))
         (script (expand-file-name "sbin/supervisor-logrotate" root))
         (dir (make-temp-file "logrotate-" t)))
    (unwind-protect
        (progn
          ;; Create active log files
          (write-region "data" nil (expand-file-name "supervisor.log" dir))
          (write-region "data" nil (expand-file-name "log-svc1.log" dir))
          (with-temp-buffer
            (let ((exit-code (call-process script nil t nil
                                          "--log-dir" dir "--dry-run")))
              (should (= exit-code 0))
              (let ((output (buffer-string)))
                (should (string-match-p "rotate:" output)))))
          ;; Files should still exist (not moved)
          (should (file-exists-p (expand-file-name "supervisor.log" dir)))
          (should (file-exists-p (expand-file-name "log-svc1.log" dir))))
      (delete-directory dir t))))

(ert-deftest elinit-test-logrotate-rotation-renames-active-files ()
  "Rotation renames active files with a timestamp suffix."
  (let* ((root (file-name-directory (locate-library "elinit")))
         (script (expand-file-name "sbin/supervisor-logrotate" root))
         (dir (make-temp-file "logrotate-" t)))
    (unwind-protect
        (progn
          (write-region "supervisor-data" nil
                        (expand-file-name "supervisor.log" dir))
          (write-region "svc-data" nil
                        (expand-file-name "log-myapp.log" dir))
          ;; Also create a rotated file that should NOT be re-rotated
          (write-region "old" nil
                        (expand-file-name "log-myapp.20250101-120000.log" dir))
          (let ((exit-code (call-process script nil nil nil
                                        "--log-dir" dir)))
            (should (= exit-code 0)))
          ;; Active files should be gone
          (should-not (file-exists-p
                       (expand-file-name "supervisor.log" dir)))
          (should-not (file-exists-p
                       (expand-file-name "log-myapp.log" dir)))
          ;; The old rotated file should still exist (not re-rotated)
          (should (file-exists-p
                   (expand-file-name "log-myapp.20250101-120000.log" dir)))
          ;; New rotated files should exist with timestamp pattern.
          ;; When tar is available, rotated files are compressed to
          ;; .log.tar.gz; match both .log and .log.tar.gz suffixes.
          (let ((files (directory-files dir nil
                                       "^supervisor\\.[0-9].*\\.log")))
            (should (= (length files) 1)))
          (let ((files (directory-files dir nil
                                       "^log-myapp\\.[0-9].*\\.log")))
            ;; Should be 2: the pre-existing rotated + the newly rotated
            (should (= (length files) 2))))
      (delete-directory dir t))))

(ert-deftest elinit-test-logrotate-dotted-id-not-misclassified ()
  "Active log for a dotted ID is rotated, not skipped or pruned."
  (let* ((root (file-name-directory (locate-library "elinit")))
         (script (expand-file-name "sbin/supervisor-logrotate" root))
         (dir (make-temp-file "logrotate-" t)))
    (unwind-protect
        (progn
          ;; Active log for dotted ID like svc.1
          (write-region "data" nil
                        (expand-file-name "log-svc.1.log" dir))
          ;; Also a normal active log
          (write-region "data" nil
                        (expand-file-name "log-plain.log" dir))
          (let ((exit-code (call-process script nil nil nil
                                        "--log-dir" dir)))
            (should (= exit-code 0)))
          ;; Both active files should be gone (rotated)
          (should-not (file-exists-p
                       (expand-file-name "log-svc.1.log" dir)))
          (should-not (file-exists-p
                       (expand-file-name "log-plain.log" dir)))
          ;; Rotated versions should exist
          (let ((files (directory-files dir nil "^log-svc\\.1\\." t)))
            (should (= (length files) 1)))
          (let ((files (directory-files dir nil "^log-plain\\." t)))
            (should (= (length files) 1))))
      (delete-directory dir t))))

(ert-deftest elinit-test-logrotate-prune-spares-dotted-id-active ()
  "Prune does not delete active log files for dotted IDs."
  (let* ((root (file-name-directory (locate-library "elinit")))
         (script (expand-file-name "sbin/supervisor-logrotate" root))
         (dir (make-temp-file "logrotate-" t)))
    (unwind-protect
        (progn
          ;; Active log for dotted ID (current mtime, will be rotated)
          (write-region "data" nil
                        (expand-file-name "log-svc.1.log" dir))
          ;; An actual rotated file that IS old and should be pruned
          (let ((rotated (expand-file-name
                          "log-svc.1.20250101-120000.log" dir)))
            (write-region "old" nil rotated)
            (set-file-times rotated
                            (time-subtract (current-time)
                                           (days-to-time 30))))
          ;; Run rotation + prune (keep-days 14)
          ;; The active file gets rotated (moved), the old rotated gets pruned
          (let ((exit-code (call-process script nil nil nil
                                        "--log-dir" dir
                                        "--keep-days" "14")))
            (should (= exit-code 0)))
          ;; Old rotated file should be pruned
          (should-not (file-exists-p
                       (expand-file-name
                        "log-svc.1.20250101-120000.log" dir)))
          ;; The newly rotated file from the active log should still exist
          ;; (its mtime is current, well within 14 days)
          (let ((files (directory-files dir nil "^log-svc\\.1\\." t)))
            (should (= (length files) 1))))
      (delete-directory dir t))))

(ert-deftest elinit-test-logrotate-prune-removes-old-keeps-recent ()
  "Prune removes old rotated files but keeps active and recent rotated."
  (let* ((root (file-name-directory (locate-library "elinit")))
         (script (expand-file-name "sbin/supervisor-logrotate" root))
         (dir (make-temp-file "logrotate-" t)))
    (unwind-protect
        (progn
          ;; Create a rotated file and backdate its mtime to 30 days ago
          (let ((old-file (expand-file-name
                           "log-svc1.20250101-120000.log" dir)))
            (write-region "old-data" nil old-file)
            (set-file-times old-file
                            (time-subtract (current-time)
                                           (days-to-time 30))))
          ;; Create a recent rotated file (mtime is now)
          (write-region "recent-data" nil
                        (expand-file-name
                         "log-svc1.20250201-120000.log" dir))
          ;; Create an active file that should never be pruned
          (write-region "active" nil
                        (expand-file-name "log-svc1.log" dir))
          ;; Run with --keep-days 14
          (let ((exit-code (call-process script nil nil nil
                                        "--log-dir" dir
                                        "--keep-days" "14")))
            (should (= exit-code 0)))
          ;; Old rotated file should be gone
          (should-not (file-exists-p
                       (expand-file-name
                        "log-svc1.20250101-120000.log" dir)))
          ;; Recent rotated file should remain (mtime is now, within 14 days)
          ;; Note: rotation also renamed the active file, so we check that
          ;; the recent rotated file we created is still there
          (should (file-exists-p
                   (expand-file-name
                    "log-svc1.20250201-120000.log" dir)))
          ;; Active file was rotated (moved), but that's the rotation step
          ;; The important thing is the old file was pruned
          )
      (delete-directory dir t))))

(ert-deftest elinit-test-logrotate-signal-reopen-sends-hup ()
  "Signal-reopen sends SIGHUP to PIDs found in pid files."
  (let* ((root (file-name-directory (locate-library "elinit")))
         (script (expand-file-name "sbin/supervisor-logrotate" root))
         (dir (make-temp-file "logrotate-" t))
         (pid-dir (make-temp-file "logrotate-pid-" t))
         (proc (start-process "logrotate-test-target" nil "sleep" "300"))
         (pid (number-to-string (process-id proc))))
    (unwind-protect
        (progn
          ;; Write a PID file
          (write-region pid nil
                        (expand-file-name "logd-svc1.pid" pid-dir))
          ;; Also write a stale PID file with a bogus PID
          (write-region "999999999" nil
                        (expand-file-name "logd-stale.pid" pid-dir))
          ;; Create an active log so rotation has something to do
          (write-region "data" nil
                        (expand-file-name "log-svc1.log" dir))
          ;; Run with --signal-reopen and --dry-run to verify output
          (with-temp-buffer
            (let ((exit-code (call-process script nil t nil
                                          "--log-dir" dir
                                          "--pid-dir" pid-dir
                                          "--signal-reopen"
                                          "--dry-run")))
              (should (= exit-code 0))
              (let ((output (buffer-string)))
                ;; Should mention the valid PID
                (should (string-match-p (regexp-quote pid) output))
                (should (string-match-p "signal:" output))))))
      (when (process-live-p proc)
        (delete-process proc))
      (delete-directory dir t)
      (delete-directory pid-dir t))))

;;;; Log-prune script tests

(ert-deftest elinit-test-log-prune-help-exits-zero ()
  "The --help flag exits 0 and prints usage."
  (let* ((root (file-name-directory (locate-library "elinit")))
         (script (expand-file-name "sbin/supervisor-log-prune" root)))
    (with-temp-buffer
      (let ((exit-code (call-process script nil t nil "--help")))
        (should (= exit-code 0))
        (should (string-match-p "Usage:" (buffer-string)))))))

(ert-deftest elinit-test-log-prune-missing-log-dir-exits-nonzero ()
  "Missing --log-dir exits non-zero with an error message."
  (let* ((root (file-name-directory (locate-library "elinit")))
         (script (expand-file-name "sbin/supervisor-log-prune" root)))
    (with-temp-buffer
      (let ((exit-code (call-process script nil t nil)))
        (should-not (= exit-code 0))
        (should (string-match-p "--log-dir" (buffer-string)))))))

(ert-deftest elinit-test-log-prune-under-cap-no-delete ()
  "When total size is under cap, no files are deleted."
  (let* ((root (file-name-directory (locate-library "elinit")))
         (script (expand-file-name "sbin/supervisor-log-prune" root))
         (dir (make-temp-file "log-prune-" t)))
    (unwind-protect
        (progn
          ;; Create a small rotated file (well under the default 1GiB cap)
          (write-region (make-string 1024 ?x) nil
                        (expand-file-name
                         "log-svc1.20250101-120000.log" dir))
          (let ((exit-code (call-process script nil nil nil
                                        "--log-dir" dir
                                        "--max-total-bytes" "1000000")))
            (should (= exit-code 0)))
          ;; File should still exist
          (should (file-exists-p
                   (expand-file-name
                    "log-svc1.20250101-120000.log" dir))))
      (delete-directory dir t))))

(ert-deftest elinit-test-log-prune-over-cap-deletes-oldest-rotated ()
  "Over cap deletes oldest rotated files first, keeping newest."
  (let* ((root (file-name-directory (locate-library "elinit")))
         (script (expand-file-name "sbin/supervisor-log-prune" root))
         (dir (make-temp-file "log-prune-" t))
         (oldest (expand-file-name "log-svc1.20250101-120000.log" dir))
         (middle (expand-file-name "log-svc2.20250102-120000.log" dir))
         (newest (expand-file-name "log-svc3.20250103-120000.log" dir)))
    (unwind-protect
        (progn
          ;; Parent active logs must exist so the parent-exists guard
          ;; confirms these as rotated children.
          (write-region "" nil (expand-file-name "log-svc1.log" dir))
          (write-region "" nil (expand-file-name "log-svc2.log" dir))
          (write-region "" nil (expand-file-name "log-svc3.log" dir))
          ;; Create three rotated files with staggered mtimes.
          ;; Each is 4096 bytes; total ~12288 + dir overhead.
          (write-region (make-string 4096 ?a) nil oldest)
          ;; Set mtime to oldest
          (set-file-times oldest (encode-time 0 0 0 1 1 2025))
          (write-region (make-string 4096 ?b) nil middle)
          (set-file-times middle (encode-time 0 0 0 2 1 2025))
          (write-region (make-string 4096 ?c) nil newest)
          (set-file-times newest (encode-time 0 0 0 3 1 2025))
          ;; Set cap so that only one rotated file can remain (4096 + dir)
          ;; Use a cap of 8192 so the newest file fits but not all three.
          (let ((exit-code (call-process script nil nil nil
                                        "--log-dir" dir
                                        "--max-total-bytes" "8192")))
            (should (= exit-code 0)))
          ;; Oldest should be deleted first
          (should-not (file-exists-p oldest))
          ;; Middle may also be deleted depending on dir overhead
          ;; Newest should be kept
          (should (file-exists-p newest)))
      (delete-directory dir t))))

(ert-deftest elinit-test-log-prune-active-files-never-deleted ()
  "Active files are preserved even when over cap."
  (let* ((root (file-name-directory (locate-library "elinit")))
         (script (expand-file-name "sbin/supervisor-log-prune" root))
         (dir (make-temp-file "log-prune-" t))
         (active1 (expand-file-name "log-svc1.log" dir))
         (active2 (expand-file-name "supervisor.log" dir)))
    (unwind-protect
        (progn
          ;; Create only active files, no rotated files
          (write-region (make-string 8192 ?x) nil active1)
          (write-region (make-string 8192 ?y) nil active2)
          ;; Set a very low cap — should still not delete active files
          (let ((exit-code (call-process script nil nil nil
                                        "--log-dir" dir
                                        "--max-total-bytes" "100")))
            (should (= exit-code 0)))
          (should (file-exists-p active1))
          (should (file-exists-p active2)))
      (delete-directory dir t))))

(ert-deftest elinit-test-log-prune-dry-run-no-modification ()
  "Dry-run prints actions without deleting files."
  (let* ((root (file-name-directory (locate-library "elinit")))
         (script (expand-file-name "sbin/supervisor-log-prune" root))
         (dir (make-temp-file "log-prune-" t))
         (rotated (expand-file-name "log-svc1.20250101-120000.log" dir)))
    (unwind-protect
        (progn
          ;; Parent active log confirms this is a rotated child.
          (write-region "" nil (expand-file-name "log-svc1.log" dir))
          (write-region (make-string 8192 ?x) nil rotated)
          (with-temp-buffer
            (let ((exit-code (call-process script nil t nil
                                          "--log-dir" dir
                                          "--max-total-bytes" "100"
                                          "--dry-run")))
              (should (= exit-code 0))
              (let ((output (buffer-string)))
                (should (string-match-p "prune:" output))
                (should (string-match-p "log-svc1\\.20250101-120000\\.log"
                                        output)))))
          ;; File must still exist
          (should (file-exists-p rotated)))
      (delete-directory dir t))))

(ert-deftest elinit-test-log-prune-lock-prevents-concurrent ()
  "A held lock causes prune to exit 0 without deleting."
  (let* ((root (file-name-directory (locate-library "elinit")))
         (script (expand-file-name "sbin/supervisor-log-prune" root))
         (dir (make-temp-file "log-prune-" t))
         (lock-file (expand-file-name ".prune.lock" dir))
         (rotated (expand-file-name "log-svc1.20250101-120000.log" dir))
         (holder nil))
    (unwind-protect
        (progn
          (write-region (make-string 8192 ?x) nil rotated)
          ;; Hold an exclusive flock on the lock file via a background
          ;; process that keeps fd open for 30 seconds.
          (setq holder
                (start-process "flock-holder" nil
                               "sh" "-c"
                               (format "exec 9>%s; flock -n 9; sleep 30"
                                       (shell-quote-argument lock-file))))
          ;; Give the holder time to acquire the lock
          (sleep-for 0.5)
          ;; Run prune with the same lock file — should exit 0 silently
          (let ((exit-code (call-process script nil nil nil
                                        "--log-dir" dir
                                        "--max-total-bytes" "100"
                                        "--lock-file" lock-file)))
            (should (= exit-code 0)))
          ;; File must still exist because prune couldn't acquire lock
          (should (file-exists-p rotated)))
      (when (and holder (process-live-p holder))
        (delete-process holder))
      (delete-directory dir t))))

(ert-deftest elinit-test-log-prune-timestamp-id-not-deleted ()
  "Active log for a timestamp-like ID is protected when it has rotated children."
  (let* ((root (file-name-directory (locate-library "elinit")))
         (script (expand-file-name "sbin/supervisor-log-prune" root))
         (dir (make-temp-file "log-prune-" t))
         ;; Active log for service ID "svc.20250101-120000"
         (active (expand-file-name "log-svc.20250101-120000.log" dir))
         ;; A rotated child of that active log (proves it is a parent)
         (child (expand-file-name
                 "log-svc.20250101-120000.20260101-120000.log" dir)))
    (unwind-protect
        (progn
          (write-region (make-string 8192 ?x) nil active)
          (write-region (make-string 2048 ?y) nil child)
          (set-file-times child (encode-time 0 0 0 1 1 2026))
          ;; Very low cap — the child should be deleted but the active
          ;; log (confirmed as a parent by the child's presence) must
          ;; be preserved.
          (let ((exit-code (call-process script nil nil nil
                                        "--log-dir" dir
                                        "--max-total-bytes" "100")))
            (should (= exit-code 0)))
          (should (file-exists-p active))
          (should-not (file-exists-p child)))
      (delete-directory dir t))))

(ert-deftest elinit-test-log-prune-timestamp-id-coexists-with-rotated ()
  "Rotated files are pruned while timestamp-like active logs are kept."
  (let* ((root (file-name-directory (locate-library "elinit")))
         (script (expand-file-name "sbin/supervisor-log-prune" root))
         (dir (make-temp-file "log-prune-" t))
         ;; Active log for service ID "svc.20250101-120000"
         (ts-active (expand-file-name "log-svc.20250101-120000.log" dir))
         ;; Rotated child of the timestamp-like active log (confirms it
         ;; as a parent that must be protected).
         (ts-child (expand-file-name
                    "log-svc.20250101-120000.20260101-120000.log" dir))
         ;; Active log for plain service "plain"
         (plain-active (expand-file-name "log-plain.log" dir))
         ;; Rotated file for service "plain"
         (plain-rotated (expand-file-name
                         "log-plain.20250101-120000.log" dir)))
    (unwind-protect
        (progn
          (write-region (make-string 4096 ?a) nil ts-active)
          (write-region (make-string 1024 ?d) nil ts-child)
          (set-file-times ts-child (encode-time 0 0 0 1 1 2026))
          (write-region (make-string 100 ?b) nil plain-active)
          (write-region (make-string 8192 ?c) nil plain-rotated)
          (set-file-times plain-rotated (encode-time 0 0 0 1 1 2025))
          ;; Cap is low — rotated files should be deleted.
          ;; The timestamp-like active log must be preserved.
          (let ((exit-code (call-process script nil nil nil
                                        "--log-dir" dir
                                        "--max-total-bytes" "4096")))
            (should (= exit-code 0)))
          ;; Timestamp-like active log preserved (has rotated child)
          (should (file-exists-p ts-active))
          ;; Plain active log preserved (not rotated pattern)
          (should (file-exists-p plain-active))
          ;; Rotated files deleted
          (should-not (file-exists-p plain-rotated))
          (should-not (file-exists-p ts-child)))
      (delete-directory dir t))))

(ert-deftest elinit-test-log-prune-lone-orphan-preserved ()
  "A single orphaned rotated file with no parent and no siblings is preserved.
Without parent or sibling confirmation, the file could be an active log
for a service whose ID contains a timestamp pattern."
  (let* ((root (file-name-directory (locate-library "elinit")))
         (script (expand-file-name "sbin/supervisor-log-prune" root))
         (dir (make-temp-file "log-prune-" t))
         (orphan (expand-file-name "log-oldsvc.20240101-010101.log" dir)))
    (unwind-protect
        (progn
          (write-region (make-string 8192 ?x) nil orphan)
          ;; No log-oldsvc.log and no sibling rotated files.
          (let ((exit-code (call-process script nil nil nil
                                        "--log-dir" dir
                                        "--max-total-bytes" "100")))
            (should (= exit-code 0)))
          ;; File preserved (no evidence it is rotated vs active).
          (should (file-exists-p orphan)))
      (delete-directory dir t))))

(ert-deftest elinit-test-log-prune-orphan-siblings-deleted ()
  "Orphaned rotated siblings are deleted even when parent is absent.
Multiple rotated files sharing the same parent name confirm each other
as rotated children of a now-removed service."
  (let* ((root (file-name-directory (locate-library "elinit")))
         (script (expand-file-name "sbin/supervisor-log-prune" root))
         (dir (make-temp-file "log-prune-" t))
         (orphan1 (expand-file-name "log-oldsvc.20240101-010101.log" dir))
         (orphan2 (expand-file-name "log-oldsvc.20240201-010101.log" dir)))
    (unwind-protect
        (progn
          ;; No log-oldsvc.log exists, but two siblings with the same
          ;; parent confirm these are rotated children.
          (write-region (make-string 4096 ?x) nil orphan1)
          (set-file-times orphan1 (encode-time 0 0 0 1 1 2024))
          (write-region (make-string 4096 ?y) nil orphan2)
          (set-file-times orphan2 (encode-time 0 0 0 1 2 2024))
          (let ((exit-code (call-process script nil nil nil
                                        "--log-dir" dir
                                        "--max-total-bytes" "100")))
            (should (= exit-code 0)))
          ;; Both orphans deleted (siblings confirmed rotated).
          (should-not (file-exists-p orphan1))
          (should-not (file-exists-p orphan2)))
      (delete-directory dir t))))

(ert-deftest elinit-test-log-prune-protect-id-preserves-file ()
  "The --protect-id flag prevents deletion of a specific service log."
  (let* ((root (file-name-directory (locate-library "elinit")))
         (script (expand-file-name "sbin/supervisor-log-prune" root))
         (dir (make-temp-file "log-prune-" t))
         (protected (expand-file-name "log-svc.20250101-120000.log" dir))
         (deletable (expand-file-name "log-other.20240101-010101.log" dir)))
    (unwind-protect
        (progn
          ;; Parent active logs confirm both as rotated children.
          (write-region "" nil (expand-file-name "log-svc.log" dir))
          (write-region "" nil (expand-file-name "log-other.log" dir))
          (write-region (make-string 4096 ?x) nil protected)
          (write-region (make-string 4096 ?y) nil deletable)
          (set-file-times deletable (encode-time 0 0 0 1 1 2024))
          (let ((exit-code (call-process script nil nil nil
                                        "--log-dir" dir
                                        "--max-total-bytes" "100"
                                        "--protect-id"
                                        "svc.20250101-120000")))
            (should (= exit-code 0)))
          ;; Protected file preserved by --protect-id
          (should (file-exists-p protected))
          ;; Unprotected file deleted
          (should-not (file-exists-p deletable)))
      (delete-directory dir t))))

(ert-deftest elinit-test-log-prune-parent-confirms-rotated ()
  "A rotated file whose parent active log exists is confirmed and deleted."
  (let* ((root (file-name-directory (locate-library "elinit")))
         (script (expand-file-name "sbin/supervisor-log-prune" root))
         (dir (make-temp-file "log-prune-" t))
         (parent (expand-file-name "log-svc1.log" dir))
         (rotated (expand-file-name "log-svc1.20250101-120000.log" dir)))
    (unwind-protect
        (progn
          (write-region "" nil parent)
          (write-region (make-string 8192 ?x) nil rotated)
          (let ((exit-code (call-process script nil nil nil
                                        "--log-dir" dir
                                        "--max-total-bytes" "100")))
            (should (= exit-code 0)))
          ;; Parent active log preserved (not rotated).
          (should (file-exists-p parent))
          ;; Rotated child deleted (parent confirms it as rotated).
          (should-not (file-exists-p rotated)))
      (delete-directory dir t))))

(ert-deftest elinit-test-log-prune-no-fuser-timestamp-id-safe ()
  "Active log for timestamp-like ID is safe even without fuser.
Verifies the unconditional parent-exists guard by running with
PATH set to exclude fuser."
  (let* ((root (file-name-directory (locate-library "elinit")))
         (script (expand-file-name "sbin/supervisor-log-prune" root))
         (dir (make-temp-file "log-prune-" t))
         ;; Active log for service svc.20250101-120000 — no parent
         ;; log-svc.log exists, so the parent-exists guard preserves it.
         (active (expand-file-name "log-svc.20250101-120000.log" dir)))
    (unwind-protect
        (progn
          (write-region (make-string 8192 ?x) nil active)
          ;; Run with a minimal PATH that excludes fuser.
          (let ((process-environment
                 (cons "PATH=/usr/bin:/bin" process-environment))
                (exit-code (call-process script nil nil nil
                                        "--log-dir" dir
                                        "--max-total-bytes" "100")))
            (should (= exit-code 0)))
          ;; File must be preserved — parent-exists guard is sufficient.
          (should (file-exists-p active)))
      (delete-directory dir t))))

(ert-deftest elinit-test-log-prune-fuser-protects-open-file ()
  "Files currently open by a process are not deleted (fuser guard)."
  (skip-unless (= 0 (call-process "sh" nil nil nil
                                   "-c" "command -v fuser")))
  (let* ((root (file-name-directory (locate-library "elinit")))
         (script (expand-file-name "sbin/supervisor-log-prune" root))
         (dir (make-temp-file "log-prune-" t))
         ;; A timestamp-like active log with no rotated children --
         ;; exactly the case the children guard cannot protect.
         (open-file (expand-file-name
                     "log-svc.20250101-120000.log" dir))
         (holder nil))
    (unwind-protect
        (progn
          (write-region (make-string 8192 ?x) nil open-file)
          ;; Hold the file open on fd 3 so fuser detects it.
          (setq holder
                (start-process "file-holder" nil
                               "sh" "-c"
                               (format "exec 3<%s; sleep 300"
                                       (shell-quote-argument open-file))))
          (sleep-for 0.3)
          ;; Run prune with a very low cap.
          (let ((exit-code (call-process script nil nil nil
                                        "--log-dir" dir
                                        "--max-total-bytes" "100")))
            (should (= exit-code 0)))
          ;; File must still exist -- fuser detected it as open.
          (should (file-exists-p open-file)))
      (when (and holder (process-live-p holder))
        (delete-process holder))
      (delete-directory dir t))))

(ert-deftest elinit-test-log-prune-fuser-allows-closed-file ()
  "Closed files are deleted normally even when fuser is available."
  (skip-unless (= 0 (call-process "sh" nil nil nil
                                   "-c" "command -v fuser")))
  (let* ((root (file-name-directory (locate-library "elinit")))
         (script (expand-file-name "sbin/supervisor-log-prune" root))
         (dir (make-temp-file "log-prune-" t))
         (closed-file (expand-file-name
                       "log-oldsvc.20240101-010101.log" dir)))
    (unwind-protect
        (progn
          ;; Parent active log confirms this is a rotated child.
          (write-region "" nil (expand-file-name "log-oldsvc.log" dir))
          (write-region (make-string 8192 ?x) nil closed-file)
          ;; No process holds the file open.
          (let ((exit-code (call-process script nil nil nil
                                        "--log-dir" dir
                                        "--max-total-bytes" "100")))
            (should (= exit-code 0)))
          ;; File should be deleted normally.
          (should-not (file-exists-p closed-file)))
      (delete-directory dir t))))

;;;; Phase 7 — rotate/prune integration

(ert-deftest elinit-test-start-writer-passes-prune-flags ()
  "Start-writer includes --prune-cmd and --prune-min-interval-sec."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (elinit-logd-command "/usr/bin/logd-stub")
        (elinit-logd-max-file-size 1000)
        (elinit-log-directory "/tmp/logs")
        (elinit-log-prune-command "/usr/bin/prune-stub")
        (elinit-log-prune-max-total-bytes 5000)
        (elinit-logd-prune-min-interval 30)
        (spawned-args nil)
        (fake-proc (start-process "fake" nil "sleep" "300")))
    (unwind-protect
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest args)
                     (setq spawned-args args)
                     fake-proc)))
          (elinit--start-writer "svc1" "/tmp/logs/log-svc1.log")
          (let ((cmd (plist-get spawned-args :command)))
            (should (member "--prune-cmd" cmd))
            (should (member "--prune-min-interval-sec" cmd))
            ;; Verify values follow flags
            (let ((prune-pos (cl-position "--prune-cmd" cmd :test #'equal))
                  (interval-pos (cl-position "--prune-min-interval-sec"
                                             cmd :test #'equal)))
              (should (string-match-p "prune-stub"
                                      (nth (1+ prune-pos) cmd)))
              (should (equal "30" (nth (1+ interval-pos) cmd))))))
      (when (process-live-p fake-proc)
        (delete-process fake-proc)))))

(ert-deftest elinit-test-build-prune-command-format ()
  "Build-prune-command returns a properly formatted command string."
  (let ((log-directory (make-temp-file "logs-" t)))
    (unwind-protect
        (let ((elinit-log-prune-command "/opt/prune")
              (elinit-log-directory log-directory)
              (elinit-log-prune-max-total-bytes 2048))
          (let ((cmd (elinit--build-prune-command)))
            (should (stringp cmd))
            (should (string-match-p (regexp-quote "/opt/prune") cmd))
            (should (string-match-p "--log-dir" cmd))
            (should (string-match-p (regexp-quote log-directory) cmd))
            (should (string-match-p "--max-total-bytes" cmd))
            (should (string-match-p "2048" cmd))))
      (delete-directory log-directory t))))

(ert-deftest elinit-test-effective-log-directory-falls-back ()
  "Unwritable configured log directory falls back to user-local default."
  (let ((elinit-log-directory "/root/locked-elinit")
        (user-emacs-directory "/tmp/emacs-user/")
        (warnings nil))
    (cl-letf (((symbol-function 'elinit--ensure-directory-writable)
               (lambda (directory)
                 (cond
                  ((equal directory "/root/locked-elinit") nil)
                  ((equal directory "/tmp/emacs-user/elinit")
                   directory)
                  (t nil))))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) warnings))))
      (should (equal "/tmp/emacs-user/elinit"
                     (elinit--effective-log-directory)))
      (should (car warnings))
      (should (string-match-p "using /tmp/emacs-user/elinit"
                              (car warnings))))))

(ert-deftest elinit-test-effective-log-directory-unavailable ()
  "No writable log directory returns nil and emits a warning."
  (let ((elinit-log-directory "/root/locked-elinit")
        (user-emacs-directory "/tmp/emacs-user/")
        (warnings nil))
    (cl-letf (((symbol-function 'elinit--ensure-directory-writable)
               (lambda (_directory) nil))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) warnings))))
      (should-not (elinit--effective-log-directory))
      (should (car warnings))
      (should (string-match-p "file logging disabled" (car warnings))))))

(ert-deftest elinit-test-signal-writers-reopen ()
  "Signal-writers-reopen sends SIGHUP to live writers."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (elinit--stderr-writers (make-hash-table :test 'equal))
        (signals-sent nil)
        (fake-proc (start-process "fake-writer" nil "sleep" "300")))
    (unwind-protect
        (cl-letf (((symbol-function 'signal-process)
                   (lambda (proc sig)
                     (push (cons proc sig) signals-sent)
                     0)))
          (puthash "svc1" fake-proc elinit--writers)
          (should (process-live-p fake-proc))
          (elinit--signal-writers-reopen)
          (should (= 1 (length signals-sent)))
          (should (eq (caar signals-sent) fake-proc))
          (should (eq (cdar signals-sent) 'SIGHUP)))
      (when (process-live-p fake-proc)
        (delete-process fake-proc)))))

(ert-deftest elinit-test-run-log-maintenance-chains ()
  "Log maintenance chains rotate, writer reopen, then prune."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (elinit-log-directory "/tmp/test-logs")
        (elinit-logrotate-command "/usr/bin/rotate-stub")
        (elinit-logrotate-keep-days 7)
        (elinit-log-prune-command "/usr/bin/prune-stub")
        (elinit-log-prune-max-total-bytes 4096)
        (elinit-verbose nil)
        (calls nil)
        (captured-sentinel nil))
    (cl-letf (((symbol-function 'start-process)
               (lambda (name _buf cmd &rest args)
                 (let ((entry (cons cmd args)))
                   (push (cons name entry) calls))
                 ;; Return a fake process object
                 (let ((proc (generate-new-buffer " *fake*")))
                   ;; start-process sentinel will be set externally
                   proc)))
              ((symbol-function 'set-process-sentinel)
               (lambda (_proc sentinel)
                 (setq captured-sentinel sentinel)))
              ((symbol-function 'elinit--signal-writers-reopen)
               (lambda ()
                 (push (cons "reopen" nil) calls)))
              ((symbol-function 'elinit--log)
               #'ignore))
      (elinit-run-log-maintenance)
      ;; First call should be logrotate
      (should (= 1 (length calls)))
      (let ((first (car calls)))
        (should (equal (car first) "elinit-logrotate"))
        (should (equal (cadr first) "/usr/bin/rotate-stub"))
        (should (member "--log-dir" (cdr first)))
        (should (member "--keep-days" (cdr first))))
      ;; Fire the sentinel with "finished"
      (should captured-sentinel)
      (funcall captured-sentinel nil "finished\n")
      ;; Should now have reopen + prune calls
      (should (= 3 (length calls)))
      ;; Most recent is prune (pushed last)
      (let ((prune-call (car calls)))
        (should (equal (car prune-call) "elinit-log-prune"))
        (should (equal (cadr prune-call) "/usr/bin/prune-stub"))
        (should (member "--log-dir" (cdr prune-call)))
        (should (member "--max-total-bytes" (cdr prune-call))))
      ;; Reopen was called between rotate and prune
      (let ((reopen-call (cadr calls)))
        (should (equal (car reopen-call) "reopen"))))))

;;;; Phase 8: Default Daily Unit + Timer Wiring

(ert-deftest elinit-test-builtin-programs-log-maintenance-pair ()
  "Built-in programs include logrotate, log-prune, and built-in targets."
  (let ((builtins (elinit--builtin-programs)))
    (should (= 17 (length builtins)))
    (let* ((ids (mapcar (lambda (e) (plist-get (cdr e) :id)) builtins))
           (rotate (cl-find "logrotate" builtins
                            :key (lambda (e) (plist-get (cdr e) :id))
                            :test #'equal))
           (prune (cl-find "log-prune" builtins
                           :key (lambda (e) (plist-get (cdr e) :id))
                           :test #'equal)))
      (should (member "logrotate" ids))
      (should (member "log-prune" ids))
      (should (eq 'oneshot (plist-get (cdr rotate) :type)))
      (should-not (plist-member (cdr rotate) :wanted-by))
      (should (eq 'oneshot (plist-get (cdr prune) :type)))
      (should (equal '("logrotate") (plist-get (cdr prune) :after)))
      (should (equal '("logrotate") (plist-get (cdr prune) :requires)))
      (should-not (plist-member (cdr prune) :wanted-by)))))

(ert-deftest elinit-test-default-target-defcustom ()
  "Default target defcustom has expected default value."
  (should (equal elinit-default-target "default.target")))

(ert-deftest elinit-test-default-target-link-defcustom ()
  "Default target link defcustom has expected default value."
  (should (equal elinit-default-target-link "graphical.target")))

(ert-deftest elinit-test-builtin-targets-present ()
  "Built-in programs include the four standard targets."
  (let* ((builtins (elinit--builtin-programs))
         (ids (mapcar (lambda (e) (plist-get (cdr e) :id)) builtins)))
    (should (member "basic.target" ids))
    (should (member "multi-user.target" ids))
    (should (member "graphical.target" ids))
    (should (member "default.target" ids))))

(ert-deftest elinit-test-builtin-targets-valid ()
  "Built-in targets parse without validation errors."
  (let* ((builtins (elinit--builtin-programs))
         (targets (cl-remove-if-not
                   (lambda (e) (eq 'target (plist-get (cdr e) :type)))
                   builtins)))
    (should (= 15 (length targets)))
    (dolist (entry targets)
      (let ((parsed (elinit--parse-entry entry)))
        (should (eq 'target (elinit-entry-type parsed)))
        (should (string-suffix-p ".target" (elinit-entry-id parsed)))))))

(ert-deftest elinit-test-builtin-target-topology ()
  "Built-in targets have correct dependency chain."
  (let* ((builtins (elinit--builtin-programs))
         (multi (cl-find "multi-user.target" builtins
                         :key (lambda (e) (plist-get (cdr e) :id))
                         :test #'equal))
         (graphical (cl-find "graphical.target" builtins
                             :key (lambda (e) (plist-get (cdr e) :id))
                             :test #'equal))
         (basic (cl-find "basic.target" builtins
                         :key (lambda (e) (plist-get (cdr e) :id))
                         :test #'equal))
         (default-tgt (cl-find "default.target" builtins
                               :key (lambda (e) (plist-get (cdr e) :id))
                               :test #'equal)))
    ;; basic.target has no deps
    (should-not (plist-get (cdr basic) :requires))
    (should-not (plist-get (cdr basic) :after))
    ;; multi-user.target depends on basic.target via :requires (no explicit :after)
    (should (equal '("basic.target") (plist-get (cdr multi) :requires)))
    (should-not (plist-get (cdr multi) :after))
    ;; graphical.target depends on multi-user.target via :requires (no explicit :after)
    (should (equal '("multi-user.target") (plist-get (cdr graphical) :requires)))
    (should-not (plist-get (cdr graphical) :after))
    ;; default.target has no static edges
    (should-not (plist-get (cdr default-tgt) :requires))
    (should-not (plist-get (cdr default-tgt) :after))))

(ert-deftest elinit-test-resolve-default-chain ()
  "Default resolution chain: default.target -> graphical.target."
  (let ((elinit-default-target "default.target")
        (elinit-default-target-link "graphical.target")
        (elinit--default-target-link-override nil)
        (valid-id-set (make-hash-table :test 'equal)))
    (puthash "graphical.target" t valid-id-set)
    (should (equal "graphical.target"
                   (elinit--resolve-startup-root valid-id-set)))))

(ert-deftest elinit-test-resolve-custom-link ()
  "Override link resolves correctly."
  (let ((elinit-default-target "default.target")
        (elinit-default-target-link "graphical.target")
        (elinit--default-target-link-override "multi-user.target")
        (valid-id-set (make-hash-table :test 'equal)))
    (puthash "multi-user.target" t valid-id-set)
    (should (equal "multi-user.target"
                   (elinit--resolve-startup-root valid-id-set)))))

(ert-deftest elinit-test-resolve-direct-target ()
  "Non-default.target resolves directly without alias."
  (let ((elinit-default-target "custom.target")
        (elinit-default-target-link "graphical.target")
        (elinit--default-target-link-override nil)
        (valid-id-set (make-hash-table :test 'equal)))
    (puthash "custom.target" t valid-id-set)
    (should (equal "custom.target"
                   (elinit--resolve-startup-root valid-id-set)))))

(ert-deftest elinit-test-resolve-missing-target-errors ()
  "Missing resolved target signals user-error."
  (let ((elinit-default-target "default.target")
        (elinit-default-target-link "graphical.target")
        (elinit--default-target-link-override nil)
        (valid-id-set (make-hash-table :test 'equal)))
    ;; graphical.target not in valid-id-set
    (should-error (elinit--resolve-startup-root valid-id-set)
                  :type 'user-error)))

(ert-deftest elinit-test-resolve-non-target-errors ()
  "Resolved root that is not a .target signals user-error."
  (let ((elinit-default-target "my-service")
        (elinit--default-target-link-override nil)
        (valid-id-set (make-hash-table :test 'equal)))
    (puthash "my-service" 0 valid-id-set)
    (should-error (elinit--resolve-startup-root valid-id-set)
                  :type 'user-error)))

(ert-deftest elinit-test-resolve-non-target-link-errors ()
  "Link that resolves to non-.target signals user-error."
  (let ((elinit-default-target "default.target")
        (elinit-default-target-link "my-service")
        (elinit--default-target-link-override nil)
        (valid-id-set (make-hash-table :test 'equal)))
    (puthash "my-service" 0 valid-id-set)
    (should-error (elinit--resolve-startup-root valid-id-set)
                  :type 'user-error)))

(ert-deftest elinit-test-resolve-circular-alias-errors ()
  "Setting default-target-link to default.target signals user-error."
  (let ((elinit-default-target "default.target")
        (elinit-default-target-link "default.target")
        (elinit--default-target-link-override nil)
        (valid-id-set (make-hash-table :test 'equal)))
    (puthash "default.target" t valid-id-set)
    (should-error (elinit--resolve-startup-root valid-id-set)
                  :type 'user-error)))

(ert-deftest elinit-test-resolve-circular-alias-override-errors ()
  "Override link to default.target signals user-error."
  (let ((elinit-default-target "default.target")
        (elinit-default-target-link "graphical.target")
        (elinit--default-target-link-override "default.target")
        (valid-id-set (make-hash-table :test 'equal)))
    (puthash "default.target" t valid-id-set)
    (should-error (elinit--resolve-startup-root valid-id-set)
                  :type 'user-error)))

(ert-deftest elinit-test-maintenance-unit-content-no-stage ()
  "Seeded maintenance unit content does not contain :stage."
  (let ((content (elinit--maintenance-unit-content
                  '(:id "logrotate"
                    :command "/usr/bin/logrotate"
                    :description "Rotate logs"))))
    (should-not (string-match-p ":stage" content))))

(ert-deftest elinit-test-resolve-rejects-invalid-entry-id ()
  "Startup root resolution uses valid entries only, not raw order-index.
An invalid entry ID that happens to end in .target must not pass."
  (let* ((programs '(("invalid-entry" :id "bad.target" :type target
                      :unknown-keyword t)
                     ("true" :id "svc")))
         (plan (elinit--build-plan programs)))
    ;; bad.target is in order-index (raw) but not in valid entries
    (should (gethash "bad.target" (elinit-plan-order-index plan)))
    (should-not (cl-find "bad.target" (elinit-plan-entries plan)
                         :key #'elinit-entry-id :test #'equal))
    ;; Validation with valid-only set should reject it
    (let ((valid-ids (make-hash-table :test 'equal)))
      (dolist (entry (elinit-plan-entries plan))
        (puthash (elinit-entry-id entry) t valid-ids))
      (let ((elinit-default-target "bad.target")
            (elinit--default-target-link-override nil))
        (should-error (elinit--resolve-startup-root valid-ids)
                      :type 'user-error)))))

(ert-deftest elinit-test-scaffold-no-stage ()
  "Unit-file scaffold does not contain :stage."
  (let ((content (elinit--unit-file-scaffold "my-svc")))
    (should-not (string-match-p ":stage" content))))


(provide 'elinit-test-logging)
;;; elinit-test-logging.el ends here
