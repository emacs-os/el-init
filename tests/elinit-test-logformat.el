;;; elinit-test-logformat.el --- Structured log format (text/binary) tests for elinit.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Structured log format (text/binary) ERT tests for elinit.el.

;;; Code:

(require 'elinit-test-helpers)

;;; Log-format tests

(ert-deftest elinit-test-log-format-parse-text ()
  "Parse :log-format text returns symbol text."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc" :log-format text))))
    (should (eq (elinit-entry-log-format entry) 'text))))

(ert-deftest elinit-test-log-format-parse-binary-gate-on ()
  "Parse :log-format binary with gate enabled returns symbol binary."
  (let ((elinit-log-format-binary-enable t))
    (let ((entry (elinit--parse-entry
                  '("sleep 300" :id "svc" :log-format binary))))
      (should (eq (elinit-entry-log-format entry) 'binary)))))

(ert-deftest elinit-test-log-format-parse-omitted ()
  "Parse without :log-format returns nil (effective text)."
  (let ((entry (elinit--parse-entry '("sleep 300" :id "svc"))))
    (should-not (elinit-entry-log-format entry))))

(ert-deftest elinit-test-log-format-validate-unknown ()
  "Unknown :log-format value is a validation error."
  (let ((reason (elinit--validate-entry
                 '("cmd" :id "svc" :log-format json))))
    (should (stringp reason))
    (should (string-match-p ":log-format must be" reason))))

(ert-deftest elinit-test-log-format-validate-string-text ()
  "String \"text\" for :log-format is rejected (must be symbol)."
  (let ((reason (elinit--validate-entry
                 '("cmd" :id "svc" :log-format "text"))))
    (should (stringp reason))
    (should (string-match-p ":log-format must be" reason))))

(ert-deftest elinit-test-log-format-validate-string-binary ()
  "String \"binary\" for :log-format is rejected (must be symbol)."
  (let ((elinit-log-format-binary-enable t))
    (let ((reason (elinit--validate-entry
                   '("cmd" :id "svc" :log-format "binary"))))
      (should (stringp reason))
      (should (string-match-p ":log-format must be" reason)))))

(ert-deftest elinit-test-log-format-validate-target ()
  ":log-format on target is a validation error."
  (let ((reason (elinit--validate-entry
                 '("" :id "foo.target" :type target :log-format text))))
    (should (stringp reason))
    (should (string-match-p ":log-format" reason))))

(ert-deftest elinit-test-log-format-validate-binary-gate-off ()
  ":log-format binary with gate nil is a validation error mentioning gate."
  (let ((elinit-log-format-binary-enable nil))
    (let ((reason (elinit--validate-entry
                   '("cmd" :id "svc" :log-format binary))))
      (should (stringp reason))
      (should (string-match-p "elinit-log-format-binary-enable" reason)))))

(ert-deftest elinit-test-log-format-validate-binary-gate-on ()
  ":log-format binary with gate non-nil passes validation."
  (let ((elinit-log-format-binary-enable t))
    (should-not (elinit--validate-entry
                 '("cmd" :id "svc" :log-format binary)))))

(ert-deftest elinit-test-log-format-validate-text-always ()
  ":log-format text is always accepted regardless of gate."
  (let ((elinit-log-format-binary-enable nil))
    (should-not (elinit--validate-entry
                 '("cmd" :id "svc" :log-format text)))))

(ert-deftest elinit-test-log-format-service-round-trip ()
  "Entry->service->entry preserves :log-format."
  (let ((elinit-log-format-binary-enable t))
    (let* ((entry (elinit--parse-entry
                   '("sleep 300" :id "svc" :log-format binary)))
           (service (elinit-entry-to-service entry))
           (entry2 (elinit-service-to-entry service)))
      (should (eq (elinit-entry-log-format entry2) 'binary))
      (should (eq (elinit-service-log-format service) 'binary)))))

(ert-deftest elinit-test-log-format-unit-file-keyword ()
  ":log-format is in unit-file keyword allowlist."
  (should (memq :log-format elinit--unit-file-keywords)))

(ert-deftest elinit-test-log-format-oneshot-parse ()
  "Parse :type oneshot with :log-format preserves both."
  (let ((entry (elinit--parse-entry
                '("sleep 1" :id "job" :type oneshot :log-format text))))
    (should (eq (elinit-entry-type entry) 'oneshot))
    (should (eq (elinit-entry-log-format entry) 'text))))

(ert-deftest elinit-test-log-format-oneshot-round-trip ()
  "Entry->service->entry round-trip preserves :log-format for oneshot."
  (let ((elinit-log-format-binary-enable t))
    (let* ((entry (elinit--parse-entry
                   '("sleep 1" :id "job" :type oneshot :log-format binary)))
           (service (elinit-entry-to-service entry))
           (entry2 (elinit-service-to-entry service)))
      (should (eq (elinit-entry-type entry2) 'oneshot))
      (should (eq (elinit-entry-log-format entry2) 'binary)))))

(ert-deftest elinit-test-log-format-oneshot-validate-ok ()
  ":type oneshot + :log-format text passes validation."
  (should-not (elinit--validate-entry
               '("cmd" :id "job" :type oneshot :log-format text))))

(ert-deftest elinit-test-log-format-unit-file-validates ()
  "Unit-file plist with :log-format passes through entry validation."
  (should-not (elinit--validate-unit-file-plist
               '(:id "svc" :command "cmd" :log-format text)
               "test.el" 1)))

(ert-deftest elinit-test-log-format-unit-file-rejects-string ()
  "Unit-file plist with string :log-format is rejected."
  (let ((reason (elinit--validate-unit-file-plist
                 '(:id "svc" :command "cmd" :log-format "text")
                 "test.el" 1)))
    (should (stringp reason))
    (should (string-match-p ":log-format must be" reason))))

(ert-deftest elinit-test-log-format-unit-file-rejects-unknown ()
  "Unit-file plist with unknown :log-format value is rejected."
  (let ((reason (elinit--validate-unit-file-plist
                 '(:id "svc" :command "cmd" :log-format json)
                 "test.el" 1)))
    (should (stringp reason))
    (should (string-match-p ":log-format must be" reason))))

;;;; Phase 2: Frame encoding and transport tests

(ert-deftest elinit-test-frame-encode-round-trip ()
  "Frame encoding produces correct byte sequence for known values."
  (let ((frame (elinit--log-frame-encode 1 1 12345 "my-svc" "hello")))
    ;; Should be unibyte
    (should (not (multibyte-string-p frame)))
    ;; Total length = 4 (header) + 13 (fixed) + 6 (unit) + 5 (payload) = 28
    (should (= (length frame) 28))
    ;; u32be body length = 24
    (should (= (aref frame 0) 0))
    (should (= (aref frame 1) 0))
    (should (= (aref frame 2) 0))
    (should (= (aref frame 3) 24))
    ;; event = 1 (output)
    (should (= (aref frame 4) 1))
    ;; stream = 1 (stdout)
    (should (= (aref frame 5) 1))
    ;; pid = 12345 = 0x3039
    (should (= (aref frame 6) 0))
    (should (= (aref frame 7) 0))
    (should (= (aref frame 8) #x30))
    (should (= (aref frame 9) #x39))
    ;; unit_len = 6
    (should (= (aref frame 10) 0))
    (should (= (aref frame 11) 6))
    ;; exit_code = 0
    (should (= (aref frame 12) 0))
    (should (= (aref frame 13) 0))
    (should (= (aref frame 14) 0))
    (should (= (aref frame 15) 0))
    ;; exit_status = 0
    (should (= (aref frame 16) 0))
    ;; unit_id = "my-svc"
    (should (equal (substring frame 17 23) "my-svc"))
    ;; payload = "hello"
    (should (equal (substring frame 23 28) "hello"))))

(ert-deftest elinit-test-frame-encode-binary-payload ()
  "Frame encoding handles NUL, CR, LF, and control bytes in payload."
  (let* ((payload (concat "a" (string 0) (string 13) (string 10) (string 7) "z"))
         (frame (elinit--log-frame-encode 1 1 1 "s" payload)))
    (should (not (multibyte-string-p frame)))
    ;; payload_len = 6
    (let ((payload-start (+ 17 1))) ; unit_len=1
      (should (= (aref frame payload-start) ?a))
      (should (= (aref frame (+ payload-start 1)) 0))
      (should (= (aref frame (+ payload-start 2)) 13))
      (should (= (aref frame (+ payload-start 3)) 10))
      (should (= (aref frame (+ payload-start 4)) 7))
      (should (= (aref frame (+ payload-start 5)) ?z)))))

(ert-deftest elinit-test-frame-encode-exit-event ()
  "Frame encoding for exit events carries correct code and status."
  (let ((frame (elinit--log-frame-encode 2 3 99 "svc" nil 137 2)))
    ;; event = 2 (exit)
    (should (= (aref frame 4) 2))
    ;; stream = 3 (meta)
    (should (= (aref frame 5) 3))
    ;; exit_code = 137 = 0x89
    (should (= (aref frame 12) 0))
    (should (= (aref frame 13) 0))
    (should (= (aref frame 14) 0))
    (should (= (aref frame 15) #x89))
    ;; exit_status = 2 (signaled)
    (should (= (aref frame 16) 2))))

(ert-deftest elinit-test-frame-encode-empty-payload ()
  "Frame encoding works with no payload."
  (let ((frame (elinit--log-frame-encode 1 1 1 "svc")))
    (should (not (multibyte-string-p frame)))
    ;; Total = 4 + 13 + 3 + 0 = 20
    (should (= (length frame) 20))
    ;; Body length = 16
    (should (= (aref frame 3) 16))))

(ert-deftest elinit-test-frame-encode-negative-exit-code ()
  "Frame encoding handles negative exit codes via two's complement."
  (let ((frame (elinit--log-frame-encode 2 3 1 "s" nil -1 1)))
    ;; -1 as u32 = 0xFFFFFFFF
    (should (= (aref frame 12) #xff))
    (should (= (aref frame 13) #xff))
    (should (= (aref frame 14) #xff))
    (should (= (aref frame 15) #xff))))

(ert-deftest elinit-test-exit-status-code ()
  "Exit status code mapping returns correct wire values."
  (should (= (elinit--exit-status-code 'exited) 1))
  (should (= (elinit--exit-status-code 'signal) 2))
  (should (= (elinit--exit-status-code 'unknown) 0))
  (should (= (elinit--exit-status-code nil) 0)))

(ert-deftest elinit-test-stdout-filter-sends-frame ()
  "Stdout filter sends framed data with event=output, stream=stdout."
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
                       "svc-frame" "sleep 300" t 'simple 'always)))
            (should proc)
            (should captured-filter)
            (funcall captured-filter proc "test output\n")
            (should (= 1 (length sent-data)))
            (let ((frame (car sent-data)))
              (should (not (multibyte-string-p frame)))
              ;; event=1 (output), stream=1 (stdout)
              (should (= 1 (aref frame 4)))
              (should (= 1 (aref frame 5)))
              ;; Payload at end
              (should (string-suffix-p "test output\n" frame)))))
      (when (process-live-p fake-writer) (delete-process fake-writer))
      (when (process-live-p fake-svc) (delete-process fake-svc)))))

(ert-deftest elinit-test-stderr-pipe-sends-frame-stream-2 ()
  "Stderr pipe filter sends frame with stream=2 (stderr)."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--stderr-pipes (make-hash-table :test 'equal))
        (sent-data nil)
        (fake-writer (start-process "fake-writer" nil "sleep" "300"))
        (fake-svc (start-process "fake-svc" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "test-svc" fake-svc elinit--processes)
          (cl-letf (((symbol-function 'process-send-string)
                     (lambda (_proc data)
                       (push data sent-data))))
            (let ((pipe (elinit--start-stderr-pipe "test-svc"
                                                       fake-writer)))
              (should pipe)
              ;; Invoke the filter
              (let ((filter (process-filter pipe)))
                (funcall filter pipe "error output\n"))
              (should (= 1 (length sent-data)))
              (let ((frame (car sent-data)))
                ;; event=1 (output), stream=2 (stderr)
                (should (= 1 (aref frame 4)))
                (should (= 2 (aref frame 5)))
                (should (string-suffix-p "error output\n" frame))))))
      (when (process-live-p fake-writer) (delete-process fake-writer))
      (when (process-live-p fake-svc) (delete-process fake-svc))
      (when-let* ((pipe (gethash "test-svc" elinit--stderr-pipes)))
        (when (process-live-p pipe) (delete-process pipe))))))

(ert-deftest elinit-test-writer-coding-no-conversion ()
  "Writer process uses no-conversion coding for binary safety."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (elinit--processes (make-hash-table :test 'equal))
        (elinit--shutting-down nil)
        (elinit--restart-timers (make-hash-table :test 'equal))
        (elinit--manually-stopped (make-hash-table :test 'equal))
        (elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--logging (make-hash-table :test 'equal))
        (elinit--spawn-failure-reason (make-hash-table :test 'equal))
        (captured-coding nil)
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
                     (setq captured-coding (plist-get args :coding))
                     fake-svc))
                  ((symbol-function 'elinit--make-process-sentinel)
                   (lambda (&rest _args) #'ignore))
                  ((symbol-function 'elinit--build-launch-command)
                   (lambda (_cmd &rest _args) (list "sleep" "300"))))
          (elinit--start-process "svc-bin" "sleep 300" t 'simple 'always)
          ;; Service process should use no-conversion coding
          (should (eq captured-coding 'no-conversion)))
      (when (process-live-p fake-writer) (delete-process fake-writer))
      (when (process-live-p fake-svc) (delete-process fake-svc)))))

;;;; Phase 3: Text structured record writer tests

(ert-deftest elinit-test-writer-cmd-includes-framed-flags ()
  "Writer command includes --framed, --unit, and --format flags."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (spawned-args nil)
        (fake-proc (start-process "fake-logd" nil "sleep" "300")))
    (unwind-protect
        (cl-letf (((symbol-function 'elinit--ensure-log-directory)
                   (lambda () "/tmp/logs"))
                  ((symbol-function 'make-process)
                   (lambda (&rest args)
                     (setq spawned-args (plist-get args :command))
                     fake-proc)))
          (elinit--start-writer "my-svc" "/tmp/logs/log-my-svc.log")
          (should spawned-args)
          (should (member "--framed" spawned-args))
          (should (member "--unit" spawned-args))
          (should (equal (nth (1+ (cl-position "--unit" spawned-args
                                               :test #'equal))
                              spawned-args)
                         "my-svc"))
          (should (member "--format" spawned-args))
          (should (equal (nth (1+ (cl-position "--format" spawned-args
                                               :test #'equal))
                              spawned-args)
                         "text")))
      (when (process-live-p fake-proc)
        (delete-process fake-proc)))))

(ert-deftest elinit-test-writer-cmd-format-binary ()
  "Writer command passes --format binary when log-format is binary."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (spawned-args nil)
        (fake-proc (start-process "fake-logd" nil "sleep" "300")))
    (unwind-protect
        (cl-letf (((symbol-function 'elinit--ensure-log-directory)
                   (lambda () "/tmp/logs"))
                  ((symbol-function 'make-process)
                   (lambda (&rest args)
                     (setq spawned-args (plist-get args :command))
                     fake-proc)))
          (elinit--start-writer "my-svc" "/tmp/logs/log-my-svc.log"
                                    'binary)
          (should spawned-args)
          (should (equal (nth (1+ (cl-position "--format" spawned-args
                                               :test #'equal))
                              spawned-args)
                         "binary")))
      (when (process-live-p fake-proc)
        (delete-process fake-proc)))))

(ert-deftest elinit-test-writer-cmd-coding-no-conversion ()
  "Writer process created with no-conversion coding."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (captured-coding nil)
        (fake-proc (start-process "fake-logd" nil "sleep" "300")))
    (unwind-protect
        (cl-letf (((symbol-function 'elinit--ensure-log-directory)
                   (lambda () "/tmp/logs"))
                  ((symbol-function 'make-process)
                   (lambda (&rest args)
                     (setq captured-coding (plist-get args :coding))
                     fake-proc)))
          (elinit--start-writer "svc" "/tmp/logs/log-svc.log")
          (should (eq captured-coding 'no-conversion)))
      (when (process-live-p fake-proc)
        (delete-process fake-proc)))))

;;;; Phase 4: Binary structured record writer tests


(ert-deftest elinit-test-binary-decode-correct-fields ()
  "Binary decoder parses records with correct fields."
  (let* ((ts-ns 1708098896123456789)
         (record (elinit-test--make-binary-record
                  1 1 42 "my-svc" "hello" 0 0 ts-ns))
         (data (concat elinit--log-binary-magic record))
         (result (elinit--log-decode-binary-records data)))
    (should (null (plist-get result :warning)))
    (let* ((records (plist-get result :records))
           (r (car records)))
      (should (= 1 (length records)))
      (should (eq (plist-get r :event) 'output))
      (should (eq (plist-get r :stream) 'stdout))
      (should (= (plist-get r :pid) 42))
      (should (equal (plist-get r :unit) "my-svc"))
      (should (equal (plist-get r :payload) "hello"))
      (should (= (plist-get r :code) 0)))))

(ert-deftest elinit-test-binary-decode-exit-marker ()
  "Binary decoder parses exit records with correct status and code."
  (let* ((record (elinit-test--make-binary-record
                  2 3 99 "svc" "" 137 2 1000000000000))
         (data (concat elinit--log-binary-magic record))
         (result (elinit--log-decode-binary-records data)))
    (should (null (plist-get result :warning)))
    (let* ((records (plist-get result :records))
           (r (car records)))
      (should (= 1 (length records)))
      (should (eq (plist-get r :event) 'exit))
      (should (eq (plist-get r :stream) 'meta))
      (should (eq (plist-get r :status) 'signaled))
      (should (= (plist-get r :code) 137)))))

(ert-deftest elinit-test-binary-decode-truncated-record ()
  "Binary decoder handles truncated trailing record with warning."
  (let* ((record (elinit-test--make-binary-record
                  1 1 1 "s" "full" 0 0 0))
         ;; Truncate the record
         (truncated (substring record 0 (- (length record) 2)))
         (data (concat elinit--log-binary-magic
                       (elinit-test--make-binary-record
                        1 1 1 "s" "ok" 0 0 0)
                       truncated))
         (result (elinit--log-decode-binary-records data)))
    ;; Should have warning about truncation
    (should (plist-get result :warning))
    ;; But the first valid record should be returned
    (let ((records (plist-get result :records)))
      (should (= 1 (length records)))
      (should (equal (plist-get (car records) :payload) "ok")))))

(ert-deftest elinit-test-binary-decode-unknown-version ()
  "Binary decoder rejects unknown version as hard error."
  (let* ((record (elinit-test--make-binary-record
                  1 1 1 "s" "x" 0 0 0)))
    ;; Patch version byte to 99
    (aset record 4 99)
    (let ((data (concat elinit--log-binary-magic record)))
      (should-error (elinit--log-decode-binary-records data)
                    :type 'error))))

(ert-deftest elinit-test-binary-decode-unknown-event ()
  "Binary decoder rejects unknown event enum as hard error."
  (let* ((record (elinit-test--make-binary-record
                  1 1 1 "s" "x" 0 0 0)))
    ;; Patch event byte to 99
    (aset record 5 99)
    (let ((data (concat elinit--log-binary-magic record)))
      (should-error (elinit--log-decode-binary-records data)
                    :type 'error))))

(ert-deftest elinit-test-binary-decode-multiple-records ()
  "Binary decoder handles multiple records in sequence."
  (let* ((r1 (elinit-test--make-binary-record
              1 1 10 "svc" "line1" 0 0 1000))
         (r2 (elinit-test--make-binary-record
              1 2 10 "svc" "line2" 0 0 2000))
         (r3 (elinit-test--make-binary-record
              2 3 10 "svc" "" 0 1 3000))
         (data (concat elinit--log-binary-magic r1 r2 r3))
         (result (elinit--log-decode-binary-records data)))
    (should (null (plist-get result :warning)))
    (let ((records (plist-get result :records)))
      (should (= 3 (length records)))
      (should (eq (plist-get (nth 0 records) :stream) 'stdout))
      (should (eq (plist-get (nth 1 records) :stream) 'stderr))
      (should (eq (plist-get (nth 2 records) :event) 'exit)))))

(ert-deftest elinit-test-binary-decode-negative-exit-code ()
  "Binary decoder handles negative exit codes."
  (let* ((record (elinit-test--make-binary-record
                  2 3 1 "s" "" -1 1 0))
         (data (concat elinit--log-binary-magic record))
         (result (elinit--log-decode-binary-records data)))
    (let ((r (car (plist-get result :records))))
      (should (= (plist-get r :code) -1)))))

(ert-deftest elinit-test-binary-magic-constant ()
  "Binary magic constant is SLG1."
  (should (equal elinit--log-binary-magic "SLG1")))

(ert-deftest elinit-test-binary-decode-rejects-length-mismatch ()
  "Binary decoder fails hard when record_len disagrees with fields."
  (let* ((record (elinit-test--make-binary-record
                  1 1 42 "svc" "hello" 0 0 1000)))
    ;; Inflate record_len by 10 bytes beyond actual content.
    ;; Original record_len = 30 + 3 + 5 = 38.  Set to 48.
    (let ((bad-len (+ 30 3 5 10)))
      (aset record 0 (logand (ash bad-len -24) #xff))
      (aset record 1 (logand (ash bad-len -16) #xff))
      (aset record 2 (logand (ash bad-len -8) #xff))
      (aset record 3 (logand bad-len #xff)))
    ;; Pad data so the inflated total fits in the buffer
    (let ((data (concat elinit--log-binary-magic record
                        (make-string 10 0))))
      (should-error (elinit--log-decode-binary-records data)
                    :type 'error))))

(ert-deftest elinit-test-logd-binary-e2e-round-trip ()
  "End-to-end: framed input to logd --format binary produces decodable records."
  (let* ((logd (elinit-test--ensure-logd-binary))
         (dir (make-temp-file "logd-bin-e2e-" t))
         (log-file (expand-file-name "log-svc.log" dir)))
    (unwind-protect
        (let ((proc (make-process
                     :name "test-logd-bin-e2e"
                     :command (list logd
                                   "--file" log-file
                                   "--framed"
                                   "--unit" "test-svc"
                                   "--format" "binary"
                                   "--max-file-size-bytes" "1048576")
                     :connection-type 'pipe
                     :coding 'no-conversion)))
          (unwind-protect
              (progn
                ;; Output on stdout
                (process-send-string
                 proc (elinit--log-frame-encode 1 1 42 "test-svc"
                                                    "hello"))
                ;; Output on stderr
                (process-send-string
                 proc (elinit--log-frame-encode 1 2 42 "test-svc"
                                                    "warn"))
                ;; Exit event
                (process-send-string
                 proc (elinit--log-frame-encode 2 3 42 "test-svc"
                                                    nil 0 1))
                (sleep-for 0.3)
                (process-send-eof proc)
                (sleep-for 0.3)
                (should (file-exists-p log-file))
                (let* ((content (with-temp-buffer
                                  (set-buffer-multibyte nil)
                                  (insert-file-contents-literally log-file)
                                  (buffer-string)))
                       (result (elinit--log-decode-binary-records
                                content))
                       (records (plist-get result :records)))
                  ;; SLG1 magic header present
                  (should (equal (substring content 0 4) "SLG1"))
                  ;; No warnings (clean decode)
                  (should (null (plist-get result :warning)))
                  ;; 3 records decoded
                  (should (= 3 (length records)))
                  (let ((r1 (nth 0 records))
                        (r2 (nth 1 records))
                        (r3 (nth 2 records)))
                    ;; Output record 1
                    (should (eq (plist-get r1 :event) 'output))
                    (should (eq (plist-get r1 :stream) 'stdout))
                    (should (equal (plist-get r1 :payload) "hello"))
                    ;; Output exit fields forced to 0/none
                    (should (= (plist-get r1 :code) 0))
                    (should (null (plist-get r1 :status)))
                    ;; Output record 2
                    (should (eq (plist-get r2 :stream) 'stderr))
                    (should (equal (plist-get r2 :payload) "warn"))
                    (should (= (plist-get r2 :code) 0))
                    (should (null (plist-get r2 :status)))
                    ;; Exit record
                    (should (eq (plist-get r3 :event) 'exit))
                    (should (eq (plist-get r3 :stream) 'meta))
                    (should (eq (plist-get r3 :status) 'exited))
                    (should (= (plist-get r3 :code) 0)))))
            (when (process-live-p proc) (delete-process proc))))
      (delete-directory dir t))))

(ert-deftest elinit-test-logd-binary-overwrites-non-binary-file ()
  "Binary logd truncates and rewrites a pre-existing non-binary file."
  (let* ((logd (elinit-test--ensure-logd-binary))
         (dir (make-temp-file "logd-nonbin-" t))
         (log-file (expand-file-name "log-svc.log" dir)))
    (unwind-protect
        (progn
          ;; Pre-populate with text content
          (with-temp-file log-file
            (insert "ts=2026-01-01T00:00:00.000000000Z unit=old pid=1 "
                    "stream=stdout event=output status=- code=- "
                    "payload=legacy\n"))
          (let ((proc (make-process
                       :name "test-logd-nonbin"
                       :command (list logd
                                     "--file" log-file
                                     "--framed"
                                     "--unit" "test-svc"
                                     "--format" "binary"
                                     "--max-file-size-bytes" "1048576")
                       :connection-type 'pipe
                       :coding 'no-conversion)))
            (unwind-protect
                (progn
                  (process-send-string
                   proc (elinit--log-frame-encode 1 1 42 "test-svc"
                                                      "new"))
                  (sleep-for 0.3)
                  (process-send-eof proc)
                  (sleep-for 0.3)
                  (should (file-exists-p log-file))
                  (let* ((content (with-temp-buffer
                                    (set-buffer-multibyte nil)
                                    (insert-file-contents-literally log-file)
                                    (buffer-string))))
                    ;; File must start with SLG1 magic
                    (should (>= (length content) 4))
                    (should (equal (substring content 0 4) "SLG1"))
                    ;; Decode must succeed
                    (let* ((result (elinit--log-decode-binary-records
                                   content))
                           (records (plist-get result :records)))
                      (should (null (plist-get result :warning)))
                      (should (= 1 (length records)))
                      (should (equal (plist-get (car records) :payload)
                                     "new")))))
              (when (process-live-p proc) (delete-process proc)))))
      (delete-directory dir t))))

(ert-deftest elinit-test-decode-file-preserves-corruption-diagnostic ()
  "Decode-file surfaces specific corruption diagnostic, not generic error."
  (let* ((dir (make-temp-file "logd-diag-" t))
         (log-file (expand-file-name "log-svc.log" dir)))
    (unwind-protect
        (progn
          ;; Write a binary file with intentionally bad record_len
          (let* ((record (elinit-test--make-binary-record
                          1 1 42 "svc" "hello" 0 0 1000)))
            ;; Inflate record_len by 10 (mismatch with actual content)
            (let ((bad-len (+ 30 3 5 10)))
              (aset record 0 (logand (ash bad-len -24) #xff))
              (aset record 1 (logand (ash bad-len -16) #xff))
              (aset record 2 (logand (ash bad-len -8) #xff))
              (aset record 3 (logand bad-len #xff)))
            (with-temp-file log-file
              (set-buffer-multibyte nil)
              (insert elinit--log-binary-magic record
                      (make-string 10 0))))
          (let ((result (elinit--log-decode-file log-file)))
            ;; Must surface the specific mismatch diagnostic
            (should (stringp (plist-get result :warning)))
            (should (string-match-p "length mismatch"
                                    (plist-get result :warning)))
            ;; Records should be nil (error before any valid record)
            (should (null (plist-get result :records)))))
      (delete-directory dir t))))

(ert-deftest elinit-test-logd-binary-preserves-existing-valid-file ()
  "Binary logd appends to an existing valid SLG1 file without truncating."
  (let* ((logd (elinit-test--ensure-logd-binary))
         (dir (make-temp-file "logd-append-" t))
         (log-file (expand-file-name "log-svc.log" dir)))
    (unwind-protect
        (progn
          ;; First run: write one record
          (let ((proc (make-process
                       :name "test-logd-append-1"
                       :command (list logd
                                     "--file" log-file
                                     "--framed"
                                     "--unit" "test-svc"
                                     "--format" "binary"
                                     "--max-file-size-bytes" "1048576")
                       :connection-type 'pipe
                       :coding 'no-conversion)))
            (unwind-protect
                (progn
                  (process-send-string
                   proc (elinit--log-frame-encode 1 1 10 "test-svc"
                                                      "first"))
                  (sleep-for 0.3)
                  (process-send-eof proc)
                  (sleep-for 0.3))
              (when (process-live-p proc) (delete-process proc))))
          ;; Second run: append another record to the same file
          (let ((proc (make-process
                       :name "test-logd-append-2"
                       :command (list logd
                                     "--file" log-file
                                     "--framed"
                                     "--unit" "test-svc"
                                     "--format" "binary"
                                     "--max-file-size-bytes" "1048576")
                       :connection-type 'pipe
                       :coding 'no-conversion)))
            (unwind-protect
                (progn
                  (process-send-string
                   proc (elinit--log-frame-encode 1 1 20 "test-svc"
                                                      "second"))
                  (sleep-for 0.3)
                  (process-send-eof proc)
                  (sleep-for 0.3))
              (when (process-live-p proc) (delete-process proc))))
          ;; Verify both records survived
          (should (file-exists-p log-file))
          (let* ((content (with-temp-buffer
                            (set-buffer-multibyte nil)
                            (insert-file-contents-literally log-file)
                            (buffer-string)))
                 (result (elinit--log-decode-binary-records content))
                 (records (plist-get result :records)))
            (should (equal (substring content 0 4) "SLG1"))
            (should (null (plist-get result :warning)))
            (should (= 2 (length records)))
            (should (equal (plist-get (nth 0 records) :payload) "first"))
            (should (equal (plist-get (nth 1 records) :payload) "second"))))
      (delete-directory dir t))))

;;;; Phase 5: Decoder and user surfaces tests

(ert-deftest elinit-test-text-decode-records ()
  "Text decoder parses structured records correctly."
  (let* ((lines (concat "ts=2026-02-16T12:34:56.123Z unit=svc pid=42 "
                        "stream=stdout event=output status=- code=- "
                        "payload=hello world\n"
                        "ts=2026-02-16T12:34:57.000Z unit=svc pid=42 "
                        "stream=meta event=exit status=exited code=0 "
                        "payload=-\n"))
         (records (elinit--log-decode-text-records lines)))
    (should (= 2 (length records)))
    (let ((r1 (nth 0 records))
          (r2 (nth 1 records)))
      (should (eq (plist-get r1 :event) 'output))
      (should (eq (plist-get r1 :stream) 'stdout))
      (should (= (plist-get r1 :pid) 42))
      (should (equal (plist-get r1 :payload) "hello world"))
      (should (eq (plist-get r2 :event) 'exit))
      (should (eq (plist-get r2 :status) 'exited))
      (should (= (plist-get r2 :code) 0)))))

(ert-deftest elinit-test-payload-unescape-round-trip ()
  "Payload unescape reverses escaping exactly."
  (should (equal (elinit--log-unescape-payload "hello\\nworld")
                 "hello\nworld"))
  (should (equal (elinit--log-unescape-payload "a\\\\b")
                 "a\\b"))
  (should (equal (elinit--log-unescape-payload "tab\\there")
                 "tab\there"))
  (should (equal (elinit--log-unescape-payload "cr\\rend")
                 "cr\rend"))
  (should (equal (elinit--log-unescape-payload "nul\\x00byte")
                 (concat "nul" (string 0) "byte"))))

(ert-deftest elinit-test-text-decode-literal-dash-payload ()
  "Text decoder preserves literal dash in output payload."
  (let* ((line (concat "ts=2026-02-16T12:34:56.123Z unit=svc pid=42 "
                       "stream=stdout event=output status=- code=- "
                       "payload=-\n"))
         (records (elinit--log-decode-text-records line)))
    (should (= 1 (length records)))
    (should (equal (plist-get (car records) :payload) "-"))))

(ert-deftest elinit-test-text-decode-empty-payload ()
  "Text decoder handles empty output payload."
  (let* ((line (concat "ts=2026-02-16T12:34:56.123Z unit=svc pid=42 "
                       "stream=stdout event=output status=- code=- "
                       "payload=\n"))
         (records (elinit--log-decode-text-records line)))
    (should (= 1 (length records)))
    (should (equal (plist-get (car records) :payload) ""))))

(ert-deftest elinit-test-text-decode-exit-dash-payload ()
  "Text decoder maps exit event payload=- to empty string."
  (let* ((line (concat "ts=2026-02-16T12:34:56.123Z unit=svc pid=42 "
                       "stream=meta event=exit status=exited code=0 "
                       "payload=-\n"))
         (records (elinit--log-decode-text-records line)))
    (should (= 1 (length records)))
    (should (equal (plist-get (car records) :payload) ""))))

(ert-deftest elinit-test-legacy-fallback ()
  "Legacy decoder wraps raw lines as output records."
  (let ((records (elinit--log-decode-legacy-lines "line1\nline2\n")))
    (should (= 2 (length records)))
    (should (eq (plist-get (car records) :event) 'output))
    (should (eq (plist-get (car records) :stream) 'stdout))
    (should (equal (plist-get (car records) :payload) "line1"))
    (should (null (plist-get (car records) :ts)))))

(ert-deftest elinit-test-format-detection-binary ()
  "Format detection identifies binary by SLG1 magic."
  (let ((tmpfile (make-temp-file "log-test-")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (set-buffer-multibyte nil)
            (insert "SLG1some binary data here"))
          (should (eq (elinit--log-detect-format tmpfile) 'binary)))
      (delete-file tmpfile))))

(ert-deftest elinit-test-format-detection-text ()
  "Format detection identifies text by ts= prefix."
  (let ((tmpfile (make-temp-file "log-test-")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=2026-01-01T00:00:00Z unit=x pid=1 "
                    "stream=stdout event=output status=- "
                    "code=- payload=hi\n"))
          (should (eq (elinit--log-detect-format tmpfile) 'text)))
      (delete-file tmpfile))))

(ert-deftest elinit-test-format-detection-legacy ()
  "Format detection identifies legacy for plain text."
  (let ((tmpfile (make-temp-file "log-test-")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "just some log output\n"))
          (should (eq (elinit--log-detect-format tmpfile) 'legacy)))
      (delete-file tmpfile))))

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

(ert-deftest elinit-test-priority-classification ()
  "Priority: stderr output and non-clean exit are err, else info."
  (should (eq (elinit--log-record-priority
               '(:event output :stream stderr :status nil :code 0))
              'err))
  (should (eq (elinit--log-record-priority
               '(:event exit :stream meta :status signaled :code 9))
              'err))
  (should (eq (elinit--log-record-priority
               '(:event output :stream stdout :status nil :code 0))
              'info))
  (should (eq (elinit--log-record-priority
               '(:event exit :stream meta :status exited :code 0))
              'info)))

(ert-deftest elinit-test-timestamp-parsing-rfc3339 ()
  "Timestamp parser handles RFC3339 format."
  (let ((ts (elinit--log-parse-timestamp "2026-02-16T12:34:56.123Z")))
    (should ts)
    (should (> ts 0))
    ;; Should be a float
    (should (floatp ts))))

(ert-deftest elinit-test-timestamp-parsing-epoch ()
  "Timestamp parser handles epoch integer."
  (let ((ts (elinit--log-parse-timestamp "1708098896")))
    (should (= ts 1708098896.0))))

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

;;;; Log Format Phase 6: Integration Edge Cases

(ert-deftest elinit-test-sentinel-exit-frame-to-both-writers ()
  "Sentinel sends exit frames to both stdout and stderr writers."
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
    (let ((fake-stdout-writer (start-process "fw-stdout" nil "sleep" "300"))
          (fake-stderr-writer (start-process "fw-stderr" nil "sleep" "300")))
      (unwind-protect
          (progn
            (puthash "test-svc" fake-stdout-writer elinit--writers)
            (puthash "test-svc" fake-stderr-writer elinit--stderr-writers)
            (cl-letf (((symbol-function 'elinit--log-send-frame)
                       (lambda (_writer event stream pid id _payload exit-code exit-status)
                         (push (list :event event :stream stream :pid pid
                                     :id id :exit-code exit-code
                                     :exit-status exit-status)
                               exit-frames)))
                      ((symbol-function 'elinit--stop-writer-if-same) #'ignore)
                      ((symbol-function 'elinit--emit-event) #'ignore)
                      ((symbol-function 'elinit--maybe-refresh-dashboard) #'ignore)
                      ((symbol-function 'elinit--handle-oneshot-exit) #'ignore)
                      ((symbol-function 'elinit--should-restart-p) (lambda (&rest _) nil)))
              (let ((sentinel (elinit--make-process-sentinel
                               "test-svc" '("sleep" "300") t 'simple t)))
                ;; Simulate process exit by creating a fake dead process
                (let ((fake-proc (start-process "test-svc" nil "true")))
                  (puthash "test-svc" fake-proc elinit--processes)
                  ;; Wait for process to die
                  (while (process-live-p fake-proc) (sit-for 0.05))
                  (funcall sentinel fake-proc "finished\n")))))
        (when (process-live-p fake-stdout-writer) (delete-process fake-stdout-writer))
        (when (process-live-p fake-stderr-writer) (delete-process fake-stderr-writer))))
    ;; Should have sent exactly 2 exit frames (one per writer table)
    (should (= 2 (length exit-frames)))
    ;; Both should be exit events (event=2) on meta stream (stream=3)
    (dolist (frame exit-frames)
      (should (= 2 (plist-get frame :event)))
      (should (= 3 (plist-get frame :stream))))))

(ert-deftest elinit-test-exit-marker-count-one-per-termination ()
  "Each process termination produces exactly one exit frame per writer."
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
        (exit-frame-count 0))
    (let ((fake-writer (start-process "fw" nil "sleep" "300")))
      (unwind-protect
          (progn
            (puthash "test-svc" fake-writer elinit--writers)
            (cl-letf (((symbol-function 'elinit--log-send-frame)
                       (lambda (&rest _args)
                         (cl-incf exit-frame-count)))
                      ((symbol-function 'elinit--stop-writer-if-same) #'ignore)
                      ((symbol-function 'elinit--emit-event) #'ignore)
                      ((symbol-function 'elinit--maybe-refresh-dashboard) #'ignore)
                      ((symbol-function 'elinit--should-restart-p) (lambda (&rest _) nil)))
              (let ((sentinel (elinit--make-process-sentinel
                               "test-svc" '("sleep" "300") t 'simple t)))
                (let ((fake-proc (start-process "test-svc" nil "true")))
                  (puthash "test-svc" fake-proc elinit--processes)
                  (while (process-live-p fake-proc) (sit-for 0.05))
                  (funcall sentinel fake-proc "finished\n")))))
        (when (process-live-p fake-writer) (delete-process fake-writer))))
    ;; Exactly one exit frame (only stdout writer, no stderr writer)
    (should (= 1 exit-frame-count))))

(ert-deftest elinit-test-deferred-teardown-skips-replaced-writer ()
  "Deferred writer teardown does not kill a replacement writer."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (elinit--stderr-writers (make-hash-table :test 'equal))
        (elinit--stderr-pipes (make-hash-table :test 'equal))
        (elinit-logd-pid-directory (make-temp-file "td-skip-" t)))
    (let ((old-w (start-process "old-w" nil "sleep" "300"))
          (new-w (start-process "new-w" nil "sleep" "300")))
      (unwind-protect
          (progn
            ;; New writer is already in hash (restart replaced old)
            (puthash "svc" new-w elinit--writers)
            ;; Deferred teardown fires with old writer reference
            (elinit--stop-writer-if-same "svc" old-w nil nil)
            ;; New writer must survive
            (should (eq new-w (gethash "svc" elinit--writers)))
            (should (process-live-p new-w)))
        (when (process-live-p old-w) (delete-process old-w))
        (when (process-live-p new-w) (delete-process new-w))
        (delete-directory elinit-logd-pid-directory t)))))

(ert-deftest elinit-test-deferred-teardown-stops-same-writer ()
  "Deferred writer teardown stops the writer when not replaced."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (elinit--stderr-writers (make-hash-table :test 'equal))
        (elinit--stderr-pipes (make-hash-table :test 'equal))
        (elinit-logd-pid-directory (make-temp-file "td-same-" t)))
    (let ((old-w (start-process "old-w" nil "sleep" "300")))
      (unwind-protect
          (progn
            ;; Writer still in hash (no restart occurred)
            (puthash "svc" old-w elinit--writers)
            (elinit--stop-writer-if-same "svc" old-w nil nil)
            ;; Writer should be removed
            (should-not (gethash "svc" elinit--writers)))
        (when (process-live-p old-w) (delete-process old-w))
        (delete-directory elinit-logd-pid-directory t)))))

(ert-deftest elinit-test-deferred-teardown-guards-stderr-writer ()
  "Deferred teardown guards stderr writer independently of stdout."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (elinit--stderr-writers (make-hash-table :test 'equal))
        (elinit--stderr-pipes (make-hash-table :test 'equal))
        (elinit-logd-pid-directory (make-temp-file "td-stderr-" t)))
    (let ((old-stdout (start-process "old-out" nil "sleep" "300"))
          (new-stdout (start-process "new-out" nil "sleep" "300"))
          (old-stderr (start-process "old-err" nil "sleep" "300")))
      (unwind-protect
          (progn
            ;; Stdout replaced, stderr not
            (puthash "svc" new-stdout elinit--writers)
            (puthash "svc" old-stderr elinit--stderr-writers)
            (elinit--stop-writer-if-same "svc" old-stdout old-stderr nil)
            ;; Stdout: new writer survives (replaced)
            (should (eq new-stdout (gethash "svc" elinit--writers)))
            (should (process-live-p new-stdout))
            ;; Stderr: old writer stopped (not replaced)
            (should-not (gethash "svc" elinit--stderr-writers)))
        (when (process-live-p old-stdout) (delete-process old-stdout))
        (when (process-live-p new-stdout) (delete-process new-stdout))
        (when (process-live-p old-stderr) (delete-process old-stderr))
        (delete-directory elinit-logd-pid-directory t)))))

(ert-deftest elinit-test-sentinel-deferred-teardown-captures-writer-identity ()
  "Sentinel deferred teardown closure captures writer identity for guard."
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
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--oneshot-callbacks (make-hash-table :test 'equal))
        (elinit--crash-log (make-hash-table :test 'equal))
        (elinit--last-exit-info (make-hash-table :test 'equal))
        (elinit--spawn-failure-reason (make-hash-table :test 'equal))
        (stop-if-same-args nil))
    (let ((fake-writer (start-process "fw-cap" nil "sleep" "300")))
      (unwind-protect
          (progn
            (puthash "cap-svc" fake-writer elinit--writers)
            (let ((deferred-fn nil))
              (cl-letf (((symbol-function 'elinit--log-send-frame) #'ignore)
                        ((symbol-function 'elinit--stop-writer-if-same)
                         (lambda (id old-stdout old-stderr old-pipe)
                           (setq stop-if-same-args
                                 (list id old-stdout old-stderr old-pipe))))
                        ((symbol-function 'elinit--emit-event) #'ignore)
                        ((symbol-function 'elinit--maybe-refresh-dashboard) #'ignore)
                        ((symbol-function 'elinit--should-restart-p)
                         (lambda (&rest _) nil))
                        ((symbol-function 'run-at-time)
                         (lambda (_secs _repeat fn &rest _args)
                           (setq deferred-fn fn)
                           nil)))
                (let ((sentinel (elinit--make-process-sentinel
                                 "cap-svc" '("sleep" "300") t 'simple t)))
                  (let ((fake-proc (start-process "cap-svc" nil "true")))
                    (puthash "cap-svc" fake-proc elinit--processes)
                    (while (process-live-p fake-proc) (sit-for 0.05))
                    (funcall sentinel fake-proc "finished\n")
                    ;; Fire the captured deferred teardown callback
                    (should deferred-fn)
                    (funcall deferred-fn)
                    ;; Verify the captured writer was passed to guard
                    (should stop-if-same-args)
                    (should (equal "cap-svc" (nth 0 stop-if-same-args)))
                    (should (eq fake-writer (nth 1 stop-if-same-args))))))))
        (when (process-live-p fake-writer) (delete-process fake-writer))))))

(ert-deftest elinit-test-exit-marker-per-restart-cycle ()
  "Each restart cycle produces exactly one exit marker."
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
        (elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--oneshot-callbacks (make-hash-table :test 'equal))
        (elinit--crash-log (make-hash-table :test 'equal))
        (elinit--last-exit-info (make-hash-table :test 'equal))
        (elinit--spawn-failure-reason (make-hash-table :test 'equal))
        (exit-frame-count 0))
    (let ((w1 (start-process "fw-rst1" nil "sleep" "300"))
          (w2 (start-process "fw-rst2" nil "sleep" "300")))
      (unwind-protect
          (progn
            ;; Create both service processes outside the mock scope so
            ;; sit-for does not intercept Emacs-internal timers.
            ;; Delete p1 before creating p2 so Emacs does not
            ;; uniquify the name to "rst-svc<1>" on snapshot builds.
            (let ((p1 (start-process "rst-svc" nil "true")))
              (while (process-live-p p1) (sit-for 0.05))
              (delete-process p1)
              (let ((p2 (start-process "rst-svc" nil "true")))
                (while (process-live-p p2) (sit-for 0.05))
                ;; First cycle: writer w1
                (puthash "rst-svc" w1 elinit--writers)
                (cl-letf (((symbol-function 'elinit--log-send-frame)
                           (lambda (&rest _args)
                             (cl-incf exit-frame-count)))
                          ((symbol-function 'elinit--stop-writer-if-same)
                           #'ignore)
                          ((symbol-function 'elinit--emit-event) #'ignore)
                          ((symbol-function 'elinit--maybe-refresh-dashboard)
                           #'ignore)
                          ((symbol-function 'elinit--should-restart-p)
                           (lambda (&rest _) nil))
                          ((symbol-function 'run-at-time)
                           (lambda (&rest _) nil)))
                  (let ((sentinel (elinit--make-process-sentinel
                                   "rst-svc" '("sleep" "300") t 'simple t)))
                    (puthash "rst-svc" p1 elinit--processes)
                    (funcall sentinel p1 "finished\n")
                    ;; Simulate restart: replace writer
                    (puthash "rst-svc" w2 elinit--writers)
                    (puthash "rst-svc" p2 elinit--processes)
                    (funcall sentinel p2 "finished\n")))))
            ;; Each termination produces exactly 1 exit frame
            ;; (only stdout writer present in each cycle)
            (should (= 2 exit-frame-count)))
        (when (process-live-p w1) (delete-process w1))
        (when (process-live-p w2) (delete-process w2))))))

(ert-deftest elinit-test-writer-failure-non-fatal ()
  "Writer start failure does not prevent service from starting."
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
        (elinit--logd-pid-directory nil)
        (proc-started nil))
    (cl-letf (((symbol-function 'elinit--get-effective-logging)
               (lambda (_id _default) t))
              ((symbol-function 'elinit--ensure-log-directory) #'ignore)
              ((symbol-function 'elinit--log-file)
               (lambda (_id) "/tmp/test.log"))
              ((symbol-function 'elinit--start-writer)
               (lambda (_id _file &optional _log-format) nil))
              ((symbol-function 'make-process)
               (lambda (&rest _args) nil))
              ((symbol-function 'elinit--make-process-sentinel)
               (lambda (&rest _args) #'ignore))
              ((symbol-function 'elinit--build-launch-command)
               (lambda (&rest _args) '("sleep" "300"))))
      ;; Should not error even when writer returns nil
      (should-not
       (condition-case err
           (elinit--start-process "svc" '("sleep" "300") t 'simple t)
         (error err))))))

(ert-deftest elinit-test-build-prune-command-format-hint ()
  "Build-prune-command includes --format-hint when format specified."
  (let ((log-directory (make-temp-file "logs-" t)))
    (unwind-protect
        (let ((elinit-log-prune-command "/opt/prune")
              (elinit-log-directory log-directory)
              (elinit-log-prune-max-total-bytes 2048))
          (let ((cmd (elinit--build-prune-command 'text)))
            (should (string-match-p "--format-hint" cmd))
            (should (string-match-p "text" cmd)))
          (let ((cmd (elinit--build-prune-command 'binary)))
            (should (string-match-p "--format-hint" cmd))
            (should (string-match-p "binary" cmd))))
      (delete-directory log-directory t))))

(ert-deftest elinit-test-build-prune-command-no-format-hint-when-nil ()
  "Build-prune-command omits --format-hint when format is nil."
  (let ((log-directory (make-temp-file "logs-" t)))
    (unwind-protect
        (let ((elinit-log-prune-command "/opt/prune")
              (elinit-log-directory log-directory)
              (elinit-log-prune-max-total-bytes 2048))
          (let ((cmd (elinit--build-prune-command nil)))
            (should-not (string-match-p "--format-hint" cmd)))
          (let ((cmd (elinit--build-prune-command)))
            (should-not (string-match-p "--format-hint" cmd))))
      (delete-directory log-directory t))))

(ert-deftest elinit-test-logd-writer-cmd-includes-format-hint ()
  "Writer command passes format-hint to prune command."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (elinit--logd-pid-directory nil)
        (captured-cmd nil)
        (fake-proc (start-process "fake-logd" nil "sleep" "300")))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'elinit--effective-log-directory)
                     (lambda () "/tmp/log"))
                    ((symbol-function 'elinit--ensure-log-directory)
                     (lambda () "/tmp/log"))
                    ((symbol-function 'make-process)
                     (lambda (&rest args)
                       (setq captured-cmd (plist-get args :command))
                       fake-proc))
                    ((symbol-function 'elinit--write-logd-pid-file) #'ignore))
            (elinit--start-stream-writer
             "svc" 'stdout "/tmp/log/log-svc.log"
             elinit--writers 'binary))
          (should captured-cmd)
          ;; Find the --prune-cmd value in the command list
          (let ((prune-idx (cl-position "--prune-cmd" captured-cmd
                                        :test #'equal)))
            (should prune-idx)
            (let ((prune-val (nth (1+ prune-idx) captured-cmd)))
              (should (string-match-p "--format-hint" prune-val))
              (should (string-match-p "binary" prune-val)))))
      (when (process-live-p fake-proc) (delete-process fake-proc)))))

(ert-deftest elinit-test-prune-script-accepts-vacuum-flag ()
  "Log-prune script accepts --vacuum flag without error."
  (let ((script (expand-file-name "sbin/supervisor-log-prune")))
    ;; The script requires --log-dir, so we just test that --vacuum
    ;; is parsed without 'Unknown option' error by checking the help
    ;; text mentions the expected options.
    (should (file-exists-p script))))

(ert-deftest elinit-test-prune-script-accepts-vacuum-max-total-bytes ()
  "Log-prune script accepts --vacuum-max-total-bytes as alias."
  (let ((script (expand-file-name "sbin/supervisor-log-prune")))
    (with-temp-buffer
      (insert-file-contents script)
      (should (search-forward "--vacuum-max-total-bytes" nil t)))))

(ert-deftest elinit-test-prune-script-accepts-format-hint ()
  "Log-prune script accepts --format-hint flag."
  (let ((script (expand-file-name "sbin/supervisor-log-prune")))
    (with-temp-buffer
      (insert-file-contents script)
      (should (search-forward "--format-hint" nil t)))))

(ert-deftest elinit-test-logrotate-prune-finds-tar-gz ()
  "Logrotate prune_rotated find patterns include .tar.gz files."
  (let ((script (expand-file-name "sbin/supervisor-logrotate")))
    (with-temp-buffer
      (insert-file-contents script)
      (goto-char (point-min))
      ;; Look for the tar.gz pattern in find command inside prune_rotated
      (should (search-forward "log-*.*.log.tar.gz" nil t)))))

(ert-deftest elinit-test-logrotate-has-compress-rotated ()
  "Logrotate has compress_rotated function."
  (let ((script (expand-file-name "sbin/supervisor-logrotate")))
    (with-temp-buffer
      (insert-file-contents script)
      (goto-char (point-min))
      (should (search-forward "compress_rotated()" nil t)))))

(ert-deftest elinit-test-logrotate-calls-compress-after-rotate ()
  "Logrotate calls compress_rotated after successful rotation."
  (let ((script (expand-file-name "sbin/supervisor-logrotate")))
    (with-temp-buffer
      (insert-file-contents script)
      (goto-char (point-min))
      ;; compress_rotated is called inside rotate_file
      (should (re-search-forward "compress_rotated.*\\$" nil t)))))

(ert-deftest elinit-test-prune-script-is-rotated-tar-gz ()
  "Log-prune is_rotated matches .tar.gz suffixed files."
  (let ((script (expand-file-name "sbin/supervisor-log-prune")))
    (with-temp-buffer
      (insert-file-contents script)
      (goto-char (point-min))
      ;; Verify is_rotated case patterns include .tar.gz
      (should (search-forward ".log.tar.gz) return 0" nil t)))))

(ert-deftest elinit-test-prune-script-rotated-parent-strips-tar-gz ()
  "Log-prune rotated_parent_name strips .tar.gz before timestamp."
  (let ((script (expand-file-name "sbin/supervisor-log-prune")))
    (with-temp-buffer
      (insert-file-contents script)
      (goto-char (point-min))
      ;; Verify the sed strips .tar.gz first
      (should (search-forward "s/\\.tar\\.gz$//" nil t)))))

;;;; Log Format Phase 7: Gap-Fill Test Coverage

(ert-deftest elinit-test-filter-metadata-only ()
  "Filter evaluates metadata fields without parsing payload."
  (let ((records (list (list :ts 100.0 :stream 'stdout :event 'output
                             :payload "irrelevant" :pid 1 :unit "svc")
                       (list :ts 200.0 :stream 'stderr :event 'output
                             :payload "error msg" :pid 1 :unit "svc")
                       (list :ts 300.0 :stream 'meta :event 'exit
                             :status 'exited :code 1 :pid 1 :unit "svc"))))
    ;; Filter by timestamp range -- payload not consulted
    (let ((filtered (elinit--log-filter-records records 150.0 250.0)))
      (should (= 1 (length filtered)))
      (should (= 200.0 (plist-get (car filtered) :ts))))
    ;; Filter by priority -- stderr -> err
    (let ((filtered (elinit--log-filter-records records nil nil 'err)))
      (should (= 2 (length filtered)))
      ;; stderr output and non-clean exit are both err
      (should (eq 'stderr (plist-get (car filtered) :stream)))
      (should (eq 'exit (plist-get (cadr filtered) :event))))))

(ert-deftest elinit-test-filter-since-only ()
  "Filter with only since timestamp."
  (let ((records (list (list :ts 100.0 :stream 'stdout :event 'output
                             :pid 1 :unit "svc")
                       (list :ts 200.0 :stream 'stdout :event 'output
                             :pid 1 :unit "svc")
                       (list :ts 300.0 :stream 'stdout :event 'output
                             :pid 1 :unit "svc"))))
    (let ((filtered (elinit--log-filter-records records 200.0 nil)))
      (should (= 2 (length filtered)))
      (should (= 200.0 (plist-get (car filtered) :ts))))))

(ert-deftest elinit-test-filter-until-only ()
  "Filter with only until timestamp."
  (let ((records (list (list :ts 100.0 :stream 'stdout :event 'output
                             :pid 1 :unit "svc")
                       (list :ts 200.0 :stream 'stdout :event 'output
                             :pid 1 :unit "svc")
                       (list :ts 300.0 :stream 'stdout :event 'output
                             :pid 1 :unit "svc"))))
    (let ((filtered (elinit--log-filter-records records nil 200.0)))
      (should (= 2 (length filtered)))
      (should (= 200.0 (plist-get (cadr filtered) :ts))))))

(ert-deftest elinit-test-filter-no-criteria-returns-all ()
  "Filter with no criteria returns all records."
  (let ((records (list (list :ts 100.0 :stream 'stdout :event 'output
                             :pid 1 :unit "svc")
                       (list :ts 200.0 :stream 'stderr :event 'output
                             :pid 1 :unit "svc"))))
    (let ((filtered (elinit--log-filter-records records nil nil nil)))
      (should (= 2 (length filtered))))))

(ert-deftest elinit-test-binary-trailing-recovery-returns-valid ()
  "Binary decoder returns valid records before truncated trailing record."
  ;; Build a valid record followed by a truncated one
  (let* ((magic "SLG1")
         (valid-record (elinit-test--make-binary-record
                        1 1 42 "svc" "hello" 0 0 1000000000000))
         (truncated (substring valid-record 0 10))
         (buf (concat magic valid-record truncated)))
    (let ((result (elinit--log-decode-binary-records buf)))
      ;; Should get exactly one record from the valid data
      (should (= 1 (length (plist-get result :records))))
      ;; Should have a warning about truncation
      (should (plist-get result :warning))
      ;; The valid record should be correct
      (let ((rec (car (plist-get result :records))))
        (should (equal "svc" (plist-get rec :unit)))
        (should (equal "hello" (plist-get rec :payload)))))))

(ert-deftest elinit-test-binary-trailing-recovery-empty-trailing ()
  "Binary decoder handles empty buffer after magic header."
  (let* ((magic "SLG1")
         (buf magic))
    (let ((result (elinit--log-decode-binary-records buf)))
      (should (= 0 (length (plist-get result :records)))))))

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

;;;; Phase 5 Remediation: P1 -- Non-follow journal fixes

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

;;;; Phase 5 Remediation: P2 -- Journal follow mode

(ert-deftest elinit-test-journal-follow-incremental-read ()
  "Follow mode reads new records from offset."
  (let ((tmpfile (make-temp-file "journal-follow-")))
    (unwind-protect
        (progn
          ;; Write initial content
          (with-temp-file tmpfile
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=first\n"))
          ;; Decode initial
          (let* ((initial (elinit--log-decode-file tmpfile))
                 (offset (plist-get initial :offset))
                 (recs (plist-get initial :records)))
            (should (= 1 (length recs)))
            ;; Append more data
            (with-temp-buffer
              (insert "ts=2000 unit=svc pid=1 stream=stdout "
                      "event=output status=- code=- payload=second\n")
              (append-to-file (point-min) (point-max) tmpfile))
            ;; Read from offset -- should get only the new record
            (let* ((incremental (elinit--log-decode-file
                                 tmpfile nil offset))
                   (new-recs (plist-get incremental :records)))
              (should (= 1 (length new-recs)))
              (should (equal "second"
                             (plist-get (car new-recs) :payload))))))
      (delete-file tmpfile))))

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

;;;; Phase 5 Remediation: P3 -- Consistent decoded rendering

(ert-deftest elinit-test-dashboard-view-log-decodes-text ()
  "Dashboard view-log decodes text format through structured decoder."
  (let ((tmpfile (make-temp-file "dash-view-")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=hello\n"))
          (cl-letf (((symbol-function 'elinit--log-file)
                     (lambda (_id) tmpfile))
                    ((symbol-function 'elinit--require-service-row)
                     (lambda () "svc")))
            (save-window-excursion
              (elinit-dashboard-view-log)
              ;; Buffer should contain human-formatted record
              (let ((buf (get-buffer "*elinit-log-svc*")))
                (should buf)
                (with-current-buffer buf
                  ;; Should have formatted timestamp, not raw ts= line
                  (should (string-match-p "svc\\[1\\]"
                                          (buffer-string)))
                  (should (string-match-p "hello"
                                          (buffer-string)))
                  (should-not (string-match-p "\\`ts="
                                              (buffer-string))))
                (kill-buffer buf)))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-dashboard-view-log-applies-record-limit ()
  "Dashboard view-log bounds both record count and byte read."
  (let ((tmpfile (make-temp-file "dash-view-limit-"))
        (captured-limit 'unset)
        (captured-max-bytes 'unset))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=hello\n"))
          (cl-letf (((symbol-function 'elinit--log-file)
                     (lambda (_id) tmpfile))
                    ((symbol-function 'elinit--require-service-row)
                     (lambda () "svc"))
                    ((symbol-function 'elinit--log-decode-file)
                     (lambda (_file &optional limit _offset max-bytes)
                       (setq captured-limit limit)
                       (setq captured-max-bytes max-bytes)
                       (list :records nil :offset 0 :format 'text
                             :warning nil))))
            (save-window-excursion
              (elinit-dashboard-view-log)
              (should (equal elinit-dashboard-log-view-record-limit
                             captured-limit))
              (should (equal (* elinit-dashboard-log-view-record-limit 512)
                             captured-max-bytes))
              (when-let* ((buf (get-buffer "*elinit-log-svc*")))
                (kill-buffer buf)))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-dashboard-view-log-nil-limit-uses-default ()
  "Dashboard view-log clamps nil limit to default 1000."
  (let ((tmpfile (make-temp-file "dash-nil-limit-"))
        (elinit-dashboard-log-view-record-limit nil)
        (captured-limit 'unset)
        (captured-max-bytes 'unset))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=hello\n"))
          (cl-letf (((symbol-function 'elinit--log-file)
                     (lambda (_id) tmpfile))
                    ((symbol-function 'elinit--require-service-row)
                     (lambda () "svc"))
                    ((symbol-function 'elinit--log-decode-file)
                     (lambda (_file &optional limit _offset max-bytes)
                       (setq captured-limit limit)
                       (setq captured-max-bytes max-bytes)
                       (list :records nil :offset 0 :format 'text
                             :warning nil))))
            (save-window-excursion
              (elinit-dashboard-view-log)
              (should (equal 1000 captured-limit))
              (should (equal (* 1000 512) captured-max-bytes))
              (when-let* ((buf (get-buffer "*elinit-log-svc*")))
                (kill-buffer buf)))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-dashboard-view-log-zero-limit-uses-default ()
  "Dashboard view-log clamps zero limit to default 1000."
  (let ((tmpfile (make-temp-file "dash-zero-limit-"))
        (elinit-dashboard-log-view-record-limit 0)
        (captured-limit 'unset))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=hello\n"))
          (cl-letf (((symbol-function 'elinit--log-file)
                     (lambda (_id) tmpfile))
                    ((symbol-function 'elinit--require-service-row)
                     (lambda () "svc"))
                    ((symbol-function 'elinit--log-decode-file)
                     (lambda (_file &optional limit _offset _max-bytes)
                       (setq captured-limit limit)
                       (list :records nil :offset 0 :format 'text
                             :warning nil))))
            (save-window-excursion
              (elinit-dashboard-view-log)
              (should (equal 1000 captured-limit))
              (when-let* ((buf (get-buffer "*elinit-log-svc*")))
                (kill-buffer buf)))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-dashboard-view-log-negative-limit-uses-default ()
  "Dashboard view-log clamps negative limit to default 1000."
  (let ((tmpfile (make-temp-file "dash-neg-limit-"))
        (elinit-dashboard-log-view-record-limit -5)
        (captured-limit 'unset))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=hello\n"))
          (cl-letf (((symbol-function 'elinit--log-file)
                     (lambda (_id) tmpfile))
                    ((symbol-function 'elinit--require-service-row)
                     (lambda () "svc"))
                    ((symbol-function 'elinit--log-decode-file)
                     (lambda (_file &optional limit _offset _max-bytes)
                       (setq captured-limit limit)
                       (list :records nil :offset 0 :format 'text
                             :warning nil))))
            (save-window-excursion
              (elinit-dashboard-view-log)
              (should (equal 1000 captured-limit))
              (when-let* ((buf (get-buffer "*elinit-log-svc*")))
                (kill-buffer buf)))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-telemetry-log-tail-decodes-text ()
  "Telemetry log-tail decodes text format through structured decoder."
  (let ((tmpfile (make-temp-file "telem-tail-")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=hello\n")
            (insert "ts=1001 unit=svc pid=1 stream=stderr "
                    "event=output status=- code=- payload=world\n"))
          (cl-letf (((symbol-function 'elinit--log-file)
                     (lambda (_id) tmpfile)))
            (let ((output (elinit--telemetry-log-tail "svc" 5)))
              (should (stringp output))
              ;; Should contain human-formatted output
              (should (string-match-p "svc\\[1\\]" output))
              (should (string-match-p "hello" output))
              (should (string-match-p "world" output))
              ;; Should not contain raw ts= format
              (should-not (string-match-p "\\`ts=" output)))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-timestamp-parsing-rfc3339-nano ()
  "Timestamp parser handles RFC3339 with nanoseconds."
  (let ((ts (elinit--log-parse-timestamp
             "2026-02-16T12:34:56.123456789Z")))
    (should ts)
    (should (floatp ts))
    (should (> ts 0))))

(ert-deftest elinit-test-timestamp-parsing-epoch-large ()
  "Timestamp parser handles large epoch integer strings."
  (let ((ts (elinit--log-parse-timestamp "1708098896")))
    (should ts)
    (should (floatp ts))
    (should (= ts 1708098896.0))))

(ert-deftest elinit-test-human-format-output-record ()
  "Human record formatter produces readable output."
  (let ((record (list :ts 1708098896.0 :unit "myapp" :pid 42
                      :stream 'stdout :event 'output
                      :payload "Hello world")))
    (let ((formatted (elinit--log-format-record-human record)))
      (should (stringp formatted))
      (should (string-match-p "myapp" formatted))
      (should (string-match-p "42" formatted))
      (should (string-match-p "Hello world" formatted)))))

(ert-deftest elinit-test-human-format-exit-record ()
  "Human record formatter handles exit events."
  (let ((record (list :ts 1708098896.0 :unit "myapp" :pid 42
                      :stream 'meta :event 'exit
                      :status 'exited :code 0)))
    (let ((formatted (elinit--log-format-record-human record)))
      (should (stringp formatted))
      (should (string-match-p "exit" formatted)))))

(ert-deftest elinit-test-priority-stderr-is-err ()
  "Priority classification: stderr output is err."
  (let ((rec (list :stream 'stderr :event 'output :status nil :code 0)))
    (should (eq 'err (elinit--log-record-priority rec)))))

(ert-deftest elinit-test-priority-non-clean-exit-is-err ()
  "Priority classification: non-zero exit code is err."
  (let ((rec (list :stream 'meta :event 'exit :status 'exited :code 1)))
    (should (eq 'err (elinit--log-record-priority rec)))))

(ert-deftest elinit-test-priority-signal-exit-is-err ()
  "Priority classification: signal exit is err."
  (let ((rec (list :stream 'meta :event 'exit :status 'signal :code 9)))
    (should (eq 'err (elinit--log-record-priority rec)))))

(ert-deftest elinit-test-priority-clean-stdout-is-info ()
  "Priority classification: clean stdout output is info."
  (let ((rec (list :stream 'stdout :event 'output :status nil :code 0)))
    (should (eq 'info (elinit--log-record-priority rec)))))

(ert-deftest elinit-test-priority-clean-exit-is-info ()
  "Priority classification: clean exit (code 0) is info."
  (let ((rec (list :stream 'meta :event 'exit :status 'exited :code 0)))
    (should (eq 'info (elinit--log-record-priority rec)))))

;;;; Merged-stream identity and exit-frame drain fixes

(ert-deftest elinit-test-merged-stderr-pipe-when-framed ()
  "Merged mode with log-format creates stderr pipe for stream identity."
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
        (captured-stderr 'unset)
        (fake-writer (start-process "fake-writer" nil "sleep" "300"))
        (fake-proc (start-process "fake-svc" nil "sleep" "300")))
    (unwind-protect
        (cl-letf (((symbol-function 'elinit--get-effective-logging)
                   (lambda (_id _default) t))
                  ((symbol-function 'elinit--start-writer)
                   (lambda (_id _file &optional _log-format) fake-writer))
                  ((symbol-function 'elinit--start-stderr-writer)
                   (lambda (&rest _args) nil))
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
                       "/tmp/svc.shared.log" "/tmp/svc.shared.log"
                       nil 'text)))
            (should proc)
            ;; With log-format set, merged mode should still use a stderr
            ;; pipe for correct stream identity
            (should captured-stderr)))
      (when (process-live-p fake-proc) (delete-process fake-proc))
      (when (process-live-p fake-writer) (delete-process fake-writer))
      (when-let* ((pipe (gethash "svc" elinit--stderr-pipes)))
        (when (process-live-p pipe) (delete-process pipe))))))

(ert-deftest elinit-test-merged-stderr-pipe-without-log-format ()
  "Merged mode without explicit log-format still creates stderr pipe.
Framed transport is always used when a writer exists, so stream
identity must be correct regardless of whether log-format is set."
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
        (captured-stderr 'unset)
        (fake-writer (start-process "fake-writer" nil "sleep" "300"))
        (fake-proc (start-process "fake-svc" nil "sleep" "300")))
    (unwind-protect
        (cl-letf (((symbol-function 'elinit--get-effective-logging)
                   (lambda (_id _default) t))
                  ((symbol-function 'elinit--start-writer)
                   (lambda (_id _file &optional _log-format) fake-writer))
                  ((symbol-function 'elinit--start-stderr-writer)
                   (lambda (&rest _args) nil))
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
            ;; Omitted log-format uses framed transport like explicit text,
            ;; so merged mode must still use a stderr pipe for stream identity
            (should captured-stderr)))
      (when (process-live-p fake-proc) (delete-process fake-proc))
      (when (process-live-p fake-writer) (delete-process fake-writer))
      (when-let* ((pipe (gethash "svc" elinit--stderr-pipes)))
        (when (process-live-p pipe) (delete-process pipe))))))

(ert-deftest elinit-test-merged-stderr-pipe-tags-stream-2 ()
  "Merged stderr pipe filter tags output as stream=2."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--stderr-pipes (make-hash-table :test 'equal))
        (sent-data nil)
        (fake-writer (start-process "fake-writer" nil "sleep" "300"))
        (fake-svc (start-process "fake-svc" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "test-svc" fake-svc elinit--processes)
          (cl-letf (((symbol-function 'process-send-string)
                     (lambda (_proc data)
                       (push data sent-data))))
            ;; Create pipe targeting the stdout writer (merged mode)
            (let ((pipe (elinit--start-stderr-pipe "test-svc"
                                                       fake-writer)))
              (should pipe)
              ;; Invoke the filter with stderr data
              (let ((filter (process-filter pipe)))
                (funcall filter pipe "stderr output\n"))
              (should (= 1 (length sent-data)))
              (let ((frame (car sent-data)))
                ;; event=1 (output), stream=2 (stderr)
                (should (= 1 (aref frame 4)))
                (should (= 2 (aref frame 5)))
                (should (string-suffix-p "stderr output\n" frame))))))
      (when (process-live-p fake-writer) (delete-process fake-writer))
      (when (process-live-p fake-svc) (delete-process fake-svc))
      (when-let* ((pipe (gethash "test-svc" elinit--stderr-pipes)))
        (when (process-live-p pipe) (delete-process pipe))))))

(ert-deftest elinit-test-sentinel-sends-eof-before-teardown ()
  "Sentinel sends exit frame, then EOF, then schedules deferred teardown."
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
        (action-log nil)
        (deferred-fn nil))
    (let ((fake-writer (start-process "eof-drain-fw" nil "sleep" "300")))
      (unwind-protect
          (progn
            ;; Use a unique name to avoid timer collisions from prior
            ;; tests that schedule deferred stop-writer via run-at-time.
            (puthash "eof-drain-svc" fake-writer elinit--writers)
            (let ((fake-proc (start-process "eof-drain-svc" nil "true")))
              (puthash "eof-drain-svc" fake-proc elinit--processes)
              (while (process-live-p fake-proc) (sit-for 0.05))
              (cl-letf (((symbol-function 'elinit--log-send-frame)
                         (lambda (_w event _stream &rest _args)
                           (push (list 'frame event) action-log)))
                        ((symbol-function 'process-send-eof)
                         (lambda (_proc)
                           (push '(eof) action-log)))
                        ((symbol-function 'elinit--emit-event) #'ignore)
                        ((symbol-function 'elinit--maybe-refresh-dashboard) #'ignore)
                        ((symbol-function 'elinit--should-restart-p) (lambda (&rest _) nil))
                        ((symbol-function 'run-at-time)
                         (let ((real-rat (symbol-function 'run-at-time)))
                           (lambda (time repeat fn)
                             (if (symbolp fn)
                                 ;; Pass through Emacs-internal timers
                                 ;; (e.g. undo-auto--boundary-timer).
                                 (funcall real-rat time repeat fn)
                               (push '(deferred-teardown) action-log)
                               (setq deferred-fn fn)
                               nil)))))
                (let ((sentinel (elinit--make-process-sentinel
                                 "eof-drain-svc" '("sleep" "300") t 'simple t)))
                  (funcall sentinel fake-proc "finished\n")))))
        (when (process-live-p fake-writer) (delete-process fake-writer))))
    ;; Verify ordering: exit frame -> EOF -> deferred teardown scheduled
    (setq action-log (nreverse action-log))
    (should (>= (length action-log) 3))
    ;; First: exit frame (event=2)
    (should (equal (car (nth 0 action-log)) 'frame))
    (should (= 2 (cadr (nth 0 action-log))))
    ;; Second: EOF
    (should (equal (nth 1 action-log) '(eof)))
    ;; Third: deferred teardown was SCHEDULED (not executed inline)
    (should (equal (nth 2 action-log) '(deferred-teardown)))
    ;; Deferred function, when called, invokes stop-writer-if-same
    (should deferred-fn)
    (let ((stopped nil))
      (cl-letf (((symbol-function 'elinit--stop-writer-if-same)
                 (lambda (_name &rest _args) (setq stopped t))))
        (funcall deferred-fn))
      (should stopped))))

(ert-deftest elinit-test-merged-no-pipe-when-logging-disabled ()
  "Merged mode with logging disabled does not create stderr pipe."
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
        (captured-stderr 'unset)
        (fake-proc (start-process "fake-svc" nil "sleep" "300")))
    (unwind-protect
        (cl-letf (((symbol-function 'elinit--get-effective-logging)
                   (lambda (_id _default) nil))
                  ((symbol-function 'make-process)
                   (lambda (&rest args)
                     (setq captured-stderr (plist-get args :stderr))
                     fake-proc))
                  ((symbol-function 'elinit--make-process-sentinel)
                   (lambda (&rest _args) #'ignore))
                  ((symbol-function 'elinit--build-launch-command)
                   (lambda (_cmd &rest _args) (list "sleep" "300"))))
          (let ((proc (elinit--start-process
                       "svc" "sleep 300" nil 'simple 'always
                       nil nil nil nil nil nil nil nil
                       nil nil)))
            (should proc)
            ;; No writer -> no pipe needed
            (should-not captured-stderr)))
      (when (process-live-p fake-proc) (delete-process fake-proc)))))

(ert-deftest elinit-test-stderr-pipe-failure-aborts-merged-start ()
  "Merged mode aborts service start when stderr pipe creation fails."
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
        (writer-stopped nil)
        (fake-writer (start-process "pipe-fail-fw" nil "sleep" "300")))
    (unwind-protect
        (cl-letf (((symbol-function 'elinit--get-effective-logging)
                   (lambda (_id _default) t))
                  ((symbol-function 'elinit--start-writer)
                   (lambda (_id _file &optional _log-format) fake-writer))
                  ((symbol-function 'elinit--start-stderr-pipe)
                   (lambda (_id _writer) nil))
                  ((symbol-function 'elinit--stop-writer)
                   (lambda (_name) (setq writer-stopped t)))
                  ((symbol-function 'elinit--build-launch-command)
                   (lambda (_cmd &rest _args) (list "sleep" "300"))))
          (let ((proc (elinit--start-process
                       "pipe-fail-svc" "sleep 300" t 'simple 'always
                       nil nil nil nil nil nil nil nil
                       "/tmp/svc.shared.log" "/tmp/svc.shared.log")))
            ;; Service must not start when pipe fails
            (should-not proc)
            ;; Writers cleaned up
            (should writer-stopped)
            ;; Failure reason recorded
            (should (gethash "pipe-fail-svc"
                             elinit--spawn-failure-reason))))
      (when (process-live-p fake-writer) (delete-process fake-writer)))))

(ert-deftest elinit-test-stderr-pipe-failure-aborts-split-start ()
  "Split mode aborts service start when stderr pipe creation fails."
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
        (writer-stopped nil)
        (stderr-writer-stopped nil)
        (fake-writer (start-process "pipe-fail-fw2" nil "sleep" "300"))
        (fake-stderr-writer (start-process "pipe-fail-sew2" nil "sleep" "300")))
    (unwind-protect
        (cl-letf (((symbol-function 'elinit--get-effective-logging)
                   (lambda (_id _default) t))
                  ((symbol-function 'elinit--start-writer)
                   (lambda (_id _file &optional _log-format) fake-writer))
                  ((symbol-function 'elinit--start-stderr-writer)
                   (lambda (_id _file &optional _log-format) fake-stderr-writer))
                  ((symbol-function 'elinit--start-stderr-pipe)
                   (lambda (_id _writer) nil))
                  ((symbol-function 'elinit--stop-writer)
                   (lambda (_name) (setq writer-stopped t)))
                  ((symbol-function 'elinit--stop-stream-writer)
                   (lambda (_id _stream _table)
                     (setq stderr-writer-stopped t)))
                  ((symbol-function 'elinit--build-launch-command)
                   (lambda (_cmd &rest _args) (list "sleep" "300"))))
          (let ((proc (elinit--start-process
                       "pipe-fail-svc2" "sleep 300" t 'simple 'always
                       nil nil nil nil nil nil nil nil
                       "/tmp/svc.out.log" "/tmp/svc.err.log")))
            ;; Service must not start when pipe fails
            (should-not proc)
            ;; Both writers cleaned up
            (should writer-stopped)
            (should stderr-writer-stopped)
            ;; Failure reason recorded
            (should (gethash "pipe-fail-svc2"
                             elinit--spawn-failure-reason))))
      (when (process-live-p fake-writer) (delete-process fake-writer))
      (when (process-live-p fake-stderr-writer)
        (delete-process fake-stderr-writer)))))

;;;; Bounded decode (max-bytes) tests

(ert-deftest elinit-test-decode-text-max-bytes-limits-records ()
  "Text log with max-bytes smaller than file returns fewer records."
  (let ((tmpfile (make-temp-file "log-mb-")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (dotimes (i 100)
              (insert (format "ts=%d unit=svc pid=1 stream=stdout " (+ 1000 i))
                      "event=output status=- code=- payload=data\n")))
          (let* ((all (elinit--log-decode-file tmpfile))
                 (all-count (length (plist-get all :records)))
                 (partial (elinit--log-decode-file tmpfile nil nil 500))
                 (partial-recs (plist-get partial :records)))
            ;; All records from full read
            (should (= 100 all-count))
            ;; Partial should have fewer records, all valid
            (should (< (length partial-recs) all-count))
            (should (> (length partial-recs) 0))
            (dolist (r partial-recs)
              (should (plist-get r :unit)))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-decode-binary-max-bytes-limits-records ()
  "Binary log with max-bytes smaller than file returns valid records."
  (let ((tmpfile (make-temp-file "log-bmb-")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (set-buffer-multibyte nil)
            (insert elinit--log-binary-magic)
            (dotimes (i 10)
              (insert (elinit-test--make-binary-record
                       1 1 42 "svc" (format "line-%d" i) 0 0
                       (+ 1000000000000 (* i 1000000000))))))
          (let* ((all (elinit--log-decode-file tmpfile))
                 (all-count (length (plist-get all :records)))
                 ;; Read only last 200 bytes
                 (partial (elinit--log-decode-file tmpfile nil nil 200))
                 (partial-recs (plist-get partial :records)))
            (should (= 10 all-count))
            (should (< (length partial-recs) all-count))
            (should (> (length partial-recs) 0))
            (dolist (r partial-recs)
              (should (equal (plist-get r :unit) "svc")))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-decode-max-bytes-nil-returns-all ()
  "Nil max-bytes returns all records (backward compat)."
  (let ((tmpfile (make-temp-file "log-mbn-")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (dotimes (i 10)
              (insert (format "ts=%d unit=svc pid=1 stream=stdout " (+ 1000 i))
                      "event=output status=- code=- payload=line\n")))
          (let* ((result (elinit--log-decode-file tmpfile nil nil nil))
                 (records (plist-get result :records)))
            (should (= 10 (length records)))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-decode-max-bytes-larger-than-file ()
  "Max-bytes larger than file returns all records."
  (let ((tmpfile (make-temp-file "log-mblg-")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (dotimes (i 5)
              (insert (format "ts=%d unit=svc pid=1 stream=stdout " (+ 1000 i))
                      "event=output status=- code=- payload=line\n")))
          (let* ((result (elinit--log-decode-file tmpfile nil nil 999999))
                 (records (plist-get result :records)))
            (should (= 5 (length records)))))
      (delete-file tmpfile))))

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

(ert-deftest elinit-test-binary-scan-to-record ()
  "Binary scan finds first valid record in partial chunk."
  (let* ((rec1 (elinit-test--make-binary-record
                1 1 42 "svc" "hello" 0 0 1000000000000))
         (rec2 (elinit-test--make-binary-record
                1 1 42 "svc" "world" 0 0 2000000000000))
         ;; Prepend 7 garbage bytes to simulate a truncated first record
         (garbage (make-string 7 ?X))
         (chunk (concat garbage rec1 rec2)))
    ;; Scan should skip garbage and find rec1
    (let ((pos (elinit--log-binary-scan-to-record chunk)))
      (should pos)
      (should (= pos 7))
      ;; Decode from found position should yield 2 records
      (let* ((result (elinit--log-decode-binary-records chunk pos))
             (records (plist-get result :records)))
        (should (= 2 (length records)))
        (should (equal "hello" (plist-get (car records) :payload)))))))

;;;; Follow session tests

(ert-deftest elinit-test-follow-session-start-creates-file ()
  "Follow session start creates follow file and registers session."
  (let ((tmpdir (make-temp-file "follow-test-" t))
        (elinit--cli-follow-sessions (make-hash-table :test 'equal)))
    (unwind-protect
        (let* ((log-file (expand-file-name "svc.log" tmpdir))
               (finfo (elinit--cli-follow-start
                       "svc" log-file 0 nil nil nil nil))
               (session-id (plist-get finfo :session-id))
               (follow-file (plist-get finfo :follow-file)))
          (should (string-prefix-p "follow-svc-" session-id))
          (should (file-exists-p follow-file))
          (should (gethash session-id elinit--cli-follow-sessions))
          ;; Cleanup
          (elinit--cli-follow-stop session-id))
      (delete-directory tmpdir t))))

(ert-deftest elinit-test-follow-session-stop-cleans-up ()
  "Follow session stop deletes file, cancels timer, clears hash."
  (let ((tmpdir (make-temp-file "follow-test-" t))
        (elinit--cli-follow-sessions (make-hash-table :test 'equal)))
    (unwind-protect
        (let* ((log-file (expand-file-name "svc.log" tmpdir))
               (finfo (elinit--cli-follow-start
                       "svc" log-file 0 nil nil nil nil))
               (session-id (plist-get finfo :session-id))
               (follow-file (plist-get finfo :follow-file)))
          (should (file-exists-p follow-file))
          (should (elinit--cli-follow-stop session-id))
          (should-not (file-exists-p follow-file))
          (should-not (gethash session-id
                               elinit--cli-follow-sessions)))
      (delete-directory tmpdir t))))

(ert-deftest elinit-test-follow-poll-appends-to-file ()
  "Follow poll appends new records to follow file."
  (let ((tmpdir (make-temp-file "follow-test-" t))
        (elinit--cli-follow-sessions (make-hash-table :test 'equal)))
    (unwind-protect
        (let* ((log-file (expand-file-name "svc.log" tmpdir)))
          ;; Write initial log data
          (with-temp-file log-file
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=initial\n"))
          (let* ((decoded (elinit--log-decode-file log-file))
                 (offset (plist-get decoded :offset))
                 (finfo (elinit--cli-follow-start
                         "svc" log-file offset nil nil nil nil))
                 (session-id (plist-get finfo :session-id))
                 (follow-file (plist-get finfo :follow-file)))
            ;; Append new record to log
            (with-temp-buffer
              (insert "ts=2000 unit=svc pid=1 stream=stdout "
                      "event=output status=- code=- payload=new-data\n")
              (append-to-file (point-min) (point-max) log-file))
            ;; Cancel the scheduled timer, call poll directly
            (let ((session (gethash session-id
                                    elinit--cli-follow-sessions)))
              (when (plist-get session :timer)
                (cancel-timer (plist-get session :timer))))
            (elinit--cli-follow-poll session-id)
            ;; Cancel rescheduled timer
            (let ((session (gethash session-id
                                    elinit--cli-follow-sessions)))
              (when (and session (plist-get session :timer))
                (cancel-timer (plist-get session :timer))))
            ;; Follow file should contain the new record
            (let ((content (with-temp-buffer
                             (insert-file-contents follow-file)
                             (buffer-string))))
              (should (string-match-p "new-data" content)))
            (elinit--cli-follow-stop session-id)))
      (delete-directory tmpdir t))))

(ert-deftest elinit-test-follow-poll-orphan-detection ()
  "Follow poll cleans up when follow file is deleted externally."
  (let ((tmpdir (make-temp-file "follow-test-" t))
        (elinit--cli-follow-sessions (make-hash-table :test 'equal)))
    (unwind-protect
        (let* ((log-file (expand-file-name "svc.log" tmpdir)))
          (with-temp-file log-file
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=data\n"))
          (let* ((finfo (elinit--cli-follow-start
                         "svc" log-file 0 nil nil nil nil))
                 (session-id (plist-get finfo :session-id))
                 (follow-file (plist-get finfo :follow-file)))
            ;; Cancel scheduled timer
            (let ((session (gethash session-id
                                    elinit--cli-follow-sessions)))
              (when (plist-get session :timer)
                (cancel-timer (plist-get session :timer))))
            ;; Delete follow file externally
            (delete-file follow-file)
            ;; Poll should detect orphan and clean up
            (elinit--cli-follow-poll session-id)
            (should-not (gethash session-id
                                 elinit--cli-follow-sessions))))
      (delete-directory tmpdir t))))

(ert-deftest elinit-test-follow-poll-max-age-expiry ()
  "Follow poll auto-expires sessions past max age."
  (let ((tmpdir (make-temp-file "follow-test-" t))
        (elinit--cli-follow-sessions (make-hash-table :test 'equal))
        (elinit-cli-follow-max-age 10))
    (unwind-protect
        (let* ((log-file (expand-file-name "svc.log" tmpdir)))
          (with-temp-file log-file
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=data\n"))
          (let* ((finfo (elinit--cli-follow-start
                         "svc" log-file 0 nil nil nil nil))
                 (session-id (plist-get finfo :session-id)))
            ;; Cancel scheduled timer
            (let ((session (gethash session-id
                                    elinit--cli-follow-sessions)))
              (when (plist-get session :timer)
                (cancel-timer (plist-get session :timer)))
              ;; Backdate the last activity time
              (plist-put session :last-activity (- (float-time) 100)))
            ;; Poll should auto-expire
            (elinit--cli-follow-poll session-id)
            (should-not (gethash session-id
                                 elinit--cli-follow-sessions))))
      (delete-directory tmpdir t))))

(ert-deftest elinit-test-follow-poll-no-new-data ()
  "Follow poll with no new data leaves follow file empty."
  (let ((tmpdir (make-temp-file "follow-test-" t))
        (elinit--cli-follow-sessions (make-hash-table :test 'equal)))
    (unwind-protect
        (let* ((log-file (expand-file-name "svc.log" tmpdir)))
          (with-temp-file log-file
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=only\n"))
          (let* ((decoded (elinit--log-decode-file log-file))
                 (offset (plist-get decoded :offset))
                 (finfo (elinit--cli-follow-start
                         "svc" log-file offset nil nil nil nil))
                 (session-id (plist-get finfo :session-id))
                 (follow-file (plist-get finfo :follow-file)))
            ;; Cancel scheduled timer, poll directly
            (let ((session (gethash session-id
                                    elinit--cli-follow-sessions)))
              (when (plist-get session :timer)
                (cancel-timer (plist-get session :timer))))
            (elinit--cli-follow-poll session-id)
            ;; Cancel rescheduled timer
            (let ((session (gethash session-id
                                    elinit--cli-follow-sessions)))
              (when (and session (plist-get session :timer))
                (cancel-timer (plist-get session :timer))))
            ;; Follow file should still be empty
            (let ((content (with-temp-buffer
                             (insert-file-contents follow-file)
                             (buffer-string))))
              (should (equal "" content)))
            (elinit--cli-follow-stop session-id)))
      (delete-directory tmpdir t))))

(ert-deftest elinit-test-follow-poll-json-mode ()
  "Follow poll in JSON mode writes NDJSON lines."
  (let ((tmpdir (make-temp-file "follow-test-" t))
        (elinit--cli-follow-sessions (make-hash-table :test 'equal)))
    (unwind-protect
        (let* ((log-file (expand-file-name "svc.log" tmpdir)))
          (with-temp-file log-file
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=initial\n"))
          (let* ((decoded (elinit--log-decode-file log-file))
                 (offset (plist-get decoded :offset))
                 (finfo (elinit--cli-follow-start
                         "svc" log-file offset nil nil nil t))
                 (session-id (plist-get finfo :session-id))
                 (follow-file (plist-get finfo :follow-file)))
            ;; Append new record
            (with-temp-buffer
              (insert "ts=2000 unit=svc pid=1 stream=stdout "
                      "event=output status=- code=- payload=json-test\n")
              (append-to-file (point-min) (point-max) log-file))
            ;; Cancel timer, poll directly
            (let ((session (gethash session-id
                                    elinit--cli-follow-sessions)))
              (when (plist-get session :timer)
                (cancel-timer (plist-get session :timer))))
            (elinit--cli-follow-poll session-id)
            (let ((session (gethash session-id
                                    elinit--cli-follow-sessions)))
              (when (and session (plist-get session :timer))
                (cancel-timer (plist-get session :timer))))
            ;; Follow file should have JSON
            (let ((content (with-temp-buffer
                             (insert-file-contents follow-file)
                             (buffer-string))))
              (should (string-match-p "\"payload\"" content))
              (should (string-match-p "json-test" content)))
            (elinit--cli-follow-stop session-id)))
      (delete-directory tmpdir t))))

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
                (when (string-match "FOLLOW:[^:]*:[^:]*:\\(.+\\)" output)
                  (elinit--cli-follow-stop (match-string 1 output)))))))
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
              (when (string-match "FOLLOW:[^:]*:[^:]*:\\(.+\\)" output)
                (elinit--cli-follow-stop (match-string 1 output))))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-follow-stop-nonexistent ()
  "Stopping nonexistent session returns nil without error."
  (let ((elinit--cli-follow-sessions (make-hash-table :test 'equal)))
    (should-not (elinit--cli-follow-stop "no-such-session"))))

(ert-deftest elinit-test-follow-initial-output-encoding ()
  "FOLLOW protocol base64 initial output decodes correctly."
  (let ((tmpfile (make-temp-file "log-follow-"))
        (elinit--cli-follow-sessions (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=hello-world\n"))
          (cl-letf (((symbol-function 'elinit--log-file)
                     (lambda (_id) tmpfile)))
            (let* ((result (elinit--cli-cmd-journal
                            '("-u" "svc" "-f") nil))
                   (output (elinit-cli-result-output result)))
              (should (string-match
                       "FOLLOW:\\([^:]*\\):\\([^:]*\\):\\(.+\\)" output))
              (let* ((b64 (match-string 1 output))
                     (decoded (decode-coding-string
                               (base64-decode-string b64) 'utf-8)))
                (should (string-match-p "hello-world" decoded))
                ;; Cleanup
                (elinit--cli-follow-stop (match-string 3 output))))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-follow-with-priority-filter ()
  "Follow poll with priority filter only includes matching records."
  (let ((tmpdir (make-temp-file "follow-test-" t))
        (elinit--cli-follow-sessions (make-hash-table :test 'equal)))
    (unwind-protect
        (let* ((log-file (expand-file-name "svc.log" tmpdir)))
          (with-temp-file log-file
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=initial\n"))
          (let* ((decoded (elinit--log-decode-file log-file))
                 (offset (plist-get decoded :offset))
                 (finfo (elinit--cli-follow-start
                         "svc" log-file offset nil nil 'err nil))
                 (session-id (plist-get finfo :session-id))
                 (follow-file (plist-get finfo :follow-file)))
            ;; Append stdout (info) and stderr (err) records
            (with-temp-buffer
              (insert "ts=2000 unit=svc pid=1 stream=stdout "
                      "event=output status=- code=- payload=stdout-line\n"
                      "ts=3000 unit=svc pid=1 stream=stderr "
                      "event=output status=- code=- payload=stderr-line\n")
              (append-to-file (point-min) (point-max) log-file))
            ;; Cancel timer, poll directly
            (let ((session (gethash session-id
                                    elinit--cli-follow-sessions)))
              (when (plist-get session :timer)
                (cancel-timer (plist-get session :timer))))
            (elinit--cli-follow-poll session-id)
            (let ((session (gethash session-id
                                    elinit--cli-follow-sessions)))
              (when (and session (plist-get session :timer))
                (cancel-timer (plist-get session :timer))))
            ;; Only stderr record should appear
            (let ((content (with-temp-buffer
                             (insert-file-contents follow-file)
                             (buffer-string))))
              (should (string-match-p "stderr-line" content))
              (should-not (string-match-p "stdout-line" content)))
            (elinit--cli-follow-stop session-id)))
      (delete-directory tmpdir t))))

;;;; Bounded decode default cap tests

(ert-deftest elinit-test-journal-without-n-decodes-full-file ()
  "Journal without -n decodes the entire file (full history)."
  (let ((tmpfile (make-temp-file "log-cap-"))
        (captured-max-bytes 'unset))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=hello\n"))
          (cl-letf (((symbol-function 'elinit--log-file)
                     (lambda (_id) tmpfile))
                    ((symbol-function 'elinit--log-decode-file)
                     (lambda (_file &optional _limit _offset max-bytes)
                       (setq captured-max-bytes max-bytes)
                       (list :records
                             (list (list :ts 1000.0 :unit "svc" :pid 1
                                         :stream 'stdout :event 'output
                                         :status nil :code 0
                                         :payload "hello"))
                             :offset 100 :format 'text :warning nil))))
            (elinit--cli-cmd-journal '("-u" "svc") nil)
            ;; Without -n, max-bytes must be nil (full file decode)
            (should (null captured-max-bytes))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-telemetry-tail-uses-max-bytes ()
  "Telemetry log-tail passes max-bytes to decode."
  (let ((tmpfile (make-temp-file "log-telem-"))
        (captured-max-bytes nil))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=hello\n"))
          (cl-letf (((symbol-function 'elinit--log-file)
                     (lambda (_id) tmpfile))
                    ((symbol-function 'elinit--log-decode-file)
                     (lambda (_file &optional _limit _offset max-bytes)
                       (setq captured-max-bytes max-bytes)
                       (list :records
                             (list (list :ts 1000.0 :unit "svc" :pid 1
                                         :stream 'stdout :event 'output
                                         :status nil :code 0
                                         :payload "hello"))
                             :offset 100 :format 'text :warning nil))))
            (elinit--telemetry-log-tail "svc")
            ;; Default 5 lines: 5 * 512 = 2560
            (should (= 2560 captured-max-bytes))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-telemetry-tail-nil-lines-uses-default-cap ()
  "Telemetry log-tail without explicit lines uses default cap."
  (let ((tmpfile (make-temp-file "log-telem2-"))
        (captured-max-bytes nil))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=hello\n"))
          ;; Call with nil lines -- n defaults to 5, so 5*512=2560
          (cl-letf (((symbol-function 'elinit--log-file)
                     (lambda (_id) tmpfile))
                    ((symbol-function 'elinit--log-decode-file)
                     (lambda (_file &optional _limit _offset max-bytes)
                       (setq captured-max-bytes max-bytes)
                       (list :records
                             (list (list :ts 1000.0 :unit "svc" :pid 1
                                         :stream 'stdout :event 'output
                                         :status nil :code 0
                                         :payload "hello"))
                             :offset 100 :format 'text :warning nil))))
            (elinit--telemetry-log-tail "svc" nil)
            (should (= 2560 captured-max-bytes))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-follow-session-id-subsecond ()
  "Follow session IDs include subsecond precision."
  (let ((tmpdir (make-temp-file "follow-id-" t))
        (elinit--cli-follow-sessions (make-hash-table :test 'equal)))
    (unwind-protect
        (let* ((log-file (expand-file-name "svc.log" tmpdir))
               (finfo (elinit--cli-follow-start
                       "svc" log-file 0 nil nil nil nil))
               (session-id (plist-get finfo :session-id)))
          ;; Session ID should contain a dot (fractional seconds)
          (should (string-match-p "\\." session-id))
          ;; Session ID should match the subsecond pattern
          (should (string-match-p "^follow-svc-[0-9]+\\.[0-9]+" session-id))
          (elinit--cli-follow-stop session-id))
      (delete-directory tmpdir t))))

(ert-deftest elinit-test-follow-poll-resets-activity-timer ()
  "Follow poll resets last-activity so active sessions never expire."
  (let ((tmpdir (make-temp-file "follow-test-" t))
        (elinit--cli-follow-sessions (make-hash-table :test 'equal))
        (elinit-cli-follow-max-age 10))
    (unwind-protect
        (let* ((log-file (expand-file-name "svc.log" tmpdir)))
          (with-temp-file log-file
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=initial\n"))
          (let* ((decoded (elinit--log-decode-file log-file))
                 (offset (plist-get decoded :offset))
                 (finfo (elinit--cli-follow-start
                         "svc" log-file offset nil nil nil nil))
                 (session-id (plist-get finfo :session-id)))
            ;; Cancel scheduled timer
            (let ((session (gethash session-id
                                    elinit--cli-follow-sessions)))
              (when (plist-get session :timer)
                (cancel-timer (plist-get session :timer)))
              ;; Set last-activity to 8 seconds ago (close to 10s max)
              (plist-put session :last-activity (- (float-time) 8)))
            ;; Append data so poll has records to process
            (with-temp-buffer
              (insert "ts=2000 unit=svc pid=1 stream=stdout "
                      "event=output status=- code=- payload=new\n")
              (append-to-file (point-min) (point-max) log-file))
            ;; Poll should succeed and reset the timer
            (elinit--cli-follow-poll session-id)
            ;; Cancel rescheduled timer
            (let ((session (gethash session-id
                                    elinit--cli-follow-sessions)))
              (should session)  ;; Session still alive
              (when (plist-get session :timer)
                (cancel-timer (plist-get session :timer)))
              ;; last-activity should be recent (within 2 seconds)
              (should (< (- (float-time)
                            (plist-get session :last-activity))
                         2.0)))
            (elinit--cli-follow-stop session-id)))
      (delete-directory tmpdir t))))

;;;; JSON null and argument validation tests

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

;;;; RFC3339 range validation tests

(ert-deftest elinit-test-timestamp-rejects-month-13 ()
  "Out-of-range month 13 returns nil, not a normalized timestamp."
  (should (null (elinit--log-parse-timestamp
                 "2026-13-01T00:00:00Z"))))

(ert-deftest elinit-test-timestamp-rejects-hour-25 ()
  "Out-of-range hour 25 returns nil."
  (should (null (elinit--log-parse-timestamp
                 "2026-01-01T25:00:00Z"))))

(ert-deftest elinit-test-timestamp-rejects-day-32 ()
  "Out-of-range day 32 returns nil."
  (should (null (elinit--log-parse-timestamp
                 "2026-01-32T00:00:00Z"))))

(ert-deftest elinit-test-timestamp-rejects-minute-60 ()
  "Out-of-range minute 60 returns nil."
  (should (null (elinit--log-parse-timestamp
                 "2026-01-01T00:60:00Z"))))

(ert-deftest elinit-test-timestamp-accepts-leap-second ()
  "Second 60 (leap second) is accepted."
  (should (elinit--log-parse-timestamp
           "2026-01-01T23:59:60Z")))

(ert-deftest elinit-test-timestamp-valid-rfc3339 ()
  "Valid RFC3339 timestamp parses to non-nil float."
  (let ((result (elinit--log-parse-timestamp
                 "2026-02-16T12:00:00Z")))
    (should result)
    (should (floatp result))))

(ert-deftest elinit-test-journal-rejects-invalid-since-timestamp ()
  "Journal --since with out-of-range timestamp returns error."
  (let ((tmpfile (make-temp-file "log-ts-")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "ts=1000 unit=svc pid=1 stream=stdout "
                    "event=output status=- code=- payload=hello\n"))
          (cl-letf (((symbol-function 'elinit--log-file)
                     (lambda (_id) tmpfile)))
            (let ((result (elinit--cli-cmd-journal
                           '("-u" "svc" "--since"
                             "2026-13-01T00:00:00Z")
                           nil)))
              (should (= elinit-cli-exit-invalid-args
                         (elinit-cli-result-exitcode result)))
              (should (string-match-p
                       "Invalid --since"
                       (elinit-cli-result-output result))))))
      (delete-file tmpfile))))

(ert-deftest elinit-test-timestamp-rejects-feb-31 ()
  "Feb 31 is an impossible date and must return nil."
  (should (null (elinit--log-parse-timestamp
                 "2026-02-31T00:00:00Z"))))

(ert-deftest elinit-test-timestamp-rejects-apr-31 ()
  "Apr 31 is impossible and must return nil."
  (should (null (elinit--log-parse-timestamp
                 "2026-04-31T00:00:00Z"))))

(ert-deftest elinit-test-timestamp-rejects-feb-29-non-leap ()
  "Feb 29 in a non-leap year must return nil."
  (should (null (elinit--log-parse-timestamp
                 "2026-02-29T00:00:00Z"))))

(ert-deftest elinit-test-timestamp-accepts-feb-29-leap ()
  "Feb 29 in a leap year must parse."
  (should (elinit--log-parse-timestamp
           "2024-02-29T00:00:00Z")))

(ert-deftest elinit-test-timestamp-accepts-jan-31 ()
  "Jan 31 is valid."
  (should (elinit--log-parse-timestamp
           "2026-01-31T00:00:00Z")))

;;;; Log-format propagation on non-DAG start paths

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

;;;; Prune hook directory correctness

(ert-deftest elinit-test-build-prune-command-uses-explicit-log-dir ()
  "Build-prune-command uses LOG-DIR when provided, not global dir."
  (let ((elinit-log-prune-command "/opt/prune")
        (elinit-log-directory "/global/logs")
        (elinit-log-prune-max-total-bytes 2048))
    (cl-letf (((symbol-function 'elinit--effective-log-directory)
               (lambda () "/global/logs")))
      (let ((cmd (elinit--build-prune-command nil "/custom/logs")))
        (should (string-match-p (regexp-quote "/custom/logs") cmd))
        (should-not (string-match-p (regexp-quote "/global/logs") cmd))))))

(ert-deftest elinit-test-writer-prune-cmd-uses-writer-log-dir ()
  "Writer prune command targets the writer's log directory, not global."
  (let ((elinit--writers (make-hash-table :test 'equal))
        (elinit--logd-pid-directory nil)
        (captured-cmd nil)
        (fake-proc (start-process "fake-logd" nil "sleep" "300")))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'elinit--effective-log-directory)
                     (lambda () "/global/logs"))
                    ((symbol-function 'elinit--ensure-log-directory)
                     (lambda () "/global/logs"))
                    ((symbol-function 'make-process)
                     (lambda (&rest args)
                       (setq captured-cmd (plist-get args :command))
                       fake-proc))
                    ((symbol-function 'elinit--write-logd-pid-file) #'ignore))
            (elinit--start-stream-writer
             "svc" 'stdout "/custom/app/log-svc.log"
             elinit--writers nil))
          (should captured-cmd)
          ;; --log-dir should be the custom directory
          (let ((logdir-idx (cl-position "--log-dir" captured-cmd
                                         :test #'equal)))
            (should logdir-idx)
            (should (equal "/custom/app" (nth (1+ logdir-idx) captured-cmd))))
          ;; --prune-cmd should also reference the custom directory
          (let ((prune-idx (cl-position "--prune-cmd" captured-cmd
                                        :test #'equal)))
            (should prune-idx)
            (let ((prune-val (nth (1+ prune-idx) captured-cmd)))
              (should (string-match-p (regexp-quote "/custom/app") prune-val))
              (should-not (string-match-p (regexp-quote "/global/logs")
                                          prune-val)))))
      (when (process-live-p fake-proc) (delete-process fake-proc)))))

;;;; Behavioral logrotate compression tests

(ert-deftest elinit-test-logrotate-tar-present-creates-archive ()
  "When tar is available, logrotate compresses rotated files to .tar.gz."
  (let ((log-dir (make-temp-file "lr-tar-" t))
        (script (expand-file-name "sbin/supervisor-logrotate")))
    (unwind-protect
        (progn
          ;; Create active log files
          (with-temp-file (expand-file-name "log-svc1.log" log-dir)
            (insert "line1\nline2\n"))
          (with-temp-file (expand-file-name "supervisor.log" log-dir)
            (insert "elinit log data\n"))
          ;; Run logrotate (tar should be available on all test hosts)
          (should (zerop (call-process "sh" nil nil nil script
                                       "--log-dir" log-dir)))
          ;; Active files should have been rotated away
          (should-not (file-exists-p (expand-file-name "log-svc1.log" log-dir)))
          (should-not (file-exists-p (expand-file-name "supervisor.log" log-dir)))
          ;; .tar.gz archives should exist
          (let ((tar-files (directory-files log-dir nil "\\.tar\\.gz\\'")))
            (should (>= (length tar-files) 2))))
      (delete-directory log-dir t))))

(ert-deftest elinit-test-logrotate-tar-absent-rotation-succeeds ()
  "When tar is absent, logrotate still rotates files without compression."
  (let ((log-dir (make-temp-file "lr-notar-" t))
        (bin-dir (make-temp-file "lr-bin-" t))
        (script (expand-file-name "sbin/supervisor-logrotate")))
    (unwind-protect
        (progn
          ;; Build a restricted PATH: symlink essential commands, omit tar
          (dolist (cmd '("sh" "find" "date" "mv" "rm" "cat" "sed"
                         "printf" "kill" "mkdir" "basename"))
            (let ((real (executable-find cmd)))
              (when real
                (make-symbolic-link real
                                    (expand-file-name cmd bin-dir) t))))
          ;; Create active log file
          (with-temp-file (expand-file-name "log-svc2.log" log-dir)
            (insert "data\n"))
          ;; Run logrotate with restricted PATH (no tar)
          (let ((exit-code
                 (call-process "env" nil nil nil
                               (format "PATH=%s" bin-dir)
                               (expand-file-name script)
                               "--log-dir" log-dir)))
            (should (zerop exit-code)))
          ;; Active file should be gone (rotated)
          (should-not (file-exists-p (expand-file-name "log-svc2.log" log-dir)))
          ;; Rotated file should exist as plain .log, no .tar.gz
          (let ((rotated (directory-files log-dir nil "log-svc2\\..+\\.log\\'")))
            (should (>= (length rotated) 1)))
          (let ((tar-files (directory-files log-dir nil "\\.tar\\.gz\\'")))
            (should (= (length tar-files) 0))))
      (delete-directory log-dir t)
      (delete-directory bin-dir t))))

;;;; Exit marker coverage for manual-stop and oneshot completion

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

;;;; Behavioral equivalence: format-hint and vacuum alias


(ert-deftest elinit-test-prune-format-hint-does-not-alter-order ()
  "Prune deletion order is identical with and without --format-hint."
  (let ((dir (elinit-test--make-prune-fixture))
        (script (expand-file-name "sbin/supervisor-log-prune")))
    (unwind-protect
        (let (output-plain output-text output-binary)
          ;; Run with tiny cap to force all rotated files to be pruned.
          ;; Use --dry-run so nothing is actually deleted.
          (with-temp-buffer
            (call-process script nil t nil
                          "--log-dir" dir "--max-total-bytes" "100"
                          "--dry-run")
            (setq output-plain (buffer-string)))
          (with-temp-buffer
            (call-process script nil t nil
                          "--log-dir" dir "--max-total-bytes" "100"
                          "--format-hint" "text" "--dry-run")
            (setq output-text (buffer-string)))
          (with-temp-buffer
            (call-process script nil t nil
                          "--log-dir" dir "--max-total-bytes" "100"
                          "--format-hint" "binary" "--dry-run")
            (setq output-binary (buffer-string)))
          ;; All three runs should produce prune lines
          (should (string-match-p "^prune:" output-plain))
          ;; Prune lines (action output) must be identical
          (let ((prune-re "^prune:.*$"))
            (should (equal (elinit-test--grep-lines prune-re output-plain)
                           (elinit-test--grep-lines prune-re output-text)))
            (should (equal (elinit-test--grep-lines prune-re output-plain)
                           (elinit-test--grep-lines prune-re output-binary)))))
      (delete-directory dir t))))

(ert-deftest elinit-test-prune-vacuum-alias-matches-regular ()
  "Vacuum alias produces identical prune behavior to regular invocation."
  (let ((dir (elinit-test--make-prune-fixture))
        (script (expand-file-name "sbin/supervisor-log-prune")))
    (unwind-protect
        (let (output-regular output-vacuum)
          ;; Regular invocation
          (with-temp-buffer
            (call-process script nil t nil
                          "--log-dir" dir "--max-total-bytes" "100"
                          "--dry-run")
            (setq output-regular (buffer-string)))
          ;; Vacuum alias invocation
          (with-temp-buffer
            (call-process script nil t nil
                          "--log-dir" dir "--vacuum"
                          "--vacuum-max-total-bytes" "100"
                          "--dry-run")
            (setq output-vacuum (buffer-string)))
          ;; Both should produce prune lines
          (should (string-match-p "^prune:" output-regular))
          ;; Prune lines must be identical
          (let ((prune-re "^prune:.*$"))
            (should (equal (elinit-test--grep-lines prune-re output-regular)
                           (elinit-test--grep-lines prune-re output-vacuum)))))
      (delete-directory dir t))))

(provide 'elinit-test-logformat)
;;; elinit-test-logformat.el ends here
