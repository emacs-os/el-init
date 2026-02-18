;;; supervisor-test-helpers.el --- Shared test helpers for supervisor.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Shared macros, functions, and variables used by supervisor test files.

;;; Code:

(require 'ert)
(require 'supervisor)

(defmacro supervisor-test-without-builtins (&rest body)
  "Execute BODY with built-in programs and timers suppressed."
  (declare (indent 0) (debug (body)))
  `(let ((supervisor--builtin-timers nil)
         (supervisor-seed-default-maintenance-units nil))
     (cl-letf (((symbol-function 'supervisor--builtin-programs)
                (lambda () nil)))
       ,@body)))

(defun supervisor-test--write-unit-files (dir programs)
  "Write PROGRAMS list as unit files in DIR.
Each entry in PROGRAMS is either a string (bare command) or
\(COMMAND . PLIST).  Generates one `.el' unit file per entry."
  (let ((idx 0))
    (dolist (entry programs)
      (let* ((cmd (if (stringp entry) entry (car entry)))
             (plist (if (stringp entry) nil (cdr entry)))
             (id (or (plist-get plist :id)
                     (format "entry%d" idx)))
             (file (expand-file-name (concat id ".el") dir)))
        ;; Build the unit-file plist
        (with-temp-file file
          (insert (format "(:id %S\n :command %S" id cmd))
          (let ((keys plist))
            (while keys
              (let ((key (car keys))
                    (val (cadr keys)))
                (unless (eq key :id)
                  (insert (format "\n %S %S" key val))))
              (setq keys (cddr keys))))
          (insert ")\n"))
        (cl-incf idx)))))

(defmacro supervisor-test-with-unit-files (programs &rest body)
  "Execute BODY with PROGRAMS written as unit files in a temp directory.
Binds `supervisor-unit-authority-path' and `supervisor-unit-directory'
to the temp dir, clears `supervisor--unit-file-invalid' and
`supervisor--programs-cache', suppresses built-in programs and timers,
and cleans up afterward."
  (declare (indent 1) (debug (form body)))
  `(supervisor-test-without-builtins
     (let* ((dir--temp (make-temp-file "units-" t))
            (supervisor-unit-authority-path (list dir--temp))
            (supervisor-unit-directory dir--temp)
            (supervisor--programs-cache :not-yet-loaded)
            (supervisor--unit-file-invalid (make-hash-table :test 'equal)))
       (supervisor-test--write-unit-files dir--temp ,programs)
       (unwind-protect
           (progn ,@body)
         (delete-directory dir--temp t)))))

(defmacro supervisor-test-with-authority-tiers (n &rest body)
  "Execute BODY with N authority tier directories.
Bind `dir1' through `dirN' to temp directories, set
`supervisor-unit-authority-path' to (dir1 ... dirN) (low to high
precedence), clear caches, suppress built-in programs and timers,
and clean up afterward."
  (declare (indent 1) (debug (form body)))
  (let ((dir-syms (cl-loop for i from 1 to n
                            collect (intern (format "dir%d" i)))))
    `(supervisor-test-without-builtins
       (let* (,@(mapcar (lambda (sym)
                          `(,sym (make-temp-file "tier-" t)))
                        dir-syms)
              (supervisor-unit-authority-path (list ,@dir-syms))
              (supervisor-unit-directory ,(car (last dir-syms)))
              (supervisor--programs-cache :not-yet-loaded)
              (supervisor--unit-file-invalid (make-hash-table :test 'equal)))
         (unwind-protect
             (progn ,@body)
           ,@(mapcar (lambda (sym)
                       `(delete-directory ,sym t))
                     dir-syms))))))

(defun supervisor-test--ensure-logd-binary ()
  "Build supervisor-logd if missing or stale, return path.
Compares source mtime against binary mtime to detect staleness."
  (let* ((root (file-name-directory (locate-library "supervisor")))
         (logd (expand-file-name "libexec/supervisor-logd" root))
         (src (expand-file-name "libexec/supervisor-logd.c" root))
         (stale (and (file-exists-p logd)
                     (file-exists-p src)
                     (time-less-p
                      (nth 5 (file-attributes logd))
                      (nth 5 (file-attributes src))))))
    (when (or (not (file-executable-p logd)) stale)
      (let ((result (supervisor-build-libexec-helpers t)))
        (unless (file-executable-p logd)
          (error "Cannot build supervisor-logd: %S"
                 (or (plist-get result :failed)
                     (plist-get result :missing-source))))))
    logd))

(defvar supervisor-test-runas-binary
  (expand-file-name "libexec/supervisor-runas"
                    (file-name-directory
                     (directory-file-name
                      (file-name-directory (or load-file-name
                                               buffer-file-name
                                               default-directory)))))
  "Path to compiled supervisor-runas binary for testing.")

(defun supervisor-test--make-binary-record (event stream pid unit-id
                                                  payload exit-code
                                                  exit-status ts-ns)
  "Build a binary log record for testing.
EVENT, STREAM, PID, UNIT-ID, PAYLOAD, EXIT-CODE, EXIT-STATUS, and
TS-NS are the record fields."
  (let* ((unit-bytes (encode-coding-string unit-id 'utf-8))
         (unit-len (length unit-bytes))
         (payload-bytes (if payload
                            (encode-coding-string payload 'utf-8)
                          ""))
         (payload-len (length payload-bytes))
         ;; record_len = 30 (header after len) + unit_len + payload_len
         (record-len (+ 30 unit-len payload-len))
         (hdr (make-string 34 0)))
    ;; u32be record_len
    (aset hdr 0 (logand (ash record-len -24) #xff))
    (aset hdr 1 (logand (ash record-len -16) #xff))
    (aset hdr 2 (logand (ash record-len -8) #xff))
    (aset hdr 3 (logand record-len #xff))
    ;; version
    (aset hdr 4 1)
    ;; event
    (aset hdr 5 event)
    ;; stream
    (aset hdr 6 stream)
    ;; reserved
    (aset hdr 7 0)
    ;; u64be timestamp_ns
    (dotimes (i 8)
      (aset hdr (+ 8 i) (logand (ash ts-ns (* -8 (- 7 i))) #xff)))
    ;; u32be pid
    (aset hdr 16 (logand (ash pid -24) #xff))
    (aset hdr 17 (logand (ash pid -16) #xff))
    (aset hdr 18 (logand (ash pid -8) #xff))
    (aset hdr 19 (logand pid #xff))
    ;; u16be unit_len
    (aset hdr 20 (logand (ash unit-len -8) #xff))
    (aset hdr 21 (logand unit-len #xff))
    ;; i32be exit_code
    (let ((ec (logand exit-code #xffffffff)))
      (aset hdr 22 (logand (ash ec -24) #xff))
      (aset hdr 23 (logand (ash ec -16) #xff))
      (aset hdr 24 (logand (ash ec -8) #xff))
      (aset hdr 25 (logand ec #xff)))
    ;; exit_status
    (aset hdr 26 exit-status)
    ;; reserved[3] + u32be payload_len
    (aset hdr 27 0) (aset hdr 28 0) (aset hdr 29 0)
    (aset hdr 30 (logand (ash payload-len -24) #xff))
    (aset hdr 31 (logand (ash payload-len -16) #xff))
    (aset hdr 32 (logand (ash payload-len -8) #xff))
    (aset hdr 33 (logand payload-len #xff))
    (string-to-unibyte (concat hdr unit-bytes payload-bytes))))

(defun supervisor-test--make-prune-fixture ()
  "Create a temp log dir with active + rotated files for prune tests.
Return the directory path.  Caller must `delete-directory' when done.
The fixture contains an active log and three rotated files with
staggered mtimes so prune ordering is deterministic (oldest first)."
  (let ((dir (make-temp-file "prune-fix-" t)))
    ;; Active log (never pruned)
    (with-temp-file (expand-file-name "log-svc.log" dir)
      (insert (make-string 64 ?A)))
    ;; Rotated files: 1024 bytes each, different timestamps in name
    ;; and different mtime so sort-by-mtime is deterministic.
    (let ((files '(("log-svc.20240101-120000.log" . "202401011200.00")
                   ("log-svc.20240201-120000.log" . "202402011200.00")
                   ("log-svc.20240301-120000.log" . "202403011200.00"))))
      (dolist (entry files)
        (let ((path (expand-file-name (car entry) dir)))
          (with-temp-file path
            (insert (make-string 1024 ?R)))
          (call-process "touch" nil nil nil "-t" (cdr entry) path))))
    dir))

(defun supervisor-test--grep-lines (regexp str)
  "Return all lines in STR matching REGEXP, preserving order."
  (let ((lines nil))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (push (match-string 0) lines)))
    (nreverse lines)))

(provide 'supervisor-test-helpers)
;;; supervisor-test-helpers.el ends here
