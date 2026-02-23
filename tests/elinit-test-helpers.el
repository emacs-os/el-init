;;; elinit-test-helpers.el --- Shared test helpers for elinit.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Shared macros, functions, and variables used by elinit test files.

;;; Code:

(require 'ert)
(require 'elinit)

(defmacro elinit-test-without-builtins (&rest body)
  "Execute BODY with built-in programs and timers suppressed."
  (declare (indent 0) (debug (body)))
  `(let ((elinit--builtin-timers nil)
         (elinit-seed-default-maintenance-units nil))
     (cl-letf (((symbol-function 'elinit--builtin-programs)
                (lambda () nil)))
       ,@body)))

(defun elinit-test--write-unit-files (dir programs)
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

(defmacro elinit-test-with-unit-files (programs &rest body)
  "Execute BODY with PROGRAMS written as unit files in a temp directory.
Binds `elinit-unit-authority-path' and `elinit-unit-directory'
to the temp dir, clears `elinit--unit-file-invalid' and
`elinit--programs-cache', suppresses built-in programs and timers,
and cleans up afterward."
  (declare (indent 1) (debug (form body)))
  `(elinit-test-without-builtins
     (let* ((dir--temp (make-temp-file "units-" t))
            (elinit-unit-authority-path (list dir--temp))
            (elinit-unit-directory dir--temp)
            (elinit--programs-cache :not-yet-loaded)
            (elinit--unit-file-invalid (make-hash-table :test 'equal)))
       (elinit-test--write-unit-files dir--temp ,programs)
       (unwind-protect
           (progn ,@body)
         (delete-directory dir--temp t)))))

(defmacro elinit-test-with-authority-tiers (n &rest body)
  "Execute BODY with N authority tier directories.
Bind `dir1' through `dirN' to temp directories, set
`elinit-unit-authority-path' to (dir1 ... dirN) (low to high
precedence), clear caches, suppress built-in programs and timers,
and clean up afterward."
  (declare (indent 1) (debug (form body)))
  (let ((dir-syms (cl-loop for i from 1 to n
                            collect (intern (format "dir%d" i)))))
    `(elinit-test-without-builtins
       (let* (,@(mapcar (lambda (sym)
                          `(,sym (make-temp-file "tier-" t)))
                        dir-syms)
              (elinit-unit-authority-path (list ,@dir-syms))
              (elinit-unit-directory ,(car (last dir-syms)))
              (elinit--programs-cache :not-yet-loaded)
              (elinit--unit-file-invalid (make-hash-table :test 'equal)))
         (unwind-protect
             (progn ,@body)
           ,@(mapcar (lambda (sym)
                       `(delete-directory ,sym t))
                     dir-syms))))))

(defun elinit-test--ensure-logd-binary ()
  "Build elinit-logd if missing or stale, return path.
Compares source mtime against binary mtime to detect staleness."
  (let* ((root (file-name-directory (locate-library "elinit")))
         (logd (expand-file-name "libexec/elinit-logd" root))
         (src (expand-file-name "libexec/elinit-logd.c" root))
         (stale (and (file-exists-p logd)
                     (file-exists-p src)
                     (time-less-p
                      (nth 5 (file-attributes logd))
                      (nth 5 (file-attributes src))))))
    (when (or (not (file-executable-p logd)) stale)
      (let ((result (elinit-build-libexec-helpers t)))
        (unless (file-executable-p logd)
          (error "Cannot build elinit-logd: %S"
                 (or (plist-get result :failed)
                     (plist-get result :missing-source))))))
    logd))

(defvar elinit-test-runas-binary
  (expand-file-name "libexec/elinit-runas"
                    (file-name-directory
                     (directory-file-name
                      (file-name-directory (or load-file-name
                                               buffer-file-name
                                               default-directory)))))
  "Path to compiled elinit-runas binary for testing.")

(defun elinit-test--make-binary-record (event stream pid unit-id
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

(defun elinit-test--make-prune-fixture ()
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

(defun elinit-test--grep-lines (regexp str)
  "Return all lines in STR matching REGEXP, preserving order."
  (let ((lines nil))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (push (match-string 0) lines)))
    (nreverse lines)))

(provide 'elinit-test-helpers)
;;; elinit-test-helpers.el ends here
