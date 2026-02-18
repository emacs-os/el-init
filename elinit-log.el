;;; elinit-log.el --- Logging subsystem for elinit.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; Author: telecommuter <telecommuter@riseup.net>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of elinit.el.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Logging subsystem for elinit.el: log framing, encode/decode,
;; record filtering/formatting, and writer lifecycle management.
;; Run M-x elinit-handbook for full documentation.

;;; Code:

(require 'cl-lib)

(declare-function json-encode "json" (object))

;; External variables defined in elinit-core.el
(defvar elinit-log-directory)
(defvar elinit-logd-pid-directory)
(defvar elinit-logd-command)
(defvar elinit-logrotate-command)
(defvar elinit-log-prune-command)
(defvar elinit-logrotate-keep-days)
(defvar elinit-logd-max-file-size)
(defvar elinit-log-prune-max-total-bytes)
(defvar elinit-logd-prune-min-interval)
(defvar elinit--writers)
(defvar elinit--stderr-writers)
(defvar elinit--stderr-pipes)
(defvar elinit--processes)

;; Forward declaration for core event logger
(declare-function elinit--log "elinit-core"
                  (level format-string &rest args))

;;; Logging

(defvar elinit--last-log-directory-warning nil
  "Last warning string emitted for log-directory fallback issues.")

(defun elinit--default-log-directory ()
  "Return the default user-local log directory."
  (expand-file-name "elinit" user-emacs-directory))

(defun elinit--warn-log-directory (format-string &rest args)
  "Emit a throttled warning for log-directory issues.
Use FORMAT-STRING and ARGS to compose the warning text.
This uses `message' directly to avoid recursive file logging when the
configured log directory is unavailable."
  (let ((warning (apply #'format format-string args)))
    (unless (equal warning elinit--last-log-directory-warning)
      (setq elinit--last-log-directory-warning warning)
      (message "Elinit: WARNING - %s" warning))))

(defun elinit--ensure-directory-writable (directory)
  "Ensure DIRECTORY exists and is writable.
Return DIRECTORY when usable, otherwise return nil."
  (condition-case nil
      (progn
        (unless (file-directory-p directory)
          (make-directory directory t))
        (and (file-directory-p directory)
             (file-writable-p directory)
             directory))
    (error nil)))

(defun elinit--effective-log-directory ()
  "Return a writable log directory path, or nil.
Prefer `elinit-log-directory'.  If it is not writable, fall back to
`elinit--default-log-directory'.  If neither is writable, return nil."
  (let* ((configured elinit-log-directory)
         (configured-dir (elinit--ensure-directory-writable configured))
         (fallback (elinit--default-log-directory))
         (fallback-dir (and (not configured-dir)
                            (not (equal configured fallback))
                            (elinit--ensure-directory-writable fallback))))
    (cond
     (configured-dir configured-dir)
     (fallback-dir
      (elinit--warn-log-directory
       "log directory %s is not writable; using %s"
       configured fallback-dir)
      fallback-dir)
     (t
      (elinit--warn-log-directory
       "log directory %s is not writable; file logging disabled"
       configured)
      nil))))

(defun elinit--ensure-log-directory ()
  "Return the effective writable log directory, or nil."
  (elinit--effective-log-directory))

(defun elinit--log-file (prog)
  "Return the log file path for PROG, or nil."
  (when-let* ((log-directory (elinit--ensure-log-directory)))
    (expand-file-name (format "log-%s.log" prog) log-directory)))

;;;; Log framing protocol
;;
;; Wire format for structured transport (Elisp -> logd stdin):
;;   u32be total_frame_len   (bytes after this 4-byte header)
;;   u8    event             (1=output, 2=exit)
;;   u8    stream            (1=stdout, 2=stderr, 3=meta)
;;   u32be pid
;;   u16be unit_len
;;   i32be exit_code         (0 for output events)
;;   u8    exit_status       (0=none, 1=exited, 2=signaled, 3=spawn-failed)
;;   <unit_len bytes>        unit_id (UTF-8)
;;   <remaining bytes>       payload

(defun elinit--log-frame-encode (event stream pid unit-id
                                          &optional payload exit-code
                                          exit-status)
  "Encode a log transport frame as a unibyte string.
EVENT is 1 (output) or 2 (exit).  STREAM is 1 (stdout), 2 (stderr),
or 3 (meta).  PID is the process ID.  UNIT-ID is the service name.
PAYLOAD is an optional unibyte string of output data.
EXIT-CODE is the process exit code (default 0).
EXIT-STATUS is 0 (none), 1 (exited), 2 (signaled), or 3 (spawn-failed)."
  (let* ((unit-bytes (encode-coding-string unit-id 'utf-8))
         (unit-len (length unit-bytes))
         (payload-bytes (if payload
                            (if (multibyte-string-p payload)
                                (encode-coding-string payload 'utf-8)
                              payload)
                          ""))
         (payload-len (length payload-bytes))
         (ec (or exit-code 0))
         (es (or exit-status 0))
         ;; Fixed header: event(1) + stream(1) + pid(4) + unit_len(2)
         ;;             + exit_code(4) + exit_status(1) = 13
         (body-len (+ 13 unit-len payload-len))
         (frame (make-string (+ 4 body-len) 0)))
    ;; u32be total_frame_len
    (aset frame 0 (logand (ash body-len -24) #xff))
    (aset frame 1 (logand (ash body-len -16) #xff))
    (aset frame 2 (logand (ash body-len -8) #xff))
    (aset frame 3 (logand body-len #xff))
    ;; u8 event
    (aset frame 4 event)
    ;; u8 stream
    (aset frame 5 stream)
    ;; u32be pid
    (aset frame 6 (logand (ash pid -24) #xff))
    (aset frame 7 (logand (ash pid -16) #xff))
    (aset frame 8 (logand (ash pid -8) #xff))
    (aset frame 9 (logand pid #xff))
    ;; u16be unit_len
    (aset frame 10 (logand (ash unit-len -8) #xff))
    (aset frame 11 (logand unit-len #xff))
    ;; i32be exit_code (two's complement for negative values)
    (let ((ec-unsigned (logand ec #xffffffff)))
      (aset frame 12 (logand (ash ec-unsigned -24) #xff))
      (aset frame 13 (logand (ash ec-unsigned -16) #xff))
      (aset frame 14 (logand (ash ec-unsigned -8) #xff))
      (aset frame 15 (logand ec-unsigned #xff)))
    ;; u8 exit_status
    (aset frame 16 es)
    ;; unit_id bytes
    (dotimes (i unit-len)
      (aset frame (+ 17 i) (aref unit-bytes i)))
    ;; payload bytes
    (dotimes (i payload-len)
      (aset frame (+ 17 unit-len i) (aref payload-bytes i)))
    (string-to-unibyte frame)))

(defun elinit--log-send-frame (writer event stream pid unit-id
                                         &optional payload exit-code
                                         exit-status)
  "Send a framed log event to WRITER process.
EVENT, STREAM, PID, UNIT-ID, PAYLOAD, EXIT-CODE, and EXIT-STATUS are
passed to `elinit--log-frame-encode'.  Silently ignores errors
when the writer process has died."
  (condition-case nil
      (process-send-string
       writer
       (elinit--log-frame-encode event stream pid unit-id
                                     payload exit-code exit-status))
    (error nil)))

(defun elinit--exit-status-code (status)
  "Map exit STATUS symbol to wire protocol value.
Return 1 for `exited', 2 for `signal', 0 for anything else."
  (pcase status
    ('exited 1)
    ('signal 2)
    (_ 0)))

;;;; Binary log decoder

(defconst elinit--log-binary-magic "SLG1"
  "Magic bytes at the start of a binary structured log file.")

(defconst elinit--log-binary-header-size 34
  "Size of a single binary record header in bytes.
Layout: u32be record_len, u8 version, u8 event, u8 stream,
u8 reserved, u64be timestamp_ns, u32be pid, u16be unit_len,
i32be exit_code, u8 exit_status, u8[3] reserved, u32be payload_len.")

(defun elinit--log-read-u32be (str offset)
  "Read big-endian u32 from unibyte string STR at OFFSET."
  (logior (ash (aref str offset) 24)
          (ash (aref str (+ offset 1)) 16)
          (ash (aref str (+ offset 2)) 8)
          (aref str (+ offset 3))))

(defun elinit--log-read-u16be (str offset)
  "Read big-endian u16 from unibyte string STR at OFFSET."
  (logior (ash (aref str offset) 8)
          (aref str (+ offset 1))))

(defun elinit--log-read-u64be (str offset)
  "Read big-endian u64 from unibyte string STR at OFFSET.
Return the value as an Emacs integer (may lose precision on 32-bit)."
  (let ((hi (elinit--log-read-u32be str offset))
        (lo (elinit--log-read-u32be str (+ offset 4))))
    (+ (ash hi 32) lo)))

(defun elinit--log-read-i32be (str offset)
  "Read big-endian i32 (signed) from unibyte string STR at OFFSET."
  (let ((u (elinit--log-read-u32be str offset)))
    (if (>= u #x80000000)
        (- u #x100000000)
      u)))

(defun elinit--log-wire-event (code)
  "Map wire event CODE to symbol."
  (pcase code
    (1 'output)
    (2 'exit)
    (_ (error "Unknown event code: %d" code))))

(defun elinit--log-wire-stream (code)
  "Map wire stream CODE to symbol."
  (pcase code
    (1 'stdout)
    (2 'stderr)
    (3 'meta)
    (_ (error "Unknown stream code: %d" code))))

(defun elinit--log-wire-exit-status (code)
  "Map wire exit status CODE to symbol."
  (pcase code
    (0 nil)
    (1 'exited)
    (2 'signaled)
    (3 'spawn-failed)
    (_ (error "Unknown exit status code: %d" code))))

(defun elinit--log-decode-binary-records (str &optional offset)
  "Parse binary log buffer STR into record plists.
OFFSET is the byte position to start parsing (default 0, skips
SLG1 magic if present at start).  Return a plist
\(:records LIST :offset INT :warning STRING-OR-NIL)."
  (let* ((pos (or offset 0))
         (len (length str))
         (records nil)
         (warning nil))
    ;; Skip magic header if at start
    (when (and (= pos 0) (>= len 4)
               (equal (substring str 0 4) elinit--log-binary-magic))
      (setq pos 4))
    (while (and (< pos len) (not warning))
      (let ((remaining (- len pos)))
        (if (< remaining elinit--log-binary-header-size)
            ;; Incomplete trailing record
            (setq warning (format "truncated record at offset %d" pos))
          (let* ((record-len (elinit--log-read-u32be str pos))
                 (total (+ 4 record-len)))
            (if (> total remaining)
                ;; Incomplete record body
                (setq warning (format "truncated record at offset %d" pos))
              (let* ((base (+ pos 4))
                     (version (aref str base))
                     (event-code (aref str (+ base 1)))
                     (stream-code (aref str (+ base 2)))
                     (ts-ns (elinit--log-read-u64be str (+ base 4)))
                     (pid (elinit--log-read-u32be str (+ base 12)))
                     (unit-len (elinit--log-read-u16be str (+ base 16)))
                     (exit-code (elinit--log-read-i32be str (+ base 18)))
                     (exit-status-code (aref str (+ base 22)))
                     (payload-len (elinit--log-read-u32be str (+ base 26)))
                     (expected-len (+ 30 unit-len payload-len))
                     (unit-start (+ base 30))
                     (payload-start (+ unit-start unit-len)))
                (when (/= version 1)
                  (error "Unknown binary log version: %d" version))
                (unless (= record-len expected-len)
                  (error "Binary record length mismatch at offset %d: \
declared %d, computed %d" pos record-len expected-len))
                (condition-case err
                    (let ((event (elinit--log-wire-event event-code))
                          (stream (elinit--log-wire-stream stream-code))
                          (exit-status (elinit--log-wire-exit-status
                                        exit-status-code))
                          (unit (substring str unit-start
                                           (+ unit-start unit-len)))
                          (payload (substring str payload-start
                                              (+ payload-start payload-len)))
                          (ts (/ (float ts-ns) 1e9)))
                      (push (list :ts ts :unit unit :pid pid
                                  :stream stream :event event
                                  :status exit-status :code exit-code
                                  :payload payload)
                            records))
                  (error
                   (error "Binary decode error at offset %d: %s"
                          pos (error-message-string err))))
                (setq pos (+ pos total))))))))
    (list :records (nreverse records) :offset pos :warning warning)))

(defun elinit--log-binary-scan-to-record (str)
  "Return offset of first valid binary record in STR, or nil.
Scan by trying each byte position as a potential record start.
Read u32be length, check that 4+length fits remaining data and
that the version byte is 1.  Only called on tail-read chunks
so linear scan is acceptable."
  (let ((len (length str))
        (found nil))
    (cl-loop for pos from 0 below len
             until found
             do (when (>= (- len pos) elinit--log-binary-header-size)
                  (let* ((record-len (elinit--log-read-u32be str pos))
                         (total (+ 4 record-len)))
                    (when (and (<= total (- len pos))
                               (> record-len 0)
                               (= 1 (aref str (+ pos 4))))
                      (setq found pos)))))
    found))

;;;; Log decoders and record utilities

(defun elinit--log-unescape-payload (escaped)
  "Reverse text-format escaping of ESCAPED payload string.
Convert \\\\->\\, \\n->newline, \\r->CR, \\t->tab, \\xNN->byte."
  (let ((result (make-string (length escaped) 0))
        (rpos 0)
        (i 0)
        (len (length escaped)))
    (while (< i len)
      (if (and (= (aref escaped i) ?\\) (< (1+ i) len))
          (let ((next (aref escaped (1+ i))))
            (pcase next
              (?\\
               (aset result rpos ?\\)
               (setq rpos (1+ rpos) i (+ i 2)))
              (?n
               (aset result rpos ?\n)
               (setq rpos (1+ rpos) i (+ i 2)))
              (?r
               (aset result rpos ?\r)
               (setq rpos (1+ rpos) i (+ i 2)))
              (?t
               (aset result rpos ?\t)
               (setq rpos (1+ rpos) i (+ i 2)))
              (?x
               (if (< (+ i 3) len)
                   (let ((hex (substring escaped (+ i 2) (+ i 4))))
                     (aset result rpos (string-to-number hex 16))
                     (setq rpos (1+ rpos) i (+ i 4)))
                 (aset result rpos (aref escaped i))
                 (setq rpos (1+ rpos) i (1+ i))))
              (_
               (aset result rpos (aref escaped i))
               (setq rpos (1+ rpos) i (1+ i)))))
        (aset result rpos (aref escaped i))
        (setq rpos (1+ rpos) i (1+ i))))
    (substring result 0 rpos)))

(defun elinit--days-in-month (month year)
  "Return the number of days in MONTH for YEAR."
  (pcase month
    ((or 1 3 5 7 8 10 12) 31)
    ((or 4 6 9 11) 30)
    (2 (if (and (= 0 (% year 4))
                (or (/= 0 (% year 100))
                    (= 0 (% year 400))))
           29 28))))

(defun elinit--log-parse-timestamp (ts-string)
  "Parse timestamp TS-STRING to float seconds.
Accept RFC3339 format or epoch integer."
  (cond
   ((string-match
     "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\(?:\\.\\([0-9]+\\)\\)?Z\\'"
     ts-string)
    (let* ((year (string-to-number (match-string 1 ts-string)))
           (month (string-to-number (match-string 2 ts-string)))
           (day (string-to-number (match-string 3 ts-string)))
           (hour (string-to-number (match-string 4 ts-string)))
           (min (string-to-number (match-string 5 ts-string)))
           (sec (string-to-number (match-string 6 ts-string)))
           (frac-str (match-string 7 ts-string))
           (frac (if frac-str
                     (/ (float (string-to-number frac-str))
                        (expt 10.0 (length frac-str)))
                   0.0)))
      (when (and (<= 1 month 12)
                 (<= 1 day (elinit--days-in-month month year))
                 (<= 0 hour 23)
                 (<= 0 min 59)
                 (<= 0 sec 60))
        (+ (float-time (encode-time (list sec min hour day month year
                                          nil nil t 0)))
           frac))))
   ((string-match "\\`[0-9]+\\'" ts-string)
    (float (string-to-number ts-string)))
   (t nil)))

(defun elinit--log-decode-text-records (str)
  "Parse text-format structured log STR into record plists.
Each line has the format: ts=TS unit=UNIT pid=PID stream=STREAM
event=EVENT status=STATUS code=CODE payload=PAYLOAD."
  (let ((records nil)
        (lines (split-string str "\n" t)))
    (dolist (line lines)
      (when (string-match
             "\\`ts=\\([^ ]+\\) unit=\\([^ ]+\\) pid=\\([0-9]+\\) stream=\\([^ ]+\\) event=\\([^ ]+\\) status=\\([^ ]+\\) code=\\([^ ]+\\) payload=\\(.*\\)\\'"
             line)
        (let* ((ts-str (match-string 1 line))
               (unit (match-string 2 line))
               (pid (string-to-number (match-string 3 line)))
               (stream (intern (match-string 4 line)))
               (event (intern (match-string 5 line)))
               (status-str (match-string 6 line))
               (code-str (match-string 7 line))
               (payload-raw (match-string 8 line))
               (ts (elinit--log-parse-timestamp ts-str))
               (status (unless (equal status-str "-")
                         (intern status-str)))
               (code (if (equal code-str "-") 0
                       (string-to-number code-str)))
               (payload (cond
                         ;; Exit events use "-" as absent-payload sentinel
                         ((and (eq event 'exit) (equal payload-raw "-")) "")
                         ;; Output events: always unescape (even empty)
                         (t (elinit--log-unescape-payload payload-raw)))))
          (push (list :ts ts :unit unit :pid pid
                      :stream stream :event event
                      :status status :code code
                      :payload payload)
                records))))
    (nreverse records)))

(defun elinit--log-decode-legacy-lines (str)
  "Wrap raw log lines in STR as output record plists.
Legacy logs have no structured metadata."
  (let ((records nil)
        (lines (split-string str "\n" t)))
    (dolist (line lines)
      (push (list :ts nil :unit nil :pid 0
                  :stream 'stdout :event 'output
                  :status nil :code 0
                  :payload line)
            records))
    (nreverse records)))

(defun elinit--log-detect-format (file)
  "Detect log format of FILE by reading first bytes.
Return `binary', `text', or `legacy'."
  (condition-case nil
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file nil 0 256)
        (cond
         ((and (>= (buffer-size) 4)
               (equal (buffer-substring 1 5) elinit--log-binary-magic))
          'binary)
         ((string-match "\\`ts=" (buffer-substring 1 (min 4 (1+ (buffer-size)))))
          'text)
         (t 'legacy)))
    (error 'legacy)))

(defun elinit--log-decode-file (file &optional limit offset max-bytes)
  "Decode structured log FILE into records.
LIMIT is maximum number of records to return (from end).
OFFSET is byte offset to start reading (for incremental reads).
MAX-BYTES, when non-nil and OFFSET is nil, reads only the last
MAX-BYTES of the file (tail read).  The first record in a tail
chunk may be truncated; text/legacy decoders skip malformed lines,
and for binary the chunk is scanned to find the first valid record
boundary.
Return (:records LIST :offset INT :format SYMBOL :warning STRING-OR-NIL)."
  (let ((fmt (elinit--log-detect-format file)))
    (condition-case err
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (let ((tail-offset nil))
            (cond
             (offset
              (insert-file-contents-literally file nil offset))
             ((and max-bytes (not offset))
              (let ((file-size (file-attribute-size (file-attributes file))))
                (if (and file-size (> file-size max-bytes))
                    (progn
                      (setq tail-offset (- file-size max-bytes))
                      (insert-file-contents-literally
                       file nil tail-offset file-size))
                  (insert-file-contents-literally file))))
             (t
              (insert-file-contents-literally file)))
            (let* ((content (buffer-string))
                   (read-start (or tail-offset offset 0))
                   (end-offset (+ read-start (length content))))
              (pcase fmt
                ('binary
                 (let* ((scan-start
                         (cond
                          (offset 0)
                          (tail-offset
                           (or (elinit--log-binary-scan-to-record content)
                               (length content)))
                          (t nil)))
                        (result (elinit--log-decode-binary-records
                                 content scan-start)))
                   (let ((records (plist-get result :records)))
                     (list :records (if (and limit (> (length records) limit))
                                        (last records limit)
                                      records)
                           :offset (+ read-start
                                      (plist-get result :offset))
                           :format 'binary
                           :warning (plist-get result :warning)))))
                ('text
                 (let* ((str (decode-coding-string content 'utf-8))
                        (records (elinit--log-decode-text-records str)))
                   (list :records (if (and limit (> (length records) limit))
                                       (last records limit)
                                     records)
                         :offset end-offset
                         :format 'text
                         :warning nil)))
                (_
                 (let* ((str (decode-coding-string content 'utf-8))
                        (records (elinit--log-decode-legacy-lines str)))
                   (list :records (if (and limit (> (length records) limit))
                                       (last records limit)
                                     records)
                         :offset end-offset
                         :format 'legacy
                         :warning nil)))))))
      (error (list :records nil :offset 0 :format fmt
                   :warning (error-message-string err))))))

(defun elinit--log-record-priority (record)
  "Return priority symbol for RECORD.
Stderr output and non-clean exits return `err', else `info'."
  (cond
   ((and (eq (plist-get record :event) 'output)
         (eq (plist-get record :stream) 'stderr))
    'err)
   ((and (eq (plist-get record :event) 'exit)
         (not (and (eq (plist-get record :status) 'exited)
                   (= (plist-get record :code) 0))))
    'err)
   (t 'info)))

(defun elinit--log-filter-records (records &optional since until priority)
  "Filter RECORDS by timestamp range and priority.
SINCE and UNTIL are float timestamps (inclusive).
PRIORITY, when non-nil, filters to records matching that priority."
  (cl-remove-if-not
   (lambda (r)
     (let ((ts (plist-get r :ts))
           (pri (elinit--log-record-priority r)))
       (and (or (null since) (null ts) (>= ts since))
            (or (null until) (null ts) (<= ts until))
            (or (null priority) (eq pri priority)))))
   records))

(defun elinit--log-format-record-human (record)
  "Format one structured log RECORD for human display."
  (let* ((ts (plist-get record :ts))
         (unit (or (plist-get record :unit) "?"))
         (pid (plist-get record :pid))
         (stream (plist-get record :stream))
         (event (plist-get record :event))
         (payload (plist-get record :payload))
         (ts-str (if ts
                     (format-time-string "%b %d %H:%M:%S" ts)
                   "---")))
    (if (eq event 'exit)
        (format "%s %s[%d] exit: status=%s code=%d"
                ts-str unit pid
                (or (plist-get record :status) '--)
                (plist-get record :code))
      (format "%s %s[%d] %s: %s"
              ts-str unit pid stream
              (or payload "")))))

(defun elinit--log-record-to-json (record)
  "Encode structured log RECORD as a JSON object string.
Return a single-line JSON string suitable for NDJSON output."
  (require 'json)
  (json-encode
   `((ts . ,(plist-get record :ts))
     (unit . ,(plist-get record :unit))
     (pid . ,(plist-get record :pid))
     (stream . ,(symbol-name (plist-get record :stream)))
     (event . ,(symbol-name (plist-get record :event)))
     (status . ,(when (plist-get record :status)
                   (symbol-name (plist-get record :status))))
     (code . ,(plist-get record :code))
     (payload . ,(plist-get record :payload))
     (priority . ,(symbol-name
                   (elinit--log-record-priority record))))))

;;;; Log writer lifecycle

(defun elinit--logd-pid-dir ()
  "Return the directory for logd PID files.
Use `elinit-logd-pid-directory' when set, otherwise
`elinit-log-directory'."
  (or elinit-logd-pid-directory
      (elinit--effective-log-directory)
      elinit-log-directory))

(defun elinit--stream-writer-id (id stream)
  "Return PID-file ID for service ID and STREAM."
  (if (eq stream 'stderr)
      (format "%s.stderr" id)
    id))

(defun elinit--write-logd-pid-file (writer-id proc)
  "Write a PID file for log writer PROC serving WRITER-ID.
The file is named `logd-WRITER-ID.pid' in `elinit--logd-pid-dir'.
The rotation script reads these files when `--signal-reopen' is
given to send SIGHUP to all active writers."
  (let ((pid-file (expand-file-name (format "logd-%s.pid" writer-id)
                                    (elinit--logd-pid-dir))))
    (condition-case err
        (write-region (number-to-string (process-id proc))
                      nil pid-file nil 'silent)
      (error
       (elinit--log 'warning "%s: could not write PID file: %s"
                        writer-id (error-message-string err))))))

(defun elinit--remove-logd-pid-file (writer-id)
  "Remove the PID file for log writer serving WRITER-ID."
  (let ((pid-file (expand-file-name (format "logd-%s.pid" writer-id)
                                    (elinit--logd-pid-dir))))
    (when (file-exists-p pid-file)
      (delete-file pid-file))))

(defun elinit--start-stream-writer (id stream log-file table
                                          &optional log-format)
  "Start log writer for service ID STREAM writing to LOG-FILE.
TABLE receives the process keyed by ID.  STREAM is `stdout' or `stderr'.
LOG-FORMAT is the structured log format symbol (`text' or `binary')."
  (condition-case err
      (let* ((writer-id (elinit--stream-writer-id id stream))
             (log-directory-raw (or (file-name-directory log-file)
                                    (elinit--ensure-log-directory)
                                    elinit-log-directory))
             (log-directory (directory-file-name log-directory-raw))
             (fmt (or (and log-format (symbol-name log-format)) "text"))
             (cmd (append (list elinit-logd-command
                                "--file" log-file
                                "--max-file-size-bytes"
                                (number-to-string elinit-logd-max-file-size)
                                "--log-dir" log-directory
                                "--prune-cmd"
                                (elinit--build-prune-command log-format
                                                            log-directory)
                                "--prune-min-interval-sec"
                                (number-to-string
                                 elinit-logd-prune-min-interval))
                          (list "--framed"
                                "--unit" id
                                "--format" fmt)))
             (proc (make-process
                    :name (format "logd-%s" writer-id)
                    :command cmd
                    :connection-type 'pipe
                    :coding 'no-conversion
                    :noquery t)))
        (set-process-query-on-exit-flag proc nil)
        (puthash id proc table)
        (elinit--write-logd-pid-file writer-id proc)
        proc)
    (error
     (elinit--log 'warning "%s(%s): log writer failed to start: %s"
                      id stream (error-message-string err))
     nil)))

(defun elinit--start-writer (id log-file &optional log-format)
  "Start stdout log writer process for service ID writing to LOG-FILE.
LOG-FORMAT is the structured log format symbol (`text' or `binary')."
  (elinit--start-stream-writer id 'stdout log-file elinit--writers
                                   log-format))

(defun elinit--start-stderr-writer (id log-file &optional log-format)
  "Start dedicated stderr log writer process for service ID and LOG-FILE.
LOG-FORMAT is the structured log format symbol (`text' or `binary')."
  (elinit--start-stream-writer id 'stderr log-file elinit--stderr-writers
                                   log-format))

(defun elinit--start-stderr-pipe (id writer)
  "Start an internal stderr pipe process for service ID using WRITER."
  (condition-case err
      (let ((pipe
             (make-pipe-process
              :name (format "elinit-stderr-%s" id)
              :coding 'no-conversion
              :filter (lambda (_proc output)
                        (when (process-live-p writer)
                          (let ((pid (when-let* ((svc (gethash id
                                                      elinit--processes)))
                                      (process-id svc))))
                            (elinit--log-send-frame
                             writer 1 2 (or pid 0) id output))))
              :sentinel (lambda (_proc _event) nil)
              :noquery t)))
        (set-process-query-on-exit-flag pipe nil)
        (puthash id pipe elinit--stderr-pipes)
        pipe)
    (error
     (elinit--log 'warning "%s(stderr): could not create stderr pipe: %s"
                      id (error-message-string err))
     nil)))

(defun elinit--stop-stream-writer (id stream table)
  "Stop stream writer for service ID STREAM from TABLE."
  (when-let* ((writer (gethash id table)))
    (when (process-live-p writer)
      (signal-process writer 'SIGTERM))
    (elinit--remove-logd-pid-file (elinit--stream-writer-id id stream))
    (remhash id table)))

(defun elinit--stop-writer (id)
  "Stop all log writer state for service ID."
  (elinit--stop-stream-writer id 'stdout elinit--writers)
  (elinit--stop-stream-writer id 'stderr elinit--stderr-writers)
  (when-let* ((stderr-pipe (gethash id elinit--stderr-pipes)))
    (when (process-live-p stderr-pipe)
      (delete-process stderr-pipe))
    (remhash id elinit--stderr-pipes)))

(defun elinit--stop-writer-if-same (id old-stdout old-stderr old-pipe)
  "Stop writers for ID only if they have not been replaced.
OLD-STDOUT, OLD-STDERR, and OLD-PIPE are the process objects that
were active when teardown was deferred.  If any writer has been
replaced by a restart, that stream is skipped to avoid killing
the replacement writer."
  (when (and old-stdout (eq old-stdout (gethash id elinit--writers)))
    (elinit--stop-stream-writer id 'stdout elinit--writers))
  (when (and old-stderr (eq old-stderr (gethash id elinit--stderr-writers)))
    (elinit--stop-stream-writer id 'stderr elinit--stderr-writers))
  (when (and old-pipe (eq old-pipe (gethash id elinit--stderr-pipes)))
    (when (process-live-p old-pipe)
      (delete-process old-pipe))
    (remhash id elinit--stderr-pipes)))

(defun elinit--stop-all-writers ()
  "Stop all log writer processes and clear writer state hashes."
  (maphash (lambda (id writer)
             (when (process-live-p writer)
               (signal-process writer 'SIGTERM))
             (elinit--remove-logd-pid-file
              (elinit--stream-writer-id id 'stdout)))
           elinit--writers)
  (maphash (lambda (id writer)
             (when (process-live-p writer)
               (signal-process writer 'SIGTERM))
             (elinit--remove-logd-pid-file
              (elinit--stream-writer-id id 'stderr)))
           elinit--stderr-writers)
  (maphash (lambda (_id pipe)
             (when (process-live-p pipe)
               (delete-process pipe)))
           elinit--stderr-pipes)
  (clrhash elinit--writers)
  (clrhash elinit--stderr-writers)
  (clrhash elinit--stderr-pipes))

(defun elinit--build-prune-command (&optional format-hint log-dir)
  "Build the shell command string for logd's --prune-cmd flag.
Return a string suitable for passing to logd as the value of its
`--prune-cmd' argument.  The command invokes the prune script with
LOG-DIR (or the effective log directory when nil) and
`elinit-log-prune-max-total-bytes'.  FORMAT-HINT, when non-nil,
is passed as `--format-hint' for forward compatibility."
  (let ((log-directory (or log-dir
                           (elinit--effective-log-directory)
                           elinit-log-directory)))
    (concat (format "%s --log-dir %s --max-total-bytes %d"
                    (shell-quote-argument elinit-log-prune-command)
                    (shell-quote-argument log-directory)
                    elinit-log-prune-max-total-bytes)
            (when format-hint
              (format " --format-hint %s"
                      (shell-quote-argument
                       (symbol-name format-hint)))))))

(defun elinit--signal-writers-reopen ()
  "Send SIGHUP to all live log writers to trigger file reopen.
After external rotation renames the active log file, each logd
writer must reopen its file descriptor.  logd handles SIGHUP by
closing and reopening the configured log file."
  (maphash (lambda (_id writer)
             (when (process-live-p writer)
               (signal-process writer 'SIGHUP)))
           elinit--writers)
  (maphash (lambda (_id writer)
             (when (process-live-p writer)
               (signal-process writer 'SIGHUP)))
           elinit--stderr-writers))

(defun elinit-run-log-maintenance ()
  "Run log maintenance: rotate, signal writers to reopen, then prune.
Execute the scheduled maintenance path asynchronously:
1. Run `elinit-logrotate-command' to rotate active logs.
2. Signal all live writers to reopen their files.
3. Run `elinit-log-prune-command' to enforce the directory size cap."
  (interactive)
  (if-let* ((log-dir (elinit--effective-log-directory)))
      (let ((keep-days (number-to-string elinit-logrotate-keep-days))
            (max-bytes (number-to-string elinit-log-prune-max-total-bytes)))
        (elinit--log 'info "log maintenance: rotating")
        (set-process-sentinel
         (start-process "elinit-logrotate" nil
                        elinit-logrotate-command
                        "--log-dir" log-dir
                        "--keep-days" keep-days)
         (lambda (_proc event)
           (when (string-match-p "finished" event)
             (elinit--signal-writers-reopen)
             (elinit--log 'info "log maintenance: pruning")
             (start-process "elinit-log-prune" nil
                            elinit-log-prune-command
                            "--log-dir" log-dir
                            "--max-total-bytes" max-bytes)))))
    (elinit--log 'warning
                     "log maintenance skipped: no writable log directory")))

(defun elinit--builtin-logrotate-command ()
  "Build the shell command for the built-in logrotate oneshot unit."
  (let ((log-directory (or (elinit--effective-log-directory)
                           elinit-log-directory)))
    (format "%s --log-dir %s --keep-days %d --signal-reopen --pid-dir %s"
            (shell-quote-argument elinit-logrotate-command)
            (shell-quote-argument log-directory)
            elinit-logrotate-keep-days
            (shell-quote-argument (elinit--logd-pid-dir)))))

(defun elinit--builtin-log-prune-command ()
  "Build the shell command for the built-in log-prune oneshot unit."
  (let ((log-directory (or (elinit--effective-log-directory)
                           elinit-log-directory)))
    (format "%s --log-dir %s --max-total-bytes %d"
            (shell-quote-argument elinit-log-prune-command)
            (shell-quote-argument log-directory)
            elinit-log-prune-max-total-bytes)))

(provide 'elinit-log)

;;; elinit-log.el ends here
