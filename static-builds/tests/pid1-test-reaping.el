;;; pid1-test-reaping.el --- PID1 child-reaping test helper  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;; On pid1-boot-hook, spawns orphan children and checks that PID 1
;; reaps them (no zombies remain).
;;
;; Markers written:
;;   reaping-result -- "NO-ZOMBIES" or "ZOMBIES-FOUND"
;;   ready          -- inspection complete, safe to read results
;;
;; Calls (kill-emacs 0) after writing markers.

;;; Code:

(defvar pid1-test-marker-dir (getenv "PID1_TEST_MARKER_DIR")
  "Directory for writing test marker files.")

(defun pid1-test--write-marker (name &optional content)
  "Write marker file NAME with optional CONTENT to `pid1-test-marker-dir'."
  (when pid1-test-marker-dir
    (let ((file (expand-file-name name pid1-test-marker-dir)))
      (with-temp-file file
        (insert (or content name) "\n")))))

(defun pid1-test--spawn-orphans ()
  "Spawn 3 short-lived orphan children.
Each runs a shell that backgrounds a sleep and exits immediately,
orphaning the sleep process.  PID 1 should reap them."
  (dotimes (i 3)
    (let ((name (format "orphan-%d" i)))
      ;; The shell exits immediately; the backgrounded sleep becomes
      ;; an orphan reparented to PID 1.
      (start-process name nil "sh" "-c" "sleep 0.2 & exit 0"))))

(defun pid1-test--check-zombies ()
  "Return non-nil if any zombie children of PID 1 exist."
  ;; Check /proc for zombie processes whose parent is PID 1.
  ;; A zombie shows "Z" in /proc/PID/stat field 3.
  (let ((found nil))
    (dolist (entry (directory-files "/proc" nil "^[0-9]+$"))
      (let ((stat-file (format "/proc/%s/stat" entry)))
        (when (file-readable-p stat-file)
          (with-temp-buffer
            (insert-file-contents stat-file)
            (let ((line (buffer-string)))
              ;; Format: pid (comm) state ppid ...
              ;; Match state=Z and ppid=1
              (when (string-match
                     "^[0-9]+ ([^)]*) Z 1 " line)
                (setq found t)))))))
    found))

(when (boundp 'pid1-boot-hook)
  (add-hook 'pid1-boot-hook
            (lambda ()
              ;; Spawn orphans.
              (pid1-test--spawn-orphans)
              ;; Wait for them to exit and be reaped.
              (run-with-timer
               2.0 nil
               (lambda ()
                 (let ((zombies (pid1-test--check-zombies)))
                   (pid1-test--write-marker
                    "reaping-result"
                    (if zombies "ZOMBIES-FOUND" "NO-ZOMBIES"))
                   (pid1-test--write-marker "ready")
                   (kill-emacs 0)))))))

;;; pid1-test-reaping.el ends here
