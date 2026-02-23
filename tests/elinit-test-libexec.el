;;; elinit-test-libexec.el --- Libexec build and compilation tests for elinit.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for the elinit-libexec module: build targets, compiler
;; selection, and build invocation.

;;; Code:

(require 'elinit-test-helpers)

;;; Libexec Build and Compiler Tests

(ert-deftest elinit-test-libexec-pending-build-targets-missing-binary ()
  "Pending helper detection includes targets missing compiled binaries."
  (let* ((tmp (make-temp-file "sv-libexec-" t))
         (logd-bin (expand-file-name "elinit-logd" tmp))
         (runas-bin (expand-file-name "elinit-runas" tmp))
         (rlimits-bin (expand-file-name "elinit-rlimits" tmp))
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
              (should (equal "elinit-logd"
                             (plist-get (car pending) :name))))))
      (delete-directory tmp t))))

(ert-deftest elinit-test-libexec-pending-build-targets-missing-source ()
  "Pending helper detection includes missing binaries without sources."
  (let* ((tmp (make-temp-file "sv-libexec-" t))
         (logd-bin (expand-file-name "elinit-logd" tmp))
         (runas-bin (expand-file-name "elinit-runas" tmp))
         (rlimits-bin (expand-file-name "elinit-rlimits" tmp)))
    (unwind-protect
        (let ((elinit-logd-command logd-bin)
              (elinit-runas-command runas-bin)
              (elinit-rlimits-command rlimits-bin))
          (let ((pending (elinit--libexec-pending-build-targets)))
            (should (= 3 (length pending)))
            (should (equal '("elinit-logd" "elinit-runas"
                             "elinit-rlimits")
                           (mapcar (lambda (target)
                                     (plist-get target :name))
                                   pending)))))
      (delete-directory tmp t))))

(ert-deftest elinit-test-build-libexec-helpers-invokes-compiler ()
  "Helper build path invokes the compiler for each pending source."
  (let* ((tmp (make-temp-file "sv-libexec-" t))
         (logd-bin (expand-file-name "elinit-logd" tmp))
         (runas-bin (expand-file-name "elinit-runas" tmp))
         (rlimits-bin (expand-file-name "elinit-rlimits" tmp))
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
         (logd-bin (expand-file-name "elinit-logd" tmp))
         (runas-bin (expand-file-name "elinit-runas" tmp))
         (rlimits-bin (expand-file-name "elinit-rlimits" tmp))
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
                 (list (list :name "elinit-logd"))))
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
                 (list (list :name "elinit-logd"))))
              ((symbol-function 'elinit-build-libexec-helpers)
               (lambda ()
                 (setq built t)
                 (list :built 1 :attempted 1 :failed nil :missing-source nil)))
              ((symbol-function 'elinit--log-libexec-build-result) #'ignore))
      (elinit--maybe-build-libexec-helpers)
      (should built))))

(provide 'elinit-test-libexec)
;;; elinit-test-libexec.el ends here
