;;; supervisor-test-rlimits.el --- Resource limit tests for supervisor.el -*- lexical-binding: t -*-

;; Copyright (C) 2026 telecommuter <telecommuter@riseup.net>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; ERT tests for resource limit (ulimit-style) support:
;; validation, parsing, accessor defaults, launch argv composition.

;;; Code:

(require 'supervisor-test-helpers)

;;; Validation tests

(ert-deftest supervisor-test-rlimits-valid-integer ()
  "Integer limit values pass validation."
  (should (null (supervisor--validate-entry
                 '("svc" :limit-nofile 1024)))))

(ert-deftest supervisor-test-rlimits-valid-string ()
  "String SOFT:HARD limit values pass validation."
  (should (null (supervisor--validate-entry
                 '("svc" :limit-nproc "256:512")))))

(ert-deftest supervisor-test-rlimits-valid-infinity ()
  "Symbol infinity limit values pass validation."
  (should (null (supervisor--validate-entry
                 '("svc" :limit-core infinity)))))

(ert-deftest supervisor-test-rlimits-valid-string-infinity ()
  "String infinity components pass validation."
  (should (null (supervisor--validate-entry
                 '("svc" :limit-fsize "0:infinity")))))

(ert-deftest supervisor-test-rlimits-valid-all-keys ()
  "All five limit keys together pass validation."
  (should (null (supervisor--validate-entry
                 '("svc" :limit-nofile 1024
                   :limit-nproc "256:512"
                   :limit-core 0
                   :limit-fsize infinity
                   :limit-as "1073741824:1073741824")))))

(ert-deftest supervisor-test-rlimits-reject-negative-integer ()
  "Negative integer limit values are rejected."
  (should (string-match ":limit-nofile must be non-negative"
                        (supervisor--validate-entry
                         '("svc" :limit-nofile -1)))))

(ert-deftest supervisor-test-rlimits-reject-bad-string-format ()
  "String without colon separator is rejected."
  (should (string-match "SOFT:HARD"
                        (supervisor--validate-entry
                         '("svc" :limit-nproc "1024")))))

(ert-deftest supervisor-test-rlimits-reject-non-numeric-string ()
  "Non-numeric string components are rejected."
  (should (string-match "non-negative integers"
                        (supervisor--validate-entry
                         '("svc" :limit-core "abc:def")))))

(ert-deftest supervisor-test-rlimits-reject-wrong-type ()
  "Non-integer, non-string, non-infinity values are rejected."
  (should (string-match ":limit-as must be"
                        (supervisor--validate-entry
                         '("svc" :limit-as (100 200))))))

(ert-deftest supervisor-test-rlimits-reject-float ()
  "Float limit values are rejected."
  (should (string-match ":limit-fsize must be"
                        (supervisor--validate-entry
                         '("svc" :limit-fsize 1.5)))))

(ert-deftest supervisor-test-rlimits-reject-on-target ()
  "Limit keys on target type are rejected."
  (should (string-match "invalid for :type target"
                        (supervisor--validate-entry
                         '(nil :id "foo.target" :type target
                           :limit-nofile 1024)))))

(ert-deftest supervisor-test-rlimits-reject-empty-hard ()
  "Empty hard component in SOFT:HARD string is rejected."
  (should (supervisor--validate-entry
           '("svc" :limit-nofile "100:"))))

(ert-deftest supervisor-test-rlimits-reject-empty-soft ()
  "Empty soft component in SOFT:HARD string is rejected."
  (should (supervisor--validate-entry
           '("svc" :limit-nofile ":100"))))

;;; Parse/accessor tests

(ert-deftest supervisor-test-rlimits-parse-defaults-nil ()
  "Parsed entry without limits has nil for all limit fields."
  (let ((entry (supervisor--parse-entry '("svc" :type simple))))
    (should (= (length entry) 44))
    (should (null (supervisor-entry-limit-nofile entry)))
    (should (null (supervisor-entry-limit-nproc entry)))
    (should (null (supervisor-entry-limit-core entry)))
    (should (null (supervisor-entry-limit-fsize entry)))
    (should (null (supervisor-entry-limit-as entry)))))

(ert-deftest supervisor-test-rlimits-parse-string-entry-nil ()
  "String entry has nil for all limit fields and correct length."
  (let ((entry (supervisor--parse-entry "echo hello")))
    (should (= (length entry) 44))
    (should (null (supervisor-entry-limit-nofile entry)))
    (should (null (supervisor-entry-limit-as entry)))))

(ert-deftest supervisor-test-rlimits-parse-values ()
  "Parsed entry preserves limit values through accessors."
  (let ((entry (supervisor--parse-entry
                '("svc" :limit-nofile 1024
                  :limit-nproc "256:512"
                  :limit-core infinity
                  :limit-fsize 0
                  :limit-as "100:200"))))
    (should (equal (supervisor-entry-limit-nofile entry) 1024))
    (should (equal (supervisor-entry-limit-nproc entry) "256:512"))
    (should (equal (supervisor-entry-limit-core entry) 'infinity))
    (should (equal (supervisor-entry-limit-fsize entry) 0))
    (should (equal (supervisor-entry-limit-as entry) "100:200"))))

(ert-deftest supervisor-test-rlimits-limits-requesting-p ()
  "Limits-requesting-p returns non-nil only when a limit is set."
  (let ((no-limits (supervisor--parse-entry '("svc" :type simple)))
        (with-limits (supervisor--parse-entry
                      '("svc" :limit-nofile 1024))))
    (should (null (supervisor--limits-requesting-p no-limits)))
    (should (supervisor--limits-requesting-p with-limits))))

;;; Entry-to-service conversion

(ert-deftest supervisor-test-rlimits-entry-to-service ()
  "Entry-to-service preserves limit fields in struct."
  (let* ((entry (supervisor--parse-entry
                 '("svc" :limit-nofile 1024 :limit-as "100:200")))
         (svc (supervisor-entry-to-service entry)))
    (should (equal (supervisor-service-limit-nofile svc) 1024))
    (should (equal (supervisor-service-limit-as svc) "100:200"))
    (should (null (supervisor-service-limit-nproc svc)))
    (should (null (supervisor-service-limit-core svc)))
    (should (null (supervisor-service-limit-fsize svc)))))

;;; Launch argv composition

(ert-deftest supervisor-test-rlimits-build-launch-no-limits ()
  "Build-launch-command without limits produces normal argv."
  (let ((args (supervisor--build-launch-command "sleep 300")))
    (should (equal args '("sleep" "300")))))

(ert-deftest supervisor-test-rlimits-build-launch-limits-only ()
  "Build-launch-command with limits prepends rlimits helper."
  (let ((entry (supervisor--parse-entry
                '("sleep 300" :id "svc" :limit-nofile 1024))))
    (let ((args (supervisor--build-launch-command "sleep 300" nil nil
                                                  nil entry)))
      ;; First element should be rlimits helper
      (should (equal (car args) supervisor-rlimits-command))
      ;; Should contain --nofile
      (should (member "--nofile" args))
      (should (member "1024:1024" args))
      ;; Should end with -- sleep 300
      (let ((sep-pos (cl-position "--" args :test #'equal :from-end t)))
        (should sep-pos)
        (should (equal (nth (+ sep-pos 1) args) "sleep"))
        (should (equal (nth (+ sep-pos 2) args) "300"))))))

(ert-deftest supervisor-test-rlimits-build-launch-limits-runas ()
  "Build-launch-command with limits + identity has correct order."
  (let ((entry (supervisor--parse-entry
                '("sleep 300" :id "svc" :limit-nproc "128:256"))))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_name) "/usr/bin/sleep")))
      (let ((args (supervisor--build-launch-command "sleep 300" "alice" nil
                                                    nil entry)))
        ;; Outermost: rlimits helper
        (should (equal (car args) supervisor-rlimits-command))
        ;; Should contain --nproc
        (should (member "--nproc" args))
        (should (member "128:256" args))
        ;; After rlimits --, runas should appear
        (let* ((first-sep (cl-position "--" args :test #'equal))
               (after-sep (nth (1+ first-sep) args)))
          (should (equal after-sep supervisor-runas-command)))))))

(ert-deftest supervisor-test-rlimits-build-launch-limits-sandbox ()
  "Build-launch-command with limits + sandbox has correct order."
  (let ((entry (supervisor--parse-entry
                '("sleep 300" :id "svc" :sandbox-profile service
                  :limit-core 0))))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_name) "/usr/bin/bwrap")))
      (let ((args (supervisor--build-launch-command "sleep 300" nil nil
                                                    entry entry)))
        ;; Outermost: rlimits helper
        (should (equal (car args) supervisor-rlimits-command))
        ;; Should contain --core
        (should (member "--core" args))
        ;; After rlimits --, bwrap should appear
        (let* ((first-sep (cl-position "--" args :test #'equal))
               (after-sep (nth (1+ first-sep) args)))
          (should (equal after-sep "/usr/bin/bwrap")))))))

(ert-deftest supervisor-test-rlimits-build-launch-limits-runas-sandbox ()
  "Build-launch-command with limits + runas + sandbox has correct order."
  (let ((entry (supervisor--parse-entry
                '("sleep 300" :id "svc" :sandbox-profile service
                  :limit-fsize infinity))))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_name) "/usr/bin/bwrap")))
      (let ((args (supervisor--build-launch-command "sleep 300" "alice" nil
                                                    entry entry)))
        ;; Outermost: rlimits
        (should (equal (car args) supervisor-rlimits-command))
        ;; Should contain --fsize infinity:infinity
        (should (member "--fsize" args))
        (should (member "infinity:infinity" args))
        ;; After rlimits --, runas should appear
        (let* ((first-sep (cl-position "--" args :test #'equal))
               (after-sep (nth (1+ first-sep) args)))
          (should (equal after-sep supervisor-runas-command)))
        ;; Runas -- should be followed by bwrap
        (let* ((seps (cl-loop for i from 0 below (length args)
                              when (equal (nth i args) "--")
                              collect i))
               (second-sep (cadr seps))
               (after-second (nth (1+ second-sep) args)))
          (should (equal after-second "/usr/bin/bwrap")))))))

(ert-deftest supervisor-test-rlimits-format-limit-value ()
  "Format limit value helper produces correct strings."
  (should (equal (supervisor--format-limit-value 1024) "1024:1024"))
  (should (equal (supervisor--format-limit-value 'infinity) "infinity:infinity"))
  (should (equal (supervisor--format-limit-value "100:200") "100:200")))

(ert-deftest supervisor-test-rlimits-build-launch-multiple-limits ()
  "Build-launch-command with multiple limits includes all."
  (let ((entry (supervisor--parse-entry
                '("sleep 300" :id "svc"
                  :limit-nofile 1024
                  :limit-nproc "256:512"
                  :limit-core 0))))
    (let ((args (supervisor--build-launch-command "sleep 300" nil nil
                                                  nil entry)))
      (should (equal (car args) supervisor-rlimits-command))
      (should (member "--nofile" args))
      (should (member "1024:1024" args))
      (should (member "--nproc" args))
      (should (member "256:512" args))
      (should (member "--core" args))
      (should (member "0:0" args)))))

(ert-deftest supervisor-test-rlimits-validate-limit-value-helper ()
  "Validate-limit-value helper accepts and rejects correctly."
  ;; Valid
  (should (null (supervisor--validate-limit-value :limit-nofile 0)))
  (should (null (supervisor--validate-limit-value :limit-nofile 1024)))
  (should (null (supervisor--validate-limit-value :limit-nofile 'infinity)))
  (should (null (supervisor--validate-limit-value :limit-nofile "100:200")))
  (should (null (supervisor--validate-limit-value :limit-nofile "infinity:infinity")))
  (should (null (supervisor--validate-limit-value :limit-nofile "0:infinity")))
  ;; Invalid
  (should (supervisor--validate-limit-value :limit-nofile -1))
  (should (supervisor--validate-limit-value :limit-nofile "bad"))
  (should (supervisor--validate-limit-value :limit-nofile "100:abc"))
  (should (supervisor--validate-limit-value :limit-nofile ":100"))
  (should (supervisor--validate-limit-value :limit-nofile "100:"))
  (should (supervisor--validate-limit-value :limit-nofile 1.5))
  (should (supervisor--validate-limit-value :limit-nofile '(1 2))))

(provide 'supervisor-test-rlimits)

;;; supervisor-test-rlimits.el ends here
