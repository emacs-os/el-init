;;; elinit-test-rlimits.el --- Resource limit tests for elinit.el -*- lexical-binding: t -*-

;; Copyright (C) 2026 telecommuter <telecommuter@riseup.net>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; ERT tests for resource limit (ulimit-style) support:
;; validation, parsing, accessor defaults, launch argv composition.

;;; Code:

(require 'elinit-test-helpers)

;;; Validation tests

(ert-deftest elinit-test-rlimits-valid-integer ()
  "Integer limit values pass validation."
  (should (null (elinit--validate-entry
                 '("svc" :limit-nofile 1024)))))

(ert-deftest elinit-test-rlimits-valid-string ()
  "String SOFT:HARD limit values pass validation."
  (should (null (elinit--validate-entry
                 '("svc" :limit-nproc "256:512")))))

(ert-deftest elinit-test-rlimits-valid-infinity ()
  "Symbol infinity limit values pass validation."
  (should (null (elinit--validate-entry
                 '("svc" :limit-core infinity)))))

(ert-deftest elinit-test-rlimits-valid-string-infinity ()
  "String infinity components pass validation."
  (should (null (elinit--validate-entry
                 '("svc" :limit-fsize "0:infinity")))))

(ert-deftest elinit-test-rlimits-valid-all-keys ()
  "All five limit keys together pass validation."
  (should (null (elinit--validate-entry
                 '("svc" :limit-nofile 1024
                   :limit-nproc "256:512"
                   :limit-core 0
                   :limit-fsize infinity
                   :limit-as "1073741824:1073741824")))))

(ert-deftest elinit-test-rlimits-reject-negative-integer ()
  "Negative integer limit values are rejected."
  (should (string-match ":limit-nofile must be non-negative"
                        (elinit--validate-entry
                         '("svc" :limit-nofile -1)))))

(ert-deftest elinit-test-rlimits-reject-bad-string-format ()
  "String without colon separator is rejected."
  (should (string-match "SOFT:HARD"
                        (elinit--validate-entry
                         '("svc" :limit-nproc "1024")))))

(ert-deftest elinit-test-rlimits-reject-non-numeric-string ()
  "Non-numeric string components are rejected."
  (should (string-match "non-negative integers"
                        (elinit--validate-entry
                         '("svc" :limit-core "abc:def")))))

(ert-deftest elinit-test-rlimits-reject-wrong-type ()
  "Non-integer, non-string, non-infinity values are rejected."
  (should (string-match ":limit-as must be"
                        (elinit--validate-entry
                         '("svc" :limit-as (100 200))))))

(ert-deftest elinit-test-rlimits-reject-float ()
  "Float limit values are rejected."
  (should (string-match ":limit-fsize must be"
                        (elinit--validate-entry
                         '("svc" :limit-fsize 1.5)))))

(ert-deftest elinit-test-rlimits-reject-on-target ()
  "Limit keys on target type are rejected."
  (should (string-match "invalid for :type target"
                        (elinit--validate-entry
                         '(nil :id "foo.target" :type target
                           :limit-nofile 1024)))))

(ert-deftest elinit-test-rlimits-reject-empty-hard ()
  "Empty hard component in SOFT:HARD string is rejected."
  (should (elinit--validate-entry
           '("svc" :limit-nofile "100:"))))

(ert-deftest elinit-test-rlimits-reject-empty-soft ()
  "Empty soft component in SOFT:HARD string is rejected."
  (should (elinit--validate-entry
           '("svc" :limit-nofile ":100"))))

;;; Parse/accessor tests

(ert-deftest elinit-test-rlimits-parse-defaults-nil ()
  "Parsed entry without limits has nil for all limit fields."
  (let ((entry (elinit--parse-entry '("svc" :type simple))))
    (should (= (length entry) 45))
    (should (null (elinit-entry-limit-nofile entry)))
    (should (null (elinit-entry-limit-nproc entry)))
    (should (null (elinit-entry-limit-core entry)))
    (should (null (elinit-entry-limit-fsize entry)))
    (should (null (elinit-entry-limit-as entry)))))

(ert-deftest elinit-test-rlimits-parse-string-entry-nil ()
  "String entry has nil for all limit fields and correct length."
  (let ((entry (elinit--parse-entry "echo hello")))
    (should (= (length entry) 45))
    (should (null (elinit-entry-limit-nofile entry)))
    (should (null (elinit-entry-limit-as entry)))))

(ert-deftest elinit-test-rlimits-parse-values ()
  "Parsed entry preserves limit values through accessors."
  (let ((entry (elinit--parse-entry
                '("svc" :limit-nofile 1024
                  :limit-nproc "256:512"
                  :limit-core infinity
                  :limit-fsize 0
                  :limit-as "100:200"))))
    (should (equal (elinit-entry-limit-nofile entry) 1024))
    (should (equal (elinit-entry-limit-nproc entry) "256:512"))
    (should (equal (elinit-entry-limit-core entry) 'infinity))
    (should (equal (elinit-entry-limit-fsize entry) 0))
    (should (equal (elinit-entry-limit-as entry) "100:200"))))

(ert-deftest elinit-test-rlimits-limits-requesting-p ()
  "Limits-requesting-p returns non-nil only when a limit is set."
  (let ((no-limits (elinit--parse-entry '("svc" :type simple)))
        (with-limits (elinit--parse-entry
                      '("svc" :limit-nofile 1024))))
    (should (null (elinit--limits-requesting-p no-limits)))
    (should (elinit--limits-requesting-p with-limits))))

;;; Entry-to-service conversion

(ert-deftest elinit-test-rlimits-entry-to-service ()
  "Entry-to-service preserves limit fields in struct."
  (let* ((entry (elinit--parse-entry
                 '("svc" :limit-nofile 1024 :limit-as "100:200")))
         (svc (elinit-entry-to-service entry)))
    (should (equal (elinit-service-limit-nofile svc) 1024))
    (should (equal (elinit-service-limit-as svc) "100:200"))
    (should (null (elinit-service-limit-nproc svc)))
    (should (null (elinit-service-limit-core svc)))
    (should (null (elinit-service-limit-fsize svc)))))

;;; Launch argv composition

(ert-deftest elinit-test-rlimits-build-launch-no-limits ()
  "Build-launch-command without limits produces normal argv."
  (let ((args (elinit--build-launch-command "sleep 300")))
    (should (equal args '("sleep" "300")))))

(ert-deftest elinit-test-rlimits-build-launch-limits-only ()
  "Build-launch-command with limits prepends rlimits helper."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc" :limit-nofile 1024))))
    (let ((args (elinit--build-launch-command "sleep 300" nil nil
                                                  nil entry)))
      ;; First element should be rlimits helper
      (should (equal (car args) elinit-rlimits-command))
      ;; Should contain --nofile
      (should (member "--nofile" args))
      (should (member "1024:1024" args))
      ;; Should end with -- sleep 300
      (let ((sep-pos (cl-position "--" args :test #'equal :from-end t)))
        (should sep-pos)
        (should (equal (nth (+ sep-pos 1) args) "sleep"))
        (should (equal (nth (+ sep-pos 2) args) "300"))))))

(ert-deftest elinit-test-rlimits-build-launch-limits-runas ()
  "Build-launch-command with limits + identity has correct order."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc" :limit-nproc "128:256"))))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_name) "/usr/bin/sleep")))
      (let ((args (elinit--build-launch-command "sleep 300" "alice" nil
                                                    nil entry)))
        ;; Outermost: rlimits helper
        (should (equal (car args) elinit-rlimits-command))
        ;; Should contain --nproc
        (should (member "--nproc" args))
        (should (member "128:256" args))
        ;; After rlimits --, runas should appear
        (let* ((first-sep (cl-position "--" args :test #'equal))
               (after-sep (nth (1+ first-sep) args)))
          (should (equal after-sep elinit-runas-command)))))))

(ert-deftest elinit-test-rlimits-build-launch-limits-sandbox ()
  "Build-launch-command with limits + sandbox has correct order."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc" :sandbox-profile service
                  :limit-core 0))))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_name) "/usr/bin/bwrap")))
      (let ((args (elinit--build-launch-command "sleep 300" nil nil
                                                    entry entry)))
        ;; Outermost: rlimits helper
        (should (equal (car args) elinit-rlimits-command))
        ;; Should contain --core
        (should (member "--core" args))
        ;; After rlimits --, bwrap should appear
        (let* ((first-sep (cl-position "--" args :test #'equal))
               (after-sep (nth (1+ first-sep) args)))
          (should (equal after-sep "/usr/bin/bwrap")))))))

(ert-deftest elinit-test-rlimits-build-launch-limits-runas-sandbox ()
  "Build-launch-command with limits + runas + sandbox has correct order."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc" :sandbox-profile service
                  :limit-fsize infinity))))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_name) "/usr/bin/bwrap")))
      (let ((args (elinit--build-launch-command "sleep 300" "alice" nil
                                                    entry entry)))
        ;; Outermost: rlimits
        (should (equal (car args) elinit-rlimits-command))
        ;; Should contain --fsize infinity:infinity
        (should (member "--fsize" args))
        (should (member "infinity:infinity" args))
        ;; After rlimits --, runas should appear
        (let* ((first-sep (cl-position "--" args :test #'equal))
               (after-sep (nth (1+ first-sep) args)))
          (should (equal after-sep elinit-runas-command)))
        ;; Runas -- should be followed by bwrap
        (let* ((seps (cl-loop for i from 0 below (length args)
                              when (equal (nth i args) "--")
                              collect i))
               (second-sep (cadr seps))
               (after-second (nth (1+ second-sep) args)))
          (should (equal after-second "/usr/bin/bwrap")))))))

(ert-deftest elinit-test-rlimits-format-limit-value ()
  "Format limit value helper produces correct strings."
  (should (equal (elinit--format-limit-value 1024) "1024:1024"))
  (should (equal (elinit--format-limit-value 'infinity) "infinity:infinity"))
  (should (equal (elinit--format-limit-value "100:200") "100:200")))

(ert-deftest elinit-test-rlimits-build-launch-multiple-limits ()
  "Build-launch-command with multiple limits includes all."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc"
                  :limit-nofile 1024
                  :limit-nproc "256:512"
                  :limit-core 0))))
    (let ((args (elinit--build-launch-command "sleep 300" nil nil
                                                  nil entry)))
      (should (equal (car args) elinit-rlimits-command))
      (should (member "--nofile" args))
      (should (member "1024:1024" args))
      (should (member "--nproc" args))
      (should (member "256:512" args))
      (should (member "--core" args))
      (should (member "0:0" args)))))

(ert-deftest elinit-test-rlimits-validate-limit-value-helper ()
  "Validate-limit-value helper accepts and rejects correctly."
  ;; Valid
  (should (null (elinit--validate-limit-value :limit-nofile 0)))
  (should (null (elinit--validate-limit-value :limit-nofile 1024)))
  (should (null (elinit--validate-limit-value :limit-nofile 'infinity)))
  (should (null (elinit--validate-limit-value :limit-nofile "100:200")))
  (should (null (elinit--validate-limit-value :limit-nofile "infinity:infinity")))
  (should (null (elinit--validate-limit-value :limit-nofile "0:infinity")))
  ;; Valid: soft equals hard
  (should (null (elinit--validate-limit-value :limit-nofile "200:200")))
  ;; Valid: soft < hard
  (should (null (elinit--validate-limit-value :limit-nofile "1:999")))
  ;; Valid: hard is infinity, soft is finite
  (should (null (elinit--validate-limit-value :limit-nofile "1024:infinity")))
  ;; Invalid
  (should (elinit--validate-limit-value :limit-nofile -1))
  (should (elinit--validate-limit-value :limit-nofile "bad"))
  (should (elinit--validate-limit-value :limit-nofile "100:abc"))
  (should (elinit--validate-limit-value :limit-nofile ":100"))
  (should (elinit--validate-limit-value :limit-nofile "100:"))
  (should (elinit--validate-limit-value :limit-nofile 1.5))
  (should (elinit--validate-limit-value :limit-nofile '(1 2)))
  ;; Invalid: soft > hard (kernel rejects this)
  (should (elinit--validate-limit-value :limit-nofile "9999:1"))
  (should (elinit--validate-limit-value :limit-nofile "200:100"))
  ;; Invalid: infinity soft with finite hard
  (should (elinit--validate-limit-value :limit-nofile "infinity:100"))
  ;; Invalid: value too large for C helper (>= 2^64-1)
  (should (elinit--validate-limit-value
           :limit-nofile (expt 2 64)))
  (should (elinit--validate-limit-value
           :limit-nofile "999999999999999999999999999999:1")))

;;; Integration tests (plan-level invalid plumbing)

(ert-deftest elinit-test-rlimits-plan-invalid-negative ()
  "Build-plan marks entry with negative limit as invalid."
  (let* ((elinit--authority-snapshot nil)
         (programs '(("sleep 300" :id "bad-svc" :limit-nofile -1)
                     ("true" :id "good-svc")))
         (plan (elinit--build-plan programs)))
    (should (gethash "bad-svc" (elinit-plan-invalid plan)))
    (should (string-match "non-negative"
                          (gethash "bad-svc" (elinit-plan-invalid plan))))
    (should-not (gethash "good-svc" (elinit-plan-invalid plan)))))

(ert-deftest elinit-test-rlimits-plan-invalid-bad-string ()
  "Build-plan marks entry with malformed limit string as invalid."
  (let* ((elinit--authority-snapshot nil)
         (programs '(("sleep 300" :id "bad-svc" :limit-core "notvalid")))
         (plan (elinit--build-plan programs)))
    (should (gethash "bad-svc" (elinit-plan-invalid plan)))
    (should (string-match "SOFT:HARD"
                          (gethash "bad-svc" (elinit-plan-invalid plan))))))

(ert-deftest elinit-test-rlimits-plan-valid-passes ()
  "Build-plan does not mark entry with valid limits as invalid."
  (let* ((elinit--authority-snapshot nil)
         (programs '(("sleep 300" :id "svc"
                      :limit-nofile 1024 :limit-core infinity)))
         (plan (elinit--build-plan programs)))
    (should-not (gethash "svc" (elinit-plan-invalid plan)))
    ;; Entry should appear in valid entries list
    (should (cl-find "svc" (elinit-plan-entries plan)
                     :key #'elinit-entry-id :test #'equal))))

(ert-deftest elinit-test-rlimits-plan-invalid-soft-exceeds-hard ()
  "Build-plan marks entry with soft > hard limit as invalid."
  (let* ((elinit--authority-snapshot nil)
         (programs '(("sleep 300" :id "bad-svc" :limit-nofile "9999:1")))
         (plan (elinit--build-plan programs)))
    (should (gethash "bad-svc" (elinit-plan-invalid plan)))
    (should (string-match "soft limit.*exceeds hard"
                          (gethash "bad-svc" (elinit-plan-invalid plan))))))

(ert-deftest elinit-test-rlimits-plan-invalid-float ()
  "Build-plan marks entry with float limit as invalid."
  (let* ((elinit--authority-snapshot nil)
         (programs '(("sleep 300" :id "bad-svc" :limit-fsize 1.5)))
         (plan (elinit--build-plan programs)))
    (should (gethash "bad-svc" (elinit-plan-invalid plan)))))

(ert-deftest elinit-test-rlimits-plan-invalid-target-limit ()
  "Build-plan marks target with limit keys as invalid."
  (let* ((elinit--authority-snapshot nil)
         (programs '((nil :id "app.target" :type target :limit-nofile 1024)))
         (plan (elinit--build-plan programs)))
    (should (gethash "app.target" (elinit-plan-invalid plan)))
    (should (string-match "invalid for :type target"
                          (gethash "app.target"
                                   (elinit-plan-invalid plan))))))

;;; Libexec build-target registration

(ert-deftest elinit-test-rlimits-in-libexec-build-targets ()
  "Elinit-rlimits appears in libexec build targets."
  (let ((names (mapcar (lambda (target) (plist-get target :name))
                       (elinit--libexec-build-targets))))
    (should (member "elinit-rlimits" names))))

;;; Phase 3 introspection surface tests

(ert-deftest elinit-test-rlimits-cli-entry-info-includes-limits ()
  "CLI entry-info alist includes limit fields from parsed entry."
  (let* ((entry (elinit--parse-entry
                 '("sleep 300" :id "svc" :limit-nofile 1024
                   :limit-core infinity :limit-as "100:200")))
         (elinit--processes (make-hash-table :test 'equal))
         (elinit--failed (make-hash-table :test 'equal))
         (elinit--restart-override (make-hash-table :test 'equal))
         (elinit--enabled-override (make-hash-table :test 'equal))
         (elinit--mask-override (make-hash-table :test 'equal))
         (elinit--logging (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal))
         (elinit--invalid (make-hash-table :test 'equal))
         (elinit--manually-stopped (make-hash-table :test 'equal))
         (elinit--manually-started (make-hash-table :test 'equal))
         (elinit--oneshot-completed (make-hash-table :test 'equal))
         (elinit--remain-active (make-hash-table :test 'equal))
         (elinit--start-times (make-hash-table :test 'equal))
         (elinit--ready-times (make-hash-table :test 'equal))
         (elinit--restart-times (make-hash-table :test 'equal))
         (elinit--logging-override (make-hash-table :test 'equal))
         (info (elinit--cli-entry-info entry)))
    (should (equal 1024 (alist-get 'limit-nofile info)))
    (should (eq 'infinity (alist-get 'limit-core info)))
    (should (equal "100:200" (alist-get 'limit-as info)))
    (should (null (alist-get 'limit-nproc info)))
    (should (null (alist-get 'limit-fsize info)))))

(ert-deftest elinit-test-rlimits-cli-entry-info-nil-when-unset ()
  "CLI entry-info alist has nil limit fields when no limits are set."
  (let* ((entry (elinit--parse-entry '("sleep 300" :id "svc")))
         (elinit--processes (make-hash-table :test 'equal))
         (elinit--failed (make-hash-table :test 'equal))
         (elinit--restart-override (make-hash-table :test 'equal))
         (elinit--enabled-override (make-hash-table :test 'equal))
         (elinit--mask-override (make-hash-table :test 'equal))
         (elinit--logging (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal))
         (elinit--invalid (make-hash-table :test 'equal))
         (elinit--manually-stopped (make-hash-table :test 'equal))
         (elinit--manually-started (make-hash-table :test 'equal))
         (elinit--oneshot-completed (make-hash-table :test 'equal))
         (elinit--remain-active (make-hash-table :test 'equal))
         (elinit--start-times (make-hash-table :test 'equal))
         (elinit--ready-times (make-hash-table :test 'equal))
         (elinit--restart-times (make-hash-table :test 'equal))
         (elinit--logging-override (make-hash-table :test 'equal))
         (info (elinit--cli-entry-info entry)))
    (should (null (alist-get 'limit-nofile info)))
    (should (null (alist-get 'limit-nproc info)))
    (should (null (alist-get 'limit-core info)))
    (should (null (alist-get 'limit-fsize info)))
    (should (null (alist-get 'limit-as info)))))

(ert-deftest elinit-test-rlimits-cli-describe-human-shows-limits ()
  "CLI describe-human output contains Limits line with set fields."
  (let ((info '((id . "svc") (type . simple) (status . stopped)
                (enabled . t) (restart . always) (logging . t)
                (limit-nofile . 1024) (limit-nproc . "256:512")
                (limit-core . nil) (limit-fsize . nil)
                (limit-as . infinity))))
    (let ((output (elinit--cli-describe-human info)))
      (should (string-match-p "Limits:" output))
      (should (string-match-p "nofile=1024" output))
      (should (string-match-p "nproc=256:512" output))
      (should (string-match-p "as=infinity" output))
      ;; nil fields should not appear
      (should-not (string-match-p "core=" output))
      (should-not (string-match-p "fsize=" output)))))

(ert-deftest elinit-test-rlimits-cli-describe-human-no-limits-line ()
  "CLI describe-human output omits Limits line when no limits set."
  (let ((info '((id . "svc") (type . simple) (status . stopped)
                (enabled . t) (restart . always) (logging . t)
                (limit-nofile . nil) (limit-nproc . nil)
                (limit-core . nil) (limit-fsize . nil)
                (limit-as . nil))))
    (let ((output (elinit--cli-describe-human info)))
      (should-not (string-match-p "Limits:" output)))))

(ert-deftest elinit-test-rlimits-cli-json-includes-limit-fields ()
  "CLI JSON object includes limit fields."
  (let ((info '((id . "svc") (type . simple) (status . stopped)
                (enabled . t) (restart . always) (logging . t)
                (limit-nofile . 1024) (limit-nproc . nil)
                (limit-core . 0) (limit-fsize . nil)
                (limit-as . "100:200"))))
    (let ((json-obj (elinit--cli-entry-to-json-obj info)))
      (should (equal 1024 (alist-get 'limit_nofile json-obj)))
      (should (null (alist-get 'limit_nproc json-obj)))
      (should (equal 0 (alist-get 'limit_core json-obj)))
      (should (null (alist-get 'limit_fsize json-obj)))
      (should (equal "100:200" (alist-get 'limit_as json-obj))))))

(ert-deftest elinit-test-rlimits-dashboard-detail-shows-limits ()
  "Dashboard detail view includes Resource limits line."
  (let* ((entry (elinit--parse-entry
                 '("cmd" :id "svc" :limit-nofile 4096 :limit-core 0)))
         (elinit--processes (make-hash-table :test 'equal))
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
         (elinit--logging-override (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'elinit--unit-file-path)
               (lambda (_id) nil))
              ((symbol-function 'elinit--telemetry-log-tail)
               (lambda (_id &optional _lines) nil))
              ((symbol-function 'elinit--telemetry-process-tree)
               (lambda (_pid) nil))
              ((symbol-function 'elinit--telemetry-process-metrics)
               (lambda (_pid) nil)))
      (elinit--describe-entry-detail "svc" entry)
      (let ((info-buf (get-buffer "*elinit-info*")))
        (unwind-protect
            (let ((output (with-current-buffer info-buf (buffer-string))))
              (should (string-match-p "Limits:" output))
              (should (string-match-p "nofile=4096" output))
              (should (string-match-p "core=0" output))
              ;; Unset limits should not appear
              (should-not (string-match-p "nproc=" output)))
          (when info-buf (kill-buffer info-buf)))))))

(ert-deftest elinit-test-rlimits-dashboard-detail-no-limits-line ()
  "Dashboard detail view omits Limits line when no limits set."
  (let* ((entry (elinit--parse-entry '("cmd" :id "svc")))
         (elinit--processes (make-hash-table :test 'equal))
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
         (elinit--logging-override (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'elinit--unit-file-path)
               (lambda (_id) nil))
              ((symbol-function 'elinit--telemetry-log-tail)
               (lambda (_id &optional _lines) nil))
              ((symbol-function 'elinit--telemetry-process-tree)
               (lambda (_pid) nil))
              ((symbol-function 'elinit--telemetry-process-metrics)
               (lambda (_pid) nil)))
      (elinit--describe-entry-detail "svc" entry)
      (let ((info-buf (get-buffer "*elinit-info*")))
        (unwind-protect
            (let ((output (with-current-buffer info-buf (buffer-string))))
              (should-not (string-match-p "Limits:" output)))
          (when info-buf (kill-buffer info-buf)))))))

(provide 'elinit-test-rlimits)

;;; elinit-test-rlimits.el ends here
