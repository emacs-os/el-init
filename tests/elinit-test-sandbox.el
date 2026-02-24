;;; elinit-test-sandbox.el --- Bubblewrap sandbox integration tests for elinit.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Bubblewrap sandbox integration ERT tests for elinit.el.

;;; Code:

(require 'elinit-test-helpers)

;;;; Sandbox (bubblewrap) Integration Tests

(ert-deftest elinit-test-sandbox-parse-profile ()
  "Parse entry extracts sandbox-profile as symbol."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc" :sandbox-profile strict))))
    (should (= (length entry) 45))
    (should (eq (elinit-entry-sandbox-profile entry) 'strict))))

(ert-deftest elinit-test-sandbox-parse-profile-string ()
  "Parse entry converts string sandbox-profile to symbol."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc" :sandbox-profile "service"))))
    (should (eq (elinit-entry-sandbox-profile entry) 'service))))

(ert-deftest elinit-test-sandbox-parse-network ()
  "Parse entry extracts sandbox-network as symbol."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc" :sandbox-network isolated))))
    (should (eq (elinit-entry-sandbox-network entry) 'isolated))))

(ert-deftest elinit-test-sandbox-parse-ro-bind ()
  "Parse entry extracts and deduplicates sandbox-ro-bind."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc"
                  :sandbox-ro-bind ("/opt" "/usr" "/opt")))))
    (should (equal (elinit-entry-sandbox-ro-bind entry)
                   '("/opt" "/usr")))))

(ert-deftest elinit-test-sandbox-parse-rw-bind ()
  "Parse entry extracts sandbox-rw-bind."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc"
                  :sandbox-rw-bind ("/var/lib/app")))))
    (should (equal (elinit-entry-sandbox-rw-bind entry)
                   '("/var/lib/app")))))

(ert-deftest elinit-test-sandbox-parse-tmpfs ()
  "Parse entry extracts sandbox-tmpfs."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc" :sandbox-tmpfs ("/tmp")))))
    (should (equal (elinit-entry-sandbox-tmpfs entry)
                   '("/tmp")))))

(ert-deftest elinit-test-sandbox-parse-raw-args ()
  "Parse entry extracts sandbox-raw-args."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc"
                  :sandbox-raw-args ("--cap-add" "CAP_NET_RAW")))))
    (should (equal (elinit-entry-sandbox-raw-args entry)
                   '("--cap-add" "CAP_NET_RAW")))))

(ert-deftest elinit-test-sandbox-parse-none-defaults ()
  "Parse entry defaults sandbox fields to nil."
  (let ((entry (elinit--parse-entry '("sleep 300" :id "svc"))))
    (should-not (elinit-entry-sandbox-profile entry))
    (should-not (elinit-entry-sandbox-network entry))
    (should-not (elinit-entry-sandbox-ro-bind entry))
    (should-not (elinit-entry-sandbox-rw-bind entry))
    (should-not (elinit-entry-sandbox-tmpfs entry))
    (should-not (elinit-entry-sandbox-raw-args entry))))

(ert-deftest elinit-test-sandbox-requesting-p-profile ()
  "Sandbox-requesting-p returns t when profile is not none."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc" :sandbox-profile strict))))
    (should (elinit--sandbox-requesting-p entry))))

(ert-deftest elinit-test-sandbox-requesting-p-none ()
  "Sandbox-requesting-p returns nil for profile none with no knobs."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc" :sandbox-profile none))))
    (should-not (elinit--sandbox-requesting-p entry))))

(ert-deftest elinit-test-sandbox-requesting-p-knob-only ()
  "Sandbox-requesting-p returns t when knobs set without profile."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc"
                  :sandbox-network isolated))))
    (should (elinit--sandbox-requesting-p entry))))

(ert-deftest elinit-test-sandbox-requesting-p-no-sandbox ()
  "Sandbox-requesting-p returns nil for non-sandbox entry."
  (let ((entry (elinit--parse-entry '("sleep 300" :id "svc"))))
    (should-not (elinit--sandbox-requesting-p entry))))

;; Validation tests

(ert-deftest elinit-test-sandbox-validate-profile-valid ()
  "Valid sandbox profiles pass validation."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (name) (when (equal name "bwrap") "/usr/bin/bwrap"))))
    (dolist (profile '(none strict service desktop))
      (should-not (elinit--validate-entry
                   `("cmd" :id "svc" :sandbox-profile ,profile))))))

(ert-deftest elinit-test-sandbox-validate-profile-invalid ()
  "Invalid sandbox profile is rejected."
  (let ((reason (elinit--validate-entry
                 '("cmd" :id "svc" :sandbox-profile bogus))))
    (should (string-match-p ":sandbox-profile must be one of" reason))))

(ert-deftest elinit-test-sandbox-validate-network-valid ()
  "Valid network modes pass validation."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (name) (when (equal name "bwrap") "/usr/bin/bwrap"))))
    (dolist (mode '(shared isolated))
      (should-not (elinit--validate-entry
                   `("cmd" :id "svc" :sandbox-network ,mode))))))

(ert-deftest elinit-test-sandbox-validate-network-invalid ()
  "Invalid network mode is rejected."
  (let ((reason (elinit--validate-entry
                 '("cmd" :id "svc" :sandbox-network bogus))))
    (should (string-match-p ":sandbox-network must be one of" reason))))

(ert-deftest elinit-test-sandbox-validate-path-absolute ()
  "Relative paths in bind lists are rejected."
  (let ((reason (elinit--validate-entry
                 '("cmd" :id "svc" :sandbox-ro-bind ("relative/path")))))
    (should (string-match-p "paths must be absolute" reason))))

(ert-deftest elinit-test-sandbox-validate-path-empty ()
  "Empty paths in bind lists are rejected."
  (let ((reason (elinit--validate-entry
                 '("cmd" :id "svc" :sandbox-rw-bind ("")))))
    (should (string-match-p "must not contain empty paths" reason))))

(ert-deftest elinit-test-sandbox-validate-path-forbidden ()
  "Forbidden paths in bind lists are rejected."
  (let ((reason (elinit--validate-entry
                 '("cmd" :id "svc" :sandbox-tmpfs ("/proc")))))
    (should (string-match-p "must not include forbidden path" reason))))

(ert-deftest elinit-test-sandbox-validate-path-forbidden-dev ()
  "Forbidden /dev path in bind lists is rejected."
  (let ((reason (elinit--validate-entry
                 '("cmd" :id "svc" :sandbox-ro-bind ("/dev")))))
    (should (string-match-p "must not include forbidden path" reason))))

(ert-deftest elinit-test-sandbox-validate-path-forbidden-trailing-slash ()
  "Forbidden paths with trailing slash are rejected."
  (let ((reason (elinit--validate-entry
                 '("cmd" :id "svc" :sandbox-tmpfs ("/proc/")))))
    (should (string-match-p "must not include forbidden path" reason))))

(ert-deftest elinit-test-sandbox-validate-path-forbidden-dev-trailing-slash ()
  "Forbidden /dev/ path with trailing slash is rejected."
  (let ((reason (elinit--validate-entry
                 '("cmd" :id "svc" :sandbox-ro-bind ("/dev/")))))
    (should (string-match-p "must not include forbidden path" reason))))

(ert-deftest elinit-test-sandbox-validate-path-forbidden-proc-dot-segment ()
  "Forbidden /proc/. dot-segment alias is rejected."
  (let ((reason (elinit--validate-entry
                 '("cmd" :id "svc" :sandbox-tmpfs ("/proc/.")))))
    (should (string-match-p "must not include forbidden path" reason))))

(ert-deftest elinit-test-sandbox-validate-path-forbidden-dev-dot-segment ()
  "Forbidden /dev/. dot-segment alias is rejected."
  (let ((reason (elinit--validate-entry
                 '("cmd" :id "svc" :sandbox-ro-bind ("/dev/.")))))
    (should (string-match-p "must not include forbidden path" reason))))

(ert-deftest elinit-test-sandbox-validate-path-valid ()
  "Valid absolute paths pass validation."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (name) (when (equal name "bwrap") "/usr/bin/bwrap"))))
    (should-not (elinit--validate-entry
                 '("cmd" :id "svc" :sandbox-ro-bind ("/tmp"))))))

(ert-deftest elinit-test-sandbox-validate-bind-source-nonexistent ()
  "Non-existent source path in ro-bind is rejected."
  (let ((reason (elinit--validate-entry
                 '("cmd" :id "svc" :sandbox-ro-bind
                   ("/definitely/does/not/exist/elinit-test")))))
    (should (string-match-p "source path does not exist" reason))))

(ert-deftest elinit-test-sandbox-validate-rw-bind-source-nonexistent ()
  "Non-existent source path in rw-bind is rejected."
  (let ((reason (elinit--validate-entry
                 '("cmd" :id "svc" :sandbox-rw-bind
                   ("/definitely/does/not/exist/elinit-test")))))
    (should (string-match-p "source path does not exist" reason))))

(ert-deftest elinit-test-sandbox-validate-tmpfs-no-existence-check ()
  "Tmpfs paths do not require source existence."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (name) (when (equal name "bwrap") "/usr/bin/bwrap"))))
    (should-not (elinit--validate-entry
                 '("cmd" :id "svc" :sandbox-tmpfs
                   ("/definitely/does/not/exist/elinit-test"))))))

(ert-deftest elinit-test-sandbox-validate-bind-source-string-nonexistent ()
  "Non-existent source path as string in ro-bind is rejected."
  (let ((reason (elinit--validate-entry
                 '("cmd" :id "svc" :sandbox-ro-bind
                   "/definitely/does/not/exist/elinit-test"))))
    (should (string-match-p "source path does not exist" reason))))

(ert-deftest elinit-test-sandbox-validate-path-invalid-type ()
  "Non-string path list is rejected."
  (let ((reason (elinit--validate-entry
                 '("cmd" :id "svc" :sandbox-rw-bind (42)))))
    (should (string-match-p "must be a string or list of absolute" reason))))

(ert-deftest elinit-test-sandbox-validate-raw-args-gate-off ()
  "Raw args rejected when gate is off."
  (let ((elinit-sandbox-allow-raw-bwrap nil))
    (let ((reason (elinit--validate-entry
                   '("cmd" :id "svc"
                     :sandbox-raw-args ("--cap-add" "CAP_NET_RAW")))))
      (should (string-match-p "requires elinit-sandbox-allow-raw-bwrap" reason)))))

(ert-deftest elinit-test-sandbox-validate-raw-args-gate-on ()
  "Raw args accepted when gate is on."
  (let ((elinit-sandbox-allow-raw-bwrap t))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name) (when (equal name "bwrap") "/usr/bin/bwrap"))))
      (should-not (elinit--validate-entry
                   '("cmd" :id "svc"
                     :sandbox-raw-args ("--cap-add" "CAP_NET_RAW")))))))

(ert-deftest elinit-test-sandbox-validate-raw-args-shape ()
  "Raw args must be list of strings."
  (let ((elinit-sandbox-allow-raw-bwrap t))
    (let ((reason (elinit--validate-entry
                   '("cmd" :id "svc" :sandbox-raw-args (42)))))
      (should (string-match-p "must be a list of strings" reason)))))

(ert-deftest elinit-test-sandbox-validate-raw-args-share-net-conflicts-isolated ()
  "Raw --share-net conflicts with explicit :sandbox-network isolated."
  (let ((elinit-sandbox-allow-raw-bwrap t))
    (let ((reason (elinit--validate-entry
                   '("cmd" :id "svc"
                     :sandbox-profile strict
                     :sandbox-network isolated
                     :sandbox-raw-args ("--share-net")))))
      (should (string-match-p "\"--share-net\" conflicts with effective network isolated"
                              reason)))))

(ert-deftest elinit-test-sandbox-validate-raw-args-unshare-net-conflicts-shared ()
  "Raw --unshare-net conflicts with explicit :sandbox-network shared."
  (let ((elinit-sandbox-allow-raw-bwrap t))
    (let ((reason (elinit--validate-entry
                   '("cmd" :id "svc"
                     :sandbox-profile service
                     :sandbox-network shared
                     :sandbox-raw-args ("--unshare-net")))))
      (should (string-match-p "\"--unshare-net\" conflicts with effective network shared"
                              reason)))))

(ert-deftest elinit-test-sandbox-validate-raw-args-share-net-conflicts-strict-default ()
  "Raw --share-net conflicts with strict profile default (isolated)."
  (let ((elinit-sandbox-allow-raw-bwrap t))
    (let ((reason (elinit--validate-entry
                   '("cmd" :id "svc"
                     :sandbox-profile strict
                     :sandbox-raw-args ("--share-net")))))
      (should (string-match-p "\"--share-net\" conflicts with effective network isolated"
                              reason)))))

(ert-deftest elinit-test-sandbox-validate-raw-args-unshare-net-conflicts-service-default ()
  "Raw --unshare-net conflicts with service profile default (shared)."
  (let ((elinit-sandbox-allow-raw-bwrap t))
    (let ((reason (elinit--validate-entry
                   '("cmd" :id "svc"
                     :sandbox-profile service
                     :sandbox-raw-args ("--unshare-net")))))
      (should (string-match-p "\"--unshare-net\" conflicts with effective network shared"
                              reason)))))

(ert-deftest elinit-test-sandbox-validate-raw-args-unshare-all-conflicts ()
  "Raw --unshare-all conflicts with profile-managed namespace setup."
  (let ((elinit-sandbox-allow-raw-bwrap t))
    (let ((reason (elinit--validate-entry
                   '("cmd" :id "svc"
                     :sandbox-profile service
                     :sandbox-raw-args ("--unshare-all")))))
      (should (string-match-p "\"--unshare-all\" conflicts with profile-managed"
                              reason)))))

(ert-deftest elinit-test-sandbox-validate-raw-args-die-with-parent-conflicts ()
  "Raw --die-with-parent conflicts with profile-managed setup."
  (let ((elinit-sandbox-allow-raw-bwrap t))
    (let ((reason (elinit--validate-entry
                   '("cmd" :id "svc"
                     :sandbox-profile strict
                     :sandbox-raw-args ("--die-with-parent")))))
      (should (string-match-p "\"--die-with-parent\" conflicts with profile-managed"
                              reason)))))

(ert-deftest elinit-test-sandbox-validate-raw-args-proc-conflicts ()
  "Raw --proc conflicts with profile-managed mount setup."
  (let ((elinit-sandbox-allow-raw-bwrap t))
    (let ((reason (elinit--validate-entry
                   '("cmd" :id "svc"
                     :sandbox-profile service
                     :sandbox-raw-args ("--proc")))))
      (should (string-match-p "\"--proc\" conflicts with profile-managed"
                              reason)))))

(ert-deftest elinit-test-sandbox-validate-raw-args-safe-arg-accepted ()
  "Non-conflicting raw args like --cap-add pass validation."
  (let ((elinit-sandbox-allow-raw-bwrap t))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name) (when (equal name "bwrap") "/usr/bin/bwrap"))))
      (should-not (elinit--validate-entry
                   '("cmd" :id "svc"
                     :sandbox-profile service
                     :sandbox-raw-args ("--cap-add" "CAP_NET_RAW")))))))

(ert-deftest elinit-test-sandbox-validate-target-rejects ()
  "Sandbox keys are rejected for target type."
  (let ((reason (elinit--validate-entry
                 '(nil :id "app.target" :type target
                       :sandbox-profile strict))))
    (should (string-match-p ":sandbox-profile is invalid for :type target"
                            reason))))

(ert-deftest elinit-test-sandbox-validate-non-linux ()
  "Sandbox-requesting units on non-Linux are rejected."
  (cl-letf (((symbol-value 'system-type) 'darwin))
    (let ((reason (elinit--validate-entry
                   '("cmd" :id "svc" :sandbox-profile strict))))
      (should (string-match-p "only supported on GNU/Linux" reason)))))

(ert-deftest elinit-test-sandbox-validate-missing-bwrap ()
  "Sandbox-requesting units without bwrap are rejected."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (name)
               (unless (equal name "bwrap")
                 (executable-find name)))))
    (let ((reason (elinit--validate-entry
                   '("cmd" :id "svc" :sandbox-profile strict))))
      (should (string-match-p "bwrap.*not found" reason)))))

(ert-deftest elinit-test-sandbox-validate-non-sandbox-ignores-bwrap ()
  "Non-sandbox units validate fine without bwrap."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (name)
               (unless (equal name "bwrap")
                 (executable-find name)))))
    (should-not (elinit--validate-entry '("cmd" :id "svc")))))

(ert-deftest elinit-test-sandbox-mixed-plan-missing-bwrap ()
  "Mixed plan invalidates sandbox entry and keeps non-sandbox entry valid.
Build a plan with one sandbox-requesting and one non-sandbox entry while
bwrap is missing.  The sandbox entry must land in the invalid hash with
a bwrap-related reason, while the non-sandbox entry must be in the valid
entries list and eligible for DAG startup."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (name)
               (unless (equal name "bwrap")
                 (executable-find name)))))
    (let* ((programs '(("sleep 300" :id "sandboxed-svc"
                         :sandbox-profile strict)
                       ("sleep 300" :id "plain-svc")))
           (warnings nil))
      ;; Capture warnings emitted by elinit--log
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) warnings))))
        (let ((plan (elinit--build-plan programs)))
          ;; Sandbox-requesting entry must be invalid
          (should (gethash "sandboxed-svc"
                           (elinit-plan-invalid plan)))
          (should (string-match-p
                   "bwrap"
                   (gethash "sandboxed-svc"
                            (elinit-plan-invalid plan))))
          ;; Non-sandbox entry must be valid
          (should (cl-find "plain-svc"
                           (elinit-plan-entries plan)
                           :key #'elinit-entry-id
                           :test #'equal))
          (should-not (gethash "plain-svc"
                               (elinit-plan-invalid plan)))
          ;; Warning must have been emitted for the sandbox entry
          (should (cl-some (lambda (w)
                             (and (string-match-p "INVALID" w)
                                  (string-match-p "sandboxed-svc" w)))
                           warnings)))))))

(ert-deftest elinit-test-sandbox-nil-bind-key-presence-requesting ()
  "Entry with :sandbox-ro-bind nil is sandbox-requesting at validation.
Per plan contract, key presence (not truthy value) triggers sandbox-request
detection in validation.  Validation uses plist-member and rejects when
bwrap is missing.  Runtime uses truthy checks on parsed tuples (where nil
and absent are indistinguishable) and correctly does not apply sandbox."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (name)
               (unless (equal name "bwrap")
                 (executable-find name)))))
    ;; Validation detects key presence: sandbox-requesting, rejected for bwrap
    (let ((reason (elinit--validate-entry
                   '("cmd" :id "svc" :sandbox-ro-bind nil))))
      (should (string-match-p "bwrap" reason)))
    ;; Runtime detection uses truthy checks on parsed tuple: not requesting
    ;; (harmless -- validation already gated entry acceptance)
    (let ((entry (elinit--parse-entry
                  '("cmd" :id "svc" :sandbox-ro-bind nil))))
      (should-not (elinit--sandbox-requesting-p entry)))))

;; Profile registry and command builder tests

(ert-deftest elinit-test-sandbox-profile-none-no-argv ()
  "Profile none produces no argv."
  (should-not (elinit--sandbox-profile-args 'none)))

(ert-deftest elinit-test-sandbox-profile-strict-deterministic ()
  "Profile strict produces deterministic argv."
  (let ((args (elinit--sandbox-profile-args 'strict)))
    (should (member "--unshare-all" args))
    (should (member "--die-with-parent" args))
    (should (member "--ro-bind" args))))

(ert-deftest elinit-test-sandbox-profile-service-deterministic ()
  "Profile service produces deterministic argv."
  (let ((args (elinit--sandbox-profile-args 'service)))
    (should (member "--die-with-parent" args))
    (should (member "--unshare-pid" args))
    (should (member "--unshare-ipc" args))
    (should (member "--ro-bind" args))
    ;; Service does NOT unshare-all (unlike strict)
    (should-not (member "--unshare-all" args))))

(ert-deftest elinit-test-sandbox-profile-desktop-deterministic ()
  "Profile desktop produces deterministic argv."
  (let ((args (elinit--sandbox-profile-args 'desktop)))
    (should (member "--die-with-parent" args))
    (should (member "--unshare-pid" args))
    (should (member "--ro-bind" args))
    ;; Desktop binds X11 socket for graphical access
    (should (member (expand-file-name ".X11-unix" (temporary-file-directory))
                    args))
    ;; Desktop does NOT unshare-all (unlike strict)
    (should-not (member "--unshare-all" args))))

(ert-deftest elinit-test-sandbox-profile-service-shared-network ()
  "Profile service defaults to shared network."
  (should (eq (elinit--sandbox-profile-default-network 'service)
              'shared)))

(ert-deftest elinit-test-sandbox-profile-desktop-shared-network ()
  "Profile desktop defaults to shared network."
  (should (eq (elinit--sandbox-profile-default-network 'desktop)
              'shared)))

(ert-deftest elinit-test-sandbox-profile-strict-isolated-network ()
  "Profile strict defaults to isolated network."
  (should (eq (elinit--sandbox-profile-default-network 'strict)
              'isolated)))

(ert-deftest elinit-test-sandbox-build-argv-none ()
  "Build-argv returns nil for non-sandbox entry."
  (let ((entry (elinit--parse-entry '("sleep 300" :id "svc"))))
    (should-not (elinit--sandbox-build-argv entry))))

(ert-deftest elinit-test-sandbox-build-argv-strict ()
  "Build-argv returns bwrap wrapper for strict profile."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc" :sandbox-profile strict))))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_name) "/usr/bin/bwrap")))
      (let ((argv (elinit--sandbox-build-argv entry)))
        (should argv)
        (should (equal (car argv) "/usr/bin/bwrap"))
        (should (member "--unshare-all" argv))
        (should (member "--unshare-net" argv))
        (should (equal (car (last argv)) "--"))))))

(ert-deftest elinit-test-sandbox-build-argv-network-override ()
  "Build-argv respects network override for strict profile.
Strict uses --unshare-all which includes network isolation.
Override to shared must emit --share-net to restore networking."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc"
                  :sandbox-profile strict
                  :sandbox-network shared))))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_name) "/usr/bin/bwrap")))
      (let ((argv (elinit--sandbox-build-argv entry)))
        ;; --share-net must be present to counter --unshare-all
        (should (member "--share-net" argv))
        ;; --unshare-net must NOT be added
        (should-not (member "--unshare-net" argv))))))

(ert-deftest elinit-test-sandbox-build-argv-service-shared-no-share-net ()
  "Service profile with shared network does not emit --share-net.
Only profiles using --unshare-all need --share-net to restore networking."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc"
                  :sandbox-profile service
                  :sandbox-network shared))))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_name) "/usr/bin/bwrap")))
      (let ((argv (elinit--sandbox-build-argv entry)))
        ;; service does not use --unshare-all, so no --share-net needed
        (should-not (member "--share-net" argv))
        (should-not (member "--unshare-net" argv))))))

(ert-deftest elinit-test-sandbox-build-argv-desktop ()
  "Build-argv for desktop profile produces deterministic argv.
Desktop uses per-namespace unshare args, shared network by default,
and includes X11 and XDG runtime dir binds."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc"
                  :sandbox-profile desktop))))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_name) "/usr/bin/bwrap")))
      (let ((argv (elinit--sandbox-build-argv entry)))
        (should (equal "/usr/bin/bwrap" (car argv)))
        (should (member "--unshare-pid" argv))
        (should (member "--unshare-ipc" argv))
        (should (member "--unshare-uts" argv))
        (should (member "--die-with-parent" argv))
        ;; Desktop has shared network by default -- no unshare-net
        (should-not (member "--unshare-net" argv))
        ;; X11 socket bind
        (should (member (expand-file-name ".X11-unix" (temporary-file-directory))
                        argv))
        ;; Separator at end
        (should (equal "--" (car (last argv))))))))

(ert-deftest elinit-test-sandbox-build-argv-ro-bind ()
  "Build-argv appends read-only bind mounts."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc"
                  :sandbox-profile service
                  :sandbox-ro-bind ("/opt/data")))))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_name) "/usr/bin/bwrap")))
      (let ((argv (elinit--sandbox-build-argv entry)))
        ;; Find the custom ro-bind (profile already has ro-bind / /)
        (let ((pos (cl-position "/opt/data" argv :test #'equal)))
          (should pos)
          (should (equal (nth (1- pos) argv) "--ro-bind")))))))

(ert-deftest elinit-test-sandbox-build-argv-rw-bind ()
  "Build-argv appends read-write bind mounts."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc"
                  :sandbox-profile service
                  :sandbox-rw-bind ("/var/lib/app")))))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_name) "/usr/bin/bwrap")))
      (let ((argv (elinit--sandbox-build-argv entry)))
        (let ((pos (cl-position "/var/lib/app" argv :test #'equal)))
          (should pos)
          (should (equal (nth (1- pos) argv) "--bind")))))))

(ert-deftest elinit-test-sandbox-build-argv-tmpfs ()
  "Build-argv appends tmpfs mounts."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc"
                  :sandbox-profile service
                  :sandbox-tmpfs ("/run/scratch")))))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_name) "/usr/bin/bwrap")))
      (let ((argv (elinit--sandbox-build-argv entry)))
        (let ((pos (cl-position "/run/scratch" argv :test #'equal)))
          (should pos)
          (should (equal (nth (1- pos) argv) "--tmpfs")))))))

(ert-deftest elinit-test-sandbox-build-argv-raw-args ()
  "Build-argv appends raw args."
  (let ((elinit-sandbox-allow-raw-bwrap t))
    (let ((entry (elinit--parse-entry
                  '("sleep 300" :id "svc"
                    :sandbox-profile service
                    :sandbox-raw-args ("--cap-add" "CAP_NET_RAW")))))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (_name) "/usr/bin/bwrap")))
        (let ((argv (elinit--sandbox-build-argv entry)))
          (should (member "--cap-add" argv))
          (should (member "CAP_NET_RAW" argv)))))))

;; Launch command integration tests

(ert-deftest elinit-test-sandbox-build-launch-no-sandbox ()
  "Build-launch-command without sandbox produces normal argv."
  (let ((args (elinit--build-launch-command "sleep 300")))
    (should (equal args '("sleep" "300")))))

(ert-deftest elinit-test-sandbox-build-launch-with-sandbox ()
  "Build-launch-command with sandbox prepends bwrap."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc" :sandbox-profile service))))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_name) "/usr/bin/bwrap")))
      (let ((args (elinit--build-launch-command "sleep 300" nil nil
                                                    entry)))
        (should (equal (car args) "/usr/bin/bwrap"))
        ;; Should end with -- sleep 300
        (let ((sep-pos (cl-position "--" args :test #'equal :from-end t)))
          (should sep-pos)
          (should (equal (nth (+ sep-pos 1) args) "/usr/bin/bwrap")))))))

(ert-deftest elinit-test-sandbox-build-launch-runas-then-bwrap ()
  "Build-launch-command with identity + sandbox uses correct order."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc" :sandbox-profile service))))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_name) "/usr/bin/bwrap")))
      (let ((args (elinit--build-launch-command "sleep 300" "alice" nil
                                                    entry)))
        ;; First element should be runas helper
        (should (equal (car args) elinit-runas-command))
        ;; Should contain --user alice
        (should (member "--user" args))
        (should (member "alice" args))
        ;; bwrap should appear after the first -- separator (runas --)
        (let* ((first-sep (cl-position "--" args :test #'equal))
               (after-sep (nth (1+ first-sep) args)))
          (should (equal after-sep "/usr/bin/bwrap")))))))

(ert-deftest elinit-test-sandbox-build-launch-resolves-service-exe ()
  "Build-launch-command resolves service executable under runas + bwrap.
The service binary must be resolved to an absolute path via executable-find
even when both identity wrapper and sandbox wrapper are active."
  (let ((entry (elinit--parse-entry
                '("sleep 300" :id "svc" :sandbox-profile service))))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name)
                 (cond ((equal name "bwrap") "/usr/bin/bwrap")
                       ((equal name "sleep") "/usr/bin/sleep")
                       (t nil)))))
      (let ((args (elinit--build-launch-command "sleep 300" "alice" nil
                                                    entry)))
        ;; Runas is first
        (should (equal (car args) elinit-runas-command))
        ;; bwrap appears after runas -- separator
        (let ((first-sep (cl-position "--" args :test #'equal)))
          (should first-sep)
          (should (equal (nth (1+ first-sep) args) "/usr/bin/bwrap")))
        ;; Service executable is resolved to absolute path after bwrap --
        (let ((last-sep (cl-position "--" args :test #'equal :from-end t)))
          (should last-sep)
          (should (equal (nth (1+ last-sep) args) "/usr/bin/sleep"))
          (should (equal (nth (+ last-sep 2) args) "300")))))))

;; Service struct roundtrip tests

(ert-deftest elinit-test-sandbox-service-roundtrip ()
  "Service-to-entry conversion preserves sandbox fields."
  (let* ((svc (elinit-service--create
               :id "svc" :command "cmd"
               :sandbox-profile 'strict
               :sandbox-network 'isolated
               :sandbox-ro-bind '("/opt")
               :sandbox-rw-bind '("/var")
               :sandbox-tmpfs '("/tmp/work")
               :sandbox-raw-args '("--cap-add" "CAP_NET_RAW")))
         (entry (elinit-service-to-entry svc)))
    (should (= (length entry) 45))
    (should (eq (elinit-entry-sandbox-profile entry) 'strict))
    (should (eq (elinit-entry-sandbox-network entry) 'isolated))
    (should (equal (elinit-entry-sandbox-ro-bind entry) '("/opt")))
    (should (equal (elinit-entry-sandbox-rw-bind entry) '("/var")))
    (should (equal (elinit-entry-sandbox-tmpfs entry) '("/tmp/work")))
    (should (equal (elinit-entry-sandbox-raw-args entry)
                   '("--cap-add" "CAP_NET_RAW")))))

;; Non-regression test

(ert-deftest elinit-test-sandbox-non-sandbox-unchanged ()
  "Non-sandbox entry launch behavior is unchanged."
  (let ((entry (elinit--parse-entry '("sleep 300" :id "svc"))))
    (should-not (elinit--sandbox-requesting-p entry))
    ;; Build-launch-command with nil sandbox-entry is identical to no sandbox
    (let ((with (elinit--build-launch-command "sleep 300" nil nil entry))
          (without (elinit--build-launch-command "sleep 300" nil nil nil)))
      (should (equal with without)))))


(provide 'elinit-test-sandbox)
;;; elinit-test-sandbox.el ends here
