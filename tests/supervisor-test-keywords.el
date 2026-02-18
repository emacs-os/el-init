;;; supervisor-test-keywords.el --- PT2/PT3 keyword expansion tests for supervisor.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; PT2/PT3 keyword expansion ERT tests for supervisor.el.

;;; Code:

(require 'supervisor-test-helpers)

;;;; Phase P1: New keyword parsing, validation, and normalization

(ert-deftest supervisor-test-parse-entry-new-fields-defaults ()
  "Parsed entry has nil defaults for P2 and PT3 fields."
  (let ((entry (supervisor--parse-entry "echo hello")))
    (should (= (length entry) 39))
    (should-not (supervisor-entry-working-directory entry))
    (should-not (supervisor-entry-environment entry))
    (should-not (supervisor-entry-environment-file entry))
    (should-not (supervisor-entry-exec-stop entry))
    (should-not (supervisor-entry-exec-reload entry))
    (should-not (supervisor-entry-restart-sec entry))
    (should-not (supervisor-entry-description entry))
    (should-not (supervisor-entry-documentation entry))
    (should-not (supervisor-entry-before entry))
    (should-not (supervisor-entry-wants entry))
    (should-not (supervisor-entry-kill-signal entry))
    (should-not (supervisor-entry-kill-mode entry))
    (should-not (supervisor-entry-remain-after-exit entry))
    (should-not (supervisor-entry-success-exit-status entry))
    (should-not (supervisor-entry-wanted-by entry))
    (should-not (supervisor-entry-required-by entry))))

(ert-deftest supervisor-test-parse-entry-working-directory ()
  "Parsed entry extracts :working-directory."
  (let ((entry (supervisor--parse-entry
                '("echo hi" :id "svc" :working-directory "/tmp"))))
    (should (equal (supervisor-entry-working-directory entry) "/tmp"))))

(ert-deftest supervisor-test-parse-entry-environment ()
  "Parsed entry extracts :environment alist."
  (let ((entry (supervisor--parse-entry
                '("echo hi" :id "svc"
                  :environment (("FOO" . "bar") ("BAZ" . "qux"))))))
    (should (equal (supervisor-entry-environment entry)
                   '(("FOO" . "bar") ("BAZ" . "qux"))))))

(ert-deftest supervisor-test-parse-entry-environment-file-string ()
  "Parsed entry normalizes :environment-file string to list."
  (let ((entry (supervisor--parse-entry
                '("echo hi" :id "svc" :environment-file "/etc/env"))))
    (should (equal (supervisor-entry-environment-file entry)
                   '("/etc/env")))))

(ert-deftest supervisor-test-parse-entry-environment-file-list ()
  "Parsed entry preserves :environment-file list."
  (let ((entry (supervisor--parse-entry
                '("echo hi" :id "svc"
                  :environment-file ("/etc/env" "-/etc/env.local")))))
    (should (equal (supervisor-entry-environment-file entry)
                   '("/etc/env" "-/etc/env.local")))))

(ert-deftest supervisor-test-parse-entry-exec-stop-string ()
  "Parsed entry normalizes :exec-stop string to list."
  (let ((entry (supervisor--parse-entry
                '("my-daemon" :id "svc" :exec-stop "kill-cmd"))))
    (should (equal (supervisor-entry-exec-stop entry) '("kill-cmd")))))

(ert-deftest supervisor-test-parse-entry-exec-stop-list ()
  "Parsed entry preserves :exec-stop list."
  (let ((entry (supervisor--parse-entry
                '("my-daemon" :id "svc"
                  :exec-stop ("stop-phase1" "stop-phase2")))))
    (should (equal (supervisor-entry-exec-stop entry)
                   '("stop-phase1" "stop-phase2")))))

(ert-deftest supervisor-test-parse-entry-exec-reload-string ()
  "Parsed entry normalizes :exec-reload string to list."
  (let ((entry (supervisor--parse-entry
                '("my-daemon" :id "svc" :exec-reload "reload-cmd"))))
    (should (equal (supervisor-entry-exec-reload entry)
                   '("reload-cmd")))))

(ert-deftest supervisor-test-parse-entry-restart-sec ()
  "Parsed entry extracts :restart-sec."
  (let ((entry (supervisor--parse-entry
                '("my-daemon" :id "svc" :restart-sec 5))))
    (should (equal (supervisor-entry-restart-sec entry) 5))))

(ert-deftest supervisor-test-parse-entry-restart-sec-zero ()
  "Parsed entry accepts :restart-sec 0."
  (let ((entry (supervisor--parse-entry
                '("my-daemon" :id "svc" :restart-sec 0))))
    (should (equal (supervisor-entry-restart-sec entry) 0))))

(ert-deftest supervisor-test-validate-duplicate-key-entry ()
  "Duplicate plist keys are rejected in entry validation."
  (let ((reason (supervisor--validate-entry
                 '("cmd" :id "svc" :delay 1 :delay 2))))
    (should reason)
    (should (string-match-p "duplicate key :delay" reason))))

(ert-deftest supervisor-test-validate-duplicate-key-unit-file ()
  "Duplicate plist keys are rejected in unit-file validation."
  (let ((reason (supervisor--validate-unit-file-plist
                 '(:id "svc" :command "cmd" :delay 1 :delay 2)
                 "/test.el" 1)))
    (should reason)
    (should (string-match-p "duplicate key :delay" reason))))

(ert-deftest supervisor-test-plist-duplicate-keys-none ()
  "No duplicates returns nil."
  (should-not (supervisor--plist-duplicate-keys
               '(:id "svc" :command "cmd" :delay 1))))

(ert-deftest supervisor-test-plist-duplicate-keys-found ()
  "Duplicate keys are detected."
  (should (equal (supervisor--plist-duplicate-keys
                  '(:id "svc" :delay 1 :id "other" :delay 2))
                 '(:id :delay))))

(ert-deftest supervisor-test-validate-duplicate-key-description ()
  "Duplicate :description key is rejected."
  (let ((reason (supervisor--validate-entry
                 '("cmd" :id "svc" :description "a" :description "b"))))
    (should reason)
    (should (string-match-p "duplicate key :description" reason))))

(ert-deftest supervisor-test-validate-duplicate-key-documentation ()
  "Duplicate :documentation key is rejected."
  (let ((reason (supervisor--validate-entry
                 '("cmd" :id "svc" :documentation "a" :documentation "b"))))
    (should reason)
    (should (string-match-p "duplicate key :documentation" reason))))

(ert-deftest supervisor-test-validate-duplicate-key-before ()
  "Duplicate :before key is rejected."
  (let ((reason (supervisor--validate-entry
                 '("cmd" :id "svc" :before "a" :before "b"))))
    (should reason)
    (should (string-match-p "duplicate key :before" reason))))

(ert-deftest supervisor-test-validate-duplicate-key-wants ()
  "Duplicate :wants key is rejected."
  (let ((reason (supervisor--validate-entry
                 '("cmd" :id "svc" :wants "a" :wants "b"))))
    (should reason)
    (should (string-match-p "duplicate key :wants" reason))))

(ert-deftest supervisor-test-validate-duplicate-key-kill-signal ()
  "Duplicate :kill-signal key is rejected."
  (let ((reason (supervisor--validate-entry
                 '("cmd" :id "svc" :kill-signal SIGTERM :kill-signal SIGHUP))))
    (should reason)
    (should (string-match-p "duplicate key :kill-signal" reason))))

(ert-deftest supervisor-test-validate-duplicate-key-kill-mode ()
  "Duplicate :kill-mode key is rejected."
  (let ((reason (supervisor--validate-entry
                 '("cmd" :id "svc" :kill-mode process :kill-mode mixed))))
    (should reason)
    (should (string-match-p "duplicate key :kill-mode" reason))))

(ert-deftest supervisor-test-validate-duplicate-key-remain-after-exit ()
  "Duplicate :remain-after-exit key is rejected."
  (let ((reason (supervisor--validate-entry
                 '("cmd" :id "svc" :type oneshot
                   :remain-after-exit t :remain-after-exit nil))))
    (should reason)
    (should (string-match-p "duplicate key :remain-after-exit" reason))))

(ert-deftest supervisor-test-validate-duplicate-key-success-exit-status ()
  "Duplicate :success-exit-status key is rejected."
  (let ((reason (supervisor--validate-entry
                 '("cmd" :id "svc" :success-exit-status 42
                   :success-exit-status 43))))
    (should reason)
    (should (string-match-p "duplicate key :success-exit-status" reason))))

(ert-deftest supervisor-test-validate-duplicate-key-working-directory ()
  "Duplicate :working-directory key is rejected."
  (let ((reason (supervisor--validate-entry
                 '("cmd" :id "svc" :working-directory "/a"
                   :working-directory "/b"))))
    (should reason)
    (should (string-match-p "duplicate key :working-directory" reason))))

(ert-deftest supervisor-test-validate-duplicate-key-environment ()
  "Duplicate :environment key is rejected."
  (let ((reason (supervisor--validate-entry
                 '("cmd" :id "svc"
                   :environment (("A" . "1"))
                   :environment (("B" . "2"))))))
    (should reason)
    (should (string-match-p "duplicate key :environment" reason))))

(ert-deftest supervisor-test-validate-duplicate-key-environment-file ()
  "Duplicate :environment-file key is rejected."
  (let ((reason (supervisor--validate-entry
                 '("cmd" :id "svc"
                   :environment-file "/a.env"
                   :environment-file "/b.env"))))
    (should reason)
    (should (string-match-p "duplicate key :environment-file" reason))))

(ert-deftest supervisor-test-validate-duplicate-key-exec-stop ()
  "Duplicate :exec-stop key is rejected."
  (let ((reason (supervisor--validate-entry
                 '("cmd" :id "svc"
                   :exec-stop "stop1"
                   :exec-stop "stop2"))))
    (should reason)
    (should (string-match-p "duplicate key :exec-stop" reason))))

(ert-deftest supervisor-test-validate-duplicate-key-exec-reload ()
  "Duplicate :exec-reload key is rejected."
  (let ((reason (supervisor--validate-entry
                 '("cmd" :id "svc"
                   :exec-reload "reload1"
                   :exec-reload "reload2"))))
    (should reason)
    (should (string-match-p "duplicate key :exec-reload" reason))))

(ert-deftest supervisor-test-validate-duplicate-key-restart-sec ()
  "Duplicate :restart-sec key is rejected."
  (let ((reason (supervisor--validate-entry
                 '("cmd" :id "svc"
                   :restart-sec 3
                   :restart-sec 5))))
    (should reason)
    (should (string-match-p "duplicate key :restart-sec" reason))))

(ert-deftest supervisor-test-validate-working-directory-invalid ()
  "Non-string :working-directory is rejected."
  (let ((reason (supervisor--validate-entry
                 '("cmd" :id "svc" :working-directory 123))))
    (should reason)
    (should (string-match-p ":working-directory must be a string" reason))))

(ert-deftest supervisor-test-validate-environment-invalid ()
  "Non-alist :environment is rejected."
  (let ((reason (supervisor--validate-entry
                 '("cmd" :id "svc" :environment "FOO=bar"))))
    (should reason)
    (should (string-match-p ":environment must be an alist" reason))))

(ert-deftest supervisor-test-validate-environment-bad-pair ()
  "Environment alist with non-string values is rejected."
  (let ((reason (supervisor--validate-entry
                 '("cmd" :id "svc"
                   :environment (("FOO" . 123))))))
    (should reason)
    (should (string-match-p ":environment must be an alist" reason))))

(ert-deftest supervisor-test-validate-environment-file-invalid ()
  "Non-string :environment-file is rejected."
  (let ((reason (supervisor--validate-entry
                 '("cmd" :id "svc" :environment-file 123))))
    (should reason)
    (should (string-match-p ":environment-file must be" reason))))

(ert-deftest supervisor-test-validate-exec-stop-invalid ()
  "Non-string :exec-stop is rejected."
  (let ((reason (supervisor--validate-entry
                 '("cmd" :id "svc" :exec-stop 123))))
    (should reason)
    (should (string-match-p ":exec-stop must be" reason))))

(ert-deftest supervisor-test-validate-exec-reload-invalid ()
  "Non-string :exec-reload is rejected."
  (let ((reason (supervisor--validate-entry
                 '("cmd" :id "svc" :exec-reload 123))))
    (should reason)
    (should (string-match-p ":exec-reload must be" reason))))

(ert-deftest supervisor-test-validate-restart-sec-invalid ()
  "Non-numeric :restart-sec is rejected."
  (let ((reason (supervisor--validate-entry
                 '("cmd" :id "svc" :restart-sec "fast"))))
    (should reason)
    (should (string-match-p ":restart-sec must be" reason))))

(ert-deftest supervisor-test-validate-restart-sec-negative ()
  "Negative :restart-sec is rejected."
  (let ((reason (supervisor--validate-entry
                 '("cmd" :id "svc" :restart-sec -1))))
    (should reason)
    (should (string-match-p ":restart-sec must be" reason))))

(ert-deftest supervisor-test-validate-exec-stop-oneshot-rejected ()
  "The :exec-stop keyword is rejected for oneshot type."
  (let ((reason (supervisor--validate-entry
                 '("cmd" :id "svc" :type oneshot
                   :exec-stop "stop-cmd"))))
    (should reason)
    (should (string-match-p ":exec-stop is invalid for :type oneshot" reason))))

(ert-deftest supervisor-test-validate-exec-reload-oneshot-rejected ()
  "The :exec-reload keyword is rejected for oneshot type."
  (let ((reason (supervisor--validate-entry
                 '("cmd" :id "svc" :type oneshot
                   :exec-reload "reload-cmd"))))
    (should reason)
    (should (string-match-p ":exec-reload is invalid for :type oneshot" reason))))

(ert-deftest supervisor-test-validate-restart-sec-oneshot-rejected ()
  "The :restart-sec keyword is rejected for oneshot type."
  (let ((reason (supervisor--validate-entry
                 '("cmd" :id "svc" :type oneshot
                   :restart-sec 5))))
    (should reason)
    (should (string-match-p ":restart-sec is invalid for :type oneshot" reason))))

(ert-deftest supervisor-test-validate-new-keywords-accepted ()
  "All six new keywords are accepted for simple type."
  (should-not (supervisor--validate-entry
               '("cmd" :id "svc"
                 :working-directory "/tmp"
                 :environment (("FOO" . "bar"))
                 :environment-file "/etc/env"
                 :exec-stop "stop"
                 :exec-reload "reload"
                 :restart-sec 5))))

(ert-deftest supervisor-test-validate-env-dir-accepted-oneshot ()
  "Working-directory, environment, environment-file accepted for oneshot."
  (should-not (supervisor--validate-entry
               '("cmd" :id "svc" :type oneshot
                 :working-directory "/tmp"
                 :environment (("FOO" . "bar"))
                 :environment-file "/etc/env"))))

(ert-deftest supervisor-test-entry-to-service-new-fields ()
  "Entry-to-service conversion carries all six new fields."
  (let* ((entry (supervisor--parse-entry
                 '("my-daemon" :id "svc"
                   :working-directory "/opt"
                   :environment (("K" . "V"))
                   :environment-file ("/etc/env")
                   :exec-stop ("stop1" "stop2")
                   :exec-reload "reload1"
                   :restart-sec 3)))
         (svc (supervisor-entry-to-service entry)))
    (should (equal (supervisor-service-working-directory svc) "/opt"))
    (should (equal (supervisor-service-environment svc) '(("K" . "V"))))
    (should (equal (supervisor-service-environment-file svc) '("/etc/env")))
    (should (equal (supervisor-service-exec-stop svc) '("stop1" "stop2")))
    (should (equal (supervisor-service-exec-reload svc) '("reload1")))
    (should (equal (supervisor-service-restart-sec svc) 3))))

(ert-deftest supervisor-test-service-to-entry-new-fields ()
  "Service-to-entry conversion preserves all P2 and PT3 fields."
  (let* ((svc (supervisor-service--create
               :id "svc" :command "cmd"
               :working-directory "/opt"
               :environment '(("K" . "V"))
               :environment-file '("/etc/env")
               :exec-stop '("stop1")
               :exec-reload '("reload1")
               :restart-sec 3
               :description "test"
               :kill-signal 'SIGTERM
               :kill-mode 'mixed))
         (entry (supervisor-service-to-entry svc)))
    (should (= (length entry) 39))
    (should (equal (supervisor-entry-working-directory entry) "/opt"))
    (should (equal (supervisor-entry-environment entry) '(("K" . "V"))))
    (should (equal (supervisor-entry-environment-file entry) '("/etc/env")))
    (should (equal (supervisor-entry-exec-stop entry) '("stop1")))
    (should (equal (supervisor-entry-exec-reload entry) '("reload1")))
    (should (equal (supervisor-entry-restart-sec entry) 3))
    (should (equal (supervisor-entry-description entry) "test"))
    (should (eq (supervisor-entry-kill-signal entry) 'SIGTERM))
    (should (eq (supervisor-entry-kill-mode entry) 'mixed))))

(ert-deftest supervisor-test-normalize-string-or-list ()
  "String-or-list normalization works correctly."
  (should-not (supervisor--normalize-string-or-list nil))
  (should (equal (supervisor--normalize-string-or-list "foo") '("foo")))
  (should (equal (supervisor--normalize-string-or-list '("a" "b")) '("a" "b")))
  (should-not (supervisor--normalize-string-or-list 123)))

(ert-deftest supervisor-test-build-plan-preserves-new-fields ()
  "Build-plan :requires normalization preserves fields 13-26."
  (let* ((supervisor--authority-snapshot nil)
         (programs '(("svc-a" :id "svc-a"
                      :working-directory "/opt"
                      :environment (("K" . "V"))
                      :exec-stop "stop-cmd"
                      :description "Service A")
                     ("svc-b" :id "svc-b"
                      :requires "svc-a"
                      :restart-sec 5
                      :exec-reload "reload-cmd"
                      :kill-signal SIGTERM)))
         (plan (supervisor--build-plan programs))
         (entries (supervisor-plan-entries plan)))
    ;; Both entries must be full parsed tuples.
    (dolist (entry entries)
      (should (= (length entry) 39)))
    ;; svc-a new fields preserved
    (let ((a (cl-find "svc-a" entries :key #'car :test #'equal)))
      (should (equal (supervisor-entry-working-directory a) "/opt"))
      (should (equal (supervisor-entry-environment a) '(("K" . "V"))))
      (should (equal (supervisor-entry-exec-stop a) '("stop-cmd")))
      (should (equal (supervisor-entry-description a) "Service A")))
    ;; svc-b new fields preserved (has :requires which triggers rewrite)
    (let ((b (cl-find "svc-b" entries :key #'car :test #'equal)))
      (should (equal (supervisor-entry-restart-sec b) 5))
      (should (equal (supervisor-entry-exec-reload b) '("reload-cmd")))
      (should (eq (supervisor-entry-kill-signal b) 'SIGTERM)))))

(ert-deftest supervisor-test-stable-topo-cycle-preserves-new-fields ()
  "Cycle fallback in stable-topo-sort preserves fields 13-26."
  (let ((supervisor--computed-deps (make-hash-table :test 'equal))
        (supervisor--cycle-fallback-ids (make-hash-table :test 'equal)))
    ;; 26-element entries with cycle: a -> b -> a
    (let* ((entries (list (list "a" "cmd" 0 t 'always t 'simple '("b")
                                t 30 nil nil "/opt" '(("K" . "V")) nil
                                '("stop") nil 3
                                "desc-a" nil nil nil nil nil nil nil)
                          (list "b" "cmd" 0 t 'always t 'simple '("a")
                                t 30 nil nil nil nil '("/env") nil
                                '("reload") nil
                                nil nil nil nil 'SIGTERM nil nil nil)))
           (sorted (supervisor--stable-topo-sort entries)))
      ;; All entries must remain 26 fields
      (dolist (entry sorted)
        (should (= (length entry) 26)))
      ;; :after (7) and :requires (11) cleared
      (dolist (entry sorted)
        (should (null (nth 7 entry)))
        (should (null (nth 11 entry))))
      ;; P2 fields preserved
      (let ((a (cl-find "a" sorted :key #'car :test #'equal)))
        (should (equal (supervisor-entry-working-directory a) "/opt"))
        (should (equal (supervisor-entry-environment a) '(("K" . "V"))))
        (should (equal (supervisor-entry-exec-stop a) '("stop")))
        (should (equal (supervisor-entry-restart-sec a) 3))
        (should (equal (supervisor-entry-description a) "desc-a")))
      (let ((b (cl-find "b" sorted :key #'car :test #'equal)))
        (should (equal (supervisor-entry-environment-file b) '("/env")))
        (should (equal (supervisor-entry-exec-reload b) '("reload")))
        (should (eq (supervisor-entry-kill-signal b) 'SIGTERM))))))

(ert-deftest supervisor-test-build-plan-topo-cycle-preserves-new-fields ()
  "Cycle fallback in build-plan-topo-sort preserves fields 12-25."
  ;; 26-element entries with cycle: a -> b -> a
  (let* ((deps (make-hash-table :test 'equal))
         (order-index (make-hash-table :test 'equal))
         (cycle-fallback-ids (make-hash-table :test 'equal))
         (entries (list (list "a" "cmd" 0 t 'always t 'simple '("b")
                              t 30 nil nil "/tmp" nil nil '("s1") '("r1") 2
                              "desc" nil nil nil nil 'mixed nil nil)
                        (list "b" "cmd" 0 t 'always t 'simple '("a")
                              t 30 nil nil nil '(("X" . "Y")) '("/e") nil nil 0
                              nil '("man:b(1)") nil nil 'SIGHUP nil nil nil))))
    (puthash "a" '("b") deps)
    (puthash "b" '("a") deps)
    (puthash "a" 0 order-index)
    (puthash "b" 1 order-index)
    (let ((sorted (supervisor--build-plan-topo-sort
                   entries deps order-index cycle-fallback-ids)))
      ;; All entries must remain 26 fields
      (dolist (entry sorted)
        (should (= (length entry) 26)))
      ;; P2 fields preserved
      (let ((a (cl-find "a" sorted :key #'car :test #'equal)))
        (should (equal (supervisor-entry-working-directory a) "/tmp"))
        (should (equal (supervisor-entry-exec-stop a) '("s1")))
        (should (equal (supervisor-entry-exec-reload a) '("r1")))
        (should (equal (supervisor-entry-restart-sec a) 2))
        (should (equal (supervisor-entry-description a) "desc"))
        (should (eq (supervisor-entry-kill-mode a) 'mixed)))
      (let ((b (cl-find "b" sorted :key #'car :test #'equal)))
        (should (equal (supervisor-entry-environment b) '(("X" . "Y"))))
        (should (equal (supervisor-entry-environment-file b) '("/e")))
        (should (equal (supervisor-entry-restart-sec b) 0))
        (should (equal (supervisor-entry-documentation b) '("man:b(1)")))
        (should (eq (supervisor-entry-kill-signal b) 'SIGHUP))))))

;;;; Phase P2: Runtime foundation tests (cwd/env/restart-sec)

(ert-deftest supervisor-test-parse-env-file ()
  "Parse a well-formed environment file."
  (let ((tmpfile (make-temp-file "env-test")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "# comment\n")
            (insert "; another comment\n")
            (insert "\n")
            (insert "FOO=bar\n")
            (insert "BAZ=qux value\n")
            (insert "export EXPORTED=yes\n")
            (insert "bad line no equals\n")
            (insert "EMPTY=\n"))
          (let ((result (supervisor--parse-env-file tmpfile)))
            (should (equal result '(("FOO" . "bar")
                                    ("BAZ" . "qux value")
                                    ("EXPORTED" . "yes")
                                    ("EMPTY" . ""))))))
      (delete-file tmpfile))))

(ert-deftest supervisor-test-parse-env-file-key-validation ()
  "Env file rejects keys that don't match [A-Za-z_][A-Za-z0-9_]*."
  (let ((tmpfile (make-temp-file "env-test")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "VALID_KEY=ok\n")
            (insert "123BAD=no\n")
            (insert "_ok=yes\n"))
          (let ((result (supervisor--parse-env-file tmpfile)))
            (should (= (length result) 2))
            (should (equal (car result) '("VALID_KEY" . "ok")))
            (should (equal (cadr result) '("_ok" . "yes")))))
      (delete-file tmpfile))))

(ert-deftest supervisor-test-parse-env-file-invalid-lines-logged ()
  "Invalid env-file lines are logged as warnings."
  (let ((tmpfile (make-temp-file "env-test"))
        (logged-warnings nil))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "GOOD=ok\n")
            (insert "bad line no equals\n")
            (insert "123BAD=no\n"))
          (cl-letf (((symbol-function 'supervisor--log)
                     (lambda (level fmt &rest args)
                       (when (eq level 'warning)
                         (push (apply #'format fmt args) logged-warnings)))))
            (let ((result (supervisor--parse-env-file tmpfile)))
              (should (= (length result) 1))
              (should (equal (car result) '("GOOD" . "ok")))
              ;; Two invalid lines should produce two warnings
              (should (= (length logged-warnings) 2))
              (should (cl-some (lambda (w) (string-match-p "bad line no equals" w))
                               logged-warnings))
              (should (cl-some (lambda (w) (string-match-p "123BAD=no" w))
                               logged-warnings)))))
      (delete-file tmpfile))))

(ert-deftest supervisor-test-resolve-env-file-path ()
  "Resolve env-file path with and without optional - prefix."
  (let ((result (supervisor--resolve-env-file-path "/etc/env" "/units/")))
    (should-not (car result))
    (should (equal (cdr result) "/etc/env")))
  (let ((result (supervisor--resolve-env-file-path "-/etc/env.local" "/units/")))
    (should (car result))
    (should (equal (cdr result) "/etc/env.local")))
  ;; Relative path resolved against unit dir
  (let ((result (supervisor--resolve-env-file-path "local.env" "/opt/units/")))
    (should-not (car result))
    (should (equal (cdr result) "/opt/units/local.env")))
  ;; Optional relative
  (let ((result (supervisor--resolve-env-file-path "-local.env" "/opt/units/")))
    (should (car result))
    (should (equal (cdr result) "/opt/units/local.env"))))

(ert-deftest supervisor-test-build-process-environment ()
  "Build effective process-environment from env-files and alist."
  (let* ((tmpfile (make-temp-file "env-test"))
         (process-environment '("INHERITED=yes" "OVERRIDE=old")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "FILE_VAR=from-file\n")
            (insert "OVERRIDE=from-file\n"))
          (let ((result (supervisor--build-process-environment
                         (list tmpfile)
                         '(("ALIST_VAR" . "from-alist")
                           ("OVERRIDE" . "from-alist"))
                         "/tmp/")))
            ;; Alist wins over file (applied later)
            (should (member "OVERRIDE=from-alist" result))
            (should (member "FILE_VAR=from-file" result))
            (should (member "ALIST_VAR=from-alist" result))
            (should (member "INHERITED=yes" result))))
      (delete-file tmpfile))))

(ert-deftest supervisor-test-build-process-environment-optional-missing ()
  "Optional missing env-file (leading -) is silently skipped."
  (let ((process-environment '("KEEP=yes")))
    (let ((result (supervisor--build-process-environment
                   '("-/nonexistent/file.env")
                   nil "/tmp/")))
      ;; Should not error, inherited env preserved
      (should (member "KEEP=yes" result)))))

(ert-deftest supervisor-test-build-process-environment-required-missing ()
  "Required missing env-file (no leading -) signals an error."
  (let ((process-environment '("KEEP=yes")))
    (should-error (supervisor--build-process-environment
                   '("/nonexistent/required.env")
                   nil "/tmp/"))))

(ert-deftest supervisor-test-environment-alist-later-wins ()
  "In :environment alist, later assignment for the same key wins."
  (let ((process-environment '()))
    (let ((result (supervisor--build-process-environment
                   nil
                   '(("KEY" . "first") ("OTHER" . "x") ("KEY" . "second"))
                   "/tmp/")))
      ;; "KEY=second" must appear later in the list than "KEY=first"
      ;; so getenv-style lookup (which scans from head) finds "second"
      (should (member "KEY=second" result))
      ;; "first" is still present but shadowed
      (should (member "KEY=first" result))
      (should (< (cl-position "KEY=second" result :test #'equal)
                 (cl-position "KEY=first" result :test #'equal))))))

(ert-deftest supervisor-test-environment-file-ordering ()
  "Multiple env-files are applied in list order; later file overrides earlier."
  (let ((file1 (make-temp-file "env1-"))
        (file2 (make-temp-file "env2-")))
    (unwind-protect
        (progn
          (with-temp-file file1
            (insert "SHARED=from-file1\n")
            (insert "ONLY1=yes\n"))
          (with-temp-file file2
            (insert "SHARED=from-file2\n")
            (insert "ONLY2=yes\n"))
          (let* ((process-environment '())
                 (result (supervisor--build-process-environment
                          (list file1 file2) nil "/tmp/")))
            ;; file2 applied after file1, so SHARED=from-file2 is later (wins)
            (should (member "SHARED=from-file2" result))
            (should (member "SHARED=from-file1" result))
            (should (< (cl-position "SHARED=from-file2" result :test #'equal)
                       (cl-position "SHARED=from-file1" result :test #'equal)))
            ;; Both unique keys present
            (should (member "ONLY1=yes" result))
            (should (member "ONLY2=yes" result))))
      (delete-file file1)
      (delete-file file2))))

(ert-deftest supervisor-test-resolve-working-directory-absolute ()
  "Resolve absolute working directory."
  (should (equal (supervisor--resolve-working-directory "/tmp" "/opt/") "/tmp")))

(ert-deftest supervisor-test-resolve-working-directory-home ()
  "Resolve ~ in working directory."
  (let ((result (supervisor--resolve-working-directory "~" "/opt/")))
    (should (equal result (expand-file-name "~")))))

(ert-deftest supervisor-test-resolve-working-directory-relative ()
  "Resolve relative working directory against unit-file directory."
  (let ((tmp-dir (make-temp-file "wdir-" t)))
    (unwind-protect
        (let ((sub (expand-file-name "subdir" tmp-dir)))
          (make-directory sub)
          (should (equal (supervisor--resolve-working-directory
                          "subdir" (file-name-as-directory tmp-dir))
                         sub)))
      (delete-directory tmp-dir t))))

(ert-deftest supervisor-test-resolve-working-directory-nonexistent ()
  "Non-existent working directory signals error."
  (should-error (supervisor--resolve-working-directory
                 "/nonexistent/dir/xyz" "/tmp/")))

(ert-deftest supervisor-test-restart-sec-overrides-global ()
  "Per-unit :restart-sec overrides `supervisor-restart-delay' in schedule-restart."
  (let* ((supervisor-restart-delay 10)
         (supervisor--restart-timers (make-hash-table :test 'equal))
         (scheduled-delay nil))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (delay _repeat &rest _args)
                 (setq scheduled-delay delay)
                 'mock-timer))
              ((symbol-function 'supervisor--log) #'ignore)
              ((symbol-function 'supervisor--format-exit-status)
               (lambda (&rest _) "exited")))
      ;; With restart-sec = 3, should use 3 not 10
      (supervisor--schedule-restart "svc" "cmd" t 'simple 'always
                                    'exit 1 nil nil nil 3)
      (should (= scheduled-delay 3)))))

(ert-deftest supervisor-test-restart-sec-nil-uses-global ()
  "Nil :restart-sec falls back to global `supervisor-restart-delay'."
  (let* ((supervisor-restart-delay 7)
         (supervisor--restart-timers (make-hash-table :test 'equal))
         (scheduled-delay nil))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (delay _repeat &rest _args)
                 (setq scheduled-delay delay)
                 'mock-timer))
              ((symbol-function 'supervisor--log) #'ignore)
              ((symbol-function 'supervisor--format-exit-status)
               (lambda (&rest _) "exited")))
      ;; With restart-sec = nil, should use global 7
      (supervisor--schedule-restart "svc" "cmd" t 'simple 'always
                                    'exit 1 nil nil nil nil)
      (should (= scheduled-delay 7)))))

(ert-deftest supervisor-test-restart-sec-zero-immediate ()
  "Restart-sec 0 means immediate retry."
  (let* ((supervisor-restart-delay 10)
         (supervisor--restart-timers (make-hash-table :test 'equal))
         (scheduled-delay nil))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (delay _repeat &rest _args)
                 (setq scheduled-delay delay)
                 'mock-timer))
              ((symbol-function 'supervisor--log) #'ignore)
              ((symbol-function 'supervisor--format-exit-status)
               (lambda (&rest _) "exited")))
      (supervisor--schedule-restart "svc" "cmd" t 'simple 'always
                                    'exit 1 nil nil nil 0)
      (should (= scheduled-delay 0)))))

(ert-deftest supervisor-test-unit-file-directory-for-id ()
  "Return directory of authoritative unit file."
  (let ((supervisor--authority-snapshot nil)
        (tmp-dir (make-temp-file "units-" t)))
    (unwind-protect
        (let ((supervisor-unit-authority-path (list tmp-dir)))
          (with-temp-file (expand-file-name "svc.el" tmp-dir)
            (insert "(:id \"svc\" :command \"echo hi\")"))
          ;; Force snapshot refresh
          (setq supervisor--authority-snapshot nil)
          (let ((dir (supervisor--unit-file-directory-for-id "svc")))
            (should (equal (file-name-as-directory dir)
                           (file-name-as-directory tmp-dir)))))
      (delete-directory tmp-dir t))))

;;;; Phase P3: Stop and Reload Command Execution

(ert-deftest supervisor-test-run-command-with-timeout-success ()
  "Command that exits 0 returns 0."
  (should (= 0 (supervisor--run-command-with-timeout
                "true" 5 nil nil nil))))

(ert-deftest supervisor-test-run-command-with-timeout-failure ()
  "Command that exits non-zero returns its exit code."
  (should (= 1 (supervisor--run-command-with-timeout
                "false" 5 nil nil nil))))

(ert-deftest supervisor-test-run-command-with-timeout-timeout ()
  "Command that exceeds timeout returns 124."
  (should (= 124 (supervisor--run-command-with-timeout
                  "sleep 60" 0.2 nil nil nil))))

(ert-deftest supervisor-test-run-command-with-timeout-inherits-dir ()
  "Command runs in the specified working directory."
  (let ((tmp-dir (make-temp-file "sv-test-" t)))
    (unwind-protect
        (should (= 0 (supervisor--run-command-with-timeout
                      "test -d ." 5 tmp-dir nil nil)))
      (delete-directory tmp-dir t))))

(ert-deftest supervisor-test-run-command-with-timeout-inherits-env ()
  "Command inherits the specified environment."
  (let ((env (cons "SV_TEST_VAR=hello42" process-environment)))
    (should (= 0 (supervisor--run-command-with-timeout
                  "test \"$SV_TEST_VAR\" = hello42" 5 nil env nil)))))

(ert-deftest supervisor-test-run-command-with-timeout-logs-output ()
  "Command output is written to log file when provided."
  (let ((log-file (make-temp-file "sv-test-log-")))
    (unwind-protect
        (progn
          (supervisor--run-command-with-timeout
           "echo test-output-marker" 5 nil nil log-file)
          (let ((content (with-temp-buffer
                           (insert-file-contents log-file)
                           (buffer-string))))
            (should (string-match-p "test-output-marker" content))))
      (delete-file log-file))))

(ert-deftest supervisor-test-exec-command-chain-all-succeed ()
  "All commands succeed returns t."
  (should (eq t (supervisor--exec-command-chain
                 '("true" "true" "true") "test-id"
                 nil nil nil 5))))

(ert-deftest supervisor-test-exec-command-chain-partial-failure ()
  "One failure returns nil but all commands still run."
  (let ((log-file (make-temp-file "sv-test-log-")))
    (unwind-protect
        (progn
          (should (eq nil (supervisor--exec-command-chain
                           '("echo before" "false" "echo after") "test-id"
                           nil nil log-file 5)))
          ;; Verify "after" command ran despite "false" failure
          (let ((content (with-temp-buffer
                           (insert-file-contents log-file)
                           (buffer-string))))
            (should (string-match-p "before" content))
            (should (string-match-p "after" content))))
      (delete-file log-file))))

(ert-deftest supervisor-test-exec-command-chain-timeout ()
  "Timeout in one command returns nil but continues to next."
  (let ((log-file (make-temp-file "sv-test-log-")))
    (unwind-protect
        (progn
          (should (eq nil (supervisor--exec-command-chain
                           '("sleep 60" "echo ran-after-timeout") "test-id"
                           nil nil log-file 0.2)))
          (let ((content (with-temp-buffer
                           (insert-file-contents log-file)
                           (buffer-string))))
            (should (string-match-p "ran-after-timeout" content))))
      (delete-file log-file))))

(ert-deftest supervisor-test-exec-command-chain-empty ()
  "Empty command list returns t."
  (should (eq t (supervisor--exec-command-chain
                 nil "test-id" nil nil nil 5))))

(ert-deftest supervisor-test-manual-stop-with-exec-stop ()
  "Exec-stop commands run before kill-signal with escalation timer."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--manually-stopped (make-hash-table :test 'equal))
        (supervisor--manually-started (make-hash-table :test 'equal))
        (chain-called nil)
        (signal-sent nil)
        (timer-set nil)
        (proc (start-process "test-stop" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "test-stop" proc supervisor--processes)
          (cl-letf (((symbol-function 'supervisor--get-entry-for-id)
                     (lambda (_id)
                       (list "test-stop" "sleep 300" 0 t 'no t 'simple
                             nil nil 30 nil nil
                             nil nil nil
                             '("my-stop-cmd") nil nil)))
                    ((symbol-function 'supervisor--exec-command-chain)
                     (lambda (cmds _id _dir _env _log _timeout)
                       (setq chain-called cmds)
                       ;; Process should still be alive at this point
                       (should (process-live-p proc))
                       t))
                    ((symbol-function 'supervisor--unit-file-directory-for-id)
                     (lambda (_id) nil))
                    ((symbol-function 'signal-process)
                     (lambda (_p sig) (setq signal-sent sig)))
                    ((symbol-function 'run-at-time)
                     (lambda (&rest _args) (setq timer-set t))))
            (let ((result (supervisor--manual-stop "test-stop")))
              (should (eq (plist-get result :status) 'stopped))
              (should (equal chain-called '("my-stop-cmd")))
              ;; Kill-signal sent after exec-stop
              (should (eq 'SIGTERM signal-sent))
              ;; Escalation timer set up
              (should timer-set))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest supervisor-test-manual-stop-without-exec-stop ()
  "Without exec-stop, process is terminated directly."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--manually-stopped (make-hash-table :test 'equal))
        (supervisor--manually-started (make-hash-table :test 'equal))
        (chain-called nil)
        (proc (start-process "test-stop2" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "test-stop2" proc supervisor--processes)
          (cl-letf (((symbol-function 'supervisor--get-entry-for-id)
                     (lambda (_id)
                       ;; Entry with no exec-stop (nil at index 16)
                       (list "test-stop2" "sleep 300" 0 t 'no t 'simple
                             nil nil 30 nil nil
                             nil nil nil
                             nil nil nil)))
                    ((symbol-function 'supervisor--exec-command-chain)
                     (lambda (cmds _id _dir _env _log _timeout)
                       (setq chain-called cmds)
                       t))
                    ((symbol-function 'supervisor--unit-file-directory-for-id)
                     (lambda (_id) nil)))
            (let ((result (supervisor--manual-stop "test-stop2")))
              (should (eq (plist-get result :status) 'stopped))
              ;; Chain should NOT have been called
              (should (null chain-called)))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest supervisor-test-manual-stop-exec-stop-failure-still-terminates ()
  "Even if exec-stop fails, kill-signal is still sent."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--manually-stopped (make-hash-table :test 'equal))
        (supervisor--manually-started (make-hash-table :test 'equal))
        (signal-sent nil)
        (proc (start-process "test-stop3" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "test-stop3" proc supervisor--processes)
          (cl-letf (((symbol-function 'supervisor--get-entry-for-id)
                     (lambda (_id)
                       (list "test-stop3" "sleep 300" 0 t 'no t 'simple
                             nil nil 30 nil nil
                             nil nil nil
                             '("false") nil nil)))
                    ((symbol-function 'supervisor--exec-command-chain)
                     (lambda (_cmds _id _dir _env _log _timeout)
                       nil))  ; Simulate failure
                    ((symbol-function 'supervisor--unit-file-directory-for-id)
                     (lambda (_id) nil))
                    ((symbol-function 'signal-process)
                     (lambda (_p sig) (setq signal-sent sig)))
                    ((symbol-function 'run-at-time)
                     (lambda (&rest _args))))
            (let ((result (supervisor--manual-stop "test-stop3")))
              (should (eq (plist-get result :status) 'stopped))
              ;; Kill-signal sent regardless of exec-stop failure
              (should (eq 'SIGTERM signal-sent)))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest supervisor-test-reload-unit-with-exec-reload ()
  "Reload with exec-reload runs commands without stop/start."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--manually-stopped (make-hash-table :test 'equal))
        (supervisor--manually-started (make-hash-table :test 'equal))
        (supervisor--mask-override (make-hash-table :test 'equal))
        (supervisor--invalid (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--restart-times (make-hash-table :test 'equal))
        (chain-called nil)
        (stop-called nil)
        (proc (start-process "test-reload" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "test-reload" proc supervisor--processes)
          (cl-letf (((symbol-function 'supervisor--reload-find-entry)
                     (lambda (_id)
                       (list "test-reload" "sleep 300" 0 t 'no t 'simple
                             nil nil 30 nil nil
                             nil nil nil
                             nil '("my-reload-cmd") nil)))
                    ((symbol-function 'supervisor--exec-command-chain)
                     (lambda (cmds _id _dir _env _log _timeout)
                       (setq chain-called cmds)
                       t))
                    ((symbol-function 'supervisor--manual-stop)
                     (lambda (_id)
                       (setq stop-called t)
                       (list :status 'stopped :reason nil)))
                    ((symbol-function 'supervisor--unit-file-directory-for-id)
                     (lambda (_id) nil)))
            (let ((result (supervisor--reload-unit "test-reload")))
              (should (equal (plist-get result :action) "reloaded"))
              (should (equal chain-called '("my-reload-cmd")))
              ;; Stop should NOT have been called
              (should (null stop-called))
              ;; Process should still be running
              (should (process-live-p proc)))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest supervisor-test-reload-unit-without-exec-reload ()
  "Reload without exec-reload does stop + start."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--manually-stopped (make-hash-table :test 'equal))
        (supervisor--manually-started (make-hash-table :test 'equal))
        (supervisor--mask-override (make-hash-table :test 'equal))
        (supervisor--invalid (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--restart-times (make-hash-table :test 'equal))
        (stop-called nil)
        (start-called nil)
        (proc (start-process "test-reload2" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "test-reload2" proc supervisor--processes)
          (cl-letf (((symbol-function 'supervisor--reload-find-entry)
                     (lambda (_id)
                       ;; No exec-reload (nil at index 17)
                       (list "test-reload2" "sleep 300" 0 t 'no t 'simple
                             nil nil 30 nil nil
                             nil nil nil
                             nil nil nil)))
                    ((symbol-function 'supervisor--manual-stop)
                     (lambda (_id)
                       (setq stop-called t)
                       (list :status 'stopped :reason nil)))
                    ((symbol-function 'supervisor--start-process)
                     (lambda (&rest _args)
                       (setq start-called t)
                       proc))
                    ((symbol-function 'supervisor--unit-file-directory-for-id)
                     (lambda (_id) nil)))
            (let ((result (supervisor--reload-unit "test-reload2")))
              (should (equal (plist-get result :action) "reloaded"))
              (should stop-called)
              (should start-called))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest supervisor-test-reload-unit-exec-reload-failure ()
  "Reload command failure reports error, keeps process running."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--manually-stopped (make-hash-table :test 'equal))
        (supervisor--manually-started (make-hash-table :test 'equal))
        (supervisor--mask-override (make-hash-table :test 'equal))
        (supervisor--invalid (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--restart-times (make-hash-table :test 'equal))
        (proc (start-process "test-reload3" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "test-reload3" proc supervisor--processes)
          (cl-letf (((symbol-function 'supervisor--reload-find-entry)
                     (lambda (_id)
                       (list "test-reload3" "sleep 300" 0 t 'no t 'simple
                             nil nil 30 nil nil
                             nil nil nil
                             nil '("false") nil)))
                    ((symbol-function 'supervisor--exec-command-chain)
                     (lambda (_cmds _id _dir _env _log _timeout)
                       nil))  ; Simulate failure
                    ((symbol-function 'supervisor--unit-file-directory-for-id)
                     (lambda (_id) nil)))
            (let ((result (supervisor--reload-unit "test-reload3")))
              (should (string-match-p "reload command failed"
                                      (plist-get result :action)))
              ;; Process should still be running
              (should (process-live-p proc)))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest supervisor-test-manual-stop-entry-not-found ()
  "Exec-stop is skipped when entry lookup returns nil."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--manually-stopped (make-hash-table :test 'equal))
        (supervisor--manually-started (make-hash-table :test 'equal))
        (chain-called nil)
        (proc (start-process "test-stop-nf" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "test-stop-nf" proc supervisor--processes)
          (cl-letf (((symbol-function 'supervisor--get-entry-for-id)
                     (lambda (_id) nil))
                    ((symbol-function 'supervisor--exec-command-chain)
                     (lambda (cmds _id _dir _env _log _timeout)
                       (setq chain-called cmds)
                       t)))
            (let ((result (supervisor--manual-stop "test-stop-nf")))
              (should (eq (plist-get result :status) 'stopped))
              (should (null chain-called)))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest supervisor-test-reload-stopped-unit-with-exec-reload ()
  "Stopped simple unit with exec-reload does not run reload chain."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--manually-stopped (make-hash-table :test 'equal))
        (supervisor--manually-started (make-hash-table :test 'equal))
        (supervisor--mask-override (make-hash-table :test 'equal))
        (supervisor--invalid (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--restart-times (make-hash-table :test 'equal))
        (supervisor--oneshot-completed (make-hash-table :test 'equal))
        (chain-called nil))
    ;; No process in supervisor--processes — unit is stopped
    (cl-letf (((symbol-function 'supervisor--reload-find-entry)
               (lambda (_id)
                 (list "stopped-svc" "echo hi" 0 t 'no t 'simple
                       nil nil 30 nil nil
                       nil nil nil
                       nil '("reload-cmd") nil)))
              ((symbol-function 'supervisor--exec-command-chain)
               (lambda (cmds _id _dir _env _log _timeout)
                 (setq chain-called cmds)
                 t))
              ((symbol-function 'supervisor--unit-file-directory-for-id)
               (lambda (_id) nil)))
      (let ((result (supervisor--reload-unit "stopped-svc")))
        ;; Should update definition, not run reload chain
        (should (equal (plist-get result :action) "updated"))
        (should (null chain-called))))))

(ert-deftest supervisor-test-exec-stop-skips-on-cwd-resolution-error ()
  "Exec-stop is skipped when working directory resolution fails."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--manually-stopped (make-hash-table :test 'equal))
        (supervisor--manually-started (make-hash-table :test 'equal))
        (chain-called nil)
        (proc (start-process "test-cwd-err" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "test-cwd-err" proc supervisor--processes)
          (cl-letf (((symbol-function 'supervisor--get-entry-for-id)
                     (lambda (_id)
                       (list "test-cwd-err" "sleep 300" 0 t 'no t 'simple
                             nil nil 30 nil nil
                             "/nonexistent/dir" nil nil
                             '("stop-cmd") nil nil)))
                    ((symbol-function 'supervisor--exec-command-chain)
                     (lambda (cmds _id _dir _env _log _timeout)
                       (setq chain-called cmds)
                       t))
                    ((symbol-function 'supervisor--unit-file-directory-for-id)
                     (lambda (_id) nil)))
            (let ((result (supervisor--manual-stop "test-cwd-err")))
              (should (eq (plist-get result :status) 'stopped))
              ;; Chain should NOT have been called due to cwd error
              (should (null chain-called)))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest supervisor-test-exec-stop-skips-on-env-resolution-error ()
  "Exec-stop is skipped when environment resolution fails."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--manually-stopped (make-hash-table :test 'equal))
        (supervisor--manually-started (make-hash-table :test 'equal))
        (chain-called nil)
        (proc (start-process "test-env-err" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "test-env-err" proc supervisor--processes)
          (cl-letf (((symbol-function 'supervisor--get-entry-for-id)
                     (lambda (_id)
                       (list "test-env-err" "sleep 300" 0 t 'no t 'simple
                             nil nil 30 nil nil
                             nil nil '("/nonexistent/env-file")
                             '("stop-cmd") nil nil)))
                    ((symbol-function 'supervisor--exec-command-chain)
                     (lambda (cmds _id _dir _env _log _timeout)
                       (setq chain-called cmds)
                       t))
                    ((symbol-function 'supervisor--unit-file-directory-for-id)
                     (lambda (_id) nil)))
            (let ((result (supervisor--manual-stop "test-env-err")))
              (should (eq (plist-get result :status) 'stopped))
              ;; Chain should NOT have been called due to env error
              (should (null chain-called)))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest supervisor-test-reload-unit-exec-reload-context-error ()
  "Reload reports error when context resolution fails."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--manually-stopped (make-hash-table :test 'equal))
        (supervisor--manually-started (make-hash-table :test 'equal))
        (supervisor--mask-override (make-hash-table :test 'equal))
        (supervisor--invalid (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--restart-times (make-hash-table :test 'equal))
        (proc (start-process "test-reload-ctx" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "test-reload-ctx" proc supervisor--processes)
          (cl-letf (((symbol-function 'supervisor--reload-find-entry)
                     (lambda (_id)
                       (list "test-reload-ctx" "sleep 300" 0 t 'no t 'simple
                             nil nil 30 nil nil
                             "/nonexistent/dir" nil nil
                             nil '("reload-cmd") nil)))
                    ((symbol-function 'supervisor--unit-file-directory-for-id)
                     (lambda (_id) nil)))
            (let ((result (supervisor--reload-unit "test-reload-ctx")))
              (should (string-match-p "cannot resolve"
                                      (plist-get result :action)))
              ;; Process should still be running
              (should (process-live-p proc)))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest supervisor-test-stop-all-runs-exec-stop ()
  "The stop-all flow runs exec-stop for applicable units."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--manually-stopped (make-hash-table :test 'equal))
        (supervisor--manually-started (make-hash-table :test 'equal))
        (supervisor--timers nil)
        (supervisor--restart-timers (make-hash-table :test 'equal))
        (supervisor--shutting-down nil)
        (supervisor--shutdown-complete-flag nil)
        (supervisor--shutdown-remaining 0)
        (supervisor--shutdown-timer nil)
        (supervisor--shutdown-callback nil)
        (exec-stop-ids nil)
        (proc (start-process "test-stop-all" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "test-stop-all" proc supervisor--processes)
          (cl-letf (((symbol-function 'supervisor--run-exec-stop-for-id)
                     (lambda (id)
                       (push id exec-stop-ids)))
                    ((symbol-function 'supervisor--dag-cleanup)
                     #'ignore)
                    ((symbol-function 'supervisor--emit-event)
                     #'ignore))
            (supervisor-stop)
            ;; exec-stop should have been called for the unit
            (should (member "test-stop-all" exec-stop-ids))))
      (when (process-live-p proc)
        (delete-process proc)))))

;;;; Phase N1: PT3 Parser/Schema/Validation Tests

;; Signal normalization

(ert-deftest supervisor-test-normalize-signal-name-sigterm ()
  "Normalize SIGTERM symbol."
  (should (eq (supervisor--normalize-signal-name 'SIGTERM) 'SIGTERM)))

(ert-deftest supervisor-test-normalize-signal-name-term ()
  "Normalize short form TERM to SIGTERM."
  (should (eq (supervisor--normalize-signal-name 'TERM) 'SIGTERM)))

(ert-deftest supervisor-test-normalize-signal-name-string ()
  "Normalize string form."
  (should (eq (supervisor--normalize-signal-name "sigterm") 'SIGTERM)))

(ert-deftest supervisor-test-normalize-signal-name-string-short ()
  "Normalize short string form."
  (should (eq (supervisor--normalize-signal-name "hup") 'SIGHUP)))

(ert-deftest supervisor-test-normalize-signal-name-unknown ()
  "Return nil for unknown signal."
  (should-not (supervisor--normalize-signal-name 'SIGFAKE)))

(ert-deftest supervisor-test-normalize-signal-name-sigusr1 ()
  "Normalize SIGUSR1."
  (should (eq (supervisor--normalize-signal-name 'USR1) 'SIGUSR1)))

;; Kill-mode normalization

(ert-deftest supervisor-test-normalize-kill-mode-process ()
  "Normalize process symbol."
  (should (eq (supervisor--normalize-kill-mode 'process) 'process)))

(ert-deftest supervisor-test-normalize-kill-mode-mixed ()
  "Normalize mixed symbol."
  (should (eq (supervisor--normalize-kill-mode 'mixed) 'mixed)))

(ert-deftest supervisor-test-normalize-kill-mode-string ()
  "Normalize string form."
  (should (eq (supervisor--normalize-kill-mode "process") 'process)))

(ert-deftest supervisor-test-normalize-kill-mode-invalid ()
  "Return nil for invalid kill mode."
  (should-not (supervisor--normalize-kill-mode 'group)))

;; Success-exit-status normalization

(ert-deftest supervisor-test-normalize-success-exit-status-nil ()
  "Return nil for nil input."
  (should-not (supervisor--normalize-success-exit-status nil)))

(ert-deftest supervisor-test-normalize-success-exit-status-int ()
  "Normalize single integer."
  (let ((result (supervisor--normalize-success-exit-status 42)))
    (should (equal (plist-get result :codes) '(42)))
    (should (equal (plist-get result :signals) nil))))

(ert-deftest supervisor-test-normalize-success-exit-status-signal ()
  "Normalize single signal."
  (let ((result (supervisor--normalize-success-exit-status 'TERM)))
    (should (equal (plist-get result :codes) nil))
    (should (equal (plist-get result :signals) '(SIGTERM)))))

(ert-deftest supervisor-test-normalize-success-exit-status-list ()
  "Normalize mixed list of ints and signals."
  (let ((result (supervisor--normalize-success-exit-status '(0 42 SIGHUP "term"))))
    (should (equal (plist-get result :codes) '(0 42)))
    (should (equal (plist-get result :signals) '(SIGHUP SIGTERM)))))

(ert-deftest supervisor-test-normalize-success-exit-status-dedup ()
  "Deduplicate codes and signals."
  (let ((result (supervisor--normalize-success-exit-status '(1 1 TERM SIGTERM))))
    (should (equal (plist-get result :codes) '(1)))
    (should (equal (plist-get result :signals) '(SIGTERM)))))

;; Deduplicate-stable

(ert-deftest supervisor-test-deduplicate-stable ()
  "Deduplicate preserving first occurrence."
  (should (equal (supervisor--deduplicate-stable '("a" "b" "a" "c" "b"))
                 '("a" "b" "c"))))

(ert-deftest supervisor-test-deduplicate-stable-empty ()
  "Empty list returns empty."
  (should (equal (supervisor--deduplicate-stable nil) nil)))

;; Parse-entry with PT3 fields

(ert-deftest supervisor-test-parse-entry-description ()
  "Parse entry with :description."
  (let ((entry (supervisor--parse-entry '("cmd" :description "my service"))))
    (should (equal (supervisor-entry-description entry) "my service"))))

(ert-deftest supervisor-test-parse-entry-documentation-string ()
  "Parse entry with :documentation as string, normalized to list."
  (let ((entry (supervisor--parse-entry '("cmd" :documentation "man:foo(1)"))))
    (should (equal (supervisor-entry-documentation entry) '("man:foo(1)")))))

(ert-deftest supervisor-test-parse-entry-documentation-list ()
  "Parse entry with :documentation as list."
  (let ((entry (supervisor--parse-entry
                '("cmd" :documentation ("man:foo(1)" "https://example.com")))))
    (should (equal (supervisor-entry-documentation entry)
                   '("man:foo(1)" "https://example.com")))))

(ert-deftest supervisor-test-parse-entry-documentation-dedup ()
  "Parse entry with :documentation deduplicates."
  (let ((entry (supervisor--parse-entry
                '("cmd" :documentation ("man:foo(1)" "man:foo(1)")))))
    (should (equal (supervisor-entry-documentation entry)
                   '("man:foo(1)")))))

(ert-deftest supervisor-test-parse-entry-before ()
  "Parse entry with :before."
  (let ((entry (supervisor--parse-entry '("cmd" :before "other"))))
    (should (equal (supervisor-entry-before entry) '("other")))))

(ert-deftest supervisor-test-parse-entry-before-list ()
  "Parse entry with :before as list."
  (let ((entry (supervisor--parse-entry '("cmd" :before ("a" "b")))))
    (should (equal (supervisor-entry-before entry) '("a" "b")))))

(ert-deftest supervisor-test-parse-entry-before-dedup ()
  "Parse entry with :before deduplicates."
  (let ((entry (supervisor--parse-entry '("cmd" :before ("a" "a" "b")))))
    (should (equal (supervisor-entry-before entry) '("a" "b")))))

(ert-deftest supervisor-test-parse-entry-wants ()
  "Parse entry with :wants."
  (let ((entry (supervisor--parse-entry '("cmd" :wants "dep"))))
    (should (equal (supervisor-entry-wants entry) '("dep")))))

(ert-deftest supervisor-test-parse-entry-wants-list ()
  "Parse entry with :wants as list."
  (let ((entry (supervisor--parse-entry '("cmd" :wants ("a" "b")))))
    (should (equal (supervisor-entry-wants entry) '("a" "b")))))

(ert-deftest supervisor-test-parse-entry-kill-signal ()
  "Parse entry with :kill-signal."
  (let ((entry (supervisor--parse-entry '("cmd" :kill-signal SIGTERM))))
    (should (eq (supervisor-entry-kill-signal entry) 'SIGTERM))))

(ert-deftest supervisor-test-parse-entry-kill-signal-short ()
  "Parse entry with :kill-signal short form."
  (let ((entry (supervisor--parse-entry '("cmd" :kill-signal HUP))))
    (should (eq (supervisor-entry-kill-signal entry) 'SIGHUP))))

(ert-deftest supervisor-test-parse-entry-kill-mode ()
  "Parse entry with :kill-mode."
  (let ((entry (supervisor--parse-entry '("cmd" :kill-mode mixed))))
    (should (eq (supervisor-entry-kill-mode entry) 'mixed))))

(ert-deftest supervisor-test-parse-entry-remain-after-exit ()
  "Parse entry with :remain-after-exit."
  (let ((entry (supervisor--parse-entry
                '("cmd" :type oneshot :remain-after-exit t))))
    (should (eq (supervisor-entry-remain-after-exit entry) t))))

(ert-deftest supervisor-test-parse-entry-success-exit-status ()
  "Parse entry with :success-exit-status."
  (let ((entry (supervisor--parse-entry
                '("cmd" :success-exit-status (42 SIGHUP)))))
    (should (equal (plist-get (supervisor-entry-success-exit-status entry) :codes)
                   '(42)))
    (should (equal (plist-get (supervisor-entry-success-exit-status entry) :signals)
                   '(SIGHUP)))))

(ert-deftest supervisor-test-parse-entry-pt3-defaults ()
  "All PT3 fields default to nil for string entry."
  (let ((entry (supervisor--parse-entry "sleep 300")))
    (should-not (supervisor-entry-description entry))
    (should-not (supervisor-entry-documentation entry))
    (should-not (supervisor-entry-before entry))
    (should-not (supervisor-entry-wants entry))
    (should-not (supervisor-entry-kill-signal entry))
    (should-not (supervisor-entry-kill-mode entry))
    (should-not (supervisor-entry-remain-after-exit entry))
    (should-not (supervisor-entry-success-exit-status entry))))

(ert-deftest supervisor-test-parse-entry-33-elements ()
  "Parse entry returns 39 elements."
  (let ((entry (supervisor--parse-entry "sleep 300")))
    (should (= (length entry) 39))))

;; Validation tests for PT3 keys

(ert-deftest supervisor-test-validate-description-valid ()
  "Valid :description passes validation."
  (should-not (supervisor--validate-entry '("cmd" :description "my service"))))

(ert-deftest supervisor-test-validate-description-nil ()
  "Nil :description passes validation."
  (should-not (supervisor--validate-entry '("cmd" :description nil))))

(ert-deftest supervisor-test-validate-description-invalid ()
  "Non-string :description fails validation."
  (should (string-match-p ":description must be"
                          (supervisor--validate-entry '("cmd" :description 42)))))

(ert-deftest supervisor-test-validate-documentation-valid-string ()
  "String :documentation passes validation."
  (should-not (supervisor--validate-entry '("cmd" :documentation "man:foo(1)"))))

(ert-deftest supervisor-test-validate-documentation-valid-list ()
  "List :documentation passes validation."
  (should-not (supervisor--validate-entry
               '("cmd" :documentation ("man:foo(1)" "https://example.com")))))

(ert-deftest supervisor-test-validate-documentation-invalid ()
  "Invalid :documentation fails validation."
  (should (string-match-p ":documentation must be"
                          (supervisor--validate-entry '("cmd" :documentation 42)))))

(ert-deftest supervisor-test-validate-before-valid ()
  "Valid :before passes."
  (should-not (supervisor--validate-entry '("cmd" :before "other"))))

(ert-deftest supervisor-test-validate-before-valid-list ()
  "List :before passes."
  (should-not (supervisor--validate-entry '("cmd" :before ("a" "b")))))

(ert-deftest supervisor-test-validate-before-invalid ()
  "Invalid :before fails."
  (should (string-match-p ":before must be"
                          (supervisor--validate-entry '("cmd" :before 42)))))

(ert-deftest supervisor-test-validate-wants-valid ()
  "Valid :wants passes."
  (should-not (supervisor--validate-entry '("cmd" :wants "dep"))))

(ert-deftest supervisor-test-validate-wants-invalid ()
  "Invalid :wants fails."
  (should (string-match-p ":wants must be"
                          (supervisor--validate-entry '("cmd" :wants 42)))))

(ert-deftest supervisor-test-validate-kill-signal-valid ()
  "Valid :kill-signal passes."
  (should-not (supervisor--validate-entry '("cmd" :kill-signal SIGTERM))))

(ert-deftest supervisor-test-validate-kill-signal-short ()
  "Short form :kill-signal passes."
  (should-not (supervisor--validate-entry '("cmd" :kill-signal HUP))))

(ert-deftest supervisor-test-validate-kill-signal-invalid ()
  "Invalid :kill-signal fails."
  (should (string-match-p ":kill-signal must be"
                          (supervisor--validate-entry '("cmd" :kill-signal SIGFAKE)))))

(ert-deftest supervisor-test-validate-kill-mode-valid ()
  "Valid :kill-mode passes."
  (should-not (supervisor--validate-entry '("cmd" :kill-mode process))))

(ert-deftest supervisor-test-validate-kill-mode-mixed ()
  "Mixed :kill-mode passes."
  (should-not (supervisor--validate-entry '("cmd" :kill-mode mixed))))

(ert-deftest supervisor-test-validate-kill-mode-invalid ()
  "Invalid :kill-mode fails."
  (should (string-match-p ":kill-mode must be"
                          (supervisor--validate-entry '("cmd" :kill-mode group)))))

(ert-deftest supervisor-test-validate-remain-after-exit-valid ()
  "Valid :remain-after-exit passes."
  (should-not (supervisor--validate-entry
               '("cmd" :type oneshot :remain-after-exit t))))

(ert-deftest supervisor-test-validate-remain-after-exit-nil ()
  "Nil :remain-after-exit passes."
  (should-not (supervisor--validate-entry
               '("cmd" :type oneshot :remain-after-exit nil))))

(ert-deftest supervisor-test-validate-remain-after-exit-invalid ()
  "Non-boolean :remain-after-exit fails."
  (should (string-match-p ":remain-after-exit must be"
                          (supervisor--validate-entry
                           '("cmd" :type oneshot :remain-after-exit 1)))))

(ert-deftest supervisor-test-validate-remain-after-exit-simple-invalid ()
  "Remain-after-exit on simple type fails."
  (should (string-match-p ":remain-after-exit is invalid for :type simple"
                          (supervisor--validate-entry
                           '("cmd" :type simple :remain-after-exit t)))))

(ert-deftest supervisor-test-validate-success-exit-status-valid-int ()
  "Valid int :success-exit-status passes."
  (should-not (supervisor--validate-entry '("cmd" :success-exit-status 42))))

(ert-deftest supervisor-test-validate-success-exit-status-valid-signal ()
  "Valid signal :success-exit-status passes."
  (should-not (supervisor--validate-entry '("cmd" :success-exit-status SIGHUP))))

(ert-deftest supervisor-test-validate-success-exit-status-valid-list ()
  "Valid list :success-exit-status passes."
  (should-not (supervisor--validate-entry
               '("cmd" :success-exit-status (42 SIGHUP)))))

(ert-deftest supervisor-test-validate-success-exit-status-invalid ()
  "Non-int/signal :success-exit-status item fails."
  (should (string-match-p ":success-exit-status item must be"
                          (supervisor--validate-entry
                           '("cmd" :success-exit-status (42 3.14))))))

(ert-deftest supervisor-test-validate-success-exit-status-unknown-signal ()
  "Unknown signal name in :success-exit-status fails."
  (should (string-match-p "unknown signal SIGFAKE"
                          (supervisor--validate-entry
                           '("cmd" :success-exit-status SIGFAKE)))))

(ert-deftest supervisor-test-validate-success-exit-status-oneshot-invalid ()
  "Success-exit-status on oneshot type fails."
  (should (string-match-p ":success-exit-status is invalid for :type oneshot"
                          (supervisor--validate-entry
                           '("cmd" :type oneshot :success-exit-status 42)))))

;; Entry-to-service and service-to-entry roundtrip with PT3 fields

(ert-deftest supervisor-test-entry-service-roundtrip-pt3 ()
  "Round-trip entry->service->entry preserves PT3 fields."
  (let* ((entry (supervisor--parse-entry
                 '("cmd" :description "test svc"
                   :documentation ("man:foo(1)")
                   :before ("b")
                   :wants ("w")
                   :kill-signal SIGTERM
                   :kill-mode mixed
                   :success-exit-status (42 SIGHUP))))
         (service (supervisor-entry-to-service entry))
         (roundtripped (supervisor-service-to-entry service)))
    (should (equal (supervisor-entry-description roundtripped) "test svc"))
    (should (equal (supervisor-entry-documentation roundtripped) '("man:foo(1)")))
    (should (equal (supervisor-entry-before roundtripped) '("b")))
    (should (equal (supervisor-entry-wants roundtripped) '("w")))
    (should (eq (supervisor-entry-kill-signal roundtripped) 'SIGTERM))
    (should (eq (supervisor-entry-kill-mode roundtripped) 'mixed))
    (should (equal (plist-get (supervisor-entry-success-exit-status roundtripped) :codes)
                   '(42)))
    (should (equal (plist-get (supervisor-entry-success-exit-status roundtripped) :signals)
                   '(SIGHUP)))))

(ert-deftest supervisor-test-entry-service-roundtrip-pt3-remain ()
  "Round-trip preserves remain-after-exit for oneshot."
  (let* ((entry (supervisor--parse-entry
                 '("cmd" :type oneshot :remain-after-exit t)))
         (service (supervisor-entry-to-service entry))
         (roundtripped (supervisor-service-to-entry service)))
    (should (eq (supervisor-entry-remain-after-exit roundtripped) t))))

;; Unit-file keyword whitelist includes PT3 keys

(ert-deftest supervisor-test-unit-file-keywords-pt3 ()
  "Unit-file keywords include all PT3 keys."
  (dolist (kw '(:description :documentation :before :wants
                :kill-signal :kill-mode :remain-after-exit :success-exit-status))
    (should (memq kw supervisor--unit-file-keywords))))

;; Core keyword whitelist includes PT3 keys

(ert-deftest supervisor-test-valid-keywords-pt3 ()
  "Valid keywords include all PT3 keys."
  (dolist (kw '(:description :documentation :before :wants
                :kill-signal :kill-mode :remain-after-exit :success-exit-status))
    (should (memq kw supervisor--valid-keywords))))

;; Unit-file and core keyword whitelists include :user/:group

(ert-deftest supervisor-test-unit-file-keywords-user-group ()
  "Unit-file keywords include :user and :group."
  (should (memq :user supervisor--unit-file-keywords))
  (should (memq :group supervisor--unit-file-keywords)))

(ert-deftest supervisor-test-valid-keywords-user-group ()
  "Core valid keywords include :user and :group."
  (should (memq :user supervisor--valid-keywords))
  (should (memq :group supervisor--valid-keywords)))

(ert-deftest supervisor-test-unit-file-user-group-accepted ()
  "Unit file with :user/:group passes validation."
  (supervisor-test-with-unit-files
      '(("echo hi" :id "svc" :user "alice" :group "staff"))
    (let* ((entries (supervisor--all-parsed-entries)))
      (should (= 1 (length entries)))
      (let ((entry (car entries)))
        (should (equal (supervisor-entry-user entry) "alice"))
        (should (equal (supervisor-entry-group entry) "staff"))))))

;; Unit-file and core keyword whitelists include sandbox keys

(ert-deftest supervisor-test-unit-file-keywords-sandbox ()
  "Unit-file keywords include all sandbox keys."
  (dolist (kw '(:sandbox-profile :sandbox-network :sandbox-ro-bind
                :sandbox-rw-bind :sandbox-tmpfs :sandbox-raw-args))
    (should (memq kw supervisor--unit-file-keywords))))

(ert-deftest supervisor-test-unit-file-sandbox-profile-accepted ()
  "Unit file with :sandbox-profile passes validation and roundtrips."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (name) (when (equal name "bwrap") "/usr/bin/bwrap"))))
    (supervisor-test-with-unit-files
        '(("echo hi" :id "svc" :sandbox-profile strict))
      (let* ((entries (supervisor--all-parsed-entries)))
        (should (= 1 (length entries)))
        (should (eq (supervisor-entry-sandbox-profile (car entries))
                    'strict))))))

(ert-deftest supervisor-test-unit-file-sandbox-ro-bind-accepted ()
  "Unit file with :sandbox-ro-bind passes validation and roundtrips."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (name) (when (equal name "bwrap") "/usr/bin/bwrap"))))
    (supervisor-test-with-unit-files
        '(("echo hi" :id "svc" :sandbox-ro-bind ("/tmp")))
      (let* ((entries (supervisor--all-parsed-entries)))
        (should (= 1 (length entries)))
        (should (equal (supervisor-entry-sandbox-ro-bind (car entries))
                       '("/tmp")))))))

;; Type-gating

(ert-deftest supervisor-test-remain-after-exit-oneshot-only ()
  "Remain-after-exit is in oneshot-only keywords."
  (should (memq :remain-after-exit supervisor--oneshot-only-keywords)))

(ert-deftest supervisor-test-success-exit-status-simple-only ()
  "Success-exit-status is in simple-only keywords."
  (should (memq :success-exit-status supervisor--simple-only-keywords)))

;; Scaffold includes PT3 keys

(ert-deftest supervisor-test-scaffold-pt3-keys ()
  "Scaffold template mentions PT3 keywords."
  (let ((scaffold (supervisor--unit-file-scaffold "test-id")))
    (dolist (kw '(":description" ":documentation" ":before" ":wants"
                  ":kill-signal" ":kill-mode" ":remain-after-exit"
                  ":success-exit-status"))
      (should (string-match-p kw scaffold)))))

;;;; Phase N2: Metadata Surface Tests

(ert-deftest supervisor-test-cli-entry-info-description ()
  "Entry info includes :description."
  (let ((entry (supervisor--parse-entry
                '("cmd" :id "svc" :description "My service"))))
    (let ((info (supervisor--cli-entry-info entry)))
      (should (equal (alist-get 'description info) "My service")))))

(ert-deftest supervisor-test-cli-entry-info-documentation ()
  "Entry info includes :documentation."
  (let ((entry (supervisor--parse-entry
                '("cmd" :id "svc" :documentation ("man:foo(1)")))))
    (let ((info (supervisor--cli-entry-info entry)))
      (should (equal (alist-get 'documentation info) '("man:foo(1)"))))))

(ert-deftest supervisor-test-cli-entry-info-description-nil ()
  "Entry info description is nil when not set."
  (let ((entry (supervisor--parse-entry '("cmd" :id "svc"))))
    (let ((info (supervisor--cli-entry-info entry)))
      (should-not (alist-get 'description info)))))

(ert-deftest supervisor-test-cli-describe-human-description ()
  "Human describe includes description line."
  (let* ((entry (supervisor--parse-entry
                 '("cmd" :id "svc" :description "My service")))
         (info (supervisor--cli-entry-info entry))
         (output (supervisor--cli-describe-human info)))
    (should (string-match-p "Description: My service" output))))

(ert-deftest supervisor-test-cli-describe-human-documentation ()
  "Human describe includes documentation line."
  (let* ((entry (supervisor--parse-entry
                 '("cmd" :id "svc" :documentation ("man:foo(1)" "https://x.com"))))
         (info (supervisor--cli-entry-info entry))
         (output (supervisor--cli-describe-human info)))
    (should (string-match-p "Documentation: man:foo(1), https://x.com" output))))

(ert-deftest supervisor-test-cli-describe-human-no-desc ()
  "Human describe omits description line when nil."
  (let* ((entry (supervisor--parse-entry '("cmd" :id "svc")))
         (info (supervisor--cli-entry-info entry))
         (output (supervisor--cli-describe-human info)))
    (should-not (string-match-p "Description:" output))))

(ert-deftest supervisor-test-cli-json-description ()
  "JSON output includes description."
  (let* ((entry (supervisor--parse-entry
                 '("cmd" :id "svc" :description "My service")))
         (info (supervisor--cli-entry-info entry))
         (json (supervisor--cli-entry-to-json-obj info)))
    (should (equal (alist-get 'description json) "My service"))))

(ert-deftest supervisor-test-cli-json-documentation ()
  "JSON output includes documentation array."
  (let* ((entry (supervisor--parse-entry
                 '("cmd" :id "svc" :documentation ("man:foo(1)"))))
         (info (supervisor--cli-entry-info entry))
         (json (supervisor--cli-entry-to-json-obj info)))
    (should (equal (alist-get 'documentation json) '("man:foo(1)")))))

(ert-deftest supervisor-test-cli-json-documentation-empty ()
  "JSON output uses empty array when no documentation."
  (let* ((entry (supervisor--parse-entry '("cmd" :id "svc")))
         (info (supervisor--cli-entry-info entry))
         (json (supervisor--cli-entry-to-json-obj info)))
    (should (equal (alist-get 'documentation json) []))))

;; CLI dispatcher integration tests for metadata

(ert-deftest supervisor-test-cli-show-includes-description ()
  "The `show ID' output includes Description when set."
  (supervisor-test-with-unit-files
      '(("cmd" :id "svc" :description "My test service"))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("show" "svc"))))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      (should (string-match-p "Description: My test service"
                              (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-show-includes-documentation ()
  "The `show ID' output includes Documentation when set."
  (supervisor-test-with-unit-files
      '(("cmd" :id "svc" :documentation ("man:svc(1)" "https://example.com")))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("show" "svc"))))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      (should (string-match-p "Documentation: man:svc(1), https://example.com"
                              (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-show-omits-description-when-nil ()
  "The `show ID' output omits Description line when not set."
  (supervisor-test-with-unit-files
      '(("cmd" :id "svc"))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("show" "svc"))))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      (should-not (string-match-p "Description:"
                                  (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-show-json-includes-metadata ()
  "The `show --json ID' includes description and documentation."
  (supervisor-test-with-unit-files
      '(("cmd" :id "svc" :description "Test"
         :documentation ("man:svc(1)")))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("show" "svc" "--json"))))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      (let ((parsed (json-read-from-string (supervisor-cli-result-output result))))
        (should (equal "Test" (alist-get 'description parsed)))
        (should (equal ["man:svc(1)"] (alist-get 'documentation parsed)))))))

(ert-deftest supervisor-test-cli-status-id-includes-description ()
  "The `status ID' detail view includes Description."
  (supervisor-test-with-unit-files
      '(("cmd" :id "svc" :description "Status desc test"
         :documentation ("man:svc(1)" "https://example.com")))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("status" "svc"))))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      (should (string-match-p "Description: Status desc test"
                              (supervisor-cli-result-output result)))
      (should (string-match-p "Documentation: man:svc(1), https://example.com"
                              (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-status-id-json-includes-metadata ()
  "The `status ID --json' includes description and documentation."
  (supervisor-test-with-unit-files
      '(("cmd" :id "svc" :description "JSON meta"
         :documentation ("man:svc(1)")))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("status" "svc" "--json"))))
      (should (= supervisor-cli-exit-success (supervisor-cli-result-exitcode result)))
      (let* ((parsed (json-read-from-string (supervisor-cli-result-output result)))
             (entry (aref (alist-get 'entries parsed) 0)))
        (should (equal "JSON meta" (alist-get 'description entry)))
        (should (equal ["man:svc(1)"] (alist-get 'documentation entry)))))))

;; Dashboard describe-entry integration test

(ert-deftest supervisor-test-dashboard-describe-shows-metadata ()
  "Dashboard describe-entry includes description and docs in detail view."
  (let* ((entry (supervisor--parse-entry
                 '("cmd" :id "svc" :description "Dashboard desc"
                   :documentation ("man:svc(1)"))))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--failed (make-hash-table :test 'equal))
         (supervisor--restart-override (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (supervisor--manually-started (make-hash-table :test 'equal))
         (supervisor--oneshot-completed (make-hash-table :test 'equal))
         (supervisor--remain-active (make-hash-table :test 'equal))
         (supervisor--last-exit-info (make-hash-table :test 'equal))
         (supervisor--start-times (make-hash-table :test 'equal))
         (supervisor--ready-times (make-hash-table :test 'equal))
         (supervisor--restart-times (make-hash-table :test 'equal))
         (supervisor--restart-timers (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal))
         (supervisor--logging-override (make-hash-table :test 'equal))
         (output nil))
    ;; describe-entry-detail uses with-help-window; read the buffer
    (cl-letf (((symbol-function 'supervisor--unit-file-path)
               (lambda (_id) nil))
              ((symbol-function 'supervisor--telemetry-log-tail)
               (lambda (_id &optional _lines) nil)))
      (supervisor--describe-entry-detail "svc" entry)
      (let ((info-buf (get-buffer "*supervisor-info*")))
        (unwind-protect
            (progn
              (should info-buf)
              (setq output (with-current-buffer info-buf
                             (buffer-string)))
              (should (string-match-p "Dashboard desc" output))
              (should (string-match-p "man:svc(1)" output)))
          (when info-buf (kill-buffer info-buf)))))))

;;;; Phase N3: Ordering and Soft Dependencies Tests

;; :before inversion parity with :after

(ert-deftest supervisor-test-before-inversion-basic ()
  "A :before B produces same ordering as B :after A."
  (let* ((supervisor--authority-snapshot nil)
         ;; A should start before B
         (programs-before '(("cmd-a" :id "a" :before ("b"))
                            ("cmd-b" :id "b")))
         (programs-after '(("cmd-a" :id "a")
                           ("cmd-b" :id "b" :after ("a"))))
         (plan-before (supervisor--build-plan programs-before))
         (plan-after (supervisor--build-plan programs-after))
         (ids-before (mapcar #'supervisor-entry-id
                             (supervisor-plan-entries plan-before)))
         (ids-after (mapcar #'supervisor-entry-id
                            (supervisor-plan-entries plan-after))))
    (should (equal ids-before ids-after))))

(ert-deftest supervisor-test-before-inversion-multiple ()
  "A :before (B C) creates edges for both."
  (let* ((supervisor--authority-snapshot nil)
         (programs '(("cmd-a" :id "a" :before ("b" "c"))
                     ("cmd-b" :id "b")
                     ("cmd-c" :id "c")))
         (plan (supervisor--build-plan programs))
         (sorted (supervisor-plan-by-target plan))
         (ids (mapcar #'supervisor-entry-id sorted)))
    ;; a must come before b and c
    (should (< (cl-position "a" ids :test #'equal)
               (cl-position "b" ids :test #'equal)))
    (should (< (cl-position "a" ids :test #'equal)
               (cl-position "c" ids :test #'equal)))))

(ert-deftest supervisor-test-before-ordering ()
  "The :before directive creates ordering edge."
  (let* ((supervisor--authority-snapshot nil)
         (programs '(("cmd-a" :id "a" :before ("b"))
                     ("cmd-b" :id "b")))
         (plan (supervisor--build-plan programs))
         (sorted (supervisor-plan-by-target plan))
         (ids (mapcar #'supervisor-entry-id sorted)))
    ;; a :before b → a comes before b
    (should (< (cl-position "a" ids :test #'equal)
               (cl-position "b" ids :test #'equal)))))

(ert-deftest supervisor-test-before-missing-target-ignored ()
  "Missing :before target is warned and ignored."
  (let* ((supervisor--authority-snapshot nil)
         (logged nil)
         (programs '(("cmd-a" :id "a" :before ("nonexistent")))))
    (cl-letf (((symbol-function 'supervisor--log)
               (lambda (_level fmt &rest args)
                 (push (apply #'format fmt args) logged))))
      (supervisor--build-plan programs)
      ;; Should have logged a warning about nonexistent
      (should (cl-some (lambda (msg)
                         (string-match-p ":before.*does not exist" msg))
                       logged)))))

(ert-deftest supervisor-test-before-combined-with-after ()
  "Both :before and :after edges combine correctly."
  (let* ((supervisor--authority-snapshot nil)
         ;; a :before c, b :after a → order: a, b, c
         (programs '(("cmd-a" :id "a" :before ("c"))
                     ("cmd-b" :id "b" :after ("a"))
                     ("cmd-c" :id "c")))
         (plan (supervisor--build-plan programs))
         (sorted (supervisor-plan-by-target plan))
         (ids (mapcar #'supervisor-entry-id sorted)))
    (should (< (cl-position "a" ids :test #'equal)
               (cl-position "b" ids :test #'equal)))
    (should (< (cl-position "a" ids :test #'equal)
               (cl-position "c" ids :test #'equal)))))

;; :before and :after cycle behavior

(ert-deftest supervisor-test-before-after-cycle-fallback ()
  "Cycle involving :before falls back correctly."
  (let* ((supervisor--authority-snapshot nil)
         ;; a :before b, b :before a → cycle
         (programs '(("cmd-a" :id "a" :before ("b"))
                     ("cmd-b" :id "b" :before ("a"))))
         (plan (supervisor--build-plan programs)))
    ;; Both should be in cycle-fallback
    (should (gethash "a" (supervisor-plan-cycle-fallback-ids plan)))
    (should (gethash "b" (supervisor-plan-cycle-fallback-ids plan)))))

;; :wants soft dependency tests

(ert-deftest supervisor-test-wants-ordering ()
  "Wants creates ordering preference when target exists."
  (let* ((supervisor--authority-snapshot nil)
         (programs '(("cmd-a" :id "a")
                     ("cmd-b" :id "b" :wants ("a"))))
         (plan (supervisor--build-plan programs))
         (sorted (supervisor-plan-by-target plan))
         (ids (mapcar #'supervisor-entry-id sorted)))
    ;; a should come before b
    (should (< (cl-position "a" ids :test #'equal)
               (cl-position "b" ids :test #'equal)))))

(ert-deftest supervisor-test-wants-missing-silently-dropped ()
  "Missing :wants target is silently dropped (no warning)."
  (let* ((supervisor--authority-snapshot nil)
         (logged nil)
         (programs '(("cmd-a" :id "a" :wants ("nonexistent")))))
    (cl-letf (((symbol-function 'supervisor--log)
               (lambda (_level fmt &rest args)
                 (push (apply #'format fmt args) logged))))
      (let ((plan (supervisor--build-plan programs)))
        ;; Entry should still be valid
        (should (= 1 (length (supervisor-plan-entries plan))))
        ;; No warning about the missing wanted unit
        (should-not (cl-some (lambda (msg)
                               (string-match-p "nonexistent" msg))
                             logged))))))

(ert-deftest supervisor-test-wants-ordering-position ()
  "The :wants directive creates ordering edge."
  (let* ((supervisor--authority-snapshot nil)
         (programs '(("cmd-a" :id "a")
                     ("cmd-b" :id "b" :wants ("a"))))
         (plan (supervisor--build-plan programs))
         (sorted (supervisor-plan-by-target plan))
         (ids (mapcar #'supervisor-entry-id sorted)))
    ;; a should come before b
    (should (< (cl-position "a" ids :test #'equal)
               (cl-position "b" ids :test #'equal)))))

(ert-deftest supervisor-test-wants-disabled-not-blocking ()
  "Disabled :wants target does not block the wanting unit."
  (let* ((supervisor--authority-snapshot nil)
         (programs '(("cmd-a" :id "a" :disabled t)
                     ("cmd-b" :id "b" :wants ("a"))))
         (plan (supervisor--build-plan programs))
         (sorted (supervisor-plan-by-target plan)))
    ;; Both entries should be in the plan
    (should (= 2 (length sorted)))))

(ert-deftest supervisor-test-wants-participates-in-cycle-detection ()
  "Wants edges participate in cycle detection."
  (let* ((supervisor--authority-snapshot nil)
         ;; a :wants b, b :wants a → cycle
         (programs '(("cmd-a" :id "a" :wants ("b"))
                     ("cmd-b" :id "b" :wants ("a"))))
         (plan (supervisor--build-plan programs)))
    ;; Both should be in cycle-fallback
    (should (gethash "a" (supervisor-plan-cycle-fallback-ids plan)))
    (should (gethash "b" (supervisor-plan-cycle-fallback-ids plan)))))

(ert-deftest supervisor-test-wants-combined-with-after ()
  "Wants and after edges combine for ordering."
  (let* ((supervisor--authority-snapshot nil)
         ;; c wants a, c :after b → order: a before c, b before c
         (programs '(("cmd-a" :id "a")
                     ("cmd-b" :id "b")
                     ("cmd-c" :id "c" :wants ("a") :after ("b"))))
         (plan (supervisor--build-plan programs))
         (sorted (supervisor-plan-by-target plan))
         (ids (mapcar #'supervisor-entry-id sorted)))
    (should (< (cl-position "a" ids :test #'equal)
               (cl-position "c" ids :test #'equal)))
    (should (< (cl-position "b" ids :test #'equal)
               (cl-position "c" ids :test #'equal)))))

;; DAG init with :wants

(ert-deftest supervisor-test-dag-init-includes-wants ()
  "DAG init includes valid wants in dependency graph."
  (let* ((entries (list (supervisor--parse-entry '("cmd-a" :id "a"))
                        (supervisor--parse-entry '("cmd-b" :id "b" :wants ("a")))))
         (supervisor--entry-state (make-hash-table :test 'equal)))
    (supervisor--dag-init entries)
    ;; b should depend on a (in-degree 1)
    (should (= 1 (gethash "b" supervisor--dag-in-degree)))
    ;; a should have b as dependent
    (should (member "b" (gethash "a" supervisor--dag-dependents)))))

(ert-deftest supervisor-test-dag-init-missing-wants-ignored ()
  "DAG init ignores wants for missing entries."
  (let* ((entries (list (supervisor--parse-entry
                         '("cmd-b" :id "b" :wants ("nonexistent")))))
         (supervisor--entry-state (make-hash-table :test 'equal)))
    (supervisor--dag-init entries)
    ;; b should have in-degree 0 (missing wants ignored)
    (should (= 0 (gethash "b" supervisor--dag-in-degree)))))

(ert-deftest supervisor-test-dag-init-masked-wants-ignored ()
  "DAG init ignores wants for masked entries."
  (let* ((entries (list (supervisor--parse-entry '("cmd-a" :id "a"))
                        (supervisor--parse-entry '("cmd-b" :id "b" :wants ("a")))))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal)))
    ;; Mask entry a
    (puthash "a" 'masked supervisor--mask-override)
    (supervisor--dag-init entries)
    ;; b should have in-degree 0 (masked wants ignored)
    (should (= 0 (gethash "b" supervisor--dag-in-degree)))))

(ert-deftest supervisor-test-wants-cycle-fallback-clears-wants ()
  "Cycle fallback clears :wants edges so runtime DAG has zero in-degree."
  (let* ((supervisor--authority-snapshot nil)
         (programs '(("cmd-a" :id "a" :wants ("b"))
                     ("cmd-b" :id "b" :wants ("a"))))
         (plan (supervisor--build-plan programs))
         (sorted (supervisor-plan-by-target plan)))
    ;; Cycle fallback should have cleared :wants
    (dolist (entry sorted)
      (should (null (supervisor-entry-wants entry))))
    ;; DAG init should produce zero in-degree for both
    (let ((supervisor--entry-state (make-hash-table :test 'equal))
          (supervisor--mask-override (make-hash-table :test 'equal)))
      (supervisor--dag-init sorted)
      (should (= 0 (gethash "a" supervisor--dag-in-degree)))
      (should (= 0 (gethash "b" supervisor--dag-in-degree))))))

(ert-deftest supervisor-test-stable-topo-cycle-clears-wants ()
  "Stable topo sort cycle fallback clears :wants."
  (let ((supervisor--computed-deps (make-hash-table :test 'equal))
        (supervisor--cycle-fallback-ids (make-hash-table :test 'equal)))
    ;; 26-element entries with :wants cycle
    (let* ((entries (list (list "a" "cmd" 0 t 'always t 'simple nil
                                t 30 nil nil nil nil nil nil nil nil
                                nil nil nil '("b") nil nil nil nil)
                          (list "b" "cmd" 0 t 'always t 'simple nil
                                t 30 nil nil nil nil nil nil nil nil
                                nil nil nil '("a") nil nil nil nil)))
           (sorted (supervisor--stable-topo-sort entries)))
      ;; :wants should be cleared
      (dolist (entry sorted)
        (should (null (supervisor-entry-wants entry)))))))

(ert-deftest supervisor-test-wants-failed-spawn-not-blocking ()
  "A wanted unit that fails to start does not block the wanting unit.
Spawn failure calls `supervisor--dag-mark-ready', which decrements
in-degree of dependents including :wants edges."
  (let* ((entries (list (supervisor--parse-entry '("cmd-a" :id "a"))
                        (supervisor--parse-entry '("cmd-b" :id "b" :wants ("a")))))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--dag-active-starts 0)
         (supervisor--dag-pending-starts nil)
         (supervisor--dag-complete-callback nil))
    (supervisor--dag-init entries)
    ;; b depends on a (in-degree 1)
    (should (= 1 (gethash "b" supervisor--dag-in-degree)))
    ;; Simulate a failing to start
    (supervisor--dag-handle-spawn-failure "a")
    ;; b should now be unblocked (in-degree 0)
    (should (= 0 (gethash "b" supervisor--dag-in-degree)))))

;;;; Phase N4: Kill Controls

(ert-deftest supervisor-test-kill-signal-for-id-default ()
  "Return SIGTERM when unit has no :kill-signal configured."
  (let* ((supervisor--programs-cache
          (list (list "sleep 300" :id "svc1")))
         (supervisor--invalid (make-hash-table :test 'equal)))
    (should (eq 'SIGTERM (supervisor--kill-signal-for-id "svc1")))))

(ert-deftest supervisor-test-kill-signal-for-id-configured ()
  "Return configured :kill-signal for unit."
  (let* ((supervisor--programs-cache
          (list (list "sleep 300" :id "svc1" :kill-signal "SIGINT")))
         (supervisor--invalid (make-hash-table :test 'equal)))
    (should (eq 'SIGINT (supervisor--kill-signal-for-id "svc1")))))

(ert-deftest supervisor-test-kill-signal-for-id-unknown ()
  "Return SIGTERM for unknown unit ID."
  (let* ((supervisor--programs-cache nil)
         (supervisor--invalid (make-hash-table :test 'equal)))
    (should (eq 'SIGTERM (supervisor--kill-signal-for-id "nonexistent")))))

(ert-deftest supervisor-test-kill-mode-for-id-default ()
  "Return `process' when unit has no :kill-mode configured."
  (let* ((supervisor--programs-cache
          (list (list "sleep 300" :id "svc1")))
         (supervisor--invalid (make-hash-table :test 'equal)))
    (should (eq 'process (supervisor--kill-mode-for-id "svc1")))))

(ert-deftest supervisor-test-kill-mode-for-id-mixed ()
  "Return `mixed' when unit has :kill-mode mixed."
  (let* ((supervisor--programs-cache
          (list (list "sleep 300" :id "svc1" :kill-mode 'mixed)))
         (supervisor--invalid (make-hash-table :test 'equal)))
    (should (eq 'mixed (supervisor--kill-mode-for-id "svc1")))))

(ert-deftest supervisor-test-kill-mode-for-id-unknown ()
  "Return `process' for unknown unit ID."
  (let* ((supervisor--programs-cache nil)
         (supervisor--invalid (make-hash-table :test 'equal)))
    (should (eq 'process (supervisor--kill-mode-for-id "nonexistent")))))

(ert-deftest supervisor-test-process-descendants-returns-children ()
  "Descendant discovery finds child PIDs via process-attributes."
  (cl-letf (((symbol-function 'list-system-processes)
             (lambda () '(1 10 20 30 40)))
            ((symbol-function 'process-attributes)
             (lambda (pid)
               (pcase pid
                 (10 '((ppid . 1)))
                 (20 '((ppid . 10)))
                 (30 '((ppid . 10)))
                 (40 '((ppid . 99)))
                 (_ nil)))))
    (let ((desc (supervisor--process-descendants 10)))
      (should (member 20 desc))
      (should (member 30 desc))
      (should-not (member 40 desc))
      (should-not (member 1 desc)))))

(ert-deftest supervisor-test-process-descendants-transitive ()
  "Descendant discovery finds grandchildren transitively."
  (cl-letf (((symbol-function 'list-system-processes)
             (lambda () '(1 10 20 30)))
            ((symbol-function 'process-attributes)
             (lambda (pid)
               (pcase pid
                 (10 '((ppid . 1)))
                 (20 '((ppid . 10)))
                 (30 '((ppid . 20)))
                 (_ nil)))))
    (let ((desc (supervisor--process-descendants 10)))
      (should (member 20 desc))
      (should (member 30 desc)))))

(ert-deftest supervisor-test-process-descendants-fallback-on-error ()
  "Descendant discovery returns nil and logs warning on error."
  (let ((warnings nil))
    (cl-letf (((symbol-function 'list-system-processes)
               (lambda () (error "Not supported")))
              ((symbol-function 'supervisor--log)
               (lambda (level fmt &rest args)
                 (when (eq level 'warning)
                   (push (apply #'format fmt args) warnings)))))
      (should-not (supervisor--process-descendants 1234))
      (should (= 1 (length warnings)))
      (should (string-match-p "descendant discovery failed" (car warnings))))))

(ert-deftest supervisor-test-kill-with-descendants-main-only ()
  "Kill with descendants sends SIGKILL to main process only.
No warning is emitted when there are simply no child processes."
  (let* ((killed-signals nil)
         (warnings nil)
         (proc (start-process "test-kill" nil "sleep" "300")))
    (unwind-protect
        (cl-letf (((symbol-function 'supervisor--process-descendants)
                   (lambda (_pid) nil))
                  ((symbol-function 'signal-process)
                   (lambda (p sig)
                     (push (list p sig) killed-signals)))
                  ((symbol-function 'supervisor--log)
                   (lambda (level _fmt &rest _args)
                     (when (eq level 'warning)
                       (push t warnings)))))
          (supervisor--kill-with-descendants proc)
          (should (= 1 (length killed-signals)))
          (should (eq 'SIGKILL (cadr (car killed-signals))))
          ;; No warning for normal "no children" case
          (should (= 0 (length warnings))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest supervisor-test-kill-with-descendants-mixed ()
  "Kill with descendants sends SIGKILL to main and descendant PIDs."
  (let* ((killed nil)
         (proc (start-process "test-kill-mixed" nil "sleep" "300")))
    (unwind-protect
        (cl-letf (((symbol-function 'supervisor--process-descendants)
                   (lambda (_pid) '(9991 9992)))
                  ((symbol-function 'signal-process)
                   (lambda (p sig)
                     (push (list p sig) killed)))
                  ((symbol-function 'supervisor--log)
                   (lambda (&rest _args))))
          (supervisor--kill-with-descendants proc)
          ;; Main process + 2 descendants = 3 SIGKILL signals
          (should (= 3 (length killed)))
          (should (cl-every (lambda (entry) (eq 'SIGKILL (cadr entry)))
                            killed)))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest supervisor-test-manual-stop-uses-kill-signal ()
  "Manual stop sends configured :kill-signal instead of default."
  (let* ((supervisor--processes (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (supervisor--manually-started (make-hash-table :test 'equal))
         (signaled-with nil)
         (proc (start-process "test-stop-sig" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "svc1" proc supervisor--processes)
          (cl-letf (((symbol-function 'supervisor--run-exec-stop-for-id)
                     (lambda (_id)))
                    ((symbol-function 'supervisor--kill-signal-for-id)
                     (lambda (_id) 'SIGUSR1))
                    ((symbol-function 'supervisor--kill-mode-for-id)
                     (lambda (_id) 'process))
                    ((symbol-function 'signal-process)
                     (lambda (_p sig)
                       (push sig signaled-with)))
                    ((symbol-function 'run-at-time)
                     (lambda (&rest _args))))
            (let ((result (supervisor--manual-stop "svc1")))
              (should (eq 'stopped (plist-get result :status)))
              ;; Should have sent SIGUSR1, not SIGTERM
              (should (memq 'SIGUSR1 signaled-with))
              (should-not (memq 'SIGTERM signaled-with)))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest supervisor-test-manual-stop-escalation-timer ()
  "Manual stop sets up SIGKILL escalation timer."
  (let* ((supervisor--processes (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (supervisor--manually-started (make-hash-table :test 'equal))
         (timer-set nil)
         (proc (start-process "test-esc" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "svc1" proc supervisor--processes)
          (cl-letf (((symbol-function 'supervisor--run-exec-stop-for-id)
                     (lambda (_id)))
                    ((symbol-function 'supervisor--kill-signal-for-id)
                     (lambda (_id) 'SIGTERM))
                    ((symbol-function 'supervisor--kill-mode-for-id)
                     (lambda (_id) 'process))
                    ((symbol-function 'signal-process)
                     (lambda (_p _sig)))
                    ((symbol-function 'run-at-time)
                     (lambda (timeout _repeat fn)
                       (setq timer-set
                             (list :timeout timeout :fn fn)))))
            (supervisor--manual-stop "svc1")
            ;; Escalation timer should be set
            (should timer-set)
            (should (= supervisor-shutdown-timeout
                       (plist-get timer-set :timeout)))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest supervisor-test-manual-stop-mixed-escalation ()
  "Manual stop with :kill-mode mixed uses kill-with-descendants on escalation."
  (let* ((supervisor--processes (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (supervisor--manually-started (make-hash-table :test 'equal))
         (escalation-fn nil)
         (kill-descendants-called nil)
         (proc (start-process "test-mixed" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "svc1" proc supervisor--processes)
          (cl-letf (((symbol-function 'supervisor--run-exec-stop-for-id)
                     (lambda (_id)))
                    ((symbol-function 'supervisor--kill-signal-for-id)
                     (lambda (_id) 'SIGTERM))
                    ((symbol-function 'supervisor--kill-mode-for-id)
                     (lambda (_id) 'mixed))
                    ((symbol-function 'signal-process)
                     (lambda (_p _sig)))
                    ((symbol-function 'run-at-time)
                     (lambda (_timeout _repeat fn)
                       (setq escalation-fn fn))))
            (supervisor--manual-stop "svc1")
            ;; Fire the escalation timer while process is still live
            (cl-letf (((symbol-function 'supervisor--kill-with-descendants)
                       (lambda (_proc)
                         (setq kill-descendants-called t))))
              (funcall escalation-fn)
              (should kill-descendants-called))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest supervisor-test-manual-kill-defaults-to-unit-signal ()
  "Manual kill without explicit signal uses unit's :kill-signal."
  (let* ((supervisor--processes (make-hash-table :test 'equal))
         (signaled-with nil)
         (proc (start-process "test-kill-def" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "svc1" proc supervisor--processes)
          (cl-letf (((symbol-function 'supervisor--kill-signal-for-id)
                     (lambda (_id) 'SIGINT))
                    ((symbol-function 'signal-process)
                     (lambda (_p sig)
                       (setq signaled-with sig))))
            (supervisor--manual-kill "svc1")
            (should (eq 'SIGINT signaled-with))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest supervisor-test-manual-kill-explicit-signal-overrides ()
  "Manual kill with explicit signal overrides unit's :kill-signal."
  (let* ((supervisor--processes (make-hash-table :test 'equal))
         (signaled-with nil)
         (proc (start-process "test-kill-exp" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "svc1" proc supervisor--processes)
          (cl-letf (((symbol-function 'supervisor--kill-signal-for-id)
                     (lambda (_id) 'SIGINT))
                    ((symbol-function 'signal-process)
                     (lambda (_p sig)
                       (setq signaled-with sig))))
            (supervisor--manual-kill "svc1" 'SIGUSR2)
            (should (eq 'SIGUSR2 signaled-with))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest supervisor-test-shutdown-uses-per-unit-kill-signal ()
  "Shutdown sends per-unit :kill-signal instead of blanket SIGTERM."
  (let* ((supervisor--processes (make-hash-table :test 'equal))
         (supervisor--timers nil)
         (supervisor--restart-timers (make-hash-table :test 'equal))
         (supervisor--shutting-down nil)
         (supervisor--shutdown-complete-flag nil)
         (supervisor--shutdown-remaining 0)
         (supervisor--shutdown-callback nil)
         (supervisor--shutdown-timer nil)
         (signals-sent nil)
         (proc1 (start-process "svc1" nil "sleep" "300"))
         (proc2 (start-process "svc2" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "svc1" proc1 supervisor--processes)
          (puthash "svc2" proc2 supervisor--processes)
          (cl-letf (((symbol-function 'supervisor--run-exec-stop-for-id)
                     (lambda (_id)))
                    ((symbol-function 'supervisor--dag-cleanup)
                     (lambda ()))
                    ((symbol-function 'supervisor--emit-event)
                     (lambda (&rest _args)))
                    ((symbol-function 'supervisor--kill-signal-for-id)
                     (lambda (id)
                       (if (string= id "svc1") 'SIGUSR1 'SIGINT)))
                    ((symbol-function 'signal-process)
                     (lambda (p sig)
                       (push (list (process-name p) sig) signals-sent)))
                    ((symbol-function 'run-at-time)
                     (lambda (&rest _args))))
            (supervisor-stop)
            ;; Each unit should get its own kill-signal
            (should (cl-find-if (lambda (e)
                                  (and (string= "svc1" (car e))
                                       (eq 'SIGUSR1 (cadr e))))
                                signals-sent))
            (should (cl-find-if (lambda (e)
                                  (and (string= "svc2" (car e))
                                       (eq 'SIGINT (cadr e))))
                                signals-sent))))
      (when (process-live-p proc1) (delete-process proc1))
      (when (process-live-p proc2) (delete-process proc2)))))

(ert-deftest supervisor-test-shutdown-escalation-respects-kill-mode ()
  "Shutdown SIGKILL escalation uses kill-with-descendants for mixed mode."
  (let* ((supervisor--processes (make-hash-table :test 'equal))
         (supervisor--timers nil)
         (supervisor--restart-timers (make-hash-table :test 'equal))
         (supervisor--shutting-down nil)
         (supervisor--shutdown-complete-flag nil)
         (supervisor--shutdown-remaining 0)
         (supervisor--shutdown-callback nil)
         (supervisor--shutdown-timer nil)
         (escalation-fn nil)
         (kill-desc-called nil)
         (plain-kill-called nil)
         (proc1 (start-process "mixed-svc" nil "sleep" "300"))
         (proc2 (start-process "normal-svc" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "mixed-svc" proc1 supervisor--processes)
          (puthash "normal-svc" proc2 supervisor--processes)
          (cl-letf (((symbol-function 'supervisor--run-exec-stop-for-id)
                     (lambda (_id)))
                    ((symbol-function 'supervisor--dag-cleanup)
                     (lambda ()))
                    ((symbol-function 'supervisor--emit-event)
                     (lambda (&rest _args)))
                    ((symbol-function 'supervisor--kill-signal-for-id)
                     (lambda (_id) 'SIGTERM))
                    ((symbol-function 'supervisor--kill-mode-for-id)
                     (lambda (id)
                       (if (string= id "mixed-svc") 'mixed 'process)))
                    ((symbol-function 'signal-process)
                     (lambda (_p _sig)))
                    ((symbol-function 'run-at-time)
                     (lambda (_timeout _repeat fn)
                       (setq escalation-fn fn))))
            (supervisor-stop)
            ;; Fire the escalation timer
            (cl-letf (((symbol-function 'supervisor--kill-with-descendants)
                       (lambda (_proc)
                         (setq kill-desc-called t)))
                      ((symbol-function 'signal-process)
                       (lambda (_p sig)
                         (when (eq sig 'SIGKILL)
                           (setq plain-kill-called t)))))
              (funcall escalation-fn)
              ;; mixed-svc should use kill-with-descendants
              (should kill-desc-called)
              ;; normal-svc should use plain SIGKILL
              (should plain-kill-called))))
      (when (process-live-p proc1) (delete-process proc1))
      (when (process-live-p proc2) (delete-process proc2)))))

(ert-deftest supervisor-test-kill-signal-real-process ()
  "Kill-signal SIGTERM terminates a real process."
  (let* ((proc (start-process "test-real-kill" nil "sleep" "300")))
    (unwind-protect
        (progn
          (should (process-live-p proc))
          (signal-process proc 'SIGTERM)
          ;; Wait briefly for process to die
          (let ((deadline (+ (float-time) 1.0)))
            (while (and (< (float-time) deadline)
                        (process-live-p proc))
              (sleep-for 0.05)))
          (should-not (process-live-p proc)))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest supervisor-test-manual-stop-not-running-skipped ()
  "Manual stop on non-running process returns skipped."
  (let* ((supervisor--processes (make-hash-table :test 'equal))
         (supervisor--manually-stopped (make-hash-table :test 'equal))
         (supervisor--manually-started (make-hash-table :test 'equal)))
    (let ((result (supervisor--manual-stop "nonexistent")))
      (should (eq 'skipped (plist-get result :status))))))

;;;; Phase N5: Oneshot Active-Latch (remain-after-exit)

(ert-deftest supervisor-test-remain-after-exit-success-active ()
  "Oneshot with :remain-after-exit t shows `active' status on success."
  (let ((supervisor--oneshot-completed (make-hash-table :test 'equal))
        (supervisor--remain-active (make-hash-table :test 'equal))
        (supervisor--processes (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--mask-override (make-hash-table :test 'equal)))
    ;; Simulate successful oneshot exit with remain-after-exit latch
    (puthash "svc1" 0 supervisor--oneshot-completed)
    (puthash "svc1" t supervisor--remain-active)
    (let ((result (supervisor--compute-entry-status "svc1" 'oneshot)))
      (should (string= "active" (car result)))
      (should (string= "exit:0" (cdr result))))))

(ert-deftest supervisor-test-remain-after-exit-failure-not-active ()
  "Oneshot with :remain-after-exit t shows `failed' on non-zero exit."
  (let ((supervisor--oneshot-completed (make-hash-table :test 'equal))
        (supervisor--remain-active (make-hash-table :test 'equal))
        (supervisor--processes (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--mask-override (make-hash-table :test 'equal)))
    ;; Failure: remain-active should NOT be set
    (puthash "svc1" 1 supervisor--oneshot-completed)
    (let ((result (supervisor--compute-entry-status "svc1" 'oneshot)))
      (should (string= "failed" (car result))))))

(ert-deftest supervisor-test-remain-after-exit-regular-oneshot-done ()
  "Regular oneshot without remain-after-exit still shows `done'."
  (let ((supervisor--oneshot-completed (make-hash-table :test 'equal))
        (supervisor--remain-active (make-hash-table :test 'equal))
        (supervisor--processes (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--mask-override (make-hash-table :test 'equal)))
    (puthash "svc1" 0 supervisor--oneshot-completed)
    ;; No remain-active entry
    (let ((result (supervisor--compute-entry-status "svc1" 'oneshot)))
      (should (string= "done" (car result))))))

(ert-deftest supervisor-test-remain-after-exit-handle-oneshot-exit ()
  "Handle-oneshot-exit sets remain-active on successful exit."
  (let ((supervisor--oneshot-completed (make-hash-table :test 'equal))
        (supervisor--remain-active (make-hash-table :test 'equal))
        (supervisor--oneshot-callbacks (make-hash-table :test 'equal))
        (supervisor--programs-cache
         (list (list "true" :id "svc1" :type 'oneshot
                     :remain-after-exit t)))
        (supervisor--invalid (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'supervisor--dag-mark-ready)
               (lambda (_id)))
              ((symbol-function 'supervisor--emit-event)
               (lambda (&rest _args)))
              ((symbol-function 'supervisor--log)
               (lambda (&rest _args))))
      (supervisor--handle-oneshot-exit "svc1" 'exit 0)
      (should (gethash "svc1" supervisor--remain-active))
      (should (= 0 (gethash "svc1" supervisor--oneshot-completed))))))

(ert-deftest supervisor-test-remain-after-exit-handle-failure ()
  "Handle-oneshot-exit does NOT set remain-active on failure."
  (let ((supervisor--oneshot-completed (make-hash-table :test 'equal))
        (supervisor--remain-active (make-hash-table :test 'equal))
        (supervisor--oneshot-callbacks (make-hash-table :test 'equal))
        (supervisor--programs-cache
         (list (list "false" :id "svc1" :type 'oneshot
                     :remain-after-exit t)))
        (supervisor--invalid (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'supervisor--dag-mark-ready)
               (lambda (_id)))
              ((symbol-function 'supervisor--emit-event)
               (lambda (&rest _args)))
              ((symbol-function 'supervisor--log)
               (lambda (&rest _args))))
      (supervisor--handle-oneshot-exit "svc1" 'exit 1)
      (should-not (gethash "svc1" supervisor--remain-active))
      (should (= 1 (gethash "svc1" supervisor--oneshot-completed))))))

(ert-deftest supervisor-test-remain-after-exit-handle-signal ()
  "Handle-oneshot-exit does NOT set remain-active on signal death."
  (let ((supervisor--oneshot-completed (make-hash-table :test 'equal))
        (supervisor--remain-active (make-hash-table :test 'equal))
        (supervisor--oneshot-callbacks (make-hash-table :test 'equal))
        (supervisor--programs-cache
         (list (list "true" :id "svc1" :type 'oneshot
                     :remain-after-exit t)))
        (supervisor--invalid (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'supervisor--dag-mark-ready)
               (lambda (_id)))
              ((symbol-function 'supervisor--emit-event)
               (lambda (&rest _args)))
              ((symbol-function 'supervisor--log)
               (lambda (&rest _args))))
      (supervisor--handle-oneshot-exit "svc1" 'signal 9)
      (should-not (gethash "svc1" supervisor--remain-active)))))

(ert-deftest supervisor-test-remain-after-exit-start-noop ()
  "Start on active remain-after-exit unit is a no-op."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--remain-active (make-hash-table :test 'equal))
        (supervisor--invalid (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--manually-stopped (make-hash-table :test 'equal))
        (supervisor--manually-started (make-hash-table :test 'equal))
        (supervisor--restart-times (make-hash-table :test 'equal))
        (supervisor--oneshot-completed (make-hash-table :test 'equal)))
    (puthash "svc1" t supervisor--remain-active)
    (let ((result (supervisor--manual-start "svc1")))
      (should (eq 'skipped (plist-get result :status)))
      (should (string= "already active" (plist-get result :reason)))
      ;; Latch should still be set
      (should (gethash "svc1" supervisor--remain-active)))))

(ert-deftest supervisor-test-remain-after-exit-stop-clears-latch ()
  "Stop on active remain-after-exit unit clears the latch."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--remain-active (make-hash-table :test 'equal))
        (supervisor--manually-stopped (make-hash-table :test 'equal))
        (supervisor--manually-started (make-hash-table :test 'equal)))
    (puthash "svc1" t supervisor--remain-active)
    (let ((result (supervisor--manual-stop "svc1")))
      (should (eq 'stopped (plist-get result :status)))
      ;; Latch should be cleared
      (should-not (gethash "svc1" supervisor--remain-active))
      ;; Manually-stopped should be set
      (should (gethash "svc1" supervisor--manually-stopped)))))

(ert-deftest supervisor-test-remain-after-exit-restart-reruns ()
  "Restart on active remain-after-exit unit clears latch and re-runs."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--remain-active (make-hash-table :test 'equal))
        (supervisor--manually-stopped (make-hash-table :test 'equal))
        (supervisor--manually-started (make-hash-table :test 'equal))
        (supervisor--oneshot-completed (make-hash-table :test 'equal))
        (supervisor--invalid (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--restart-times (make-hash-table :test 'equal))
        (supervisor--mask-override (make-hash-table :test 'equal))
        (supervisor--programs-cache
         (list (list "true" :id "svc1" :type 'oneshot
                     :remain-after-exit t)))
        (started nil))
    (puthash "svc1" t supervisor--remain-active)
    (puthash "svc1" 0 supervisor--oneshot-completed)
    ;; Stop clears the latch
    (supervisor--manual-stop "svc1")
    (should-not (gethash "svc1" supervisor--remain-active))
    ;; Start re-runs (mock the actual process creation)
    (cl-letf (((symbol-function 'supervisor--start-process)
               (lambda (&rest _args)
                 (setq started t)
                 (start-process "mock" nil "true")))
              ((symbol-function 'supervisor--unit-file-directory-for-id)
               (lambda (_id) nil)))
      (let ((result (supervisor--manual-start "svc1")))
        (should (eq 'started (plist-get result :status)))
        (should started)
        ;; Oneshot-completed should have been cleared
        (should-not (gethash "svc1" supervisor--oneshot-completed))
        ;; Remain-active should have been cleared
        (should-not (gethash "svc1" supervisor--remain-active))))))

(ert-deftest supervisor-test-remain-after-exit-snapshot ()
  "Snapshot includes remain-active state."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--oneshot-completed (make-hash-table :test 'equal))
        (supervisor--entry-state (make-hash-table :test 'equal))
        (supervisor--invalid (make-hash-table :test 'equal))
        (supervisor--enabled-override (make-hash-table :test 'equal))
        (supervisor--restart-override (make-hash-table :test 'equal))
        (supervisor--logging (make-hash-table :test 'equal))
        (supervisor--mask-override (make-hash-table :test 'equal))
        (supervisor--manually-started (make-hash-table :test 'equal))
        (supervisor--remain-active (make-hash-table :test 'equal)))
    (puthash "svc1" t supervisor--remain-active)
    (let ((snapshot (supervisor--build-snapshot)))
      (should (gethash "svc1" (supervisor-snapshot-remain-active snapshot))))))

(ert-deftest supervisor-test-remain-after-exit-status-via-snapshot ()
  "Status computed via snapshot respects remain-active."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--oneshot-completed (make-hash-table :test 'equal))
        (supervisor--entry-state (make-hash-table :test 'equal))
        (supervisor--invalid (make-hash-table :test 'equal))
        (supervisor--enabled-override (make-hash-table :test 'equal))
        (supervisor--restart-override (make-hash-table :test 'equal))
        (supervisor--logging (make-hash-table :test 'equal))
        (supervisor--mask-override (make-hash-table :test 'equal))
        (supervisor--manually-started (make-hash-table :test 'equal))
        (supervisor--remain-active (make-hash-table :test 'equal)))
    (puthash "svc1" 0 supervisor--oneshot-completed)
    (puthash "svc1" t supervisor--remain-active)
    (let* ((snapshot (supervisor--build-snapshot))
           (result (supervisor--compute-entry-status "svc1" 'oneshot snapshot)))
      (should (string= "active" (car result))))))

(ert-deftest supervisor-test-remain-after-exit-stop-status-becomes-stopped ()
  "After stopping an active remain-after-exit unit, status is stopped."
  (supervisor-test-with-unit-files
      '(("true" :id "svc1" :type oneshot :remain-after-exit t))
    (let ((supervisor--processes (make-hash-table :test 'equal))
          (supervisor--remain-active (make-hash-table :test 'equal))
          (supervisor--oneshot-completed (make-hash-table :test 'equal))
          (supervisor--manually-stopped (make-hash-table :test 'equal))
          (supervisor--manually-started (make-hash-table :test 'equal))
          (supervisor--failed (make-hash-table :test 'equal))
          (supervisor--entry-state (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor--restart-override (make-hash-table :test 'equal))
          (supervisor--mask-override (make-hash-table :test 'equal))
          (supervisor--logging (make-hash-table :test 'equal)))
      ;; Simulate active latch state (successful oneshot exit)
      (puthash "svc1" 0 supervisor--oneshot-completed)
      (puthash "svc1" t supervisor--remain-active)
      ;; Verify status is active before stop
      (let ((status (car (supervisor--compute-entry-status "svc1" 'oneshot))))
        (should (string= "active" status)))
      ;; Stop the unit
      (supervisor--manual-stop "svc1")
      ;; Verify status is stopped after stop
      (let ((status (car (supervisor--compute-entry-status "svc1" 'oneshot))))
        (should (string= "stopped" status))))))

(ert-deftest supervisor-test-cli-is-active-remain-after-exit ()
  "The `is-active' returns exit 0 for an active remain-after-exit unit."
  (supervisor-test-with-unit-files
      '(("true" :id "svc" :type oneshot :remain-after-exit t))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (supervisor--remain-active (make-hash-table :test 'equal))
           (supervisor--oneshot-completed (make-hash-table :test 'equal))
           (supervisor--failed (make-hash-table :test 'equal))
           (supervisor--invalid (make-hash-table :test 'equal))
           (supervisor--enabled-override (make-hash-table :test 'equal))
           (supervisor--restart-override (make-hash-table :test 'equal))
           (supervisor--mask-override (make-hash-table :test 'equal))
           (supervisor--logging (make-hash-table :test 'equal))
           (supervisor--manually-started (make-hash-table :test 'equal)))
      ;; Simulate active latch state
      (puthash "svc" 0 supervisor--oneshot-completed)
      (puthash "svc" t supervisor--remain-active)
      (let ((result (supervisor--cli-dispatch '("is-active" "svc"))))
        (should (= supervisor-cli-exit-success
                   (supervisor-cli-result-exitcode result)))
        (should (string-match "active" (supervisor-cli-result-output result)))))))

(ert-deftest supervisor-test-dashboard-stop-active-remain-after-exit ()
  "Dashboard stop succeeds on active remain-after-exit oneshot."
  (supervisor-test-with-unit-files
      '(("true" :id "svc1" :type oneshot :remain-after-exit t))
    (let ((supervisor--processes (make-hash-table :test 'equal))
          (supervisor--remain-active (make-hash-table :test 'equal))
          (supervisor--oneshot-completed (make-hash-table :test 'equal))
          (supervisor--manually-stopped (make-hash-table :test 'equal))
          (supervisor--manually-started (make-hash-table :test 'equal))
          (supervisor--failed (make-hash-table :test 'equal))
          (supervisor--entry-state (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor--restart-override (make-hash-table :test 'equal))
          (supervisor--mask-override (make-hash-table :test 'equal))
          (supervisor--logging (make-hash-table :test 'equal))
          (refreshed nil))
      (puthash "svc1" 0 supervisor--oneshot-completed)
      (puthash "svc1" t supervisor--remain-active)
      (cl-letf (((symbol-function 'tabulated-list-get-id)
                 (lambda () (cons :service "svc1")))
                ((symbol-function 'yes-or-no-p)
                 (lambda (_prompt) t))
                ((symbol-function 'supervisor--refresh-dashboard)
                 (lambda () (setq refreshed t))))
        (supervisor-dashboard-stop)
        ;; Latch cleared, manually-stopped set
        (should-not (gethash "svc1" supervisor--remain-active))
        (should (gethash "svc1" supervisor--manually-stopped))
        (should refreshed)))))

(ert-deftest supervisor-test-dashboard-restart-active-remain-after-exit ()
  "Dashboard restart succeeds on active remain-after-exit oneshot."
  (supervisor-test-with-unit-files
      '(("true" :id "svc1" :type oneshot :remain-after-exit t))
    (let ((supervisor--processes (make-hash-table :test 'equal))
          (supervisor--remain-active (make-hash-table :test 'equal))
          (supervisor--oneshot-completed (make-hash-table :test 'equal))
          (supervisor--manually-stopped (make-hash-table :test 'equal))
          (supervisor--manually-started (make-hash-table :test 'equal))
          (supervisor--failed (make-hash-table :test 'equal))
          (supervisor--restart-times (make-hash-table :test 'equal))
          (supervisor--entry-state (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal))
          (supervisor--enabled-override (make-hash-table :test 'equal))
          (supervisor--restart-override (make-hash-table :test 'equal))
          (supervisor--mask-override (make-hash-table :test 'equal))
          (supervisor--logging (make-hash-table :test 'equal))
          (supervisor--programs-cache
           (list (list "true" :id "svc1" :type 'oneshot
                       :remain-after-exit t)))
          (refreshed nil)
          (started nil))
      (puthash "svc1" 0 supervisor--oneshot-completed)
      (puthash "svc1" t supervisor--remain-active)
      (cl-letf (((symbol-function 'tabulated-list-get-id)
                 (lambda () (cons :service "svc1")))
                ((symbol-function 'yes-or-no-p)
                 (lambda (_prompt) t))
                ((symbol-function 'supervisor--refresh-dashboard)
                 (lambda () (setq refreshed t)))
                ((symbol-function 'supervisor--start-process)
                 (lambda (&rest _args)
                   (setq started t)
                   (start-process "mock" nil "true")))
                ((symbol-function 'supervisor--unit-file-directory-for-id)
                 (lambda (_id) nil)))
        (supervisor-dashboard-restart)
        ;; Stop cleared latch, start re-ran
        (should started)
        (should refreshed)))))

(ert-deftest supervisor-test-cli-is-active-json-remain-after-exit ()
  "The `is-active --json' returns active=true for active latched oneshot."
  (supervisor-test-with-unit-files
      '(("true" :id "svc" :type oneshot :remain-after-exit t))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (supervisor--remain-active (make-hash-table :test 'equal))
           (supervisor--oneshot-completed (make-hash-table :test 'equal))
           (supervisor--failed (make-hash-table :test 'equal))
           (supervisor--invalid (make-hash-table :test 'equal))
           (supervisor--enabled-override (make-hash-table :test 'equal))
           (supervisor--restart-override (make-hash-table :test 'equal))
           (supervisor--mask-override (make-hash-table :test 'equal))
           (supervisor--logging (make-hash-table :test 'equal))
           (supervisor--manually-started (make-hash-table :test 'equal))
           (supervisor--manually-stopped (make-hash-table :test 'equal)))
      (puthash "svc" 0 supervisor--oneshot-completed)
      (puthash "svc" t supervisor--remain-active)
      (let* ((result (supervisor--cli-dispatch '("is-active" "svc" "--json")))
             (parsed (json-read-from-string
                      (supervisor-cli-result-output result))))
        (should (= supervisor-cli-exit-success
                   (supervisor-cli-result-exitcode result)))
        (should (eq t (alist-get 'active parsed)))
        (should (equal "active" (alist-get 'status parsed)))))))


(provide 'supervisor-test-keywords)
;;; supervisor-test-keywords.el ends here
