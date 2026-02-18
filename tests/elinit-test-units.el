;;; elinit-test-units.el --- Unit files, authority, and overrides tests for elinit.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit files, authority, and overrides ERT tests for elinit.el.

;;; Code:

(require 'elinit-test-helpers)

;;; Unit-File Tests

(ert-deftest elinit-test-unit-file-path-resolution ()
  "Unit file path returns nil when no authority roots exist."
  (let ((elinit-unit-authority-path nil))
    (should-not (elinit--unit-file-path "nm-applet"))))

(ert-deftest elinit-test-active-authority-roots-skips-missing ()
  "Active roots filters out non-existent directories."
  (let* ((dir1 (make-temp-file "tier1-" t))
         (dir2 "/nonexistent-elinit-tier2")
         (dir3 (make-temp-file "tier3-" t))
         (elinit-unit-authority-path (list dir1 dir2 dir3)))
    (unwind-protect
        (let ((active (elinit--active-authority-roots)))
          (should (equal (list dir1 dir3) active)))
      (delete-directory dir1 t)
      (delete-directory dir3 t))))

(ert-deftest elinit-test-active-authority-roots-preserves-order ()
  "Active roots preserves configured order (low to high)."
  (let* ((dir1 (make-temp-file "tier1-" t))
         (dir2 (make-temp-file "tier2-" t))
         (dir3 (make-temp-file "tier3-" t))
         (elinit-unit-authority-path (list dir1 dir2 dir3)))
    (unwind-protect
        (let ((active (elinit--active-authority-roots)))
          (should (equal (list dir1 dir2 dir3) active)))
      (delete-directory dir1 t)
      (delete-directory dir2 t)
      (delete-directory dir3 t))))

(ert-deftest elinit-test-authority-root-for-id-highest-wins ()
  "Authority root returns highest-precedence root containing the ID."
  (let* ((dir1 (make-temp-file "tier1-" t))
         (dir2 (make-temp-file "tier2-" t))
         (elinit-unit-authority-path (list dir1 dir2))
         (elinit--authority-snapshot nil))
    (unwind-protect
        (progn
          ;; Place same ID in both tiers
          (with-temp-file (expand-file-name "svc.el" dir1)
            (insert "(:id \"svc\" :command \"echo low\")"))
          (with-temp-file (expand-file-name "svc.el" dir2)
            (insert "(:id \"svc\" :command \"echo high\")"))
          ;; Highest-precedence (dir2) should win
          (should (equal dir2 (elinit--authority-root-for-id "svc"))))
      (delete-directory dir1 t)
      (delete-directory dir2 t))))

(ert-deftest elinit-test-authority-root-for-id-returns-nil-when-missing ()
  "Authority root returns nil when ID does not exist in any root."
  (let* ((dir1 (make-temp-file "tier1-" t))
         (elinit-unit-authority-path (list dir1))
         (elinit--authority-snapshot nil))
    (unwind-protect
        (should-not (elinit--authority-root-for-id "nonexistent"))
      (delete-directory dir1 t))))

(ert-deftest elinit-test-unit-file-path-authority-aware ()
  "Unit file path resolves through authority snapshot.
Adding a file on disk does not change the result until the snapshot
is re-published (daemon-reload semantics)."
  (let* ((dir1 (make-temp-file "tier1-" t))
         (dir2 (make-temp-file "tier2-" t))
         (elinit-unit-authority-path (list dir1 dir2))
         (elinit--authority-snapshot nil))
    (unwind-protect
        (progn
          ;; svc exists only in tier 1 (lower precedence)
          (with-temp-file (expand-file-name "svc.el" dir1)
            (insert "(:id \"svc\" :command \"echo\")"))
          (should (equal (expand-file-name "svc.el" dir1)
                         (elinit--unit-file-path "svc")))
          ;; Add to tier 2 — snapshot is stale, still returns tier 1
          (with-temp-file (expand-file-name "svc.el" dir2)
            (insert "(:id \"svc\" :command \"echo\")"))
          (should (equal (expand-file-name "svc.el" dir1)
                         (elinit--unit-file-path "svc")))
          ;; Re-publish snapshot — now tier 2 wins
          (elinit--publish-authority-snapshot)
          (should (equal (expand-file-name "svc.el" dir2)
                         (elinit--unit-file-path "svc"))))
      (delete-directory dir1 t)
      (delete-directory dir2 t))))

(ert-deftest elinit-test-unit-file-path-new-unit-targets-highest ()
  "New unit file path targets highest-precedence active root."
  (let* ((dir1 (make-temp-file "tier1-" t))
         (dir2 (make-temp-file "tier2-" t))
         (elinit-unit-authority-path (list dir1 dir2))
         (elinit--authority-snapshot nil))
    (unwind-protect
        ;; Non-existent ID should target highest root (dir2)
        (should (equal (expand-file-name "new-svc.el" dir2)
                       (elinit--unit-file-path "new-svc")))
      (delete-directory dir1 t)
      (delete-directory dir2 t))))

(ert-deftest elinit-test-snapshot-resolves-mismatched-filename ()
  "Cold-start resolution finds units whose filename differs from :id.
Lazy-init publishes the snapshot automatically, so even with no
prior snapshot, lookup returns the correct root and actual file path."
  (let* ((dir (make-temp-file "tier-" t))
         (elinit-unit-authority-path (list dir))
         (elinit--authority-snapshot nil)
         (elinit--programs-cache :not-yet-loaded)
         (elinit--unit-file-invalid (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; File foo.el contains :id "bar" (filename != :id)
          (with-temp-file (expand-file-name "foo.el" dir)
            (insert "(:id \"bar\" :command \"echo\")"))
          ;; Cold-start: no snapshot yet, but lazy-init resolves correctly
          (should (equal dir (elinit--authority-root-for-id "bar")))
          (should (equal (expand-file-name "foo.el" dir)
                         (elinit--unit-file-path "bar"))))
      (delete-directory dir t))))

;;; Authority resolver tests (Phase 2)

(ert-deftest elinit-test-scan-authority-root-parses-files ()
  "Scanning a root returns candidate structs with metadata."
  (let ((dir (make-temp-file "tier-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "svc.el" dir)
            (insert "(:id \"svc\" :command \"echo\")"))
          (let ((cands (elinit--scan-authority-root dir 0)))
            (should (= 1 (length cands)))
            (let ((c (car cands)))
              (should (elinit--authority-candidate-p c))
              (should (equal "svc" (elinit--authority-candidate-id c)))
              (should (equal dir (elinit--authority-candidate-root c)))
              (should (= 0 (elinit--authority-candidate-tier c)))
              (should (elinit--authority-candidate-valid-p c))
              (should-not (elinit--authority-candidate-reason c)))))
      (delete-directory dir t))))

(ert-deftest elinit-test-scan-authority-root-invalid-candidate ()
  "Scanning produces invalid candidate for malformed unit file."
  (let ((dir (make-temp-file "tier-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "bad.el" dir)
            (insert "(:id \"bad\")"))  ; missing :command
          (let* ((cands (elinit--scan-authority-root dir 0))
                 (c (car cands)))
            (should (= 1 (length cands)))
            (should-not (elinit--authority-candidate-valid-p c))
            (should (elinit--authority-candidate-reason c))))
      (delete-directory dir t))))

(ert-deftest elinit-test-scan-authority-root-lexicographic-order ()
  "Candidates are returned in lexicographic file order."
  (let ((dir (make-temp-file "tier-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "b-svc.el" dir)
            (insert "(:id \"b-svc\" :command \"echo b\")"))
          (with-temp-file (expand-file-name "a-svc.el" dir)
            (insert "(:id \"a-svc\" :command \"echo a\")"))
          (let ((cands (elinit--scan-authority-root dir 0)))
            (should (= 2 (length cands)))
            (should (equal "a-svc"
                           (elinit--authority-candidate-id (nth 0 cands))))
            (should (equal "b-svc"
                           (elinit--authority-candidate-id (nth 1 cands))))))
      (delete-directory dir t))))

(ert-deftest elinit-test-resolve-authority-higher-tier-wins ()
  "Higher-precedence tier wins for same ID."
  (let* ((dir1 (make-temp-file "tier1-" t))
         (dir2 (make-temp-file "tier2-" t))
         (elinit-unit-authority-path (list dir1 dir2)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "svc.el" dir1)
            (insert "(:id \"svc\" :command \"echo low\")"))
          (with-temp-file (expand-file-name "svc.el" dir2)
            (insert "(:id \"svc\" :command \"echo high\")"))
          (let* ((result (elinit--resolve-authority))
                 (winners (plist-get result :winners))
                 (shadowed (plist-get result :shadowed))
                 (winner (gethash "svc" winners)))
            ;; Higher tier wins
            (should (= 1 (elinit--authority-candidate-tier winner)))
            (should (equal dir2 (elinit--authority-candidate-root winner)))
            ;; Lower tier is shadowed
            (should (= 1 (length (gethash "svc" shadowed))))))
      (delete-directory dir1 t)
      (delete-directory dir2 t))))

(ert-deftest elinit-test-resolve-authority-invalid-blocks-fallback ()
  "Invalid unit at highest tier blocks lower-tier valid unit."
  (let* ((dir1 (make-temp-file "tier1-" t))
         (dir2 (make-temp-file "tier2-" t))
         (elinit-unit-authority-path (list dir1 dir2)))
    (unwind-protect
        (progn
          ;; Valid in lower tier
          (with-temp-file (expand-file-name "svc.el" dir1)
            (insert "(:id \"svc\" :command \"echo good\")"))
          ;; Invalid in higher tier (missing :command)
          (with-temp-file (expand-file-name "svc.el" dir2)
            (insert "(:id \"svc\")"))
          (let* ((result (elinit--resolve-authority))
                 (winners (plist-get result :winners))
                 (invalid (plist-get result :invalid))
                 (winner (gethash "svc" winners)))
            ;; Higher tier still wins even though invalid
            (should (= 1 (elinit--authority-candidate-tier winner)))
            (should-not (elinit--authority-candidate-valid-p winner))
            ;; ID is in invalid hash
            (should (gethash "svc" invalid))))
      (delete-directory dir1 t)
      (delete-directory dir2 t))))

(ert-deftest elinit-test-resolve-authority-same-tier-duplicate ()
  "Same-tier duplicate: first-seen (lexicographic) wins."
  (let* ((dir (make-temp-file "tier-" t))
         (elinit-unit-authority-path (list dir)))
    (unwind-protect
        (progn
          ;; Two files with same :id, a-file.el sorts before z-file.el
          (with-temp-file (expand-file-name "a-file.el" dir)
            (insert "(:id \"dup\" :command \"echo first\")"))
          (with-temp-file (expand-file-name "z-file.el" dir)
            (insert "(:id \"dup\" :command \"echo second\")"))
          (let* ((result (elinit--resolve-authority))
                 (winners (plist-get result :winners))
                 (invalid (plist-get result :invalid))
                 (winner (gethash "dup" winners)))
            ;; First-seen (a-file.el) wins
            (should (string-match "a-file\\.el"
                                  (elinit--authority-candidate-path winner)))
            ;; Duplicate is in invalid hash
            (should (string-match "duplicate"
                                  (gethash "dup" invalid)))))
      (delete-directory dir t))))

(ert-deftest elinit-test-resolve-authority-disjoint-ids ()
  "Units with different IDs across tiers all appear as winners."
  (let* ((dir1 (make-temp-file "tier1-" t))
         (dir2 (make-temp-file "tier2-" t))
         (elinit-unit-authority-path (list dir1 dir2)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "a.el" dir1)
            (insert "(:id \"a\" :command \"echo a\")"))
          (with-temp-file (expand-file-name "b.el" dir2)
            (insert "(:id \"b\" :command \"echo b\")"))
          (let* ((result (elinit--resolve-authority))
                 (winners (plist-get result :winners)))
            (should (gethash "a" winners))
            (should (gethash "b" winners))
            (should (= 2 (hash-table-count winners)))))
      (delete-directory dir1 t)
      (delete-directory dir2 t))))

(ert-deftest elinit-test-resolve-authority-empty-roots ()
  "Resolver with no active roots returns empty results."
  (let ((elinit-unit-authority-path nil))
    (let* ((result (elinit--resolve-authority))
           (winners (plist-get result :winners)))
      (should (= 0 (hash-table-count winners))))))

(ert-deftest elinit-test-resolve-authority-three-tiers ()
  "Three-tier resolution: highest tier always wins."
  (let* ((dir1 (make-temp-file "tier1-" t))
         (dir2 (make-temp-file "tier2-" t))
         (dir3 (make-temp-file "tier3-" t))
         (elinit-unit-authority-path (list dir1 dir2 dir3)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "svc.el" dir1)
            (insert "(:id \"svc\" :command \"echo t1\")"))
          (with-temp-file (expand-file-name "svc.el" dir2)
            (insert "(:id \"svc\" :command \"echo t2\")"))
          (with-temp-file (expand-file-name "svc.el" dir3)
            (insert "(:id \"svc\" :command \"echo t3\")"))
          (let* ((result (elinit--resolve-authority))
                 (winners (plist-get result :winners))
                 (shadowed (plist-get result :shadowed))
                 (winner (gethash "svc" winners)))
            ;; Tier 3 (index 2) wins
            (should (= 2 (elinit--authority-candidate-tier winner)))
            ;; Two shadowed entries
            (should (= 2 (length (gethash "svc" shadowed))))))
      (delete-directory dir1 t)
      (delete-directory dir2 t)
      (delete-directory dir3 t))))

(ert-deftest elinit-test-resolve-authority-invalid-clears-lower-invalid ()
  "Higher tier overriding clears lower-tier invalid entry for same ID."
  (let* ((dir1 (make-temp-file "tier1-" t))
         (dir2 (make-temp-file "tier2-" t))
         (elinit-unit-authority-path (list dir1 dir2)))
    (unwind-protect
        (progn
          ;; Invalid in lower tier - same-tier dup produces invalid entry
          (with-temp-file (expand-file-name "a-svc.el" dir1)
            (insert "(:id \"svc\" :command \"echo first\")"))
          (with-temp-file (expand-file-name "z-svc.el" dir1)
            (insert "(:id \"svc\" :command \"echo dup\")"))
          ;; Valid in higher tier overrides everything
          (with-temp-file (expand-file-name "svc.el" dir2)
            (insert "(:id \"svc\" :command \"echo winner\")"))
          (let* ((result (elinit--resolve-authority))
                 (winners (plist-get result :winners))
                 (invalid (plist-get result :invalid))
                 (winner (gethash "svc" winners)))
            ;; Higher tier wins and is valid
            (should (= 1 (elinit--authority-candidate-tier winner)))
            (should (elinit--authority-candidate-valid-p winner))
            ;; No invalid entry for this ID (cleared by higher tier)
            (should-not (gethash "svc" invalid))))
      (delete-directory dir1 t)
      (delete-directory dir2 t))))

(ert-deftest elinit-test-unit-file-keywords-include-command ()
  "Unit-file valid keywords include :command."
  (should (memq :command elinit--unit-file-keywords))
  (should (memq :id elinit--unit-file-keywords))
  (should (memq :type elinit--unit-file-keywords)))

(ert-deftest elinit-test-load-unit-file-valid ()
  "Loading a valid unit file returns (LINE . PLIST)."
  (let ((path (make-temp-file "unit-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "(:id \"test\" :command \"echo hello\" :type simple)"))
          (let* ((result (elinit--load-unit-file path))
                 (line (car result))
                 (plist (cdr result)))
            (should (integerp line))
            (should (equal "test" (plist-get plist :id)))
            (should (equal "echo hello" (plist-get plist :command)))
            (should (eq 'simple (plist-get plist :type)))))
      (delete-file path))))

(ert-deftest elinit-test-load-unit-file-line-number ()
  "Line number reflects plist position after leading whitespace."
  (let ((path (make-temp-file "unit-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "\n\n(:id \"test\" :command \"echo\")"))
          (let* ((result (elinit--load-unit-file path))
                 (line (car result)))
            (should (= 3 line))))
      (delete-file path))))

(ert-deftest elinit-test-load-unit-file-missing ()
  "Loading a missing unit file signals error with path:line context."
  (let ((err (should-error
              (elinit--load-unit-file "/tmp/nonexistent-unit-file.el")
              :type 'error)))
    (should (string-match "/tmp/nonexistent-unit-file\\.el:1:"
                          (error-message-string err)))))

(ert-deftest elinit-test-load-unit-file-empty ()
  "Loading an empty unit file signals error with path:line context."
  (let ((path (make-temp-file "unit-test-" nil ".el")))
    (unwind-protect
        (let ((err (should-error (elinit--load-unit-file path)
                                 :type 'error)))
          (should (string-match (regexp-quote (concat path ":1:"))
                                (error-message-string err))))
      (delete-file path))))

(ert-deftest elinit-test-load-unit-file-not-plist ()
  "Loading a non-plist unit file signals error with path:line context."
  (let ((path (make-temp-file "unit-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "(defun foo () nil)"))
          (let ((err (should-error (elinit--load-unit-file path)
                                   :type 'error)))
            (should (string-match (regexp-quote path)
                                  (error-message-string err)))
            (should (string-match ":[0-9]+:" (error-message-string err)))))
      (delete-file path))))

(ert-deftest elinit-test-load-unit-file-trailing-content ()
  "Loading a unit file with trailing content signals error with path:line."
  (let ((path (make-temp-file "unit-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "(:id \"test\" :command \"echo\")\n(extra stuff)"))
          (let ((err (should-error (elinit--load-unit-file path)
                                   :type 'error)))
            (should (string-match (regexp-quote path)
                                  (error-message-string err)))
            (should (string-match ":[0-9]+:" (error-message-string err)))))
      (delete-file path))))

(ert-deftest elinit-test-validate-unit-file-missing-id ()
  "Unit file plist without :id is invalid with path:line context."
  (let ((reason (elinit--validate-unit-file-plist
                 '(:command "echo") "/tmp/test.el" 1)))
    (should (stringp reason))
    (should (string-match "missing :id" reason))
    (should (string-match "/tmp/test\\.el:1:" reason))))

(ert-deftest elinit-test-validate-unit-file-missing-command ()
  "Unit file plist without :command is invalid with path:line context."
  (let ((reason (elinit--validate-unit-file-plist
                 '(:id "test") "/tmp/test.el" 3)))
    (should (stringp reason))
    (should (string-match "missing :command" reason))
    (should (string-match "/tmp/test\\.el:3:" reason))))

(ert-deftest elinit-test-validate-unit-file-unknown-keyword ()
  "Unit file plist with unknown keyword is invalid with path:line context."
  (let ((reason (elinit--validate-unit-file-plist
                 '(:id "test" :command "echo" :bogus 42) "/tmp/test.el" 1)))
    (should (stringp reason))
    (should (string-match "unknown keyword" reason))
    (should (string-match "/tmp/test\\.el:1:" reason))))

(ert-deftest elinit-test-validate-unit-file-valid ()
  "Valid unit file plist returns nil."
  (should-not (elinit--validate-unit-file-plist
               '(:id "test" :command "echo" :type simple)
               "/tmp/test.el" 1)))

(ert-deftest elinit-test-unit-file-to-program-entry ()
  "Unit file plist converts to program entry format."
  (let ((entry (elinit--unit-file-to-program-entry
                '(:id "nm" :command "nm-applet" :type simple :restart t))))
    ;; Car is the command string
    (should (equal "nm-applet" (car entry)))
    ;; Rest is a plist with :id but no :command
    (should (equal "nm" (plist-get (cdr entry) :id)))
    (should (eq 'simple (plist-get (cdr entry) :type)))
    (should (eq t (plist-get (cdr entry) :restart)))
    ;; :command should not be in the output plist
    (should-not (plist-member (cdr entry) :command))))

(ert-deftest elinit-test-load-all-unit-files-missing-dir ()
  "Loading from nonexistent directory returns nil."
  (let ((elinit-unit-authority-path
         '("/tmp/nonexistent-elinit-units-dir"))
        (elinit-unit-directory "/tmp/nonexistent-elinit-units-dir"))
    (should-not (elinit--load-all-unit-files))))

(ert-deftest elinit-test-load-all-unit-files-alphabetical ()
  "Unit files are loaded in alphabetical order as plists."
  (let ((dir (make-temp-file "units-" t)))
    (unwind-protect
        (let ((elinit-unit-authority-path (list dir))
              (elinit-unit-directory dir))
          (with-temp-file (expand-file-name "bravo.el" dir)
            (insert "(:id \"bravo\" :command \"bravo-cmd\")"))
          (with-temp-file (expand-file-name "alpha.el" dir)
            (insert "(:id \"alpha\" :command \"alpha-cmd\")"))
          (let ((results (elinit--load-all-unit-files)))
            (should (= 2 (length results)))
            ;; Alpha first (alphabetical); results are plists directly
            (should (equal "alpha" (plist-get (nth 0 results) :id)))
            (should (equal "bravo" (plist-get (nth 1 results) :id)))))
      (delete-directory dir t))))

(ert-deftest elinit-test-load-all-unit-files-invalid-tracked ()
  "Invalid unit files are recorded in elinit--unit-file-invalid."
  (let ((dir (make-temp-file "units-" t)))
    (unwind-protect
        (let ((elinit-unit-authority-path (list dir))
              (elinit-unit-directory dir))
          ;; Valid file
          (with-temp-file (expand-file-name "good.el" dir)
            (insert "(:id \"good\" :command \"echo ok\")"))
          ;; Invalid file (missing :command)
          (with-temp-file (expand-file-name "bad.el" dir)
            (insert "(:id \"bad\")"))
          (let ((results (elinit--load-all-unit-files)))
            ;; Only valid entries returned
            (should (= 1 (length results)))
            (should (equal "good" (plist-get (nth 0 results) :id)))
            ;; Invalid tracked in hash with path:line context
            (should (= 1 (hash-table-count elinit--unit-file-invalid)))
            (let ((reason (gethash "bad" elinit--unit-file-invalid)))
              (should (stringp reason))
              (should (string-match "missing :command" reason))
              (should (string-match ":[0-9]+:" reason)))))
      (delete-directory dir t))))

(ert-deftest elinit-test-load-all-unit-files-load-error-tracked ()
  "Unit files that fail to parse are recorded in invalid hash with path:line."
  (let ((dir (make-temp-file "units-" t)))
    (unwind-protect
        (let ((elinit-unit-authority-path (list dir))
              (elinit-unit-directory dir))
          (with-temp-file (expand-file-name "broken.el" dir)
            (insert "not a plist at all"))
          (let ((results (elinit--load-all-unit-files)))
            (should (= 0 (length results)))
            (should (= 1 (hash-table-count elinit--unit-file-invalid)))
            (let ((reason (gethash "broken" elinit--unit-file-invalid)))
              (should (stringp reason))
              (should (string-match ":[0-9]+:" reason)))))
      (delete-directory dir t))))

(ert-deftest elinit-test-unit-file-invalid-merged-into-plan ()
  "Invalid unit files appear in elinit--invalid after plan build."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit--programs-cache :not-yet-loaded)
         (elinit--invalid (make-hash-table :test 'equal))
         (elinit--cycle-fallback-ids (make-hash-table :test 'equal))
         (elinit--computed-deps (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "bad.el" dir)
            (insert "(:id \"bad-unit\")"))
          (let ((plan (elinit--build-plan (elinit--effective-programs))))
            (maphash (lambda (k v) (puthash k v elinit--invalid))
                     (elinit-plan-invalid plan))
            (elinit--merge-unit-file-invalid)
            ;; The invalid unit file should now be visible
            (should (gethash "bad-unit" elinit--invalid))))
      (delete-directory dir t))))

(ert-deftest elinit-test-load-programs-empty-directory ()
  "With no unit files present, returns empty list."
  (elinit-test-without-builtins
    (let* ((dir (make-temp-file "units-" t))
           (elinit-unit-authority-path (list dir))
           (elinit-unit-directory dir)
           (elinit--programs-cache :not-yet-loaded))
      (unwind-protect
          (should (null (elinit--effective-programs)))
        (delete-directory dir t)))))

(ert-deftest elinit-test-load-programs-unit-file-merge ()
  "Unit file entries are loaded from disk via effective-programs."
  (elinit-test-without-builtins
    (let* ((dir (make-temp-file "units-" t))
           (elinit-unit-authority-path (list dir))
           (elinit-unit-directory dir)
           (elinit--programs-cache :not-yet-loaded))
      (unwind-protect
          (progn
            (with-temp-file (expand-file-name "svc.el" dir)
              (insert "(:id \"svc\" :command \"new-cmd\" :type simple)"))
            (let ((programs (elinit--effective-programs)))
              ;; Should have exactly one entry (unit-file wins)
              (should (= 1 (length programs)))
              ;; The command should be the unit-file version
              (should (equal "new-cmd" (car (nth 0 programs))))))
        (delete-directory dir t)))))

(ert-deftest elinit-test-load-programs-unit-file-adds-new ()
  "Multiple unit files are loaded and returned."
  (elinit-test-without-builtins
    (let* ((dir (make-temp-file "units-" t))
           (elinit-unit-authority-path (list dir))
           (elinit-unit-directory dir)
           (elinit--programs-cache :not-yet-loaded))
      (unwind-protect
          (progn
            (with-temp-file (expand-file-name "alpha.el" dir)
              (insert "(:id \"alpha\" :command \"alpha-cmd\" :type simple)"))
            (with-temp-file (expand-file-name "beta.el" dir)
              (insert "(:id \"beta\" :command \"beta-cmd\" :type simple)"))
            (let ((programs (elinit--effective-programs)))
              ;; Should have both entries (alphabetical order)
              (should (= 2 (length programs)))
              (should (equal "alpha-cmd" (car (nth 0 programs))))
              (should (equal "beta-cmd" (car (nth 1 programs))))))
        (delete-directory dir t)))))

(ert-deftest elinit-test-loader-cross-tier-precedence ()
  "Loader returns higher-tier unit when same ID exists in multiple roots."
  (elinit-test-without-builtins
    (let* ((dir1 (make-temp-file "tier1-" t))
           (dir2 (make-temp-file "tier2-" t))
           (elinit-unit-authority-path (list dir1 dir2))
           (elinit-unit-directory dir2)
           (elinit--programs-cache :not-yet-loaded))
      (unwind-protect
          (progn
            (with-temp-file (expand-file-name "svc.el" dir1)
              (insert "(:id \"svc\" :command \"low-cmd\" :type simple)"))
            (with-temp-file (expand-file-name "svc.el" dir2)
              (insert "(:id \"svc\" :command \"high-cmd\" :type simple)"))
            ;; effective-programs should return the higher-tier version
            (let ((programs (elinit--effective-programs)))
              (should (= 1 (length programs)))
              (should (equal "high-cmd" (car (nth 0 programs))))))
        (delete-directory dir1 t)
        (delete-directory dir2 t)))))

(ert-deftest elinit-test-loader-invalid-blocks-lower-tier ()
  "Loader blocks lower-tier valid unit when higher tier is invalid."
  (elinit-test-without-builtins
    (let* ((dir1 (make-temp-file "tier1-" t))
           (dir2 (make-temp-file "tier2-" t))
           (elinit-unit-authority-path (list dir1 dir2))
           (elinit-unit-directory dir2)
           (elinit--programs-cache :not-yet-loaded))
      (unwind-protect
          (progn
            ;; Valid in lower tier
            (with-temp-file (expand-file-name "svc.el" dir1)
              (insert "(:id \"svc\" :command \"good-cmd\")"))
            ;; Invalid in higher tier (missing :command)
            (with-temp-file (expand-file-name "svc.el" dir2)
              (insert "(:id \"svc\")"))
            (let ((programs (elinit--effective-programs)))
              ;; No valid programs - higher tier blocks
              (should (= 0 (length programs)))
              ;; Invalid hash should record it
              (should (gethash "svc" elinit--unit-file-invalid))))
        (delete-directory dir1 t)
        (delete-directory dir2 t)))))

(ert-deftest elinit-test-loader-snapshot-consistency ()
  "Authority snapshot is consistent with loader output."
  (elinit-test-without-builtins
    (let* ((dir (make-temp-file "tier-" t))
           (elinit-unit-authority-path (list dir))
           (elinit-unit-directory dir)
           (elinit--programs-cache :not-yet-loaded))
      (unwind-protect
          (progn
            (with-temp-file (expand-file-name "a.el" dir)
              (insert "(:id \"a\" :command \"echo a\")"))
            (with-temp-file (expand-file-name "b.el" dir)
              (insert "(:id \"b\" :command \"echo b\")"))
            (let ((programs (elinit--effective-programs)))
              (should (= 2 (length programs)))
              ;; Snapshot should exist and match
              (should elinit--authority-snapshot)
            (let ((winners (plist-get elinit--authority-snapshot :winners)))
              (should (= 2 (hash-table-count winners)))
              (should (gethash "a" winners))
              (should (gethash "b" winners)))))
        (delete-directory dir t)))))

(ert-deftest elinit-test-dashboard-shows-invalid-authority-units ()
  "Dashboard entries include invalid units from authority resolution."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit--programs-cache :not-yet-loaded)
         (elinit--processes (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal))
         (elinit--invalid (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; Valid unit
          (with-temp-file (expand-file-name "good.el" dir)
            (insert "(:id \"good\" :command \"echo ok\")"))
          ;; Invalid unit (missing :command)
          (with-temp-file (expand-file-name "bad.el" dir)
            (insert "(:id \"bad\")"))
          (let* ((programs (elinit--effective-programs))
                 (snapshot (elinit--build-snapshot))
                 (entries (elinit--get-entries snapshot programs)))
            ;; Should have entries for both good and bad (typed IDs)
            (should (cl-some (lambda (e) (equal (cons :service "good") (car e))) entries))
            (should (cl-some (lambda (e) (equal (cons :service "bad") (car e))) entries))))
      (delete-directory dir t))))

(ert-deftest elinit-test-health-summary-counts-invalid-authority ()
  "Health summary counts invalid authority units."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit--programs-cache :not-yet-loaded)
         (elinit--processes (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal))
         (elinit--failed (make-hash-table :test 'equal))
         (elinit--oneshot-completed (make-hash-table :test 'equal))
         (elinit--invalid (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "bad.el" dir)
            (insert "(:id \"bad\")"))
          (let* ((programs (elinit--effective-programs))
                 (snapshot (elinit--build-snapshot))
                 (summary (elinit--health-summary snapshot programs)))
            ;; Summary should show 1 invalid
            (should (string-match "1 inv" summary))))
      (delete-directory dir t))))

(ert-deftest elinit-test-dashboard-single-snapshot-refresh ()
  "Dashboard refresh uses single authority snapshot (no re-publish)."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit--programs-cache :not-yet-loaded)
         (publish-count 0))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "svc.el" dir)
            (insert "(:id \"svc\" :command \"echo\")"))
          ;; Count publish calls during effective-programs
          (cl-letf (((symbol-function 'elinit--resolve-authority)
                     (let ((orig (symbol-function
                                  'elinit--resolve-authority)))
                       (lambda ()
                         (cl-incf publish-count)
                         (funcall orig)))))
            (let ((programs (elinit--effective-programs)))
              ;; One publish for loading programs
              (should (= 1 publish-count))
              ;; Passing programs to get-entries and health-summary
              ;; should NOT trigger additional publishes
              (let ((snapshot (elinit--build-snapshot)))
                (elinit--get-entries snapshot programs)
                (elinit--health-summary snapshot programs)
                (should (= 1 publish-count))))))
      (delete-directory dir t))))

(ert-deftest elinit-test-programs-cache-isolates-disk-changes ()
  "Programs cache prevents disk changes from leaking without daemon-reload."
  (elinit-test-without-builtins
    (let* ((dir (make-temp-file "cache-" t))
           (elinit-unit-authority-path (list dir))
           (elinit-unit-directory dir)
           (elinit--programs-cache :not-yet-loaded)
           (elinit--authority-snapshot nil)
           (elinit--unit-file-invalid (make-hash-table :test 'equal)))
      (unwind-protect
          (progn
            ;; Create one unit and load
            (with-temp-file (expand-file-name "a.el" dir)
              (insert "(:id \"a\" :command \"echo a\")"))
            (let ((programs (elinit--effective-programs)))
              (should (= 1 (length programs))))
            ;; Add a second unit on disk WITHOUT refreshing
            (with-temp-file (expand-file-name "b.el" dir)
              (insert "(:id \"b\" :command \"echo b\")"))
            ;; Cache should still return only 1 program
            (let ((programs (elinit--effective-programs)))
              (should (= 1 (length programs))))
            ;; Explicit refresh (simulates daemon-reload) picks up the new unit
            (elinit--refresh-programs)
            (let ((programs (elinit--effective-programs)))
              (should (= 2 (length programs)))))
        (delete-directory dir t)))))

(ert-deftest elinit-test-start-path-publishes-authority-snapshot ()
  "The `elinit-start' path refreshes the programs cache from disk."
  (elinit-test-without-builtins
    (let* ((dir (make-temp-file "start-" t))
           (elinit-unit-authority-path (list dir))
           (elinit-unit-directory dir)
           (elinit--programs-cache :not-yet-loaded)
           (elinit--authority-snapshot nil)
           (elinit--unit-file-invalid (make-hash-table :test 'equal))
           (publish-count 0))
      (unwind-protect
          (progn
            (with-temp-file (expand-file-name "svc.el" dir)
              (insert "(:id \"svc\" :command \"echo ok\")"))
            ;; Count authority resolutions during refresh-programs
            (cl-letf (((symbol-function 'elinit--resolve-authority)
                       (let ((orig (symbol-function
                                    'elinit--resolve-authority)))
                         (lambda ()
                           (cl-incf publish-count)
                           (funcall orig)))))
              ;; Simulate what elinit-start does: refresh then read
              (elinit--refresh-programs)
              (should (= 1 publish-count))
              ;; Subsequent reads use cache (no extra publish)
              (let ((programs (elinit--effective-programs)))
                (should (= 1 (length programs)))
                (should (= 1 publish-count)))))
        (delete-directory dir t)))))

(ert-deftest elinit-test-three-tier-loader-precedence ()
  "Loader returns highest-tier unit across a three-tier authority path."
  (elinit-test-with-authority-tiers 3
    (with-temp-file (expand-file-name "svc.el" dir1)
      (insert "(:id \"svc\" :command \"vendor-cmd\")"))
    (with-temp-file (expand-file-name "svc.el" dir2)
      (insert "(:id \"svc\" :command \"admin-cmd\")"))
    (with-temp-file (expand-file-name "svc.el" dir3)
      (insert "(:id \"svc\" :command \"user-cmd\")"))
    (let ((programs (elinit--effective-programs)))
      (should (= 1 (length programs)))
      (should (equal "user-cmd" (car (nth 0 programs)))))))

(ert-deftest elinit-test-authority-tiers-macro-cleanup ()
  "Authority tiers macro creates and cleans up temp directories."
  (let (saved-dirs)
    (elinit-test-with-authority-tiers 2
      (setq saved-dirs (list dir1 dir2))
      (should (file-directory-p dir1))
      (should (file-directory-p dir2))
      (should (= 2 (length elinit-unit-authority-path))))
    ;; Directories should be cleaned up after the macro
    (should-not (file-directory-p (nth 0 saved-dirs)))
    (should-not (file-directory-p (nth 1 saved-dirs)))))

(ert-deftest elinit-test-mixed-tier-disjoint-and-override ()
  "Disjoint IDs coexist while shared IDs follow tier precedence."
  (elinit-test-with-authority-tiers 2
    ;; dir1 (low): a and shared
    (with-temp-file (expand-file-name "a.el" dir1)
      (insert "(:id \"a\" :command \"low-a\")"))
    (with-temp-file (expand-file-name "shared.el" dir1)
      (insert "(:id \"shared\" :command \"low-shared\")"))
    ;; dir2 (high): b and shared
    (with-temp-file (expand-file-name "b.el" dir2)
      (insert "(:id \"b\" :command \"high-b\")"))
    (with-temp-file (expand-file-name "shared.el" dir2)
      (insert "(:id \"shared\" :command \"high-shared\")"))
    (let ((programs (elinit--effective-programs)))
      ;; 3 programs: a (low), b (high), shared (high wins)
      (should (= 3 (length programs)))
      (let ((cmds (mapcar #'car programs)))
        (should (member "low-a" cmds))
        (should (member "high-b" cmds))
        (should (member "high-shared" cmds))
        (should-not (member "low-shared" cmds))))))

(ert-deftest elinit-test-reordered-root-list-flips-winner ()
  "User-configured root order determines precedence, not directory names.
Reversing the authority path order flips which root wins for a
conflicting ID, proving precedence derives from list position."
  (elinit-test-without-builtins
    (let* ((dir-a (make-temp-file "rootA-" t))
           (dir-b (make-temp-file "rootB-" t))
           (elinit--programs-cache :not-yet-loaded)
           (elinit--authority-snapshot nil)
           (elinit--unit-file-invalid (make-hash-table :test 'equal)))
      (unwind-protect
          (progn
            ;; Same ID in both roots, different commands
            (with-temp-file (expand-file-name "svc.el" dir-a)
              (insert "(:id \"svc\" :command \"from-a\")"))
            (with-temp-file (expand-file-name "svc.el" dir-b)
              (insert "(:id \"svc\" :command \"from-b\")"))
            ;; Order A, B: B is last = highest precedence
            (let ((elinit-unit-authority-path (list dir-a dir-b)))
              (elinit--publish-authority-snapshot)
              (setq elinit--programs-cache :not-yet-loaded)
              (let ((programs (elinit--effective-programs)))
                (should (= 1 (length programs)))
                (should (equal "from-b" (caar programs)))))
            ;; Reverse to B, A: A is last = highest precedence
            (let ((elinit-unit-authority-path (list dir-b dir-a)))
              (setq elinit--authority-snapshot nil)
              (setq elinit--programs-cache :not-yet-loaded)
              (let ((programs (elinit--effective-programs)))
                (should (= 1 (length programs)))
                (should (equal "from-a" (caar programs))))))
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest elinit-test-cli-verify-invalid-unit-file ()
  "CLI verify reports invalid unit files in count and output."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit--programs-cache :not-yet-loaded))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "bad.el" dir)
            (insert "(:id \"bad-unit\")"))
          (let ((result (elinit--cli-dispatch '("verify"))))
            (should (elinit-cli-result-p result))
            (should (= elinit-cli-exit-validation-failed
                        (elinit-cli-result-exitcode result)))
            (should (string-match "1 invalid"
                                  (elinit-cli-result-output result)))
            (should (string-match "bad-unit"
                                  (elinit-cli-result-output result)))))
      (delete-directory dir t))))

(ert-deftest elinit-test-cli-list-units-shows-invalid-unit-file ()
  "CLI list-units includes invalid unit files in output."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit--programs-cache :not-yet-loaded)
         (elinit--processes (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "bad.el" dir)
            (insert "(:id \"bad-unit\")"))
          (let ((result (elinit--cli-dispatch '("list-units"))))
            (should (elinit-cli-result-p result))
            (should (string-match "bad-unit"
                                  (elinit-cli-result-output result)))))
      (delete-directory dir t))))

(ert-deftest elinit-test-cli-status-invalid-unit-file ()
  "CLI status ID recognizes invalid unit files, not just plan-level invalids."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit--programs-cache :not-yet-loaded)
         (elinit--processes (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "bad.el" dir)
            (insert "(:id \"bad-unit\")"))
          (let ((result (elinit--cli-dispatch '("status" "bad-unit"))))
            (should (elinit-cli-result-p result))
            ;; Should find it as invalid, not "not found"
            (should (= elinit-cli-exit-success
                        (elinit-cli-result-exitcode result)))
            (should (string-match "invalid"
                                  (elinit-cli-result-output result)))
            (should-not (string-match "could not be found"
                                      (elinit-cli-result-output result)))))
      (delete-directory dir t))))

(ert-deftest elinit-test-dry-run-shows-invalid-unit-files ()
  "Dry-run summary includes invalid unit files in count and listing."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit--programs-cache :not-yet-loaded)
         (elinit--invalid (make-hash-table :test 'equal))
         (elinit--cycle-fallback-ids (make-hash-table :test 'equal))
         (elinit--computed-deps (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "bad.el" dir)
            (insert "(:id \"bad-unit\")"))
          (elinit-dry-run)
          (with-current-buffer "*elinit-dry-run*"
            (let ((content (buffer-string)))
              (should (string-match "1 invalid" content))
              (should (string-match "bad-unit" content)))))
      (delete-directory dir t))))

;;; Unit-File Scaffold and Edit/Cat Tests

(ert-deftest elinit-test-unit-file-scaffold-contains-id ()
  "Scaffold template contains the given ID."
  (let ((scaffold (elinit--unit-file-scaffold "my-svc")))
    (should (stringp scaffold))
    (should (string-match ":id \"my-svc\"" scaffold))
    (should (string-match ":command" scaffold))))

(ert-deftest elinit-test-unit-file-scaffold-is-valid-plist ()
  "Scaffold template can be read as a valid plist (with command filled)."
  (let* ((scaffold (elinit--unit-file-scaffold "test-svc"))
         ;; Replace empty command placeholder with actual value
         (filled (replace-regexp-in-string
                  ":command \"\"" ":command \"echo hello\"" scaffold)))
    (with-temp-buffer
      (insert filled)
      (goto-char (point-min))
      (let ((form (read (current-buffer))))
        (should (listp form))
        (should (keywordp (car form)))
        (should (equal "test-svc" (plist-get form :id)))
        (should (equal "echo hello" (plist-get form :command)))))))

(ert-deftest elinit-test-seed-default-maintenance-units-creates-files ()
  "Default maintenance seeding creates logrotate and log-prune units."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-logrotate-command "/usr/bin/rotate-stub")
         (elinit-log-prune-command "/usr/bin/prune-stub")
         (elinit-log-directory "/tmp/sv-test-logs")
         (elinit-logrotate-keep-days 9)
         (elinit-log-prune-max-total-bytes 12345))
    (unwind-protect
        (progn
          (elinit--ensure-default-maintenance-units)
          (let ((rotate-file (expand-file-name "logrotate.el" dir))
                (prune-file (expand-file-name "log-prune.el" dir)))
            (should (file-exists-p rotate-file))
            (should (file-exists-p prune-file))
            (with-temp-buffer
              (insert-file-contents rotate-file)
              (should (string-match-p ":id \"logrotate\"" (buffer-string)))
              (should (string-match-p "rotate-stub" (buffer-string)))
              (should (string-match-p "--signal-reopen" (buffer-string))))
            (with-temp-buffer
              (insert-file-contents prune-file)
              (should (string-match-p ":id \"log-prune\"" (buffer-string)))
              (should (string-match-p "prune-stub" (buffer-string)))
              (should (string-match-p ":requires (\"logrotate\")" (buffer-string))))))
      (delete-directory dir t))))

(ert-deftest elinit-test-seed-default-maintenance-units-does-not-overwrite-existing ()
  "Maintenance seeding preserves existing units while seeding missing ones."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-logrotate-command "/usr/bin/rotate-stub")
         (elinit-log-prune-command "/usr/bin/prune-stub")
         (elinit-log-directory "/tmp/sv-test-logs")
         (elinit-logrotate-keep-days 14)
         (elinit-log-prune-max-total-bytes 999))
    (unwind-protect
        (let ((rotate-file (expand-file-name "logrotate.el" dir)))
          (write-region "(:id \"logrotate\" :command \"echo custom\")\n"
                        nil rotate-file nil 'silent)
          (elinit--ensure-default-maintenance-units)
          (with-temp-buffer
            (insert-file-contents rotate-file)
            (should (string-match-p "echo custom" (buffer-string))))
          (should (file-exists-p (expand-file-name "log-prune.el" dir))))
      (delete-directory dir t))))

(ert-deftest elinit-test-seed-skips-masked-units ()
  "Seeding does not create units that are masked via runtime override."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-logrotate-command "/usr/bin/rotate-stub")
         (elinit-log-prune-command "/usr/bin/prune-stub")
         (elinit-log-directory "/tmp/sv-test-logs")
         (elinit-logrotate-keep-days 14)
         (elinit-log-prune-max-total-bytes 999)
         (elinit--mask-override (make-hash-table :test 'equal)))
    (puthash "logrotate" 'masked elinit--mask-override)
    (unwind-protect
        (progn
          (elinit--ensure-default-maintenance-units)
          ;; logrotate masked: should NOT be seeded
          (should-not (file-exists-p
                       (expand-file-name "logrotate.el" dir)))
          ;; log-prune not masked: should be seeded
          (should (file-exists-p
                   (expand-file-name "log-prune.el" dir))))
      (delete-directory dir t))))

(ert-deftest elinit-test-seed-skips-disabled-units ()
  "Seeding does not create units that are disabled via runtime override."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-logrotate-command "/usr/bin/rotate-stub")
         (elinit-log-prune-command "/usr/bin/prune-stub")
         (elinit-log-directory "/tmp/sv-test-logs")
         (elinit-logrotate-keep-days 14)
         (elinit-log-prune-max-total-bytes 999)
         (elinit--mask-override (make-hash-table :test 'equal))
         (elinit--enabled-override (make-hash-table :test 'equal)))
    (puthash "log-prune" 'disabled elinit--enabled-override)
    (unwind-protect
        (progn
          (elinit--ensure-default-maintenance-units)
          ;; logrotate not disabled: should be seeded
          (should (file-exists-p
                   (expand-file-name "logrotate.el" dir)))
          ;; log-prune disabled: should NOT be seeded
          (should-not (file-exists-p
                       (expand-file-name "log-prune.el" dir))))
      (delete-directory dir t))))

(ert-deftest elinit-test-cli-cat-existing-unit-file ()
  "CLI cat outputs raw content of existing unit file."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "my-svc.el" dir)
            (insert "(:id \"my-svc\" :command \"echo ok\")"))
          (let ((result (elinit--cli-dispatch '("cat" "my-svc"))))
            (should (elinit-cli-result-p result))
            (should (= elinit-cli-exit-success
                        (elinit-cli-result-exitcode result)))
            (should (string-match ":id \"my-svc\""
                                  (elinit-cli-result-output result)))))
      (delete-directory dir t))))

(ert-deftest elinit-test-cli-cat-missing-unit-file ()
  "CLI cat returns error for missing unit file."
  (let ((elinit-unit-authority-path '("/tmp/nonexistent-elinit-units-dir"))
        (elinit-unit-directory "/tmp/nonexistent-elinit-units-dir"))
    (let ((result (elinit--cli-dispatch '("cat" "nope"))))
      (should (elinit-cli-result-p result))
      (should (= elinit-cli-exit-failure
                  (elinit-cli-result-exitcode result)))
      (should (string-match "not found"
                            (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-cat-builtin-no-unit-file ()
  "CLI cat explains built-in entries without a backing unit file."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit--authority-snapshot nil)
         (elinit--programs-cache :not-yet-loaded)
         (elinit--unit-file-invalid (make-hash-table :test 'equal)))
    (unwind-protect
        (let ((result (elinit--cli-dispatch '("cat" "logrotate"))))
          (should (elinit-cli-result-p result))
          (should (= elinit-cli-exit-failure
                     (elinit-cli-result-exitcode result)))
          (should (string-match
                   "No unit file on disk for 'logrotate'"
                   (elinit-cli-result-output result))))
      (delete-directory dir t))))

(ert-deftest elinit-test-cli-cat-no-args ()
  "CLI cat requires an ID argument."
  (let ((result (elinit--cli-dispatch '("cat"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args
                (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-cat-too-many-args ()
  "CLI cat takes exactly one ID."
  (let ((result (elinit--cli-dispatch '("cat" "a" "b"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args
                (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-cat-json-format ()
  "CLI cat --json outputs path and content fields."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "svc.el" dir)
            (insert "(:id \"svc\" :command \"echo\")"))
          (let ((result (elinit--cli-dispatch '("cat" "svc" "--json"))))
            (should (elinit-cli-result-p result))
            (should (= elinit-cli-exit-success
                        (elinit-cli-result-exitcode result)))
            (should (eq 'json (elinit-cli-result-format result)))
            (let ((parsed (json-read-from-string
                           (elinit-cli-result-output result))))
              (should (assoc 'path parsed))
              (should (assoc 'content parsed))
              (should (string-match ":id" (cdr (assoc 'content parsed)))))))
      (delete-directory dir t))))

(ert-deftest elinit-test-cli-edit-creates-scaffold ()
  "CLI edit creates scaffold template for missing unit file."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir))
    (unwind-protect
        ;; Use JSON mode to test scaffold creation without editor launch
        (let ((result (elinit--cli-dispatch '("edit" "new-svc" "--json"))))
          (should (elinit-cli-result-p result))
          (should (= elinit-cli-exit-success
                      (elinit-cli-result-exitcode result)))
          ;; File should have been created
          (let ((path (expand-file-name "new-svc.el" dir)))
            (should (file-exists-p path))
            ;; Content should have the ID
            (with-temp-buffer
              (insert-file-contents path)
              (should (string-match ":id \"new-svc\""
                                    (buffer-string))))))
      (delete-directory dir t))))

(ert-deftest elinit-test-cli-edit-existing-file ()
  "CLI edit on existing file does not overwrite it."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "svc.el" dir)
            (insert "(:id \"svc\" :command \"existing\")"))
          ;; Use JSON mode to test without editor launch
          (let ((result (elinit--cli-dispatch '("edit" "svc" "--json"))))
            (should (elinit-cli-result-p result))
            (should (= elinit-cli-exit-success
                        (elinit-cli-result-exitcode result)))
            ;; File content should be unchanged
            (with-temp-buffer
              (insert-file-contents (expand-file-name "svc.el" dir))
              (should (string-match "existing" (buffer-string))))))
      (delete-directory dir t))))

(ert-deftest elinit-test-cli-edit-json-format ()
  "CLI edit --json outputs path and created flag."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir))
    (unwind-protect
        (let ((result (elinit--cli-dispatch '("edit" "new" "--json"))))
          (should (elinit-cli-result-p result))
          (should (= elinit-cli-exit-success
                      (elinit-cli-result-exitcode result)))
          (should (eq 'json (elinit-cli-result-format result)))
          (let ((parsed (json-read-from-string
                         (elinit-cli-result-output result))))
            (should (assoc 'path parsed))
            (should (eq t (cdr (assoc 'created parsed))))
            ;; Root should be reported even for new units
            (should (stringp (cdr (assoc 'root parsed))))))
      (delete-directory dir t))))

(ert-deftest elinit-test-cli-edit-json-includes-authority-root ()
  "CLI edit --json output includes authority root and tier."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit--authority-snapshot nil)
         (elinit--programs-cache :not-yet-loaded))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "svc.el" dir)
            (insert "(:id \"svc\" :command \"echo\")"))
          ;; Publish snapshot so tier lookup works
          (elinit--publish-authority-snapshot)
          (let* ((result (elinit--cli-dispatch '("edit" "svc" "--json")))
                 (parsed (json-read-from-string
                          (elinit-cli-result-output result))))
            (should (equal dir (cdr (assoc 'root parsed))))
            (should (equal 0 (cdr (assoc 'tier parsed))))
            (should (eq :json-false (cdr (assoc 'created parsed))))))
      (delete-directory dir t))))

(ert-deftest elinit-test-cli-status-shows-unit-file-path ()
  "CLI status shows unit-file path in detailed output."
  (elinit-test-with-unit-files
      '(("echo ok" :id "svc" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (result (elinit--cli-dispatch '("status" "svc"))))
      (should (string-match "Unit file:" (elinit-cli-result-output result))))))

(ert-deftest elinit-test-cli-status-json-includes-unit-file ()
  "CLI status --json output includes unit_file field."
  (elinit-test-with-unit-files
      '(("echo ok" :id "svc" :type simple))
    (let* ((elinit--processes (make-hash-table :test 'equal))
           (elinit--entry-state (make-hash-table :test 'equal))
           (result (elinit--cli-dispatch '("status" "svc" "--json")))
           (parsed (json-read-from-string
                    (elinit-cli-result-output result)))
           (entries (cdr (assoc 'entries parsed)))
           (first-entry (aref entries 0)))
      (should (assoc 'unit_file first-entry))
      (should (stringp (cdr (assoc 'unit_file first-entry)))))))

(ert-deftest elinit-test-cli-status-builtin-omits-unit-file-line ()
  "CLI status omits unit-file line for built-ins without disk units."
  (let* ((dir (make-temp-file "units-" t))
         (logs (make-temp-file "logs-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit-log-directory logs)
         (elinit--authority-snapshot nil)
         (elinit--programs-cache :not-yet-loaded)
         (elinit--unit-file-invalid (make-hash-table :test 'equal))
         (elinit--processes (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal))
         (result (elinit--cli-dispatch '("status" "logrotate"))))
    (unwind-protect
        (progn
          (should (= elinit-cli-exit-success
                     (elinit-cli-result-exitcode result)))
          (should-not (string-match "Unit file:"
                                    (elinit-cli-result-output result))))
      (delete-directory logs t)
      (delete-directory dir t))))

(ert-deftest elinit-test-cli-status-json-builtin-unit-file-null ()
  "CLI status --json sets unit_file to null for fileless built-ins."
  (let* ((dir (make-temp-file "units-" t))
         (logs (make-temp-file "logs-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit-log-directory logs)
         (elinit--authority-snapshot nil)
         (elinit--programs-cache :not-yet-loaded)
         (elinit--unit-file-invalid (make-hash-table :test 'equal))
         (elinit--processes (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal))
         (result (elinit--cli-dispatch '("status" "logrotate" "--json"))))
    (unwind-protect
        (let* ((parsed (json-read-from-string
                        (elinit-cli-result-output result)))
               (entries (cdr (assoc 'entries parsed)))
               (first-entry (and (> (length entries) 0)
                                 (aref entries 0))))
          (should (= elinit-cli-exit-success
                     (elinit-cli-result-exitcode result)))
          (should first-entry)
          (should (assoc 'unit_file first-entry))
          (should-not (cdr (assoc 'unit_file first-entry))))
      (delete-directory logs t)
      (delete-directory dir t))))

(ert-deftest elinit-test-cli-edit-no-args ()
  "CLI edit requires an ID argument."
  (let ((result (elinit--cli-dispatch '("edit"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args
                (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-cli-edit-too-many-args ()
  "CLI edit takes exactly one ID."
  (let ((result (elinit--cli-dispatch '("edit" "a" "b"))))
    (should (elinit-cli-result-p result))
    (should (= elinit-cli-exit-invalid-args
                (elinit-cli-result-exitcode result)))))

(ert-deftest elinit-test-dashboard-cat-keybinding ()
  "Dashboard keymap binds `t' to proced."
  (should (eq #'proced
              (lookup-key elinit-dashboard-mode-map "t"))))

(ert-deftest elinit-test-dashboard-edit-keybinding ()
  "Dashboard keymap binds `F' to tag filter."
  (should (eq #'elinit-dashboard-cycle-tag-filter
              (lookup-key elinit-dashboard-mode-map "F"))))

(ert-deftest elinit-test-dashboard-cat-rejects-separator ()
  "Dashboard cat rejects separator rows."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list '--services-- (vector "" "" "" "" "" "" "" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should-error (elinit-dashboard-cat)
                    :type 'user-error))))

(ert-deftest elinit-test-dashboard-cat-rejects-timer ()
  "Dashboard cat rejects timer rows."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list "t1" (vector "t1" "timer" "-" "yes"
                                    "waiting" "---" "---" "-" "-")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should-error (elinit-dashboard-cat)
                    :type 'user-error))))

(ert-deftest elinit-test-dashboard-cat-builtin-no-unit-file ()
  "Dashboard cat explains fileless built-in entries."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit--authority-snapshot nil)
         (elinit--programs-cache :not-yet-loaded)
         (elinit--unit-file-invalid (make-hash-table :test 'equal)))
    (unwind-protect
        (with-temp-buffer
          (elinit-dashboard-mode)
          (let ((tabulated-list-entries
                 (list (list (cons :service "logrotate")
                             (vector "logrotate" "oneshot" "-"
                                     "yes" "pending" "n/a" "yes" "-" "-")))))
            (tabulated-list-init-header)
            (tabulated-list-print)
            (goto-char (point-min))
            (let ((err (should-error (elinit-dashboard-cat) :type 'user-error)))
              (should (string-match-p
                       "No unit file on disk for logrotate"
                       (error-message-string err))))))
      (delete-directory dir t))))

(ert-deftest elinit-test-dashboard-edit-rejects-separator ()
  "Dashboard edit rejects separator rows."
  (with-temp-buffer
    (elinit-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list '--services-- (vector "" "" "" "" "" "" "" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should-error (elinit-dashboard-edit)
                    :type 'user-error))))

(ert-deftest elinit-test-cli-edit-launches-editor ()
  "CLI edit launches $VISUAL/$EDITOR via `call-process'."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (launch-args nil))
    (unwind-protect
        (cl-letf (((symbol-function 'elinit--cli-edit-launch-editor)
                   (lambda (editor path)
                     (setq launch-args (list editor path))
                     0)))
          (let* ((process-environment
                  (cons "VISUAL=my-editor" process-environment))
                 (result (elinit--cli-dispatch '("edit" "new-svc"))))
            (should (elinit-cli-result-p result))
            (should (= elinit-cli-exit-success
                        (elinit-cli-result-exitcode result)))
            ;; Editor should have been called
            (should launch-args)
            (should (equal "my-editor" (car launch-args)))))
      (delete-directory dir t))))

(ert-deftest elinit-test-cli-edit-editor-failure ()
  "CLI edit reports failure when editor exits non-zero."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir))
    (unwind-protect
        (cl-letf (((symbol-function 'elinit--cli-edit-launch-editor)
                   (lambda (_editor _path) 1)))
          (let* ((process-environment
                  (cons "VISUAL=my-editor" process-environment))
                 (result (elinit--cli-dispatch '("edit" "new-svc"))))
            (should (elinit-cli-result-p result))
            (should (= elinit-cli-exit-failure
                        (elinit-cli-result-exitcode result)))
            (should (string-match "exited with status 1"
                                  (elinit-cli-result-output result)))))
      (delete-directory dir t))))

(ert-deftest elinit-test-cli-edit-no-editor-error ()
  "CLI edit returns error when no $VISUAL or $EDITOR is set."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir))
    (unwind-protect
        (let* ((process-environment
                (cl-remove-if (lambda (e)
                                (or (string-prefix-p "VISUAL=" e)
                                    (string-prefix-p "EDITOR=" e)))
                              process-environment))
               (result (elinit--cli-dispatch '("edit" "new-svc"))))
          (should (elinit-cli-result-p result))
          (should (= elinit-cli-exit-failure
                      (elinit-cli-result-exitcode result)))
          (should (string-match "No \\$VISUAL or \\$EDITOR"
                                (elinit-cli-result-output result))))
      (delete-directory dir t))))

(ert-deftest elinit-test-dashboard-edit-enables-edit-mode ()
  "Dashboard edit activates `elinit-edit-mode' with return-on-q."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (path (expand-file-name "test-svc.el" dir)))
    (unwind-protect
        (progn
          (write-region "(:id \"test-svc\" :command \"echo\")" nil path)
          ;; Simulate dashboard with a valid entry
          (with-temp-buffer
            (elinit-dashboard-mode)
            (let ((tabulated-list-entries
                   (list (list (cons :service "test-svc")
                               (vector "test-svc" "simple" "-"
                                       "yes" "running" "yes" "---"
                                       "-" "-")))))
              (tabulated-list-init-header)
              (tabulated-list-print)
              (goto-char (point-min))
              (elinit-dashboard-edit)
              ;; Should have opened the file with edit mode
              (let ((edit-buf (get-file-buffer path)))
                (should edit-buf)
                (with-current-buffer edit-buf
                  (should elinit-edit-mode)
                  ;; q should be bound
                  (should (eq #'elinit-edit-quit
                              (lookup-key elinit-edit-mode-map "q")))
                  ;; C-c C-q should be bound
                  (should (eq #'elinit-edit-finish
                              (lookup-key elinit-edit-mode-map
                                          (kbd "C-c C-q")))))
                (kill-buffer edit-buf)))))
      (delete-directory dir t))))

(ert-deftest elinit-test-edit-quit-returns ()
  "Pressing `q' in edit buffer returns to dashboard."
  (let* ((dir (make-temp-file "units-" t))
         (path (expand-file-name "svc.el" dir))
         (returned nil))
    (unwind-protect
        (progn
          (write-region "(:id \"svc\" :command \"echo\")" nil path)
          (find-file path)
          (elinit-edit-mode 1)
          (cl-letf (((symbol-function 'elinit--return-to-dashboard)
                     (lambda () (setq returned t)))
                    ((symbol-function 'y-or-n-p) (lambda (_) nil)))
            ;; Buffer is unmodified, q should quit
            (should-not (buffer-modified-p))
            (elinit-edit-quit)
            (should returned)))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest elinit-test-dashboard-edit-save-hook-fires ()
  "Dashboard edit save-hook wiring fires validator on save."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (path (expand-file-name "hook-svc.el" dir))
         (validated nil))
    (unwind-protect
        (progn
          (write-region "(:id \"hook-svc\" :command \"echo\")" nil path)
          (with-temp-buffer
            (elinit-dashboard-mode)
            (let ((tabulated-list-entries
                   (list (list (cons :service "hook-svc")
                               (vector "hook-svc" "simple" "-"
                                       "yes" "running" "yes" "---"
                                       "-" "-")))))
              (tabulated-list-init-header)
              (tabulated-list-print)
              (goto-char (point-min))
              (elinit-dashboard-edit)
              (let ((edit-buf (get-file-buffer path)))
                (should edit-buf)
                (with-current-buffer edit-buf
                  ;; Validator should be in after-save-hook
                  (should (memq #'elinit--validate-unit-file-buffer
                                after-save-hook))
                  ;; Mock validator to track invocation
                  (cl-letf (((symbol-function
                              'elinit--validate-unit-file-buffer)
                             (lambda () (setq validated t))))
                    ;; Modify and save to trigger hook
                    (goto-char (point-max))
                    (insert " ")
                    (save-buffer)))
                (should validated)
                (kill-buffer edit-buf)))))
      (delete-directory dir t))))

(ert-deftest elinit-test-validate-unit-file-buffer-hook ()
  "Validate-on-save reports validation result."
  (let* ((dir (make-temp-file "units-" t))
         (path (expand-file-name "test.el" dir)))
    (unwind-protect
        (progn
          (write-region "(:id \"test\" :command \"echo\")" nil path)
          (with-temp-buffer
            (insert-file-contents path)
            (setq buffer-file-name path)
            ;; Should not error for valid file
            (elinit--validate-unit-file-buffer)))
      (delete-directory dir t))))

;;; Schema v1 Tests

(ert-deftest elinit-test-service-schema-version ()
  "Service schema version constant is defined."
  (should (= elinit-service-schema-version 1)))

(ert-deftest elinit-test-service-struct-fields ()
  "Service struct has all expected fields."
  (let ((service (elinit-service--create
                  :id "test"
                  :command "sleep 100"
                  :type 'simple
                  :delay 5
                  :enabled t
                  :restart 'no
                  :logging t
                  :after '("dep1")
                  :requires '("req1")
                  :oneshot-blocking nil
                  :oneshot-timeout 60
                  :tags '(tag1 tag2))))
    (should (equal "test" (elinit-service-id service)))
    (should (equal "sleep 100" (elinit-service-command service)))
    (should (eq 'simple (elinit-service-type service)))
    (should (= 5 (elinit-service-delay service)))
    (should (elinit-service-enabled service))
    (should (eq 'no (elinit-service-restart service)))
    (should (elinit-service-logging service))
    (should (equal '("dep1") (elinit-service-after service)))
    (should (equal '("req1") (elinit-service-requires service)))
    (should-not (elinit-service-oneshot-blocking service))
    (should (= 60 (elinit-service-oneshot-timeout service)))
    (should (equal '(tag1 tag2) (elinit-service-tags service)))))

(ert-deftest elinit-test-entry-accessors ()
  "Entry accessor functions work correctly."
  (let ((entry (elinit--parse-entry
                '("sleep 100" :id "test" :type simple                  :delay 5 :restart nil :after ("dep1") :requires ("req1")))))
    (should (equal "test" (elinit-entry-id entry)))
    (should (equal "sleep 100" (elinit-entry-command entry)))
    (should (eq 'simple (elinit-entry-type entry)))
    (should (= 5 (elinit-entry-delay entry)))
    (should-not (elinit-entry-restart-p entry))
    (should (equal '("dep1") (elinit-entry-after entry)))
    (should (equal '("req1") (elinit-entry-requires entry)))))

(ert-deftest elinit-test-entry-to-service-conversion ()
  "Entry to service conversion preserves all fields."
  (let* ((entry (elinit--parse-entry
                 '("sleep 100" :id "test" :type oneshot                   :after ("dep") :requires ("req") :tags (t1 t2))))
         (service (elinit-entry-to-service entry)))
    (should (equal "test" (elinit-service-id service)))
    (should (equal "sleep 100" (elinit-service-command service)))
    (should (eq 'oneshot (elinit-service-type service)))
    (should (equal '("dep") (elinit-service-after service)))
    (should (equal '("req") (elinit-service-requires service)))
    (should (equal '(t1 t2) (elinit-service-tags service)))))

(ert-deftest elinit-test-service-to-entry-conversion ()
  "Service to entry conversion preserves all fields."
  (let* ((service (elinit-service--create
                   :id "test"
                   :command "sleep 100"
                   :type 'oneshot
                   :delay 10
                   :enabled nil
                   :restart 'always
                   :logging nil
                   :after '("dep")
                   :requires '("req")
                   :oneshot-blocking t
                   :oneshot-timeout 120
                   :tags '(t1)))
         (entry (elinit-service-to-entry service)))
    (should (equal "test" (elinit-entry-id entry)))
    (should (equal "sleep 100" (elinit-entry-command entry)))
    (should (eq 'oneshot (elinit-entry-type entry)))
    (should (= 10 (elinit-entry-delay entry)))
    (should-not (elinit-entry-enabled-p entry))
    (should (elinit-entry-restart-p entry))
    (should-not (elinit-entry-logging-p entry))
    (should (equal '("dep") (elinit-entry-after entry)))
    (should (equal '("req") (elinit-entry-requires entry)))
    (should (elinit-entry-oneshot-blocking entry))
    (should (= 120 (elinit-entry-oneshot-timeout entry)))
    (should (equal '(t1) (elinit-entry-tags entry)))))

;;; Dependency Semantics Tests

(ert-deftest elinit-test-requires-keyword-valid ()
  ":requires keyword is accepted as valid."
  (should-not (elinit--validate-entry
               '("foo" :type simple :requires "bar"))))

(ert-deftest elinit-test-requires-parsed-correctly ()
  ":requires is parsed into entry correctly."
  (let ((entry (elinit--parse-entry
                '("foo" :requires ("bar" "baz")))))
    (should (equal '("bar" "baz") (elinit-entry-requires entry)))))

(ert-deftest elinit-test-requires-missing-id-warned ()
  "Missing :requires target is warned and dropped."
  (let* ((logged nil)
         (programs '(("sleep 100" :id "a")
                     ("sleep 200" :id "b" :requires "nonexistent"))))
    (cl-letf (((symbol-function 'elinit--log)
               (lambda (_level fmt &rest args)
                 (push (apply #'format fmt args) logged))))
      (let ((plan (elinit--build-plan programs)))
        (should (cl-some (lambda (m) (string-match-p "does not exist" m))
                         logged))))))

(ert-deftest elinit-test-requires-combined-with-after ()
  ":requires and :after are combined for topological sort."
  (let* ((programs '(("sleep 100" :id "a")
                     ("sleep 200" :id "b" :after "a")
                     ("sleep 300" :id "c" :requires "a")))
         (plan (elinit--build-plan programs))
         (sorted (elinit-plan-by-target plan))
         (ids (mapcar #'car sorted)))
    ;; a should come before both b and c
    (should (< (cl-position "a" ids :test #'equal)
               (cl-position "b" ids :test #'equal)))
    (should (< (cl-position "a" ids :test #'equal)
               (cl-position "c" ids :test #'equal)))))

(ert-deftest elinit-test-plan-requires-deps-populated ()
  "Plan includes separate requires-deps hash."
  (let* ((programs '(("sleep 100" :id "a")
                     ("sleep 200" :id "b" :requires "a")))
         (plan (elinit--build-plan programs)))
    (should (elinit-plan-requires-deps plan))
    (should (equal '("a") (gethash "b" (elinit-plan-requires-deps plan))))))

;;; Persistent Overrides Tests

(ert-deftest elinit-test-overrides-file-path ()
  "Overrides file path respects custom setting."
  (let ((elinit-overrides-file "/custom/path/overrides.eld"))
    (should (equal "/custom/path/overrides.eld"
                   (elinit--overrides-file-path))))
  (let ((elinit-overrides-file nil))
    (should-not (elinit--overrides-file-path))))

(ert-deftest elinit-test-overrides-to-alist ()
  "Overrides are correctly collected into an alist."
  (let ((elinit--enabled-override (make-hash-table :test 'equal))
        (elinit--restart-override (make-hash-table :test 'equal))
        (elinit--logging (make-hash-table :test 'equal)))
    (puthash "a" 'enabled elinit--enabled-override)
    (puthash "b" 'no elinit--restart-override)
    (puthash "a" 'enabled elinit--logging)
    (let ((alist (elinit--overrides-to-alist)))
      (should (= 2 (length alist)))
      (let ((a-entry (cdr (assoc "a" alist)))
            (b-entry (cdr (assoc "b" alist))))
        (should (eq 'enabled (plist-get a-entry :enabled)))
        (should (eq 'enabled (plist-get a-entry :logging)))
        (should (eq 'no (plist-get b-entry :restart)))))))

(ert-deftest elinit-test-overrides-save-load-roundtrip ()
  "Overrides survive save and load roundtrip."
  (let* ((temp-file (make-temp-file "elinit-test-overrides-" nil ".eld"))
         (elinit-overrides-file temp-file)
         (elinit--enabled-override (make-hash-table :test 'equal))
         (elinit--restart-override (make-hash-table :test 'equal))
         (elinit--logging (make-hash-table :test 'equal))
         (elinit--overrides-loaded nil))
    (unwind-protect
        (progn
          ;; Set some overrides
          (puthash "test-entry" 'disabled elinit--enabled-override)
          (puthash "test-entry" 'always elinit--restart-override)
          ;; Save
          (should (elinit--save-overrides))
          ;; Clear memory
          (clrhash elinit--enabled-override)
          (clrhash elinit--restart-override)
          ;; Load
          (should (elinit--load-overrides))
          ;; Verify
          (should (eq 'disabled (gethash "test-entry" elinit--enabled-override)))
          (should (eq 'always (gethash "test-entry" elinit--restart-override))))
      (delete-file temp-file))))

(ert-deftest elinit-test-overrides-corrupt-file-handled ()
  "Corrupt overrides file is handled gracefully."
  (let* ((temp-file (make-temp-file "elinit-test-corrupt-" nil ".eld"))
         (elinit-overrides-file temp-file)
         (elinit--enabled-override (make-hash-table :test 'equal))
         (elinit--restart-override (make-hash-table :test 'equal))
         (elinit--logging (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; Write corrupt data
          (with-temp-file temp-file
            (insert "this is not valid lisp (((("))
          ;; Load should return nil but not error
          (should-not (elinit--load-overrides))
          ;; File should still exist (not deleted)
          (should (file-exists-p temp-file)))
      (delete-file temp-file))))

(ert-deftest elinit-test-cli-disable-loads-overrides-before-save ()
  "Disable CLI command preserves existing overrides on first save."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc1" :type simple)
        ("sleep 300" :id "svc2" :type simple))
    (let* ((temp-file (make-temp-file "elinit-test-cli-load-" nil ".eld"))
           (elinit-overrides-file temp-file)
           (elinit--enabled-override (make-hash-table :test 'equal))
           (elinit--restart-override (make-hash-table :test 'equal))
           (elinit--logging (make-hash-table :test 'equal))
           (elinit--mask-override (make-hash-table :test 'equal))
           (elinit--overrides-loaded nil))
      (unwind-protect
          (progn
            ;; Seed file with an existing override for svc1.
            (with-temp-file temp-file
              (insert ";; Elinit overrides file - do not edit manually\n")
              (insert ";; Schema version: 1\n")
              (pp '((version . 1)
                    (timestamp . "2026-02-15T00:00:00+0000")
                    (overrides ("svc1" :enabled disabled)))
                  (current-buffer)))
            (let ((result (elinit--cli-dispatch '("disable" "svc2"))))
              (should (= elinit-cli-exit-success
                         (elinit-cli-result-exitcode result))))
            ;; Reload from disk and verify both overrides survive.
            (clrhash elinit--enabled-override)
            (clrhash elinit--restart-override)
            (clrhash elinit--logging)
            (clrhash elinit--mask-override)
            (setq elinit--overrides-loaded nil)
            (should (elinit--load-overrides))
            (should (eq 'disabled (gethash "svc1" elinit--enabled-override)))
            (should (eq 'disabled (gethash "svc2" elinit--enabled-override))))
        (delete-file temp-file)))))

(ert-deftest elinit-test-cli-disable-does-not-overwrite-corrupt-overrides ()
  "Disable CLI command refuses to overwrite a corrupt overrides file."
  (elinit-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple))
    (let* ((temp-file (make-temp-file "elinit-test-cli-corrupt-" nil ".eld"))
           (elinit-overrides-file temp-file)
           (elinit--enabled-override (make-hash-table :test 'equal))
           (elinit--restart-override (make-hash-table :test 'equal))
           (elinit--logging (make-hash-table :test 'equal))
           (elinit--mask-override (make-hash-table :test 'equal))
           (elinit--overrides-loaded nil))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "this is not valid lisp (((("))
            (let ((result (elinit--cli-dispatch '("disable" "svc"))))
              (should (= elinit-cli-exit-failure
                         (elinit-cli-result-exitcode result))))
            (should-not (gethash "svc" elinit--enabled-override))
            (with-temp-buffer
              (insert-file-contents temp-file)
              (should (string-match-p "not valid lisp" (buffer-string)))))
        (delete-file temp-file)))))

;;; Migration Tests

(ert-deftest elinit-test-migrate-string-entry ()
  "Migration handles simple string entries."
  (let ((result (elinit--migrate-entry-to-plist "nm-applet")))
    ;; Simple string with defaults should remain a string
    (should (stringp result))
    (should (equal "nm-applet" result))))

(ert-deftest elinit-test-migrate-plist-entry ()
  "Migration handles plist entries."
  (let ((result (elinit--migrate-entry-to-plist
                 '("sleep 100" :type oneshot))))
    (should (listp result))
    (should (equal "sleep 100" (car result)))
    (should (eq 'oneshot (plist-get (cdr result) :type)))))

(ert-deftest elinit-test-migrate-all-entries-skips-invalid ()
  "Migration skips invalid entries with reason."
  (elinit-test-with-unit-files
      '(("valid-cmd" :id "valid" :type simple)
        ("invalid-cmd" :id "invalid" :type "bad"))
    (let ((result (elinit--migrate-all-entries)))
      (should (= 1 (length (plist-get result :migrated))))
      ;; Invalid entry is caught at unit-file validation (delegation)
      ;; and tracked in elinit--unit-file-invalid
      (should (gethash "invalid" elinit--unit-file-invalid)))))

(ert-deftest elinit-test-migrate-all-entries-skips-duplicates ()
  "Migration sees only one entry when unit-file loader deduplicates."
  (elinit-test-without-builtins
    (let* ((dir (make-temp-file "units-" t))
           (elinit-unit-authority-path (list dir))
           (elinit-unit-directory dir)
           (elinit--programs-cache :not-yet-loaded)
           (elinit--unit-file-invalid (make-hash-table :test 'equal)))
      ;; Two unit files with the same :id but different filenames
      (with-temp-file (expand-file-name "test-a.el" dir)
        (insert "(:id \"test\" :command \"a\" :type simple)"))
      (with-temp-file (expand-file-name "test-b.el" dir)
        (insert "(:id \"test\" :command \"b\" :type oneshot)"))
      (unwind-protect
          (let ((result (elinit--migrate-all-entries)))
            ;; Unit-file loader deduplicates, so migration sees only one
            (should (= 1 (length (plist-get result :migrated))))
            ;; No skipped entries at migration level (dedup happened upstream)
            (should (= 0 (length (plist-get result :skipped)))))
        (delete-directory dir t)))))

(ert-deftest elinit-test-migrate-entry-to-service ()
  "High-level migration function works."
  (let ((service (elinit-migrate-entry-to-service
                  '("sleep 100" :id "test" :type oneshot))))
    (should (elinit-service-p service))
    (should (equal "test" (elinit-service-id service)))
    (should (eq 'oneshot (elinit-service-type service)))))

(ert-deftest elinit-test-migrate-entry-to-service-invalid-errors ()
  "High-level migration function signals error for invalid entry."
  (should-error
   (elinit-migrate-entry-to-service '("foo" :type "bad"))
   :type 'error))

;;; Bug fix verification tests

(ert-deftest elinit-test-validate-after-must-be-string-or-list ()
  "Validation rejects non-string/list :after values."
  ;; Valid cases
  (should-not (elinit--validate-entry '("foo" :after "bar")))
  (should-not (elinit--validate-entry '("foo" :after ("a" "b"))))
  (should-not (elinit--validate-entry '("foo" :after nil)))
  ;; Invalid cases
  (should (string-match ":after must be"
                        (elinit--validate-entry '("foo" :after 123))))
  (should (string-match ":after must be"
                        (elinit--validate-entry '("foo" :after (1 2 3))))))

(ert-deftest elinit-test-validate-requires-must-be-string-or-list ()
  "Validation rejects non-string/list :requires values."
  ;; Valid cases
  (should-not (elinit--validate-entry '("foo" :requires "bar")))
  (should-not (elinit--validate-entry '("foo" :requires ("a" "b"))))
  (should-not (elinit--validate-entry '("foo" :requires nil)))
  ;; Invalid cases
  (should (string-match ":requires must be"
                        (elinit--validate-entry '("foo" :requires 123))))
  (should (string-match ":requires must be"
                        (elinit--validate-entry '("foo" :requires (1 2 3))))))

(ert-deftest elinit-test-requires-valid-ordering ()
  "Entries with :requires are valid and ordered."
  (let* ((programs '(("a" :id "a")
                     ("b" :id "b" :requires "a")))
         (plan (elinit--build-plan programs))
         (sorted (elinit-plan-by-target plan)))
    ;; Both should be valid
    (should-not (gethash "b" (elinit-plan-invalid plan)))
    ;; Both should appear in sorted entries
    (should (cl-find "a" sorted :key #'elinit-entry-id :test #'equal))
    (should (cl-find "b" sorted :key #'elinit-entry-id :test #'equal))))

(ert-deftest elinit-test-dag-uses-requires-edges ()
  "DAG scheduler uses :requires edges for in-degree calculation."
  (let* ((entries '(("a" "sleep 1" 0 t always t simple nil t 30 nil nil)
                    ("b" "sleep 1" 0 t always t simple nil t 30 nil ("a"))))
         ;; Entry "b" has :requires "a" at index 12
         (elinit--dag-in-degree (make-hash-table :test 'equal))
         (elinit--dag-dependents (make-hash-table :test 'equal))
         (elinit--dag-entries (make-hash-table :test 'equal))
         (elinit--dag-blocking (make-hash-table :test 'equal))
         (elinit--dag-started (make-hash-table :test 'equal))
         (elinit--dag-ready (make-hash-table :test 'equal))
         (elinit--dag-timeout-timers (make-hash-table :test 'equal))
         (elinit--dag-delay-timers (make-hash-table :test 'equal))
         (elinit--dag-id-to-index (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal)))
    (elinit--dag-init entries)
    ;; "a" has no deps, in-degree should be 0
    (should (= 0 (gethash "a" elinit--dag-in-degree)))
    ;; "b" has :requires "a", in-degree should be 1
    (should (= 1 (gethash "b" elinit--dag-in-degree)))
    ;; "a" should have "b" as dependent
    (should (member "b" (gethash "a" elinit--dag-dependents)))))

(ert-deftest elinit-test-override-load-clears-stale-state ()
  "Loading overrides clears stale in-memory state not present in file."
  (let* ((temp-file (make-temp-file "elinit-test-stale-" nil ".eld"))
         (elinit-overrides-file temp-file)
         (elinit--enabled-override (make-hash-table :test 'equal))
         (elinit--restart-override (make-hash-table :test 'equal))
         (elinit--logging (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; First, save a file with only "file-entry"
          (puthash "file-entry" 'enabled elinit--enabled-override)
          (elinit--save-overrides)
          ;; Clear and add stale state that's NOT in the file
          (clrhash elinit--enabled-override)
          (clrhash elinit--restart-override)
          (clrhash elinit--logging)
          (puthash "stale-entry" 'disabled elinit--enabled-override)
          (puthash "stale-entry" 'no elinit--restart-override)
          (puthash "stale-entry" 'disabled elinit--logging)
          (puthash "another-stale" 'enabled elinit--enabled-override)
          ;; Load from file - should clear stale state and restore file state
          (elinit--load-overrides)
          ;; Stale entries should be gone (not in file)
          (should-not (gethash "stale-entry" elinit--enabled-override))
          (should-not (gethash "another-stale" elinit--enabled-override))
          (should-not (gethash "stale-entry" elinit--restart-override))
          (should-not (gethash "stale-entry" elinit--logging))
          ;; File entry should be restored
          (should (eq 'enabled (gethash "file-entry" elinit--enabled-override))))
      (delete-file temp-file))))

(ert-deftest elinit-test-duplicate-id-invalid-does-not-poison-valid ()
  "Valid entry is not poisoned by later invalid duplicate with same ID."
  (let* ((programs '(("cmd" :id "test" :type simple)      ; valid first
                     ("cmd" :id "test" :type "bad")))     ; invalid duplicate
         (plan (elinit--build-plan programs)))
    ;; The valid entry should be in plan.entries
    (should (= 1 (length (elinit-plan-entries plan))))
    (should (equal "test" (elinit-entry-id (car (elinit-plan-entries plan)))))
    ;; The ID should NOT be in the invalid hash (first valid wins)
    (should-not (gethash "test" (elinit-plan-invalid plan)))))

(ert-deftest elinit-test-duplicate-id-invalid-first-valid-later ()
  "First valid occurrence wins even when first occurrence is invalid."
  (let* ((programs '(("cmd" :id "test" :type "bad")       ; invalid first
                     ("cmd" :id "test" :type simple)))    ; valid later
         (plan (elinit--build-plan programs)))
    ;; The valid entry should be in plan.entries
    (should (= 1 (length (elinit-plan-entries plan))))
    (should (equal "test" (elinit-entry-id (car (elinit-plan-entries plan)))))
    ;; The ID should NOT be in the invalid hash (valid cleared stale invalid)
    (should-not (gethash "test" (elinit-plan-invalid plan)))))

(ert-deftest elinit-test-requires-included-in-plan-entries ()
  "Entries with :requires are included in plan.entries."
  (let* ((programs '(("a" :id "a")
                     ("b" :id "b" :requires "a")))
         (plan (elinit--build-plan programs)))
    ;; Both should be valid and in plan.entries
    (should-not (gethash "b" (elinit-plan-invalid plan)))
    (should (cl-find "b" (elinit-plan-entries plan)
                     :key #'elinit-entry-id :test #'equal))))

(ert-deftest elinit-test-validate-dotted-list-after ()
  "Validation handles dotted lists in :after without error."
  ;; Dotted list should return validation error, not signal
  (let ((result (elinit--validate-entry '("foo" :after (a . b)))))
    (should (stringp result))
    (should (string-match ":after must be" result))))

(ert-deftest elinit-test-validate-dotted-list-requires ()
  "Validation handles dotted lists in :requires without error."
  ;; Dotted list should return validation error, not signal
  (let ((result (elinit--validate-entry '("foo" :requires (x . y)))))
    (should (stringp result))
    (should (string-match ":requires must be" result))))

(ert-deftest elinit-test-empty-programs-no-by-target ()
  "Empty program list produces empty by-target."
  (let* ((programs nil)
         (plan (elinit--build-plan programs)))
    (should (null (elinit-plan-by-target plan)))))


(provide 'elinit-test-units)
;;; elinit-test-units.el ends here
