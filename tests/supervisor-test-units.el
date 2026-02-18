;;; supervisor-test-units.el --- Unit files, authority, and overrides tests for supervisor.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit files, authority, and overrides ERT tests for supervisor.el.

;;; Code:

(require 'supervisor-test-helpers)

;;; Unit-File Tests

(ert-deftest supervisor-test-unit-file-path-resolution ()
  "Unit file path returns nil when no authority roots exist."
  (let ((supervisor-unit-authority-path nil))
    (should-not (supervisor--unit-file-path "nm-applet"))))

(ert-deftest supervisor-test-active-authority-roots-skips-missing ()
  "Active roots filters out non-existent directories."
  (let* ((dir1 (make-temp-file "tier1-" t))
         (dir2 "/nonexistent-supervisor-tier2")
         (dir3 (make-temp-file "tier3-" t))
         (supervisor-unit-authority-path (list dir1 dir2 dir3)))
    (unwind-protect
        (let ((active (supervisor--active-authority-roots)))
          (should (equal (list dir1 dir3) active)))
      (delete-directory dir1 t)
      (delete-directory dir3 t))))

(ert-deftest supervisor-test-active-authority-roots-preserves-order ()
  "Active roots preserves configured order (low to high)."
  (let* ((dir1 (make-temp-file "tier1-" t))
         (dir2 (make-temp-file "tier2-" t))
         (dir3 (make-temp-file "tier3-" t))
         (supervisor-unit-authority-path (list dir1 dir2 dir3)))
    (unwind-protect
        (let ((active (supervisor--active-authority-roots)))
          (should (equal (list dir1 dir2 dir3) active)))
      (delete-directory dir1 t)
      (delete-directory dir2 t)
      (delete-directory dir3 t))))

(ert-deftest supervisor-test-authority-root-for-id-highest-wins ()
  "Authority root returns highest-precedence root containing the ID."
  (let* ((dir1 (make-temp-file "tier1-" t))
         (dir2 (make-temp-file "tier2-" t))
         (supervisor-unit-authority-path (list dir1 dir2))
         (supervisor--authority-snapshot nil))
    (unwind-protect
        (progn
          ;; Place same ID in both tiers
          (with-temp-file (expand-file-name "svc.el" dir1)
            (insert "(:id \"svc\" :command \"echo low\")"))
          (with-temp-file (expand-file-name "svc.el" dir2)
            (insert "(:id \"svc\" :command \"echo high\")"))
          ;; Highest-precedence (dir2) should win
          (should (equal dir2 (supervisor--authority-root-for-id "svc"))))
      (delete-directory dir1 t)
      (delete-directory dir2 t))))

(ert-deftest supervisor-test-authority-root-for-id-returns-nil-when-missing ()
  "Authority root returns nil when ID does not exist in any root."
  (let* ((dir1 (make-temp-file "tier1-" t))
         (supervisor-unit-authority-path (list dir1))
         (supervisor--authority-snapshot nil))
    (unwind-protect
        (should-not (supervisor--authority-root-for-id "nonexistent"))
      (delete-directory dir1 t))))

(ert-deftest supervisor-test-unit-file-path-authority-aware ()
  "Unit file path resolves through authority snapshot.
Adding a file on disk does not change the result until the snapshot
is re-published (daemon-reload semantics)."
  (let* ((dir1 (make-temp-file "tier1-" t))
         (dir2 (make-temp-file "tier2-" t))
         (supervisor-unit-authority-path (list dir1 dir2))
         (supervisor--authority-snapshot nil))
    (unwind-protect
        (progn
          ;; svc exists only in tier 1 (lower precedence)
          (with-temp-file (expand-file-name "svc.el" dir1)
            (insert "(:id \"svc\" :command \"echo\")"))
          (should (equal (expand-file-name "svc.el" dir1)
                         (supervisor--unit-file-path "svc")))
          ;; Add to tier 2 — snapshot is stale, still returns tier 1
          (with-temp-file (expand-file-name "svc.el" dir2)
            (insert "(:id \"svc\" :command \"echo\")"))
          (should (equal (expand-file-name "svc.el" dir1)
                         (supervisor--unit-file-path "svc")))
          ;; Re-publish snapshot — now tier 2 wins
          (supervisor--publish-authority-snapshot)
          (should (equal (expand-file-name "svc.el" dir2)
                         (supervisor--unit-file-path "svc"))))
      (delete-directory dir1 t)
      (delete-directory dir2 t))))

(ert-deftest supervisor-test-unit-file-path-new-unit-targets-highest ()
  "New unit file path targets highest-precedence active root."
  (let* ((dir1 (make-temp-file "tier1-" t))
         (dir2 (make-temp-file "tier2-" t))
         (supervisor-unit-authority-path (list dir1 dir2))
         (supervisor--authority-snapshot nil))
    (unwind-protect
        ;; Non-existent ID should target highest root (dir2)
        (should (equal (expand-file-name "new-svc.el" dir2)
                       (supervisor--unit-file-path "new-svc")))
      (delete-directory dir1 t)
      (delete-directory dir2 t))))

(ert-deftest supervisor-test-snapshot-resolves-mismatched-filename ()
  "Cold-start resolution finds units whose filename differs from :id.
Lazy-init publishes the snapshot automatically, so even with no
prior snapshot, lookup returns the correct root and actual file path."
  (let* ((dir (make-temp-file "tier-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor--authority-snapshot nil)
         (supervisor--programs-cache :not-yet-loaded)
         (supervisor--unit-file-invalid (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; File foo.el contains :id "bar" (filename != :id)
          (with-temp-file (expand-file-name "foo.el" dir)
            (insert "(:id \"bar\" :command \"echo\")"))
          ;; Cold-start: no snapshot yet, but lazy-init resolves correctly
          (should (equal dir (supervisor--authority-root-for-id "bar")))
          (should (equal (expand-file-name "foo.el" dir)
                         (supervisor--unit-file-path "bar"))))
      (delete-directory dir t))))

;;; Authority resolver tests (Phase 2)

(ert-deftest supervisor-test-scan-authority-root-parses-files ()
  "Scanning a root returns candidate structs with metadata."
  (let ((dir (make-temp-file "tier-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "svc.el" dir)
            (insert "(:id \"svc\" :command \"echo\")"))
          (let ((cands (supervisor--scan-authority-root dir 0)))
            (should (= 1 (length cands)))
            (let ((c (car cands)))
              (should (supervisor--authority-candidate-p c))
              (should (equal "svc" (supervisor--authority-candidate-id c)))
              (should (equal dir (supervisor--authority-candidate-root c)))
              (should (= 0 (supervisor--authority-candidate-tier c)))
              (should (supervisor--authority-candidate-valid-p c))
              (should-not (supervisor--authority-candidate-reason c)))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-scan-authority-root-invalid-candidate ()
  "Scanning produces invalid candidate for malformed unit file."
  (let ((dir (make-temp-file "tier-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "bad.el" dir)
            (insert "(:id \"bad\")"))  ; missing :command
          (let* ((cands (supervisor--scan-authority-root dir 0))
                 (c (car cands)))
            (should (= 1 (length cands)))
            (should-not (supervisor--authority-candidate-valid-p c))
            (should (supervisor--authority-candidate-reason c))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-scan-authority-root-lexicographic-order ()
  "Candidates are returned in lexicographic file order."
  (let ((dir (make-temp-file "tier-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "b-svc.el" dir)
            (insert "(:id \"b-svc\" :command \"echo b\")"))
          (with-temp-file (expand-file-name "a-svc.el" dir)
            (insert "(:id \"a-svc\" :command \"echo a\")"))
          (let ((cands (supervisor--scan-authority-root dir 0)))
            (should (= 2 (length cands)))
            (should (equal "a-svc"
                           (supervisor--authority-candidate-id (nth 0 cands))))
            (should (equal "b-svc"
                           (supervisor--authority-candidate-id (nth 1 cands))))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-resolve-authority-higher-tier-wins ()
  "Higher-precedence tier wins for same ID."
  (let* ((dir1 (make-temp-file "tier1-" t))
         (dir2 (make-temp-file "tier2-" t))
         (supervisor-unit-authority-path (list dir1 dir2)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "svc.el" dir1)
            (insert "(:id \"svc\" :command \"echo low\")"))
          (with-temp-file (expand-file-name "svc.el" dir2)
            (insert "(:id \"svc\" :command \"echo high\")"))
          (let* ((result (supervisor--resolve-authority))
                 (winners (plist-get result :winners))
                 (shadowed (plist-get result :shadowed))
                 (winner (gethash "svc" winners)))
            ;; Higher tier wins
            (should (= 1 (supervisor--authority-candidate-tier winner)))
            (should (equal dir2 (supervisor--authority-candidate-root winner)))
            ;; Lower tier is shadowed
            (should (= 1 (length (gethash "svc" shadowed))))))
      (delete-directory dir1 t)
      (delete-directory dir2 t))))

(ert-deftest supervisor-test-resolve-authority-invalid-blocks-fallback ()
  "Invalid unit at highest tier blocks lower-tier valid unit."
  (let* ((dir1 (make-temp-file "tier1-" t))
         (dir2 (make-temp-file "tier2-" t))
         (supervisor-unit-authority-path (list dir1 dir2)))
    (unwind-protect
        (progn
          ;; Valid in lower tier
          (with-temp-file (expand-file-name "svc.el" dir1)
            (insert "(:id \"svc\" :command \"echo good\")"))
          ;; Invalid in higher tier (missing :command)
          (with-temp-file (expand-file-name "svc.el" dir2)
            (insert "(:id \"svc\")"))
          (let* ((result (supervisor--resolve-authority))
                 (winners (plist-get result :winners))
                 (invalid (plist-get result :invalid))
                 (winner (gethash "svc" winners)))
            ;; Higher tier still wins even though invalid
            (should (= 1 (supervisor--authority-candidate-tier winner)))
            (should-not (supervisor--authority-candidate-valid-p winner))
            ;; ID is in invalid hash
            (should (gethash "svc" invalid))))
      (delete-directory dir1 t)
      (delete-directory dir2 t))))

(ert-deftest supervisor-test-resolve-authority-same-tier-duplicate ()
  "Same-tier duplicate: first-seen (lexicographic) wins."
  (let* ((dir (make-temp-file "tier-" t))
         (supervisor-unit-authority-path (list dir)))
    (unwind-protect
        (progn
          ;; Two files with same :id, a-file.el sorts before z-file.el
          (with-temp-file (expand-file-name "a-file.el" dir)
            (insert "(:id \"dup\" :command \"echo first\")"))
          (with-temp-file (expand-file-name "z-file.el" dir)
            (insert "(:id \"dup\" :command \"echo second\")"))
          (let* ((result (supervisor--resolve-authority))
                 (winners (plist-get result :winners))
                 (invalid (plist-get result :invalid))
                 (winner (gethash "dup" winners)))
            ;; First-seen (a-file.el) wins
            (should (string-match "a-file\\.el"
                                  (supervisor--authority-candidate-path winner)))
            ;; Duplicate is in invalid hash
            (should (string-match "duplicate"
                                  (gethash "dup" invalid)))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-resolve-authority-disjoint-ids ()
  "Units with different IDs across tiers all appear as winners."
  (let* ((dir1 (make-temp-file "tier1-" t))
         (dir2 (make-temp-file "tier2-" t))
         (supervisor-unit-authority-path (list dir1 dir2)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "a.el" dir1)
            (insert "(:id \"a\" :command \"echo a\")"))
          (with-temp-file (expand-file-name "b.el" dir2)
            (insert "(:id \"b\" :command \"echo b\")"))
          (let* ((result (supervisor--resolve-authority))
                 (winners (plist-get result :winners)))
            (should (gethash "a" winners))
            (should (gethash "b" winners))
            (should (= 2 (hash-table-count winners)))))
      (delete-directory dir1 t)
      (delete-directory dir2 t))))

(ert-deftest supervisor-test-resolve-authority-empty-roots ()
  "Resolver with no active roots returns empty results."
  (let ((supervisor-unit-authority-path nil))
    (let* ((result (supervisor--resolve-authority))
           (winners (plist-get result :winners)))
      (should (= 0 (hash-table-count winners))))))

(ert-deftest supervisor-test-resolve-authority-three-tiers ()
  "Three-tier resolution: highest tier always wins."
  (let* ((dir1 (make-temp-file "tier1-" t))
         (dir2 (make-temp-file "tier2-" t))
         (dir3 (make-temp-file "tier3-" t))
         (supervisor-unit-authority-path (list dir1 dir2 dir3)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "svc.el" dir1)
            (insert "(:id \"svc\" :command \"echo t1\")"))
          (with-temp-file (expand-file-name "svc.el" dir2)
            (insert "(:id \"svc\" :command \"echo t2\")"))
          (with-temp-file (expand-file-name "svc.el" dir3)
            (insert "(:id \"svc\" :command \"echo t3\")"))
          (let* ((result (supervisor--resolve-authority))
                 (winners (plist-get result :winners))
                 (shadowed (plist-get result :shadowed))
                 (winner (gethash "svc" winners)))
            ;; Tier 3 (index 2) wins
            (should (= 2 (supervisor--authority-candidate-tier winner)))
            ;; Two shadowed entries
            (should (= 2 (length (gethash "svc" shadowed))))))
      (delete-directory dir1 t)
      (delete-directory dir2 t)
      (delete-directory dir3 t))))

(ert-deftest supervisor-test-resolve-authority-invalid-clears-lower-invalid ()
  "Higher tier overriding clears lower-tier invalid entry for same ID."
  (let* ((dir1 (make-temp-file "tier1-" t))
         (dir2 (make-temp-file "tier2-" t))
         (supervisor-unit-authority-path (list dir1 dir2)))
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
          (let* ((result (supervisor--resolve-authority))
                 (winners (plist-get result :winners))
                 (invalid (plist-get result :invalid))
                 (winner (gethash "svc" winners)))
            ;; Higher tier wins and is valid
            (should (= 1 (supervisor--authority-candidate-tier winner)))
            (should (supervisor--authority-candidate-valid-p winner))
            ;; No invalid entry for this ID (cleared by higher tier)
            (should-not (gethash "svc" invalid))))
      (delete-directory dir1 t)
      (delete-directory dir2 t))))

(ert-deftest supervisor-test-unit-file-keywords-include-command ()
  "Unit-file valid keywords include :command."
  (should (memq :command supervisor--unit-file-keywords))
  (should (memq :id supervisor--unit-file-keywords))
  (should (memq :type supervisor--unit-file-keywords)))

(ert-deftest supervisor-test-load-unit-file-valid ()
  "Loading a valid unit file returns (LINE . PLIST)."
  (let ((path (make-temp-file "unit-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "(:id \"test\" :command \"echo hello\" :type simple)"))
          (let* ((result (supervisor--load-unit-file path))
                 (line (car result))
                 (plist (cdr result)))
            (should (integerp line))
            (should (equal "test" (plist-get plist :id)))
            (should (equal "echo hello" (plist-get plist :command)))
            (should (eq 'simple (plist-get plist :type)))))
      (delete-file path))))

(ert-deftest supervisor-test-load-unit-file-line-number ()
  "Line number reflects plist position after leading whitespace."
  (let ((path (make-temp-file "unit-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "\n\n(:id \"test\" :command \"echo\")"))
          (let* ((result (supervisor--load-unit-file path))
                 (line (car result)))
            (should (= 3 line))))
      (delete-file path))))

(ert-deftest supervisor-test-load-unit-file-missing ()
  "Loading a missing unit file signals error with path:line context."
  (let ((err (should-error
              (supervisor--load-unit-file "/tmp/nonexistent-unit-file.el")
              :type 'error)))
    (should (string-match "/tmp/nonexistent-unit-file\\.el:1:"
                          (error-message-string err)))))

(ert-deftest supervisor-test-load-unit-file-empty ()
  "Loading an empty unit file signals error with path:line context."
  (let ((path (make-temp-file "unit-test-" nil ".el")))
    (unwind-protect
        (let ((err (should-error (supervisor--load-unit-file path)
                                 :type 'error)))
          (should (string-match (regexp-quote (concat path ":1:"))
                                (error-message-string err))))
      (delete-file path))))

(ert-deftest supervisor-test-load-unit-file-not-plist ()
  "Loading a non-plist unit file signals error with path:line context."
  (let ((path (make-temp-file "unit-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "(defun foo () nil)"))
          (let ((err (should-error (supervisor--load-unit-file path)
                                   :type 'error)))
            (should (string-match (regexp-quote path)
                                  (error-message-string err)))
            (should (string-match ":[0-9]+:" (error-message-string err)))))
      (delete-file path))))

(ert-deftest supervisor-test-load-unit-file-trailing-content ()
  "Loading a unit file with trailing content signals error with path:line."
  (let ((path (make-temp-file "unit-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "(:id \"test\" :command \"echo\")\n(extra stuff)"))
          (let ((err (should-error (supervisor--load-unit-file path)
                                   :type 'error)))
            (should (string-match (regexp-quote path)
                                  (error-message-string err)))
            (should (string-match ":[0-9]+:" (error-message-string err)))))
      (delete-file path))))

(ert-deftest supervisor-test-validate-unit-file-missing-id ()
  "Unit file plist without :id is invalid with path:line context."
  (let ((reason (supervisor--validate-unit-file-plist
                 '(:command "echo") "/tmp/test.el" 1)))
    (should (stringp reason))
    (should (string-match "missing :id" reason))
    (should (string-match "/tmp/test\\.el:1:" reason))))

(ert-deftest supervisor-test-validate-unit-file-missing-command ()
  "Unit file plist without :command is invalid with path:line context."
  (let ((reason (supervisor--validate-unit-file-plist
                 '(:id "test") "/tmp/test.el" 3)))
    (should (stringp reason))
    (should (string-match "missing :command" reason))
    (should (string-match "/tmp/test\\.el:3:" reason))))

(ert-deftest supervisor-test-validate-unit-file-unknown-keyword ()
  "Unit file plist with unknown keyword is invalid with path:line context."
  (let ((reason (supervisor--validate-unit-file-plist
                 '(:id "test" :command "echo" :bogus 42) "/tmp/test.el" 1)))
    (should (stringp reason))
    (should (string-match "unknown keyword" reason))
    (should (string-match "/tmp/test\\.el:1:" reason))))

(ert-deftest supervisor-test-validate-unit-file-valid ()
  "Valid unit file plist returns nil."
  (should-not (supervisor--validate-unit-file-plist
               '(:id "test" :command "echo" :type simple)
               "/tmp/test.el" 1)))

(ert-deftest supervisor-test-unit-file-to-program-entry ()
  "Unit file plist converts to program entry format."
  (let ((entry (supervisor--unit-file-to-program-entry
                '(:id "nm" :command "nm-applet" :type simple :restart t))))
    ;; Car is the command string
    (should (equal "nm-applet" (car entry)))
    ;; Rest is a plist with :id but no :command
    (should (equal "nm" (plist-get (cdr entry) :id)))
    (should (eq 'simple (plist-get (cdr entry) :type)))
    (should (eq t (plist-get (cdr entry) :restart)))
    ;; :command should not be in the output plist
    (should-not (plist-member (cdr entry) :command))))

(ert-deftest supervisor-test-load-all-unit-files-missing-dir ()
  "Loading from nonexistent directory returns nil."
  (let ((supervisor-unit-authority-path
         '("/tmp/nonexistent-supervisor-units-dir"))
        (supervisor-unit-directory "/tmp/nonexistent-supervisor-units-dir"))
    (should-not (supervisor--load-all-unit-files))))

(ert-deftest supervisor-test-load-all-unit-files-alphabetical ()
  "Unit files are loaded in alphabetical order as plists."
  (let ((dir (make-temp-file "units-" t)))
    (unwind-protect
        (let ((supervisor-unit-authority-path (list dir))
              (supervisor-unit-directory dir))
          (with-temp-file (expand-file-name "bravo.el" dir)
            (insert "(:id \"bravo\" :command \"bravo-cmd\")"))
          (with-temp-file (expand-file-name "alpha.el" dir)
            (insert "(:id \"alpha\" :command \"alpha-cmd\")"))
          (let ((results (supervisor--load-all-unit-files)))
            (should (= 2 (length results)))
            ;; Alpha first (alphabetical); results are plists directly
            (should (equal "alpha" (plist-get (nth 0 results) :id)))
            (should (equal "bravo" (plist-get (nth 1 results) :id)))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-load-all-unit-files-invalid-tracked ()
  "Invalid unit files are recorded in supervisor--unit-file-invalid."
  (let ((dir (make-temp-file "units-" t)))
    (unwind-protect
        (let ((supervisor-unit-authority-path (list dir))
              (supervisor-unit-directory dir))
          ;; Valid file
          (with-temp-file (expand-file-name "good.el" dir)
            (insert "(:id \"good\" :command \"echo ok\")"))
          ;; Invalid file (missing :command)
          (with-temp-file (expand-file-name "bad.el" dir)
            (insert "(:id \"bad\")"))
          (let ((results (supervisor--load-all-unit-files)))
            ;; Only valid entries returned
            (should (= 1 (length results)))
            (should (equal "good" (plist-get (nth 0 results) :id)))
            ;; Invalid tracked in hash with path:line context
            (should (= 1 (hash-table-count supervisor--unit-file-invalid)))
            (let ((reason (gethash "bad" supervisor--unit-file-invalid)))
              (should (stringp reason))
              (should (string-match "missing :command" reason))
              (should (string-match ":[0-9]+:" reason)))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-load-all-unit-files-load-error-tracked ()
  "Unit files that fail to parse are recorded in invalid hash with path:line."
  (let ((dir (make-temp-file "units-" t)))
    (unwind-protect
        (let ((supervisor-unit-authority-path (list dir))
              (supervisor-unit-directory dir))
          (with-temp-file (expand-file-name "broken.el" dir)
            (insert "not a plist at all"))
          (let ((results (supervisor--load-all-unit-files)))
            (should (= 0 (length results)))
            (should (= 1 (hash-table-count supervisor--unit-file-invalid)))
            (let ((reason (gethash "broken" supervisor--unit-file-invalid)))
              (should (stringp reason))
              (should (string-match ":[0-9]+:" reason)))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-unit-file-invalid-merged-into-plan ()
  "Invalid unit files appear in supervisor--invalid after plan build."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-unit-directory dir)
         (supervisor--programs-cache :not-yet-loaded)
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
         (supervisor--computed-deps (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "bad.el" dir)
            (insert "(:id \"bad-unit\")"))
          (let ((plan (supervisor--build-plan (supervisor--effective-programs))))
            (maphash (lambda (k v) (puthash k v supervisor--invalid))
                     (supervisor-plan-invalid plan))
            (supervisor--merge-unit-file-invalid)
            ;; The invalid unit file should now be visible
            (should (gethash "bad-unit" supervisor--invalid))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-load-programs-empty-directory ()
  "With no unit files present, returns empty list."
  (supervisor-test-without-builtins
    (let* ((dir (make-temp-file "units-" t))
           (supervisor-unit-authority-path (list dir))
           (supervisor-unit-directory dir)
           (supervisor--programs-cache :not-yet-loaded))
      (unwind-protect
          (should (null (supervisor--effective-programs)))
        (delete-directory dir t)))))

(ert-deftest supervisor-test-load-programs-unit-file-merge ()
  "Unit file entries are loaded from disk via effective-programs."
  (supervisor-test-without-builtins
    (let* ((dir (make-temp-file "units-" t))
           (supervisor-unit-authority-path (list dir))
           (supervisor-unit-directory dir)
           (supervisor--programs-cache :not-yet-loaded))
      (unwind-protect
          (progn
            (with-temp-file (expand-file-name "svc.el" dir)
              (insert "(:id \"svc\" :command \"new-cmd\" :type simple)"))
            (let ((programs (supervisor--effective-programs)))
              ;; Should have exactly one entry (unit-file wins)
              (should (= 1 (length programs)))
              ;; The command should be the unit-file version
              (should (equal "new-cmd" (car (nth 0 programs))))))
        (delete-directory dir t)))))

(ert-deftest supervisor-test-load-programs-unit-file-adds-new ()
  "Multiple unit files are loaded and returned."
  (supervisor-test-without-builtins
    (let* ((dir (make-temp-file "units-" t))
           (supervisor-unit-authority-path (list dir))
           (supervisor-unit-directory dir)
           (supervisor--programs-cache :not-yet-loaded))
      (unwind-protect
          (progn
            (with-temp-file (expand-file-name "alpha.el" dir)
              (insert "(:id \"alpha\" :command \"alpha-cmd\" :type simple)"))
            (with-temp-file (expand-file-name "beta.el" dir)
              (insert "(:id \"beta\" :command \"beta-cmd\" :type simple)"))
            (let ((programs (supervisor--effective-programs)))
              ;; Should have both entries (alphabetical order)
              (should (= 2 (length programs)))
              (should (equal "alpha-cmd" (car (nth 0 programs))))
              (should (equal "beta-cmd" (car (nth 1 programs))))))
        (delete-directory dir t)))))

(ert-deftest supervisor-test-loader-cross-tier-precedence ()
  "Loader returns higher-tier unit when same ID exists in multiple roots."
  (supervisor-test-without-builtins
    (let* ((dir1 (make-temp-file "tier1-" t))
           (dir2 (make-temp-file "tier2-" t))
           (supervisor-unit-authority-path (list dir1 dir2))
           (supervisor-unit-directory dir2)
           (supervisor--programs-cache :not-yet-loaded))
      (unwind-protect
          (progn
            (with-temp-file (expand-file-name "svc.el" dir1)
              (insert "(:id \"svc\" :command \"low-cmd\" :type simple)"))
            (with-temp-file (expand-file-name "svc.el" dir2)
              (insert "(:id \"svc\" :command \"high-cmd\" :type simple)"))
            ;; effective-programs should return the higher-tier version
            (let ((programs (supervisor--effective-programs)))
              (should (= 1 (length programs)))
              (should (equal "high-cmd" (car (nth 0 programs))))))
        (delete-directory dir1 t)
        (delete-directory dir2 t)))))

(ert-deftest supervisor-test-loader-invalid-blocks-lower-tier ()
  "Loader blocks lower-tier valid unit when higher tier is invalid."
  (supervisor-test-without-builtins
    (let* ((dir1 (make-temp-file "tier1-" t))
           (dir2 (make-temp-file "tier2-" t))
           (supervisor-unit-authority-path (list dir1 dir2))
           (supervisor-unit-directory dir2)
           (supervisor--programs-cache :not-yet-loaded))
      (unwind-protect
          (progn
            ;; Valid in lower tier
            (with-temp-file (expand-file-name "svc.el" dir1)
              (insert "(:id \"svc\" :command \"good-cmd\")"))
            ;; Invalid in higher tier (missing :command)
            (with-temp-file (expand-file-name "svc.el" dir2)
              (insert "(:id \"svc\")"))
            (let ((programs (supervisor--effective-programs)))
              ;; No valid programs - higher tier blocks
              (should (= 0 (length programs)))
              ;; Invalid hash should record it
              (should (gethash "svc" supervisor--unit-file-invalid))))
        (delete-directory dir1 t)
        (delete-directory dir2 t)))))

(ert-deftest supervisor-test-loader-snapshot-consistency ()
  "Authority snapshot is consistent with loader output."
  (supervisor-test-without-builtins
    (let* ((dir (make-temp-file "tier-" t))
           (supervisor-unit-authority-path (list dir))
           (supervisor-unit-directory dir)
           (supervisor--programs-cache :not-yet-loaded))
      (unwind-protect
          (progn
            (with-temp-file (expand-file-name "a.el" dir)
              (insert "(:id \"a\" :command \"echo a\")"))
            (with-temp-file (expand-file-name "b.el" dir)
              (insert "(:id \"b\" :command \"echo b\")"))
            (let ((programs (supervisor--effective-programs)))
              (should (= 2 (length programs)))
              ;; Snapshot should exist and match
              (should supervisor--authority-snapshot)
            (let ((winners (plist-get supervisor--authority-snapshot :winners)))
              (should (= 2 (hash-table-count winners)))
              (should (gethash "a" winners))
              (should (gethash "b" winners)))))
        (delete-directory dir t)))))

(ert-deftest supervisor-test-dashboard-shows-invalid-authority-units ()
  "Dashboard entries include invalid units from authority resolution."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-unit-directory dir)
         (supervisor--programs-cache :not-yet-loaded)
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; Valid unit
          (with-temp-file (expand-file-name "good.el" dir)
            (insert "(:id \"good\" :command \"echo ok\")"))
          ;; Invalid unit (missing :command)
          (with-temp-file (expand-file-name "bad.el" dir)
            (insert "(:id \"bad\")"))
          (let* ((programs (supervisor--effective-programs))
                 (snapshot (supervisor--build-snapshot))
                 (entries (supervisor--get-entries snapshot programs)))
            ;; Should have entries for both good and bad (typed IDs)
            (should (cl-some (lambda (e) (equal (cons :service "good") (car e))) entries))
            (should (cl-some (lambda (e) (equal (cons :service "bad") (car e))) entries))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-health-summary-counts-invalid-authority ()
  "Health summary counts invalid authority units."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-unit-directory dir)
         (supervisor--programs-cache :not-yet-loaded)
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (supervisor--failed (make-hash-table :test 'equal))
         (supervisor--oneshot-completed (make-hash-table :test 'equal))
         (supervisor--invalid (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "bad.el" dir)
            (insert "(:id \"bad\")"))
          (let* ((programs (supervisor--effective-programs))
                 (snapshot (supervisor--build-snapshot))
                 (summary (supervisor--health-summary snapshot programs)))
            ;; Summary should show 1 invalid
            (should (string-match "1 inv" summary))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-dashboard-single-snapshot-refresh ()
  "Dashboard refresh uses single authority snapshot (no re-publish)."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-unit-directory dir)
         (supervisor--programs-cache :not-yet-loaded)
         (publish-count 0))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "svc.el" dir)
            (insert "(:id \"svc\" :command \"echo\")"))
          ;; Count publish calls during effective-programs
          (cl-letf (((symbol-function 'supervisor--resolve-authority)
                     (let ((orig (symbol-function
                                  'supervisor--resolve-authority)))
                       (lambda ()
                         (cl-incf publish-count)
                         (funcall orig)))))
            (let ((programs (supervisor--effective-programs)))
              ;; One publish for loading programs
              (should (= 1 publish-count))
              ;; Passing programs to get-entries and health-summary
              ;; should NOT trigger additional publishes
              (let ((snapshot (supervisor--build-snapshot)))
                (supervisor--get-entries snapshot programs)
                (supervisor--health-summary snapshot programs)
                (should (= 1 publish-count))))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-programs-cache-isolates-disk-changes ()
  "Programs cache prevents disk changes from leaking without daemon-reload."
  (supervisor-test-without-builtins
    (let* ((dir (make-temp-file "cache-" t))
           (supervisor-unit-authority-path (list dir))
           (supervisor-unit-directory dir)
           (supervisor--programs-cache :not-yet-loaded)
           (supervisor--authority-snapshot nil)
           (supervisor--unit-file-invalid (make-hash-table :test 'equal)))
      (unwind-protect
          (progn
            ;; Create one unit and load
            (with-temp-file (expand-file-name "a.el" dir)
              (insert "(:id \"a\" :command \"echo a\")"))
            (let ((programs (supervisor--effective-programs)))
              (should (= 1 (length programs))))
            ;; Add a second unit on disk WITHOUT refreshing
            (with-temp-file (expand-file-name "b.el" dir)
              (insert "(:id \"b\" :command \"echo b\")"))
            ;; Cache should still return only 1 program
            (let ((programs (supervisor--effective-programs)))
              (should (= 1 (length programs))))
            ;; Explicit refresh (simulates daemon-reload) picks up the new unit
            (supervisor--refresh-programs)
            (let ((programs (supervisor--effective-programs)))
              (should (= 2 (length programs)))))
        (delete-directory dir t)))))

(ert-deftest supervisor-test-start-path-publishes-authority-snapshot ()
  "The `supervisor-start' path refreshes the programs cache from disk."
  (supervisor-test-without-builtins
    (let* ((dir (make-temp-file "start-" t))
           (supervisor-unit-authority-path (list dir))
           (supervisor-unit-directory dir)
           (supervisor--programs-cache :not-yet-loaded)
           (supervisor--authority-snapshot nil)
           (supervisor--unit-file-invalid (make-hash-table :test 'equal))
           (publish-count 0))
      (unwind-protect
          (progn
            (with-temp-file (expand-file-name "svc.el" dir)
              (insert "(:id \"svc\" :command \"echo ok\")"))
            ;; Count authority resolutions during refresh-programs
            (cl-letf (((symbol-function 'supervisor--resolve-authority)
                       (let ((orig (symbol-function
                                    'supervisor--resolve-authority)))
                         (lambda ()
                           (cl-incf publish-count)
                           (funcall orig)))))
              ;; Simulate what supervisor-start does: refresh then read
              (supervisor--refresh-programs)
              (should (= 1 publish-count))
              ;; Subsequent reads use cache (no extra publish)
              (let ((programs (supervisor--effective-programs)))
                (should (= 1 (length programs)))
                (should (= 1 publish-count)))))
        (delete-directory dir t)))))

(ert-deftest supervisor-test-three-tier-loader-precedence ()
  "Loader returns highest-tier unit across a three-tier authority path."
  (supervisor-test-with-authority-tiers 3
    (with-temp-file (expand-file-name "svc.el" dir1)
      (insert "(:id \"svc\" :command \"vendor-cmd\")"))
    (with-temp-file (expand-file-name "svc.el" dir2)
      (insert "(:id \"svc\" :command \"admin-cmd\")"))
    (with-temp-file (expand-file-name "svc.el" dir3)
      (insert "(:id \"svc\" :command \"user-cmd\")"))
    (let ((programs (supervisor--effective-programs)))
      (should (= 1 (length programs)))
      (should (equal "user-cmd" (car (nth 0 programs)))))))

(ert-deftest supervisor-test-authority-tiers-macro-cleanup ()
  "Authority tiers macro creates and cleans up temp directories."
  (let (saved-dirs)
    (supervisor-test-with-authority-tiers 2
      (setq saved-dirs (list dir1 dir2))
      (should (file-directory-p dir1))
      (should (file-directory-p dir2))
      (should (= 2 (length supervisor-unit-authority-path))))
    ;; Directories should be cleaned up after the macro
    (should-not (file-directory-p (nth 0 saved-dirs)))
    (should-not (file-directory-p (nth 1 saved-dirs)))))

(ert-deftest supervisor-test-mixed-tier-disjoint-and-override ()
  "Disjoint IDs coexist while shared IDs follow tier precedence."
  (supervisor-test-with-authority-tiers 2
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
    (let ((programs (supervisor--effective-programs)))
      ;; 3 programs: a (low), b (high), shared (high wins)
      (should (= 3 (length programs)))
      (let ((cmds (mapcar #'car programs)))
        (should (member "low-a" cmds))
        (should (member "high-b" cmds))
        (should (member "high-shared" cmds))
        (should-not (member "low-shared" cmds))))))

(ert-deftest supervisor-test-reordered-root-list-flips-winner ()
  "User-configured root order determines precedence, not directory names.
Reversing the authority path order flips which root wins for a
conflicting ID, proving precedence derives from list position."
  (supervisor-test-without-builtins
    (let* ((dir-a (make-temp-file "rootA-" t))
           (dir-b (make-temp-file "rootB-" t))
           (supervisor--programs-cache :not-yet-loaded)
           (supervisor--authority-snapshot nil)
           (supervisor--unit-file-invalid (make-hash-table :test 'equal)))
      (unwind-protect
          (progn
            ;; Same ID in both roots, different commands
            (with-temp-file (expand-file-name "svc.el" dir-a)
              (insert "(:id \"svc\" :command \"from-a\")"))
            (with-temp-file (expand-file-name "svc.el" dir-b)
              (insert "(:id \"svc\" :command \"from-b\")"))
            ;; Order A, B: B is last = highest precedence
            (let ((supervisor-unit-authority-path (list dir-a dir-b)))
              (supervisor--publish-authority-snapshot)
              (setq supervisor--programs-cache :not-yet-loaded)
              (let ((programs (supervisor--effective-programs)))
                (should (= 1 (length programs)))
                (should (equal "from-b" (caar programs)))))
            ;; Reverse to B, A: A is last = highest precedence
            (let ((supervisor-unit-authority-path (list dir-b dir-a)))
              (setq supervisor--authority-snapshot nil)
              (setq supervisor--programs-cache :not-yet-loaded)
              (let ((programs (supervisor--effective-programs)))
                (should (= 1 (length programs)))
                (should (equal "from-a" (caar programs))))))
        (delete-directory dir-a t)
        (delete-directory dir-b t)))))

(ert-deftest supervisor-test-cli-verify-invalid-unit-file ()
  "CLI verify reports invalid unit files in count and output."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-unit-directory dir)
         (supervisor--programs-cache :not-yet-loaded))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "bad.el" dir)
            (insert "(:id \"bad-unit\")"))
          (let ((result (supervisor--cli-dispatch '("verify"))))
            (should (supervisor-cli-result-p result))
            (should (= supervisor-cli-exit-validation-failed
                        (supervisor-cli-result-exitcode result)))
            (should (string-match "1 invalid"
                                  (supervisor-cli-result-output result)))
            (should (string-match "bad-unit"
                                  (supervisor-cli-result-output result)))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-cli-list-units-shows-invalid-unit-file ()
  "CLI list-units includes invalid unit files in output."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-unit-directory dir)
         (supervisor--programs-cache :not-yet-loaded)
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "bad.el" dir)
            (insert "(:id \"bad-unit\")"))
          (let ((result (supervisor--cli-dispatch '("list-units"))))
            (should (supervisor-cli-result-p result))
            (should (string-match "bad-unit"
                                  (supervisor-cli-result-output result)))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-cli-status-invalid-unit-file ()
  "CLI status ID recognizes invalid unit files, not just plan-level invalids."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-unit-directory dir)
         (supervisor--programs-cache :not-yet-loaded)
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "bad.el" dir)
            (insert "(:id \"bad-unit\")"))
          (let ((result (supervisor--cli-dispatch '("status" "bad-unit"))))
            (should (supervisor-cli-result-p result))
            ;; Should find it as invalid, not "not found"
            (should (= supervisor-cli-exit-success
                        (supervisor-cli-result-exitcode result)))
            (should (string-match "invalid"
                                  (supervisor-cli-result-output result)))
            (should-not (string-match "could not be found"
                                      (supervisor-cli-result-output result)))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-dry-run-shows-invalid-unit-files ()
  "Dry-run summary includes invalid unit files in count and listing."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-unit-directory dir)
         (supervisor--programs-cache :not-yet-loaded)
         (supervisor--invalid (make-hash-table :test 'equal))
         (supervisor--cycle-fallback-ids (make-hash-table :test 'equal))
         (supervisor--computed-deps (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "bad.el" dir)
            (insert "(:id \"bad-unit\")"))
          (supervisor-dry-run)
          (with-current-buffer "*supervisor-dry-run*"
            (let ((content (buffer-string)))
              (should (string-match "1 invalid" content))
              (should (string-match "bad-unit" content)))))
      (delete-directory dir t))))

;;; Unit-File Scaffold and Edit/Cat Tests

(ert-deftest supervisor-test-unit-file-scaffold-contains-id ()
  "Scaffold template contains the given ID."
  (let ((scaffold (supervisor--unit-file-scaffold "my-svc")))
    (should (stringp scaffold))
    (should (string-match ":id \"my-svc\"" scaffold))
    (should (string-match ":command" scaffold))))

(ert-deftest supervisor-test-unit-file-scaffold-is-valid-plist ()
  "Scaffold template can be read as a valid plist (with command filled)."
  (let* ((scaffold (supervisor--unit-file-scaffold "test-svc"))
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

(ert-deftest supervisor-test-seed-default-maintenance-units-creates-files ()
  "Default maintenance seeding creates logrotate and log-prune units."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-logrotate-command "/usr/bin/rotate-stub")
         (supervisor-log-prune-command "/usr/bin/prune-stub")
         (supervisor-log-directory "/tmp/sv-test-logs")
         (supervisor-logrotate-keep-days 9)
         (supervisor-log-prune-max-total-bytes 12345))
    (unwind-protect
        (progn
          (supervisor--ensure-default-maintenance-units)
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

(ert-deftest supervisor-test-seed-default-maintenance-units-does-not-overwrite-existing ()
  "Maintenance seeding preserves existing units while seeding missing ones."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-logrotate-command "/usr/bin/rotate-stub")
         (supervisor-log-prune-command "/usr/bin/prune-stub")
         (supervisor-log-directory "/tmp/sv-test-logs")
         (supervisor-logrotate-keep-days 14)
         (supervisor-log-prune-max-total-bytes 999))
    (unwind-protect
        (let ((rotate-file (expand-file-name "logrotate.el" dir)))
          (write-region "(:id \"logrotate\" :command \"echo custom\")\n"
                        nil rotate-file nil 'silent)
          (supervisor--ensure-default-maintenance-units)
          (with-temp-buffer
            (insert-file-contents rotate-file)
            (should (string-match-p "echo custom" (buffer-string))))
          (should (file-exists-p (expand-file-name "log-prune.el" dir))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-seed-skips-masked-units ()
  "Seeding does not create units that are masked via runtime override."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-logrotate-command "/usr/bin/rotate-stub")
         (supervisor-log-prune-command "/usr/bin/prune-stub")
         (supervisor-log-directory "/tmp/sv-test-logs")
         (supervisor-logrotate-keep-days 14)
         (supervisor-log-prune-max-total-bytes 999)
         (supervisor--mask-override (make-hash-table :test 'equal)))
    (puthash "logrotate" 'masked supervisor--mask-override)
    (unwind-protect
        (progn
          (supervisor--ensure-default-maintenance-units)
          ;; logrotate masked: should NOT be seeded
          (should-not (file-exists-p
                       (expand-file-name "logrotate.el" dir)))
          ;; log-prune not masked: should be seeded
          (should (file-exists-p
                   (expand-file-name "log-prune.el" dir))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-seed-skips-disabled-units ()
  "Seeding does not create units that are disabled via runtime override."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-logrotate-command "/usr/bin/rotate-stub")
         (supervisor-log-prune-command "/usr/bin/prune-stub")
         (supervisor-log-directory "/tmp/sv-test-logs")
         (supervisor-logrotate-keep-days 14)
         (supervisor-log-prune-max-total-bytes 999)
         (supervisor--mask-override (make-hash-table :test 'equal))
         (supervisor--enabled-override (make-hash-table :test 'equal)))
    (puthash "log-prune" 'disabled supervisor--enabled-override)
    (unwind-protect
        (progn
          (supervisor--ensure-default-maintenance-units)
          ;; logrotate not disabled: should be seeded
          (should (file-exists-p
                   (expand-file-name "logrotate.el" dir)))
          ;; log-prune disabled: should NOT be seeded
          (should-not (file-exists-p
                       (expand-file-name "log-prune.el" dir))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-cli-cat-existing-unit-file ()
  "CLI cat outputs raw content of existing unit file."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-unit-directory dir))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "my-svc.el" dir)
            (insert "(:id \"my-svc\" :command \"echo ok\")"))
          (let ((result (supervisor--cli-dispatch '("cat" "my-svc"))))
            (should (supervisor-cli-result-p result))
            (should (= supervisor-cli-exit-success
                        (supervisor-cli-result-exitcode result)))
            (should (string-match ":id \"my-svc\""
                                  (supervisor-cli-result-output result)))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-cli-cat-missing-unit-file ()
  "CLI cat returns error for missing unit file."
  (let ((supervisor-unit-authority-path '("/tmp/nonexistent-supervisor-units-dir"))
        (supervisor-unit-directory "/tmp/nonexistent-supervisor-units-dir"))
    (let ((result (supervisor--cli-dispatch '("cat" "nope"))))
      (should (supervisor-cli-result-p result))
      (should (= supervisor-cli-exit-failure
                  (supervisor-cli-result-exitcode result)))
      (should (string-match "not found"
                            (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-cat-builtin-no-unit-file ()
  "CLI cat explains built-in entries without a backing unit file."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-unit-directory dir)
         (supervisor--authority-snapshot nil)
         (supervisor--programs-cache :not-yet-loaded)
         (supervisor--unit-file-invalid (make-hash-table :test 'equal)))
    (unwind-protect
        (let ((result (supervisor--cli-dispatch '("cat" "logrotate"))))
          (should (supervisor-cli-result-p result))
          (should (= supervisor-cli-exit-failure
                     (supervisor-cli-result-exitcode result)))
          (should (string-match
                   "No unit file on disk for 'logrotate'"
                   (supervisor-cli-result-output result))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-cli-cat-no-args ()
  "CLI cat requires an ID argument."
  (let ((result (supervisor--cli-dispatch '("cat"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args
                (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-cat-too-many-args ()
  "CLI cat takes exactly one ID."
  (let ((result (supervisor--cli-dispatch '("cat" "a" "b"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args
                (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-cat-json-format ()
  "CLI cat --json outputs path and content fields."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-unit-directory dir))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "svc.el" dir)
            (insert "(:id \"svc\" :command \"echo\")"))
          (let ((result (supervisor--cli-dispatch '("cat" "svc" "--json"))))
            (should (supervisor-cli-result-p result))
            (should (= supervisor-cli-exit-success
                        (supervisor-cli-result-exitcode result)))
            (should (eq 'json (supervisor-cli-result-format result)))
            (let ((parsed (json-read-from-string
                           (supervisor-cli-result-output result))))
              (should (assoc 'path parsed))
              (should (assoc 'content parsed))
              (should (string-match ":id" (cdr (assoc 'content parsed)))))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-cli-edit-creates-scaffold ()
  "CLI edit creates scaffold template for missing unit file."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-unit-directory dir))
    (unwind-protect
        ;; Use JSON mode to test scaffold creation without editor launch
        (let ((result (supervisor--cli-dispatch '("edit" "new-svc" "--json"))))
          (should (supervisor-cli-result-p result))
          (should (= supervisor-cli-exit-success
                      (supervisor-cli-result-exitcode result)))
          ;; File should have been created
          (let ((path (expand-file-name "new-svc.el" dir)))
            (should (file-exists-p path))
            ;; Content should have the ID
            (with-temp-buffer
              (insert-file-contents path)
              (should (string-match ":id \"new-svc\""
                                    (buffer-string))))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-cli-edit-existing-file ()
  "CLI edit on existing file does not overwrite it."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-unit-directory dir))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "svc.el" dir)
            (insert "(:id \"svc\" :command \"existing\")"))
          ;; Use JSON mode to test without editor launch
          (let ((result (supervisor--cli-dispatch '("edit" "svc" "--json"))))
            (should (supervisor-cli-result-p result))
            (should (= supervisor-cli-exit-success
                        (supervisor-cli-result-exitcode result)))
            ;; File content should be unchanged
            (with-temp-buffer
              (insert-file-contents (expand-file-name "svc.el" dir))
              (should (string-match "existing" (buffer-string))))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-cli-edit-json-format ()
  "CLI edit --json outputs path and created flag."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-unit-directory dir))
    (unwind-protect
        (let ((result (supervisor--cli-dispatch '("edit" "new" "--json"))))
          (should (supervisor-cli-result-p result))
          (should (= supervisor-cli-exit-success
                      (supervisor-cli-result-exitcode result)))
          (should (eq 'json (supervisor-cli-result-format result)))
          (let ((parsed (json-read-from-string
                         (supervisor-cli-result-output result))))
            (should (assoc 'path parsed))
            (should (eq t (cdr (assoc 'created parsed))))
            ;; Root should be reported even for new units
            (should (stringp (cdr (assoc 'root parsed))))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-cli-edit-json-includes-authority-root ()
  "CLI edit --json output includes authority root and tier."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-unit-directory dir)
         (supervisor--authority-snapshot nil)
         (supervisor--programs-cache :not-yet-loaded))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "svc.el" dir)
            (insert "(:id \"svc\" :command \"echo\")"))
          ;; Publish snapshot so tier lookup works
          (supervisor--publish-authority-snapshot)
          (let* ((result (supervisor--cli-dispatch '("edit" "svc" "--json")))
                 (parsed (json-read-from-string
                          (supervisor-cli-result-output result))))
            (should (equal dir (cdr (assoc 'root parsed))))
            (should (equal 0 (cdr (assoc 'tier parsed))))
            (should (eq :json-false (cdr (assoc 'created parsed))))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-cli-status-shows-unit-file-path ()
  "CLI status shows unit-file path in detailed output."
  (supervisor-test-with-unit-files
      '(("echo ok" :id "svc" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("status" "svc"))))
      (should (string-match "Unit file:" (supervisor-cli-result-output result))))))

(ert-deftest supervisor-test-cli-status-json-includes-unit-file ()
  "CLI status --json output includes unit_file field."
  (supervisor-test-with-unit-files
      '(("echo ok" :id "svc" :type simple))
    (let* ((supervisor--processes (make-hash-table :test 'equal))
           (supervisor--entry-state (make-hash-table :test 'equal))
           (result (supervisor--cli-dispatch '("status" "svc" "--json")))
           (parsed (json-read-from-string
                    (supervisor-cli-result-output result)))
           (entries (cdr (assoc 'entries parsed)))
           (first-entry (aref entries 0)))
      (should (assoc 'unit_file first-entry))
      (should (stringp (cdr (assoc 'unit_file first-entry)))))))

(ert-deftest supervisor-test-cli-status-builtin-omits-unit-file-line ()
  "CLI status omits unit-file line for built-ins without disk units."
  (let* ((dir (make-temp-file "units-" t))
         (logs (make-temp-file "logs-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-unit-directory dir)
         (supervisor-log-directory logs)
         (supervisor--authority-snapshot nil)
         (supervisor--programs-cache :not-yet-loaded)
         (supervisor--unit-file-invalid (make-hash-table :test 'equal))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (result (supervisor--cli-dispatch '("status" "logrotate"))))
    (unwind-protect
        (progn
          (should (= supervisor-cli-exit-success
                     (supervisor-cli-result-exitcode result)))
          (should-not (string-match "Unit file:"
                                    (supervisor-cli-result-output result))))
      (delete-directory logs t)
      (delete-directory dir t))))

(ert-deftest supervisor-test-cli-status-json-builtin-unit-file-null ()
  "CLI status --json sets unit_file to null for fileless built-ins."
  (let* ((dir (make-temp-file "units-" t))
         (logs (make-temp-file "logs-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-unit-directory dir)
         (supervisor-log-directory logs)
         (supervisor--authority-snapshot nil)
         (supervisor--programs-cache :not-yet-loaded)
         (supervisor--unit-file-invalid (make-hash-table :test 'equal))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (result (supervisor--cli-dispatch '("status" "logrotate" "--json"))))
    (unwind-protect
        (let* ((parsed (json-read-from-string
                        (supervisor-cli-result-output result)))
               (entries (cdr (assoc 'entries parsed)))
               (first-entry (and (> (length entries) 0)
                                 (aref entries 0))))
          (should (= supervisor-cli-exit-success
                     (supervisor-cli-result-exitcode result)))
          (should first-entry)
          (should (assoc 'unit_file first-entry))
          (should-not (cdr (assoc 'unit_file first-entry))))
      (delete-directory logs t)
      (delete-directory dir t))))

(ert-deftest supervisor-test-cli-edit-no-args ()
  "CLI edit requires an ID argument."
  (let ((result (supervisor--cli-dispatch '("edit"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args
                (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-cli-edit-too-many-args ()
  "CLI edit takes exactly one ID."
  (let ((result (supervisor--cli-dispatch '("edit" "a" "b"))))
    (should (supervisor-cli-result-p result))
    (should (= supervisor-cli-exit-invalid-args
                (supervisor-cli-result-exitcode result)))))

(ert-deftest supervisor-test-dashboard-cat-keybinding ()
  "Dashboard keymap binds `t' to proced."
  (should (eq #'proced
              (lookup-key supervisor-dashboard-mode-map "t"))))

(ert-deftest supervisor-test-dashboard-edit-keybinding ()
  "Dashboard keymap binds `F' to tag filter."
  (should (eq #'supervisor-dashboard-cycle-tag-filter
              (lookup-key supervisor-dashboard-mode-map "F"))))

(ert-deftest supervisor-test-dashboard-cat-rejects-separator ()
  "Dashboard cat rejects separator rows."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list '--services-- (vector "" "" "" "" "" "" "" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should-error (supervisor-dashboard-cat)
                    :type 'user-error))))

(ert-deftest supervisor-test-dashboard-cat-rejects-timer ()
  "Dashboard cat rejects timer rows."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list "t1" (vector "t1" "timer" "-" "yes"
                                    "waiting" "---" "---" "-" "-")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should-error (supervisor-dashboard-cat)
                    :type 'user-error))))

(ert-deftest supervisor-test-dashboard-cat-builtin-no-unit-file ()
  "Dashboard cat explains fileless built-in entries."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-unit-directory dir)
         (supervisor--authority-snapshot nil)
         (supervisor--programs-cache :not-yet-loaded)
         (supervisor--unit-file-invalid (make-hash-table :test 'equal)))
    (unwind-protect
        (with-temp-buffer
          (supervisor-dashboard-mode)
          (let ((tabulated-list-entries
                 (list (list (cons :service "logrotate")
                             (vector "logrotate" "oneshot" "-"
                                     "yes" "pending" "n/a" "yes" "-" "-")))))
            (tabulated-list-init-header)
            (tabulated-list-print)
            (goto-char (point-min))
            (let ((err (should-error (supervisor-dashboard-cat) :type 'user-error)))
              (should (string-match-p
                       "No unit file on disk for logrotate"
                       (error-message-string err))))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-dashboard-edit-rejects-separator ()
  "Dashboard edit rejects separator rows."
  (with-temp-buffer
    (supervisor-dashboard-mode)
    (let ((tabulated-list-entries
           (list (list '--services-- (vector "" "" "" "" "" "" "" "" "")))))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (should-error (supervisor-dashboard-edit)
                    :type 'user-error))))

(ert-deftest supervisor-test-cli-edit-launches-editor ()
  "CLI edit launches $VISUAL/$EDITOR via `call-process'."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-unit-directory dir)
         (launch-args nil))
    (unwind-protect
        (cl-letf (((symbol-function 'supervisor--cli-edit-launch-editor)
                   (lambda (editor path)
                     (setq launch-args (list editor path))
                     0)))
          (let* ((process-environment
                  (cons "VISUAL=my-editor" process-environment))
                 (result (supervisor--cli-dispatch '("edit" "new-svc"))))
            (should (supervisor-cli-result-p result))
            (should (= supervisor-cli-exit-success
                        (supervisor-cli-result-exitcode result)))
            ;; Editor should have been called
            (should launch-args)
            (should (equal "my-editor" (car launch-args)))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-cli-edit-editor-failure ()
  "CLI edit reports failure when editor exits non-zero."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-unit-directory dir))
    (unwind-protect
        (cl-letf (((symbol-function 'supervisor--cli-edit-launch-editor)
                   (lambda (_editor _path) 1)))
          (let* ((process-environment
                  (cons "VISUAL=my-editor" process-environment))
                 (result (supervisor--cli-dispatch '("edit" "new-svc"))))
            (should (supervisor-cli-result-p result))
            (should (= supervisor-cli-exit-failure
                        (supervisor-cli-result-exitcode result)))
            (should (string-match "exited with status 1"
                                  (supervisor-cli-result-output result)))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-cli-edit-no-editor-error ()
  "CLI edit returns error when no $VISUAL or $EDITOR is set."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-unit-directory dir))
    (unwind-protect
        (let* ((process-environment
                (cl-remove-if (lambda (e)
                                (or (string-prefix-p "VISUAL=" e)
                                    (string-prefix-p "EDITOR=" e)))
                              process-environment))
               (result (supervisor--cli-dispatch '("edit" "new-svc"))))
          (should (supervisor-cli-result-p result))
          (should (= supervisor-cli-exit-failure
                      (supervisor-cli-result-exitcode result)))
          (should (string-match "No \\$VISUAL or \\$EDITOR"
                                (supervisor-cli-result-output result))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-dashboard-edit-enables-edit-mode ()
  "Dashboard edit activates `supervisor-edit-mode' with return-on-q."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-unit-directory dir)
         (path (expand-file-name "test-svc.el" dir)))
    (unwind-protect
        (progn
          (write-region "(:id \"test-svc\" :command \"echo\")" nil path)
          ;; Simulate dashboard with a valid entry
          (with-temp-buffer
            (supervisor-dashboard-mode)
            (let ((tabulated-list-entries
                   (list (list (cons :service "test-svc")
                               (vector "test-svc" "simple" "-"
                                       "yes" "running" "yes" "---"
                                       "-" "-")))))
              (tabulated-list-init-header)
              (tabulated-list-print)
              (goto-char (point-min))
              (supervisor-dashboard-edit)
              ;; Should have opened the file with edit mode
              (let ((edit-buf (get-file-buffer path)))
                (should edit-buf)
                (with-current-buffer edit-buf
                  (should supervisor-edit-mode)
                  ;; q should be bound
                  (should (eq #'supervisor-edit-quit
                              (lookup-key supervisor-edit-mode-map "q")))
                  ;; C-c C-q should be bound
                  (should (eq #'supervisor-edit-finish
                              (lookup-key supervisor-edit-mode-map
                                          (kbd "C-c C-q")))))
                (kill-buffer edit-buf)))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-edit-quit-returns ()
  "Pressing `q' in edit buffer returns to dashboard."
  (let* ((dir (make-temp-file "units-" t))
         (path (expand-file-name "svc.el" dir))
         (returned nil))
    (unwind-protect
        (progn
          (write-region "(:id \"svc\" :command \"echo\")" nil path)
          (find-file path)
          (supervisor-edit-mode 1)
          (cl-letf (((symbol-function 'supervisor--return-to-dashboard)
                     (lambda () (setq returned t)))
                    ((symbol-function 'y-or-n-p) (lambda (_) nil)))
            ;; Buffer is unmodified, q should quit
            (should-not (buffer-modified-p))
            (supervisor-edit-quit)
            (should returned)))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest supervisor-test-dashboard-edit-save-hook-fires ()
  "Dashboard edit save-hook wiring fires validator on save."
  (let* ((dir (make-temp-file "units-" t))
         (supervisor-unit-authority-path (list dir))
         (supervisor-unit-directory dir)
         (path (expand-file-name "hook-svc.el" dir))
         (validated nil))
    (unwind-protect
        (progn
          (write-region "(:id \"hook-svc\" :command \"echo\")" nil path)
          (with-temp-buffer
            (supervisor-dashboard-mode)
            (let ((tabulated-list-entries
                   (list (list (cons :service "hook-svc")
                               (vector "hook-svc" "simple" "-"
                                       "yes" "running" "yes" "---"
                                       "-" "-")))))
              (tabulated-list-init-header)
              (tabulated-list-print)
              (goto-char (point-min))
              (supervisor-dashboard-edit)
              (let ((edit-buf (get-file-buffer path)))
                (should edit-buf)
                (with-current-buffer edit-buf
                  ;; Validator should be in after-save-hook
                  (should (memq #'supervisor--validate-unit-file-buffer
                                after-save-hook))
                  ;; Mock validator to track invocation
                  (cl-letf (((symbol-function
                              'supervisor--validate-unit-file-buffer)
                             (lambda () (setq validated t))))
                    ;; Modify and save to trigger hook
                    (goto-char (point-max))
                    (insert " ")
                    (save-buffer)))
                (should validated)
                (kill-buffer edit-buf)))))
      (delete-directory dir t))))

(ert-deftest supervisor-test-validate-unit-file-buffer-hook ()
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
            (supervisor--validate-unit-file-buffer)))
      (delete-directory dir t))))

;;; Schema v1 Tests

(ert-deftest supervisor-test-service-schema-version ()
  "Service schema version constant is defined."
  (should (= supervisor-service-schema-version 1)))

(ert-deftest supervisor-test-service-struct-fields ()
  "Service struct has all expected fields."
  (let ((service (supervisor-service--create
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
    (should (equal "test" (supervisor-service-id service)))
    (should (equal "sleep 100" (supervisor-service-command service)))
    (should (eq 'simple (supervisor-service-type service)))
    (should (= 5 (supervisor-service-delay service)))
    (should (supervisor-service-enabled service))
    (should (eq 'no (supervisor-service-restart service)))
    (should (supervisor-service-logging service))
    (should (equal '("dep1") (supervisor-service-after service)))
    (should (equal '("req1") (supervisor-service-requires service)))
    (should-not (supervisor-service-oneshot-blocking service))
    (should (= 60 (supervisor-service-oneshot-timeout service)))
    (should (equal '(tag1 tag2) (supervisor-service-tags service)))))

(ert-deftest supervisor-test-entry-accessors ()
  "Entry accessor functions work correctly."
  (let ((entry (supervisor--parse-entry
                '("sleep 100" :id "test" :type simple                  :delay 5 :restart nil :after ("dep1") :requires ("req1")))))
    (should (equal "test" (supervisor-entry-id entry)))
    (should (equal "sleep 100" (supervisor-entry-command entry)))
    (should (eq 'simple (supervisor-entry-type entry)))
    (should (= 5 (supervisor-entry-delay entry)))
    (should-not (supervisor-entry-restart-p entry))
    (should (equal '("dep1") (supervisor-entry-after entry)))
    (should (equal '("req1") (supervisor-entry-requires entry)))))

(ert-deftest supervisor-test-entry-to-service-conversion ()
  "Entry to service conversion preserves all fields."
  (let* ((entry (supervisor--parse-entry
                 '("sleep 100" :id "test" :type oneshot                   :after ("dep") :requires ("req") :tags (t1 t2))))
         (service (supervisor-entry-to-service entry)))
    (should (equal "test" (supervisor-service-id service)))
    (should (equal "sleep 100" (supervisor-service-command service)))
    (should (eq 'oneshot (supervisor-service-type service)))
    (should (equal '("dep") (supervisor-service-after service)))
    (should (equal '("req") (supervisor-service-requires service)))
    (should (equal '(t1 t2) (supervisor-service-tags service)))))

(ert-deftest supervisor-test-service-to-entry-conversion ()
  "Service to entry conversion preserves all fields."
  (let* ((service (supervisor-service--create
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
         (entry (supervisor-service-to-entry service)))
    (should (equal "test" (supervisor-entry-id entry)))
    (should (equal "sleep 100" (supervisor-entry-command entry)))
    (should (eq 'oneshot (supervisor-entry-type entry)))
    (should (= 10 (supervisor-entry-delay entry)))
    (should-not (supervisor-entry-enabled-p entry))
    (should (supervisor-entry-restart-p entry))
    (should-not (supervisor-entry-logging-p entry))
    (should (equal '("dep") (supervisor-entry-after entry)))
    (should (equal '("req") (supervisor-entry-requires entry)))
    (should (supervisor-entry-oneshot-blocking entry))
    (should (= 120 (supervisor-entry-oneshot-timeout entry)))
    (should (equal '(t1) (supervisor-entry-tags entry)))))

;;; Dependency Semantics Tests

(ert-deftest supervisor-test-requires-keyword-valid ()
  ":requires keyword is accepted as valid."
  (should-not (supervisor--validate-entry
               '("foo" :type simple :requires "bar"))))

(ert-deftest supervisor-test-requires-parsed-correctly ()
  ":requires is parsed into entry correctly."
  (let ((entry (supervisor--parse-entry
                '("foo" :requires ("bar" "baz")))))
    (should (equal '("bar" "baz") (supervisor-entry-requires entry)))))

(ert-deftest supervisor-test-requires-missing-id-warned ()
  "Missing :requires target is warned and dropped."
  (let* ((logged nil)
         (programs '(("sleep 100" :id "a")
                     ("sleep 200" :id "b" :requires "nonexistent"))))
    (cl-letf (((symbol-function 'supervisor--log)
               (lambda (_level fmt &rest args)
                 (push (apply #'format fmt args) logged))))
      (let ((plan (supervisor--build-plan programs)))
        (should (cl-some (lambda (m) (string-match-p "does not exist" m))
                         logged))))))

(ert-deftest supervisor-test-requires-combined-with-after ()
  ":requires and :after are combined for topological sort."
  (let* ((programs '(("sleep 100" :id "a")
                     ("sleep 200" :id "b" :after "a")
                     ("sleep 300" :id "c" :requires "a")))
         (plan (supervisor--build-plan programs))
         (sorted (supervisor-plan-by-target plan))
         (ids (mapcar #'car sorted)))
    ;; a should come before both b and c
    (should (< (cl-position "a" ids :test #'equal)
               (cl-position "b" ids :test #'equal)))
    (should (< (cl-position "a" ids :test #'equal)
               (cl-position "c" ids :test #'equal)))))

(ert-deftest supervisor-test-plan-requires-deps-populated ()
  "Plan includes separate requires-deps hash."
  (let* ((programs '(("sleep 100" :id "a")
                     ("sleep 200" :id "b" :requires "a")))
         (plan (supervisor--build-plan programs)))
    (should (supervisor-plan-requires-deps plan))
    (should (equal '("a") (gethash "b" (supervisor-plan-requires-deps plan))))))

;;; Persistent Overrides Tests

(ert-deftest supervisor-test-overrides-file-path ()
  "Overrides file path respects custom setting."
  (let ((supervisor-overrides-file "/custom/path/overrides.eld"))
    (should (equal "/custom/path/overrides.eld"
                   (supervisor--overrides-file-path))))
  (let ((supervisor-overrides-file nil))
    (should-not (supervisor--overrides-file-path))))

(ert-deftest supervisor-test-overrides-to-alist ()
  "Overrides are correctly collected into an alist."
  (let ((supervisor--enabled-override (make-hash-table :test 'equal))
        (supervisor--restart-override (make-hash-table :test 'equal))
        (supervisor--logging (make-hash-table :test 'equal)))
    (puthash "a" 'enabled supervisor--enabled-override)
    (puthash "b" 'no supervisor--restart-override)
    (puthash "a" 'enabled supervisor--logging)
    (let ((alist (supervisor--overrides-to-alist)))
      (should (= 2 (length alist)))
      (let ((a-entry (cdr (assoc "a" alist)))
            (b-entry (cdr (assoc "b" alist))))
        (should (eq 'enabled (plist-get a-entry :enabled)))
        (should (eq 'enabled (plist-get a-entry :logging)))
        (should (eq 'no (plist-get b-entry :restart)))))))

(ert-deftest supervisor-test-overrides-save-load-roundtrip ()
  "Overrides survive save and load roundtrip."
  (let* ((temp-file (make-temp-file "supervisor-test-overrides-" nil ".eld"))
         (supervisor-overrides-file temp-file)
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--restart-override (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal))
         (supervisor--overrides-loaded nil))
    (unwind-protect
        (progn
          ;; Set some overrides
          (puthash "test-entry" 'disabled supervisor--enabled-override)
          (puthash "test-entry" 'always supervisor--restart-override)
          ;; Save
          (should (supervisor--save-overrides))
          ;; Clear memory
          (clrhash supervisor--enabled-override)
          (clrhash supervisor--restart-override)
          ;; Load
          (should (supervisor--load-overrides))
          ;; Verify
          (should (eq 'disabled (gethash "test-entry" supervisor--enabled-override)))
          (should (eq 'always (gethash "test-entry" supervisor--restart-override))))
      (delete-file temp-file))))

(ert-deftest supervisor-test-overrides-corrupt-file-handled ()
  "Corrupt overrides file is handled gracefully."
  (let* ((temp-file (make-temp-file "supervisor-test-corrupt-" nil ".eld"))
         (supervisor-overrides-file temp-file)
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--restart-override (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; Write corrupt data
          (with-temp-file temp-file
            (insert "this is not valid lisp (((("))
          ;; Load should return nil but not error
          (should-not (supervisor--load-overrides))
          ;; File should still exist (not deleted)
          (should (file-exists-p temp-file)))
      (delete-file temp-file))))

(ert-deftest supervisor-test-cli-disable-loads-overrides-before-save ()
  "Disable CLI command preserves existing overrides on first save."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "svc1" :type simple)
        ("sleep 300" :id "svc2" :type simple))
    (let* ((temp-file (make-temp-file "supervisor-test-cli-load-" nil ".eld"))
           (supervisor-overrides-file temp-file)
           (supervisor--enabled-override (make-hash-table :test 'equal))
           (supervisor--restart-override (make-hash-table :test 'equal))
           (supervisor--logging (make-hash-table :test 'equal))
           (supervisor--mask-override (make-hash-table :test 'equal))
           (supervisor--overrides-loaded nil))
      (unwind-protect
          (progn
            ;; Seed file with an existing override for svc1.
            (with-temp-file temp-file
              (insert ";; Supervisor overrides file - do not edit manually\n")
              (insert ";; Schema version: 1\n")
              (pp '((version . 1)
                    (timestamp . "2026-02-15T00:00:00+0000")
                    (overrides ("svc1" :enabled disabled)))
                  (current-buffer)))
            (let ((result (supervisor--cli-dispatch '("disable" "svc2"))))
              (should (= supervisor-cli-exit-success
                         (supervisor-cli-result-exitcode result))))
            ;; Reload from disk and verify both overrides survive.
            (clrhash supervisor--enabled-override)
            (clrhash supervisor--restart-override)
            (clrhash supervisor--logging)
            (clrhash supervisor--mask-override)
            (setq supervisor--overrides-loaded nil)
            (should (supervisor--load-overrides))
            (should (eq 'disabled (gethash "svc1" supervisor--enabled-override)))
            (should (eq 'disabled (gethash "svc2" supervisor--enabled-override))))
        (delete-file temp-file)))))

(ert-deftest supervisor-test-cli-disable-does-not-overwrite-corrupt-overrides ()
  "Disable CLI command refuses to overwrite a corrupt overrides file."
  (supervisor-test-with-unit-files
      '(("sleep 300" :id "svc" :type simple))
    (let* ((temp-file (make-temp-file "supervisor-test-cli-corrupt-" nil ".eld"))
           (supervisor-overrides-file temp-file)
           (supervisor--enabled-override (make-hash-table :test 'equal))
           (supervisor--restart-override (make-hash-table :test 'equal))
           (supervisor--logging (make-hash-table :test 'equal))
           (supervisor--mask-override (make-hash-table :test 'equal))
           (supervisor--overrides-loaded nil))
      (unwind-protect
          (progn
            (with-temp-file temp-file
              (insert "this is not valid lisp (((("))
            (let ((result (supervisor--cli-dispatch '("disable" "svc"))))
              (should (= supervisor-cli-exit-failure
                         (supervisor-cli-result-exitcode result))))
            (should-not (gethash "svc" supervisor--enabled-override))
            (with-temp-buffer
              (insert-file-contents temp-file)
              (should (string-match-p "not valid lisp" (buffer-string)))))
        (delete-file temp-file)))))

;;; Migration Tests

(ert-deftest supervisor-test-migrate-string-entry ()
  "Migration handles simple string entries."
  (let ((result (supervisor--migrate-entry-to-plist "nm-applet")))
    ;; Simple string with defaults should remain a string
    (should (stringp result))
    (should (equal "nm-applet" result))))

(ert-deftest supervisor-test-migrate-plist-entry ()
  "Migration handles plist entries."
  (let ((result (supervisor--migrate-entry-to-plist
                 '("sleep 100" :type oneshot))))
    (should (listp result))
    (should (equal "sleep 100" (car result)))
    (should (eq 'oneshot (plist-get (cdr result) :type)))))

(ert-deftest supervisor-test-migrate-all-entries-skips-invalid ()
  "Migration skips invalid entries with reason."
  (supervisor-test-with-unit-files
      '(("valid-cmd" :id "valid" :type simple)
        ("invalid-cmd" :id "invalid" :type "bad"))
    (let ((result (supervisor--migrate-all-entries)))
      (should (= 1 (length (plist-get result :migrated))))
      ;; Invalid entry is caught at unit-file validation (delegation)
      ;; and tracked in supervisor--unit-file-invalid
      (should (gethash "invalid" supervisor--unit-file-invalid)))))

(ert-deftest supervisor-test-migrate-all-entries-skips-duplicates ()
  "Migration sees only one entry when unit-file loader deduplicates."
  (supervisor-test-without-builtins
    (let* ((dir (make-temp-file "units-" t))
           (supervisor-unit-authority-path (list dir))
           (supervisor-unit-directory dir)
           (supervisor--programs-cache :not-yet-loaded)
           (supervisor--unit-file-invalid (make-hash-table :test 'equal)))
      ;; Two unit files with the same :id but different filenames
      (with-temp-file (expand-file-name "test-a.el" dir)
        (insert "(:id \"test\" :command \"a\" :type simple)"))
      (with-temp-file (expand-file-name "test-b.el" dir)
        (insert "(:id \"test\" :command \"b\" :type oneshot)"))
      (unwind-protect
          (let ((result (supervisor--migrate-all-entries)))
            ;; Unit-file loader deduplicates, so migration sees only one
            (should (= 1 (length (plist-get result :migrated))))
            ;; No skipped entries at migration level (dedup happened upstream)
            (should (= 0 (length (plist-get result :skipped)))))
        (delete-directory dir t)))))

(ert-deftest supervisor-test-migrate-entry-to-service ()
  "High-level migration function works."
  (let ((service (supervisor-migrate-entry-to-service
                  '("sleep 100" :id "test" :type oneshot))))
    (should (supervisor-service-p service))
    (should (equal "test" (supervisor-service-id service)))
    (should (eq 'oneshot (supervisor-service-type service)))))

(ert-deftest supervisor-test-migrate-entry-to-service-invalid-errors ()
  "High-level migration function signals error for invalid entry."
  (should-error
   (supervisor-migrate-entry-to-service '("foo" :type "bad"))
   :type 'error))

;;; Bug fix verification tests

(ert-deftest supervisor-test-validate-after-must-be-string-or-list ()
  "Validation rejects non-string/list :after values."
  ;; Valid cases
  (should-not (supervisor--validate-entry '("foo" :after "bar")))
  (should-not (supervisor--validate-entry '("foo" :after ("a" "b"))))
  (should-not (supervisor--validate-entry '("foo" :after nil)))
  ;; Invalid cases
  (should (string-match ":after must be"
                        (supervisor--validate-entry '("foo" :after 123))))
  (should (string-match ":after must be"
                        (supervisor--validate-entry '("foo" :after (1 2 3))))))

(ert-deftest supervisor-test-validate-requires-must-be-string-or-list ()
  "Validation rejects non-string/list :requires values."
  ;; Valid cases
  (should-not (supervisor--validate-entry '("foo" :requires "bar")))
  (should-not (supervisor--validate-entry '("foo" :requires ("a" "b"))))
  (should-not (supervisor--validate-entry '("foo" :requires nil)))
  ;; Invalid cases
  (should (string-match ":requires must be"
                        (supervisor--validate-entry '("foo" :requires 123))))
  (should (string-match ":requires must be"
                        (supervisor--validate-entry '("foo" :requires (1 2 3))))))

(ert-deftest supervisor-test-requires-valid-ordering ()
  "Entries with :requires are valid and ordered."
  (let* ((programs '(("a" :id "a")
                     ("b" :id "b" :requires "a")))
         (plan (supervisor--build-plan programs))
         (sorted (supervisor-plan-by-target plan)))
    ;; Both should be valid
    (should-not (gethash "b" (supervisor-plan-invalid plan)))
    ;; Both should appear in sorted entries
    (should (cl-find "a" sorted :key #'supervisor-entry-id :test #'equal))
    (should (cl-find "b" sorted :key #'supervisor-entry-id :test #'equal))))

(ert-deftest supervisor-test-dag-uses-requires-edges ()
  "DAG scheduler uses :requires edges for in-degree calculation."
  (let* ((entries '(("a" "sleep 1" 0 t always t simple nil t 30 nil nil)
                    ("b" "sleep 1" 0 t always t simple nil t 30 nil ("a"))))
         ;; Entry "b" has :requires "a" at index 12
         (supervisor--dag-in-degree (make-hash-table :test 'equal))
         (supervisor--dag-dependents (make-hash-table :test 'equal))
         (supervisor--dag-entries (make-hash-table :test 'equal))
         (supervisor--dag-blocking (make-hash-table :test 'equal))
         (supervisor--dag-started (make-hash-table :test 'equal))
         (supervisor--dag-ready (make-hash-table :test 'equal))
         (supervisor--dag-timeout-timers (make-hash-table :test 'equal))
         (supervisor--dag-delay-timers (make-hash-table :test 'equal))
         (supervisor--dag-id-to-index (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal)))
    (supervisor--dag-init entries)
    ;; "a" has no deps, in-degree should be 0
    (should (= 0 (gethash "a" supervisor--dag-in-degree)))
    ;; "b" has :requires "a", in-degree should be 1
    (should (= 1 (gethash "b" supervisor--dag-in-degree)))
    ;; "a" should have "b" as dependent
    (should (member "b" (gethash "a" supervisor--dag-dependents)))))

(ert-deftest supervisor-test-override-load-clears-stale-state ()
  "Loading overrides clears stale in-memory state not present in file."
  (let* ((temp-file (make-temp-file "supervisor-test-stale-" nil ".eld"))
         (supervisor-overrides-file temp-file)
         (supervisor--enabled-override (make-hash-table :test 'equal))
         (supervisor--restart-override (make-hash-table :test 'equal))
         (supervisor--logging (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; First, save a file with only "file-entry"
          (puthash "file-entry" 'enabled supervisor--enabled-override)
          (supervisor--save-overrides)
          ;; Clear and add stale state that's NOT in the file
          (clrhash supervisor--enabled-override)
          (clrhash supervisor--restart-override)
          (clrhash supervisor--logging)
          (puthash "stale-entry" 'disabled supervisor--enabled-override)
          (puthash "stale-entry" 'no supervisor--restart-override)
          (puthash "stale-entry" 'disabled supervisor--logging)
          (puthash "another-stale" 'enabled supervisor--enabled-override)
          ;; Load from file - should clear stale state and restore file state
          (supervisor--load-overrides)
          ;; Stale entries should be gone (not in file)
          (should-not (gethash "stale-entry" supervisor--enabled-override))
          (should-not (gethash "another-stale" supervisor--enabled-override))
          (should-not (gethash "stale-entry" supervisor--restart-override))
          (should-not (gethash "stale-entry" supervisor--logging))
          ;; File entry should be restored
          (should (eq 'enabled (gethash "file-entry" supervisor--enabled-override))))
      (delete-file temp-file))))

(ert-deftest supervisor-test-duplicate-id-invalid-does-not-poison-valid ()
  "Valid entry is not poisoned by later invalid duplicate with same ID."
  (let* ((programs '(("cmd" :id "test" :type simple)      ; valid first
                     ("cmd" :id "test" :type "bad")))     ; invalid duplicate
         (plan (supervisor--build-plan programs)))
    ;; The valid entry should be in plan.entries
    (should (= 1 (length (supervisor-plan-entries plan))))
    (should (equal "test" (supervisor-entry-id (car (supervisor-plan-entries plan)))))
    ;; The ID should NOT be in the invalid hash (first valid wins)
    (should-not (gethash "test" (supervisor-plan-invalid plan)))))

(ert-deftest supervisor-test-duplicate-id-invalid-first-valid-later ()
  "First valid occurrence wins even when first occurrence is invalid."
  (let* ((programs '(("cmd" :id "test" :type "bad")       ; invalid first
                     ("cmd" :id "test" :type simple)))    ; valid later
         (plan (supervisor--build-plan programs)))
    ;; The valid entry should be in plan.entries
    (should (= 1 (length (supervisor-plan-entries plan))))
    (should (equal "test" (supervisor-entry-id (car (supervisor-plan-entries plan)))))
    ;; The ID should NOT be in the invalid hash (valid cleared stale invalid)
    (should-not (gethash "test" (supervisor-plan-invalid plan)))))

(ert-deftest supervisor-test-requires-included-in-plan-entries ()
  "Entries with :requires are included in plan.entries."
  (let* ((programs '(("a" :id "a")
                     ("b" :id "b" :requires "a")))
         (plan (supervisor--build-plan programs)))
    ;; Both should be valid and in plan.entries
    (should-not (gethash "b" (supervisor-plan-invalid plan)))
    (should (cl-find "b" (supervisor-plan-entries plan)
                     :key #'supervisor-entry-id :test #'equal))))

(ert-deftest supervisor-test-validate-dotted-list-after ()
  "Validation handles dotted lists in :after without error."
  ;; Dotted list should return validation error, not signal
  (let ((result (supervisor--validate-entry '("foo" :after (a . b)))))
    (should (stringp result))
    (should (string-match ":after must be" result))))

(ert-deftest supervisor-test-validate-dotted-list-requires ()
  "Validation handles dotted lists in :requires without error."
  ;; Dotted list should return validation error, not signal
  (let ((result (supervisor--validate-entry '("foo" :requires (x . y)))))
    (should (stringp result))
    (should (string-match ":requires must be" result))))

(ert-deftest supervisor-test-empty-programs-no-by-target ()
  "Empty program list produces empty by-target."
  (let* ((programs nil)
         (plan (supervisor--build-plan programs)))
    (should (null (supervisor-plan-by-target plan)))))


(provide 'supervisor-test-units)
;;; supervisor-test-units.el ends here
