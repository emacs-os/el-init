;;; supervisor-units.el --- Unit-file support for supervisor.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; Author: telecommuter <telecommuter@riseup.net>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of supervisor.el.

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

;; Modular unit-file support for supervisor.el.
;; Each unit is a single `.el' file containing a plist expression.
;; Run M-x supervisor-handbook for full documentation.

;;; Code:

(require 'cl-lib)

;; Forward declarations for supervisor-core functions
(declare-function supervisor--log "supervisor-core" (level format-string &rest args))
(declare-function supervisor--plist-duplicate-keys "supervisor-core" (plist))
;;; Customization

(defcustom supervisor-unit-authority-path
  (list "/usr/lib/supervisor.el/"
        "/etc/supervisor.el/"
        (expand-file-name "supervisor.el/"
                          (or (getenv "XDG_CONFIG_HOME")
                              (expand-file-name "~/.config"))))
  "Ordered list of authority roots for unit-file resolution.
Roots are listed from lowest to highest precedence.  When a unit
ID exists in multiple roots, the highest-precedence root wins
completely (no key-level merge).  Non-existent roots are silently
skipped.

Each entry is a directory path.  The canonical directory basename
is `supervisor.el/'.  Unit files are `*.el' files within each root.

Default tiers (low to high):
  Tier 1: /usr/lib/supervisor.el/   (vendor units)
  Tier 2: /etc/supervisor.el/       (system admin units)
  Tier 3: ~/.config/supervisor.el/  (user units)"
  :type '(repeat directory)
  :group 'supervisor)

(defcustom supervisor-unit-directory
  (expand-file-name "supervisor/units/"
                    (or (getenv "XDG_CONFIG_HOME")
                        (expand-file-name "~/.config")))
  "Directory containing unit files.
Each unit is a single `.el' file with a plist expression.
Deprecated: use `supervisor-unit-authority-path' instead."
  :type 'directory
  :group 'supervisor)

;;; Authority Root Resolution

;; Forward declaration: defined below in "Authority Snapshot" section.
(defvar supervisor--authority-snapshot)

(defun supervisor--active-authority-roots ()
  "Return the list of active authority roots.
Filter `supervisor-unit-authority-path' to only existing directories,
preserving the configured order (low to high precedence)."
  (cl-remove-if-not #'file-directory-p supervisor-unit-authority-path))

(defun supervisor--authority-root-for-id (id)
  "Return the highest-precedence authority root containing a unit for ID.
Ensure the authority snapshot is published (lazy-init if needed) and
look up the winning candidate.  Return nil if no root contains ID."
  (let* ((snap (supervisor--ensure-authority-snapshot))
         (winner (gethash id (plist-get snap :winners))))
    (when winner
      (supervisor--authority-candidate-root winner))))

(defun supervisor--authority-tier-for-id (id)
  "Return the tier index of the winning authority candidate for ID.
Ensure the authority snapshot is published (lazy-init if needed).
Return nil if ID is not found.  Tier 0 is the lowest-precedence root."
  (let* ((snap (supervisor--ensure-authority-snapshot))
         (winner (gethash id (plist-get snap :winners))))
    (when winner
      (supervisor--authority-candidate-tier winner))))

;;; Authority Candidate Structure

(cl-defstruct (supervisor--authority-candidate (:constructor supervisor--make-candidate))
  "A unit-file candidate discovered during authority resolution."
  (id nil :documentation "Unit ID string.")
  (plist nil :documentation "Parsed unit-file plist.")
  (path nil :documentation "Absolute file path.")
  (root nil :documentation "Authority root directory.")
  (tier nil :documentation "Tier index (position in authority path, 0-based).")
  (valid-p nil :documentation "Non-nil when the candidate passed validation.")
  (reason nil :documentation "Validation failure reason, or nil if valid."))

;;; Authority Root Scanning

(defun supervisor--scan-authority-root (root tier)
  "Scan ROOT at TIER index and return a list of candidates.
Parse and validate each `*.el' file in ROOT.  Return a list of
`supervisor--authority-candidate' structs in deterministic
lexicographic order."
  (let ((files (sort (directory-files root t "\\.el\\'" t) #'string<))
        (candidates nil))
    (dolist (path files)
      (condition-case err
          (let* ((line-and-plist (supervisor--load-unit-file path))
                 (line (car line-and-plist))
                 (plist (cdr line-and-plist))
                 (reason (supervisor--validate-unit-file-plist
                          plist path line))
                 (id (or (plist-get plist :id)
                         (file-name-sans-extension
                          (file-name-nondirectory path)))))
            (push (supervisor--make-candidate
                   :id id :plist plist :path path
                   :root root :tier tier
                   :valid-p (not reason) :reason reason)
                  candidates))
        (error
         (let ((id (file-name-sans-extension
                    (file-name-nondirectory path))))
           (push (supervisor--make-candidate
                  :id id :plist nil :path path
                  :root root :tier tier
                  :valid-p nil
                  :reason (error-message-string err))
                 candidates)))))
    (nreverse candidates)))

;;; Authority Resolver

(defun supervisor--resolve-authority ()
  "Resolve unit-file authority across all active roots.
Scan roots from low to high precedence.  For each ID, the
highest-tier candidate wins completely.  Same-tier duplicate IDs
use first-seen (lexicographic) with later duplicates marked
invalid.  Invalid winners block lower-tier fallback.

Return a plist with keys:
  :winners   - hash table of ID to winning candidate
  :invalid   - hash table of ID to reason string
  :shadowed  - hash table of ID to list of shadowed candidates"
  (let ((roots (supervisor--active-authority-roots))
        (winners (make-hash-table :test 'equal))
        (invalid (make-hash-table :test 'equal))
        (shadowed (make-hash-table :test 'equal))
        (tier 0))
    (dolist (root roots)
      (let ((candidates (supervisor--scan-authority-root root tier)))
        (dolist (cand candidates)
          (let* ((id (supervisor--authority-candidate-id cand))
                 (existing (gethash id winners)))
            (cond
             ;; Same-tier duplicate: first-seen wins, mark duplicate invalid
             ((and existing
                   (= (supervisor--authority-candidate-tier existing)
                      (supervisor--authority-candidate-tier cand)))
              (supervisor--log
               'warning "Duplicate ID '%s' in same tier %s, keeping %s"
               id root (supervisor--authority-candidate-path existing))
              (puthash id
                       (format "%s: duplicate ID '%s' in same authority root"
                               (supervisor--authority-candidate-path cand) id)
                       invalid)
              (push cand (gethash id shadowed)))
             ;; Higher tier overrides lower: push old winner to shadowed
             (existing
              (push existing (gethash id shadowed))
              (puthash id cand winners)
              ;; Clear any invalid from lower tier for this ID since
              ;; authority has moved up
              (remhash id invalid))
             ;; First occurrence of this ID
             (t
              (puthash id cand winners)))))
        (cl-incf tier)))
    ;; Post-pass: mark invalid winners in the invalid hash
    (maphash (lambda (id cand)
               (unless (supervisor--authority-candidate-valid-p cand)
                 (puthash id (supervisor--authority-candidate-reason cand)
                          invalid)))
             winners)
    (list :winners winners :invalid invalid :shadowed shadowed)))

;;; Valid unit-file keywords

(defconst supervisor--unit-file-keywords
  '(:id :command :type :stage :delay :after :requires :enabled :disabled
    :restart :no-restart :logging :oneshot-blocking :oneshot-async :oneshot-timeout :tags
    :working-directory :environment :environment-file
    :exec-stop :exec-reload :restart-sec)
  "Valid keywords in a unit-file plist.
Includes `:command' which is unit-file specific.")

;;; Unit-File Path Resolution

(defun supervisor--unit-file-path (id)
  "Return the authoritative path to the unit file for ID.
Ensure the authority snapshot is published (lazy-init if needed) and
return the winning candidate's actual path (which may differ from
ID.el if the file has a different name).  If ID is not in the
snapshot, return a creation path (ID.el in the highest-precedence
active root).  Return nil when no active roots exist."
  (let* ((snap (supervisor--ensure-authority-snapshot))
         (winner (gethash id (plist-get snap :winners))))
    (if winner
        (supervisor--authority-candidate-path winner)
      ;; Not in snapshot; return creation path.
      (let ((active (supervisor--active-authority-roots)))
        (when active
          (expand-file-name (concat id ".el")
                            (car (last active))))))))

;;; Authority Snapshot

(defvar supervisor--authority-snapshot nil
  "Last published authority resolution result.
A plist with keys `:winners', `:invalid', and `:shadowed' as
returned by `supervisor--resolve-authority'.  Set atomically by
`supervisor--publish-authority-snapshot'.  Runtime consumers must
read from this snapshot for a consistent view.")

(defvar supervisor--programs-cache :not-yet-loaded
  "Cached program list from last authority resolution.
Set by `supervisor--load-programs' when it publishes and reads the
authority snapshot.  The keyword `:not-yet-loaded' indicates no cache
exists yet; an empty list nil means zero programs were found.
Runtime consumers access this via `supervisor--effective-programs'
in supervisor-core.  Explicitly refreshed by `supervisor-start'
and `supervisor-daemon-reload'.")

(defun supervisor--publish-authority-snapshot ()
  "Resolve authority and publish the snapshot atomically.
Compute the full resolution, then store the result in
`supervisor--authority-snapshot' in a single assignment."
  (setq supervisor--authority-snapshot (supervisor--resolve-authority)))

(defun supervisor--ensure-authority-snapshot ()
  "Ensure the authority snapshot is published, lazy-initializing if needed.
Return the snapshot.  This guarantees that path lookups always use
the canonical snapshot rather than a lossy filesystem scan."
  (or supervisor--authority-snapshot
      (progn (supervisor--publish-authority-snapshot)
             supervisor--authority-snapshot)))

;;; Invalid unit-file tracking

(defvar supervisor--unit-file-invalid (make-hash-table :test 'equal)
  "Hash table mapping unit-file ID (or filename) to reason string.
Populated from the authority snapshot during
`supervisor--load-all-unit-files'.  Merged into the plan's invalid
hash so entries appear in the dashboard and CLI.")

;;; Unit-File Loading

(defun supervisor--load-unit-file (path)
  "Read and parse a single unit file at PATH.
The file must contain exactly one plist expression.
Returns (LINE . PLIST) where LINE is the line number of the plist,
or signals an error with file:line context."
  (condition-case _
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (skip-chars-forward " \t\n\r")
        (let ((line (line-number-at-pos (point))))
          (condition-case read-err
              (let ((form (read (current-buffer))))
                ;; Validate it is a plist (list starting with keyword)
                (unless (and (listp form) (keywordp (car form)))
                  (error "%s:%d: file must contain a plist starting with a keyword"
                         path line))
                ;; Check for trailing content
                (skip-chars-forward " \t\n\r")
                (unless (eobp)
                  (error "%s:%d: file contains extra content after plist"
                         path (line-number-at-pos (point))))
                (cons line form))
            (end-of-file
             (error "%s:%d: unit file is empty or has incomplete expression"
                    path (line-number-at-pos (point))))
            (invalid-read-syntax
             (error "%s:%d: invalid syntax: %s"
                    path (line-number-at-pos (point))
                    (error-message-string read-err))))))
    (file-missing
     (error "%s:1: unit file not found" path))))

(defun supervisor--validate-unit-file-plist (plist path line)
  "Validate unit-file PLIST loaded from PATH at LINE.
Return nil if valid, or a reason string with file:line context if invalid."
  (cond
   ;; Check for duplicate keys first
   ((let ((dupes (supervisor--plist-duplicate-keys plist)))
      (when dupes
        (format "%s:%d: duplicate key %s"
                path line (car dupes)))))
   ((not (plist-get plist :id))
    (format "%s:%d: missing :id" path line))
   ((not (stringp (plist-get plist :id)))
    (format "%s:%d: :id must be a string" path line))
   ((string-empty-p (plist-get plist :id))
    (format "%s:%d: :id must be non-empty" path line))
   ((not (plist-get plist :command))
    (format "%s:%d: missing :command" path line))
   ((not (stringp (plist-get plist :command)))
    (format "%s:%d: :command must be a string" path line))
   (t
    ;; Check for unknown keywords
    (let ((keys plist)
          (bad nil))
      (while keys
        (let ((key (car keys)))
          (unless (memq key supervisor--unit-file-keywords)
            (setq bad (format "%s:%d: unknown keyword %s"
                              path line key))))
        (setq keys (cddr keys)))
      bad))))

(defun supervisor--unit-file-to-program-entry (plist)
  "Convert unit-file PLIST to program entry format.
Input: (:id \"x\" :command \"cmd\" :type simple ...)
Output: (\"cmd\" :id \"x\" :type simple ...)
The `:command' key is consumed and becomes the car of the entry."
  (let ((cmd (plist-get plist :command))
        (rest nil))
    ;; Copy all keys except :command
    (let ((keys plist))
      (while keys
        (let ((key (car keys))
              (val (cadr keys)))
          (unless (eq key :command)
            (setq rest (append rest (list key val)))))
        (setq keys (cddr keys))))
    (cons cmd rest)))

(defun supervisor--read-authority-snapshot ()
  "Read winning valid plists from the current authority snapshot.
Populate `supervisor--unit-file-invalid' from the snapshot's invalid
hash and return a list of valid winning plists in deterministic ID
order.  Must be called after `supervisor--publish-authority-snapshot'."
  (clrhash supervisor--unit-file-invalid)
  (let* ((snapshot supervisor--authority-snapshot)
         (winners (plist-get snapshot :winners))
         (invalid (plist-get snapshot :invalid))
         (results nil))
    ;; Populate invalid hash for dashboard/CLI
    (maphash (lambda (id reason)
               (supervisor--log 'warning "INVALID unit %s - %s" id reason)
               (puthash id reason supervisor--unit-file-invalid))
             invalid)
    ;; Collect valid winning plists in deterministic ID order
    (maphash (lambda (id cand)
               (when (supervisor--authority-candidate-valid-p cand)
                 (push (cons id cand) results)))
             winners)
    (setq results (sort results (lambda (a b) (string< (car a) (car b)))))
    (mapcar (lambda (pair)
              (supervisor--authority-candidate-plist (cdr pair)))
            results)))

(defun supervisor--load-all-unit-files ()
  "Publish authority snapshot and return winning valid plists.
Convenience wrapper that publishes then reads.  For callers that
need to separate publication from reading, use
`supervisor--publish-authority-snapshot' and
`supervisor--read-authority-snapshot' directly."
  (supervisor--publish-authority-snapshot)
  (supervisor--read-authority-snapshot))

(defun supervisor--load-programs ()
  "Reload programs from disk and update the cache.
Publish the authority snapshot, read valid winners, convert to
program entries, and store the result in `supervisor--programs-cache'.
This is the explicit refresh point called by `supervisor-start' and
`supervisor-daemon-reload'.  Consumers that just need the current
program list should use `supervisor--effective-programs' instead."
  (supervisor--publish-authority-snapshot)
  (let ((unit-plists (supervisor--read-authority-snapshot)))
    (setq supervisor--programs-cache
          (mapcar #'supervisor--unit-file-to-program-entry unit-plists))))

;;; Unit-File Template

(defun supervisor--unit-file-scaffold (id)
  "Return a scaffold template string for a new unit file with ID."
  (concat "(:id \"" id "\"\n"
          " :command \"\"  ; command to run\n"
          " ;; :type simple        ; simple (daemon) or oneshot (run-once)\n"
          " ;; :stage stage3       ; stage1-4, lower runs first\n"
          " ;; :restart always     ; no, on-success, on-failure, always (simple only)\n"
          " ;; :enabled t          ; start on supervisor-start\n"
          " ;; :delay 0            ; seconds to wait before starting\n"
          " ;; :after (\"dep-id\")   ; start after these IDs (same stage)\n"
          " ;; :requires (\"id\")   ; pull-in + ordering dependency\n"
          " ;; :logging t          ; log output to file\n"
          " ;; :oneshot-blocking t ; block stage until exit (oneshot only)\n"
          " ;; :oneshot-async nil  ; inverse of :oneshot-blocking (oneshot only)\n"
          " ;; :oneshot-timeout 30 ; timeout in seconds (oneshot only)\n"
          " ;; :tags (tag1 tag2)   ; tags for filtering\n"
          " )\n"))

;;; Unit-File Validate-on-Save

(defun supervisor--validate-unit-file-buffer ()
  "Validate the unit-file plist in the current buffer.
Reports errors via `message'.  Intended as `after-save-hook'."
  (condition-case err
      (let* ((result (supervisor--load-unit-file (buffer-file-name)))
             (line (car result))
             (plist (cdr result))
             (reason (supervisor--validate-unit-file-plist
                      plist (buffer-file-name) line)))
        (if reason
            (message "Unit file validation: %s" reason)
          (message "Unit file OK: %s" (plist-get plist :id))))
    (error
     (message "Unit file validation error: %s"
              (error-message-string err)))))

(provide 'supervisor-units)

;;; supervisor-units.el ends here
