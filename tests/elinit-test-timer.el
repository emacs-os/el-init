;;; elinit-test-timer.el --- Timer schema and scheduler tests for elinit.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Timer schema and scheduler ERT tests for elinit.el.

;;; Code:

(require 'elinit-test-helpers)

;;; Timer Schema tests

(ert-deftest elinit-test-timer-struct-fields ()
  "Timer struct has expected fields."
  (let ((timer (elinit-timer--create
                :id "test"
                :target "target"
                :enabled t
                :on-startup-sec 60
                :persistent t)))
    (should (elinit-timer-p timer))
    (should (equal "test" (elinit-timer-id timer)))
    (should (equal "target" (elinit-timer-target timer)))
    (should (eq t (elinit-timer-enabled timer)))
    (should (= 60 (elinit-timer-on-startup-sec timer)))
    (should (eq t (elinit-timer-persistent timer)))))

(ert-deftest elinit-test-timer-validate-missing-id ()
  "Timer without :id is rejected."
  (let ((err (elinit-timer--validate '(:target "foo" :on-startup-sec 60) nil)))
    (should (string-match-p ":id must be" err))))

(ert-deftest elinit-test-timer-validate-missing-target ()
  "Timer without :target is rejected."
  (let ((err (elinit-timer--validate '(:id "t" :on-startup-sec 60) nil)))
    (should (string-match-p ":target must be" err))))

(ert-deftest elinit-test-timer-validate-empty-id ()
  "Timer with empty string :id is rejected."
  (let ((err (elinit-timer--validate '(:id "" :target "foo" :on-startup-sec 60) nil)))
    (should (string-match-p ":id must be a non-empty string" err))))

(ert-deftest elinit-test-timer-validate-empty-target ()
  "Timer with empty string :target is rejected."
  (let ((err (elinit-timer--validate '(:id "t" :target "" :on-startup-sec 60) nil)))
    (should (string-match-p ":target must be a non-empty string" err))))

(ert-deftest elinit-test-timer-validate-no-trigger ()
  "Timer without any trigger is rejected."
  (let ((err (elinit-timer--validate '(:id "t" :target "foo") nil)))
    (should (string-match-p "at least one trigger" err))))

(ert-deftest elinit-test-timer-validate-unknown-keyword ()
  "Timer with unknown keyword is rejected."
  (let ((err (elinit-timer--validate
              '(:id "t" :target "foo" :on-startup-sec 60 :bogus t) nil)))
    (should (string-match-p "unknown keyword" err))))

(ert-deftest elinit-test-timer-validate-startup-sec-type ()
  "Timer with non-integer :on-startup-sec is rejected."
  (let ((err (elinit-timer--validate
              '(:id "t" :target "foo" :on-startup-sec "60") nil)))
    (should (string-match-p ":on-startup-sec must be" err))))

(ert-deftest elinit-test-timer-validate-unit-active-sec-positive ()
  "Timer with zero :on-unit-active-sec is rejected."
  (let ((err (elinit-timer--validate
              '(:id "t" :target "foo" :on-unit-active-sec 0) nil)))
    (should (string-match-p ":on-unit-active-sec must be a positive" err))))

(ert-deftest elinit-test-timer-validate-startup-sec-nil ()
  "Timer with only nil :on-startup-sec has no valid trigger."
  (let ((err (elinit-timer--validate
              '(:id "t" :target "foo" :on-startup-sec nil) nil)))
    (should (string-match-p "at least one trigger" err))))

(ert-deftest elinit-test-timer-validate-unit-active-sec-nil ()
  "Timer with only nil :on-unit-active-sec has no valid trigger."
  (let ((err (elinit-timer--validate
              '(:id "t" :target "foo" :on-unit-active-sec nil) nil)))
    (should (string-match-p "at least one trigger" err))))

(ert-deftest elinit-test-timer-validate-startup-sec-nil-with-other-trigger ()
  "Timer with nil :on-startup-sec but valid :on-calendar still validates."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (elinit--build-plan programs))
         (err (elinit-timer--validate
               '(:id "t" :target "script" :on-startup-sec nil
                 :on-calendar (:hour 3))
               plan)))
    ;; Should fail on nil :on-startup-sec type check, not on missing trigger
    (should (string-match-p ":on-startup-sec must be" err))))

(ert-deftest elinit-test-timer-validate-calendar-unknown-field ()
  "Timer with unknown calendar field is rejected."
  (let ((err (elinit-timer--validate
              '(:id "t" :target "foo" :on-calendar (:bogus 5)) nil)))
    (should (string-match-p "unknown field" err))))

(ert-deftest elinit-test-timer-validate-calendar-bad-value ()
  "Timer with invalid calendar value type is rejected."
  (let ((err (elinit-timer--validate
              '(:id "t" :target "foo" :on-calendar (:hour "3")) nil)))
    (should (string-match-p "must be integer" err))))

(ert-deftest elinit-test-timer-validate-enabled-boolean ()
  "Timer with non-boolean :enabled is rejected."
  (let ((err (elinit-timer--validate
              '(:id "t" :target "foo" :on-startup-sec 60 :enabled "yes") nil)))
    (should (string-match-p ":enabled must be a boolean" err))))

(ert-deftest elinit-test-timer-validate-target-not-found ()
  "Timer targeting nonexistent service is rejected."
  (let* ((programs '(("real" :type oneshot :id "real")))
         (plan (elinit--build-plan programs))
         (err (elinit-timer--validate
               '(:id "t" :target "missing" :on-startup-sec 60) plan)))
    (should (string-match-p "not found" err))))

(ert-deftest elinit-test-timer-validate-target-simple-accepted ()
  "Timer targeting simple service is accepted."
  (let* ((programs '(("daemon" :type simple :id "daemon")))
         (plan (elinit--build-plan programs))
         (err (elinit-timer--validate
               '(:id "t" :target "daemon" :on-startup-sec 60) plan)))
    (should-not err)))

(ert-deftest elinit-test-timer-validate-target-type-target-accepted ()
  "Timer targeting a target unit is accepted."
  (let* ((programs '(("" :type target :id "app.target")))
         (plan (elinit--build-plan programs))
         (err (elinit-timer--validate
               '(:id "t" :target "app.target" :on-startup-sec 60) plan)))
    (should-not err)))

(ert-deftest elinit-test-timer-validate-target-type-target-bad-suffix ()
  "Timer targeting a target entry without .target suffix is rejected.
The suffix check at timer validation level catches any target type
entry whose ID does not end in .target (entry validation blocks
this in practice, but the timer validator defends independently)."
  ;; Construct a plan with a hand-built entry where type=target but ID
  ;; lacks the .target suffix.  This bypasses entry validation to test
  ;; the timer validator's own suffix guard.
  (let* ((fake-entry (elinit--parse-entry
                      '(nil :id "bad.target" :type target)))
         ;; Rename the ID in the parsed tuple to remove the suffix
         (bad-entry (cons "bad-no-suffix" (cdr fake-entry)))
         (plan (elinit-plan--create
                :entries (list bad-entry)
                :by-target nil
                :deps (make-hash-table :test 'equal)
                :requires-deps (make-hash-table :test 'equal)
                :dependents (make-hash-table :test 'equal)
                :invalid (make-hash-table :test 'equal)
                :cycle-fallback-ids (make-hash-table :test 'equal)
                :order-index (make-hash-table :test 'equal)
                :meta nil))
         (err (elinit-timer--validate
               '(:id "t" :target "bad-no-suffix" :on-startup-sec 60) plan)))
    (should err)
    (should (string-match "does not end in .target" err))))

(ert-deftest elinit-test-timer-validate-disallowed-target-type ()
  "Timer targeting a timer entry (not oneshot/simple/target) is rejected."
  ;; Timers can only target oneshot, simple, or target entries.
  ;; This tests the rejection of an unsupported type.
  (let* ((fake-entry (elinit--parse-entry
                      '("sleep 1" :id "my-timer" :type timer
                        :on-calendar (:hour 3))))
         (plan (elinit-plan--create
                :entries (list fake-entry)
                :by-target nil
                :deps (make-hash-table :test 'equal)
                :requires-deps (make-hash-table :test 'equal)
                :dependents (make-hash-table :test 'equal)
                :invalid (make-hash-table :test 'equal)
                :cycle-fallback-ids (make-hash-table :test 'equal)
                :order-index (make-hash-table :test 'equal)
                :meta nil))
         (err (elinit-timer--validate
               '(:id "t2" :target "my-timer" :on-startup-sec 60) plan)))
    (should err)
    (should (string-match "must be oneshot, simple, or target" err))))

(ert-deftest elinit-test-timer-validate-valid ()
  "Valid timer passes validation."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (elinit--build-plan programs))
         (err (elinit-timer--validate
               '(:id "t" :target "script" :on-startup-sec 60) plan)))
    (should-not err)))

(ert-deftest elinit-test-timer-validate-calendar-valid ()
  "Valid calendar schedule passes validation."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (elinit--build-plan programs))
         (err (elinit-timer--validate
               '(:id "t" :target "script"
                 :on-calendar (:hour 3 :minute 0 :day-of-week *))
               plan)))
    (should-not err)))

(ert-deftest elinit-test-timer-validate-calendar-list-valid ()
  "Valid list of calendar schedules passes validation."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (elinit--build-plan programs))
         (err (elinit-timer--validate
               '(:id "t" :target "script"
                 :on-calendar ((:hour 3 :minute 0)
                               (:hour 15 :minute 30)))
               plan)))
    (should-not err)))

(ert-deftest elinit-test-timer-validate-calendar-list-invalid ()
  "Invalid entry in list of calendar schedules is rejected."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (elinit--build-plan programs))
         (err (elinit-timer--validate
               '(:id "t" :target "script"
                 :on-calendar ((:hour 3 :minute 0)
                               (:bogus 15)))
               plan)))
    (should (string-match-p "unknown field" err))))

(ert-deftest elinit-test-timer-validate-calendar-empty ()
  "Empty calendar list with no other trigger has no valid trigger."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (elinit--build-plan programs))
         (err (elinit-timer--validate
               '(:id "t" :target "script" :on-calendar ())
               plan)))
    ;; Empty list is falsy so fails trigger check first
    (should (string-match-p "at least one trigger" err))))

(ert-deftest elinit-test-timer-validate-calendar-dotted-pair ()
  "Dotted pair calendar is rejected, not crash."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (elinit--build-plan programs))
         (err (elinit-timer--validate
               '(:id "t" :target "script" :on-calendar (:hour . 3))
               plan)))
    (should (string-match-p "proper plist" err))))

(ert-deftest elinit-test-timer-validate-calendar-empty-with-other-trigger ()
  "Empty calendar with valid :on-startup-sec fails on empty calendar."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (elinit--build-plan programs))
         (err (elinit-timer--validate
               '(:id "t" :target "script" :on-calendar () :on-startup-sec 60)
               plan)))
    (should (string-match-p "cannot be empty" err))))

(ert-deftest elinit-test-timer-validate-calendar-list-non-plist ()
  "Non-plist entry in calendar list is rejected."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (elinit--build-plan programs))
         (err (elinit-timer--validate
               '(:id "t" :target "script"
                 :on-calendar ((:hour 3) foo))
               plan)))
    (should (string-match-p "must be a plist" err))))

(ert-deftest elinit-test-timer-parse ()
  "Timer parsing produces correct struct."
  (let ((timer (elinit-timer--parse
                '(:id "backup" :target "backup-script"
                  :on-calendar (:hour 3 :minute 0)
                  :enabled nil :persistent nil))))
    (should (equal "backup" (elinit-timer-id timer)))
    (should (equal "backup-script" (elinit-timer-target timer)))
    (should (eq nil (elinit-timer-enabled timer)))
    (should (equal '(:hour 3 :minute 0) (elinit-timer-on-calendar timer)))
    (should (eq nil (elinit-timer-persistent timer)))))

(ert-deftest elinit-test-timer-parse-defaults ()
  "Timer parsing applies correct defaults."
  (let ((timer (elinit-timer--parse
                '(:id "t" :target "foo" :on-startup-sec 60))))
    (should (eq t (elinit-timer-enabled timer)))
    (should (eq t (elinit-timer-persistent timer)))))

(ert-deftest elinit-test-timer-build-list-valid ()
  "Build timer list from valid config."
  (let* ((elinit--builtin-timers nil)
         (elinit-timers '((:id "t1" :target "s1" :on-startup-sec 60)
                              (:id "t2" :target "s2" :on-startup-sec 120)))
         (programs '(("s1" :type oneshot :id "s1")
                     ("s2" :type oneshot :id "s2")))
         (plan (elinit--build-plan programs))
         (timers (elinit-timer-build-list plan)))
    (should (= 2 (length timers)))
    (should (= 0 (hash-table-count elinit--invalid-timers)))))

(ert-deftest elinit-test-timer-build-list-invalid-rejected ()
  "Invalid timers are rejected and tracked."
  (let* ((elinit--builtin-timers nil)
         (elinit-timers '((:id "valid" :target "s1" :on-startup-sec 60)
                              (:id "invalid" :target "missing" :on-startup-sec 60)))
         (programs '(("s1" :type oneshot :id "s1")))
         (plan (elinit--build-plan programs))
         (timers (elinit-timer-build-list plan)))
    (should (= 1 (length timers)))
    (should (= 1 (hash-table-count elinit--invalid-timers)))
    (should (gethash "invalid" elinit--invalid-timers))))

(ert-deftest elinit-test-timer-build-list-duplicate-rejected ()
  "Duplicate timer IDs are rejected."
  (let* ((elinit--builtin-timers nil)
         (elinit-timers '((:id "dup" :target "s1" :on-startup-sec 60)
                              (:id "dup" :target "s1" :on-startup-sec 120)))
         (programs '(("s1" :type oneshot :id "s1")))
         (plan (elinit--build-plan programs))
         (timers (elinit-timer-build-list plan)))
    (should (= 1 (length timers)))
    (should (= 1 (hash-table-count elinit--invalid-timers)))))

(ert-deftest elinit-test-timer-build-list-duplicate-invalid-first ()
  "Duplicate timer ID rejected deterministically when first is invalid.
Second valid occurrence should not activate when first invalid occurrence used the ID."
  (let* ((elinit--builtin-timers nil)
         (elinit-timers '((:id "dup" :target "missing" :on-startup-sec 60)  ; invalid (bad target)
                              (:id "dup" :target "s1" :on-startup-sec 120)))    ; valid but duplicate
         (programs '(("s1" :type oneshot :id "s1")))
         (plan (elinit--build-plan programs))
         (timers (elinit-timer-build-list plan)))
    ;; No active timers (first invalid, second duplicate)
    (should (= 0 (length timers)))
    ;; One invalid entry (hash key = "dup", first error preserved)
    (should (= 1 (hash-table-count elinit--invalid-timers)))
    ;; First error (target not found) should be preserved, not overwritten by duplicate
    (should (string-match-p "target" (gethash "dup" elinit--invalid-timers)))))

(ert-deftest elinit-test-timer-validate-startup-sec-zero-rejected ()
  "Timer with zero :on-startup-sec is rejected (must be positive)."
  (let ((err (elinit-timer--validate
              '(:id "t" :target "foo" :on-startup-sec 0) nil)))
    (should err)
    (should (string-match-p ":on-startup-sec must be a positive" err))))

(ert-deftest elinit-test-timer-validate-calendar-field-range-minute ()
  "Calendar :minute field rejects values outside 0-59."
  (let ((err (elinit-timer--validate
              '(:id "t" :target "foo" :on-calendar (:minute 60)) nil)))
    (should err)
    (should (string-match-p "out of range" err))))

(ert-deftest elinit-test-timer-validate-calendar-field-range-hour ()
  "Calendar :hour field rejects values outside 0-23."
  (let ((err (elinit-timer--validate
              '(:id "t" :target "foo" :on-calendar (:hour 24)) nil)))
    (should err)
    (should (string-match-p "out of range" err))))

(ert-deftest elinit-test-timer-validate-calendar-field-range-day ()
  "Calendar :day-of-month field rejects values outside 1-31."
  (let ((err (elinit-timer--validate
              '(:id "t" :target "foo" :on-calendar (:day-of-month 0)) nil)))
    (should err)
    (should (string-match-p "out of range" err))))

(ert-deftest elinit-test-timer-validate-calendar-field-range-month ()
  "Calendar :month field rejects values outside 1-12."
  (let ((err (elinit-timer--validate
              '(:id "t" :target "foo" :on-calendar (:month 13)) nil)))
    (should err)
    (should (string-match-p "out of range" err))))

(ert-deftest elinit-test-timer-validate-calendar-field-range-dow ()
  "Calendar :day-of-week field rejects values outside 0-6."
  (let ((err (elinit-timer--validate
              '(:id "t" :target "foo" :on-calendar (:day-of-week 7)) nil)))
    (should err)
    (should (string-match-p "out of range" err))))

(ert-deftest elinit-test-timer-validate-calendar-field-range-list ()
  "Calendar field list with out-of-range value is rejected."
  (let ((err (elinit-timer--validate
              '(:id "t" :target "foo" :on-calendar (:minute (0 30 61))) nil)))
    (should err)
    (should (string-match-p "out of range" err))))

(ert-deftest elinit-test-timer-validate-calendar-dotted-pair-rejected ()
  "Calendar field dotted pair is rejected (not crash)."
  (let ((err (elinit-timer--validate
              '(:id "t" :target "foo" :on-calendar (:minute (0 . 1))) nil)))
    (should err)
    (should (string-match-p "non-empty list of integers" err))))

(ert-deftest elinit-test-timer-validate-calendar-empty-list-rejected ()
  "Calendar field empty list is rejected."
  (let ((err (elinit-timer--validate
              '(:id "t" :target "foo" :on-calendar (:minute ())) nil)))
    (should err)
    (should (string-match-p "non-empty list of integers" err))))

;;; Timer Scheduler Core tests

(ert-deftest elinit-test-calendar-field-matches-star ()
  "Calendar field * matches any value."
  (should (elinit-timer--calendar-field-matches-p '* 0))
  (should (elinit-timer--calendar-field-matches-p '* 23))
  (should (elinit-timer--calendar-field-matches-p '* 59)))

(ert-deftest elinit-test-calendar-field-matches-integer ()
  "Calendar field integer matches exact value."
  (should (elinit-timer--calendar-field-matches-p 3 3))
  (should-not (elinit-timer--calendar-field-matches-p 3 4))
  (should-not (elinit-timer--calendar-field-matches-p 3 0)))

(ert-deftest elinit-test-calendar-field-matches-list ()
  "Calendar field list matches any value in list."
  (should (elinit-timer--calendar-field-matches-p '(1 3 5) 1))
  (should (elinit-timer--calendar-field-matches-p '(1 3 5) 3))
  (should (elinit-timer--calendar-field-matches-p '(1 3 5) 5))
  (should-not (elinit-timer--calendar-field-matches-p '(1 3 5) 2))
  (should-not (elinit-timer--calendar-field-matches-p '(1 3 5) 4)))

(ert-deftest elinit-test-calendar-matches-time ()
  "Calendar spec matches decoded time correctly."
  ;; Create a decoded time for 2025-01-15 03:30:00 (Wednesday)
  (let ((decoded (decode-time (encode-time 0 30 3 15 1 2025))))
    ;; Exact match
    (should (elinit-timer--calendar-matches-time-p
             '(:hour 3 :minute 30) decoded))
    ;; Wildcards
    (should (elinit-timer--calendar-matches-time-p
             '(:hour 3 :minute *) decoded))
    ;; Lists
    (should (elinit-timer--calendar-matches-time-p
             '(:hour (1 2 3) :minute 30) decoded))
    ;; Mismatch
    (should-not (elinit-timer--calendar-matches-time-p
                 '(:hour 4 :minute 30) decoded))
    (should-not (elinit-timer--calendar-matches-time-p
                 '(:hour 3 :minute 0) decoded))))

(ert-deftest elinit-test-calendar-next-minute-finds-match ()
  "Calendar next minute finds matching time."
  ;; From 2025-01-15 00:00:00, find next 03:30
  (let* ((from (encode-time 0 0 0 15 1 2025))
         (next (elinit-timer--calendar-next-minute
                from '(:hour 3 :minute 30) 2)))  ; 2 days max
    (should next)
    ;; Should be 03:30
    (let ((decoded (decode-time next)))
      (should (= 3 (decoded-time-hour decoded)))
      (should (= 30 (decoded-time-minute decoded))))))

(ert-deftest elinit-test-calendar-next-minute-respects-limit ()
  "Calendar next minute respects iteration limit."
  ;; Looking for hour 25 (impossible) with small limit
  (let* ((from (encode-time 0 0 0 15 1 2025))
         (next (elinit-timer--calendar-next-minute
                from '(:hour 25 :minute 0) 7)))  ; 7 days max
    (should-not next)))

(ert-deftest elinit-test-calendar-next-minute-leap-day ()
  "Calendar next minute finds leap day across multi-year gap.
From March 2025, next Feb 29 is in 2028 (~3 years away)."
  (let* ((from (encode-time 0 0 0 1 3 2025))  ; 2025-03-01 00:00:00
         (next (elinit-timer--calendar-next-minute
                from '(:month 2 :day-of-month 29 :hour 0 :minute 0) 10228)))  ; 28 years
    (should next)
    (let ((decoded (decode-time next)))
      (should (= 2028 (decoded-time-year decoded)))
      (should (= 2 (decoded-time-month decoded)))
      (should (= 29 (decoded-time-day decoded))))))

(ert-deftest elinit-test-calendar-next-minute-leap-day-weekday ()
  "Calendar next minute finds leap day + weekday across long gap.
From March 2025, next Feb 29 that is Sunday (dow=0) is in 2032 (~7 years away)."
  (let* ((from (encode-time 0 0 0 1 3 2025))  ; 2025-03-01 00:00:00
         (next (elinit-timer--calendar-next-minute
                from '(:month 2 :day-of-month 29 :day-of-week 0 :hour 0 :minute 0) 10228)))
    (should next)
    (let ((decoded (decode-time next)))
      (should (= 2032 (decoded-time-year decoded)))
      (should (= 2 (decoded-time-month decoded)))
      (should (= 29 (decoded-time-day decoded)))
      ;; Verify it's actually Sunday (day-of-week 0)
      (should (= 0 (decoded-time-weekday decoded))))))

(ert-deftest elinit-test-calendar-next-minute-strictly-after ()
  "Calendar next minute returns time strictly after from-time.
When from-time is exactly at a matching minute boundary, should return next occurrence."
  ;; From 2025-01-15 03:30:00 exactly (matches :hour 3 :minute 30)
  (let* ((from (encode-time 0 30 3 15 1 2025))
         (next (elinit-timer--calendar-next-minute
                from '(:hour 3 :minute 30) 2)))  ; 2 days max
    (should next)
    ;; Should be next day's 03:30, not the same time
    (should (> next (float-time from)))
    (let ((decoded (decode-time next)))
      (should (= 16 (decoded-time-day decoded)))  ; Next day
      (should (= 3 (decoded-time-hour decoded)))
      (should (= 30 (decoded-time-minute decoded))))))

(ert-deftest elinit-test-calendar-next-minute-dst-gap ()
  "Calendar next minute skips non-existent DST gap times.
On March 9, 2025 in America/New_York, 2:00-2:59 AM doesn't exist (spring forward).
Searching for 2:30 AM should skip March 9 and return March 10 2:30 AM.
Uses `setenv' rather than `process-environment' to ensure the C-level
timezone that `encode-time' and `decode-time' use is actually changed."
  (let ((orig-tz (getenv "TZ")))
    (unwind-protect
        (progn
          (setenv "TZ" "America/New_York")
          ;; Skip test if TZ setting doesn't take effect
          ;; (CI may lack timezone data for America/New_York)
          (let* ((test-time (encode-time 0 30 2 9 3 2025))
                 (test-decoded (decode-time test-time)))
            (skip-unless (= 3 (decoded-time-hour test-decoded))))
          ;; From March 9, 2025 00:00:00 (before DST transition)
          (let* ((from (encode-time 0 0 0 9 3 2025))
                 (next (elinit-timer--calendar-next-minute
                        from '(:hour 2 :minute 30) 7)))  ; 7 days max
            (should next)
            (let ((decoded (decode-time next)))
              ;; Should be March 10 (next day), not March 9
              (should (= 10 (decoded-time-day decoded)))
              (should (= 2 (decoded-time-hour decoded)))
              (should (= 30 (decoded-time-minute decoded))))))
      (setenv "TZ" orig-tz))))

(ert-deftest elinit-test-timer-next-startup-time ()
  "Startup trigger computes time relative to scheduler start."
  (let* ((timer (elinit-timer--create
                 :id "t1" :target "s1" :on-startup-sec 120))
         (elinit--scheduler-startup-time 1000.0)
         (elinit--timer-state (make-hash-table :test 'equal)))
    ;; No previous run - should return startup + delay
    (should (= 1120.0 (elinit-timer--next-startup-time timer)))
    ;; After startup trigger has fired - should return nil
    (puthash "t1" '(:startup-triggered t) elinit--timer-state)
    (should-not (elinit-timer--next-startup-time timer))))

(ert-deftest elinit-test-timer-next-unit-active-time ()
  "Unit-active trigger computes time relative to last success."
  (let* ((timer (elinit-timer--create
                 :id "t1" :target "s1" :on-unit-active-sec 300))
         (elinit--timer-state (make-hash-table :test 'equal)))
    ;; No previous success - should return nil
    (should-not (elinit-timer--next-unit-active-time timer))
    ;; After a successful run
    (puthash "t1" '(:last-success-at 2000.0) elinit--timer-state)
    (should (= 2300.0 (elinit-timer--next-unit-active-time timer)))))

(ert-deftest elinit-test-timer-compute-next-run-picks-earliest ()
  "Timer picks earliest trigger when multiple are configured."
  (let* ((timer (elinit-timer--create
                 :id "t1" :target "s1"
                 :on-startup-sec 60
                 :on-unit-active-sec 300))
         (elinit--scheduler-startup-time 1000.0)
         (elinit--timer-state (make-hash-table :test 'equal)))
    ;; Only startup is available (no previous success)
    (should (= 1060.0 (elinit-timer--compute-next-run timer 1000.0)))
    ;; After startup trigger has fired and a success recorded, unit-active becomes available
    (puthash "t1" '(:last-success-at 1050.0 :startup-triggered t) elinit--timer-state)
    ;; Startup already fired, unit-active at 1350
    (should (= 1350.0 (elinit-timer--compute-next-run timer 1100.0)))))

(ert-deftest elinit-test-timer-overlap-detection ()
  "Timer detects when target is still running."
  (let* ((timer (elinit-timer--create :id "t1" :target "s1"))
         (elinit--processes (make-hash-table :test 'equal)))
    ;; No process - not active
    (should-not (elinit-timer--target-active-p timer))
    ;; Dead process - not active
    (puthash "s1" (start-process "test" nil "true") elinit--processes)
    (sleep-for 0.1) ; Let it die
    (should-not (elinit-timer--target-active-p timer))
    ;; Cleanup
    (clrhash elinit--processes)))

(ert-deftest elinit-test-timer-trigger-disabled-timer ()
  "Timer trigger skips disabled timer and records miss."
  (elinit-test-with-unit-files
      '(("true" :id "s1" :type oneshot))
    (let* ((timer (elinit-timer--create :id "t1" :target "s1" :enabled nil))
           (elinit--timer-state (make-hash-table :test 'equal)))
      ;; Trigger disabled timer
      (should-not (elinit-timer--trigger timer 'scheduled))
      ;; Should record miss with reason 'disabled
      (let ((state (gethash "t1" elinit--timer-state)))
        (should state)
        (should (plist-get state :last-missed-at))
        (should (eq 'disabled (plist-get state :last-miss-reason))))
      ;; Cleanup
      (clrhash elinit--timer-state))))

(ert-deftest elinit-test-timer-trigger-disabled-target ()
  "Timer trigger skips disabled target and records miss."
  (elinit-test-with-unit-files
      '(("true" :id "s1" :type oneshot :disabled t))
    (let* ((timer (elinit-timer--create :id "t1" :target "s1" :enabled t))
           (elinit--timer-state (make-hash-table :test 'equal))
           (elinit--processes (make-hash-table :test 'equal))
           (elinit--enabled-override (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal)))
      ;; Trigger timer with disabled target (via config)
      (should-not (elinit-timer--trigger timer 'scheduled))
      ;; Should record miss with reason 'disabled-target
      (let ((state (gethash "t1" elinit--timer-state)))
        (should state)
        (should (plist-get state :last-missed-at))
        (should (eq 'disabled-target (plist-get state :last-miss-reason))))
      ;; Cleanup
      (clrhash elinit--timer-state)
      (clrhash elinit--processes)
      (clrhash elinit--enabled-override)
      (clrhash elinit--invalid))))

(ert-deftest elinit-test-timer-trigger-disabled-target-override ()
  "Timer trigger respects runtime disabled override."
  (elinit-test-with-unit-files
      '(("true" :id "s1" :type oneshot))
    (let* ((timer (elinit-timer--create :id "t1" :target "s1" :enabled t))
           (elinit--timer-state (make-hash-table :test 'equal))
           (elinit--processes (make-hash-table :test 'equal))
           (elinit--enabled-override (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal)))
      ;; Disable target via runtime override
      (puthash "s1" 'disabled elinit--enabled-override)
      ;; Trigger timer
      (should-not (elinit-timer--trigger timer 'scheduled))
      ;; Should record miss with reason 'disabled-target
      (let ((state (gethash "t1" elinit--timer-state)))
        (should state)
        (should (eq 'disabled-target (plist-get state :last-miss-reason))))
      ;; Cleanup
      (clrhash elinit--timer-state)
      (clrhash elinit--processes)
      (clrhash elinit--enabled-override)
      (clrhash elinit--invalid))))

(ert-deftest elinit-test-timer-trigger-target-not-found ()
  "Timer trigger handles missing target as failure."
  (elinit-test-with-unit-files nil
    (let* ((timer (elinit-timer--create :id "t1" :target "nonexistent" :enabled t))
           (elinit--timer-state (make-hash-table :test 'equal)))
      ;; Trigger timer with nonexistent target
      (should-not (elinit-timer--trigger timer 'scheduled))
      ;; Failure is recorded per plan (surfaced diagnostic)
      (let ((state (gethash "t1" elinit--timer-state)))
        (should state)
        (should (eq 'failure (plist-get state :last-result))))
      ;; Cleanup
      (clrhash elinit--timer-state))))

(ert-deftest elinit-test-timer-trigger-overlap-skips ()
  "Timer trigger skips when target is still running."
  (elinit-test-with-unit-files
      '(("sleep 10" :id "s1" :type oneshot))
    (let* ((timer (elinit-timer--create :id "t1" :target "s1" :enabled t))
           (elinit--timer-state (make-hash-table :test 'equal))
           (elinit--processes (make-hash-table :test 'equal))
           (elinit--enabled-override (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal)))
      ;; Simulate running process
      (puthash "s1" (start-process "test-overlap" nil "sleep" "10")
               elinit--processes)
      ;; Trigger timer
      (should-not (elinit-timer--trigger timer 'scheduled))
      ;; Should record miss with reason 'overlap
      (let ((state (gethash "t1" elinit--timer-state)))
        (should state)
        (should (eq 'overlap (plist-get state :last-miss-reason))))
      ;; Cleanup
      (let ((proc (gethash "s1" elinit--processes)))
        (when (process-live-p proc)
          (delete-process proc)))
      (clrhash elinit--invalid)
      (clrhash elinit--timer-state)
      (clrhash elinit--processes)
      (clrhash elinit--enabled-override))))

(ert-deftest elinit-test-timer-trigger-success-path ()
  "Timer trigger succeeds and emits timer-trigger event."
  (elinit-test-with-unit-files
      '(("true" :id "s1" :type oneshot))
    (let* ((elinit-mode t)
           (timer (elinit-timer--create :id "t1" :target "s1" :enabled t))
           (elinit--timer-state (make-hash-table :test 'equal))
           (elinit--processes (make-hash-table :test 'equal))
           (elinit--enabled-override (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal))
           (elinit--scheduler-startup-time (float-time))
           (events nil)
           (hook-fn (lambda (event)
                      (when (eq (plist-get event :type) 'timer-trigger)
                        (push event events)))))
      ;; Capture timer-trigger event
      (add-hook 'elinit-event-hook hook-fn)
      (unwind-protect
          (progn
            ;; Trigger timer
            (should (elinit-timer--trigger timer 'scheduled))
            ;; Should have recorded :last-run-at
            (let ((state (gethash "t1" elinit--timer-state)))
              (should state)
              (should (plist-get state :last-run-at)))
            ;; Should have emitted timer-trigger event
            (should (= 1 (length events)))
            (let ((event (car events)))
              (should (eq 'timer-trigger (plist-get event :type)))
              (should (equal "t1" (plist-get event :id)))
              (should (equal "s1" (plist-get (plist-get event :data) :target)))))
        ;; Cleanup
        (remove-hook 'elinit-event-hook hook-fn)
        (clrhash elinit--invalid)
        (clrhash elinit--timer-state)
        (clrhash elinit--processes)
        (clrhash elinit--enabled-override)))))

(ert-deftest elinit-test-timer-startup-trigger-independent ()
  "Startup trigger is independent from calendar/unit-active triggers."
  (let* ((timer (elinit-timer--create :id "t1" :target "s1" :enabled t
                                          :on-startup-sec 60
                                          :on-calendar '(:minute 0)))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--scheduler-startup-time 1000.0))
    ;; Simulate a calendar trigger having fired (sets :last-run-at)
    (puthash "t1" (list :last-run-at 1010.0) elinit--timer-state)
    ;; Startup trigger should still return a time (not cancelled by calendar)
    (let ((next (elinit-timer--next-startup-time timer)))
      (should next)
      (should (= 1060.0 next)))
    ;; Now mark startup as triggered
    (puthash "t1" (list :last-run-at 1010.0 :startup-triggered t)
             elinit--timer-state)
    ;; Now startup trigger should return nil
    (should-not (elinit-timer--next-startup-time timer))
    ;; Cleanup
    (clrhash elinit--timer-state)))

(ert-deftest elinit-test-timer-startup-consumed-on-skipped-run ()
  "Startup trigger is consumed even when run is skipped (disabled-target)."
  ;; Regression test: startup trigger must not cause 1s retry loop on skip
  (elinit-test-with-unit-files
      '(("true" :id "s1" :type oneshot :disabled t))
    (let* ((timer (elinit-timer--create :id "t1" :target "s1" :enabled t
                                            :on-startup-sec 10))
           (elinit--timer-state (make-hash-table :test 'equal))
           (elinit--processes (make-hash-table :test 'equal))
           (elinit--enabled-override (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal))
           (elinit--scheduler-startup-time 1000.0))
      ;; Initialize next-run-at to startup trigger time
      (puthash "t1" '(:next-run-at 1010.0) elinit--timer-state)
      ;; Simulate time is now past startup trigger
      (cl-letf (((symbol-function 'float-time) (lambda () 1015.0)))
        ;; Trigger should fail (target disabled)
        (should-not (elinit-timer--trigger timer 'scheduled))
        ;; But startup-triggered should be set to prevent retry loop
        (let ((state (gethash "t1" elinit--timer-state)))
          (should (plist-get state :startup-triggered)))
        ;; Next startup time should now be nil (consumed)
        (should-not (elinit-timer--next-startup-time timer)))
      ;; Cleanup
      (clrhash elinit--timer-state)
      (clrhash elinit--processes)
      (clrhash elinit--enabled-override)
      (clrhash elinit--invalid))))

(ert-deftest elinit-test-timer-scheduler-not-started-during-shutdown ()
  "Timer scheduler is not started when shutting down."
  (elinit-test-with-unit-files
      '(("true" :id "s1" :type oneshot))
    (let ((elinit--shutting-down t)
          (elinit-timers '((:id "t1" :target "s1" :on-startup-sec 60)))
          (scheduler-started nil)
          (callback-ran nil))
      ;; Mock the scheduler start function
      (cl-letf (((symbol-function 'elinit-timer-scheduler-start)
                 (lambda () (setq scheduler-started t))))
        ;; Simulate completion callback with no entries (immediate callback)
        (elinit--start-entries-async
         nil
         (lambda ()
           (elinit--dag-cleanup)
           (when (and (not elinit--shutting-down)
                      (fboundp 'elinit-timer-scheduler-start))
             (elinit-timer-scheduler-start))
           (setq callback-ran t)))
        ;; Callback should have fired
        (should callback-ran)
        ;; Scheduler should NOT have been started
        (should-not scheduler-started)))))

;;; Phase 3: Retry and Catch-up Tests

(ert-deftest elinit-test-timer-failure-retryable-positive-exit ()
  "Positive exit codes are retryable."
  (should (elinit-timer--failure-retryable-p 1))
  (should (elinit-timer--failure-retryable-p 127)))

(ert-deftest elinit-test-timer-failure-not-retryable-signal ()
  "Signal deaths (stored as negative values) are not retryable.
Emacs provides signal numbers as positive values with process-status='signal.
The oneshot exit handler encodes these as negative for retry gating."
  (should-not (elinit-timer--failure-retryable-p -9))   ; SIGKILL
  (should-not (elinit-timer--failure-retryable-p -15))) ; SIGTERM

(ert-deftest elinit-test-oneshot-exit-encodes-signal-as-negative ()
  "Signal deaths are stored as negative values in oneshot-completed.
This ensures retry eligibility correctly rejects signal deaths."
  (let ((elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--oneshot-callbacks (make-hash-table :test 'equal))
        (elinit--dag-blocking-oneshots nil)
        (elinit--dag-in-degree (make-hash-table :test 'equal))
        (elinit--dag-dependents (make-hash-table :test 'equal))
        (elinit--dag-ready nil)
        (elinit--event-handlers nil))
    ;; Simulate signal death: proc-status='signal, exit-code=9 (SIGKILL)
    (elinit--handle-oneshot-exit "test-oneshot" 'signal 9)
    ;; Should be stored as -9
    (should (= -9 (gethash "test-oneshot" elinit--oneshot-completed)))
    ;; Therefore not retryable
    (should-not (elinit-timer--failure-retryable-p
                 (gethash "test-oneshot" elinit--oneshot-completed)))))

(ert-deftest elinit-test-oneshot-exit-preserves-normal-exit-code ()
  "Normal exit codes are stored as-is (positive values)."
  (let ((elinit--oneshot-completed (make-hash-table :test 'equal))
        (elinit--oneshot-callbacks (make-hash-table :test 'equal))
        (elinit--dag-blocking-oneshots nil)
        (elinit--dag-in-degree (make-hash-table :test 'equal))
        (elinit--dag-dependents (make-hash-table :test 'equal))
        (elinit--dag-ready nil)
        (elinit--event-handlers nil))
    ;; Simulate normal exit: proc-status='exit, exit-code=1
    (elinit--handle-oneshot-exit "test-oneshot" 'exit 1)
    ;; Should be stored as 1 (positive)
    (should (= 1 (gethash "test-oneshot" elinit--oneshot-completed)))
    ;; Therefore retryable
    (should (elinit-timer--failure-retryable-p
             (gethash "test-oneshot" elinit--oneshot-completed)))))

(ert-deftest elinit-test-timer-failure-not-retryable-zero ()
  "Zero exit (success) is not retryable."
  (should-not (elinit-timer--failure-retryable-p 0)))

(ert-deftest elinit-test-timer-failure-not-retryable-nil ()
  "Nil exit code is not retryable."
  (should-not (elinit-timer--failure-retryable-p nil)))

(ert-deftest elinit-test-signal-death-status-is-failed ()
  "Signal deaths (negative exit codes) are classified as failed in status.
This is a regression test: signals are non-retryable but still failed."
  (let ((elinit--processes (make-hash-table :test 'equal))
        (elinit--failed (make-hash-table :test 'equal))
        (elinit--oneshot-completed (make-hash-table :test 'equal)))
    ;; Simulate SIGKILL death stored as -9
    (puthash "test-oneshot" -9 elinit--oneshot-completed)
    (let ((result (elinit--compute-entry-status "test-oneshot" 'oneshot)))
      (should (equal "failed" (car result)))
      (should (equal "exit:-9" (cdr result))))))

(ert-deftest elinit-test-signal-death-dashboard-count-is-failed ()
  "Signal deaths are counted as failed in dashboard summary."
  (elinit-test-with-unit-files
      '(("true" :id "sig-test" :type oneshot))
    (let ((elinit--processes (make-hash-table :test 'equal))
          (elinit--failed (make-hash-table :test 'equal))
          (elinit--oneshot-completed (make-hash-table :test 'equal))
          (elinit--invalid (make-hash-table :test 'equal)))
      ;; Simulate SIGTERM death stored as -15
      (puthash "sig-test" -15 elinit--oneshot-completed)
      (let ((summary (elinit--health-summary)))
        ;; Should show 1 fail, not 1 done
        (should (string-match-p "1 fail" summary))
        (should-not (string-match-p "1 done" summary))))))

(ert-deftest elinit-test-timer-schedule-retry-first-attempt ()
  "First retry is scheduled with first interval."
  (let ((elinit-timer-retry-intervals '(30 120 600))
        (elinit--timer-state (make-hash-table :test 'equal))
        (state nil))
    (puthash "t1" state elinit--timer-state)
    (cl-letf (((symbol-function 'float-time) (lambda () 1000.0)))
      (let ((updated (elinit-timer--schedule-retry "t1" state)))
        (should updated)
        (should (= 1 (plist-get updated :retry-attempt)))
        (should (= 1030.0 (plist-get updated :retry-next-at)))))
    (clrhash elinit--timer-state)))

(ert-deftest elinit-test-timer-schedule-retry-second-attempt ()
  "Second retry uses second interval."
  (let ((elinit-timer-retry-intervals '(30 120 600))
        (elinit--timer-state (make-hash-table :test 'equal))
        (state '(:retry-attempt 1)))
    (puthash "t1" state elinit--timer-state)
    (cl-letf (((symbol-function 'float-time) (lambda () 1000.0)))
      (let ((updated (elinit-timer--schedule-retry "t1" state)))
        (should updated)
        (should (= 2 (plist-get updated :retry-attempt)))
        (should (= 1120.0 (plist-get updated :retry-next-at)))))
    (clrhash elinit--timer-state)))

(ert-deftest elinit-test-timer-schedule-retry-exhausted ()
  "No retry scheduled when max attempts reached."
  (let ((elinit-timer-retry-intervals '(30 120 600))
        (elinit--timer-state (make-hash-table :test 'equal))
        (state '(:retry-attempt 3)))
    (puthash "t1" state elinit--timer-state)
    (should-not (elinit-timer--schedule-retry "t1" state))
    (clrhash elinit--timer-state)))

(ert-deftest elinit-test-timer-schedule-retry-disabled ()
  "No retry when retry-intervals is nil."
  (let ((elinit-timer-retry-intervals nil)
        (elinit--timer-state (make-hash-table :test 'equal))
        (state nil))
    (puthash "t1" state elinit--timer-state)
    (should-not (elinit-timer--schedule-retry "t1" state))
    (clrhash elinit--timer-state)))

(ert-deftest elinit-test-timer-catch-up-needed ()
  "Catch-up detected for persistent timer with missed run."
  (let* ((timer (elinit-timer--create :id "t1" :target "s1"
                                          :on-calendar '(:minute 0)
                                          :persistent t))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--scheduler-startup-time 1000.0)
         (elinit-timer-catch-up-limit 86400))
    ;; Last run was 2 hours ago, missed the hourly schedule
    (puthash "t1" '(:last-run-at 900.0) elinit--timer-state)
    ;; Mock calendar computation to return a time in the past
    (cl-letf (((symbol-function 'elinit-timer--compute-next-run)
               (lambda (_timer _from) 950.0))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      (should (elinit-timer--needs-catch-up-p timer)))
    (clrhash elinit--timer-state)))

(ert-deftest elinit-test-timer-catch-up-not-needed-recent ()
  "No catch-up for timer with recent run."
  (let* ((timer (elinit-timer--create :id "t1" :target "s1"
                                          :on-calendar '(:minute 0)
                                          :persistent t))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--scheduler-startup-time 1000.0)
         (elinit-timer-catch-up-limit 86400))
    ;; Last run was just now
    (puthash "t1" '(:last-run-at 999.0) elinit--timer-state)
    ;; Mock calendar computation to return a time in the future
    (cl-letf (((symbol-function 'elinit-timer--compute-next-run)
               (lambda (_timer _from) 1060.0))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      (should-not (elinit-timer--needs-catch-up-p timer)))
    (clrhash elinit--timer-state)))

(ert-deftest elinit-test-timer-catch-up-not-needed-non-persistent ()
  "No catch-up for non-persistent timer."
  (let* ((timer (elinit-timer--create :id "t1" :target "s1"
                                          :on-calendar '(:minute 0)
                                          :persistent nil))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--scheduler-startup-time 1000.0)
         (elinit-timer-catch-up-limit 86400))
    (puthash "t1" '(:last-run-at 900.0) elinit--timer-state)
    (should-not (elinit-timer--needs-catch-up-p timer))
    (clrhash elinit--timer-state)))

(ert-deftest elinit-test-timer-catch-up-not-needed-too-old ()
  "No catch-up for missed run beyond catch-up limit."
  (let* ((timer (elinit-timer--create :id "t1" :target "s1"
                                          :on-calendar '(:minute 0)
                                          :persistent t))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--scheduler-startup-time 100000.0)
         (elinit-timer-catch-up-limit 86400))
    ;; Last run was way before catch-up window
    (puthash "t1" '(:last-run-at 1000.0) elinit--timer-state)
    ;; Mock calendar computation to return a time before catch-up window
    (cl-letf (((symbol-function 'elinit-timer--compute-next-run)
               (lambda (_timer _from) 2000.0))  ; Before cutoff
              ((symbol-function 'float-time) (lambda () 100000.0)))
      (should-not (elinit-timer--needs-catch-up-p timer)))
    (clrhash elinit--timer-state)))

(ert-deftest elinit-test-timer-catch-up-boundary-no-false-trigger ()
  "No false catch-up when last-run-at is exactly at schedule boundary.
The fix passes (1+ last-run) to compute-next-run to avoid the case where
compute-next-run returns the same timestamp as last-run for calendar timers
at minute boundaries."
  (let* ((timer (elinit-timer--create :id "t1" :target "s1"
                                          :on-calendar '(:minute 0)
                                          :persistent t))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--scheduler-startup-time 1000.0)
         (elinit-timer-catch-up-limit 86400)
         (call-count 0)
         (from-times nil))
    ;; Last run was at exactly time 900.0 (a schedule boundary)
    (puthash "t1" '(:last-run-at 900.0) elinit--timer-state)
    ;; Mock compute-next-run to return 900.0 when called with 900.0,
    ;; but return future time 1800.0 when called with 901.0
    ;; This simulates the boundary condition we fixed
    (cl-letf (((symbol-function 'elinit-timer--compute-next-run)
               (lambda (_timer from)
                 (setq call-count (1+ call-count))
                 (push from from-times)
                 (if (= from 900.0) 900.0 1800.0)))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      ;; Should NOT need catch-up because next run is in the future
      (should-not (elinit-timer--needs-catch-up-p timer))
      ;; Verify we called with 901.0 (last-run + 1), not 900.0
      (should (= call-count 1))
      (should (= (car from-times) 901.0)))
    (clrhash elinit--timer-state)))

(ert-deftest elinit-test-timer-state-to-alist ()
  "Timer state to alist only includes persist keys."
  (let ((elinit--timer-state (make-hash-table :test 'equal)))
    (puthash "t1" '(:last-run-at 1000.0
                    :last-success-at 1000.0
                    :next-run-at 2000.0
                    :retry-attempt 1
                    :startup-triggered t)
             elinit--timer-state)
    (let ((alist (elinit--timer-state-to-alist)))
      ;; Should include persisted keys
      (should (equal (alist-get "t1" alist nil nil #'equal)
                     '(:last-run-at 1000.0 :last-success-at 1000.0)))
      ;; Transient keys should not be included
      (should-not (plist-get (cdr (assoc "t1" alist)) :next-run-at))
      (should-not (plist-get (cdr (assoc "t1" alist)) :retry-attempt))
      (should-not (plist-get (cdr (assoc "t1" alist)) :startup-triggered)))
    (clrhash elinit--timer-state)))

(ert-deftest elinit-test-timer-state-save-load-roundtrip ()
  "Timer state survives save/load cycle."
  (let* ((elinit-mode t)
         (temp-file (make-temp-file "elinit-test-timer-state-" nil ".eld"))
         (elinit-timer-state-file temp-file)
         (elinit--timer-state (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; Save some state
          (puthash "t1" '(:last-run-at 1000.0 :last-success-at 1000.0)
                   elinit--timer-state)
          (puthash "t2" '(:last-failure-at 900.0 :last-exit 1)
                   elinit--timer-state)
          (should (elinit-timer--save-state))
          ;; Clear and reconcile
          (clrhash elinit--timer-state)
          (should (elinit-timer--load-state))
          ;; Verify state restored
          (let ((t1 (gethash "t1" elinit--timer-state))
                (t2 (gethash "t2" elinit--timer-state)))
            (should (equal (plist-get t1 :last-run-at) 1000.0))
            (should (equal (plist-get t1 :last-success-at) 1000.0))
            (should (equal (plist-get t2 :last-failure-at) 900.0))
            (should (equal (plist-get t2 :last-exit) 1))))
      (delete-file temp-file)
      (clrhash elinit--timer-state))))

(ert-deftest elinit-test-timer-state-corrupt-file-handled ()
  "Corrupt timer state file is handled gracefully."
  (let* ((elinit-mode t)
         (temp-file (make-temp-file "elinit-test-corrupt-" nil ".eld"))
         (elinit-timer-state-file temp-file)
         (elinit--timer-state (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "this is not valid lisp data"))
          (should-not (elinit-timer--load-state))
          ;; Hash should remain unchanged
          (should (= (hash-table-count elinit--timer-state) 0)))
      (delete-file temp-file)
      (clrhash elinit--timer-state))))

(ert-deftest elinit-test-timer-state-persistence-disabled ()
  "Nil timer state file path disables persistence."
  (let ((elinit-timer-state-file nil)
        (elinit--timer-state (make-hash-table :test 'equal)))
    (puthash "t1" '(:last-run-at 1000.0) elinit--timer-state)
    ;; Save returns nil when disabled
    (should-not (elinit-timer--save-state))
    ;; Load returns nil when disabled
    (should-not (elinit-timer--load-state))
    (clrhash elinit--timer-state)))

(ert-deftest elinit-test-timer-state-file-path ()
  "Timer state file path helper returns configured path."
  (let ((elinit-timer-state-file "/test/path/timer-state.eld"))
    (should (equal (elinit--timer-state-file-path) "/test/path/timer-state.eld")))
  (let ((elinit-timer-state-file nil))
    (should-not (elinit--timer-state-file-path))))

(ert-deftest elinit-test-timer-state-newer-version-rejected ()
  "Newer schema version is rejected, not just warned."
  (let* ((elinit-mode t)
         (temp-file (make-temp-file "elinit-test-version-" nil ".eld"))
         (elinit-timer-state-file temp-file)
         (elinit--timer-state (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; Write file with future version
          (with-temp-file temp-file
            (insert (format "((version . %d) (timestamp . \"test\") (timers . ((\"t1\" :last-run-at 1000.0))))"
                            (1+ elinit-timer-state-schema-version))))
          ;; Load should fail
          (should-not (elinit-timer--load-state))
          ;; Hash should remain empty (data not merged)
          (should (= (hash-table-count elinit--timer-state) 0)))
      (delete-file temp-file)
      (clrhash elinit--timer-state))))

(ert-deftest elinit-test-timer-state-stale-ids-pruned ()
  "Stale timer IDs are pruned from state during scheduler startup."
  (elinit-test-with-unit-files
      '(("true" :id "s1" :type oneshot))
    (let* ((elinit-mode t)
           (elinit-timers '((:id "active" :target "s1" :on-startup-sec 60)))
           (elinit--timer-state (make-hash-table :test 'equal))
           (elinit--timer-list nil)
           (elinit--timer-scheduler nil)
           (elinit--shutting-down nil)
           (elinit--scheduler-startup-time nil)
           (elinit-timer-state-file nil))
      ;; Pre-populate state with a stale ID
      (puthash "stale-removed" '(:last-run-at 500.0) elinit--timer-state)
      (puthash "active" '(:last-run-at 900.0) elinit--timer-state)
      ;; Mock to prevent actual scheduling
      (cl-letf (((symbol-function 'elinit--timer-scheduler-tick) #'ignore)
                ((symbol-function 'elinit-timer--process-catch-ups) #'ignore)
                ((symbol-function 'float-time) (lambda () 1000.0)))
        (elinit-timer-scheduler-start))
      ;; Stale ID should be pruned
      (should-not (gethash "stale-removed" elinit--timer-state))
      ;; Active ID should remain
      (should (gethash "active" elinit--timer-state))
      (clrhash elinit--timer-state))))

(ert-deftest elinit-test-timer-cross-restart-catch-up ()
  "Integration test: scheduler startup with persisted state triggers catch-up."
  (elinit-test-with-unit-files
      '(("true" :id "s1" :type oneshot))
    (let* ((elinit-mode t)
           (temp-file (make-temp-file "elinit-test-catchup-" nil ".eld"))
           (elinit-timers '((:id "t1" :target "s1" :on-calendar (:minute 0)
                                 :persistent t)))
           (elinit--timer-state (make-hash-table :test 'equal))
           (elinit--timer-list nil)
           (elinit--timer-scheduler nil)
           (elinit--shutting-down nil)
           (elinit--scheduler-startup-time nil)
           (elinit--timer-state-loaded nil)
           (elinit-timer-state-file temp-file)
           (elinit-timer-catch-up-limit 86400)
           (catch-up-triggered nil))
      (unwind-protect
          (progn
            ;; Write persisted state with old last-run (simulates restart)
            (with-temp-file temp-file
              (insert (format "((version . %d) (timestamp . \"test\") (timers . ((\"t1\" :last-run-at 900.0 :last-success-at 900.0))))"
                              elinit-timer-state-schema-version)))
            ;; Mock to track catch-up and prevent actual scheduling
            (cl-letf (((symbol-function 'elinit--timer-scheduler-tick) #'ignore)
                      ((symbol-function 'elinit-timer--trigger)
                       (lambda (_timer reason)
                         (when (eq reason 'catch-up)
                           (setq catch-up-triggered t))))
                      ;; Mock calendar to return a time between last-run and now
                      ((symbol-function 'elinit-timer--compute-next-run)
                       (lambda (_timer _from) 950.0))
                      ((symbol-function 'float-time) (lambda () 1000.0)))
              (elinit-timer-scheduler-start))
            ;; Verify state was loaded from file
            (should elinit--timer-state-loaded)
            ;; Verify catch-up was triggered
            (should catch-up-triggered))
        (when (file-exists-p temp-file) (delete-file temp-file))
        (clrhash elinit--timer-state)))))

(ert-deftest elinit-test-timer-catch-up-no-double-trigger ()
  "Catch-up followed by tick does not double-trigger the same timer.
After catch-up, :next-run-at must not remain in the past."
  (let* ((elinit-mode t)
         (timer (elinit-timer--create :id "t1" :target "s1"
                                          :on-calendar '(:minute 0)
                                          :persistent t :enabled t))
         (elinit--timer-list (list timer))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--timer-scheduler nil)
         (elinit--shutting-down nil)
         (elinit--scheduler-startup-time 1000.0)
         (elinit-timer-catch-up-limit 86400)
         (trigger-reasons nil))
    ;; Seed state: last run was 100s ago, next-run in the past
    (puthash "t1" '(:last-run-at 900.0 :next-run-at 950.0)
             elinit--timer-state)
    (cl-letf (((symbol-function 'elinit-timer--trigger)
               (lambda (_timer reason)
                 (push reason trigger-reasons)))
              ((symbol-function 'elinit-timer--compute-next-run)
               (lambda (_timer _from) 950.0))
              ((symbol-function 'float-time) (lambda () 1000.0))
              ((symbol-function 'run-at-time) (lambda (&rest _) nil)))
      ;; Process catch-ups, then tick
      (elinit-timer--process-catch-ups)
      ;; :next-run-at must be nil (cleared because still in past)
      (should-not (plist-get (gethash "t1" elinit--timer-state)
                             :next-run-at))
      ;; Now run tick -- should NOT trigger again
      (elinit--timer-scheduler-tick)
      ;; Only catch-up should have fired, not scheduled
      (should (equal '(catch-up) (reverse trigger-reasons))))
    (clrhash elinit--timer-state)))

(ert-deftest elinit-test-timer-catch-up-simple-target ()
  "Catch-up fires for simple target type timer."
  (let* ((elinit-mode t)
         (timer (elinit-timer--create :id "t1" :target "s1"
                                          :on-calendar '(:minute 0)
                                          :persistent t :enabled t))
         (elinit--timer-list (list timer))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--scheduler-startup-time 1000.0)
         (elinit-timer-catch-up-limit 86400)
         (entry (list "s1" "echo hi" 0 t nil nil nil nil 'simple nil nil
                      nil nil nil nil nil nil nil nil nil nil nil nil
                      nil nil nil nil nil nil nil nil nil nil))
         (triggered nil))
    (puthash "t1" '(:last-run-at 900.0 :last-success-at 900.0)
             elinit--timer-state)
    (cl-letf (((symbol-function 'elinit-timer--compute-next-run)
               (lambda (_timer _from) 950.0))
              ((symbol-function 'float-time) (lambda () 1000.0))
              ((symbol-function 'elinit-timer--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'elinit--get-effective-enabled)
               (lambda (_id _p) t))
              ((symbol-function 'elinit--start-entry-async)
               (lambda (_entry _cb) (setq triggered 'simple)))
              ((symbol-function 'elinit-timer--save-state) #'ignore)
              ((symbol-function 'elinit--emit-event) #'ignore)
              ((symbol-function 'elinit--log) #'ignore))
      (elinit-timer--process-catch-ups)
      (should (eq 'simple triggered)))
    (clrhash elinit--timer-state)))

(ert-deftest elinit-test-timer-catch-up-target-type ()
  "Catch-up fires for target-type timer."
  (let* ((elinit-mode t)
         (timer (elinit-timer--create :id "t1" :target "app.target"
                                          :on-calendar '(:minute 0)
                                          :persistent t :enabled t))
         (elinit--timer-list (list timer))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--scheduler-startup-time 1000.0)
         (elinit-timer-catch-up-limit 86400)
         (entry (list "app.target" nil 0 t nil nil nil nil 'target nil nil
                      nil nil nil nil nil nil nil nil nil nil nil nil
                      nil nil nil nil nil nil nil nil nil nil))
         (elinit--mask-override (make-hash-table :test 'equal))
         (triggered-reason nil))
    (puthash "t1" '(:last-run-at 900.0 :last-success-at 900.0)
             elinit--timer-state)
    (cl-letf (((symbol-function 'elinit-timer--compute-next-run)
               (lambda (_timer _from) 950.0))
              ((symbol-function 'float-time) (lambda () 1000.0))
              ((symbol-function 'elinit-timer--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'elinit--get-effective-enabled)
               (lambda (_id _p) t))
              ((symbol-function 'elinit-timer--trigger-target)
               (lambda (_timer reason)
                 (setq triggered-reason reason)))
              ((symbol-function 'elinit-timer--save-state) #'ignore)
              ((symbol-function 'elinit--emit-event) #'ignore)
              ((symbol-function 'elinit--log) #'ignore))
      (elinit-timer--process-catch-ups)
      (should (eq 'catch-up triggered-reason)))
    (clrhash elinit--timer-state)))

(ert-deftest elinit-test-timer-scheduler-tick-handles-retry ()
  "Scheduler tick triggers retry when due."
  (let* ((elinit-mode t)
         (timer (elinit-timer--create :id "t1" :target "s1" :enabled t))
         (elinit--timer-list (list timer))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--timer-scheduler nil)
         (elinit--shutting-down nil)
         (triggered-reason nil))
    ;; Set up state with pending retry
    (puthash "t1" '(:retry-next-at 999.0 :next-run-at 2000.0)
             elinit--timer-state)
    ;; Mock trigger to capture reason
    (cl-letf (((symbol-function 'elinit-timer--trigger)
               (lambda (_timer reason) (setq triggered-reason reason)))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      (elinit--timer-scheduler-tick)
      (should (eq 'retry triggered-reason)))
    (clrhash elinit--timer-state)))

(ert-deftest elinit-test-timer-retry-budget-reset-on-scheduled ()
  "Retry budget is reset on fresh scheduled trigger."
  (let* ((elinit-mode t)
         (timer (elinit-timer--create :id "t1" :target "s1" :enabled t))
         (elinit--timer-list (list timer))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--timer-scheduler nil)
         (elinit--shutting-down nil))
    ;; Set up state with exhausted retry budget and due scheduled run
    (puthash "t1" '(:retry-attempt 3 :retry-next-at nil :next-run-at 999.0)
             elinit--timer-state)
    ;; Mock trigger and update functions
    (cl-letf (((symbol-function 'elinit-timer--trigger)
               (lambda (_timer _reason) nil))
              ((symbol-function 'elinit-timer--update-next-run)
               (lambda (_id) nil))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      (elinit--timer-scheduler-tick)
      ;; Retry budget should be reset
      (let ((state (gethash "t1" elinit--timer-state)))
        (should (= 0 (plist-get state :retry-attempt)))
        (should-not (plist-get state :retry-next-at))))
    (clrhash elinit--timer-state)))

(ert-deftest elinit-test-timer-scheduler-tick-scheduled ()
  "Scheduler tick triggers scheduled run when due."
  (let* ((elinit-mode t)
         (timer (elinit-timer--create :id "t1" :target "s1" :enabled t))
         (elinit--timer-list (list timer))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--timer-scheduler nil)
         (elinit--shutting-down nil)
         (triggered-reason nil))
    ;; Set up state with scheduled run due
    (puthash "t1" '(:next-run-at 999.0) elinit--timer-state)
    ;; Mock trigger to capture reason
    (cl-letf (((symbol-function 'elinit-timer--trigger)
               (lambda (_timer reason) (setq triggered-reason reason)))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      (elinit--timer-scheduler-tick)
      (should (eq 'scheduled triggered-reason)))
    (clrhash elinit--timer-state)))

(ert-deftest elinit-test-timer-scheduler-tick-disabled-skips ()
  "Scheduler tick skips disabled timers."
  (let* ((timer (elinit-timer--create :id "t1" :target "s1" :enabled nil))
         (elinit--timer-list (list timer))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--timer-scheduler nil)
         (elinit--shutting-down nil)
         (triggered nil))
    ;; Set up state with scheduled run due
    (puthash "t1" '(:next-run-at 999.0) elinit--timer-state)
    ;; Mock trigger to detect if called
    (cl-letf (((symbol-function 'elinit-timer--trigger)
               (lambda (_timer _reason) (setq triggered t)))
              ((symbol-function 'float-time) (lambda () 1000.0))
              ((symbol-function 'run-at-time) (lambda (&rest _) nil)))
      (elinit--timer-scheduler-tick)
      ;; Should not trigger disabled timer
      (should-not triggered))
    (clrhash elinit--timer-state)))

(ert-deftest elinit-test-timer-scheduler-tick-simultaneous-order ()
  "Scheduler tick processes simultaneous due timers in list order."
  (let* ((elinit-mode t)
         (timer1 (elinit-timer--create :id "t1" :target "s1" :enabled t))
         (timer2 (elinit-timer--create :id "t2" :target "s2" :enabled t))
         (timer3 (elinit-timer--create :id "t3" :target "s3" :enabled t))
         ;; List order is t1, t2, t3
         (elinit--timer-list (list timer1 timer2 timer3))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--timer-scheduler nil)
         (elinit--shutting-down nil)
         (trigger-order nil))
    ;; All timers due at same time
    (puthash "t1" '(:next-run-at 999.0) elinit--timer-state)
    (puthash "t2" '(:next-run-at 999.0) elinit--timer-state)
    (puthash "t3" '(:next-run-at 999.0) elinit--timer-state)
    ;; Capture trigger order
    (cl-letf (((symbol-function 'elinit-timer--trigger)
               (lambda (timer _reason)
                 (push (elinit-timer-id timer) trigger-order)))
              ((symbol-function 'elinit-timer--update-next-run)
               (lambda (_id) nil))
              ((symbol-function 'float-time) (lambda () 1000.0))
              ((symbol-function 'run-at-time) (lambda (&rest _) nil)))
      (elinit--timer-scheduler-tick)
      ;; Should process in list order: t1, t2, t3
      (should (equal '("t1" "t2" "t3") (nreverse trigger-order))))
    (clrhash elinit--timer-state)))

(ert-deftest elinit-test-cli-relative-time-formatter ()
  "Relative time formatter handles various time differences."
  ;; Test with mock time at 100000
  (cl-letf (((symbol-function 'float-time) (lambda () 100000.0)))
    ;; Recent past (seconds)
    (should (string-match "\\`[0-9]+s ago\\'" (elinit--cli-format-relative-time 99990.0)))
    ;; Minutes ago
    (should (string-match "\\`[0-9]+m ago\\'" (elinit--cli-format-relative-time 99800.0)))
    ;; Hours ago (2h = 7200s, so 100000 - 10000 = 90000)
    (should (string-match "\\`[0-9]+h ago\\'" (elinit--cli-format-relative-time 90000.0)))
    ;; Future (in X)
    (should (string-match "\\`in [0-9]+s\\'" (elinit--cli-format-relative-time 100010.0)))
    (should (string-match "\\`in [0-9]+m\\'" (elinit--cli-format-relative-time 100200.0)))
    ;; Nil returns dash
    (should (equal "-" (elinit--cli-format-relative-time nil)))))

(ert-deftest elinit-test-cli-relative-time-boundaries ()
  "Relative time formatter handles exact boundaries correctly."
  (cl-letf (((symbol-function 'float-time) (lambda () 100000.0)))
    ;; Exactly 60 seconds (boundary between s and m)
    (should (string-match "\\`1m ago\\'" (elinit--cli-format-relative-time 99940.0)))
    ;; 59 seconds (should be seconds)
    (should (string-match "\\`59s ago\\'" (elinit--cli-format-relative-time 99941.0)))
    ;; Exactly 3600 seconds (boundary between m and h)
    (should (string-match "\\`1h ago\\'" (elinit--cli-format-relative-time 96400.0)))
    ;; 3599 seconds (should be minutes)
    (should (string-match "\\`60m ago\\'" (elinit--cli-format-relative-time 96401.0)))
    ;; Exactly 86400 seconds (boundary between h and d)
    (should (string-match "\\`1d ago\\'" (elinit--cli-format-relative-time 13600.0)))
    ;; Day path works for large values
    (should (string-match "\\`2d ago\\'" (elinit--cli-format-relative-time (- 100000.0 (* 2 86400)))))
    ;; Future day path
    (should (string-match "\\`in 2d\\'" (elinit--cli-format-relative-time (+ 100000.0 (* 2 86400)))))))

(ert-deftest elinit-test-timer-state-load-merges-correctly ()
  "Load timer state merges with existing runtime state."
  (let* ((elinit-mode t)
         (temp-file (make-temp-file "elinit-test-merge-" nil ".eld"))
         (elinit-timer-state-file temp-file)
         (elinit--timer-state (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; Create state file with persisted data
          (with-temp-file temp-file
            (insert ";; Test state\n")
            (pp '((version . 1)
                  (timestamp . "2024-01-01T00:00:00")
                  (timers . (("t1" :last-run-at 500.0 :last-success-at 500.0))))
                (current-buffer)))
          ;; Pre-populate runtime state
          (puthash "t1" '(:next-run-at 2000.0 :retry-attempt 1)
                   elinit--timer-state)
          ;; Load should merge
          (should (elinit-timer--load-state))
          (let ((state (gethash "t1" elinit--timer-state)))
            ;; Should have persisted keys from file
            (should (equal (plist-get state :last-run-at) 500.0))
            (should (equal (plist-get state :last-success-at) 500.0))
            ;; Should preserve runtime keys
            (should (equal (plist-get state :next-run-at) 2000.0))
            (should (equal (plist-get state :retry-attempt) 1))))
      (delete-file temp-file)
      (clrhash elinit--timer-state))))

(ert-deftest elinit-test-timer-state-v1-load-saves-as-v2 ()
  "Loading v1 state and saving writes v2 schema metadata."
  (let* ((elinit-mode t)
         (temp-file (make-temp-file "elinit-test-v1-upgrade-" nil ".eld"))
         (elinit-timer-state-file temp-file)
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--timer-state-loaded nil))
    (unwind-protect
        (progn
          ;; Write v1 state file
          (with-temp-file temp-file
            (pp '((version . 1)
                  (timestamp . "2024-01-01T00:00:00")
                  (timers . (("t1" :last-run-at 500.0
                              :last-success-at 500.0))))
                (current-buffer)))
          ;; Load v1
          (should (elinit-timer--load-state))
          ;; Save -- should write v2
          (elinit-timer--save-state)
          ;; Re-read file and verify version
          (let* ((data (with-temp-buffer
                         (insert-file-contents temp-file)
                         (read (current-buffer))))
                 (version (alist-get 'version data)))
            (should (= elinit-timer-state-schema-version version))
            (should (= 2 version))))
      (delete-file temp-file)
      (clrhash elinit--timer-state))))

(ert-deftest elinit-test-timer-multiple-triggers-earliest-wins ()
  "Timer with multiple triggers uses earliest due time."
  (let* ((timer (elinit-timer--create :id "t1" :target "s1"
                                          :on-startup-sec 60
                                          :on-unit-active-sec 120))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--scheduler-startup-time 1000.0))
    ;; Set last success for unit-active calculation
    (puthash "t1" '(:last-success-at 900.0) elinit--timer-state)
    ;; Startup trigger: 1000 + 60 = 1060
    ;; Unit-active trigger: 900 + 120 = 1020 (earlier)
    (let ((next (elinit-timer--compute-next-run timer 1001.0)))
      (should (= 1020.0 next)))
    (clrhash elinit--timer-state)))

;;; Timer Subsystem Gating Tests

(ert-deftest elinit-test-timer-gate-scheduler-start-noop ()
  "Scheduler start is a no-op when timer subsystem is disabled."
  (elinit-test-with-unit-files
      '(("true" :id "s1" :type oneshot))
    (let ((elinit-timer-subsystem-mode nil)
          (elinit-timers '((:id "t1" :target "s1" :on-startup-sec 60)))
          (elinit--timer-list nil)
          (elinit--timer-scheduler nil)
          (elinit--shutting-down nil)
          (elinit--scheduler-startup-time nil)
          (elinit--timer-state-loaded nil)
          (elinit-timer-state-file nil))
      ;; Start should do nothing when gated off
      (elinit-timer-scheduler-start)
      ;; Timer list should remain empty
      (should (null elinit--timer-list))
      ;; Scheduler should not be running
      (should (null elinit--timer-scheduler)))))

(ert-deftest elinit-test-timer-gate-state-save-noop ()
  "State save is a no-op when timer subsystem is disabled."
  (let* ((elinit-timer-subsystem-mode nil)
         (temp-file (concat (make-temp-name
                             (expand-file-name "elinit-test-gate-"
                                               temporary-file-directory))
                            ".eld"))
         (elinit-timer-state-file temp-file)
         (elinit--timer-state (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (puthash "t1" '(:last-run-at 1000.0) elinit--timer-state)
          ;; Save should return nil when gated off
          (should-not (elinit-timer--save-state))
          ;; File should NOT be created
          (should-not (file-exists-p temp-file)))
      (when (file-exists-p temp-file) (delete-file temp-file))
      (clrhash elinit--timer-state))))

(ert-deftest elinit-test-timer-gate-state-load-noop ()
  "State load is a no-op when timer subsystem is disabled."
  (let* ((elinit-timer-subsystem-mode nil)
         (temp-file (make-temp-file "elinit-test-gate-" nil ".eld"))
         (elinit-timer-state-file temp-file)
         (elinit--timer-state (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; Write a valid state file
          (with-temp-file temp-file
            (insert (format "((version . %d) (timestamp . \"test\") (timers . ((\"t1\" :last-run-at 1000.0))))"
                            elinit-timer-state-schema-version)))
          ;; Load should return nil when gated off
          (should-not (elinit-timer--load-state))
          ;; State should NOT be populated
          (should (= 0 (hash-table-count elinit--timer-state))))
      (delete-file temp-file)
      (clrhash elinit--timer-state))))

(ert-deftest elinit-test-timer-gate-scheduler-tick-noop ()
  "Scheduler tick is a no-op when timer subsystem is disabled."
  (let* ((elinit-timer-subsystem-mode nil)
         (timer (elinit-timer--create :id "t1" :target "s1" :enabled t
                                          :on-startup-sec 1))
         (elinit--timer-list (list timer))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--timer-scheduler nil)
         (elinit--shutting-down nil)
         (triggered nil))
    ;; Set up state so timer would be due
    (puthash "t1" '(:next-run-at 900.0) elinit--timer-state)
    ;; Mock trigger to detect if called
    (cl-letf (((symbol-function 'elinit-timer--trigger)
               (lambda (_timer _reason) (setq triggered t)))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      (elinit--timer-scheduler-tick))
    ;; Timer should NOT have been triggered
    (should-not triggered)
    (clrhash elinit--timer-state)))

(ert-deftest elinit-test-timer-gate-enabled-works ()
  "Timer functions work normally when subsystem is enabled."
  (let* ((elinit-mode t)  ; Parent mode must also be enabled
         (timer (elinit-timer--create :id "t1" :target "s1" :enabled t
                                          :on-startup-sec 1))
         (elinit--timer-list (list timer))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--timer-scheduler nil)
         (elinit--shutting-down nil)
         (triggered nil))
    ;; Set up state so timer would be due
    (puthash "t1" '(:next-run-at 900.0) elinit--timer-state)
    ;; Mock trigger to detect if called
    (cl-letf (((symbol-function 'elinit-timer--trigger)
               (lambda (_timer _reason) (setq triggered t)))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      (elinit--timer-scheduler-tick))
    ;; Timer SHOULD have been triggered when enabled
    (should triggered)
    ;; Clean up scheduler timer if created
    (when (timerp elinit--timer-scheduler)
      (cancel-timer elinit--timer-scheduler))
    (clrhash elinit--timer-state)))

(ert-deftest elinit-test-timer-gate-parent-mode-off ()
  "Timer subsystem is a no-op when parent elinit-mode is off."
  (let* ((elinit-mode nil)                 ; But parent mode is OFF
         (timer (elinit-timer--create :id "t1" :target "s1" :enabled t
                                          :on-startup-sec 1))
         (elinit--timer-list (list timer))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--timer-scheduler nil)
         (elinit--shutting-down nil)
         (triggered nil))
    ;; Set up state so timer would be due
    (puthash "t1" '(:next-run-at 900.0) elinit--timer-state)
    ;; Mock trigger to detect if called
    (cl-letf (((symbol-function 'elinit-timer--trigger)
               (lambda (_timer _reason) (setq triggered t)))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      (elinit--timer-scheduler-tick))
    ;; Timer should NOT be triggered when parent mode is off
    (should-not triggered)
    (clrhash elinit--timer-state)))

;;; Timer Phase 6: Expanded Coverage

(ert-deftest elinit-test-timer-trigger-simple-success ()
  "Timer trigger for simple service records success on spawn."
  (let* ((elinit-mode t)
         (timer (elinit-timer--create :id "t1" :target "svc" :enabled t))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--processes (make-hash-table :test 'equal))
         (entry (list "svc" nil 0 t nil nil nil nil 'simple nil nil
                      nil nil nil nil nil nil nil nil nil nil nil nil
                      nil nil nil nil nil nil nil nil nil nil))
         (async-callback nil))
    (puthash "t1" nil elinit--timer-state)
    (cl-letf (((symbol-function 'elinit-timer--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'elinit--get-effective-enabled)
               (lambda (_id _p) t))
              ((symbol-function 'elinit--start-entry-async)
               (lambda (_entry cb) (setq async-callback cb)))
              ((symbol-function 'elinit-timer--save-state) #'ignore)
              ((symbol-function 'elinit--emit-event) #'ignore)
              ((symbol-function 'elinit--timer-scheduler-tick) #'ignore)
              ((symbol-function 'elinit-timer--update-next-run) #'ignore))
      (elinit-timer--trigger timer 'scheduled)
      ;; Simulate successful spawn
      (funcall async-callback t)
      (let ((state (gethash "t1" elinit--timer-state)))
        (should (eq 'success (plist-get state :last-result)))
        (should (eq 'simple (plist-get state :last-target-type)))))))

(ert-deftest elinit-test-timer-trigger-simple-already-active ()
  "Timer trigger for already-running simple service records success no-op.
Stale retry state from a prior failure must be cleared."
  (let* ((elinit-mode t)
         (timer (elinit-timer--create :id "t1" :target "svc" :enabled t))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--processes (make-hash-table :test 'equal))
         (entry (list "svc" nil 0 t nil nil nil nil 'simple nil nil
                      nil nil nil nil nil nil nil nil nil nil nil nil
                      nil nil nil nil nil nil nil nil nil nil))
         (mock-proc (start-process "test" nil "sleep" "300")))
    (unwind-protect
        (progn
          ;; Seed with stale retry state from a prior failure
          (puthash "t1" '(:retry-attempt 2 :retry-next-at 55555.0)
                   elinit--timer-state)
          (puthash "svc" mock-proc elinit--processes)
          (cl-letf (((symbol-function 'elinit-timer--get-entry-for-id)
                     (lambda (_id) entry))
                    ((symbol-function 'elinit--get-effective-enabled)
                     (lambda (_id _p) t))
                    ((symbol-function 'elinit-timer--save-state) #'ignore)
                    ((symbol-function 'elinit--emit-event) #'ignore)
                    ((symbol-function 'elinit--timer-scheduler-tick) #'ignore)
                    ((symbol-function 'elinit-timer--update-next-run) #'ignore))
            (elinit-timer--trigger timer 'scheduled)
            (let ((state (gethash "t1" elinit--timer-state)))
              (should (eq 'success (plist-get state :last-result)))
              (should (eq 'already-active
                          (plist-get state :last-result-reason)))
              ;; Stale retry state must be cleared (success clears retry)
              (should (= 0 (plist-get state :retry-attempt)))
              (should-not (plist-get state :retry-next-at)))))
      (delete-process mock-proc))))

(ert-deftest elinit-test-timer-trigger-simple-spawn-failure-retries ()
  "Timer trigger for simple spawn failure schedules retry."
  (let* ((elinit-mode t)
         (timer (elinit-timer--create :id "t1" :target "svc" :enabled t))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--processes (make-hash-table :test 'equal))
         (elinit-timer-retry-intervals '(30 120 600))
         (entry (list "svc" nil 0 t nil nil nil nil 'simple nil nil
                      nil nil nil nil nil nil nil nil nil nil nil nil
                      nil nil nil nil nil nil nil nil nil nil))
         (async-callback nil))
    (puthash "t1" nil elinit--timer-state)
    (cl-letf (((symbol-function 'elinit-timer--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'elinit--get-effective-enabled)
               (lambda (_id _p) t))
              ((symbol-function 'elinit--start-entry-async)
               (lambda (_entry cb) (setq async-callback cb)))
              ((symbol-function 'elinit-timer--save-state) #'ignore)
              ((symbol-function 'elinit--emit-event) #'ignore)
              ((symbol-function 'elinit--timer-scheduler-tick) #'ignore)
              ((symbol-function 'elinit-timer--update-next-run) #'ignore))
      (elinit-timer--trigger timer 'scheduled)
      ;; Simulate spawn failure
      (funcall async-callback nil)
      (let ((state (gethash "t1" elinit--timer-state)))
        (should (eq 'failure (plist-get state :last-result)))
        (should (eq 'spawn-failed (plist-get state :last-result-reason)))
        ;; Retry should be scheduled
        (should (= 1 (plist-get state :retry-attempt)))
        (should (plist-get state :retry-next-at))))))

(ert-deftest elinit-test-timer-oneshot-complete-success ()
  "Oneshot completion callback records success with exit 0.
Exercises elinit-timer--on-target-complete success branch end-to-end:
sets last-success-at, last-exit, last-result, clears retry state."
  (let* ((elinit-mode t)
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--oneshot-completed (make-hash-table :test 'equal))
         (elinit--timer-list nil))
    ;; Seed with stale retry state
    (puthash "t1" '(:retry-attempt 2 :retry-next-at 33333.0)
             elinit--timer-state)
    ;; Oneshot exited with code 0
    (puthash "svc" 0 elinit--oneshot-completed)
    (cl-letf (((symbol-function 'elinit-timer--save-state) #'ignore)
              ((symbol-function 'elinit--emit-event) #'ignore)
              ((symbol-function 'elinit-timer--update-next-run) #'ignore)
              ((symbol-function 'elinit--timer-scheduler-tick) #'ignore))
      (elinit-timer--on-target-complete "t1" "svc" t)
      (let ((state (gethash "t1" elinit--timer-state)))
        (should (eq 'success (plist-get state :last-result)))
        (should-not (plist-get state :last-result-reason))
        (should (= 0 (plist-get state :last-exit)))
        (should (plist-get state :last-success-at))
        ;; Retry state cleared
        (should (= 0 (plist-get state :retry-attempt)))
        (should-not (plist-get state :retry-next-at))))))

(ert-deftest elinit-test-timer-oneshot-complete-failure-retries ()
  "Oneshot completion callback records failure with non-zero exit and schedules retry."
  (let* ((elinit-mode t)
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--oneshot-completed (make-hash-table :test 'equal))
         (elinit--timer-list nil)
         (elinit-timer-retry-intervals '(30 120 600)))
    (puthash "t1" nil elinit--timer-state)
    ;; Oneshot exited with code 1 (retryable)
    (puthash "svc" 1 elinit--oneshot-completed)
    (cl-letf (((symbol-function 'elinit-timer--save-state) #'ignore)
              ((symbol-function 'elinit--emit-event) #'ignore)
              ((symbol-function 'elinit-timer--update-next-run) #'ignore)
              ((symbol-function 'elinit--timer-scheduler-tick) #'ignore))
      (elinit-timer--on-target-complete "t1" "svc" nil)
      (let ((state (gethash "t1" elinit--timer-state)))
        (should (eq 'failure (plist-get state :last-result)))
        (should (= 1 (plist-get state :last-exit)))
        (should (plist-get state :last-failure-at))
        ;; Retry scheduled
        (should (= 1 (plist-get state :retry-attempt)))
        (should (plist-get state :retry-next-at))))))

(ert-deftest elinit-test-timer-oneshot-complete-signal-no-retry ()
  "Oneshot killed by signal records failure but does not schedule retry."
  (let* ((elinit-mode t)
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--oneshot-completed (make-hash-table :test 'equal))
         (elinit--timer-list nil)
         (elinit-timer-retry-intervals '(30 120 600)))
    (puthash "t1" nil elinit--timer-state)
    ;; Signal death encoded as negative exit code
    (puthash "svc" -9 elinit--oneshot-completed)
    (cl-letf (((symbol-function 'elinit-timer--save-state) #'ignore)
              ((symbol-function 'elinit--emit-event) #'ignore)
              ((symbol-function 'elinit-timer--update-next-run) #'ignore)
              ((symbol-function 'elinit--timer-scheduler-tick) #'ignore))
      (elinit-timer--on-target-complete "t1" "svc" nil)
      (let ((state (gethash "t1" elinit--timer-state)))
        (should (eq 'failure (plist-get state :last-result)))
        (should (= -9 (plist-get state :last-exit)))
        ;; Signal deaths are NOT retryable
        (should (= 0 (or (plist-get state :retry-attempt) 0)))
        (should-not (plist-get state :retry-next-at))))))

(ert-deftest elinit-test-timer-trigger-target-reached ()
  "Timer trigger for target records success on reached convergence.
Stale retry state from a prior failure must be cleared."
  (let* ((elinit-mode t)
         (timer (elinit-timer--create :id "t1" :target "app.target"
                                          :enabled t))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--processes (make-hash-table :test 'equal))
         (elinit--target-convergence (make-hash-table :test 'equal))
         (elinit--target-converging (make-hash-table :test 'equal))
         (elinit--current-plan t)
         (entry (list "app.target" nil 0 t nil nil nil nil 'target nil nil
                      nil nil nil nil nil nil nil nil nil nil nil nil
                      nil nil nil nil nil nil nil nil nil nil)))
    ;; Target already reached -- no-op success
    (puthash "app.target" 'reached elinit--target-convergence)
    ;; Seed with stale retry state from a prior failure
    (puthash "t1" '(:retry-attempt 3 :retry-next-at 44444.0)
             elinit--timer-state)
    (cl-letf (((symbol-function 'elinit-timer--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'elinit--get-effective-enabled)
               (lambda (_id _p) t))
              ((symbol-function 'elinit-timer--save-state) #'ignore)
              ((symbol-function 'elinit--emit-event) #'ignore)
              ((symbol-function 'elinit--timer-scheduler-tick) #'ignore)
              ((symbol-function 'elinit-timer--update-next-run) #'ignore))
      (elinit-timer--trigger timer 'scheduled)
      (let ((state (gethash "t1" elinit--timer-state)))
        (should (eq 'success (plist-get state :last-result)))
        (should (eq 'already-reached
                    (plist-get state :last-result-reason)))
        ;; Stale retry state must be cleared (success clears retry)
        (should (= 0 (plist-get state :retry-attempt)))
        (should-not (plist-get state :retry-next-at))))))

(ert-deftest elinit-test-timer-trigger-target-converging-skips ()
  "Timer trigger for converging target records miss and skip result."
  (let* ((elinit-mode t)
         (timer (elinit-timer--create :id "t1" :target "app.target"
                                          :enabled t))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--processes (make-hash-table :test 'equal))
         (elinit--target-convergence (make-hash-table :test 'equal))
         (elinit--target-converging (make-hash-table :test 'equal))
         (entry (list "app.target" nil 0 t nil nil nil nil 'target nil nil
                      nil nil nil nil nil nil nil nil nil nil nil nil
                      nil nil nil nil nil nil nil nil nil nil)))
    ;; Target is converging
    (puthash "app.target" t elinit--target-converging)
    ;; Seed with stale retry state from a prior failure
    (puthash "t1" '(:retry-attempt 1 :retry-next-at 77777.0)
             elinit--timer-state)
    (cl-letf (((symbol-function 'elinit-timer--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'elinit--get-effective-enabled)
               (lambda (_id _p) t))
              ((symbol-function 'elinit-timer--save-state) #'ignore)
              ((symbol-function 'elinit--emit-event) #'ignore)
              ((symbol-function 'elinit--timer-scheduler-tick) #'ignore))
      (let ((result (elinit-timer--trigger timer 'scheduled)))
        (should-not result)
        (let ((state (gethash "t1" elinit--timer-state)))
          (should (eq 'target-converging
                      (plist-get state :last-miss-reason)))
          (should (eq 'skip (plist-get state :last-result)))
          (should (eq 'target-converging
                      (plist-get state :last-result-reason)))
          ;; Stale retry state must be cleared
          (should (= 0 (plist-get state :retry-attempt)))
          (should-not (plist-get state :retry-next-at)))))))

(ert-deftest elinit-test-timer-target-degraded-retries ()
  "Timer target convergence to degraded schedules retry."
  (let* ((elinit-mode t)
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--target-convergence (make-hash-table :test 'equal))
         (elinit-timer-retry-intervals '(30 120 600)))
    (puthash "t1" '(:retry-attempt 0) elinit--timer-state)
    (puthash "app.target" 'degraded elinit--target-convergence)
    (cl-letf (((symbol-function 'elinit-timer--save-state) #'ignore)
              ((symbol-function 'elinit--emit-event) #'ignore)
              ((symbol-function 'elinit--timer-scheduler-tick) #'ignore)
              ((symbol-function 'elinit-timer--update-next-run) #'ignore))
      (elinit-timer--on-target-converge "t1" "app.target")
      (let ((state (gethash "t1" elinit--timer-state)))
        (should (eq 'failure (plist-get state :last-result)))
        (should (eq 'target-degraded (plist-get state :last-result-reason)))
        ;; Retry should be scheduled
        (should (= 1 (plist-get state :retry-attempt)))
        (should (plist-get state :retry-next-at))))))

(ert-deftest elinit-test-timer-unit-active-oneshot-anchors-on-success ()
  "on-unit-active for oneshot timer anchors on last success."
  (let* ((timer (elinit-timer--create :id "t1" :target "svc" :enabled t
                                          :on-unit-active-sec 120))
         (elinit--timer-state (make-hash-table :test 'equal)))
    (puthash "t1" '(:last-success-at 800.0) elinit--timer-state)
    (let ((next (elinit-timer--next-unit-active-time timer)))
      (should (= 920.0 next)))))

(ert-deftest elinit-test-timer-unit-active-simple-anchors-on-success ()
  "on-unit-active for simple timer anchors on last success."
  (let* ((timer (elinit-timer--create :id "t1" :target "svc" :enabled t
                                          :on-unit-active-sec 300))
         (elinit--timer-state (make-hash-table :test 'equal)))
    (puthash "t1" '(:last-success-at 1000.0) elinit--timer-state)
    (let ((next (elinit-timer--next-unit-active-time timer)))
      (should (= 1300.0 next)))))

(ert-deftest elinit-test-timer-unit-active-target-anchors-on-success ()
  "on-unit-active for target timer anchors on last success."
  (let* ((timer (elinit-timer--create :id "t1" :target "app.target"
                                          :enabled t :on-unit-active-sec 600))
         (elinit--timer-state (make-hash-table :test 'equal)))
    (puthash "t1" '(:last-success-at 2000.0) elinit--timer-state)
    (let ((next (elinit-timer--next-unit-active-time timer)))
      (should (= 2600.0 next)))))

(ert-deftest elinit-test-timer-overlap-no-retry ()
  "Overlap skip clears stale retry state from prior failure."
  (let* ((elinit-mode t)
         (timer (elinit-timer--create :id "t1" :target "svc" :enabled t))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--processes (make-hash-table :test 'equal))
         (entry (list "svc" nil 0 t nil nil nil nil 'oneshot nil nil
                      nil nil nil nil nil nil nil nil nil nil nil nil
                      nil nil nil nil nil nil nil nil nil nil))
         (mock-proc (start-process "test" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "svc" mock-proc elinit--processes)
          ;; Seed with stale retry state from a prior failure
          (puthash "t1" '(:retry-attempt 1 :retry-next-at 99999.0)
                   elinit--timer-state)
          (cl-letf (((symbol-function 'elinit-timer--get-entry-for-id)
                     (lambda (_id) entry))
                    ((symbol-function 'elinit--get-effective-enabled)
                     (lambda (_id _p) t))
                    ((symbol-function 'elinit-timer--save-state) #'ignore)
                    ((symbol-function 'elinit--emit-event) #'ignore))
            (elinit-timer--trigger timer 'scheduled)
            (let ((state (gethash "t1" elinit--timer-state)))
              ;; Overlap recorded as miss and skip result
              (should (eq 'overlap (plist-get state :last-miss-reason)))
              (should (eq 'skip (plist-get state :last-result)))
              (should (eq 'overlap (plist-get state :last-result-reason)))
              ;; Stale retry state must be cleared
              (should (= 0 (plist-get state :retry-attempt)))
              (should-not (plist-get state :retry-next-at)))))
      (delete-process mock-proc))))

(ert-deftest elinit-test-timer-missing-target-records-failure ()
  "Missing target at runtime records failure with target-not-found."
  (let* ((elinit-mode t)
         (timer (elinit-timer--create :id "t1" :target "gone" :enabled t))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--mask-override (make-hash-table :test 'equal)))
    (puthash "t1" nil elinit--timer-state)
    (cl-letf (((symbol-function 'elinit-timer--get-entry-for-id)
               (lambda (_id) nil))
              ((symbol-function 'elinit-timer--save-state) #'ignore)
              ((symbol-function 'elinit--emit-event) #'ignore)
              ((symbol-function 'elinit-timer--update-next-run) #'ignore))
      (elinit-timer--trigger timer 'scheduled)
      (let ((state (gethash "t1" elinit--timer-state)))
        (should (eq 'failure (plist-get state :last-result)))
        (should (eq 'target-not-found
                    (plist-get state :last-result-reason)))))))

(ert-deftest elinit-test-timer-masked-target-skips ()
  "Masked target skips with masked-target miss and skip result."
  (let* ((elinit-mode t)
         (timer (elinit-timer--create :id "t1" :target "svc" :enabled t))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--mask-override (make-hash-table :test 'equal))
         (entry (list "svc" nil 0 t nil nil nil nil 'simple nil nil
                      nil nil nil nil nil nil nil nil nil nil nil nil
                      nil nil nil nil nil nil nil nil nil nil)))
    (puthash "svc" 'masked elinit--mask-override)
    (puthash "t1" '(:retry-attempt 1 :retry-next-at 66666.0)
             elinit--timer-state)
    (cl-letf (((symbol-function 'elinit-timer--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'elinit-timer--save-state) #'ignore)
              ((symbol-function 'elinit--emit-event) #'ignore)
              ((symbol-function 'elinit--log) #'ignore))
      (elinit-timer--trigger timer 'scheduled)
      (let ((state (gethash "t1" elinit--timer-state)))
        (should (eq 'masked-target
                    (plist-get state :last-miss-reason)))
        (should (eq 'skip (plist-get state :last-result)))
        (should (eq 'masked-target
                    (plist-get state :last-result-reason)))
        (should (eq 0 (plist-get state :retry-attempt)))
        (should-not (plist-get state :retry-next-at))))))

(ert-deftest elinit-test-timer-convergence-nil-is-failure ()
  "Nil convergence state is classified as failure, not success."
  (let* ((elinit-mode t)
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--target-convergence (make-hash-table :test 'equal)))
    (puthash "t1" nil elinit--timer-state)
    ;; No convergence entry for app.target -- nil case
    (cl-letf (((symbol-function 'elinit-timer--save-state) #'ignore)
              ((symbol-function 'elinit--emit-event) #'ignore)
              ((symbol-function 'elinit--timer-scheduler-tick) #'ignore)
              ((symbol-function 'elinit-timer--update-next-run) #'ignore))
      (elinit-timer--on-target-converge "t1" "app.target")
      (let ((state (gethash "t1" elinit--timer-state)))
        (should (eq 'failure (plist-get state :last-result)))
        (should (eq 'convergence-unknown
                    (plist-get state :last-result-reason)))))))

(ert-deftest elinit-test-timer-convergence-converging-is-failure ()
  "Converging state at callback time is classified as failure."
  (let* ((elinit-mode t)
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--target-convergence (make-hash-table :test 'equal))
         (elinit-timer-retry-intervals '(30)))
    (puthash "t1" '(:retry-attempt 0) elinit--timer-state)
    (puthash "app.target" 'converging elinit--target-convergence)
    (cl-letf (((symbol-function 'elinit-timer--save-state) #'ignore)
              ((symbol-function 'elinit--emit-event) #'ignore)
              ((symbol-function 'elinit--timer-scheduler-tick) #'ignore)
              ((symbol-function 'elinit-timer--update-next-run) #'ignore))
      (elinit-timer--on-target-converge "t1" "app.target")
      (let ((state (gethash "t1" elinit--timer-state)))
        (should (eq 'failure (plist-get state :last-result)))
        (should (eq 'target-not-converged
                    (plist-get state :last-result-reason)))
        ;; Should schedule retry
        (should (= 1 (plist-get state :retry-attempt)))))))

(ert-deftest elinit-test-timer-target-trigger-uses-closure ()
  "Target timer trigger only starts entries in the target's closure."
  (let* ((elinit-mode t)
         (timer (elinit-timer--create :id "t1" :target "app.target"
                                          :enabled t))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--processes (make-hash-table :test 'equal))
         (elinit--target-convergence (make-hash-table :test 'equal))
         (elinit--target-converging (make-hash-table :test 'equal))
         (elinit--target-members (make-hash-table :test 'equal))
         (elinit--target-member-reverse (make-hash-table :test 'equal))
         (programs '(("echo a" :id "svc-a" :wanted-by ("app.target"))
                     ("echo b" :id "svc-b" :wanted-by ("other.target"))
                     (nil :id "app.target" :type target)
                     (nil :id "other.target" :type target)))
         (plan (elinit--build-plan programs))
         (elinit--current-plan plan)
         (started-ids nil)
         (entry (list "app.target" nil 0 t nil nil nil nil 'target nil nil
                      nil nil nil nil nil nil nil nil nil nil nil nil
                      nil nil nil nil nil nil nil nil nil nil)))
    (puthash "t1" nil elinit--timer-state)
    (cl-letf (((symbol-function 'elinit-timer--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'elinit--get-effective-enabled)
               (lambda (_id _p) t))
              ((symbol-function 'elinit-timer--save-state) #'ignore)
              ((symbol-function 'elinit--emit-event) #'ignore)
              ((symbol-function 'elinit--timer-scheduler-tick) #'ignore)
              ((symbol-function 'elinit-timer--update-next-run) #'ignore)
              ((symbol-function 'elinit--dag-start-with-deps)
               (lambda (entries callback)
                 (dolist (e entries)
                   (push (elinit-entry-id e) started-ids))
                 (funcall callback))))
      (elinit-timer--trigger timer 'scheduled)
      ;; svc-a and app.target should be in DAG (closure members),
      ;; but not svc-b (in other.target closure)
      (should (member "svc-a" started-ids))
      (should (member "app.target" started-ids))
      (should-not (member "svc-b" started-ids)))))

(ert-deftest elinit-test-timer-target-trigger-convergence-success ()
  "Target timer trigger reaches success when DAG processes convergence."
  (let* ((elinit-mode t)
         (timer (elinit-timer--create :id "t1" :target "app.target"
                                          :enabled t))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--processes (make-hash-table :test 'equal))
         (elinit--target-convergence (make-hash-table :test 'equal))
         (elinit--target-converging (make-hash-table :test 'equal))
         (elinit--target-members (make-hash-table :test 'equal))
         (elinit--target-member-reverse (make-hash-table :test 'equal))
         (elinit--entry-state (make-hash-table :test 'equal))
         (programs '(("echo a" :id "svc-a" :wanted-by ("app.target"))
                     (nil :id "app.target" :type target)))
         (plan (elinit--build-plan programs))
         (elinit--current-plan plan)
         (entry (list "app.target" nil 0 t nil nil nil nil 'target nil nil
                      nil nil nil nil nil nil nil nil nil nil nil nil
                      nil nil nil nil nil nil nil nil nil nil)))
    (puthash "t1" nil elinit--timer-state)
    (cl-letf (((symbol-function 'elinit-timer--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'elinit--get-effective-enabled)
               (lambda (_id _p) t))
              ((symbol-function 'elinit-timer--save-state) #'ignore)
              ((symbol-function 'elinit--emit-event) #'ignore)
              ((symbol-function 'elinit--timer-scheduler-tick) #'ignore)
              ((symbol-function 'elinit-timer--update-next-run) #'ignore)
              ((symbol-function 'elinit--dag-start-with-deps)
               (lambda (entries callback)
                 ;; Simulate DAG processing: target entry triggers
                 ;; begin-convergence, service entries become ready
                 (dolist (e entries)
                   (let ((eid (elinit-entry-id e)))
                     (if (eq (elinit-entry-type e) 'target)
                         (elinit--target-begin-convergence eid)
                       ;; Mark service as ready (simulates successful start)
                       (elinit--dag-mark-ready eid))))
                 (funcall callback))))
      (elinit-timer--trigger timer 'scheduled)
      ;; Target should have converged to reached
      (should (eq 'reached
                  (gethash "app.target" elinit--target-convergence)))
      ;; Timer state should record success
      (let ((state (gethash "t1" elinit--timer-state)))
        (should (eq 'success (plist-get state :last-result)))
        (should (eq 'target-reached
                    (plist-get state :last-result-reason)))))))

(ert-deftest elinit-test-timer-disabled-timer-records-skip-result ()
  "Disabled timer records skip and clears stale retry state."
  (let* ((elinit-mode t)
         (timer (elinit-timer--create :id "t1" :target "svc"
                                          :enabled nil))
         (elinit--timer-state (make-hash-table :test 'equal)))
    ;; Seed with stale retry state from a prior failure
    (puthash "t1" '(:retry-attempt 2 :retry-next-at 88888.0)
             elinit--timer-state)
    (cl-letf (((symbol-function 'elinit-timer--save-state) #'ignore)
              ((symbol-function 'elinit--emit-event) #'ignore)
              ((symbol-function 'elinit--log) #'ignore))
      (elinit-timer--trigger timer 'scheduled)
      (let ((state (gethash "t1" elinit--timer-state)))
        (should (eq 'disabled (plist-get state :last-miss-reason)))
        (should (eq 'skip (plist-get state :last-result)))
        (should (eq 'disabled (plist-get state :last-result-reason)))
        ;; Stale retry state must be cleared
        (should (= 0 (plist-get state :retry-attempt)))
        (should-not (plist-get state :retry-next-at))))))

(ert-deftest elinit-test-cli-list-timers-json-v2-fields ()
  "CLI list-timers JSON includes v2 fields: target_type, last_result.
Target type is resolved from current config, not runtime state."
  (let* ((elinit-mode t)
         (timer (elinit-timer--create :id "t1" :target "svc"
                                          :enabled t :persistent t))
         (elinit--timer-list (list timer))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--invalid-timers (make-hash-table :test 'equal))
         (mock-entry (list "svc" "echo hi" 0 t nil nil nil nil 'oneshot nil nil
                           nil nil nil nil nil nil nil nil nil nil nil nil
                           nil nil nil nil nil nil nil nil nil nil)))
    (puthash "t1" '(:last-result success
                    :last-result-reason nil)
             elinit--timer-state)
    (cl-letf (((symbol-function 'elinit--get-entry-for-id)
               (lambda (_id) mock-entry)))
      (let ((result (elinit--cli-dispatch '("--json" "list-timers"))))
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (data (json-read-from-string
                      (elinit-cli-result-output result)))
               (timers (alist-get 'timers data))
               (entry (car timers)))
          (should (equal "success" (alist-get 'last_result entry)))
          (should (equal "oneshot" (alist-get 'target_type entry))))))))

(ert-deftest elinit-test-cli-list-timers-human-v2-columns ()
  "CLI list-timers human output includes TYPE and RESULT columns.
TYPE is resolved from current config."
  (let* ((elinit-mode t)
         (timer (elinit-timer--create :id "t1" :target "svc"
                                          :enabled t))
         (elinit--timer-list (list timer))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--invalid-timers (make-hash-table :test 'equal))
         (mock-entry (list "svc" "echo hi" 0 t nil nil nil nil 'simple nil nil
                           nil nil nil nil nil nil nil nil nil nil nil nil
                           nil nil nil nil nil nil nil nil nil nil)))
    (puthash "t1" '(:last-result failure)
             elinit--timer-state)
    (cl-letf (((symbol-function 'elinit--get-entry-for-id)
               (lambda (_id) mock-entry)))
      (let ((result (elinit--cli-dispatch '("list-timers"))))
        (let ((output (elinit-cli-result-output result)))
          ;; Header should have TYPE and RESULT columns
          (should (string-match-p "TYPE" output))
          (should (string-match-p "RESULT" output))
          ;; Data row should have the values
          (should (string-match-p "simple" output))
          (should (string-match-p "failure" output)))))))

(ert-deftest elinit-test-dashboard-timer-entry-v2-columns ()
  "Dashboard timer entry vector includes TYPE, RESULT, and REASON columns.
TYPE is resolved from current config, not runtime state."
  (let* ((timer (elinit-timer--create :id "t1" :target "svc" :enabled t))
         (elinit--timer-list (list timer))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--processes (make-hash-table :test 'equal))
         (mock-entry (list "svc" "echo hi" 0 t nil nil nil nil 'oneshot nil nil
                           nil nil nil nil nil nil nil nil nil nil nil nil
                           nil nil nil nil nil nil nil nil nil nil)))
    (puthash "t1" '(:last-result success
                    :last-result-reason already-active)
             elinit--timer-state)
    (cl-letf (((symbol-function 'elinit--get-entry-for-id)
               (lambda (_id) mock-entry)))
      (let ((entry (elinit--make-timer-dashboard-entry timer)))
        ;; Column 6 is REASON, column 7 is TYPE, column 8 is RESULT
        (should (string= "already-active" (aref entry 6)))
        (should (string= "oneshot" (aref entry 7)))
        (should (string-match-p "success" (aref entry 8)))))))

(ert-deftest elinit-test-dashboard-timer-entry-empty-v2-defaults ()
  "Dashboard timer entry shows dash for missing REASON and RESULT.
TYPE is resolved from config; shows dash when target not found."
  (let* ((timer (elinit-timer--create :id "t1" :target "svc" :enabled t))
         (elinit--timer-list (list timer))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--processes (make-hash-table :test 'equal)))
    ;; No state set, target not found in config
    (cl-letf (((symbol-function 'elinit--get-entry-for-id)
               (lambda (_id) nil)))
      (let ((entry (elinit--make-timer-dashboard-entry timer)))
        (should (string= "-" (aref entry 6)))
        (should (string= "-" (aref entry 7)))
        (should (string= "-" (aref entry 8)))))))

(ert-deftest elinit-test-dashboard-timer-entry-failure-result-reason ()
  "Dashboard timer entry shows result-reason for failure outcomes."
  (let* ((timer (elinit-timer--create :id "t1" :target "svc" :enabled t))
         (elinit--timer-list (list timer))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--processes (make-hash-table :test 'equal))
         (mock-entry (list "svc" "echo hi" 0 t nil nil nil nil 'oneshot nil nil
                           nil nil nil nil nil nil nil nil nil nil nil nil
                           nil nil nil nil nil nil nil nil nil nil)))
    (puthash "t1" '(:last-result failure
                    :last-result-reason spawn-failed)
             elinit--timer-state)
    (cl-letf (((symbol-function 'elinit--get-entry-for-id)
               (lambda (_id) mock-entry)))
      (let ((entry (elinit--make-timer-dashboard-entry timer)))
        ;; Column 6 is REASON showing result-reason, not miss-reason
        (should (string= "spawn-failed" (aref entry 6)))
        (should (string-match-p "failure" (aref entry 8)))))))

(ert-deftest elinit-test-dashboard-timer-entry-fresh-timer-shows-type ()
  "Fresh timer (never triggered) shows TYPE resolved from current config."
  (let* ((timer (elinit-timer--create :id "t1" :target "svc" :enabled t))
         (elinit--timer-list (list timer))
         (elinit--timer-state (make-hash-table :test 'equal))
         (elinit--processes (make-hash-table :test 'equal))
         (mock-entry (list "svc" "echo hi" 0 t nil nil nil nil 'simple nil nil
                           nil nil nil nil nil nil nil nil nil nil nil nil
                           nil nil nil nil nil nil nil nil nil nil)))
    ;; No state at all -- timer has never triggered
    (cl-letf (((symbol-function 'elinit--get-entry-for-id)
               (lambda (_id) mock-entry)))
      (let ((entry (elinit--make-timer-dashboard-entry timer)))
        ;; TYPE column shows resolved type even without runtime state
        (should (string= "simple" (aref entry 7)))))))


;;; Targets-Related Timer Tests

(ert-deftest elinit-test-builtin-timers-present ()
  "Built-in timers are included when `elinit-timers' is nil."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit--programs-cache :not-yet-loaded)
         (elinit--unit-file-invalid (make-hash-table :test 'equal)))
    (elinit-test--write-unit-files
     dir '(("echo rotate" :id "logrotate" :type oneshot)))
    (unwind-protect
        (let* ((elinit-timers nil)
               (programs (elinit--effective-programs))
               (plan (elinit--build-plan programs))
               (timers (elinit-timer-build-list plan)))
          (should (cl-find "logrotate-daily" timers
                           :key #'elinit-timer-id
                           :test #'equal))
          (should (cl-find "log-prune-daily" timers
                           :key #'elinit-timer-id
                           :test #'equal)))
      (delete-directory dir t))))

(ert-deftest elinit-test-builtin-timers-overridden-by-user ()
  "User timer with same ID overrides the built-in timer."
  (let* ((dir (make-temp-file "units-" t))
         (elinit-unit-authority-path (list dir))
         (elinit-unit-directory dir)
         (elinit--programs-cache :not-yet-loaded)
         (elinit--unit-file-invalid (make-hash-table :test 'equal)))
    (elinit-test--write-unit-files
     dir '(("echo rotate" :id "logrotate" :type oneshot)))
    (unwind-protect
        (let* ((elinit-timers '((:id "logrotate-daily"
                                     :target "logrotate"
                                     :on-startup-sec 300
                                     :enabled t)))
               (programs (elinit--effective-programs))
               (plan (elinit--build-plan programs))
               (timers (elinit-timer-build-list plan)))
          ;; Should appear exactly once
          (should (= 1 (cl-count "logrotate-daily" timers
                                  :key #'elinit-timer-id
                                  :test #'equal)))
          ;; Should have user's on-startup-sec, not builtin's on-calendar
          (let ((timer (cl-find "logrotate-daily" timers
                                :key #'elinit-timer-id
                                :test #'equal)))
            (should (= 300 (elinit-timer-on-startup-sec timer))))
          ;; Other built-in timer remains present.
          (should (cl-find "log-prune-daily" timers
                           :key #'elinit-timer-id
                           :test #'equal)))
      (delete-directory dir t))))

(ert-deftest elinit-test-timer-completion-does-not-signal-writers ()
  "Timer completion does not send reopen signals implicitly."
  (elinit-test-with-unit-files
      '(("true" :id "s1" :type oneshot))
    (let* ((elinit-mode t)
           (timer (elinit-timer--create :id "t1" :target "s1" :enabled t))
           (elinit--timer-state (make-hash-table :test 'equal))
           (elinit--processes (make-hash-table :test 'equal))
           (elinit--enabled-override (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal))
           (elinit--scheduler-startup-time (float-time))
           (captured-callback nil)
           (reopen-called nil))
      (cl-letf (((symbol-function 'elinit--start-entry-async)
                 (lambda (_entry callback) (setq captured-callback callback)))
                ((symbol-function 'elinit--signal-writers-reopen)
                 (lambda () (setq reopen-called t)))
                ((symbol-function 'elinit-timer--on-target-complete)
                 (lambda (_id _target-id _success) nil)))
        (unwind-protect
            (progn
              (elinit-timer--trigger timer 'scheduled)
              ;; Callback should have been captured
              (should captured-callback)
              ;; Completion callback should run without signaling writers.
              (funcall captured-callback t)
              (funcall captured-callback nil)
              (should-not reopen-called))
          (clrhash elinit--invalid)
          (clrhash elinit--timer-state)
          (clrhash elinit--processes)
          (clrhash elinit--enabled-override))))))

(ert-deftest elinit-test-logrotate-daily-not-scheduled-when-timer-off ()
  "Scheduler does not process timers when subsystem is off."
  (let ((elinit-timer-subsystem-mode nil)
        (elinit-mode nil)
        (build-list-called nil))
    (cl-letf (((symbol-function 'elinit--log) #'ignore))
      ;; elinit-timer-scheduler-start guards on subsystem-active-p
      ;; and returns early without building or processing any timers
      (cl-letf (((symbol-function 'elinit-timer-build-list)
                 (lambda (_plan)
                   (setq build-list-called t)
                   nil)))
        (elinit-timer-scheduler-start)
        ;; build-list should not have been called
        (should-not build-list-called)))))

(ert-deftest elinit-test-timer-enabled-triggers-maintenance ()
  "Enabled logrotate-daily timer triggers target oneshot on schedule."
  (elinit-test-with-unit-files
      '(("echo maintenance" :id "logrotate" :type oneshot))
    (let* ((elinit-mode t)
           (timer (elinit-timer--create :id "logrotate-daily"
                                            :target "logrotate"
                                            :enabled t))
           (elinit--timer-state (make-hash-table :test 'equal))
           (elinit--processes (make-hash-table :test 'equal))
           (elinit--enabled-override (make-hash-table :test 'equal))
           (elinit--invalid (make-hash-table :test 'equal))
           (elinit--scheduler-startup-time (float-time))
           (triggered-target nil))
      (cl-letf (((symbol-function 'elinit--start-entry-async)
                 (lambda (entry callback)
                   (setq triggered-target (elinit-entry-id entry))
                   ;; Simulate success
                   (when callback (funcall callback t))))
                ((symbol-function 'elinit--signal-writers-reopen) #'ignore)
                ((symbol-function 'elinit-timer--on-target-complete)
                 (lambda (_id _target-id _success) nil))
                ((symbol-function 'elinit-timer--save-state) #'ignore))
        (unwind-protect
            (progn
              (elinit-timer--trigger timer 'scheduled)
              ;; The trigger should have started the "logrotate" target
              (should (equal "logrotate" triggered-target)))
          (clrhash elinit--invalid)
          (clrhash elinit--timer-state)
          (clrhash elinit--processes)
          (clrhash elinit--enabled-override))))))

(ert-deftest elinit-test-timer-rejects-init-transition-target ()
  "Timer validation rejects init-transition targets."
  (elinit-test-with-unit-files
      '((nil :id "rescue.target" :type target)
        (nil :id "shutdown.target" :type target)
        (nil :id "poweroff.target" :type target)
        (nil :id "reboot.target" :type target)
        (nil :id "runlevel0.target" :type target)
        (nil :id "default.target" :type target))
    (let* ((plan (elinit--build-plan (elinit--effective-programs))))
      (dolist (tid '("rescue.target" "shutdown.target"
                     "poweroff.target" "reboot.target"
                     "runlevel0.target"))
        (let ((reason (elinit-timer--validate
                       `(:id ,(format "timer-%s" tid)
                             :target ,tid
                             :on-calendar (:hour 3))
                       plan)))
          (should reason)
          (should (string-match "init-transition" reason)))))))

(ert-deftest elinit-test-timer-allows-non-init-targets ()
  "Timer validation allows non-init-transition targets."
  (elinit-test-with-unit-files
      '(("sleep 1" :id "svc-a" :type oneshot)
        ("sleep 1" :id "svc-b" :type simple)
        (nil :id "app.target" :type target)
        (nil :id "default.target" :type target))
    (let* ((plan (elinit--build-plan (elinit--effective-programs))))
      (dolist (tid '("svc-a" "svc-b" "app.target"))
        (let ((reason (elinit-timer--validate
                       `(:id ,(format "timer-%s" tid)
                             :target ,tid
                             :on-calendar (:hour 3))
                       plan)))
          (should-not reason))))))

(provide 'elinit-test-timer)
;;; elinit-test-timer.el ends here
