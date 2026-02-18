;;; supervisor-test-timer.el --- Timer schema and scheduler tests for supervisor.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 telecommuter <telecommuter@riseup.net>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Timer schema and scheduler ERT tests for supervisor.el.

;;; Code:

(require 'supervisor-test-helpers)

;;; Timer Schema tests

(ert-deftest supervisor-test-timer-struct-fields ()
  "Timer struct has expected fields."
  (let ((timer (supervisor-timer--create
                :id "test"
                :target "target"
                :enabled t
                :on-startup-sec 60
                :persistent t)))
    (should (supervisor-timer-p timer))
    (should (equal "test" (supervisor-timer-id timer)))
    (should (equal "target" (supervisor-timer-target timer)))
    (should (eq t (supervisor-timer-enabled timer)))
    (should (= 60 (supervisor-timer-on-startup-sec timer)))
    (should (eq t (supervisor-timer-persistent timer)))))

(ert-deftest supervisor-test-timer-validate-missing-id ()
  "Timer without :id is rejected."
  (let ((err (supervisor-timer--validate '(:target "foo" :on-startup-sec 60) nil)))
    (should (string-match-p ":id must be" err))))

(ert-deftest supervisor-test-timer-validate-missing-target ()
  "Timer without :target is rejected."
  (let ((err (supervisor-timer--validate '(:id "t" :on-startup-sec 60) nil)))
    (should (string-match-p ":target must be" err))))

(ert-deftest supervisor-test-timer-validate-empty-id ()
  "Timer with empty string :id is rejected."
  (let ((err (supervisor-timer--validate '(:id "" :target "foo" :on-startup-sec 60) nil)))
    (should (string-match-p ":id must be a non-empty string" err))))

(ert-deftest supervisor-test-timer-validate-empty-target ()
  "Timer with empty string :target is rejected."
  (let ((err (supervisor-timer--validate '(:id "t" :target "" :on-startup-sec 60) nil)))
    (should (string-match-p ":target must be a non-empty string" err))))

(ert-deftest supervisor-test-timer-validate-no-trigger ()
  "Timer without any trigger is rejected."
  (let ((err (supervisor-timer--validate '(:id "t" :target "foo") nil)))
    (should (string-match-p "at least one trigger" err))))

(ert-deftest supervisor-test-timer-validate-unknown-keyword ()
  "Timer with unknown keyword is rejected."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-startup-sec 60 :bogus t) nil)))
    (should (string-match-p "unknown keyword" err))))

(ert-deftest supervisor-test-timer-validate-startup-sec-type ()
  "Timer with non-integer :on-startup-sec is rejected."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-startup-sec "60") nil)))
    (should (string-match-p ":on-startup-sec must be" err))))

(ert-deftest supervisor-test-timer-validate-unit-active-sec-positive ()
  "Timer with zero :on-unit-active-sec is rejected."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-unit-active-sec 0) nil)))
    (should (string-match-p ":on-unit-active-sec must be a positive" err))))

(ert-deftest supervisor-test-timer-validate-startup-sec-nil ()
  "Timer with only nil :on-startup-sec has no valid trigger."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-startup-sec nil) nil)))
    (should (string-match-p "at least one trigger" err))))

(ert-deftest supervisor-test-timer-validate-unit-active-sec-nil ()
  "Timer with only nil :on-unit-active-sec has no valid trigger."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-unit-active-sec nil) nil)))
    (should (string-match-p "at least one trigger" err))))

(ert-deftest supervisor-test-timer-validate-startup-sec-nil-with-other-trigger ()
  "Timer with nil :on-startup-sec but valid :on-calendar still validates."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (supervisor--build-plan programs))
         (err (supervisor-timer--validate
               '(:id "t" :target "script" :on-startup-sec nil
                 :on-calendar (:hour 3))
               plan)))
    ;; Should fail on nil :on-startup-sec type check, not on missing trigger
    (should (string-match-p ":on-startup-sec must be" err))))

(ert-deftest supervisor-test-timer-validate-calendar-unknown-field ()
  "Timer with unknown calendar field is rejected."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-calendar (:bogus 5)) nil)))
    (should (string-match-p "unknown field" err))))

(ert-deftest supervisor-test-timer-validate-calendar-bad-value ()
  "Timer with invalid calendar value type is rejected."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-calendar (:hour "3")) nil)))
    (should (string-match-p "must be integer" err))))

(ert-deftest supervisor-test-timer-validate-enabled-boolean ()
  "Timer with non-boolean :enabled is rejected."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-startup-sec 60 :enabled "yes") nil)))
    (should (string-match-p ":enabled must be a boolean" err))))

(ert-deftest supervisor-test-timer-validate-target-not-found ()
  "Timer targeting nonexistent service is rejected."
  (let* ((programs '(("real" :type oneshot :id "real")))
         (plan (supervisor--build-plan programs))
         (err (supervisor-timer--validate
               '(:id "t" :target "missing" :on-startup-sec 60) plan)))
    (should (string-match-p "not found" err))))

(ert-deftest supervisor-test-timer-validate-target-simple-accepted ()
  "Timer targeting simple service is accepted."
  (let* ((programs '(("daemon" :type simple :id "daemon")))
         (plan (supervisor--build-plan programs))
         (err (supervisor-timer--validate
               '(:id "t" :target "daemon" :on-startup-sec 60) plan)))
    (should-not err)))

(ert-deftest supervisor-test-timer-validate-target-type-target-accepted ()
  "Timer targeting a target unit is accepted."
  (let* ((programs '(("" :type target :id "app.target")))
         (plan (supervisor--build-plan programs))
         (err (supervisor-timer--validate
               '(:id "t" :target "app.target" :on-startup-sec 60) plan)))
    (should-not err)))

(ert-deftest supervisor-test-timer-validate-target-type-target-bad-suffix ()
  "Timer targeting a target entry without .target suffix is rejected.
The suffix check at timer validation level catches any target type
entry whose ID does not end in .target (entry validation blocks
this in practice, but the timer validator defends independently)."
  ;; Construct a plan with a hand-built entry where type=target but ID
  ;; lacks the .target suffix.  This bypasses entry validation to test
  ;; the timer validator's own suffix guard.
  (let* ((fake-entry (supervisor--parse-entry
                      '(nil :id "bad.target" :type target)))
         ;; Rename the ID in the parsed tuple to remove the suffix
         (bad-entry (cons "bad-no-suffix" (cdr fake-entry)))
         (plan (supervisor-plan--create
                :entries (list bad-entry)
                :by-target nil
                :deps (make-hash-table :test 'equal)
                :requires-deps (make-hash-table :test 'equal)
                :dependents (make-hash-table :test 'equal)
                :invalid (make-hash-table :test 'equal)
                :cycle-fallback-ids (make-hash-table :test 'equal)
                :order-index (make-hash-table :test 'equal)
                :meta nil))
         (err (supervisor-timer--validate
               '(:id "t" :target "bad-no-suffix" :on-startup-sec 60) plan)))
    (should err)
    (should (string-match "does not end in .target" err))))

(ert-deftest supervisor-test-timer-validate-disallowed-target-type ()
  "Timer targeting a timer entry (not oneshot/simple/target) is rejected."
  ;; Timers can only target oneshot, simple, or target entries.
  ;; This tests the rejection of an unsupported type.
  (let* ((fake-entry (supervisor--parse-entry
                      '("sleep 1" :id "my-timer" :type timer
                        :on-calendar (:hour 3))))
         (plan (supervisor-plan--create
                :entries (list fake-entry)
                :by-target nil
                :deps (make-hash-table :test 'equal)
                :requires-deps (make-hash-table :test 'equal)
                :dependents (make-hash-table :test 'equal)
                :invalid (make-hash-table :test 'equal)
                :cycle-fallback-ids (make-hash-table :test 'equal)
                :order-index (make-hash-table :test 'equal)
                :meta nil))
         (err (supervisor-timer--validate
               '(:id "t2" :target "my-timer" :on-startup-sec 60) plan)))
    (should err)
    (should (string-match "must be oneshot, simple, or target" err))))

(ert-deftest supervisor-test-timer-validate-valid ()
  "Valid timer passes validation."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (supervisor--build-plan programs))
         (err (supervisor-timer--validate
               '(:id "t" :target "script" :on-startup-sec 60) plan)))
    (should-not err)))

(ert-deftest supervisor-test-timer-validate-calendar-valid ()
  "Valid calendar schedule passes validation."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (supervisor--build-plan programs))
         (err (supervisor-timer--validate
               '(:id "t" :target "script"
                 :on-calendar (:hour 3 :minute 0 :day-of-week *))
               plan)))
    (should-not err)))

(ert-deftest supervisor-test-timer-validate-calendar-list-valid ()
  "Valid list of calendar schedules passes validation."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (supervisor--build-plan programs))
         (err (supervisor-timer--validate
               '(:id "t" :target "script"
                 :on-calendar ((:hour 3 :minute 0)
                               (:hour 15 :minute 30)))
               plan)))
    (should-not err)))

(ert-deftest supervisor-test-timer-validate-calendar-list-invalid ()
  "Invalid entry in list of calendar schedules is rejected."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (supervisor--build-plan programs))
         (err (supervisor-timer--validate
               '(:id "t" :target "script"
                 :on-calendar ((:hour 3 :minute 0)
                               (:bogus 15)))
               plan)))
    (should (string-match-p "unknown field" err))))

(ert-deftest supervisor-test-timer-validate-calendar-empty ()
  "Empty calendar list with no other trigger has no valid trigger."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (supervisor--build-plan programs))
         (err (supervisor-timer--validate
               '(:id "t" :target "script" :on-calendar ())
               plan)))
    ;; Empty list is falsy so fails trigger check first
    (should (string-match-p "at least one trigger" err))))

(ert-deftest supervisor-test-timer-validate-calendar-dotted-pair ()
  "Dotted pair calendar is rejected, not crash."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (supervisor--build-plan programs))
         (err (supervisor-timer--validate
               '(:id "t" :target "script" :on-calendar (:hour . 3))
               plan)))
    (should (string-match-p "proper plist" err))))

(ert-deftest supervisor-test-timer-validate-calendar-empty-with-other-trigger ()
  "Empty calendar with valid :on-startup-sec fails on empty calendar."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (supervisor--build-plan programs))
         (err (supervisor-timer--validate
               '(:id "t" :target "script" :on-calendar () :on-startup-sec 60)
               plan)))
    (should (string-match-p "cannot be empty" err))))

(ert-deftest supervisor-test-timer-validate-calendar-list-non-plist ()
  "Non-plist entry in calendar list is rejected."
  (let* ((programs '(("script" :type oneshot :id "script")))
         (plan (supervisor--build-plan programs))
         (err (supervisor-timer--validate
               '(:id "t" :target "script"
                 :on-calendar ((:hour 3) foo))
               plan)))
    (should (string-match-p "must be a plist" err))))

(ert-deftest supervisor-test-timer-parse ()
  "Timer parsing produces correct struct."
  (let ((timer (supervisor-timer--parse
                '(:id "backup" :target "backup-script"
                  :on-calendar (:hour 3 :minute 0)
                  :enabled nil :persistent nil))))
    (should (equal "backup" (supervisor-timer-id timer)))
    (should (equal "backup-script" (supervisor-timer-target timer)))
    (should (eq nil (supervisor-timer-enabled timer)))
    (should (equal '(:hour 3 :minute 0) (supervisor-timer-on-calendar timer)))
    (should (eq nil (supervisor-timer-persistent timer)))))

(ert-deftest supervisor-test-timer-parse-defaults ()
  "Timer parsing applies correct defaults."
  (let ((timer (supervisor-timer--parse
                '(:id "t" :target "foo" :on-startup-sec 60))))
    (should (eq t (supervisor-timer-enabled timer)))
    (should (eq t (supervisor-timer-persistent timer)))))

(ert-deftest supervisor-test-timer-build-list-valid ()
  "Build timer list from valid config."
  (let* ((supervisor--builtin-timers nil)
         (supervisor-timers '((:id "t1" :target "s1" :on-startup-sec 60)
                              (:id "t2" :target "s2" :on-startup-sec 120)))
         (programs '(("s1" :type oneshot :id "s1")
                     ("s2" :type oneshot :id "s2")))
         (plan (supervisor--build-plan programs))
         (timers (supervisor-timer-build-list plan)))
    (should (= 2 (length timers)))
    (should (= 0 (hash-table-count supervisor--invalid-timers)))))

(ert-deftest supervisor-test-timer-build-list-invalid-rejected ()
  "Invalid timers are rejected and tracked."
  (let* ((supervisor--builtin-timers nil)
         (supervisor-timers '((:id "valid" :target "s1" :on-startup-sec 60)
                              (:id "invalid" :target "missing" :on-startup-sec 60)))
         (programs '(("s1" :type oneshot :id "s1")))
         (plan (supervisor--build-plan programs))
         (timers (supervisor-timer-build-list plan)))
    (should (= 1 (length timers)))
    (should (= 1 (hash-table-count supervisor--invalid-timers)))
    (should (gethash "invalid" supervisor--invalid-timers))))

(ert-deftest supervisor-test-timer-build-list-duplicate-rejected ()
  "Duplicate timer IDs are rejected."
  (let* ((supervisor--builtin-timers nil)
         (supervisor-timers '((:id "dup" :target "s1" :on-startup-sec 60)
                              (:id "dup" :target "s1" :on-startup-sec 120)))
         (programs '(("s1" :type oneshot :id "s1")))
         (plan (supervisor--build-plan programs))
         (timers (supervisor-timer-build-list plan)))
    (should (= 1 (length timers)))
    (should (= 1 (hash-table-count supervisor--invalid-timers)))))

(ert-deftest supervisor-test-timer-build-list-duplicate-invalid-first ()
  "Duplicate timer ID rejected deterministically when first is invalid.
Second valid occurrence should not activate when first invalid occurrence used the ID."
  (let* ((supervisor--builtin-timers nil)
         (supervisor-timers '((:id "dup" :target "missing" :on-startup-sec 60)  ; invalid (bad target)
                              (:id "dup" :target "s1" :on-startup-sec 120)))    ; valid but duplicate
         (programs '(("s1" :type oneshot :id "s1")))
         (plan (supervisor--build-plan programs))
         (timers (supervisor-timer-build-list plan)))
    ;; No active timers (first invalid, second duplicate)
    (should (= 0 (length timers)))
    ;; One invalid entry (hash key = "dup", first error preserved)
    (should (= 1 (hash-table-count supervisor--invalid-timers)))
    ;; First error (target not found) should be preserved, not overwritten by duplicate
    (should (string-match-p "target" (gethash "dup" supervisor--invalid-timers)))))

(ert-deftest supervisor-test-timer-validate-startup-sec-zero-rejected ()
  "Timer with zero :on-startup-sec is rejected (must be positive)."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-startup-sec 0) nil)))
    (should err)
    (should (string-match-p ":on-startup-sec must be a positive" err))))

(ert-deftest supervisor-test-timer-validate-calendar-field-range-minute ()
  "Calendar :minute field rejects values outside 0-59."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-calendar (:minute 60)) nil)))
    (should err)
    (should (string-match-p "out of range" err))))

(ert-deftest supervisor-test-timer-validate-calendar-field-range-hour ()
  "Calendar :hour field rejects values outside 0-23."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-calendar (:hour 24)) nil)))
    (should err)
    (should (string-match-p "out of range" err))))

(ert-deftest supervisor-test-timer-validate-calendar-field-range-day ()
  "Calendar :day-of-month field rejects values outside 1-31."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-calendar (:day-of-month 0)) nil)))
    (should err)
    (should (string-match-p "out of range" err))))

(ert-deftest supervisor-test-timer-validate-calendar-field-range-month ()
  "Calendar :month field rejects values outside 1-12."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-calendar (:month 13)) nil)))
    (should err)
    (should (string-match-p "out of range" err))))

(ert-deftest supervisor-test-timer-validate-calendar-field-range-dow ()
  "Calendar :day-of-week field rejects values outside 0-6."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-calendar (:day-of-week 7)) nil)))
    (should err)
    (should (string-match-p "out of range" err))))

(ert-deftest supervisor-test-timer-validate-calendar-field-range-list ()
  "Calendar field list with out-of-range value is rejected."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-calendar (:minute (0 30 61))) nil)))
    (should err)
    (should (string-match-p "out of range" err))))

(ert-deftest supervisor-test-timer-validate-calendar-dotted-pair-rejected ()
  "Calendar field dotted pair is rejected (not crash)."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-calendar (:minute (0 . 1))) nil)))
    (should err)
    (should (string-match-p "non-empty list of integers" err))))

(ert-deftest supervisor-test-timer-validate-calendar-empty-list-rejected ()
  "Calendar field empty list is rejected."
  (let ((err (supervisor-timer--validate
              '(:id "t" :target "foo" :on-calendar (:minute ())) nil)))
    (should err)
    (should (string-match-p "non-empty list of integers" err))))

;;; Timer Scheduler Core tests

(ert-deftest supervisor-test-calendar-field-matches-star ()
  "Calendar field * matches any value."
  (should (supervisor-timer--calendar-field-matches-p '* 0))
  (should (supervisor-timer--calendar-field-matches-p '* 23))
  (should (supervisor-timer--calendar-field-matches-p '* 59)))

(ert-deftest supervisor-test-calendar-field-matches-integer ()
  "Calendar field integer matches exact value."
  (should (supervisor-timer--calendar-field-matches-p 3 3))
  (should-not (supervisor-timer--calendar-field-matches-p 3 4))
  (should-not (supervisor-timer--calendar-field-matches-p 3 0)))

(ert-deftest supervisor-test-calendar-field-matches-list ()
  "Calendar field list matches any value in list."
  (should (supervisor-timer--calendar-field-matches-p '(1 3 5) 1))
  (should (supervisor-timer--calendar-field-matches-p '(1 3 5) 3))
  (should (supervisor-timer--calendar-field-matches-p '(1 3 5) 5))
  (should-not (supervisor-timer--calendar-field-matches-p '(1 3 5) 2))
  (should-not (supervisor-timer--calendar-field-matches-p '(1 3 5) 4)))

(ert-deftest supervisor-test-calendar-matches-time ()
  "Calendar spec matches decoded time correctly."
  ;; Create a decoded time for 2025-01-15 03:30:00 (Wednesday)
  (let ((decoded (decode-time (encode-time 0 30 3 15 1 2025))))
    ;; Exact match
    (should (supervisor-timer--calendar-matches-time-p
             '(:hour 3 :minute 30) decoded))
    ;; Wildcards
    (should (supervisor-timer--calendar-matches-time-p
             '(:hour 3 :minute *) decoded))
    ;; Lists
    (should (supervisor-timer--calendar-matches-time-p
             '(:hour (1 2 3) :minute 30) decoded))
    ;; Mismatch
    (should-not (supervisor-timer--calendar-matches-time-p
                 '(:hour 4 :minute 30) decoded))
    (should-not (supervisor-timer--calendar-matches-time-p
                 '(:hour 3 :minute 0) decoded))))

(ert-deftest supervisor-test-calendar-next-minute-finds-match ()
  "Calendar next minute finds matching time."
  ;; From 2025-01-15 00:00:00, find next 03:30
  (let* ((from (encode-time 0 0 0 15 1 2025))
         (next (supervisor-timer--calendar-next-minute
                from '(:hour 3 :minute 30) 2)))  ; 2 days max
    (should next)
    ;; Should be 03:30
    (let ((decoded (decode-time next)))
      (should (= 3 (decoded-time-hour decoded)))
      (should (= 30 (decoded-time-minute decoded))))))

(ert-deftest supervisor-test-calendar-next-minute-respects-limit ()
  "Calendar next minute respects iteration limit."
  ;; Looking for hour 25 (impossible) with small limit
  (let* ((from (encode-time 0 0 0 15 1 2025))
         (next (supervisor-timer--calendar-next-minute
                from '(:hour 25 :minute 0) 7)))  ; 7 days max
    (should-not next)))

(ert-deftest supervisor-test-calendar-next-minute-leap-day ()
  "Calendar next minute finds leap day across multi-year gap.
From March 2025, next Feb 29 is in 2028 (~3 years away)."
  (let* ((from (encode-time 0 0 0 1 3 2025))  ; 2025-03-01 00:00:00
         (next (supervisor-timer--calendar-next-minute
                from '(:month 2 :day-of-month 29 :hour 0 :minute 0) 10228)))  ; 28 years
    (should next)
    (let ((decoded (decode-time next)))
      (should (= 2028 (decoded-time-year decoded)))
      (should (= 2 (decoded-time-month decoded)))
      (should (= 29 (decoded-time-day decoded))))))

(ert-deftest supervisor-test-calendar-next-minute-leap-day-weekday ()
  "Calendar next minute finds leap day + weekday across long gap.
From March 2025, next Feb 29 that is Sunday (dow=0) is in 2032 (~7 years away)."
  (let* ((from (encode-time 0 0 0 1 3 2025))  ; 2025-03-01 00:00:00
         (next (supervisor-timer--calendar-next-minute
                from '(:month 2 :day-of-month 29 :day-of-week 0 :hour 0 :minute 0) 10228)))
    (should next)
    (let ((decoded (decode-time next)))
      (should (= 2032 (decoded-time-year decoded)))
      (should (= 2 (decoded-time-month decoded)))
      (should (= 29 (decoded-time-day decoded)))
      ;; Verify it's actually Sunday (day-of-week 0)
      (should (= 0 (decoded-time-weekday decoded))))))

(ert-deftest supervisor-test-calendar-next-minute-strictly-after ()
  "Calendar next minute returns time strictly after from-time.
When from-time is exactly at a matching minute boundary, should return next occurrence."
  ;; From 2025-01-15 03:30:00 exactly (matches :hour 3 :minute 30)
  (let* ((from (encode-time 0 30 3 15 1 2025))
         (next (supervisor-timer--calendar-next-minute
                from '(:hour 3 :minute 30) 2)))  ; 2 days max
    (should next)
    ;; Should be next day's 03:30, not the same time
    (should (> next (float-time from)))
    (let ((decoded (decode-time next)))
      (should (= 16 (decoded-time-day decoded)))  ; Next day
      (should (= 3 (decoded-time-hour decoded)))
      (should (= 30 (decoded-time-minute decoded))))))

(ert-deftest supervisor-test-calendar-next-minute-dst-gap ()
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
                 (next (supervisor-timer--calendar-next-minute
                        from '(:hour 2 :minute 30) 7)))  ; 7 days max
            (should next)
            (let ((decoded (decode-time next)))
              ;; Should be March 10 (next day), not March 9
              (should (= 10 (decoded-time-day decoded)))
              (should (= 2 (decoded-time-hour decoded)))
              (should (= 30 (decoded-time-minute decoded))))))
      (setenv "TZ" orig-tz))))

(ert-deftest supervisor-test-timer-next-startup-time ()
  "Startup trigger computes time relative to scheduler start."
  (let* ((timer (supervisor-timer--create
                 :id "t1" :target "s1" :on-startup-sec 120))
         (supervisor--scheduler-startup-time 1000.0)
         (supervisor--timer-state (make-hash-table :test 'equal)))
    ;; No previous run - should return startup + delay
    (should (= 1120.0 (supervisor-timer--next-startup-time timer)))
    ;; After startup trigger has fired - should return nil
    (puthash "t1" '(:startup-triggered t) supervisor--timer-state)
    (should-not (supervisor-timer--next-startup-time timer))))

(ert-deftest supervisor-test-timer-next-unit-active-time ()
  "Unit-active trigger computes time relative to last success."
  (let* ((timer (supervisor-timer--create
                 :id "t1" :target "s1" :on-unit-active-sec 300))
         (supervisor--timer-state (make-hash-table :test 'equal)))
    ;; No previous success - should return nil
    (should-not (supervisor-timer--next-unit-active-time timer))
    ;; After a successful run
    (puthash "t1" '(:last-success-at 2000.0) supervisor--timer-state)
    (should (= 2300.0 (supervisor-timer--next-unit-active-time timer)))))

(ert-deftest supervisor-test-timer-compute-next-run-picks-earliest ()
  "Timer picks earliest trigger when multiple are configured."
  (let* ((timer (supervisor-timer--create
                 :id "t1" :target "s1"
                 :on-startup-sec 60
                 :on-unit-active-sec 300))
         (supervisor--scheduler-startup-time 1000.0)
         (supervisor--timer-state (make-hash-table :test 'equal)))
    ;; Only startup is available (no previous success)
    (should (= 1060.0 (supervisor-timer--compute-next-run timer 1000.0)))
    ;; After startup trigger has fired and a success recorded, unit-active becomes available
    (puthash "t1" '(:last-success-at 1050.0 :startup-triggered t) supervisor--timer-state)
    ;; Startup already fired, unit-active at 1350
    (should (= 1350.0 (supervisor-timer--compute-next-run timer 1100.0)))))

(ert-deftest supervisor-test-timer-overlap-detection ()
  "Timer detects when target is still running."
  (let* ((timer (supervisor-timer--create :id "t1" :target "s1"))
         (supervisor--processes (make-hash-table :test 'equal)))
    ;; No process - not active
    (should-not (supervisor-timer--target-active-p timer))
    ;; Dead process - not active
    (puthash "s1" (start-process "test" nil "true") supervisor--processes)
    (sleep-for 0.1) ; Let it die
    (should-not (supervisor-timer--target-active-p timer))
    ;; Cleanup
    (clrhash supervisor--processes)))

(ert-deftest supervisor-test-timer-trigger-disabled-timer ()
  "Timer trigger skips disabled timer and records miss."
  (supervisor-test-with-unit-files
      '(("true" :id "s1" :type oneshot))
    (let* ((timer (supervisor-timer--create :id "t1" :target "s1" :enabled nil))
           (supervisor--timer-state (make-hash-table :test 'equal)))
      ;; Trigger disabled timer
      (should-not (supervisor-timer--trigger timer 'scheduled))
      ;; Should record miss with reason 'disabled
      (let ((state (gethash "t1" supervisor--timer-state)))
        (should state)
        (should (plist-get state :last-missed-at))
        (should (eq 'disabled (plist-get state :last-miss-reason))))
      ;; Cleanup
      (clrhash supervisor--timer-state))))

(ert-deftest supervisor-test-timer-trigger-disabled-target ()
  "Timer trigger skips disabled target and records miss."
  (supervisor-test-with-unit-files
      '(("true" :id "s1" :type oneshot :disabled t))
    (let* ((timer (supervisor-timer--create :id "t1" :target "s1" :enabled t))
           (supervisor--timer-state (make-hash-table :test 'equal))
           (supervisor--processes (make-hash-table :test 'equal))
           (supervisor--enabled-override (make-hash-table :test 'equal))
           (supervisor--invalid (make-hash-table :test 'equal)))
      ;; Trigger timer with disabled target (via config)
      (should-not (supervisor-timer--trigger timer 'scheduled))
      ;; Should record miss with reason 'disabled-target
      (let ((state (gethash "t1" supervisor--timer-state)))
        (should state)
        (should (plist-get state :last-missed-at))
        (should (eq 'disabled-target (plist-get state :last-miss-reason))))
      ;; Cleanup
      (clrhash supervisor--timer-state)
      (clrhash supervisor--processes)
      (clrhash supervisor--enabled-override)
      (clrhash supervisor--invalid))))

(ert-deftest supervisor-test-timer-trigger-disabled-target-override ()
  "Timer trigger respects runtime disabled override."
  (supervisor-test-with-unit-files
      '(("true" :id "s1" :type oneshot))
    (let* ((timer (supervisor-timer--create :id "t1" :target "s1" :enabled t))
           (supervisor--timer-state (make-hash-table :test 'equal))
           (supervisor--processes (make-hash-table :test 'equal))
           (supervisor--enabled-override (make-hash-table :test 'equal))
           (supervisor--invalid (make-hash-table :test 'equal)))
      ;; Disable target via runtime override
      (puthash "s1" 'disabled supervisor--enabled-override)
      ;; Trigger timer
      (should-not (supervisor-timer--trigger timer 'scheduled))
      ;; Should record miss with reason 'disabled-target
      (let ((state (gethash "t1" supervisor--timer-state)))
        (should state)
        (should (eq 'disabled-target (plist-get state :last-miss-reason))))
      ;; Cleanup
      (clrhash supervisor--timer-state)
      (clrhash supervisor--processes)
      (clrhash supervisor--enabled-override)
      (clrhash supervisor--invalid))))

(ert-deftest supervisor-test-timer-trigger-target-not-found ()
  "Timer trigger handles missing target as failure."
  (supervisor-test-with-unit-files nil
    (let* ((timer (supervisor-timer--create :id "t1" :target "nonexistent" :enabled t))
           (supervisor--timer-state (make-hash-table :test 'equal)))
      ;; Trigger timer with nonexistent target
      (should-not (supervisor-timer--trigger timer 'scheduled))
      ;; Failure is recorded per plan (surfaced diagnostic)
      (let ((state (gethash "t1" supervisor--timer-state)))
        (should state)
        (should (eq 'failure (plist-get state :last-result))))
      ;; Cleanup
      (clrhash supervisor--timer-state))))

(ert-deftest supervisor-test-timer-trigger-overlap-skips ()
  "Timer trigger skips when target is still running."
  (supervisor-test-with-unit-files
      '(("sleep 10" :id "s1" :type oneshot))
    (let* ((timer (supervisor-timer--create :id "t1" :target "s1" :enabled t))
           (supervisor--timer-state (make-hash-table :test 'equal))
           (supervisor--processes (make-hash-table :test 'equal))
           (supervisor--enabled-override (make-hash-table :test 'equal))
           (supervisor--invalid (make-hash-table :test 'equal)))
      ;; Simulate running process
      (puthash "s1" (start-process "test-overlap" nil "sleep" "10")
               supervisor--processes)
      ;; Trigger timer
      (should-not (supervisor-timer--trigger timer 'scheduled))
      ;; Should record miss with reason 'overlap
      (let ((state (gethash "t1" supervisor--timer-state)))
        (should state)
        (should (eq 'overlap (plist-get state :last-miss-reason))))
      ;; Cleanup
      (let ((proc (gethash "s1" supervisor--processes)))
        (when (process-live-p proc)
          (delete-process proc)))
      (clrhash supervisor--invalid)
      (clrhash supervisor--timer-state)
      (clrhash supervisor--processes)
      (clrhash supervisor--enabled-override))))

(ert-deftest supervisor-test-timer-trigger-success-path ()
  "Timer trigger succeeds and emits timer-trigger event."
  (supervisor-test-with-unit-files
      '(("true" :id "s1" :type oneshot))
    (let* ((supervisor-mode t)
           (timer (supervisor-timer--create :id "t1" :target "s1" :enabled t))
           (supervisor--timer-state (make-hash-table :test 'equal))
           (supervisor--processes (make-hash-table :test 'equal))
           (supervisor--enabled-override (make-hash-table :test 'equal))
           (supervisor--invalid (make-hash-table :test 'equal))
           (supervisor--scheduler-startup-time (float-time))
           (events nil)
           (hook-fn (lambda (event)
                      (when (eq (plist-get event :type) 'timer-trigger)
                        (push event events)))))
      ;; Capture timer-trigger event
      (add-hook 'supervisor-event-hook hook-fn)
      (unwind-protect
          (progn
            ;; Trigger timer
            (should (supervisor-timer--trigger timer 'scheduled))
            ;; Should have recorded :last-run-at
            (let ((state (gethash "t1" supervisor--timer-state)))
              (should state)
              (should (plist-get state :last-run-at)))
            ;; Should have emitted timer-trigger event
            (should (= 1 (length events)))
            (let ((event (car events)))
              (should (eq 'timer-trigger (plist-get event :type)))
              (should (equal "t1" (plist-get event :id)))
              (should (equal "s1" (plist-get (plist-get event :data) :target)))))
        ;; Cleanup
        (remove-hook 'supervisor-event-hook hook-fn)
        (clrhash supervisor--invalid)
        (clrhash supervisor--timer-state)
        (clrhash supervisor--processes)
        (clrhash supervisor--enabled-override)))))

(ert-deftest supervisor-test-timer-startup-trigger-independent ()
  "Startup trigger is independent from calendar/unit-active triggers."
  (let* ((timer (supervisor-timer--create :id "t1" :target "s1" :enabled t
                                          :on-startup-sec 60
                                          :on-calendar '(:minute 0)))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--scheduler-startup-time 1000.0))
    ;; Simulate a calendar trigger having fired (sets :last-run-at)
    (puthash "t1" (list :last-run-at 1010.0) supervisor--timer-state)
    ;; Startup trigger should still return a time (not cancelled by calendar)
    (let ((next (supervisor-timer--next-startup-time timer)))
      (should next)
      (should (= 1060.0 next)))
    ;; Now mark startup as triggered
    (puthash "t1" (list :last-run-at 1010.0 :startup-triggered t)
             supervisor--timer-state)
    ;; Now startup trigger should return nil
    (should-not (supervisor-timer--next-startup-time timer))
    ;; Cleanup
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-startup-consumed-on-skipped-run ()
  "Startup trigger is consumed even when run is skipped (disabled-target)."
  ;; Regression test: startup trigger must not cause 1s retry loop on skip
  (supervisor-test-with-unit-files
      '(("true" :id "s1" :type oneshot :disabled t))
    (let* ((timer (supervisor-timer--create :id "t1" :target "s1" :enabled t
                                            :on-startup-sec 10))
           (supervisor--timer-state (make-hash-table :test 'equal))
           (supervisor--processes (make-hash-table :test 'equal))
           (supervisor--enabled-override (make-hash-table :test 'equal))
           (supervisor--invalid (make-hash-table :test 'equal))
           (supervisor--scheduler-startup-time 1000.0))
      ;; Initialize next-run-at to startup trigger time
      (puthash "t1" '(:next-run-at 1010.0) supervisor--timer-state)
      ;; Simulate time is now past startup trigger
      (cl-letf (((symbol-function 'float-time) (lambda () 1015.0)))
        ;; Trigger should fail (target disabled)
        (should-not (supervisor-timer--trigger timer 'scheduled))
        ;; But startup-triggered should be set to prevent retry loop
        (let ((state (gethash "t1" supervisor--timer-state)))
          (should (plist-get state :startup-triggered)))
        ;; Next startup time should now be nil (consumed)
        (should-not (supervisor-timer--next-startup-time timer)))
      ;; Cleanup
      (clrhash supervisor--timer-state)
      (clrhash supervisor--processes)
      (clrhash supervisor--enabled-override)
      (clrhash supervisor--invalid))))

(ert-deftest supervisor-test-timer-scheduler-not-started-during-shutdown ()
  "Timer scheduler is not started when shutting down."
  (supervisor-test-with-unit-files
      '(("true" :id "s1" :type oneshot))
    (let ((supervisor--shutting-down t)
          (supervisor-timers '((:id "t1" :target "s1" :on-startup-sec 60)))
          (scheduler-started nil)
          (callback-ran nil))
      ;; Mock the scheduler start function
      (cl-letf (((symbol-function 'supervisor-timer-scheduler-start)
                 (lambda () (setq scheduler-started t))))
        ;; Simulate completion callback with no entries (immediate callback)
        (supervisor--start-entries-async
         nil
         (lambda ()
           (supervisor--dag-cleanup)
           (when (and (not supervisor--shutting-down)
                      (fboundp 'supervisor-timer-scheduler-start))
             (supervisor-timer-scheduler-start))
           (setq callback-ran t)))
        ;; Callback should have fired
        (should callback-ran)
        ;; Scheduler should NOT have been started
        (should-not scheduler-started)))))

;;; Phase 3: Retry and Catch-up Tests

(ert-deftest supervisor-test-timer-failure-retryable-positive-exit ()
  "Positive exit codes are retryable."
  (should (supervisor-timer--failure-retryable-p 1))
  (should (supervisor-timer--failure-retryable-p 127)))

(ert-deftest supervisor-test-timer-failure-not-retryable-signal ()
  "Signal deaths (stored as negative values) are not retryable.
Emacs provides signal numbers as positive values with process-status='signal.
The oneshot exit handler encodes these as negative for retry gating."
  (should-not (supervisor-timer--failure-retryable-p -9))   ; SIGKILL
  (should-not (supervisor-timer--failure-retryable-p -15))) ; SIGTERM

(ert-deftest supervisor-test-oneshot-exit-encodes-signal-as-negative ()
  "Signal deaths are stored as negative values in oneshot-completed.
This ensures retry eligibility correctly rejects signal deaths."
  (let ((supervisor--oneshot-completed (make-hash-table :test 'equal))
        (supervisor--oneshot-callbacks (make-hash-table :test 'equal))
        (supervisor--dag-blocking-oneshots nil)
        (supervisor--dag-in-degree (make-hash-table :test 'equal))
        (supervisor--dag-dependents (make-hash-table :test 'equal))
        (supervisor--dag-ready nil)
        (supervisor--event-handlers nil))
    ;; Simulate signal death: proc-status='signal, exit-code=9 (SIGKILL)
    (supervisor--handle-oneshot-exit "test-oneshot" 'signal 9)
    ;; Should be stored as -9
    (should (= -9 (gethash "test-oneshot" supervisor--oneshot-completed)))
    ;; Therefore not retryable
    (should-not (supervisor-timer--failure-retryable-p
                 (gethash "test-oneshot" supervisor--oneshot-completed)))))

(ert-deftest supervisor-test-oneshot-exit-preserves-normal-exit-code ()
  "Normal exit codes are stored as-is (positive values)."
  (let ((supervisor--oneshot-completed (make-hash-table :test 'equal))
        (supervisor--oneshot-callbacks (make-hash-table :test 'equal))
        (supervisor--dag-blocking-oneshots nil)
        (supervisor--dag-in-degree (make-hash-table :test 'equal))
        (supervisor--dag-dependents (make-hash-table :test 'equal))
        (supervisor--dag-ready nil)
        (supervisor--event-handlers nil))
    ;; Simulate normal exit: proc-status='exit, exit-code=1
    (supervisor--handle-oneshot-exit "test-oneshot" 'exit 1)
    ;; Should be stored as 1 (positive)
    (should (= 1 (gethash "test-oneshot" supervisor--oneshot-completed)))
    ;; Therefore retryable
    (should (supervisor-timer--failure-retryable-p
             (gethash "test-oneshot" supervisor--oneshot-completed)))))

(ert-deftest supervisor-test-timer-failure-not-retryable-zero ()
  "Zero exit (success) is not retryable."
  (should-not (supervisor-timer--failure-retryable-p 0)))

(ert-deftest supervisor-test-timer-failure-not-retryable-nil ()
  "Nil exit code is not retryable."
  (should-not (supervisor-timer--failure-retryable-p nil)))

(ert-deftest supervisor-test-signal-death-status-is-failed ()
  "Signal deaths (negative exit codes) are classified as failed in status.
This is a regression test: signals are non-retryable but still failed."
  (let ((supervisor--processes (make-hash-table :test 'equal))
        (supervisor--failed (make-hash-table :test 'equal))
        (supervisor--oneshot-completed (make-hash-table :test 'equal)))
    ;; Simulate SIGKILL death stored as -9
    (puthash "test-oneshot" -9 supervisor--oneshot-completed)
    (let ((result (supervisor--compute-entry-status "test-oneshot" 'oneshot)))
      (should (equal "failed" (car result)))
      (should (equal "exit:-9" (cdr result))))))

(ert-deftest supervisor-test-signal-death-dashboard-count-is-failed ()
  "Signal deaths are counted as failed in dashboard summary."
  (supervisor-test-with-unit-files
      '(("true" :id "sig-test" :type oneshot))
    (let ((supervisor--processes (make-hash-table :test 'equal))
          (supervisor--failed (make-hash-table :test 'equal))
          (supervisor--oneshot-completed (make-hash-table :test 'equal))
          (supervisor--invalid (make-hash-table :test 'equal)))
      ;; Simulate SIGTERM death stored as -15
      (puthash "sig-test" -15 supervisor--oneshot-completed)
      (let ((summary (supervisor--health-summary)))
        ;; Should show 1 fail, not 1 done
        (should (string-match-p "1 fail" summary))
        (should-not (string-match-p "1 done" summary))))))

(ert-deftest supervisor-test-timer-schedule-retry-first-attempt ()
  "First retry is scheduled with first interval."
  (let ((supervisor-timer-retry-intervals '(30 120 600))
        (supervisor--timer-state (make-hash-table :test 'equal))
        (state nil))
    (puthash "t1" state supervisor--timer-state)
    (cl-letf (((symbol-function 'float-time) (lambda () 1000.0)))
      (let ((updated (supervisor-timer--schedule-retry "t1" state)))
        (should updated)
        (should (= 1 (plist-get updated :retry-attempt)))
        (should (= 1030.0 (plist-get updated :retry-next-at)))))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-schedule-retry-second-attempt ()
  "Second retry uses second interval."
  (let ((supervisor-timer-retry-intervals '(30 120 600))
        (supervisor--timer-state (make-hash-table :test 'equal))
        (state '(:retry-attempt 1)))
    (puthash "t1" state supervisor--timer-state)
    (cl-letf (((symbol-function 'float-time) (lambda () 1000.0)))
      (let ((updated (supervisor-timer--schedule-retry "t1" state)))
        (should updated)
        (should (= 2 (plist-get updated :retry-attempt)))
        (should (= 1120.0 (plist-get updated :retry-next-at)))))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-schedule-retry-exhausted ()
  "No retry scheduled when max attempts reached."
  (let ((supervisor-timer-retry-intervals '(30 120 600))
        (supervisor--timer-state (make-hash-table :test 'equal))
        (state '(:retry-attempt 3)))
    (puthash "t1" state supervisor--timer-state)
    (should-not (supervisor-timer--schedule-retry "t1" state))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-schedule-retry-disabled ()
  "No retry when retry-intervals is nil."
  (let ((supervisor-timer-retry-intervals nil)
        (supervisor--timer-state (make-hash-table :test 'equal))
        (state nil))
    (puthash "t1" state supervisor--timer-state)
    (should-not (supervisor-timer--schedule-retry "t1" state))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-catch-up-needed ()
  "Catch-up detected for persistent timer with missed run."
  (let* ((timer (supervisor-timer--create :id "t1" :target "s1"
                                          :on-calendar '(:minute 0)
                                          :persistent t))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--scheduler-startup-time 1000.0)
         (supervisor-timer-catch-up-limit 86400))
    ;; Last run was 2 hours ago, missed the hourly schedule
    (puthash "t1" '(:last-run-at 900.0) supervisor--timer-state)
    ;; Mock calendar computation to return a time in the past
    (cl-letf (((symbol-function 'supervisor-timer--compute-next-run)
               (lambda (_timer _from) 950.0))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      (should (supervisor-timer--needs-catch-up-p timer)))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-catch-up-not-needed-recent ()
  "No catch-up for timer with recent run."
  (let* ((timer (supervisor-timer--create :id "t1" :target "s1"
                                          :on-calendar '(:minute 0)
                                          :persistent t))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--scheduler-startup-time 1000.0)
         (supervisor-timer-catch-up-limit 86400))
    ;; Last run was just now
    (puthash "t1" '(:last-run-at 999.0) supervisor--timer-state)
    ;; Mock calendar computation to return a time in the future
    (cl-letf (((symbol-function 'supervisor-timer--compute-next-run)
               (lambda (_timer _from) 1060.0))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      (should-not (supervisor-timer--needs-catch-up-p timer)))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-catch-up-not-needed-non-persistent ()
  "No catch-up for non-persistent timer."
  (let* ((timer (supervisor-timer--create :id "t1" :target "s1"
                                          :on-calendar '(:minute 0)
                                          :persistent nil))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--scheduler-startup-time 1000.0)
         (supervisor-timer-catch-up-limit 86400))
    (puthash "t1" '(:last-run-at 900.0) supervisor--timer-state)
    (should-not (supervisor-timer--needs-catch-up-p timer))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-catch-up-not-needed-too-old ()
  "No catch-up for missed run beyond catch-up limit."
  (let* ((timer (supervisor-timer--create :id "t1" :target "s1"
                                          :on-calendar '(:minute 0)
                                          :persistent t))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--scheduler-startup-time 100000.0)
         (supervisor-timer-catch-up-limit 86400))
    ;; Last run was way before catch-up window
    (puthash "t1" '(:last-run-at 1000.0) supervisor--timer-state)
    ;; Mock calendar computation to return a time before catch-up window
    (cl-letf (((symbol-function 'supervisor-timer--compute-next-run)
               (lambda (_timer _from) 2000.0))  ; Before cutoff
              ((symbol-function 'float-time) (lambda () 100000.0)))
      (should-not (supervisor-timer--needs-catch-up-p timer)))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-catch-up-boundary-no-false-trigger ()
  "No false catch-up when last-run-at is exactly at schedule boundary.
The fix passes (1+ last-run) to compute-next-run to avoid the case where
compute-next-run returns the same timestamp as last-run for calendar timers
at minute boundaries."
  (let* ((timer (supervisor-timer--create :id "t1" :target "s1"
                                          :on-calendar '(:minute 0)
                                          :persistent t))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--scheduler-startup-time 1000.0)
         (supervisor-timer-catch-up-limit 86400)
         (call-count 0)
         (from-times nil))
    ;; Last run was at exactly time 900.0 (a schedule boundary)
    (puthash "t1" '(:last-run-at 900.0) supervisor--timer-state)
    ;; Mock compute-next-run to return 900.0 when called with 900.0,
    ;; but return future time 1800.0 when called with 901.0
    ;; This simulates the boundary condition we fixed
    (cl-letf (((symbol-function 'supervisor-timer--compute-next-run)
               (lambda (_timer from)
                 (setq call-count (1+ call-count))
                 (push from from-times)
                 (if (= from 900.0) 900.0 1800.0)))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      ;; Should NOT need catch-up because next run is in the future
      (should-not (supervisor-timer--needs-catch-up-p timer))
      ;; Verify we called with 901.0 (last-run + 1), not 900.0
      (should (= call-count 1))
      (should (= (car from-times) 901.0)))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-state-to-alist ()
  "Timer state to alist only includes persist keys."
  (let ((supervisor--timer-state (make-hash-table :test 'equal)))
    (puthash "t1" '(:last-run-at 1000.0
                    :last-success-at 1000.0
                    :next-run-at 2000.0
                    :retry-attempt 1
                    :startup-triggered t)
             supervisor--timer-state)
    (let ((alist (supervisor--timer-state-to-alist)))
      ;; Should include persisted keys
      (should (equal (alist-get "t1" alist nil nil #'equal)
                     '(:last-run-at 1000.0 :last-success-at 1000.0)))
      ;; Transient keys should not be included
      (should-not (plist-get (cdr (assoc "t1" alist)) :next-run-at))
      (should-not (plist-get (cdr (assoc "t1" alist)) :retry-attempt))
      (should-not (plist-get (cdr (assoc "t1" alist)) :startup-triggered)))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-state-save-load-roundtrip ()
  "Timer state survives save/load cycle."
  (let* ((supervisor-mode t)
         (temp-file (make-temp-file "supervisor-test-timer-state-" nil ".eld"))
         (supervisor-timer-state-file temp-file)
         (supervisor--timer-state (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; Save some state
          (puthash "t1" '(:last-run-at 1000.0 :last-success-at 1000.0)
                   supervisor--timer-state)
          (puthash "t2" '(:last-failure-at 900.0 :last-exit 1)
                   supervisor--timer-state)
          (should (supervisor-timer--save-state))
          ;; Clear and reconcile
          (clrhash supervisor--timer-state)
          (should (supervisor-timer--load-state))
          ;; Verify state restored
          (let ((t1 (gethash "t1" supervisor--timer-state))
                (t2 (gethash "t2" supervisor--timer-state)))
            (should (equal (plist-get t1 :last-run-at) 1000.0))
            (should (equal (plist-get t1 :last-success-at) 1000.0))
            (should (equal (plist-get t2 :last-failure-at) 900.0))
            (should (equal (plist-get t2 :last-exit) 1))))
      (delete-file temp-file)
      (clrhash supervisor--timer-state))))

(ert-deftest supervisor-test-timer-state-corrupt-file-handled ()
  "Corrupt timer state file is handled gracefully."
  (let* ((supervisor-mode t)
         (temp-file (make-temp-file "supervisor-test-corrupt-" nil ".eld"))
         (supervisor-timer-state-file temp-file)
         (supervisor--timer-state (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "this is not valid lisp data"))
          (should-not (supervisor-timer--load-state))
          ;; Hash should remain unchanged
          (should (= (hash-table-count supervisor--timer-state) 0)))
      (delete-file temp-file)
      (clrhash supervisor--timer-state))))

(ert-deftest supervisor-test-timer-state-persistence-disabled ()
  "Nil timer state file path disables persistence."
  (let ((supervisor-timer-state-file nil)
        (supervisor--timer-state (make-hash-table :test 'equal)))
    (puthash "t1" '(:last-run-at 1000.0) supervisor--timer-state)
    ;; Save returns nil when disabled
    (should-not (supervisor-timer--save-state))
    ;; Load returns nil when disabled
    (should-not (supervisor-timer--load-state))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-state-file-path ()
  "Timer state file path helper returns configured path."
  (let ((supervisor-timer-state-file "/test/path/timer-state.eld"))
    (should (equal (supervisor--timer-state-file-path) "/test/path/timer-state.eld")))
  (let ((supervisor-timer-state-file nil))
    (should-not (supervisor--timer-state-file-path))))

(ert-deftest supervisor-test-timer-state-newer-version-rejected ()
  "Newer schema version is rejected, not just warned."
  (let* ((supervisor-mode t)
         (temp-file (make-temp-file "supervisor-test-version-" nil ".eld"))
         (supervisor-timer-state-file temp-file)
         (supervisor--timer-state (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; Write file with future version
          (with-temp-file temp-file
            (insert (format "((version . %d) (timestamp . \"test\") (timers . ((\"t1\" :last-run-at 1000.0))))"
                            (1+ supervisor-timer-state-schema-version))))
          ;; Load should fail
          (should-not (supervisor-timer--load-state))
          ;; Hash should remain empty (data not merged)
          (should (= (hash-table-count supervisor--timer-state) 0)))
      (delete-file temp-file)
      (clrhash supervisor--timer-state))))

(ert-deftest supervisor-test-timer-state-stale-ids-pruned ()
  "Stale timer IDs are pruned from state during scheduler startup."
  (supervisor-test-with-unit-files
      '(("true" :id "s1" :type oneshot))
    (let* ((supervisor-mode t)
           (supervisor-timers '((:id "active" :target "s1" :on-startup-sec 60)))
           (supervisor--timer-state (make-hash-table :test 'equal))
           (supervisor--timer-list nil)
           (supervisor--timer-scheduler nil)
           (supervisor--shutting-down nil)
           (supervisor--scheduler-startup-time nil)
           (supervisor-timer-state-file nil))
      ;; Pre-populate state with a stale ID
      (puthash "stale-removed" '(:last-run-at 500.0) supervisor--timer-state)
      (puthash "active" '(:last-run-at 900.0) supervisor--timer-state)
      ;; Mock to prevent actual scheduling
      (cl-letf (((symbol-function 'supervisor--timer-scheduler-tick) #'ignore)
                ((symbol-function 'supervisor-timer--process-catch-ups) #'ignore)
                ((symbol-function 'float-time) (lambda () 1000.0)))
        (supervisor-timer-scheduler-start))
      ;; Stale ID should be pruned
      (should-not (gethash "stale-removed" supervisor--timer-state))
      ;; Active ID should remain
      (should (gethash "active" supervisor--timer-state))
      (clrhash supervisor--timer-state))))

(ert-deftest supervisor-test-timer-cross-restart-catch-up ()
  "Integration test: scheduler startup with persisted state triggers catch-up."
  (supervisor-test-with-unit-files
      '(("true" :id "s1" :type oneshot))
    (let* ((supervisor-mode t)
           (temp-file (make-temp-file "supervisor-test-catchup-" nil ".eld"))
           (supervisor-timers '((:id "t1" :target "s1" :on-calendar (:minute 0)
                                 :persistent t)))
           (supervisor--timer-state (make-hash-table :test 'equal))
           (supervisor--timer-list nil)
           (supervisor--timer-scheduler nil)
           (supervisor--shutting-down nil)
           (supervisor--scheduler-startup-time nil)
           (supervisor--timer-state-loaded nil)
           (supervisor-timer-state-file temp-file)
           (supervisor-timer-catch-up-limit 86400)
           (catch-up-triggered nil))
      (unwind-protect
          (progn
            ;; Write persisted state with old last-run (simulates restart)
            (with-temp-file temp-file
              (insert (format "((version . %d) (timestamp . \"test\") (timers . ((\"t1\" :last-run-at 900.0 :last-success-at 900.0))))"
                              supervisor-timer-state-schema-version)))
            ;; Mock to track catch-up and prevent actual scheduling
            (cl-letf (((symbol-function 'supervisor--timer-scheduler-tick) #'ignore)
                      ((symbol-function 'supervisor-timer--trigger)
                       (lambda (_timer reason)
                         (when (eq reason 'catch-up)
                           (setq catch-up-triggered t))))
                      ;; Mock calendar to return a time between last-run and now
                      ((symbol-function 'supervisor-timer--compute-next-run)
                       (lambda (_timer _from) 950.0))
                      ((symbol-function 'float-time) (lambda () 1000.0)))
              (supervisor-timer-scheduler-start))
            ;; Verify state was loaded from file
            (should supervisor--timer-state-loaded)
            ;; Verify catch-up was triggered
            (should catch-up-triggered))
        (when (file-exists-p temp-file) (delete-file temp-file))
        (clrhash supervisor--timer-state)))))

(ert-deftest supervisor-test-timer-catch-up-no-double-trigger ()
  "Catch-up followed by tick does not double-trigger the same timer.
After catch-up, :next-run-at must not remain in the past."
  (let* ((supervisor-mode t)
         (timer (supervisor-timer--create :id "t1" :target "s1"
                                          :on-calendar '(:minute 0)
                                          :persistent t :enabled t))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--timer-scheduler nil)
         (supervisor--shutting-down nil)
         (supervisor--scheduler-startup-time 1000.0)
         (supervisor-timer-catch-up-limit 86400)
         (trigger-reasons nil))
    ;; Seed state: last run was 100s ago, next-run in the past
    (puthash "t1" '(:last-run-at 900.0 :next-run-at 950.0)
             supervisor--timer-state)
    (cl-letf (((symbol-function 'supervisor-timer--trigger)
               (lambda (_timer reason)
                 (push reason trigger-reasons)))
              ((symbol-function 'supervisor-timer--compute-next-run)
               (lambda (_timer _from) 950.0))
              ((symbol-function 'float-time) (lambda () 1000.0))
              ((symbol-function 'run-at-time) (lambda (&rest _) nil)))
      ;; Process catch-ups, then tick
      (supervisor-timer--process-catch-ups)
      ;; :next-run-at must be nil (cleared because still in past)
      (should-not (plist-get (gethash "t1" supervisor--timer-state)
                             :next-run-at))
      ;; Now run tick -- should NOT trigger again
      (supervisor--timer-scheduler-tick)
      ;; Only catch-up should have fired, not scheduled
      (should (equal '(catch-up) (reverse trigger-reasons))))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-catch-up-simple-target ()
  "Catch-up fires for simple target type timer."
  (let* ((supervisor-mode t)
         (timer (supervisor-timer--create :id "t1" :target "s1"
                                          :on-calendar '(:minute 0)
                                          :persistent t :enabled t))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--scheduler-startup-time 1000.0)
         (supervisor-timer-catch-up-limit 86400)
         (entry (list "s1" "echo hi" 0 t nil nil nil nil 'simple nil nil
                      nil nil nil nil nil nil nil nil nil nil nil nil
                      nil nil nil nil nil nil nil nil nil nil))
         (triggered nil))
    (puthash "t1" '(:last-run-at 900.0 :last-success-at 900.0)
             supervisor--timer-state)
    (cl-letf (((symbol-function 'supervisor-timer--compute-next-run)
               (lambda (_timer _from) 950.0))
              ((symbol-function 'float-time) (lambda () 1000.0))
              ((symbol-function 'supervisor-timer--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'supervisor--get-effective-enabled)
               (lambda (_id _p) t))
              ((symbol-function 'supervisor--start-entry-async)
               (lambda (_entry _cb) (setq triggered 'simple)))
              ((symbol-function 'supervisor-timer--save-state) #'ignore)
              ((symbol-function 'supervisor--emit-event) #'ignore)
              ((symbol-function 'supervisor--log) #'ignore))
      (supervisor-timer--process-catch-ups)
      (should (eq 'simple triggered)))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-catch-up-target-type ()
  "Catch-up fires for target-type timer."
  (let* ((supervisor-mode t)
         (timer (supervisor-timer--create :id "t1" :target "app.target"
                                          :on-calendar '(:minute 0)
                                          :persistent t :enabled t))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--scheduler-startup-time 1000.0)
         (supervisor-timer-catch-up-limit 86400)
         (entry (list "app.target" nil 0 t nil nil nil nil 'target nil nil
                      nil nil nil nil nil nil nil nil nil nil nil nil
                      nil nil nil nil nil nil nil nil nil nil))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (triggered-reason nil))
    (puthash "t1" '(:last-run-at 900.0 :last-success-at 900.0)
             supervisor--timer-state)
    (cl-letf (((symbol-function 'supervisor-timer--compute-next-run)
               (lambda (_timer _from) 950.0))
              ((symbol-function 'float-time) (lambda () 1000.0))
              ((symbol-function 'supervisor-timer--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'supervisor--get-effective-enabled)
               (lambda (_id _p) t))
              ((symbol-function 'supervisor-timer--trigger-target)
               (lambda (_timer reason)
                 (setq triggered-reason reason)))
              ((symbol-function 'supervisor-timer--save-state) #'ignore)
              ((symbol-function 'supervisor--emit-event) #'ignore)
              ((symbol-function 'supervisor--log) #'ignore))
      (supervisor-timer--process-catch-ups)
      (should (eq 'catch-up triggered-reason)))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-scheduler-tick-handles-retry ()
  "Scheduler tick triggers retry when due."
  (let* ((supervisor-mode t)
         (timer (supervisor-timer--create :id "t1" :target "s1" :enabled t))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--timer-scheduler nil)
         (supervisor--shutting-down nil)
         (triggered-reason nil))
    ;; Set up state with pending retry
    (puthash "t1" '(:retry-next-at 999.0 :next-run-at 2000.0)
             supervisor--timer-state)
    ;; Mock trigger to capture reason
    (cl-letf (((symbol-function 'supervisor-timer--trigger)
               (lambda (_timer reason) (setq triggered-reason reason)))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      (supervisor--timer-scheduler-tick)
      (should (eq 'retry triggered-reason)))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-retry-budget-reset-on-scheduled ()
  "Retry budget is reset on fresh scheduled trigger."
  (let* ((supervisor-mode t)
         (timer (supervisor-timer--create :id "t1" :target "s1" :enabled t))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--timer-scheduler nil)
         (supervisor--shutting-down nil))
    ;; Set up state with exhausted retry budget and due scheduled run
    (puthash "t1" '(:retry-attempt 3 :retry-next-at nil :next-run-at 999.0)
             supervisor--timer-state)
    ;; Mock trigger and update functions
    (cl-letf (((symbol-function 'supervisor-timer--trigger)
               (lambda (_timer _reason) nil))
              ((symbol-function 'supervisor-timer--update-next-run)
               (lambda (_id) nil))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      (supervisor--timer-scheduler-tick)
      ;; Retry budget should be reset
      (let ((state (gethash "t1" supervisor--timer-state)))
        (should (= 0 (plist-get state :retry-attempt)))
        (should-not (plist-get state :retry-next-at))))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-scheduler-tick-scheduled ()
  "Scheduler tick triggers scheduled run when due."
  (let* ((supervisor-mode t)
         (timer (supervisor-timer--create :id "t1" :target "s1" :enabled t))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--timer-scheduler nil)
         (supervisor--shutting-down nil)
         (triggered-reason nil))
    ;; Set up state with scheduled run due
    (puthash "t1" '(:next-run-at 999.0) supervisor--timer-state)
    ;; Mock trigger to capture reason
    (cl-letf (((symbol-function 'supervisor-timer--trigger)
               (lambda (_timer reason) (setq triggered-reason reason)))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      (supervisor--timer-scheduler-tick)
      (should (eq 'scheduled triggered-reason)))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-scheduler-tick-disabled-skips ()
  "Scheduler tick skips disabled timers."
  (let* ((timer (supervisor-timer--create :id "t1" :target "s1" :enabled nil))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--timer-scheduler nil)
         (supervisor--shutting-down nil)
         (triggered nil))
    ;; Set up state with scheduled run due
    (puthash "t1" '(:next-run-at 999.0) supervisor--timer-state)
    ;; Mock trigger to detect if called
    (cl-letf (((symbol-function 'supervisor-timer--trigger)
               (lambda (_timer _reason) (setq triggered t)))
              ((symbol-function 'float-time) (lambda () 1000.0))
              ((symbol-function 'run-at-time) (lambda (&rest _) nil)))
      (supervisor--timer-scheduler-tick)
      ;; Should not trigger disabled timer
      (should-not triggered))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-scheduler-tick-simultaneous-order ()
  "Scheduler tick processes simultaneous due timers in list order."
  (let* ((supervisor-mode t)
         (timer1 (supervisor-timer--create :id "t1" :target "s1" :enabled t))
         (timer2 (supervisor-timer--create :id "t2" :target "s2" :enabled t))
         (timer3 (supervisor-timer--create :id "t3" :target "s3" :enabled t))
         ;; List order is t1, t2, t3
         (supervisor--timer-list (list timer1 timer2 timer3))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--timer-scheduler nil)
         (supervisor--shutting-down nil)
         (trigger-order nil))
    ;; All timers due at same time
    (puthash "t1" '(:next-run-at 999.0) supervisor--timer-state)
    (puthash "t2" '(:next-run-at 999.0) supervisor--timer-state)
    (puthash "t3" '(:next-run-at 999.0) supervisor--timer-state)
    ;; Capture trigger order
    (cl-letf (((symbol-function 'supervisor-timer--trigger)
               (lambda (timer _reason)
                 (push (supervisor-timer-id timer) trigger-order)))
              ((symbol-function 'supervisor-timer--update-next-run)
               (lambda (_id) nil))
              ((symbol-function 'float-time) (lambda () 1000.0))
              ((symbol-function 'run-at-time) (lambda (&rest _) nil)))
      (supervisor--timer-scheduler-tick)
      ;; Should process in list order: t1, t2, t3
      (should (equal '("t1" "t2" "t3") (nreverse trigger-order))))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-cli-relative-time-formatter ()
  "Relative time formatter handles various time differences."
  ;; Test with mock time at 100000
  (cl-letf (((symbol-function 'float-time) (lambda () 100000.0)))
    ;; Recent past (seconds)
    (should (string-match "\\`[0-9]+s ago\\'" (supervisor--cli-format-relative-time 99990.0)))
    ;; Minutes ago
    (should (string-match "\\`[0-9]+m ago\\'" (supervisor--cli-format-relative-time 99800.0)))
    ;; Hours ago (2h = 7200s, so 100000 - 10000 = 90000)
    (should (string-match "\\`[0-9]+h ago\\'" (supervisor--cli-format-relative-time 90000.0)))
    ;; Future (in X)
    (should (string-match "\\`in [0-9]+s\\'" (supervisor--cli-format-relative-time 100010.0)))
    (should (string-match "\\`in [0-9]+m\\'" (supervisor--cli-format-relative-time 100200.0)))
    ;; Nil returns dash
    (should (equal "-" (supervisor--cli-format-relative-time nil)))))

(ert-deftest supervisor-test-cli-relative-time-boundaries ()
  "Relative time formatter handles exact boundaries correctly."
  (cl-letf (((symbol-function 'float-time) (lambda () 100000.0)))
    ;; Exactly 60 seconds (boundary between s and m)
    (should (string-match "\\`1m ago\\'" (supervisor--cli-format-relative-time 99940.0)))
    ;; 59 seconds (should be seconds)
    (should (string-match "\\`59s ago\\'" (supervisor--cli-format-relative-time 99941.0)))
    ;; Exactly 3600 seconds (boundary between m and h)
    (should (string-match "\\`1h ago\\'" (supervisor--cli-format-relative-time 96400.0)))
    ;; 3599 seconds (should be minutes)
    (should (string-match "\\`60m ago\\'" (supervisor--cli-format-relative-time 96401.0)))
    ;; Exactly 86400 seconds (boundary between h and d)
    (should (string-match "\\`1d ago\\'" (supervisor--cli-format-relative-time 13600.0)))
    ;; Day path works for large values
    (should (string-match "\\`2d ago\\'" (supervisor--cli-format-relative-time (- 100000.0 (* 2 86400)))))
    ;; Future day path
    (should (string-match "\\`in 2d\\'" (supervisor--cli-format-relative-time (+ 100000.0 (* 2 86400)))))))

(ert-deftest supervisor-test-timer-state-load-merges-correctly ()
  "Load timer state merges with existing runtime state."
  (let* ((supervisor-mode t)
         (temp-file (make-temp-file "supervisor-test-merge-" nil ".eld"))
         (supervisor-timer-state-file temp-file)
         (supervisor--timer-state (make-hash-table :test 'equal)))
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
                   supervisor--timer-state)
          ;; Load should merge
          (should (supervisor-timer--load-state))
          (let ((state (gethash "t1" supervisor--timer-state)))
            ;; Should have persisted keys from file
            (should (equal (plist-get state :last-run-at) 500.0))
            (should (equal (plist-get state :last-success-at) 500.0))
            ;; Should preserve runtime keys
            (should (equal (plist-get state :next-run-at) 2000.0))
            (should (equal (plist-get state :retry-attempt) 1))))
      (delete-file temp-file)
      (clrhash supervisor--timer-state))))

(ert-deftest supervisor-test-timer-state-v1-load-saves-as-v2 ()
  "Loading v1 state and saving writes v2 schema metadata."
  (let* ((supervisor-mode t)
         (temp-file (make-temp-file "supervisor-test-v1-upgrade-" nil ".eld"))
         (supervisor-timer-state-file temp-file)
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--timer-state-loaded nil))
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
          (should (supervisor-timer--load-state))
          ;; Save -- should write v2
          (supervisor-timer--save-state)
          ;; Re-read file and verify version
          (let* ((data (with-temp-buffer
                         (insert-file-contents temp-file)
                         (read (current-buffer))))
                 (version (alist-get 'version data)))
            (should (= supervisor-timer-state-schema-version version))
            (should (= 2 version))))
      (delete-file temp-file)
      (clrhash supervisor--timer-state))))

(ert-deftest supervisor-test-timer-multiple-triggers-earliest-wins ()
  "Timer with multiple triggers uses earliest due time."
  (let* ((timer (supervisor-timer--create :id "t1" :target "s1"
                                          :on-startup-sec 60
                                          :on-unit-active-sec 120))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--scheduler-startup-time 1000.0))
    ;; Set last success for unit-active calculation
    (puthash "t1" '(:last-success-at 900.0) supervisor--timer-state)
    ;; Startup trigger: 1000 + 60 = 1060
    ;; Unit-active trigger: 900 + 120 = 1020 (earlier)
    (let ((next (supervisor-timer--compute-next-run timer 1001.0)))
      (should (= 1020.0 next)))
    (clrhash supervisor--timer-state)))

;;; Timer Subsystem Gating Tests

(ert-deftest supervisor-test-timer-gate-scheduler-start-noop ()
  "Scheduler start is a no-op when timer subsystem is disabled."
  (supervisor-test-with-unit-files
      '(("true" :id "s1" :type oneshot))
    (let ((supervisor-timer-subsystem-mode nil)
          (supervisor-timers '((:id "t1" :target "s1" :on-startup-sec 60)))
          (supervisor--timer-list nil)
          (supervisor--timer-scheduler nil)
          (supervisor--shutting-down nil)
          (supervisor--scheduler-startup-time nil)
          (supervisor--timer-state-loaded nil)
          (supervisor-timer-state-file nil))
      ;; Start should do nothing when gated off
      (supervisor-timer-scheduler-start)
      ;; Timer list should remain empty
      (should (null supervisor--timer-list))
      ;; Scheduler should not be running
      (should (null supervisor--timer-scheduler)))))

(ert-deftest supervisor-test-timer-gate-state-save-noop ()
  "State save is a no-op when timer subsystem is disabled."
  (let* ((supervisor-timer-subsystem-mode nil)
         (temp-file (concat (make-temp-name
                             (expand-file-name "supervisor-test-gate-"
                                               temporary-file-directory))
                            ".eld"))
         (supervisor-timer-state-file temp-file)
         (supervisor--timer-state (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (puthash "t1" '(:last-run-at 1000.0) supervisor--timer-state)
          ;; Save should return nil when gated off
          (should-not (supervisor-timer--save-state))
          ;; File should NOT be created
          (should-not (file-exists-p temp-file)))
      (when (file-exists-p temp-file) (delete-file temp-file))
      (clrhash supervisor--timer-state))))

(ert-deftest supervisor-test-timer-gate-state-load-noop ()
  "State load is a no-op when timer subsystem is disabled."
  (let* ((supervisor-timer-subsystem-mode nil)
         (temp-file (make-temp-file "supervisor-test-gate-" nil ".eld"))
         (supervisor-timer-state-file temp-file)
         (supervisor--timer-state (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; Write a valid state file
          (with-temp-file temp-file
            (insert (format "((version . %d) (timestamp . \"test\") (timers . ((\"t1\" :last-run-at 1000.0))))"
                            supervisor-timer-state-schema-version)))
          ;; Load should return nil when gated off
          (should-not (supervisor-timer--load-state))
          ;; State should NOT be populated
          (should (= 0 (hash-table-count supervisor--timer-state))))
      (delete-file temp-file)
      (clrhash supervisor--timer-state))))

(ert-deftest supervisor-test-timer-gate-scheduler-tick-noop ()
  "Scheduler tick is a no-op when timer subsystem is disabled."
  (let* ((supervisor-timer-subsystem-mode nil)
         (timer (supervisor-timer--create :id "t1" :target "s1" :enabled t
                                          :on-startup-sec 1))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--timer-scheduler nil)
         (supervisor--shutting-down nil)
         (triggered nil))
    ;; Set up state so timer would be due
    (puthash "t1" '(:next-run-at 900.0) supervisor--timer-state)
    ;; Mock trigger to detect if called
    (cl-letf (((symbol-function 'supervisor-timer--trigger)
               (lambda (_timer _reason) (setq triggered t)))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      (supervisor--timer-scheduler-tick))
    ;; Timer should NOT have been triggered
    (should-not triggered)
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-gate-enabled-works ()
  "Timer functions work normally when subsystem is enabled."
  (let* ((supervisor-mode t)  ; Parent mode must also be enabled
         (timer (supervisor-timer--create :id "t1" :target "s1" :enabled t
                                          :on-startup-sec 1))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--timer-scheduler nil)
         (supervisor--shutting-down nil)
         (triggered nil))
    ;; Set up state so timer would be due
    (puthash "t1" '(:next-run-at 900.0) supervisor--timer-state)
    ;; Mock trigger to detect if called
    (cl-letf (((symbol-function 'supervisor-timer--trigger)
               (lambda (_timer _reason) (setq triggered t)))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      (supervisor--timer-scheduler-tick))
    ;; Timer SHOULD have been triggered when enabled
    (should triggered)
    ;; Clean up scheduler timer if created
    (when (timerp supervisor--timer-scheduler)
      (cancel-timer supervisor--timer-scheduler))
    (clrhash supervisor--timer-state)))

(ert-deftest supervisor-test-timer-gate-parent-mode-off ()
  "Timer subsystem is a no-op when parent supervisor-mode is off."
  (let* ((supervisor-mode nil)                 ; But parent mode is OFF
         (timer (supervisor-timer--create :id "t1" :target "s1" :enabled t
                                          :on-startup-sec 1))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--timer-scheduler nil)
         (supervisor--shutting-down nil)
         (triggered nil))
    ;; Set up state so timer would be due
    (puthash "t1" '(:next-run-at 900.0) supervisor--timer-state)
    ;; Mock trigger to detect if called
    (cl-letf (((symbol-function 'supervisor-timer--trigger)
               (lambda (_timer _reason) (setq triggered t)))
              ((symbol-function 'float-time) (lambda () 1000.0)))
      (supervisor--timer-scheduler-tick))
    ;; Timer should NOT be triggered when parent mode is off
    (should-not triggered)
    (clrhash supervisor--timer-state)))

;;; Timer Phase 6: Expanded Coverage

(ert-deftest supervisor-test-timer-trigger-simple-success ()
  "Timer trigger for simple service records success on spawn."
  (let* ((supervisor-mode t)
         (timer (supervisor-timer--create :id "t1" :target "svc" :enabled t))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--processes (make-hash-table :test 'equal))
         (entry (list "svc" nil 0 t nil nil nil nil 'simple nil nil
                      nil nil nil nil nil nil nil nil nil nil nil nil
                      nil nil nil nil nil nil nil nil nil nil))
         (async-callback nil))
    (puthash "t1" nil supervisor--timer-state)
    (cl-letf (((symbol-function 'supervisor-timer--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'supervisor--get-effective-enabled)
               (lambda (_id _p) t))
              ((symbol-function 'supervisor--start-entry-async)
               (lambda (_entry cb) (setq async-callback cb)))
              ((symbol-function 'supervisor-timer--save-state) #'ignore)
              ((symbol-function 'supervisor--emit-event) #'ignore)
              ((symbol-function 'supervisor--timer-scheduler-tick) #'ignore)
              ((symbol-function 'supervisor-timer--update-next-run) #'ignore))
      (supervisor-timer--trigger timer 'scheduled)
      ;; Simulate successful spawn
      (funcall async-callback t)
      (let ((state (gethash "t1" supervisor--timer-state)))
        (should (eq 'success (plist-get state :last-result)))
        (should (eq 'simple (plist-get state :last-target-type)))))))

(ert-deftest supervisor-test-timer-trigger-simple-already-active ()
  "Timer trigger for already-running simple service records success no-op.
Stale retry state from a prior failure must be cleared."
  (let* ((supervisor-mode t)
         (timer (supervisor-timer--create :id "t1" :target "svc" :enabled t))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--processes (make-hash-table :test 'equal))
         (entry (list "svc" nil 0 t nil nil nil nil 'simple nil nil
                      nil nil nil nil nil nil nil nil nil nil nil nil
                      nil nil nil nil nil nil nil nil nil nil))
         (mock-proc (start-process "test" nil "sleep" "300")))
    (unwind-protect
        (progn
          ;; Seed with stale retry state from a prior failure
          (puthash "t1" '(:retry-attempt 2 :retry-next-at 55555.0)
                   supervisor--timer-state)
          (puthash "svc" mock-proc supervisor--processes)
          (cl-letf (((symbol-function 'supervisor-timer--get-entry-for-id)
                     (lambda (_id) entry))
                    ((symbol-function 'supervisor--get-effective-enabled)
                     (lambda (_id _p) t))
                    ((symbol-function 'supervisor-timer--save-state) #'ignore)
                    ((symbol-function 'supervisor--emit-event) #'ignore)
                    ((symbol-function 'supervisor--timer-scheduler-tick) #'ignore)
                    ((symbol-function 'supervisor-timer--update-next-run) #'ignore))
            (supervisor-timer--trigger timer 'scheduled)
            (let ((state (gethash "t1" supervisor--timer-state)))
              (should (eq 'success (plist-get state :last-result)))
              (should (eq 'already-active
                          (plist-get state :last-result-reason)))
              ;; Stale retry state must be cleared (success clears retry)
              (should (= 0 (plist-get state :retry-attempt)))
              (should-not (plist-get state :retry-next-at)))))
      (delete-process mock-proc))))

(ert-deftest supervisor-test-timer-trigger-simple-spawn-failure-retries ()
  "Timer trigger for simple spawn failure schedules retry."
  (let* ((supervisor-mode t)
         (timer (supervisor-timer--create :id "t1" :target "svc" :enabled t))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor-timer-retry-intervals '(30 120 600))
         (entry (list "svc" nil 0 t nil nil nil nil 'simple nil nil
                      nil nil nil nil nil nil nil nil nil nil nil nil
                      nil nil nil nil nil nil nil nil nil nil))
         (async-callback nil))
    (puthash "t1" nil supervisor--timer-state)
    (cl-letf (((symbol-function 'supervisor-timer--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'supervisor--get-effective-enabled)
               (lambda (_id _p) t))
              ((symbol-function 'supervisor--start-entry-async)
               (lambda (_entry cb) (setq async-callback cb)))
              ((symbol-function 'supervisor-timer--save-state) #'ignore)
              ((symbol-function 'supervisor--emit-event) #'ignore)
              ((symbol-function 'supervisor--timer-scheduler-tick) #'ignore)
              ((symbol-function 'supervisor-timer--update-next-run) #'ignore))
      (supervisor-timer--trigger timer 'scheduled)
      ;; Simulate spawn failure
      (funcall async-callback nil)
      (let ((state (gethash "t1" supervisor--timer-state)))
        (should (eq 'failure (plist-get state :last-result)))
        (should (eq 'spawn-failed (plist-get state :last-result-reason)))
        ;; Retry should be scheduled
        (should (= 1 (plist-get state :retry-attempt)))
        (should (plist-get state :retry-next-at))))))

(ert-deftest supervisor-test-timer-oneshot-complete-success ()
  "Oneshot completion callback records success with exit 0.
Exercises supervisor-timer--on-target-complete success branch end-to-end:
sets last-success-at, last-exit, last-result, clears retry state."
  (let* ((supervisor-mode t)
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--oneshot-completed (make-hash-table :test 'equal))
         (supervisor--timer-list nil))
    ;; Seed with stale retry state
    (puthash "t1" '(:retry-attempt 2 :retry-next-at 33333.0)
             supervisor--timer-state)
    ;; Oneshot exited with code 0
    (puthash "svc" 0 supervisor--oneshot-completed)
    (cl-letf (((symbol-function 'supervisor-timer--save-state) #'ignore)
              ((symbol-function 'supervisor--emit-event) #'ignore)
              ((symbol-function 'supervisor-timer--update-next-run) #'ignore)
              ((symbol-function 'supervisor--timer-scheduler-tick) #'ignore))
      (supervisor-timer--on-target-complete "t1" "svc" t)
      (let ((state (gethash "t1" supervisor--timer-state)))
        (should (eq 'success (plist-get state :last-result)))
        (should-not (plist-get state :last-result-reason))
        (should (= 0 (plist-get state :last-exit)))
        (should (plist-get state :last-success-at))
        ;; Retry state cleared
        (should (= 0 (plist-get state :retry-attempt)))
        (should-not (plist-get state :retry-next-at))))))

(ert-deftest supervisor-test-timer-oneshot-complete-failure-retries ()
  "Oneshot completion callback records failure with non-zero exit and schedules retry."
  (let* ((supervisor-mode t)
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--oneshot-completed (make-hash-table :test 'equal))
         (supervisor--timer-list nil)
         (supervisor-timer-retry-intervals '(30 120 600)))
    (puthash "t1" nil supervisor--timer-state)
    ;; Oneshot exited with code 1 (retryable)
    (puthash "svc" 1 supervisor--oneshot-completed)
    (cl-letf (((symbol-function 'supervisor-timer--save-state) #'ignore)
              ((symbol-function 'supervisor--emit-event) #'ignore)
              ((symbol-function 'supervisor-timer--update-next-run) #'ignore)
              ((symbol-function 'supervisor--timer-scheduler-tick) #'ignore))
      (supervisor-timer--on-target-complete "t1" "svc" nil)
      (let ((state (gethash "t1" supervisor--timer-state)))
        (should (eq 'failure (plist-get state :last-result)))
        (should (= 1 (plist-get state :last-exit)))
        (should (plist-get state :last-failure-at))
        ;; Retry scheduled
        (should (= 1 (plist-get state :retry-attempt)))
        (should (plist-get state :retry-next-at))))))

(ert-deftest supervisor-test-timer-oneshot-complete-signal-no-retry ()
  "Oneshot killed by signal records failure but does not schedule retry."
  (let* ((supervisor-mode t)
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--oneshot-completed (make-hash-table :test 'equal))
         (supervisor--timer-list nil)
         (supervisor-timer-retry-intervals '(30 120 600)))
    (puthash "t1" nil supervisor--timer-state)
    ;; Signal death encoded as negative exit code
    (puthash "svc" -9 supervisor--oneshot-completed)
    (cl-letf (((symbol-function 'supervisor-timer--save-state) #'ignore)
              ((symbol-function 'supervisor--emit-event) #'ignore)
              ((symbol-function 'supervisor-timer--update-next-run) #'ignore)
              ((symbol-function 'supervisor--timer-scheduler-tick) #'ignore))
      (supervisor-timer--on-target-complete "t1" "svc" nil)
      (let ((state (gethash "t1" supervisor--timer-state)))
        (should (eq 'failure (plist-get state :last-result)))
        (should (= -9 (plist-get state :last-exit)))
        ;; Signal deaths are NOT retryable
        (should (= 0 (or (plist-get state :retry-attempt) 0)))
        (should-not (plist-get state :retry-next-at))))))

(ert-deftest supervisor-test-timer-trigger-target-reached ()
  "Timer trigger for target records success on reached convergence.
Stale retry state from a prior failure must be cleared."
  (let* ((supervisor-mode t)
         (timer (supervisor-timer--create :id "t1" :target "app.target"
                                          :enabled t))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--target-convergence (make-hash-table :test 'equal))
         (supervisor--target-converging (make-hash-table :test 'equal))
         (supervisor--current-plan t)
         (entry (list "app.target" nil 0 t nil nil nil nil 'target nil nil
                      nil nil nil nil nil nil nil nil nil nil nil nil
                      nil nil nil nil nil nil nil nil nil nil)))
    ;; Target already reached -- no-op success
    (puthash "app.target" 'reached supervisor--target-convergence)
    ;; Seed with stale retry state from a prior failure
    (puthash "t1" '(:retry-attempt 3 :retry-next-at 44444.0)
             supervisor--timer-state)
    (cl-letf (((symbol-function 'supervisor-timer--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'supervisor--get-effective-enabled)
               (lambda (_id _p) t))
              ((symbol-function 'supervisor-timer--save-state) #'ignore)
              ((symbol-function 'supervisor--emit-event) #'ignore)
              ((symbol-function 'supervisor--timer-scheduler-tick) #'ignore)
              ((symbol-function 'supervisor-timer--update-next-run) #'ignore))
      (supervisor-timer--trigger timer 'scheduled)
      (let ((state (gethash "t1" supervisor--timer-state)))
        (should (eq 'success (plist-get state :last-result)))
        (should (eq 'already-reached
                    (plist-get state :last-result-reason)))
        ;; Stale retry state must be cleared (success clears retry)
        (should (= 0 (plist-get state :retry-attempt)))
        (should-not (plist-get state :retry-next-at))))))

(ert-deftest supervisor-test-timer-trigger-target-converging-skips ()
  "Timer trigger for converging target records miss and skip result."
  (let* ((supervisor-mode t)
         (timer (supervisor-timer--create :id "t1" :target "app.target"
                                          :enabled t))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--target-convergence (make-hash-table :test 'equal))
         (supervisor--target-converging (make-hash-table :test 'equal))
         (entry (list "app.target" nil 0 t nil nil nil nil 'target nil nil
                      nil nil nil nil nil nil nil nil nil nil nil nil
                      nil nil nil nil nil nil nil nil nil nil)))
    ;; Target is converging
    (puthash "app.target" t supervisor--target-converging)
    ;; Seed with stale retry state from a prior failure
    (puthash "t1" '(:retry-attempt 1 :retry-next-at 77777.0)
             supervisor--timer-state)
    (cl-letf (((symbol-function 'supervisor-timer--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'supervisor--get-effective-enabled)
               (lambda (_id _p) t))
              ((symbol-function 'supervisor-timer--save-state) #'ignore)
              ((symbol-function 'supervisor--emit-event) #'ignore)
              ((symbol-function 'supervisor--timer-scheduler-tick) #'ignore))
      (let ((result (supervisor-timer--trigger timer 'scheduled)))
        (should-not result)
        (let ((state (gethash "t1" supervisor--timer-state)))
          (should (eq 'target-converging
                      (plist-get state :last-miss-reason)))
          (should (eq 'skip (plist-get state :last-result)))
          (should (eq 'target-converging
                      (plist-get state :last-result-reason)))
          ;; Stale retry state must be cleared
          (should (= 0 (plist-get state :retry-attempt)))
          (should-not (plist-get state :retry-next-at)))))))

(ert-deftest supervisor-test-timer-target-degraded-retries ()
  "Timer target convergence to degraded schedules retry."
  (let* ((supervisor-mode t)
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--target-convergence (make-hash-table :test 'equal))
         (supervisor-timer-retry-intervals '(30 120 600)))
    (puthash "t1" '(:retry-attempt 0) supervisor--timer-state)
    (puthash "app.target" 'degraded supervisor--target-convergence)
    (cl-letf (((symbol-function 'supervisor-timer--save-state) #'ignore)
              ((symbol-function 'supervisor--emit-event) #'ignore)
              ((symbol-function 'supervisor--timer-scheduler-tick) #'ignore)
              ((symbol-function 'supervisor-timer--update-next-run) #'ignore))
      (supervisor-timer--on-target-converge "t1" "app.target")
      (let ((state (gethash "t1" supervisor--timer-state)))
        (should (eq 'failure (plist-get state :last-result)))
        (should (eq 'target-degraded (plist-get state :last-result-reason)))
        ;; Retry should be scheduled
        (should (= 1 (plist-get state :retry-attempt)))
        (should (plist-get state :retry-next-at))))))

(ert-deftest supervisor-test-timer-unit-active-oneshot-anchors-on-success ()
  "on-unit-active for oneshot timer anchors on last success."
  (let* ((timer (supervisor-timer--create :id "t1" :target "svc" :enabled t
                                          :on-unit-active-sec 120))
         (supervisor--timer-state (make-hash-table :test 'equal)))
    (puthash "t1" '(:last-success-at 800.0) supervisor--timer-state)
    (let ((next (supervisor-timer--next-unit-active-time timer)))
      (should (= 920.0 next)))))

(ert-deftest supervisor-test-timer-unit-active-simple-anchors-on-success ()
  "on-unit-active for simple timer anchors on last success."
  (let* ((timer (supervisor-timer--create :id "t1" :target "svc" :enabled t
                                          :on-unit-active-sec 300))
         (supervisor--timer-state (make-hash-table :test 'equal)))
    (puthash "t1" '(:last-success-at 1000.0) supervisor--timer-state)
    (let ((next (supervisor-timer--next-unit-active-time timer)))
      (should (= 1300.0 next)))))

(ert-deftest supervisor-test-timer-unit-active-target-anchors-on-success ()
  "on-unit-active for target timer anchors on last success."
  (let* ((timer (supervisor-timer--create :id "t1" :target "app.target"
                                          :enabled t :on-unit-active-sec 600))
         (supervisor--timer-state (make-hash-table :test 'equal)))
    (puthash "t1" '(:last-success-at 2000.0) supervisor--timer-state)
    (let ((next (supervisor-timer--next-unit-active-time timer)))
      (should (= 2600.0 next)))))

(ert-deftest supervisor-test-timer-overlap-no-retry ()
  "Overlap skip clears stale retry state from prior failure."
  (let* ((supervisor-mode t)
         (timer (supervisor-timer--create :id "t1" :target "svc" :enabled t))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--processes (make-hash-table :test 'equal))
         (entry (list "svc" nil 0 t nil nil nil nil 'oneshot nil nil
                      nil nil nil nil nil nil nil nil nil nil nil nil
                      nil nil nil nil nil nil nil nil nil nil))
         (mock-proc (start-process "test" nil "sleep" "300")))
    (unwind-protect
        (progn
          (puthash "svc" mock-proc supervisor--processes)
          ;; Seed with stale retry state from a prior failure
          (puthash "t1" '(:retry-attempt 1 :retry-next-at 99999.0)
                   supervisor--timer-state)
          (cl-letf (((symbol-function 'supervisor-timer--get-entry-for-id)
                     (lambda (_id) entry))
                    ((symbol-function 'supervisor--get-effective-enabled)
                     (lambda (_id _p) t))
                    ((symbol-function 'supervisor-timer--save-state) #'ignore)
                    ((symbol-function 'supervisor--emit-event) #'ignore))
            (supervisor-timer--trigger timer 'scheduled)
            (let ((state (gethash "t1" supervisor--timer-state)))
              ;; Overlap recorded as miss and skip result
              (should (eq 'overlap (plist-get state :last-miss-reason)))
              (should (eq 'skip (plist-get state :last-result)))
              (should (eq 'overlap (plist-get state :last-result-reason)))
              ;; Stale retry state must be cleared
              (should (= 0 (plist-get state :retry-attempt)))
              (should-not (plist-get state :retry-next-at)))))
      (delete-process mock-proc))))

(ert-deftest supervisor-test-timer-missing-target-records-failure ()
  "Missing target at runtime records failure with target-not-found."
  (let* ((supervisor-mode t)
         (timer (supervisor-timer--create :id "t1" :target "gone" :enabled t))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal)))
    (puthash "t1" nil supervisor--timer-state)
    (cl-letf (((symbol-function 'supervisor-timer--get-entry-for-id)
               (lambda (_id) nil))
              ((symbol-function 'supervisor-timer--save-state) #'ignore)
              ((symbol-function 'supervisor--emit-event) #'ignore)
              ((symbol-function 'supervisor-timer--update-next-run) #'ignore))
      (supervisor-timer--trigger timer 'scheduled)
      (let ((state (gethash "t1" supervisor--timer-state)))
        (should (eq 'failure (plist-get state :last-result)))
        (should (eq 'target-not-found
                    (plist-get state :last-result-reason)))))))

(ert-deftest supervisor-test-timer-masked-target-skips ()
  "Masked target skips with masked-target miss and skip result."
  (let* ((supervisor-mode t)
         (timer (supervisor-timer--create :id "t1" :target "svc" :enabled t))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--mask-override (make-hash-table :test 'equal))
         (entry (list "svc" nil 0 t nil nil nil nil 'simple nil nil
                      nil nil nil nil nil nil nil nil nil nil nil nil
                      nil nil nil nil nil nil nil nil nil nil)))
    (puthash "svc" 'masked supervisor--mask-override)
    (puthash "t1" '(:retry-attempt 1 :retry-next-at 66666.0)
             supervisor--timer-state)
    (cl-letf (((symbol-function 'supervisor-timer--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'supervisor-timer--save-state) #'ignore)
              ((symbol-function 'supervisor--emit-event) #'ignore)
              ((symbol-function 'supervisor--log) #'ignore))
      (supervisor-timer--trigger timer 'scheduled)
      (let ((state (gethash "t1" supervisor--timer-state)))
        (should (eq 'masked-target
                    (plist-get state :last-miss-reason)))
        (should (eq 'skip (plist-get state :last-result)))
        (should (eq 'masked-target
                    (plist-get state :last-result-reason)))
        (should (eq 0 (plist-get state :retry-attempt)))
        (should-not (plist-get state :retry-next-at))))))

(ert-deftest supervisor-test-timer-convergence-nil-is-failure ()
  "Nil convergence state is classified as failure, not success."
  (let* ((supervisor-mode t)
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--target-convergence (make-hash-table :test 'equal)))
    (puthash "t1" nil supervisor--timer-state)
    ;; No convergence entry for app.target -- nil case
    (cl-letf (((symbol-function 'supervisor-timer--save-state) #'ignore)
              ((symbol-function 'supervisor--emit-event) #'ignore)
              ((symbol-function 'supervisor--timer-scheduler-tick) #'ignore)
              ((symbol-function 'supervisor-timer--update-next-run) #'ignore))
      (supervisor-timer--on-target-converge "t1" "app.target")
      (let ((state (gethash "t1" supervisor--timer-state)))
        (should (eq 'failure (plist-get state :last-result)))
        (should (eq 'convergence-unknown
                    (plist-get state :last-result-reason)))))))

(ert-deftest supervisor-test-timer-convergence-converging-is-failure ()
  "Converging state at callback time is classified as failure."
  (let* ((supervisor-mode t)
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--target-convergence (make-hash-table :test 'equal))
         (supervisor-timer-retry-intervals '(30)))
    (puthash "t1" '(:retry-attempt 0) supervisor--timer-state)
    (puthash "app.target" 'converging supervisor--target-convergence)
    (cl-letf (((symbol-function 'supervisor-timer--save-state) #'ignore)
              ((symbol-function 'supervisor--emit-event) #'ignore)
              ((symbol-function 'supervisor--timer-scheduler-tick) #'ignore)
              ((symbol-function 'supervisor-timer--update-next-run) #'ignore))
      (supervisor-timer--on-target-converge "t1" "app.target")
      (let ((state (gethash "t1" supervisor--timer-state)))
        (should (eq 'failure (plist-get state :last-result)))
        (should (eq 'target-not-converged
                    (plist-get state :last-result-reason)))
        ;; Should schedule retry
        (should (= 1 (plist-get state :retry-attempt)))))))

(ert-deftest supervisor-test-timer-target-trigger-uses-closure ()
  "Target timer trigger only starts entries in the target's closure."
  (let* ((supervisor-mode t)
         (timer (supervisor-timer--create :id "t1" :target "app.target"
                                          :enabled t))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--target-convergence (make-hash-table :test 'equal))
         (supervisor--target-converging (make-hash-table :test 'equal))
         (supervisor--target-members (make-hash-table :test 'equal))
         (supervisor--target-member-reverse (make-hash-table :test 'equal))
         (programs '(("echo a" :id "svc-a" :wanted-by ("app.target"))
                     ("echo b" :id "svc-b" :wanted-by ("other.target"))
                     (nil :id "app.target" :type target)
                     (nil :id "other.target" :type target)))
         (plan (supervisor--build-plan programs))
         (supervisor--current-plan plan)
         (started-ids nil)
         (entry (list "app.target" nil 0 t nil nil nil nil 'target nil nil
                      nil nil nil nil nil nil nil nil nil nil nil nil
                      nil nil nil nil nil nil nil nil nil nil)))
    (puthash "t1" nil supervisor--timer-state)
    (cl-letf (((symbol-function 'supervisor-timer--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'supervisor--get-effective-enabled)
               (lambda (_id _p) t))
              ((symbol-function 'supervisor-timer--save-state) #'ignore)
              ((symbol-function 'supervisor--emit-event) #'ignore)
              ((symbol-function 'supervisor--timer-scheduler-tick) #'ignore)
              ((symbol-function 'supervisor-timer--update-next-run) #'ignore)
              ((symbol-function 'supervisor--dag-start-with-deps)
               (lambda (entries callback)
                 (dolist (e entries)
                   (push (supervisor-entry-id e) started-ids))
                 (funcall callback))))
      (supervisor-timer--trigger timer 'scheduled)
      ;; svc-a and app.target should be in DAG (closure members),
      ;; but not svc-b (in other.target closure)
      (should (member "svc-a" started-ids))
      (should (member "app.target" started-ids))
      (should-not (member "svc-b" started-ids)))))

(ert-deftest supervisor-test-timer-target-trigger-convergence-success ()
  "Target timer trigger reaches success when DAG processes convergence."
  (let* ((supervisor-mode t)
         (timer (supervisor-timer--create :id "t1" :target "app.target"
                                          :enabled t))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--processes (make-hash-table :test 'equal))
         (supervisor--target-convergence (make-hash-table :test 'equal))
         (supervisor--target-converging (make-hash-table :test 'equal))
         (supervisor--target-members (make-hash-table :test 'equal))
         (supervisor--target-member-reverse (make-hash-table :test 'equal))
         (supervisor--entry-state (make-hash-table :test 'equal))
         (programs '(("echo a" :id "svc-a" :wanted-by ("app.target"))
                     (nil :id "app.target" :type target)))
         (plan (supervisor--build-plan programs))
         (supervisor--current-plan plan)
         (entry (list "app.target" nil 0 t nil nil nil nil 'target nil nil
                      nil nil nil nil nil nil nil nil nil nil nil nil
                      nil nil nil nil nil nil nil nil nil nil)))
    (puthash "t1" nil supervisor--timer-state)
    (cl-letf (((symbol-function 'supervisor-timer--get-entry-for-id)
               (lambda (_id) entry))
              ((symbol-function 'supervisor--get-effective-enabled)
               (lambda (_id _p) t))
              ((symbol-function 'supervisor-timer--save-state) #'ignore)
              ((symbol-function 'supervisor--emit-event) #'ignore)
              ((symbol-function 'supervisor--timer-scheduler-tick) #'ignore)
              ((symbol-function 'supervisor-timer--update-next-run) #'ignore)
              ((symbol-function 'supervisor--dag-start-with-deps)
               (lambda (entries callback)
                 ;; Simulate DAG processing: target entry triggers
                 ;; begin-convergence, service entries become ready
                 (dolist (e entries)
                   (let ((eid (supervisor-entry-id e)))
                     (if (eq (supervisor-entry-type e) 'target)
                         (supervisor--target-begin-convergence eid)
                       ;; Mark service as ready (simulates successful start)
                       (supervisor--dag-mark-ready eid))))
                 (funcall callback))))
      (supervisor-timer--trigger timer 'scheduled)
      ;; Target should have converged to reached
      (should (eq 'reached
                  (gethash "app.target" supervisor--target-convergence)))
      ;; Timer state should record success
      (let ((state (gethash "t1" supervisor--timer-state)))
        (should (eq 'success (plist-get state :last-result)))
        (should (eq 'target-reached
                    (plist-get state :last-result-reason)))))))

(ert-deftest supervisor-test-timer-disabled-timer-records-skip-result ()
  "Disabled timer records skip and clears stale retry state."
  (let* ((supervisor-mode t)
         (timer (supervisor-timer--create :id "t1" :target "svc"
                                          :enabled nil))
         (supervisor--timer-state (make-hash-table :test 'equal)))
    ;; Seed with stale retry state from a prior failure
    (puthash "t1" '(:retry-attempt 2 :retry-next-at 88888.0)
             supervisor--timer-state)
    (cl-letf (((symbol-function 'supervisor-timer--save-state) #'ignore)
              ((symbol-function 'supervisor--emit-event) #'ignore)
              ((symbol-function 'supervisor--log) #'ignore))
      (supervisor-timer--trigger timer 'scheduled)
      (let ((state (gethash "t1" supervisor--timer-state)))
        (should (eq 'disabled (plist-get state :last-miss-reason)))
        (should (eq 'skip (plist-get state :last-result)))
        (should (eq 'disabled (plist-get state :last-result-reason)))
        ;; Stale retry state must be cleared
        (should (= 0 (plist-get state :retry-attempt)))
        (should-not (plist-get state :retry-next-at))))))

(ert-deftest supervisor-test-cli-list-timers-json-v2-fields ()
  "CLI list-timers JSON includes v2 fields: target_type, last_result.
Target type is resolved from current config, not runtime state."
  (let* ((supervisor-mode t)
         (timer (supervisor-timer--create :id "t1" :target "svc"
                                          :enabled t :persistent t))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--invalid-timers (make-hash-table :test 'equal))
         (mock-entry (list "svc" "echo hi" 0 t nil nil nil nil 'oneshot nil nil
                           nil nil nil nil nil nil nil nil nil nil nil nil
                           nil nil nil nil nil nil nil nil nil nil)))
    (puthash "t1" '(:last-result success
                    :last-result-reason nil)
             supervisor--timer-state)
    (cl-letf (((symbol-function 'supervisor--get-entry-for-id)
               (lambda (_id) mock-entry)))
      (let ((result (supervisor--cli-dispatch '("--json" "list-timers"))))
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (data (json-read-from-string
                      (supervisor-cli-result-output result)))
               (timers (alist-get 'timers data))
               (entry (car timers)))
          (should (equal "success" (alist-get 'last_result entry)))
          (should (equal "oneshot" (alist-get 'target_type entry))))))))

(ert-deftest supervisor-test-cli-list-timers-human-v2-columns ()
  "CLI list-timers human output includes TYPE and RESULT columns.
TYPE is resolved from current config."
  (let* ((supervisor-mode t)
         (timer (supervisor-timer--create :id "t1" :target "svc"
                                          :enabled t))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--invalid-timers (make-hash-table :test 'equal))
         (mock-entry (list "svc" "echo hi" 0 t nil nil nil nil 'simple nil nil
                           nil nil nil nil nil nil nil nil nil nil nil nil
                           nil nil nil nil nil nil nil nil nil nil)))
    (puthash "t1" '(:last-result failure)
             supervisor--timer-state)
    (cl-letf (((symbol-function 'supervisor--get-entry-for-id)
               (lambda (_id) mock-entry)))
      (let ((result (supervisor--cli-dispatch '("list-timers"))))
        (let ((output (supervisor-cli-result-output result)))
          ;; Header should have TYPE and RESULT columns
          (should (string-match-p "TYPE" output))
          (should (string-match-p "RESULT" output))
          ;; Data row should have the values
          (should (string-match-p "simple" output))
          (should (string-match-p "failure" output)))))))

(ert-deftest supervisor-test-dashboard-timer-entry-v2-columns ()
  "Dashboard timer entry vector includes TYPE, RESULT, and REASON columns.
TYPE is resolved from current config, not runtime state."
  (let* ((timer (supervisor-timer--create :id "t1" :target "svc" :enabled t))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--processes (make-hash-table :test 'equal))
         (mock-entry (list "svc" "echo hi" 0 t nil nil nil nil 'oneshot nil nil
                           nil nil nil nil nil nil nil nil nil nil nil nil
                           nil nil nil nil nil nil nil nil nil nil)))
    (puthash "t1" '(:last-result success
                    :last-result-reason already-active)
             supervisor--timer-state)
    (cl-letf (((symbol-function 'supervisor--get-entry-for-id)
               (lambda (_id) mock-entry)))
      (let ((entry (supervisor--make-timer-dashboard-entry timer)))
        ;; Column 6 is REASON, column 7 is TYPE, column 8 is RESULT
        (should (string= "already-active" (aref entry 6)))
        (should (string= "oneshot" (aref entry 7)))
        (should (string-match-p "success" (aref entry 8)))))))

(ert-deftest supervisor-test-dashboard-timer-entry-empty-v2-defaults ()
  "Dashboard timer entry shows dash for missing REASON and RESULT.
TYPE is resolved from config; shows dash when target not found."
  (let* ((timer (supervisor-timer--create :id "t1" :target "svc" :enabled t))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--processes (make-hash-table :test 'equal)))
    ;; No state set, target not found in config
    (cl-letf (((symbol-function 'supervisor--get-entry-for-id)
               (lambda (_id) nil)))
      (let ((entry (supervisor--make-timer-dashboard-entry timer)))
        (should (string= "-" (aref entry 6)))
        (should (string= "-" (aref entry 7)))
        (should (string= "-" (aref entry 8)))))))

(ert-deftest supervisor-test-dashboard-timer-entry-failure-result-reason ()
  "Dashboard timer entry shows result-reason for failure outcomes."
  (let* ((timer (supervisor-timer--create :id "t1" :target "svc" :enabled t))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--processes (make-hash-table :test 'equal))
         (mock-entry (list "svc" "echo hi" 0 t nil nil nil nil 'oneshot nil nil
                           nil nil nil nil nil nil nil nil nil nil nil nil
                           nil nil nil nil nil nil nil nil nil nil)))
    (puthash "t1" '(:last-result failure
                    :last-result-reason spawn-failed)
             supervisor--timer-state)
    (cl-letf (((symbol-function 'supervisor--get-entry-for-id)
               (lambda (_id) mock-entry)))
      (let ((entry (supervisor--make-timer-dashboard-entry timer)))
        ;; Column 6 is REASON showing result-reason, not miss-reason
        (should (string= "spawn-failed" (aref entry 6)))
        (should (string-match-p "failure" (aref entry 8)))))))

(ert-deftest supervisor-test-dashboard-timer-entry-fresh-timer-shows-type ()
  "Fresh timer (never triggered) shows TYPE resolved from current config."
  (let* ((timer (supervisor-timer--create :id "t1" :target "svc" :enabled t))
         (supervisor--timer-list (list timer))
         (supervisor--timer-state (make-hash-table :test 'equal))
         (supervisor--processes (make-hash-table :test 'equal))
         (mock-entry (list "svc" "echo hi" 0 t nil nil nil nil 'simple nil nil
                           nil nil nil nil nil nil nil nil nil nil nil nil
                           nil nil nil nil nil nil nil nil nil nil)))
    ;; No state at all -- timer has never triggered
    (cl-letf (((symbol-function 'supervisor--get-entry-for-id)
               (lambda (_id) mock-entry)))
      (let ((entry (supervisor--make-timer-dashboard-entry timer)))
        ;; TYPE column shows resolved type even without runtime state
        (should (string= "simple" (aref entry 7)))))))


(provide 'supervisor-test-timer)
;;; supervisor-test-timer.el ends here
