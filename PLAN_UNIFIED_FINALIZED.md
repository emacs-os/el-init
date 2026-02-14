# PLAN: Unified Validation Hardening (Finalized)

Status: LOCKED
Date: 2026-02-14
Inputs: `FINDINGS_UNIFIED.md`, current `supervisor-core.el`, `supervisor-units.el`, `supervisor-test.el`

This document is the finalized implementation spec for closing all unified
validation findings (U1-U17). It replaces ambiguity in `PLAN_UNIFIED.md` with
normative rules and implementation-safe details.

## 1. Scope

This plan hardens service and unit-file validation so that:
- misconfigurations are rejected early,
- behavior is deterministic (no truthiness surprises),
- validation never crashes on malformed input,
- invalid configs are surfaced as explicit reasons.

Out of scope:
- runtime feature expansion unrelated to validation,
- policy changes to scheduling/restart semantics beyond contradiction checks,
- migration tooling.

## 2. Non-Negotiable Invariants

1. Validation functions MUST NOT signal on malformed user input.
2. `supervisor--validate-entry` MUST return either `nil` or a reason string.
3. `supervisor--validate-unit-file-plist` MUST return either `nil` or
   `"PATH:LINE: reason"`.
4. No boolean-ish config key may use raw Lisp truthiness as accepted input.
5. Empty or whitespace-only command strings MUST be rejected before parse/start.
6. Malformed plist shape (odd pairs, dotted list tails) MUST be rejected.
7. Unit-file validation and entry validation must enforce the same semantic
   rules (unit path includes file/line prefix).

## 3. Canonical Error Style

- Entry-level errors: concise reason string.
- Unit-file errors: `"%s:%d: %s"` with path and line.
- Error text should remain stable; tests should use focused substring matching.

## 4. Finding-to-Phase Mapping

| Finding IDs | Phase |
|---|---|
| U3 | V1 |
| U1, U2 | V2 |
| U5, U15 | V3 |
| U4 | V4 |
| U7 | V5 |
| U6, U8, U9 | V6 |
| U10 | V7 |
| U11, U14 | V8 |
| U12, U13 | V9 |
| U16 | V10 |
| U17 | V11 |

## 5. Phase Specs

## V1: Plist Shape Guard (U3)

Goal: reject malformed plist structures before any keyword-level processing.

### V1.1 Entry plist shape check

Target: `supervisor--validate-entry`.

Rules:
- For list entries, `plist` (`cdr entry`) MUST be a proper list.
- Proper-list plist length MUST be even.

Required behavior:
- If not proper list: add error `"malformed plist: must be a proper key/value list"`.
- If odd length: add error `"malformed plist: odd number of elements (missing value?)"`.

Implementation note:
- Do NOT call `length` on non-proper lists.
- Shape check must run before duplicate-key and unknown-key scans.

### V1.2 Unit-file plist shape check

Target: `supervisor--validate-unit-file-plist`.

Rules:
- Entire plist must be a proper list and even-length.

Required unit-file errors:
- `"%s:%d: malformed plist: must be a proper key/value list"`
- `"%s:%d: malformed plist: odd number of elements"`

### V1.3 Tests

Add:
1. `supervisor-test-validate-entry-odd-plist`
2. `supervisor-test-validate-entry-dotted-plist`
3. `supervisor-test-validate-unit-file-odd-plist`
4. `supervisor-test-validate-unit-file-dotted-plist`

Acceptance:
- No malformed plist shape can crash validation.

## V2: Command Non-Empty Guards (U1, U2)

Goal: reject empty/whitespace command values at validation time.

### V2.1 Entry command checks

Target: `supervisor--validate-entry`.

Rules:
- String entry (`entry` is string): trimmed string must be non-empty.
- List entry (`(stringp (car entry))`): trimmed command must be non-empty.

Errors:
- `"command string must not be empty or whitespace-only"` (string entry)
- `"command must not be empty or whitespace-only"` (list entry)

### V2.2 Unit-file command checks

Target: `supervisor--validate-unit-file-plist`.

Rules:
- `:command` must be string and `string-trim` non-empty.

Error:
- `"%s:%d: :command must not be empty or whitespace-only"`

### V2.3 Defensive parse guards

Target: `supervisor--parse-entry`.

Rules:
- If tokenization yields no first token, signal explicit internal error
  (`error`) with deterministic text instead of reaching type errors.

Guard errors:
- `"Supervisor: empty command string"`
- `"Supervisor: empty command in entry"`

Rationale:
- Validation should prevent this path; parser guard prevents crash cascades.

### V2.4 Tests

Add:
1. `supervisor-test-validate-entry-empty-string-command`
2. `supervisor-test-validate-entry-whitespace-string-command`
3. `supervisor-test-validate-entry-empty-list-command`
4. `supervisor-test-validate-unit-file-empty-command`
5. `supervisor-test-validate-unit-file-whitespace-command`

Acceptance:
- Empty command is rejected in both entry and unit-file paths.

## V3: ID Hardening (U5, U15)

Goal: enforce non-empty, safe, deterministic IDs.

### V3.1 Entry ID checks

Target: `supervisor--validate-entry`.

Rules for `:id` when present:
- must be string,
- must be non-empty,
- must match `^[A-Za-z0-9._:@-]+$`.

Errors:
- `":id must be a string"`
- `":id must not be empty"`
- `":id contains invalid characters (allowed: A-Z a-z 0-9 . _ : @ -)"`

### V3.2 Unit-file parity

Target: `supervisor--validate-unit-file-plist`.

Rules:
- retain existing required/non-empty checks,
- enforce the same allowed-character rule.

Error:
- `"%s:%d: :id contains invalid characters (allowed: A-Z a-z 0-9 . _ : @ -)"`

### V3.3 Tests

Add:
1. `supervisor-test-validate-entry-empty-id`
2. `supervisor-test-validate-entry-id-with-slash`
3. `supervisor-test-validate-entry-id-with-control-char`
4. `supervisor-test-validate-entry-id-valid-chars`
5. `supervisor-test-validate-unit-file-id-invalid-chars`

Acceptance:
- Empty and unsafe IDs are consistently rejected.

## V4: Strict Boolean Flags (U4)

Goal: remove truthiness coercion for flag keys.

Target: `supervisor--validate-entry`.

Boolean-only keys:
- `:enabled`
- `:disabled`
- `:logging`
- `:no-restart`
- `:oneshot-blocking`
- `:oneshot-async`

Rule:
- If key exists, value MUST be exactly `t` or `nil`.

Error template:
- `"%s must be t or nil, got %S"`

Placement:
- run before mutual-exclusion checks.

Tests:
1. `supervisor-test-validate-boolean-enabled-string`
2. `supervisor-test-validate-boolean-disabled-string`
3. `supervisor-test-validate-boolean-logging-number`
4. `supervisor-test-validate-boolean-no-restart-string`
5. `supervisor-test-validate-boolean-oneshot-blocking-string`
6. `supervisor-test-validate-boolean-oneshot-async-string`

Acceptance:
- No non-boolean values pass for flag keys.

## V5: Tags Validation (U7)

Goal: validate `:tags` values while preserving current compatibility.

Target: `supervisor--validate-entry`.

Compatibility contract:
- Accept tag as:
  - single symbol,
  - single string,
  - proper list of symbols/strings.
- Reject numbers, hash tables, vectors, nested lists, and nil elements.
- String tags must be non-empty after trim.

Errors:
- `":tags must be a symbol, string, or list of symbols/strings"`
- `":tags must not contain empty strings"`
- `":tags must not contain nil values"`

Tests:
1. `supervisor-test-validate-tags-number`
2. `supervisor-test-validate-tags-valid-symbol-list`
3. `supervisor-test-validate-tags-valid-string-list`
4. `supervisor-test-validate-tags-empty-string`
5. `supervisor-test-validate-tags-nil-element`

Acceptance:
- Tag filtering input domain is explicit and deterministic.

## V6: Dependency and Timeout Hardening (U6, U8, U9)

Goal: reject impossible dependency declarations and invalid timeout values.

### V6.1 `:oneshot-timeout` validation

Target: `supervisor--validate-entry`.

Rule:
- must be `nil` or positive number (`> 0`).

Error:
- `":oneshot-timeout must be a positive number or nil"`

### V6.2 Empty dependency token rejection

Targets:
- `:after`, `:requires`, `:before`, `:wants`.

Rules:
- existing shape checks remain,
- reject any string element that is empty or whitespace-only.

Errors:
- `":after must not contain empty dependency IDs"`
- `":requires must not contain empty dependency IDs"`
- `":before must not contain empty dependency IDs"`
- `":wants must not contain empty dependency IDs"`

### V6.3 Self-reference rejection

Targets:
- `:after`, `:requires`, `:before`, `:wants`.

Rule:
- if effective entry ID is known, list must not contain that ID.

Errors:
- `":after must not reference the entry's own ID"`
- `":requires must not reference the entry's own ID"`
- `":before must not reference the entry's own ID"`
- `":wants must not reference the entry's own ID"`

Tests:
1. `supervisor-test-validate-oneshot-timeout-negative`
2. `supervisor-test-validate-oneshot-timeout-zero`
3. `supervisor-test-validate-oneshot-timeout-valid`
4. `supervisor-test-validate-after-empty-string`
5. `supervisor-test-validate-requires-empty-string`
6. `supervisor-test-validate-before-empty-string`
7. `supervisor-test-validate-wants-empty-string`
8. `supervisor-test-validate-after-self-dependency`
9. `supervisor-test-validate-requires-self-dependency`
10. `supervisor-test-validate-before-self-dependency`
11. `supervisor-test-validate-wants-self-dependency`

Acceptance:
- Invalid dependency declarations fail validation, not scheduler fallback.

## V7: Cross-Keyword Contradictions (U10)

Goal: reject restart-delay settings that can never apply.

Target: `supervisor--validate-entry`.

Rule:
- If `:restart-sec` is non-nil and restart policy is effectively disabled
  (`:no-restart t`, `:restart no`, or `:restart nil`), reject.

Error:
- `":restart-sec is contradictory with disabled restart policy"`

Tests:
1. `supervisor-test-validate-restart-sec-with-no-restart-true`
2. `supervisor-test-validate-restart-sec-with-restart-no`
3. `supervisor-test-validate-restart-sec-with-restart-nil`
4. `supervisor-test-validate-restart-sec-with-restart-always`

Acceptance:
- restart delay only valid when restart can occur.

## V8: Environment Validation (U11, U14)

Goal: enforce valid environment variable key semantics.

Target: `supervisor--validate-entry` (`:environment` branch).

Rules:
- existing pair shape checks remain,
- key must match `^[A-Za-z_][A-Za-z0-9_]*$`,
- duplicate keys are rejected.

Errors:
- `":environment key %S is not a valid variable name"`
- `":environment contains duplicate key %S"`

Tests:
1. `supervisor-test-validate-environment-empty-key`
2. `supervisor-test-validate-environment-key-with-space`
3. `supervisor-test-validate-environment-key-with-equals`
4. `supervisor-test-validate-environment-valid-key`
5. `supervisor-test-validate-environment-duplicate-key`

Acceptance:
- no malformed or ambiguous environment map passes.

## V9: Exec and Exit-Status Hardening (U12, U13)

Goal: reject dead/unrunnable command values.

### V9.1 `:exec-stop` and `:exec-reload`

Target: `supervisor--validate-entry`.

Rules:
- existing type checks remain,
- reject empty/whitespace strings in scalar or list form.

Errors:
- `":exec-stop must not contain empty commands"`
- `":exec-reload must not contain empty commands"`

### V9.2 `:success-exit-status` integer range

Target: `supervisor--validate-entry`.

Rule:
- integer entries must be within 0..255.

Error:
- `":success-exit-status code %d is outside valid range 0-255"`

Tests:
1. `supervisor-test-validate-exec-stop-empty-string`
2. `supervisor-test-validate-exec-stop-list-with-empty`
3. `supervisor-test-validate-exec-reload-empty-string`
4. `supervisor-test-validate-success-exit-status-negative`
5. `supervisor-test-validate-success-exit-status-over-255`
6. `supervisor-test-validate-success-exit-status-valid-boundaries`

Acceptance:
- exec command hooks and exit-status criteria are semantically valid.

## V10: Unit-File Delegation to Entry Validation (U16)

Goal: unit-file path enforces full entry semantics immediately.

Target: `supervisor--validate-unit-file-plist`.

Rule:
- after unit-file structural checks pass, construct an entry form and run
  `supervisor--validate-entry`.
- on error, return `"%s:%d: %s"` with delegated reason.

Dependency constraints:
- keep module layering (no unconditional runtime `require`).
- use `declare-function` and guarded call if needed.

Tests:
1. `supervisor-test-validate-unit-file-delegates-boolean`
2. `supervisor-test-validate-unit-file-delegates-type-gate`
3. `supervisor-test-validate-unit-file-delegates-self-dependency`
4. `supervisor-test-validate-unit-file-delegates-empty-command`

Acceptance:
- invalid unit-file values fail at unit-file validation stage, not later.

## V11: Coverage Sweep and Lock (U17)

Goal: prove every added rule has tests and zero regressions.

Checklist:
- each new rule has negative and positive test coverage,
- boundary-value tests present where applicable,
- no validation path signals for malformed input,
- full `make check` passes.

Add a final checklist section in this file after implementation with:
- date,
- commit SHAs per phase,
- final `make check` result.

## 6. Execution Order

Required order:
1. V1
2. V2
3. V3
4. V4
5. V5
6. V6
7. V7
8. V8
9. V9
10. V10
11. V11

Rationale:
- V1 and V2 prevent validation/parser crashes.
- V10 depends on all prior entry-level rule additions.

## 7. Quality Gates

- `make check` MUST pass at the end of each phase.
- No phase may proceed with failing tests/lint.
- Every new error message must be deterministic and test-covered.

## 8. Lock Criteria

This plan is complete only when all are true:
1. U1-U17 are closed.
2. `PLAN_UNIFIED_FINALIZED.md` checklist is filled with actual SHAs.
3. `README.org` validation rules match implemented behavior exactly.
4. `make check` passes on final tip.
