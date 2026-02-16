# PLAN: Timer to Target/Service Trigger Support

Date: 2026-02-15
Status: Final Locked
Authority: This file is the implementation and audit contract.

## Rule of Interpretation
All requirements in this document are mandatory.

- MUST means required for completion.
- MUST NOT means prohibited.
- SHOULD is not used in this file.
- MAY is not used in this file.

Any behavior not explicitly listed here is out of scope.

## Objective
Expand timer triggering from oneshot-only to systemd-like start semantics for
all supported runtime unit kinds:

- `oneshot` service units,
- `simple` service units,
- `target` units.

This plan is intentionally narrow:

- trigger model remains timer-driven start jobs,
- no broad timer syntax redesign,
- validation and test coverage are expanded aggressively.

## Locked Decisions
1. Timer definitions keep existing keyword schema; `:target` remains the unit
   ID field.
2. `:target` MUST be resolved as a generic unit ID, not oneshot-only.
3. Allowed timer target unit types are exactly: `oneshot`, `simple`, `target`.
4. Trigger operation is always start semantics (systemd-like `start` job).
5. Timer triggers MUST NOT perform implicit restart, stop, or isolate actions.
6. Existing oneshot timer behavior remains source-compatible.
7. `on-unit-active` semantics are supported for all three allowed unit types.
8. Timer definition schema version remains `1`.
9. Timer state schema version increments to `2`.
10. Existing CLI `list-timers` command remains stable; output gains additional
    fields instead of breaking current fields.
11. Existing dashboard timer rows remain; additional status detail is additive.
12. Timer subsystem gate behavior remains unchanged.
13. `make check` is required at every phase gate.

## Runtime Semantics

### 1) Unit-Type Resolution
For each timer trigger, unit type is resolved from the loaded plan entry for
`:target`.

- Missing target unit at runtime is a failed trigger outcome with surfaced
  diagnostic.
- Invalid target type is a validation error and the timer is excluded from the
  active scheduler list.

### 2) Trigger Action Semantics
Trigger behavior is fixed by resolved unit type.

- `oneshot`:
  - If currently running, trigger is skipped with reason `overlap`.
  - Otherwise start asynchronously and complete on process exit or timeout.
- `simple`:
  - If already running, trigger is successful no-op with reason
    `already-active`.
  - If not running, start asynchronously and complete when spawn succeeds or
    fails.
- `target`:
  - Trigger starts a target activation transaction equivalent to
    `start --target TARGET`.
  - If same target is already converging, trigger is skipped with reason
    `target-converging`.
  - Completion occurs when target reaches terminal convergence state.

### 3) Success and Failure Classification
Result classification is fixed:

- `oneshot` success: clean completion; failure: non-clean completion.
- `simple` success: start accepted (spawned) or already active no-op;
  failure: spawn/start failure.
- `target` success: converged to `reached` or already reached no-op;
  failure: converged to `degraded` or `unreachable`.

Retry eligibility is fixed:

- Retries apply only to failures.
- Retries do not apply to skip/no-op outcomes (`overlap`, `already-active`,
  `target-converging`, disabled cases).

### 4) `:on-unit-active-sec` Anchor Semantics
`on-unit-active` anchor is type-aware and deterministic.

- `oneshot`: anchor at last successful completion timestamp.
- `simple`: anchor at last successful activation timestamp (spawn success or
  already-active no-op).
- `target`: anchor at last successful convergence timestamp (reached or
  already-reached no-op).

### 5) Persistent Catch-Up Semantics
Catch-up behavior remains enabled by existing `:persistent` and global
catch-up window settings.

- Catch-up applies to `oneshot`, `simple`, and `target` timers.
- Catch-up trigger uses the same type-specific semantics as scheduled trigger.

## Validation Contract
Validation runs during timer list build against merged runtime plan and is
strict.

1. `:id` MUST be a non-empty string.
2. `:target` MUST be a non-empty string.
3. Timer IDs MUST be unique.
4. At least one trigger (`:on-calendar`, `:on-startup-sec`,
   `:on-unit-active-sec`) MUST be configured.
5. `:on-startup-sec` and `:on-unit-active-sec` MUST be positive integers when
   present.
6. `:enabled` and `:persistent` MUST be booleans when present.
7. `:target` MUST resolve to an existing plan entry.
8. Resolved target entry type MUST be one of `oneshot`, `simple`, `target`.
9. If resolved target type is `target`, target ID MUST end with `.target`.
10. Unknown timer keywords MUST invalidate the timer.
11. Invalid timers MUST be excluded from scheduler activation and surfaced via
    CLI/dashboard invalid timer diagnostics.

## Timer State Contract (Schema v2)
Timer state persistence schema increments to `2` with additive fields.

Required persisted fields:

- existing persisted keys from schema v1 remain valid,
- `:last-result` (symbol),
- `:last-result-reason` (symbol or nil),
- `:last-target-type` (symbol: `oneshot`/`simple`/`target`).

Required non-persisted runtime fields:

- `:next-run-at`,
- `:retry-attempt`,
- `:retry-next-at`.

Compatibility rules:

- Loading schema v1 state MUST succeed with upgrade defaults.
- New writes MUST persist schema v2 format.

## CLI and Dashboard Contract
1. `list-timers` MUST include target unit type and last result classification.
2. Existing `list-timers` machine-readable keys remain backward compatible;
   new keys are additive.
3. Dashboard timer rows MUST show target type and last result reason.
4. Invalid timer reasons MUST be visible in both CLI and dashboard.

## Phase Plan

### Phase 1: Validation and Schema Expansion
Deliverables:

1. Extend timer validation to allow target types `oneshot`, `simple`, `target`.
2. Add strict validation rules from Validation Contract.
3. Add timer state schema v2 read/write support with v1 upgrade path.

Acceptance:

1. Timers targeting `oneshot`, `simple`, and `target` validate when configured
   correctly.
2. Invalid target types are rejected deterministically.
3. Schema v1 timer state loads without data loss.
4. Schema v2 state writes are atomic and deterministic.
5. `make check` passes.

### Phase 2: Type-Aware Trigger Dispatcher
Deliverables:

1. Refactor trigger path into type-specific execution handlers.
2. Implement `simple` and `target` trigger completion callbacks.
3. Standardize result classification fields for all types.

Acceptance:

1. `oneshot` behavior is preserved.
2. `simple` already-running triggers return successful no-op
   `already-active`.
3. `target` converging triggers return skipped `target-converging`.
4. Failures are classified consistently across all types.
5. `make check` passes.

### Phase 3: Scheduling and Retry Parity
Deliverables:

1. Implement `on-unit-active` anchors for `simple` and `target`.
2. Apply retry policy uniformly for retry-eligible failures.
3. Ensure skip/no-op outcomes do not schedule retries.

Acceptance:

1. `on-unit-active` repeats deterministically for all supported target types.
2. No retry is scheduled for `overlap`, `already-active`,
   `target-converging`, or disabled skips.
3. Retry scheduling for failures follows configured intervals exactly.
4. `make check` passes.

### Phase 4: Catch-Up and Runtime Edge Cases
Deliverables:

1. Extend catch-up execution to `simple` and `target` triggers.
2. Harden behavior for runtime missing target, disabled target, masked target,
   and shutdown transitions.
3. Ensure deterministic diagnostics for each edge case.

Acceptance:

1. Catch-up behavior is deterministic for all three target types.
2. Runtime missing target is surfaced as failure with reason.
3. Scheduler remains non-polling and free of busy loops.
4. `make check` passes.

### Phase 5: CLI and Dashboard Surfaces
Deliverables:

1. Add timer result/type fields to CLI `list-timers` output.
2. Add timer result/type fields to dashboard timer rows.
3. Ensure invalid timer reporting remains complete and stable.

Acceptance:

1. Human and JSON timer outputs include target type and last result.
2. Existing JSON keys are not removed or renamed.
3. Dashboard reflects updated timer status contract accurately.
4. `make check` passes.

### Phase 6: Test Expansion (Hard Requirement)
Deliverables:

1. Add ERT validation tests for all new allowed/disallowed target types.
2. Add ERT runtime tests for `oneshot`, `simple`, `target` trigger paths.
3. Add ERT tests for `simple` already-active no-op behavior.
4. Add ERT tests for `target` converging and reached/degraded outcomes.
5. Add ERT tests for `on-unit-active` semantics per type.
6. Add ERT tests for retry eligibility per result class.
7. Add ERT tests for catch-up behavior per type.
8. Add ERT tests for state schema v1 -> v2 compatibility.
9. Add ERT tests for CLI JSON/human output additive fields.
10. Add ERT tests for dashboard row rendering fields.

Acceptance:

1. New tests pass.
2. Existing tests pass.
3. `make check` passes.

### Phase 7: Documentation and Manual Update
Deliverables:

1. Update `README.org` Timer Subsystem section to document support for
   `oneshot`, `simple`, and `target` timer targets.
2. Update README examples to include one example for each target type.
3. Update timer semantics docs for `on-unit-active` by target type.
4. Update CLI docs for `list-timers` additional fields.
5. Update failure/skip reason documentation.
6. Remove stale statements claiming timers are oneshot-only.

Acceptance:

1. Manual reflects implemented behavior exactly.
2. No stale oneshot-only claims remain.
3. All new validation and runtime semantics are documented.
4. `make check` passes.

## Explicit Non-Goals
1. No new timer trigger keywords.
2. No rename of existing timer keys (`:target`, `:on-calendar`,
   `:on-startup-sec`, `:on-unit-active-sec`).
3. No timer-driven implicit restart policy.
4. No timer-driven stop or isolate actions.
5. No cron syntax redesign.
6. No D-Bus timer activation.
7. No change to global timer subsystem gate model.

## Audit Evidence Requirements
Each phase audit MUST include:

1. Code evidence:
   - exact files changed,
   - exact functions changed or added.
2. Validation evidence:
   - accepted and rejected target types,
   - surfaced invalid reasons.
3. Runtime evidence:
   - one successful trigger path per target type,
   - one failure/skip path per target type,
   - one deterministic scheduling proof point.
4. State evidence:
   - schema v1 load example,
   - schema v2 write example.
5. CLI/dashboard evidence:
   - updated output examples for new fields.
6. Test evidence:
   - new ERT test names,
   - exact `make check` result,
   - explicit skipped test list.
7. Documentation evidence:
   - exact README headings updated,
   - confirmation stale statements were removed.

## Audit Checklist (Final Signoff)
All items MUST be true:

1. Timers validate `oneshot`, `simple`, and `target` targets correctly.
2. Invalid timer targets are rejected with deterministic reasons.
3. Trigger dispatcher is type-aware and deterministic.
4. `simple` timers use start semantics and never force restart.
5. `target` timers trigger target activation transactions correctly.
6. `on-unit-active` semantics are implemented per target type as specified.
7. Retry semantics match failure-only policy.
8. Catch-up behavior works for all supported target types.
9. Timer state schema v2 is implemented with v1 compatibility.
10. CLI and dashboard expose target type and last result fields.
11. Full test suite passes with no regressions.
12. README manual is synchronized with actual behavior.
13. `make check` passes at release gate.
