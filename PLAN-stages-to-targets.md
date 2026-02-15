# PLAN: Stage to Target Startup Model

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
Replace stage-partitioned startup with an optional target-based activation mode while preserving current robustness guarantees and full backward compatibility.

The migration is complete only when target mode is production-ready and stage mode behavior remains unchanged.

## Locked Decisions
1. Targets are first-class unit entries using `:type target` in the same unit-file system.
2. No separate target file format is introduced.
3. No separate `supervisor-targets` defcustom is introduced.
4. Target entries MUST NOT define `:command`.
5. Stage mode remains default indefinitely.
6. New defcustom `supervisor-startup-mode` has exactly two values: `stage`, `target`.
7. New defcustom `supervisor-default-target` defaults to `"default"`.
8. Built-in compatibility targets are fixed: `stage1`, `stage2`, `stage3`, `stage4`, `default`.
9. Existing service `:stage` declarations remain valid and unchanged.
10. In target mode, `:stage stageN` is materialized as implicit `:wanted-by ("stageN")` when `:wanted-by` is absent.
11. In target mode, scheduler graph is global and not partitioned by stage.
12. In stage mode, current stage scheduler path and semantics remain unchanged.
13. Cross-stage `:requires` remains invalid in stage mode.
14. Cross-stage `:requires` is valid in target mode.
15. Existing CLI/dashboard commands and keybindings remain stable; target commands are additive.

## Robustness Invariants
The following invariants MUST hold in both modes unless explicitly scoped to one mode.

1. Deterministic ordering for all tie cases.
   Tie-breaker is the original parsed entry index.
2. Cycle detection with explicit fallback and surfaced reason.
3. Invalid unit isolation with reason visibility.
4. No polling loops introduced into startup scheduling.
5. Spawn failures never deadlock unrelated graph paths.
6. Service readiness semantics are unchanged:
   - `simple`: ready on spawn.
   - `oneshot`: ready on exit or timeout.
   - disabled: ready immediately.
   - delayed: ready only after delay timer fires and spawn attempt completes.
7. Existing restart and crash-loop behavior is unchanged.
8. Stage completion semantics in stage mode are unchanged.

## Canonical Data Model

### 1) Entry Types
Valid `:type` values become:

- `simple`
- `oneshot`
- `target`

### 2) Target Entry Contract
Valid fields for `:type target`:

- `:id` (required)
- `:type` (required, value `target`)
- `:enabled`, `:disabled`
- `:after`, `:before`, `:requires`, `:wants`
- `:description`, `:documentation`, `:tags`

Invalid for `target` and MUST raise validation error:

- `:command`, `:delay`, `:restart`, `:no-restart`, `:logging`
- `:oneshot-blocking`, `:oneshot-async`, `:oneshot-timeout`
- `:working-directory`, `:environment`, `:environment-file`
- `:exec-stop`, `:exec-reload`, `:restart-sec`
- `:kill-signal`, `:kill-mode`, `:remain-after-exit`
- `:success-exit-status`, `:user`, `:group`

### 3) Service to Target Membership Keys
New keys for service entries:

- `:wanted-by` list of target IDs
- `:required-by` list of target IDs

Materialization rules:

- Service `A` with `:wanted-by ("X")` adds `A` to target `X` wants-membership.
- Service `A` with `:required-by ("X")` adds `A` to target `X` requires-membership.
- Missing target references are dropped with warning.

### 4) Entry Tuple Extension
Entry tuple extends from 29 to 31 elements.

- index 29: `wanted-by`
- index 30: `required-by`

Required accessors:

- `supervisor-entry-wanted-by`
- `supervisor-entry-required-by`

### 5) Built-in Compatibility Targets
Built-in targets are always present at lowest authority precedence.

- `stage1`: no `:after`
- `stage2`: `:after ("stage1")`
- `stage3`: `:after ("stage2")`
- `stage4`: `:after ("stage3")`
- `default`: `:requires ("stage4")`

These IDs are built-in defaults and are overrideable by higher authority tiers.

### 6) Plan Struct Contract
Plan version is incremented to `2`.

New plan fields:

- `by-target`: global activation order in target mode
- `target-members`: mapping target -> `(:requires ... :wants ...)`
- `activation-root`: target root ID used for expansion
- `activation-closure`: all target and service IDs in transaction

Mode-specific rule:

- stage mode: `by-stage` populated, `by-target` nil
- target mode: `by-target` populated, `by-stage` nil

## Runtime Semantics

### 1) Stage Mode
Stage mode MUST preserve existing behavior exactly.

### 2) Target Mode Transaction Expansion
Given `ROOT` target:

1. Initialize closure with `ROOT`.
2. Expand target `:requires` and `:wants` members transitively.
3. Materialize inverse memberships from `:wanted-by` and `:required-by` before expansion.
4. Include reachable services and targets only.
5. Mark non-closure units as unreachable diagnostics.
6. Expansion order MUST be deterministic.

### 3) Target Mode Scheduling
Target mode uses one global DAG over closure nodes.

Graph edges:

- ordering edges from `:after` and inverted `:before`
- hard dependency edges from `:requires`
- soft dependency edges from `:wants`

Targets participate as passive nodes and are never spawned as processes.

### 4) Target Convergence
Target state machine is fixed:

- `pending`
- `converging`
- `reached`
- `degraded`
- `unreachable`

Convergence rules:

- target reaches `reached` when all required members are ready
- wanted members never block target reach
- invalid or failed required members yield `degraded`
- degraded target still unlocks dependents and records reason chain

## Command Surface
New additive CLI commands are fixed to these names:

1. `list-targets`
2. `target-status TARGET`
3. `explain-target TARGET`
4. `isolate TARGET`
5. `start --target TARGET`

`isolate TARGET` MUST require confirmation.

Dashboard target support is mandatory in target mode:

- target rows rendered with type `target`
- target state shown in status column
- convergence reasons shown for degraded targets
- target-based filter cycle added

## Phase Plan

### Phase 1: Schema and Validation
Deliverables:

1. Add `target` type validation.
2. Add `:wanted-by` and `:required-by` keywords.
3. Extend tuple to 31 elements and add accessors.
4. Enforce target-only and target-invalid field constraints.

Acceptance:

1. Target entry without command is valid.
2. Target entry with command is invalid.
3. Service entry parses `:wanted-by` and `:required-by` correctly.
4. Tuple length is 31 for parsed entries.
5. `make check` passes.

### Phase 2: Built-in Targets and Compatibility Mapping
Deliverables:

1. Add built-in targets `stage1` `stage2` `stage3` `stage4` `default`.
2. Inject built-ins at lowest precedence.
3. Add compatibility mapping from `:stage` to implicit membership in target mode.
4. Add `supervisor-startup-mode` and `supervisor-default-target` defcustoms.

Acceptance:

1. Stage mode behavior remains unchanged and existing stage-mode tests pass without semantic test rewrites.
2. Target mode with stage-only config maps units into built-in stage targets.
3. Explicit `:wanted-by` is not overwritten by stage mapping.
4. `make check` passes.

### Phase 3: Transaction Expansion Engine
Deliverables:

1. Implement deterministic expansion from root target.
2. Materialize inverse memberships from service membership keys.
3. Emit warnings for missing target references.
4. Detect cycles during expansion without infinite loops.

Acceptance:

1. Expanding `default` includes full compatible stage chain.
2. Expanding `stage2` excludes `stage3` and `stage4` members.
3. Missing target references are warned and dropped.
4. Expansion output is deterministic across identical input.
5. `make check` passes.

### Phase 4: Target-Aware Plan Builder
Deliverables:

1. Gate plan build path by `supervisor-startup-mode`.
2. Populate new plan fields in target mode.
3. Build one topological order for target mode.
4. Preserve existing stage plan path untouched.
5. Add deterministic plan fingerprint to plan metadata.

Acceptance:

1. Stage mode plan parity tests pass unchanged and expected order matches pre-migration snapshots.
2. Target mode plan includes global order and closure metadata.
3. Cross-stage requires in target mode is accepted.
4. Cycles in mixed target/service graphs trigger fallback deterministically.
5. `make check` passes.

### Phase 5: Target Runtime and Convergence
Deliverables:

1. Add target-mode startup path from `by-target` plan.
2. Extend DAG runtime to support passive target nodes.
3. Add target convergence checks and state transitions.
4. Complete startup when root target convergence reaches terminal state.

Acceptance:

1. Required members gate target reach.
2. Wanted-member failures do not block target reach.
3. Required-member invalid or failure yields `degraded` state.
4. Delays and blocking oneshots behave correctly in global DAG.
5. No polling loops are introduced.
6. `make check` passes.

### Phase 6: Dashboard Target UX
Deliverables:

1. Render target rows and convergence states in target mode.
2. Add target filter cycle.
3. Display degraded reasons.
4. Add member drill-down from target row.

Acceptance:

1. Stage mode dashboard output remains unchanged.
2. Target mode target rows and states are correct.
3. Filter cycle includes all defined targets.
4. `make check` passes.

### Phase 7: CLI Target Commands
Deliverables:

1. Implement `list-targets`.
2. Implement `target-status TARGET`.
3. Implement `explain-target TARGET` with root-cause chain.
4. Implement `isolate TARGET` with confirmation.
5. Implement `start --target TARGET` override.

Acceptance:

1. Commands work when supervisor is running.
2. Commands return clean actionable errors when supervisor is not running.
3. `start --target` uses the provided root target only for that invocation.
4. `make check` passes.

### Phase 8: Test Expansion
Deliverables:

1. Add ERT coverage for target parsing and validation.
2. Add ERT coverage for tuple extension and accessors.
3. Add ERT coverage for compatibility mapping.
4. Add ERT coverage for transaction expansion and determinism.
5. Add ERT coverage for target runtime convergence and degraded paths.
6. Add ERT coverage for dashboard target rendering.
7. Add ERT coverage for CLI target commands.

Acceptance:

1. All new tests pass.
2. All existing tests pass.
3. `make check` passes.

### Phase 9: Documentation and Final Audit
Deliverables:

1. Update `README.org` with target model, compatibility mapping, and command reference.
2. Update `CLAUDE.md` architecture and schema sections.
3. Update any stale stage-only statements to reflect mode-specific behavior.
4. Produce final audit report with evidence links.

Acceptance:

1. Documentation matches implementation exactly.
2. No stale claims about cross-stage requires in target mode.
3. Final audit checklist below is fully satisfied.
4. `make check` passes.

## Explicit Non-Goals
1. No socket activation.
2. No D-Bus activation.
3. No cgroup feature parity work.
4. No target-level restart policy.
5. No timer model redesign.
6. No removal of stage mode.
7. No automatic unit-file rewrite migration tool.

## Audit Evidence Requirements
Every phase audit MUST include the following artifacts.

1. Code change evidence:
   - exact files modified,
   - exact functions added, removed, or changed.
2. Test evidence:
   - new ERT test names introduced in the phase,
   - exact `make check` result,
   - explicit list of skipped tests.
3. Runtime evidence:
   - one successful path example,
   - one degraded or failure path example,
   - one deterministic-order proof point for the phase.
4. Compatibility evidence:
   - stage-mode parity confirmation,
   - target-mode activation example from a chosen root target.
5. Documentation evidence:
   - exact headings updated,
   - confirmation that no stale contradictions remain.

## Audit Checklist (Final Signoff)
All items MUST be true:

1. `target` entry type validated and enforced.
2. `:wanted-by` and `:required-by` parsed and materialized.
3. Built-in stage targets exist and preserve backward compatibility.
4. Startup mode gate works and is mode-correct.
5. Transaction expansion closure is deterministic and correct.
6. Target convergence states behave per contract.
7. Dashboard target UX is present and accurate.
8. CLI target commands are complete and accurate.
9. Existing behavior in stage mode is unchanged.
10. Full test suite passes with no regressions.
11. Documentation is synchronized with implemented behavior.
12. `make check` passes at release gate.
