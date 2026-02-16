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
Replace stage-partitioned startup with a target-based activation model as the
only startup model, while preserving current robustness guarantees.

Adopt familiar systemd target terminology and default target names that make
sense for supervisor.el user-session workflows.

The migration is complete only when stage mode is removed and target mode is
production-ready.

## Locked Decisions
1. Targets are first-class unit entries using `:type target` in the same
   unit-file system.
2. No separate target file format is introduced.
3. No separate `supervisor-targets` defcustom is introduced.
4. Target entries MUST NOT define `:command`.
5. Stage mode is removed.
6. No `supervisor-startup-mode` defcustom exists after cutover.
7. Startup root is selected by `supervisor-default-target`.
8. `supervisor-default-target` defaults to `"default.target"`.
9. New defcustom `supervisor-default-target-link` is required.
10. `supervisor-default-target-link` accepts any `.target` ID.
11. The configured link target MUST exist after unit merge, or startup fails.
12. `supervisor-default-target-link` defaults to `"graphical.target"`.
13. Built-in targets are fixed to exactly these IDs:
    `"basic.target"`, `"multi-user.target"`, `"graphical.target"`,
    `"default.target"`.
14. Built-in target dependency graph is fixed:
    - `multi-user.target` has `:requires ("basic.target")` and
      `:after ("basic.target")`.
    - `graphical.target` has `:requires ("multi-user.target")` and
      `:after ("multi-user.target")`.
    - `default.target` is alias-style and resolves to
      `supervisor-default-target-link` at startup.
15. Existing service `:stage` declarations are invalid and MUST surface
    validation errors.
16. No compatibility mapping from `:stage` to target membership exists.
17. Scheduler graph is global and never partitioned by stage.
18. Existing CLI/dashboard commands and keybindings remain stable; target
    commands are additive.
19. No automatic unit-file rewrite migration tool is provided.
20. Target IDs MUST end with `.target`.
21. Non-target IDs MUST NOT end with `.target`.
22. CLI MUST provide `get-default` and `set-default TARGET`.
23. `set-default TARGET` MUST persist the default-target link value.
24. `isolate TARGET` MUST NOT persist default-target changes.

## Robustness Invariants
The following invariants MUST hold.

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
8. Root target resolution is deterministic.

## Canonical Data Model

### 1) Entry Types
Valid `:type` values are:

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
- `:stage`, `:wanted-by`, `:required-by`

### 3) Target Naming Contract

- `:type target` units MUST use IDs ending in `.target`.
- Non-target units MUST use IDs not ending in `.target`.
- Violations MUST be marked invalid with explicit reason.

### 4) Service to Target Membership Keys
New keys for service entries:

- `:wanted-by` list of target IDs
- `:required-by` list of target IDs

Materialization rules:

- Service `A` with `:wanted-by ("X.target")` adds `A` to target `X.target`
  wants-membership.
- Service `A` with `:required-by ("X.target")` adds `A` to target `X.target`
  requires-membership.
- Missing target references are validation errors on the owning service entry.

### 5) Target Reference Validation Contract
Reference validation is mandatory and runs after authority merge.

- `:wanted-by` and `:required-by` values MUST reference existing
  `:type target` units.
- In `:type target` entries, every `:requires` member MUST reference an
  existing unit.
- In `:type target` entries, missing `:wants`, `:after`, and `:before`
  references are warnings and are dropped deterministically.
- Self-reference in `:requires`, `:wants`, `:after`, or `:before` is invalid.
- Hard-dependency cycles formed by `:requires` are invalid for participating
  units and MUST be surfaced with a cycle reason.

### 6) Deprecated Stage Key Contract
The `:stage` key is removed from startup semantics.

- Any unit declaring `:stage` MUST be marked invalid.
- Invalid reason MUST explicitly instruct using `:wanted-by` and
  `:required-by`.

### 7) Entry Tuple Extension
Entry tuple extends from 31 to 33 elements.
(Indices 29-30 are occupied by `:user` and `:group` from prior work.)

- index 31: `wanted-by`
- index 32: `required-by`

Required accessors:

- `supervisor-entry-wanted-by`
- `supervisor-entry-required-by`

### 8) Built-in Targets
Built-in targets are always present at lowest authority precedence:

- `basic.target`
- `multi-user.target`
- `graphical.target`
- `default.target`

Any of these IDs may be overridden by higher authority tiers.

### 9) Built-in Default Topology
The built-in topology is fixed:

1. `basic.target` has no required built-in predecessor.
2. `multi-user.target` requires and starts after `basic.target`.
3. `graphical.target` requires and starts after `multi-user.target`.
4. `default.target` is alias-style and resolves to
   `supervisor-default-target-link` at startup.

### 10) Automatic Ordering Rule for Target Dependencies
For `:type target` entries only:

- Every declared `:requires` dependency MUST also imply an `:after` edge.
- Every declared `:wants` dependency MUST also imply an `:after` edge.

No automatic dependency injection is applied to non-target units.
No `DefaultDependencies=no` equivalent is provided in this plan.

### 11) Plan Struct Contract
Plan version is incremented to `2`.

Plan fields:

- `by-target`: global activation order
- `target-members`: mapping target -> `(:requires ... :wants ...)`
- `activation-root`: target root ID used for expansion
- `activation-closure`: all target and service IDs in transaction

`by-stage` is removed from the runtime plan contract.

### 12) Default-Target State Contract
Persistent state MUST include `default-target-link`.

- Value type is string.
- Value MUST end with `.target`.
- Value MUST NOT equal `default.target`.
- Value MUST reference an existing target after merge when used.
- Source priority is: persisted state, then defcustom default.

## Runtime Semantics

### 0) Preflight Validation Gate
Startup MUST execute a preflight validation gate before plan build.

1. Run schema validation and target-reference validation.
2. Persist validation diagnostics into invalid-reason surfaces.
3. If configured default-target link is invalid, startup MUST fail.
4. If resolved startup root target is invalid, startup MUST fail.
5. Invalid non-root units are isolated and MUST NOT block unrelated valid units.

### 1) Root Target Resolution
Startup root resolution is fixed:

1. Read `supervisor-default-target`.
2. Resolve effective default-target-link from persisted state override or
   `supervisor-default-target-link`.
3. If root is `default.target`, resolve to effective default-target-link.
4. If resolved root target does not exist after merge, fail startup with
   actionable error.
5. Resolved root target ID is recorded in plan `activation-root`.

### 2) Target Transaction Expansion
Given resolved `ROOT` target:

1. Initialize closure with `ROOT`.
2. Expand target `:requires` and `:wants` members transitively.
3. Materialize inverse memberships from `:wanted-by` and `:required-by`
   before expansion.
4. Include reachable services and targets only.
5. Mark non-closure units as unreachable diagnostics.
6. Expansion order MUST be deterministic.

### 3) Target Scheduling
Startup uses one global DAG over closure nodes.

Graph edges:

- ordering edges from `:after` and inverted `:before`
- hard dependency edges from `:requires`
- soft dependency edges from `:wants`
- implicit target-order edges from Rule 9 in Canonical Data Model

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
6. `get-default`
7. `set-default TARGET`

`isolate TARGET` MUST require confirmation.
`isolate TARGET` is transaction-scoped and does not change persisted default.

`get-default` prints the effective default target that `default.target`
resolves to.

`set-default TARGET` updates persisted default-target-link.
`set-default TARGET` MUST reject `default.target`.
`set-default TARGET` MUST reject unknown targets.

Dashboard target support is mandatory:

- target rows rendered with type `target`
- target state shown in status column
- convergence reasons shown for degraded targets
- target-based filter cycle added
- resolved startup root is displayed when root is `default.target`

## Phase Plan

### Phase 1: Schema and Validation
Deliverables:

1. Add `target` type validation.
2. Add `:wanted-by` and `:required-by` keywords.
3. Extend tuple to 33 elements and add accessors.
4. Enforce target-only and target-invalid field constraints.
5. Enforce `:stage` as invalid with explicit replacement guidance.
6. Enforce `.target` suffix rules for target and non-target IDs.
7. Add merged-graph reference validation rules from Canonical Data Model
   Section 5.

Acceptance:

1. Target entry without command is valid.
2. Target entry with command is invalid.
3. Service entry parses `:wanted-by` and `:required-by` correctly.
4. Service entry with `:stage` is invalid with actionable reason.
5. Target ID without `.target` suffix is invalid.
6. Non-target ID with `.target` suffix is invalid.
7. Tuple length is 33 for parsed entries.
8. Missing `:wanted-by` and `:required-by` targets invalidate the owner unit.
9. Target `:requires` missing-reference is invalid.
10. Target missing `:wants`/`:after`/`:before` references warn and drop.
11. Hard `:requires` cycles are surfaced as invalid with cycle reason.
12. `make check` passes.

### Phase 2: Systemd-Style Defaults and Hard Cutover
Deliverables:

1. Add built-in targets:
   `basic.target`, `multi-user.target`, `graphical.target`, `default.target`.
2. Add built-in target topology from Canonical Data Model Section 8.
3. Add `supervisor-default-target` with default `default.target`.
4. Add `supervisor-default-target-link` with default `graphical.target`.
   Allow any `.target` value that resolves to an existing target.
5. Implement alias-style `default.target` resolution.
6. Remove stage startup path.
7. Remove mode gate logic; startup is target-based only.
8. Remove compatibility mapping and related conditionals.

Acceptance:

1. No code path remains that partitions startup by stage.
2. Stage-only unit configurations fail validation and are surfaced clearly.
3. Fresh setup resolves default root chain to `graphical.target`.
4. Setting `supervisor-default-target-link` to `multi-user.target` works.
5. Setting `supervisor-default-target-link` to an existing custom
   `X.target` works.
6. Built-in target graph ordering is deterministic and test-covered.
7. `make check` passes.

### Phase 3: Transaction Expansion Engine
Deliverables:

1. Implement deterministic expansion from resolved root target.
2. Materialize inverse memberships from service membership keys.
3. Expand only from validation-accepted units.
4. Detect cycles during expansion without infinite loops.

Acceptance:

1. Expanding `default.target` includes exactly its reachable closure.
2. Validation-invalid references are excluded before expansion.
3. Expansion output is deterministic across identical input.
4. `make check` passes.

### Phase 4: Target-Aware Plan Builder
Deliverables:

1. Populate plan version 2 fields.
2. Build one topological order for target mode.
3. Apply automatic target-order rule (`:requires` and `:wants` imply
   `:after` for target entries).
4. Remove stage-plan field population.
5. Add deterministic plan fingerprint to plan metadata.

Acceptance:

1. Plan includes global order and closure metadata.
2. Cycles in mixed target/service graphs trigger fallback deterministically.
3. Target dependency auto-ordering is test-covered.
4. No stage-only plan assertions remain in tests.
5. `make check` passes.

### Phase 5: Target Runtime and Convergence
Deliverables:

1. Add target startup path from `by-target` plan.
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

1. Render target rows and convergence states.
2. Add target filter cycle.
3. Display degraded reasons.
4. Add member drill-down from target row.
5. Show resolved startup root when `default.target` indirection is used.

Acceptance:

1. Target rows and states are correct.
2. Filter cycle includes all defined targets.
3. Root target indirection is visible in dashboard status surfaces.
4. `make check` passes.

### Phase 7: CLI Target Commands
Deliverables:

1. Implement `list-targets`.
2. Implement `target-status TARGET`.
3. Implement `explain-target TARGET` with root-cause chain.
4. Implement `isolate TARGET` with confirmation.
5. Implement `start --target TARGET` override.
6. Implement `get-default`.
7. Implement `set-default TARGET`.

Acceptance:

1. Commands work when supervisor is running.
2. Commands return clean actionable errors when supervisor is not running.
3. `start --target` uses the provided root target only for that invocation.
4. `target-status default.target` reports resolved root target.
5. `get-default` returns the effective link target.
6. `set-default TARGET` persists and is visible to subsequent `get-default`.
7. `set-default default.target` is rejected with actionable error.
8. `make check` passes.

### Phase 8: Test Expansion
Deliverables:

1. Add ERT coverage for target parsing and validation.
2. Add ERT coverage for tuple extension and accessors.
3. Add ERT coverage for stage-key rejection.
4. Add ERT coverage for `.target` suffix validation rules.
5. Add ERT coverage for built-in target defaults and topology.
6. Add ERT coverage for `default.target` indirection behavior.
7. Add ERT coverage for transaction expansion and determinism.
8. Add ERT coverage for target runtime convergence and degraded paths.
9. Add ERT coverage for dashboard target rendering.
10. Add ERT coverage for CLI target commands.
11. Remove or rewrite stage-mode parity tests.
12. Add ERT coverage for `get-default` and `set-default` persistence.
13. Add ERT coverage for all new target reference-validation failure modes.

Acceptance:

1. All new tests pass.
2. All retained tests pass.
3. `make check` passes.

### Phase 9: Documentation and Final Audit
Deliverables:

1. Update `README.org` with target model and command reference.
2. Document built-in defaults:
   `default.target`, `graphical.target`, `multi-user.target`, `basic.target`.
3. Document `supervisor-default-target` and
   `supervisor-default-target-link` behavior.
4. Update `CLAUDE.md` architecture and schema sections.
5. Remove stale stage-mode statements throughout docs.
6. Produce final audit report with evidence links.

Acceptance:

1. Documentation matches implementation exactly.
2. No stale claims about stage-mode behavior remain.
3. Built-in default-target behavior is documented with concrete examples.
4. Final audit checklist below is fully satisfied.
5. `make check` passes.

## Explicit Non-Goals
1. No socket activation.
2. No D-Bus activation.
3. No cgroup feature parity work.
4. No target-level restart policy.
5. No timer model redesign.
6. No stage-mode compatibility layer.
7. No automatic unit-file rewrite migration tool.
8. No full `systemd.special(7)` parity beyond the four built-in targets in
   this plan.
9. No `.wants/` or `.requires/` filesystem directory management model.
10. No automatic `Conflicts=` or `Before=` injection against
    `shutdown.target`.
11. No `DefaultDependencies=no` feature flag.
12. No full `systemctl` parity beyond target commands in this plan.

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
4. Cutover evidence:
   - proof that no stage startup path remains,
   - proof that `:stage` is rejected with explicit guidance.
5. Default-target evidence:
   - proof of built-in target graph,
   - proof that `default.target` resolves through
     `supervisor-default-target-link`.
6. Default-target command evidence:
   - proof of `get-default` behavior,
   - proof of `set-default TARGET` persistence and validation.
7. Validation evidence:
   - proof of reference-validation failures and surfaced diagnostics,
   - proof that invalid root/default configuration fails startup.
8. Documentation evidence:
   - exact headings updated,
   - confirmation that no stale contradictions remain.

## Audit Checklist (Final Signoff)
All items MUST be true:

1. `target` entry type validated and enforced.
2. `:wanted-by` and `:required-by` parsed and materialized.
3. `:stage` is rejected and surfaced with actionable error text.
4. `.target` naming contract is enforced for target and non-target IDs.
5. No stage startup mode or mode gate code path remains.
6. Built-in targets exist exactly as specified.
7. `default.target` resolves through `supervisor-default-target-link`.
8. `supervisor-default-target-link` accepts existing custom `.target` IDs.
9. Startup root selection is target-based and deterministic.
10. Transaction expansion closure is deterministic and correct.
11. Reference-validation contract is implemented exactly.
12. Invalid root/default target configuration fails fast with actionable error.
13. Target convergence states behave per contract.
14. Dashboard target UX is present and accurate.
15. CLI target commands are complete and accurate.
16. `get-default` and `set-default TARGET` are implemented and validated.
17. Full test suite passes with no regressions.
18. Documentation is synchronized with implemented behavior.
19. `make check` passes at release gate.
