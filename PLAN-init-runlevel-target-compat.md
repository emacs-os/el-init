# PLAN: SysV Init Runlevel Compatibility on Target Model

Date: 2026-02-16
Status: Final Locked
Authority: This file is the implementation and audit contract.

## Rule of Interpretation
All requirements in this document are mandatory.

- MUST means required for completion.
- MUST NOT means prohibited.
- SHOULD is not used in this file.
- MAY is not used in this file.

Any behavior not explicitly listed here is out of scope.

## Preconditions
This plan is sequenced after these plans are implemented:

1. `PLAN-stages-to-targets.md`
2. `PLAN-timer-to-target-trigger-support.md`

This plan extends target-mode behavior only. It does not reintroduce stage mode.

## Objective
Add SysV-compatible numeric init entry points (`init N`) on top of the target
system, using systemd mapping semantics as the only mapping model.

If "common Unix runlevel folklore" conflicts with systemd mapping, systemd
mapping wins.

## Locked Decisions
1. Systemd mapping semantics are authoritative when conflicts exist.
2. Numeric runlevel mapping is fixed exactly as follows:
   - `0 -> poweroff.target`
   - `1 -> rescue.target`
   - `2 -> multi-user.target`
   - `3 -> multi-user.target`
   - `4 -> multi-user.target`
   - `5 -> graphical.target`
   - `6 -> reboot.target`
3. `init 5` maps to `graphical.target`, not `multi-user.target`.
4. No alternate runlevel mapping table is configurable.
5. New built-in canonical targets are fixed to exactly:
   - `rescue.target`
   - `shutdown.target`
   - `poweroff.target`
   - `reboot.target`
6. Existing built-in targets from target plan remain unchanged:
   - `basic.target`
   - `multi-user.target`
   - `graphical.target`
   - `default.target`
7. Add fixed alias targets with immutable alias mapping:
   - `runlevel0.target -> poweroff.target`
   - `runlevel1.target -> rescue.target`
   - `runlevel2.target -> multi-user.target`
   - `runlevel3.target -> multi-user.target`
   - `runlevel4.target -> multi-user.target`
   - `runlevel5.target -> graphical.target`
   - `runlevel6.target -> reboot.target`
8. Alias targets are passive alias entries and are never spawnable processes.
9. `init N` is implemented as a strict alias for `isolate` on the mapped target.
10. `init N` MUST NOT persist default-target changes.
11. Existing `get-default` and `set-default TARGET` semantics remain unchanged.
12. `set-default TARGET` MAY accept `runlevel*.target`, but MUST persist the
    resolved canonical target ID.
13. `init` accepts only integer values `0` through `6`.
14. `init` values outside `0..6` are hard errors.
15. `init 0` and `init 6` MUST require explicit destructive-action
    confirmation.
16. This plan does not add host-kernel power control primitives.
17. `poweroff.target` and `reboot.target` are supervisor transaction targets,
    not direct kernel power operations.
18. Timer-trigger semantics remain start-only per timer plan; timers MUST NOT be
    allowed to trigger init-transition targets.
19. Timer validation MUST reject these targets:
    - `rescue.target`
    - `shutdown.target`
    - `poweroff.target`
    - `reboot.target`
    - `runlevel0.target` through `runlevel6.target`
20. Existing non-goal "no `systemd.special(7)` parity beyond four built-ins"
    from `PLAN-stages-to-targets.md` is superseded only for the targets in this
    document.

## Canonical Data Model

### 1) New Built-in Target Topology
The canonical built-in topology is fixed to:

1. `rescue.target` requires and starts after `basic.target`.
2. `shutdown.target` is a synchronization barrier for shutdown transactions.
3. `poweroff.target` requires and starts after `shutdown.target`.
4. `reboot.target` requires and starts after `shutdown.target`.

No additional built-in targets are introduced by this plan.

### 2) Alias Target Contract
Alias targets are name-resolution entries only.

1. Alias targets MUST resolve to canonical target IDs before graph expansion.
2. Alias resolution MUST be deterministic and acyclic.
3. Alias cycles are invalid configuration and MUST fail preflight.
4. Runtime diagnostics MUST always include both alias ID and resolved canonical
   ID for operator clarity.

### 3) Runlevel Mapping Contract
Runlevel mapping is represented as alias mapping, not a second scheduler mode.

1. `init N` resolves to `runlevelN.target`.
2. `runlevelN.target` resolves to canonical target from Locked Decisions.
3. Scheduler executes one target transaction model only.

## Runtime Semantics

### 1) `init N` Execution Flow
`init N` execution is fixed:

1. Parse and validate `N` in `0..6`.
2. Resolve `runlevelN.target` alias to canonical target.
3. Execute `isolate` against resolved canonical target.
4. Do not mutate persisted default target state.
5. Surface result summary including numeric runlevel and target IDs.

### 2) Isolate Behavior Reuse
`init N` MUST reuse `isolate TARGET` semantics and safety checks.

1. No separate runlevel scheduler path exists.
2. No separate runlevel state machine exists.
3. All convergence and degraded-state handling comes from target runtime.

### 3) Destructive Transition Guard
For `init 0` and `init 6`:

1. Explicit confirmation prompt is mandatory in interactive CLI mode.
2. Non-interactive mode requires explicit force flag.
3. Missing confirmation or force flag is a hard error.

### 4) Timer Interaction Guard
To preserve timer plan guarantees:

1. Timer validation rejects init-transition targets listed above.
2. Rejected timers are invalid and excluded from scheduler activation.
3. Dashboard and CLI invalid timer diagnostics MUST clearly state
   "init-transition target is not timer-eligible".

## Command Surface
Additive command behavior:

1. Add CLI command: `init N`.
2. Add CLI alias command: `telinit N` (identical behavior to `init N`).
3. Extend `isolate TARGET` to accept `runlevel*.target` aliases.
4. `list-targets` MUST indicate whether a target row is canonical or alias.
5. `target-status TARGET` MUST display resolved canonical target for aliases.

No other command names are introduced by this plan.

## Validation Contract
Validation requirements are fixed:

1. Alias target IDs MUST end with `.target`.
2. Alias target IDs MUST resolve to existing canonical targets.
3. Canonical target IDs in this plan MUST exist after merge.
4. Any redefinition of fixed built-in alias mapping is invalid.
5. Any user unit attempting to redefine `runlevel*.target` mapping is invalid.
6. Timer definitions targeting init-transition targets are invalid.

## Phase Plan

### Phase 1: Built-in Targets and Alias Model
Deliverables:

1. Add canonical built-in targets from this plan.
2. Add fixed alias targets `runlevel0.target`..`runlevel6.target`.
3. Add deterministic alias-resolution path with diagnostics.

Acceptance:

1. Alias resolution is deterministic and cycle-safe.
2. Canonical targets and alias targets appear in merged target set.
3. `make check` passes.

### Phase 2: CLI `init` and `telinit`
Deliverables:

1. Implement `init N` command parser and execution path.
2. Implement `telinit N` alias command.
3. Enforce numeric range validation and error messaging.

Acceptance:

1. `init 0..6` resolves correctly to mapped targets.
2. Invalid values fail with actionable errors.
3. `telinit N` behavior is identical to `init N`.
4. `make check` passes.

### Phase 3: Isolate Integration and Safety Guards
Deliverables:

1. Route `init N` through existing isolate transaction path.
2. Add destructive-transition confirmation/force handling for `0` and `6`.
3. Ensure no default-target persistence side effects.

Acceptance:

1. `init N` never mutates persisted default target link.
2. Confirmation guard works for destructive transitions.
3. Runtime results include both numeric and target identity.
4. `make check` passes.

### Phase 4: Timer Compatibility Guard
Deliverables:

1. Extend timer validation with init-transition denylist.
2. Add explicit invalid reasons in CLI and dashboard timer surfaces.
3. Keep existing timer start semantics unchanged for eligible targets.

Acceptance:

1. Timer targets in denylist are rejected deterministically.
2. Eligible timers (`oneshot`/`simple`/regular `target`) continue to work.
3. No new timer isolate behavior is introduced.
4. `make check` passes.

### Phase 5: CLI and Dashboard Visibility
Deliverables:

1. Show alias/canonical distinction in `list-targets` output.
2. Show alias resolution in `target-status` and `explain-target` output.
3. Add concise operator messages for `init N` actions/results.

Acceptance:

1. Alias resolution is visible and unambiguous in human and JSON surfaces.
2. Output remains backward compatible with additive fields only.
3. `make check` passes.

### Phase 6: Test Expansion
Deliverables:

1. Add ERT tests for mapping table correctness (`0..6`).
2. Add ERT tests for alias resolution and alias-cycle rejection.
3. Add ERT tests for `init`/`telinit` command parsing and errors.
4. Add ERT tests for destructive-confirmation behavior.
5. Add ERT tests proving `init N` does not persist default target.
6. Add ERT tests for timer denylist validation and diagnostics.

Acceptance:

1. New tests pass.
2. Existing tests pass.
3. `make check` passes.

### Phase 7: Documentation and Final Audit
Deliverables:

1. Update `README.org` target and command sections for `init`/`telinit` and
   runlevel mapping table.
2. Document systemd-authoritative mapping explicitly.
3. Document timer denylist for init-transition targets.
4. Produce final audit evidence per checklist below.

Acceptance:

1. Documentation reflects implementation exactly.
2. No contradictory runlevel mapping claims remain.
3. `make check` passes.

## Explicit Non-Goals
1. No alternate non-systemd runlevel mapping profiles.
2. No reintroduction of stage startup mode.
3. No parsing of SysV `/etc/inittab`.
4. No SysV script directory emulation (`/etc/rc*.d`).
5. No kernel-level poweroff/reboot implementation in this plan.
6. No expansion to additional `systemd.special(7)` targets beyond this file.
7. No timer-driven isolate or timer-driven runlevel transitions.

## Audit Evidence Requirements
Every phase audit MUST include:

1. Code evidence:
   - exact files changed,
   - exact functions added/changed.
2. Mapping evidence:
   - proof that each runlevel `0..6` resolves to correct canonical target.
3. Safety evidence:
   - proof destructive confirmation guard works for `0` and `6`.
4. Persistence evidence:
   - proof `init N` does not change default target link.
5. Timer evidence:
   - proof init-transition timer targets are rejected with explicit reason.
6. Test evidence:
   - new ERT test names,
   - exact `make check` result,
   - explicit skipped test list.
7. Documentation evidence:
   - exact `README.org` headings updated,
   - confirmation stale or conflicting mapping text removed.

## Audit Checklist (Final Signoff)
All items MUST be true:

1. `init N` exists and supports only `0..6`.
2. Mapping table matches this plan exactly.
3. `init 5` resolves to `graphical.target`.
4. `init 2`, `init 3`, and `init 4` resolve to `multi-user.target`.
5. `init 0` resolves to `poweroff.target`.
6. `init 1` resolves to `rescue.target`.
7. `init 6` resolves to `reboot.target`.
8. `telinit N` parity with `init N` is complete.
9. `init N` reuses isolate path and does not persist default target state.
10. Destructive transition guards are enforced.
11. Timer denylist for init-transition targets is enforced.
12. Alias and canonical target identity are visible in CLI/dashboard surfaces.
13. Full test suite passes without regressions.
14. Documentation is synchronized with implemented behavior.
15. `make check` passes at release gate.
