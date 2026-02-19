# PLAN: Full `:conflicts` Semantics for elinit

Date: 2026-02-19
Status: Draft Locked
Authority: This file is the implementation contract for full `:conflicts`
support.

## Rule of Interpretation
All requirements in this file are mandatory.

- MUST means required for completion.
- MUST NOT means prohibited.
- SHOULD and MAY are not used.

## Objective
Add `:conflicts` support with systemd-like behavior across all start paths, not
just manual start.

Target outcome:

1. Units declared as conflicts MUST NOT remain active together.
2. Conflict handling MUST be transaction-aware (startup, manual start, restart,
   reconcile, isolate).
3. Runtime behavior MUST be deterministic and free from restart thrash loops.
4. CLI/dashboard status MUST explain conflict-driven actions.

## Non-Goals
The following are explicitly out of scope for this plan:

1. Implementing all of systemd job engine semantics.
2. Introducing persistent "job objects" or a systemd-compatible DBus model.
3. Adding new process supervision backends.

## Product Contract

### 1) Schema contract
1. `:conflicts` MUST be accepted in entry and unit-file keyword whitelists.
2. `:conflicts` value MUST be string or proper list of strings (or nil).
3. Empty IDs and self-references in `:conflicts` MUST be rejected.
4. Unknown conflict targets MUST be handled consistently with the selected
   policy in this plan (see planner contract).

### 2) Planner contract
1. Plan construction MUST validate and normalize `:conflicts`.
2. Plan artifact MUST expose conflict relations for runtime use.
3. Conflict relations MUST be deterministic and deduplicated.
4. Conflict metadata MUST survive plan publication and be available to
   CLI/dashboard.

### 3) Transaction contract
1. Every start path MUST perform conflict preflight before spawn:
   - startup DAG path
   - manual start path
   - reconcile start path
   - isolate start path
   - crash-restart timer path
2. Conflict preflight MUST stop (or suppress) conflicting active units before
   starting the requester.
3. Conflict preflight MUST cancel incompatible delayed-start and restart timers
   for conflicting units.
4. Transaction ordering MUST avoid A<->B restart ping-pong.

### 4) Runtime contract
1. A conflict-driven stop MUST suppress automatic restart for the stopped unit
   until explicitly reactivated by policy/transaction.
2. Conflict handling MUST preserve existing enabled/disabled/masked semantics.
3. Conflict handling MUST preserve oneshot readiness and target convergence
   correctness.

### 5) UX contract
1. CLI MUST surface conflict reasons in human and JSON output.
2. Dashboard detail/status MUST surface conflict reasons.
3. Logs/events MUST include conflict-cause context for traceability.

## Architecture Plan

## Phase C0: Semantics lock and policy decisions (LOCKED)

Decisions:

1. Missing conflict IDs: warning + drop (same as `:after`/`:wants`).
2. Targets MAY declare `:conflicts`.
3. Conflicts are symmetric at runtime (A conflicts B implies B conflicts A).
4. Conflict stop reason symbol: `conflict-stopped`.
5. User-facing text: "conflict-stopped (by %s)".
6. Suppression state symbol: `conflict-suppressed` (blocks auto-restart).

## Phase C1: Schema and parser plumbing
Deliverables:

1. Add `:conflicts` to:
   - `elinit--valid-keywords` in `elinit-core.el`
   - `elinit--unit-file-keywords` in `elinit-units.el`
2. Extend `elinit-service` struct and optional field defaults with conflicts.
3. Extend tuple schema, parse path, and accessor function:
   - add `elinit-entry-conflicts`.
4. Extend migration helpers and service roundtrip conversion.
5. Extend unit scaffold template comments in `elinit-units.el`.

Acceptance:

1. Parsing and roundtrip tests pass for string/list/nil `:conflicts`.
2. Unknown keyword tests still pass.
3. Duplicate-key behavior still passes for `:conflicts`.

## Phase C2: Validation and planner integration
Deliverables:

1. Add validation for `:conflicts` type and empty/self ID checks.
2. Add global reference validation policy for conflicts.
3. Extend `elinit-plan` with conflicts maps:
   - `conflicts-deps` (id -> conflicts list)
   - `conflict-reverse` (id -> units that conflict with id), if needed.
4. Ensure plan fingerprint includes conflicts data.
5. Ensure cycle fallback logic does not corrupt conflict metadata.

Acceptance:

1. `elinit--build-plan` produces deterministic conflicts metadata.
2. Conflict metadata is available after `elinit-start`, `daemon-reload`,
   `reconcile`, and `isolate`.
3. Plan determinism tests cover conflict changes.

## Phase C3: Conflict transaction primitives
Deliverables:

1. Implement core helpers in `elinit-core.el`:
   - find active conflicting units for requester
   - stop conflicting active units with explicit reason
   - cancel conflicting delay/restart timers
   - mark conflict-stop suppression state
2. Add dedicated runtime state for conflict suppression (separate from
   manual-stop semantics if needed).
3. Add helper to clear suppression when explicit transaction restarts a unit.

Acceptance:

1. Primitive helpers are pure where possible and side-effect boundaries are
   explicit.
2. Unit tests validate stop/cancel/suppress behavior in isolation.

## Phase C4: Integrate all start paths
Deliverables:

1. DAG startup integration:
   - preflight conflict handling before each spawn attempt.
2. Manual start integration:
   - apply same preflight and suppression semantics.
3. Reconcile integration:
   - start operations apply conflict preflight.
4. Isolate integration:
   - conflict preflight runs for all started units.
5. Restart timer integration:
   - restart callback checks suppression/conflicts before re-spawn.

Acceptance:

1. No start path can activate a unit while active conflicts remain.
2. Transaction results are deterministic under stable input.
3. Existing lifecycle behavior (oneshot, delays, startup completion rules)
   remains correct.

## Phase C5: Sentinel and restart loop hardening
Deliverables:

1. Sentinel restart logic honors conflict suppression state.
2. Conflict-stopped units do not immediately auto-restart into thrash loops.
3. Crash-loop detection and conflict suppression interactions are defined and
   tested.
4. Conflict stop reason survives status/reason queries until cleared.

Acceptance:

1. A conflicts-with-B and B conflicts-with-A cannot oscillate indefinitely.
2. Restart policy still works for non-conflict exits.

## Phase C6: CLI and dashboard surface
Deliverables:

1. Add `conflicts` field to CLI entry info and JSON output.
2. Add human-readable conflicts line in `describe` output.
3. Add dashboard detail panel line for conflicts.
4. Add status reason text for conflict-driven stop/suppression.

Acceptance:

1. CLI and dashboard expose conflicts and conflict reasons consistently.
2. JSON fields are stable and covered by tests.

## Phase C7: Documentation and comprehensive test pass
Deliverables:

1. Update `README.org`:
   - keyword table (`:conflicts`)
   - validation rules
   - dependency/conflict semantics section
   - examples
2. Add or extend ERT coverage:
   - keyword whitelist + parse + validate
   - plan metadata + determinism/fingerprint
   - startup DAG conflict handling
   - manual start/restart/reconcile/isolate conflict handling
   - restart suppression and anti-thrash
   - CLI/dashboard representation
3. Run full checks:
   - `make check`

Acceptance:

1. `make check` passes with all new tests.
2. README reflects implemented behavior exactly.

## Required Code Touchpoints
The implementation MUST cover these modules:

1. `elinit-core.el` (schema, validation, planner, runtime, restart/sentinel).
2. `elinit-units.el` (unit keyword list, scaffold comment).
3. `elinit-cli.el` (JSON/human fields and reason strings).
4. `elinit-dashboard.el` (detail/status surfaces).
5. `README.org` (handbook updates).
6. `assets/tests/*.el` (ERT coverage).

## Risk Register and Controls

1. Risk: restart ping-pong between mutual conflicts.
Control: dedicated suppression state and sentinel checks.

2. Risk: async stop/start races in DAG path.
Control: preflight stop and timer cancellation before spawn attempt.

3. Risk: regression to startup completion criteria.
Control: keep DAG readiness and blocking invariants unchanged; add dedicated
tests.

4. Risk: status inconsistency across CLI/dashboard.
Control: single source for reason/status mapping in core, reused by UI/CLI.

## Rollout Strategy

1. Merge by phase with tests in same commit as behavior change.
2. Do not expose docs claiming full support until Phase C7 is complete.
3. If a mid-phase cut is needed, expose only "best effort conflict on manual
   start" under an explicit temporary note and keep full plan active.

## Exit Criteria
This plan is complete when:

1. All phases C0..C7 acceptance criteria are met.
2. `:conflicts` is enforced consistently across all start paths.
3. No known restart thrash bug remains in conflict scenarios.
4. `make check` passes.
5. README and UI/CLI behavior match implementation.
