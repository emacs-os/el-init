# PLAN: Runtime Telemetry, Parity Docs, and Mutator Consistency Audit

Date: 2026-02-12
Status: Draft for implementation

## Scope

This plan covers all remaining TODO items except keybind redesign (already covered in `PLAN-dashboard-keybinds-nested-menu.md`):

- richer per-service status telemetry for CLI and dashboard,
- comparative design review of supervisor stage model vs runit stage model,
- handbook/systemd-alignment documentation expansion,
- full consistency audit of explicit-verb mutations vs legacy toggle/cycle semantics.

This plan does not include logging-architecture rework or unit-file schema expansion beyond what is needed to document current behavior.

## Locked Decisions (From TODO)

1. Telemetry should be systemctl-status-inspired and available in both CLI and interactive dashboard detail views.
2. Stage-model work in this plan is analysis/documentation-first (compare/contrast/recommendation), not a mandatory runtime redesign.
3. Documentation must clearly separate:
- systemd-aligned behavior and terminology,
- intentional format/architecture differences in supervisor.
4. Governance for mutations:
- operational state changes must use explicit verbs,
- unit-definition semantics must be changed via unit-file edit + reload path,
- no hidden state twiddles for unit-definition behavior.

## Canonical Contracts

### A) Telemetry contract (status/detail)

For each unit, detail views should expose where available:

- active state and reason,
- running-since / uptime,
- PID and process-tree summary (best effort),
- restart count and last failure reason (exit/signal),
- next restart ETA when backoff is pending,
- authoritative unit source path (and tier when available),
- lightweight memory/CPU/tasks metrics when obtainable without blocking,
- recent log tail preview.

Constraints:

- non-blocking and low overhead,
- degrade gracefully when platform/process metadata is unavailable,
- avoid noisy minibuffer/UI churn for periodic updates.

### B) Stage-model analysis contract

Produce an explicit compare/contrast between:

- supervisor stage model,
- runit stage model,
- systemd target model (context section).

Required outcomes:

- semantics table,
- operational tradeoffs,
- recommendation: keep as-is or targeted refinements.

### C) Documentation contract

`README.org` must include:

- systemd-compat terminology and command mapping section,
- service/unit feature support table (supported / partial / unplanned),
- 2-3 systemd unit examples with supervisor Elisp equivalents,
- stage vs target explanation,
- supervisor-stage vs runit comparison table.

Docs must describe current behavior only (no aspirational claims).

### D) Mutator consistency contract

All mutating paths in CLI/dashboard/core must classify as either:

- explicit operational verb,
- unit-definition change (must route to edit workflow),
- approved UI convenience that still maps 1:1 to explicit canonical operation.

No behavioral drift between CLI and dashboard for the same action.

## Phase Plan (8 Phases)

### Phase 1: Mutator Inventory and Classification

Deliverables:

- inventory all state-mutating entry points in:
  - `supervisor-cli.el`,
  - `supervisor-dashboard.el`,
  - `supervisor-core.el`.
- classify each mutator as explicit verb, toggle/cycle, or unit-definition mutation.

Acceptance:

- complete mutator matrix exists and is reviewable,
- every mutator has an owner path and expected semantic category.

### Phase 2: Parity Audit and Gap Report

Deliverables:

- verify CLI/dashboard parity for lifecycle and policy actions:
  - start/stop/restart/kill/reload/reset-failed,
  - enable/disable,
  - mask/unmask,
  - restart-policy,
  - logging.
- identify drift, ambiguity, and hidden toggles.

Acceptance:

- written gap list with exact file/function references,
- each gap assigned a concrete remediation decision.

### Phase 3: Mutator Normalization Implementation

Deliverables:

- remove/replace hidden toggle semantics where they conflict with explicit-verb model,
- ensure unit-definition mutations route to edit workflow,
- keep runtime-operational overrides explicit and bounded.

Acceptance:

- no unauthorized toggle/twiddle path remains for protected settings,
- command and interactive behavior produce consistent state transitions.

### Phase 4: Telemetry Data Model and Collection Hooks

Deliverables:

- define telemetry fields and optionality,
- add non-blocking collection hooks for uptime/restarts/exit-reason/backoff ETA,
- add best-effort process metrics pipeline (optional per platform).

Acceptance:

- telemetry collection does not block UI/command dispatch,
- unavailable metrics are surfaced as missing/unknown without errors.

### Phase 5: CLI and Dashboard Telemetry Presentation

Deliverables:

- enrich CLI detail output (status/show-equivalent paths),
- enrich dashboard per-unit detail surfaces,
- add recent log-tail snippet in detail view with bounded size.

Acceptance:

- both interfaces expose the canonical telemetry set where available,
- output remains readable and deterministic.

### Phase 6: Stage-Model Comparative Review

Deliverables:

- produce stage-model analysis artifact (can be in README section or dedicated doc note),
- compare supervisor vs runit (and contextualize vs systemd targets),
- provide recommendation and rationale.

Acceptance:

- review section is explicit, concrete, and decision-oriented,
- recommendation is actionable and consistent with current architecture.

### Phase 7: Handbook Documentation Expansion

Deliverables:

- implement the required systemd-alignment + differences sections,
- add feature support matrix,
- add example conversions (systemd -> supervisor units),
- add stage/target/runit comparison tables.

Acceptance:

- documentation is accurate to current code behavior,
- all required tables/examples are present and internally consistent.

### Phase 8: Tests and Final Consistency Pass

Deliverables:

- add/adjust ERT tests for mutator semantics and CLI/dashboard parity,
- add/adjust tests for telemetry formatting/availability fallbacks,
- run full quality gate.

Acceptance:

- `make check` passes,
- regression coverage exists for identified parity risks,
- docs and behavior are aligned.

## Required Files (Expected)

- `supervisor-core.el`
- `supervisor-cli.el`
- `supervisor-dashboard.el`
- `supervisor-test.el`
- `README.org`
- optional analysis note file if stage-model comparison is split out

## Explicit Non-Goals

- no implementation of full systemd metric/journal semantics,
- no logging pipeline replacement in this plan,
- no wholesale stage-model rewrite unless explicitly approved later.

## Definition of Done

All must be true:

1. Remaining mutator paths are explicitly classified and normalized.
2. CLI and dashboard lifecycle/policy behaviors are semantically consistent.
3. Unit-definition changes are not mutated via hidden runtime toggles.
4. Per-unit telemetry is richer and available in both CLI and dashboard detail views.
5. Telemetry collection is non-blocking and graceful on unsupported metrics.
6. Stage-model comparative review is documented with recommendation.
7. README includes required parity/differences tables and examples.
8. `make check` passes.
