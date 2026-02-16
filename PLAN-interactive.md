# PLAN: Interactive Dashboard Timer Section Integration

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

## Objective
Integrate timer visibility and timer actions directly into `M-x supervisor` so
users can analyze service and timer state in one place without switching to
`sbin/supervisorctl list-timers`.

The dashboard must present timers as a dedicated second section below services,
with deterministic behavior for interactive actions and clear handling of
service-only commands that do not apply to timers.

This plan does not introduce timer visibility from zero.  Timers are already
visible today.  This plan upgrades layout, parity, and interaction correctness.

## Baseline Constraints
1. Service units and timer definitions are different object types.
2. Timer IDs are not unit IDs and do not have unit files.
3. Existing lifecycle and policy mutators are unit-centric.
4. Existing `list-timers` output is the canonical timer status surface.
5. Current timer rows in dashboard are informational and not command-complete.

## Concern Coverage Map
This section maps the raised concerns to mandatory plan outcomes.

1. Concern: timer analysis requires separate CLI call.
   Resolution:
   - timer section rendered as dedicated second section in dashboard,
   - timer columns aligned to `list-timers` semantics.
2. Concern: unclear which actions work on timers.
   Resolution:
   - explicit compatibility matrix,
   - stable rejection messaging for service-only commands on timer rows.
3. Concern: `?` menu needs useful timer interactions.
   Resolution:
   - dedicated timer submenu and transient group,
   - fixed timer command set (trigger, inspect, jump, reset, refresh).
4. Concern: timer objects are special and not unit files.
   Resolution:
   - lock `cat` and `edit` as unsupported for timer definitions in this plan,
   - document this in help and README.
5. Concern: gate and runtime edge cases are confusing.
   Resolution:
   - explicit timer section states for mode-off, empty, valid, invalid.
6. Concern: timer and service ID collisions can create wrong behavior.
   Resolution:
   - typed row identity model and collision tests are mandatory.

## Locked Decisions
1. Dashboard keeps a service section first and adds a timer section second.
2. Timer section is rendered below all service rows.
3. Timer section column contract matches `list-timers` semantics:
   - `ID`, `TARGET`, `ENABLED`, `LAST-RUN`, `NEXT-RUN`, `EXIT`, `MISS`.
4. Service rows keep existing dashboard column contract and behavior.
5. Service filters (`stage`, `tag`) apply only to service rows.
6. Timer section visibility is controlled only by `supervisor-dashboard-show-timers`.
7. When timer mode is off (`supervisor-timer-subsystem-mode` nil), timer section
   still renders with explicit disabled state text and no interactive timer
   mutators.
8. Timer rows are typed and must not be identified by raw string ID alone.
9. Row identity must prevent service/timer ID collision ambiguity.
10. Existing service lifecycle/policy subcommands remain service-only.
11. Timer actions are exposed through a dedicated timer submenu in the `?`
    transient menu and direct key prefix in dashboard mode.
12. Dashboard timer commands must not require `supervisorctl` shell execution.
13. Timer actions are additive and do not change service command semantics.
14. Timer definitions remain config-backed (no timer unit-file generation in this plan).
15. `cat` and `edit` remain unavailable for timer definitions in this plan.
16. `make check` is required at each phase gate.
17. Service health counters are rendered in the dashboard body, not in
    `header-line-format`.
18. Service health counters must not be rendered as the first content row.
19. Service health summary placement is fixed between services and timers.
20. Service health summary is surrounded by exactly two blank lines above and
    two blank lines below.
21. Service health summary is explicitly service-scoped and does not aggregate
    timer rows.

## Canonical UI Contract

### 1) Section Ordering
Rendering order is fixed:

1. Service section (existing behavior).
2. Two blank lines.
3. Service health summary block.
4. Two blank lines.
5. Timer section header.
6. Timer rows.
7. Invalid timer rows block.

### 2) Service Health Summary Block
Service summary block requirements are fixed:

1. It appears only in the buffer body.
2. It appears after all service rows and before any timer rows.
3. It must be labeled to indicate service scope.
4. It reports only service counts:
   - `run`, `done`, `pend`, `fail`, `inv`.
5. It must not include timer counts.
6. It must not be mirrored in header line.

### 3) Timer Section Content
Timer rows must expose these fields:

1. Timer ID.
2. Target service ID.
3. Enabled state.
4. Last run relative time.
5. Next run relative time.
6. Last exit code.
7. Last miss reason.

Invalid timers must include:

1. Timer ID.
2. Validation reason.
3. Non-actionable status marker.

### 4) Timer Section State Modes
Timer section must support and display these states:

1. Disabled mode gate.
2. No timers configured.
3. Timers configured and valid.
4. Timers configured with invalid definitions.

### 5) Row Kind Model
Dashboard row IDs must be structured:

1. Service row ID: `(:service . ID)`.
2. Timer row ID: `(:timer . ID)`.
3. Separator/summary rows remain symbol IDs.

All point-based action dispatch must branch on row kind first.

## Interactive Command Contract

### 1) New Timer Submenu
A new timer submenu is required in dashboard:

1. Direct key prefix: `y` (timer actions).
2. Included in `?` transient menu as group "Timers".

Timer submenu commands are fixed:

1. Trigger timer now.
2. Show timer details.
3. Jump to timer target service row.
4. Reset timer runtime state.
5. Refresh timer section.

### 2) Timer Command Semantics
Timer commands behave as follows:

1. Trigger now:
   - invokes timer trigger with reason `manual`,
   - rejects invalid timers,
   - rejects when timer mode gate is off.
2. Show timer details:
   - opens details including target, schedule fields, runtime timestamps,
     retry metadata, and last miss reason.
3. Jump to target:
   - moves point to matching service row when present,
   - emits explicit message when target row is absent.
4. Reset timer runtime state:
   - clears runtime fields for selected timer:
     `:last-run-at`, `:last-success-at`, `:last-failure-at`, `:last-exit`,
     `:retry-attempt`, `:retry-next-at`, `:last-missed-at`,
     `:last-miss-reason`, `:next-run-at`, `:startup-triggered`,
   - recomputes next run,
   - persists timer state file when persistence is enabled.
5. Refresh timers:
   - rebuilds timer projection and redraws section without changing service state.

## Compatibility Matrix

### 1) Dashboard Service Subcommands on Timer Rows
The following commands must reject timer rows with stable, actionable errors:

1. Lifecycle: stop, restart, kill, reload-unit, reset-failed.
2. Policy: enable, disable, mask, unmask, restart-policy, logging.
3. Inspect: cat, edit, service dependency graph commands.

Stable error format:

`Not available for timer rows: use timer actions (y or ? -> Timers)`

### 2) Dashboard Commands Allowed on Timer Rows
Allowed commands on timer rows are exactly:

1. Timer submenu commands.
2. Generic refresh and navigation commands.
3. Help commands.

### 3) CLI Compatibility Rules
`supervisorctl list-timers` remains canonical CLI timer listing.

Service-oriented CLI commands are not upgraded to timer semantics in this plan.
When a timer ID is passed to a service command, behavior must remain deterministic
and include a hint to use `list-timers` or dashboard timer actions.

No existing CLI command names are removed.

## Implementation Phases

### Phase 1: Row Kind Refactor and Data Projection
Deliverables:

1. Add typed row identity model for service and timer rows.
2. Add shared timer projection helper for dashboard rendering parity with
   `list-timers`.
3. Guarantee collision-safe row dispatch.

Acceptance:

1. Service ID and timer ID collisions do not misroute actions.
2. Dashboard refresh remains deterministic.
3. `make check` passes.

### Phase 2: Two-Section Dashboard Rendering
Deliverables:

1. Implement timer section rendering below services.
2. Implement timer state mode rendering (disabled, empty, valid, invalid).
3. Move service health summary from top/body-first position to between services
   and timers.
4. Ensure service health summary is not in header line.
5. Enforce exactly two blank lines above and below service summary block.
6. Keep service section rendering unchanged.

Acceptance:

1. Timer section always appears in second position when
   `supervisor-dashboard-show-timers` is non-nil.
2. Stage/tag filters do not hide timer section.
3. Timer columns match plan contract.
4. Service health summary is below services, above timers, and not in line 1.
5. Service health summary remains service-only.
6. `make check` passes.

### Phase 3: Timer Submenu and Command Routing
Deliverables:

1. Add `y` timer submenu dispatcher.
2. Add timer group to `?` transient menu.
3. Add timer command implementations from Interactive Command Contract.
4. Add stable service-only rejection messages on timer rows.

Acceptance:

1. Timer trigger, details, jump-target, reset-state, and refresh work from timer rows.
2. Service-only commands reject timer rows with stable message.
3. No service behavior regression.
4. `make check` passes.

### Phase 4: Help and Discoverability
Deliverables:

1. Update dashboard help buffer with timer section semantics and keymap.
2. Document timer command limitations and service-only boundaries in help text.
3. Update transient labels to make row-kind scope explicit.

Acceptance:

1. Help text includes timer commands and compatibility caveats.
2. New users can discover timer actions without external docs.
3. `make check` passes.

### Phase 5: Test Expansion (Hard Requirement)
Deliverables:

1. Add tests for typed row IDs and collision safety.
2. Add tests for timer section rendering in all state modes.
3. Add tests for each timer submenu command.
4. Add tests for stable rejection of service-only commands on timer rows.
5. Add tests for transient menu timer group presence and key bindings.
6. Add tests for disabled timer-mode behavior in dashboard section.
7. Add tests that service summary is not in header line.
8. Add tests that service summary placement is between services and timers.
9. Add tests for required blank-line spacing around service summary.
10. Add tests that service summary counts are service-only.

Acceptance:

1. New tests pass.
2. Existing tests pass.
3. `make check` passes.

### Phase 6: Documentation Update
Deliverables:

1. Update `README.org` dashboard and timer sections with two-section model.
2. Update command tables for new timer submenu and limitations.
3. Add operator notes comparing dashboard timer section to `list-timers`.

Acceptance:

1. README reflects implementation exactly.
2. No stale claims about timer rows being informational-only.
3. `make check` passes.

## Explicit Non-Goals
1. No timer unit-file format.
2. No timer policy override persistence model (`enable`/`disable` overrides).
3. No conversion of timers into full service-unit parity objects.
4. No redesign of timer schema keys.
5. No D-Bus activation work.
6. No target-system redesign work.

## Audit Evidence Requirements
Each phase audit must include:

1. Code evidence:
   - exact files changed,
   - exact functions added or modified.
2. Behavior evidence:
   - screenshot or textual capture showing two-section rendering,
   - screenshot or textual capture showing service-summary placement and
     blank-line spacing,
   - one timer-row interaction trace for each timer command,
   - one rejected service-only command on timer row.
3. Compatibility evidence:
   - collision case with same service ID and timer ID,
   - disabled timer-mode section behavior.
4. Validation evidence:
   - `make check` output with pass status.
