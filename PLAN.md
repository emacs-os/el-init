# PLAN: Systemctl-First Parity + Modular Unit Files (Planning Only)

Date: 2026-02-12
Based on: `FINDINGS.md` (current)
Status: Design and execution plan only. No implementation in this step.

## Goal
Move supervisor tooling toward `systemctl` 1:1 parity as much as practical, while preserving a small set of intentional supervisor-specific capabilities.

This plan also introduces a human-centered modular Elisp unit-file model to support `systemctl edit`-style workflows.

## Policy Baseline
- Default: prefer 1:1 `systemctl` naming and behavior.
- Keep supervisor-specific features only when they are deliberate value-add.
- Keep explicitly unplanned items unplanned unless direction changes.

## Explicit Scope Decisions

### Planned parity work
- `describe` -> `show`
- `graph` -> `list-dependencies`
- `timers` -> `list-timers`
- Add explicit interactive `stop` and `restart` actions
- Introduce true `reload UNIT...` semantics distinct from `reconcile`
- Introduce a supervisor-scoped `daemon-reload` equivalent
- Align `enable`/`disable` semantics closer to systemctl model
- Add modular unit files and `edit UNIT` workflow

### Planned preserve (supervisor-specific extensions)
- `reconcile`
- `validate`
- `restart-policy`
- `logging`
- file-based logs workflow (`logs`, dashboard `L`)

### Unplanned (as currently directed)
- `systemctl cat UNIT`
- `systemctl mask/unmask UNIT`
- `journalctl -u UNIT` parity (keep file-based logging model)
- `systemctl is-active`
- `systemctl is-enabled`
- `systemctl is-failed`
- `systemctl reset-failed`
- change to `version` behavior

Note: `systemctl edit UNIT` is now planned via the modular unit-file design below.

## Target End State

### CLI parity target
- Core lifecycle and introspection commands follow systemctl naming for equivalent behavior.
- Commands with non-equivalent behavior keep distinct names (`reconcile`, `validate`, policy toggles).
- Help text clearly differentiates systemctl-equivalent commands from supervisor-specific commands.

### Interactive parity target
- Dashboard provides direct equivalents for `start`, `stop`, `restart`, and `kill`.
- Dashboard includes direct edit action for a unit file and returns user to overview flow after save/close.

### Config model target
- Supervisor supports modular unit files (one file per unit/service) in addition to existing config.
- Unit-file loading is deterministic and validated.
- Edit workflow operates on first-class unit files instead of ad-hoc inline list edits.

## Workstreams

## 1) Command Surface Normalization
Deliverables:
- Rename CLI verbs:
  - `describe` -> `show`
  - `graph` -> `list-dependencies`
  - `timers` -> `list-timers`
- Update wrapper help, README, and tests for renamed commands.

Acceptance:
- Command usage/help reflects normalized names.
- Old names removed or retained only if explicitly approved.
- `make check` passes.

## 2) Interactive Command Parity
Deliverables:
- Add explicit dashboard actions for `stop` and `restart` (separate from kill).
- Keep kill behavior as signal-only.
- Update dashboard help/transient menu/docs.

Acceptance:
- Dashboard has direct, named actions matching CLI/systemctl mental model.
- No semantic collision between stop and kill.
- `make check` passes.

## 3) `reload` vs `reconcile` Split
Deliverables:
- Define and implement true `reload UNIT...` behavior (unit-level config reload semantics where supported).
- Keep `reconcile` as full-state convergence operation.
- Update docs with exact semantic boundary.

Acceptance:
- `reload` and `reconcile` are both present and non-overlapping.
- User can predict command outcome from name alone.

## 4) Supervisor-Scoped `daemon-reload`
Deliverables:
- Add a supervisor command equivalent to reloading unit definitions from disk.
- Rebuild in-memory plan/config model without forcing full reconcile unless requested.

Acceptance:
- Command exists and behavior is documented.
- Compatible with modular unit-file architecture.

## 5) `enable` / `disable` Semantic Alignment
Deliverables:
- Redesign semantics toward systemctl-like persistent unit enablement model.
- Clarify relationship between config defaults, overrides, and enabled state.
- Migrate existing override behavior safely.

Acceptance:
- `enable/disable` meaning is clear and stable.
- Docs and UI reflect one coherent model.

## 6) Modular Elisp Unit Files (Foundation)
Objective:
- Introduce first-class unit files to support human-friendly editing and clearer lifecycle ownership.

Proposed model:
- A configurable unit directory (example: `~/.config/supervisor/units/`).
- One unit per file, deterministic naming (`<unit-id>.el` or `<unit-id>.eld`).
- Each file defines one canonical unit form.
- Loader builds runtime plan from unit files (and optional legacy bridge from `supervisor-programs` during transition).

Human factors requirements:
- Predictable file locations.
- Readable structure with templates.
- Validation feedback at save/load time.
- Minimal hidden magic in loading order.

Acceptance:
- Unit files can be loaded deterministically.
- Validation errors map back to concrete files/lines.
- Existing users have a migration path.

## 7) `edit UNIT` Human-Centered Workflow
Objective:
- Provide a systemctl-like edit flow optimized for humans in Emacs.

CLI/interactive workflow target:
1. User runs `supervisorctl edit UNIT` (or dashboard edit action).
2. Supervisor resolves the unit file path; if missing, offers scaffold template.
3. Opens the unit buffer in Emacs.
4. On save, validate just that unit (and dependency references as needed).
5. On close, return user context to overview (dashboard/command flow), with clear next action hint (`daemon-reload` and/or `reconcile` as appropriate).

UX constraints:
- No surprising auto-apply without clear feedback.
- Clear error messaging with file/line context.
- Preserve user navigation context after editing.
- Fast path for repeated edit-test cycles.

Acceptance:
- `edit UNIT` is discoverable and predictable.
- Save/close/return flow works without context loss.
- Documentation includes practical examples.

## 8) Documentation and Migration
Deliverables:
- Update README command tables and examples.
- Add unit-file handbook section (layout, template, edit flow).
- Add compatibility notes for systemctl parity and intentional divergences.
- Add migration doc from monolithic config to modular units.

Acceptance:
- Docs match behavior exactly.
- No stale command references remain.

## 9) Testing and Quality Gates
Required:
- Extend ERT for renamed commands and removed legacy names.
- Add tests for interactive stop/restart actions.
- Add tests for reload vs reconcile split.
- Add tests for unit-file load, validation, and edit flow behavior contracts.
- `make check` must pass at each merged phase.

Acceptance:
- Green CI and local `make check`.
- No regressions in JSON contracts unless explicitly versioned.

## Execution Order
1. Command renames (`show`, `list-dependencies`, `list-timers`)
2. Interactive `stop`/`restart` parity
3. Unit-file foundation
4. `edit UNIT` workflow on top of unit files
5. `daemon-reload` implementation
6. `reload` semantic split from `reconcile`
7. `enable/disable` model alignment
8. Documentation and migration completion pass

## Risks and Mitigations
- Risk: semantic churn confuses users during transition.
  - Mitigation: sequence changes with tight docs/tests per phase.
- Risk: unit-file model creates migration friction.
  - Mitigation: include legacy bridge and migration tooling.
- Risk: edit flow becomes too Emacs-internal for CLI users.
  - Mitigation: define explicit CLI behavior and fallback messaging.

## Completion Criteria
This plan is complete when:
- Planned parity commands use systemctl-aligned names/behaviors.
- `edit UNIT` is implemented with modular unit files and human-centered flow.
- Dashboard and CLI command semantics remain consistent.
- Unplanned items remain explicitly out of scope.
- `make check` passes across all phases.

## Open Design Questions (to resolve before implementation)
1. Unit-file format canonical form:
- pure plist expression vs helper macro wrapper.

2. Unit-file location policy:
- single directory vs layered directories (system/user/local precedence).

3. Edit return behavior:
- always return to dashboard buffer when invoked from dashboard,
- and what CLI should do when no interactive frame is available.

4. Reload application semantics:
- whether `reload UNIT` should auto-apply to live process immediately,
- or stage changes until `reconcile`.
