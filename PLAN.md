# PLAN: Systemctl-First Parity + Modular Unit Files (Planning Only)

Date: 2026-02-12
Based on: `FINDINGS.md` (current)
Status: Phased execution plan; phase sections track implementation status.

## Goal
Move supervisor tooling toward `systemctl` 1:1 parity as much as practical, while preserving a small set of intentional supervisor-specific capabilities.

This plan also introduces a human-centered modular Elisp unit-file model to support `systemctl edit`-style workflows.

## Policy Baseline
- Default: prefer 1:1 `systemctl` naming and behavior.
- Hard ABI break policy: do not preserve legacy command names/aliases unless explicitly requested.
- This project has no legacy compatibility constraints; establish a clean base now.
- Keep supervisor-specific features only when they are deliberate value-add.
- Keep explicitly unplanned items unplanned unless direction changes.

## Explicit Scope Decisions

### Planned parity work
- Reassign current `status` overview output to `list-units`
- Add true detailed `status UNIT...` behavior
- `describe` -> `show`
- `graph` -> `list-dependencies`
- `timers` -> `list-timers`
- Add explicit interactive `stop` and `restart` actions
- Introduce true `reload UNIT...` semantics distinct from `reconcile`
- Introduce a supervisor-scoped `daemon-reload` equivalent
- Align `enable`/`disable` semantics closer to systemctl model
- Add modular unit files and `edit UNIT` workflow
- Add `cat UNIT` parity for unit-file display
- Add `mask/unmask UNIT` parity
- Add `is-active`, `is-enabled`, `is-failed` parity
- Keep `version` parity explicitly verified (already functionally aligned)

### Planned preserve (supervisor-specific extensions)
- `reconcile`
- `validate`
- `restart-policy`
- `logging`
- file-based logs workflow (`logs`, dashboard `L`)

### Unplanned (as currently directed)
- `journalctl -u UNIT` parity (keep file-based logging model)
- `systemctl reset-failed`

Note: `systemctl edit UNIT` and `systemctl cat UNIT` are planned via the modular
unit-file design below.

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
- Re-scope status commands:
  - make current overview table canonical under `list-units`
  - implement true detailed `status UNIT...` output
- Rename CLI verbs:
  - `describe` -> `show`
  - `graph` -> `list-dependencies`
  - `timers` -> `list-timers`
- Update wrapper help, README, and tests for renamed commands.

Acceptance:
- Command usage/help reflects normalized names.
- `status` provides detailed unit view semantics, while `list-units` is overview/list semantics.
- Old names are removed by default (hard ABI break; no compatibility aliases).
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
- Single directory policy for phase 1 (no layered precedence in initial implementation).
- One unit per file, deterministic naming (`<unit-id>.el`).
- Each file defines one canonical pure plist form (data-first, no helper macro wrapper).
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
3. Opens editor based on context:
- dashboard/interactive context: open unit file buffer in Emacs.
- non-interactive CLI context: launch `$VISUAL` or `$EDITOR` against unit file path (do not assume `emacsclient`).
4. On save, validate just that unit (and dependency references as needed).
5. On close:
- if invoked from dashboard, return focus to dashboard overview.
- otherwise, print clear next-step hints (`daemon-reload`, `reload`, `reconcile`) in CLI output.

UX constraints:
- No surprising auto-apply without clear feedback.
- Clear error messaging with file/line context.
- Preserve user navigation context after editing.
- Fast path for repeated edit-test cycles.

Acceptance:
- `edit UNIT` is discoverable and predictable.
- Save/close/return flow works without context loss.
- Documentation includes practical examples.

## 8) Additional Systemctl Parity Commands
Deliverables:
- Implement `cat UNIT` as raw unit-file content output in CLI (literal file content).
- Implement interactive/unit-inspection equivalent by opening unit file read-only in buffer.
- Implement `mask UNIT` and `unmask UNIT` semantics and persistence model.
- Implement mask model as persistent file-state with precedence:
- `mask` > `enable/disable` > manual start.
- Implement `is-active`, `is-enabled`, `is-failed` with strict systemctl-compatible exit-code behavior.
- Verify `version` output/exit behavior remains systemctl-comparable.

Acceptance:
- `cat`, `mask/unmask`, and `is-*` are present and documented.
- Exit codes for `is-*` are deterministic for automation use.
- Mask state precedence is enforced exactly as specified.

## 9) Documentation and Migration
Deliverables:
- Update README command tables and examples.
- Add unit-file handbook section (layout, template, edit flow).
- Add compatibility notes for systemctl parity and intentional divergences.
- Add migration doc from monolithic config to modular units.

Acceptance:
- Docs match behavior exactly.
- No stale command references remain.

## 10) Testing and Quality Gates
Required:
- Extend ERT for renamed commands and removed legacy names.
- Add tests for interactive stop/restart actions.
- Add tests for reload vs reconcile split.
- Add tests for unit-file load, validation, and edit flow behavior contracts.
- Add tests for `cat`, `mask/unmask`, and `is-*` behaviors/exit codes.
- `make check` must pass at each merged phase.

Acceptance:
- Green CI and local `make check`.
- No regressions in JSON contracts unless explicitly versioned.

## Execution Order
1. CLI command surface normalization (`status`/`list-units` split + renames).
2. Interactive `stop`/`restart` parity.
3. Unit-file foundation.
4. `edit UNIT` + `cat UNIT` workflows on top of unit files.
5. `mask/unmask` model and command implementation.
6. `is-active` / `is-enabled` / `is-failed` parity commands.
7. `daemon-reload` implementation.
8. `reload` semantic split from `reconcile`.
9. `enable/disable` model alignment.
10. Documentation, migration, and final quality gate.

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
- `status` and `list-units` semantics are clearly separated and systemctl-comparable.
- `edit UNIT` and `cat UNIT` are implemented with modular unit files and human-centered flow.
- Dashboard and CLI command semantics remain consistent.
- Only explicitly unplanned items remain out of scope (`journalctl -u` parity and `reset-failed`).
- ABI remains intentionally clean: no legacy compatibility aliases unless explicitly approved.
- `make check` passes across all phases.

## Locked Design Decisions
1. Unit-file canonical form:
- pure plist expression (no helper macro wrapper).

2. Unit-file location policy:
- single directory in phase 1 (`~/.config/supervisor/units/`).

3. Edit behavior:
- from dashboard: return to dashboard buffer after close.
- from CLI without interactive frame: use `$VISUAL` or `$EDITOR` (no `emacsclient` assumption).

4. Reload semantics:
- `reload UNIT` auto-applies to live units immediately.
- `reconcile` remains a broader convergence operation.

5. Mask model:
- persistent file-state mask.
- precedence: `mask` > `enable/disable` > manual start.

6. `is-*` contract:
- use strict systemctl-compatible exit-code behavior.

7. ABI stance:
- hard break by default; no legacy aliases unless explicitly approved.

8. `cat` behavior:
- CLI: literal/raw unit file content.
- interactive: open unit file as read-only buffer for inspection.

---

## Implementation Phases (Detailed)

Each phase is a single commit. `make check` must pass at each phase boundary.

### Phase 1: CLI Command Surface Normalization

**Goal:** Rename CLI commands to systemctl-aligned names and split `status` semantics.
Spec mapping: Workstream `1) Command Surface Normalization`.
Status rule: mark complete only after CLI help, tests, and README command
references are all updated.

**Files changed:**
- `supervisor-cli.el`: Dispatch table, command handlers, usage/help text
- `sbin/supervisorctl`: Wrapper help comments
- `supervisor-test.el`: All tests referencing old command names

**Specific changes:**
- `status` (no args) delegates to `list-units` (overview table).
- `status ID...` shows detailed per-unit output (describe format); prints found
  units and appends "Unit X could not be found." for missing IDs; non-zero exit
  when any ID is missing.  JSON includes `entries` + `not_found` arrays.
- `list-units [ID...]` is the canonical overview table command.  When IDs are
  specified, both valid and invalid rows are filtered consistently.
- `describe` → `show` (single unit properties).
- `graph` → `list-dependencies`.
- `timers` → `list-timers`.
- `list` (old alias) removed.
- Old names return "Unknown command" (hard ABI break).
- Help text categorized into systemctl-compatible vs supervisor-specific sections.
- README command references for renamed CLI commands are updated in this phase
  (minimum requirement for Phase 1 completion; deeper handbook expansion remains
  in Phase 10).

**Tests added/updated:** 12 renamed, 10 new (old-name rejection, new-behavior
coverage, partial-missing, invalid-filtering).

### Phase 2: Interactive Stop/Restart Parity

**Goal:** Dashboard gets explicit `stop` and `restart` actions, distinct from `kill`.
Spec mapping: Workstream `2) Interactive Command Parity`.

**Files changed:**
- `supervisor-dashboard.el`: Keymap, interactive commands, transient menu, help text
- `supervisor-test.el`: New tests for dashboard stop/restart

**Specific changes:**
- Add `supervisor-dashboard-stop` bound to `x` (mnemonic: e**x**it).
  - Calls `supervisor--manual-stop` on entry at point.
  - Confirms before stopping.  Sets `manually-stopped` flag (suppresses restart).
  - Rejects action on separator rows, timer rows, oneshot rows (stop is no-op
    for oneshots that already exited).
- Add `supervisor-dashboard-restart` bound to `R`.
  - Calls `supervisor--manual-stop` then `supervisor--manual-start` sequentially.
  - Only for `simple` entries that are currently running.
  - Confirms before restarting.
- Keep `k`/`K` (kill) as signal-only (no restart suppression, no stop semantics).
- Update transient menu: add "Stop" and "Restart" to "Entry Actions" group.
- Update `supervisor-dashboard-help` help buffer text with new keys.
- Update header hints line to include new keys.

**Semantic boundaries:**
- `stop` = graceful lifecycle stop + suppress restart.
- `restart` = stop + start (lifecycle operation).
- `kill` = send signal only (no lifecycle state change).

### Phase 3: Unit-File Foundation

**Goal:** Introduce `supervisor-units.el` module for modular one-file-per-unit config.
Spec mapping: Workstream `6) Modular Elisp Unit Files (Foundation)`.

**Files changed:**
- `supervisor-units.el` (new module)
- `supervisor-core.el`: Add unit-file integration point in plan building
- `supervisor.el`: Load new module
- `Makefile`: Add module to compile/lint/test targets
- `supervisor-test.el`: Unit-file loading/validation tests

**New module `supervisor-units.el` provides:**
- `supervisor-unit-directory` defcustom (default `~/.config/supervisor/units/`).
- `supervisor--unit-file-path` — resolve `<id>.el` path in unit directory.
- `supervisor--load-unit-file` — read and parse a single unit file.
  - File contains a single plist expression:
    ```elisp
    (:id "nm-applet"
     :command "nm-applet"
     :type simple
     :stage stage3
     :restart t)
    ```
  - Returns parsed plist or signals error with file context (path + line).
- `supervisor--load-all-unit-files` — scan directory, load each `*.el` file,
  return list of plists.  Deterministic alphabetical order.
- `supervisor--unit-file-to-program-entry` — convert plist to the
  `supervisor-programs` entry format `("cmd" :id "x" ...)`.
- `supervisor--load-programs` — unified loader: merges unit-file entries with
  `supervisor-programs` (legacy bridge).  Unit-file entries take precedence on
  ID collision.  Emits warnings for collisions.
- Validation at load time: unknown keys rejected, file path included in error
  messages.

**Integration with core:**
- `supervisor--build-plan` gains an option to call `supervisor--load-programs`
  instead of reading `supervisor-programs` directly.
- New variable `supervisor-use-unit-files` (default nil) gates the behavior.
  When nil, pure legacy path.  When non-nil, unit files are loaded and merged.

**Tests:** Unit-file parsing, validation with file context, directory scanning,
merge/collision semantics, missing directory handling.

### Phase 4: `edit UNIT` + `cat UNIT` Workflows

**Goal:** Implement `edit` and `cat` commands in CLI and dashboard.
Spec mapping: Workstreams `7) edit UNIT Human-Centered Workflow` and
`8) Additional Systemctl Parity Commands` (`cat` portion).

**Files changed:**
- `supervisor-units.el`: Template scaffolding, file resolution
- `supervisor-cli.el`: `edit` and `cat` command handlers + dispatch entries
- `supervisor-dashboard.el`: Dashboard edit action (new key)
- `sbin/supervisorctl`: Help text update
- `supervisor-test.el`: New tests

**`cat UNIT` (CLI):**
- Resolve unit file path via `supervisor--unit-file-path`.
- If file exists, output literal raw file content.
- If file does not exist, return an error (`unit file not found`).
- JSON mode (if retained) wraps raw file content only (`path` + `content`);
  no synthesized content from legacy in-memory config.

**`cat UNIT` (interactive):**
- `supervisor-dashboard-cat` bound to `c`.
- Opens unit file read-only in `view-mode`.  Returns to dashboard on `q`.

**`edit UNIT` (CLI):**
- Resolve unit file path.  If missing, create scaffold from template.
- Template populated with `:id`, `:command` placeholder, common keys commented.
- Non-interactive context: launch `$VISUAL` first, then `$EDITOR` as fallback.
- Do not assume `emacsclient` for CLI edit behavior.
- If neither `$VISUAL` nor `$EDITOR` is set, return an actionable error with the
  unit file path.
- JSON mode: return `{"path":..., "created": true/false}`.

**`edit UNIT` (interactive/dashboard):**
- `supervisor-dashboard-edit` bound to `E`.
- Opens unit file buffer with `emacs-lisp-mode`.
- If file missing, scaffold template into new buffer visiting the path.
- On save, run `supervisor--validate-unit-file` and report errors.
- On `kill-buffer` or `q`, return to `*supervisor*` dashboard buffer if it
  exists via `pop-to-buffer`.

**Tests:** cat existing/missing, edit scaffold creation, template content,
validation on save hook.

### Phase 5: `mask/unmask` Model and Commands

**Goal:** Implement persistent mask state with strict precedence.
Spec mapping: Workstream `8) Additional Systemctl Parity Commands` (`mask/unmask`).

**Files changed:**
- `supervisor-core.el`: Mask state hash table, precedence logic, persistence
- `supervisor-cli.el`: `mask` and `unmask` command handlers
- `supervisor-dashboard.el`: Dashboard mask toggle action
- `supervisor-test.el`: Mask precedence tests, CLI tests

**Mask model:**
- New hash table `supervisor--mask-override` (id → `'masked` or nil).
- Persisted in `supervisor-overrides-file` alongside enabled/restart/logging.
- Precedence chain for effective enabled: `masked?` → always disabled, skip
  everything.  `enabled-override?` → use override.  Else → config default.
- Masked entries: dashboard shows status `masked`, reason `masked`.
  CLI `status` and `list-units` show `masked` status.
- `supervisor--manual-start` on masked entry returns `(:status skipped :reason "masked")`.
- `reconcile` treats masked entries as disabled (stops if running).

**CLI commands:**
- `mask [--] ID...` — set mask in hash + persist.
- `unmask [--] ID...` — remove mask + persist.
- Both support `--` separator for hyphen-prefixed IDs.
- JSON output: `{"masked": ["id1", "id2"]}` / `{"unmasked": [...]}`.

**Dashboard:**
- `supervisor-dashboard-toggle-mask` bound to `m`.
- Cycles: unmasked → masked → unmasked.  Persists immediately.
- Masked rows show `masked` in status column with `supervisor-status-dead` face.

**Tests:** Mask precedence over enable/disable, mask blocks manual-start,
reconcile stops masked running entries, persistence round-trip, CLI arg handling.

### Phase 6: `is-active`, `is-enabled`, `is-failed` Commands

**Goal:** Predicate commands with strict systemctl-compatible exit codes.
Spec mapping: Workstream `8) Additional Systemctl Parity Commands` (`is-*` portion),
Locked Design Decision `6. is-* contract`.

**Files changed:**
- `supervisor-cli.el`: Three new command handlers
- `sbin/supervisorctl`: Help text
- `supervisor-test.el`: Exit code tests

**Contract (all `is-*` commands):**
- Use exact `systemctl` success/failure and unknown-unit exit semantics for each
  predicate command; do not invent supervisor-only numeric codes.
- Require exactly one ID argument.
- Human output mirrors systemctl-style predicate text closely enough for operator
  expectations; JSON remains optional but stable when enabled.

**`is-active ID`:**
- Report active vs non-active using systemctl-compatible status classification.
- Output includes the current status string.

**`is-enabled ID`:**
- Report enabled state with mask precedence respected.
- Output reflects enabled/disabled/masked style states.

**`is-failed ID`:**
- Report failed vs non-failed based on supervisor failed/crash state model.
- Output includes the current status string.

**Tests:** Each predicate with active/inactive/enabled/disabled/masked/failed
states, unknown ID handling, extra-args rejection, and exact exit-code parity
checks against the documented systemctl contract.

### Phase 7: `daemon-reload` Implementation

**Goal:** Reload unit definitions from disk into memory without reconciling runtime.
Spec mapping: Workstream `4) Supervisor-Scoped daemon-reload`.

**Files changed:**
- `supervisor-core.el`: New `supervisor-daemon-reload` command
- `supervisor-cli.el`: `daemon-reload` command handler
- `supervisor-dashboard.el`: Optional keybinding or transient entry
- `supervisor-test.el`: Tests

**Semantics:**
- `supervisor-daemon-reload` re-reads `supervisor-programs` and unit files (when
  `supervisor-use-unit-files` is non-nil).
- Rebuilds the internal plan (`supervisor--build-plan`) and stores the result.
- Does NOT start/stop/restart anything.  Runtime state is untouched.
- After `daemon-reload`, the next `reconcile`, `start`, or `reload` operates on
  the refreshed plan.
- Useful after editing unit files or changing `supervisor-programs`.

**CLI:**
- `daemon-reload` — no arguments.  Returns success message.
- JSON: `{"reloaded": true, "entries": N, "invalid": N}`.

**Dashboard:**
- Not bound to a key by default (infrequent operation).
- Available in transient menu under "System" group.

**Core implementation:**
- New variable `supervisor--current-plan` holds the last-built plan.
- `supervisor-daemon-reload` calls `supervisor--load-programs` (if unit files
  enabled) or reads `supervisor-programs`, then `supervisor--build-plan`, then
  stores result in `supervisor--current-plan`.
- Other commands that currently call `supervisor--build-plan` inline can
  optionally use `supervisor--current-plan` when available.

**Tests:** Reload picks up config changes, runtime state untouched, invalid
entries surfaced in plan.

### Phase 8: `reload` Semantic Split from `reconcile`

**Goal:** `reload UNIT...` hot-reloads specific units; `reconcile` stays global.
Spec mapping: Workstream `3) reload vs reconcile Split`.

**Files changed:**
- `supervisor-core.el`: New `supervisor--reload-unit` function
- `supervisor-cli.el`: `reload` command handler
- `supervisor-dashboard.el`: Optional transient entry
- `supervisor-test.el`: Tests

**`reload UNIT...` semantics (per locked design decision):**
- For each ID:
  1. Re-read the unit's config (from unit file if exists, else from
     `supervisor-programs`).
  2. Rebuild the parsed entry.
  3. If the entry is currently running (`simple`):
     - Stop the old process (graceful).
     - Start the new definition.
  4. If the entry is not running: update the stored definition only (next start
     uses new config).
  5. If the entry is masked: skip with warning.
- Auto-applies immediately (per locked decision).
- Does NOT affect other units (no global convergence).

**`reconcile` stays unchanged:**
- Full plan rebuild.
- Global convergence: stop removed/disabled, start new/re-enabled.

**CLI:**
- `reload [--] ID...` — requires at least one ID.
- Output: per-ID result lines (`reloaded`, `updated`, `skipped (masked)`).
- JSON: `{"results": [{"id": "x", "action": "reloaded"}, ...]}`.

**Tests:** Reload running unit restarts it, reload stopped unit updates config,
reload masked unit skips, reload unknown ID errors.

### Phase 9: `enable/disable` Model Alignment

**Goal:** Make enable/disable semantics clearer and closer to systemctl model.
Spec mapping: Workstream `5) enable/disable Semantic Alignment`.

**Files changed:**
- `supervisor-core.el`: Clarify enable/disable state model
- `supervisor-cli.el`: Possibly adjust enable/disable output
- `supervisor-dashboard.el`: Possibly adjust toggle behavior
- `supervisor-test.el`: Updated tests

**Current model (override-based):**
- Config says `:enabled t` or `:disabled t`.
- Runtime override in `supervisor--enabled-override` (can flip the default).
- Effective = override if present, else config default.

**Aligned model:**
- `enable ID` means: "this unit should start on next supervisor-start/reconcile."
  Persisted as override.  Matches systemctl "unit is wanted."
- `disable ID` means: "this unit should NOT start automatically."  Persisted.
- `start ID` on a disabled unit: starts it this session only (does not change
  enabled state).  This matches `systemctl start` on a disabled unit.
- Reconcile skips disabled units (does not stop manually-started disabled units
  that are still running — only stops units removed from config or newly disabled
  since last reconcile).

**Key clarification (may already be current behavior — audit needed):**
- Verify `start` on disabled unit works without changing enabled override.
- Verify `reconcile` does not kill a disabled unit that was manually started.
- If behavior already matches, this phase is documentation + test hardening only.
- If behavior diverges, adjust core logic.

**Deliverables:**
- Audit current enable/disable/start interaction.
- Fix any divergences from the aligned model above.
- Add explicit tests for: start-disabled-unit-works, reconcile-keeps-manually-started,
  enable-persists, disable-persists.
- Update CLI help and README to document the exact model.

### Phase 10: Documentation, Migration, and Final Quality Gate

**Goal:** Close all documentation gaps and enforce final testing/quality gates.
Spec mapping: Workstreams `9) Documentation and Migration` and
`10) Testing and Quality Gates`.

**Files changed:**
- `README.org`: Full update
- `CLAUDE.md`: Update command references, module list
- `FINDINGS.md`: Mark resolved items, update comparison table
- `supervisor-test.el`: Final parity/contract hardening tests

**README.org updates:**
- CLI Commands section: replace old command names with new ones.
  - `status [ID...]` with dual behavior.
  - `list-units` replaces old `status`/`list` overview.
  - `show` replaces `describe`.
  - `list-dependencies` replaces `graph`.
  - `list-timers` replaces `timers`.
  - Add `edit`, `cat`, `mask`, `unmask`, `is-active`, `is-enabled`, `is-failed`,
    `daemon-reload`, `reload`.
- Dashboard Keymap table: add `x` (stop), `R` (restart), `m` (mask), `c` (cat),
  `E` (edit).
- Service Definition section: add unit-file subsection with layout, template
  example, and `supervisor-unit-directory` documentation.
- Customization Reference: add new defcustom entries.
- Quick Start: add note about unit-file alternative.
- Indexes: add new topical entries.

**CLAUDE.md updates:**
- Module Structure table: add `supervisor-units.el`.
- Load order: add units module.
- Architecture section: add unit-file loading.
- Development Commands: unchanged (make check still the gate).

**FINDINGS.md updates:**
- Update comparison table: mark implemented items.
- Update divergence summary.
- Add note about intentional divergences (reconcile, validate, etc.).

**Migration documentation:**
- Add section to README or separate file explaining:
  - How to convert `supervisor-programs` entries to unit files.
  - `supervisor-migrate-config` command (if it exists) or manual steps.
  - How to enable unit-file loading (`supervisor-use-unit-files`).
  - Coexistence: legacy `supervisor-programs` + unit files merge behavior.

**Final quality gate:**
- Verify each prior phase acceptance criterion is satisfied with no carryover gaps.
- Ensure tests exist for renamed commands, removed legacy names, interactive
  stop/restart, reload vs reconcile split, unit-file load/validation/edit, `cat`,
  `mask/unmask`, and `is-*` exit contracts.
- Run `make check` as the release gate for Phase 10 completion.
