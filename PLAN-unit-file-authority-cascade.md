# PLAN: Cascading Unit-File Authority (Systemd-Style)

Date: 2026-02-12
Status: Draft for implementation

## Scope

This plan defines a hierarchical unit-file authority model with deterministic
cascading overrides, similar to systemd load-path precedence.

Target outcome:

- support tiered unit directories,
- load units from existing tiers only,
- resolve conflicts by higher-tier authority,
- make tier path/ordering user-configurable via a single list variable.

This plan does not add new service semantics beyond source/precedence behavior.

## Locked Decisions

1. Canonical directory basename is `supervisor.el/`.

2. Authority roots are configured by a user-settable ordered list variable
   (low -> high precedence), e.g. `supervisor-unit-authority-path`.

3. Shipped defaults for that list are:
- Tier 1: vendor units: `/usr/lib/supervisor.el/`
- Tier 2: system admin units: `/etc/supervisor.el/`
- Tier 3: user units: `~/.config/supervisor.el/` (or XDG-resolved equivalent)

4. Users may add/reorder/remove tiers by editing the list directly (e.g. add
   a Tier 4 path such as `~/.emacs.d/superfoo/` as highest precedence).

5. Missing directories are silently skipped.

6. Override model is full-unit replacement by ID:
- higher tier completely overrides lower-tier unit with same ID,
- no key-level merge across tiers.

7. "True authority" behavior on invalid override:
- if highest-authority candidate for an ID is invalid, that ID is invalid,
- do not fall back to lower-tier definition for that ID.

8. Drop-in directories are explicitly unplanned:
- no `unit.d/*.conf`-style layering,
- operators edit full unit files directly via existing `edit` workflow,
- apply changes with `daemon-reload` (and runtime action commands as needed).

9. `edit UNIT` must be authority-aware:
- if UNIT exists in multiple tiers, open the winning highest-authority file,
- never open a shadowed lower-tier file for edit by default.

## Canonical Resolution Contract

### A) Root selection

- Resolve active roots from `supervisor-unit-authority-path` in listed order
  (low -> high precedence).
- Default value for that list is Tier 1 -> Tier 2 -> Tier 3.
- User customizations may add/remove/reorder tiers; resolver must honor the
  configured order exactly.
- Non-existent roots are skipped.

### A.1) Declarative and atomic resolution model

- Resolution is declarative:
  - effective unit set is a pure function of active roots + on-disk unit files.
- Resolution publication is atomic:
  - loader computes full candidate set, validates, resolves precedence, then
    publishes one complete effective snapshot in a single commit point.
- Runtime consumers (core/CLI/dashboard/timer) must observe a consistent
  snapshot, never a partially updated intermediate state.
- `daemon-reload` and equivalent refresh paths must use the same atomic
  snapshot pipeline.

### B) Unit discovery

- Scan active roots for `*.el` files.
- Parse/validate unit files in deterministic lexical order per root.
- Track origin metadata: root, tier, file path.

### C) ID authority and conflict handling

- Build a map `id -> winning-candidate` by tier precedence.
- Effective selection is highest-tier authoritative per ID.
- Implementation may scan low->high (overwrite) or high->low (first hit),
  but resolved winner must always be the highest available tier for that ID.
- A lower-tier unit must never win when the same ID exists in a higher tier.
- Same-tier duplicate ID is deterministic error policy:
  keep first-seen candidate by deterministic lexical scan order, and mark each
  later duplicate as invalid with explicit duplicate-authority reason.

### D) Invalid precedence semantics

- Highest-tier candidate claims authority for that ID even when invalid.
- Lower-tier versions for same ID are ignored (blocked by authority).

### E) Observability

- Expose enough metadata for diagnostics (log/debug/CLI/dashboard future display):
  - winning source path,
  - overridden source paths,
  - invalid reason with source path.

### F) Edit target selection

- `edit UNIT` resolves the effective authoritative candidate first.
- If UNIT exists, open the winning file path only.
- If UNIT does not exist in any active tier:
  - create in the highest writable configured tier (highest precedence writable root).
- UI/CLI messaging should state the chosen path to avoid ambiguity.

## Phase Plan (8 Phases)

### Phase 1: Path Model and API Surface

Deliverables:

- add path-resolution API in units module:
  - compute active roots,
  - resolve default roots,
  - honor configured root ordering.
- add user-facing list variable for authority roots with documented defaults.

Acceptance:

- root resolution is deterministic,
- configured root ordering behavior is explicit and tested.

### Phase 2: Authority Resolver Design

Deliverables:

- define winner-selection algorithm by tier and ID,
- define same-tier duplicate-ID policy with deterministic behavior,
- define invalid-winner blocking behavior.

Acceptance:

- resolver behavior documented and codified in tests,
- no ambiguity in tie/duplicate handling.

### Phase 3: Loader Refactor in `supervisor-units.el`

Deliverables:

- refactor unit discovery/loading to use authority resolver,
- preserve existing parse/validation flow,
- store source metadata for winning and shadowed candidates.
- implement snapshot-style load pipeline with single atomic publish step.

Acceptance:

- loaded effective unit set reflects tier precedence,
- invalid authoritative unit blocks lower fallback for same ID.
- no consumer can observe a partially resolved authority state.

### Phase 4: Core Integration Consistency

Deliverables:

- ensure `supervisor--effective-programs` consumes resolved authority output only,
- verify core/CLI/dashboard/timer continue consuming one unified effective set,
- ensure no legacy merge paths reappear.
- ensure all consumers read from the same published snapshot instance.

Acceptance:

- all runtime consumers observe same winning unit set,
- startup/status/daemon-reload consistency maintained.
- refresh/reload paths are atomic from consumer perspective.

### Phase 5: CLI/Diagnostics Enhancements (Minimal)

Deliverables:

- add optional visibility hooks for source path/authority in diagnostics/logging,
- make `edit UNIT` report selected authority tier/path before launching editor,
- update user-facing migration/help text that references
  `supervisor-unit-directory` to authority-root terminology and behavior,
- keep default output stable unless explicitly requested.

Acceptance:

- operators can identify which tier/file won for an ID when debugging,
- `edit UNIT` consistently opens the authoritative file.

### Phase 6: Test Matrix Expansion

Deliverables:

- add ERT helper(s) for multi-tier temp directory fixtures,
- add tests for:
  - tier precedence,
  - missing-dir skip,
  - user-configured root list precedence,
  - invalid winner blocks fallback,
  - same-tier duplicate behavior.

Acceptance:

- precedence matrix fully covered,
- tests deterministic across Emacs versions.

### Phase 7: Documentation Updates

Deliverables:

- update `README.org` with:
  - tier hierarchy,
  - precedence rules,
  - configurable root-list semantics,
  - examples of override behavior.

Acceptance:

- docs match implemented behavior exactly,
- examples include at least one override and one invalid-authority case.

### Phase 8: Final Validation and Cleanup

Deliverables:

- run full quality gate,
- ensure stale legacy wording is removed from affected docs/comments.

Acceptance:

- `make check` passes,
- no contradictory docs about unit source precedence remain.

## Required Files (Expected)

- `supervisor-units.el`
- `supervisor-core.el`
- `supervisor-cli.el` (authority-aware `cat`/`edit` target resolution)
- `supervisor-dashboard.el` (authority-aware `cat`/`edit` target resolution)
- `supervisor-test.el`
- `README.org`

## Explicit Non-Goals

- no drop-in directory semantics in this plan,
- no key-level merge of unit fields across tiers,
- no system-wide init/root-mode behavior changes beyond path precedence.

## Definition of Done

All must be true:

1. Tiered authority model is implemented and deterministic.
2. Default roots follow Tier 1 -> Tier 2 -> Tier 3 precedence.
3. Authority root list is user-configurable and ordering is honored exactly.
4. Higher-tier unit fully overrides lower-tier unit with same ID.
5. Invalid highest-tier unit blocks lower-tier fallback for that ID.
6. Runtime consumers use one shared resolved effective unit set.
7. Authority resolution and publish are declarative and atomic.
8. Tests cover precedence and invalid-authority edge cases.
9. `edit UNIT` always targets the authoritative file path.
10. Documentation reflects final behavior.
11. `make check` passes.
