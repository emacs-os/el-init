# PLAN 01: Service Definition Layer

Roadmap reference: `ROADMAP.md` item 1.

## Objective

Implement a formal, versioned service-definition layer with:

- explicit schema
- persistent runtime overrides
- migration path from existing `supervisor-programs`
- complete validation coverage

This plan is implementation-facing and intentionally concrete.

## Scope

In scope:

- schema design + parser/validator
- dependency model split (ordering vs requirement)
- override persistence format and lifecycle
- migration tooling and compatibility behavior
- tests and docs

Out of scope:

- CLI transport and command UX (covered by Plan 02)
- file modularization split (covered by roadmap item 3)
- PID 1 behavior

## Current Baseline

Current config is `supervisor-programs` with plist keywords and normalized
internal tuples. Runtime overrides live in memory only.

## Target Design

1. Schema v1 with explicit version constant and structured service records.
2. Dependency model split:
- ordering dependencies (start order only)
- requirement dependencies (pull-in/availability semantics)
3. Persistent override store for enabled/restart/logging policies.
4. Backward compatibility with existing `supervisor-programs`.

## Phases

### Phase 1: Schema Contract

Deliverables:

- Define schema v1 document and canonical field map.
- Add version constant (for example `supervisor-service-schema-version`).
- Define parser output contract independent from legacy tuple indexing.

Acceptance criteria:

- Schema fields are fully documented with required/optional/default rules.
- Legacy entry forms map unambiguously into schema v1.

### Phase 2: Dependency Semantics

Deliverables:

- Add explicit ordering dependency fields.
- Add explicit requirement dependency fields.
- Update planning logic to validate and store both models separately.

Acceptance criteria:

- Cross-stage and missing dependency errors are explicit and test-covered.
- Scheduling remains deterministic for identical input.

### Phase 3: Persistent Overrides

Deliverables:

- Introduce persistent overrides file (single source of truth for runtime overrides).
- Implement load, merge, save, and clear semantics.
- Use atomic write pattern (temp file + rename).

Acceptance criteria:

- Overrides survive Emacs restart.
- Corrupt override file handling is safe and non-destructive.
- In-memory and persisted override states stay consistent.

### Phase 4: Migration Layer

Deliverables:

- Migration function from current `supervisor-programs` entries to schema v1 records.
- Compatibility behavior for existing configs without user rewrite.
- Clear warning/report path for unmigratable entries.

Acceptance criteria:

- Existing valid configs continue to run unchanged.
- Migration output is deterministic.
- Duplicate/invalid entries preserve current skip-and-report behavior.

### Phase 5: Validation and Test Coverage

Deliverables:

- Extend validator for schema v1 and dependency split rules.
- Add tests for:
- schema field validation
- ordering vs requirement behavior
- override persistence lifecycle
- migration success/failure cases
- crash-safe load/save behavior

Acceptance criteria:

- `make check` passes.
- New behavior is enforced by regression tests, not only implementation logic.

### Phase 6: Documentation and Rollout

Deliverables:

- Update `README.org` to reflect schema v1 and persistent overrides.
- Add migration guidance section.
- Update developer docs (`CLAUDE.md` if architecture notes changed).

Acceptance criteria:

- Docs match implemented behavior exactly.
- No “future tense” in user docs for shipped behavior.

## Risks and Mitigations

- Risk: schema adoption breaks existing configs.
- Mitigation: keep compatibility adapter and migration tests for real-world entry shapes.

- Risk: override persistence introduces state drift.
- Mitigation: single load/apply path, atomic writes, and explicit source-priority rules.

- Risk: dependency split changes startup behavior unexpectedly.
- Mitigation: golden tests for representative staged DAG scenarios before/after refactor.

## Definition of Done

- Formal schema v1 is implemented and versioned.
- Persistent overrides are implemented and stable.
- Migration path is shipped and tested.
- Validation coverage is complete for schema/dependency/override paths.
- `make check` passes and docs are updated.
