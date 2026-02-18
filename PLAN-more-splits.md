# PLAN: Additional Core Splits (Logging, Overrides, Sandbox/Libexec)

Date: 2026-02-18
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
Reduce `supervisor-core.el` size and complexity by extracting cohesive
subsystems into dedicated files without changing runtime behavior.

This plan covers exactly these abstraction targets:

1. Logging subsystem -> `supervisor-log.el`.
2. Overrides persistence subsystem -> `supervisor-overrides.el`.
3. Sandbox and libexec rebuild helpers -> `supervisor-sandbox.el` and/or
   `supervisor-libexec.el`.

## Baseline Anchors and Call Sites
Anchors are reference points from the current baseline and may move as code is
edited. They define scope, not frozen line numbers.

Logging subsystem anchors:

- `supervisor-core.el:3808`
- `supervisor-core.el:3899`
- `supervisor-core.el:4018`
- `supervisor-core.el:4238`

Logging cross-module usage already present:

- `supervisor-cli.el:1783`
- `supervisor-dashboard.el:1540`

Overrides subsystem anchors:

- `supervisor-core.el:1431`
- `supervisor-core.el:1514`

Overrides cross-module usage already present:

- `supervisor-dashboard.el:1468`
- `supervisor-cli.el:131`

Sandbox/libexec subsystem anchors:

- `supervisor-core.el:5537`
- `supervisor-core.el:5699`
- `supervisor-core.el:5747`

## Locked Decisions
1. This refactor is structural only; user-visible behavior MUST remain
   unchanged.
2. Public and private symbol names MUST remain stable unless explicitly listed
   in this file.
3. Existing CLI and dashboard behavior MUST remain unchanged.
4. Existing log format behavior and decode behavior MUST remain unchanged.
5. Existing overrides file format and load/save behavior MUST remain unchanged.
6. Existing sandbox argv and libexec rebuild behavior MUST remain unchanged.
7. All extracted files MUST use lexical binding and provide a feature.
8. `make check` MUST pass at each phase gate.
9. If sandbox and libexec are split together, preferred output is two files:
   `supervisor-sandbox.el` and `supervisor-libexec.el`.
10. A temporary single-file extraction is permitted only if needed for
    correctness, and MUST still isolate sandbox and libexec sections clearly.

## Module Contracts

### 1) Logging Module Contract (`supervisor-log.el`)
The module MUST own:

- log framing helpers and wire encode/decode helpers,
- binary decoder helpers,
- text decoder helpers,
- record filtering and formatting helpers,
- writer lifecycle helpers currently grouped under the logging lifecycle block.

The module MUST expose stable callable symbols currently used by:

- core process/writer orchestration,
- CLI journal/log rendering paths,
- dashboard log inspection paths,
- existing tests.

No semantic changes are allowed to:

- record field names,
- record priority classification,
- follow/decode offsets and warning behavior.

### 2) Overrides Module Contract (`supervisor-overrides.el`)
The module MUST own:

- overrides file path resolution,
- overrides load/migration/save functions,
- override lifecycle state variables tied to persistence.

The module MUST preserve:

- compatibility with existing overrides file contents,
- current failure handling and warning behavior,
- existing policy mutator integrations and CLI/dashboard calls.

### 3) Sandbox/Libexec Module Contract
The extraction MUST isolate:

- libexec build target enumeration and stale detection,
- compiler selection and build invocation,
- startup-time helper build policy flow,
- sandbox profile argument construction and launch argv wrapping.

Allowed output shapes:

- `supervisor-libexec.el` plus `supervisor-sandbox.el` (preferred),
- or a temporary single extraction file if phase constraints require it.

Behavioral invariants:

- stale helper detection based on executable/source mtimes remains identical,
- launch argv ordering remains identical (runas -> sandbox -> executable),
- sandbox gate and profile semantics remain identical.

## Dependency and Load Semantics
1. New files MUST be added to the load graph in a deterministic order.
2. Cross-file calls MUST compile cleanly (`declare-function`, `require`, or
   equivalent boundary wiring).
3. Circular runtime requires MUST NOT be introduced.
4. Existing entrypoint behavior through `supervisor.el` MUST continue to load
   all required functionality.
5. If module dependency rules are changed, `CLAUDE.md` MUST be updated in the
   same phase as the code move.

## Phase Plan

### Phase 1: Boundary Scaffolding
Deliverables:

1. Add empty or minimal module files with headers and `(provide ...)`.
2. Wire load order in entrypoint/module list without behavior changes.
3. Add compile-safe cross-module declarations for planned moved functions.

Acceptance:

1. No functional diffs in runtime behavior.
2. Byte compilation is warning-free.
3. `make check` passes.

### Phase 2: Logging Extraction
Deliverables:

1. Move logging blocks anchored at:
   - `supervisor-core.el:3808`
   - `supervisor-core.el:3899`
   - `supervisor-core.el:4018`
   - `supervisor-core.el:4238`
2. Keep all external call sites working:
   - `supervisor-cli.el:1783`
   - `supervisor-dashboard.el:1540`
3. Preserve all current decoder, formatter, and writer lifecycle semantics.

Acceptance:

1. CLI journal/log commands produce unchanged output for existing tests.
2. Dashboard log inspection remains functional and unchanged.
3. Log writer startup/shutdown behavior remains unchanged.
4. `make check` passes.

### Phase 3: Overrides Extraction
Deliverables:

1. Move overrides persistence blocks anchored at:
   - `supervisor-core.el:1431`
   - `supervisor-core.el:1514`
2. Keep call sites working:
   - `supervisor-dashboard.el:1468`
   - `supervisor-cli.el:131`
3. Preserve override load/save/migration semantics.

Acceptance:

1. Override toggles from CLI and dashboard still persist identically.
2. Corrupt/missing overrides handling remains unchanged.
3. `make check` passes.

### Phase 4: Sandbox and Libexec Extraction
Deliverables:

1. Move sandbox/libexec helper blocks anchored at:
   - `supervisor-core.el:5537`
   - `supervisor-core.el:5699`
   - `supervisor-core.el:5747`
2. Extract to `supervisor-libexec.el` and `supervisor-sandbox.el`, or a
   temporary single extraction file with clear separation.
3. Preserve startup helper rebuild decisions and sandbox argv generation.

Acceptance:

1. Stale helper detection and rebuild triggers are unchanged.
2. Launch command composition remains byte-for-byte equivalent for covered
   test inputs.
3. Sandbox validation/building behavior remains unchanged.
4. `make check` passes.

### Phase 5: Cleanup, Docs, and Audit
Deliverables:

1. Remove dead wrappers/shims no longer needed.
2. Ensure module headers and dependency comments reflect final boundaries.
3. Update `CLAUDE.md` if module dependency/load-order guidance changed.
4. Add/update architecture notes for new module boundaries.

Acceptance:

1. No duplicate logic remains between core and extracted modules.
2. `supervisor-core.el` line count is materially reduced.
3. `make check` passes.

## Explicit Non-Goals
1. No redesign of log record schema or log transport protocol.
2. No new CLI flags or dashboard features.
3. No behavior changes to policy semantics, lifecycle, restart, or scheduler.
4. No rewrite of unit parsing or timer subsystem in this plan.
