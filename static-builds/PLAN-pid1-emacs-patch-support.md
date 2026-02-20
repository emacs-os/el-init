# PLAN: Emacs --pid1 Patch Support Contract

Date: 2026-02-18
Status: Complete (2026-02-20)
Authority: This file is the implementation contract for PID1 patch work in
`~/repos/emacs`.

## Rule of Interpretation
All requirements in this file are mandatory.

- MUST means required for completion.
- MUST NOT means prohibited.
- SHOULD and MAY are not used.

## Objective
Define the exact requirements for creating an Emacs patch that adds optional
PID1 support (`--pid1`) for static boot use cases in this repository.

This patch is a prerequisite for:

1. `static-builds/PLAN.md` PID1-labeled variant gates (Track A Phase A3/A4)
   and Track B.
2. `static-builds/README.md` administrator workflows for PID1 operation.

This file is the prerequisite contract. It is not the administrator handbook.
Admin-facing guidance remains in `static-builds/README.md`.

## Scope and Workspace

### In scope
1. Patch development in `~/repos/emacs`.
2. Exporting patch files into this repository under `static-builds/patches/`.
3. Defining stable runtime behavior and integration points required by
   elinit.

### Out of scope
1. Replacing initramfs responsibilities (mount/fsck/network).
2. Making PID1 behavior default for all Emacs invocations.
3. Hard-coding elinit as a compile-time Emacs dependency.

## Required Deliverables

1. A patch file for Emacs command-line and runtime PID1 support:
   - `static-builds/patches/emacs-0001-add-pid1-runtime-mode.patch`
2. A patch file (or included in patch 1) for PID1 hook integration points:
   - `static-builds/patches/emacs-0002-pid1-hooks-and-signals.patch`
3. A validation note with exact build/test commands and outputs:
   - `static-builds/patches/README-pid1-validation.md`
4. A short integration note mapping Emacs hook points to elinit behavior:
   - `static-builds/patches/README-pid1-elinit-integration.md`

Implementation note:

1. Final artifact set also includes:
   - `static-builds/patches/emacs-0003-fix-pid1-signal-handler-overrides.patch`
2. Patch 0003 is a follow-up correctness fix to preserve PID1 signal handlers
   after later initialization stages.

## Behavioral Contract

### 1) Command-line interface
1. Emacs MUST accept `--pid1`.
2. `--pid1` MUST enable PID1 mode logic explicitly.
3. Without `--pid1`, behavior MUST remain unchanged from upstream baseline.

### 2) PID1 mode state
1. Emacs MUST expose a Lisp-visible boolean for PID1 mode state.
2. PID1 mode MUST be queryable from Lisp during startup and runtime.
3. PID1 mode MUST NOT require elinit to be loaded.

### 3) Child reaping
1. PID1 mode MUST reap child processes using non-blocking wait semantics.
2. Reaping MUST be performed in a safe runtime context, not in blocking signal
   handler paths.
3. Reaping MUST be idempotent and must not stall the editor main loop.
4. Non-PID1 mode MUST NOT alter existing process behavior.

### 4) Signal handling model
1. Signal handlers MUST only set atomic flags.
2. Main runtime loop MUST process flagged actions.
3. Signal handling in PID1 mode MUST support reboot/poweroff style events used
   by init-style workflows.
4. The exact signal mapping MUST be documented in
   `README-pid1-validation.md`.

### 5) Lisp integration points
1. Emacs patch MUST expose deterministic Lisp hook points for PID1 lifecycle:
   - PID1 boot hook
   - PID1 reboot hook
   - PID1 poweroff hook
2. Hook names and invocation timing MUST be documented and stable.
3. Hook execution failures MUST be surfaced clearly to stderr/log output.

### 6) rc script integration contract
1. Emacs patch MUST support running Lisp-managed boot/shutdown logic through
   hook points.
2. Script execution policy logic MUST remain in Lisp layer (elinit/site
   startup), not hard-coded in C.
3. Emacs patch MUST NOT require implicit rc script path probing in C.
4. Any script execution behavior remains an explicit Lisp/admin concern, not a
   PID1 C runtime default.

## Elinit Integration Contract
This patch MUST enable the elinit-side workflow defined in
`static-builds/PLAN.md` and `static-builds/README.md`.

Required elinit-side variable (implemented in this repository):

1. `elinit-pid1-mode-enabled`

The Emacs patch MUST provide hook/state primitives sufficient for this
variable to drive behavior entirely from Lisp.

## Downstream handoff contract
On completion, this plan MUST unblock the following downstream work:

1. `static-builds/PLAN.md` Track A Phase A3/A4 for
   `*-elinit-patched-for-pid1` variants.
2. Finalization of PID1 sections in `static-builds/README.md` from "planned"
   wording to "available" wording.
3. A coherent static-builds document set where:
   - `README.md` is admin guidance,
   - `static-builds/PLAN.md` is packaging execution,
   - `static-builds/PLAN-pid1-emacs-patch-support.md` is the Emacs patch prerequisite spec.

## Compatibility and Safety Constraints

1. Patch MUST be backward-compatible for non-`--pid1` runs.
2. Patch MUST compile without introducing warnings in default builds.
3. Patch MUST avoid blocking, unsafe, or alloc-heavy operations in raw signal
   handlers.
4. Patch MUST avoid introducing startup regressions for normal desktop/server
   Emacs usage.

## Reference Behavior Source
The patch design MUST be informed by sinit-style init behavior patterns:

- https://git.suckless.org/sinit/file/sinit.c.html

This is a behavioral reference, not a requirement for identical code shape.

## Phase Plan

### Phase 1: Design and touchpoint mapping
Deliverables:

1. Identify Emacs source touchpoints for:
   - option parsing
   - signal setup/handling
   - main loop integration
   - Lisp hook exposure
2. Document intended control flow before coding.

Acceptance:

1. Design note exists in `README-pid1-validation.md`.
2. Touchpoints are specific enough to implement without ambiguity.

### Phase 2: `--pid1` mode and state exposure
Deliverables:

1. Implement `--pid1` option parse.
2. Expose Lisp-visible PID1 mode state.

Acceptance:

1. Emacs starts with and without `--pid1`.
2. Lisp can query PID1 mode in both cases.
3. No behavior change without `--pid1`.

### Phase 3: Reaping and signal bridge
Deliverables:

1. Implement non-blocking child reaping path.
2. Implement signal flag bridge from handlers to main loop work.
3. Map reboot/poweroff style events into hook triggers.

Acceptance:

1. PID1-mode reaping works in test harness.
2. Signal events trigger expected hook paths.
3. Non-`--pid1` behavior remains unchanged.

### Phase 4: Lisp hook API and docs
Deliverables:

1. Add stable hook symbols and invocation points.
2. Document hook semantics and timing.
3. Document integration contract for elinit-side rc logic.

Acceptance:

1. Hook API is callable and deterministic.
2. Documentation matches implementation exactly.

### Phase 5: Validation and patch export
Deliverables:

1. Run build/test validation in `~/repos/emacs`.
2. Export final patch files to `static-builds/patches/`.
3. Update validation and integration notes.
4. Update downstream status notes in `static-builds/PLAN.md` and
   `static-builds/README.md` to reflect prerequisite completion.

Acceptance:

1. Patch files apply cleanly to documented Emacs baseline.
2. Validation docs include exact command lines and outcomes.
3. `static-builds/PLAN.md` and `static-builds/README.md` references are
   consistent with delivered patch artifacts.

## Exit Criteria
This plan is complete when:

1. Emacs patch artifacts exist in `static-builds/patches/`.
2. `--pid1` mode, reaping, and hook primitives are implemented and validated.
3. Elinit-side PID1 Lisp policies can be layered on top without additional
   C changes.

Completion record (2026-02-20):

1. Patch artifacts exported to `static-builds/patches/`:
   `emacs-0001`, `emacs-0002`, and follow-up fix `emacs-0003`.
2. Validation and integration docs are present and aligned:
   `README-pid1-validation.md`, `README-pid1-elinit-integration.md`.
3. Downstream contract is satisfied:
   `elinit-pid1.el` provides policy variables and hook-driven behavior without
   additional C changes.

## Explicit Non-Goals
1. No mandatory rc script execution in C.
2. No requirement to duplicate full init system logic.
3. No default enablement of PID1 mode outside explicit `--pid1` operation.
