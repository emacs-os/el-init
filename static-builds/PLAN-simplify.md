# PLAN: Simplify static-builds to PID1-patched Emacs only

Date: 2026-02-20
Status: Draft Proposed
Authority: This file defines the simplification contract for `static-builds/`.

## Rule of Interpretation
All requirements in this file are mandatory.

- MUST means required for completion.
- MUST NOT means prohibited.
- SHOULD and MAY are not used.

## Objective
Simplify `static-builds` so PID1 variants provide only:

1. Static Emacs builds with `--pid1` patch support.
2. Runtime hook/signal/reaping capability required for init-style operation.

These variants MUST NOT bake in `el-init` as part of the build output. System
administrators are responsible for providing their own `init.el`/early startup
configuration and any optional `el-init` installation.

## Motivation
This simplification separates concerns:

1. Build artifacts provide a minimal PID1-capable Emacs base.
2. Admin policy and init logic remain site-controlled.
3. Security and maintenance risk from auto-baked startup behavior is reduced.

## Scope

### In scope
1. PID1 static variant packaging behavior in `static-builds/`.
2. Static-builds documentation and plan alignment.
3. Validation updates to prove PID1 support without bundled `el-init`.

### Out of scope
1. Removal of upstream PID1 patch artifacts.
2. Removal of `el-init` from this repository.
3. Redesign of `el-init` core behavior.

## Product Contract (Simplified)
PID1 static variants MUST:

1. Build and install patched Emacs with `--pid1`.
2. Preserve PID1 signal mapping, hook points, and child reaping semantics.
3. Leave default startup policy neutral (no forced `el-init` autoload).

PID1 static variants MUST NOT:

1. Install a `site-start.el` bootstrap that auto-loads `el-init`.
2. Bundle `el-init` Lisp modules into Emacs site-lisp payload by default.
3. Bundle `el-init` helper binaries (`elinit-logd`, `elinit-runas`,
   `elinit-rlimits`) by default.

## Administrator Responsibility Contract
Administrators MUST provide and own runtime policy:

1. Their own startup files (`early-init.el`, `init.el`, or equivalent).
2. Their own optional `el-init` installation path and versioning policy.
3. Their own wiring for hook registration and unit configuration.
4. Their own helper binary provisioning strategy if using `el-init` features
   that depend on helpers.

## Script Execution Contract
Early boot/shutdown script execution MUST use one explicit path:

1. Supervisor-managed units (typically deterministic early oneshot units).

The system MUST NOT maintain a second implicit path based on PID1 hook-time
file probing (for example auto-loading `/lib/init/rc.boot.el` if present).

For pure Elisp boot logic, admins can still repurpose examples in
`static-builds/scripts/rc.boot.el.example` and
`static-builds/scripts/rc.shutdown.el.example`, but invocation MUST be explicit
through unit config or admin init files.

## Phase Plan

### Phase S1: Policy lock and naming
Deliverables:

1. Lock simplification policy in this file.
2. Define canonical naming for neutral PID1 variants.
3. Mark legacy baked behavior as deprecated in docs.

Acceptance:

1. Docs consistently describe PID1 variants as base runtime only.
2. No documentation claims baked `el-init` autostart by default.

### Phase S2: Packaging de-bake
Deliverables:

1. Remove `el-init` payload installation from PID1 variant build files.
2. Remove custom `site-start.el` autostart bootstrap from PID1 variants.
3. Remove build/install of `el-init` helper binaries from PID1 variants.
4. Keep PID1 patch application and patch verification checks.

Acceptance:

1. Built PID1 package contains patched Emacs but no bundled `el-init` payload.
2. Launching with `--pid1` does not auto-load `el-init`.
3. Existing PID1 core checks still pass (`pid1-mode`, hooks, signals, reaping).

### Phase S3: Validation update
Deliverables:

1. Update smoke tests to verify neutral startup behavior.
2. Add explicit check proving no `el-init` autostart in package default.
3. Preserve namespace PID1 runtime tests for patch behavior.

Acceptance:

1. Packaging checks fail if `el-init` is injected by default.
2. PID1 namespace test suite remains green with patched Emacs binary.

### Phase S4: Documentation and migration
Deliverables:

1. Update `static-builds/README.md` to describe admin-owned wiring model.
2. Add minimal examples showing how admins can optionally load `el-init`.
3. Document migration from baked variant behavior to explicit admin config.

Acceptance:

1. README reflects only current, implemented behavior.
2. Migration path is explicit for existing baked-variant users.

## Exit Criteria
This plan is complete when:

1. PID1 static variants ship patched Emacs only, with no baked `el-init`.
2. Tests prove neutral default startup plus intact PID1 behavior.
3. `static-builds` docs and plan files consistently reflect the simplified
   contract.

## Non-Goals
This plan does not include:

1. Forcing any specific init framework on administrators.
2. Removing ability to use `el-init` manually.
3. Changing upstream Emacs PID1 hook semantics.
