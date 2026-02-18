# PLAN: static-builds supervisor-baked variants and optional PID1 mode

Date: 2026-02-18
Status: Draft Locked
Authority: This file is the implementation contract for static-builds work.

## Rule of Interpretation
All requirements in this file are mandatory.

- MUST means required for completion.
- MUST NOT means prohibited.
- SHOULD and MAY are not used.

## Objective
Add a new "baked-in supervisor" static Emacs path that requires no prior user
configuration, while preserving existing static build variants exactly as-is.

This plan has two tracks:

1. Track A (now): add two new build variants only.
2. Track B (next): optional Emacs PID1 patch and optional rc.boot/rc.shutdown
   integration.

## Scope Freeze
The following existing files MUST remain unchanged in this plan:

- `static-builds/PKGBUILD-static-nox`
- `static-builds/PKGBUILD-static-nox-minimal`
- `static-builds/PKGBUILD-static-nox-nativecomp`
- `static-builds/emacs-static-nox.nix`
- `static-builds/emacs-static-nox-minimal.nix`
- `static-builds/emacs-static-nox-nativecomp.nix`

Only two new build files are introduced in Track A:

- `static-builds/PKGBUILD-static-nox-supervisor`
- `static-builds/emacs-static-nox-supervisor.nix`

## Product Contract

### Baked-in supervisor contract
The two new variants MUST produce a static Emacs build that:

1. Includes supervisor.el in the final artifact.
2. Boots with supervisor enabled by default, with no user init file required.
3. Starts supervisor automatically on startup through packaged startup Lisp.
4. Provides a documented escape hatch to disable autostart at runtime.

### PID1 contract (optional track)
PID1 behavior MUST be optional and explicit. It MUST NOT be forced by default.
When enabled, PID1 behavior MUST provide:

1. Child reaping for orphaned/exited children.
2. Optional boot/shutdown Lisp script execution policy.
3. Signal-safe handling (handlers set flags; main loop performs work).

## Track A: Two New Build Variants

### Phase A1: Naming and layout lock
Deliverables:

1. Lock canonical names:
   - `PKGBUILD-static-nox-supervisor`
   - `emacs-static-nox-supervisor.nix`
2. Document exact relationship to "full static nox" baseline.
3. Add file header notes that originals are untouched legacy variants.

Acceptance:

1. Exactly two new build files are added.
2. No diff in the six existing variant files.

### Phase A2: Baked-in startup bootstrap
Deliverables:

1. Add packaged startup Lisp that is installed into site startup paths and
   auto-loads supervisor.
2. Bootstrap MUST call supervisor startup path without requiring `~/.emacs`.
3. Bootstrap MUST include an explicit disable mechanism:
   - env var gate, or
   - Lisp variable gate in early init context.

Acceptance:

1. Fresh runtime with no user config starts supervisor automatically.
2. Disable gate prevents autostart predictably.

### Phase A3: PKGBUILD baked variant
Deliverables:

1. Implement `PKGBUILD-static-nox-supervisor` as a new variant.
2. Reuse static-linking checks already used by existing variants.
3. Install supervisor runtime files and bootstrap startup Lisp in package.
4. Add package-level smoke test that validates baked startup behavior.

Acceptance:

1. Package builds and passes static checks.
2. `emacs --batch` smoke confirms supervisor load path is valid.
3. Non-interactive startup test confirms autostart hook executes.

### Phase A4: Nix baked variant
Deliverables:

1. Implement `emacs-static-nox-supervisor.nix` as a new variant.
2. Keep feature set aligned with PKGBUILD baked variant.
3. Install the same supervisor files and startup bootstrap logic.
4. Add Nix-side smoke tests equivalent to PKGBUILD checks.

Acceptance:

1. Nix build produces static binary and baked supervisor files.
2. Nix smoke test confirms autostart bootstrap is wired.
3. Runtime behavior matches PKGBUILD baked variant contract.

### Phase A5: Parity and docs
Deliverables:

1. Expand `static-builds/README.md` to include a variant matrix:
   - minimal
   - full
   - nativecomp
   - new baked variant
2. Document "no-user-config" startup behavior and disable gate.
3. Document intended use and non-goals for baked variant.

Acceptance:

1. README has runnable commands for PKGBUILD and Nix baked variants.
2. README clearly distinguishes old variants vs new baked variant.

## Track B: Optional PID1 and rc scripts

Track B is planned work after Track A and is optional by design.

### Phase B1: Emacs PID1 patch proposal
Deliverables:

1. Add patch design doc under `static-builds/patches/`.
2. Define optional CLI/runtime gate (`--pid1` and/or explicit variable).
3. Specify reaping implementation point in main loop.

Acceptance:

1. Patch design documents signal-safety and reaping semantics.
2. Default behavior remains unchanged when PID1 mode is disabled.

### Phase B2: Supervisor PID1 Lisp interface
Deliverables:

1. Introduce PID1 control variables:
   - `supervisor-pid1-mode-enabled`
   - `supervisor-pid1-boot-script`
   - `supervisor-pid1-shutdown-script`
   - `supervisor-pid1-boot-policy`
   - `supervisor-pid1-shutdown-policy`
2. Policy set MUST support:
   - `never`
   - `if-present`
   - `require`

Acceptance:

1. Variables are documented and default to safe non-invasive values.
2. PID1 mode disabled means no reaping/script side effects.

### Phase B3: rc.boot/rc.shutdown execution model
Deliverables:

1. Define optional script execution flow inspired by sinit lifecycle:
   - early boot hook
   - reboot/poweroff hook
2. Implement all script execution in normal runtime context, not in raw signal
   handlers.

Acceptance:

1. Boot/shutdown scripts run only when policy allows.
2. Missing scripts are handled per policy (`never/if-present/require`).

### Phase B4: PID namespace tests
Deliverables:

1. Add test harness for PID1 behavior in isolated namespace/container.
2. Verify child reaping and signal forwarding logic.
3. Verify boot/shutdown policy behavior.

Acceptance:

1. Tests prove reaping behavior when enabled.
2. Tests prove no behavior change when disabled.

## Non-Goals
This plan does not include:

1. Editing or removing existing six static build variants.
2. Replacing initramfs responsibilities like mount/fsck/network.
3. Making PID1 behavior mandatory.
4. Enforcing rc scripts in environments where initramfs already handles setup.

## Exit Criteria
Track A is complete when:

1. Exactly two new build files exist and are documented.
2. Both produce static Emacs with baked-in supervisor autostart.
3. Existing six build files remain unchanged.

Track B is complete only after optional PID1 patch and script policy behavior
are implemented and tested behind explicit opt-in gates.
