# PLAN: static-builds elinit-baked variants and optional PID1 mode

Date: 2026-02-18
Status: Draft Locked
Authority: This file is the implementation contract for static-builds work.

## Rule of Interpretation
All requirements in this file are mandatory.

- MUST means required for completion.
- MUST NOT means prohibited.
- SHOULD and MAY are not used.

## Objective
Add a new "baked-in elinit" static Emacs path that requires no prior user
configuration, while preserving existing static build variants exactly as-is.

This plan has two tracks:

1. Track A (now): add two new build variants only.
2. Track B (next): optional Emacs PID1 patch and optional rc.boot/rc.shutdown
   integration.

Emacs-side PID1 patch requirements are defined in:

- `static-builds/PLAN-pid1-emacs-patch-support.md`

## Document roles

1. `static-builds/README.md` is administrator guidance.
2. `static-builds/PLAN-pid1-emacs-patch-support.md` is the Emacs patch prerequisite spec.
3. `static-builds/PLAN.md` is the packaging/integration execution contract that
   depends on the prerequisite spec for PID1-labeled outputs.

## Prerequisite gates

1. The PID1-labeled variant outputs in this plan are blocked on completion of
   `static-builds/PLAN-pid1-emacs-patch-support.md`.
2. Track A Phase A3/A4 implementation MUST NOT be declared complete until patch
   artifacts from `static-builds/patches/` exist and validate.
3. `static-builds/README.md` is the administrator-facing handbook and MUST be
   kept consistent with the active gate status (planned vs available).

## Scope Freeze
The following existing files MUST remain unchanged in this plan:

- `static-builds/PKGBUILD-static-nox`
- `static-builds/PKGBUILD-static-nox-minimal`
- `static-builds/PKGBUILD-static-nox-nativecomp`
- `static-builds/emacs-static-nox.nix`
- `static-builds/emacs-static-nox-minimal.nix`
- `static-builds/emacs-static-nox-nativecomp.nix`

Only two new build files are introduced in Track A:

- `static-builds/PKGBUILD-static-nox-elinit-patched-for-pid1`
- `static-builds/emacs-static-nox-elinit-patched-for-pid1.nix`

## Product Contract

### Baked-in elinit contract
The two new variants MUST produce a static Emacs build that:

1. Includes elinit in the final artifact.
2. Boots with elinit enabled by default, with no user init file required.
3. Starts elinit automatically on startup through packaged startup Lisp.
4. Provides a documented escape hatch to disable autostart at runtime.

### C helper provisioning contract
The two new variants MUST define how `libexec` C helpers are provided:

1. `elinit-logd` and `elinit-runas` MUST be built at package-build
   time and installed as executable files in the shipped artifact.
2. Normal runtime MUST NOT depend on having a C compiler installed.
3. Helper source files in runtime images MUST be omitted, or guaranteed older
   than binaries, so startup does not spuriously mark helpers stale.
4. Admin fallback path MUST be documented:
   - either supply externally managed helper binaries and point
     `elinit-logd-command` / `elinit-runas-command` to them,
   - or rebuild helpers from source in an admin/dev environment.
5. Documentation-only fallback is acceptable for admins, but prebuilt helper
   binaries remain the default contract for these two new variants.

### PID1 contract (optional track)
PID1 behavior MUST be optional and explicit. It MUST NOT be forced by default.
When enabled, PID1 behavior MUST provide:

1. Child reaping for orphaned/exited children.
2. Optional boot/shutdown Lisp script execution policy.
3. Signal-safe handling (handlers set flags; main loop performs work).

The `-patched-for-pid1` suffix in variant filenames is a capability marker. It
MUST NOT imply PID1 mode is enabled by default.

## Track A: Two New Build Variants

### Phase A1: Naming and layout lock
Deliverables:

1. Lock canonical names:
   - `PKGBUILD-static-nox-elinit-patched-for-pid1`
   - `emacs-static-nox-elinit-patched-for-pid1.nix`
2. Document exact relationship to "full static nox" baseline.
3. Add file header notes that originals are untouched legacy variants.

Acceptance:

1. Exactly two new build files are added.
2. No diff in the six existing variant files.

### Phase A2: Baked-in startup bootstrap
Deliverables:

1. Add packaged startup Lisp that is installed into site startup paths and
   auto-loads elinit in `--pid1` mode.
2. Bootstrap MUST call elinit startup path without requiring `~/.emacs`.
   Autostart fires only when `--pid1` is passed; normal Emacs usage is
   unaffected.
3. Bootstrap MUST include an explicit disable mechanism:
   - env var gate (`EMACS_ELINIT_DISABLE`), and
   - Lisp variable gate (`elinit-pid1-autostart-disabled`) in early init context.

Acceptance:

1. Fresh `--pid1` runtime with no user config starts elinit automatically.
2. Without `--pid1`, elinit is on the load-path but does not autostart.
3. `EMACS_ELINIT_DISABLE=1` prevents autostart in `--pid1` mode.
4. `elinit-pid1-autostart-disabled` set to t prevents autostart in `--pid1` mode.

### Phase A3: PKGBUILD baked variant
Deliverables:

1. Implement `PKGBUILD-static-nox-elinit-patched-for-pid1` as a new variant.
2. Reuse static-linking checks already used by existing variants.
3. Install elinit runtime files and bootstrap startup Lisp in package.
4. Build and install `libexec/elinit-logd` and `libexec/elinit-runas`
   in the package payload.
5. Add package-level smoke test that validates baked startup behavior.
6. Add smoke check that helper binaries are executable and no startup rebuild is
   required on first run.
7. Consume documented Emacs PID1 patch artifact(s) from
   `static-builds/patches/` as required by
   `static-builds/PLAN-pid1-emacs-patch-support.md`.

Acceptance:

1. Package builds and passes static checks.
2. `emacs --batch` smoke confirms elinit load path is valid.
3. Non-interactive startup test confirms autostart hook executes.

### Phase A4: Nix baked variant
Deliverables:

1. Implement `emacs-static-nox-elinit-patched-for-pid1.nix` as a new variant.
2. Keep feature set aligned with PKGBUILD baked variant.
3. Install the same elinit files and startup bootstrap logic.
4. Build and install `libexec/elinit-logd` and `libexec/elinit-runas`
   in the derivation output.
5. Add Nix-side smoke tests equivalent to PKGBUILD checks.
6. Add smoke check that helper binaries are executable and no startup rebuild is
   required on first run.
7. Consume documented Emacs PID1 patch artifact(s) from
   `static-builds/patches/` as required by
   `static-builds/PLAN-pid1-emacs-patch-support.md`.

Acceptance:

1. Nix build produces static binary and baked elinit files.
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
3. Document helper-binary provisioning policy and admin fallback workflow.
4. Document intended use and non-goals for baked variant.

Acceptance:

1. README has runnable commands for PKGBUILD and Nix baked variants.
2. README clearly distinguishes old variants vs new baked variant.

## Track B: Optional PID1 and rc scripts

Track B is planned work after Track A and is optional by design.

### Phase B1: Emacs PID1 patch proposal
Deliverables:

1. Add patch design doc under `static-builds/patches/`.
2. Define optional CLI/runtime gate (`--pid1` and/or explicit variable).
3. Specify reaping implementation point (SIGCHLD handler with non-blocking waitpid).

Acceptance:

1. Patch design documents signal-safety and reaping semantics.
2. Default behavior remains unchanged when PID1 mode is disabled.
   CI-enforced by ERT tests in `tests/elinit-test-pid1.el`:
   `elinit-test-pid1-mode-enabled-default-nil`,
   `elinit-test-pid1-no-hooks-at-load-without-pid1-mode`,
   `elinit-test-pid1-no-process-side-effects-when-disabled`,
   `elinit-test-pid1-detect-mode-unbound-pid1-mode`.

### Phase B2: Elinit PID1 Lisp interface
Deliverables:

1. Introduce PID1 control variables:
   - `elinit-pid1-mode-enabled`
   - `elinit-pid1-boot-script`
   - `elinit-pid1-shutdown-script`
   - `elinit-pid1-boot-policy`
   - `elinit-pid1-shutdown-policy`
2. Policy set MUST support:
   - `never`
   - `if-present`
   - `require`

Acceptance:

1. Variables are documented and default to safe non-invasive values.
2. `elinit-pid1-mode-enabled` set to nil means no Lisp-side script
   loading or service lifecycle calls (`elinit-start`, `elinit-stop-now`).
   C-level reaping and signal handling are controlled by the `--pid1`
   flag (B1 scope) and are outside this Lisp variable's authority.

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

Note: C-level reaping and signal tests are covered at the patch level
(see `patches/README-pid1-validation.md` tests 8-13).  The ERT test
suite (`tests/elinit-test-pid1.el`) covers Lisp-side policy dispatch,
auto-detection, boot/shutdown function behavior, and hook registration.

Implementation: `static-builds/tests/test-pid1-namespace.sh` provides
an automated, reproducible test harness that runs inside isolated PID
namespaces via `unshare --user --pid --fork --mount-proc`.  It covers
11 tests: pre-flight checks (pid1-mode, hooks defined), boot hook
firing, signal handling (SIGTERM, SIGUSR1, SIGUSR2, SIGHUP), child
reaping, and backward compatibility (no PID1 hooks without --pid1).

CI job `pid1-tests` exists in `.github/workflows/ci.yml` but requires
`ELINIT_PID1_EMACS` pointing to a patched binary to activate.  Without
the env var, all tests skip gracefully (exit 0).

Run locally:

    make pid1-check ELINIT_PID1_EMACS=/path/to/patched/emacs

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
2. Both produce static Emacs with baked-in elinit autostart.
3. Existing six build files remain unchanged.
4. Required PID1 patch artifact prerequisite is satisfied for PID1-labeled
   outputs.

Track B is complete only after optional PID1 patch and script policy behavior
are implemented and tested behind explicit opt-in gates.
