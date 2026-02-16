# FINDINGS: Optional bubblewrap Integration

Date: 2026-02-15
Scope: Feasibility assessment only (no implementation in this report)

## Executive Verdict
Optional bubblewrap integration is feasible and not crazy if kept narrow.

A minimal, opt-in Linux-only sandbox mode is realistic in this codebase.
A broad "systemd sandbox parity" feature set is high complexity and not
recommended in the same effort.

## What systemd does in this area
systemd service hardening uses many unit options (for example:
`ProtectSystem`, `ProtectHome`, `NoNewPrivileges`, namespace restrictions,
seccomp filters, etc.).

The project README already correctly positions these as out-of-scope today
for this supervisor model (`README.org` cgroup/namespace hardening table and
non-goal notes).

## Current codebase fit (important)
The existing launch pipeline is favorable for optional wrapping:

1. Command construction is centralized in `supervisor--build-launch-command`
   (`supervisor-core.el:4108`).
2. Process spawn is centralized in `supervisor--start-process`
   (`supervisor-core.el:4138`).
3. Privilege-drop helper chaining already exists (`supervisor-runas`) and uses
   direct `execv` with absolute executable paths (`libexec/supervisor-runas.c`).

This means bubblewrap can be inserted as an additional wrapper without
reworking scheduler/runtime fundamentals.

## Local environment viability (observed)
On this host:

1. `bwrap` exists: `bubblewrap 0.11.0`.
2. User namespace clone is enabled (`kernel.unprivileged_userns_clone = 1`).
3. A basic unprivileged `bwrap` command executed successfully (`exit 0`).

Interpretation: this environment supports the minimal model.

## Feasibility Rating

1. Minimal opt-in wrapper: **High feasibility**.
2. Full systemd-style hardening option matrix: **Low feasibility** in one pass.

## Recommended Narrow Scope (if pursued)

1. Linux-only feature gate.
2. Optional per-unit enable flag (off by default).
3. Curated sandbox profiles first, not raw free-form args first.
4. Fail closed on invalid/missing sandbox prerequisites.
5. Keep launch action as "start" semantics only (no restart policy coupling).

## Recommended User-Facing Model

Support should be profile-first, with constrained customization:

1. Preset profiles:
   - `none` (default)
   - `strict` (tight isolation baseline)
   - `service` (safer baseline for typical non-GUI daemons)
   - `desktop` (service baseline plus runtime paths for desktop applets)
2. Limited safe knobs:
   - network mode (`shared` or `isolated`)
   - read-only bind allowlist
   - read-write bind allowlist
   - tmpfs destination allowlist
3. Expert mode:
   - optional raw `bwrap` argument passthrough behind a global safety gate
   - clearly documented as advanced and low-support

This model balances usability with supportability and keeps validation tractable.

## Why this helps

1. Adds meaningful isolation for untrusted or high-risk helper services.
2. Reduces filesystem/network/process visibility when profile is strict.
3. Gives an incremental hardening path without cgroup parity work.

## Main costs and risks

1. Cross-platform mismatch: bubblewrap is Linux-specific; macOS needs explicit
   no-op/unsupported handling.
2. Distro variance: user namespaces may be disabled, restricted, or patched.
3. UX risk: strict profiles can break desktop applets/services unless binds are
   explicitly correct.
4. Operational complexity: troubleshooting sandbox breakage is non-trivial.
5. Security footguns if arbitrary `bwrap` args are accepted too early.

## Why not expose all knobs first

1. Validation complexity grows non-linearly with unrestricted argument surfaces.
2. Support burden rises due to distro-specific namespace and mount behavior.
3. Misconfigured units become hard to debug and may appear intermittently broken.
4. Security posture weakens if unsafe combinations are accepted by default.

## Interaction with current identity model (`:user` / `:group`)
Recommended execution order when both are used:

1. `supervisor-runas` drops privileges first.
2. Then `bwrap` runs as target identity.
3. Then service executable starts.

Reason: preserves least privilege and avoids needing elevated bubblewrap mode.

Implementation note: keep argv-based command composition only; do not shell-wrap
generated `bwrap` command lines.

## Validation requirements (must-have if implemented)

1. Reject sandbox feature on non-Linux with explicit reason.
2. Reject enabled sandbox if `bwrap` executable is missing.
3. Reject invalid profile names/options deterministically.
4. Reject non-absolute bind paths and forbidden destination collisions.
5. Reject dangerous or unsupported option combinations.
6. Reject raw args unless expert gate is enabled.
7. Surface invalid reasons in verify, CLI, and dashboard invalid states.

## Testing requirements (must-have if implemented)

1. Unit tests for command construction permutations:
   - plain command,
   - `:user/:group` only,
   - bubblewrap only,
   - combined runas + bubblewrap.
2. Validation tests for OS gate, missing binary, invalid profile.
3. Validation tests for safe knob constraints and raw-mode gate behavior.
4. Runtime tests for successful start and deterministic failure reporting.
5. Non-regression tests for existing logging, timers, and restart behavior.

## Practical recommendation
Proceed only as a small isolated phase with a strict cap:

1. Minimal Linux-only optional integration.
2. Curated profile set first.
3. Limited safe knobs second.
4. Expert raw mode only behind an explicit global gate.
5. Strong validation and tests before expanding profile surface.

Do not attempt systemd hardening parity in the same effort.
