# Are We Shepherd Yet?

> NOTE: This document describes the long-term vision and roadmap. The current
> release of supervisor.el is a user-session supervisor, not an init system or
> PID 1 replacement. See README.org for current scope and capabilities.

We are building a complete PID 1 init and supervision system inside Emacs,
securely and robustly. This is a serious engineering goal. The question is
not "is it funny". The question is "what do we need to build to be correct".

Short answer today: not yet. The rest of this document is the plan, the spec,
and the path to get there.

## Why

- We want a transparent, hackable init with the power of Emacs.
- We want full observability and a non-interactive control plane.
- We want an init that is explainable and deterministic, not opaque.

## Reality Check: PID 1 Is Different

PID 1 has special responsibilities. It must:

- Reap orphaned children so zombies do not accumulate.
- Handle termination and child signals explicitly and predictably.
- Stay alive; if PID 1 exits, the PID namespace dies.

These are baseline requirements for an init process, not optional features.

## Baseline: Shepherd Parity (Minimum)

The GNU Shepherd provides a baseline we must match:

- Shepherd can run as a system-wide daemon or a per-user service manager.
- The `herd` command controls the daemon and provides start/stop/restart/status,
  enable/disable, and dependency graph inspection.
- Services declare provisions and requirements and can be inspected and
  operated on by name.

Shepherd-level usability is the minimum bar for our control plane.

## Ordering Semantics: Systemd-Style Split

Systemd distinguishes two concepts:

- Ordering dependencies: After/Before control start order only.
- Requirement dependencies: Wants/Requires pull units in, but do not order.

We must model both explicitly for deterministic orchestration.

## Control Plane and Security

The control plane is a security boundary. If we use `emacsclient --eval` for
control, any client that can connect can execute code as the Emacs process.
We must treat server access as privileged and lock it down.

## Roadmap (Remaining Required Sequence)

Completed milestones (already shipped):

1. Service definition layer.
2. Control plane implementation.
3. Modularized codebase split (core/dashboard/CLI + facade).

Remaining mandatory milestones (do in order):

1. Security hardening: restricted control channel, explicit auth model, and a
   documented threat model.
2. PID 1 engineering: child reaping, signal handling, safe shutdown semantics,
   crash safety, and tests.
3. Parity and expansion: systemd-timer-equivalent scheduling, socket
   activation, advanced readiness, and remaining capability additions.

**Remaining Roadmap Requirements (Detailed)**

**1. Security Hardening**
- Lock down control channels (local socket by default).
- If TCP is enabled, require strong auth and protect server files.
- Document the threat model and safe deployment guidance.

**2. PID 1 Engineering**
- Implement child reaping and SIGCHLD handling.
- Define explicit signal handling for shutdown and reboot flows.
- Ensure safe shutdown order and crash safety under PID 1 semantics.
- Add tests where possible and document any untestable behaviors.
- Emacs source reality check (from `src/process.c` and `src/sysdep.c`):
- Emacs installs a SIGCHLD handler (`catch_child_signal` → `deliver_child_signal`
  → `handle_child_signal`) and updates process state there.
- Actual reaping is done via `waitpid` through `get_child_status` and
  `child_status_changed`, but only for known child PIDs.
- Emacs explicitly avoids `waitpid(-1)` to prevent reaping children created by
  other threads (e.g., GLib). This means it does **not** reap unknown/orphaned
  children by default.
- Emacs uses a self-pipe to wake its `pselect` loop on SIGCHLD, which works for
  known processes but does not solve orphan reaping.

Required PID 1 deployment paths (must choose one and implement it fully):
- Path A (recommended): use a tiny PID 1 (e.g., `sinit`) and run Emacs as its
  managed child. This is the correct no-Emacs-patch path; Emacs is not PID 1.
- Path B: patch Emacs to support a true PID 1 mode (global reaping plus safe
  signal handling) so Emacs itself is PID 1.

Shim approach (preferred):
- Implement a minimal PID 1 shim by repurposing core `sinit` code (only the
  parts we need). The shim runs as PID 1, reaps all children, handles signals,
  and launches Emacs as its managed child.
- Explicit rule: if the shim is PID 1, Emacs is **not** PID 1. If the project
  requires Emacs to be PID 1, the shim logic must be integrated into Emacs
  (or Emacs must be patched to reap all children itself).

**3. Parity and Expansion**
- Add systemd-timer-equivalent timers (calendar + monotonic triggers,
  persistence/catch-up behavior, and jitter/accuracy controls) and socket
  activation.
- Add advanced readiness semantics as needed.
- Keep CLI parity with the interactive UI.

## Definition of Done

We are "Shepherd yet" when:

- PID 1 responsibilities are implemented and tested.
- The CLI is complete and stable, with JSON output.
- Service definitions are validated, versioned, and documented.
- Security posture is explicit and enforced.
- The interactive UI is optional, not required.

## Sources Consulted

- https://www.gnu.org/software/shepherd/manual/shepherd.html
- https://www.gnu.org/software/emacs/manual/html_node/emacs/Emacs-Server.html
- https://www.gnu.org/software/emacs/manual/html_node/emacs/emacsclient-Options.html
- https://man7.org/linux/man-pages/man1/systemd-nspawn.1.html
- https://man7.org/linux/man-pages/man5/systemd.unit.5.html
- https://man7.org/linux/man-pages/man2/waitpid.2.html
- https://man7.org/linux/man-pages/man7/signal.7.html
- Emacs source: `src/process.c`, `src/sysdep.c`
