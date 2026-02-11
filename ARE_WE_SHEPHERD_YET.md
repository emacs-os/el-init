# Are We Shepherd Yet?

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
- `supervisor.el` today is only a per-user service manager.
- We want to experimentally offer an opt-in path toward PID 1 for tinkerers,
  while keeping per-user mode as the default supported path.
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

## Roadmap (Remaining Required Sequence)

**Remaining Roadmap Requirements (Detailed)**

**1. PID 1 Engineering**
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
