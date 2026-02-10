# Are We Shepherd Yet?

This project aims to build a complete PID 1 init and supervision system that
runs inside Emacs, securely and robustly. This is a serious engineering goal.
The question is not whether we can do it, but whether we are willing to build
it to the standard that PID 1 demands.

This document defines the target, the requirements, and the path forward.

## Target: Full PID 1 Init and Supervisor

We are building:

- A PID 1 capable init and supervisor that can run in the initial PID namespace.
- A user-session init that is already usable today and can grow to full system
  responsibilities.
- A complete non-interactive control plane (CLI + JSON) so interactive Emacs
  UI is optional, not required.

## Reality Check: PID 1 Is Different

If Emacs (or a shim that execs Emacs) is PID 1, it must handle the special
responsibilities of init:

- Reap orphaned children to avoid zombie accumulation.
- Set explicit signal handlers for termination, shutdown, and child events.
- Remain responsive and alive; if init dies, the PID namespace dies with it.

These are not optional. This is what differentiates a normal process from
an init process.

## Shepherd Parity (Minimum)

The GNU Shepherd manual defines core capabilities we must match or exceed:

- Service model with explicit start/stop actions and structured service state.
- Dependency model (requirements and provisions) with a dependency graph.
- Restart policy with respawn limits and delay.
- One-shot and transient services.
- A non-interactive CLI (herd) that controls the daemon via a Unix socket.

Our plan must reach feature parity with these fundamentals.

## Systemd-Class Semantics (Ordering vs Requirements)

Systemd distinguishes two concepts:

- Requirement dependencies (Wants/Requires) which pull units in.
- Ordering dependencies (After/Before) which only control sequence.

This separation is critical for deterministic orchestration and must be
modeled explicitly in the supervisor dependency graph.

## Security Model (Required)

Emacs server access is a control boundary. A CLI built on `emacsclient --eval`
must be treated as privileged. If a client can connect, it can execute code
as the Emacs process.

Security requirements:

- Default to Unix domain sockets with strict file permissions.
- If TCP is enabled, require a strong auth key and protect the server file.
- Provide a locked-down mode that rejects arbitrary evaluation for untrusted
  callers.
- Document the threat model clearly.

## Requirements: The Specification

### PID 1 Core

- SIGCHLD handling and reliable child reaping.
- Explicit signal handlers for SIGTERM, SIGINT, SIGQUIT, SIGPWR, SIGUSR1/2.
- Safe shutdown ordering: stop dependents before dependencies.
- Crash safety: init must not exit due to unhandled signals.

### Service Model

- Service types: long-running, oneshot, transient, and socket-activated (future).
- Readiness semantics (spawned vs exited vs explicit ready) must be defined.
- Dependency graph with cycle detection and stable ordering.
- Strict validation of service definitions.

### Lifecycle and Policy

- start/stop/restart/reload per service.
- enable/disable (persistent + runtime overrides).
- bounded restart policy with delay and crash-loop suppression.
- no busy-wait loops; use timers and sentinels only.

### Observability

- status view with PID, exit code, enabled state, and reason.
- dependency graph inspection.
- timing and blame output for slow startup.
- per-service logs with rotation.

### Control Plane (CLI)

- systemctl-style commands with human output by default.
- `--json` output for automation.
- stable schema and exit codes.
- parity with all interactive UI actions.

### Security

- authenticated local control via socket.
- optional TCP access requires explicit config and strong auth.
- no implicit trust of environment variables for server selection.
- clear guidance for running as PID 1 or user-session init.

### Compatibility and Migration

- offer compatibility patterns for Shepherd-like service definitions.
- provide a mapping from `herd` actions to CLI actions.

### Testing and Verification

- if a behavior can be tested, it must be tested.
- CI must include byte-compile, checkdoc, package-lint, and ERT.
- include tests for PID 1 signal handling (when feasible in CI).

## Roadmap (Concrete Phases)

### Phase 1: Control Plane (CLI First)

- Implement `sbin/supervisorctl` and supporting Emacs functions.
- Provide `status`, `list`, `describe`, `graph`, `blame`, `logs`.
- Provide `start/stop/restart/enable/disable/reload/validate`.

### Phase 2: Service Definition Layer

- Formal schema, versioned.
- Persistent enable/disable overrides.
- Strict validation and explicit error reporting.

### Phase 3: PID 1 Engineering

- Decide on a minimal init shim vs direct Emacs PID 1.
- Guarantee SIGCHLD reaping and signal handling.
- Provide safe shutdown and reboot hooks.

### Phase 4: Security Hardening

- Harden emacsclient access and reduce attack surface.
- Add optional restricted control channel (no arbitrary eval).

### Phase 5: Parity and Beyond

- Match Shepherd features: custom actions, transient services, timers.
- Expand readiness and socket activation.
- Documentation for production use.

## Definition of Done

We are “Shepherd yet” when:

- PID 1 responsibilities are implemented and tested.
- CLI is complete and stable, with JSON output.
- Service definitions are validated, versioned, and documented.
- Security posture is explicit and enforced.
- The interactive UI is optional, not required.

## Sources Consulted

- The GNU Shepherd Manual
- systemd.unit(5) and systemd.target(5)
- Linux waitpid(2) manual
- Linux signal handling and PID 1 behavior documentation
- GNU Emacs server documentation
