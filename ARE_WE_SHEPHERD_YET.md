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

## Spec and Requirements (Authoritative)

This section reproduces the current plan spec so this document is self
contained. It is the authoritative checklist for completion.

### P0 Requirements (Must)

Configuration validity whitelist:

- Define a finite whitelist of valid option combinations for
  `supervisor-programs`.
- Encode constraints and aliases explicitly (mutual exclusivity, type specific
  options, `:async t` as `:oneshot-wait nil`, `:oneshot-timeout` type, etc).
- Validate entries at the start of `supervisor-start` and allow the supervisor
  to continue if some are invalid.
- Skip invalid entries, surface them in the dashboard with reason, and log a
  warning per invalid entry.

Scheduler semantics (async DAG):

- Use async callbacks only (timers and sentinels). No polling loops.
- Start entries within a stage using a DAG built from `:after` dependencies.
- Allow parallel startup of unrelated entries.
- Treat `:after` as waiting on ready state.
- Preserve stable ordering for unconstrained nodes using original list order.
- Fall back to list order on cycle detection and clear `:after` edges for that
  stage to prevent stalls.
- Do not mark delayed entries as started until delay elapses and spawn begins.
- Oneshot timeouts unlock dependents and stage completion with a warning.
- Disabled entries are ready immediately.
- Clean up all DAG state on stage completion, shutdown, and restart.

Stage completion:

- A stage is complete only when all simple processes have spawned, all blocking
  oneshots have exited or timed out, and all delayed entries have started.

Logging and observability:

- Provide a `supervisor-verbose` toggle.
- When verbose, log all meaningful events (stage start/completion, entry
  start/skip/failure, dependency unlocks, oneshot timeouts, restarts,
  crash-loop suppression).
- When not verbose, only warnings and errors should be logged.

Dashboard:

- Display stage, enabled state, status, restart, logging, PID, and invalid
  state if applicable.
- Show `failed` for oneshots with non-zero exit.
- Show `invalid` for invalid entries with a reason string.

Global minor mode:

- Provide a global minor mode named `supervisor-mode`.
- Enabling starts supervisor, disabling stops it.
- `C-h f` on `supervisor-mode` must include accurate help and sample config.

Testing policy:

- Use ERT for all tests.
- If a behavior can be tested, it must be tested.
- Required coverage: whitelist validation, cycle fallback, stable ordering,
  oneshot timeout unlock, delayed entry handling, async oneshot non-blocking.

Retries and resilience:

- Keep restart behavior bounded (crash-loop detection).
- Honor restart delay and cancel pending restart timers on shutdown.
- Start failures must not block dependents or stages.

### Product Positioning: Systemd Jab (P0)

- Explicitly state scope in docs/help: this is a user-session supervisor, not
  an init system or PID 1 replacement.
- Runtime inspectability: dashboard shows resolved config and dependency edges
  or computed DAG order.
- Logging stays simple and inspectable: plain files with timestamps/rotation.
- Configuration is explicit and validated; no undocumented defaults.
- Startup ordering is deterministic and explainable; ready semantics documented
  in README and `C-h f` help.
- Dashboard is interactive; overrides for restart/logging are visible and clear;
  status explains why an entry is not running.
- Track and expose startup timing: start and ready timestamps and blame view.
- Calculate and expose the dependency graph; `d` opens deps for entry.
- Include an ASCII boot banner reflecting stage progress.
- Provide `supervisor-validate` command.
- Include "Why supervisor.el exists" in README without ranting.

### P1 Requirements (Should)

- `supervisor-validate` (already promoted to P0 above).
- `supervisor-reload` to reconcile config changes without restart.
- Stage-level timeout to avoid stalls.
- `supervisor-max-concurrent-starts` to avoid bursts.
- Hooks for stage start/complete and process exit.
- Distinguish signal death vs non-zero exit in diagnostics.
- `supervisor-describe-entry` for resolved config display.
- Dashboard toggle to filter by stage or show dependency edges.
- Supervisor-level log file when verbose.
- Runtime enable/disable overrides.
- Remove or gate demo entries before release builds.

### P2 Requirements (May)

- File watch to trigger `supervisor-reload` on config changes.
- Per-unit tags for dashboard filtering.
- Dry-run mode that validates and orders entries without starting them.

### Addendum: Autoloads (Recommended)

- Add `;;;###autoload` cookies for all interactive entry points:
  `supervisor-start`, `supervisor-stop`, `supervisor`, `supervisor-validate`,
  `supervisor-mode`, and any other `defun` with `(interactive)`.
- Add autoload for `supervisor-dashboard-mode` if it is user-facing.

### MELPA Submission Requirements (Quality Gate)

- Lexical binding enabled.
- Standard package headers and GPL boilerplate.
- `Package-Requires` minimum Emacs version.
- LICENSE file present.
- Pass `package-lint`, `checkdoc`, and `byte-compile` with no warnings.
- Provide feature matching filename and end with `;;; supervisor.el ends here`.

## Roadmap (Required Sequence)

All items below are mandatory. There are no optional phases. Do them in order.

1. Deep codebase review and refactor against `CLAUDE.md` and `STANDARDS.md`.
   Fix all violations and run `make check` until clean.
2. Declarative/data-driven review and refactor based on
   `DATA_DRIVEN_DECLARATIVE.md`. Convert the research into concrete design
   targets and incremental refactors.
3. Control plane implementation: ship `supervisorctl` and the POSIX wrappers
   planned in `sbin/README.md`, with human + JSON output, stable schema, exit
   codes, and tests.
4. Service definition layer: formal schema, versioning, persistent overrides,
   migration plan, and validation coverage.
5. PID 1 engineering: child reaping, signal handling, safe shutdown semantics,
   crash safety, and tests.
6. Security hardening: restricted control channel, explicit auth model, and a
   documented threat model.
7. Parity and expansion: timers, socket activation, advanced readiness, and
   remaining capability additions.

**Roadmap Requirements (Detailed)**

**1. Coding Standards Review and Refactor**
- Audit all Elisp against `CLAUDE.md` and `STANDARDS.md`.
- Fix all violations (naming, docstrings, autoloads, conventions, hooks).
- Ensure `make check` passes cleanly after refactor.

**2. Declarative/Data-Driven Review and Refactor**
- Translate `DATA_DRIVEN_DECLARATIVE.md` into concrete design targets.
- Define the minimal plan/state data structures and pure functions needed.
- Implement incremental refactors that reduce implicit state and side effects.

**3. Control Plane (CLI + POSIX Wrappers)**
- Implement `supervisorctl` with human-readable defaults and `--json`.
- Define a stable JSON schema and exit codes; add tests for all outputs.
- Use `emacsclient --eval` with explicit server selection flags (`-s`/`-f`).
- Treat server access as privileged; document socket and auth handling.

**4. Service Definition Layer**
- Define a versioned schema for entries and overrides.
- Separate requirement dependencies from ordering dependencies (systemd-style).
- Provide a migration strategy for existing configs.
- Ensure validation errors are explicit and test-covered.

**5. PID 1 Engineering**
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

**6. Security Hardening**
- Lock down control channels (local socket by default).
- If TCP is enabled, require strong auth and protect server files.
- Document the threat model and safe deployment guidance.

**7. Parity and Expansion**
- Add timers and socket activation.
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
