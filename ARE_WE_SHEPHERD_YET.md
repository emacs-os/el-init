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

## Roadmap (Concrete Phases)

1. Control plane: implement `supervisorctl` with human and JSON output.
2. Service definition layer: formal schema and persistent overrides.
3. PID 1 engineering: reaping, signal handling, safe shutdown, crash safety.
4. Security hardening: restrict control channel and document threat model.
5. Parity and beyond: timers, socket activation, advanced readiness.
6. Scheduler hardening: move fully to event-driven completion for shutdown and
   oneshots, eliminating periodic polling timers.

## Scheduler Hardening Details (Future)

- Shutdown: track a live-process counter, decrement in sentinels, and complete
  when the counter reaches zero.
- Shutdown timeout: use a single one-shot timer to SIGKILL survivors, then
  complete.
- Oneshot completion: rely on process sentinels for success/failure, with a
  single one-shot timeout timer as a fallback.
- Remove repeating timer checks that poll process state; prefer sentinels plus
  one-shot timers only.
- Ensure completion callbacks fire exactly once, including when no processes
  are running at shutdown start.

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
