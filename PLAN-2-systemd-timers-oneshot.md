# PLAN 2: Systemd-Style Timers for Oneshot Services

Roadmap reference: `ROADMAP.md` parity/expansion workstream.

## Objective

Add reliable, systemd-style timers for `oneshot` services using an
Elisp-native configuration model and the existing supervisor engine.

This plan prioritizes correctness, deterministic behavior, and
implementation simplicity.

## Product Constraints (Locked)

- Timer targets are `oneshot` services only.
- Elisp-native config (no systemd unit-file syntax).
- No cron compatibility (use calendar plists instead).
- No jitter/randomized delay knobs.

## **CRITICAL** Addendum: Experimental Timer Subsystem Gate (Blocking)

This addendum is mandatory for all phases in this plan.
Timer functionality is explicitly experimental and must be strongly gated.

Required mode contract:

- Introduce `supervisor-timer-subsystem-mode` as a global minor mode.
- `supervisor-timer-subsystem-mode` is a child subsystem of `supervisor-mode`.
- Default state is disabled (experimental opt-in).
- If `supervisor-mode` is disabled, timer subsystem behavior must be a no-op even if
  `supervisor-timer-subsystem-mode` is non-nil.

Required modularization contract (MUST):

- Extract timer subsystem implementation into a dedicated module file
  (for example `supervisor-timer.el`).
- Keep timer concerns owned by that module: timer schema/validation, calendar
  computation, scheduler loop, retry/catch-up logic, timer state, and timer
  persistence.
- Keep `supervisor-core.el` as orchestrator/integration layer that calls the
  timer module through a narrow internal API (start/stop/build/list/state hooks).
- Timer subsystem experimental gating must be implemented in/through the module
  boundary, not scattered ad hoc across unrelated files.

Hard gating requirements (MUST):

- When timer subsystem is gated off (or parent `supervisor-mode` is off), timer code must not:
  - build timer plans/lists in startup or reload flows
  - start, tick, or reschedule timer scheduler loops
  - trigger timer-driven oneshot runs
  - execute retry or catch-up logic
  - emit timer-specific events
  - load or save timer-state persistence data
  - render timer sections in dashboard
  - expose active timer runtime data in CLI status surfaces
- Transitioning gate from enabled to disabled (or parent mode to disabled) must:
  - cancel scheduler timers immediately
  - stop further timer side effects in the same session
- CLI behavior while gated off must be explicit (human and JSON) and indicate that
  the timer subsystem is disabled/experimental.

Acceptance requirements (MUST test):

- `supervisor-start` and `supervisor-reload` do not activate timer subsystem when gated off.
- Parent `supervisor-mode` off forces timer subsystem no-op behavior.
- No timer state file I/O occurs while gated off.
- Dashboard/CLI timer surfaces reflect disabled subsystem state.
- Existing timer behavior tests remain valid under gate-on mode.
- Module boundaries are respected: timer internals load from timer module and
  core only depends on the timer module API surface.

## Design Decisions (Implemented)

- Timer triggers supported:
  - `:on-calendar` - cron-style calendar matching (minute/hour/day/month/dow)
  - `:on-startup-sec` - seconds after supervisor-start
  - `:on-unit-active-sec` - seconds after last successful completion
  - Multiple triggers can be combined (earliest wins)
- Overlap behavior:
  - If target oneshot is still active when due, skip and record
    miss reason `overlap`.
- Disabled behavior:
  - Disabled timer or disabled target is skipped with explicit miss reason.
- Failure behavior:
  - Configurable retry policy with exponential backoff (default: 30s, 2m, 10m)
  - Retry budget resets on each fresh scheduled trigger
  - Non-zero exits are retryable; signal deaths are not
  - Scheduler continues running other timers if one target fails.
- Catch-up behavior:
  - Persistent timers can catch up missed runs after downtime
  - Catch-up window is configurable (default: 24 hours)
  - NOTE: Requires timer state persistence (Phase 4) to work across restarts

## In Scope

- Timer definition schema and validation.
- Calendar, startup, and unit-active triggers.
- Interval scheduler integrated with existing process lifecycle.
- In-memory timer runtime state.
- Retry policy with configurable intervals.
- Catch-up logic for persistent timers (requires Phase 4 for cross-restart).
- Timer event emission for observability.
- Comprehensive tests for scheduling and skip/failure semantics.

## Deferred to Phase 4

- Timer state persistence file (enables catch-up across restarts).

## Deferred (Future Work)

- Timer control subcommands/actions (enable/disable timers at runtime).

## Configuration Model

Top-level variable:

- `supervisor-timers` (list of timer definitions)

Each timer references a target oneshot service ID in `supervisor-programs`.

Timer fields:

- Identity:
  - `:id` string (required)
  - `:target` string (required, must resolve to oneshot service)
  - `:enabled` boolean (default `t`)
  - `:persistent` boolean (default `t`, enables catch-up)
- Triggers (at least one required):
  - `:on-calendar` plist with `:minute`, `:hour`, `:day-of-month`, `:month`, `:day-of-week`
  - `:on-startup-sec` positive number
  - `:on-unit-active-sec` positive number

Validation requirements:

- Unknown keys rejected.
- `:id` and `:target` must be non-empty strings.
- `:target` must exist and be `oneshot`.
- At least one trigger must be specified.
- Duplicate timer IDs rejected deterministically.

## Runtime Architecture

### Scheduler Responsibilities

- Compute and maintain per-timer `next-run-at`.
- Trigger oneshot runs via existing start/process primitives.
- Enforce overlap-skip policy.
- Schedule retries on retryable failures.
- Process catch-up runs on startup for persistent timers.
- Emit structured timer events.
- Maintain deterministic ordering for simultaneous due timers.

### Engine Integration

- Start oneshot via current process lifecycle path.
- Reuse oneshot completion and timeout behavior already in engine.
- Reuse logging and structured event infrastructure.
- Start scheduler only after stage startup completes.
- Never start/restart scheduler during shutdown.

### Timer State Model (In-Memory)

Per timer, track at minimum:

- `last-run-at`
- `last-exit`
- `next-run-at`
- `last-missed-at`
- `last-miss-reason`
- `last-success-at`
- `last-failure-at`
- `retry-attempt`
- `retry-next-at`
- `startup-triggered`

## Work Plan

### Phase 1: Schema and Validation ✓

- Timer struct and validation functions
- Calendar plist validation
- Target existence and type validation
- Invalid timer surfacing

### Phase 2: Scheduler Core ✓

- Calendar matching and next-time computation
- Startup and unit-active trigger computation
- Non-polling scheduler loop using Emacs timers
- Overlap detection and skip
- Trigger execution with event emission
- Startup trigger independence (dedicated flag)

### Phase 3: Retry and Catch-up ✓

- Configurable retry policy (30s, 2m, 10m default)
- Retry eligibility gating (positive exits only)
- Retry budget reset on fresh scheduled trigger
- Catch-up logic for persistent timers (cross-restart via Phase 4)

### Phase 4: Durable State Persistence ✓

- Load timer state from `supervisor-timer-state-file` on startup
- Save timer state on significant changes (trigger start, completion)
- Atomic write pattern (temp file + rename) for crash safety
- Only persist relevant keys (last-run-at, last-success-at, etc.)
- Transient keys (retry state, next-run-at) computed fresh each session
- Enable catch-up across Emacs restarts

### Phase 5: Dashboard and CLI ✓

- CLI `timers` command with human and JSON output
- Displays timer ID, target, enabled, last-run, next-run, exit code, miss reason
- Relative time formatting for timestamps (e.g., "5m ago", "in 2h")
- Shows invalid timers with reasons

### Phase 6: Test Matrix and Hardening ✓

- 68 timer-related tests (validation, scheduling, triggers, retry, catch-up)
- Scheduler tick edge cases (scheduled, retry, disabled skip)
- State persistence merge/roundtrip tests
- CLI timers command tests (human, JSON, invalid)
- Relative time formatter edge cases
- Multiple trigger type interaction tests
