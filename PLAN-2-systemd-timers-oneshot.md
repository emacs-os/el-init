# PLAN 2: Basic Interval Timers for Oneshot Services

Roadmap reference: `ROADMAP.md` parity/expansion workstream.

## Objective

Add reliable, easy-to-test interval timers for `oneshot` services using an
Elisp-native configuration model and the existing supervisor engine.

This plan intentionally prioritizes correctness, deterministic behavior, and
implementation simplicity over systemd feature parity.

## Product Constraints (Locked)

- Timer targets are `oneshot` services only.
- Elisp-native config (no systemd unit-file syntax).
- No cron compatibility.
- No calendar expressions.
- No jitter/randomized delay knobs.
- No durable on-disk timer state in this plan.

## Design Decisions (Agreed Defaults)

- Timer mode supported: fixed interval only.
  - Run every `:every-sec` seconds.
  - Optional one-time startup delay via `:initial-delay-sec`.
- Overlap behavior:
  - If target oneshot is still active when due, skip and record
    miss reason `overlap`.
- Disabled behavior:
  - Disabled timer or disabled target is skipped with explicit miss reason.
- Failure behavior:
  - No retry/backoff in this plan.
  - Scheduler continues running other timers if one target fails.

## In Scope

- Timer definition schema and validation.
- Interval scheduler integrated with existing process lifecycle.
- In-memory timer runtime state.
- Timer event emission for observability.
- Basic timer visibility in existing status surfaces (read-only is sufficient).
- Comprehensive tests for interval scheduling and skip/failure semantics.

## Out of Scope

- Calendar scheduling (`:on-calendar`).
- Mixed trigger composition (`:on-startup-sec` + `:on-unit-active-sec` + calendar).
- Retry/backoff policy.
- Missed-run catch-up/persistent semantics.
- Timer state persistence file.
- New timer control subcommands/actions (beyond existing surfaces).

## Configuration Model

Introduce/retain top-level variable:

- `supervisor-timers` (list of timer definitions)

Each timer references a target oneshot service ID in `supervisor-programs`.

Timer fields (planned contract):

- Identity:
  - `:id` string (required)
  - `:target` string (required, must resolve to oneshot service)
  - `:enabled` boolean (default `t`)
- Scheduling:
  - `:every-sec` positive integer seconds (required)
  - `:initial-delay-sec` non-negative integer seconds (optional, default `0`)

Validation requirements:

- Unknown keys rejected.
- `:id` and `:target` must be non-empty strings.
- `:target` must exist and be `oneshot`.
- `:every-sec` required and must be positive integer.
- `:initial-delay-sec`, when present, must be non-negative integer.
- Duplicate timer IDs rejected deterministically.

## Runtime Architecture

### Scheduler Responsibilities

- Compute and maintain per-timer `next-run-at`.
- Trigger oneshot runs via existing start/process primitives.
- Enforce overlap-skip policy.
- Emit structured timer events.
- Maintain deterministic ordering for simultaneous due timers.

### Engine Integration

- Start oneshot via current process lifecycle path.
- Reuse oneshot completion and timeout behavior already in engine.
- Reuse logging and structured event infrastructure.
- Start scheduler only after stage startup completes.
- Never start/restart scheduler during shutdown.

### Timer State Model (In-Memory Only)

Per timer, track at minimum:

- `last-run-at`
- `last-exit`
- `next-run-at`
- `last-missed-at`
- `last-miss-reason`

No on-disk state file in this plan.

## Control Plane and UI Surface

### Dashboard / Status Surfaces

Timer visibility should include:

- timer ID
- target service ID
- enabled state
- next run time
- last run/result
- last miss reason

No timer action UI required in this plan.

### CLI

No new timer command surface required for this plan.
Any existing status output may include timer state as read-only data.

## Failure Semantics

- Scheduler does not silently drop due triggers.
- Every skip/failure path records explicit reason and emits event.
- One timer failure must not stall unrelated timers.
- Overlap never causes double-run for the same target.

## Work Plan

### Phase 1: Schema and Validation

Deliverables:

- Define simplified `supervisor-timers` schema.
- Validate timer->target mapping to oneshot services.
- Add explicit invalid-timer surfacing for user-facing validation commands.

Acceptance criteria:

- Invalid timer definitions are rejected deterministically.
- Valid timers compile into canonical internal representations.

### Phase 2: Interval Scheduler Core

Deliverables:

- Implement fixed interval computation using `:every-sec`.
- Implement optional one-time startup offset using `:initial-delay-sec`.
- Add due-check mechanism using Emacs timers (no busy polling).
- Implement overlap skip, disabled skip, and event emission.

Acceptance criteria:

- Deterministic scheduling with stable ordering for simultaneous due timers.
- No overlap double-run for same timer/target.
- Scheduler does not start during shutdown.

### Phase 3: Observability and Status Integration

Deliverables:

- Expose timer runtime state in existing status/dashboard surfaces (read-only).
- Ensure timer events are emitted and documented for hook consumers.

Acceptance criteria:

- Operators can inspect timer health from existing interfaces.
- Event stream reflects trigger, skip, success, and failure paths.

### Phase 4: Test Matrix and Hardening

Deliverables:

- ERT coverage for:
  - schema/validation failures
  - interval computation correctness
  - initial-delay behavior
  - overlap skip behavior
  - disabled timer/target behavior
  - deterministic simultaneous ordering
  - shutdown guard behavior
  - non-interference between timers

Acceptance criteria:

- `make check` passes with deterministic, repeatable results.
- No known scheduler regressions in documented edge cases.
