# Data-Driven, Functional, Declarative, Atomic

This document is a research note on how supervisor.el could evolve toward a
more data-driven, functional, declarative, and atomic architecture over time.
It is not an implementation plan. It is intended to guide future design work.

## Why These Properties Matter

- Data-driven: behavior emerges from explicit data structures, not scattered
  control flow. This improves predictability, auditability, and tooling.
- Functional: prefer pure functions and immutable inputs for scheduling and
  decision-making. This reduces hidden state and makes testing easier.
- Declarative: users describe desired state, the system converges to it. This
  reduces imperative configuration and clarifies intent.
- Atomic: state transitions are explicit and consistent. Partial failures
  should not leave the system in ambiguous or half-applied states.

## Current State (High-Level)

supervisor.el already has some building blocks that align with these goals:

- A parsed entry format that normalizes configuration.
- A DAG scheduler that separates dependency resolution from execution.
- Validation logic that enforces an explicit whitelist.
- Runtime state tracked in dedicated hash tables.

However, the system is still primarily imperative. Much of the runtime behavior
is expressed as side effects across functions and hash tables that can be
updated from multiple paths.

## Design Direction

### 1) Make the Scheduler Data-Driven

Goal: all scheduling decisions are based on a single immutable input object and
produce a single immutable output object.

Potential design:

- Define a `supervisor-plan` data structure that includes:
  - Entries (normalized)
  - Computed deps
  - Stage ordering
  - Derived fields (in-degree, dependents)
- Generate this plan with pure functions.
- The runtime engine consumes the plan, but does not mutate it.
- Track runtime state in a separate, explicit state object.

This enables:

- Deterministic scheduling (same input plan yields same ordering).
- Offline tools that inspect or simulate the scheduler.
- Clear separation between planning and execution.

### 2) Move to Declarative Desired State

Goal: treat supervisor.el as a desired-state engine rather than a procedural
launcher.

Proposed model:

- Desired state is a data structure: `desired = { enabled, stage, type, after }`.
- Actual state is observed: `actual = { running, exit-code, started-at }`.
- Reconciliation compares desired vs actual and emits actions.

This unlocks:

- A clean `supervisor-reload` behavior.
- Idempotent operations (running `start` twice is safe).
- A better CLI and automation story.

### 3) Atomic Transitions

Goal: avoid partial application of configuration changes.

Possible approach:

- Stage changes in an intermediate plan.
- Validate and compute all derived state up front.
- Apply changes only if the plan is consistent.
- If validation fails, reject the entire change set.

This also encourages a two-phase model:

1. Plan: compute and validate all changes.
2. Apply: execute changes with precise logging and rollback points.

### 4) Pure Functions for Core Logic

Identify core logic that should be pure:

- Entry parsing
- Dependency validation
- DAG construction
- Stable ordering
- Stage completion checks
- Status computation

Benefits:

- Easier ERT testing (no global state required).
- Easier auditing for correctness.
- Compatibility with a future CLI or daemon split.

### 5) Explicit State Machines

For each entry, define a clear finite state machine:

- `pending` -> `starting` -> `ready` -> `running` -> `stopped`
- `failed` as a terminal or recoverable state based on policy

For oneshots:

- `pending` -> `starting` -> `exited` -> `ready`

The FSM should be declarative, not implicit. This makes it possible to
generate status outputs and reason strings directly from state transitions.

### 6) Declarative Hooks and Events

Rather than scattering `message` calls and manual state updates:

- Emit structured events (`entry-start`, `entry-ready`, `entry-failed`).
- Provide a single event dispatcher that fans out to hooks.
- Allow hooks to be data-driven: map event type to handler list.

This makes it easier to build:

- Observability tooling
- Alternative UIs
- Structured logging

### 7) Configuration as Data, Not Code

Longer-term, consider moving from Elisp config to a strict data format:

- Emacs Lisp is powerful but makes static analysis difficult.
- A restricted schema (JSON, S-expressions, or a data DSL) improves validation.
- A schema version field enables safe migrations.

This should remain optional for now, but the architecture can prepare for it.

## Concrete Future Steps

Short-term ideas that move in this direction without a rewrite:

- Add a `supervisor-plan` data structure and generate it in `supervisor-start`.
- Add pure helper functions for:
  - `supervisor--compute-deps`
  - `supervisor--compute-status`
  - `supervisor--compute-reasons`
- Move dashboard rendering to consume a computed snapshot, not live state.
- Add a `supervisor-dry-run` command that returns the computed plan and status.

Medium-term:

- Introduce a reconciler: `supervisor-apply-plan`.
- Split process execution from scheduling decisions.
- Introduce a test harness that runs the scheduler purely in memory.

Long-term:

- Evaluate a minimal daemon mode with a stable RPC interface.
- Move to a declarative config file format with versioning.

## Risks and Tradeoffs

- Increased complexity: data models add up-front overhead.
- Performance: immutable structures can allocate more.
- Migration: existing config conventions should continue to work.
- Over-engineering: some features might not justify a full declarative engine.

The guiding principle should be: introduce these changes only where they reduce
ambiguity and improve testability, not just for architectural purity.

## Summary

A data-driven, functional, declarative, and atomic supervisor is achievable in
Emacs Lisp. The key is to separate planning from execution, adopt explicit
state machines, and drive behavior from structured data with pure transforms.
This should be done incrementally, aligning each step with real product needs.
