# FINDINGS: Stages vs Targets

## Scope
This document compares supervisor.el's current stage based startup model with systemd's target based model, then outlines what it would take to move supervisor.el toward a target model without losing current robustness guarantees.

## Current Supervisor Model (Stage Based)
Current behavior in supervisor.el:

- Fixed pipeline of `stage1`, `stage2`, `stage3`, `stage4`.
- Stages run sequentially.
- Entries inside one stage are scheduled by DAG rules from `:after`, `:requires`, and `:wants`.
- `:requires` across stages is invalid.
- `:after` and `:wants` across stages are ignored with warning.
- Stage completion is strict and readiness aware:
  - all simple units spawned,
  - all blocking oneshots exited or timed out,
  - delayed starts actually fired.
- Cycle detection falls back to deterministic list order.
- Invalid entries are surfaced and skipped.
- Failures do not deadlock dependent scheduling.

Strengths:

- Predictable startup shape.
- Easy mental model for desktop session boot.
- Strong deterministic and failure tolerant behavior in scheduler.
- Good fit for the package's userland scope.

Limits:

- Startup grouping is fixed to four buckets.
- No first class activation graph for arbitrary synchronization points.
- Limited expression for alternate startup goals and "profiles".
- Cross group dependency expression is constrained by design.

## systemd Model (Target Based)
Target model characteristics:

- Targets are graph nodes used as synchronization points.
- Any service can be associated with one or more targets.
- Targets can depend on other targets.
- Ordering and requirement edges are global.
- Activation is transaction based from a requested target set.

Strengths:

- Arbitrary graph composition.
- Multiple entry goals (`multi-user`, `graphical`, custom profiles).
- Flexible dependency expression across the full graph.

Tradeoffs:

- More graph complexity and harder manual reasoning.
- More ways to create subtle configuration problems.

## Key Semantic Delta
Main difference is not just "four stages vs many targets".

The deeper delta is:

- supervisor currently uses stage barriers as startup progress boundaries,
- systemd uses target graph convergence as progress boundaries.

If supervisor moves to targets, it should keep explicit convergence and readiness rules. That is the core of current robustness.

## Non Negotiable Robustness to Preserve
Any target migration should preserve all of the following:

- Deterministic ordering for ties.
- Cycle detection with explicit, visible fallback behavior.
- Invalid unit isolation with reason visibility.
- Readiness aware progression for oneshot, delay, and spawn states.
- Failure of one node must not deadlock unrelated parts of the graph.
- No polling loops introduced into scheduling.
- Existing restart and crash loop policy behavior remains stable.

## Proposed Target Oriented Design for supervisor.el
### 1) Add First Class Target Nodes
Introduce target definitions as explicit graph nodes.

Suggested shape:

- New `supervisor-targets` defcustom.
- Target record fields:
  - id,
  - wants,
  - requires,
  - after,
  - before,
  - description.

### 2) Add Unit to Target Membership
For service units, add target membership keys.

Suggested keys:

- `:wanted-by` list of target ids.
- `:required-by` list of target ids.

Keep existing `:stage` for compatibility during migration.

### 3) Build Global Activation Transaction
Replace stage partitioning as primary startup plan with transaction expansion:

- Start from one or more requested root targets (default root target).
- Expand closure of required and wanted dependencies.
- Build one graph containing units and targets.
- Compute deterministic topological run order with current cycle fallback principles.

### 4) Target Convergence Semantics
Define explicit target completion rules so robustness is not lost:

- Target is reached when all required members are ready under existing readiness semantics.
- Wanted members are best effort and do not block target reach.
- Invalid required members mark target degraded with clear reasons.

This is stricter and more observable than bare systemd target activation, and should improve debuggability.

### 5) Keep Stage Compatibility as Built In Targets
Map legacy stages to internal targets during transition:

- `stage1.target`, `stage2.target`, `stage3.target`, `stage4.target`.
- Default chain preserves current stage order behavior.
- Existing unit files with only `:stage` continue to work unchanged.

### 6) Add Target Aware UX
Dashboard and CLI should gain target operations:

- list targets,
- status of target convergence,
- start to target,
- isolate target style operation,
- explain why target is not reached.

## Where to Change Code
Likely modules touched:

- `supervisor-core.el`
  - plan schema and builder,
  - graph scheduler inputs,
  - startup entry points (`supervisor-start` variants),
  - readiness and convergence accounting.
- `supervisor-units.el`
  - parse and validate new target membership keys.
- `supervisor-dashboard.el`
  - target views, filters, convergence status, drill down.
- `supervisor-cli.el`
  - target commands and output formats.
- `README.org`
  - new model docs and migration guidance.
- `supervisor-test.el`
  - transaction expansion, convergence, compatibility, and failure matrix tests.

## Migration Plan (Low Risk Path)
### Phase A: Data Model and Validation
- Add target schema and unit membership keys.
- Add full validation and clear diagnostics.

### Phase B: Plan Builder Dual Mode
- Keep current stage planner.
- Add target planner behind feature gate.
- Add parity tests for equivalent stage only configs.

### Phase C: Runtime and UX
- Enable target start path.
- Add CLI and dashboard target commands.
- Keep stage view for backward compatibility.

### Phase D: Compatibility and Hardening
- Ensure legacy `:stage` configs map to internal stage targets.
- Add migration helpers and warnings for ambiguous configs.
- Add comprehensive failure and cycle tests.

### Phase E: Default Model Decision
- After parity and field validation, decide whether default startup root is still stage chain or a configurable default target.

## Opportunities to Exceed systemd Robustness
A target rework can add robustness beyond typical systemd visibility:

- Deterministic transaction fingerprints (plan hash in logs).
- First class "why not reached" explanation tree per target.
- Explicit degraded target status with typed causes.
- Safety modes for missing optional members vs hard fail policies.
- Transaction dry run that predicts target reachability before launch.

## Recommendation
Reimagine toward targets, but keep strict convergence semantics and deterministic fallback rules from the current stage engine.

In short:

- adopt target graph flexibility,
- keep supervisor's stronger readiness and failure accounting,
- preserve stage compatibility as an on ramp.

This gives systemd-like composition without sacrificing the current robustness profile.
