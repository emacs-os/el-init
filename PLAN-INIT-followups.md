# Supervisor Follow-ups Spec (Authoritative)

This document is normative. Implementations MUST follow the requirements below. Where priorities are listed, P0 is required, P1 is recommended, and P2 is optional.

## Scope

This spec defines follow-up requirements for the supervisor system described in `PLAN-init.md`. It covers validation, scheduling, logging, testing, and UX behavior.

## Definitions

- Entry: One element of `supervisor-programs` after parsing and normalization.
- Unit: Synonym for Entry.
- Stage: One of `early`, `services`, `session`, `ui`.
- Ready: A dependency is ready when a simple process is spawned or a oneshot has exited (success or failure) or timed out.
- Blocking oneshot: A oneshot that blocks stage completion.
- Async oneshot: A oneshot that does not block stage completion.
- Stage completion: All simple processes spawned and all blocking oneshots completed or timed out.
- Valid entry: An entry that conforms to the whitelist rules.
- Invalid entry: An entry that violates any whitelist rule.

## Priority Legend

- P0: MUST implement
- P1: SHOULD implement
- P2: MAY implement

## P0 Requirements

### 1) Configuration Validity Whitelist

- MUST define a finite whitelist of valid option combinations for `supervisor-programs`.
- MUST explicitly encode constraints and aliases. Examples include:
  - `:restart` and `:no-restart` are mutually exclusive.
  - `:enabled` and `:disabled` are mutually exclusive.
  - `:async t` is equivalent to `:oneshot-wait nil`.
  - `:oneshot-timeout` must be a number or nil.
  - `:restart` and `:no-restart` are invalid for `:type oneshot`.
  - `:oneshot-wait`, `:oneshot-timeout`, and `:async` are invalid for `:type simple`.
- MUST define and document the whitelist using a precise, finite description.
- MUST validate all entries at the start of `supervisor-start`.
- MUST allow the supervisor to continue even if some entries are invalid.
- MUST start only valid entries; invalid entries are skipped.
- MUST surface invalid entries in the dashboard with status `invalid` and a reason string.
- MUST log a warning per invalid entry including the entry ID and reason.

### 2) Scheduler Semantics (Async DAG)

- MUST use async callbacks (sentinels and timers). Polling loops are not allowed.
- MUST start entries within a stage based on a DAG of `:after` dependencies.
- MUST allow unrelated entries to start in parallel.
- MUST treat `:after` as waiting for ready state (simple spawn, oneshot exit or timeout).
- MUST preserve stable ordering for unconstrained nodes using original list order.
- MUST fall back to list order if a cycle is detected, and MUST clear `:after` edges for that stage to prevent stalls.
- MUST NOT mark delayed entries as started until the delay elapses and spawn begins.
- MUST allow oneshot timeouts to unlock dependents and stage completion with a warning.
- MUST treat disabled entries as ready immediately.
- MUST clean up all DAG state on stage completion, shutdown, and restart.

### 3) Stage Completion

- MUST consider a stage complete only when:
  - All simple processes in the stage have spawned.
  - All blocking oneshots in the stage have exited or timed out.
  - All delayed entries have actually started.

### 4) Logging and Observability

- MUST provide a `supervisor-verbose` toggle.
- When `supervisor-verbose` is non-nil, the system MUST `message` all meaningful events.
- Events that MUST be logged in verbose mode include:
  - stage start and completion
  - entry start, skip, and failure
  - dependency unlocks
  - oneshot timeouts
  - restarts and crash-loop suppression
- When `supervisor-verbose` is nil, only warnings and errors should be logged.

### 5) Dashboard Requirements

- MUST display stage, enabled state, status, restart, logging, PID, and invalid state if applicable.
- MUST show `failed` for oneshots with non-zero exit.
- MUST show `invalid` for invalid entries, with a reason string.

### 6) Global Minor Mode

- MUST provide a global minor mode named `supervisor-mode`.
- `supervisor-mode` enables `supervisor-start` and disables `supervisor-stop`.
- `C-h f` on `supervisor-mode` MUST show accurate help and a sample config.

### 7) Testing Policy

- MUST use ERT.
- If a test can be written, it MUST be written.
- MUST include tests for:
  - whitelist validation and invalid entry handling
  - cycle fallback behavior
  - stable ordering of unconstrained nodes
  - oneshot timeout unlock behavior
  - delayed entries not completing stages early
  - async oneshots not blocking stage completion

### 8) Retries and Resilience

- MUST keep restart behavior bounded (crash-loop detection).
- MUST honor restart delay and cancel pending restart timers on shutdown.
- MUST not allow start failures to block dependents or stages.

## P1 Requirements (Recommended)

- SHOULD implement `M-x supervisor-validate` to run the whitelist checks without starting any processes.
- SHOULD implement `supervisor-reload` to reconcile config changes without restarting Emacs.
- SHOULD add a stage-level timeout (`supervisor-stage-timeout`) to avoid stalls.
- SHOULD add `supervisor-max-concurrent-starts` to avoid thundering-herd start bursts.
- SHOULD add hooks:
  - `supervisor-stage-start-hook`
  - `supervisor-stage-complete-hook`
  - `supervisor-process-exit-hook` with ID, status, exit code
- SHOULD differentiate signal death vs non-zero exit in diagnostics.
- SHOULD provide a `supervisor-describe-entry` command for resolved config display.
- SHOULD add a dashboard toggle to filter by stage or show dependency edges.
- SHOULD add a supervisor-level log file when verbose is enabled.
- SHOULD add runtime enable/disable overrides (parallel to restart overrides).
- SHOULD remove or gate demo entries (e.g., `/bin/false`) before release builds.

## P2 Requirements (Optional)

- MAY implement a file watch that triggers `supervisor-reload` on config changes.
- MAY add per-unit tags for dashboard filtering.
- MAY add a dry-run mode that validates and orders entries without starting them.

## Product Positioning: "Systemd Jab" Requirements (P0)

These requirements are intended to sharpen the contrast with systemd by making
the user-session supervisor simpler, more transparent, and more interactive.

- MUST explicitly state scope in docs/help: this is a user-session supervisor,
  not an init system / PID1 replacement.
- MUST provide runtime inspectability:
  - dashboard can show resolved config for an entry (effective defaults applied)
  - dashboard can show current dependency edges or computed DAG order
- MUST keep logging simple and inspectable: per-process log files remain plain
  files with timestamps/rotation (no opaque logging backend).
- MUST keep configuration explicit and validated:
  - whitelist validation with precise reasons (already required above)
  - no implicit, undocumented defaults in user-facing behavior
- MUST keep startup ordering deterministic and explainable:
  - stable ordering and explicit dependency semantics (already required above)
  - "ready" semantics must be documented in README and `C-h f` help
- MUST keep the dashboard interactive:
  - runtime overrides for restart and logging are visible and clear
  - entry status clearly explains "why not running"
- MUST track and expose startup timing:
  - record per-entry start timestamps and ready timestamps
  - provide a "blame"-style view that ranks entries by startup duration
- MUST calculate and expose the dependency graph:
  - computed DAG edges must be available for inspection
  - dashboard key `d` (or similar) must open a dependency view for the entry at point
- MUST include a small ASCII boot banner in the dashboard header that reflects
  stage progress (ticks or similar).
- MUST provide a `supervisor-validate` command that checks config without
  starting any processes (already recommended above; promoted to P0 here).
- MUST include a short "Why supervisor.el exists" section in README, contrasting
  the user-session focus and transparency without being a rant.

## MELPA Submission Requirements

This package MUST be structured for MELPA submission. See [MELPA CONTRIBUTING.org](https://github.com/melpa/melpa/blob/master/CONTRIBUTING.org) for full details.

### P0 (Required for submission)

- MUST use lexical binding (`-*- lexical-binding: t; -*-` on first line).
- MUST include standard Emacs package headers:
  - `Author:`, `Version:`, `Package-Requires:`, `Keywords:`, `URL:`
- MUST declare `Package-Requires: ((emacs "27.1"))` or appropriate minimum version.
- MUST include GPL-compatible license boilerplate above `;;; Commentary:`.
- MUST include a LICENSE or COPYING file (GPLv3 recommended).
- MUST pass `package-lint` with no errors.
- MUST pass `checkdoc` with no errors.
- MUST pass `byte-compile` with no warnings.
- MUST provide the feature matching the filename (`(provide 'supervisor)`).
- MUST have `;;; Commentary:` section with usage overview.
- MUST have `;;; Code:` section marker.
- MUST end with `;;; supervisor.el ends here`.

### P1 (Recommended for quality)

- SHOULD pass `flycheck-package` checks.
- SHOULD follow Emacs Lisp conventions and style guide.
- SHOULD use `#'function` syntax instead of `'function` for function references.
- SHOULD decompose long functions into smaller, well-named documented functions.
- SHOULD avoid faces that both inherit and override attributes.
- SHOULD include an `.info` manual for complex packages.

### Pre-submission Checklist

```bash
# Required checks before MELPA PR
emacs -Q --batch -f batch-byte-compile supervisor.el
emacs -Q --batch --eval "(checkdoc-file \"supervisor.el\")"
emacs -Q --batch -l package-lint --eval "(package-lint-batch-and-exit \"supervisor.el\")"
```

## Helpful Guidance for Invalid Configs

- MUST emit a reason string for each invalid entry.
- SHOULD include a short hint if the fix is obvious and unambiguous.
- Examples of acceptable hints:
  - “`:restart` is invalid for `:type oneshot`; consider removing it.”
  - “Both `:enabled` and `:disabled` are set; remove one.”
  - “Unknown `:stage` value; use one of: `early`, `services`, `session`, `ui`.”

## Final Verification Checklist

### Functionality
- [ ] Invalid entries are detected, skipped, and shown as `invalid` with reasons
- [ ] Cycle fallback clears `:after` edges and uses list order
- [ ] Stable ordering preserved for unconstrained nodes
- [ ] Delayed entries do not allow early stage completion
- [ ] Oneshot timeout unlocks dependents and stage completion
- [ ] Async oneshots do not block stage completion
- [ ] Stage completion criteria match the spec
- [ ] Verbose logging emits all required events
- [ ] Global minor mode help is correct and discoverable
- [ ] ERT coverage exists for all P0 behaviors

### MELPA Readiness
- [ ] `byte-compile` passes with no warnings
- [ ] `checkdoc` passes with no errors
- [ ] `package-lint` passes with no errors
- [ ] Package headers complete (Author, Version, Package-Requires, Keywords, URL)
- [ ] GPL license boilerplate present above `;;; Commentary:`
- [ ] LICENSE file exists in repository
- [ ] `;;; Commentary:` section has usage overview
- [ ] Feature provided matches filename
