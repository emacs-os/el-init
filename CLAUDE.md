# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

supervisor.el is a single-file Emacs Lisp package for managing background processes. It provides staged startup with dependency ordering, crash recovery, and a dashboard UI.

## Development Commands

```bash
# Byte-compile (must pass with no warnings)
emacs -Q --batch -f batch-byte-compile supervisor.el

# Run checkdoc (must pass with no errors)
emacs -Q --batch --eval "(checkdoc-file \"supervisor.el\")"

# Run package-lint (must pass for MELPA)
emacs -Q --batch -l package-lint --eval "(package-lint-batch-and-exit \"supervisor.el\")"

# Run ERT tests
emacs -Q --batch -l supervisor.el -l supervisor-test.el -f ert-run-tests-batch-and-exit

# Run a single test
emacs -Q --batch -l supervisor.el -l supervisor-test.el --eval "(ert-run-tests-batch-and-exit 'test-name)"

# Load and test interactively
emacs -Q -l supervisor.el
```

## Architecture

### Entry Parsing
`supervisor--parse-entry` converts user config into an 11-element list:
`(id cmd delay enabled-p restart-p logging-p type stage after oneshot-wait oneshot-timeout)`

All internal functions work with this parsed format, not raw config entries.

### Staged Startup Flow
1. `supervisor-start` parses all entries and partitions by stage (early→services→session→ui)
2. `supervisor--start-stages-async` processes stages sequentially via continuation-passing
3. Within each stage, `supervisor--dag-init` builds a dependency graph from `:after` declarations
4. Entries with in-degree 0 start immediately; others wait for dependencies
5. `supervisor--dag-mark-ready` is called when a process is ready (spawned for simple, exited for oneshot)

### Process Types
- **simple**: Long-running daemons. "Ready" when spawned. Restarts on crash.
- **oneshot**: Run-once scripts. "Ready" when exited (success or failure) or timed out. No restart.

### State Management
All runtime state lives in `supervisor--*` hash tables:
- `supervisor--processes`: id → process object
- `supervisor--failed`: ids that have crash-looped
- `supervisor--restart-override`: runtime restart policy overrides
- `supervisor--oneshot-completed`: id → exit code

DAG scheduler state (`supervisor--dag-*` variables) is per-stage and reset between stages.

### Dashboard
`supervisor-dashboard-mode` extends `tabulated-list-mode`. The dashboard reads from both config (`supervisor-programs`) and runtime state hash tables to display current status.

## Code Quality Standards

See PLAN-INIT-followups.md for the authoritative spec. Key requirements:

### Validation
- Entry options must be validated against a whitelist before starting
- Invalid entries are skipped but surfaced in the dashboard with status `invalid` and a reason
- Mutually exclusive options (`:restart`/`:no-restart`, `:enabled`/`:disabled`) must be detected
- Type-specific options must be enforced (`:restart` invalid for oneshot, `:oneshot-wait` invalid for simple)

### Async Scheduling
- No polling loops—use sentinels and timers only
- Cycle detection must fall back to list order and clear `:after` edges
- Disabled entries are immediately ready
- Start failures must not block dependents or stages
- Delayed entries must not allow early stage completion

### Stage Completion Criteria
A stage is complete only when:
1. All simple processes have spawned
2. All blocking oneshots have exited or timed out
3. All delayed entries have actually started

### Testing
- Use ERT for all tests
- If a behavior can be tested, it must be tested
- Required test coverage: whitelist validation, cycle fallback, stable ordering, oneshot timeout unlock, delayed entry handling, async oneshot non-blocking

## MELPA Standards

This package must be structured for [MELPA](https://github.com/melpa/melpa/blob/master/CONTRIBUTING.org) submission.

### Required Checks
```bash
# All must pass with no errors/warnings before commits
emacs -Q --batch -f batch-byte-compile supervisor.el
emacs -Q --batch --eval "(checkdoc-file \"supervisor.el\")"
emacs -Q --batch -l package-lint --eval "(package-lint-batch-and-exit \"supervisor.el\")"
```

### Code Style
- Use `#'function` syntax (not `'function`) for function references
- Decompose long functions into smaller documented functions
- All public functions and variables need docstrings
- Follow Emacs Lisp naming conventions (`supervisor-` public, `supervisor--` private)

### Package Structure
- Lexical binding required (already present)
- Standard headers: Author, Version, Package-Requires, Keywords, URL
- GPL-compatible license with boilerplate above `;;; Commentary:`
- Must include LICENSE file
- Feature name must match filename (`(provide 'supervisor)`)
