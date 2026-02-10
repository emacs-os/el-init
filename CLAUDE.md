# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

supervisor.el is a single-file Emacs Lisp package for managing background processes. It provides staged startup with dependency ordering, crash recovery, and a dashboard UI.

## Emacs Lisp Standards (MANDATORY)

**CRITICAL:** All code must adhere to `STANDARDS.md` in this repository. This document contains:
- GNU Coding Standards for Emacs Lisp
- Emacs Lisp coding conventions from the official manual
- Naming conventions, predicate suffixes, library loading rules
- Minor/major mode conventions
- Documentation and commenting standards

This is a **hard requirement** for all Elisp projects. Read `STANDARDS.md` before writing or modifying any code.

## Development Commands

```bash
make check          # Run all CI checks (byte-compile, checkdoc, package-lint, ERT tests)
make lint           # Run byte-compile, checkdoc, package-lint only
make test           # Run ERT tests only
make test-one TEST=supervisor-test-parse-string-entry  # Run single test

# Load and test interactively
emacs -Q -l supervisor.el
```

## Pre-commit Requirement

**IMPORTANT:** Always run `make check` and ensure it passes before:
- Committing changes
- Moving to the next task or phase
- Declaring work complete

This matches CI exactly (GitHub Actions runs `make check` on Emacs 28.2, 29.4, and snapshot).
CI failures that pass locally often involve Emacs version differences (e.g., `when-let` → `when-let*` for Emacs 31+).

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
make check   # Must pass before commits (runs lint + test)
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
