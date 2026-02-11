# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

supervisor.el is an Emacs Lisp package for managing background processes. It provides staged startup with dependency ordering, crash recovery, and a dashboard UI.

## Module Structure

The package is split into focused modules:

| File | Purpose |
|------|---------|
| `supervisor-core.el` | Engine, parsing, scheduling, process lifecycle, state management |
| `supervisor-dashboard.el` | UI rendering, keymaps, interactive commands |
| `supervisor-cli.el` | CLI dispatcher, formatters, command handlers |
| `supervisor.el` | Facade that loads all modules and provides the `supervisor` feature |

**Load order:** core → dashboard → cli → facade.

**Dependency rules:**
- `supervisor-core.el` has no dependencies on dashboard or CLI (can load standalone)
- `supervisor-dashboard.el` requires only `supervisor-core`
- `supervisor-cli.el` requires only `supervisor-core`
- `supervisor.el` requires all modules

Cross-module calls use `declare-function` for proper byte-compilation.

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

### Entry Parsing (Schema v1)
`supervisor--parse-entry` converts user config into a 13-element list:
`(id cmd delay enabled-p restart-p logging-p type stage after oneshot-wait oneshot-timeout tags requires)`

Use accessor functions (`supervisor-entry-id`, `supervisor-entry-command`, etc.) instead of direct indexing.
The `supervisor-service` struct provides a canonical schema v1 representation with conversion functions.

### Staged Startup Flow
1. `supervisor-start` builds a plan using `supervisor--build-plan` (pure, deterministic)
2. Plan contains entries partitioned by stage (stage1→stage2→stage3→stage4), pre-sorted
3. `supervisor--start-stages-from-plan` processes stages sequentially via continuation-passing
4. Within each stage, `supervisor--dag-init` builds a dependency graph from `:after` and `:requires` declarations
5. Entries with in-degree 0 start immediately; others wait for dependencies
6. `supervisor--dag-mark-ready` is called when a process is ready (spawned for simple, exited for oneshot)

### Dependency Model
- `:after` - ordering only (controls start order, same stage)
- `:requires` - pull-in + ordering (same stage, cross-stage is an error)
Both are combined for topological sorting.

### Process Types
- **simple**: Long-running daemons. "Ready" when spawned. Restarts on crash.
- **oneshot**: Run-once scripts. "Ready" when exited (success or failure) or timed out. No restart.

### State Management
All runtime state lives in `supervisor--*` hash tables:
- `supervisor--processes`: id → process object
- `supervisor--failed`: ids that have crash-looped
- `supervisor--restart-override`: runtime restart policy overrides
- `supervisor--enabled-override`: runtime enabled policy overrides
- `supervisor--oneshot-completed`: id → exit code

DAG scheduler state (`supervisor--dag-*` variables) is per-stage and reset between stages.

### Persistent Overrides
Override state is persisted to `supervisor-overrides-file` (default: `~/.local/state/supervisor/overrides.eld`).
- Loaded on `supervisor-start` after clearing runtime state
- Saved on dashboard toggle actions
- Uses atomic write pattern (temp file + rename)
- Corrupt files are logged and preserved, not deleted

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
  - Exception: `supervisor-stop-now` uses a brief polling loop (max 0.5s) for synchronous
    shutdown in `kill-emacs-hook`, where async completion is not possible
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

## Documentation Files

- **README.org**: The authoritative user handbook. Requirements:
  - **Source of truth**: Must accurately reflect the codebase (the actual source of truth)
  - **Handbook style**: Thorough reference work, not a quick-start guide
  - **Complete coverage**: Document every single feature, option, and configuration possible
  - **Indexed and searchable**: Well-organized with clear headings for navigation
  - **No aspirational content**: Only document what currently exists in the code
  - **Human-readable**: Clear prose suitable for end users, not developer notes
- **ROADMAP.md**: Future plans and ideas. Not yet implemented.
- **CLAUDE.md**: Development guidance for AI assistants.
- **STANDARDS.md**: Emacs Lisp coding standards (mandatory).

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
- Lexical binding required in all module files
- Standard headers: Author, Version, Package-Requires, Keywords, URL (in facade)
- GPL-compatible license with boilerplate above `;;; Commentary:`
- Must include LICENSE file
- Main feature provided by facade: `(provide 'supervisor)`
- Each module provides its own feature: `supervisor-core`, `supervisor-dashboard`, `supervisor-cli`
