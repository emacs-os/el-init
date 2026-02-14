# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

supervisor.el is an Emacs Lisp package for managing background processes. It provides staged startup with dependency ordering, crash recovery, and a dashboard UI.

## Module Structure

The package is split into focused modules:

| File | Purpose |
|------|---------|
| `supervisor-core.el` | Engine, parsing, scheduling, process lifecycle, state management |
| `supervisor-units.el` | Unit-file loading, validation, merge with legacy config |
| `supervisor-dashboard.el` | UI rendering, keymaps, interactive commands |
| `supervisor-cli.el` | CLI dispatcher, formatters, command handlers |
| `supervisor.el` | Entry point that loads all modules and provides the `supervisor` feature |

**Load order:** core → units → timer → dashboard → cli (loaded by entry point).

**Dependency rules:**
- `supervisor-core.el` has no dependencies on other modules (can load standalone)
- `supervisor-units.el` uses `declare-function` for core (no hard require)
- `supervisor-dashboard.el` requires only `supervisor-core`
- `supervisor-cli.el` requires only `supervisor-core`
- `supervisor.el` requires all modules

Cross-module calls use `declare-function` for proper byte-compilation.

## Emacs Lisp Standards (MANDATORY)

**CRITICAL:** All code must adhere to STANDARDS in this repository. This document contains:
- GNU Coding Standards for Emacs Lisp
- Emacs Lisp coding conventions from the official manual
- Naming conventions, predicate suffixes, library loading rules
- Minor/major mode conventions
- Documentation and commenting standards

This is a **hard requirement** for all Elisp projects. Read before writing or modifying any code.

## Git Workflow (MANDATORY)

Use only basic git commands. **Never** use `git commit --amend`, `git rebase`,
`git reset`, or any history-rewriting commands. Every commit is a new commit.

```bash
git add <specific-files>
git commit -m "message"
git push
```

Rules:
- **No amending**: if a commit needs a fix, make a new commit on top.
- **No resets**: if something went wrong, fix forward with a new commit.
- **No rebasing**: keep a linear append-only history.
- **Always push** after committing to keep local and remote in sync.
- **Always `make check`** before committing.

## Development Commands

```bash
make check          # Run all CI checks (byte-compile, checkdoc, package-lint, ERT tests)
make lint           # Run byte-compile, checkdoc, package-lint only
make test           # Run ERT tests only
make test-one TEST=supervisor-test-parse-string-entry  # Run single test

# Load and test interactively
emacs -Q -l supervisor.el
```

## Emacs as a Reference Tool

Look up function documentation from the shell when you need to confirm behavior:

```bash
emacs --batch --eval "(princ (documentation 'process-live-p))"
```

More generally, use `emacs --batch --eval "..."` to test expressions and confirm
assumptions. Do this often rather than guessing.

The full Emacs source code is available locally at `~/repos/emacs` for reading
implementation details, understanding C primitives, or checking how built-in
modes work.

## Pre-commit Requirement

**IMPORTANT:** Always run `make check` and ensure it passes before:
- Committing changes
- Moving to the next task or phase
- Declaring work complete

This matches CI exactly (GitHub Actions runs `make check` on Emacs 28.2, 29.4, and snapshot).
CI failures that pass locally often involve Emacs version differences (e.g., `when-let` → `when-let*` for Emacs 31+).

## Architecture

### Entry Parsing (Schema v1)
`supervisor--parse-entry` converts user config into a 27-element list:
`(id cmd delay enabled-p restart-p logging-p type stage after oneshot-blocking oneshot-timeout tags requires working-directory environment environment-file exec-stop exec-reload restart-sec description documentation before wants kill-signal kill-mode remain-after-exit success-exit-status)`

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
- `supervisor--mask-override`: id → `masked` or nil
- `supervisor--oneshot-completed`: id → exit code
- `supervisor--manually-stopped`: ids stopped via CLI/dashboard (suppresses restart)
- `supervisor--manually-started`: ids started via CLI/dashboard (session-only, does not change enabled state)

DAG scheduler state (`supervisor--dag-*` variables) is per-stage and reset between stages.

### Persistent Overrides
Override state is persisted to `supervisor-overrides-file` (default: `~/.local/state/supervisor/overrides.eld`).
- Loaded on `supervisor-start` after clearing runtime state
- Saved on dashboard toggle actions
- Uses atomic write pattern (temp file + rename)
- Corrupt files are logged and preserved, not deleted

### Dashboard
`supervisor-dashboard-mode` extends `tabulated-list-mode`. The dashboard reads from unit files (via `supervisor--effective-programs`) and runtime state hash tables to display current status.

## Code Quality Standards

See PLAN-INIT-followups.md for the authoritative spec. Key requirements:

### Validation
- Entry options must be validated against a whitelist before starting
- Invalid entries are skipped but surfaced in the dashboard with status `invalid` and a reason
- Mutually exclusive options (`:restart`/`:no-restart`, `:enabled`/`:disabled`) must be detected
- Type-specific options must be enforced (`:restart` invalid for oneshot, `:oneshot-blocking` invalid for simple)

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
  - **Two-part style**: The intro through Quick Start is beginner-friendly and welcoming,
    explaining what the package does and who it's for. Everything after Quick Start is
    thorough technical reference handbook style.
  - **Complete coverage**: Document every single feature, option, and configuration possible
  - **Indexed and searchable**: Well-organized with clear headings for navigation
  - **No aspirational content**: Only document what currently exists in the code
  - **Human-readable**: Clear prose suitable for end users, not developer notes
  - **Keep in sync**: When modifying code, update the associated handbook documentation
- **ROADMAP.md**: Future plans and ideas. Not yet implemented.
- **CLAUDE.md**: Development guidance for AI assistants.

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
- Standard headers: Author, Version, Package-Requires, Keywords, URL (in entry point)
- GPL-compatible license with boilerplate above `;;; Commentary:`
- Must include LICENSE file
- Main feature provided by entry point: `(provide 'supervisor)`
- Each module provides its own feature: `supervisor-core`, `supervisor-units`, `supervisor-timer`, `supervisor-dashboard`, `supervisor-cli`

**CRITICAL: All code must follow the GNU Coding Standards and Emacs Lisp conventions.**

Full references:
- GNU Coding Standards: https://www.gnu.org/prep/standards/
- Emacs Lisp Tips and Conventions: https://www.gnu.org/software/emacs/manual/html_node/elisp/Tips.html

### Naming Conventions

- All global symbols use the package prefix `supervisor-` (public) or `supervisor--` (private)
- Predicates: one-word names end in `p` (e.g., `framep`), multi-word in `-p` (e.g., `frame-live-p`)
- Boolean variables: use `-flag` suffix or `is-foo`, not `-p` (unless bound to a predicate function)
- Function-storing variables: end in `-function`; hook variables: follow hook naming conventions
- File/directory variables: use `file`, `file-name`, or `directory`, never `path` (reserved for search paths)
- No `*var*` naming convention; that is not used in Emacs Lisp

### Coding Conventions

- Lexical binding required in all files
- Put `(provide 'feature)` at the end of each file
- Use `require` for hard dependencies; `(eval-when-compile (require 'bar))` for compile-time-only macro dependencies
- Minimize runtime `require`; use `autoload` or deferred `require` inside functions when possible
- Use `cl-lib`, never the deprecated `cl` library
- Never use `defadvice`, `eval-after-load`, or `with-eval-after-load`
- Never redefine or advise Emacs primitives
- Loading a package must not change editing behavior; require explicit enable/invoke commands
- Use default indentation; never put closing parens on their own line
- Remove all trailing whitespace; use `?\s` for space character (not `? `)
- Source files use UTF-8 encoding

### Key Binding Conventions

- `C-c LETTER` is reserved for users -- never bind these in packages
- `F5`-`F9` (unmodified) are reserved for users
- `C-c CTRL-CHAR` or `C-c DIGIT`: reserved for major modes
- `C-c { } < > : ;`: reserved for major modes
- `C-c` + other punctuation: allocated for minor modes
- Never bind `C-h` after a prefix (it auto-provides help)
- Never bind sequences ending in `ESC` (except after another `ESC`) or `C-g`

### Documentation Strings

- Every public function and variable needs a docstring
- First line: complete sentence, imperative voice, capital letter, ends with period, max 74 chars
- Function docstrings: "Return X." not "Returns X." Active voice, present tense.
- Argument references: UPPERCASE (e.g., "Evaluate FORM and return its value.")
- Symbol references: lowercase with backtick-quote (e.g., `` `lambda' ``) -- except t and nil unquoted
- Predicates: start with "Return t if"
- Boolean variables: start with "Non-nil means"
- User options: use `defcustom`
- Use `\\[command]` for key bindings, not literal key sequences
- Don't indent continuation lines to align with first line in source
- No leading/trailing whitespace in docstrings

### Comment Conventions

- `;` -- right-aligned inline comments on code lines
- `;;` -- indented to code level, describes following code or program state
- `;;;` -- left margin, section headings (Outline mode); `;;;;` for sub-sections
- Standard file sections: `;;; Commentary:`, `;;; Code:`, `;;; filename ends here`

### File Headers

Each `.el` file needs the standard header:
```elisp
;;; foo.el --- Description  -*- lexical-binding: t; -*-
;; Copyright, Author, Keywords, URL, Package-Requires headers
;; License boilerplate
;;; Commentary:
;;; Code:
;; ... code ...
(provide 'foo)
;;; foo.el ends here
```

### Programming Tips

- Use `forward-line` not `next-line`/`previous-line`
- Don't call `beginning-of-buffer`, `end-of-buffer`, `replace-string`, `replace-regexp`, `insert-file`, `insert-buffer` in programs
- Use `message` for echo area output, not `princ`
- Use `error` or `signal` for error conditions (not `message`, `throw`, `sleep-for`, `beep`)
- Error messages: capital letter, no trailing period. Optionally prefix with `symbol-name:`
- Minibuffer prompts: questions end with `?`, defaults shown as `(default VALUE)`
- Progress messages: `"Operating..."` then `"Operating...done"` (no spaces around ellipsis)
- Prefer lists over vectors unless random access on large tables is needed
- Prefer iteration over recursion (function calls are slow in Elisp)
- Use `memq`, `member`, `assq`, `assoc` over manual iteration

### Compiler Warnings

- Use `(defvar foo)` to suppress free variable warnings
- Use `declare-function` for functions known to be defined elsewhere
- Use `with-no-warnings` as last resort for intentional non-standard usage
