# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

supervisor.el is an Emacs Lisp package for managing background processes. It provides staged startup with dependency ordering, crash recovery, and a dashboard UI.

## Module Structure

The package is split into focused modules:

| File | Purpose |
|------|---------|
| `supervisor-core.el` | Engine, parsing, scheduling, process lifecycle, state management |
| `supervisor-log.el` | Log framing, encode/decode, record filtering/formatting, writer lifecycle |
| `supervisor-overrides.el` | Overrides persistence, effective state getters, policy mutators |
| `supervisor-libexec.el` | Libexec build targets, compiler selection, build invocation |
| `supervisor-sandbox.el` | Sandbox profile argument construction and bwrap argv wrapping |
| `supervisor-units.el` | Unit-file loading, validation, merge with legacy config |
| `supervisor-timer.el` | Timer subsystem for scheduled and calendar-based triggers |
| `supervisor-dashboard.el` | UI rendering, keymaps, interactive commands |
| `supervisor-cli.el` | CLI dispatcher, formatters, command handlers |
| `supervisor.el` | Entry point that loads all modules and provides the `supervisor` feature |

**Load order:** core (requires log, overrides, libexec, sandbox) then units, timer, dashboard, cli (loaded by entry point).

**Dependency rules:**
- `supervisor-core.el` requires `supervisor-log`, `supervisor-overrides`, `supervisor-libexec`, and `supervisor-sandbox` (these are extracted subsystems, not optional)
- `supervisor-log.el` uses `declare-function` for core's `supervisor--log` (no hard require back to core)
- `supervisor-overrides.el` uses `declare-function` for core entry accessors and helpers (no hard require back to core)
- `supervisor-libexec.el` uses `declare-function` for core's `supervisor--log` (no hard require back to core)
- `supervisor-sandbox.el` uses `declare-function` for core entry accessors (no hard require back to core)
- `supervisor-units.el` uses `declare-function` for core (no hard require)
- `supervisor-dashboard.el` requires only `supervisor-core`
- `supervisor-cli.el` requires only `supervisor-core`
- `supervisor.el` requires all modules

Cross-module calls use `declare-function` for proper byte-compilation. The extracted subsystem modules (log, overrides, libexec, sandbox) use `defvar` forward declarations for variables defined in core.

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

## CI Badge Infrastructure

The README test-count badge is powered by a GitHub Gist + shields.io endpoint.

- **Gist:** `e686727a6d88c17c557003e73a9c020c` (secret gist, user `cypherpunk2001`)
- **Filename inside gist:** `supervisor-el-tests.json`
- **Secret:** `GIST_TOKEN` in repo Actions secrets -- a classic GitHub token with `gist` scope
- **Updated by:** `.github/workflows/ci.yml`, `elisp` job, `snapshot` matrix run only, on push to master

The workflow captures ERT output, parses the `Ran N tests` summary line, and
PATCHes the Gist via the GitHub API.  The badge URL in README.org reads from
that Gist via `https://img.shields.io/endpoint?url=...`.

If the token expires, generate a new classic token with `gist` scope at
https://github.com/settings/tokens and update the `GIST_TOKEN` secret in
repo Settings -> Secrets and variables -> Actions.

## Development Commands

```bash
make check          # Run all CI checks (Elisp + C + shell)
make lint           # Run byte-compile, checkdoc, package-lint only
make test           # Run ERT tests only
make test-one TEST=supervisor-test-parse-string-entry  # Run single test
make libexec-check  # Build and test C helpers (supervisor-logd, supervisor-runas, supervisor-rlimits)
make sbin-check     # Run shellcheck + shell tests for sbin/ scripts

# Subsystem checks in isolation
make -C libexec check
make -C sbin check          # shellcheck lint + all shell tests
make -C sbin test           # shell tests only (skip shellcheck)
make -C sbin lint           # shellcheck only

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

`make check` validates Elisp (lint + ERT), C helper tests (`libexec/`), and shell script
tests (`sbin/`).  To run subsystems in isolation: `make -C libexec check` or
`make -C sbin check`.

## Code Quality Standards

See PLAN-INIT-followups.md for the authoritative spec. Key requirements:

### Validation
- Entry options must be validated against a whitelist before starting
- Invalid entries are skipped but surfaced in the dashboard with status `invalid` and a reason
- Mutually exclusive options (`:restart`/`:no-restart`, `:enabled`/`:disabled`) must be detected
- Type-specific options must be enforced (`:restart` invalid for oneshot, `:oneshot-blocking` invalid for simple)

### Async Scheduling
- No polling loops -- use sentinels and timers only
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

### Testing (Elisp)
- Use ERT for all tests
- If a behavior can be tested, it must be tested
- Required test coverage: whitelist validation, cycle fallback, stable ordering, oneshot timeout unlock, delayed entry handling, async oneshot non-blocking, limit-key validation (value shapes, target rejection, plan-level invalid plumbing), rlimits launch-argv composition

### Testing (Shell -- sbin/)

Shell scripts in `sbin/` have their own test suite under `sbin/tests/`.

**Framework:** `sbin/tests/testlib.sh` is a minimal POSIX-sh test framework
(no external dependencies).  Source it, define `test_*` functions, call
`run_tests "$(basename "${0}")"` at the end.

**Test files:**
- `sbin/tests/test-logrotate.sh` -- tests for `supervisor-logrotate`
- `sbin/tests/test-log-prune.sh` -- tests for `supervisor-log-prune`
- `sbin/tests/test-supervisorctl.sh` -- tests for `supervisorctl`

**Writing new tests:**
- First test in every file must be `test_shellcheck` (runs shellcheck on the script under test).
- Use `run_cmd SCRIPT ARGS...` to capture `TEST_STDOUT`, `TEST_STDERR`, `TEST_STATUS`.
- Use `${TEST_TMPDIR}` (auto-created per test, cleaned up after) for fixtures.
- Assertions: `assert_eq`, `assert_ne`, `assert_contains`, `assert_not_contains`,
  `assert_status`, `assert_file_exists`, `assert_file_not_exists`, `assert_match`.
- Keep tests hermetic: never depend on global state or real services.
- For `supervisorctl` tests, stub `emacsclient` via PATH manipulation (write args
  to a temp file, not stderr, because supervisorctl captures stderr with `2>&1`).
- Prefer `--dry-run` for testing output contracts without side effects.

**What to test for each script:**
- Argument parsing: required flags, missing values, unknown options, validation.
- Exit codes: correct codes for success, failure, and edge cases.
- Output contracts: dry-run output format, info/error message content.
- Behavioral correctness: file creation/deletion, naming patterns, ordering.
- Edge cases: empty directories, lock contention, missing dependencies.

**Style rules (must pass shellcheck):**
- POSIX sh only, no bashisms.  See `sbin/shell-scripting-style.txt`.
- All test files and testlib.sh are checked by `make -C sbin lint`.
- Use `# shellcheck disable=SCXXXX` sparingly and only with justification.

### Testing (C -- libexec/)

C helpers in `libexec/` (`supervisor-logd`, `supervisor-runas`, `supervisor-rlimits`)
have a test suite under `libexec/tests/`.

**Framework:** [acutest](https://github.com/mity/acutest) -- a single-header C
test framework vendored at `libexec/tests/vendor/acutest.h`.  No external
dependencies.

**Test files:**
- `libexec/tests/test_supervisor_logd.c` -- tests for `supervisor-logd`
- `libexec/tests/test_supervisor_runas.c` -- tests for `supervisor-runas`
- `libexec/tests/test_supervisor_rlimits.c` -- tests for `supervisor-rlimits`

**Shared helpers** (`test_helpers.h` / `test_helpers.c`):
- `run_cmd(argv, stdin_data, stdin_len, result)` -- fork/exec the binary,
  pipe stdin, capture stdout/stderr/exit-status.  All tests use this for
  black-box process testing.
- `build_frame(buf, event, stream, pid, unit_id, exit_code, exit_status,
  payload, payload_len)` -- construct a wire-protocol frame (u32be length
  header + 13-byte fixed fields + unit + payload) for logd framed-mode tests.
- `make_tmpdir()` / `remove_tmpdir(path)` -- per-test temp directory lifecycle.
- `read_file(path, out_len)` -- read entire file into malloc'd buffer.
- `read_u32be(p)` / `read_u16be(p)` / `read_i32be(p)` -- big-endian readers
  for binary log format verification.

**Binary paths:** The Makefile injects `-DLOGD_PATH="..."`,
`-DRUNAS_PATH="..."`, and `-DRLIMITS_PATH="..."` at compile time so tests
always exec the freshly-built binary from the same build tree.

**Writing new tests:**
- Define a `void test_foo(void)` function, add it to the `TEST_LIST` array.
- Use `TEST_CHECK(expr)` for assertions and `TEST_MSG("fmt", ...)` for
  diagnostic output on failure.
- Use `run_cmd()` for all black-box tests.  Check `result.status` for exit
  code, `result.out` / `result.err` for stdout/stderr, `result.out_len` /
  `result.err_len` for lengths.
- Use `make_tmpdir()` for file-producing tests (rotation, binary logs).
  Always `remove_tmpdir()` in cleanup.
- For framed-mode tests, build frames with `build_frame()`, concatenate into
  a buffer, and pass as stdin.

**What to test for each binary:**
- Argument validation: missing required flags, unknown options, invalid values,
  negative numbers, mutually exclusive flags.
- Protocol correctness: valid frames produce correct output, field values
  round-trip through encode/decode.
- Protocol errors: invalid event/stream codes, truncated frames, oversized
  unit_len, incomplete frames at EOF.
- Signal handling: SIGHUP (log reopen/rotation), SIGTERM (graceful shutdown).
  Send signals with `kill()` after a brief `msleep()` delay.
- Rotation: file exceeds max-size, new file created, old data preserved.
  Test `.log` extension rename and non-`.log` fallback.
- Binary format: SLG1 magic header, record structure, nanosecond timestamps,
  append-to-existing behavior.
- Write errors: use `/dev/full` as output path to exercise error branches.
- Edge cases: empty input, large payloads, NUL bytes in payloads.

**Root-gated tests:** Tests requiring privilege operations (setuid/setgid)
check `getuid() == 0` at runtime and skip otherwise.  Set
`SUPERVISOR_TEST_ROOT=1` to run root-only tests.

**Pitfalls:**
- Use `msleep(ms)` (via `nanosleep`) instead of `usleep()` -- `usleep` is
  removed from POSIX.1-2008 and unavailable under `_POSIX_C_SOURCE 200809L`.
- Background-process rotation tests need multiple writes with delays between
  them; a single large write may arrive in one `read()` call before rotation
  triggers.
- `signal(SIGPIPE, SIG_IGN)` before writing to a pipe where the reader may
  have exited; restore with `SIG_DFL` afterward.
- Building with `--coverage -O0 -Werror` can trigger `maybe-uninitialized`
  warnings that do not appear at `-O2`.  Zero-initialize structs to be safe.

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
  - **No fancy dashes or emojis**: Never use Unicode em-dashes, en-dashes, or
    emojis in documentation files (README.org, CLAUDE.md).
    Never use Org triple-dash `---` (GitHub renders it as an em-dash).
    Use `--` with spaces on both sides for inline separators, or rewrite
    with parentheses/commas.
  - **Org verbatim and tilde paths**: GitHub's Org renderer mishandles adjacent
    `~` boundaries.  Rules:
    - Never write `~foo~/~bar~` (use `~foo/bar~` instead).
    - Never write `~~/.config/...~` (use `=~/.config/...=` instead).
    - Never place `--` immediately before `~` (e.g. `--~M-x ...~` breaks;
      write `-- ~M-x ...~` with a space).
    - The `=...=` verbatim marker is safe for paths containing `~`.
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
- Each module provides its own feature: `supervisor-core`, `supervisor-log`, `supervisor-overrides`, `supervisor-libexec`, `supervisor-sandbox`, `supervisor-units`, `supervisor-timer`, `supervisor-dashboard`, `supervisor-cli`

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
