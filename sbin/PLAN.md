# PLAN: sbin Shell Test Integration

Date: 2026-02-18
Status: Draft

## Objective

Add first-class automated tests for all shell programs in `sbin/` and integrate
them with `./sbin/Makefile` and root `make check`.

Targets in scope:
- `sbin/elinit-logrotate`
- `sbin/elinit-log-prune`
- `sbin/elinitctl`

## Current State

- There is currently no `sbin/Makefile`.
- Shell script behavior is tested mostly through Elisp ERT coverage, not through
  a dedicated shell test harness.

## Framework Decision

Use a custom minimal one-file shell test framework in-repo.

Proposed file:
- `sbin/tests/testlib.sh`

Rationale:
- No external dependency installation.
- Full control of style and behavior.
- Works with POSIX `sh` and existing script constraints.

## Minimal One-File Framework Design

`sbin/tests/testlib.sh` provides:

1. Test runner:
- `run_test NAME FUNCTION`
- Tracks pass/fail counters
- Prints TAP-like lines (ok/not ok) or simple deterministic lines

2. Assertions:
- `assert_eq EXPECT ACTUAL MSG`
- `assert_ne EXPECT ACTUAL MSG`
- `assert_contains HAYSTACK NEEDLE MSG`
- `assert_not_contains HAYSTACK NEEDLE MSG`
- `assert_status EXPECTED ACTUAL MSG`
- `assert_file_exists PATH MSG`

3. Command helper:
- `run_cmd` to execute command and capture:
  - stdout
  - stderr
  - exit status
- Stores results in well-known vars:
  - `TEST_STDOUT`
  - `TEST_STDERR`
  - `TEST_STATUS`

4. Fixture lifecycle:
- `setup` and `teardown` hooks (optional per test file)
- Auto tempdir creation via `mktemp -d`
- Trap-based cleanup

5. Summary/exit:
- Prints totals
- Exits nonzero when any test fails

Implementation constraints:
- Keep framework in one file.
- POSIX shell only.
- No bashisms.

## Proposed Test Tree

- `sbin/tests/testlib.sh`
- `sbin/tests/test-logrotate.sh`
- `sbin/tests/test-log-prune.sh`
- `sbin/tests/test-elinitctl.sh`

Each test file:
- Starts with `#!/bin/sh` and `set -eu`
- Sources `./testlib.sh`
- Defines `test_*` functions
- Calls framework main to run tests

## sbin/Makefile Integration

Create `sbin/Makefile` with these targets:

- `lint`
  - Runs `shellcheck` on scripts and tests
  - Fails on any warning/error

- `test`
  - Executes all `sbin/tests/test-*.sh`
  - Deterministic order
  - Fails fast or aggregates, but exits nonzero on any failure

- `check`
  - Depends on `lint` and `test`

- `clean`
  - Removes test temp artifacts if persisted

Recommended variables:
- `SHELL := /bin/sh`
- `SCRIPTS := elinit-logrotate elinit-log-prune elinitctl`
- `TESTS := tests/test-logrotate.sh tests/test-log-prune.sh tests/test-elinitctl.sh`

## Root Makefile Integration

After adding `sbin/Makefile`:

1. Add target in root `Makefile`:
- `sbin-check:`
- `\t$(MAKE) -C sbin check`

2. Add `sbin-check` to root `check` dependency chain.

This keeps one canonical command:
- `make check`

## Coverage Plan

### elinit-logrotate

- Argument parsing and required `--log-dir`.
- Validation for `--keep-days`.
- Unknown option failure.
- Dry-run output contract.
- Rotation naming behavior (timestamp + collision suffix).
- `--signal-reopen` behavior with valid/invalid pid files.
- Tar present vs tar missing behavior.

### elinit-log-prune

- Argument parsing and required `--log-dir`.
- Alias flags (`--vacuum`, `--vacuum-max-total-bytes`, `--format-hint`).
- Under-cap no-op behavior.
- Lock busy no-op behavior.
- Oldest-first prune behavior.
- Protection behavior (`--protect-id`, active file guards).
- Dry-run output contract.

### elinitctl

- Option parsing (`--json`, `--socket`, `--server-file`, `--timeout`).
- Mutual exclusion of socket/server-file.
- Server unavailable exit code path.
- Wrapper parse/decode behavior for dispatcher output.
- Invalid dispatcher output fallback behavior.

Test strategy note:
- Use PATH stubs for `emacsclient`, `base64`, `date`, `tar`, `flock`, etc.
- Keep tests hermetic using per-test temp directories.

## CI Plan

Add a CI step:
- `make -C sbin check`

Then keep root:
- `make check`

During migration, both can run until root integration is complete.

## Completion Gate

Task is complete when:
1. `sbin/Makefile` exists and `make -C sbin check` passes.
2. One-file framework `sbin/tests/testlib.sh` is implemented.
3. Comprehensive tests exist for all three `sbin` scripts.
4. Root `make check` includes `sbin` checks.
