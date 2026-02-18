# PLAN: libexec C Test Integration

Date: 2026-02-18
Status: Draft

## Objective

Add first-class automated tests for all C programs under `libexec/` and wire
them into `libexec/Makefile` so they can run locally and in CI as part of the
normal quality gates.

Scope:
- `libexec/supervisor-logd.c`
- `libexec/supervisor-runas.c`
- `libexec/Makefile`
- root `Makefile` (integration point)
- `CLAUDE.md` testing requirements

## Framework Decision

Use a minimal single-header C test framework: `acutest`.

Why:
- C99-friendly.
- No external library linking.
- One header file, low setup overhead.
- Easy to run multiple test executables from Make.

How to get and set up:
1. Create vendor directory:
   - `mkdir -p libexec/tests/vendor`
2. Download a pinned `acutest.h` from a specific upstream tag or commit into:
   - `libexec/tests/vendor/acutest.h`
3. Record the exact upstream URL + commit in a short comment at the top of
   test sources and in:
   - `libexec/tests/THIRD_PARTY.md`
4. Add `-Itests/vendor` to test compile flags in `libexec/Makefile`.

Note:
- Keep the framework vendored in-repo so tests do not require network access.

## Test Layout

Create this directory structure:

- `libexec/tests/test_supervisor_logd.c`
- `libexec/tests/test_supervisor_runas.c`
- `libexec/tests/test_helpers.h`
- `libexec/tests/test_helpers.c`
- `libexec/tests/vendor/acutest.h`
- `libexec/tests/THIRD_PARTY.md`

Testing model:
- Black-box process tests for helper binaries (primary path).
- Focus on observable behavior (exit code, stderr messages, log file output,
  record format, rotation artifacts).
- Avoid relying on internal static functions unless a specific unit seam is
  added intentionally.

## Makefile Integration Plan

Update `libexec/Makefile` with test-aware targets.

Add variables:
- `TEST_CFLAGS` (inherits warning flags + include paths)
- `TEST_LDFLAGS`
- `TEST_BINS`

Add targets:
- `all`: build `supervisor-runas` and `supervisor-logd` (existing behavior)
- `tests`: build all test executables
- `test`: run all test executables
- `check`: run `all` then `test`
- `clean`: remove binaries and test artifacts

Behavior requirements:
- Test runs must build helpers from current source first (no stale binary
  dependency).
- `test` should fail fast on first failing test executable.
- Keep output readable and deterministic.

## Root Makefile Integration

After `libexec/Makefile` has `check`:
1. Add `libexec-check` target in root `Makefile`:
   - `$(MAKE) -C libexec check`
2. Include `libexec-check` in root `check`.
3. Keep existing Elisp checks unchanged.

This ensures `make check` validates both Elisp and C paths.

## Coverage Plan

### `supervisor-logd` comprehensive coverage

Text mode:
- Required field presence in every record line.
- Output event rules (`status=-`, `code=-`, payload escaping).
- Exit event rules (event marker fields and payload sentinel behavior as
  implemented by contract decisions).
- Escaping round-trip:
  - empty payload
  - literal `-`
  - backslash, newline, carriage return, tab
  - non-printable bytes including NUL and high-bit bytes
- Stream identity (`stdout`, `stderr`, `meta` for exit).
- Invalid frame enum handling (reject malformed event/stream/status).

Transport and framing:
- Incomplete frame buffering behavior.
- Bad length handling.
- Protocol error recovery path behavior.

File behavior:
- Rotation on max-size boundary.
- Reopen on SIGHUP.
- Exit handling and clean EOF behavior.

### `supervisor-runas` comprehensive coverage

Argument and usage handling:
- Missing command, missing identity, unknown options.
- Unknown user/group failures and exit codes.

Privilege path checks:
- Non-root failure paths.
- Root-only success path (gated with explicit env var in CI lane).

Exec behavior:
- Bare-name exec failure behavior.
- Absolute-path command success behavior.

## CI Plan

Add a dedicated CI step:
- `make -C libexec check`

Then, after root integration, keep:
- `make check`

Both can temporarily run in parallel lanes while migrating.

## Final Step (Task Completion Gate)

1. Implement the full C test suite for all programs in `libexec/`.
2. Wire `libexec` test execution into both local Make targets and CI.
3. Final step: update `CLAUDE.md` so testing requirements explicitly cover both
   Elisp checks (`make check`) and C helper checks (`make -C libexec check`).
4. Do not mark this task complete until both code paths are enforced by tests.
