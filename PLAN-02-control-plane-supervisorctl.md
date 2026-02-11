# PLAN 02: Control Plane (`supervisorctl` + POSIX Wrappers)

Roadmap reference: `ROADMAP.md` item 2.

## Objective

Ship a usable non-interactive control plane based on:

- `supervisorctl` command surface
- minimal POSIX wrapper transport
- Elisp-owned parsing/dispatch/output logic
- human and JSON outputs with stable schema
- explicit exit codes and test coverage

## Scope

In scope:

- command contract
- Elisp dispatcher and handlers
- output formatting and JSON schema
- wrapper scripts
- error handling and security posture docs

Out of scope:

- repo/package rebranding
- full module split (`supervisor-core.el`/`supervisor-cli.el`) from roadmap item 3
- PID 1 deployment concerns

## Design Principles

- Wrapper is transport-only.
- Business logic lives in Elisp.
- Human-readable output is default.
- `--json` output is stable and script-safe.
- Exit codes are deterministic and documented.

## Target Command Surface

Primary command:

- `supervisorctl`

Core lifecycle:

- `supervisorctl start [ID...]`
- `supervisorctl stop [ID...]`
- `supervisorctl restart [ID...]`
- `supervisorctl reload`
- `supervisorctl validate`

Inspection:

- `supervisorctl status [ID...]`
- `supervisorctl list`
- `supervisorctl describe ID`
- `supervisorctl graph [ID]`
- `supervisorctl blame`
- `supervisorctl logs ID [--tail N]`

Runtime policy:

- `supervisorctl enable [ID...]`
- `supervisorctl disable [ID...]`
- `supervisorctl restart-policy (on|off) [ID...]`
- `supervisorctl logging (on|off) [ID...]`

## Exit Codes

- `0` success
- `1` runtime failure
- `2` invalid arguments
- `3` Emacs server unavailable
- `4` validation failed

## Phases

### Phase 1: Contract Freeze

Deliverables:

- Finalize command grammar, options, and exit-code mapping.
- Finalize JSON response envelope and per-command payload shapes.
- Define compatibility policy for output fields.

Acceptance criteria:

- CLI contract documented and reviewed.
- No ambiguous argument forms remain.

### Phase 2: Elisp Dispatcher

Deliverables:

- Implement a single dispatcher entry (for example `supervisor--dispatch`).
- Implement subcommand handlers with structured return objects:
- `:exit`
- `:format`
- `:output`
- Add argument parsing and validation in Elisp.

Acceptance criteria:

- Every command path returns structured output and explicit exit code.
- Unknown commands/options fail with exit code `2`.

### Phase 3: Output Layer

Deliverables:

- Human formatter for table and detail views.
- JSON formatter with stable keys and normalized types.
- Shared conversion helpers for dashboard/status data.

Acceptance criteria:

- Human output is readable and consistent across commands.
- JSON is deterministic for identical state.
- Schema is documented in `sbin/README.md`.

### Phase 4: POSIX Wrapper(s)

Deliverables:

- Implement minimal wrapper in `sbin/` to call `emacsclient --eval`.
- Support server-selection flags (`-s`, `-f`) and timeout handling.
- Ensure wrapper only transports args and prints dispatcher response.

Acceptance criteria:

- Wrapper contains no business logic beyond transport/error plumbing.
- Fails fast with clear message when server is unavailable.

### Phase 5: Integration Coverage

Deliverables:

- Integrate command handlers with current supervisor runtime APIs.
- Ensure event/logging side effects remain consistent with interactive flows.
- Add regression tests for command-to-action mapping.

Acceptance criteria:

- CLI operations and dashboard operations converge on same core behavior.
- No duplicate logic forks for business semantics.

### Phase 6: Test Matrix and Hardening

Deliverables:

- Add tests for:
- argument parsing failures
- command success/failure exit codes
- JSON schema snapshots
- server-unavailable behavior
- representative end-to-end command flows
- Document security boundary and deployment guidance.

Acceptance criteria:

- `make check` passes with CLI tests included.
- Security notes are explicit and actionable.

## Risks and Mitigations

- Risk: CLI and dashboard behavior drift.
- Mitigation: shared Elisp handlers and parity regression tests.

- Risk: JSON instability breaks automation.
- Mitigation: schema snapshots and compatibility policy.

- Risk: Wrapper complexity grows into duplicate control plane.
- Mitigation: strict transport-only rule and code review gate.

## Definition of Done

- `supervisorctl` command surface is implemented and usable.
- POSIX wrapper(s) are minimal and stable.
- Human and JSON output modes are production-ready.
- Exit-code contract is enforced by tests.
- `make check` passes and docs are updated (`README.org`, `sbin/README.md`).
