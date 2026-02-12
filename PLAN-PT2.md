# PLAN: Unit-File MUST-HAVE Expansion (PT2)

Date: 2026-02-12
Status: Tightened draft

## Scope

Implement only the remaining MUST-HAVE unit options that are not already
covered by existing code or other approved plans.

Already covered elsewhere and therefore out of scope here:

- `ExecStart` equivalent (`:command`) - already implemented.
- `Type` (limited simple/oneshot) - already implemented.
- `After` and `Requires` - already implemented.
- Restart policy value model (`no|on-success|on-failure|always`) - covered in
  `PLAN-service-units.md`.

This plan implements exactly these six options:

| Feature | Unit key | Default | systemd analog |
|---|---|---|---|
| Working directory | `:working-directory` | `nil` | `WorkingDirectory=` |
| Environment variables | `:environment` | `nil` | `Environment=` |
| Environment files | `:environment-file` | `nil` | `EnvironmentFile=` |
| Custom stop command(s) | `:exec-stop` | `nil` | `ExecStop=` |
| Custom reload command(s) | `:exec-reload` | `nil` | `ExecReload=` |
| Per-unit restart delay | `:restart-sec` | `nil` | `RestartSec=` |

## Locked Decisions

1. Unit files remain Elisp plist format. No INI parser work in this plan.
2. No aliases/shims for old names; this plan follows hard-break policy.
3. Multi-declarable systemd directives are represented as list values under one
   plist key (Elisp-native approach).
4. Duplicate plist keys are rejected as invalid for all unit files.
5. `:exec-stop`, `:exec-reload`, and `:restart-sec` are `simple`-only.
6. `:working-directory`, `:environment`, and `:environment-file` are valid for
   both `simple` and `oneshot`.
7. No PID magic variable injection (no `$SUPERVISOR_MAINPID`).
8. No expansion into NICE-TO-HAVE/PROBABLY-BLOAT items.

## Canonical Value Shapes (Unambiguous)

| Key | Accepted user form(s) | Normalized internal form |
|---|---|---|
| `:working-directory` | string or `nil` | absolute string path or `nil` |
| `:environment` | `((KEY . VALUE) ...)` or `nil` | ordered alist of string pairs |
| `:environment-file` | string, list of strings, or `nil` | ordered list of strings |
| `:exec-stop` | string, list of strings, or `nil` | ordered list of strings |
| `:exec-reload` | string, list of strings, or `nil` | ordered list of strings |
| `:restart-sec` | non-negative number or `nil` | non-negative number or `nil` |

Key normalization rules:

- Single string forms are normalized to one-element lists for
  `:environment-file`, `:exec-stop`, `:exec-reload`.
- `:environment` order is preserved. On duplicate keys, later entries win.
- Duplicate plist keys are invalid; do not use repeated keys as a substitute for
  list values.

## Multi-Declaration Parity Model (Systemd -> Elisp)

Systemd allows repeating some directives. In this plan, repeated directives are
encoded as list values:

- `ExecStop=` repeated lines -> `:exec-stop` list of commands.
- `ExecReload=` repeated lines -> `:exec-reload` list of commands.
- `EnvironmentFile=` repeated lines -> `:environment-file` list of files.
- `Environment=` repeated lines -> one ordered `:environment` alist.

Example:

```elisp
(:id "my-daemon"
 :command "my-daemon"
 :environment (("APP_ENV" . "prod")
               ("LOG_LEVEL" . "info")
               ("LOG_LEVEL" . "warn"))  ; later wins
 :environment-file ("/etc/my-daemon/env"
                    "-/etc/my-daemon/env.local")
 :exec-stop ("my-daemonctl shutdown" "sleep 1")
 :exec-reload "my-daemonctl reload")
```

## Runtime Semantics Contract

### 1) `:working-directory`

- If set, process starts with that cwd.
- `"~"` and `~/...` are expanded to home.
- Relative paths are resolved against `supervisor-unit-directory` (not buffer
  `default-directory`) for deterministic behavior.
- If resolved directory does not exist or is not a directory: start fails with a
  clear validation/runtime error.

### 2) `:environment` and `:environment-file`

Effective environment build order:

1. Start from inherited `process-environment`.
2. Apply `:environment-file` entries in list order.
3. Apply `:environment` pairs in list order.
4. Later assignment for the same key overrides earlier assignment.

Env-file parsing subset (explicitly defined):

- Ignore blank lines.
- Ignore lines starting with `#` or `;` after leading whitespace trim.
- Optional `export ` prefix is accepted and removed.
- Parse first `=` as separator.
- Key must match `[A-Za-z_][A-Za-z0-9_]*`.
- Value is taken as raw text after `=` (trim outer whitespace).
- Relative file paths are resolved against `supervisor-unit-directory`.
- Optional leading `-` in path means "missing file is ignored".
- Without leading `-`, missing file is an error.

Out of scope for this plan:

- Shell interpolation/expansion in env-file values.
- `UnsetEnvironment=` semantics.

### 3) `:restart-sec`

- Used only for auto-restart scheduling.
- If non-`nil`, overrides global `supervisor-restart-delay` for that unit.
- If `nil`, global delay applies.
- `0` is valid and means immediate retry.

### 4) `:exec-stop`

- Applies to stopping running `simple` units.
- Commands execute sequentially in listed order.
- Each command runs with unit's effective cwd and environment.
- Stop command stdout/stderr follows unit logging policy.
- Each stop command has timeout `supervisor-shutdown-timeout`; timeout counts as
  command failure.
- If any stop command fails (non-zero/timeout), stop flow continues to signal
  escalation (do not abort shutdown path).
- After stop commands complete (or fail), existing SIGTERM/SIGKILL fallback
  remains authoritative.

### 5) `:exec-reload`

- Applies only to running `simple` units.
- Commands execute sequentially in listed order.
- Each command runs with unit's effective cwd and environment.
- Each reload command has timeout `supervisor-shutdown-timeout`; timeout counts
  as failure.
- Service process is not stopped/restarted when `:exec-reload` is present.
- If reload command chain fails, report reload failure and keep service running.
- If `:exec-reload` is `nil`, existing reload behavior stays in effect.

## Validation Contract Additions

- Add new keys to keyword whitelist(s).
- Reject duplicate plist keys early with precise error.
- Reject wrong shapes with key-specific errors.
- Reject simple-only keys on oneshot units.
- Keep current unknown-keyword and existing mutual-exclusion checks intact.

## Phase Plan

### Phase P1: Parser/Schema/Validation

Deliverables:

- Add six keys to unit keyword and entry keyword whitelists.
- Extend service struct + accessor set.
- Implement duplicate-key detection.
- Implement shape/type validation and simple-only checks.
- Implement normalization helpers for string-or-list forms.

Acceptance:

- Parsing tests for valid/invalid forms pass.
- Duplicate key tests pass.
- No runtime behavior change yet.

### Phase P2: Runtime Foundation (cwd/env/restart delay)

Deliverables:

- Apply `:working-directory` to process spawn context.
- Apply `:environment-file` and `:environment` merge pipeline.
- Apply per-unit `:restart-sec` in restart scheduler.

Acceptance:

- Cwd/env/restart-delay ERT coverage present.
- Deterministic path resolution verified.

### Phase P3: Stop and Reload Command Execution

Deliverables:

- Implement `:exec-stop` chain with fallback signal escalation.
- Implement `:exec-reload` chain for running simple units.
- Ensure command chains inherit unit cwd+env and logging behavior.

Acceptance:

- ERT tests for success, failure, nil/default behavior, and scope limits
  (simple-only, running-only for reload).

### Phase P4: CLI and Dashboard Surface

Deliverables:

- Show new fields in `show ID` and JSON output.
- Show new fields in dashboard detail/info view.
- Keep main dashboard table unchanged (no new columns).
- Update unit scaffold comments for new keys.

Acceptance:

- CLI/dashboard tests updated and passing.

### Phase P5: Documentation Sync

Deliverables:

- Update `README.org` unit-key reference and examples.
- Add environment/env-file and stop/reload behavior sections.
- Update developer docs where schema is described.

Acceptance:

- No stale docs; examples use canonical value shapes.

## Required Test Matrix (Minimum)

1. Duplicate key rejection for each new key.
2. `:environment` duplicate variable resolution (later wins).
3. `:environment-file` ordering and `-path` optional missing behavior.
4. Invalid env-file line handling (bad key, missing `=`) produces clear errors.
5. Relative and home path resolution for `:working-directory`.
6. `:restart-sec` precedence over global delay.
7. `:exec-stop` chain + fallback signal path.
8. `:exec-reload` chain success/failure without restart.
9. Simple-only restrictions for stop/reload/restart-sec keys.

## Explicit Non-Goals

- Any NICE-TO-HAVE/PROBABLY-BLOAT feature from `FINDINGS-PT2.md`.
- INI `[Unit]/[Service]/[Install]` parsing.
- systemd-complete env-file grammar or variable expansion.
- PID variable injection convenience.

## Definition of Done

1. All six keys parse, validate, and are normalized consistently.
2. Multi-declaration parity is achieved via list value semantics.
3. Runtime behavior for cwd/env/restart-sec/exec-stop/exec-reload matches this
   contract.
4. CLI/dashboard surfaces and docs are updated.
5. Full `make check` passes.
