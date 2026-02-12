# PLAN: Logging Architecture and Disk-Safety

Date: 2026-02-12
Status: Approved planning baseline
Source of truth: `FINDINGS.md` logging decisions (TODO items 7, 8, 10)

## Scope

This plan covers only logging architecture work:

- removing legacy startup rotation,
- introducing supervisor-managed external log writers,
- adding scheduled rotation/pruning scripts,
- enforcing per-file and directory-wide log size caps,
- wiring default daily maintenance via the timer subsystem,
- keeping user-facing logging commands unchanged.

## Locked Decisions (Do Not Reopen)

1. Remove startup rotation behavior from core:
- delete `supervisor--rotate-logs`,
- delete the `supervisor--rotate-logs` call from `supervisor-start`.

2. Keep logging command surface stable:
- CLI remains `logging` and `logs`,
- dashboard remains logging toggle (`l`) and log view (`L`),
- backend changes only.

3. Move process log writing to external supervisor-managed writers:
- users never create or manage writer processes manually.

4. Add script-based maintenance:
- `sbin` rotation script with simple `--keep-days`,
- separate `sbin` global prune script with `--max-total-bytes`,
- both follow `sbin/supervisorctl` header/style conventions.

5. Timer behavior:
- timer subsystem remains optional,
- default changes to enabled,
- if disabled, no automatic log maintenance,
- if enabled, default daily maintenance runs.

6. Safety controls required:
- per-file cap (`--max-file-size-bytes`) in `logd`,
- directory-wide cap (`--max-total-bytes`) in prune script.

7. Non-functional requirement:
- no minibuffer noise on normal maintenance paths,
- avoid UI hitches by removing per-chunk file I/O from Emacs.

## Canonical Component Contracts

### 1) Per-service log writer

File: `libexec/supervisor-logd`

Required behavior:

- reads service output from stdin,
- writes to one target log file with append semantics,
- keeps file descriptor open during steady state,
- on `HUP`: close and reopen target file,
- on `TERM`/`INT`: flush and exit cleanly,
- enforce `--max-file-size-bytes N` by local rotate + reopen.

Required flags:

- `--file PATH`
- `--max-file-size-bytes N`
- `--log-dir DIR` (for optional prune trigger files)
- `--prune-cmd CMD` (optional)
- `--prune-min-interval-sec N` (optional; throttle)

### 2) Rotation script

File: `sbin/supervisor-logrotate`

Required behavior:

- rotate active service logs: `log-<id>.log` -> `log-<id>.YYYYMMDD-HHMMSS.log`,
- rotate active supervisor log: `supervisor.log` -> `supervisor.YYYYMMDD-HHMMSS.log`,
- prune rotated files older than `--keep-days N` using file mtime,
- optional writer reopen signal (`HUP`) after rotation,
- quiet on success, non-zero exit on failure.

Required flags:

- `--log-dir DIR`
- `--keep-days N`
- `--signal-reopen` (optional)
- `--pid-dir DIR` (for writer PID discovery)
- `--dry-run` (optional)

### 3) Global prune script

File: `sbin/supervisor-log-prune`

Required behavior:

- enforce hard cap on total bytes in log directory (`--max-total-bytes N`),
- acquire exclusive lock via shared lock file before prune,
- never delete active current files (`log-*.log`, `supervisor.log`),
- delete oldest rotated files first until at or below cap,
- exit success if already under cap.

Required flags:

- `--log-dir DIR`
- `--max-total-bytes N`
- `--lock-file PATH` (default: `<log-dir>/.prune.lock`)
- `--dry-run` (optional)

### 4) Default schedule artifacts

Required default service ID: `logrotate`

Required default timer ID: `logrotate-daily`

Authority placement contract:

- shipped defaults live in the vendor/unit base tier,
- higher-precedence unit tiers may fully override `logrotate` and
  `logrotate-daily` definitions by ID.

Timer schedule (current timer schema): daily at 03:00 local time:

```elisp
(:id "logrotate-daily"
 :target "logrotate"
 :on-calendar (:hour 3 :minute 0)
 :enabled t
 :persistent t)
```

Default maintenance command must run rotate then prune.

## Defaults (Initial Values)

These defaults are part of this plan to keep implementation deterministic:

- keep-days: `14`
- max-file-size-bytes: `52428800` (50 MiB)
- max-total-bytes: `1073741824` (1 GiB)
- prune trigger throttle: `60` seconds

## Phase Plan (10 Phases)

### Phase 1: Core Logging Config Contract

Deliverables:

- add defcustoms/constants for:
  - logd program path,
  - logrotate script path,
  - log-prune script path,
  - keep-days default,
  - max-file-size default,
  - max-total default,
  - prune trigger throttle interval,
  - writer PID directory path.

Acceptance:

- values are discoverable in customization docs,
- no behavior change yet.

### Phase 2: Implement `libexec/supervisor-logd`

Deliverables:

- executable helper with required flags and signal behavior,
- local size-cap rotation logic,
- optional throttled prune trigger hook.

Acceptance:

- helper passes shellcheck if shell-based,
- helper rotates/reopens correctly on cap and `HUP`.

### Phase 3: Integrate Writer Lifecycle in `supervisor-core.el`

Deliverables:

- new internal writer state table (`id -> writer process`),
- start writer when effective logging is enabled,
- route service output to writer stdin instead of file `write-region`,
- stop/restart writer with service lifecycle,
- shutdown cleanup for all writers.

Acceptance:

- process output still appears in the same log file paths,
- no direct per-chunk file writes remain in service process filter path,
- command surface unchanged.

### Phase 4: Remove Legacy Startup Rotation

Deliverables:

- remove `supervisor--rotate-logs` function,
- remove startup invocation from `supervisor-start`,
- remove dead code/tests tied to startup rotation.

Acceptance:

- startup no longer performs blanket log file rename,
- no regressions in startup flow.

### Phase 5: Implement `sbin/supervisor-logrotate`

Deliverables:

- script with supervisorctl-style header conventions,
- rotates active files and mtime-prunes rotated files by `--keep-days`,
- optional reopen signaling to writers using PID files.

Acceptance:

- dry-run and real-run behavior validated,
- script works standalone with explicit `--log-dir`.

### Phase 6: Implement `sbin/supervisor-log-prune`

Deliverables:

- script enforcing `--max-total-bytes`,
- shared lock file coordination,
- oldest-first rotated-file pruning only.

Acceptance:

- total directory size reduced under cap when possible,
- active logs are never deleted,
- concurrent invocations are safe.

### Phase 7: Rotate/Prune Integration Rules

Deliverables:

- scheduled maintenance path runs rotate then prune,
- logd local-rotation path may trigger prune (throttled),
- consistent lock usage for prune path regardless of caller.

Acceptance:

- runaway service cannot grow logs without bounds:
  - file-level bounded by logd cap,
  - directory-level bounded by prune cap.

### Phase 8: Default Daily Unit + Timer Wiring

Deliverables:

- default `logrotate` oneshot unit definition present by default,
- default `logrotate-daily` timer entry present by default,
- timer subsystem default set to enabled.

Implementation note:

- if timer subsystem is disabled by user, no automatic rotation occurs (expected),
- this phase must not change existing logging command names.

Acceptance:

- fresh/default setup performs daily maintenance automatically,
- disabling timer subsystem disables auto-maintenance.

### Phase 9: Test Coverage Expansion

Deliverables:

- ERT coverage for:
  - writer spawn/cleanup lifecycle,
  - logging toggle interactions,
  - writer reopen and size-cap rotation,
  - rotate script keep-days behavior,
  - prune script max-total behavior and lock behavior,
  - timer-enabled default maintenance path,
  - timer-disabled no-auto-maintenance path.

Acceptance:

- `make check` passes.

### Phase 10: Documentation and Migration Notes

Deliverables:

- update `README.org` logging sections:
  - architecture update (external writers + scripts),
  - defaults and tuning knobs,
  - timer-default-enabled behavior,
  - how to disable auto-maintenance,
  - disk safety model.
- update `sbin/README.md` for new scripts and options.

Acceptance:

- docs reflect implemented behavior only,
- no stale references to startup `supervisor--rotate-logs`.

## Explicit Non-Goals

- No journald integration.
- No rename of `logging` or `logs` commands.
- No changes to dashboard logging keybindings.
- No introduction of `analyze` command namespace in this logging plan.

## Definition of Done

All must be true:

1. startup rotation code path is removed.
2. service output no longer does per-chunk file `write-region`.
3. log writers are fully supervisor-managed.
4. daily rotate+prune runs by default when timer subsystem is enabled.
5. per-file and total-directory caps are enforced via the two-script model.
6. command surface (`logging`, `logs`, dashboard `l`/`L`) is unchanged.
7. `make check` passes.
8. `README.org` and `sbin/README.md` are in sync with implementation.
