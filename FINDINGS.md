# FINDINGS: supervisorctl/dashboard vs systemctl/systemd

Date: 2026-02-12
Repository: supervisor.el
Status: fresh parity/difference audit

## Scope
This report compares current behavior of:
- `supervisorctl` (CLI)
- supervisor dashboard (`M-x supervisor`)

against common `systemctl` workflows and systemd expectations.

## Evidence
- `supervisor-cli.el`
- `supervisor-dashboard.el`
- `supervisor-core.el`
- `supervisor-units.el`
- `README.org`

## Executive Summary
- Command naming is now strongly systemctl-shaped for core lifecycle/introspection.
- Dashboard and CLI are mostly consistent for start/stop/restart/kill/mask/edit/cat.
- Biggest intentional divergences are architectural, not naming:
  - override-based enable/disable model,
  - explicit `reconcile`,
  - file-log model instead of journald,
  - Emacs/runtime-scoped supervisor rather than PID 1 init.

## 1) CLI Parity (`supervisorctl`)

### 1.1 Systemctl-aligned commands
- `status [ID...]`, `list-units [ID...]`, `show ID`
- `start [-- ID...]`, `stop [-- ID...]`, `restart [-- ID...]`
- `kill [--signal SIG] [--] ID`
- `enable [--] ID...`, `disable [--] ID...`
- `mask [--] ID...`, `unmask [--] ID...`
- `is-active ID`, `is-enabled ID`, `is-failed ID`
- `daemon-reload`, `reload [--] ID...`
- `cat ID`, `edit ID`
- `list-dependencies [ID]`, `list-timers`
- `version`

### 1.2 Supervisor-specific commands
- `reconcile`
- `validate`
- `restart-policy (on|off) [--] ID...`
- `logging (on|off) [--] ID...`
- `blame`
- `logs [--tail N] [--] ID`
- `ping`

## 2) Dashboard Parity

### 2.1 Lifecycle and policy keys
- Lifecycle: `s` start, `x` stop, `R` restart, `k`/`K` kill
- Policy toggles: `e` enabled, `m` mask, `r` restart, `l` logging
- Unit file actions: `c` cat (read-only), `E` edit
- System actions: `u` reload-unit, `X` daemon-reload (transient menu)
- Ops/inspection: `d`/`D` dependencies, `B` blame, `L` logs

### 2.2 Dashboard-only capabilities (no direct systemctl analog)
- Stage/tag filtering (`f`, `t`)
- Interactive process supervision view with grouped stage tables
- Tight edit-return workflow for unit files

## 3) Command-by-Command Parity Table

| systemctl/systemd workflow | supervisorctl | dashboard | parity | notes |
|---|---|---|---|---|
| `systemctl status [UNIT...]` | `status [ID...]` | main table + `i` details | Partial | `status` is dual-mode: with IDs = detail; without IDs = overview (list-like). |
| `systemctl list-units` | `list-units [ID...]` | main table | Aligned | canonical overview table. |
| `systemctl show UNIT` | `show ID` | `i` | Aligned | property-style detail for one unit. |
| `systemctl start UNIT...` | `start [-- ID...]` | `s` | Partial | disabled units can still be manually started (session-only), by design. |
| `systemctl stop UNIT...` | `stop [-- ID...]` | `x` | Aligned | stop sets manual-stop suppression for restart behavior. |
| `systemctl restart UNIT...` | `restart [-- ID...]` | `R` | Aligned | implemented as stop then start. |
| `systemctl kill UNIT` | `kill [--signal SIG] [--] ID` | `k` / `K` | Aligned | signal-only semantics; does not set manual-stop suppression. |
| `systemctl reload UNIT...` | `reload [--] ID...` | `u` (transient System group) | Partial | per-unit hot-reload contract; not daemon-native SIGHUP semantics. |
| `systemctl daemon-reload` | `daemon-reload` | `X` (transient System group) | Aligned | re-read definitions, no runtime start/stop effects. |
| `systemctl enable UNIT...` | `enable [--] ID...` | `e` toggle | Partial | override-state model, not install/symlink model. |
| `systemctl disable UNIT...` | `disable [--] ID...` | `e` toggle | Partial | override-state model; manual start still allowed. |
| `systemctl mask/unmask UNIT` | `mask/unmask` | `m` toggle | Aligned | mask has highest precedence and blocks manual start. |
| `systemctl is-active UNIT` | `is-active ID` | N/A | Aligned | exit code parity (0/3/4). |
| `systemctl is-enabled UNIT` | `is-enabled ID` | N/A | Aligned | exit code parity (0/1/4), includes masked state. |
| `systemctl is-failed UNIT` | `is-failed ID` | N/A | Aligned | exit code parity (0/1/4). |
| `systemctl list-dependencies [UNIT]` | `list-dependencies [ID]` | `d` / `D` | Aligned | per-unit or full graph. |
| `systemctl list-timers` | `list-timers` | timer section | Partial | present, but scoped to supervisor timer subsystem model. |
| `systemctl cat UNIT` | `cat ID` | `c` | Aligned | raw unit file content. |
| `systemctl edit UNIT` | `edit ID` | `E` | Partial | `$VISUAL/$EDITOR` for CLI; Emacs edit-mode flow in dashboard. |
| `journalctl -u UNIT` | `logs [--tail N] [--] ID` | `L` | Divergent | file logs, no journal query model. |
| `systemctl reset-failed [UNIT...]` | none | none | Missing | no direct equivalent. |

## 4) Notable Semantic Differences

### 4.1 Enable/disable model
- systemd: install-state linkage model.
- supervisor: persisted runtime overrides (`supervisor--enabled-override`).
- Effective precedence is:
  - masked => disabled,
  - explicit enable/disable override,
  - config default.

### 4.2 Start/disabled/reconcile interaction
- `start` on disabled unit succeeds (session-only).
- `reconcile` preserves disabled units that were manually started.
- masked units are always stopped/blocked even if manually started before.

### 4.3 Reload split
- `daemon-reload`: disk-to-memory definition refresh only.
- `reload UNIT...`: per-unit hot-swap behavior.
- `reconcile`: global convergence operation (plan + snapshot + action application).

### 4.4 Status contract
- `status` with IDs:
  - prints valid details,
  - prints invalid details,
  - appends not-found lines for truly missing IDs,
  - exits non-zero if any IDs are missing.
- `status` without IDs delegates to `list-units`.

### 4.5 Logging model
- systemd: journald + `journalctl`.
- supervisor: per-service file logs + `logs` command/dashboard `L`.

## 5) Broader Architectural Differences (Not Just Commands)

### 5.1 Runtime scope
- systemd is an init/service manager ecosystem.
- supervisor.el is an Emacs-embedded supervisor for user-session/service orchestration.

### 5.2 Unit definition model
- systemd: `.service`/`.timer` unit file grammar.
- supervisor: Elisp program entries plus optional modular `.el` unit files (`supervisor-units.el`).

### 5.3 Configuration workflow
- Supervisor supports:
  - mixed legacy list + unit-file merge,
  - unit-file scaffold-on-edit,
  - validate-on-save in dashboard edit mode,
  - explicit migration helper (`supervisor-migrate-config`).

## 6) Missing or Intentionally Not Mirrored
- No `reset-failed`.
- No journald/journal query model.
- No attempt at full systemd unit taxonomy and full `systemctl` command universe.

## 7) Overall Position
- The project is now close to systemctl mental-model parity for day-to-day service operations.
- Remaining differences are mostly intentional and tied to the Emacs/runtime architecture, not accidental naming drift.

## TODO (Running)
1. Unit-files-only mode (decided):
- Drop legacy `supervisor-programs` runtime path and move to unit files only.
- Rationale:
  - codebase simplicity,
  - familiarity with systemd-style unit-file workflows,
  - reduced feature/behavior complexity,
  - smoother UX consistency for commands like `edit`.

2. Remove `reconcile` from user-facing surface (decided):
- Remove `reconcile` command and dashboard/menu/help references.
- Shift users to explicit, familiar flows with `daemon-reload`, `reload`, `start/stop/restart`, `enable/disable`, `mask/unmask`.
- Preserve precision and avoid bulk implicit convergence behavior.

3. Restart policy model alignment (decided):
- Replace binary restart policy (`on|off`) with systemd-style subset:
  - `no` (default): never restart automatically.
  - `on-success`: restart only on clean exit.
  - `on-failure`: restart on non-zero exit, unclean signal exit, timeout, or watchdog expiry.
  - `always`: restart regardless of exit cause.
- Apply this model consistently in CLI, dashboard, parser/validation, runtime behavior, and docs.

4. Oneshot option rename (decided):
- Rename `:async` to `:oneshot-async`.
- Rename `:oneshot-wait` to `:oneshot-blocking`.
- Keep behavior semantics identical during rename (blocking and async remain inverse).

5. Execute full consistency pass for the rename:
- Parser and defaults (`supervisor--oneshot-wait-p`, defcustom interactions).
- Validation rules and error messages.
- README handbook and examples.
- ERT coverage for parse/validate/runtime behavior.

6. Documentation clarity task:
- Add a short “systemd comparison note” for oneshot behavior (`Type=oneshot`, startup waiting, timeout model) so users understand why supervisor has explicit blocking/async control.

7. Log rotation architecture shift (decided):
- Remove startup log rotation from `supervisor-start` (delete the `supervisor--rotate-logs` startup behavior).
- Remove built-in `supervisor--rotate-logs` implementation from core.
- Add a dedicated `sbin` log-rotation script (follow `sbin/supervisorctl` header conventions/comments style).
- Script contract must include a simple retention option in days (for example `--keep-days N`).
- Keep retention logic KISS: compare file age against day threshold using system file timestamps; avoid month/leap-year calendar logic.
- Add a default log-rotation unit file plus default timer entry.
- Keep timer subsystem optional, but change default mode to enabled.
- Rotation behavior policy:
  - timer subsystem disabled => no automatic log rotation,
  - timer subsystem enabled (default) => automatic daily log rotation.

8. Logging pipeline robustness and scale alignment (decided):
- Replace per-chunk synchronous `write-region` logging path with supervisor-managed external log writers.
- Implement a private helper (for example `libexec/supervisor-logd`) that:
  - keeps target log FD open with append semantics,
  - receives service output over stdin/pipe,
  - supports explicit reopen signal (HUP/USR1) after rotation,
  - enforces per-file hard cap (`--max-file-size-bytes N`) by rotating/reopening when exceeded.
- Supervisor must auto-manage log writers for users (no user-defined log writer units/scripts):
  - auto-start writer when unit logging is enabled,
  - auto-stop writer on unit stop/restart/shutdown,
  - track writer PID/state internally.
- Add a separate global-prune script in `sbin` (for example `sbin/supervisor-log-prune`) that:
  - enforces directory-wide hard cap (`--max-total-bytes N`),
  - uses a shared lock file to coordinate across multiple writers/invocations,
  - prunes oldest rotated logs first (never delete active current log files).
- Integrate with log maintenance flow:
  - default logrotate oneshot + timer runs scheduled maintenance,
  - global prune script is run on schedule and may also be triggered by `supervisor-logd` after local rotation (throttled),
  - post-rotate reopen signal is sent to managed writers so they bind to new files.

9. CLI terminology normalization updates (decided):
- Rename `validate` command to `verify` (hard ABI break, no alias).
- Keep `blame` command name as-is for now.
- Do not introduce an `analyze` namespace/shim at this time.

10. Logging command surface stability (decided):
- Keep existing logging user interface names and flows:
  - CLI: `logging`, `logs`
  - Dashboard: logging toggle and log-view actions
- Apply logging changes underneath these interfaces via backend architecture updates only.

11.
- [ ] Improve interactive transient-menu keybinds to be more standardized/common/mnemonic.
TBA
