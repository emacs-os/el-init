# FINDINGS: supervisorctl vs systemctl (systemd) — Current State

Date: 2026-02-12
Repository: supervisor.el
Status: Post-Phase 10 snapshot of current command surface

## Scope
This document records the current behavior and command surface of:
- `supervisorctl` (CLI)
- supervisor dashboard interactive mode (`M-x supervisor`)

It compares those current behaviors against common `systemctl` expectations.

## Evidence Source
- `supervisor-cli.el`
- `supervisor-dashboard.el`
- `supervisor-core.el`
- `supervisor-units.el`
- `sbin/supervisorctl`

## Current Command Surface

### supervisorctl (CLI)
Systemctl-compatible commands:
- `status [ID...]`, `list-units [ID...]`, `show ID`
- `start [-- ID...]`, `stop [-- ID...]`, `restart [-- ID...]`
- `enable [--] ID...`, `disable [--] ID...`
- `mask [--] ID...`, `unmask [--] ID...`
- `kill [--signal SIG] [--] ID`
- `is-active ID`, `is-enabled ID`, `is-failed ID`
- `daemon-reload`, `reload [--] ID...`
- `cat ID`, `edit ID`
- `list-dependencies [ID]`, `list-timers`

Supervisor-specific commands:
- `reconcile`, `validate`
- `restart-policy (on|off) [--] ID...`, `logging (on|off) [--] ID...`
- `blame`, `logs [--tail N] [--] ID`, `ping`, `version`

Notable current semantics:
- `status` with IDs gives detailed per-unit output; without IDs delegates to `list-units`.
- `reload` hot-reloads specific units; `reconcile` performs global convergence.
- `daemon-reload` re-reads config from disk without affecting runtime.
- `start` on a disabled unit succeeds (session-only, no override change; only mask blocks).
- `is-active`/`is-enabled`/`is-failed` use systemctl-compatible exit codes (0/1/3/4).
- `kill` is signal-only and does not mark manual-stop restart suppression.
- `stop`/`restart` (all services path) attempt graceful stop first, then force-stop fallback.

### Dashboard (interactive)
Key actions:
- Lifecycle: `s` (start), `x` (stop), `R` (restart), `k`/`K` (kill)
- Policy toggles: `e` (enabled), `m` (mask), `r` (restart), `l` (logging)
- Views: `c` (cat unit), `E` (edit unit), `L` (log), `d`/`D` (deps), `B` (blame)
- System: `u` (reload unit), `X` (daemon-reload)
- UI/filtering: `g`/`G` (refresh), `f`/`t` (filter), `?` (menu), `i`/`h`/`q`

## Current Comparison Table

| Common systemctl command/workflow | Current supervisorctl | Current interactive | Alignment status |
|---|---|---|---|
| `systemctl status [UNIT...]` | `status [ID...]` | Main dashboard table | **Aligned.** Dual behavior: detail with IDs, overview without. |
| `systemctl list-units` | `list-units [ID...]` | Main dashboard table | **Aligned.** Renamed from `list`/`status`. |
| `systemctl show UNIT` | `show ID` | `i` (entry details) | **Aligned.** Renamed from `describe`. |
| `systemctl start UNIT...` | `start [-- ID...]` | `s` | **Aligned.** Works on disabled units (session-only). |
| `systemctl stop UNIT...` | `stop [-- ID...]` | `x` | **Aligned.** Dashboard stop added. |
| `systemctl restart UNIT...` | `restart [-- ID...]` | `R` | **Aligned.** Dashboard restart added. |
| `systemctl kill UNIT [--signal=]` | `kill [--signal SIG] [--] ID` | `k` / `K` | **Aligned.** Signal-only semantics. |
| `systemctl reload UNIT...` | `reload [--] ID...` | `u` (System menu) | **Aligned.** Hot-reload specific units. |
| `systemctl daemon-reload` | `daemon-reload` | `X` (System menu) | **Aligned.** Re-reads config from disk. |
| `systemctl enable UNIT...` | `enable [--] ID...` | `e` toggle | **Aligned.** Override-based (intentional divergence from install-state). |
| `systemctl disable UNIT...` | `disable [--] ID...` | `e` toggle | **Aligned.** Override-based; disabled units can be manually started. |
| `systemctl mask/unmask UNIT` | `mask [--] ID...` / `unmask [--] ID...` | `m` toggle | **Aligned.** Mask overrides enable/disable. |
| `systemctl is-active UNIT` | `is-active ID` | N/A | **Aligned.** Exit 0/3/4 parity. |
| `systemctl is-enabled UNIT` | `is-enabled ID` | N/A | **Aligned.** Exit 0/1/4 parity. |
| `systemctl is-failed UNIT` | `is-failed ID` | N/A | **Aligned.** Exit 0/1/4 parity. |
| `systemctl reset-failed [UNIT...]` | No equivalent | No equivalent | Not present. |
| `systemctl list-dependencies [UNIT]` | `list-dependencies [ID]` | `d` / `D` | **Aligned.** Renamed from `graph`. |
| `systemctl list-timers` | `list-timers` | Timer dashboard section | **Aligned.** Renamed from `timers`. |
| `systemctl cat UNIT` | `cat ID` | `c` | **Aligned.** Shows raw unit file content. |
| `systemctl edit UNIT` | `edit ID` | `E` | **Aligned.** Scaffold creation if missing. |
| `journalctl -u UNIT` workflow | `logs [--tail N] [--] ID` | `L` opens log file | File-log model; no journal semantics. |
| `systemctl --version` | `version` | N/A | **Aligned.** |

## Intentional Divergences

- **enable/disable**: Override-based (runtime toggle) rather than systemd install-state
  (symlink creation). This is appropriate for an Emacs-embedded supervisor where unit
  installation is handled by config, not filesystem state.
- **reconcile**: Global convergence operation with no systemctl equivalent.  Supervisor's
  declarative model (plan → snapshot → actions → apply) requires an explicit converge step.
- **validate**: Config validation command with no systemctl equivalent.  Returns both
  service and timer invalid sets with exit code 4 on errors.
- **Logging model**: File-based logging rather than journal-based.  Appropriate for the
  Emacs context where journald is not assumed.
- **Server-unavailable exit code**: Uses 69 (EX_UNAVAILABLE from sysexits.h) instead of
  systemctl's exit codes to avoid collision.

## Remaining Gaps

- `reset-failed`: Not implemented.  Manual workaround: restart the unit.
- Journal-style log querying: Not applicable (file-based logging model).

## Current Strengths

- Consistent CLI + dashboard control surface for all operations.
- JSON output mode in CLI for all commands.
- Explicit `reconcile` operation for config/runtime convergence.
- `reload` for per-unit hot-swap vs `reconcile` for global convergence.
- `daemon-reload` for disk-to-memory config refresh without runtime impact.
- Signal-only `kill` semantics aligned with user expectation.
- systemctl-compatible exit codes for `is-*` predicates.
- Unit-file support with modular config, scaffold editing, and validate-on-save.
- Enable/disable follows systemctl session model: start on disabled works, reconcile
  preserves manually-started disabled units.
