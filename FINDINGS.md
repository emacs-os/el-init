# FINDINGS: supervisorctl vs systemctl (systemd) — Current State Only

Date: 2026-02-12
Repository: supervisor.el
Status: Descriptive snapshot of current code behavior only (no planning content)

## Scope
This document records the current behavior and command surface of:
- `supervisorctl` (CLI)
- supervisor dashboard interactive mode (`M-x supervisor`)

It compares those current behaviors against common `systemctl` expectations.

## Evidence Source
- `supervisor-cli.el`
- `supervisor-dashboard.el`
- `supervisor-core.el`
- `sbin/supervisorctl`

## Current Command Surface

### supervisorctl (CLI)
Current commands:
- `status`, `list`, `describe`
- `start`, `stop`, `restart`, `reconcile`, `validate`
- `enable`, `disable`, `restart-policy`, `logging`
- `blame`, `graph`, `logs`, `kill`, `ping`, `version`, `timers`

Notable current semantics:
- `list` is an alias of `status`.
- `reconcile` exists; `reload` is not a CLI command.
- `kill` is signal-only and does not mark manual-stop restart suppression.
- `stop`/`restart` (all services path) attempt graceful stop first, then force-stop fallback.

### Dashboard (interactive)
Current key actions include:
- Lifecycle: `s` (start), `k`/`K` (kill)
- Policy toggles: `e` (enabled), `r` (restart), `l` (logging)
- Introspection: `i`, `d`, `D`, `B`, `L`
- UI/filtering: `g`, `G`, `f`, `t`, `?`, `h`, `q`

Current gap vs lifecycle parity:
- No dedicated dashboard `stop` command.
- No dedicated dashboard `restart` command.

## Current Comparison Table

| Common systemctl command/workflow | Current supervisorctl | Current interactive behavior | Current alignment note |
|---|---|---|---|
| `systemctl status [UNIT...]` | `status [ID...]` / `list [ID...]` | Main dashboard table | Current output is overview/table-oriented, not systemctl-style detailed status view. |
| `systemctl list-units` | `status` / `list` | Main dashboard table | Current behavior is closest to list-units-style overview. |
| `systemctl show UNIT` | `describe ID` | `i` (entry details), `d`/`D` (deps) | Similar intent, different naming/output format. |
| `systemctl start UNIT...` | `start [--] ID...` | `s` | Close behavior parity. |
| `systemctl stop UNIT...` | `stop [--] ID...` | No dedicated stop action | CLI has stop; dashboard does not. |
| `systemctl restart UNIT...` | `restart [--] ID...` | No dedicated restart action | CLI has restart; dashboard does not. |
| `systemctl kill UNIT [--signal=]` | `kill [--signal SIG] [--] ID` | `k` / `K` | Signal behavior aligned (signal-only). |
| `systemctl reload UNIT...` | No equivalent | No equivalent | Not present; supervisor has `reconcile` instead (different operation). |
| `systemctl daemon-reload` | No equivalent | No equivalent | Not present. |
| `systemctl enable UNIT...` | `enable [--] ID...` | `e` toggle | Same verb, different underlying model (override-based, not unit-file install state). |
| `systemctl disable UNIT...` | `disable [--] ID...` | `e` toggle | Same divergence as enable. |
| `systemctl is-active UNIT` | No equivalent | No equivalent | Not present. |
| `systemctl is-enabled UNIT` | No equivalent | No equivalent | Not present. |
| `systemctl is-failed UNIT` | No equivalent | No equivalent | Not present. |
| `systemctl reset-failed [UNIT...]` | No equivalent | No equivalent | Not present. |
| `systemctl list-dependencies [UNIT]` | `graph [ID]` | `d` / `D` | Dependency information exists; nomenclature differs. |
| `systemctl list-timers` | `timers` | Timers section in dashboard | Similar capability, different naming. |
| `systemctl cat UNIT` | No equivalent | No equivalent | Not present. |
| `systemctl edit UNIT` | No equivalent | No equivalent | Not present. |
| `systemctl mask/unmask UNIT` | No equivalent | No equivalent | Not present. |
| `journalctl -u UNIT` workflow | `logs ID [--tail N]` | `L` opens log file | File-log model; no journal semantics. |
| `systemctl --version` | `version` | N/A | Equivalent intent. |

## Current Divergence Summary
- `status` currently behaves like overview/list output, closer to `list-units` semantics.
- `describe`, `graph`, and `timers` provide related capabilities but under non-systemctl names.
- `enable`/`disable` naming matches systemctl, but behavior is override-based rather than systemd install-state behavior.
- No current equivalents for `is-*`, `reset-failed`, `cat`, `edit`, `mask/unmask`, or `daemon-reload`.
- Logging is currently file-based (`logs`, dashboard `L`) rather than journal-based.

## Current Strengths (As Implemented)
- Consistent CLI + dashboard control surface for core operations.
- JSON output mode in CLI.
- Explicit `reconcile` operation for config/runtime convergence.
- Signal-only `kill` semantics aligned with user expectation of kill-as-signal.
