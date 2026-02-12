# FINDINGS: supervisorctl vs systemctl (systemd) — Where We Stand

Date: 2026-02-12
Repository: supervisor.el
Status: Current to latest normalization work in this branch

## Scope
This document compares `supervisorctl` and the supervisor dashboard (interactive mode) against common `systemctl` expectations.

Focus:
- current command/behavior parity,
- current divergence points,
- explicit guidance on what to align 1:1 next,
- explicit guidance on what should stay supervisor-unique.

## Current Status Snapshot (What Changed Since Prior Findings)
- `reload` has been removed from `supervisorctl`; canonical command is now `reconcile`.
- `kill` semantics were changed to signal-only (no restart suppression), aligned with `systemctl kill` expectations.
- Dashboard `k`/`K` kill now follows the same signal-only semantics as CLI kill.
- `stop`/`restart` (all services path) now uses graceful stop first, with forced fallback.
- No compatibility alias was kept for `reload` (intentional ABI break per project direction).

## High-Level Assessment
- Strong parity today: `start`, `stop`, `restart`, `kill`, `status`.
- Partial parity: `describe` (vs `show`), `graph` (vs `list-dependencies`), `timers` (vs `list-timers`), `logs`.
- Intentional supervisor-specific commands: `reconcile`, `validate`, `restart-policy`, `logging`, staged startup model.
- Major missing systemctl-style script probes: `is-active`, `is-enabled`, `is-failed`, plus `reset-failed`.

## Current Command Surface (Supervisor)
CLI (`supervisorctl`):
- `status`, `list`, `describe`, `start`, `stop`, `restart`, `reconcile`, `validate`
- `enable`, `disable`, `restart-policy`, `logging`
- `blame`, `graph`, `logs`, `kill`, `ping`, `version`, `timers`

Interactive (`M-x supervisor` dashboard):
- service lifecycle actions: `s` (start), `k`/`K` (kill)
- policy toggles: `e` (enabled), `r` (restart), `l` (logging)
- introspection: `i`, `d`, `D`, `B`, `L`
- refresh/filter/UI: `g`, `G`, `f`, `t`, `?`, `h`, `q`

## Current Comparison Table

| Common systemctl commands | supervisorctl | supervisor interactive | Current parity / divergence notes |
|---|---|---|---|
| `systemctl status [UNIT...]` | `status [ID...]` / `list [ID...]` | Main dashboard table view | Close parity. Different state vocabulary (`running`, `pending`, `dead`, etc.). |
| `systemctl list-units` | `status` / `list` | Main dashboard view | Close parity for service listing. |
| `systemctl show UNIT` | `describe ID` | `i` (entry details), `d`/`D` (deps) | Partial parity. Name differs; output model differs. |
| `systemctl start UNIT...` | `start [--] ID...` | `s` | Close parity. Disabled entries are blocked unless enabled. |
| `systemctl stop UNIT...` | `stop [--] ID...` | No dedicated stop key | Partial parity. CLI exists; dashboard has kill, not explicit stop verb. |
| `systemctl restart UNIT...` | `restart [--] ID...` | No dedicated restart key (manual `k` then `s`) | Partial parity in interactive mode. |
| `systemctl kill UNIT [--signal=]` | `kill [--signal SIG] [--] ID` | `k` / `K` (SIGTERM, confirm/no-confirm) | Now closely aligned on semantics (signal-only; restart policy remains in effect). |
| `systemctl reload UNIT...` | No equivalent | No equivalent | Intentional divergence. Supervisor uses `reconcile` instead (different operation). |
| `systemctl daemon-reload` | No equivalent | No equivalent | Missing by design (no systemd manager/unit-file model). |
| `systemctl enable UNIT...` | `enable [--] ID...` | `e` toggle | Same verb, different model: supervisor runtime/startup override persistence, not unit install-state symlink model. |
| `systemctl disable UNIT...` | `disable [--] ID...` | `e` toggle | Same divergence as `enable`. |
| `systemctl is-active UNIT` | No equivalent | No equivalent | Missing. |
| `systemctl is-enabled UNIT` | No equivalent | No equivalent | Missing. |
| `systemctl is-failed UNIT` | No equivalent | No equivalent | Missing. |
| `systemctl reset-failed [UNIT...]` | No equivalent | No equivalent | Missing. |
| `systemctl list-dependencies [UNIT]` | `graph [ID]` | `d` / `D` | Partial parity. Capability exists, naming/output differ. |
| `systemctl list-timers` | `timers` | Timers section in dashboard | Partial parity. Similar concept; supervisor timer model is custom. |
| `systemctl cat UNIT` | No equivalent | No equivalent | Missing. |
| `systemctl edit UNIT` | No equivalent | No equivalent | Missing. |
| `systemctl mask/unmask UNIT` | No equivalent | No equivalent | Missing. |
| `journalctl -u UNIT` workflow | `logs ID [--tail N]` | `L` (open log file) | Partial parity. File-based logs; no journal query semantics. |
| `systemctl --version` | `version` | N/A | Equivalent intent. |

## Divergence Hotspots (Current)
1. `enable`/`disable`
- Same words as systemctl, but not same install-state behavior.
- Current behavior is supervisor override persistence.

2. `stop` behavior split by interface
- CLI has explicit `stop`.
- Interactive mode currently lacks a dedicated stop action and relies on kill/start actions.

3. Missing script-first probes
- No `is-active` / `is-enabled` / `is-failed` equivalents yet.

4. Reconcile naming decision
- This is now clean: `reconcile` is explicit and avoids overloading `reload`.

## Proposed Target Table (Systemctl-First Policy)

Policy used here:
- Default direction is 1:1 `systemctl` parity where practical.
- Only the maintainer-specified exceptions are marked unplanned/as-is.

| systemctl command / workflow | Current supervisorctl | Target state | Status | Notes |
|---|---|---|---|---|
| `status [UNIT...]` | `status [ID...]` / `list [ID...]` | Keep parity and tighten wording toward service/unit language | Planned parity | Already close; mainly terminology cleanup. |
| `list-units` | `list` / `status` | Add `list-units` naming parity | Planned parity | May keep `list` for convenience if desired. |
| `show UNIT` | `describe ID` | Rename to `show` | Planned parity | Direct nomenclature alignment target. |
| `start UNIT...` | `start [--] ID...` | Keep 1:1 behavior | Planned parity | Already close. |
| `stop UNIT...` | `stop [--] ID...` | Keep 1:1 behavior in CLI and interactive | Planned parity | Add explicit interactive stop action for full parity. |
| `restart UNIT...` | `restart [--] ID...` | Keep 1:1 behavior in CLI and interactive | Planned parity | Add explicit interactive restart action for symmetry. |
| `kill UNIT [--signal=]` | `kill [--signal SIG] [--] ID` | Keep current behavior | Planned parity | Semantics now aligned (signal-only; no restart suppression side effects). |
| `reload UNIT...` | No equivalent (`reconcile` is different) | Add true reload semantics | Planned parity | Keep `reconcile` as separate supervisor-specific operation. |
| `daemon-reload` | No equivalent | Add nearest practical manager-reload equivalent | Planned parity | Scoped to supervisor’s config/plan domain, not full systemd manager scope. |
| `enable UNIT...` | `enable [--] ID...` (override model) | Align semantics toward systemctl-style enable/disable | Planned parity | Current behavior diverges; requires model decision/refactor. |
| `disable UNIT...` | `disable [--] ID...` (override model) | Align semantics toward systemctl-style enable/disable | Planned parity | Same divergence as above. |
| `list-dependencies [UNIT]` | `graph [ID]` | Rename/reshape to `list-dependencies` | Planned parity | Underlying dependency info already exists. |
| `list-timers` | `timers` | Rename/reshape to `list-timers` | Planned parity | Underlying timer listing exists. |
| `cat UNIT` | No equivalent | Keep missing | Unplanned (as requested) | Explicitly out of plan. |
| `edit UNIT` | No equivalent | Keep missing | Unplanned (as requested) | Explicitly out of plan. |
| `mask/unmask UNIT` | No equivalent | Keep missing | Unplanned (as requested) | Explicitly out of plan. |
| `journalctl -u UNIT` workflow | `logs ID [--tail N]` | Keep file-log model as-is | Unplanned (as requested) | Intentional non-journal design choice. |
| `--version` | `version` | Keep as-is | Unplanned change (as requested) | Already equivalent intent. |
| `is-active UNIT` | No equivalent | Keep missing | Unplanned (as requested) | Explicitly out of plan. |
| `is-enabled UNIT` | No equivalent | Keep missing | Unplanned (as requested) | Explicitly out of plan. |
| `is-failed UNIT` | No equivalent | Keep missing | Unplanned (as requested) | Explicitly out of plan. |
| `reset-failed [UNIT...]` | No equivalent | Keep missing | Unplanned (as requested) | Explicitly out of plan. |
| Supervisor extra: `reconcile` | `reconcile` | Keep as supervisor-specific extension | Planned preserve | Not a systemctl equivalent; useful control-plane convergence verb. |
| Supervisor extra: `validate` | `validate` | Keep as supervisor-specific extension | Planned preserve | Valuable built-in validation command. |
| Supervisor extra: `restart-policy`, `logging` | `restart-policy`, `logging` | Keep as supervisor-specific extensions | Planned preserve | Intentional policy controls beyond systemctl command set. |

## Suggested Next Compatibility Wave (Based on This Policy)
1. Rename `describe -> show`, `graph -> list-dependencies`, `timers -> list-timers`.
2. Add explicit interactive `stop` and `restart` actions for dashboard parity.
3. Design and implement true `reload UNIT...` semantics separate from `reconcile`.
4. Define and implement a practical `daemon-reload` equivalent in supervisor’s scope.
5. Decide and execute semantic alignment plan for `enable`/`disable`.

## Final Where-We-Stand Summary
- The biggest semantic confusion points from prior findings (`reload`, `kill`) are now significantly cleaner.
- Remaining gaps are now mostly command naming parity and script-oriented compatibility verbs.
- Supervisor-unique strengths (reconcile, validation, staged startup, integrated dashboard) are clear and worth preserving.
