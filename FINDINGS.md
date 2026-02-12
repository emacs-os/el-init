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

## Proposed Target Table (Recommended Direction)

| Capability / command family | Target state recommendation | Why | Suggested path |
|---|---|---|---|
| `status`, `start`, `stop`, `restart`, `kill` | Full 1:1 operational parity target | These are the core operator verbs users expect to behave predictably | Keep names; tighten edge semantics where needed; ensure interactive has equivalent stop/restart affordances. |
| `show` | Adopt 1:1 naming target (`show`) | `describe` is understandable but non-standard | Rename `describe` to `show` (or replace outright if ABI breaks are acceptable). |
| `list-dependencies` | Adopt mainstream naming target | `graph` is clear but less standard than `list-dependencies` | Rename `graph` to `list-dependencies` (optionally keep graph-style output mode). |
| `list-timers` | Adopt mainstream naming target | `timers` is close, but standard command name improves predictability | Rename `timers` to `list-timers` (or support both then remove old name if desired). |
| `is-active`, `is-enabled`, `is-failed` | Add for script compatibility (strongly recommended) | High value for automation parity | Add strict exit-code commands mapped from existing state; no model rewrite required. |
| `reset-failed` | Add if parity is a goal | Common operational recovery action | Implement by clearing failed/crash-loop markers for selected IDs. |
| `enable` / `disable` | Keep names, but either fully align semantics later or rename to policy-scoped verbs | Current semantics are not systemd install-state semantics | Short term: explicit docs; long term: choose between true install-state model or rename to avoid ambiguity. |
| `reload` | Keep absent unless true reload semantics are introduced | Avoid semantic confusion | Continue using `reconcile` as canonical supervisor-specific verb. |
| `reconcile` | Preserve as supervisor-unique | Useful and explicit; not a systemctl equivalent | Keep as-is; document as supervisor control-plane convergence action. |
| `validate` | Preserve as supervisor-unique | Useful built-in static validation | Keep as-is; optionally add `verify` alias only if desired later. |
| `restart-policy`, `logging` | Preserve as supervisor-unique | Valuable runtime toggles not directly modeled by systemctl command names | Keep as-is, clearly documented as supervisor policy controls. |
| Dashboard/TUI workflow | Preserve as supervisor-unique “icing” | Strong UX differentiator beyond systemctl | Keep and continue parity with CLI semantics. |

## Suggested Next Compatibility Wave
1. Decide whether to rename `describe -> show`, `graph -> list-dependencies`, `timers -> list-timers` in one breaking pass.
2. Add script-probe commands (`is-active`, `is-enabled`, `is-failed`) for automation parity.
3. Add an explicit interactive `stop` action so dashboard has direct parity with CLI/systemctl stop intent.
4. Decide long-term semantic plan for `enable`/`disable`: true systemctl-like install-state or explicit supervisor-policy naming.

## Final Where-We-Stand Summary
- The biggest semantic confusion points from prior findings (`reload`, `kill`) are now significantly cleaner.
- Remaining gaps are now mostly command naming parity and script-oriented compatibility verbs.
- Supervisor-unique strengths (reconcile, validation, staged startup, integrated dashboard) are clear and worth preserving.
