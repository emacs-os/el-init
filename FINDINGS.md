# FINDINGS: supervisorctl vs systemctl (systemd)

Date: 2026-02-12
Repository: supervisor.el

## Scope
This report compares `supervisorctl` (this repository) with `systemctl` terminology and command surface.

The goal is not to replicate all of systemd, but to identify:
- what `supervisorctl` already covers,
- what it does not cover,
- where naming/semantics diverge from `systemctl`,
- how to standardize terms toward systemd conventions without breaking users.

## Evidence Sources
- `supervisorctl` command surface: `supervisor-cli.el`, `sbin/supervisorctl`
- Dashboard vocabulary and status labels: `supervisor-dashboard.el`, `README.org`
- Core state model and lifecycle semantics: `supervisor-core.el`
- Local system reference: `systemctl --help` (systemd 259.1), `systemd-analyze --help`, `man systemctl`

## Executive Summary
- `supervisorctl` already matches core lifecycle verbs (`start`, `stop`, `restart`, `reload`, `enable`, `disable`, `kill`, `status`) and supports timers.
- Major differences are mostly terminology and semantics, not missing basic controls.
- Biggest deviations are object naming (`entry`/`program` vs `unit`/`service`), state labels (`running`/`dead`/`pending`), and several command names (`describe`, `validate`, `graph`, `blame`, `ping`, `restart-policy`, `logging`).
- Two same-name commands carry different semantics from systemd and should be treated as high-priority alignment items: `kill` and `reload`.
- A non-breaking alias strategy can get close to systemctl terminology quickly.

## What supervisorctl Has Today

### Lifecycle and control
- `start`, `stop`, `restart`, `reload`, `kill`
- Per-ID or all-service operation for start/stop/restart
- Signal selection for `kill` (`--signal`)

### Service policy toggles
- `enable`, `disable`
- `restart-policy on|off`
- `logging on|off`

### Introspection and diagnostics
- `status`, `list` (alias), `describe`
- `logs` (file tailing)
- `graph` (dependency edges)
- `blame` (startup timing)
- `validate`
- `timers`
- `ping`, `version`

### UX and integration
- Stable human and JSON output modes (`--json`)
- Interactive dashboard with keybindings (`M-x supervisor`)
- Persistent override file for enabled/restart/logging runtime policy

## Feature Comparison Matrix

| Area | supervisorctl | systemctl | Gap Summary |
|---|---|---|---|
| Basic lifecycle | Yes | Yes | Close parity |
| Boolean activity checks | No (`is-active`, `is-failed`, `is-enabled` absent) | Yes | Missing script-friendly checks |
| Unit file management | Partial (`enable`/`disable`) | Rich (`mask`, `unmask`, `preset`, `edit`, `revert`, `link`, etc.) | Major surface gap |
| Dependency inspection | `graph` | `list-dependencies` (+ `systemd-analyze dot`) | Similar capability, different naming/output |
| Validation | `validate` | Mostly `systemd-analyze verify` (not systemctl core verb) | Function exists, name/tool mismatch |
| Startup timing | `blame` | `systemd-analyze blame` | Capability exists, command family mismatch |
| Logs | `logs --tail N` (file-based) | status log excerpt + journal integration (`-n`, `-o`, journalctl) | Less filtering, no journal semantics |
| Timers | `timers` | `list-timers` + timer unit model | Similar concept, different naming/model depth |
| Manager/system ops | No | Extensive (`daemon-reload`, reboot/power, rescue, etc.) | Out of scope for now |
| Jobs/transactions | No explicit surface | Yes (`list-jobs`, `cancel`, job modes) | Missing |

## Terminology Deviations (Primary Focus)

### 1) Object model nouns
Current supervisor terms:
- `entry`, `entries`, `program`, `supervisor-programs`
- service types: `simple`, `oneshot`
- startup `stage1..stage4`

systemd terms:
- `unit` (service/timer/socket/target/...)
- `service`, `timer`
- dependency ordering rather than fixed stages

Assessment:
- `entry` and `program` are the biggest terminology mismatch in user-facing CLI/dashboard output.
- `simple` and `oneshot` already align with systemd service type vocabulary.
- `stage` is custom and useful, but not systemd-native terminology.

### 2) State vocabulary
Current supervisor service states (user-facing):
- `running`, `done`, `failed`, `dead`, `pending`, `stopped`, `invalid`

systemd active-state vocabulary:
- `active`, `inactive`, `failed`, `activating`, `deactivating`, `maintenance`, `reloading`, `refreshing`

Assessment:
- `dead` and `pending` are the least systemd-like labels.
- `done` (for oneshot) is not wrong, but systemd users expect `active (exited)`/substates framing.
- `invalid` is config validation state, which systemctl does not expose as a primary runtime state label in the same way.

### 3) Command naming deviations
Commands with close systemctl analog but different name:
- `describe` vs `show`/`status`
- `graph` vs `list-dependencies` (or `systemd-analyze dot` for graph rendering)
- `timers` vs `list-timers`

Commands closer to systemd-analyze than systemctl:
- `blame` (closest: `systemd-analyze blame`)
- `validate` (closest: `systemd-analyze verify`)

Commands with no standard systemctl analog:
- `restart-policy`
- `logging` (as capture toggle)
- `ping`

### 4) Same-name semantic divergence (highest risk)
`enable` / `disable`:
- supervisorctl: runtime/startup policy override persisted in supervisor state file
- systemctl: unit-file install state (symlink management)

`kill`:
- supervisorctl: sends signal and marks manually stopped, suppressing restart
- systemctl: sends signal to unit processes; restart behavior remains per unit policy

`reload`:
- supervisorctl: reconcile config and runtime set
- systemctl: ask unit to reload configuration (if supported), not a full reconcile action

These are the most confusing differences because names look identical but behavior is different.

## What supervisorctl Has That systemctl Does Not (Directly)
- Built-in schema validation command in same CLI (`validate`)
- Built-in deterministic startup stage model (`stage1..stage4`)
- Combined CLI + TUI (dashboard) in one toolchain
- Stable JSON contracts by command

## What supervisorctl Lacks (Relative to systemctl Family)
- `is-active`, `is-failed`, `is-enabled` exit-code-oriented checks
- `try-restart`, `reload-or-restart`, `try-reload-or-restart`
- `reset-failed`
- `show`-style property output
- `list-unit-files`-style install-state view
- `mask`/`unmask` semantics
- richer log selection (`--follow`, `--since`, `--until`, output modes)
- dependency views beyond edge list (`before`/`after` trees)

## Standardization Recommendations

### Priority 0: Non-breaking terminology alignment (add aliases)
Add command aliases while keeping current commands:
- `show` -> existing `describe`
- `list-units` -> existing `list`
- `list-dependencies` -> existing `graph`
- `list-timers` -> existing `timers`
- `verify` -> existing `validate`
- `is-active`, `is-enabled`, `is-failed` -> new thin commands over current state

Update user-facing strings:
- prefer `service` (or `unit`) over `entry` in CLI/dashboard text
- keep internal symbol names unchanged initially to avoid churn

### Priority 1: Semantic clarifications for same-name commands
- `kill`: decide whether to align with systemctl behavior (signal only) or keep current restart-suppress semantics.
- `reload`: either rename current behavior to `reconcile` and keep `reload` alias documented as reconcile, or implement a systemctl-like `reload` subset and move current behavior to `reconcile`.
- `enable`/`disable`: explicitly label as supervisor policy enablement in help text; add `is-enabled` for scripting parity.

### Priority 2: State terminology bridge
Keep current status values for backward compatibility, but add a systemd-style view:
- `active_state`: `active`/`inactive`/`failed`/`activating`...
- `sub_state`: mapped detail (`running`, `dead`, `done`, `pending`, etc.)

This can be exposed first in JSON, then optional human columns.

### Priority 3: Command-family separation for analysis verbs
Consider moving analysis-style verbs under an `analyze` namespace:
- `supervisorctl analyze blame`
- `supervisorctl analyze graph`
- `supervisorctl verify`

Keep old top-level verbs as aliases.

## Proposed Terminology Map

| Current term | Suggested standard term | Notes |
|---|---|---|
| entry | service (or unit) | `service` is practical because this supervisor manages service-like processes |
| programs | services | Keep `supervisor-programs` variable internally for compatibility |
| describe | show | Keep `describe` alias |
| validate | verify | Matches `systemd-analyze verify` wording |
| graph | list-dependencies | Keep `graph` alias for current users |
| timers | list-timers | Keep `timers` alias |
| dead | failed (substate crash-loop) | Better fit for systemd mental model |
| pending | activating / waiting | Depends on implementation detail availability |

## Implementation Plan (Low-Risk Path)
1. Add aliases and help-text updates only.
2. Add `is-active`/`is-enabled`/`is-failed` with strict exit codes.
3. Introduce systemd-style `active_state` + `sub_state` in JSON.
4. Revisit `kill` and `reload` semantics with explicit compatibility decision.
5. Update README CLI handbook terminology and examples.

## Final Assessment
`supervisorctl` is already strong in core lifecycle control and visibility for a user-session supervisor.

The main blocker to "systemctl-like" feel is terminology and semantic expectations, not missing basic operations.

A staged alias-first standardization can deliver systemctl familiarity quickly without breaking current users.
