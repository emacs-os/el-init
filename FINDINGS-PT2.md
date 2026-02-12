# FINDINGS-PT2.md

Date: 2026-02-12

## Scope and method

This report compares:

- systemd service-unit options (using your local `/usr/lib/systemd/system/*.service` corpus and `systemd-service-unit-docs.txt`), and
- current `supervisor.el` unit-file support (`supervisor-units.el`, `supervisor-core.el`).

Dataset used:

- 278 local `.service` files.
- Strict intersection and frequency counts computed from those files.
- Default/implicit semantics cross-checked from local man pages (`systemd.service`, `systemd.unit`, `systemd.exec`, `systemd.kill`) and your `systemd-service-unit-docs.txt`.

## Table 1: Local corpus snapshot

| Metric | Value |
|---|---:|
| `.service` files scanned | 278 |
| Files with `[Unit]` section | 278 |
| Files with `[Service]` section | 271 |
| Files with `[Install]` section | 120 |
| Files without `[Service]` | 7 |
| Files with `[Service]` but no `ExecStart=` | 3 |

Notes:

- The 7 without `[Service]` are special/reboot/halt style service units on this host.
- So: even inside `*.service`, `[Service]` is not 100% universal in practice.

## Table 2: What ALL units share in common (strict intersection)

| Directive | Present in 278/278? | Comment |
|---|---|---|
| `Unit.Description` | Yes (100%) | Only strict universal directive found in this corpus. |

Conclusion: the only truly universal baseline in this real corpus is `Description=`.

## Table 3: Practical baseline (high-frequency directives)

| Directive | Count | Percent |
|---|---:|---:|
| `Unit.Description` | 278 | 100.0% |
| `Service.ExecStart` | 268 | 96.4% |
| `Service.Type` | 223 | 80.2% |
| `Unit.Documentation` | 221 | 79.5% |
| `Unit.After` | 198 | 71.2% |
| `Unit.Before` | 152 | 54.7% |
| `Unit.DefaultDependencies` | 147 | 52.9% |
| `Unit.Conflicts` | 117 | 42.1% |
| `Install.WantedBy` | 105 | 37.8% |
| `Service.RemainAfterExit` | 94 | 33.8% |
| `Unit.Wants` | 64 | 23.0% |
| `Service.Restart` | 42 | 15.1% |
| `Unit.Requires` | 42 | 15.1% |

Service-section normalization (for units that actually have `[Service]`, n=271):

| Directive | Count | Percent of `[Service]` units |
|---|---:|---:|
| `ExecStart=` | 268 | 98.9% |
| `Type=` | 223 | 82.3% |

## Table 4: Baseline relevance for `supervisor.el` (userland supervisor)

| Baseline item | Relevance | Why |
|---|---|---|
| `Description=` | High | Human-readable metadata in dashboard/CLI is broadly useful. |
| `ExecStart=` | Mandatory | Core command launcher equivalent (maps to `:command`). |
| `Type=` | High | Maps well to `simple`/`oneshot`; additional systemd types are likely out-of-scope. |
| `After=` | High | Startup ordering/dependency graph is directly relevant. |
| `Before=` | Medium | Useful counterpart to `After=` but not strictly required if `After=` is canonical. |
| `Requires=` | High | Hard dependency semantics are useful and already conceptually present. |
| `Wants=` | Medium-High | Soft dependency is useful and currently missing in supervisor semantics. |
| `DefaultDependencies=` | Low | Mostly boot/init manager semantics, weak fit for userland Emacs supervisor. |
| `WantedBy=` | Medium | Useful only for install/enable linkage model; not required for runtime process control. |
| `Conflicts=` | Medium | Useful but optional for minimal userland scope. |

## Table 5: Common implicit/default semantics in systemd (important for compatibility)

| Option | systemd default / implicit behavior | Why this matters for supervisor parity |
|---|---|---|
| `Type=` | Default `simple` when `ExecStart=` is set and `Type=` omitted. | If you add a `Type` key, this default should match. |
| `Restart=` | Default `no`. | Current supervisor default is effectively restart enabled for simple entries; this diverges strongly. |
| `RestartSec=` | Default `100ms`. | If restart policy is expanded, restart delay/backoff defaults need explicit policy. |
| `RemainAfterExit=` | Default `no`. | Oneshot active-state behavior depends on this. |
| `TimeoutStartSec=` | Manager default; for `Type=oneshot`, startup timeout disabled by default. | Supervisor currently has a finite oneshot timeout default; behavior diverges. |
| `TimeoutStopSec=` | Manager default. | Stop behavior needs explicit timeout contract if parity is desired. |
| `DefaultDependencies=` | Default `yes`. | Usually not relevant in non-init userland model. |
| `KillMode=` | Default `control-group`. | Supervisor kill/stop model differs (single process with manual suppression logic). |
| `KillSignal=` | Default `SIGTERM`. | Easy parity improvement for stop/kill semantics. |
| `User=` | Default root for system services; user instance runs as that user. | In a userland Emacs supervisor, user switching may be constrained or optional. |
| `WorkingDirectory=` | Defaults to `/` (system manager), user home in user manager context. | Commonly needed for app services; currently not first-class in supervisor unit files. |

## Table 6: Current supervisor unit-file options (supported absolutely, as of code now)

Source: `supervisor-units.el` keyword whitelist + `supervisor-core.el` parsing defaults.

| Supervisor unit key | Required in unit file? | Default if omitted | Closest systemd concept |
|---|---|---|---|
| `:id` | Yes (unit-file validator enforces) | Derived from command basename in generic parser | Unit name |
| `:command` | Yes (unit-file validator enforces) | None | `ExecStart=` |
| `:type` | No | `simple` | `Type=` (partial) |
| `:stage` | No | `stage3` | No direct systemd equivalent |
| `:delay` | No | `0` | Roughly timer-ish delay, not a direct service key |
| `:after` | No | `nil` | `After=` |
| `:requires` | No | `nil` | `Requires=` |
| `:enabled` | No | `t` | Roughly enabled/start participation |
| `:disabled` | No | inverse form of `:enabled` | Inverse convenience key |
| `:restart` | No | `t` | Simplified `Restart=` |
| `:no-restart` | No | inverse form of `:restart` | Inverse convenience key |
| `:logging` | No | `t` | Roughly `StandardOutput/StandardError` policy toggle |
| `:oneshot-wait` | No | global `supervisor-oneshot-default-wait` (currently `t`) | Roughly oneshot blocking behavior |
| `:async` | No | inverse helper to `:oneshot-wait` | No direct systemd key |
| `:oneshot-timeout` | No | global `supervisor-oneshot-timeout` (currently `30`) | Roughly `TimeoutStartSec` for oneshot |
| `:tags` | No | `nil` | No direct systemd equivalent |

Important reality check:

- Current unit-file schema is plist-based and supervisor-specific.
- It is not `.service`-syntax compatible (no `[Unit]/[Service]/[Install]` sections yet).

## Table 7: High-value options outside strict baseline (good candidates)

| Option | Why useful for supervisor.el | Priority |
|---|---|---|
| `Restart=` policy values (`no`, `on-success`, `on-failure`, `always`) | Major parity and behavior clarity; currently boolean-style in code surface. | High |
| `RestartSec=` | Needed to avoid hot-loop restarts and to match expected restart tuning. | High |
| `ExecReload=` | Gives meaningful `reload UNIT` semantics, separate from lifecycle reconcile-style operations. | High |
| `ExecStop=` | Better graceful-stop contract than a fixed kill behavior only. | High |
| `Environment=` / `EnvironmentFile=` | Common, practical, and widely expected for userland services. | High |
| `WorkingDirectory=` | Very common requirement for app startup correctness. | High |
| `Wants=` | Soft dependency model is often needed without hard coupling. | Medium-High |
| `Before=` | Symmetric ordering expression; useful in unit-file-only workflows. | Medium |
| `SuccessExitStatus=` | Important if supporting richer `on-success`/`on-failure` semantics. | Medium |
| `KillSignal=` | Good control for graceful stop behavior. | Medium |
| `KillMode=` | Useful if process-group management is expanded. | Medium |
| `RemainAfterExit=` | Useful for oneshot state semantics and status clarity. | Medium |
| `User=` / `Group=` | Potentially useful, but userland supervisor constraints may limit practical value. | Low-Medium |

## Table 8: Recommendation matrix (MUST HAVE | NICE TO HAVE | PROBABLY BLOAT)

| Bucket | Recommended options | Rationale |
|---|---|---|
| MUST HAVE | `ExecStart` equivalent, `Type` (limited), `After`, `Requires`, restart policy values (`no/on-success/on-failure/always`), `RestartSec`, `ExecStop`, `ExecReload`, `Environment`, `EnvironmentFile`, `WorkingDirectory` | Core process lifecycle + dependency + operability; high-value parity without init-system baggage. |
| NICE TO HAVE | `Wants`, `Before`, `SuccessExitStatus`, `KillSignal`, `KillMode`, `RemainAfterExit`, `Description` metadata, `Documentation` metadata | Improves operator ergonomics and compatibility but not required for minimal robust supervisor. |
| PROBABLY BLOAT (for this project scope) | `DefaultDependencies`, boot-target/install graph semantics beyond simple enable/disable, cgroup hardening matrix (`ProtectSystem`, `NoNewPrivileges`, namespace knobs), deep init-only controls (`OOMPolicy`, `FailureAction`, etc.) | These are mostly PID1/system-manager concerns and would add large complexity with limited userland payoff. |

## Final take

- Strict common baseline in real units is tiny (`Description=` only).
- Practical baseline for your scope is: command, type, ordering/dependencies, restart/stop/reload behavior, env/cwd.
- For `supervisor.el` as a userland supervisor, a focused subset is the right approach; full systemd service spec is unnecessary and likely harmful complexity.
