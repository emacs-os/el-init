# supervisor.el

`supervisor.el` is a user-session process supervisor for Emacs.

It starts, monitors, and stops background programs (for example EXWM session
services), with staged startup, dependency ordering, crash recovery, and a
dashboard UI.

Scope: this package supervises user-session processes from inside Emacs. It is
not PID 1 and not a system init replacement.

## What It Does Today

- Parses and validates `supervisor-programs` entries.
- Starts valid entries in `stage1 -> stage2 -> stage3 -> stage4`.
- Resolves `:after` dependencies within each stage.
- Detects dependency cycles and falls back to list order.
- Supports `simple` and `oneshot` process types.
- Supports delayed starts, oneshot timeout, and stage timeout.
- Tracks runtime state (running, crash-loop failed, oneshot completion).
- Provides a tabulated dashboard for control and inspection.
- Logs per-process stdout/stderr to files.
- Supports graceful async stop (`supervisor-stop`) and synchronous hard stop
  (`supervisor-stop-now`).
- Supports config-file watching and auto-reload.

## Requirements

- Emacs 27.1+
- `supervisor.el` in `load-path`

## Quick Start

```emacs-lisp
(require 'supervisor)

(setq supervisor-programs
      '(("xsetroot -cursor_name left_ptr" :type oneshot :stage stage1 :id "cursor")
        ("/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
         :type simple :stage stage2 :id "polkit" :restart t)
        ("nm-applet" :type simple :stage stage3 :id "nm" :restart t)
        ("pasystray" :type simple :stage stage4 :id "tray" :restart t)))

(supervisor-mode 1)
```

Recommended exit hooks when Emacs is your session:

```emacs-lisp
(add-hook 'elpaca-after-init-hook #'supervisor-start)
(add-hook 'kill-emacs-hook #'supervisor-stop-now)
```

`supervisor-stop-now` is synchronous and intended for `kill-emacs-hook`.

## Public Commands

| Command | Behavior |
|---|---|
| `M-x supervisor-mode` | Global mode. On enable: `supervisor-start`; on disable: `supervisor-stop` |
| `M-x supervisor-start` | Validate, parse, stage, and start all valid entries |
| `M-x supervisor-stop` | Async shutdown: `SIGTERM`, then `SIGKILL` after timeout |
| `M-x supervisor-stop-now` | Sync shutdown: immediate `SIGKILL`, brief wait, cleanup |
| `M-x supervisor-reload` | Reconcile running state with current config |
| `M-x supervisor` | Open dashboard buffer (`*supervisor*`) |
| `M-x supervisor-validate` | Validate config and show report without starting |
| `M-x supervisor-dry-run` | Show computed startup plan without starting |

## Configuration Model

Set `supervisor-programs` to a list of entries. Each entry is either:

- A command string: `"nm-applet"`
- A list: `("command" :keyword value ...)`

### Normalized Internal Shape

Every valid entry is normalized to:

```elisp
(id cmd delay enabled-p restart-p logging-p type stage after
    oneshot-wait oneshot-timeout tags)
```

### Entry Keywords

| Keyword | Type | Default | Meaning |
|---|---|---|---|
| `:id` | string | basename of executable | Stable entry identifier |
| `:type` | symbol | `simple` | `simple` or `oneshot` |
| `:stage` | symbol | `stage3` | `stage1`, `stage2`, `stage3`, or `stage4` |
| `:delay` | number | `0` | Seconds before spawn |
| `:after` | string or list of strings | `nil` | Dependencies inside same stage |
| `:enabled` | boolean | `t` | Whether entry is startable by supervisor |
| `:disabled` | boolean | `nil` | Inverse form of `:enabled` |
| `:restart` | boolean | `t` | Restart on crash (simple only) |
| `:no-restart` | boolean | `nil` | Inverse form of `:restart` |
| `:logging` | boolean | `t` | Log stdout/stderr |
| `:oneshot-wait` | boolean | `supervisor-oneshot-default-wait` | Whether oneshot blocks stage completion |
| `:async` | boolean | `nil` | Alias for `:oneshot-wait nil` |
| `:oneshot-timeout` | number or `nil` | `supervisor-oneshot-timeout` | Timeout for blocking oneshots |
| `:tags` | list/symbol/string | `nil` | Dashboard tag filtering labels |

### Validation Rules

Validation runs before startup and during dry-run/validate.

- Unknown keywords are rejected.
- `:type` must be a symbol and one of `simple`/`oneshot`.
- `:stage` must be a symbol and one of `stage1..stage4`.
- `:delay` must be a non-negative number.
- `:oneshot-timeout` must be a number or `nil`.
- Mutually exclusive pairs are rejected: `:enabled` + `:disabled`,
  `:restart` + `:no-restart`, `:oneshot-wait` + `:async`.
- Type restrictions are enforced:
- `oneshot` rejects `:restart` and `:no-restart`.
- `simple` rejects `:oneshot-wait`, `:async`, and `:oneshot-timeout`.

Invalid entries are skipped for startup and shown in the dashboard as
`status=invalid` with a reason.

Duplicate IDs are skipped after the first entry.

## Startup Semantics

`supervisor-start` is asynchronous and stage-based.

### Stage Execution

1. Parse/validate entries and partition by stage.
2. Run stages sequentially: `stage1`, `stage2`, `stage3`, `stage4`.
3. For each stage, validate `:after` references against same-stage IDs only.
4. Ignore cross-stage or missing dependencies (warning logged).
5. Apply stable topological sort (original list order is tie-break).
6. If a cycle is detected, clear all `:after` edges in that stage and continue
   in original list order.

### Ready Semantics

An entry is considered "ready" when:

- `simple`: spawned successfully.
- `oneshot`: exits (success or failure), or times out.
- disabled: immediately ready.
- spawn failure: immediately ready.

### Stage Completion Criteria

A stage completes only when all are true:

- Every stage entry has reached "started" state (including delayed entries that
  actually spawned or failed spawn).
- No pending delay timers remain.
- No blocking oneshots remain.

### Delay, Timeout, and Concurrency Controls

- `:delay N` schedules start with a timer.
- Blocking oneshots can be bounded by `:oneshot-timeout` or
  `supervisor-oneshot-timeout`.
- Optional `supervisor-stage-timeout` forces stage completion and marks
  unstarted entries as `stage-timeout`.
- Optional `supervisor-max-concurrent-starts` limits parallel spawn attempts
  within a stage.

## Process Lifecycle

### Start Behavior

- Commands are executed with `make-process` and
  `split-string-and-unquote`.
- No shell interpolation is performed automatically.
- If you need shell behavior (`$VAR`, redirection, pipes), run an explicit
  shell command (for example `sh -c '...'`).

### Logging

- Per-process logs are written to `supervisor-log-directory` as
  `log-<id>.log`.
- Session start rotates existing `log-*.log` to timestamped names.
- Dashboard `l` toggles logging override for next start.

### Restart and Crash Loop Handling (`simple` only)

- Restart happens only when effective restart policy is enabled.
- Restarts are delayed by `supervisor-restart-delay`.
- Crash tracking window: `supervisor-restart-window`.
- Threshold: when recent crashes reach `supervisor-max-restarts`, the process is
  marked failed (`dead`) and restart stops.

`oneshot` entries never restart.

### Runtime Overrides

Dashboard actions can override config values at runtime:

- enabled state override (`e`)
- restart policy override (`r`)
- logging override (`l`)

Overrides affect subsequent starts/restarts.

## Shutdown and Reload

### `supervisor-stop` (async graceful)

- Sets shutdown mode.
- Cancels pending start/restart timers.
- Runs `supervisor-cleanup-hook`.
- Sends `SIGTERM` to live supervised processes.
- Completes via sentinels.
- After `supervisor-shutdown-timeout`, sends `SIGKILL` to survivors.
- Optional callback is called when shutdown completes.

### `supervisor-stop-now` (sync hard stop)

- Intended for `kill-emacs-hook`.
- Sets shutdown mode.
- Cancels pending timers.
- Runs `supervisor-cleanup-hook`.
- Sends immediate `SIGKILL` to all live supervised processes.
- Waits briefly (up to ~0.5s) for processes to exit.
- Clears process table and marks shutdown complete.

### `supervisor-reload`

Reload does a reconcile pass:

- Stops running entries that were removed from config.
- Stops running entries that are now disabled.
- Starts entries present in config but not running and not in failed/completed
  oneshot state.
- Does not automatically restart changed command lines for already running
  entries.

## Dashboard (`M-x supervisor`)

`supervisor` opens `*supervisor*` using `tabulated-list-mode`.

### Columns

- `ID`
- `Type`
- `Stage`
- `Enabled`
- `Status`
- `Restart`
- `Log`
- `PID`
- `Reason`

### Status Values

- `running`
- `stopped`
- `pending`
- `done`
- `failed`
- `dead`
- `invalid`

### Reason Values (when applicable)

- `disabled`
- `delayed`
- `waiting-on-deps`
- `stage-not-started`
- `failed-to-spawn`
- `stage-timeout`
- `crash-loop`

### Keybindings

| Key | Action |
|---|---|
| `e` | Toggle enabled override |
| `f` | Cycle stage filter |
| `t` | Cycle tag filter |
| `r` | Toggle restart override |
| `k` | Kill process at point and disable restart |
| `s` | Start process at point |
| `l` | Toggle logging override |
| `L` | Open process log file |
| `p` | Open `proced` |
| `P` | Toggle `proced` auto-update |
| `?` | Show resolved entry details |
| `d` | Show dependencies/dependents for entry |
| `D` | Show full dependency graph |
| `B` | Show startup timing blame view |
| `g` | Refresh |
| `q` | Quit dashboard window |

Notes:

- Dependency graph/blame views require startup data (`supervisor-start` run).
- Invalid entries are visible when no stage filter is active.

## Config Watch

`supervisor-watch-config` controls file-watch reload behavior:

- `nil`: disabled
- `t`: watch `user-init-file`
- string path: watch that file

When a watched file changes, supervisor debounces for 1 second and runs
`supervisor-reload`.

`supervisor-mode` starts/stops the watcher automatically.

## Hooks

| Hook | Arguments | When |
|---|---|---|
| `supervisor-cleanup-hook` | none | Before shutdown kills/signals processes |
| `supervisor-stage-start-hook` | `stage-name` | At stage start |
| `supervisor-stage-complete-hook` | `stage-name` | At stage completion |
| `supervisor-process-exit-hook` | `id status code` | On supervised process exit |

`supervisor-process-exit-hook` status values:

- `exited`
- `signal`
- `unknown`

## Customization Variables

| Variable | Default |
|---|---|
| `supervisor-programs` | `nil` |
| `supervisor-log-directory` | `~/.emacs.d/supervisor/` |
| `supervisor-restart-delay` | `2` |
| `supervisor-max-restarts` | `3` |
| `supervisor-restart-window` | `60` |
| `supervisor-shutdown-timeout` | `3` |
| `supervisor-oneshot-default-wait` | `t` |
| `supervisor-oneshot-timeout` | `30` |
| `supervisor-stage-timeout` | `nil` |
| `supervisor-max-concurrent-starts` | `nil` |
| `supervisor-verbose` | `nil` |
| `supervisor-log-to-file` | `nil` |
| `supervisor-watch-config` | `nil` |

## Development Commands

```bash
make check
make lint
make test
make test-one TEST=supervisor-test-parse-string-entry
```

## License

GPL-3.0-or-later. See `LICENSE`.
