# sbin — CLI and Maintenance Scripts

This directory holds the CLI control shim (`supervisorctl`) and log
maintenance scripts (`supervisor-logrotate`, `supervisor-log-prune`).
The `libexec/` directory holds the per-service log writer binary
(`supervisor-logd`).

# supervisorctl — CLI Control Plane (Minimal Wrapper + Pure Elisp)

This folder will hold a single minimal CLI shim that controls `supervisor.el`
via `emacsclient`. All command parsing, dispatch, output formatting, and
policy live in Elisp. The goal is a systemctl-like UX that covers all
supervisor features so the interactive dashboard (`M-x supervisor`) remains
optional.

Status: implemented. See `supervisorctl` wrapper in this directory.

## Principles

- Human-readable output by default.
- `--json` returns machine-friendly output.
- Prefer a single minimal shim (`supervisorctl`), not multiple wrappers.
- Keep all logic in Elisp; the shim only transports argv and prints output.
- Fail fast with clear errors if the Emacs server is unavailable.
- The CLI must expose the same operations as the dashboard.

## Primary Command

- `sbin/supervisorctl` (tiny shim; no per-command scripts)

If shell scripts are to be avoided entirely, this shim can be a tiny compiled
launcher instead (C/Rust/Go), but it must remain a thin transport layer only.

## Command Surface (systemctl-style)

Lifecycle:

- `supervisorctl start [ID...]`
- `supervisorctl stop [ID...]`
- `supervisorctl restart [ID...]`
- `supervisorctl verify`
- `supervisorctl reset-failed [ID...]`

State and inspect:

- `supervisorctl status [ID...]`
- `supervisorctl list`
- `supervisorctl describe ID`
- `supervisorctl graph [ID]`
- `supervisorctl blame`
- `supervisorctl logs ID [--tail N]`

Runtime overrides (dashboard parity):

- `supervisorctl enable [ID...]`
- `supervisorctl disable [ID...]`
- `supervisorctl restart-policy (no|on-success|on-failure|always) [ID...]`
- `supervisorctl logging (on|off) [ID...]`

Low-level control:

- `supervisorctl kill ID [--signal SIG]`
- `supervisorctl ping`
- `supervisorctl version`

## Options

- `--json` Output JSON instead of human text
- `--socket-name NAME` Use a specific Emacs server socket (emacsclient `-s`)
- `--server-file PATH` Use a server file path for TCP (emacsclient `-f`)
- `--timeout N` Wait at most N seconds for Emacs response (emacsclient `-w`)

## Output (Human-Readable Defaults)

`status` / `list` example:

```
ID            TYPE     STAGE    ENABLED  STATUS   RESTART     LOG   PID   REASON
nm-applet     simple   session  yes      running  always      yes   1234  -
blueman       simple   session  yes      stopped  on-failure  yes   -     waiting on deps
oneshot-xrdb  oneshot  early    yes      done     n/a         yes   -     -
invalid#0     -        -        -        invalid  -           -     -     :type must be a symbol
```

`describe ID` example:

```
ID: nm-applet
Type: simple
Stage: session
Enabled: yes
Restart: always (override)
Logging: yes
Delay: 0
After: none
```

`verify` example:

```
Services: 5 valid, 1 invalid
  INVALID invalid#0: :type must be a symbol
Timers: 2 valid, 0 invalid
```

`graph ID` example:

```
ID: nm-applet
Depends-on: dbus, gpg-agent
Blocks: tray
```

`blame` example:

```
ID             START            READY            DURATION
nm-applet      1692133234.12    1692133235.01    0.89s
blueman        1692133234.20    1692133236.40    2.20s
```

## JSON Output

All JSON is a single object with a stable schema. Key order is fixed by construction.

Example `status --json`:

```
{
  "entries": [
    {
      "id": "nm-applet",
      "type": "simple",
      "stage": "stage3",
      "enabled": true,
      "status": "running",
      "restart": "always",
      "logging": true,
      "pid": 1234,
      "reason": null,
      "delay": 0,
      "after": ["dbus", "gpg-agent"],
      "requires": [],
      "start_time": 1692133234.12,
      "ready_time": 1692133235.01,
      "duration": 0.89
    }
  ],
  "invalid": [
    {"id": "malformed#0", "reason": ":type must be a symbol"}
  ]
}
```

Example `graph ID --json` (single entry):

```
{
  "id": "nm-applet",
  "after": ["dbus", "gpg-agent"],
  "requires": [],
  "blocks": ["tray"]
}
```

Example `graph --json` (full graph):

```
{
  "edges": [
    ["dbus", "nm-applet"],
    ["gpg-agent", "nm-applet"],
    ["nm-applet", "tray"]
  ]
}
```

## Emacs Functions (Implementation)

The CLI is backed by a single dispatcher with private command handlers:

- `supervisor--cli-dispatch` - parses argv and routes to handlers
- `supervisor--cli-dispatch-for-wrapper` - wrapper entry point, returns simple format

Command handlers (all private, `supervisor--cli-cmd-*`):

- `supervisor--cli-cmd-status` - status/list command
- `supervisor--cli-cmd-describe` - describe command
- `supervisor--cli-cmd-start` - start command
- `supervisor--cli-cmd-stop` - stop command
- `supervisor--cli-cmd-restart` - restart command
- `supervisor--cli-cmd-verify` - verify command
- `supervisor--cli-cmd-reset-failed` - reset-failed command
- `supervisor--cli-cmd-enable` - enable command
- `supervisor--cli-cmd-disable` - disable command
- `supervisor--cli-cmd-restart-policy` - restart-policy command
- `supervisor--cli-cmd-logging` - logging command
- `supervisor--cli-cmd-blame` - blame command
- `supervisor--cli-cmd-graph` - graph command
- `supervisor--cli-cmd-logs` - logs command
- `supervisor--cli-cmd-kill` - kill command
- `supervisor--cli-cmd-ping` - ping command
- `supervisor--cli-cmd-version` - version command

Result structure: `supervisor-cli-result` struct with exitcode, format, and output.

## Transport

The shim encodes argv as a Lisp list and invokes `supervisor--cli-dispatch-for-wrapper`:

```
emacsclient --eval "(supervisor--cli-dispatch-for-wrapper (list \"cmd\" \"arg1\" ...))"
```

The dispatcher returns a string in the format:

```
EXITCODE:BASE64OUTPUT
```

Where `EXITCODE` is a numeric exit code and `BASE64OUTPUT` is base64-encoded output.
Base64 encoding avoids escaping issues with newlines and special characters in JSON.

The shim:
1. Strips the outer quotes from `emacsclient --eval` output
2. Extracts the exit code (before the colon)
3. Decodes the base64 output (after the colon)
4. Prints decoded output and exits with the extracted code

If needed, pass `-s NAME` or `--server-file PATH` for server selection.

## Exit Codes

- `0` success
- `1` generic failure
- `2` invalid args
- `3` Emacs server unavailable
- `4` validation failed

# supervisor-logrotate — Log Rotation

Rotates active supervisor.el log files by renaming them with a
timestamp suffix, optionally signals logd writers to reopen their files,
and prunes old rotated files beyond a configurable age.

Designed to be called periodically by the supervisor timer subsystem or
an external scheduler (cron, systemd timer).

## Usage

```
supervisor-logrotate [OPTIONS]
```

## Options

| Option | Description |
|--------|-------------|
| `--log-dir DIR` | Log directory to operate on (required) |
| `--keep-days N` | Prune rotated files older than N days (default: 14) |
| `--signal-reopen` | Send SIGHUP to logd writers after rotation |
| `--pid-dir DIR` | Directory for writer PID files (default: same as `--log-dir`) |
| `--dry-run` | Print actions without executing |
| `--help` | Show help message |

## Behavior

1. Rotates `supervisor.log` and all active `log-<id>.log` files by
   renaming them to `<base>.YYYYMMDD-HHMMSS.log`.  Already-rotated
   files (identified by timestamp suffix) are skipped.
2. If `--signal-reopen` is given, sends SIGHUP to each logd writer
   whose PID file is found in `--pid-dir`, causing them to close and
   reopen their log files.
3. Prunes rotated files older than `--keep-days` days.  Uses
   `is_rotated()` pattern matching to distinguish rotated files from
   active logs whose service ID happens to contain dots.

## Exit Codes

- `0` — success
- `1` — usage or runtime error

---

# supervisor-log-prune — Directory Size Cap Enforcement

Enforces a hard cap on total log directory size by deleting the oldest
rotated log files first.  Active log files are never deleted.

Uses an exclusive lock file to prevent concurrent prune races.  If the
lock cannot be acquired, the script exits silently (exit 0) — another
instance is already running.

Designed to be invoked by `supervisor-logd` after local file rotation,
or directly by the supervisor timer subsystem.

## Usage

```
supervisor-log-prune [OPTIONS]
```

## Options

| Option | Description |
|--------|-------------|
| `--log-dir DIR` | Log directory to operate on (required) |
| `--max-total-bytes N` | Hard cap on total directory size in bytes (default: 1073741824 / 1 GiB) |
| `--lock-file PATH` | Exclusive lock file path (default: `<log-dir>/.prune.lock`) |
| `--protect-id ID` | Never delete `log-<ID>.log` (repeatable) |
| `--dry-run` | Print actions without executing |
| `--help` | Show help message |

## Safety Model

The script uses four confirmation layers before deleting any file:

1. **Parent-exists**: the rotated file's parent active log
   (`log-<id>.log`) exists in the directory.
2. **Sibling**: the rotated file shares a parent with at least one
   other rotated file (confirms a now-removed service once existed).
3. **Children**: lone orphan files matching the timestamp pattern are
   preserved when neither parent nor sibling confirmation succeeds
   (the file may be an active log for a service whose ID contains a
   timestamp pattern).
4. **Fuser**: files currently held open by a process (e.g., an active
   logd writer) are skipped.  Soft dependency — falls back to the
   other guards when `fuser` is not available.

## Exit Codes

- `0` — success (or nothing to do, or lock already held)
- `1` — usage or runtime error

---

# libexec/supervisor-logd — Per-Service Log Writer

A small C binary that reads service output from stdin and writes to a
log file with append semantics.  Compiled from `libexec/supervisor-logd.c`.

## Compilation

```
cd libexec && make
```

## Usage

```
supervisor-logd --file PATH --max-file-size-bytes N [OPTIONS]
```

## Options

| Option | Description |
|--------|-------------|
| `--file PATH` | Target log file (required) |
| `--max-file-size-bytes N` | Rotate when file exceeds N bytes (required) |
| `--log-dir DIR` | Directory for prune trigger context |
| `--prune-cmd CMD` | Command to run after local rotation |
| `--prune-min-interval-sec N` | Minimum seconds between prune calls |

## Signal Behavior

- **SIGHUP** — close and reopen the target file (for log rotation)
- **SIGTERM / SIGINT** — flush and exit cleanly

## Local Rotation

When the file exceeds the size cap, it is renamed to
`<base>.YYYYMMDD-HHMMSS.log` and a fresh file is opened.  If
`--prune-cmd` is set, the prune command is spawned after rotation
(throttled by `--prune-min-interval-sec`).

## Exit Codes

- `0` — clean exit (stdin EOF or TERM/INT)
- `1` — usage error
- `2` — file operation error

---

## Notes

- These wrappers are a convenience layer, not a daemon.
- CLI should be usable for full lifecycle management without `M-x supervisor`.
- Keep output stable for scripting, especially under `--json`.

## Security Considerations

`emacsclient --eval` can evaluate Lisp on the server. Any client that can
connect effectively controls the Emacs process. Design the CLI and deployment
accordingly.

Key points:

- Emacs normally listens on a local Unix domain socket. TCP is optional and
  must be explicitly enabled.
- TCP sockets are not protected by file permissions, so access control depends
  on the auth key and the permissions of the server auth directory and server
  file.
- `server-auth-key` should remain random and private; avoid setting a static
  key unless you have a specific need.
- Treat the server socket and server file as sensitive; restrict filesystem
  permissions to the owner.

Practical guidance:

- Prefer local sockets for user-session supervisors.
- If you need TCP, bind to localhost only and keep `server-auth-dir` private.
- Consider running Emacs under a dedicated user account for system-level use.
- Use explicit `--socket-name` or `--server-file` instead of trusting ambient
  environment variables in scripts.
