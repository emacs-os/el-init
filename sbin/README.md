# CLI Control Plane Plan (Minimal Wrapper + Pure Elisp)

This folder will hold a single minimal CLI shim that controls `supervisor.el`
via `emacsclient`. All command parsing, dispatch, output formatting, and
policy live in Elisp. The goal is a systemctl-like UX that covers all
supervisor features so the interactive dashboard (`M-x system`, with
`M-x supervisor` compatibility during migration) remains optional.

Status: design plan only, not yet implemented.

## Principles

- Human-readable output by default.
- `--json` returns machine-friendly output.
- Prefer a single minimal shim (`system`), not multiple wrappers.
- Keep all logic in Elisp; the shim only transports argv and prints output.
- Fail fast with clear errors if the Emacs server is unavailable.
- The CLI must expose the same operations as the dashboard.

## Primary Command

- `sbin/system` (tiny shim; no per-command scripts)
- `sbin/supervisorctl` compatibility alias during migration

If shell scripts are to be avoided entirely, this shim can be a tiny compiled
launcher instead (C/Rust/Go), but it must remain a thin transport layer only.

## Command Surface (systemctl-style)

Lifecycle:

- `system start [ID...]`
- `system stop [ID...]`
- `system restart [ID...]`
- `system reload`
- `system validate`

State and inspect:

- `system status [ID...]`
- `system list`
- `system describe ID`
- `system graph [ID]`
- `system blame`
- `system logs ID [--tail N]`
- `system stage`
- `system wait [--stage STAGE|--all] [--timeout N]`

Runtime overrides (dashboard parity):

- `system enable [ID...]`
- `system disable [ID...]`
- `system restart-policy (on|off) [ID...]`
- `system logging (on|off) [ID...]`

Low-level control:

- `system kill ID [--signal SIG]`
- `system ping`
- `system version`

Note: some commands map to P1 items (reload, wait, runtime enable/disable) and
will be no-ops until implemented.

## Options

- `--json` Output JSON instead of human text
- `--socket-name NAME` Use a specific Emacs server socket (emacsclient `-s`)
- `--server-file PATH` Use a server file path for TCP (emacsclient `-f`)
- `--timeout N` Wait at most N seconds for Emacs response (emacsclient `-w`)
- `--verbose` Verbose CLI output (not the same as `supervisor-verbose`)
- `--quiet` Minimal output (errors only)
- `--no-color` Disable ANSI color (if used)

## Output (Human-Readable Defaults)

`status` / `list` example:

```
ID            TYPE     STAGE    ENABLED  STATUS   RESTART  LOG   PID   REASON
nm-applet     simple   session  yes      running  yes      yes   1234  -
blueman       simple   session  yes      stopped  yes      yes   -     waiting on deps
oneshot-xrdb  oneshot  early    yes      done     n/a      yes   -     -
invalid#0     -        -        -        invalid  -        -     -     :type must be a symbol
```

`describe ID` example:

```
ID: nm-applet
Type: simple
Stage: session
Enabled: yes
Restart: yes (override: no)
Logging: yes
Delay: 0
After: none
```

`validate` example:

```
Validation: 5 valid, 1 invalid
INVALID invalid#0: :type must be a symbol
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

## JSON Output (Proposal)

All JSON should be a single object with a stable schema and sorted keys.

Example `status --json`:

```
{
  "entries": [
    {
      "id": "nm-applet",
      "type": "simple",
      "stage": "session",
      "enabled": true,
      "status": "running",
      "restart": true,
      "logging": true,
      "pid": 1234,
      "reason": null,
      "after": ["dbus", "gpg-agent"],
      "delay": 0,
      "start_time": 1692133234.12,
      "ready_time": 1692133235.01,
      "duration": 0.89
    }
  ],
  "invalid": [
    {"id": "malformed#0", "reason": ":type must be a symbol"}
  ],
  "stage": {
    "current": "session",
    "completed": ["early", "services"]
  }
}
```

Example `graph --json`:

```
{
  "id": "nm-applet",
  "depends_on": ["dbus", "gpg-agent"],
  "blocks": ["tray"],
  "edges": [
    ["dbus", "nm-applet"],
    ["gpg-agent", "nm-applet"],
    ["nm-applet", "tray"]
  ]
}
```

## Emacs Functions Required (to back the CLI)

These should return structured data instead of only `message` output.
Prefer a single dispatcher plus small helpers.

- `system-cli-status` -> list/alist for all entries
- `system-cli-describe` -> alist for one entry
- `system-cli-graph` -> dependency edges
- `system-cli-blame` -> startup timing info
- `system-cli-logs` -> recent log lines
- `system-cli-set-enabled` -> enable/disable by id(s)
- `system-cli-set-restart` -> runtime restart override by id(s)
- `system-cli-set-logging` -> runtime logging override by id(s)
- `system-cli-signal` -> send signal to a process
- `system-cli-validate` -> validation summary and invalid list
- `system-cli-reload` -> reconcile config without restart

## Transport

The shim encodes argv and invokes a single Elisp dispatcher:

```
emacsclient --eval "(system--dispatch \"<encoded-argv>\")"
```

The dispatcher returns a structured payload containing:
- `exit` (integer)
- `format` (`human` or `json`)
- `output` (string)

The shim prints `output` and exits with `exit`.
If needed, pass `-s NAME` or `--server-file PATH` for server selection.

## Exit Codes

- `0` success
- `1` generic failure
- `2` invalid args
- `3` Emacs server unavailable
- `4` validation failed

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
