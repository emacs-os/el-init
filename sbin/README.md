# CLI Wrapper Plan (POSIX shell)

This folder will hold small POSIX shell wrappers that control `supervisor.el`
via `emacsclient`. The goal is a *systemctl-like* UX that covers nearly all
supervisor capabilities so the interactive dashboard (`M-x supervisor`) remains
optional.

## Principles

- Default output is human-readable.
- `--json` returns machine-friendly output.
- Keep transport simple: `emacsclient --eval`.
- Fail fast with clear errors if the Emacs server is unavailable.
- No extra dependencies beyond POSIX shell and `emacsclient`.

## Primary Command

- `sbin/supervisorctl` (POSIX sh)

## Command Set (systemctl-style)

- `supervisorctl start [ID...]`
  - Start all configured programs (no args) or specific IDs.
- `supervisorctl stop [ID...]`
  - Stop all supervised processes or specific IDs.
- `supervisorctl restart [ID...]`
  - Restart all or specific IDs.
- `supervisorctl enable [ID...]`
  - Enable entries (config/runtime override). If none, enable all.
- `supervisorctl disable [ID...]`
  - Disable entries. If none, disable all.
- `supervisorctl status [ID...]`
  - Human-readable status by default.
  - `--json` returns structured status.
- `supervisorctl list`
  - List all entries with status in a compact table.
- `supervisorctl describe ID`
  - Show resolved config for a single entry (human or JSON).
- `supervisorctl validate`
  - Run validation only (no start). Always prints invalid reasons.
- `supervisorctl reload`
  - If implemented, reconcile config without restart.
- `supervisorctl graph [ID]`
  - Print dependency edges or subgraph for ID.
- `supervisorctl blame`
  - Show startup timing / blame list (longest first).
- `supervisorctl logs ID [--tail N]`
  - Print recent log lines for an entry.

## Options

- `--json`            Output JSON instead of human text
- `--socket NAME`     Use a specific Emacs server socket
- `--server-file PATH` Use a server file path
- `--verbose`         Verbose CLI output (not the same as supervisor-verbose)
- `--quiet`           Minimal output (errors only)

## Output (Human-Readable Defaults)

`status` / `list` example:

```
ID            TYPE     STAGE    ENABLED  STATUS   RESTART  LOG   PID   REASON
nm-applet     simple   session  yes      running  yes      yes   1234  -
blueman       simple   session  yes      stopped  yes      yes   -     -
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

## JSON Output (Proposal)

All JSON should be a single object with a stable schema.

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
      "reason": null
    }
  ],
  "invalid": [
    {"id": "malformed#0", "reason": ":type must be a symbol"}
  ]
}
```

## Emacs Functions Required (to back the CLI)

These should return structured data instead of only `message`:

- `supervisor-status` -> list/alist for all entries
- `supervisor-describe-entry` -> alist for one entry
- `supervisor-set-enabled` -> enable/disable by id(s)
- `supervisor-restart` -> restart by id(s)
- `supervisor-graph` -> dependency edges
- `supervisor-blame` -> startup timing info
- `supervisor-logs` -> recent log lines

## Transport

Each CLI command invokes Emacs via:

```
emacsclient --eval "(supervisor-status)"
```

If needed, pass `-s NAME` or `--server-file PATH`.

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

Emacs clients can do more than open files. The `emacsclient --eval` option can
evaluate arbitrary Lisp on the server. In practice, anyone who can connect to
the server can run code with the privileges of the Emacs process. Design the
CLI and deployment accordingly. citeturn3search0

Key points:

- Prefer Unix domain sockets (default). File system permissions on the socket
  constrain who can connect.
- If TCP is used (`server-use-tcp`), file permissions no longer apply. Access
  control then depends on the authorization key and the permissions on
  `server-auth-dir` (defaults to `~/.emacs.d/server/`). Keep that directory
  private. citeturn1view0
- TCP defaults to a random port on localhost. Avoid binding to non-local
  interfaces unless you explicitly want remote access, and secure it. citeturn1view0
- Avoid setting a static `server-auth-key` unless you have a specific need.
  The default random key is the recommended mode. citeturn1view0

Practical guidance:

- Treat the server socket and server file as sensitive. If another user can
  read the server file or connect to the socket, they effectively control
  your Emacs session.
- If you plan to use this as a user-session supervisor (or in place of a user
  init system), consider running Emacs under a dedicated user account and keep
  the server socket private to that account.
