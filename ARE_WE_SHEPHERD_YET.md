# Are We Shepherd Yet?

Short answer: not quite, and that’s okay.

`supervisor.el` is a user-session process supervisor. GNU Shepherd is a
full-featured init/service manager with richer semantics, isolation, and
long-lived guarantees. But it’s worth asking the question because we’re
closing in on a fun subset: a transparent, interactive, Emacs-native
supervisor with sane defaults and deterministic behavior.

## What We *Are*

- A user-session supervisor built for Emacs-driven desktops and workflows.
- Staged startup with dependency ordering and a DAG scheduler.
- Crash recovery, bounded restart behavior, and observability.
- Fully scriptable from the CLI via `emacsclient` (planned in `sbin/`).
- Simple config with explicit validation and human-readable errors.

## What We’re *Not* (Yet)

- A PID1 replacement or system-wide init.
- A security boundary or isolation mechanism.
- A replacement for Shepherd on multi-user systems.

## The Honest Comparison

If you want:

- **System boot orchestration** → Shepherd (or systemd).
- **User session orchestration with transparency** → supervisor.el.

## What Would Move the Needle?

If we want to get closer to “Shepherd for user sessions,” we’d need:

- A stable, non-interactive CLI (systemctl-style) for all functions.
- Structured status output and `--json` for automation.
- Stronger lifecycle controls (enable/disable overrides, reload, graph/blame).
- Clear security guidance around `emacsclient` and server access.
- A formal, versioned interface contract for scripts.

## Verdict

**Are we Shepherd yet?**

Not yet. But we’re close enough to be *useful*, and the gap is shrinking fast.
