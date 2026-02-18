# Patches

Optional Emacs patches used by some of the static-build/ files for the tinkerers among us crazy enough to run Emacs as pid1.

## TODO

- **Emacs `--pid1` flag**: Add an optional `--pid1` command-line flag to Emacs
  that enables PID 1 behavior (zombie reaping via `waitpid(-1, ..., WNOHANG)`
  in the main loop and basic signal forwarding), similar to
  [sinit](https://git.suckless.org/sinit/). This allows Emacs running as
  container init to supervise processes without an external init shim.
