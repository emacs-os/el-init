# Patches

Emacs source patches for PID1 init process support. Apply to Emacs 30.2.

## Patch files

| File | Description |
|------|-------------|
| `emacs-0001-add-pid1-runtime-mode.patch` | `--pid1` CLI option and `pid1-mode` Lisp variable |
| `emacs-0002-pid1-hooks-and-signals.patch` | Signal handlers, child reaping, boot/poweroff/reboot hooks |
| `emacs-0003-fix-pid1-signal-handler-overrides.patch` | Fix signal handlers overridden by init_keyboard/init_signals |

## Apply

```sh
cd /path/to/emacs-30.2
git apply path/to/emacs-0001-add-pid1-runtime-mode.patch
git apply path/to/emacs-0002-pid1-hooks-and-signals.patch
git apply path/to/emacs-0003-fix-pid1-signal-handler-overrides.patch
```

## Documentation

- [README-pid1-validation.md](README-pid1-validation.md) - Build commands, test results, signal mapping
- [README-pid1-elinit-integration.md](README-pid1-elinit-integration.md) - Hook API, elinit integration
