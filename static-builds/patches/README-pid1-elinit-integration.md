# PID1 Emacs Patch: Elinit Integration Guide

## Overview

The PID1 Emacs patches add three Lisp hooks that elinit uses to
orchestrate PID1 lifecycle behavior. The Emacs C code handles low-level
concerns (signal handling, child reaping, option parsing), while elinit
controls service-manager lifecycle.

As of 2026-02-20, built-in rc script autoloading is removed from
`elinit-pid1.el`. Early boot/shutdown scripts are admin-owned and should be
invoked through explicit oneshot units (or explicit init-file Lisp), not by
implicit file-path probing in PID1 hooks.

## Hook API

### `pid1-boot-hook`

- **When**: Runs once after startup, after `emacs-startup-hook` and all init
  files have been processed.
- **Where**: In `normal-top-level` unwind-protect cleanup (`lisp/startup.el`).
- **Purpose**: Start managed services by invoking `elinit-start`.
- **Batch mode**: Does not fire in `--batch` mode. Fires in interactive and
  daemon modes.

### `pid1-poweroff-hook`

- **When**: Inside `kill-emacs`, before `kill-emacs-hook`. Fires when a
  poweroff signal was received (SIGTERM or SIGUSR1).
- **Where**: In `Fkill_emacs()` (`src/emacs.c`).
- **Purpose**: Stop managed services by invoking `elinit-stop-now`.
- **Guard**: Only fires once per process (static reentrancy guard).

### `pid1-reboot-hook`

- **When**: Inside `kill-emacs`, before `kill-emacs-hook`. Fires when a
  reboot signal was received (SIGINT or SIGUSR2).
- **Where**: In `Fkill_emacs()` (`src/emacs.c`).
- **Purpose**: Stop managed services by invoking `elinit-stop-now`.
- **Guard**: Only fires once per process (static reentrancy guard).

### Execution order on shutdown

```
signal received (e.g. SIGTERM)
  -> signal handler sets pid1_poweroff_pending flag
  -> main loop detects quit flag
  -> Fkill_emacs() called
    -> pid1-poweroff-hook (or pid1-reboot-hook)   <- elinit cleanup
    -> kill-emacs-hook                             <- standard Emacs cleanup
    -> exit()
```

## Lisp state exposure

| Variable | Type | Description |
|----------|------|-------------|
| `pid1-mode` | boolean | `t` when `--pid1` was passed, `nil` otherwise |
| `pid1-boot-hook` | hook | Runs after startup in PID1 mode |
| `pid1-poweroff-hook` | hook | Runs on SIGTERM/SIGUSR1 in PID1 mode |
| `pid1-reboot-hook` | hook | Runs on SIGINT/SIGUSR2 in PID1 mode |

## Elinit-side integration

Elinit adds PID1 integration through `elinit-pid1.el`.

### Variables

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `elinit-pid1-mode-enabled` | boolean | auto-detect | Gates `elinit-start`/`elinit-stop-now` calls on PID1 hooks |

### Mapping hooks to elinit behavior

```elisp
;; Elinit registers on pid1-boot-hook:
(add-hook 'pid1-boot-hook #'elinit--pid1-boot)

(defun elinit--pid1-boot ()
  "Initialize elinit PID1 mode."
  (when elinit-pid1-mode-enabled
    (elinit-start)))

;; Elinit registers on pid1-poweroff-hook and pid1-reboot-hook:
(add-hook 'pid1-poweroff-hook #'elinit--pid1-shutdown)
(add-hook 'pid1-reboot-hook   #'elinit--pid1-shutdown)

(defun elinit--pid1-shutdown ()
  "Clean shutdown of all managed services."
  (when elinit-pid1-mode-enabled
    (elinit-stop-now)))
```

### How `elinit-pid1-mode-enabled` is auto-detected

```elisp
(defun elinit--pid1-detect-mode ()
  "Return non-nil when running as PID 1 with pid1-mode enabled."
  (and (bound-and-true-p pid1-mode)
       (= (emacs-pid) 1)))
```

The double-check (`pid1-mode` and PID 1) allows testing with `--pid1` on
non-PID1 processes while ensuring real PID1 behavior only activates when
actually running as the init process.

### Gating architecture

PID1 behavior has three independent gates at different levels:

| Gate | What it controls | Where |
|------|------------------|-------|
| `pid1-mode` (C-level) | Hook registration at module load time | `elinit-pid1.el` load-time `when` block |
| `elinit-pid1-mode-enabled` (Lisp) | `elinit-start` and `elinit-stop-now` hook actions | `elinit--pid1-boot`, `elinit--pid1-shutdown` |
| C signal handlers (C-level) | Child reaping (SIGCHLD), shutdown signals (SIGTERM, etc.) | Emacs C code |

Setting `elinit-pid1-mode-enabled` to nil disables Lisp-side service lifecycle
actions but does not affect C-level reaping or signal handling, which are
controlled by the `--pid1` flag in Emacs C code.

## Integration flow

```
Container start
  -> Emacs --pid1 starts as PID 1
  -> Emacs parses --pid1, sets pid1_mode=true
  -> Signal handlers installed (SIGTERM->poweroff, SIGINT->reboot)
  -> Child reaping enabled (waitpid(-1) in SIGCHLD handler)
  -> Init files loaded (site-start.el, user init.el)
  -> emacs-startup-hook fires
  -> pid1-boot-hook fires
    -> elinit--pid1-boot runs
      -> starts managed services
  -> Emacs enters command loop (stays running as PID 1)

... normal operation, elinit managing services ...

Container shutdown (docker stop / SIGTERM)
  -> SIGTERM received
  -> signal handler sets pid1_poweroff_pending, quit flag
  -> main loop processes quit -> calls kill-emacs
    -> pid1-poweroff-hook fires
      -> elinit--pid1-shutdown runs
        -> stops all services cleanly
    -> kill-emacs-hook fires (standard cleanup)
    -> process exits
```

## Early boot scripts (admin-owned)

If initramfs does not provide early setup, use explicit early oneshot units.
Do not rely on implicit rc script file loading.

For pure Elisp workflows, repurpose:

- `static-builds/scripts/rc.boot.el.example`
- `static-builds/scripts/rc.shutdown.el.example`

and invoke your script through a deterministic oneshot command.

## No additional C changes needed

The Emacs patch provides all C-level primitives elinit needs. Elinit PID1
lifecycle now remains intentionally minimal: start on boot hook, stop on
shutdown hooks.
