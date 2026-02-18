# PID1 Emacs Patch: Supervisor Integration Guide

## Overview

The PID1 Emacs patches add three Lisp hooks that supervisor.el uses to
orchestrate PID1 init behavior. The Emacs C code handles the low-level
concerns (signal handling, child reaping, option parsing), while all
policy decisions live in Lisp (supervisor.el).

## Hook API

### `pid1-boot-hook`

- **When**: Runs once after startup, after `emacs-startup-hook` and all init
  files have been processed.
- **Where**: In `normal-top-level` unwind-protect cleanup (lisp/startup.el).
- **Purpose**: Initialize the init system — load `rc.boot.el`, start services.
- **Batch mode**: Does NOT fire in `--batch` mode (PID1 is a long-running
  process, not batch). Fires in interactive and daemon modes.

### `pid1-poweroff-hook`

- **When**: Inside `kill-emacs`, BEFORE `kill-emacs-hook`. Fires when a
  poweroff signal was received (SIGTERM or SIGUSR1).
- **Where**: In `Fkill_emacs()` (src/emacs.c).
- **Purpose**: Run `rc.shutdown.el` for clean poweroff, stop all services.
- **Guard**: Only fires once per process (static reentrancy guard).

### `pid1-reboot-hook`

- **When**: Inside `kill-emacs`, BEFORE `kill-emacs-hook`. Fires when a
  reboot signal was received (SIGINT or SIGUSR2).
- **Where**: In `Fkill_emacs()` (src/emacs.c).
- **Purpose**: Run `rc.shutdown.el` for clean reboot, stop all services.
- **Guard**: Only fires once per process (static reentrancy guard).

### Execution order on shutdown

```
signal received (e.g. SIGTERM)
  → signal handler sets pid1_poweroff_pending flag
  → main loop detects quit flag
  → Fkill_emacs() called
    → pid1-poweroff-hook (or pid1-reboot-hook)   ← supervisor cleanup
    → kill-emacs-hook                             ← standard Emacs cleanup
    → exit()
```

## Lisp state exposure

| Variable | Type | Description |
|----------|------|-------------|
| `pid1-mode` | boolean | `t` when `--pid1` was passed, `nil` otherwise |
| `pid1-boot-hook` | hook | Runs after startup in PID1 mode |
| `pid1-poweroff-hook` | hook | Runs on SIGTERM/SIGUSR1 in PID1 mode |
| `pid1-reboot-hook` | hook | Runs on SIGINT/SIGUSR2 in PID1 mode |

## Supervisor-side integration

Supervisor.el adds its PID1 integration via these hooks. The supervisor-side
Lisp variables control behavior:

### Variables (implemented in supervisor.el, not in the patch)

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `supervisor-pid1-mode-enabled` | boolean | auto-detect | Gates all PID1 behavior |
| `supervisor-pid1-boot-script` | string | `/lib/init/rc.boot.el` | Boot script path |
| `supervisor-pid1-shutdown-script` | string | `/lib/init/rc.shutdown.el` | Shutdown script path |
| `supervisor-pid1-boot-policy` | symbol | `if-present` | Boot script policy |
| `supervisor-pid1-shutdown-policy` | symbol | `if-present` | Shutdown script policy |

### Policies

| Policy | Behavior |
|--------|----------|
| `never` | Never load the script, even if present |
| `if-present` | Load if the file exists, silently skip if not |
| `require` | Load the script; signal error if missing |

### Mapping hooks to supervisor behavior

```elisp
;; Supervisor registers on pid1-boot-hook:
(add-hook 'pid1-boot-hook #'supervisor--pid1-boot)

(defun supervisor--pid1-boot ()
  "Initialize supervisor PID1 mode."
  (when supervisor-pid1-mode-enabled
    ;; Load boot script per policy
    (supervisor--load-script supervisor-pid1-boot-script
                              supervisor-pid1-boot-policy)
    ;; Start service supervision
    (supervisor-start)))

;; Supervisor registers on pid1-poweroff-hook and pid1-reboot-hook:
(add-hook 'pid1-poweroff-hook #'supervisor--pid1-shutdown)
(add-hook 'pid1-reboot-hook   #'supervisor--pid1-shutdown)

(defun supervisor--pid1-shutdown ()
  "Clean shutdown of all supervised services."
  (when supervisor-pid1-mode-enabled
    ;; Load shutdown script per policy
    (supervisor--load-script supervisor-pid1-shutdown-script
                              supervisor-pid1-shutdown-policy)
    ;; Stop all services (with timeout)
    (supervisor-stop-all)))
```

### How `supervisor-pid1-mode-enabled` is auto-detected

```elisp
(defun supervisor--detect-pid1-mode ()
  "Auto-detect PID1 mode from Emacs state."
  (and (bound-and-true-p pid1-mode)     ; --pid1 flag was passed
       (= (emacs-pid) 1)))              ; actually running as PID 1
```

The double-check (`pid1-mode` AND PID 1) allows testing with `--pid1` on
non-PID1 processes while ensuring real PID1 behavior only activates when
actually running as the init process.

## Integration flow

```
Container start
  → Emacs --pid1 starts as PID 1
  → Emacs parses --pid1, sets pid1_mode=true
  → Signal handlers installed (SIGTERM→poweroff, SIGINT→reboot)
  → Child reaping enabled (waitpid(-1) in SIGCHLD handler)
  → Init files loaded (site-start.el, user init.el)
  → emacs-startup-hook fires
  → pid1-boot-hook fires
    → supervisor--pid1-boot runs
      → loads /lib/init/rc.boot.el (per policy)
      → starts supervised services
  → Emacs enters command loop (stays running as PID 1)

... normal operation, supervisor managing services ...

Container shutdown (docker stop / SIGTERM)
  → SIGTERM received
  → Signal handler sets pid1_poweroff_pending, quit flag
  → Main loop processes quit → calls kill-emacs
    → pid1-poweroff-hook fires
      → supervisor--pid1-shutdown runs
        → loads /lib/init/rc.shutdown.el (per policy)
        → stops all services cleanly
    → kill-emacs-hook fires (standard cleanup)
    → process exits
```

## No additional C changes needed

The Emacs patch provides all the C-level primitives that supervisor.el needs.
All policy decisions (script paths, load policies, service management) are
implemented entirely in Emacs Lisp. No additional C patches are required to
layer supervisor-side PID1 functionality on top.
