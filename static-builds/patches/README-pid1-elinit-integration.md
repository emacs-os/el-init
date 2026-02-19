# PID1 Emacs Patch: Elinit Integration Guide

## Overview

The PID1 Emacs patches add three Lisp hooks that elinit uses to
orchestrate PID1 init behavior. The Emacs C code handles the low-level
concerns (signal handling, child reaping, option parsing), while all
policy decisions live in Lisp (elinit).

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
    → pid1-poweroff-hook (or pid1-reboot-hook)   ← elinit cleanup
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

## Elinit-side integration

Elinit adds its PID1 integration via these hooks through the
`elinit-pid1` module (`elinit-pid1.el`).  The elinit-side Lisp
variables control behavior:

### Variables

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `elinit-pid1-mode-enabled` | boolean | auto-detect | Gates boot/shutdown script loading and service lifecycle calls |
| `elinit-pid1-boot-script` | string | `/lib/init/rc.boot.el` | Boot script path |
| `elinit-pid1-shutdown-script` | string | `/lib/init/rc.shutdown.el` | Shutdown script path |
| `elinit-pid1-boot-policy` | symbol | `if-present` | Boot script policy |
| `elinit-pid1-shutdown-policy` | symbol | `if-present` | Shutdown script policy |

### Policies

| Policy | Behavior |
|--------|----------|
| `never` | Never load the script, even if present |
| `if-present` | Load if the file is a readable regular file, silently skip otherwise |
| `require` | Load the script; signal error if missing or unreadable |

### Mapping hooks to elinit behavior

```elisp
;; Elinit registers on pid1-boot-hook:
(add-hook 'pid1-boot-hook #'elinit--pid1-boot)

(defun elinit--pid1-boot ()
  "Initialize elinit PID1 mode."
  (when elinit-pid1-mode-enabled
    ;; Load boot script per policy
    (elinit--pid1-load-script elinit-pid1-boot-script
                              elinit-pid1-boot-policy)
    ;; Start service management
    (elinit-start)))

;; Elinit registers on pid1-poweroff-hook and pid1-reboot-hook:
(add-hook 'pid1-poweroff-hook #'elinit--pid1-shutdown)
(add-hook 'pid1-reboot-hook   #'elinit--pid1-shutdown)

(defun elinit--pid1-shutdown ()
  "Clean shutdown of all managed services."
  (when elinit-pid1-mode-enabled
    ;; Load shutdown script per policy
    (elinit--pid1-load-script elinit-pid1-shutdown-script
                              elinit-pid1-shutdown-policy)
    ;; Stop all services synchronously
    (elinit-stop-now)))
```

### How `elinit-pid1-mode-enabled` is auto-detected

```elisp
(defun elinit--pid1-detect-mode ()
  "Return non-nil when running as PID 1 with pid1-mode enabled."
  (and (bound-and-true-p pid1-mode)     ; --pid1 flag was passed
       (= (emacs-pid) 1)))              ; actually running as PID 1
```

The double-check (`pid1-mode` AND PID 1) allows testing with `--pid1` on
non-PID1 processes while ensuring real PID1 behavior only activates when
actually running as the init process.

### Gating architecture

PID1 behavior has three independent gates at different levels:

| Gate | What it controls | Where |
|------|-----------------|-------|
| `pid1-mode` (C-level) | Hook registration at module load time | `elinit-pid1.el` load-time `when` block |
| `elinit-pid1-mode-enabled` (Lisp) | Boot/shutdown script loading and `elinit-start`/`elinit-stop-now` calls | `elinit--pid1-boot`, `elinit--pid1-shutdown` |
| C signal handlers (C-level) | Child reaping (SIGCHLD), shutdown signals (SIGTERM, etc.) | Emacs C code, outside Lisp control |

Setting `elinit-pid1-mode-enabled` to nil disables the Lisp-side service
lifecycle actions but does not affect C-level reaping or signal handling,
which are controlled entirely by the `--pid1` flag at the Emacs C level.

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
    → elinit--pid1-boot runs
      → loads /lib/init/rc.boot.el (per policy)
      → starts managed services
  → Emacs enters command loop (stays running as PID 1)

... normal operation, elinit managing services ...

Container shutdown (docker stop / SIGTERM)
  → SIGTERM received
  → Signal handler sets pid1_poweroff_pending, quit flag
  → Main loop processes quit → calls kill-emacs
    → pid1-poweroff-hook fires
      → elinit--pid1-shutdown runs
        → loads /lib/init/rc.shutdown.el (per policy)
        → stops all services cleanly
    → kill-emacs-hook fires (standard cleanup)
    → process exits
```

## No additional C changes needed

The Emacs patch provides all the C-level primitives that elinit needs.
All policy decisions (script paths, load policies, service management) are
implemented entirely in Emacs Lisp. No additional C patches are required to
layer elinit-side PID1 functionality on top.
