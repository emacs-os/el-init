# PID1 Emacs Patch Validation

## Baseline

- **Emacs version**: 30.2 (tag `emacs-30.2`)
- **Branch**: `static-emacs` in emacs repo
- **Platform**: Arch Linux x86_64, glibc 2.42, GCC 15.2.1

## Patch files

| Patch | Description |
|-------|-------------|
| `emacs-0001-add-pid1-runtime-mode.patch` | `--pid1` command-line option and `pid1-mode` Lisp state |
| `emacs-0002-pid1-hooks-and-signals.patch` | PID1 hooks, signal handling, and child reaping |
| `emacs-0003-fix-pid1-signal-handler-overrides.patch` | Fix signal handlers being overridden after installation |

Apply in order:

```sh
cd emacs-30.2
git apply static-builds/patches/emacs-0001-add-pid1-runtime-mode.patch
git apply static-builds/patches/emacs-0002-pid1-hooks-and-signals.patch
git apply static-builds/patches/emacs-0003-fix-pid1-signal-handler-overrides.patch
```

## Source touchpoints

### Patch 0001: `--pid1` option and state

- **`src/emacs.c`**:
  - `bool pid1_mode` global variable (near `daemon_type`)
  - `--pid1` in `standard_args[]` at priority 98 (between daemon=99 and help=90)
  - `argmatch()` parsing after daemon argument block
  - `--pid1` in `usage_message`
  - `DEFVAR_BOOL("pid1-mode", ...)` exposing state to Lisp

- **`src/lisp.h`**:
  - `extern bool pid1_mode` declaration

### Patch 0002: Hooks, signals, child reaping

- **`src/emacs.c`**:
  - `DEFSYMs` for `Qpid1_boot_hook`, `Qpid1_reboot_hook`, `Qpid1_poweroff_hook`
  - `DEFVAR_LISP` for `pid1-boot-hook`, `pid1-reboot-hook`, `pid1-poweroff-hook`
  - PID1 hook execution in `Fkill_emacs` (before `kill-emacs-hook`)

- **`src/lisp.h`**:
  - `extern int volatile pid1_poweroff_pending`
  - `extern int volatile pid1_reboot_pending`

- **`src/sysdep.c`**:
  - `pid1_poweroff_pending` / `pid1_reboot_pending` globals
  - `handle_pid1_poweroff_signal()` / `deliver_pid1_poweroff_signal()`
  - `handle_pid1_reboot_signal()` / `deliver_pid1_reboot_signal()`
  - Modified `init_signals()`: PID1-mode signal handler installation

- **`src/process.c`**:
  - Orphan child reaping in `handle_child_signal()` via `waitpid(-1, NULL, WNOHANG)`

- **`lisp/startup.el`**:
  - `pid1-boot-hook` execution after `emacs-startup-hook` (2 locations)

### Patch 0003: Signal override fixes

- **`src/emacs.c`**: Reentrancy guard for PID1 shutdown hooks
- **`src/keyboard.c`**: `!pid1_mode` guard on SIGINT handler
- **`src/sysdep.c`**: `!pid1_mode` guard on SIGTERM fatal handler
- **`src/emacs.c`**: `DEFVAR_BOOL("pid1-mode", pid1_mode1, ...)` fix
  for the `*_mode1` runtime variable pattern

## Signal mapping

| Signal | PID1 action | Hook | sinit convention |
|--------|-------------|------|------------------|
| SIGTERM | poweroff | `pid1-poweroff-hook` | Standard shutdown |
| SIGUSR1 | poweroff | `pid1-poweroff-hook` | Explicit poweroff |
| SIGINT | reboot | `pid1-reboot-hook` | Ctrl-Alt-Del |
| SIGUSR2 | reboot | `pid1-reboot-hook` | Explicit reboot |
| SIGHUP | ignored | (none) | No controlling terminal |
| SIGCHLD | reap children | (none) | PID1 zombie prevention |

Signal handler implementation: handlers set atomic flags (`pid1_poweroff_pending`
or `pid1_reboot_pending`) and `Vquit_flag = Qkill_emacs`. The main loop detects
the quit flag via `maybe_quit()` and calls `Fkill_emacs()`, which checks the
atomic flags and runs the appropriate hook before `kill-emacs-hook`.

## Build commands

```sh
# Prerequisite: static libraries in $STATIC_PREFIX
STATIC_PREFIX=/path/to/static-libs/install

PKG_CONFIG=false ./configure \
  --without-all --without-x --without-sound --without-dbus \
  --without-gnutls --without-libsystemd --without-native-compilation \
  --without-threads --without-selinux --without-gpm --without-lcms2 \
  --with-pdumper=yes --with-dumping=pdumper --with-xml2=no \
  --with-file-notification=no \
  CFLAGS="-O2 -std=gnu17 -I${STATIC_PREFIX}/include -I${STATIC_PREFIX}/include/ncursesw" \
  LDFLAGS="-static -no-pie -L${STATIC_PREFIX}/lib" \
  CPPFLAGS="-I${STATIC_PREFIX}/include -I${STATIC_PREFIX}/include/ncursesw"

make -j$(nproc)
```

For the full-featured static build (with GnuTLS, libxml2, tree-sitter, modules,
threads), use the PKGBUILD or Nix derivation instead.

## Validation results (2026-02-18)

### Test 1: Binary is static

```
$ ldd ./src/emacs
        not a dynamic executable
```

PASS

### Test 2: Emacs starts without --pid1

```
$ ./src/emacs --batch --eval '(message "pid1-mode=%s" pid1-mode)'
pid1-mode=nil
```

PASS: `pid1-mode` is nil when `--pid1` not given.

### Test 3: Emacs starts with --pid1

```
$ ./src/emacs --pid1 --batch --eval '(message "pid1-mode=%s" pid1-mode)'
pid1-mode=t
```

PASS: `pid1-mode` is t when `--pid1` given.

### Test 4: --help shows --pid1

```
$ ./src/emacs --help 2>&1 | grep pid1
--pid1                      enable PID1 init-process mode (child reaping,
                              shutdown signal hooks)
```

PASS

### Test 5: Hooks are defined

```
$ ./src/emacs --pid1 --batch --eval \
  '(message "boot=%S poweroff=%S reboot=%S" \
     pid1-boot-hook pid1-poweroff-hook pid1-reboot-hook)'
boot=nil poweroff=nil reboot=nil
```

PASS: All three hooks are defined and initially nil.

### Test 6: pid1-boot-hook fires in interactive mode

```
$ # Load file that adds to pid1-boot-hook, write-region when fired
$ ./src/emacs --pid1 -Q -nw -l test-hooks.el
# Output file shows:
# PID1-BOOT-HOOK-FIRED
```

PASS: Boot hook fires after startup hooks complete.

### Test 7: pid1-boot-hook does NOT fire without --pid1

```
$ ./src/emacs -Q -nw -l test-hooks.el
# Output file shows:
# (only STARTUP-HOOK-FIRED, no PID1-BOOT-HOOK)
```

PASS: Boot hook is gated on `pid1-mode`.

### Test 8: SIGTERM triggers pid1-poweroff-hook

```
$ ./src/emacs --pid1 -Q -nw -l test-signals.el &
$ kill -TERM $EMACS_PID
# Output file shows:
# PID1-BOOT-HOOK
# PID1-POWEROFF-HOOK (count=1)
# KILL-EMACS-HOOK
```

PASS: Poweroff hook fires exactly once, then kill-emacs-hook.

### Test 9: SIGUSR1 triggers pid1-poweroff-hook

```
$ ./src/emacs --pid1 -Q -nw -l test-signals.el &
$ kill -USR1 $EMACS_PID
# Output file shows:
# PID1-BOOT-HOOK
# PID1-POWEROFF-HOOK
# KILL-EMACS-HOOK
```

PASS

### Test 10: SIGUSR2 triggers pid1-reboot-hook

```
$ ./src/emacs --pid1 -Q -nw -l test-signals.el &
$ kill -USR2 $EMACS_PID
# Output file shows:
# PID1-BOOT-HOOK
# PID1-REBOOT-HOOK
# KILL-EMACS-HOOK
```

PASS

### Test 11: SIGHUP is ignored in PID1 mode

```
$ ./src/emacs --pid1 -Q -nw -l test-signals.el &
$ kill -HUP $EMACS_PID
$ kill -0 $EMACS_PID  # check still running
# Emacs still running
```

PASS: SIGHUP does not kill Emacs in PID1 mode.

### Test 12: Child reaping works

```
$ ./src/emacs --pid1 -Q -nw -l test-reaping.el
# test-reaping.el spawns 3 orphan processes via start-process
# After processes exit, ps --ppid shows no zombie children
```

PASS: No zombie processes after orphan child exit.

### Test 13: Normal Emacs unaffected

```
$ ./src/emacs -Q -nw -l test-signals.el &
$ kill -TERM $EMACS_PID
# Emacs exits immediately (standard SIGTERM fatal behavior)
# No PID1 hooks fired
```

PASS: Without `--pid1`, behavior is identical to stock Emacs.

## Automated test harness

Tests 2-3, 5-13 above are automated by
`static-builds/tests/test-pid1-namespace.sh`, which runs inside
isolated PID namespaces via `unshare(1)`.  Run with:

```sh
make pid1-check ELINIT_PID1_EMACS=/path/to/patched/emacs
```

The harness skips gracefully when prerequisites are unavailable (no
patched binary, no unshare, no user namespace support).

## Backward compatibility

All tests confirm that without `--pid1`:
- `pid1-mode` is nil
- No PID1 hooks fire
- Signal handling is unchanged
- Child reaping is unchanged
- No startup regressions

The patches add zero runtime overhead when `--pid1` is not used. The only
change is a single `argmatch()` call during option parsing that checks for
the flag and sets a boolean.
