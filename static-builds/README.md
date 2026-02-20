# static-builds

Administrator guide for PID1-patched static Emacs builds.  These builds
ship PID1 capability only -- the administrator owns all runtime policy
(what to load, what to start, and how to wire el-init).

*For build instruction files for a static, portable Emacs without the PID1
patchset, see the `./stalimacs` directory.*

## Building the PID1-patched variant

Nix:

```bash
nix-build static-builds/emacs-static-nox-elinit-patched-for-pid1.nix
```

Arch PKGBUILD:

```bash
cd static-builds
makepkg -p PKGBUILD-static-nox-elinit-patched-for-pid1
sudo pacman -U emacs-nox-static-elinit-patched-for-pid1-*.pkg.tar.zst
```

The resulting binary includes:
- All features of the vanilla `emacs-static-nox` build (see `./stalimacs`)
- `--pid1` flag for PID1 init process mode
- `pid1-mode` Lisp variable (t when `--pid1` is passed, nil otherwise)
- `pid1-boot-hook`, `pid1-poweroff-hook`, `pid1-reboot-hook`
- Signal mapping: SIGTERM/SIGUSR1 trigger poweroff, SIGINT/SIGUSR2 trigger
  reboot, SIGHUP is ignored
- Automatic orphan child reaping

The binary does **not** include el-init Lisp files, C helper binaries
(`elinit-logd`, `elinit-runas`, `elinit-rlimits`), sbin scripts, or any
`site-start.el` autostart wiring.  The administrator provides all runtime
policy.

## Administrator startup wiring

The PID1-patched binary provides hooks but does not load el-init
automatically.  Add your own startup wiring in `early-init.el` or
`init.el`.

Minimal `early-init.el` example (runs before the main init file -- use
this when el-init must be available before `init.el` loads):

```emacs-lisp
;; early-init.el
(when (bound-and-true-p pid1-mode)
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/elinit")
  ;; If using prebuilt helpers, skip compile-on-startup
  (setq elinit-libexec-build-on-startup 'never))
```

Minimal `init.el` example:

```emacs-lisp
;; init.el
(when (bound-and-true-p pid1-mode)
  (require 'elinit))
```

Or for unconditional loading (when this Emacs instance is always PID1):

```emacs-lisp
;; init.el
(add-to-list 'load-path "/usr/share/emacs/site-lisp/elinit")
(require 'elinit)
(setq elinit-libexec-build-on-startup 'never)
```

## Common requirements

Both deployment paths (initramfs-based and Lisp-based early boot) share
the same base setup:

1. Build/install the `*-patched-for-pid1` variant.
2. Install el-init Lisp files where Emacs can find them (e.g.,
   `/usr/share/emacs/site-lisp/elinit/`).
3. Add startup wiring in `early-init.el` or `init.el` (see above).
4. Install Emacs at a stable absolute path, typically `/usr/bin/emacs`.
5. Configure bootloader kernel cmdline to hand off PID1 to Emacs:
   `init=/usr/bin/emacs --pid1`
6. Place system unit files under `/etc/elinit.el/`.
7. Set unit enablement in unit files (`:enabled t`) plus target membership
   (`:wanted-by (...)` or `:required-by (...)`) before first boot.
8. Install sbin scripts (`elinitctl`, `elinit-import`, `elinit-log-prune`,
   `elinit-logrotate`) from the el-init `sbin/` directory into your PATH.
9. If using features that depend on C helper binaries (`elinit-logd`,
   `elinit-runas`, `elinit-rlimits`), install them and configure their
   paths (see C helper binary provisioning below).

### Early boot responsibilities

Before elinit can supervise services, the system needs fundamental
infrastructure in place.  A Linux machine coming out of the bootloader
has almost nothing set up: no virtual filesystems, no device nodes, an
unchecked root filesystem mounted read-only, and no networking.  The
following tasks must happen before any real services can start:

1. **Mount virtual filesystems** -- `/proc`, `/sys`, `/dev` (devtmpfs),
   `/dev/pts` (devpts), `/dev/shm` (tmpfs), `/run` (tmpfs).
2. **Create runtime directories** -- `/run/lock`, `/run/log`, `/run/user`,
   and any other directories expected by services.
3. **Set permissions** -- correct modes on `/run` (0755), `/dev/pts`
   (0620, gid=5), `/dev/shm` (1777), etc.
4. **Seed the random pool** -- write a saved seed into `/dev/urandom`, or
   gather entropy if no seed file exists.
5. **Start device management** -- trigger udev (or mdev/eudev) to populate
   `/dev`, load firmware, and create device symlinks.  Alternatively, udev
   can run as a supervised elinit unit once the virtual filesystems are
   mounted (see note below).
6. **Remount root read-only** -- prepare for filesystem check.
7. **Check filesystems** -- run `fsck` on all local filesystems.  Drop to
   an emergency shell or halt on failure.
8. **Remount root read-write** -- after a clean fsck, remount `/` as rw.
9. **Mount all local filesystems** -- `mount -a` for everything in fstab
   that is not a network filesystem.
10. **Set up loopback networking** -- `ip link set up dev lo`.
11. **Set the hostname** -- read `/etc/hostname` and write to
    `/proc/sys/kernel/hostname`.
12. **Apply sysctl settings** -- `sysctl -p /etc/sysctl.conf`.

After these steps complete, the system is ready for normal service
supervision (sshd, dhcpcd, getty, syslog, and so on), which elinit
handles through its unit system.

**These responsibilities must be handled by exactly one of the two paths
described below.  They cannot be skipped.**

#### Path 1: Initramfs handles early boot (more common)

Most modern Linux systems use an initramfs (initial RAM filesystem) that
the bootloader loads alongside the kernel.  The initramfs runs its own
`/init` script before pivoting to the real root filesystem.  When using
an initramfs, all of the early boot tasks listed above (mounting virtual
filesystems, fsck, device setup, etc.) are handled inside the initramfs
before control ever reaches Emacs.

In this deployment, the kernel cmdline `init=` parameter points to Emacs
on the real root, and the initramfs `switch_root` hands off PID1 to it
with the system already in a usable state.  Elinit only needs to
supervise services; it does not need an early boot oneshot unit.

This is the simpler path: the initramfs tools (dracut, mkinitcpio,
initramfs-tools, or a custom script) already know how to do early boot
correctly.

#### Path 2: Elinit handles early boot (static kernel, no initramfs)

When booting a static kernel with no initramfs, the kernel mounts the
root filesystem directly and executes `init=` immediately.  Emacs becomes
PID1 on a bare system with nothing set up.  All of the early boot tasks
listed above must be performed by elinit itself, as early as possible.

The recommended approach is a blocking oneshot unit that runs before
everything else:

```emacs-lisp
;; /etc/elinit.el/rc-boot.el
(:id "rc-boot"
 :command "/usr/bin/emacs --batch -Q -l /usr/local/lib/elinit/rc.boot.el"
 :type oneshot
 :oneshot-blocking t
 :enabled t
 :required-by ("basic.target"))
```

The script loaded by this unit (`rc.boot.el`) is responsible for every
step: mounting proc/sys/dev/run, creating directories, setting
permissions, seeding the RNG, starting udev, doing the remount-fsck-remount
cycle, mounting all local filesystems, bringing up loopback, setting the
hostname, and applying sysctl.  See `static-builds/scripts/rc.boot.el.example`
for a complete working example that performs all of these tasks.

Because the unit has `:oneshot-blocking t` and `:required-by
("basic.target")`, no other units will start until it finishes
successfully.

**Note on udev:** udev can either run inside the early boot script (as a
one-shot trigger-and-settle sequence) or as a supervised elinit unit.
Running it inside the boot script is simpler because device nodes are
guaranteed to exist before any service units start.  Running it as a
supervised unit allows elinit to restart it on failure, but requires
careful ordering (`:after ("rc-boot")`) so that `/dev`, `/sys`, and
`/run` are already mounted.

## C helper binary provisioning

The PID1 build does not bundle C helper binaries.  When using features
that depend on them (structured logging, privilege dropping, resource
limits), you must provision them yourself:

1. **Build from source:** compile the helpers from the el-init `libexec/`
   directory and install them at a known path.
2. **Package separately:** include the helpers in a separate package or
   your system image build.
3. **Point el-init to the binaries:** set the command variables in your
   init file:

```emacs-lisp
(setq elinit-logd-command "/usr/local/libexec/elinit-logd")
(setq elinit-runas-command "/usr/local/libexec/elinit-runas")
(setq elinit-rlimits-command "/usr/local/libexec/elinit-rlimits")
```

If not using these features, no helpers are needed.

## File placement reference

Use these canonical system paths:

| Purpose | Path |
| --- | --- |
| El-init Lisp files | `/usr/share/emacs/site-lisp/elinit/` |
| C helper binaries | `/usr/local/libexec/` or admin-chosen path |
| Sbin scripts (`elinitctl`, etc.) | `/usr/local/bin/` or anywhere in PATH |
| Elinit unit files (system admin tier) | `/etc/elinit.el/*.el` |
| Optional script examples for admin reuse (not auto-loaded) | `/lib/init/rc.boot.el`, `/lib/init/rc.shutdown.el` |
| Optional local boot extension | `/etc/rc.0.local.el` |
| Optional local post-boot extension | `/etc/rc.1.local.el` |

The script paths above are conventions only. `elinit-pid1` no longer auto-loads
these files.  Use explicit units if you want them executed.

## How to enable units from chroot before first boot

Recommended workflow:

1. Mount target root at `/mnt`.
2. Create unit root directory in the target.
3. Write unit files with `:enabled t` and target membership.
4. Optionally stage your own early-boot assets.
5. Ensure permissions and executable bits are correct.

Example:

```bash
install -d /mnt/etc/elinit.el
install -d /mnt/usr/local/lib/elinit

cat > /mnt/etc/elinit.el/sshd.el <<'EOF'
(:id "sshd"
 :command "/usr/sbin/sshd -D -e"
 :type simple
 :enabled t
 :wanted-by ("multi-user.target")
 :restart on-failure
 :logging t)
EOF

cp static-builds/scripts/rc.boot.el.example /mnt/usr/local/lib/elinit/early-boot.el

cat > /mnt/etc/elinit.el/early-boot.el <<'EOF'
(:id "early-boot"
 :command "/usr/bin/emacs --batch -Q -l /usr/local/lib/elinit/early-boot.el"
 :type oneshot
 :oneshot-blocking t
 :enabled t
 :required-by ("basic.target"))
EOF
```

This is enough to seed first boot behavior without running `elinitctl` in
the chroot.

## Early boot strategies

See "Early boot responsibilities" under Common requirements above for
the full list of tasks and the two deployment paths (initramfs vs.
elinit-managed early boot).

`elinit-pid1` handles lifecycle (`elinit-start`/`elinit-stop-now`) but
does not auto-load boot/shutdown script files.  All early boot logic
must be wired through explicit units.

## Shutdown and reboot scripting

PID1 signal handling and service-stop behavior:

- SIGTERM/SIGUSR1 -> `pid1-poweroff-hook` -> `elinit-stop-now`
- SIGINT/SIGUSR2 -> `pid1-reboot-hook` -> `elinit-stop-now`
- No implicit `rc.shutdown.el` probing or auto-load

When no extra transition tasks are needed, rely on this default lifecycle stop
path.

When custom shutdown/reboot tasks are needed, keep them explicit and
supervisor-managed:

1. Add blocking oneshot units tied to transition targets.
2. Use `shutdown.target` for common pre-stop work.
3. Use `poweroff.target` and/or `reboot.target` for mode-specific work.
4. Trigger transitions explicitly with `elinitctl init --yes 0` (poweroff path)
   or `elinitctl init --yes 6` (reboot path).

Example unit wiring:

```emacs-lisp
;; /etc/elinit.el/pre-shutdown.el
(:id "pre-shutdown"
 :command "/usr/bin/emacs --batch -Q -l /usr/local/lib/elinit/pre-shutdown.el"
 :type oneshot
 :oneshot-blocking t
 :enabled t
 :required-by ("shutdown.target"))

;; /etc/elinit.el/pre-poweroff.el
(:id "pre-poweroff"
 :command "/usr/bin/emacs --batch -Q -l /usr/local/lib/elinit/pre-poweroff.el"
 :type oneshot
 :oneshot-blocking t
 :enabled t
 :required-by ("poweroff.target"))

;; /etc/elinit.el/pre-reboot.el
(:id "pre-reboot"
 :command "/usr/bin/emacs --batch -Q -l /usr/local/lib/elinit/pre-reboot.el"
 :type oneshot
 :oneshot-blocking t
 :enabled t
 :required-by ("reboot.target"))
```

`init 0`/`init 6` are target transitions; they do not directly issue kernel
poweroff/reboot syscalls.

## First-boot validation checklist

After first boot:

1. Verify PID1 handoff worked (`ps -p 1 -o comm,args`).
2. Verify elinit came up (`elinitctl status`).
3. Verify configured units are loaded (`elinitctl list-units`).
4. Verify C helper binaries are reachable (if using logging/runas/rlimits).
