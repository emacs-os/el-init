# static-builds

Administrator guide for PID1-patched static Emacs builds with baked-in elinit.
Building with the `-patched-for-pid1` variant produces a PID1-capable binary,
but PID1 behavior is only active when launched with `--pid1`.

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
- Bundled elinit with autostart (when `--pid1` is used) -- no user init
  files or `~/.emacs` configuration required; the packaged `site-start.el`
  bootstrap handles loading and startup automatically
- Prebuilt C helpers (no compiler needed at runtime)

### Disable gate

The elinit autostart can be suppressed even when `--pid1` is active:

```bash
# Environment variable
EMACS_ELINIT_DISABLE=1 emacs --pid1

# Or in early-init.el
(setq elinit-pid1-autostart-disabled t)
```

## Common requirements

Both deployment paths (initramfs-based and Lisp-based early boot) share the
same base mechanics:

1. Build/install the `*-elinit-patched-for-pid1` variant.
2. Install Emacs at a stable absolute path, typically `/usr/bin/emacs`.
3. Configure bootloader kernel cmdline to hand off PID1 to Emacs:
   `init=/usr/bin/emacs`
4. Place system unit files under `/etc/elinit.el/`.
5. Set unit enablement in unit files (`:enabled t`) plus target membership
   (`:wanted-by (...)` or `:required-by (...)`) before first boot.
6. Keep helper binaries (`elinit-logd`, `elinit-runas`) installed and
   executable in `libexec` for runtime operation.

## C helper binary policy

For patched+baked variants, the expected process is:

1. Ship prebuilt helper binaries in the package output.
2. Do not require a compiler at runtime.
3. If you choose to manage helpers yourself, either:
   - point `elinit-logd-command` / `elinit-runas-command` at your
     managed binaries, or
   - rebuild helpers in an admin/dev environment.

## File placement reference

Use these canonical system paths:

| Purpose | Path |
| --- | --- |
| Elinit unit files (system admin tier) | `/etc/elinit.el/*.el` |
| Optional rc boot script | `/lib/init/rc.boot.el` |
| Optional rc shutdown script | `/lib/init/rc.shutdown.el` |
| Optional local boot extension | `/etc/rc.0.local.el` |
| Optional local post-boot extension | `/etc/rc.1.local.el` |

The rc script path convention above matches `scripts/*.example` in this directory.

## How to enable units from chroot before first boot

Recommended workflow:

1. Mount target root at `/mnt`.
2. Create unit root directory in the target.
3. Write unit files with `:enabled t` and target membership.
4. Optionally stage `rc.boot.el` / `rc.shutdown.el`.
5. Ensure permissions and executable bits are correct.

Example:

```bash
install -d /mnt/etc/elinit.el
install -d /mnt/lib/init

cat > /mnt/etc/elinit.el/sshd.el <<'EOF'
(:id "sshd"
 :command "/usr/sbin/sshd -D -e"
 :type simple
 :enabled t
 :wanted-by ("multi-user.target")
 :restart on-failure
 :logging t)
EOF

cp static-builds/scripts/rc.boot.el.example /mnt/lib/init/rc.boot.el
cp static-builds/scripts/rc.shutdown.el.example /mnt/lib/init/rc.shutdown.el
chmod 755 /mnt/lib/init/rc.boot.el /mnt/lib/init/rc.shutdown.el
```

This is enough to seed first boot behavior without running `elinitctl` in
the chroot.

## Early boot strategies

### Initramfs handles early boot

Use this when mount/dev/fsck/network setup is already done before PID1 handoff.
Do not rely on rc Lisp scripts for core early init -- keep `/lib/init/rc.boot.el`
optional or absent and focus on the elinit unit set under `/etc/elinit.el/`.

### Emacs Lisp handles early boot

Use this when you want sinit-style boot/shutdown logic in Lisp.
The `elinit-pid1` module (`elinit-pid1.el`) provides PID1 variables,
script loading policies, and hook wiring.

1. Place `rc.boot.el` at `/lib/init/rc.boot.el`.
2. Place `rc.shutdown.el` at `/lib/init/rc.shutdown.el`.
3. Keep scripts executable and deterministic.
4. Keep infrastructure setup in `rc.boot.el` (mounts, `/run`, udev, etc.) only
   if initramfs does not already handle it.

## First-boot validation checklist

After first boot:

1. Verify PID1 handoff worked (`ps -p 1 -o comm,args`).
2. Verify elinit came up (`elinitctl status`).
3. Verify configured units are loaded (`elinitctl list-units`).
4. Verify no stale helper rebuild is required at startup.

