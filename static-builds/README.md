# static-builds

Administrator guide for static Emacs build paths.

This file is operator-facing guidance. Implementation requirements and delivery
gates live in plan files:

- `static-builds/PLAN-pid1-emacs-patch-support.md` -- Emacs upstream patch prerequisite
- `static-builds/PLAN.md` -- packaging/integration execution plan

## Dependency chain

1. `PLAN-pid1-emacs-patch-support.md` defines the Emacs patch prerequisite.
   Status: **complete** — patches in `patches/`.
2. `PLAN.md` defines the packaging/integration plan.
   Status: **Track A in progress** -- 6 vanilla variants available, 2 baked variants under review.
3. This `README.md` is the administrator handbook.

## Pick your path

1. I just want a vanilla portable glibc-compiled static Emacs.
2. I want static Emacs with `--pid1` reaping support plus baked-in
   elinit, and I handle early boot in initramfs.
3. I want static Emacs with `--pid1` reaping support plus baked-in
   elinit, and I handle early boot in Emacs Lisp (`rc.boot.el` /
   `rc.shutdown.el`). **(Planned -- Track B, not yet implemented.)**

## Variant matrix

| Variant | Type | PID1 | Elinit | Size |
|---------|------|------|------------|------|
| `PKGBUILD-static-nox-minimal` | Arch | no | no | ~10MB |
| `PKGBUILD-static-nox` | Arch | no | no | ~14MB |
| `PKGBUILD-static-nox-nativecomp` | Arch | no | no | ~376MB |
| `PKGBUILD-static-nox-elinit-patched-for-pid1` | Arch | yes | yes | ~14MB |
| `emacs-static-nox-minimal.nix` | Nix | no | no | ~10MB |
| `emacs-static-nox.nix` | Nix | no | no | ~14MB |
| `emacs-static-nox-nativecomp.nix` | Nix | no | no | ~376MB |
| `emacs-static-nox-elinit-patched-for-pid1.nix` | Nix | yes | yes | ~14MB |

PID1 patches: `patches/emacs-0001-*.patch` through `emacs-0003-*.patch`
(see `patches/README.md` for details).

See `PLAN.md` for the implementation contract.
See `PLAN-pid1-emacs-patch-support.md` for the Emacs patch spec.

## Path 1 -- vanilla portable static Emacs

Use the existing vanilla files above. These variants do not assume PID1 mode
and do not require baked elinit startup.

Example Nix commands:

```bash
nix-build static-builds/emacs-static-nox-minimal.nix
nix-build static-builds/emacs-static-nox.nix
nix-build static-builds/emacs-static-nox-nativecomp.nix
```

Example PKGBUILD workflow:

```bash
cp static-builds/PKGBUILD-static-nox ./PKGBUILD
makepkg -f
```

Swap `PKGBUILD-static-nox` for `-minimal` or `-nativecomp` as needed.

### Building the PID1-patched variant

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
- All features of `PKGBUILD-static-nox` / `emacs-static-nox.nix`
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

## Paths 2 and 3 -- common requirements

These two paths share the same base mechanics:

1. Build/install the `*-elinit-patched-for-pid1` variant.
2. Install Emacs at a stable absolute path, typically `/usr/bin/emacs`.
3. Configure bootloader kernel cmdline to hand off PID1 to Emacs:
   `init=/usr/bin/emacs`
4. Place system unit files under `/etc/elinit.el/`.
5. Set unit enablement in unit files (`:enabled t`) plus target membership
   (`:wanted-by (...)` or `:required-by (...)`) before first boot.
6. Keep helper binaries (`elinit-logd`, `elinit-runas`) installed and
   executable in `libexec` for runtime operation.

Build with `PKGBUILD-static-nox-elinit-patched-for-pid1` (Arch) or
`emacs-static-nox-elinit-patched-for-pid1.nix` (Nix).

### C helper binary policy for admins

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

The rc script path convention above matches `static-builds/scripts/*.example`.

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

## Path 2 specifics -- initramfs handles early boot

Use this when mount/dev/fsck/network setup is already done before PID1 handoff.

1. Do not rely on rc Lisp scripts for core early init.
2. Keep `/lib/init/rc.boot.el` optional or absent.
3. Focus on elinit unit set under `/etc/elinit.el/`.

## Path 3 specifics -- Emacs Lisp handles early boot

**Status: planned (Track B) -- not yet implemented in elinit.**
The PID1 Emacs patches (hooks, signals, reaping) are complete, but the
elinit-side PID1 variables and rc script loading logic described below are
Track B deliverables that have not been implemented yet. See `PLAN.md`
phases B2/B3.

Use this when you want sinit-style boot/shutdown logic in Lisp.

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

## Notes

The `-patched-for-pid1` suffix indicates PID1-capable build output. It does not
mean PID1 behavior is automatically enabled in all contexts.
