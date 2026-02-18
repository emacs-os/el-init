# static-builds

Administrator guide for static Emacs build paths.

This file is operator-facing guidance. Implementation requirements and delivery
gates live in plan files:

- `static-builds/PLAN-pid1-emacs-patch-support.md` -- Emacs upstream patch prerequisite
- `static-builds/PLAN.md` -- packaging/integration execution plan

## Dependency and status chain

1. `static-builds/PLAN-pid1-emacs-patch-support.md` is the prerequisite for all
   `*-patched-for-pid1` build outputs.
2. `static-builds/PLAN.md` consumes that prerequisite to produce concrete
   PKGBUILD/Nix variants and final static-builds documentation wiring.
3. This `README.md` remains the administrator handbook and is updated as those
   plan gates are completed.

## Pick your path

1. I just want a vanilla portable glibc-compiled static Emacs.
2. I want static Emacs with `--pid1` reaping support plus baked-in
   supervisor.el, and I handle early boot in initramfs.
3. I want static Emacs with `--pid1` reaping support plus baked-in
   supervisor.el, and I handle early boot in Emacs Lisp (`rc.boot.el` /
   `rc.shutdown.el`).

## Current file map

Vanilla static variants (available now):

- `PKGBUILD-static-nox`
- `PKGBUILD-static-nox-minimal`
- `PKGBUILD-static-nox-nativecomp`
- `emacs-static-nox.nix`
- `emacs-static-nox-minimal.nix`
- `emacs-static-nox-nativecomp.nix`

Patched + baked variants (plan target names):

- `PKGBUILD-static-nox-supervisor-patched-for-pid1`
- `emacs-static-nox-supervisor-patched-for-pid1.nix`

Status: planned names pending prerequisite completion in
`static-builds/PLAN-pid1-emacs-patch-support.md`.

See `static-builds/PLAN.md` for the phased implementation contract.
See `static-builds/PLAN-pid1-emacs-patch-support.md` for the Emacs patch requirement spec.

## Path 1 -- vanilla portable static Emacs

Use the existing vanilla files above. These variants do not assume PID1 mode
and do not require baked supervisor startup.

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

## Paths 2 and 3 -- common requirements

These two paths share the same base mechanics:

1. Build/install the `*-supervisor-patched-for-pid1` variant.
2. Install Emacs at a stable absolute path, typically `/usr/bin/emacs`.
3. Configure bootloader kernel cmdline to hand off PID1 to Emacs:
   `init=/usr/bin/emacs`
4. Place system unit files under `/etc/supervisor.el/`.
5. Set unit enablement in unit files (`:enabled t`) plus target membership
   (`:wanted-by (...)` or `:required-by (...)`) before first boot.
6. Keep helper binaries (`supervisor-logd`, `supervisor-runas`) installed and
   executable in `libexec` for runtime operation.

Prerequisite gate: paths 2 and 3 require completed Emacs patch artifacts from
`static-builds/PLAN-pid1-emacs-patch-support.md`.

### C helper binary policy for admins

For patched+baked variants, the expected process is:

1. Ship prebuilt helper binaries in the package output.
2. Do not require a compiler at runtime.
3. If you choose to manage helpers yourself, either:
   - point `supervisor-logd-command` / `supervisor-runas-command` at your
     managed binaries, or
   - rebuild helpers in an admin/dev environment.

## File placement reference

Use these canonical system paths:

| Purpose | Path |
| --- | --- |
| Supervisor unit files (system admin tier) | `/etc/supervisor.el/*.el` |
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
install -d /mnt/etc/supervisor.el
install -d /mnt/lib/init

cat > /mnt/etc/supervisor.el/sshd.el <<'EOF'
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

This is enough to seed first boot behavior without running `supervisorctl` in
the chroot.

## Path 2 specifics -- initramfs handles early boot

Use this when mount/dev/fsck/network setup is already done before PID1 handoff.

1. Do not rely on rc Lisp scripts for core early init.
2. Keep `/lib/init/rc.boot.el` optional or absent.
3. Focus on supervisor unit set under `/etc/supervisor.el/`.

## Path 3 specifics -- Emacs Lisp handles early boot

Use this when you want sinit-style boot/shutdown logic in Lisp.

1. Place `rc.boot.el` at `/lib/init/rc.boot.el`.
2. Place `rc.shutdown.el` at `/lib/init/rc.shutdown.el`.
3. Keep scripts executable and deterministic.
4. Keep infrastructure setup in `rc.boot.el` (mounts, `/run`, udev, etc.) only
   if initramfs does not already handle it.

## First-boot validation checklist

After first boot:

1. Verify PID1 handoff worked (`ps -p 1 -o comm,args`).
2. Verify supervisor came up (`supervisorctl status`).
3. Verify configured units are loaded (`supervisorctl list-units`).
4. Verify no stale helper rebuild is required at startup.

## Notes

The `-patched-for-pid1` suffix indicates PID1-capable build output. It does not
mean PID1 behavior is automatically enabled in all contexts.
