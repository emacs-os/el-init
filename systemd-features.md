# Systemd Features (Comprehensive)

Everything systemd has absorbed over the years, organized by domain.


## Core Service Management

### PID 1 and Service Manager
- **systemd** -- main system and service manager, PID 1
- **systemctl** -- primary CLI for controlling the service manager
- **init** -- symlink to systemd for SysV compatibility
- **systemd-analyze** -- boot performance profiling, unit verification, calendar expression testing, security auditing, dependency graphing

### Unit Types
- **.service** -- system services and daemons
- **.socket** -- socket-based activation (stream, datagram, sequential-packet, FIFO)
- **.device** -- device units populated from udev/sysfs
- **.mount** -- filesystem mount points (auto-generated from /etc/fstab or declared)
- **.automount** -- on-demand filesystem mounting triggered by access
- **.swap** -- swap space activation
- **.target** -- synchronization points for grouping units (replaces runlevels)
- **.path** -- inotify-based file/directory watching that activates units
- **.timer** -- calendar/monotonic scheduled activation (cron replacement)
- **.slice** -- cgroup resource management hierarchy nodes
- **.scope** -- externally created process groups (e.g. user sessions, VMs)


## Service Supervision Model

### Service Types
- **Type=simple** -- main process is the service, ready immediately on exec
- **Type=exec** -- like simple but ready after exec() succeeds (not just fork)
- **Type=forking** -- traditional daemon that forks and parents exits; uses PIDFile or GuessMainPID
- **Type=oneshot** -- runs to completion then exits; often paired with RemainAfterExit
- **Type=dbus** -- ready when BusName appears on the bus
- **Type=notify** -- ready when service sends sd_notify(READY=1)
- **Type=notify-reload** -- like notify but also handles reload via sd_notify
- **Type=idle** -- like simple but delays until all jobs dispatched

### Restart Policies
- **Restart=no** -- never restart
- **Restart=always** -- restart regardless of exit reason
- **Restart=on-success** -- restart only on clean exit (code 0 or SIGHUP/SIGINT/SIGTERM/SIGPIPE + SuccessExitStatus)
- **Restart=on-failure** -- restart on unclean exit, signal, timeout, or watchdog
- **Restart=on-abnormal** -- restart on signal, timeout, or watchdog (not exit code)
- **Restart=on-abort** -- restart only on uncaught signal
- **Restart=on-watchdog** -- restart only on watchdog timeout
- **RestartSec=** -- delay between restart attempts
- **RestartSteps=** / **RestartMaxDelaySec=** -- graduated restart backoff (v254+)
- **StartLimitIntervalSec=** / **StartLimitBurst=** -- crash-loop detection
- **StartLimitAction=** -- action on crash-loop (none, reboot, reboot-force, reboot-immediate, poweroff, exit)

### Lifecycle Commands
- **ExecStartPre=** -- commands run before the main process
- **ExecStart=** -- main process command line
- **ExecStartPost=** -- commands run after main process starts
- **ExecReload=** -- command for reload (sent via systemctl reload)
- **ExecStop=** -- command for graceful stop
- **ExecStopPost=** -- commands run after stop (always, even on failure)
- **ExecCondition=** -- conditions checked before ExecStartPre (v247+)

### Kill Behavior
- **KillSignal=** -- signal sent to main process on stop (default SIGTERM)
- **KillMode=control-group** -- kill entire cgroup (default)
- **KillMode=mixed** -- SIGTERM to main, SIGKILL to remaining
- **KillMode=process** -- kill main process only
- **KillMode=none** -- only run ExecStop, send nothing
- **FinalKillSignal=** -- signal after TimeoutStopSec (default SIGKILL)
- **SendSIGHUP=** -- send SIGHUP after SIGTERM
- **SendSIGKILL=** -- whether to escalate to SIGKILL (default yes)
- **TimeoutStartSec=** / **TimeoutStopSec=** / **TimeoutSec=** -- start/stop timeouts
- **TimeoutAbortSec=** -- timeout specifically for watchdog abort
- **WatchdogSec=** -- service must call sd_notify(WATCHDOG=1) within this interval

### Dependencies and Ordering
- **Requires=** -- hard dependency; if required unit fails, this unit fails too
- **Requisite=** -- like Requires but must already be active (not started on demand)
- **Wants=** -- weak dependency; failure of wanted unit is ignored
- **BindsTo=** -- like Requires but also stops when bound unit stops
- **PartOf=** -- subordinate: stopped/restarted when parent is
- **Upholds=** -- persistent restart trigger (v249+); if upheld unit stops, restart it
- **Conflicts=** -- mutual exclusion; starting one stops the other
- **Before=** / **After=** -- ordering (orthogonal to dependency strength)
- **OnSuccess=** / **OnFailure=** -- trigger units on success/failure
- **PropagatesReloadTo=** / **ReloadPropagatedFrom=** -- reload cascading
- **PropagatesStopTo=** / **StopPropagatedFrom=** -- stop cascading (v249+)
- **JoinsNamespaceOf=** -- share namespaces between units

### Conditions and Asserts
Skip or fail unit activation based on system state:
- **ConditionPathExists=** / **ConditionPathIsDirectory=** / **ConditionPathIsMountPoint=**
- **ConditionFileNotEmpty=** / **ConditionFileIsExecutable=**
- **ConditionDirectoryNotEmpty=**
- **ConditionKernelCommandLine=** / **ConditionKernelVersion=**
- **ConditionArchitecture=** / **ConditionVirtualization=** / **ConditionHost=**
- **ConditionSecurity=** (selinux, apparmor, etc.)
- **ConditionCapability=** / **ConditionACPower=** / **ConditionMemory=** / **ConditionCPUs=**
- **ConditionUser=** / **ConditionGroup=** / **ConditionControlGroupController=**
- **ConditionEnvironment=** / **ConditionFirmware=**
- Assert variants are identical but cause hard failure instead of skip.

### Socket Activation
- **ListenStream=** / **ListenDatagram=** / **ListenSequentialPacket=** -- TCP/UDP/Unix sockets
- **ListenFIFO=** / **ListenSpecial=** / **ListenNetlink=** / **ListenMessageQueue=** / **ListenUSBFunction=**
- **Accept=** -- per-connection instance spawning (inetd-style)
- **MaxConnections=** / **MaxConnectionsPerSource=**
- **SocketUser=** / **SocketGroup=** / **SocketMode=** -- socket permissions
- **PassCredentials=** / **PassSecurity=** / **PassPacketInfo=**
- **BindIPv6Only=** / **BindToDevice=** / **FreeBind=** / **Transparent=**
- **Backlog=** / **KeepAlive=** / **Priority=** / **ReceiveBuffer=** / **SendBuffer=**
- **TriggerLimitIntervalSec=** / **TriggerLimitBurst=** -- rate limiting
- File descriptor store: **FileDescriptorStoreMax=** on the service side

### Timer Activation
- **OnActiveSec=** / **OnBootSec=** / **OnStartupSec=** -- monotonic timers
- **OnUnitActiveSec=** / **OnUnitInactiveSec=** -- relative to associated unit
- **OnCalendar=** -- calendar expression scheduling (cron-like but more expressive)
- **AccuracySec=** -- timer coalescing window (power-saving)
- **RandomizedDelaySec=** -- jitter to spread load
- **FixedRandomDelay=** -- stable jitter per machine (v247+)
- **Persistent=** -- catch up missed runs after downtime
- **WakeSystem=** -- wake system from suspend to fire timer
- **RemainAfterElapse=** -- keep timer unit active after last trigger

### Path Activation
- **PathExists=** / **PathExistsGlob=** -- activate when path appears
- **PathChanged=** / **PathModified=** -- activate on modification
- **DirectoryNotEmpty=** -- activate when directory gets content
- **MakeDirectory=** / **DirectoryMode=** -- auto-create watched directories
- Uses inotify internally.

### D-Bus Activation
- **Type=dbus** with **BusName=** on the service
- Bus activation files in /usr/share/dbus-1/system-services/
- Service auto-started when D-Bus name is requested

### Targets (Grouping / Synchronization)
Standard targets act as synchronization barriers:
- **default.target** -- default boot target (usually symlink to multi-user or graphical)
- **sysinit.target** -- early system initialization complete
- **basic.target** -- basic system ready (sockets, timers, paths all started)
- **multi-user.target** -- full multi-user system (like runlevel 3)
- **graphical.target** -- graphical login (like runlevel 5)
- **rescue.target** -- single-user rescue (like runlevel 1)
- **emergency.target** -- minimal emergency shell (root fs only)
- **network-online.target** -- network fully configured
- **remote-fs.target** -- remote filesystems mounted
- **sound.target** / **bluetooth.target** / **printer.target** -- hardware subsystem targets
- **sleep.target** / **suspend.target** / **hibernate.target** -- power state transitions
- **shutdown.target** / **poweroff.target** / **reboot.target** / **halt.target** / **kexec.target**
- **initrd.target** / **initrd-root-fs.target** -- initramfs-specific targets
- Custom targets are just empty units that other units can order against.

### Template and Instantiated Units
- **unit@.service** -- template definition (contains %i specifier)
- **unit@instance.service** -- instantiated from template
- **DefaultInstance=** -- default instance name in [Install] section
- Specifiers: %i (instance), %I (unescaped instance), %n (full name), %N (unescaped name), %p (prefix), %u (user), %U (UID), %h (home), %s (shell), %m (machine-id), %b (boot-id), %H (hostname), %t (runtime dir), %S (state dir), %C (cache dir), %L (log dir), and more

### Preset System
- **.preset** files define default enable/disable policy for units
- **systemctl preset** -- apply presets to a unit
- **systemctl preset-all** -- apply presets to all installed units
- Distribution vendors ship presets in /lib/systemd/system-preset/
- Admin overrides in /etc/systemd/system-preset/

### Drop-in Overrides
- Unit directories: /etc/systemd/system/unit.d/*.conf
- Merge with base unit; can override individual directives
- **systemctl edit** -- create drop-ins interactively
- Precedence: /etc overrides /run overrides /lib

### Install Section
- **WantedBy=** / **RequiredBy=** -- create symlinks on enable
- **Also=** -- enable/disable additional units together
- **Alias=** -- alternative names for the unit
- **DefaultInstance=** -- default instance for templates


## Logging and Journal

- **systemd-journald** -- structured binary logging daemon; captures stdout/stderr, syslog, kernel, audit
- **journalctl** -- query tool with filtering by unit, priority, time range, boot, field matching
- **systemd-cat** -- pipe arbitrary command output into the journal
- **systemd-journal-remote** -- receive journal entries over HTTPS
- **systemd-journal-upload** -- push journal entries to remote
- **systemd-journal-gatewayd** -- HTTP/SSE gateway for journal queries
- Journal features: forward-secure sealing (FSS), rate limiting, field indexing, automatic rotation, disk usage caps, volatile/persistent/auto storage


## Login and Session Management

- **systemd-logind** -- login/seat/session management daemon
- **loginctl** -- CLI for sessions, seats, users
- **pam_systemd** -- PAM module that registers sessions with logind
- **user@.service** -- per-user systemd instance (systemd --user)
- **user-runtime-dir@.service** -- manages /run/user/UID
- Seat management, multi-seat support, VT switching, idle tracking
- Inhibitor locks (prevent suspend/shutdown/idle during critical operations)
- **loginctl enable-linger** -- allow user services without active login session


## Networking

- **systemd-networkd** -- network configuration manager (link, bridge, bond, vlan, vxlan, wireguard, tunnel)
- **networkctl** -- CLI for networkd
- **systemd-resolved** -- stub resolver, DNS cache, DNSSEC validation, DNS-over-TLS, LLMNR, mDNS
- **resolvectl** -- CLI for resolved (query, flush-caches, statistics, per-link DNS config)
- **nss-resolve** / **nss-myhostname** / **nss-mymachines** -- NSS modules
- **systemd-timesyncd** -- simple SNTP client
- **timedatectl** -- CLI for time/date/timezone/NTP


## Boot and UEFI

- **systemd-boot** (sd-boot) -- minimal UEFI boot manager; Boot Loader Specification entries
- **bootctl** -- install/update/manage boot loader entries
- **systemd-stub** (sd-stub) -- UEFI stub for Unified Kernel Images (UKI)
- **ukify** -- build UKIs from kernel + initrd + cmdline + stub
- **systemd-measure** -- pre-calculate TPM2 PCR values for UKI components
- **systemd-pcrextend** -- extend TPM2 PCRs at runtime
- **systemd-pcrlock** -- manage TPM2 PCR policies (bind LUKS to measured boot, v255+)
- **systemd-bsod** -- blue screen showing emergency log messages on boot failure (v255+)
- **systemd-boot-check-no-failures** -- verify previous boot succeeded
- **systemd-soft-reboot** -- restart userspace without kernel reboot (preserve /run state)


## Boot Initialization Services

Early-boot one-shots:
- **systemd-modules-load** -- load kernel modules from modules-load.d
- **systemd-sysctl** -- apply kernel parameters from sysctl.d
- **systemd-binfmt** -- register binary formats for misc executables
- **systemd-tmpfiles** -- create/delete/clean volatile and temporary files/dirs (tmpfiles.d)
- **systemd-sysusers** -- create system users/groups from sysusers.d
- **systemd-firstboot** -- initialize locale, timezone, hostname, root password on first boot
- **systemd-random-seed** -- load/save random seed across boots
- **systemd-update-utmp** -- update utmp/wtmp records
- **systemd-remount-fs** -- remount root and API filesystems
- **systemd-fsck** / **systemd-quotacheck** -- filesystem check and quota check
- **systemd-backlight** / **systemd-rfkill** -- save/restore backlight and RF kill states
- **systemd-machine-id-setup** -- initialize /etc/machine-id
- **systemd-hwdb** -- compile and query hardware database


## Generators

Generators dynamically create units at boot/reload:
- **systemd-fstab-generator** -- units from /etc/fstab
- **systemd-gpt-auto-generator** -- auto-discover GPT partitions (Discoverable Partitions Specification)
- **systemd-cryptsetup-generator** -- units for encrypted volumes from /etc/crypttab
- **systemd-veritysetup-generator** -- units for dm-verity volumes
- **systemd-hibernate-resume-generator** -- resume-from-hibernation unit
- **systemd-system-update-generator** -- redirect to system-update.target for offline updates
- **systemd-getty-generator** -- spawn getty on active consoles
- **systemd-debug-generator** -- enable debug shell from kernel cmdline
- **systemd-rc-local-generator** -- compatibility for /etc/rc.local
- Custom generators: any executable in generator directories


## Device Management (udev)

- **systemd-udevd** -- device manager daemon; processes kernel uevents, applies rules, creates /dev nodes
- **udevadm** -- CLI (info, trigger, settle, control, test, monitor)
- **hwdb** -- hardware database for device properties
- udev rules: match on device attributes, set properties, run programs, create symlinks, set permissions, tag devices for systemd units


## Filesystem and Storage

- **.mount** / **.automount** units -- generated from fstab or declared
- **systemd-mount** / **systemd-umount** -- create transient mount points from CLI
- **systemd-makefs** -- create filesystems on devices (x-systemd.makefs fstab option)
- **systemd-growfs** -- grow filesystem to fill partition (x-systemd.growfs fstab option)
- **systemd-cryptsetup** -- set up LUKS/dm-crypt volumes
- **systemd-cryptenroll** -- enroll LUKS2 tokens: password, FIDO2, TPM2, PKCS#11, recovery key
- **systemd-veritysetup** -- set up dm-verity integrity-checked volumes
- **systemd-integritysetup** -- set up dm-integrity volumes
- **systemd-repart** -- declarative GPT partition management (create/grow/remove partitions)
- **systemd-dissect** -- inspect Discoverable Disk Images (DDI)
- **systemd-vpick** -- resolve .v/ versioned resource directories (v256+)


## Container and VM Management

- **systemd-nspawn** -- lightweight OS container (like chroot with namespaces, seccomp, cgroups)
- **systemd-machined** -- registration daemon for containers and VMs
- **machinectl** -- CLI (start, stop, login, shell, copy-to, copy-from, bind, image management)
- **systemd-vmspawn** -- spawn VMs via QEMU with ergonomic defaults (v256+)
- **systemd-importd** / **importctl** -- download/import/export disk images (raw, tar, qcow2)
- **systemd-portabled** / **portablectl** -- attach portable service images (self-contained /usr trees)
- **systemd-sysext** -- extend /usr and /opt with overlay images (composable OS)
- **systemd-confext** -- extend /etc with overlay images


## Home Directory Management

- **systemd-homed** -- manage home directories with built-in encryption (LUKS), quotas, portability
- **homectl** -- create/remove/resize/lock/unlock home directories
- **pam_systemd_home** -- PAM module for homed authentication
- Backends: directory, subvolume (btrfs), LUKS on file/partition, fscrypt, CIFS


## User and Group Management

- **systemd-sysusers** -- declarative system user/group creation from sysusers.d
- **systemd-userdb** -- Varlink-based user/group record multiplexer
- **userdbctl** -- query users/groups from all backends (NSS, homed, machined, etc.)
- **nss-systemd** -- NSS module bridging DynamicUser allocations and systemd-homed users


## System Updates

- **systemd-sysupdate** / **systemd-sysupdated** -- A/B-style atomic updates for partitions, filesystem trees, and DDIs
- **systemd-system-update-generator** -- boot into system-update.target for offline update application


## Resource Control (cgroups v2)

Per-unit cgroup knobs:
- **CPUWeight=** / **CPUQuota=** / **AllowedCPUs=** -- CPU scheduling
- **MemoryMin=** / **MemoryLow=** / **MemoryHigh=** / **MemoryMax=** / **MemorySwapMax=** -- memory limits and protection
- **IOWeight=** / **IOReadBandwidthMax=** / **IOWriteBandwidthMax=** / **IOReadIOPSMax=** / **IOWriteIOPSMax=** -- I/O control
- **TasksMax=** -- limit number of tasks (processes + threads)
- **IPAddressAllow=** / **IPAddressDeny=** -- IP-level access control via BPF
- **IPAccounting=** -- track bytes/packets in/out
- **DeviceAllow=** / **DevicePolicy=** -- device access control
- **ManagedOOMSwap=** / **ManagedOOMMemoryPressure=** -- oomd integration
- **Delegate=** -- delegate cgroup subtree to service (for container managers)
- **CPUAccounting=** / **MemoryAccounting=** / **IOAccounting=** -- enable accounting

### Monitoring
- **systemd-cgls** -- display cgroup tree with processes
- **systemd-cgtop** -- real-time cgroup resource usage (like top)


## Security and Sandboxing

### Credentials
- **systemd-creds** -- encrypt/decrypt service credentials (AES-256-GCM)
- **LoadCredential=** / **LoadCredentialEncrypted=** -- load credentials from files
- **SetCredential=** / **SetCredentialEncrypted=** -- inline credentials in unit files
- **ImportCredential=** -- import from system credentials directory
- TPM2 binding: credentials sealed to hardware; unlocked only on same machine

### Sandboxing Directives
- **ProtectSystem=** (true, full, strict) -- make /usr, /boot, /efi, (optionally entire /) read-only
- **ProtectHome=** (true, read-only, tmpfs) -- hide or protect /home, /root, /run/user
- **PrivateTmp=** -- isolated /tmp and /var/tmp
- **PrivateDevices=** -- minimal /dev (no physical devices)
- **PrivateNetwork=** -- isolated network namespace
- **PrivateUsers=** -- user namespace with mapped uid/gid
- **PrivateIPC=** -- isolated IPC namespace (v248+)
- **PrivateMounts=** -- isolated mount namespace
- **PrivatePIDs=** -- isolated PID namespace (v256+)
- **ProtectKernelTunables=** -- read-only /proc/sys, /sys
- **ProtectKernelModules=** -- deny module loading
- **ProtectKernelLogs=** -- deny /dev/kmsg access
- **ProtectControlGroups=** -- read-only /sys/fs/cgroup
- **ProtectClock=** -- deny clock adjustment
- **ProtectHostname=** -- deny hostname changes
- **ProtectProc=** (invisible, ptraceable, noaccess, default) -- hide other processes (v247+)
- **ProcSubset=** (all, pid) -- restrict /proc contents (v247+)
- **NoNewPrivileges=** -- prevent privilege escalation
- **RestrictNamespaces=** -- limit namespace creation
- **RestrictRealtime=** -- deny realtime scheduling
- **RestrictSUIDSGID=** -- deny SUID/SGID file creation
- **RestrictAddressFamilies=** -- limit socket address families
- **RestrictFileSystems=** -- limit filesystem types (v248+)
- **LockPersonality=** -- lock execution domain
- **MemoryDenyWriteExecute=** -- deny W+X memory mappings
- **SystemCallFilter=** -- seccomp syscall whitelist/blacklist
- **SystemCallArchitectures=** -- restrict to specific architectures (e.g. native only)
- **SystemCallErrorNumber=** -- return error instead of killing on denied syscall
- **DynamicUser=** -- allocate ephemeral uid/gid per invocation

### Filesystem Restrictions
- **ReadOnlyPaths=** / **ReadWritePaths=** / **InaccessiblePaths=** -- per-path access control
- **BindPaths=** / **BindReadOnlyPaths=** -- bind mounts into namespace
- **TemporaryFileSystem=** -- mount tmpfs at specified paths
- **RootDirectory=** / **RootImage=** -- chroot or pivot-root to image
- **MountImages=** / **ExtensionImages=** -- mount additional images
- **StateDirectory=** / **RuntimeDirectory=** / **CacheDirectory=** / **LogsDirectory=** / **ConfigurationDirectory=** -- managed directories with correct ownership
- **UMask=** -- file creation mask

### Capabilities
- **CapabilityBoundingSet=** -- limit capabilities
- **AmbientCapabilities=** -- retain capabilities across exec
- **SecureBits=** -- control secure-bits for capability handling

### Security Analysis
- **systemd-analyze security** -- audit sandboxing of all units, compute exposure score


## OOM Handling

- **systemd-oomd** -- userspace OOM killer using PSI (Pressure Stall Information) metrics
- **oomctl** -- show oomd status and cgroup contexts
- Kills based on memory pressure, not just usage; more intelligent than kernel OOM


## Core Dump Handling

- **systemd-coredump** -- capture, compress, and store core dumps
- **coredumpctl** -- list, info, dump, debug (auto-launch gdb on stored core)
- Storage in journal and/or /var/lib/systemd/coredump/


## Power Management

- **systemd-logind** handles idle tracking, lid switch, power button actions
- **systemd-sleep** -- suspend, hibernate, hybrid-sleep, suspend-then-hibernate
- **systemd-inhibit** -- take inhibitor locks from CLI (prevent sleep/shutdown during operations)
- **systemd-soft-reboot** -- fast reboot without kernel restart (preserve /run, pass file descriptors)
- Shutdown commands: halt, poweroff, reboot, kexec (all wired through systemctl)


## Hostname, Locale, Machine Identity

- **systemd-hostnamed** / **hostnamectl** -- static, pretty, transient hostname; chassis type, deployment, icon name
- **systemd-localed** / **localectl** -- locale, X11 keymap, console keymap
- **systemd-id128** -- generate, print, and compare 128-bit IDs; app-specific IDs


## D-Bus Integration

- **busctl** -- D-Bus introspection, monitoring, method calls, property access
- Over 14 D-Bus APIs: systemd1, login1, hostname1, locale1, timedate1, machine1, network1, resolve1, home1, import1, portable1, oom1, sysupdate1, timesync1
- D-Bus activation: services auto-started when their bus name is requested


## Varlink Protocol

- IPC protocol replacing some D-Bus interfaces; 19+ Varlink interfaces as of v257
- **systemd-mountfsd** -- DDI mounting via Varlink
- **systemd-nsresourced** -- user namespace resource delegation via Varlink
- Used by resolved, homed, userdb, and more


## Utility Tools

- **systemd-run** -- run commands in transient units (services, scopes, timers)
- **systemd-escape** -- escape strings for unit names
- **systemd-notify** -- send sd_notify messages from shell scripts
- **systemd-ask-password** / **systemd-tty-ask-password-agent** -- password prompting framework
- **systemd-detect-virt** -- detect VM/container environment
- **systemd-path** -- query standard system and user paths
- **systemd-delta** -- find overridden/masked/extended configuration files
- **systemd-socket-activate** -- test socket activation from CLI
- **systemd-socket-proxyd** -- proxy connections between sockets

### Newer Utilities
- **run0** -- sudo replacement using systemd-run and PTY transport (v256+)
- **systemd-keyutil** -- TPM key operations (v257+)
- **systemd-bsod** -- blue screen on emergency failure (v255+)


## Developer Libraries (libsystemd)

- **sd-bus** -- D-Bus protocol library
- **sd-event** -- event loop (I/O, timers, signals, child processes, deferred)
- **sd-daemon** -- sd_notify, sd_listen_fds, sd_is_socket, sd_watchdog_enabled
- **sd-journal** -- journal API (read, write, follow, filter, seek)
- **sd-device** -- device enumeration and monitoring
- **sd-login** -- session/seat/user information
- **sd-id128** -- 128-bit ID operations
- **sd-hwdb** -- hardware database queries
- **sd-path** -- lookup standard directories
- **sd-varlink** -- Varlink IPC client/server
- **sd-json** -- JSON handling
- **libudev** -- device management (legacy, prefer sd-device)


## Configuration Files

System-wide:
- /etc/systemd/system.conf, user.conf, journald.conf, logind.conf, resolved.conf, networkd.conf, timesyncd.conf, sleep.conf, homed.conf, coredump.conf, oomd.conf

Drop-in directories:
- environment.d, sysctl.d, tmpfiles.d, sysusers.d, modules-load.d, binfmt.d


---


## Comparison Table: systemd vs runit vs s6 vs supervisor.el

Where behavior diverges, parenthetical notes explain.

| Feature | systemd | runit | s6 | supervisor.el |
|---|---|---|---|---|
| **Init System (PID 1)** | | | | |
| Can run as PID 1 | yes (systemd is PID 1) | yes (runit-init, 3-stage boot) | yes (s6-linux-init, separate package) | no (needs sinit-style shim as PID 1, runs as PID 2) |
| **Process Supervision** | | | | |
| Restart crashed daemons | yes | yes | yes | yes |
| Restart policies (always/on-failure/on-success/no) | yes (6 modes) | no (always or never) | no (always or never) | yes (4 modes) |
| Restart backoff / graduated delay | yes (v254 RestartSteps) | no | no | yes (restart-sec per unit) |
| Crash-loop detection | yes (StartLimitBurst) | no | no | yes (supervisor--failed) |
| Configurable kill signal | yes (KillSignal) | yes (control/ scripts) | yes (down-signal file) | yes (kill-signal) |
| Kill mode (cgroup/process/mixed) | yes (KillMode) | no (process only) | no (process only) | yes (kill-mode, process/mixed) |
| Exec-stop custom shutdown | yes (ExecStop) | yes (finish script) | yes (finish script) | yes (exec-stop) |
| Exec-reload custom reload | yes (ExecReload) | no | no | yes (exec-reload) |
| **Service Types** | | | | |
| Long-running daemons | yes (Type=simple) | yes | yes (longruns) | yes (type simple) |
| Oneshot run-to-completion | yes (Type=oneshot) | partial (sv once, no dedicated type) | yes (s6-rc oneshot) | yes (type oneshot) |
| Forking/double-fork PID tracking | yes (Type=forking) | no | no | no |
| D-Bus readiness type | yes (Type=dbus) | no | no | no |
| Notify readiness protocol | yes (Type=notify, sd_notify) | no | yes (notification-fd, no library needed) | no |
| Remain-after-exit | yes (RemainAfterExit) | no | no | yes (remain-after-exit) |
| Success-exit-status override | yes (SuccessExitStatus) | no | no | yes (success-exit-status) |
| **Dependencies and Ordering** | | | | |
| Explicit dependency declarations | yes (Requires/Wants/BindsTo) | no | yes (s6-rc dependencies) | yes (requires/wants) |
| Ordering declarations | yes (Before/After) | no (manual in run scripts) | yes (s6-rc ordering) | yes (before/after) |
| Topological sort with cycle detection | yes | no | yes | yes (DAG with cycle fallback) |
| Parallel startup respecting ordering | yes | yes (all parallel, no ordering) | yes | yes (DAG in-degree scheduling) |
| Conflict declarations | yes (Conflicts) | no | no | no |
| Conditional activation | yes (ConditionXxx) | no | no | no |
| **Staged / Phased Startup** | | | | |
| Named synchronization barriers | yes (targets) | partial (runlevels as dirs) | yes (s6-rc bundles) | yes (4 stages) |
| Runlevel/target switching at runtime | yes (isolate) | yes (runsvchdir) | yes (s6-rc -u/-d bundles) | no (stages are boot-time only) |
| **Activation Mechanisms** | | | | |
| Socket activation | yes (centralized, fd passing) | no | partial (s6-ipcserver, s6-tcpserver, composable not centralized) | no |
| Timer activation | yes (OnCalendar, monotonic) | no | no | yes (on-calendar, on-startup-sec, on-unit-active-sec) |
| Path activation (inotify) | yes (.path units) | no | no (ftrig is FIFO-based, not inotify) | no |
| D-Bus activation | yes | no | no | no |
| Device activation | yes (.device from udev) | no | no | no |
| **Logging** | | | | |
| Integrated logging daemon | yes (journald, structured binary) | yes (svlogd, plain text) | yes (s6-log, plain text) | yes (supervisor-logd, plain text) |
| Per-service log capture | yes (journal tags by unit) | yes (service/log/run pipe) | yes (servicedir/log pipe) | yes (per-id log files) |
| Log rotation | yes (journal size/time) | yes (svlogd built-in) | yes (s6-log built-in) | yes (supervisor-logrotate) |
| Log pruning | yes (journald vacuum) | no (manual) | no (manual) | yes (supervisor-log-prune) |
| Structured/indexed log queries | yes (journalctl field filtering) | no | no | no |
| Network log shipping | yes (journal-remote/upload) | partial (svlogd UDP) | no | no |
| **Resource Control** | | | | |
| Cgroup integration | yes (native, per-unit cgroup) | no | no | no |
| CPU/memory/IO limits | yes (CPUQuota, MemoryMax, etc.) | no | no | no |
| Task limits | yes (TasksMax) | no | no | no |
| Resource accounting | yes (CPUAccounting, etc.) | no | no | no |
| Resource limits (ulimit-style) | yes (LimitNOFILE, etc.) | yes (chpst) | yes (s6-softlimit) | no |
| **Sandboxing** | | | | |
| Namespace isolation | yes (PrivateTmp, PrivateNetwork, etc.) | no | no | no |
| Seccomp syscall filtering | yes (SystemCallFilter) | no | no | no |
| Capability restriction | yes (CapabilityBoundingSet) | no | no | no |
| Filesystem protection | yes (ProtectSystem, ReadOnlyPaths) | no | no | no |
| Dynamic user allocation | yes (DynamicUser) | no | no | no |
| Security audit scoring | yes (systemd-analyze security) | no | no | no |
| **Environment and Execution Context** | | | | |
| Environment variables | yes (Environment, EnvironmentFile) | partial (chpst -e envdir) | yes (s6-envdir) | yes (environment, environment-file) |
| Working directory | yes (WorkingDirectory) | yes (set in run script) | yes (set in run script) | yes (working-directory) |
| User/group execution | yes (User, Group) | yes (chpst -u) | yes (s6-setuidgid) | yes (user, group fields) |
| Supplementary groups | yes (SupplementaryGroups) | yes (chpst -u uid:gid:gid) | yes (s6-applyuidgid) | no |
| **Enable / Disable / Mask** | | | | |
| Enable/disable services | yes (systemctl enable/disable) | yes (symlink in/out of rundir) | yes (s6-rc enable/disable) | yes (enabled/disabled, runtime overrides) |
| Mask (force-prevent start) | yes (systemctl mask) | no | no | yes (mask-override) |
| Persistent runtime overrides | partial (enable/disable persist, runtime overrides do not) | yes (symlinks persist) | yes (compiled db persists) | yes (overrides.eld atomic file) |
| Preset defaults | yes (systemd.preset files) | no | no | no |
| Drop-in override fragments | yes (unit.d/*.conf) | no | no | no |
| **Template / Instance Services** | | | | |
| Template units with instances | yes (unit@.service, %i specifier) | no | yes (s6-instance-maker) | no |
| **Watchdog** | | | | |
| Service heartbeat watchdog | yes (WatchdogSec, sd_notify WATCHDOG=1) | no | no | no |
| Hardware watchdog | yes (RuntimeWatchdogSec) | no | no | no |
| **UI and Observability** | | | | |
| Interactive dashboard | no (third-party cockpit) | no | no | yes (supervisor-dashboard-mode) |
| CLI status/control | yes (systemctl) | yes (sv) | yes (s6-rc, s6-svc) | yes (supervisorctl) |
| JSON output mode | yes (systemctl --output=json) | no | no | yes (supervisorctl --json) |
| Telemetry (uptime, restarts, RSS, CPU) | partial (systemctl show, cgroup accounting) | no | no | yes (built-in telemetry model) |
| **Boot Loader** | | | | |
| UEFI boot manager | yes (systemd-boot) | no | no | no |
| Unified Kernel Images | yes (ukify, sd-stub) | no | no | no |
| TPM2 measured boot | yes (pcrlock, pcrextend, measure) | no | no | no |
| **Networking** | | | | |
| Network configuration | yes (networkd) | no | no | no |
| DNS resolver/cache | yes (resolved) | no | no | no |
| NTP client | yes (timesyncd) | no | no | no |
| **Container / VM Management** | | | | |
| OS containers | yes (nspawn) | no | no | no |
| VM spawning | yes (vmspawn) | no | no | no |
| Machine registration | yes (machined) | no | no | no |
| Portable services | yes (portabled) | no | no | no |
| System extensions | yes (sysext, confext) | no | no | no |
| **Home / User / Identity** | | | | |
| Encrypted home dirs | yes (homed) | no | no | no |
| Declarative system users | yes (sysusers) | no | no | no |
| User record multiplexer | yes (userdb) | no | no | no |
| Hostname/locale daemons | yes (hostnamed, localed) | no | no | no |
| **Storage / Filesystem** | | | | |
| LUKS/dm-crypt setup | yes (cryptsetup, cryptenroll) | no | no | no |
| dm-verity/dm-integrity | yes (veritysetup, integritysetup) | no | no | no |
| Declarative partitioning | yes (repart) | no | no | no |
| Disk image inspection | yes (dissect) | no | no | no |
| **Credentials / Secrets** | | | | |
| Encrypted service credentials | yes (systemd-creds, TPM2-sealed) | no | no | no |
| **OOM / Core Dumps** | | | | |
| Userspace OOM killer | yes (oomd, PSI-based) | no | no | no |
| Core dump capture/management | yes (coredump, coredumpctl) | no | no | no |
| **Power Management** | | | | |
| Suspend/hibernate/hybrid | yes (systemd-sleep) | no | no | no |
| Inhibitor locks | yes (systemd-inhibit) | no | no | no |
| Soft reboot (userspace only) | yes (systemd-soft-reboot) | no | no | no |
| **Misc** | | | | |
| Transient units from CLI | yes (systemd-run) | no | no | no |
| Boot performance analysis | yes (systemd-analyze blame/plot/critical-chain) | no | no | no |
| Virtualization detection | yes (systemd-detect-virt) | no | no | no |
| sudo replacement | yes (run0, v256+) | no | no | no |
| Config override delta view | yes (systemd-delta) | no | no | no |
| Unit file validation | yes (systemd-analyze verify) | no | no | yes (entry whitelist validation) |
