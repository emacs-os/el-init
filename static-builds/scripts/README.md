Example init scripts.

Typically this needs to be done at the initramfs level prior to the kernel, OR if there is a static kernel this can be done from pid1.

for example an sinit's config.def.h could be

```
static char *const rcinitcmd[]     = { "/lib/init/rc.boot.el", NULL };
static char *const rcrebootcmd[]   = { "/lib/init/rc.shutdown.el", "reboot", NULL };
static char *const rcpoweroffcmd[] = { "/lib/init/rc.shutdown.el", "poweroff", NULL };
```

The above should be handled by emacs --pid1 itself as part of the patched functionality though.

# TODO

-  clean these up
- figure out how to provide these and make them tinkerable
