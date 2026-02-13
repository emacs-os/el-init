TODO

make sure initramfs handles init script duties e.g. mount /proc /sys /dev/ etc etc . prior so that computer is setup
properly for the long running emacs

patch Emacs to support a true PID 1 mode (global reaping plus safe
  signal handling) so Emacs itself is PID 1. as emacs --daemon + supervisor.el manages the system services, can potentially even make an emacsclient pop open on the tty1 very early with graphical display of the supervisor.el console while services are init'ing, and perhaps just fire up the getty to tty2?

is this krazy? maybe try it in a virtual machine first.
