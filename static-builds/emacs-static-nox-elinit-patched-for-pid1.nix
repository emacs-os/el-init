# Full-featured static emacs-nox with PID1 patches and baked-in elinit
#
# Usage: nix-build emacs-static-nox-elinit-patched-for-pid1.nix
#
# The six original variants (minimal, full, nativecomp x Arch/Nix) are
# untouched legacy variants; this file is an additive new variant.
#
# This variant extends emacs-static-nox.nix with:
#   1. PID1 Emacs patches (--pid1 flag, signal handling, child reaping, hooks)
#   2. Bundled elinit (service manager) in site-lisp
#   3. Prebuilt C helpers (elinit-logd, elinit-runas, elinit-rlimits)
#   4. Autostart bootstrap: elinit hooks into pid1-boot-hook when --pid1 is used
#
# The -patched-for-pid1 suffix is a CAPABILITY marker: the binary supports
# --pid1 mode but does NOT activate it unless --pid1 is explicitly passed.
# Normal Emacs usage (without --pid1) is completely unchanged.
#
# Disable gate: set EMACS_ELINIT_DISABLE=1 env var, or set
# elinit-pid1-autostart-disabled to t in early-init.el.
#
# Requirements: Nix with <nixpkgs> channel (any recent nixpkgs)
# Build time: ~10 minutes on 12 cores

{ pkgs ? import <nixpkgs> {} }:

let
  inherit (pkgs) stdenv fetchurl;

  # ── Elinit source (from local repo) ──
  # To build from a released version, replace with:
  #   elinitSrc = fetchFromGitHub { owner = "..."; repo = "elinit"; ... };
  elinitSrc = builtins.path {
    path = ./..;
    name = "elinit-src";
    filter = path: type:
      let
        base = builtins.baseNameOf path;
        dir = builtins.baseNameOf (builtins.dirOf path);
      in
      # Include .el files, libexec C sources, patches, and Makefiles
      (type == "regular" && (
        pkgs.lib.hasSuffix ".el" base ||
        pkgs.lib.hasSuffix ".c" base ||
        pkgs.lib.hasSuffix ".h" base ||
        pkgs.lib.hasSuffix ".patch" base ||
        base == "Makefile"
      )) ||
      # Include sbin scripts (extensionless executables)
      (type == "regular" && dir == "sbin") ||
      (type == "directory" && builtins.elem base [
        "libexec" "sbin" "static-builds" "tests"
      ]);
  };

  # ── PID1 patches (from static-builds/patches/) ──
  pid1Patches = [
    ./patches/emacs-0001-add-pid1-runtime-mode.patch
    ./patches/emacs-0002-pid1-hooks-and-signals.patch
    ./patches/emacs-0003-fix-pid1-signal-handler-overrides.patch
  ];

  # ── Static ncurses (wide-char) ──
  staticNcurses = stdenv.mkDerivation {
    pname = "static-ncurses";
    version = "6.5";
    src = fetchurl {
      url = "https://ftp.gnu.org/pub/gnu/ncurses/ncurses-6.5.tar.gz";
      sha256 = "1ihwjxkwgsqcm6jybqscc27l4mxbfsy81sgrwn2mg6ls4sy92v8k";
    };
    configureFlags = [
      "--with-normal" "--without-shared" "--without-debug"
      "--without-cxx" "--without-cxx-binding" "--enable-widec"
      "--without-ada" "--without-manpages" "--without-tests"
    ];
    enableParallelBuilding = true;
    postInstall = ''
      cd $out/lib
      ln -sf libncursesw.a libncurses.a
      ln -sf libncursesw.a libtinfo.a
      ln -sf libncursesw.a libtinfow.a
    '';
  };

  # ── Static GMP ──
  staticGmp = stdenv.mkDerivation {
    pname = "static-gmp";
    version = "6.3.0";
    src = fetchurl {
      url = "https://ftp.gnu.org/pub/gnu/gmp/gmp-6.3.0.tar.xz";
      sha256 = "1648ad1mr7c1r8lkkqshrv1jfjgfdb30plsadxhni7mq041bihm3";
    };
    nativeBuildInputs = [ pkgs.m4 ];
    configureFlags = [ "--enable-static" "--disable-shared" ];
    preConfigure = ''
      export CC="gcc -std=gnu17"
    '';
    enableParallelBuilding = true;
    postInstall = ''
      rm -f $out/include/config.h
    '';
  };

  # ── Static nettle ──
  staticNettle = stdenv.mkDerivation {
    pname = "static-nettle";
    version = "3.10.1";
    src = fetchurl {
      url = "https://ftp.gnu.org/pub/gnu/nettle/nettle-3.10.1.tar.gz";
      sha256 = "0cli5lkr7h9vxrz3j9kylnsdbw2ag6x8bpgivj06xsndq1zxvz5h";
    };
    nativeBuildInputs = [ pkgs.m4 ];
    buildInputs = [ staticGmp ];
    configureFlags = [
      "--enable-static" "--disable-shared"
      "--disable-documentation" "--disable-openssl"
    ];
    preConfigure = ''
      export CFLAGS="-O2"
      export LDFLAGS="-L${staticGmp}/lib"
      export CPPFLAGS="-I${staticGmp}/include"
    '';
    enableParallelBuilding = true;
  };

  # ── Static GnuTLS ──
  staticGnutls = stdenv.mkDerivation {
    pname = "static-gnutls";
    version = "3.8.9";
    src = fetchurl {
      url = "https://www.gnupg.org/ftp/gcrypt/gnutls/v3.8/gnutls-3.8.9.tar.xz";
      sha256 = "1v9090cbajf02cw01idfbp0cgmgjn5091ff1b96hqryi0bc17qb9";
    };
    buildInputs = [ staticGmp staticNettle ];
    nativeBuildInputs = [ pkgs.pkg-config ];
    configureFlags = [
      "--enable-static" "--disable-shared"
      "--disable-cxx" "--disable-tools" "--disable-doc"
      "--disable-libdane" "--disable-guile" "--disable-nls"
      "--without-p11-kit" "--without-idn" "--without-brotli" "--without-zstd"
      "--without-tpm" "--with-tpm2=no"
      "--with-included-unistring" "--with-included-libtasn1"
      "--disable-hardware-acceleration"
    ];
    preConfigure = ''
      export CFLAGS="-O2"
      export LDFLAGS="-L${staticGmp}/lib -L${staticNettle}/lib"
      export CPPFLAGS="-I${staticGmp}/include -I${staticNettle}/include"
      export GMP_LIBS="-L${staticGmp}/lib -lgmp"
      export GMP_CFLAGS="-I${staticGmp}/include"
      export NETTLE_LIBS="-L${staticNettle}/lib -lhogweed -lnettle -L${staticGmp}/lib -lgmp"
      export NETTLE_CFLAGS="-I${staticNettle}/include"
      export HOGWEED_LIBS="-L${staticNettle}/lib -lhogweed -lnettle -L${staticGmp}/lib -lgmp"
      export HOGWEED_CFLAGS="-I${staticNettle}/include"
    '';
    enableParallelBuilding = true;
  };

  # ── Static libxml2 ──
  staticLibxml2 = stdenv.mkDerivation {
    pname = "static-libxml2";
    version = "2.15.1";
    src = fetchurl {
      url = "https://download.gnome.org/sources/libxml2/2.15/libxml2-2.15.1.tar.xz";
      sha256 = "0k65kg1j8qmjsgpx5y0gv201sn7shgr732kvgylb9iymiz0bl260";
    };
    configureFlags = [
      "--enable-static" "--disable-shared"
      "--without-python" "--without-icu" "--without-lzma"
      "--without-readline" "--without-http"
    ];
    preConfigure = ''
      export CFLAGS="-O2"
    '';
    enableParallelBuilding = true;
  };

  # ── Static tree-sitter ──
  staticTreeSitter = stdenv.mkDerivation {
    pname = "static-tree-sitter";
    version = "0.25.6";
    src = fetchurl {
      url = "https://github.com/tree-sitter/tree-sitter/archive/refs/tags/v0.25.6.tar.gz";
      sha256 = "0z4m54v3yhxgcj92k5v4gdn1yri2zb9mqv947rayhjfqqqcxjvmc";
    };
    dontConfigure = true;
    enableParallelBuilding = true;
    makeFlags = [ "PREFIX=$(out)" ];
    installFlags = [ "PREFIX=$(out)" ];
  };

in stdenv.mkDerivation {
  pname = "emacs-static-nox-elinit-patched-for-pid1";
  version = "30.2";

  src = fetchurl {
    url = "https://ftp.gnu.org/gnu/emacs/emacs-30.2.tar.xz";
    sha256 = "1nggbgnns7lvxn68gzlcsgwh3bigvrbn45kh6dqia9yxlqc6zwxk";
  };

  nativeBuildInputs = with pkgs; [ autoconf automake texinfo pkg-config ];
  buildInputs = [
    staticNcurses staticGmp staticNettle staticGnutls
    staticLibxml2 staticTreeSitter
    pkgs.glibc.static pkgs.zlib pkgs.zlib.static
  ];

  hardeningDisable = [ "all" ];
  dontFixup = true;

  # ── Apply PID1 patches ──
  patches = pid1Patches;

  configurePhase = ''
    runHook preConfigure

    PKG_CONFIG=false ./configure \
      --prefix=$out \
      --without-all \
      --without-x \
      --without-sound \
      --without-dbus \
      --without-libsystemd \
      --without-compress-install \
      --without-native-compilation \
      --without-selinux \
      --without-gpm \
      --without-lcms2 \
      --with-modules \
      --with-threads \
      --with-zlib \
      --with-xml2=yes \
      --with-gnutls=yes \
      --with-tree-sitter=yes \
      --with-pdumper=yes \
      --with-dumping=pdumper \
      --with-file-notification=inotify \
      CFLAGS="-O2 -std=gnu17 -I${staticGmp}/include -I${staticNcurses}/include -I${staticNcurses}/include/ncursesw -I${staticNettle}/include -I${staticGnutls}/include -I${staticLibxml2}/include -I${staticTreeSitter}/include" \
      LDFLAGS="-static -no-pie -L${staticGmp}/lib -L${staticNcurses}/lib -L${staticNettle}/lib -L${staticGnutls}/lib -L${staticLibxml2}/lib -L${staticTreeSitter}/lib -Wl,--allow-multiple-definition" \
      CPPFLAGS="-I${staticGmp}/include -I${staticNcurses}/include -I${staticNcurses}/include/ncursesw -I${staticNettle}/include -I${staticGnutls}/include -I${staticLibxml2}/include -I${staticTreeSitter}/include" \
      LIBXML2_CFLAGS="-I${staticLibxml2}/include/libxml2" \
      LIBXML2_LIBS="-L${staticLibxml2}/lib -lxml2 -lz -lm" \
      LIBGNUTLS_CFLAGS="-I${staticGnutls}/include" \
      LIBGNUTLS_LIBS="-L${staticGnutls}/lib -lgnutls -L${staticNettle}/lib -lhogweed -lnettle -L${staticGmp}/lib -lgmp" \
      TREE_SITTER_CFLAGS="-I${staticTreeSitter}/include" \
      TREE_SITTER_LIBS="-L${staticTreeSitter}/lib -ltree-sitter"

    runHook postConfigure
  '';

  enableParallelBuilding = true;

  installPhase = ''
    runHook preInstall

    make DESTDIR="" install

    # ── Build elinit C helpers ──
    echo "=== Building elinit C helpers ==="
    for helper in elinit-logd elinit-runas elinit-rlimits; do
      echo "Building $helper..."
      gcc -Wall -Wextra -Werror -pedantic -std=c99 -O2 \
        -o "''${helper}" "${elinitSrc}/libexec/''${helper}.c"
    done

    # ── Install elinit runtime files ──
    local sup_dest=$out/share/emacs/site-lisp/elinit
    mkdir -p "$sup_dest/libexec"

    for el_file in \
      elinit.el elinit-core.el elinit-log.el \
      elinit-overrides.el elinit-sandbox.el elinit-libexec.el \
      elinit-units.el elinit-timer.el elinit-dashboard.el \
      elinit-cli.el; do
      cp "${elinitSrc}/$el_file" "$sup_dest/"
    done

    # ── Install prebuilt C helpers ──
    for helper in elinit-logd elinit-runas elinit-rlimits; do
      install -m 755 "''${helper}" "$sup_dest/libexec/"
    done

    # ── Install sbin scripts ──
    for script in elinitctl elinit-import elinit-log-prune elinit-logrotate; do
      if [ -f "${elinitSrc}/sbin/$script" ]; then
        install -m 755 "${elinitSrc}/sbin/$script" "$out/bin/"
      fi
    done

    # ── Install autostart bootstrap ──
    cat > "$out/share/emacs/site-lisp/site-start.el" <<SITE_START_EOF
;;; site-start.el --- Elinit PID1 autostart bootstrap  -*- lexical-binding: t -*-

;; Installed by emacs-static-nox-elinit-patched-for-pid1.
;; This file auto-loads elinit when Emacs is started with --pid1.

;; Always add elinit to load-path (available for manual use too)
(add-to-list 'load-path "$sup_dest")

;; Autostart elinit in PID1 mode (unless explicitly disabled)
(when (and (bound-and-true-p pid1-mode)
           (not (getenv "EMACS_ELINIT_DISABLE"))
           (not (bound-and-true-p elinit-pid1-autostart-disabled)))
  (require 'elinit)
  ;; Helpers are prebuilt — never attempt to compile on startup
  (setq elinit-libexec-build-on-startup 'never)
  (add-hook 'pid1-boot-hook #'elinit-start))

;;; site-start.el ends here
SITE_START_EOF

    # ── Static linkage verification (use emacs-30.2, as emacs is a symlink) ──
    local binary=$out/bin/emacs-30.2
    echo "=== Static linkage verification ==="

    file "$binary"
    file "$binary" | grep -q "statically linked" || { echo "FAIL: not statically linked"; exit 1; }
    echo "PASS: file reports statically linked"

    ldd_output=$(ldd "$binary" 2>&1 || true)
    echo "ldd: $ldd_output"
    echo "$ldd_output" | grep -q "not a dynamic executable" || { echo "FAIL: ldd check failed"; exit 1; }
    echo "PASS: ldd confirms not a dynamic executable"

    if readelf -d "$binary" 2>/dev/null | grep -q NEEDED; then
      echo "FAIL: has NEEDED entries"; exit 1
    fi
    echo "PASS: no NEEDED entries"

    if readelf -l "$binary" 2>/dev/null | grep -q INTERP; then
      echo "FAIL: has INTERP segment"; exit 1
    fi
    echo "PASS: no INTERP segment"

    # Feature verification
    $out/bin/emacs --version
    echo "PASS: emacs --version works"
    features=$($out/bin/emacs --batch --eval '(message "%s" system-configuration-features)' 2>&1)
    echo "Features: $features"
    echo "$features" | grep -q "GNUTLS" || { echo "FAIL: GnuTLS not enabled"; exit 1; }
    echo "$features" | grep -q "TREE_SITTER" || { echo "FAIL: tree-sitter not enabled"; exit 1; }
    echo "$features" | grep -q "MODULES" || { echo "FAIL: modules not enabled"; exit 1; }
    echo "$features" | grep -q "LIBXML2" || { echo "FAIL: libxml2 not enabled"; exit 1; }
    echo "PASS: All expected features present"

    # PID1 patch verification
    $out/bin/emacs --batch --eval '(unless (eq pid1-mode nil) (kill-emacs 1))' 2>&1
    echo "PASS: pid1-mode is nil without --pid1"
    $out/bin/emacs --pid1 --batch --eval '(unless (eq pid1-mode t) (kill-emacs 1))' 2>&1
    echo "PASS: pid1-mode is t with --pid1"
    $out/bin/emacs --help 2>&1 | grep -q "\-\-pid1" || { echo "FAIL: --help missing --pid1"; exit 1; }
    echo "PASS: --help shows --pid1"

    # Elinit load-path verification
    $out/bin/emacs --batch \
      --eval "(add-to-list 'load-path \"$sup_dest\")" \
      --eval '(require (quote elinit))' \
      --eval '(message "elinit loaded: %s" (featurep (quote elinit)))' 2>&1
    echo "PASS: elinit loads successfully"

    # C helper binary verification
    for helper in elinit-logd elinit-runas elinit-rlimits; do
      test -x "$sup_dest/libexec/$helper" || { echo "FAIL: $helper not executable"; exit 1; }
      echo "PASS: $helper is executable"
    done

    # A2/A3 autostart verification (site-start.el is already installed above)
    local site_lisp=$out/share/emacs/site-lisp
    echo "=== A2/A3 autostart verification ==="
    # A2 acceptance #1 / A3 acceptance #3: validate autostart wiring and execution.
    # pid1-boot-hook does not fire in --batch mode (fires in normal-top-level),
    # so we validate: (1) elinit loaded, (2) elinit-start registered on hook,
    # (3) running the hook executes elinit-start without error.
    # Actual startup-path hook timing is validated by patch-level tests
    # (see patches/README-pid1-validation.md test #6).
    $out/bin/emacs --pid1 --batch \
      --eval "(add-to-list 'load-path \"$sup_dest\")" \
      --eval "(load \"$site_lisp/site-start\" nil t)" \
      --eval '(unless (featurep (quote elinit)) (kill-emacs 1))' \
      --eval '(unless (member (function elinit-start) pid1-boot-hook) (kill-emacs 1))' \
      --eval '(run-hooks (quote pid1-boot-hook))' 2>&1
    echo "PASS: elinit-start registered on pid1-boot-hook and executes successfully"
    # Verify no-rebuild setting
    $out/bin/emacs --pid1 --batch \
      --eval "(add-to-list 'load-path \"$sup_dest\")" \
      --eval "(load \"$site_lisp/site-start\" nil t)" \
      --eval '(unless (eq elinit-libexec-build-on-startup (quote never)) (kill-emacs 1))' 2>&1
    echo "PASS: elinit-libexec-build-on-startup is never (no startup rebuild)"
    # A2 acceptance #2: without --pid1, elinit does not autostart
    $out/bin/emacs --batch \
      --eval "(add-to-list 'load-path \"$sup_dest\")" \
      --eval "(load \"$site_lisp/site-start\" nil t)" \
      --eval '(when (featurep (quote elinit)) (kill-emacs 1))' 2>&1
    echo "PASS: no autostart without --pid1"
    # A2 acceptance #3: EMACS_ELINIT_DISABLE env var gate
    EMACS_ELINIT_DISABLE=1 $out/bin/emacs --pid1 --batch \
      --eval "(add-to-list 'load-path \"$sup_dest\")" \
      --eval "(load \"$site_lisp/site-start\" nil t)" \
      --eval '(when (featurep (quote elinit)) (kill-emacs 1))' 2>&1
    echo "PASS: EMACS_ELINIT_DISABLE=1 prevents autostart"
    # A2 acceptance #4: elinit-pid1-autostart-disabled Lisp gate
    # Simulates early-init load order: variable set before site-start runs.
    # (--batch does not load early-init.el, so we use -l to replicate ordering.)
    local gate_tmpdir
    gate_tmpdir=$(mktemp -d)
    printf '(setq elinit-pid1-autostart-disabled t)\n' > "$gate_tmpdir/gate.el"
    $out/bin/emacs --pid1 --batch \
      -l "$gate_tmpdir/gate.el" \
      --eval "(add-to-list 'load-path \"$sup_dest\")" \
      --eval "(load \"$site_lisp/site-start\" nil t)" \
      --eval '(when (featurep (quote elinit)) (kill-emacs 1))' 2>&1
    rm -rf "$gate_tmpdir"
    echo "PASS: elinit-pid1-autostart-disabled prevents autostart"

    echo "=== All checks passed ==="

    runHook postInstall
  '';

  meta = with pkgs.lib; {
    description = "GNU Emacs (nox) — static, full-featured, PID1 patched, with elinit";
    homepage = "https://www.gnu.org/software/emacs/";
    license = licenses.gpl3Plus;
    platforms = [ "x86_64-linux" ];
    mainProgram = "emacs";
  };
}
