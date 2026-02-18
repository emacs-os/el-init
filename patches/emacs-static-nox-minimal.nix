# Minimal static emacs-nox — smallest possible binary (~10MB)
#
# Usage: nix-build emacs-static-nox-minimal.nix
#
# Produces a fully statically linked Emacs binary with only essential
# features: GMP (bignum), ncurses (terminal UI), pdumper.
#
# No Emacs source patches needed — static linking is achieved entirely
# through configure flags and building static library dependencies.
#
# Equivalent to PKGBUILD-static-nox-minimal for Arch Linux.
#
# Requirements: Nix with <nixpkgs> channel (any recent nixpkgs)
# Build time: ~5 minutes on 12 cores

{ pkgs ? import <nixpkgs> {} }:

let
  inherit (pkgs) stdenv fetchurl;

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
    # GCC 15 defaults to -std=gnu23 which breaks GMP's configure tests
    configureFlags = [ "--enable-static" "--disable-shared" ];
    preConfigure = ''
      export CC="gcc -std=gnu17"
    '';
    enableParallelBuilding = true;
    # CRITICAL: GMP installs a spurious config.h that shadows Emacs's own
    postInstall = ''
      rm -f $out/include/config.h
    '';
  };

in stdenv.mkDerivation {
  pname = "emacs-static-nox-minimal";
  version = "30.2";

  src = fetchurl {
    url = "https://ftp.gnu.org/gnu/emacs/emacs-30.2.tar.xz";
    sha256 = "1nggbgnns7lvxn68gzlcsgwh3bigvrbn45kh6dqia9yxlqc6zwxk";
  };

  nativeBuildInputs = with pkgs; [ autoconf automake texinfo pkg-config ];
  buildInputs = [ staticNcurses staticGmp pkgs.glibc.static ];

  # Nix stdenv adds PIE, FORTIFY_SOURCE, RELRO etc. which conflict with -static
  hardeningDisable = [ "all" ];
  # Prevent patchelf from trying to set RPATH/interpreter on the static binary
  dontFixup = true;

  configurePhase = ''
    runHook preConfigure

    # PKG_CONFIG=false prevents detection of libseccomp (no --without-seccomp)
    PKG_CONFIG=false ./configure \
      --prefix=$out \
      --without-all \
      --without-x \
      --without-sound \
      --without-dbus \
      --without-gnutls \
      --without-libsystemd \
      --without-modules \
      --without-tree-sitter \
      --without-compress-install \
      --without-native-compilation \
      --without-threads \
      --without-selinux \
      --without-gpm \
      --without-lcms2 \
      --with-pdumper=yes \
      --with-dumping=pdumper \
      --with-xml2=no \
      --with-file-notification=no \
      CFLAGS="-O2 -std=gnu17 -I${staticGmp}/include -I${staticNcurses}/include -I${staticNcurses}/include/ncursesw" \
      LDFLAGS="-static -no-pie -L${staticGmp}/lib -L${staticNcurses}/lib" \
      CPPFLAGS="-I${staticGmp}/include -I${staticNcurses}/include -I${staticNcurses}/include/ncursesw"

    runHook postConfigure
  '';

  enableParallelBuilding = true;

  installPhase = ''
    runHook preInstall

    make DESTDIR="" install

    # Verify static linkage (use emacs-30.2, as emacs is a symlink)
    local binary=$out/bin/emacs-30.2
    echo "=== Static linkage verification ==="

    # Check 1: file command should show "statically linked"
    file "$binary"
    file "$binary" | grep -q "statically linked" || { echo "FAIL: not statically linked"; exit 1; }
    echo "PASS: file reports statically linked"

    # Check 2: ldd should report "not a dynamic executable"
    ldd_output=$(ldd "$binary" 2>&1 || true)
    echo "ldd: $ldd_output"
    echo "$ldd_output" | grep -q "not a dynamic executable" || { echo "FAIL: ldd check failed"; exit 1; }
    echo "PASS: ldd confirms not a dynamic executable"

    # Check 3: no NEEDED entries in dynamic section
    if readelf -d "$binary" 2>/dev/null | grep -q NEEDED; then
      echo "FAIL: has NEEDED entries"
      readelf -d "$binary"
      exit 1
    fi
    echo "PASS: no NEEDED entries"

    # Check 4: no INTERP segment
    if readelf -l "$binary" 2>/dev/null | grep -q INTERP; then
      echo "FAIL: has INTERP segment"
      readelf -l "$binary"
      exit 1
    fi
    echo "PASS: no INTERP segment"

    # Check 5: runs correctly
    $out/bin/emacs --version
    echo "PASS: emacs --version works"
    echo "=== All static checks passed ==="

    runHook postInstall
  '';

  meta = with pkgs.lib; {
    description = "GNU Emacs (nox) — minimal fully static build";
    homepage = "https://www.gnu.org/software/emacs/";
    license = licenses.gpl3Plus;
    platforms = [ "x86_64-linux" ];
    mainProgram = "emacs";
  };
}
