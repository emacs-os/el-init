# Full-featured static emacs-nox (~14MB)
#
# Usage: nix-build emacs-static-nox.nix
#
# Produces a fully statically linked Emacs binary with maximum features
# achievable through static linking: GMP, GnuTLS, libxml2, tree-sitter,
# dynamic modules, threads, inotify, zlib, pdumper, ncurses.
#
# No Emacs source patches needed — static linking is achieved entirely
# through configure flags and building static library dependencies.
#
# Equivalent to PKGBUILD-static-nox for Arch Linux.
#
# Requirements: Nix with <nixpkgs> channel (any recent nixpkgs)
# Build time: ~10 minutes on 12 cores
#
# Runtime notes:
#   - tree-sitter grammars (.so) loaded via dlopen at runtime
#   - Dynamic modules use dlopen (works in glibc 2.28+ static binaries)

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
    configureFlags = [ "--enable-static" "--disable-shared" ];
    preConfigure = ''
      export CC="gcc -std=gnu17"
    '';
    enableParallelBuilding = true;
    postInstall = ''
      rm -f $out/include/config.h
    '';
  };

  # ── Static nettle (libnettle.a + libhogweed.a) ──
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

  # ── Static libxml2 (without ICU to minimize deps) ──
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
    # tree-sitter uses a plain Makefile, no configure
    dontConfigure = true;
    enableParallelBuilding = true;
    makeFlags = [ "PREFIX=$(out)" ];
    installFlags = [ "PREFIX=$(out)" ];
  };

in stdenv.mkDerivation {
  pname = "emacs-static-nox";
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

    # ── Static linkage verification (use emacs-30.2, as emacs is a symlink) ──
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

    # Check 3: no NEEDED entries
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

    # Check 5: runs correctly and features present
    $out/bin/emacs --version
    echo "PASS: emacs --version works"
    features=$($out/bin/emacs --batch --eval '(message "%s" system-configuration-features)' 2>&1)
    echo "Features: $features"
    echo "$features" | grep -q "GNUTLS" || { echo "FAIL: GnuTLS not enabled"; exit 1; }
    echo "$features" | grep -q "TREE_SITTER" || { echo "FAIL: tree-sitter not enabled"; exit 1; }
    echo "$features" | grep -q "MODULES" || { echo "FAIL: modules not enabled"; exit 1; }
    echo "$features" | grep -q "LIBXML2" || { echo "FAIL: libxml2 not enabled"; exit 1; }
    echo "PASS: All expected features present"
    echo "=== All static checks passed ==="

    runHook postInstall
  '';

  meta = with pkgs.lib; {
    description = "GNU Emacs (nox) — fully static, full-featured";
    homepage = "https://www.gnu.org/software/emacs/";
    license = licenses.gpl3Plus;
    platforms = [ "x86_64-linux" ];
    mainProgram = "emacs";
  };
}
