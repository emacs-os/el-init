# Full-featured static emacs-nox with native-compilation (~376MB)
#
# Usage: nix-build emacs-static-nox-nativecomp.nix
#
# Produces a fully statically linked Emacs binary with native-compilation
# (nativecomp/JIT) support — the maximum achievable feature set for a
# static terminal Emacs.
#
# BUILD TIME: ~45-60 minutes (GCC build dominates)
#
# This builds GCC 15 from source to obtain libgccjit.a, which is not
# shipped by any Linux distribution. The GCC build adds ~20-30 minutes.
#
# No Emacs source patches needed — static linking is achieved entirely
# through configure flags and building static library dependencies.
#
# Equivalent to PKGBUILD-static-nox-nativecomp for Arch Linux.
#
# Requirements: Nix with <nixpkgs> channel (any recent nixpkgs)
#
# Features: GnuTLS, libxml2, tree-sitter, modules, threads, inotify,
#           zlib, pdumper, ncurses, NATIVE-COMPILATION
#
# Runtime notes:
#   - tree-sitter grammars (.so) loaded via dlopen at runtime
#   - Native-compiled .eln files produced at runtime via embedded JIT
#   - Dynamic modules use dlopen (works in glibc 2.28+ static binaries)
#   - The JIT needs 'as' and 'ld' at runtime (from binutils)
#   - Binary is ~376MB (includes entire GCC compiler backend)

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

  # ── Static zstd (needed by GCC LTO) ──
  staticZstd = stdenv.mkDerivation {
    pname = "static-zstd";
    version = "1.5.7";
    src = fetchurl {
      url = "https://github.com/facebook/zstd/releases/download/v1.5.7/zstd-1.5.7.tar.gz";
      sha256 = "18vgkvh7w6zw4jn2aj1mp0yv98m4fk52ay6da0wh4pm194gyaczb";
    };
    dontConfigure = true;
    enableParallelBuilding = true;
    buildPhase = ''
      cd lib
      make -j$NIX_BUILD_CORES libzstd.a
    '';
    installPhase = ''
      mkdir -p $out/lib $out/include
      cp lib/libzstd.a $out/lib/
      cp lib/zstd.h lib/zstd_errors.h lib/zdict.h $out/include/
    '';
  };

  # ── GCC from source (for libgccjit.a) ──
  # This is the most time-consuming component (~20-30 min).
  # No distribution ships libgccjit.a — only libgccjit.so.
  staticGccJit = stdenv.mkDerivation {
    pname = "static-libgccjit";
    version = "15.1.0";
    src = fetchurl {
      url = "https://ftp.gnu.org/gnu/gcc/gcc-15.1.0/gcc-15.1.0.tar.xz";
      sha256 = "1skcy1a3wwb8k25f9l1qy11nj8b5089f05dpzzn1zw302v19xc72";
    };
    nativeBuildInputs = with pkgs; [ flex bison texinfo which perl ];
    buildInputs = [ pkgs.glibc.static pkgs.zlib.static staticZstd ];

    hardeningDisable = [ "all" ];

    # GCC's download_prerequisites fetches GMP/MPFR/MPC/ISL from GNU mirrors.
    # In Nix we can't do network access during build, so we use the versions
    # already in nixpkgs as build inputs for the configure test, but let GCC
    # build its own bundled copies (which it does by default when the dirs exist).
    postUnpack = ''
      cd $sourceRoot
      # download_prerequisites won't work in sandbox, but GCC bundles
      # the source for GMP/MPFR/MPC/ISL if we create the symlinks.
      # The tarball already includes these dirs when downloaded as full source.
      ls -d gmp* mpfr* mpc* isl* 2>/dev/null || true
      cd ..
    '';

    configurePhase = ''
      runHook preConfigure

      mkdir -p build && cd build
      ../configure \
        --prefix=$out/gcc \
        --enable-languages=c,c++,jit,lto \
        --enable-host-shared \
        --disable-bootstrap \
        --disable-multilib \
        --disable-libsanitizer \
        --disable-libvtv \
        --disable-libgomp \
        --disable-libquadmath-support \
        --disable-libssp \
        --disable-libstdcxx-pch \
        --with-system-zlib

      runHook postConfigure
    '';

    enableParallelBuilding = true;

    buildPhase = ''
      cd build
      make -j$NIX_BUILD_CORES
    '';

    installPhase = ''
      cd build
      make install

      # ── Create libgccjit.a from the GCC build tree ──
      mkdir -p $out/lib $out/include

      local _jit_tmp=$(mktemp -d)

      # Extract objects from thin archives with unique prefixes
      for archive in \
        gcc/libbackend.a \
        gcc/libcommon-target.a \
        gcc/libcommon.a \
        libcpp/libcpp.a \
        libdecnumber/libdecnumber.a \
        libbacktrace/.libs/libbacktrace.a \
        libiberty/pic/libiberty.a; do
        if [ ! -f "$archive" ]; then
          echo "WARNING: Expected archive not found: $archive"
          continue
        fi
        local _adir _aname
        _adir=$(dirname "$archive")
        _aname=$(basename "$archive" .a)

        ar t "$archive" | while read -r obj; do
          local _fullpath
          if [[ "$obj" = /* ]]; then
            _fullpath="$obj"
          else
            _fullpath="''${_adir}/''${obj}"
          fi
          if [ -f "$_fullpath" ]; then
            # Skip ggc-none.o — the JIT needs ggc-page.o from libbackend
            case "$obj" in *ggc-none*) continue ;; esac
            local _safename="''${_aname}__$(echo "$obj" | tr '/' '__')"
            cp "$_fullpath" "''${_jit_tmp}/''${_safename}"
          fi
        done
      done

      # Add standalone JIT objects
      for obj in \
        gcc/attribs.o \
        gcc/gcc.o \
        gcc/driver-i386.o \
        gcc/common/common-targhooks.o \
        gcc/jit/dummy-frontend.o \
        gcc/jit/libgccjit.o \
        gcc/jit/jit-logging.o \
        gcc/jit/jit-recording.o \
        gcc/jit/jit-playback.o \
        gcc/jit/jit-result.o \
        gcc/jit/jit-tempdir.o \
        gcc/jit/jit-builtins.o \
        gcc/jit/jit-spec.o; do
        if [ -f "$obj" ]; then
          local _dirname _basename
          _dirname=$(basename "$(dirname "$obj")")
          _basename=$(basename "$obj")
          cp "$obj" "''${_jit_tmp}/standalone__''${_dirname}__''${_basename}"
        fi
      done

      # Add text-art objects (GCC 15+ diagnostic rendering)
      for obj in gcc/text-art/*.o; do
        if [ -f "$obj" ]; then
          local _basename
          _basename=$(basename "$obj")
          cp "$obj" "''${_jit_tmp}/standalone__text-art__''${_basename}"
        fi
      done

      ar rcs $out/lib/libgccjit.a ''${_jit_tmp}/*.o
      echo "Created libgccjit.a: $(du -h $out/lib/libgccjit.a | cut -f1)"

      # Install GCC's internal GMP/MPFR/MPC/ISL static libs
      cp gmp/.libs/libgmp.a $out/lib/libgccjit_gmp.a
      cp mpfr/src/.libs/libmpfr.a $out/lib/libgccjit_mpfr.a
      cp mpc/src/.libs/libmpc.a $out/lib/libgccjit_mpc.a
      cp isl/.libs/libisl.a $out/lib/libgccjit_isl.a

      # Copy zstd
      cp ${staticZstd}/lib/libzstd.a $out/lib/

      # Copy libgccjit headers
      cp ../gcc/jit/libgccjit.h $out/include/
      cp ../gcc/jit/libgccjit++.h $out/include/ 2>/dev/null || true
    '';

    meta = with pkgs.lib; {
      description = "Static libgccjit.a built from GCC source";
      license = licenses.gpl3Plus;
    };
  };

in stdenv.mkDerivation {
  pname = "emacs-static-nox-nativecomp";
  version = "30.2";

  src = fetchurl {
    url = "https://ftp.gnu.org/gnu/emacs/emacs-30.2.tar.xz";
    sha256 = "1nggbgnns7lvxn68gzlcsgwh3bigvrbn45kh6dqia9yxlqc6zwxk";
  };

  nativeBuildInputs = with pkgs; [ autoconf automake texinfo pkg-config ];
  buildInputs = [
    staticNcurses staticGmp staticNettle staticGnutls
    staticLibxml2 staticTreeSitter staticGccJit
    pkgs.glibc.static pkgs.zlib pkgs.zlib.static
  ];

  hardeningDisable = [ "all" ];
  dontFixup = true;

  configurePhase = ''
    runHook preConfigure

    # JIT smoke test needs our GCC install on PATH
    export PATH="${staticGccJit}/gcc/bin:$PATH"

    PKG_CONFIG=false ./configure \
      --prefix=$out \
      --without-all \
      --without-x \
      --without-sound \
      --without-dbus \
      --without-libsystemd \
      --without-compress-install \
      --without-selinux \
      --without-gpm \
      --without-lcms2 \
      --with-modules \
      --with-threads \
      --with-zlib \
      --with-xml2=yes \
      --with-gnutls=yes \
      --with-tree-sitter=yes \
      --with-native-compilation \
      --with-pdumper=yes \
      --with-dumping=pdumper \
      --with-file-notification=inotify \
      CC="gcc -std=gnu17" \
      CFLAGS="-O2 -I${staticGmp}/include -I${staticNcurses}/include -I${staticNcurses}/include/ncursesw -I${staticNettle}/include -I${staticGnutls}/include -I${staticLibxml2}/include -I${staticTreeSitter}/include -I${staticGccJit}/include -I${staticGccJit}/gcc/include" \
      LDFLAGS="-static -no-pie -L${staticGmp}/lib -L${staticNcurses}/lib -L${staticNettle}/lib -L${staticGnutls}/lib -L${staticLibxml2}/lib -L${staticTreeSitter}/lib -L${staticGccJit}/lib -L${staticGccJit}/gcc/lib64 -L${staticGccJit}/gcc/lib -Wl,--allow-multiple-definition" \
      LIBS="-Wl,--start-group -lgccjit -lgccjit_isl -lgccjit_mpc -lgccjit_mpfr -lgccjit_gmp -lzstd -lstdc++ -lz -lm -ldl -lpthread -Wl,--end-group" \
      LIBGNUTLS_CFLAGS="-I${staticGnutls}/include" \
      LIBGNUTLS_LIBS="-L${staticGnutls}/lib -lgnutls -L${staticNettle}/lib -lhogweed -lnettle -L${staticGmp}/lib -lgmp" \
      LIBXML2_CFLAGS="-I${staticLibxml2}/include/libxml2" \
      LIBXML2_LIBS="-L${staticLibxml2}/lib -lxml2 -lz -lm" \
      TREE_SITTER_CFLAGS="-I${staticTreeSitter}/include" \
      TREE_SITTER_LIBS="-L${staticTreeSitter}/lib -ltree-sitter"

    runHook postConfigure
  '';

  enableParallelBuilding = true;

  buildPhase = ''
    runHook preBuild

    # Override LIBGCCJIT_LIBS at make time — configure.ac hardcodes it
    make -j$NIX_BUILD_CORES \
      LIBGCCJIT_LIBS="-Wl,--start-group -lgccjit -lgccjit_isl -lgccjit_mpc -lgccjit_mpfr -lgccjit_gmp -lzstd -lstdc++ -lz -lm -ldl -lpthread -Wl,--end-group"

    runHook postBuild
  '';

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

    # Check 5: runs and features present
    $out/bin/emacs --version
    echo "PASS: emacs --version works"
    features=$($out/bin/emacs --batch --eval '(message "%s" system-configuration-features)' 2>&1)
    echo "Features: $features"
    echo "$features" | grep -q "NATIVE_COMP" || { echo "FAIL: native-comp not enabled"; exit 1; }
    echo "$features" | grep -q "GNUTLS" || { echo "FAIL: GnuTLS not enabled"; exit 1; }
    echo "$features" | grep -q "TREE_SITTER" || { echo "FAIL: tree-sitter not enabled"; exit 1; }
    echo "$features" | grep -q "MODULES" || { echo "FAIL: modules not enabled"; exit 1; }
    echo "PASS: All expected features present (including NATIVE_COMP)"
    echo "=== All static checks passed ==="

    runHook postInstall
  '';

  meta = with pkgs.lib; {
    description = "GNU Emacs (nox) — fully static with native-compilation";
    homepage = "https://www.gnu.org/software/emacs/";
    license = licenses.gpl3Plus;
    platforms = [ "x86_64-linux" ];
    mainProgram = "emacs";
  };
}
