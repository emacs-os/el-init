#!/bin/sh
# SPDX-License-Identifier: GPL-3.0-or-later
# test-neutral-packaging.sh - Verify PID1 packaging files ship no implicit
# el-init payload or autostart wiring.
#
# This is a static analysis test that greps the PKGBUILD and Nix files
# for patterns that would reintroduce baked autoloading.  It runs in the
# default CI path without requiring a package build or patched Emacs.
set -eu

cd "$(dirname "${0}")"
# shellcheck source=../../sbin/tests/testlib.sh
. ./testlib.sh

STATIC_BUILDS_DIR="$(cd .. && pwd)"
PKGBUILD="${STATIC_BUILDS_DIR}/PKGBUILD-static-nox-elinit-patched-for-pid1"
NIX="${STATIC_BUILDS_DIR}/emacs-static-nox-elinit-patched-for-pid1.nix"

test_shellcheck() {
    shellcheck -x -P tests -s sh "$(cd "${STATIC_BUILDS_DIR}/tests" && pwd)/test-neutral-packaging.sh"
}

# ── Helper: fail if a pattern is found in a file ──
assert_pattern_absent() {
    _file="${1}"
    _pattern="${2}"
    _description="${3}"
    if grep -qE "${_pattern}" "${_file}"; then
        _fail_test "${_description}: pattern '${_pattern}' found in $(basename "${_file}")"
        return 1
    fi
}

# ── PKGBUILD checks ──

test_pkgbuild_no_site_start() {
    # Match creation patterns (cat/install/cp to site-start.el), not
    # validation checks that test for its absence.
    assert_pattern_absent "${PKGBUILD}" \
        "(cat|install|cp).*site-start\.el|site-start\.el.*<<" \
        "PKGBUILD must not generate or install site-start.el"
}

test_pkgbuild_no_elinit_payload() {
    assert_pattern_absent "${PKGBUILD}" \
        "install.*elinit.*\.el" \
        "PKGBUILD must not install elinit .el files"
}

test_pkgbuild_no_helper_install() {
    assert_pattern_absent "${PKGBUILD}" \
        "install.*elinit-(logd|runas|rlimits)" \
        "PKGBUILD must not install elinit C helper binaries"
}

test_pkgbuild_no_sbin_install() {
    assert_pattern_absent "${PKGBUILD}" \
        "install.*elinitctl|install.*elinit-import|install.*elinit-log-prune|install.*elinit-logrotate" \
        "PKGBUILD must not install sbin scripts"
}

test_pkgbuild_no_autostart_wiring() {
    assert_pattern_absent "${PKGBUILD}" \
        "add-to-list.*load-path|require.*elinit|EMACS_ELINIT_DISABLE|elinit-pid1-autostart-disabled" \
        "PKGBUILD must not contain autostart wiring code"
}

# ── Nix checks ──

test_nix_no_site_start() {
    # Match creation patterns (cat/install/cp to site-start.el), not
    # validation checks that test for its absence.
    assert_pattern_absent "${NIX}" \
        "(cat|install|cp).*site-start\.el|site-start\.el.*<<" \
        "Nix must not generate or install site-start.el"
}

test_nix_no_elinit_payload() {
    assert_pattern_absent "${NIX}" \
        'elinit\.el|elinit-core\.el|elinit-log\.el|elinit-overrides\.el' \
        "Nix must not install elinit .el files"
}

test_nix_no_elinit_src() {
    assert_pattern_absent "${NIX}" \
        "elinitSrc" \
        "Nix must not reference elinitSrc"
}

test_nix_no_helper_build() {
    assert_pattern_absent "${NIX}" \
        "elinit-(logd|runas|rlimits)" \
        "Nix must not build or install elinit C helper binaries"
}

test_nix_no_sbin_install() {
    assert_pattern_absent "${NIX}" \
        "elinitctl|elinit-import|elinit-log-prune|elinit-logrotate" \
        "Nix must not install sbin scripts"
}

test_nix_no_autostart_wiring() {
    assert_pattern_absent "${NIX}" \
        "add-to-list.*load-path|require.*elinit|EMACS_ELINIT_DISABLE|elinit-pid1-autostart-disabled" \
        "Nix must not contain autostart wiring code"
}

run_tests "$(basename "${0}")"
