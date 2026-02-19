#!/bin/sh
# test-pid1-namespace.sh - PID1 namespace integration tests
#
# Tests PID1 behavior (hooks, signals, child reaping) inside isolated
# PID namespaces using unshare(1).
#
# Prerequisites:
#   ELINIT_PID1_EMACS  -- path to a patched Emacs with --pid1 support
#   unshare(1)         -- available and user namespaces enabled
#
# When prerequisites are unavailable, all tests are skipped (exit 0).
set -eu

cd "$(dirname "${0}")"
# shellcheck source=../../sbin/tests/testlib.sh
. ./testlib.sh

TESTS_DIR="$(pwd)"
EMACS="${ELINIT_PID1_EMACS:-}"

# --- Skip gates ---
# Check all prerequisites before defining any tests.  If any gate
# fails, print a reason and exit 0 with "0 tests: 0 passed, 0 failed".

skip_all() {
    printf 'SKIP: %s\n' "${1}"
    printf '\n# 0 tests: 0 passed, 0 failed\n'
    exit 0
}

if [ -z "${EMACS}" ]; then
    skip_all "ELINIT_PID1_EMACS is not set"
fi

if [ ! -x "${EMACS}" ]; then
    skip_all "ELINIT_PID1_EMACS is not executable: ${EMACS}"
fi

if ! command -v unshare >/dev/null 2>&1; then
    skip_all "unshare not found"
fi

if ! command -v timeout >/dev/null 2>&1; then
    skip_all "timeout not found"
fi

# Test that user namespaces + PID namespaces work.
if ! unshare --user --pid --fork true 2>/dev/null; then
    skip_all "unshare --user --pid --fork not supported (user namespaces disabled?)"
fi

# Test that the binary supports --pid1.
if ! "${EMACS}" --pid1 --batch --eval '(kill-emacs 0)' 2>/dev/null; then
    skip_all "ELINIT_PID1_EMACS does not support --pid1"
fi

# --- Helpers ---

# wait_for_file FILE TIMEOUT_SECS
#   Poll for a file to appear.  Returns 0 on success, 1 on timeout.
wait_for_file() {
    _wf_file="${1}"
    _wf_timeout="${2:-10}"
    _wf_elapsed=0
    while [ ! -e "${_wf_file}" ]; do
        sleep 0.2
        _wf_elapsed=$((_wf_elapsed + 1))
        # Each iteration is ~0.2s, so timeout*5 iterations
        if [ "${_wf_elapsed}" -ge "$((_wf_timeout * 5))" ]; then
            return 1
        fi
    done
    return 0
}

# run_pid1_daemon MARKER_DIR ELISP_FILE [EXTRA_ARGS...]
#   Start Emacs as PID 1 inside a PID namespace with --fg-daemon.
#   Runs in background; PID of the unshare process is stored in
#   _PID1_UNSHARE_PID.  The caller must clean up.
_PID1_UNSHARE_PID=""

run_pid1_daemon() {
    _rp_marker_dir="${1}"
    _rp_elisp="${2}"
    shift 2
    _rp_socket_dir="${_rp_marker_dir}/socket"
    mkdir -p "${_rp_socket_dir}"
    # shellcheck disable=SC2086
    PID1_TEST_MARKER_DIR="${_rp_marker_dir}" \
    timeout 30 \
        unshare --user --pid --fork --mount-proc \
        "${EMACS}" --pid1 -Q --fg-daemon \
        --eval "(setq server-socket-dir \"${_rp_socket_dir}\")" \
        -l "${_rp_elisp}" \
        "$@" \
        >/dev/null 2>&1 &
    _PID1_UNSHARE_PID="${!}"
}

# run_pid1_daemon_no_pid1 MARKER_DIR ELISP_FILE [EXTRA_ARGS...]
#   Same as run_pid1_daemon but WITHOUT --pid1 flag.
run_pid1_daemon_no_pid1() {
    _rp_marker_dir="${1}"
    _rp_elisp="${2}"
    shift 2
    _rp_socket_dir="${_rp_marker_dir}/socket"
    mkdir -p "${_rp_socket_dir}"
    # shellcheck disable=SC2086
    PID1_TEST_MARKER_DIR="${_rp_marker_dir}" \
    timeout 30 \
        unshare --user --pid --fork --mount-proc \
        "${EMACS}" -Q --fg-daemon \
        --eval "(setq server-socket-dir \"${_rp_socket_dir}\")" \
        -l "${_rp_elisp}" \
        "$@" \
        >/dev/null 2>&1 &
    _PID1_UNSHARE_PID="${!}"
}

# cleanup_pid1
#   Kill the unshare process tree if still running.
cleanup_pid1() {
    if [ -n "${_PID1_UNSHARE_PID}" ]; then
        kill "${_PID1_UNSHARE_PID}" 2>/dev/null || true
        wait "${_PID1_UNSHARE_PID}" 2>/dev/null || true
        _PID1_UNSHARE_PID=""
    fi
}

# send_signal_to_pid1 SIGNAL
#   Send a signal to the Emacs process inside the namespace.
#   We find the child of the unshare process (which is PID 1 inside
#   the namespace but has a real PID outside).
send_signal_to_pid1() {
    _ss_signal="${1}"
    if [ -z "${_PID1_UNSHARE_PID}" ]; then
        return 1
    fi
    # The unshare forks; its direct child is the namespace init.
    # pgrep -P finds children of the unshare process.
    _ss_child=""
    _ss_tries=0
    while [ -z "${_ss_child}" ] && [ "${_ss_tries}" -lt 25 ]; do
        _ss_child="$(pgrep -P "${_PID1_UNSHARE_PID}" 2>/dev/null | head -1)" || true
        if [ -z "${_ss_child}" ]; then
            sleep 0.2
            _ss_tries=$((_ss_tries + 1))
        fi
    done
    if [ -z "${_ss_child}" ]; then
        printf '  WARN: could not find child of unshare PID %s\n' \
            "${_PID1_UNSHARE_PID}" >&2
        return 1
    fi
    kill "-${_ss_signal}" "${_ss_child}"
}

# ===== Pre-flight tests (--batch, no namespace needed) =====

test_shellcheck() {
    command -v shellcheck >/dev/null 2>&1 || {
        printf '  SKIP: shellcheck not installed\n'
        return 0
    }
    run_cmd shellcheck -x -P tests -s sh "${TESTS_DIR}/test-pid1-namespace.sh"
    assert_status "0" "${TEST_STATUS}" "shellcheck passes"
}

test_pid1_mode_set() {
    run_cmd "${EMACS}" --pid1 --batch \
        --eval '(message "pid1-mode=%s" pid1-mode)'
    assert_status "0" "${TEST_STATUS}" "--pid1 --batch exits 0"
    assert_contains "${TEST_STDERR}" "pid1-mode=t" "pid1-mode is t with --pid1"
}

test_pid1_mode_nil() {
    run_cmd "${EMACS}" --batch \
        --eval '(message "pid1-mode=%s" pid1-mode)'
    assert_status "0" "${TEST_STATUS}" "--batch exits 0"
    assert_contains "${TEST_STDERR}" "pid1-mode=nil" "pid1-mode is nil without --pid1"
}

test_hooks_defined() {
    run_cmd "${EMACS}" --pid1 --batch \
        --eval '(message "boot=%S poweroff=%S reboot=%S" pid1-boot-hook pid1-poweroff-hook pid1-reboot-hook)'
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_contains "${TEST_STDERR}" "boot=" "pid1-boot-hook defined"
    assert_contains "${TEST_STDERR}" "poweroff=" "pid1-poweroff-hook defined"
    assert_contains "${TEST_STDERR}" "reboot=" "pid1-reboot-hook defined"
}

# ===== Namespace tests (--fg-daemon inside unshare) =====

test_boot_hook_fires() {
    marker_dir="${TEST_TMPDIR}/markers"
    mkdir -p "${marker_dir}"
    run_pid1_daemon "${marker_dir}" "${TESTS_DIR}/pid1-test-hooks.el"
    if ! wait_for_file "${marker_dir}/ready" 15; then
        cleanup_pid1
        _fail_test "timed out waiting for ready marker"
        return 1
    fi
    cleanup_pid1
    assert_file_exists "${marker_dir}/startup-hook" "startup-hook fired"
    assert_file_exists "${marker_dir}/pid1-boot-hook" "pid1-boot-hook fired"
}

test_boot_hook_skipped_without_pid1() {
    marker_dir="${TEST_TMPDIR}/markers"
    mkdir -p "${marker_dir}"
    run_pid1_daemon_no_pid1 "${marker_dir}" "${TESTS_DIR}/pid1-test-hooks.el"
    if ! wait_for_file "${marker_dir}/ready" 15; then
        cleanup_pid1
        _fail_test "timed out waiting for ready marker"
        return 1
    fi
    cleanup_pid1
    assert_file_exists "${marker_dir}/startup-hook" "startup-hook fired"
    assert_file_not_exists "${marker_dir}/pid1-boot-hook" \
        "pid1-boot-hook should NOT fire without --pid1"
}

test_sigterm_poweroff_hook() {
    marker_dir="${TEST_TMPDIR}/markers"
    mkdir -p "${marker_dir}"
    run_pid1_daemon "${marker_dir}" "${TESTS_DIR}/pid1-test-signals.el"
    if ! wait_for_file "${marker_dir}/ready" 15; then
        cleanup_pid1
        _fail_test "timed out waiting for ready marker"
        return 1
    fi
    send_signal_to_pid1 TERM
    # Wait for kill-emacs-hook marker (signals exit).
    if ! wait_for_file "${marker_dir}/kill-emacs-hook" 10; then
        cleanup_pid1
        _fail_test "timed out waiting for kill-emacs-hook after SIGTERM"
        return 1
    fi
    cleanup_pid1
    assert_file_exists "${marker_dir}/pid1-poweroff-hook" \
        "SIGTERM triggers pid1-poweroff-hook"
    assert_file_exists "${marker_dir}/kill-emacs-hook" \
        "kill-emacs-hook fires after SIGTERM"
}

test_sigusr1_poweroff_hook() {
    marker_dir="${TEST_TMPDIR}/markers"
    mkdir -p "${marker_dir}"
    run_pid1_daemon "${marker_dir}" "${TESTS_DIR}/pid1-test-signals.el"
    if ! wait_for_file "${marker_dir}/ready" 15; then
        cleanup_pid1
        _fail_test "timed out waiting for ready marker"
        return 1
    fi
    send_signal_to_pid1 USR1
    if ! wait_for_file "${marker_dir}/kill-emacs-hook" 10; then
        cleanup_pid1
        _fail_test "timed out waiting for kill-emacs-hook after SIGUSR1"
        return 1
    fi
    cleanup_pid1
    assert_file_exists "${marker_dir}/pid1-poweroff-hook" \
        "SIGUSR1 triggers pid1-poweroff-hook"
}

test_sigusr2_reboot_hook() {
    marker_dir="${TEST_TMPDIR}/markers"
    mkdir -p "${marker_dir}"
    run_pid1_daemon "${marker_dir}" "${TESTS_DIR}/pid1-test-signals.el"
    if ! wait_for_file "${marker_dir}/ready" 15; then
        cleanup_pid1
        _fail_test "timed out waiting for ready marker"
        return 1
    fi
    send_signal_to_pid1 USR2
    if ! wait_for_file "${marker_dir}/kill-emacs-hook" 10; then
        cleanup_pid1
        _fail_test "timed out waiting for kill-emacs-hook after SIGUSR2"
        return 1
    fi
    cleanup_pid1
    assert_file_exists "${marker_dir}/pid1-reboot-hook" \
        "SIGUSR2 triggers pid1-reboot-hook"
}

test_sighup_ignored() {
    marker_dir="${TEST_TMPDIR}/markers"
    mkdir -p "${marker_dir}"
    run_pid1_daemon "${marker_dir}" "${TESTS_DIR}/pid1-test-signals.el"
    if ! wait_for_file "${marker_dir}/ready" 15; then
        cleanup_pid1
        _fail_test "timed out waiting for ready marker"
        return 1
    fi
    send_signal_to_pid1 HUP
    # Wait a moment, then verify Emacs is still alive.
    sleep 1
    if [ -n "${_PID1_UNSHARE_PID}" ] && kill -0 "${_PID1_UNSHARE_PID}" 2>/dev/null; then
        # Process still alive -- PASS.
        assert_file_not_exists "${marker_dir}/kill-emacs-hook" \
            "SIGHUP should not trigger kill-emacs-hook"
    else
        cleanup_pid1
        _fail_test "Emacs died after SIGHUP (should be ignored in PID1 mode)"
        return 1
    fi
    cleanup_pid1
}

test_child_reaping() {
    marker_dir="${TEST_TMPDIR}/markers"
    mkdir -p "${marker_dir}"
    run_pid1_daemon "${marker_dir}" "${TESTS_DIR}/pid1-test-reaping.el"
    if ! wait_for_file "${marker_dir}/ready" 20; then
        cleanup_pid1
        _fail_test "timed out waiting for ready marker"
        return 1
    fi
    cleanup_pid1
    assert_file_exists "${marker_dir}/reaping-result" "reaping-result marker written"
    result="$(cat "${marker_dir}/reaping-result")"
    assert_eq "NO-ZOMBIES" "${result}" "orphan children reaped, no zombies"
}

test_normal_emacs_no_pid1_hooks() {
    # Without --pid1, SIGTERM should kill Emacs without PID1 hooks.
    marker_dir="${TEST_TMPDIR}/markers"
    mkdir -p "${marker_dir}"
    run_pid1_daemon_no_pid1 "${marker_dir}" "${TESTS_DIR}/pid1-test-signals.el"
    # Without --pid1, pid1-boot-hook does not fire, so "ready" is never
    # written by pid1-test-signals.el.  Wait for startup-hook via a short
    # timer-based ready signal instead.  We just wait a few seconds for
    # the daemon to be up.
    sleep 3
    send_signal_to_pid1 TERM || true
    # Wait for the process to exit.
    sleep 2
    cleanup_pid1
    assert_file_not_exists "${marker_dir}/pid1-poweroff-hook" \
        "without --pid1, pid1-poweroff-hook should NOT fire"
    assert_file_not_exists "${marker_dir}/pid1-reboot-hook" \
        "without --pid1, pid1-reboot-hook should NOT fire"
}

run_tests "$(basename "${0}")"
