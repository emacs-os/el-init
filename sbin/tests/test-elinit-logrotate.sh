#!/bin/sh
# test-logrotate.sh - tests for sbin/elinit-logrotate
set -eu

cd "$(dirname "${0}")"
. ./testlib.sh

SCRIPT="$(cd .. && pwd)/elinit-logrotate"

# ===== ShellCheck =====

test_shellcheck() {
    command -v shellcheck >/dev/null 2>&1 || {
        printf '  SKIP: shellcheck not installed\n'
        return 0
    }
    run_cmd shellcheck -s sh "${SCRIPT}"
    assert_status "0" "${TEST_STATUS}" "shellcheck passes"
}

# ===== Argument parsing =====

test_help_exits_zero() {
    run_cmd "${SCRIPT}" --help
    assert_status "0" "${TEST_STATUS}" "--help exits 0"
    assert_contains "${TEST_STDOUT}" "Usage" "--help shows usage"
}

test_missing_log_dir_fails() {
    run_cmd "${SCRIPT}"
    assert_status "1" "${TEST_STATUS}" "missing --log-dir fails"
    assert_contains "${TEST_STDERR}" "--log-dir is required" "error message"
}

test_nonexistent_log_dir_fails() {
    run_cmd "${SCRIPT}" --log-dir /nonexistent/path/xyz
    assert_status "1" "${TEST_STATUS}" "nonexistent dir fails"
    assert_contains "${TEST_STDERR}" "not a directory" "error message"
}

test_unknown_option_fails() {
    run_cmd "${SCRIPT}" --bogus
    assert_status "1" "${TEST_STATUS}" "unknown option fails"
    assert_contains "${TEST_STDERR}" "Unknown option" "error message"
}

test_keep_days_non_integer_fails() {
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --keep-days abc
    assert_status "1" "${TEST_STATUS}" "non-integer --keep-days fails"
    assert_contains "${TEST_STDERR}" "positive integer" "error message"
}

test_keep_days_missing_value_fails() {
    run_cmd "${SCRIPT}" --keep-days
    assert_status "1" "${TEST_STATUS}" "missing --keep-days value fails"
    assert_contains "${TEST_STDERR}" "requires" "error mentions requires"
}

test_log_dir_missing_value_fails() {
    run_cmd "${SCRIPT}" --log-dir
    assert_status "1" "${TEST_STATUS}" "missing --log-dir value fails"
    assert_contains "${TEST_STDERR}" "requires" "error mentions requires"
}

test_pid_dir_missing_value_fails() {
    run_cmd "${SCRIPT}" --pid-dir
    assert_status "1" "${TEST_STATUS}" "missing --pid-dir value fails"
    assert_contains "${TEST_STDERR}" "requires" "error mentions requires"
}

# ===== Dry-run output contract =====

test_dry_run_rotates_active_files() {
    # Create active log files
    printf 'data' > "${TEST_TMPDIR}/elinit.log"
    printf 'data' > "${TEST_TMPDIR}/log-myapp.log"
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --dry-run
    assert_status "0" "${TEST_STATUS}" "dry-run exits 0"
    assert_contains "${TEST_STDOUT}" "rotate:" "dry-run prints rotate actions"
    assert_contains "${TEST_STDOUT}" "elinit.log" "rotates elinit.log"
    assert_contains "${TEST_STDOUT}" "log-myapp.log" "rotates log-myapp.log"
    # Files should still exist (dry-run)
    assert_file_exists "${TEST_TMPDIR}/elinit.log" "elinit.log still exists"
    assert_file_exists "${TEST_TMPDIR}/log-myapp.log" "log-myapp.log still exists"
}

test_dry_run_skips_rotated_files() {
    # Create a rotated file (should be skipped during rotation)
    printf 'old' > "${TEST_TMPDIR}/log-svc.20250101-120000.log"
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --dry-run
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_not_contains "${TEST_STDOUT}" "rotate: ${TEST_TMPDIR}/log-svc.20250101-120000.log" \
        "does not rotate already-rotated files"
}

# ===== Rotation naming =====

test_rotation_creates_timestamped_file() {
    printf 'data' > "${TEST_TMPDIR}/log-svc.log"
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}"
    assert_status "0" "${TEST_STATUS}" "rotation exits 0"
    # Original should be gone
    assert_file_not_exists "${TEST_TMPDIR}/log-svc.log" "original removed after rotate"
    # A rotated file should exist (match the timestamp pattern)
    rotated="$(find "${TEST_TMPDIR}" -maxdepth 1 -name 'log-svc.*.log*' | head -1)"
    assert_ne "" "${rotated}" "rotated file created"
}

test_rotation_collision_suffix() {
    printf 'data' > "${TEST_TMPDIR}/log-svc.log"
    # Pre-create files for the current second AND the next second so
    # the collision is guaranteed regardless of clock tick between our
    # date call and the script's date call.
    stamp0="$(date +%Y%m%d-%H%M%S)"
    stamp1="$(date -d '+1 second' +%Y%m%d-%H%M%S 2>/dev/null)" || \
        stamp1=""
    printf 'old' > "${TEST_TMPDIR}/log-svc.${stamp0}.log"
    [ -n "${stamp1}" ] && printf 'old' > "${TEST_TMPDIR}/log-svc.${stamp1}.log"
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}"
    assert_status "0" "${TEST_STATUS}" "collision handled"
    # Should have created a sequence-suffixed variant (.1) for whichever
    # timestamp the script picked.
    collision="$(find "${TEST_TMPDIR}" -maxdepth 1 -name 'log-svc.*.*.log*' \
        | grep -E '\.[0-9]+\.log' | head -1)"
    assert_ne "" "${collision}" "collision file created with sequence suffix"
}

# ===== is_rotated detection =====

test_is_rotated_patterns() {
    # Create various files and verify which are detected as active vs rotated
    printf 'a' > "${TEST_TMPDIR}/log-svc.log"
    printf 'r' > "${TEST_TMPDIR}/log-svc.20250101-120000.log"
    printf 'r' > "${TEST_TMPDIR}/log-svc.20250101-120000.1.log"
    printf 'r' > "${TEST_TMPDIR}/log-svc.20250101-120000.log.tar.gz"
    # Dotted service IDs should be treated as active
    printf 'a' > "${TEST_TMPDIR}/log-svc.1.log"

    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --dry-run
    assert_status "0" "${TEST_STATUS}" "exits 0"
    # Active files should be listed for rotation
    assert_contains "${TEST_STDOUT}" "log-svc.log" "active log-svc.log rotated"
    assert_contains "${TEST_STDOUT}" "log-svc.1.log" "active dotted-id log rotated"
}

# ===== Signal reopen =====

test_signal_reopen_no_pidfiles() {
    printf 'data' > "${TEST_TMPDIR}/log-svc.log"
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --signal-reopen --dry-run
    assert_status "0" "${TEST_STATUS}" "no pidfiles is fine"
    # No signal lines expected
    assert_not_contains "${TEST_STDOUT}" "signal:" "no signal without pidfiles"
}

test_signal_reopen_invalid_pid() {
    printf 'data' > "${TEST_TMPDIR}/log-svc.log"
    printf 'notanumber' > "${TEST_TMPDIR}/logd-svc.pid"
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --signal-reopen --dry-run
    assert_status "0" "${TEST_STATUS}" "invalid pid handled gracefully"
    assert_not_contains "${TEST_STDOUT}" "signal:" "no signal for invalid pid"
}

test_signal_reopen_dead_pid() {
    printf 'data' > "${TEST_TMPDIR}/log-svc.log"
    # PID 99999999 is almost certainly not running
    printf '99999999' > "${TEST_TMPDIR}/logd-svc.pid"
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --signal-reopen --dry-run
    assert_status "0" "${TEST_STATUS}" "dead pid handled gracefully"
    assert_not_contains "${TEST_STDOUT}" "signal:" "no signal for dead pid"
}

test_signal_reopen_live_pid_dry_run() {
    printf 'data' > "${TEST_TMPDIR}/log-svc.log"
    sleep 300 &
    bg_pid="${!}"
    printf '%s' "${bg_pid}" > "${TEST_TMPDIR}/logd-svc.pid"
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --signal-reopen --dry-run
    kill "${bg_pid}" 2>/dev/null || true
    wait "${bg_pid}" 2>/dev/null || true
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_contains "${TEST_STDOUT}" "signal:" "dry-run shows signal line"
    assert_contains "${TEST_STDOUT}" "${bg_pid}" "correct PID in signal output"
}

test_signal_reopen_live_pid_actual() {
    printf 'data' > "${TEST_TMPDIR}/log-svc.log"
    sleep 300 &
    bg_pid="${!}"
    printf '%s' "${bg_pid}" > "${TEST_TMPDIR}/logd-svc.pid"
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --signal-reopen
    kill "${bg_pid}" 2>/dev/null || true
    wait "${bg_pid}" 2>/dev/null || true
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_contains "${TEST_STDOUT}" "signal: SIGHUP" "SIGHUP sent"
    assert_contains "${TEST_STDOUT}" "${bg_pid}" "correct PID signaled"
}

# ===== Tar compression =====

test_rotation_compresses_when_tar_available() {
    command -v tar >/dev/null 2>&1 || {
        printf '  SKIP: tar not available\n'
        return 0
    }
    printf 'data' > "${TEST_TMPDIR}/log-svc.log"
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}"
    assert_status "0" "${TEST_STATUS}" "exits 0"
    # Should have a .tar.gz file
    tarfile="$(find "${TEST_TMPDIR}" -maxdepth 1 -name 'log-svc.*.log.tar.gz' | head -1)"
    assert_ne "" "${tarfile}" "tar.gz archive created"
}

test_dry_run_no_compress_line() {
    # In dry-run mode, rotate_file prints rotate: but does not call
    # compress_rotated, so no compress: line appears.
    command -v tar >/dev/null 2>&1 || {
        printf '  SKIP: tar not available\n'
        return 0
    }
    printf 'data' > "${TEST_TMPDIR}/log-svc.log"
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --dry-run
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_contains "${TEST_STDOUT}" "rotate:" "dry-run shows rotate action"
    assert_not_contains "${TEST_STDOUT}" "compress:" "dry-run skips compress step"
}

test_compression_failure_preserves_file() {
    command -v tar >/dev/null 2>&1 || {
        printf '  SKIP: tar not available\n'
        return 0
    }
    printf 'data' > "${TEST_TMPDIR}/log-svc.log"
    # Create a tar stub that always fails
    tar_stub_dir="${TEST_TMPDIR}/tar-stubs"
    mkdir -p "${tar_stub_dir}"
    cat > "${tar_stub_dir}/tar" <<'STUB'
#!/bin/sh
exit 1
STUB
    chmod +x "${tar_stub_dir}/tar"
    PATH="${tar_stub_dir}:${PATH}" run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}"
    assert_status "0" "${TEST_STATUS}" "exits 0 despite compress failure"
    assert_contains "${TEST_STDERR}" "Failed to compress" "warning about compress failure"
    # Uncompressed rotated file should still exist
    rotated="$(find "${TEST_TMPDIR}" -maxdepth 1 -name 'log-svc.*.log' -not -name '*.tar.gz' | head -1)"
    assert_ne "" "${rotated}" "uncompressed file preserved"
}

# ===== Pruning old rotated files =====

test_prune_old_rotated_dry_run() {
    # Create rotated files and backdate them past keep_days
    printf 'old' > "${TEST_TMPDIR}/log-svc.20250101-120000.log"
    touch -d "30 days ago" "${TEST_TMPDIR}/log-svc.20250101-120000.log"
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --keep-days 1 --dry-run
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_contains "${TEST_STDOUT}" "prune:" "dry-run shows prune lines"
    assert_file_exists "${TEST_TMPDIR}/log-svc.20250101-120000.log" "file preserved in dry-run"
}

test_prune_old_rotated_actual() {
    # Create rotated files and backdate them past keep_days
    printf 'old' > "${TEST_TMPDIR}/log-svc.20250101-120000.log"
    touch -d "30 days ago" "${TEST_TMPDIR}/log-svc.20250101-120000.log"
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --keep-days 1
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_contains "${TEST_STDOUT}" "prune:" "prune output shown"
    assert_file_not_exists "${TEST_TMPDIR}/log-svc.20250101-120000.log" "old file deleted"
}

test_prune_old_tar_gz_no_suffix() {
    # Cover is_rotated tar.gz branch without sequence suffix (L132)
    printf 'old' > "${TEST_TMPDIR}/log-svc.20250101-120000.log.tar.gz"
    touch -d "30 days ago" "${TEST_TMPDIR}/log-svc.20250101-120000.log.tar.gz"
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --keep-days 1 --dry-run
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_contains "${TEST_STDOUT}" "prune:" "tar.gz file detected for prune"
}

test_prune_old_tar_gz_sequence_suffix() {
    # Cover is_rotated tar.gz branch with sequence suffix (L129)
    printf 'old' > "${TEST_TMPDIR}/log-svc.20250101-120000.1.log.tar.gz"
    touch -d "30 days ago" "${TEST_TMPDIR}/log-svc.20250101-120000.1.log.tar.gz"
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --keep-days 1 --dry-run
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_contains "${TEST_STDOUT}" "prune:" "tar.gz with seq suffix detected for prune"
}

test_prune_skips_recent_files() {
    # Recent rotated files should not be pruned
    printf 'new' > "${TEST_TMPDIR}/log-svc.20250101-120000.log"
    # File mtime is now (< 1 day), keep_days=1 means -mtime +1
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --keep-days 1 --dry-run
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_not_contains "${TEST_STDOUT}" "prune:" "recent files not pruned"
}

# ===== Empty directory =====

test_empty_log_dir() {
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}"
    assert_status "0" "${TEST_STATUS}" "empty dir is fine"
    assert_contains "${TEST_STDOUT}" "logrotate: complete" "completes normally"
}

# ===== Custom pid-dir =====

test_custom_pid_dir() {
    pid_dir="${TEST_TMPDIR}/pids"
    mkdir -p "${pid_dir}"
    printf 'data' > "${TEST_TMPDIR}/log-svc.log"
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --pid-dir "${pid_dir}" --signal-reopen --dry-run
    assert_status "0" "${TEST_STATUS}" "custom pid-dir accepted"
}

run_tests "$(basename "${0}")"
