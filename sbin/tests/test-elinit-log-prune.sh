#!/bin/sh
# SPDX-License-Identifier: GPL-3.0-or-later
# test-log-prune.sh - tests for sbin/elinit-log-prune
set -eu

cd "$(dirname "${0}")"
. ./testlib.sh

SCRIPT="$(cd .. && pwd)/elinit-log-prune"

# Helper: create a file of approximately N bytes
make_file() {
    path="${1}"
    size="${2}"
    dd if=/dev/zero of="${path}" bs=1 count="${size}" 2>/dev/null
}

# Helper: create a standard log directory layout for prune tests.
# Creates an active log plus several rotated files.
make_log_layout() {
    dir="${1}"
    # Active log (never deleted)
    make_file "${dir}/log-svc.log" 1000
    # Rotated files (oldest to newest by mtime)
    make_file "${dir}/log-svc.20250101-120000.log" 2000
    sleep 0.1
    make_file "${dir}/log-svc.20250201-120000.log" 2000
    sleep 0.1
    make_file "${dir}/log-svc.20250301-120000.log" 2000
}

# Helper: create a restricted PATH missing a specific command.
# Includes the essential commands the script needs.
path_without() {
    excluded="${1}"
    dir="${TEST_TMPDIR}/path-no-${excluded}"
    mkdir -p "${dir}"
    for cmd in flock find sort awk stat du fuser sed printf cat wc mktemp rm; do
        [ "${cmd}" = "${excluded}" ] && continue
        real="$(command -v "${cmd}" 2>/dev/null)" || continue
        ln -sf "${real}" "${dir}/${cmd}"
    done
    printf '%s' "${dir}"
}

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
    assert_contains "${TEST_STDOUT}" "Usage" "shows usage"
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

test_max_total_bytes_non_integer_fails() {
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --max-total-bytes abc
    assert_status "1" "${TEST_STATUS}" "non-integer cap fails"
    assert_contains "${TEST_STDERR}" "positive integer" "error message"
}

# ===== Missing-value validation paths =====

test_max_total_bytes_missing_value_fails() {
    run_cmd "${SCRIPT}" --max-total-bytes
    assert_status "1" "${TEST_STATUS}" "missing --max-total-bytes value fails"
    assert_contains "${TEST_STDERR}" "requires" "error mentions requires"
}

test_lock_file_missing_value_fails() {
    run_cmd "${SCRIPT}" --lock-file
    assert_status "1" "${TEST_STATUS}" "missing --lock-file value fails"
    assert_contains "${TEST_STDERR}" "requires" "error mentions requires"
}

test_protect_id_missing_value_fails() {
    run_cmd "${SCRIPT}" --protect-id
    assert_status "1" "${TEST_STATUS}" "missing --protect-id value fails"
    assert_contains "${TEST_STDERR}" "requires" "error mentions requires"
}

test_format_hint_missing_value_fails() {
    run_cmd "${SCRIPT}" --format-hint
    assert_status "1" "${TEST_STATUS}" "missing --format-hint value fails"
    assert_contains "${TEST_STDERR}" "requires" "error mentions requires"
}

test_vacuum_max_total_bytes_missing_value_fails() {
    run_cmd "${SCRIPT}" --vacuum-max-total-bytes
    assert_status "1" "${TEST_STATUS}" "missing --vacuum-max-total-bytes value fails"
    assert_contains "${TEST_STDERR}" "requires" "error mentions requires"
}

# ===== Alias flags =====

test_vacuum_alias_accepted() {
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --vacuum
    # Should succeed (under cap, no-op)
    assert_status "0" "${TEST_STATUS}" "--vacuum accepted"
}

test_vacuum_max_total_bytes_alias() {
    make_file "${TEST_TMPDIR}/log-svc.log" 100
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --vacuum-max-total-bytes 999999999
    assert_status "0" "${TEST_STATUS}" "alias accepted"
    assert_contains "${TEST_STDOUT}" "no action" "under cap"
}

test_format_hint_accepted() {
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --format-hint binary
    assert_status "0" "${TEST_STATUS}" "--format-hint accepted"
}

# ===== Dependency failures =====

test_missing_flock_fails() {
    restricted="$(path_without flock)"
    PATH="${restricted}" run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}"
    assert_status "1" "${TEST_STATUS}" "missing flock fails"
    assert_contains "${TEST_STDERR}" "flock" "error mentions flock"
}

test_missing_find_fails() {
    restricted="$(path_without find)"
    PATH="${restricted}" run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}"
    assert_status "1" "${TEST_STATUS}" "missing find fails"
    assert_contains "${TEST_STDERR}" "find" "error mentions find"
}

test_missing_sort_fails() {
    restricted="$(path_without sort)"
    PATH="${restricted}" run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}"
    assert_status "1" "${TEST_STATUS}" "missing sort fails"
    assert_contains "${TEST_STDERR}" "sort" "error mentions sort"
}

test_missing_awk_fails() {
    restricted="$(path_without awk)"
    PATH="${restricted}" run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}"
    assert_status "1" "${TEST_STATUS}" "missing awk fails"
    assert_contains "${TEST_STDERR}" "awk" "error mentions awk"
}

test_missing_stat_fails() {
    restricted="$(path_without stat)"
    PATH="${restricted}" run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}"
    assert_status "1" "${TEST_STATUS}" "missing stat fails"
    assert_contains "${TEST_STDERR}" "stat" "error mentions stat"
}

# ===== Under-cap no-op =====

test_under_cap_no_op() {
    make_file "${TEST_TMPDIR}/log-svc.log" 100
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --max-total-bytes 999999999
    assert_status "0" "${TEST_STATUS}" "under cap exits 0"
    assert_contains "${TEST_STDOUT}" "no action" "reports no action"
}

# ===== Over cap but no rotated candidates =====

test_over_cap_no_rotated_candidates() {
    # Large active file pushes over cap, but no rotated files match find patterns.
    make_file "${TEST_TMPDIR}/log-svc.log" 10000
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --max-total-bytes 1
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_contains "${TEST_STDOUT}" "no rotated candidates" "reports no candidates"
    assert_file_exists "${TEST_TMPDIR}/log-svc.log" "active file preserved"
}

# ===== Oldest-first prune =====

test_oldest_pruned_first() {
    make_log_layout "${TEST_TMPDIR}"
    # Cap just above the active log size; should prune oldest rotated first
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --max-total-bytes 4000 --dry-run
    assert_status "0" "${TEST_STATUS}" "exits 0"
    # Oldest file should appear in prune output
    assert_contains "${TEST_STDOUT}" "20250101-120000" "oldest pruned first"
}

# ===== Protection: --protect-id =====

test_protect_id_skips_protected() {
    make_log_layout "${TEST_TMPDIR}"
    # Prune everything but protect svc
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --max-total-bytes 1 --protect-id svc --dry-run
    assert_status "0" "${TEST_STATUS}" "exits 0"
}

test_protect_id_multiple() {
    make_file "${TEST_TMPDIR}/log-alpha.log" 1000
    make_file "${TEST_TMPDIR}/log-alpha.20250101-120000.log" 2000
    make_file "${TEST_TMPDIR}/log-beta.log" 1000
    make_file "${TEST_TMPDIR}/log-beta.20250101-120000.log" 2000
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --max-total-bytes 1 \
        --protect-id alpha --protect-id beta --dry-run
    assert_status "0" "${TEST_STATUS}" "multiple --protect-id accepted"
}

# ===== Dry-run output contract =====

test_dry_run_does_not_delete() {
    make_log_layout "${TEST_TMPDIR}"
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --max-total-bytes 1 --dry-run
    assert_status "0" "${TEST_STATUS}" "dry-run exits 0"
    # All files should still exist
    assert_file_exists "${TEST_TMPDIR}/log-svc.20250101-120000.log" "oldest still exists"
    assert_file_exists "${TEST_TMPDIR}/log-svc.20250201-120000.log" "middle still exists"
    assert_file_exists "${TEST_TMPDIR}/log-svc.20250301-120000.log" "newest still exists"
    assert_contains "${TEST_STDOUT}" "prune:" "shows prune lines"
}

test_dry_run_shows_file_sizes() {
    make_log_layout "${TEST_TMPDIR}"
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --max-total-bytes 1 --dry-run
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_contains "${TEST_STDOUT}" "bytes)" "file sizes shown in prune output"
}

# ===== Actual prune (non-dry-run) =====

test_actual_prune_deletes_files() {
    make_log_layout "${TEST_TMPDIR}"
    # Set cap to 4000 bytes -- should need to delete some rotated files
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --max-total-bytes 4000
    assert_status "0" "${TEST_STATUS}" "prune exits 0"
    # At least the oldest should be gone
    assert_file_not_exists "${TEST_TMPDIR}/log-svc.20250101-120000.log" "oldest deleted"
    # Active log should always survive
    assert_file_exists "${TEST_TMPDIR}/log-svc.log" "active log preserved"
}

test_actual_prune_completes() {
    make_log_layout "${TEST_TMPDIR}"
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --max-total-bytes 1
    assert_status "0" "${TEST_STATUS}" "prune exits 0"
    assert_contains "${TEST_STDOUT}" "prune: complete" "completes"
    assert_file_exists "${TEST_TMPDIR}/log-svc.log" "active log preserved"
}

# ===== Lock busy =====

test_lock_busy_skips() {
    make_file "${TEST_TMPDIR}/log-svc.log" 100
    lock_file="${TEST_TMPDIR}/.prune.lock"
    # Hold the lock in a subshell
    (
        exec 9>"${lock_file}"
        flock -n 9
        # Run prune while lock is held
        run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --lock-file "${lock_file}" --max-total-bytes 1
        assert_status "0" "${TEST_STATUS}" "lock busy exits 0"
        assert_contains "${TEST_STDOUT}" "lock busy" "reports lock busy"
    )
}

# ===== Lone orphan preservation =====

test_lone_orphan_preserved() {
    # A single rotated file with no active parent and no siblings
    # should be preserved (naming ambiguity -- could be active for timestamp-ID service)
    make_file "${TEST_TMPDIR}/log-svc.20250101-120000.log" 5000
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --max-total-bytes 1 --dry-run
    assert_status "0" "${TEST_STATUS}" "exits 0"
    # Should NOT appear in prune output (lone orphan preserved)
    assert_not_contains "${TEST_STDOUT}" "prune: ${TEST_TMPDIR}/log-svc.20250101-120000.log" \
        "lone orphan not pruned"
}

# ===== Sibling confirmation =====

test_sibling_rotated_allows_prune() {
    # Two rotated files with same parent but no active parent file.
    # The sibling relationship confirms they are genuinely rotated.
    make_file "${TEST_TMPDIR}/log-svc.20250101-120000.log" 3000
    sleep 0.1
    make_file "${TEST_TMPDIR}/log-svc.20250201-120000.log" 3000
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --max-total-bytes 1 --dry-run
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_contains "${TEST_STDOUT}" "prune:" "siblings allow pruning"
}

# ===== Fuser guard =====

test_fuser_skips_open_files() {
    make_log_layout "${TEST_TMPDIR}"
    fuser_target="${TEST_TMPDIR}/log-svc.20250101-120000.log"
    fuser_dir="${TEST_TMPDIR}/fuser-stubs"
    mkdir -p "${fuser_dir}"
    # Stub fuser: report target file as open (exit 0), others as closed (exit 1)
    cat > "${fuser_dir}/fuser" <<STUB
#!/bin/sh
case "\${1}" in
    "${fuser_target}") exit 0 ;;
    *) exit 1 ;;
esac
STUB
    chmod +x "${fuser_dir}/fuser"
    PATH="${fuser_dir}:${PATH}" run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --max-total-bytes 1 --dry-run
    assert_status "0" "${TEST_STATUS}" "exits 0"
    # The oldest file should NOT be pruned (fuser says it's open)
    assert_not_contains "${TEST_STDOUT}" "20250101-120000" "open file skipped by fuser"
    # But newer files should still appear in prune output
    assert_contains "${TEST_STDOUT}" "prune:" "other files still prunable"
}

# ===== Tar.gz rotated files =====

test_tar_gz_rotated_pruned() {
    # Rotated .tar.gz files (both with and without sequence suffix) should be
    # prunable when over cap and confirmed by active parent.
    make_file "${TEST_TMPDIR}/log-svc.log" 1000
    make_file "${TEST_TMPDIR}/log-svc.20250101-120000.log.tar.gz" 3000
    sleep 0.1
    make_file "${TEST_TMPDIR}/log-svc.20250201-120000.1.log.tar.gz" 3000
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --max-total-bytes 1 --dry-run
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_contains "${TEST_STDOUT}" "tar.gz" "tar.gz files in prune output"
}

# ===== Empty directory =====

test_empty_dir_no_candidates() {
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --max-total-bytes 1
    assert_status "0" "${TEST_STATUS}" "empty dir exits 0"
}

# ===== Custom lock file =====

test_custom_lock_file() {
    make_file "${TEST_TMPDIR}/log-svc.log" 100
    lock="${TEST_TMPDIR}/custom.lock"
    run_cmd "${SCRIPT}" --log-dir "${TEST_TMPDIR}" --lock-file "${lock}" --max-total-bytes 999999999
    assert_status "0" "${TEST_STATUS}" "custom lock file accepted"
    assert_file_exists "${lock}" "custom lock file created"
}

run_tests "$(basename "${0}")"
