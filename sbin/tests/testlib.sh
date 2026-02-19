#!/bin/sh
# testlib.sh - minimal POSIX shell test framework for elinit sbin tests
#
# Source this file from test scripts.  Define test_ functions and call
# run_tests at the end.
#
# Usage:
#   . ./testlib.sh
#   test_something() { assert_eq "a" "a" "values match"; }
#   run_tests

set -eu

# --- Counters ---
_test_pass=0
_test_fail=0
_test_total=0
_test_current=""

# --- Colors (disabled when not a terminal) ---
_c_red=""
_c_green=""
_c_reset=""
if [ -t 1 ]; then
    _c_red="$(printf '\033[31m')"
    _c_green="$(printf '\033[32m')"
    _c_reset="$(printf '\033[0m')"
fi

# --- Temp directory lifecycle ---
TEST_TMPDIR=""

_create_tmpdir() {
    TEST_TMPDIR="$(mktemp -d)"
}

_remove_tmpdir() {
    [ -n "${TEST_TMPDIR}" ] && [ -d "${TEST_TMPDIR}" ] && rm -rf "${TEST_TMPDIR}"
    TEST_TMPDIR=""
}

# --- Optional hooks (override in test files) ---
setup() { :; }
teardown() { :; }

# --- Assertions ---

_fail_test() {
    msg="${1}"
    printf '  %sFAIL%s: %s\n' "${_c_red}" "${_c_reset}" "${msg}" >&2
    _test_fail=$((_test_fail + 1))
    return 1
}

_pass_assert() {
    : # silent on success
}

assert_eq() {
    expected="${1}"
    actual="${2}"
    msg="${3:-assert_eq}"
    if [ "${expected}" = "${actual}" ]; then
        _pass_assert
    else
        _fail_test "${msg}: expected '${expected}', got '${actual}'"
        return 1
    fi
}

assert_ne() {
    unexpected="${1}"
    actual="${2}"
    msg="${3:-assert_ne}"
    if [ "${unexpected}" != "${actual}" ]; then
        _pass_assert
    else
        _fail_test "${msg}: expected not '${unexpected}', got '${actual}'"
        return 1
    fi
}

assert_contains() {
    haystack="${1}"
    needle="${2}"
    msg="${3:-assert_contains}"
    case "${haystack}" in
        *"${needle}"*) _pass_assert ;;
        *)
            _fail_test "${msg}: '${haystack}' does not contain '${needle}'"
            return 1
            ;;
    esac
}

assert_not_contains() {
    haystack="${1}"
    needle="${2}"
    msg="${3:-assert_not_contains}"
    case "${haystack}" in
        *"${needle}"*)
            _fail_test "${msg}: '${haystack}' should not contain '${needle}'"
            return 1
            ;;
        *) _pass_assert ;;
    esac
}

assert_status() {
    expected="${1}"
    actual="${2}"
    msg="${3:-assert_status}"
    assert_eq "${expected}" "${actual}" "${msg}"
}

assert_file_exists() {
    path="${1}"
    msg="${2:-assert_file_exists}"
    if [ -e "${path}" ]; then
        _pass_assert
    else
        _fail_test "${msg}: file does not exist: ${path}"
        return 1
    fi
}

assert_file_not_exists() {
    path="${1}"
    msg="${2:-assert_file_not_exists}"
    if [ ! -e "${path}" ]; then
        _pass_assert
    else
        _fail_test "${msg}: file should not exist: ${path}"
        return 1
    fi
}

assert_match() {
    haystack="${1}"
    pattern="${2}"
    msg="${3:-assert_match}"
    if printf '%s' "${haystack}" | grep -qE "${pattern}"; then
        _pass_assert
    else
        _fail_test "${msg}: '${haystack}' does not match pattern '${pattern}'"
        return 1
    fi
}

# --- Command runner ---
# Captures stdout, stderr, and exit status of a command.
TEST_STDOUT=""
TEST_STDERR=""
TEST_STATUS=0

run_cmd() {
    _stdout_file="$(mktemp)"
    _stderr_file="$(mktemp)"
    # shellcheck disable=SC2034
    TEST_STATUS=0
    # shellcheck disable=SC2034
    "${@}" >"${_stdout_file}" 2>"${_stderr_file}" || TEST_STATUS="${?}"
    # shellcheck disable=SC2034
    TEST_STDOUT="$(cat "${_stdout_file}")"
    # shellcheck disable=SC2034
    TEST_STDERR="$(cat "${_stderr_file}")"
    rm -f "${_stdout_file}" "${_stderr_file}"
}

# --- Test runner ---

run_test() {
    name="${1}"
    func="${2}"
    _test_total=$((_test_total + 1))
    _test_current="${name}"
    _old_fail="${_test_fail}"

    _create_tmpdir
    setup

    # Run the test function; capture failure
    _test_ok=0
    if "${func}"; then
        _test_ok=1
    fi

    teardown
    _remove_tmpdir

    if [ "${_test_ok}" = "1" ] && [ "${_test_fail}" = "${_old_fail}" ]; then
        _test_pass=$((_test_pass + 1))
        printf '%sok %d - %s%s\n' "${_c_green}" "${_test_total}" "${name}" "${_c_reset}"
    else
        # If function returned failure but _fail_test was not called, count it
        [ "${_test_fail}" = "${_old_fail}" ] && _test_fail=$((_test_fail + 1))
        printf '%snot ok %d - %s%s\n' "${_c_red}" "${_test_total}" "${name}" "${_c_reset}"
    fi
}

# --- Auto-discover and run all test_ functions ---
# Call this at the end of your test file.
run_tests() {
    script_file="${1:-${0}}"
    # Extract function names matching test_*
    _test_funcs="$(grep -oE '^test_[a-zA-Z0-9_]+[[:space:]]*\(' "${script_file}" | sed 's/[[:space:]]*($//')"
    for fn in ${_test_funcs}; do
        run_test "${fn}" "${fn}"
    done
    _print_summary
}

# --- Summary ---
_print_summary() {
    printf '\n# %d tests: %s%d passed%s, %s%d failed%s\n' \
        "${_test_total}" \
        "${_c_green}" "${_test_pass}" "${_c_reset}" \
        "${_c_red}" "${_test_fail}" "${_c_reset}"
    [ "${_test_fail}" -eq 0 ] || exit 1
}
