#!/bin/sh
# test-elinitctl.sh - tests for sbin/elinitctl
set -eu

cd "$(dirname "${0}")"
. ./testlib.sh

SCRIPT="$(cd .. && pwd)/elinitctl"

# We stub emacsclient and base64 in PATH to avoid needing a live Emacs server.
# Each test that needs stubs should call setup_stubs.

setup_stubs() {
    stub_dir="${TEST_TMPDIR}/stubs"
    mkdir -p "${stub_dir}"

    # Default emacsclient stub: returns a valid EXITCODE:BASE64 response.
    # "hello" in base64 is "aGVsbG8="
    cat > "${stub_dir}/emacsclient" <<'STUB'
#!/bin/sh
printf '"0:aGVsbG8="\n'
STUB
    chmod +x "${stub_dir}/emacsclient"

    # base64 stub: pass through to real base64
    if command -v base64 >/dev/null 2>&1; then
        ln -sf "$(command -v base64)" "${stub_dir}/base64"
    fi

    # sed stub: pass through to real sed
    if command -v sed >/dev/null 2>&1; then
        ln -sf "$(command -v sed)" "${stub_dir}/sed"
    fi

    # Scratch file for stubs that need to capture args
    stub_capture="${TEST_TMPDIR}/stub_capture"
}

# Run elinitctl with stubs in PATH
run_with_stubs() {
    PATH="${stub_dir}:${PATH}" run_cmd "${@}"
}

# Helper: create an arg-capturing emacsclient stub
make_capture_stub() {
    cat > "${stub_dir}/emacsclient" <<STUB
#!/bin/sh
for arg in "\$@"; do printf '%s\n' "\${arg}"; done > "${stub_capture}"
printf '"0:"\n'
STUB
    chmod +x "${stub_dir}/emacsclient"
}

# Helper: create an elisp-capturing emacsclient stub
make_elisp_capture_stub() {
    cat > "${stub_dir}/emacsclient" <<STUB
#!/bin/sh
for arg in "\$@"; do
    case "\${arg}" in
        "(elinit"*) printf '%s' "\${arg}" > "${stub_capture}" ;;
    esac
done
printf '"0:"\n'
STUB
    chmod +x "${stub_dir}/emacsclient"
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

# ===== Option parsing =====

test_help_exits_zero() {
    run_cmd "${SCRIPT}" --help
    assert_status "0" "${TEST_STATUS}" "--help exits 0"
    assert_contains "${TEST_STDOUT}" "Usage" "shows usage"
}

test_help_alias_h() {
    run_cmd "${SCRIPT}" -h
    assert_status "0" "${TEST_STATUS}" "-h exits 0"
    assert_contains "${TEST_STDOUT}" "Usage" "shows usage"
}

test_help_command() {
    run_cmd "${SCRIPT}" help
    assert_status "0" "${TEST_STATUS}" "help command exits 0"
    assert_contains "${TEST_STDOUT}" "Usage" "shows usage"
}

# ===== Missing-value validation =====

test_socket_missing_value_fails() {
    setup_stubs
    run_with_stubs "${SCRIPT}" --socket
    assert_status "2" "${TEST_STATUS}" "missing --socket value fails"
    assert_contains "${TEST_STDERR}" "requires" "error mentions requires"
}

test_server_file_missing_value_fails() {
    setup_stubs
    run_with_stubs "${SCRIPT}" --server-file
    assert_status "2" "${TEST_STATUS}" "missing --server-file value fails"
    assert_contains "${TEST_STDERR}" "requires" "error mentions requires"
}

test_timeout_missing_value_fails() {
    setup_stubs
    run_with_stubs "${SCRIPT}" --timeout
    assert_status "2" "${TEST_STATUS}" "missing --timeout value fails"
    assert_contains "${TEST_STDERR}" "requires" "error mentions requires"
}

# ===== Mutual exclusion =====

test_socket_and_server_file_exclusive() {
    setup_stubs
    run_with_stubs "${SCRIPT}" --socket foo --server-file bar status
    assert_status "2" "${TEST_STATUS}" "mutual exclusion exits 2"
    assert_contains "${TEST_STDERR}" "mutually exclusive" "error message"
}

# ===== TCP warning =====

test_server_file_tcp_warning() {
    setup_stubs
    run_with_stubs "${SCRIPT}" --server-file /tmp/test-server-file status
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_contains "${TEST_STDERR}" "TCP transport" "TCP warning emitted"
}

# ===== Server unavailable =====

test_server_unavailable_exit_code() {
    setup_stubs
    # Make emacsclient fail
    cat > "${stub_dir}/emacsclient" <<'STUB'
#!/bin/sh
printf 'emacsclient: cannot find socket\n' >&2
exit 1
STUB
    chmod +x "${stub_dir}/emacsclient"

    run_with_stubs "${SCRIPT}" status
    assert_status "69" "${TEST_STATUS}" "server unavailable exits 69"
    assert_contains "${TEST_STDERR}" "Cannot connect" "error message"
}

test_server_unavailable_forwards_output() {
    setup_stubs
    # emacsclient output is captured via 2>&1, then forwarded to stderr (L206)
    cat > "${stub_dir}/emacsclient" <<'STUB'
#!/bin/sh
printf 'emacsclient: server not running\n'
exit 1
STUB
    chmod +x "${stub_dir}/emacsclient"

    run_with_stubs "${SCRIPT}" status
    assert_status "69" "${TEST_STATUS}" "exits 69"
    assert_contains "${TEST_STDERR}" "server not running" "emacsclient output forwarded"
}

# ===== Dispatcher output parsing =====

test_valid_dispatch_response() {
    setup_stubs
    # Stub returns "0:aGVsbG8=" -> exit 0, output "hello"
    run_with_stubs "${SCRIPT}" status
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_eq "hello" "${TEST_STDOUT}" "decoded output matches"
}

test_nonzero_exit_code_forwarded() {
    setup_stubs
    # "ZmFpbGVk" = base64("failed")
    cat > "${stub_dir}/emacsclient" <<'STUB'
#!/bin/sh
printf '"1:ZmFpbGVk"\n'
STUB
    chmod +x "${stub_dir}/emacsclient"

    run_with_stubs "${SCRIPT}" status
    assert_status "1" "${TEST_STATUS}" "nonzero exit forwarded"
    assert_eq "failed" "${TEST_STDOUT}" "decoded output"
}

test_invalid_dispatch_output_fallback() {
    setup_stubs
    # Return something that is not EXITCODE:BASE64
    cat > "${stub_dir}/emacsclient" <<'STUB'
#!/bin/sh
printf '"garbage output without colon format"\n'
STUB
    chmod +x "${stub_dir}/emacsclient"

    run_with_stubs "${SCRIPT}" status
    assert_status "1" "${TEST_STATUS}" "invalid output fails"
    # L262: raw output printed to stdout as fallback
    assert_contains "${TEST_STDOUT}" "garbage output" "raw output forwarded"
}

# ===== Base64 decode failure =====

test_base64_decode_failure() {
    setup_stubs
    # Return valid exit code but make base64 -d fail
    cat > "${stub_dir}/emacsclient" <<'STUB'
#!/bin/sh
printf '"0:validlookingdata"\n'
STUB
    chmod +x "${stub_dir}/emacsclient"
    # Replace base64 symlink with a stub that fails on -d
    rm -f "${stub_dir}/base64"
    cat > "${stub_dir}/base64" <<'STUB'
#!/bin/sh
exit 1
STUB
    chmod +x "${stub_dir}/base64"

    run_with_stubs "${SCRIPT}" status
    assert_status "1" "${TEST_STATUS}" "decode failure exits 1"
    assert_contains "${TEST_STDERR}" "Failed to decode" "error mentions decode failure"
}

# ===== --json flag =====

test_json_flag_passed_through() {
    setup_stubs
    make_elisp_capture_stub

    run_with_stubs "${SCRIPT}" --json status
    assert_status "0" "${TEST_STATUS}" "exits 0"
    captured="$(cat "${stub_capture}")"
    assert_contains "${captured}" "--json" "json flag passed to dispatcher"
}

test_json_output_format() {
    setup_stubs
    # "eyJhIjoxfQ==" is base64 for '{"a":1}'
    cat > "${stub_dir}/emacsclient" <<'STUB'
#!/bin/sh
printf '"0:eyJhIjoxfQ=="\n'
STUB
    chmod +x "${stub_dir}/emacsclient"

    run_with_stubs "${SCRIPT}" --json status
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_eq '{"a":1}' "${TEST_STDOUT}" "JSON output correct"
}

# ===== --socket flag =====

test_socket_flag_accepted() {
    setup_stubs
    make_capture_stub

    run_with_stubs "${SCRIPT}" --socket mysock status
    assert_status "0" "${TEST_STATUS}" "exits 0"
    args="$(cat "${stub_capture}")"
    assert_contains "${args}" "-s" "socket flag present"
    assert_contains "${args}" "mysock" "socket name present"
}

test_socket_short_flag() {
    setup_stubs
    make_capture_stub

    run_with_stubs "${SCRIPT}" -s shortsock status
    assert_status "0" "${TEST_STATUS}" "exits 0"
    args="$(cat "${stub_capture}")"
    assert_contains "${args}" "-s" "socket flag present"
    assert_contains "${args}" "shortsock" "socket name present"
}

# ===== --server-file flag =====

test_server_file_flag_accepted() {
    setup_stubs
    make_capture_stub

    run_with_stubs "${SCRIPT}" --server-file /tmp/server status
    assert_status "0" "${TEST_STATUS}" "exits 0"
    args="$(cat "${stub_capture}")"
    assert_contains "${args}" "-f" "server-file flag present"
    assert_contains "${args}" "/tmp/server" "server-file path present"
}

test_server_file_short_flag() {
    setup_stubs
    make_capture_stub

    run_with_stubs "${SCRIPT}" -f /tmp/server2 status
    assert_status "0" "${TEST_STATUS}" "exits 0"
    args="$(cat "${stub_capture}")"
    assert_contains "${args}" "-f" "server-file flag present"
    assert_contains "${args}" "/tmp/server2" "server-file path present"
}

# ===== --timeout flag =====

test_timeout_flag_accepted() {
    setup_stubs
    make_capture_stub

    run_with_stubs "${SCRIPT}" --timeout 5 status
    assert_status "0" "${TEST_STATUS}" "exits 0"
    args="$(cat "${stub_capture}")"
    assert_contains "${args}" "-w" "timeout flag present"
    assert_contains "${args}" "5" "timeout value present"
}

test_timeout_short_flag() {
    setup_stubs
    make_capture_stub

    run_with_stubs "${SCRIPT}" -t 3 status
    assert_status "0" "${TEST_STATUS}" "exits 0"
    args="$(cat "${stub_capture}")"
    assert_contains "${args}" "-w" "timeout flag present"
    assert_contains "${args}" "3" "timeout value present"
}

# ===== Unknown options passed through to dispatcher =====

test_unknown_flag_passed_to_dispatcher() {
    setup_stubs
    make_elisp_capture_stub

    run_with_stubs "${SCRIPT}" --unknown-flag value status
    assert_status "0" "${TEST_STATUS}" "exits 0"
    captured="$(cat "${stub_capture}")"
    assert_contains "${captured}" "--unknown-flag" "unknown flag in dispatcher args"
    assert_contains "${captured}" "value" "flag value in dispatcher args"
}

# ===== Missing required commands =====

test_missing_emacsclient() {
    no_ec_dir="${TEST_TMPDIR}/no-ec"
    mkdir -p "${no_ec_dir}"
    # Symlink every needed command EXCEPT emacsclient into an isolated dir.
    for cmd in base64 sed printf cat rm mktemp sh; do
        real="$(command -v "${cmd}" 2>/dev/null)" || continue
        ln -sf "${real}" "${no_ec_dir}/${cmd}"
    done
    # Use ONLY this dir in PATH so emacsclient cannot be found.
    PATH="${no_ec_dir}" run_cmd "${SCRIPT}" status
    assert_status "1" "${TEST_STATUS}" "missing emacsclient fails"
    assert_contains "${TEST_STDERR}" "emacsclient" "error mentions emacsclient"
}

test_missing_base64() {
    no_b64_dir="${TEST_TMPDIR}/no-b64"
    mkdir -p "${no_b64_dir}"
    # Create a dummy emacsclient (may not exist on CI runners without Emacs)
    printf '#!/bin/sh\nprintf "\"0:aGVsbG8=\"\\n"\n' > "${no_b64_dir}/emacsclient"
    chmod +x "${no_b64_dir}/emacsclient"
    for cmd in sed printf cat rm mktemp sh; do
        real="$(command -v "${cmd}" 2>/dev/null)" || continue
        ln -sf "${real}" "${no_b64_dir}/${cmd}"
    done
    PATH="${no_b64_dir}" run_cmd "${SCRIPT}" status
    assert_status "1" "${TEST_STATUS}" "missing base64 fails"
    assert_contains "${TEST_STDERR}" "base64" "error mentions base64"
}

test_missing_sed() {
    no_sed_dir="${TEST_TMPDIR}/no-sed"
    mkdir -p "${no_sed_dir}"
    # Create a dummy emacsclient (may not exist on CI runners without Emacs)
    printf '#!/bin/sh\nprintf "\"0:aGVsbG8=\"\\n"\n' > "${no_sed_dir}/emacsclient"
    chmod +x "${no_sed_dir}/emacsclient"
    for cmd in base64 printf cat rm mktemp sh; do
        real="$(command -v "${cmd}" 2>/dev/null)" || continue
        ln -sf "${real}" "${no_sed_dir}/${cmd}"
    done
    PATH="${no_sed_dir}" run_cmd "${SCRIPT}" status
    assert_status "1" "${TEST_STATUS}" "missing sed fails"
    assert_contains "${TEST_STDERR}" "sed" "error mentions sed"
}

# ===== Argument escaping =====

test_special_chars_in_args() {
    setup_stubs
    make_elisp_capture_stub

    run_with_stubs "${SCRIPT}" cat 'unit-with"quotes'
    assert_status "0" "${TEST_STATUS}" "special chars handled"
    captured="$(cat "${stub_capture}")"
    assert_contains "${captured}" 'unit-with\"quotes' "quotes escaped for elisp"
}

test_backslash_in_args() {
    setup_stubs
    make_elisp_capture_stub

    run_with_stubs "${SCRIPT}" cat 'unit\path'
    assert_status "0" "${TEST_STATUS}" "backslash handled"
    captured="$(cat "${stub_capture}")"
    assert_contains "${captured}" 'unit\\path' "backslash escaped for elisp"
}

# ===== FOLLOW protocol =====

test_follow_protocol_initial_output() {
    command -v timeout >/dev/null 2>&1 || {
        printf '  SKIP: timeout not available\n'
        return 0
    }
    setup_stubs
    follow_file="${TEST_TMPDIR}/follow.log"
    printf 'streamed line 1\nstreamed line 2\n' > "${follow_file}"

    # Also need tail and kill in the stub PATH
    for cmd in tail kill; do
        real="$(command -v "${cmd}" 2>/dev/null)" || continue
        ln -sf "${real}" "${stub_dir}/${cmd}"
    done

    # "initial" base64-encoded is "aW5pdGlhbA=="
    cat > "${stub_dir}/emacsclient" <<STUB
#!/bin/sh
# Cleanup calls have "journal-follow-stop" in args
case "\$*" in
    *journal-follow-stop*) exit 0 ;;
esac
printf '"FOLLOW:aW5pdGlhbA==:${follow_file}:test-session"\n'
STUB
    chmod +x "${stub_dir}/emacsclient"

    # Run with timeout to prevent tail -f from blocking forever.
    stdout_file="${TEST_TMPDIR}/follow_stdout"
    stderr_file="${TEST_TMPDIR}/follow_stderr"
    PATH="${stub_dir}:${PATH}" timeout 2 "${SCRIPT}" journal -f -u svc \
        >"${stdout_file}" 2>"${stderr_file}" || true

    follow_out="$(cat "${stdout_file}")"
    assert_contains "${follow_out}" "initial" "initial output decoded and printed"
    assert_contains "${follow_out}" "streamed line 1" "follow file content streamed"
}

test_follow_decode_failure() {
    command -v timeout >/dev/null 2>&1 || {
        printf '  SKIP: timeout not available\n'
        return 0
    }
    setup_stubs
    follow_file="${TEST_TMPDIR}/follow.log"
    printf 'data\n' > "${follow_file}"

    for cmd in tail kill; do
        real="$(command -v "${cmd}" 2>/dev/null)" || continue
        ln -sf "${real}" "${stub_dir}/${cmd}"
    done

    # base64 stub that always fails on decode (L228-230)
    rm -f "${stub_dir}/base64"
    cat > "${stub_dir}/base64" <<'STUB'
#!/bin/sh
exit 1
STUB
    chmod +x "${stub_dir}/base64"

    cat > "${stub_dir}/emacsclient" <<STUB
#!/bin/sh
printf '"FOLLOW:baddata:${follow_file}:test-session"\n'
STUB
    chmod +x "${stub_dir}/emacsclient"

    stdout_file="${TEST_TMPDIR}/follow_stdout"
    stderr_file="${TEST_TMPDIR}/follow_stderr"
    PATH="${stub_dir}:${PATH}" timeout 2 "${SCRIPT}" journal -f -u svc \
        >"${stdout_file}" 2>"${stderr_file}" || true

    follow_err="$(cat "${stderr_file}")"
    assert_contains "${follow_err}" "Failed to decode initial" "follow decode failure caught"
}

# ===== Empty output =====

test_empty_output_decoded() {
    setup_stubs
    cat > "${stub_dir}/emacsclient" <<'STUB'
#!/bin/sh
printf '"0:"\n'
STUB
    chmod +x "${stub_dir}/emacsclient"

    run_with_stubs "${SCRIPT}" ping
    assert_status "0" "${TEST_STATUS}" "empty output exits 0"
    assert_eq "" "${TEST_STDOUT}" "empty output is empty"
}

# ===== Multiple args build =====

test_multiple_args_built() {
    setup_stubs
    make_elisp_capture_stub

    run_with_stubs "${SCRIPT}" start -- svc1 svc2 svc3
    assert_status "0" "${TEST_STATUS}" "exits 0"
    captured="$(cat "${stub_capture}")"
    assert_contains "${captured}" '"start"' "command in args"
    assert_contains "${captured}" '"svc1"' "first ID in args"
    assert_contains "${captured}" '"svc3"' "last ID in args"
}

run_tests "$(basename "${0}")"
