#!/bin/sh
# test-supervisor-import.sh - tests for sbin/supervisor-import
set -eu

cd "$(dirname "${0}")"
. ./testlib.sh

SCRIPT="$(cd .. && pwd)/supervisor-import"

# Helper: create a minimal .service file in TEST_TMPDIR
make_service() {
    _name="${1}"
    _content="${2}"
    _path="${TEST_TMPDIR}/${_name}.service"
    printf '%s\n' "${_content}" > "${_path}"
    printf '%s' "${_path}"
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

# ===== Help =====

test_help_exits_zero() {
    run_cmd "${SCRIPT}" --help
    assert_status "0" "${TEST_STATUS}" "--help exits 0"
    assert_contains "${TEST_STDOUT}" "Usage" "--help shows usage"
}

test_help_short_flag() {
    run_cmd "${SCRIPT}" -h
    assert_status "0" "${TEST_STATUS}" "-h exits 0"
    assert_contains "${TEST_STDOUT}" "Usage" "-h shows usage"
}

# ===== Argument errors =====

test_missing_file_arg() {
    run_cmd "${SCRIPT}"
    assert_status "2" "${TEST_STATUS}" "missing file arg exits 2"
    assert_contains "${TEST_STDERR}" "missing input file" "error message"
}

test_file_not_found() {
    run_cmd "${SCRIPT}" "/nonexistent/foo.service"
    assert_status "1" "${TEST_STATUS}" "not found exits 1"
    assert_contains "${TEST_STDERR}" "file not found" "error message"
}

test_output_dir_missing_value() {
    _f="$(make_service "dummy" "[Service]
ExecStart=/bin/true")"
    run_cmd "${SCRIPT}" --output-dir
    assert_status "2" "${TEST_STATUS}" "missing value exits 2"
}

test_output_dir_not_directory() {
    _f="$(make_service "dummy" "[Service]
ExecStart=/bin/true")"
    run_cmd "${SCRIPT}" --output-dir "/nonexistent/dir" "${_f}"
    assert_status "2" "${TEST_STATUS}" "not a directory exits 2"
    assert_contains "${TEST_STDERR}" "not a directory" "error message"
}

# ===== Full examples from README =====

test_simple_daemon() {
    _f="$(make_service "my-daemon" "[Unit]
Description=My Background Service
After=network-ready.service

[Service]
Type=simple
ExecStart=/usr/bin/my-daemon --config /etc/my-daemon.conf
Restart=on-failure
RestartSec=5
WorkingDirectory=/var/lib/my-daemon

[Install]
WantedBy=multi-user.target")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_contains "${TEST_STDOUT}" ':id "my-daemon"' "id"
    assert_contains "${TEST_STDOUT}" ':command "/usr/bin/my-daemon --config /etc/my-daemon.conf"' "command"
    assert_contains "${TEST_STDOUT}" ':description "My Background Service"' "description"
    assert_contains "${TEST_STDOUT}" ':type simple' "type"
    assert_contains "${TEST_STDOUT}" ':wanted-by ("multi-user.target")' "wanted-by"
    assert_contains "${TEST_STDOUT}" ':after ("network-ready")' "after strips .service"
    assert_contains "${TEST_STDOUT}" ':restart on-failure' "restart"
    assert_contains "${TEST_STDOUT}" ':restart-sec 5' "restart-sec"
    assert_contains "${TEST_STDOUT}" ':working-directory "/var/lib/my-daemon"' "working-directory"
    assert_contains "${TEST_STDOUT}" ':enabled t)' "enabled"
}

test_oneshot_service() {
    _f="$(make_service "setup-dirs" "[Unit]
Description=Create runtime directories

[Service]
Type=oneshot
ExecStart=/usr/bin/mkdir -p /run/myapp
RemainAfterExit=yes

[Install]
WantedBy=multi-user.target")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_contains "${TEST_STDOUT}" ':id "setup-dirs"' "id"
    assert_contains "${TEST_STDOUT}" ':type oneshot' "type"
    assert_contains "${TEST_STDOUT}" ':remain-after-exit t' "remain-after-exit"
}

test_service_with_deps_and_env() {
    _f="$(make_service "webapp" "[Unit]
Description=Web Application
After=database.service
Requires=database.service

[Service]
Type=simple
ExecStart=/usr/bin/node /opt/webapp/server.js
Environment=NODE_ENV=production
Environment=PORT=3000
Restart=always
ExecStop=/usr/bin/pkill -TERM -f /opt/webapp/server.js
KillSignal=SIGTERM

[Install]
WantedBy=multi-user.target")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_contains "${TEST_STDOUT}" ':after ("database")' "after"
    assert_contains "${TEST_STDOUT}" ':requires ("database")' "requires"
    assert_contains "${TEST_STDOUT}" '"NODE_ENV" . "production"' "env NODE_ENV"
    assert_contains "${TEST_STDOUT}" '"PORT" . "3000"' "env PORT"
    assert_contains "${TEST_STDOUT}" ':restart always' "restart"
    assert_contains "${TEST_STDOUT}" ':exec-stop "/usr/bin/pkill -TERM -f /opt/webapp/server.js"' "exec-stop"
    assert_contains "${TEST_STDOUT}" ':kill-signal SIGTERM' "kill-signal"
}

test_privilege_drop() {
    _f="$(make_service "nginx" "[Unit]
Description=Web server

[Service]
Type=simple
ExecStart=/usr/bin/nginx -g \"daemon off;\"
User=www-data
Group=www-data
Restart=on-failure

[Install]
WantedBy=multi-user.target")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_contains "${TEST_STDOUT}" ':user "www-data"' "user"
    assert_contains "${TEST_STDOUT}" ':group "www-data"' "group"
}

# ===== Individual directive tests =====

test_id_from_filename() {
    _f="$(make_service "my-cool-service" "[Service]
ExecStart=/bin/true")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_contains "${TEST_STDOUT}" ':id "my-cool-service"' "id from filename"
}

test_type_defaults_to_simple() {
    _f="$(make_service "no-type" "[Service]
ExecStart=/bin/true")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_contains "${TEST_STDOUT}" ':type simple' "default type is simple"
}

test_type_oneshot() {
    _f="$(make_service "oneshot" "[Service]
Type=oneshot
ExecStart=/bin/true")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_contains "${TEST_STDOUT}" ':type oneshot' "type oneshot"
}

test_type_forking_warns() {
    _f="$(make_service "forking" "[Service]
Type=forking
ExecStart=/bin/true")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_contains "${TEST_STDERR}" "not directly supported" "forking warns"
    assert_contains "${TEST_STDOUT}" ':type simple' "defaults to simple"
}

test_restart_values() {
    for _rv in always on-failure on-success; do
        _f="$(make_service "restart-${_rv}" "[Service]
ExecStart=/bin/true
Restart=${_rv}")"
        run_cmd "${SCRIPT}" "${_f}"
        assert_contains "${TEST_STDOUT}" ":restart ${_rv}" "restart ${_rv}"
    done
}

test_restart_sec_strip_suffix() {
    _f="$(make_service "rsec" "[Service]
ExecStart=/bin/true
RestartSec=10s")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_contains "${TEST_STDOUT}" ':restart-sec 10' "strips s suffix"
}

test_after_strips_service_suffix() {
    _f="$(make_service "after-svc" "[Service]
ExecStart=/bin/true

[Unit]
After=network.service")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_contains "${TEST_STDOUT}" ':after ("network")' "strips .service"
}

test_after_keeps_target_suffix() {
    _f="$(make_service "after-tgt" "[Service]
ExecStart=/bin/true

[Unit]
After=network.target")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_contains "${TEST_STDOUT}" ':after ("network.target")' "keeps .target"
}

test_after_multiple_values() {
    _f="$(make_service "after-multi" "[Unit]
After=foo.service bar.service baz.target

[Service]
ExecStart=/bin/true")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_contains "${TEST_STDOUT}" ':after ("foo" "bar" "baz.target")' "multiple after values"
}

test_requires_space_separated() {
    _f="$(make_service "reqs" "[Unit]
Requires=alpha.service beta.service

[Service]
ExecStart=/bin/true")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_contains "${TEST_STDOUT}" ':requires ("alpha" "beta")' "requires space-separated"
}

test_wants_directive() {
    _f="$(make_service "wants" "[Unit]
Wants=optional.service

[Service]
ExecStart=/bin/true")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_contains "${TEST_STDOUT}" ':wants ("optional")' "wants"
}

test_before_directive() {
    _f="$(make_service "before" "[Unit]
Before=later.service

[Service]
ExecStart=/bin/true")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_contains "${TEST_STDOUT}" ':before ("later")' "before"
}

test_environment_single() {
    _f="$(make_service "env1" "[Service]
ExecStart=/bin/true
Environment=FOO=bar")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_contains "${TEST_STDOUT}" '"FOO" . "bar"' "single env"
}

test_environment_multiple_lines() {
    _f="$(make_service "envn" "[Service]
ExecStart=/bin/true
Environment=FOO=bar
Environment=BAZ=qux")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_contains "${TEST_STDOUT}" '"FOO" . "bar"' "first env"
    assert_contains "${TEST_STDOUT}" '"BAZ" . "qux"' "second env"
}

test_environment_file() {
    _f="$(make_service "envfile" "[Service]
ExecStart=/bin/true
EnvironmentFile=/etc/default/myapp")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_contains "${TEST_STDOUT}" ':environment-file "/etc/default/myapp"' "env file"
}

test_environment_file_strips_dash() {
    _f="$(make_service "envfile-dash" "[Service]
ExecStart=/bin/true
EnvironmentFile=-/etc/default/myapp")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_contains "${TEST_STDOUT}" ':environment-file "/etc/default/myapp"' "strips dash"
}

test_exec_stop() {
    _f="$(make_service "es" "[Service]
ExecStart=/bin/true
ExecStop=/usr/bin/kill-it")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_contains "${TEST_STDOUT}" ':exec-stop "/usr/bin/kill-it"' "exec-stop"
}

test_exec_reload() {
    _f="$(make_service "er" "[Service]
ExecStart=/bin/true
ExecReload=/bin/kill -HUP \$MAINPID")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_contains "${TEST_STDOUT}" ':exec-reload' "exec-reload present"
}

test_kill_signal() {
    _f="$(make_service "ks" "[Service]
ExecStart=/bin/true
KillSignal=SIGQUIT")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_contains "${TEST_STDOUT}" ':kill-signal SIGQUIT' "kill-signal"
}

test_kill_mode_process() {
    _f="$(make_service "km-proc" "[Service]
ExecStart=/bin/true
KillMode=process")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_contains "${TEST_STDOUT}" ':kill-mode process' "kill-mode process"
}

test_kill_mode_mixed() {
    _f="$(make_service "km-mixed" "[Service]
ExecStart=/bin/true
KillMode=mixed")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_contains "${TEST_STDOUT}" ':kill-mode mixed' "kill-mode mixed"
}

test_remain_after_exit() {
    _f="$(make_service "rae" "[Service]
Type=oneshot
ExecStart=/bin/true
RemainAfterExit=yes")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_contains "${TEST_STDOUT}" ':remain-after-exit t' "remain-after-exit"
}

test_user_group() {
    _f="$(make_service "ug" "[Service]
ExecStart=/bin/true
User=nobody
Group=nogroup")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_contains "${TEST_STDOUT}" ':user "nobody"' "user"
    assert_contains "${TEST_STDOUT}" ':group "nogroup"' "group"
}

test_wanted_by_multiple() {
    _f="$(make_service "wbm" "[Install]
WantedBy=multi-user.target graphical.target

[Service]
ExecStart=/bin/true")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_contains "${TEST_STDOUT}" ':wanted-by ("multi-user.target" "graphical.target")' "multiple wanted-by"
}

test_missing_exec_start() {
    _f="$(make_service "no-exec" "[Unit]
Description=No command

[Service]
Type=simple")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_status "1" "${TEST_STATUS}" "missing ExecStart exits 1"
    assert_contains "${TEST_STDERR}" "missing ExecStart" "error message"
}

test_unknown_directives_warn() {
    _f="$(make_service "unk" "[Service]
ExecStart=/bin/true
LimitNOFILE=65536")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_status "0" "${TEST_STATUS}" "still succeeds"
    assert_contains "${TEST_STDERR}" "unknown directive" "warns on unknown"
    assert_contains "${TEST_STDERR}" "LimitNOFILE" "mentions directive name"
}

test_comments_and_blanks_skipped() {
    _f="$(make_service "svc" "# This is a comment

[Service]
ExecStart=/bin/true
# Another comment")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_not_contains "${TEST_STDOUT}" "# This is" "comments not in output"
    assert_not_contains "${TEST_STDOUT}" "# Another" "comments not in output"
}

test_section_headers_skipped() {
    _f="$(make_service "sections" "[Unit]
Description=Foo

[Service]
ExecStart=/bin/true

[Install]
WantedBy=multi-user.target")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_not_contains "${TEST_STDOUT}" "[Unit]" "no section headers"
    assert_not_contains "${TEST_STDOUT}" "[Service]" "no section headers"
    assert_not_contains "${TEST_STDOUT}" "[Install]" "no section headers"
}

test_exec_start_strips_dash() {
    _f="$(make_service "dash" "[Service]
ExecStart=-/usr/bin/maybe-fail")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_contains "${TEST_STDOUT}" ':command "/usr/bin/maybe-fail"' "strips leading dash"
}

test_output_dir_creates_file() {
    _outdir="${TEST_TMPDIR}/out"
    mkdir -p "${_outdir}"
    _f="$(make_service "outtest" "[Service]
ExecStart=/bin/true")"
    run_cmd "${SCRIPT}" --output-dir "${_outdir}" "${_f}"
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_file_exists "${_outdir}/outtest.el" "output file created"
    _content="$(cat "${_outdir}/outtest.el")"
    assert_contains "${_content}" ':id "outtest"' "file has correct content"
}

test_output_is_valid_plist_shape() {
    _f="$(make_service "shape" "[Service]
ExecStart=/bin/true")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_status "0" "${TEST_STATUS}" "exits 0"
    # Must start with (: and end with )
    assert_match "${TEST_STDOUT}" '^\(:id' "starts with (:id"
    assert_match "${TEST_STDOUT}" ':enabled t\)$' "ends with :enabled t)"
}

test_enabled_always_present() {
    _f="$(make_service "enabled" "[Service]
ExecStart=/bin/true")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_contains "${TEST_STDOUT}" ':enabled t' "enabled always present"
}

test_success_exit_status() {
    _f="$(make_service "ses" "[Service]
ExecStart=/bin/true
SuccessExitStatus=0 1 75")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_contains "${TEST_STDOUT}" ':success-exit-status (0 1 75)' "success-exit-status"
}

test_required_by() {
    _f="$(make_service "reqby" "[Install]
RequiredBy=multi-user.target

[Service]
ExecStart=/bin/true")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_contains "${TEST_STDOUT}" ':required-by ("multi-user.target")' "required-by"
}

test_dry_run_does_not_write() {
    _outdir="${TEST_TMPDIR}/dryout"
    mkdir -p "${_outdir}"
    _f="$(make_service "drytest" "[Service]
ExecStart=/bin/true")"
    run_cmd "${SCRIPT}" --dry-run --output-dir "${_outdir}" "${_f}"
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_file_not_exists "${_outdir}/drytest.el" "file not written in dry-run"
    assert_contains "${TEST_STDOUT}" "Would write" "dry-run message"
    assert_contains "${TEST_STDOUT}" ':id "drytest"' "still shows output"
}

test_kill_mode_control_group_warns() {
    _f="$(make_service "km-cg" "[Service]
ExecStart=/bin/true
KillMode=control-group")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_status "0" "${TEST_STATUS}" "still succeeds"
    assert_contains "${TEST_STDERR}" "not supported" "warns on control-group"
    assert_not_contains "${TEST_STDOUT}" ':kill-mode' "kill-mode not in output"
}

test_restart_no_omitted() {
    _f="$(make_service "rno" "[Service]
ExecStart=/bin/true
Restart=no")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_not_contains "${TEST_STDOUT}" ':restart' "Restart=no omitted from output"
}

test_exec_start_strips_plus_prefix() {
    _f="$(make_service "plus" "[Service]
ExecStart=+/usr/bin/privtool")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_contains "${TEST_STDOUT}" ':command "/usr/bin/privtool"' "strips + prefix"
}

test_exec_start_strips_bang_prefix() {
    _f="$(make_service "bang" "[Service]
ExecStart=!!/usr/bin/ambienttool")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_contains "${TEST_STDOUT}" ':command "/usr/bin/ambienttool"' "strips !! prefix"
}

test_environment_quoted_value() {
    _f="$(make_service "envq" "[Service]
ExecStart=/bin/true
Environment=\"FOO=bar baz\"")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_contains "${TEST_STDOUT}" '"FOO" . "bar baz"' "quoted env value"
}

test_skipped_summary_printed() {
    _f="$(make_service "skip" "[Service]
ExecStart=/bin/true
LimitNOFILE=65536
CapabilityBoundingSet=CAP_NET_BIND")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_contains "${TEST_STDERR}" "Skipped directives" "summary header"
    assert_contains "${TEST_STDERR}" "LimitNOFILE" "mentions skipped directive"
    assert_contains "${TEST_STDERR}" "CapabilityBoundingSet" "mentions second skipped"
}

test_no_skipped_summary_when_clean() {
    _f="$(make_service "clean" "[Service]
ExecStart=/bin/true")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_not_contains "${TEST_STDERR}" "Skipped" "no summary when nothing skipped"
}

test_unsupported_dep_type_skipped() {
    _f="$(make_service "sockdep" "[Unit]
After=foo.socket bar.service

[Service]
ExecStart=/bin/true")"
    run_cmd "${SCRIPT}" "${_f}"
    assert_status "0" "${TEST_STATUS}" "exits 0"
    assert_contains "${TEST_STDERR}" "unsupported dependency type" "warns on .socket"
    assert_contains "${TEST_STDOUT}" ':after ("bar")' "keeps valid dep, drops socket"
}

# --- Run all tests ---
run_tests "$(basename "${0}")"
