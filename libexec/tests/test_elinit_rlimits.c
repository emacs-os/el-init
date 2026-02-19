/*
 * test_elinit_rlimits.c -- tests for the elinit-rlimits helper
 */

#include "vendor/acutest.h"
#include "test_helpers.h"

#include <stdio.h>
#include <string.h>
#include <sys/resource.h>

#ifndef RLIMITS_PATH
#error "RLIMITS_PATH must be defined at compile time"
#endif

/* --- Argument parsing --- */

void test_no_args(void)
{
	const char *argv[] = { RLIMITS_PATH, NULL };
	struct run_result r = {0};
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 111);
	TEST_CHECK(strstr(r.err, "usage") != NULL);
	run_result_free(&r);
}

void test_missing_separator(void)
{
	const char *argv[] = { RLIMITS_PATH, "--nofile", "1024:1024", NULL };
	struct run_result r = {0};
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 111);
	TEST_CHECK(strstr(r.err, "separator") != NULL ||
	           strstr(r.err, "command") != NULL);
	run_result_free(&r);
}

void test_missing_command_after_separator(void)
{
	const char *argv[] = { RLIMITS_PATH, "--nofile", "1024:1024",
	                        "--", NULL };
	struct run_result r = {0};
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 111);
	run_result_free(&r);
}

void test_no_limits_specified(void)
{
	const char *argv[] = { RLIMITS_PATH, "--", "true", NULL };
	struct run_result r = {0};
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 111);
	TEST_CHECK(strstr(r.err, "no limits") != NULL);
	run_result_free(&r);
}

void test_unknown_option(void)
{
	const char *argv[] = { RLIMITS_PATH, "--bogus", "42:42",
	                        "--", "true", NULL };
	struct run_result r = {0};
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 111);
	TEST_CHECK(strstr(r.err, "unknown") != NULL);
	run_result_free(&r);
}

void test_missing_limit_value(void)
{
	const char *argv[] = { RLIMITS_PATH, "--nofile", "--", "true", NULL };
	struct run_result r = {0};
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	/* --nofile sees "--" as its argument value which is invalid */
	TEST_CHECK(r.exit_code == 111);
	run_result_free(&r);
}

/* --- Invalid limit values --- */

void test_invalid_limit_no_colon(void)
{
	const char *argv[] = { RLIMITS_PATH, "--nofile", "1024",
	                        "--", "true", NULL };
	struct run_result r = {0};
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 111);
	TEST_CHECK(strstr(r.err, "invalid") != NULL);
	run_result_free(&r);
}

void test_invalid_limit_negative(void)
{
	const char *argv[] = { RLIMITS_PATH, "--nproc", "-1:100",
	                        "--", "true", NULL };
	struct run_result r = {0};
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 111);
	run_result_free(&r);
}

void test_invalid_limit_text(void)
{
	const char *argv[] = { RLIMITS_PATH, "--core", "abc:def",
	                        "--", "true", NULL };
	struct run_result r = {0};
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 111);
	run_result_free(&r);
}

void test_invalid_limit_empty_soft(void)
{
	const char *argv[] = { RLIMITS_PATH, "--fsize", ":100",
	                        "--", "true", NULL };
	struct run_result r = {0};
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 111);
	run_result_free(&r);
}

void test_invalid_limit_empty_hard(void)
{
	const char *argv[] = { RLIMITS_PATH, "--as", "100:",
	                        "--", "true", NULL };
	struct run_result r = {0};
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 111);
	run_result_free(&r);
}

/* --- Successful exec --- */

void test_nofile_exec_true(void)
{
	const char *argv[] = { RLIMITS_PATH, "--nofile", "1024:4096",
	                        "--", "true", NULL };
	struct run_result r = {0};
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 0);
	run_result_free(&r);
}

void test_nproc_exec_true(void)
{
	const char *argv[] = { RLIMITS_PATH, "--nproc", "512:1024",
	                        "--", "true", NULL };
	struct run_result r = {0};
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 0);
	run_result_free(&r);
}

void test_core_exec_true(void)
{
	const char *argv[] = { RLIMITS_PATH, "--core", "0:0",
	                        "--", "true", NULL };
	struct run_result r = {0};
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 0);
	run_result_free(&r);
}

void test_fsize_exec_true(void)
{
	const char *argv[] = { RLIMITS_PATH, "--fsize", "1048576:1048576",
	                        "--", "true", NULL };
	struct run_result r = {0};
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 0);
	run_result_free(&r);
}

void test_as_exec_true(void)
{
	const char *argv[] = { RLIMITS_PATH, "--as", "1073741824:1073741824",
	                        "--", "true", NULL };
	struct run_result r = {0};
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 0);
	run_result_free(&r);
}

void test_infinity_values(void)
{
	/* Use core limit for infinity test since nofile infinity
	 * requires root on most systems. */
	const char *argv[] = { RLIMITS_PATH, "--core", "infinity:infinity",
	                        "--", "true", NULL };
	struct run_result r = {0};
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 0);
	run_result_free(&r);
}

void test_mixed_infinity(void)
{
	const char *argv[] = { RLIMITS_PATH, "--core", "0:infinity",
	                        "--", "true", NULL };
	struct run_result r = {0};
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 0);
	run_result_free(&r);
}

void test_multiple_limits(void)
{
	const char *argv[] = { RLIMITS_PATH,
	                        "--nofile", "1024:4096",
	                        "--nproc", "256:512",
	                        "--core", "0:0",
	                        "--", "true", NULL };
	struct run_result r = {0};
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 0);
	run_result_free(&r);
}

void test_all_limits(void)
{
	const char *argv[] = { RLIMITS_PATH,
	                        "--nofile", "1024:4096",
	                        "--nproc", "256:512",
	                        "--core", "0:0",
	                        "--fsize", "1048576:1048576",
	                        "--as", "infinity:infinity",
	                        "--", "true", NULL };
	struct run_result r = {0};
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 0);
	run_result_free(&r);
}

/* --- Verify limits are applied (use sh -c 'ulimit -n') --- */

void test_nofile_applied(void)
{
	const char *argv[] = { RLIMITS_PATH, "--nofile", "1234:4096",
	                        "--", "sh", "-c", "ulimit -n", NULL };
	struct run_result r = {0};
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 0);
	/* ulimit -n prints the soft limit */
	TEST_CHECK(strstr(r.out, "1234") != NULL);
	TEST_MSG("stdout: %s", r.out);
	run_result_free(&r);
}

void test_nproc_applied(void)
{
	/* Query current hard limit and set to half of it.  We cannot
	   raise nproc above the hard limit without root, and values
	   below the running process count prevent fork. */
	struct rlimit rl;
	if (getrlimit(RLIMIT_NPROC, &rl) != 0 ||
	    rl.rlim_max == RLIM_INFINITY || rl.rlim_max < 200) {
		TEST_MSG("skipping: cannot determine safe nproc value");
		return;
	}
	rlim_t val = rl.rlim_max / 2;
	char buf[64];
	char expect[64];
	snprintf(buf, sizeof(buf), "%llu:%llu",
	         (unsigned long long)val, (unsigned long long)val);
	snprintf(expect, sizeof(expect), "%llu", (unsigned long long)val);
	const char *argv[] = { RLIMITS_PATH, "--nproc", buf,
	                        "--", "sh", "-c", "ulimit -u", NULL };
	struct run_result r = {0};
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 0);
	TEST_CHECK(strstr(r.out, expect) != NULL);
	TEST_MSG("stdout: %s, expected: %s", r.out, expect);
	run_result_free(&r);
}

void test_core_applied(void)
{
	const char *argv[] = { RLIMITS_PATH, "--core", "0:0",
	                        "--", "sh", "-c", "ulimit -c", NULL };
	struct run_result r = {0};
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 0);
	TEST_CHECK(strstr(r.out, "0") != NULL);
	TEST_MSG("stdout: %s", r.out);
	run_result_free(&r);
}

/* --- Exec failure --- */

void test_exec_nonexistent(void)
{
	const char *argv[] = { RLIMITS_PATH, "--nofile", "1024:1024",
	                        "--", "/nonexistent-binary-xyz", NULL };
	struct run_result r = {0};
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 114);
	TEST_CHECK(strstr(r.err, "exec") != NULL);
	run_result_free(&r);
}

/* --- setrlimit failure (exit 112) --- */

void test_setrlimit_failure_soft_exceeds_hard(void)
{
	/* soft > hard is EINVAL on all POSIX systems */
	const char *argv[] = { RLIMITS_PATH, "--nofile", "9999:1",
	                        "--", "true", NULL };
	struct run_result r = {0};
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 112);
	TEST_CHECK(strstr(r.err, "setrlimit") != NULL);
	TEST_MSG("exit_code: %d, stderr: %s", r.exit_code, r.err);
	run_result_free(&r);
}

/* --- Command exit code pass-through --- */

void test_exit_code_passthrough(void)
{
	const char *argv[] = { RLIMITS_PATH, "--nofile", "1024:1024",
	                        "--", "sh", "-c", "exit 42", NULL };
	struct run_result r = {0};
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 42);
	run_result_free(&r);
}


TEST_LIST = {
	/* Argument parsing */
	{ "no_args",                     test_no_args },
	{ "missing_separator",           test_missing_separator },
	{ "missing_command",             test_missing_command_after_separator },
	{ "no_limits_specified",         test_no_limits_specified },
	{ "unknown_option",              test_unknown_option },
	{ "missing_limit_value",         test_missing_limit_value },
	/* Invalid limit values */
	{ "invalid_no_colon",            test_invalid_limit_no_colon },
	{ "invalid_negative",            test_invalid_limit_negative },
	{ "invalid_text",                test_invalid_limit_text },
	{ "invalid_empty_soft",          test_invalid_limit_empty_soft },
	{ "invalid_empty_hard",          test_invalid_limit_empty_hard },
	/* Successful exec */
	{ "nofile_exec",                 test_nofile_exec_true },
	{ "nproc_exec",                  test_nproc_exec_true },
	{ "core_exec",                   test_core_exec_true },
	{ "fsize_exec",                  test_fsize_exec_true },
	{ "as_exec",                     test_as_exec_true },
	{ "infinity_values",             test_infinity_values },
	{ "mixed_infinity",              test_mixed_infinity },
	{ "multiple_limits",             test_multiple_limits },
	{ "all_limits",                  test_all_limits },
	/* Verify limits applied */
	{ "nofile_applied",              test_nofile_applied },
	{ "nproc_applied",               test_nproc_applied },
	{ "core_applied",                test_core_applied },
	/* setrlimit failure */
	{ "setrlimit_failure",           test_setrlimit_failure_soft_exceeds_hard },
	/* Exec failure */
	{ "exec_nonexistent",            test_exec_nonexistent },
	/* Exit code pass-through */
	{ "exit_code_passthrough",       test_exit_code_passthrough },
	{ NULL, NULL }
};
