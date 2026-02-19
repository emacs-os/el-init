/*
 * test_elinit_runas.c -- black-box tests for elinit-runas
 *
 * acutest.h: https://github.com/mity/acutest
 * Commit: 31751b4089c93b46a9fd8a8183a695f772de66de
 *
 * Note: most privilege-drop tests require root and are gated behind
 * the SUPERVISOR_TEST_ROOT=1 environment variable.  Without root,
 * only argument parsing and error-path tests run.
 */

#define _POSIX_C_SOURCE 200809L
#include "vendor/acutest.h"
#include "test_helpers.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* Path to the built binary, set by Makefile via -D. */
#ifndef RUNAS_PATH
#define RUNAS_PATH "../elinit-runas"
#endif

/* ----------------------------------------------------------------
 * Usage / argument validation
 * ---------------------------------------------------------------- */

void test_runas_no_args(void)
{
	const char *argv[] = { RUNAS_PATH, NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 111);
	TEST_CHECK(strstr(r.err, "missing command") != NULL);
	run_result_free(&r);
}

void test_runas_no_separator(void)
{
	const char *argv[] = { RUNAS_PATH, "--user", "nobody", NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 111);
	TEST_CHECK(strstr(r.err, "missing command") != NULL);
	run_result_free(&r);
}

void test_runas_no_identity(void)
{
	const char *argv[] = { RUNAS_PATH, "--", "/bin/true", NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 111);
	TEST_CHECK(strstr(r.err, "at least one") != NULL);
	run_result_free(&r);
}

void test_runas_unknown_option(void)
{
	const char *argv[] = { RUNAS_PATH,
			       "--bogus", "--", "/bin/true", NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 111);
	TEST_CHECK(strstr(r.err, "unknown option") != NULL);
	run_result_free(&r);
}

void test_runas_user_missing_arg(void)
{
	const char *argv[] = { RUNAS_PATH, "--user", NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 111);
	TEST_CHECK(strstr(r.err, "--user requires") != NULL);
	run_result_free(&r);
}

void test_runas_group_missing_arg(void)
{
	const char *argv[] = { RUNAS_PATH, "--group", NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 111);
	TEST_CHECK(strstr(r.err, "--group requires") != NULL);
	run_result_free(&r);
}

void test_runas_empty_command(void)
{
	/* "--" is present but no command follows */
	const char *argv[] = { RUNAS_PATH,
			       "--user", "nobody", "--", NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 111);
	run_result_free(&r);
}

/* ----------------------------------------------------------------
 * Identity resolution failures
 * ---------------------------------------------------------------- */

void test_runas_unknown_user(void)
{
	const char *argv[] = { RUNAS_PATH,
			       "--user", "sv_no_such_user_999",
			       "--", "/bin/true", NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 112);
	TEST_CHECK(strstr(r.err, "unknown user") != NULL);
	run_result_free(&r);
}

void test_runas_unknown_group(void)
{
	const char *argv[] = { RUNAS_PATH,
			       "--group", "sv_no_such_group_999",
			       "--", "/bin/true", NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 112);
	TEST_CHECK(strstr(r.err, "unknown group") != NULL);
	run_result_free(&r);
}

/* ----------------------------------------------------------------
 * Non-root failure paths
 * ---------------------------------------------------------------- */

void test_runas_nonroot_setuid_fails(void)
{
	/* When not root, setuid/setgid/initgroups will fail with EPERM.
	 * Skip this test if we ARE root. */
	if (getuid() == 0) {
		TEST_MSG("Skipping: running as root");
		return;
	}

	const char *argv[] = { RUNAS_PATH,
			       "--user", "nobody",
			       "--", "/bin/true", NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	/* Should fail at privilege drop (initgroups or setgid or setuid) */
	TEST_CHECK(r.exit_code == 113);
	run_result_free(&r);
}

void test_runas_nonroot_group_only_fails(void)
{
	if (getuid() == 0) {
		TEST_MSG("Skipping: running as root");
		return;
	}

	/* Use "root" (gid 0) which exists on all Linux distros.
	 * "nobody" group does not exist on Ubuntu (it's "nogroup"). */
	const char *argv[] = { RUNAS_PATH,
			       "--group", "root",
			       "--", "/bin/true", NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 113);
	run_result_free(&r);
}

/* ----------------------------------------------------------------
 * Exec failure
 * ---------------------------------------------------------------- */

void test_runas_exec_failure(void)
{
	/* initgroups/setgroups require CAP_SETGID even for own uid/gid,
	 * so exec failure can only be tested as root. */
	if (getuid() != 0) {
		TEST_MSG("Skipping: requires root (setgroups needs CAP_SETGID)");
		return;
	}

	char uid_str[32];
	char gid_str[32];
	snprintf(uid_str, sizeof(uid_str), "%u", getuid());
	snprintf(gid_str, sizeof(gid_str), "%u", getgid());

	const char *argv[] = { RUNAS_PATH,
			       "--user", uid_str,
			       "--group", gid_str,
			       "--", "/no/such/binary",
			       NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 114);
	TEST_CHECK(strstr(r.err, "exec") != NULL);
	run_result_free(&r);
}

/* ----------------------------------------------------------------
 * Exec success with own identity (non-root safe)
 * ---------------------------------------------------------------- */

void test_runas_exec_success_own_identity(void)
{
	/* initgroups/setgroups require CAP_SETGID even for own uid/gid,
	 * so successful exec can only be tested as root. */
	if (getuid() != 0) {
		TEST_MSG("Skipping: requires root (setgroups needs CAP_SETGID)");
		return;
	}

	char uid_str[32];
	char gid_str[32];
	snprintf(uid_str, sizeof(uid_str), "%u", getuid());
	snprintf(gid_str, sizeof(gid_str), "%u", getgid());

	const char *argv[] = { RUNAS_PATH,
			       "--user", uid_str,
			       "--group", gid_str,
			       "--", "/bin/echo", "hello",
			       NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 0);
	TEST_CHECK(strstr(r.out, "hello") != NULL);
	run_result_free(&r);
}

void test_runas_numeric_gid_only(void)
{
	char gid_str[32];
	snprintf(gid_str, sizeof(gid_str), "%u", getgid());

	const char *argv[] = { RUNAS_PATH,
			       "--group", gid_str,
			       "--", "/bin/echo", "gid-ok",
			       NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	/* setgroups with own gid should succeed if we are not changing uid */
	/* This may still fail as non-root due to setgroups requiring CAP_SETGID.
	 * Accept either success (0) or privilege-drop failure (113). */
	TEST_CHECK(r.exit_code == 0 || r.exit_code == 113);
	run_result_free(&r);
}

/* ----------------------------------------------------------------
 * Root-only tests (gated by SUPERVISOR_TEST_ROOT=1)
 * ---------------------------------------------------------------- */

void test_runas_root_drop_to_nobody(void)
{
	if (getuid() != 0 || !getenv("SUPERVISOR_TEST_ROOT")) {
		TEST_MSG("Skipping: requires root + SUPERVISOR_TEST_ROOT=1");
		return;
	}

	const char *argv[] = { RUNAS_PATH,
			       "--user", "nobody",
			       "--", "/usr/bin/id", "-u",
			       NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 0);
	/* id -u should print the nobody uid (65534 on most systems) */
	TEST_CHECK(r.out != NULL);
	TEST_CHECK(r.out_len > 0);
	/* Just verify it is NOT "0" (root) */
	TEST_CHECK(strstr(r.out, "0\n") == NULL || atoi(r.out) != 0);
	run_result_free(&r);
}

/* ================================================================ */

TEST_LIST = {
	/* Usage / argument validation */
	{ "runas_no_args",             test_runas_no_args },
	{ "runas_no_separator",        test_runas_no_separator },
	{ "runas_no_identity",         test_runas_no_identity },
	{ "runas_unknown_option",      test_runas_unknown_option },
	{ "runas_user_missing_arg",    test_runas_user_missing_arg },
	{ "runas_group_missing_arg",   test_runas_group_missing_arg },
	{ "runas_empty_command",       test_runas_empty_command },

	/* Identity resolution failures */
	{ "runas_unknown_user",        test_runas_unknown_user },
	{ "runas_unknown_group",       test_runas_unknown_group },

	/* Non-root failure paths */
	{ "runas_nonroot_setuid_fails",      test_runas_nonroot_setuid_fails },
	{ "runas_nonroot_group_only_fails",  test_runas_nonroot_group_only_fails },

	/* Exec behavior */
	{ "runas_exec_failure",              test_runas_exec_failure },
	{ "runas_exec_success_own_identity", test_runas_exec_success_own_identity },
	{ "runas_numeric_gid_only",          test_runas_numeric_gid_only },

	/* Root-only */
	{ "runas_root_drop_to_nobody",       test_runas_root_drop_to_nobody },

	{ NULL, NULL }
};
