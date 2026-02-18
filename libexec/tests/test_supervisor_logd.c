/*
 * test_supervisor_logd.c -- black-box tests for supervisor-logd
 *
 * acutest.h: https://github.com/mity/acutest
 * Commit: 31751b4089c93b46a9fd8a8183a695f772de66de
 */

#define _POSIX_C_SOURCE 200809L
#include "vendor/acutest.h"
#include "test_helpers.h"

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

/* POSIX-compliant 100ms sleep. */
static void msleep(int ms)
{
	struct timespec ts;
	ts.tv_sec = ms / 1000;
	ts.tv_nsec = (ms % 1000) * 1000000L;
	nanosleep(&ts, NULL);
}

/* Return non-zero if /dev/full exists and actually rejects writes.
 * Some container runtimes expose /dev/full but silently accept writes. */
static int dev_full_rejects_write(void)
{
	int fd = open("/dev/full", O_WRONLY);
	if (fd < 0)
		return 0;
	char byte = 'x';
	ssize_t w = write(fd, &byte, 1);
	int saved = errno;
	close(fd);
	return (w < 0 && saved == ENOSPC);
}

/* Write helper that checks the return value (silences -Werror=unused-result). */
static void write_all(int fd, const void *buf, size_t len)
{
	ssize_t n = write(fd, buf, len);
	TEST_CHECK(n == (ssize_t)len);
}

/* Write helper for intentional-overflow tests where the peer may close
 * the pipe mid-write.  Consumes the return value without asserting. */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-result"
static void write_may_fail(int fd, const void *buf, size_t len)
{
	write(fd, buf, len);
}
#pragma GCC diagnostic pop

/* Path to the built binary, set by Makefile via -D. */
#ifndef LOGD_PATH
#define LOGD_PATH "../supervisor-logd"
#endif

/* ----------------------------------------------------------------
 * Usage / argument validation
 * ---------------------------------------------------------------- */

void test_logd_missing_file(void)
{
	const char *argv[] = { LOGD_PATH,
			       "--max-file-size-bytes", "1000",
			       NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 1);
	TEST_CHECK(strstr(r.err, "--file is required") != NULL);
	run_result_free(&r);
}

void test_logd_missing_max_size(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 1);
	TEST_CHECK(strstr(r.err, "--max-file-size-bytes is required") != NULL);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

void test_logd_negative_max_size(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "-5",
			       NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 1);
	TEST_CHECK(strstr(r.err, "must be positive") != NULL);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

void test_logd_unknown_option(void)
{
	const char *argv[] = { LOGD_PATH,
			       "--file", "/dev/null",
			       "--max-file-size-bytes", "1000",
			       "--bogus",
			       NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 1);
	TEST_CHECK(strstr(r.err, "unknown option") != NULL);
	run_result_free(&r);
}

void test_logd_framed_requires_unit(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed",
			       NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 1);
	TEST_CHECK(strstr(r.err, "--unit is required") != NULL);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

void test_logd_bad_format(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "json",
			       NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 1);
	TEST_CHECK(strstr(r.err, "text or binary") != NULL);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Raw passthrough mode
 * ---------------------------------------------------------------- */

void test_logd_raw_passthrough(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       NULL };
	const char *input = "hello world\nsecond line\n";
	struct run_result r;
	TEST_CHECK(run_cmd(argv, (const unsigned char *)input,
			   strlen(input), &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	size_t flen;
	unsigned char *data = read_file(logpath, &flen);
	TEST_CHECK(data != NULL);
	TEST_CHECK(flen == strlen(input));
	TEST_CHECK(memcmp(data, input, flen) == 0);
	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

void test_logd_raw_empty_input(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	size_t flen;
	unsigned char *data = read_file(logpath, &flen);
	TEST_CHECK(data != NULL);
	TEST_CHECK(flen == 0);
	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Text mode: field presence and output event rules
 * ---------------------------------------------------------------- */

void test_logd_text_output_fields(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "myunit",
			       "--format", "text",
			       NULL };

	/* Build an output frame: stdout, pid=42, payload="hello" */
	unsigned char frame[FRAME_BUF_SIZE];
	int flen = build_frame(frame, EVT_OUTPUT, STREAM_STDOUT, 42,
			       "myunit", 0, STATUS_NONE,
			       (const unsigned char *)"hello", 5);
	TEST_CHECK(flen > 0);

	struct run_result r;
	TEST_CHECK(run_cmd(argv, frame, (size_t)flen, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	char *line = (char *)data;

	/* Required fields present */
	TEST_CHECK(strstr(line, "ts=") != NULL);
	TEST_CHECK(strstr(line, "unit=myunit") != NULL);
	TEST_CHECK(strstr(line, "pid=42") != NULL);
	TEST_CHECK(strstr(line, "stream=stdout") != NULL);
	TEST_CHECK(strstr(line, "event=output") != NULL);
	TEST_CHECK(strstr(line, "status=-") != NULL);
	TEST_CHECK(strstr(line, "code=-") != NULL);
	TEST_CHECK(strstr(line, "payload=hello") != NULL);

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

void test_logd_text_stderr_stream(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "text",
			       NULL };

	unsigned char frame[FRAME_BUF_SIZE];
	int flen = build_frame(frame, EVT_OUTPUT, STREAM_STDERR, 99,
			       "svc", 0, STATUS_NONE,
			       (const unsigned char *)"err!", 4);

	struct run_result r;
	TEST_CHECK(run_cmd(argv, frame, (size_t)flen, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	TEST_CHECK(strstr((char *)data, "stream=stderr") != NULL);
	TEST_CHECK(strstr((char *)data, "payload=err!") != NULL);

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Text mode: exit event rules
 * ---------------------------------------------------------------- */

void test_logd_text_exit_exited(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "text",
			       NULL };

	unsigned char frame[FRAME_BUF_SIZE];
	int flen = build_frame(frame, EVT_EXIT, STREAM_META, 42,
			       "svc", 0, STATUS_EXITED, NULL, 0);

	struct run_result r;
	TEST_CHECK(run_cmd(argv, frame, (size_t)flen, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	TEST_CHECK(strstr((char *)data, "event=exit") != NULL);
	TEST_CHECK(strstr((char *)data, "stream=meta") != NULL);
	TEST_CHECK(strstr((char *)data, "status=exited") != NULL);
	TEST_CHECK(strstr((char *)data, "code=0") != NULL);
	TEST_CHECK(strstr((char *)data, "payload=-") != NULL);

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

void test_logd_text_exit_signaled(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "text",
			       NULL };

	unsigned char frame[FRAME_BUF_SIZE];
	int flen = build_frame(frame, EVT_EXIT, STREAM_META, 42,
			       "svc", 9, STATUS_SIGNALED, NULL, 0);

	struct run_result r;
	TEST_CHECK(run_cmd(argv, frame, (size_t)flen, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	TEST_CHECK(strstr((char *)data, "status=signaled") != NULL);
	TEST_CHECK(strstr((char *)data, "code=9") != NULL);

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

void test_logd_text_exit_spawn_failed(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "text",
			       NULL };

	unsigned char frame[FRAME_BUF_SIZE];
	int flen = build_frame(frame, EVT_EXIT, STREAM_META, 0,
			       "svc", 127, STATUS_SPAWN_FAILED, NULL, 0);

	struct run_result r;
	TEST_CHECK(run_cmd(argv, frame, (size_t)flen, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	TEST_CHECK(strstr((char *)data, "status=spawn-failed") != NULL);
	TEST_CHECK(strstr((char *)data, "code=127") != NULL);

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Text mode: escaping round-trip
 * ---------------------------------------------------------------- */

void test_logd_text_escape_empty(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "text",
			       NULL };

	unsigned char frame[FRAME_BUF_SIZE];
	int flen = build_frame(frame, EVT_OUTPUT, STREAM_STDOUT, 1,
			       "svc", 0, STATUS_NONE, NULL, 0);

	struct run_result r;
	TEST_CHECK(run_cmd(argv, frame, (size_t)flen, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	/* Empty payload becomes empty string (no dash) */
	TEST_CHECK(strstr((char *)data, "payload=\n") != NULL);

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

void test_logd_text_escape_dash(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "text",
			       NULL };

	unsigned char frame[FRAME_BUF_SIZE];
	int flen = build_frame(frame, EVT_OUTPUT, STREAM_STDOUT, 1,
			       "svc", 0, STATUS_NONE,
			       (const unsigned char *)"-", 1);

	struct run_result r;
	TEST_CHECK(run_cmd(argv, frame, (size_t)flen, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	/* Literal dash is printable, not escaped */
	TEST_CHECK(strstr((char *)data, "payload=-\n") != NULL);

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

void test_logd_text_escape_backslash(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "text",
			       NULL };

	unsigned char frame[FRAME_BUF_SIZE];
	int flen = build_frame(frame, EVT_OUTPUT, STREAM_STDOUT, 1,
			       "svc", 0, STATUS_NONE,
			       (const unsigned char *)"a\\b", 3);

	struct run_result r;
	TEST_CHECK(run_cmd(argv, frame, (size_t)flen, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	TEST_CHECK(strstr((char *)data, "payload=a\\\\b") != NULL);

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

void test_logd_text_escape_newline_cr_tab(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "text",
			       NULL };

	/* Payload: newline, carriage return, tab */
	const unsigned char payload[] = { '\n', '\r', '\t' };
	unsigned char frame[FRAME_BUF_SIZE];
	int flen = build_frame(frame, EVT_OUTPUT, STREAM_STDOUT, 1,
			       "svc", 0, STATUS_NONE, payload, 3);

	struct run_result r;
	TEST_CHECK(run_cmd(argv, frame, (size_t)flen, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	TEST_CHECK(strstr((char *)data, "payload=\\n\\r\\t") != NULL);

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

void test_logd_text_escape_nul_highbit(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "text",
			       NULL };

	/* NUL byte and 0xFF */
	const unsigned char payload[] = { 0x00, 0xFF };
	unsigned char frame[FRAME_BUF_SIZE];
	int flen = build_frame(frame, EVT_OUTPUT, STREAM_STDOUT, 1,
			       "svc", 0, STATUS_NONE, payload, 2);

	struct run_result r;
	TEST_CHECK(run_cmd(argv, frame, (size_t)flen, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	TEST_CHECK(strstr((char *)data, "payload=\\x00\\xff") != NULL);

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Text mode: multiple frames in one pipe write
 * ---------------------------------------------------------------- */

void test_logd_text_multiple_frames(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "text",
			       NULL };

	unsigned char buf[FRAME_BUF_SIZE];
	int total = 0;

	int n1 = build_frame(buf + total, EVT_OUTPUT, STREAM_STDOUT, 10,
			     "svc", 0, STATUS_NONE,
			     (const unsigned char *)"line1", 5);
	TEST_CHECK(n1 > 0);
	total += n1;

	int n2 = build_frame(buf + total, EVT_OUTPUT, STREAM_STDOUT, 10,
			     "svc", 0, STATUS_NONE,
			     (const unsigned char *)"line2", 5);
	TEST_CHECK(n2 > 0);
	total += n2;

	int n3 = build_frame(buf + total, EVT_EXIT, STREAM_META, 10,
			     "svc", 0, STATUS_EXITED, NULL, 0);
	TEST_CHECK(n3 > 0);
	total += n3;

	struct run_result r;
	TEST_CHECK(run_cmd(argv, buf, (size_t)total, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);

	/* Count lines -- should be 3 (2 output + 1 exit) */
	int lines = 0;
	for (size_t i = 0; i < dlen; i++) {
		if (data[i] == '\n')
			lines++;
	}
	TEST_CHECK(lines == 3);
	TEST_CHECK(strstr((char *)data, "payload=line1") != NULL);
	TEST_CHECK(strstr((char *)data, "payload=line2") != NULL);
	TEST_CHECK(strstr((char *)data, "event=exit") != NULL);

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Framing: protocol error recovery
 * ---------------------------------------------------------------- */

void test_logd_text_invalid_event_skipped(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "text",
			       NULL };

	unsigned char buf[FRAME_BUF_SIZE];
	int total = 0;

	/* Build a frame with invalid event type (99) */
	int n1 = build_frame(buf + total, 99, STREAM_STDOUT, 42,
			     "svc", 0, STATUS_NONE,
			     (const unsigned char *)"bad", 3);
	TEST_CHECK(n1 > 0);
	total += n1;

	/* Follow with a valid frame */
	int n2 = build_frame(buf + total, EVT_OUTPUT, STREAM_STDOUT, 42,
			     "svc", 0, STATUS_NONE,
			     (const unsigned char *)"good", 4);
	TEST_CHECK(n2 > 0);
	total += n2;

	struct run_result r;
	TEST_CHECK(run_cmd(argv, buf, (size_t)total, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	/* stderr should mention protocol error */
	TEST_CHECK(strstr(r.err, "protocol error") != NULL);

	/* Only the valid frame should appear in the log */
	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	TEST_CHECK(strstr((char *)data, "payload=good") != NULL);
	/* "bad" should NOT appear as output */
	TEST_CHECK(strstr((char *)data, "payload=bad") == NULL);

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

void test_logd_text_invalid_stream_skipped(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "text",
			       NULL };

	unsigned char buf[FRAME_BUF_SIZE];
	int total = 0;

	/* Invalid: stream=0 (out of range) */
	int n1 = build_frame(buf + total, EVT_OUTPUT, 0, 42,
			     "svc", 0, STATUS_NONE,
			     (const unsigned char *)"bad", 3);
	total += n1;

	/* Valid follow-up */
	int n2 = build_frame(buf + total, EVT_OUTPUT, STREAM_STDOUT, 42,
			     "svc", 0, STATUS_NONE,
			     (const unsigned char *)"ok", 2);
	total += n2;

	struct run_result r;
	TEST_CHECK(run_cmd(argv, buf, (size_t)total, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	TEST_CHECK(strstr((char *)data, "payload=ok") != NULL);

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

void test_logd_text_output_on_meta_rejected(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "text",
			       NULL };

	unsigned char buf[FRAME_BUF_SIZE];
	int total = 0;

	/* Invalid: EVT_OUTPUT with STREAM_META */
	int n1 = build_frame(buf + total, EVT_OUTPUT, STREAM_META, 42,
			     "svc", 0, STATUS_NONE,
			     (const unsigned char *)"bad", 3);
	total += n1;

	/* Valid follow-up */
	int n2 = build_frame(buf + total, EVT_OUTPUT, STREAM_STDOUT, 42,
			     "svc", 0, STATUS_NONE,
			     (const unsigned char *)"ok", 2);
	total += n2;

	struct run_result r;
	TEST_CHECK(run_cmd(argv, buf, (size_t)total, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	TEST_CHECK(strstr((char *)data, "payload=ok") != NULL);

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

void test_logd_text_exit_on_stdout_rejected(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "text",
			       NULL };

	unsigned char buf[FRAME_BUF_SIZE];
	int total = 0;

	/* Invalid: EVT_EXIT with STREAM_STDOUT */
	int n1 = build_frame(buf + total, EVT_EXIT, STREAM_STDOUT, 42,
			     "svc", 0, STATUS_EXITED, NULL, 0);
	total += n1;

	/* Valid follow-up */
	int n2 = build_frame(buf + total, EVT_OUTPUT, STREAM_STDOUT, 42,
			     "svc", 0, STATUS_NONE,
			     (const unsigned char *)"ok", 2);
	total += n2;

	struct run_result r;
	TEST_CHECK(run_cmd(argv, buf, (size_t)total, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	TEST_CHECK(strstr((char *)data, "payload=ok") != NULL);

	/* The bad exit frame should not produce a record */
	int lines = 0;
	for (size_t i = 0; i < dlen; i++) {
		if (data[i] == '\n')
			lines++;
	}
	TEST_CHECK(lines == 1);

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* Structural error: body_len < 13 (too small for fixed fields). */
void test_logd_text_truncated_body_recovery(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "text",
			       NULL };

	unsigned char buf[FRAME_BUF_SIZE];
	int total = 0;

	/* Manually craft a frame with body_len=5 (too small for the 13-byte
	 * fixed fields).  This triggers the structural error path (-1
	 * return, 1-byte skip).  Recovery from structural errors is
	 * best-effort; we only verify the error is detected. */
	unsigned char bad[9];
	bad[0] = 0; bad[1] = 0; bad[2] = 0; bad[3] = 5; /* body_len=5 */
	bad[4] = 1; bad[5] = 1; bad[6] = 0; bad[7] = 0; bad[8] = 0;
	memcpy(buf + total, bad, sizeof(bad));
	total += (int)sizeof(bad);

	struct run_result r;
	TEST_CHECK(run_cmd(argv, buf, (size_t)total, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	/* stderr should mention protocol error from the bad frame */
	TEST_CHECK(strstr(r.err, "protocol error") != NULL);

	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Raw mode: rotation on max-size boundary
 * ---------------------------------------------------------------- */

void test_logd_raw_rotation(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	/* Set max-size to 50 bytes */
	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "50",
			       NULL };

	/* Use a background process and multiple writes to ensure rotation
	 * happens mid-stream, producing data in both old and new files. */
	int in_pipe[2];
	TEST_CHECK(pipe(in_pipe) == 0);

	pid_t pid = fork();
	TEST_CHECK(pid >= 0);

	if (pid == 0) {
		close(in_pipe[1]);
		dup2(in_pipe[0], STDIN_FILENO);
		close(in_pipe[0]);
		execv(argv[0], (char *const *)argv);
		_exit(127);
	}

	close(in_pipe[0]);

	/* Write first batch (60 bytes > 50, triggers rotation) */
	const char *batch1 =
	    "aaaaaaaaaa" "aaaaaaaaaa" "aaaaaaaaaa"
	    "aaaaaaaaaa" "aaaaaaaaaa" "aaaaaaaaaa";
	write_all(in_pipe[1], batch1, strlen(batch1));
	msleep(100);

	/* Write second batch into the new (post-rotation) file */
	const char *batch2 = "bbbbbbbbbb\n";
	write_all(in_pipe[1], batch2, strlen(batch2));
	msleep(100);

	close(in_pipe[1]);
	int status;
	waitpid(pid, &status, 0);
	TEST_CHECK(WIFEXITED(status) && WEXITSTATUS(status) == 0);

	/* The current log file should contain data from batch2 */
	size_t cur_len;
	unsigned char *cur = read_file(logpath, &cur_len);
	TEST_CHECK(cur != NULL);
	TEST_CHECK(cur_len > 0);
	free(cur);

	/* List directory to find rotated file */
	struct run_result ls;
	const char *ls_argv[] = { "/bin/ls", tmpdir, NULL };
	TEST_CHECK(run_cmd(ls_argv, NULL, 0, &ls) == 0);
	/* Should have at least 2 files: test.log and test.<timestamp>.log */
	TEST_CHECK(strstr(ls.out, "test.log") != NULL);
	TEST_CHECK(ls.out_len > (size_t)(strlen("test.log") + 1));

	run_result_free(&ls);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * SIGHUP: reopen file
 * ---------------------------------------------------------------- */

void test_logd_sighup_reopen(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	/* We need to run logd as a real background process to send signals.
	 * Fork, write to stdin pipe, send HUP, write more, close pipe. */
	int in_pipe[2];
	TEST_CHECK(pipe(in_pipe) == 0);

	pid_t pid = fork();
	TEST_CHECK(pid >= 0);

	if (pid == 0) {
		/* Child: exec logd */
		close(in_pipe[1]);
		dup2(in_pipe[0], STDIN_FILENO);
		close(in_pipe[0]);
		execl(LOGD_PATH, "supervisor-logd",
		      "--file", logpath,
		      "--max-file-size-bytes", "100000",
		      (char *)NULL);
		_exit(127);
	}

	/* Parent */
	close(in_pipe[0]);

	/* Write first data */
	const char *msg1 = "before-hup\n";
	write_all(in_pipe[1], msg1, strlen(msg1));
	msleep(100); /* 100ms for logd to process */

	/* Rename the log file (simulating rotation) */
	char rotated[512];
	snprintf(rotated, sizeof(rotated), "%s/test.old.log", tmpdir);
	rename(logpath, rotated);

	/* Send SIGHUP */
	kill(pid, SIGHUP);
	msleep(100);

	/* Write more data */
	const char *msg2 = "after-hup\n";
	write_all(in_pipe[1], msg2, strlen(msg2));
	msleep(100);

	/* Close stdin to trigger clean exit */
	close(in_pipe[1]);

	int status;
	waitpid(pid, &status, 0);
	TEST_CHECK(WIFEXITED(status) && WEXITSTATUS(status) == 0);

	/* The new log file should contain the after-hup data */
	size_t new_len;
	unsigned char *new_data = read_file(logpath, &new_len);
	TEST_CHECK(new_data != NULL);
	TEST_CHECK(strstr((char *)new_data, "after-hup") != NULL);

	/* The old file should contain the before-hup data */
	size_t old_len;
	unsigned char *old_data = read_file(rotated, &old_len);
	TEST_CHECK(old_data != NULL);
	TEST_CHECK(strstr((char *)old_data, "before-hup") != NULL);

	free(new_data);
	free(old_data);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Binary mode: SLG1 header and record structure
 * ---------------------------------------------------------------- */

void test_logd_binary_header(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "binary",
			       NULL };

	/* Send one output frame */
	unsigned char frame[FRAME_BUF_SIZE];
	int flen = build_frame(frame, EVT_OUTPUT, STREAM_STDOUT, 42,
			       "svc", 0, STATUS_NONE,
			       (const unsigned char *)"hi", 2);

	struct run_result r;
	TEST_CHECK(run_cmd(argv, frame, (size_t)flen, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	TEST_CHECK(dlen >= 4);

	/* Check SLG1 magic */
	TEST_CHECK(data[0] == 'S');
	TEST_CHECK(data[1] == 'L');
	TEST_CHECK(data[2] == 'G');
	TEST_CHECK(data[3] == '1');

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

void test_logd_binary_output_record(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "binary",
			       NULL };

	unsigned char frame[FRAME_BUF_SIZE];
	int flen = build_frame(frame, EVT_OUTPUT, STREAM_STDOUT, 42,
			       "svc", 0, STATUS_NONE,
			       (const unsigned char *)"hi", 2);

	struct run_result r;
	TEST_CHECK(run_cmd(argv, frame, (size_t)flen, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);

	/* Skip SLG1 magic (4 bytes), parse record header (34 bytes) */
	TEST_CHECK(dlen >= 4 + 34);
	unsigned char *rec = data + 4;

	unsigned int record_len = read_u32be(rec);
	TEST_CHECK(rec[4] == 1);                  /* version */
	TEST_CHECK(rec[5] == EVT_OUTPUT);         /* event */
	TEST_CHECK(rec[6] == STREAM_STDOUT);      /* stream */
	TEST_CHECK(rec[7] == 0);                  /* reserved */

	unsigned long long ts_ns = read_u64be(rec + 8);
	TEST_CHECK(ts_ns > 0);                    /* timestamp nonzero */

	unsigned int rpid = read_u32be(rec + 16);
	TEST_CHECK(rpid == 42);                   /* pid */

	unsigned short runit_len = read_u16be(rec + 20);
	TEST_CHECK(runit_len == 3);               /* "svc" */

	/* Output event: exit_code=0, exit_status=0 */
	int rcode = read_i32be(rec + 22);
	TEST_CHECK(rcode == 0);
	TEST_CHECK(rec[26] == 0);                 /* exit_status */

	unsigned int rpayload_len = read_u32be(rec + 30);
	TEST_CHECK(rpayload_len == 2);            /* "hi" */

	/* Verify record_len = 30 + unit_len + payload_len */
	TEST_CHECK(record_len == 30 + 3 + 2);

	/* Verify total file size = 4(magic) + 34(header) + 3(unit) + 2(payload) */
	TEST_CHECK(dlen == 4 + 34 + 3 + 2);

	/* Check unit and payload bytes */
	TEST_CHECK(memcmp(rec + 34, "svc", 3) == 0);
	TEST_CHECK(memcmp(rec + 37, "hi", 2) == 0);

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

void test_logd_binary_exit_record(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "binary",
			       NULL };

	unsigned char frame[FRAME_BUF_SIZE];
	int flen = build_frame(frame, EVT_EXIT, STREAM_META, 42,
			       "svc", 15, STATUS_SIGNALED, NULL, 0);

	struct run_result r;
	TEST_CHECK(run_cmd(argv, frame, (size_t)flen, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	TEST_CHECK(dlen >= 4 + 34);

	unsigned char *rec = data + 4;
	TEST_CHECK(rec[5] == EVT_EXIT);
	TEST_CHECK(rec[6] == STREAM_META);

	int rcode = read_i32be(rec + 22);
	TEST_CHECK(rcode == 15);
	TEST_CHECK(rec[26] == STATUS_SIGNALED);

	unsigned int rpayload_len = read_u32be(rec + 30);
	TEST_CHECK(rpayload_len == 0);

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Binary mode: rotation preserves SLG1 header
 * ---------------------------------------------------------------- */

void test_logd_binary_rotation_header(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	/* Very small max-size to trigger rotation */
	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "80",
			       "--framed", "--unit", "svc",
			       "--format", "binary",
			       NULL };

	/* Send multiple frames to exceed 80 bytes */
	unsigned char buf[FRAME_BUF_SIZE];
	int total = 0;

	for (int i = 0; i < 5; i++) {
		int n = build_frame(buf + total, EVT_OUTPUT, STREAM_STDOUT,
				    42, "svc", 0, STATUS_NONE,
				    (const unsigned char *)"AAAAAAAAAAAA", 12);
		TEST_CHECK(n > 0);
		total += n;
	}

	struct run_result r;
	TEST_CHECK(run_cmd(argv, buf, (size_t)total, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	/* The current log file should start with SLG1 */
	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	TEST_CHECK(dlen >= 4);
	TEST_CHECK(memcmp(data, "SLG1", 4) == 0);

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * File error: bad path
 * ---------------------------------------------------------------- */

void test_logd_bad_file_path(void)
{
	const char *argv[] = { LOGD_PATH,
			       "--file", "/no/such/dir/test.log",
			       "--max-file-size-bytes", "1000",
			       NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 2);
	TEST_CHECK(strstr(r.err, "open") != NULL);
	run_result_free(&r);
}

/* ----------------------------------------------------------------
 * Raw framed mode (--framed without --format): exit marker format
 * ---------------------------------------------------------------- */

void test_logd_framed_default_text(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	/* --framed without --format defaults to text format.
	 * Verify text records are produced when --format is omitted. */
	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       NULL };

	unsigned char buf[FRAME_BUF_SIZE];
	int total = 0;

	int n1 = build_frame(buf + total, EVT_OUTPUT, STREAM_STDOUT, 42,
			     "svc", 0, STATUS_NONE,
			     (const unsigned char *)"data", 4);
	total += n1;

	int n2 = build_frame(buf + total, EVT_EXIT, STREAM_META, 42,
			     "svc", 1, STATUS_EXITED, NULL, 0);
	total += n2;

	struct run_result r;
	TEST_CHECK(run_cmd(argv, buf, (size_t)total, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	/* Default framed mode is text, so we should see text records */
	TEST_CHECK(strstr((char *)data, "event=exit") != NULL);
	TEST_CHECK(strstr((char *)data, "status=exited") != NULL);

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Text mode: rotation in framed mode preserves alignment
 * ---------------------------------------------------------------- */

void test_logd_text_rotation(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	/* Small max-size to trigger rotation */
	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "200",
			       "--framed", "--unit", "svc",
			       "--format", "text",
			       NULL };

	/* Use background process with multiple writes so rotation
	 * happens between writes and the new file gets data. */
	int in_pipe[2];
	TEST_CHECK(pipe(in_pipe) == 0);

	pid_t pid = fork();
	TEST_CHECK(pid >= 0);

	if (pid == 0) {
		close(in_pipe[1]);
		dup2(in_pipe[0], STDIN_FILENO);
		close(in_pipe[0]);
		execv(argv[0], (char *const *)argv);
		_exit(127);
	}

	close(in_pipe[0]);

	/* Write frames in batches to ensure rotation triggers mid-stream */
	for (int batch = 0; batch < 3; batch++) {
		unsigned char buf[FRAME_BUF_SIZE];
		int total = 0;
		for (int i = 0; i < 4; i++) {
			int n = build_frame(
			    buf + total, EVT_OUTPUT, STREAM_STDOUT,
			    42, "svc", 0, STATUS_NONE,
			    (const unsigned char *)"some-payload", 12);
			TEST_CHECK(n > 0);
			total += n;
		}
		write_all(in_pipe[1], buf, (size_t)total);
		msleep(200);
	}

	close(in_pipe[1]);
	int status;
	waitpid(pid, &status, 0);
	TEST_CHECK(WIFEXITED(status) && WEXITSTATUS(status) == 0);

	/* Verify a rotated file exists (rotation happened) */
	struct run_result ls;
	const char *ls_argv[] = { "/bin/ls", tmpdir, NULL };
	TEST_CHECK(run_cmd(ls_argv, NULL, 0, &ls) == 0);
	/* Directory should contain test.log plus at least one rotated file */
	TEST_CHECK(strstr(ls.out, "test.log") != NULL);
	TEST_CHECK(ls.out_len > (size_t)(strlen("test.log") + 1));
	run_result_free(&ls);

	/* Current log file should exist (may be empty if all data was
	 * written to the pre-rotation file in the final batch). */
	struct stat st;
	TEST_CHECK(stat(logpath, &st) == 0);

	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Missing-arg branches for optional flags
 * ---------------------------------------------------------------- */

void test_logd_log_dir_missing_arg(void)
{
	const char *argv[] = { LOGD_PATH,
			       "--file", "/dev/null",
			       "--max-file-size-bytes", "1000",
			       "--log-dir",
			       NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 1);
	TEST_CHECK(strstr(r.err, "--log-dir") != NULL);
	run_result_free(&r);
}

void test_logd_prune_cmd_missing_arg(void)
{
	const char *argv[] = { LOGD_PATH,
			       "--file", "/dev/null",
			       "--max-file-size-bytes", "1000",
			       "--prune-cmd",
			       NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 1);
	TEST_CHECK(strstr(r.err, "--prune-cmd") != NULL);
	run_result_free(&r);
}

void test_logd_prune_interval_missing_arg(void)
{
	const char *argv[] = { LOGD_PATH,
			       "--file", "/dev/null",
			       "--max-file-size-bytes", "1000",
			       "--prune-min-interval-sec",
			       NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 1);
	TEST_CHECK(strstr(r.err, "--prune-min-interval-sec") != NULL);
	run_result_free(&r);
}

void test_logd_unit_missing_arg(void)
{
	const char *argv[] = { LOGD_PATH,
			       "--file", "/dev/null",
			       "--max-file-size-bytes", "1000",
			       "--unit",
			       NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 1);
	TEST_CHECK(strstr(r.err, "--unit") != NULL);
	run_result_free(&r);
}

void test_logd_format_missing_arg(void)
{
	const char *argv[] = { LOGD_PATH,
			       "--file", "/dev/null",
			       "--max-file-size-bytes", "1000",
			       "--format",
			       NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 1);
	TEST_CHECK(strstr(r.err, "--format") != NULL);
	run_result_free(&r);
}

void test_logd_file_missing_arg(void)
{
	const char *argv[] = { LOGD_PATH,
			       "--max-file-size-bytes", "1000",
			       "--file",
			       NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 1);
	TEST_CHECK(strstr(r.err, "--file") != NULL);
	run_result_free(&r);
}

void test_logd_max_size_missing_arg(void)
{
	const char *argv[] = { LOGD_PATH,
			       "--file", "/dev/null",
			       "--max-file-size-bytes",
			       NULL };
	struct run_result r;
	TEST_CHECK(run_cmd(argv, NULL, 0, &r) == 0);
	TEST_CHECK(r.exit_code == 1);
	TEST_CHECK(strstr(r.err, "--max-file-size-bytes") != NULL);
	run_result_free(&r);
}

/* ----------------------------------------------------------------
 * SIGTERM: clean exit
 * ---------------------------------------------------------------- */

void test_logd_sigterm_clean_exit(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	int in_pipe[2];
	TEST_CHECK(pipe(in_pipe) == 0);

	pid_t pid = fork();
	TEST_CHECK(pid >= 0);

	if (pid == 0) {
		close(in_pipe[1]);
		dup2(in_pipe[0], STDIN_FILENO);
		close(in_pipe[0]);
		execl(LOGD_PATH, "supervisor-logd",
		      "--file", logpath,
		      "--max-file-size-bytes", "100000",
		      (char *)NULL);
		_exit(127);
	}

	close(in_pipe[0]);

	/* Write some data */
	const char *msg = "before-term\n";
	write_all(in_pipe[1], msg, strlen(msg));
	msleep(100);

	/* Send SIGTERM */
	kill(pid, SIGTERM);

	int status;
	waitpid(pid, &status, 0);
	TEST_CHECK(WIFEXITED(status));
	TEST_CHECK(WEXITSTATUS(status) == 0);

	/* Data should have been flushed */
	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	TEST_CHECK(strstr((char *)data, "before-term") != NULL);

	free(data);
	close(in_pipe[1]);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Prune command execution on rotation
 * ---------------------------------------------------------------- */

void test_logd_prune_on_rotation(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	char marker[512];
	char prune_cmd[1024];

	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);
	snprintf(marker, sizeof(marker), "%s/prune-ran", tmpdir);
	snprintf(prune_cmd, sizeof(prune_cmd), "touch %s", marker);

	/* Small max-size so rotation triggers, with prune-cmd */
	int in_pipe[2];
	TEST_CHECK(pipe(in_pipe) == 0);

	pid_t pid = fork();
	TEST_CHECK(pid >= 0);

	if (pid == 0) {
		close(in_pipe[1]);
		dup2(in_pipe[0], STDIN_FILENO);
		close(in_pipe[0]);
		execl(LOGD_PATH, "supervisor-logd",
		      "--file", logpath,
		      "--max-file-size-bytes", "50",
		      "--prune-cmd", prune_cmd,
		      "--prune-min-interval-sec", "0",
		      (char *)NULL);
		_exit(127);
	}

	close(in_pipe[0]);

	/* Write enough to trigger rotation */
	const char *batch =
	    "aaaaaaaaaa" "aaaaaaaaaa" "aaaaaaaaaa"
	    "aaaaaaaaaa" "aaaaaaaaaa" "aaaaaaaaaa";
	write_all(in_pipe[1], batch, strlen(batch));
	msleep(200);

	/* Write more to ensure we're past rotation */
	write_all(in_pipe[1], "bbb\n", 4);
	msleep(200);

	close(in_pipe[1]);
	int status;
	waitpid(pid, &status, 0);
	TEST_CHECK(WIFEXITED(status) && WEXITSTATUS(status) == 0);

	/* The prune marker file should exist */
	struct stat st;
	TEST_CHECK(stat(marker, &st) == 0);

	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Prune throttle: second rotation within interval skips prune
 * ---------------------------------------------------------------- */

void test_logd_prune_throttle(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	char counter_file[512];
	char prune_cmd[1024];

	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);
	snprintf(counter_file, sizeof(counter_file),
		 "%s/prune-count", tmpdir);
	/* Append a line each time prune runs; count lines later */
	snprintf(prune_cmd, sizeof(prune_cmd),
		 "echo x >> %s", counter_file);

	int in_pipe[2];
	TEST_CHECK(pipe(in_pipe) == 0);

	pid_t pid = fork();
	TEST_CHECK(pid >= 0);

	if (pid == 0) {
		close(in_pipe[1]);
		dup2(in_pipe[0], STDIN_FILENO);
		close(in_pipe[0]);
		execl(LOGD_PATH, "supervisor-logd",
		      "--file", logpath,
		      "--max-file-size-bytes", "50",
		      "--prune-cmd", prune_cmd,
		      /* 60s interval: second rotation should be throttled */
		      "--prune-min-interval-sec", "60",
		      (char *)NULL);
		_exit(127);
	}

	close(in_pipe[0]);

	/* Trigger two rotations in quick succession */
	for (int i = 0; i < 3; i++) {
		const char *batch =
		    "aaaaaaaaaa" "aaaaaaaaaa" "aaaaaaaaaa"
		    "aaaaaaaaaa" "aaaaaaaaaa" "aaaaaaaaaa";
		write_all(in_pipe[1], batch, strlen(batch));
		msleep(200);
	}

	close(in_pipe[1]);
	int status;
	waitpid(pid, &status, 0);
	TEST_CHECK(WIFEXITED(status) && WEXITSTATUS(status) == 0);

	/* Prune should have run exactly once (throttled on second) */
	size_t clen;
	unsigned char *cdata = read_file(counter_file, &clen);
	if (cdata) {
		int lines = 0;
		for (size_t j = 0; j < clen; j++) {
			if (cdata[j] == '\n')
				lines++;
		}
		TEST_CHECK(lines == 1);
		free(cdata);
	} else {
		/* File might not exist if prune never ran -- that's a
		 * failure since at least the first rotation should
		 * have triggered it. */
		TEST_CHECK(cdata != NULL);
	}

	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * SIGHUP in framed mode reopens file
 * ---------------------------------------------------------------- */

void test_logd_framed_sighup_reopen(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	int in_pipe[2];
	TEST_CHECK(pipe(in_pipe) == 0);

	pid_t pid = fork();
	TEST_CHECK(pid >= 0);

	if (pid == 0) {
		close(in_pipe[1]);
		dup2(in_pipe[0], STDIN_FILENO);
		close(in_pipe[0]);
		execl(LOGD_PATH, "supervisor-logd",
		      "--file", logpath,
		      "--max-file-size-bytes", "100000",
		      "--framed", "--unit", "svc",
		      "--format", "text",
		      (char *)NULL);
		_exit(127);
	}

	close(in_pipe[0]);

	/* Write a frame */
	unsigned char frame[FRAME_BUF_SIZE];
	int flen = build_frame(frame, EVT_OUTPUT, STREAM_STDOUT, 42,
			       "svc", 0, STATUS_NONE,
			       (const unsigned char *)"before", 6);
	write_all(in_pipe[1], frame, (size_t)flen);
	msleep(100);

	/* Rename and HUP */
	char rotated[512];
	snprintf(rotated, sizeof(rotated), "%s/test.old.log", tmpdir);
	rename(logpath, rotated);
	kill(pid, SIGHUP);
	msleep(100);

	/* Write another frame */
	flen = build_frame(frame, EVT_OUTPUT, STREAM_STDOUT, 42,
			   "svc", 0, STATUS_NONE,
			   (const unsigned char *)"after", 5);
	write_all(in_pipe[1], frame, (size_t)flen);
	msleep(100);

	close(in_pipe[1]);
	int status;
	waitpid(pid, &status, 0);
	TEST_CHECK(WIFEXITED(status) && WEXITSTATUS(status) == 0);

	/* New file should have "after" */
	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	TEST_CHECK(strstr((char *)data, "payload=after") != NULL);

	/* Old file should have "before" */
	size_t olen;
	unsigned char *odata = read_file(rotated, &olen);
	TEST_CHECK(odata != NULL);
	TEST_CHECK(strstr((char *)odata, "payload=before") != NULL);

	free(data);
	free(odata);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Binary mode SIGHUP: reopen writes new SLG1 header
 * ---------------------------------------------------------------- */

void test_logd_binary_sighup_reopen(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	int in_pipe[2];
	TEST_CHECK(pipe(in_pipe) == 0);

	pid_t pid = fork();
	TEST_CHECK(pid >= 0);

	if (pid == 0) {
		close(in_pipe[1]);
		dup2(in_pipe[0], STDIN_FILENO);
		close(in_pipe[0]);
		execl(LOGD_PATH, "supervisor-logd",
		      "--file", logpath,
		      "--max-file-size-bytes", "100000",
		      "--framed", "--unit", "svc",
		      "--format", "binary",
		      (char *)NULL);
		_exit(127);
	}

	close(in_pipe[0]);

	/* Write a frame */
	unsigned char frame[FRAME_BUF_SIZE];
	int flen = build_frame(frame, EVT_OUTPUT, STREAM_STDOUT, 42,
			       "svc", 0, STATUS_NONE,
			       (const unsigned char *)"hi", 2);
	write_all(in_pipe[1], frame, (size_t)flen);
	msleep(100);

	/* Rename and HUP */
	char rotated[512];
	snprintf(rotated, sizeof(rotated), "%s/test.old.log", tmpdir);
	rename(logpath, rotated);
	kill(pid, SIGHUP);
	msleep(100);

	/* Write another frame */
	flen = build_frame(frame, EVT_OUTPUT, STREAM_STDOUT, 42,
			   "svc", 0, STATUS_NONE,
			   (const unsigned char *)"lo", 2);
	write_all(in_pipe[1], frame, (size_t)flen);
	msleep(100);

	close(in_pipe[1]);
	int status;
	waitpid(pid, &status, 0);
	TEST_CHECK(WIFEXITED(status) && WEXITSTATUS(status) == 0);

	/* New file should start with SLG1 */
	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	TEST_CHECK(dlen >= 4);
	TEST_CHECK(memcmp(data, "SLG1", 4) == 0);

	free(data);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Framed write error: bad fd forces EXIT_IO
 * ---------------------------------------------------------------- */

void test_logd_framed_write_error(void)
{
	if (!dev_full_rejects_write()) {
		TEST_MSG("Skipping: /dev/full not available or not rejecting writes");
		return;
	}
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	/* Use /dev/full which returns ENOSPC on write. */
	const char *argv[] = { LOGD_PATH,
			       "--file", "/dev/full",
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "text",
			       NULL };

	unsigned char frame[FRAME_BUF_SIZE];
	int flen = build_frame(frame, EVT_OUTPUT, STREAM_STDOUT, 42,
			       "svc", 0, STATUS_NONE,
			       (const unsigned char *)"data", 4);

	struct run_result r;
	TEST_CHECK(run_cmd(argv, frame, (size_t)flen, &r) == 0);
	TEST_CHECK(r.exit_code == 2);
	TEST_CHECK(strstr(r.err, "write") != NULL);
	TEST_MSG("stderr: %s", r.err);
	run_result_free(&r);

	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Raw write error path
 * ---------------------------------------------------------------- */

void test_logd_raw_write_error(void)
{
	if (!dev_full_rejects_write()) {
		TEST_MSG("Skipping: /dev/full not available or not rejecting writes");
		return;
	}
	const char *argv[] = { LOGD_PATH,
			       "--file", "/dev/full",
			       "--max-file-size-bytes", "100000",
			       NULL };
	const char *input = "some data\n";
	struct run_result r;
	TEST_CHECK(run_cmd(argv, (const unsigned char *)input,
			   strlen(input), &r) == 0);
	TEST_CHECK(r.exit_code == 2);
	TEST_CHECK(strstr(r.err, "write") != NULL);
	TEST_MSG("stderr: %s", r.err);
	run_result_free(&r);
}

/* ----------------------------------------------------------------
 * Valid use of --log-dir and --prune-cmd options (accepted)
 * ---------------------------------------------------------------- */

void test_logd_log_dir_accepted(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--log-dir", tmpdir,
			       "--prune-cmd", "true",
			       "--prune-min-interval-sec", "0",
			       NULL };
	const char *input = "hello\n";
	struct run_result r;
	TEST_CHECK(run_cmd(argv, (const unsigned char *)input,
			   strlen(input), &r) == 0);
	TEST_CHECK(r.exit_code == 0);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Invalid exit status in frame (> STATUS_SPAWN_FAILED)
 * ---------------------------------------------------------------- */

void test_logd_text_invalid_exit_status_skipped(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "text",
			       NULL };

	unsigned char buf[FRAME_BUF_SIZE];
	int total = 0;

	/* Exit status 99 is out of range (> 3) */
	int n1 = build_frame(buf + total, EVT_EXIT, STREAM_META, 42,
			     "svc", 0, 99, NULL, 0);
	total += n1;

	/* Valid follow-up */
	int n2 = build_frame(buf + total, EVT_OUTPUT, STREAM_STDOUT, 42,
			     "svc", 0, STATUS_NONE,
			     (const unsigned char *)"ok", 2);
	total += n2;

	struct run_result r;
	TEST_CHECK(run_cmd(argv, buf, (size_t)total, &r) == 0);
	TEST_CHECK(r.exit_code == 0);
	TEST_CHECK(strstr(r.err, "protocol error") != NULL);

	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	TEST_CHECK(strstr((char *)data, "payload=ok") != NULL);

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Negative exit code in text mode
 * ---------------------------------------------------------------- */

void test_logd_text_negative_exit_code(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "text",
			       NULL };

	unsigned char frame[FRAME_BUF_SIZE];
	int flen = build_frame(frame, EVT_EXIT, STREAM_META, 42,
			       "svc", -1, STATUS_EXITED, NULL, 0);

	struct run_result r;
	TEST_CHECK(run_cmd(argv, frame, (size_t)flen, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	TEST_CHECK(strstr((char *)data, "code=-1") != NULL);
	TEST_CHECK(strstr((char *)data, "status=exited") != NULL);

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Binary mode: negative exit code
 * ---------------------------------------------------------------- */

void test_logd_binary_negative_exit_code(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "binary",
			       NULL };

	unsigned char frame[FRAME_BUF_SIZE];
	int flen = build_frame(frame, EVT_EXIT, STREAM_META, 42,
			       "svc", -1, STATUS_EXITED, NULL, 0);

	struct run_result r;
	TEST_CHECK(run_cmd(argv, frame, (size_t)flen, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	TEST_CHECK(dlen >= 4 + 34);

	/* Check exit code field in binary record */
	unsigned char *rec = data + 4;
	int rcode = read_i32be(rec + 22);
	TEST_CHECK(rcode == -1);

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Binary mode: write error
 * ---------------------------------------------------------------- */

void test_logd_binary_write_error(void)
{
	if (!dev_full_rejects_write()) {
		TEST_MSG("Skipping: /dev/full not available or not rejecting writes");
		return;
	}
	const char *argv[] = { LOGD_PATH,
			       "--file", "/dev/full",
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "binary",
			       NULL };

	unsigned char frame[FRAME_BUF_SIZE];
	int flen = build_frame(frame, EVT_OUTPUT, STREAM_STDOUT, 42,
			       "svc", 0, STATUS_NONE,
			       (const unsigned char *)"data", 4);

	struct run_result r;
	/* /dev/full opens fine but write fails with ENOSPC.
	 * However, ensure_binary_header writes 4 bytes first and
	 * that write to /dev/full will fail. Exit code should be 2. */
	TEST_CHECK(run_cmd(argv, frame, (size_t)flen, &r) == 0);
	TEST_CHECK(r.exit_code == 2);
	run_result_free(&r);
}

/* ----------------------------------------------------------------
 * Binary mode: append to existing binary file (ensure_binary_header)
 * ---------------------------------------------------------------- */

void test_logd_binary_append_existing(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	/* First run: write one record */
	unsigned char frame1[FRAME_BUF_SIZE];
	int flen1 = build_frame(frame1, EVT_OUTPUT, STREAM_STDOUT, 42,
				"svc", 0, STATUS_NONE,
				(const unsigned char *)"first", 5);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "binary",
			       NULL };
	struct run_result r1;
	TEST_CHECK(run_cmd(argv, frame1, (size_t)flen1, &r1) == 0);
	TEST_CHECK(r1.exit_code == 0);
	run_result_free(&r1);

	/* Check file starts with SLG1 */
	size_t len1;
	unsigned char *data1 = read_file(logpath, &len1);
	TEST_CHECK(data1 != NULL);
	TEST_CHECK(len1 >= 4);
	TEST_CHECK(memcmp(data1, "SLG1", 4) == 0);
	size_t first_size = len1;
	free(data1);

	/* Second run: append to existing binary file */
	unsigned char frame2[FRAME_BUF_SIZE];
	int flen2 = build_frame(frame2, EVT_OUTPUT, STREAM_STDOUT, 43,
				"svc", 0, STATUS_NONE,
				(const unsigned char *)"second", 6);
	struct run_result r2;
	TEST_CHECK(run_cmd(argv, frame2, (size_t)flen2, &r2) == 0);
	TEST_CHECK(r2.exit_code == 0);
	run_result_free(&r2);

	/* File should be bigger now and still start with SLG1 */
	size_t len2;
	unsigned char *data2 = read_file(logpath, &len2);
	TEST_CHECK(data2 != NULL);
	TEST_CHECK(len2 > first_size);
	TEST_CHECK(memcmp(data2, "SLG1", 4) == 0);

	free(data2);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Binary mode: existing non-binary file gets truncated
 * ---------------------------------------------------------------- */

void test_logd_binary_truncate_non_binary(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	/* Create a file with non-binary content */
	FILE *fp = fopen(logpath, "w");
	TEST_CHECK(fp != NULL);
	fprintf(fp, "this is text, not SLG1\n");
	fclose(fp);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "binary",
			       NULL };

	unsigned char frame[FRAME_BUF_SIZE];
	int flen = build_frame(frame, EVT_OUTPUT, STREAM_STDOUT, 42,
			       "svc", 0, STATUS_NONE,
			       (const unsigned char *)"hi", 2);

	struct run_result r;
	TEST_CHECK(run_cmd(argv, frame, (size_t)flen, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	/* Should warn about truncation on stderr */
	TEST_CHECK(strstr(r.err, "not a binary log") != NULL);

	/* File should now start with SLG1 */
	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	TEST_CHECK(dlen >= 4);
	TEST_CHECK(memcmp(data, "SLG1", 4) == 0);

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Rotation with non-.log file extension
 * ---------------------------------------------------------------- */

void test_logd_rotation_non_log_extension(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/output.txt", tmpdir);

	int in_pipe[2];
	TEST_CHECK(pipe(in_pipe) == 0);

	pid_t pid = fork();
	TEST_CHECK(pid >= 0);

	if (pid == 0) {
		close(in_pipe[1]);
		dup2(in_pipe[0], STDIN_FILENO);
		close(in_pipe[0]);
		execl(LOGD_PATH, "supervisor-logd",
		      "--file", logpath,
		      "--max-file-size-bytes", "50",
		      (char *)NULL);
		_exit(127);
	}

	close(in_pipe[0]);

	const char *batch =
	    "aaaaaaaaaa" "aaaaaaaaaa" "aaaaaaaaaa"
	    "aaaaaaaaaa" "aaaaaaaaaa" "aaaaaaaaaa";
	write_all(in_pipe[1], batch, strlen(batch));
	msleep(100);

	write_all(in_pipe[1], "bbb\n", 4);
	msleep(100);

	close(in_pipe[1]);
	int status;
	waitpid(pid, &status, 0);
	TEST_CHECK(WIFEXITED(status) && WEXITSTATUS(status) == 0);

	/* Rotated file should exist with timestamp (no .log suffix) */
	struct run_result ls;
	const char *ls_argv[] = { "/bin/ls", tmpdir, NULL };
	TEST_CHECK(run_cmd(ls_argv, NULL, 0, &ls) == 0);
	TEST_CHECK(strstr(ls.out, "output.txt") != NULL);
	/* Should have more than just the one file */
	TEST_CHECK(ls.out_len > (size_t)(strlen("output.txt") + 1));

	run_result_free(&ls);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Framed text mode SIGTERM clean exit
 * ---------------------------------------------------------------- */

void test_logd_framed_sigterm(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	int in_pipe[2];
	TEST_CHECK(pipe(in_pipe) == 0);

	pid_t pid = fork();
	TEST_CHECK(pid >= 0);

	if (pid == 0) {
		close(in_pipe[1]);
		dup2(in_pipe[0], STDIN_FILENO);
		close(in_pipe[0]);
		execl(LOGD_PATH, "supervisor-logd",
		      "--file", logpath,
		      "--max-file-size-bytes", "100000",
		      "--framed", "--unit", "svc",
		      "--format", "text",
		      (char *)NULL);
		_exit(127);
	}

	close(in_pipe[0]);

	unsigned char frame[FRAME_BUF_SIZE];
	int flen = build_frame(frame, EVT_OUTPUT, STREAM_STDOUT, 42,
			       "svc", 0, STATUS_NONE,
			       (const unsigned char *)"data", 4);
	write_all(in_pipe[1], frame, (size_t)flen);
	msleep(100);

	kill(pid, SIGTERM);

	int status;
	waitpid(pid, &status, 0);
	TEST_CHECK(WIFEXITED(status));
	TEST_CHECK(WEXITSTATUS(status) == 0);

	/* Data should be flushed */
	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	TEST_CHECK(strstr((char *)data, "payload=data") != NULL);

	free(data);
	close(in_pipe[1]);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Incomplete frame at EOF (buffered but never completed)
 * ---------------------------------------------------------------- */

void test_logd_incomplete_frame_at_eof(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "text",
			       NULL };

	/* Send just 2 bytes (less than 4-byte length header) then EOF */
	unsigned char partial[2] = { 0x00, 0x01 };

	struct run_result r;
	TEST_CHECK(run_cmd(argv, partial, 2, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	/* Log file should be empty (no complete frames processed) */
	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	TEST_CHECK(dlen == 0);

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Large payload exercising write_full multi-chunk path
 * ---------------------------------------------------------------- */

void test_logd_text_large_payload(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "1000000",
			       "--framed", "--unit", "svc",
			       "--format", "text",
			       NULL };

	/* Build a 10K payload of 'A' characters */
	unsigned char payload[10000];
	memset(payload, 'A', sizeof(payload));

	unsigned char frame[FRAME_BUF_SIZE];
	int flen = build_frame(frame, EVT_OUTPUT, STREAM_STDOUT, 42,
			       "svc", 0, STATUS_NONE,
			       payload, (unsigned int)sizeof(payload));
	TEST_CHECK(flen > 0);

	struct run_result r;
	TEST_CHECK(run_cmd(argv, frame, (size_t)flen, &r) == 0);
	TEST_CHECK(r.exit_code == 0);

	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	/* The escaped payload should be all A's (printable, no escaping) */
	TEST_CHECK(dlen > 10000);

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Framing: unit_len exceeds body capacity (semantic error skip)
 * ---------------------------------------------------------------- */

void test_logd_text_unit_len_overflow(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	const char *argv[] = { LOGD_PATH,
			       "--file", logpath,
			       "--max-file-size-bytes", "100000",
			       "--framed", "--unit", "svc",
			       "--format", "text",
			       NULL };

	unsigned char buf[FRAME_BUF_SIZE];
	int total = 0;

	/* Craft a frame where body_len=14 but unit_len=5.
	 * 13 + 5 = 18 > 14, triggering the unit_len overflow check.
	 * This is a semantic error: parser returns -(4+14) = -18. */
	unsigned char bad[18]; /* 4 header + 14 body */
	memset(bad, 0, sizeof(bad));
	/* body_len = 14 */
	bad[0] = 0; bad[1] = 0; bad[2] = 0; bad[3] = 14;
	/* event=1 (OUTPUT), stream=1 (STDOUT) */
	bad[4] = EVT_OUTPUT; bad[5] = STREAM_STDOUT;
	/* pid=0 (4 bytes at offset 6) */
	/* unit_len = 5 at offset 10-11 (big-endian) */
	bad[10] = 0; bad[11] = 5;
	/* exit_code=0, exit_status=0 (already zeroed) */

	memcpy(buf + total, bad, sizeof(bad));
	total += (int)sizeof(bad);

	/* Valid frame after */
	int n2 = build_frame(buf + total, EVT_OUTPUT, STREAM_STDOUT, 42,
			     "svc", 0, STATUS_NONE,
			     (const unsigned char *)"ok", 2);
	total += n2;

	struct run_result r;
	TEST_CHECK(run_cmd(argv, buf, (size_t)total, &r) == 0);
	TEST_CHECK(r.exit_code == 0);
	TEST_CHECK(strstr(r.err, "protocol error") != NULL);

	size_t dlen;
	unsigned char *data = read_file(logpath, &dlen);
	TEST_CHECK(data != NULL);
	TEST_CHECK(strstr((char *)data, "payload=ok") != NULL);

	free(data);
	run_result_free(&r);
	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ----------------------------------------------------------------
 * Framed: accumulation buffer overflow (> 64K incomplete frame)
 * ---------------------------------------------------------------- */

void test_logd_framed_buffer_overflow(void)
{
	char *tmpdir = make_tmpdir();
	char logpath[512];
	snprintf(logpath, sizeof(logpath), "%s/test.log", tmpdir);

	int in_pipe[2];
	TEST_CHECK(pipe(in_pipe) == 0);

	pid_t pid = fork();
	TEST_CHECK(pid >= 0);

	if (pid == 0) {
		close(in_pipe[1]);
		dup2(in_pipe[0], STDIN_FILENO);
		close(in_pipe[0]);
		execl(LOGD_PATH, "supervisor-logd",
		      "--file", logpath,
		      "--max-file-size-bytes", "1000000",
		      "--framed", "--unit", "svc",
		      "--format", "text",
		      (char *)NULL);
		_exit(127);
	}

	close(in_pipe[0]);

	/* Ignore SIGPIPE: logd will exit on overflow, and our writes
	 * to the closed pipe should not kill the test process. */
	signal(SIGPIPE, SIG_IGN);

	/* Send a 4-byte frame header claiming body_len = 70000.
	 * This makes parse_frame return 0 (incomplete) since
	 * total needed = 70004 > available data.  Then keep sending
	 * data until the 64K accumulation buffer overflows. */
	unsigned char hdr[4];
	hdr[0] = 0; hdr[1] = 1; hdr[2] = 0x11; hdr[3] = 0x70;
	/* body_len = 0x00011170 = 69,936 */
	write_may_fail(in_pipe[1], hdr, 4);

	/* Send 8K chunks to fill the 64K buffer */
	unsigned char chunk[8192];
	memset(chunk, 0, sizeof(chunk));
	for (int i = 0; i < 9; i++) {
		write_may_fail(in_pipe[1], chunk, sizeof(chunk));
		msleep(50);
	}

	close(in_pipe[1]);
	signal(SIGPIPE, SIG_DFL);
	int status;
	waitpid(pid, &status, 0);
	TEST_CHECK(WIFEXITED(status));
	TEST_CHECK(WEXITSTATUS(status) == 0);

	remove_tmpdir(tmpdir);
	free(tmpdir);
}

/* ================================================================ */

TEST_LIST = {
	/* Usage / argument validation */
	{ "logd_missing_file",           test_logd_missing_file },
	{ "logd_missing_max_size",       test_logd_missing_max_size },
	{ "logd_negative_max_size",      test_logd_negative_max_size },
	{ "logd_unknown_option",         test_logd_unknown_option },
	{ "logd_framed_requires_unit",   test_logd_framed_requires_unit },
	{ "logd_bad_format",             test_logd_bad_format },

	/* Raw passthrough */
	{ "logd_raw_passthrough",        test_logd_raw_passthrough },
	{ "logd_raw_empty_input",        test_logd_raw_empty_input },

	/* Text mode: fields and streams */
	{ "logd_text_output_fields",     test_logd_text_output_fields },
	{ "logd_text_stderr_stream",     test_logd_text_stderr_stream },

	/* Text mode: exit events */
	{ "logd_text_exit_exited",       test_logd_text_exit_exited },
	{ "logd_text_exit_signaled",     test_logd_text_exit_signaled },
	{ "logd_text_exit_spawn_failed", test_logd_text_exit_spawn_failed },

	/* Text mode: escaping */
	{ "logd_text_escape_empty",      test_logd_text_escape_empty },
	{ "logd_text_escape_dash",       test_logd_text_escape_dash },
	{ "logd_text_escape_backslash",  test_logd_text_escape_backslash },
	{ "logd_text_escape_newline_cr_tab", test_logd_text_escape_newline_cr_tab },
	{ "logd_text_escape_nul_highbit", test_logd_text_escape_nul_highbit },

	/* Text mode: multiple frames */
	{ "logd_text_multiple_frames",   test_logd_text_multiple_frames },

	/* Protocol error recovery */
	{ "logd_text_invalid_event_skipped",  test_logd_text_invalid_event_skipped },
	{ "logd_text_invalid_stream_skipped", test_logd_text_invalid_stream_skipped },
	{ "logd_text_output_on_meta_rejected", test_logd_text_output_on_meta_rejected },
	{ "logd_text_exit_on_stdout_rejected", test_logd_text_exit_on_stdout_rejected },
	{ "logd_text_truncated_body_recovery", test_logd_text_truncated_body_recovery },

	/* Raw mode: rotation */
	{ "logd_raw_rotation",           test_logd_raw_rotation },

	/* SIGHUP reopen */
	{ "logd_sighup_reopen",          test_logd_sighup_reopen },

	/* Binary mode */
	{ "logd_binary_header",          test_logd_binary_header },
	{ "logd_binary_output_record",   test_logd_binary_output_record },
	{ "logd_binary_exit_record",     test_logd_binary_exit_record },
	{ "logd_binary_rotation_header", test_logd_binary_rotation_header },

	/* File errors */
	{ "logd_bad_file_path",          test_logd_bad_file_path },

	/* Framed default format */
	{ "logd_framed_default_text",    test_logd_framed_default_text },

	/* Text rotation in framed mode */
	{ "logd_text_rotation",          test_logd_text_rotation },

	/* Missing-arg branches */
	{ "logd_log_dir_missing_arg",    test_logd_log_dir_missing_arg },
	{ "logd_prune_cmd_missing_arg",  test_logd_prune_cmd_missing_arg },
	{ "logd_prune_interval_missing_arg", test_logd_prune_interval_missing_arg },
	{ "logd_unit_missing_arg",       test_logd_unit_missing_arg },
	{ "logd_format_missing_arg",     test_logd_format_missing_arg },
	{ "logd_file_missing_arg",       test_logd_file_missing_arg },
	{ "logd_max_size_missing_arg",   test_logd_max_size_missing_arg },

	/* Signals */
	{ "logd_sigterm_clean_exit",     test_logd_sigterm_clean_exit },

	/* Prune */
	{ "logd_prune_on_rotation",      test_logd_prune_on_rotation },
	{ "logd_prune_throttle",         test_logd_prune_throttle },

	/* Framed SIGHUP */
	{ "logd_framed_sighup_reopen",   test_logd_framed_sighup_reopen },
	{ "logd_binary_sighup_reopen",   test_logd_binary_sighup_reopen },

	/* Write errors */
	{ "logd_framed_write_error",     test_logd_framed_write_error },
	{ "logd_raw_write_error",        test_logd_raw_write_error },
	{ "logd_binary_write_error",     test_logd_binary_write_error },

	/* Options accepted */
	{ "logd_log_dir_accepted",       test_logd_log_dir_accepted },

	/* Additional protocol validation */
	{ "logd_text_invalid_exit_status_skipped", test_logd_text_invalid_exit_status_skipped },
	{ "logd_text_negative_exit_code", test_logd_text_negative_exit_code },
	{ "logd_binary_negative_exit_code", test_logd_binary_negative_exit_code },

	/* Binary append and truncation */
	{ "logd_binary_append_existing", test_logd_binary_append_existing },
	{ "logd_binary_truncate_non_binary", test_logd_binary_truncate_non_binary },

	/* Rotation with non-.log extension */
	{ "logd_rotation_non_log_extension", test_logd_rotation_non_log_extension },

	/* Framed SIGTERM */
	{ "logd_framed_sigterm",         test_logd_framed_sigterm },

	/* Incomplete frame */
	{ "logd_incomplete_frame_at_eof", test_logd_incomplete_frame_at_eof },

	/* Large payload */
	{ "logd_text_large_payload",     test_logd_text_large_payload },

	/* Unit_len overflow */
	{ "logd_text_unit_len_overflow", test_logd_text_unit_len_overflow },

	/* Buffer overflow */
	{ "logd_framed_buffer_overflow", test_logd_framed_buffer_overflow },

	{ NULL, NULL }
};
