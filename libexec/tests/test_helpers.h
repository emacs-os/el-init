/*
 * test_helpers.h -- shared utilities for libexec C tests
 *
 * acutest.h: https://github.com/mity/acutest
 * Commit: 31751b4089c93b46a9fd8a8183a695f772de66de
 */

#ifndef TEST_HELPERS_H
#define TEST_HELPERS_H

#include <stddef.h>
#include <sys/types.h>

/* Maximum size for a built wire frame. */
#define FRAME_BUF_SIZE 65536

/* Wire frame event types (must match elinit-logd.c). */
#define EVT_OUTPUT 1
#define EVT_EXIT   2

/* Wire frame stream types. */
#define STREAM_STDOUT 1
#define STREAM_STDERR 2
#define STREAM_META   3

/* Wire frame exit status values. */
#define STATUS_NONE         0
#define STATUS_EXITED       1
#define STATUS_SIGNALED     2
#define STATUS_SPAWN_FAILED 3

/* Build a wire frame into BUF.
 * Returns total frame size (header + body), or -1 on error.
 * BUF must be at least FRAME_BUF_SIZE bytes. */
int build_frame(unsigned char *buf, unsigned char event,
		unsigned char stream, unsigned int pid,
		const char *unit_id, int exit_code,
		unsigned char exit_status, const unsigned char *payload,
		unsigned int payload_len);

/* Result of running a subprocess. */
struct run_result {
	int exit_code;      /* exit status from waitpid */
	char *out;          /* captured stdout (malloc'd, NUL-terminated) */
	size_t out_len;     /* length of stdout data */
	char *err;          /* captured stderr (malloc'd, NUL-terminated) */
	size_t err_len;     /* length of stderr data */
};

/* Run a command with optional stdin data piped in.
 * Returns 0 on success, -1 on fork/pipe error.
 * Caller must free result->out and result->err. */
int run_cmd(const char *const argv[], const unsigned char *stdin_data,
	    size_t stdin_len, struct run_result *result);

/* Free the buffers inside a run_result. */
void run_result_free(struct run_result *r);

/* Create a unique temporary directory under /tmp.
 * Returns a malloc'd string with the path, or NULL on error. */
char *make_tmpdir(void);

/* Recursively remove a directory tree (rm -rf). */
void remove_tmpdir(const char *path);

/* Read an entire file into a malloc'd buffer.
 * Sets *out_len to the number of bytes read.
 * Returns NULL on error. */
unsigned char *read_file(const char *path, size_t *out_len);

/* Read a big-endian u32 from buffer. */
unsigned int read_u32be(const unsigned char *p);

/* Read a big-endian u16 from buffer. */
unsigned short read_u16be(const unsigned char *p);

/* Read a big-endian i32 from buffer. */
int read_i32be(const unsigned char *p);

/* Read a big-endian u64 from buffer. */
unsigned long long read_u64be(const unsigned char *p);

#endif /* TEST_HELPERS_H */
