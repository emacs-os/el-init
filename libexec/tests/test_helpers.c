/*
 * test_helpers.c -- shared utilities for libexec C tests
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * acutest.h: https://github.com/mity/acutest
 * Commit: 31751b4089c93b46a9fd8a8183a695f772de66de
 */

#define _POSIX_C_SOURCE 200809L
#include "test_helpers.h"

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>
#include <dirent.h>

/* ---------- wire frame builder ---------- */

static void put_u32be(unsigned char *p, unsigned int v)
{
	p[0] = (v >> 24) & 0xff;
	p[1] = (v >> 16) & 0xff;
	p[2] = (v >> 8) & 0xff;
	p[3] = v & 0xff;
}

static void put_u16be(unsigned char *p, unsigned short v)
{
	p[0] = (v >> 8) & 0xff;
	p[1] = v & 0xff;
}

int build_frame(unsigned char *buf, unsigned char event,
		unsigned char stream, unsigned int pid,
		const char *unit_id, int exit_code,
		unsigned char exit_status, const unsigned char *payload,
		unsigned int payload_len)
{
	size_t unit_len = unit_id ? strlen(unit_id) : 0;
	/* body = 13 fixed + unit_len + payload_len */
	unsigned int body_len =
	    13 + (unsigned int)unit_len + payload_len;
	size_t total = 4 + (size_t)body_len;

	if (total > FRAME_BUF_SIZE)
		return -1;

	/* Length header */
	put_u32be(buf, body_len);

	/* Fixed fields */
	unsigned char *body = buf + 4;
	body[0] = event;
	body[1] = stream;
	put_u32be(body + 2, pid);
	put_u16be(body + 6, (unsigned short)unit_len);

	/* exit_code as signed i32be */
	put_u32be(body + 8, (unsigned int)exit_code);

	body[12] = exit_status;

	/* unit_id */
	if (unit_len > 0)
		memcpy(body + 13, unit_id, unit_len);

	/* payload */
	if (payload_len > 0)
		memcpy(body + 13 + unit_len, payload, payload_len);

	return (int)total;
}

/* ---------- endian readers ---------- */

unsigned int read_u32be(const unsigned char *p)
{
	return ((unsigned int)p[0] << 24) | ((unsigned int)p[1] << 16) |
	       ((unsigned int)p[2] << 8) | (unsigned int)p[3];
}

unsigned short read_u16be(const unsigned char *p)
{
	return (unsigned short)(((unsigned int)p[0] << 8) |
				(unsigned int)p[1]);
}

int read_i32be(const unsigned char *p)
{
	unsigned int u = read_u32be(p);
	return (int)u;
}

unsigned long long read_u64be(const unsigned char *p)
{
	unsigned long long hi = read_u32be(p);
	unsigned long long lo = read_u32be(p + 4);
	return (hi << 32) | lo;
}

/* ---------- process runner ---------- */

/* Read all data from fd into a malloc'd buffer.
 * Returns the buffer and sets *out_len. */
static char *drain_fd(int fd, size_t *out_len)
{
	size_t cap = 4096;
	size_t len = 0;
	char *buf = malloc(cap);
	if (!buf)
		return NULL;

	for (;;) {
		if (len + 1024 > cap) {
			cap *= 2;
			char *tmp = realloc(buf, cap);
			if (!tmp) {
				free(buf);
				return NULL;
			}
			buf = tmp;
		}
		ssize_t n = read(fd, buf + len, cap - len - 1);
		if (n < 0) {
			if (errno == EINTR)
				continue;
			break;
		}
		if (n == 0)
			break;
		len += (size_t)n;
	}
	buf[len] = '\0';
	*out_len = len;
	return buf;
}

int run_cmd(const char *const argv[], const unsigned char *stdin_data,
	    size_t stdin_len, struct run_result *result)
{
	int in_pipe[2] = { -1, -1 };
	int out_pipe[2] = { -1, -1 };
	int err_pipe[2] = { -1, -1 };

	memset(result, 0, sizeof(*result));

	if (pipe(in_pipe) < 0 || pipe(out_pipe) < 0 || pipe(err_pipe) < 0)
		goto fail;

	pid_t pid = fork();
	if (pid < 0)
		goto fail;

	if (pid == 0) {
		/* Child */
		close(in_pipe[1]);
		close(out_pipe[0]);
		close(err_pipe[0]);
		dup2(in_pipe[0], STDIN_FILENO);
		dup2(out_pipe[1], STDOUT_FILENO);
		dup2(err_pipe[1], STDERR_FILENO);
		close(in_pipe[0]);
		close(out_pipe[1]);
		close(err_pipe[1]);
		execv(argv[0], (char *const *)argv);
		_exit(127);
	}

	/* Parent */
	close(in_pipe[0]);
	in_pipe[0] = -1;
	close(out_pipe[1]);
	out_pipe[1] = -1;
	close(err_pipe[1]);
	err_pipe[1] = -1;

	/* Write stdin data */
	if (stdin_data && stdin_len > 0) {
		size_t written = 0;
		while (written < stdin_len) {
			ssize_t w = write(in_pipe[1],
					  stdin_data + written,
					  stdin_len - written);
			if (w < 0) {
				if (errno == EINTR)
					continue;
				break;
			}
			written += (size_t)w;
		}
	}
	close(in_pipe[1]);
	in_pipe[1] = -1;

	/* Drain stdout and stderr */
	result->out = drain_fd(out_pipe[0], &result->out_len);
	close(out_pipe[0]);
	out_pipe[0] = -1;

	result->err = drain_fd(err_pipe[0], &result->err_len);
	close(err_pipe[0]);
	err_pipe[0] = -1;

	/* Wait for child */
	int status;
	while (waitpid(pid, &status, 0) < 0 && errno == EINTR)
		;

	if (WIFEXITED(status))
		result->exit_code = WEXITSTATUS(status);
	else
		result->exit_code = -1;

	return 0;

fail:
	if (in_pipe[0] >= 0)
		close(in_pipe[0]);
	if (in_pipe[1] >= 0)
		close(in_pipe[1]);
	if (out_pipe[0] >= 0)
		close(out_pipe[0]);
	if (out_pipe[1] >= 0)
		close(out_pipe[1]);
	if (err_pipe[0] >= 0)
		close(err_pipe[0]);
	if (err_pipe[1] >= 0)
		close(err_pipe[1]);
	return -1;
}

void run_result_free(struct run_result *r)
{
	free(r->out);
	free(r->err);
	r->out = NULL;
	r->err = NULL;
}

/* ---------- temp directory ---------- */

char *make_tmpdir(void)
{
	char tmpl[] = "/tmp/sv-test-XXXXXX";
	char *dir = mkdtemp(tmpl);
	if (!dir)
		return NULL;
	return strdup(dir);
}

/* Recursively remove a directory tree. */
void remove_tmpdir(const char *path)
{
	DIR *d = opendir(path);
	if (!d)
		return;

	struct dirent *ent;
	char child[4096];
	while ((ent = readdir(d)) != NULL) {
		if (strcmp(ent->d_name, ".") == 0 ||
		    strcmp(ent->d_name, "..") == 0)
			continue;
		snprintf(child, sizeof(child), "%s/%s", path, ent->d_name);
		struct stat st;
		if (lstat(child, &st) == 0 && S_ISDIR(st.st_mode))
			remove_tmpdir(child);
		else
			unlink(child);
	}
	closedir(d);
	rmdir(path);
}

/* ---------- file I/O ---------- */

unsigned char *read_file(const char *path, size_t *out_len)
{
	int fd = open(path, O_RDONLY);
	if (fd < 0)
		return NULL;

	struct stat st;
	if (fstat(fd, &st) < 0) {
		close(fd);
		return NULL;
	}

	size_t sz = (size_t)st.st_size;
	unsigned char *buf = malloc(sz + 1);
	if (!buf) {
		close(fd);
		return NULL;
	}

	size_t total = 0;
	while (total < sz) {
		ssize_t n = read(fd, buf + total, sz - total);
		if (n < 0) {
			if (errno == EINTR)
				continue;
			free(buf);
			close(fd);
			return NULL;
		}
		if (n == 0)
			break;
		total += (size_t)n;
	}
	close(fd);
	buf[total] = '\0';
	*out_len = total;
	return buf;
}
