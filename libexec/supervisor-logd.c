/*
 * supervisor-logd - per-service log writer for supervisor.el
 *
 * Reads service output from stdin and writes to a log file with
 * append semantics.  Keeps the file descriptor open during steady
 * state for efficiency.
 *
 * Usage:
 *   supervisor-logd --file PATH --max-file-size-bytes N [OPTIONS]
 *
 * Required flags:
 *   --file PATH              Target log file
 *   --max-file-size-bytes N  Rotate when file exceeds N bytes
 *
 * Optional flags:
 *   --log-dir DIR            Directory for prune trigger context
 *   --prune-cmd CMD          Command to run after local rotation
 *   --prune-min-interval-sec N  Minimum seconds between prune calls
 *
 * Signal behavior:
 *   HUP:       Close and reopen the target file
 *   TERM/INT:  Flush and exit cleanly
 *
 * Local rotation: when the file exceeds the size cap, it is renamed
 * to <base>.YYYYMMDD-HHMMSS.log and a fresh file is opened.
 *
 * Exit codes:
 *   0    Clean exit (stdin EOF or TERM/INT)
 *   1    Usage error
 *   2    File operation error
 *
 * Copyright (C) 2026 supervisor.el contributors
 * License: GPL-3.0-or-later
 */

#define _POSIX_C_SOURCE 200809L
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

enum { EXIT_OK = 0, EXIT_USAGE = 1, EXIT_IO = 2 };

static volatile sig_atomic_t got_hup = 0;
static volatile sig_atomic_t got_term = 0;

static void handle_hup(int sig)
{
	(void)sig;
	got_hup = 1;
}

static void handle_term(int sig)
{
	(void)sig;
	got_term = 1;
}

static void handle_chld(int sig)
{
	(void)sig;
	/* Reap all finished children to prevent zombies. */
	while (waitpid(-1, NULL, WNOHANG) > 0)
		;
}

/* Build a unique rotated file name.
 * Format: <base>.YYYYMMDD-HHMMSS[.N].log
 * If the candidate already exists, appends .1, .2, ... until unique.
 * Returns a malloc'd string or NULL on error. */
static char *make_rotated_name(const char *path)
{
	time_t now = time(NULL);
	struct tm tm;
	char stamp[64];
	size_t pathlen = strlen(path);
	size_t baselen;
	const char *ext = "";
	/* Allocate generously: base + "." + stamp + ".99999" + ".log" + NUL */
	size_t alloc;
	char *result;
	int seq;

	if (localtime_r(&now, &tm) == NULL)
		return NULL;

	snprintf(stamp, sizeof(stamp), "%04d%02d%02d-%02d%02d%02d",
		 tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday, tm.tm_hour,
		 tm.tm_min, tm.tm_sec);

	if (pathlen >= 4 && strcmp(path + pathlen - 4, ".log") == 0) {
		baselen = pathlen - 4;
		ext = ".log";
	} else {
		baselen = pathlen;
	}

	alloc = baselen + 1 + strlen(stamp) + 8 + strlen(ext) + 1;
	result = malloc(alloc);
	if (!result)
		return NULL;

	/* Try without sequence suffix first */
	snprintf(result, alloc, "%.*s.%s%s", (int)baselen, path, stamp, ext);

	for (seq = 1; access(result, F_OK) == 0 && seq < 100000; seq++)
		snprintf(result, alloc, "%.*s.%s.%d%s", (int)baselen, path,
			 stamp, seq, ext);

	return result;
}

/* Get current file size via fstat.  Returns -1 on error. */
static off_t file_size(int fd)
{
	struct stat st;
	if (fstat(fd, &st) != 0)
		return -1;
	return st.st_size;
}

/* Open the log file for appending.  Returns fd or -1. */
static int open_log(const char *path)
{
	return open(path, O_WRONLY | O_CREAT | O_APPEND, 0644);
}

/* Run prune command in background (fire-and-forget).
 * The SIGCHLD handler reaps the child when it exits. */
static void run_prune(const char *cmd)
{
	pid_t pid = fork();
	if (pid == 0) {
		/* Child: exec the prune command via shell */
		execl("/bin/sh", "sh", "-c", cmd, (char *)NULL);
		_exit(127);
	}
	/* Parent: child reaped by handle_chld via SIGCHLD */
	(void)pid;
}

static void usage(void)
{
	fprintf(stderr,
		"usage: supervisor-logd --file PATH --max-file-size-bytes N "
		"[--log-dir DIR] [--prune-cmd CMD] "
		"[--prune-min-interval-sec N]\n");
}

int main(int argc, char **argv)
{
	const char *file_path = NULL;
	long long max_size = 0;
	const char *log_dir = NULL;
	const char *prune_cmd = NULL;
	long prune_interval = 0;
	int i;

	/* Parse arguments */
	for (i = 1; i < argc; i++) {
		if (strcmp(argv[i], "--file") == 0) {
			if (++i >= argc) {
				fprintf(stderr, "supervisor-logd: --file "
						"requires an argument\n");
				return EXIT_USAGE;
			}
			file_path = argv[i];
		} else if (strcmp(argv[i], "--max-file-size-bytes") == 0) {
			if (++i >= argc) {
				fprintf(
				    stderr,
				    "supervisor-logd: --max-file-size-bytes "
				    "requires an argument\n");
				return EXIT_USAGE;
			}
			max_size = atoll(argv[i]);
			if (max_size <= 0) {
				fprintf(
				    stderr,
				    "supervisor-logd: --max-file-size-bytes "
				    "must be positive\n");
				return EXIT_USAGE;
			}
		} else if (strcmp(argv[i], "--log-dir") == 0) {
			if (++i >= argc) {
				fprintf(stderr, "supervisor-logd: --log-dir "
						"requires an argument\n");
				return EXIT_USAGE;
			}
			log_dir = argv[i];
		} else if (strcmp(argv[i], "--prune-cmd") == 0) {
			if (++i >= argc) {
				fprintf(stderr, "supervisor-logd: --prune-cmd "
						"requires an argument\n");
				return EXIT_USAGE;
			}
			prune_cmd = argv[i];
		} else if (strcmp(argv[i], "--prune-min-interval-sec") == 0) {
			if (++i >= argc) {
				fprintf(
				    stderr,
				    "supervisor-logd: --prune-min-interval-sec "
				    "requires an argument\n");
				return EXIT_USAGE;
			}
			prune_interval = atol(argv[i]);
		} else {
			fprintf(stderr, "supervisor-logd: unknown option: %s\n",
				argv[i]);
			return EXIT_USAGE;
		}
	}

	if (!file_path) {
		fprintf(stderr, "supervisor-logd: --file is required\n");
		usage();
		return EXIT_USAGE;
	}
	if (max_size <= 0) {
		fprintf(stderr,
			"supervisor-logd: --max-file-size-bytes is required\n");
		usage();
		return EXIT_USAGE;
	}

	/* Suppress unused warning when log_dir is not used yet */
	(void)log_dir;

	/* Install signal handlers */
	{
		struct sigaction sa;
		memset(&sa, 0, sizeof(sa));

		sa.sa_handler = handle_hup;
		sa.sa_flags = 0; /* no SA_RESTART: interrupt read() on HUP */
		sigemptyset(&sa.sa_mask);
		sigaction(SIGHUP, &sa, NULL);

		sa.sa_handler = handle_term;
		sigaction(SIGTERM, &sa, NULL);
		sigaction(SIGINT, &sa, NULL);

		sa.sa_handler = handle_chld;
		sa.sa_flags = SA_RESTART | SA_NOCLDSTOP;
		sigaction(SIGCHLD, &sa, NULL);
	}

	/* Open log file */
	int fd = open_log(file_path);
	if (fd < 0) {
		fprintf(stderr, "supervisor-logd: open %s: %s\n", file_path,
			strerror(errno));
		return EXIT_IO;
	}

	off_t current_size = file_size(fd);
	if (current_size < 0)
		current_size = 0;

	time_t last_prune = 0;
	char buf[8192];
	ssize_t n;

	while (!got_term) {
		/* Check for HUP (reopen) */
		if (got_hup) {
			got_hup = 0;
			close(fd);
			fd = open_log(file_path);
			if (fd < 0) {
				fprintf(stderr,
					"supervisor-logd: reopen %s: %s\n",
					file_path, strerror(errno));
				return EXIT_IO;
			}
			current_size = file_size(fd);
			if (current_size < 0)
				current_size = 0;
		}

		n = read(STDIN_FILENO, buf, sizeof(buf));

		if (n < 0) {
			if (errno == EINTR)
				continue; /* signal interrupted read; re-check
					     flags */
			/* Real read error */
			break;
		}
		if (n == 0)
			break; /* EOF - service closed its output */

		/* Write to log file */
		{
			ssize_t written = 0;
			while (written < n) {
				ssize_t w = write(fd, buf + written,
						  (size_t)(n - written));
				if (w < 0) {
					if (errno == EINTR)
						continue;
					fprintf(
					    stderr,
					    "supervisor-logd: write %s: %s\n",
					    file_path, strerror(errno));
					close(fd);
					return EXIT_IO;
				}
				written += w;
			}
			current_size += n;
		}

		/* Check size cap and rotate if needed */
		if (current_size >= (off_t)max_size) {
			char *rotated = make_rotated_name(file_path);
			if (rotated) {
				if (rename(file_path, rotated) != 0) {
					fprintf(stderr,
						"supervisor-logd: rotate %s -> "
						"%s: %s\n",
						file_path, rotated,
						strerror(errno));
					free(rotated);
					/* Rotation failed; truncate the file to
					 * enforce cap */
					if (ftruncate(fd, 0) == 0) {
						lseek(fd, 0, SEEK_SET);
						current_size = 0;
					}
					continue;
				}
				close(fd);
				fd = open_log(file_path);
				if (fd < 0) {
					fprintf(stderr,
						"supervisor-logd: reopen after "
						"rotate %s: %s\n",
						file_path, strerror(errno));
					free(rotated);
					return EXIT_IO;
				}
				current_size = 0;
				free(rotated);

				/* Trigger prune if configured and throttle
				 * allows */
				if (prune_cmd) {
					time_t now = time(NULL);
					if (now - last_prune >=
					    prune_interval) {
						last_prune = now;
						run_prune(prune_cmd);
					}
				}
			}
		}
	}

	/* Clean exit: flush and close */
	close(fd);
	return EXIT_OK;
}
