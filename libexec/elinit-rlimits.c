/*
 * elinit-rlimits - resource limit helper for elinit
 *
 * Sets ulimit-style resource limits (via setrlimit) and then
 * execv's the target command.  No shell mediation.
 *
 * Usage:
 *   elinit-rlimits [--nofile SOFT:HARD] [--nproc SOFT:HARD] \
 *                      [--core SOFT:HARD] [--fsize SOFT:HARD]   \
 *                      [--as SOFT:HARD] -- COMMAND [ARGS...]
 *
 * Each limit value is SOFT:HARD where SOFT and HARD are
 * non-negative integers or the word "infinity".
 *
 * Exit codes:
 *   111  usage error (bad arguments)
 *   112  setrlimit failed
 *   114  exec failed
 *
 * Copyright (C) 2026 elinit contributors
 * License: GPL-3.0-or-later
 */

#define _POSIX_C_SOURCE 200809L
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <unistd.h>

enum {
	EXIT_USAGE    = 111,
	EXIT_RLIMIT   = 112,
	EXIT_EXEC     = 114
};

struct limit_spec {
	int resource;
	const char *name;
	rlim_t soft;
	rlim_t hard;
	int set;
};

/* Parse a "SOFT:HARD" string into soft and hard values.
 * Returns 0 on success, -1 on error. */
static int parse_limit(const char *arg, rlim_t *soft, rlim_t *hard)
{
	const char *colon;
	char *end;

	if (!arg || !*arg)
		return -1;

	colon = strchr(arg, ':');
	if (!colon)
		return -1;

	/* Parse soft */
	if (colon == arg)
		return -1;
	if (strncmp(arg, "infinity", (size_t)(colon - arg)) == 0 &&
	    (size_t)(colon - arg) == 8) {
		*soft = RLIM_INFINITY;
	} else {
		unsigned long long v;
		end = NULL;
		v = strtoull(arg, &end, 10);
		if (end != colon || v == 0xFFFFFFFFFFFFFFFFULL)
			return -1;
		*soft = (rlim_t)v;
	}

	/* Parse hard */
	{
		const char *hstr = colon + 1;
		if (!*hstr)
			return -1;
		if (strcmp(hstr, "infinity") == 0) {
			*hard = RLIM_INFINITY;
		} else {
			unsigned long long v;
			end = NULL;
			v = strtoull(hstr, &end, 10);
			if (*end != '\0' || v == 0xFFFFFFFFFFFFFFFFULL)
				return -1;
			*hard = (rlim_t)v;
		}
	}

	return 0;
}

int main(int argc, char **argv)
{
	struct limit_spec limits[5];
	int nlimits = 0;
	int cmd_start = -1;
	int i;

	memset(limits, 0, sizeof(limits));

	/* Initialize limit entries */
	limits[0].resource = RLIMIT_NOFILE;
	limits[0].name = "nofile";
	limits[1].resource = RLIMIT_NPROC;
	limits[1].name = "nproc";
	limits[2].resource = RLIMIT_CORE;
	limits[2].name = "core";
	limits[3].resource = RLIMIT_FSIZE;
	limits[3].name = "fsize";
	limits[4].resource = RLIMIT_AS;
	limits[4].name = "as";

	if (argc < 2) {
		fprintf(stderr, "elinit-rlimits: usage: elinit-rlimits "
				"[--nofile S:H] [--nproc S:H] [--core S:H] "
				"[--fsize S:H] [--as S:H] -- CMD [ARGS...]\n");
		return EXIT_USAGE;
	}

	/* Parse arguments before "--" */
	for (i = 1; i < argc; i++) {
		if (strcmp(argv[i], "--") == 0) {
			cmd_start = i + 1;
			break;
		}
		if (strcmp(argv[i], "--nofile") == 0) {
			if (i + 1 >= argc) {
				fprintf(stderr, "elinit-rlimits: "
						"--nofile requires an argument\n");
				return EXIT_USAGE;
			}
			if (parse_limit(argv[++i], &limits[0].soft,
					&limits[0].hard) < 0) {
				fprintf(stderr, "elinit-rlimits: "
						"invalid --nofile value: %s\n",
					argv[i]);
				return EXIT_USAGE;
			}
			limits[0].set = 1;
		} else if (strcmp(argv[i], "--nproc") == 0) {
			if (i + 1 >= argc) {
				fprintf(stderr, "elinit-rlimits: "
						"--nproc requires an argument\n");
				return EXIT_USAGE;
			}
			if (parse_limit(argv[++i], &limits[1].soft,
					&limits[1].hard) < 0) {
				fprintf(stderr, "elinit-rlimits: "
						"invalid --nproc value: %s\n",
					argv[i]);
				return EXIT_USAGE;
			}
			limits[1].set = 1;
		} else if (strcmp(argv[i], "--core") == 0) {
			if (i + 1 >= argc) {
				fprintf(stderr, "elinit-rlimits: "
						"--core requires an argument\n");
				return EXIT_USAGE;
			}
			if (parse_limit(argv[++i], &limits[2].soft,
					&limits[2].hard) < 0) {
				fprintf(stderr, "elinit-rlimits: "
						"invalid --core value: %s\n",
					argv[i]);
				return EXIT_USAGE;
			}
			limits[2].set = 1;
		} else if (strcmp(argv[i], "--fsize") == 0) {
			if (i + 1 >= argc) {
				fprintf(stderr, "elinit-rlimits: "
						"--fsize requires an argument\n");
				return EXIT_USAGE;
			}
			if (parse_limit(argv[++i], &limits[3].soft,
					&limits[3].hard) < 0) {
				fprintf(stderr, "elinit-rlimits: "
						"invalid --fsize value: %s\n",
					argv[i]);
				return EXIT_USAGE;
			}
			limits[3].set = 1;
		} else if (strcmp(argv[i], "--as") == 0) {
			if (i + 1 >= argc) {
				fprintf(stderr, "elinit-rlimits: "
						"--as requires an argument\n");
				return EXIT_USAGE;
			}
			if (parse_limit(argv[++i], &limits[4].soft,
					&limits[4].hard) < 0) {
				fprintf(stderr, "elinit-rlimits: "
						"invalid --as value: %s\n",
					argv[i]);
				return EXIT_USAGE;
			}
			limits[4].set = 1;
		} else {
			fprintf(stderr, "elinit-rlimits: "
					"unknown option: %s\n", argv[i]);
			return EXIT_USAGE;
		}
	}

	if (cmd_start < 0 || cmd_start >= argc) {
		fprintf(stderr, "elinit-rlimits: "
				"missing -- separator or command\n");
		return EXIT_USAGE;
	}

	/* Count how many limits are being set */
	for (i = 0; i < 5; i++) {
		if (limits[i].set)
			nlimits++;
	}

	if (nlimits == 0) {
		fprintf(stderr, "elinit-rlimits: "
				"no limits specified\n");
		return EXIT_USAGE;
	}

	/* Apply limits */
	for (i = 0; i < 5; i++) {
		if (limits[i].set) {
			struct rlimit rl;
			rl.rlim_cur = limits[i].soft;
			rl.rlim_max = limits[i].hard;
			if (setrlimit(limits[i].resource, &rl) < 0) {
				fprintf(stderr, "elinit-rlimits: "
						"setrlimit %s failed: %s\n",
					limits[i].name, strerror(errno));
				return EXIT_RLIMIT;
			}
		}
	}

	/* exec the target command */
	execvp(argv[cmd_start], &argv[cmd_start]);

	/* Only reached on exec failure */
	fprintf(stderr, "elinit-rlimits: exec %s: %s\n",
		argv[cmd_start], strerror(errno));
	return EXIT_EXEC;
}
