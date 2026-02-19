/*
 * elinit-runas - privilege-drop helper for elinit
 *
 * Drops privileges to the specified user/group and execve's
 * the target command.  No shell mediation.
 *
 * Usage:
 *   elinit-runas --user USER [--group GROUP] -- COMMAND [ARGS...]
 *   elinit-runas --group GROUP -- COMMAND [ARGS...]
 *
 * USER/GROUP may be a name or numeric id.
 *
 * Privilege-drop order:
 *   1. Resolve user and group
 *   2. initgroups (supplementary groups)
 *   3. setgid (primary group)
 *   4. setuid (target user)
 *   5. execve target command
 *
 * Exit codes:
 *   111  usage error (bad arguments)
 *   112  identity resolution failed (unknown user/group)
 *   113  privilege operation failed (setuid/setgid/initgroups)
 *   114  exec failed
 *
 * Copyright (C) 2026 elinit contributors
 * License: GPL-3.0-or-later
 */

#define _GNU_SOURCE
#include <errno.h>
#include <grp.h>
#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

enum {
	EXIT_USAGE = 111,
	EXIT_RESOLVE = 112,
	EXIT_PRIVDROP = 113,
	EXIT_EXEC = 114
};

/* Return 1 if s is entirely digits (non-empty). */
static int is_numeric(const char *s)
{
	if (!s || !*s)
		return 0;
	for (; *s; s++) {
		if (*s < '0' || *s > '9')
			return 0;
	}
	return 1;
}

int main(int argc, char **argv)
{
	const char *user_arg = NULL;
	const char *group_arg = NULL;
	int cmd_start = -1;
	int i;

	/* Parse arguments before "--" */
	for (i = 1; i < argc; i++) {
		if (strcmp(argv[i], "--") == 0) {
			cmd_start = i + 1;
			break;
		}
		if (strcmp(argv[i], "--user") == 0) {
			if (i + 1 >= argc) {
				fprintf(stderr, "elinit-runas: --user "
						"requires an argument\n");
				return EXIT_USAGE;
			}
			user_arg = argv[++i];
		} else if (strcmp(argv[i], "--group") == 0) {
			if (i + 1 >= argc) {
				fprintf(stderr, "elinit-runas: --group "
						"requires an argument\n");
				return EXIT_USAGE;
			}
			group_arg = argv[++i];
		} else {
			fprintf(stderr,
				"elinit-runas: unknown option: %s\n",
				argv[i]);
			return EXIT_USAGE;
		}
	}

	if (cmd_start < 0 || cmd_start >= argc) {
		fprintf(stderr,
			"elinit-runas: missing command after \"--\"\n"
			"usage: elinit-runas --user USER [--group GROUP] "
			"-- CMD [ARGS...]\n");
		return EXIT_USAGE;
	}

	if (!user_arg && !group_arg) {
		fprintf(stderr, "elinit-runas: at least one of --user or "
				"--group required\n");
		return EXIT_USAGE;
	}

	uid_t target_uid = 0;
	gid_t target_gid = 0;
	int have_uid = 0;
	int have_gid = 0;
	const char *user_name = NULL; /* for initgroups */

	/* Resolve user */
	if (user_arg) {
		if (is_numeric(user_arg)) {
			target_uid = (uid_t)atol(user_arg);
			/* Look up name for initgroups */
			struct passwd *pw = getpwuid(target_uid);
			if (pw)
				user_name = pw->pw_name;
			have_uid = 1;
			/* If no group specified, use user's primary group */
			if (!group_arg) {
				if (!pw) {
					fprintf(
					    stderr,
					    "elinit-runas: uid %s exists "
					    "but has no passwd entry "
					    "(cannot determine primary "
					    "group)\n",
					    user_arg);
					return EXIT_RESOLVE;
				}
				target_gid = pw->pw_gid;
				have_gid = 1;
			}
		} else {
			struct passwd *pw = getpwnam(user_arg);
			if (!pw) {
				fprintf(stderr,
					"elinit-runas: unknown user: %s\n",
					user_arg);
				return EXIT_RESOLVE;
			}
			target_uid = pw->pw_uid;
			user_name = pw->pw_name;
			have_uid = 1;
			/* If no group specified, use user's primary group */
			if (!group_arg) {
				target_gid = pw->pw_gid;
				have_gid = 1;
			}
		}
	}

	/* Resolve group */
	if (group_arg) {
		if (is_numeric(group_arg)) {
			target_gid = (gid_t)atol(group_arg);
		} else {
			struct group *gr = getgrnam(group_arg);
			if (!gr) {
				fprintf(stderr,
					"elinit-runas: unknown group: %s\n",
					group_arg);
				return EXIT_RESOLVE;
			}
			target_gid = gr->gr_gid;
		}
		have_gid = 1;
	}

	/* Step 1: Set supplementary groups */
	if (user_name) {
		if (initgroups(user_name, target_gid) != 0) {
			fprintf(stderr,
				"elinit-runas: initgroups(%s, %u): %s\n",
				user_name, (unsigned)target_gid,
				strerror(errno));
			return EXIT_PRIVDROP;
		}
	} else if (have_gid) {
		/* No user name available; set supplementary groups to just the
		 * target gid (group-only mode). */
		if (setgroups(1, &target_gid) != 0) {
			fprintf(stderr, "elinit-runas: setgroups: %s\n",
				strerror(errno));
			return EXIT_PRIVDROP;
		}
	}

	/* Step 2: Set primary group */
	if (have_gid) {
		if (setgid(target_gid) != 0) {
			fprintf(stderr, "elinit-runas: setgid(%u): %s\n",
				(unsigned)target_gid, strerror(errno));
			return EXIT_PRIVDROP;
		}
	}

	/* Step 3: Set uid (must be last — after this we lose privilege) */
	if (have_uid) {
		if (setuid(target_uid) != 0) {
			fprintf(stderr, "elinit-runas: setuid(%u): %s\n",
				(unsigned)target_uid, strerror(errno));
			return EXIT_PRIVDROP;
		}
	}

	/* Step 4: exec the target command (direct execv, no PATH search) */
	execv(argv[cmd_start], &argv[cmd_start]);

	/* If we get here, exec failed */
	fprintf(stderr, "elinit-runas: exec %s: %s\n", argv[cmd_start],
		strerror(errno));
	return EXIT_EXEC;
}
