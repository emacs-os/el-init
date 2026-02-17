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
 *   --framed                 Enable framed transport protocol on stdin
 *   --unit ID                Unit identifier (required with --framed)
 *   --format text|binary     Output record format (default: text)
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

/* Output format modes */
enum format_mode { FMT_RAW = 0, FMT_TEXT = 1, FMT_BINARY = 2 };

/* Frame event types (wire protocol) */
enum frame_event { EVT_OUTPUT = 1, EVT_EXIT = 2 };

/* Frame stream types (wire protocol) */
enum frame_stream { STREAM_STDOUT = 1, STREAM_STDERR = 2, STREAM_META = 3 };

/* Frame exit status (wire protocol) */
enum frame_exit_status {
	STATUS_NONE = 0,
	STATUS_EXITED = 1,
	STATUS_SIGNALED = 2,
	STATUS_SPAWN_FAILED = 3
};

/* Parsed frame from the wire protocol */
struct frame {
	unsigned char event;
	unsigned char stream;
	unsigned int pid;
	unsigned short unit_len;
	int exit_code;
	unsigned char exit_status;
	const unsigned char *unit_id; /* points into buffer, not owned */
	const unsigned char *payload; /* points into buffer, not owned */
	unsigned int payload_len;
};

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

/* Read a big-endian u32 from buffer. */
static unsigned int read_u32be(const unsigned char *p)
{
	return ((unsigned int)p[0] << 24) | ((unsigned int)p[1] << 16) |
	       ((unsigned int)p[2] << 8) | (unsigned int)p[3];
}

/* Read a big-endian u16 from buffer. */
static unsigned short read_u16be(const unsigned char *p)
{
	return (unsigned short)(((unsigned int)p[0] << 8) |
				(unsigned int)p[1]);
}

/* Read a big-endian i32 (signed) from buffer. */
static int read_i32be(const unsigned char *p)
{
	unsigned int u = read_u32be(p);
	return (int)u;
}

/* Parse one frame from buffer.
 * Returns bytes consumed (header + body) on success,
 * 0 if buffer is incomplete, -1 on protocol error. */
static int parse_frame(const unsigned char *buf, size_t len,
		       struct frame *out)
{
	if (len < 4)
		return 0; /* need length header */

	unsigned int body_len = read_u32be(buf);
	if (body_len < 13)
		return -1; /* body too small for fixed fields */

	size_t total = 4 + (size_t)body_len;
	if (len < total)
		return 0; /* incomplete frame */

	const unsigned char *body = buf + 4;
	out->event = body[0];
	out->stream = body[1];
	out->pid = read_u32be(body + 2);
	out->unit_len = read_u16be(body + 6);
	out->exit_code = read_i32be(body + 8);
	out->exit_status = body[12];

	/* Validate unit_len fits within body */
	if (13 + (unsigned int)out->unit_len > body_len)
		return -1;

	out->unit_id = body + 13;
	out->payload = body + 13 + out->unit_len;
	out->payload_len =
	    body_len - 13 - (unsigned int)out->unit_len;

	return (int)total;
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

/* Full write with retry on EINTR. */
static ssize_t write_full(int fd, const void *data, size_t len)
{
	const unsigned char *p = data;
	size_t written = 0;
	while (written < len) {
		ssize_t w = write(fd, p + written, len - written);
		if (w < 0) {
			if (errno == EINTR)
				continue;
			return -1;
		}
		written += (size_t)w;
	}
	return (ssize_t)written;
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

/* Rotate the log file: rename current, open fresh, trigger prune.
 * Returns the new fd, or -1 on error. Updates *current_size. */
static int rotate_log(int fd, const char *file_path,
		      off_t *current_size, const char *prune_cmd,
		      long prune_interval, time_t *last_prune)
{
	char *rotated = make_rotated_name(file_path);
	if (!rotated) {
		/* Rotation name failed; truncate as fallback */
		if (ftruncate(fd, 0) == 0) {
			lseek(fd, 0, SEEK_SET);
			*current_size = 0;
		}
		return fd;
	}

	if (rename(file_path, rotated) != 0) {
		fprintf(stderr, "supervisor-logd: rotate %s -> %s: %s\n",
			file_path, rotated, strerror(errno));
		free(rotated);
		if (ftruncate(fd, 0) == 0) {
			lseek(fd, 0, SEEK_SET);
			*current_size = 0;
		}
		return fd;
	}

	close(fd);
	int new_fd = open_log(file_path);
	if (new_fd < 0) {
		fprintf(stderr,
			"supervisor-logd: reopen after rotate %s: %s\n",
			file_path, strerror(errno));
		free(rotated);
		return -1;
	}
	*current_size = 0;
	free(rotated);

	/* Trigger prune if configured and throttle allows */
	if (prune_cmd) {
		time_t now = time(NULL);
		if (now - *last_prune >= prune_interval) {
			*last_prune = now;
			run_prune(prune_cmd);
		}
	}
	return new_fd;
}

/* Write a text exit marker for framed mode without --format text
 * (Phase 2 basic framed behavior). */
static ssize_t write_exit_marker(int fd, const struct frame *f)
{
	const char *status_str;
	switch (f->exit_status) {
	case STATUS_EXITED:
		status_str = "exited";
		break;
	case STATUS_SIGNALED:
		status_str = "signaled";
		break;
	case STATUS_SPAWN_FAILED:
		status_str = "spawn-failed";
		break;
	default:
		status_str = "unknown";
		break;
	}
	char marker[256];
	int mlen = snprintf(marker, sizeof(marker),
			    "[EXIT code=%d status=%s]\n", f->exit_code,
			    status_str);
	if (mlen <= 0 || (size_t)mlen >= sizeof(marker))
		return 0;
	return write_full(fd, marker, (size_t)mlen);
}

/* Escape payload for text record format.
 * Rules: \ -> \\, \n -> \n literal, \r -> \r literal,
 * \t -> \t literal, bytes outside 0x20-0x7E -> \xNN.
 * Returns malloc'd string (caller frees) and sets *out_len. */
static char *escape_payload(const unsigned char *data, size_t len,
			    size_t *out_len)
{
	/* Worst case: every byte becomes \xNN (4 chars) */
	size_t alloc = len * 4 + 1;
	char *out = malloc(alloc);
	if (!out)
		return NULL;
	size_t pos = 0;
	for (size_t i = 0; i < len; i++) {
		unsigned char c = data[i];
		if (c == '\\') {
			out[pos++] = '\\';
			out[pos++] = '\\';
		} else if (c == '\n') {
			out[pos++] = '\\';
			out[pos++] = 'n';
		} else if (c == '\r') {
			out[pos++] = '\\';
			out[pos++] = 'r';
		} else if (c == '\t') {
			out[pos++] = '\\';
			out[pos++] = 't';
		} else if (c >= 0x20 && c <= 0x7E) {
			out[pos++] = (char)c;
		} else {
			pos += (size_t)snprintf(out + pos, alloc - pos,
						"\\x%02x", c);
		}
	}
	out[pos] = '\0';
	*out_len = pos;
	return out;
}

/* Format an RFC3339Nano UTC timestamp into buf.
 * Returns bytes written (excluding NUL). */
static int format_timestamp(char *buf, size_t buflen)
{
	struct timespec ts;
	struct tm tm;
	clock_gettime(CLOCK_REALTIME, &ts);
	gmtime_r(&ts.tv_sec, &tm);
	return snprintf(buf, buflen, "%04d-%02d-%02dT%02d:%02d:%02d.%09ldZ",
			tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday,
			tm.tm_hour, tm.tm_min, tm.tm_sec, ts.tv_nsec);
}

/* Write one structured text record to fd.
 * Returns bytes written, or -1 on error. */
static ssize_t write_text_record(int fd, const struct frame *f,
				 const char *unit_id)
{
	const char *stream_str;
	switch (f->stream) {
	case STREAM_STDOUT:
		stream_str = "stdout";
		break;
	case STREAM_STDERR:
		stream_str = "stderr";
		break;
	case STREAM_META:
		stream_str = "meta";
		break;
	default:
		stream_str = "unknown";
		break;
	}

	const char *event_str =
	    (f->event == EVT_EXIT) ? "exit" : "output";

	const char *status_str;
	switch (f->exit_status) {
	case STATUS_EXITED:
		status_str = "exited";
		break;
	case STATUS_SIGNALED:
		status_str = "signaled";
		break;
	case STATUS_SPAWN_FAILED:
		status_str = "spawn-failed";
		break;
	default:
		status_str = "-";
		break;
	}

	char ts[64];
	format_timestamp(ts, sizeof(ts));

	char code_str[32];
	if (f->event == EVT_EXIT)
		snprintf(code_str, sizeof(code_str), "%d", f->exit_code);
	else
		snprintf(code_str, sizeof(code_str), "-");

	/* Escape payload for output events, use "-" for exit */
	char *escaped = NULL;
	size_t escaped_len = 0;
	const char *payload_str = "-";
	if (f->event == EVT_OUTPUT && f->payload_len > 0) {
		escaped = escape_payload(f->payload, f->payload_len,
					 &escaped_len);
		if (escaped)
			payload_str = escaped;
	}

	/* Build record line */
	/* ts=<TS> unit=<UNIT> pid=<PID> stream=<S> event=<E>
	 * status=<ST> code=<C> payload=<P>\n */
	size_t line_alloc = 64 + strlen(ts) + strlen(unit_id) + 32 +
			    strlen(stream_str) + strlen(event_str) +
			    strlen(status_str) + strlen(code_str) +
			    escaped_len + 64;
	char *line = malloc(line_alloc);
	if (!line) {
		free(escaped);
		return -1;
	}

	int llen =
	    snprintf(line, line_alloc,
		     "ts=%s unit=%s pid=%u stream=%s event=%s status=%s "
		     "code=%s payload=%s\n",
		     ts, unit_id, f->pid, stream_str, event_str, status_str,
		     code_str, payload_str);

	free(escaped);

	if (llen <= 0 || (size_t)llen >= line_alloc) {
		free(line);
		return -1;
	}

	ssize_t w = write_full(fd, line, (size_t)llen);
	free(line);
	return w;
}

/* Write a big-endian u32 to buffer. */
static void put_u32be(unsigned char *p, unsigned int v)
{
	p[0] = (v >> 24) & 0xff;
	p[1] = (v >> 16) & 0xff;
	p[2] = (v >> 8) & 0xff;
	p[3] = v & 0xff;
}

/* Write a big-endian u16 to buffer. */
static void put_u16be(unsigned char *p, unsigned short v)
{
	p[0] = (v >> 8) & 0xff;
	p[1] = v & 0xff;
}

/* Write a big-endian u64 to buffer. */
static void put_u64be(unsigned char *p, unsigned long long v)
{
	p[0] = (v >> 56) & 0xff;
	p[1] = (v >> 48) & 0xff;
	p[2] = (v >> 40) & 0xff;
	p[3] = (v >> 32) & 0xff;
	p[4] = (v >> 24) & 0xff;
	p[5] = (v >> 16) & 0xff;
	p[6] = (v >> 8) & 0xff;
	p[7] = v & 0xff;
}

/* Binary record header: u32be record_len, u8 version(1), u8 event,
 * u8 stream, u8 reserved(0), u64be timestamp_ns, u32be pid,
 * u16be unit_len, i32be exit_code, u8 exit_status,
 * u8[3] reserved(0), u32be payload_len,
 * unit_id[unit_len], payload[payload_len]
 * Fixed header size = 4+1+1+1+1+8+4+2+4+1+3+4 = 34 bytes */
enum { BIN_HEADER_SIZE = 34 };

static const unsigned char SLG1_MAGIC[4] = { 'S', 'L', 'G', '1' };

/* Write binary file header (SLG1 magic). */
static ssize_t write_binary_header(int fd)
{
	return write_full(fd, SLG1_MAGIC, 4);
}

/* Write one binary structured record. Returns bytes written or -1. */
static ssize_t write_binary_record(int fd, const struct frame *f,
				   const char *unit_id,
				   size_t unit_id_len)
{
	unsigned int record_len =
	    BIN_HEADER_SIZE - 4 + (unsigned int)unit_id_len + f->payload_len;
	unsigned char hdr[BIN_HEADER_SIZE];
	memset(hdr, 0, sizeof(hdr));

	put_u32be(hdr, record_len);             /* record_len */
	hdr[4] = 1;                              /* version */
	hdr[5] = f->event;                       /* event */
	hdr[6] = f->stream;                      /* stream */
	hdr[7] = 0;                              /* reserved */

	/* timestamp_ns */
	struct timespec ts;
	clock_gettime(CLOCK_REALTIME, &ts);
	unsigned long long ns =
	    (unsigned long long)ts.tv_sec * 1000000000ULL +
	    (unsigned long long)ts.tv_nsec;
	put_u64be(hdr + 8, ns);

	put_u32be(hdr + 16, f->pid);             /* pid */
	put_u16be(hdr + 20, (unsigned short)unit_id_len); /* unit_len */

	/* exit_code as unsigned for put */
	put_u32be(hdr + 22, (unsigned int)f->exit_code);
	hdr[26] = f->exit_status;                /* exit_status */
	/* hdr[27..29] reserved, already 0 */
	put_u32be(hdr + 30, f->payload_len);     /* payload_len */

	ssize_t w1 = write_full(fd, hdr, BIN_HEADER_SIZE);
	if (w1 < 0)
		return -1;

	ssize_t total = w1;
	if (unit_id_len > 0) {
		ssize_t w2 = write_full(fd, unit_id, unit_id_len);
		if (w2 < 0)
			return -1;
		total += w2;
	}
	if (f->payload_len > 0) {
		ssize_t w3 =
		    write_full(fd, f->payload, f->payload_len);
		if (w3 < 0)
			return -1;
		total += w3;
	}
	return total;
}

static void usage(void)
{
	fprintf(stderr,
		"usage: supervisor-logd --file PATH --max-file-size-bytes N "
		"[--log-dir DIR] [--prune-cmd CMD] "
		"[--prune-min-interval-sec N] "
		"[--framed] [--unit ID] [--format text|binary]\n");
}

int main(int argc, char **argv)
{
	const char *file_path = NULL;
	long long max_size = 0;
	const char *log_dir = NULL;
	const char *prune_cmd = NULL;
	long prune_interval = 0;
	int framed = 0;
	const char *unit_id = NULL;
	enum format_mode format = FMT_RAW;
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
		} else if (strcmp(argv[i], "--framed") == 0) {
			framed = 1;
		} else if (strcmp(argv[i], "--unit") == 0) {
			if (++i >= argc) {
				fprintf(stderr, "supervisor-logd: --unit "
						"requires an argument\n");
				return EXIT_USAGE;
			}
			unit_id = argv[i];
		} else if (strcmp(argv[i], "--format") == 0) {
			if (++i >= argc) {
				fprintf(stderr, "supervisor-logd: --format "
						"requires an argument\n");
				return EXIT_USAGE;
			}
			if (strcmp(argv[i], "text") == 0)
				format = FMT_TEXT;
			else if (strcmp(argv[i], "binary") == 0)
				format = FMT_BINARY;
			else {
				fprintf(stderr,
					"supervisor-logd: --format must be "
					"text or binary\n");
				return EXIT_USAGE;
			}
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
	if (framed && !unit_id) {
		fprintf(stderr,
			"supervisor-logd: --unit is required with --framed\n");
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

	/* Write binary header for fresh binary files */
	if (format == FMT_BINARY && current_size == 0) {
		if (write_binary_header(fd) < 0) {
			fprintf(stderr,
				"supervisor-logd: write header %s: %s\n",
				file_path, strerror(errno));
			close(fd);
			return EXIT_IO;
		}
		current_size = 4;
	}

	time_t last_prune = 0;

	if (!framed) {
		/* Raw passthrough mode (backward compatible) */
		char buf[8192];
		ssize_t n;

		while (!got_term) {
			if (got_hup) {
				got_hup = 0;
				close(fd);
				fd = open_log(file_path);
				if (fd < 0) {
					fprintf(stderr,
						"supervisor-logd: reopen "
						"%s: %s\n",
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
					continue;
				break;
			}
			if (n == 0)
				break;

			if (write_full(fd, buf, (size_t)n) < 0) {
				fprintf(stderr,
					"supervisor-logd: write %s: %s\n",
					file_path, strerror(errno));
				close(fd);
				return EXIT_IO;
			}
			current_size += n;

			if (current_size >= (off_t)max_size) {
				fd = rotate_log(fd, file_path, &current_size,
						prune_cmd, prune_interval,
						&last_prune);
				if (fd < 0)
					return EXIT_IO;
			}
		}
	} else {
		/* Framed transport mode */
		enum { ACCUM_SIZE = 65536 };
		unsigned char accum[ACCUM_SIZE];
		size_t accum_len = 0;
		char buf[8192];

		size_t uid_len = strlen(unit_id);

		while (!got_term) {
			if (got_hup) {
				got_hup = 0;
				close(fd);
				fd = open_log(file_path);
				if (fd < 0) {
					fprintf(stderr,
						"supervisor-logd: reopen "
						"%s: %s\n",
						file_path, strerror(errno));
					return EXIT_IO;
				}
				current_size = file_size(fd);
				if (current_size < 0)
					current_size = 0;
				/* Write binary header after reopen */
				if (format == FMT_BINARY &&
				    current_size == 0) {
					if (write_binary_header(fd) < 0) {
						close(fd);
						return EXIT_IO;
					}
					current_size = 4;
				}
			}

			ssize_t n = read(STDIN_FILENO, buf, sizeof(buf));
			if (n < 0) {
				if (errno == EINTR)
					continue;
				break;
			}
			if (n == 0)
				break;

			/* Append to accumulation buffer */
			if (accum_len + (size_t)n > ACCUM_SIZE) {
				fprintf(stderr,
					"supervisor-logd: frame buffer "
					"overflow\n");
				break;
			}
			memcpy(accum + accum_len, buf, (size_t)n);
			accum_len += (size_t)n;

			/* Parse and process complete frames */
			size_t consumed = 0;
			while (consumed < accum_len) {
				struct frame f;
				int rc = parse_frame(accum + consumed,
						     accum_len - consumed, &f);
				if (rc == 0)
					break; /* incomplete */
				if (rc < 0) {
					fprintf(stderr,
						"supervisor-logd: protocol "
						"error\n");
					/* Skip 1 byte and try to recover */
					consumed++;
					continue;
				}

				ssize_t written = 0;
				if (format == FMT_TEXT) {
					written = write_text_record(
					    fd, &f, unit_id);
				} else if (format == FMT_BINARY) {
					written = write_binary_record(
					    fd, &f, unit_id, uid_len);
				} else {
					/* Raw framed: write payload
					 * directly */
					if (f.event == EVT_OUTPUT &&
					    f.payload_len > 0) {
						written = write_full(
						    fd, f.payload,
						    f.payload_len);
					} else if (f.event == EVT_EXIT) {
						written =
						    write_exit_marker(fd, &f);
					}
				}

				if (written < 0) {
					fprintf(stderr,
						"supervisor-logd: write "
						"%s: %s\n",
						file_path, strerror(errno));
					close(fd);
					return EXIT_IO;
				}
				current_size += written;
				consumed += (size_t)rc;
			}

			/* Move remainder to front */
			if (consumed > 0 && consumed < accum_len) {
				memmove(accum, accum + consumed,
					accum_len - consumed);
			}
			accum_len -= consumed;

			/* Check size cap and rotate */
			if (current_size >= (off_t)max_size) {
				fd = rotate_log(fd, file_path, &current_size,
						prune_cmd, prune_interval,
						&last_prune);
				if (fd < 0)
					return EXIT_IO;
				/* Write binary header after rotation */
				if (format == FMT_BINARY) {
					if (write_binary_header(fd) < 0) {
						close(fd);
						return EXIT_IO;
					}
					current_size = 4;
				}
			}
		}
	}

	/* Clean exit: flush and close */
	close(fd);
	return EXIT_OK;
}
