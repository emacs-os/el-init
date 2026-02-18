CC ?= cc
CFLAGS ?= -Wall -Wextra -Werror -pedantic -std=c99 -O2 -D_FORTIFY_SOURCE=2

# Absolute paths to built binaries (passed to test sources via -D)
LOGD_ABS  = $(abspath supervisor-logd)
RUNAS_ABS = $(abspath supervisor-runas)

# Test compiler flags -- pass all binary paths so any test can use them
TEST_CFLAGS  = $(CFLAGS) -Itests/vendor \
               -DLOGD_PATH=\"$(LOGD_ABS)\" -DRUNAS_PATH=\"$(RUNAS_ABS)\"
TEST_LDFLAGS =

TEST_HELPERS = tests/test_helpers.c tests/test_helpers.h
TEST_SRCS = $(sort $(filter-out tests/test_helpers.c, \
              $(wildcard tests/test_*.c)))
TEST_BINS = $(TEST_SRCS:.c=)

.PHONY: all tests test check clean

all: supervisor-runas supervisor-logd

supervisor-runas: supervisor-runas.c
	$(CC) $(CFLAGS) -o $@ $<

supervisor-logd: supervisor-logd.c
	$(CC) $(CFLAGS) -o $@ $<

tests: all $(TEST_BINS)

tests/test_%: tests/test_%.c $(TEST_HELPERS)
	$(CC) $(TEST_CFLAGS) -o $@ $< tests/test_helpers.c $(TEST_LDFLAGS)

test: tests
	@for t in $(TEST_BINS); do \
		echo "=== $$(basename $$t) ==="; \
		./$$t || exit 1; \
		echo ""; \
	done

check: all test

clean:
	rm -f supervisor-runas supervisor-logd $(TEST_BINS)
