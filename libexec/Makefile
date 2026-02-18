CC ?= cc
CFLAGS ?= -Wall -Wextra -Werror -pedantic -std=c99 -O2 -D_FORTIFY_SOURCE=2

# Absolute paths to built binaries (passed to test sources via -D)
LOGD_ABS  = $(abspath supervisor-logd)
RUNAS_ABS = $(abspath supervisor-runas)

# Test compiler flags
TEST_CFLAGS  = $(CFLAGS) -Itests/vendor
TEST_LDFLAGS =

TEST_BINS = tests/test_supervisor_logd tests/test_supervisor_runas

.PHONY: all tests test check clean

all: supervisor-runas supervisor-logd

supervisor-runas: supervisor-runas.c
	$(CC) $(CFLAGS) -o $@ $<

supervisor-logd: supervisor-logd.c
	$(CC) $(CFLAGS) -o $@ $<

tests: all $(TEST_BINS)

tests/test_supervisor_logd: tests/test_supervisor_logd.c tests/test_helpers.c tests/test_helpers.h
	$(CC) $(TEST_CFLAGS) -DLOGD_PATH=\"$(LOGD_ABS)\" \
		-o $@ tests/test_supervisor_logd.c tests/test_helpers.c $(TEST_LDFLAGS)

tests/test_supervisor_runas: tests/test_supervisor_runas.c tests/test_helpers.c tests/test_helpers.h
	$(CC) $(TEST_CFLAGS) -DRUNAS_PATH=\"$(RUNAS_ABS)\" \
		-o $@ tests/test_supervisor_runas.c tests/test_helpers.c $(TEST_LDFLAGS)

test: tests
	@echo "=== supervisor-logd tests ==="
	@./tests/test_supervisor_logd
	@echo ""
	@echo "=== supervisor-runas tests ==="
	@./tests/test_supervisor_runas

check: all test

clean:
	rm -f supervisor-runas supervisor-logd $(TEST_BINS)
