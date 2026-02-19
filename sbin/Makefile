SHELL := /bin/sh

SCRIPTS := elinit-logrotate elinit-log-prune elinitctl elinit-import
TESTS   := $(sort $(wildcard tests/test-*.sh))

.PHONY: all check lint test clean

all: check

check: lint test

lint:
	@echo "=== shellcheck ==="
	@shellcheck -x -P tests -s sh $(SCRIPTS) $(TESTS) tests/testlib.sh

test:
	@echo "=== sbin tests ==="
	@for t in $(TESTS); do \
		echo "=== $$t ==="; \
		sh "$$t" || exit 1; \
	done

clean:
	@printf 'Nothing to clean.\n'
