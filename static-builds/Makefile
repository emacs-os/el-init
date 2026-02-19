SHELL := /bin/sh

TESTS := $(sort $(wildcard tests/test-*.sh))

.PHONY: all check lint test

all: check

check: lint test

lint:
	@echo "=== shellcheck (static-builds) ==="
	@shellcheck -x -P tests -s sh $(TESTS)

test:
	@echo "=== static-builds tests ==="
	@for t in $(TESTS); do \
		echo "=== $$t ==="; \
		sh "$$t" || exit 1; \
	done
