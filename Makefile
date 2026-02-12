EMACS ?= emacs
BATCH = $(EMACS) -Q --batch -L .

EL_MAIN = supervisor.el
EL_MODULES = supervisor-core.el supervisor-units.el supervisor-timer.el supervisor-dashboard.el supervisor-cli.el
EL_ALL = $(EL_MODULES) $(EL_MAIN)
TEST_EL = supervisor-test.el

.PHONY: all check lint test byte-compile checkdoc package-lint clean

all: check

check: lint test

lint: byte-compile checkdoc package-lint

byte-compile: $(EL_ALL)
	@echo "=== byte-compile ==="
	@$(BATCH) --eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $(EL_MODULES)
	@$(BATCH) --eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $(EL_MAIN)
	@rm -f *.elc

checkdoc: $(EL_ALL)
	@echo "=== checkdoc ==="
	@for f in $(EL_ALL); do \
		output=$$($(BATCH) --eval "(checkdoc-file \"$$f\")" 2>&1); \
		if [ -n "$$output" ]; then echo "$$f:"; echo "$$output"; exit 1; fi; \
	done

package-lint: $(EL_MAIN)
	@echo "=== package-lint ==="
	@$(BATCH) --eval "(progn (package-initialize) (require 'package-lint))" \
		--eval "(let ((errs (with-current-buffer (find-file-noselect \"$(EL_MAIN)\") (package-lint-buffer)))) \
			(dolist (e errs) (message \"%d:%d: %s\" (nth 0 e) (nth 1 e) (nth 3 e))) \
			(kill-emacs (if errs 1 0)))"

test: $(EL_ALL) $(TEST_EL)
	@echo "=== ERT tests ==="
	@$(BATCH) -l $(EL_MAIN) -l $(TEST_EL) -f ert-run-tests-batch-and-exit

test-verbose: $(EL_ALL) $(TEST_EL)
	@$(BATCH) -l $(EL_MAIN) -l $(TEST_EL) --eval "(ert-run-tests-batch-and-exit t)"

# Run a single test: make test-one TEST=test-name
test-one: $(EL_ALL) $(TEST_EL)
	@$(BATCH) -l $(EL_MAIN) -l $(TEST_EL) --eval "(ert-run-tests-batch-and-exit '$(TEST))"

clean:
	rm -f *.elc
