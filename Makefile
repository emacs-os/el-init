EMACS ?= emacs
BATCH = $(EMACS) -Q --batch -L . -L $(TEST_DIR)

EL_MAIN = supervisor.el
EL_MODULES = supervisor-core.el supervisor-log.el supervisor-overrides.el supervisor-libexec.el supervisor-sandbox.el supervisor-units.el supervisor-timer.el supervisor-dashboard.el supervisor-cli.el
EL_ALL = $(EL_MODULES) $(EL_MAIN)
TEST_DIR = tests
TEST_HELPERS = $(TEST_DIR)/supervisor-test-helpers.el
TEST_ELS = $(TEST_HELPERS) \
           $(TEST_DIR)/supervisor-test-core.el \
           $(TEST_DIR)/supervisor-test-restart.el \
           $(TEST_DIR)/supervisor-test-validation.el \
           $(TEST_DIR)/supervisor-test-dag.el \
           $(TEST_DIR)/supervisor-test-plan.el \
           $(TEST_DIR)/supervisor-test-dashboard.el \
           $(TEST_DIR)/supervisor-test-units.el \
           $(TEST_DIR)/supervisor-test-timer.el \
           $(TEST_DIR)/supervisor-test-cli.el \
           $(TEST_DIR)/supervisor-test-keywords.el \
           $(TEST_DIR)/supervisor-test-policy.el \
           $(TEST_DIR)/supervisor-test-identity.el \
           $(TEST_DIR)/supervisor-test-logging.el \
           $(TEST_DIR)/supervisor-test-targets.el \
           $(TEST_DIR)/supervisor-test-sandbox.el \
           $(TEST_DIR)/supervisor-test-logformat.el

.PHONY: all check lint test byte-compile checkdoc package-lint \
       libexec-check sbin-check clean

all: check

check: lint test libexec-check sbin-check

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

test: $(EL_ALL) $(TEST_ELS)
	@echo "=== ERT tests ==="
	@$(BATCH) -l $(EL_MAIN) $(foreach f,$(TEST_ELS),-l $(f)) -f ert-run-tests-batch-and-exit

test-verbose: $(EL_ALL) $(TEST_ELS)
	@$(BATCH) -l $(EL_MAIN) $(foreach f,$(TEST_ELS),-l $(f)) --eval "(ert-run-tests-batch-and-exit t)"

# Run a single test: make test-one TEST=test-name
test-one: $(EL_ALL) $(TEST_ELS)
	@$(BATCH) -l $(EL_MAIN) $(foreach f,$(TEST_ELS),-l $(f)) --eval "(ert-run-tests-batch-and-exit '$(TEST))"

libexec-check:
	@$(MAKE) -C libexec check

sbin-check:
	@$(MAKE) -C sbin check

clean:
	rm -f *.elc tests/*.elc
	@$(MAKE) -C libexec clean
