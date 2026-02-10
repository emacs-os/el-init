EMACS ?= emacs
BATCH = $(EMACS) -Q --batch

EL = supervisor.el
TEST_EL = supervisor-test.el

.PHONY: all check lint test byte-compile checkdoc package-lint clean

all: check

check: lint test

lint: byte-compile checkdoc package-lint

byte-compile: $(EL)
	@echo "=== byte-compile ==="
	@$(BATCH) --eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $(EL)
	@rm -f *.elc

checkdoc: $(EL)
	@echo "=== checkdoc ==="
	@output=$$($(BATCH) --eval "(checkdoc-file \"$(EL)\")" 2>&1); \
	if [ -n "$$output" ]; then echo "$$output"; exit 1; fi

package-lint: $(EL)
	@echo "=== package-lint ==="
	@$(BATCH) --eval "(progn (package-initialize) (require 'package-lint))" \
		--eval "(let ((errs (with-current-buffer (find-file-noselect \"$(EL)\") (package-lint-buffer)))) \
			(dolist (e errs) (message \"%d:%d: %s\" (nth 0 e) (nth 1 e) (nth 3 e))) \
			(kill-emacs (if errs 1 0)))"

test: $(EL) $(TEST_EL)
	@echo "=== ERT tests ==="
	@$(BATCH) -l $(EL) -l $(TEST_EL) -f ert-run-tests-batch-and-exit

test-verbose: $(EL) $(TEST_EL)
	@$(BATCH) -l $(EL) -l $(TEST_EL) --eval "(ert-run-tests-batch-and-exit t)"

# Run a single test: make test-one TEST=test-name
test-one: $(EL) $(TEST_EL)
	@$(BATCH) -l $(EL) -l $(TEST_EL) --eval "(ert-run-tests-batch-and-exit '$(TEST))"

clean:
	rm -f *.elc
