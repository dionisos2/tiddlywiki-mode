.PHONY: all test compile clean lint package help

EMACS ?= emacs

# Default target
all: compile test

# Run tests with ERT
test:
	$(EMACS) -Q -batch -L . -l tiddlywiki-mode.el -l tiddlywiki-mode-test.el \
		-f ert-run-tests-batch-and-exit

# Byte-compile
compile:
	$(EMACS) -Q -batch -L . -f batch-byte-compile tiddlywiki-mode.el

# Clean generated files
clean:
	rm -f *.elc

# Check for common issues (requires package-lint)
lint:
	$(EMACS) -Q -batch -L . \
		--eval "(require 'package)" \
		--eval "(package-initialize)" \
		--eval "(require 'package-lint nil t)" \
		--eval "(if (fboundp 'package-lint-batch-and-exit) \
		            (package-lint-batch-and-exit) \
		          (message \"package-lint not installed, skipping\"))" \
		tiddlywiki-mode.el

# Run tests with Eldev (if installed)
eldev-test:
	eldev test

# Byte-compile with Eldev
eldev-compile:
	eldev compile

# Lint with Eldev
eldev-lint:
	eldev lint

help:
	@echo "Available targets:"
	@echo "  all          - Compile and run tests (default)"
	@echo "  test         - Run ERT tests"
	@echo "  compile      - Byte-compile the package"
	@echo "  clean        - Remove generated .elc files"
	@echo "  lint         - Check code with package-lint"
	@echo "  eldev-test   - Run tests using Eldev"
	@echo "  eldev-compile - Byte-compile using Eldev"
	@echo "  eldev-lint   - Lint using Eldev"
	@echo "  help         - Show this help message"
