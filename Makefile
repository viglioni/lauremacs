EMACS = emacs
EMACS_FLAGS = -Q --batch
EMACS_DIR = ~/.emacs.d
EMACS_TEST_DIR = $(EMACS_DIR)/test
BUTTERCUP_DIR = ~/.emacs.d/straight/build/buttercup

.PHONY: test test-file clean docs deps clean-deps prune-deps release-major release-minor release-patch

test:
	$(EMACS) $(EMACS_FLAGS) \
		--directory $(EMACS_TEST_DIR) \
		--load run-tests.el

test-file:
	$(EMACS) $(EMACS_FLAGS) \
		--directory $(EMACS_TEST_DIR) \
		--directory $(BUTTERCUP_DIR) \
		--load $(FILE) \
		--eval "(buttercup-run)"

clean:
	rm -f *.elc test/*.elc

clean-deps:
	rm -rf $(EMACS_DIR)/straight/build
	rm -rf $(EMACS_DIR)/straight/repos
	rm -rf $(EMACS_DIR)/elpa

docs:
	$(EMACS) $(EMACS_FLAGS) \
		--directory $(EMACS_DIR) \
		--load scripts/tangle-org.el
	@if git diff --quiet */readme.org; then \
		echo "No changes to readme files"; \
	else \
		git add $(EMACS_DIR)/*/readme.org && \
		git commit -m "docs: update readme files [automated]"; \
	fi


release-patch:
	$(EMACS) --batch --load scripts/release.el --eval "(release-version \"patch\")"

release-minor:
	$(EMACS) --batch --load scripts/release.el --eval "(release-version \"minor\")"

release-major:
	$(EMACS) --batch --load scripts/release.el --eval "(release-version \"major\")"

install-external-deps:
	brew install font-source-code-pro \
	# for typescript-mode:
	brew install eslint

help:
	@echo "Available targets:"
	@echo "  test        - Run all tests"
	@echo "  test-file   - Run specific test file (use FILE=path/to/test.el)"
	@echo "  clean       - Remove compiled Elisp files"
	@echo "  clean-deps  - Remove all dependencies (keeps lockfile)"
	@echo "  prune-deps  - Remove unused packages"
	@echo "  docs        - Process all readme.org files and commit changes"
	@echo "  deps        - Install/update dependencies from lockfile"
	@echo "  release-major - Release a major version update"
	@echo "  release-minor - Release a minor version update"
	@echo "  release-patch - Release a patch version update"
	@echo "  help        - Show this help message"
