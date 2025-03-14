EMACS = emacs
EMACS_FLAGS = -Q --batch
EMACS_DIR = ~/.emacs.d
EMACS_TEST_DIR = $(EMACS_DIR)/test

.PHONY: test clean docs deps clean-deps prune-deps release-major release-minor release-patch

test:
	$(EMACS) $(EMACS_FLAGS) \
		--directory $(EMACS_TEST_DIR) \
		--load run-tests.el

clean:
	rm -f *.elc test/*.elc

clean-deps:
	rm -rf $(EMACS_DIR)/straight/build
	rm -rf $(EMACS_DIR)/straight/repos

prune-deps:
	$(EMACS) $(EMACS_FLAGS) \
		--eval "(setq user-emacs-directory (expand-file-name \"$(EMACS_DIR)\"))" \
		--load core/package-manager.el \
		--eval "(lauremacs/sync-straight-packages)"

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

deps:
	$(EMACS) $(EMACS_FLAGS) \
		--eval "(setq user-emacs-directory (expand-file-name \"$(EMACS_DIR)\"))" \
		--load core/package-manager.el \
    --load core/core-packages.el \
		--eval "(straight-thaw-versions)" \
		--eval "(straight-check-all)"

release-major:
	$(EMACS) $(EMACS_FLAGS) \
		--directory $(EMACS_DIR) \
		--load scripts/release.el \
		--eval "(lauremacs/release-version 'major)"

release-minor:
	$(EMACS) $(EMACS_FLAGS) \
		--directory $(EMACS_DIR) \
		--load scripts/release.el \
		--eval "(lauremacs/release-version 'minor)"

release-patch:
	$(EMACS) $(EMACS_FLAGS) \
		--directory $(EMACS_DIR) \
		--load scripts/release.el \
		--eval "(lauremacs/release-version 'patch)"

help:
	@echo "Available targets:"
	@echo "  test        - Run all tests"
	@echo "  clean       - Remove compiled Elisp files"
	@echo "  clean-deps  - Remove all dependencies (keeps lockfile)"
	@echo "  prune-deps  - Remove unused packages"
	@echo "  docs        - Process all readme.org files and commit changes"
	@echo "  deps        - Install/update dependencies from lockfile"
	@echo "  release-major - Release a major version update"
	@echo "  release-minor - Release a minor version update"
	@echo "  release-patch - Release a patch version update"
	@echo "  help        - Show this help message"
