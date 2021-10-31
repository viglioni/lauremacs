;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;


(use-package haskell-mode
	:mode "\\.hs\\'"
	:hook ((haskell-mode . lsp-deferred))
	:init
	(lauremacs-major-mode-leader
		:keymaps 'haskell-mode-map
		"s" '(nil :which-key "repl")
		"ss" '(haskell-interactive-switch :which-key "switch to repl")
		"sb" '(haskell-process-load-file :which-key "send buffer to repl"))
	(lauremacs-major-mode-leader
		:keymaps 'haskell-interactive-mode-map
		"s" '(nil :which-key "repl")
		"ss" '(haskell-interactive-switch-back :which-key "switch to code")))

