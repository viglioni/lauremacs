;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;


(use-package haskell-mode
	:mode "\\.hs\\'"
	:hook ((haskell-mode . lsp-deferred)))

