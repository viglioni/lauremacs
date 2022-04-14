;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; Functions
;;

(defun lauremacs/haskell-format-imports ()
  "Sort and align import statements from anywhere in the source file."
  (interactive)
  (save-excursion
    (haskell-navigate-imports)
    (haskell-mode-format-imports)))


;;
;; Configs
;;
																			

(use-package hindent
	:commands hindent-mode
	:custom
	(hindent-reformat-buffer-on-save t))

(use-package hlint-refactor
	:commands hlint-refactor-mode)

(use-package haskell-mode
	:mode "\\.hs\\'"
	:hook ((haskell-mode . lsp-deferred)
				 (haskell-mode . hindent-mode)
				 (haskell-mode . hlint-refactor-mode))
	:init
	(lauremacs-major-mode-leader
		:keymaps 'haskell-mode-map
		"s"  '(nil :which-key "repl")
		"ss" '(haskell-interactive-switch       :which-key "switch to repl")
		"sb" '(haskell-process-load-file        :which-key "send buffer to repl")
		"="  '(nil :which-key "format")
		"==" '(hindent-reformat-buffer          :which-key "format buffer")
		"=b" '(hindent-reformat-buffer          :which-key "format buffer")
		"=d" '(hindent-reformat-decl-or-fill    :which-key "format buffer")
		"=r" '(hindent-reformat-region          :which-key "format region")
		"=i" '(lauremacs/haskell-format-imports :which-key "format imports")
		"r"  '(nil :which-key "refactor")
		"rb" '(hlint-refactor-refactor-buffer   :which-key "refactor buffer")
		"rp" '(hlint-refactor-refactor-at-point :which-key "refactor at point")
		"h"  '(nil :which-key "help/docs")
		"hh" '(haskell-hoogle                   :which-key "hoogle"))
	(lauremacs-major-mode-leader
		:keymaps 'haskell-interactive-mode-map
		"s"  '(nil :which-key "repl")
		"ss" '(haskell-interactive-switch-back  :which-key "switch to code")))

