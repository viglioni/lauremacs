
;;
;; Some minor modes
;;

(use-package highlight-indentation
	:commands highlight-indentation-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode)
	:init
	(add-hook 'eshell-mode-hook 'highlight-parentheses-mode))

(use-package company
  :bind (("C-/" . 'company-complete)
	 :map company-search-map
	 ("s-." . 'company-select-previous)
	 ("s-," . 'company-select-next))
  :init
  (global-company-mode 1))

(use-package evil
	:commands (evil-mode
							evil-window-down
							evil-window-left
							evil-window-up
							evil-window-right))

(global-prettify-symbols-mode 1)

(use-package emojify
	:commands (emojify-mode
						 emojify-insert-emoji
						 emojify-apropos-emoji)
	:hook (org-mode . emojify-mode))

