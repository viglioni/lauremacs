;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;


;;
;; Doom modeline
;;

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))


;;
;; Highlight indentation
;;

(use-package highlight-indentation
	:commands highlight-indentation-mode)

;;
;; rainbow delimiters
;;

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;
;; Highlight parentheses
;;

(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode)
	:init
	(add-hook 'eshell-mode-hook 'highlight-parentheses-mode))

;;
;; Company
;;

(use-package company
  :bind (("C-/" . 'company-complete)
	 :map company-search-map
	 ("s-." . 'company-select-previous)
	 ("s-," . 'company-select-next))
  :init
  (global-company-mode 1))

;;
;; Evil
;;

(use-package evil
	:commands (evil-mode
							evil-window-down
							evil-window-left
							evil-window-up
							evil-window-right))

;;
;; Prettify symbols
;;

(global-prettify-symbols-mode 1)

;;
;; Emojify mode
;;

(use-package emojify
	:commands (emojify-mode
						 emojify-insert-emoji
						 emojify-apropos-emoji)
	:hook '((org-mode . emojify-mode)
					(magit-status-mode . emojify-mode)))

;;
;; Yasnippet
;;

(use-package yasnippet
	:init
	(yas-global-mode 1)
	:bind (:map yas-minor-mode-map
							("M-/" . yas-expand)
							("TAB" . nil)))

(use-package yasnippet-snippets
	:after yasnippet)

;;
;; Iedit
;;

(use-package iedit
	:commands iedit-mode
	:bind (:map iedit-mode-keymap
							("M-f" . iedit-restrict-function)
							("M-l" . iedit-rescrit-line)
							("M-," . iedit-expand-down-a-line)
							("M-." . iedit-expand-up-a-line)
							("TAB" . iedit-toggle-selection)
							("C-n" . iedit-next-occurrence)
							("C-p" . iedit-prev-occurence)
							("M-c" . iedit-toggle-case-sensitive))
	:init
	(lauremacs-leader
		"se" '(iedit-mode :which-key "iedit mode")))

;;
;; Flycheck
;;

(bind-lazy-function 'explain-error-at-point
										'lauremacs-ide-explain-error-at-point
										'lauremacs-ide-extra)

(use-package flycheck
  :init
	(global-flycheck-mode)
	(lauremacs-leader		
		"e" '(:keymap flycheck-command-map :package flycheck :which-key "errors")
		"ee" '(explain-error-at-point :which-key "explain error at point"))
	(set-face-attribute 'flycheck-fringe-info nil
											:background "#3a81c3"
											:foreground "white")
	(set-face-attribute 'flycheck-fringe-error nil
											:background "#e0211d"
											:foreground "white")
	(set-face-attribute 'flycheck-fringe-warning nil
											:background "#dc752f"
											:foreground "white"))


