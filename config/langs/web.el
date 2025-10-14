;;; -*- lexical-binding: t; l-syntax: t -*-
(config-package typescript-mode
  :mode ( "\\.ts\\'" "\\.js\\'")
  :hook ((typescript-mode . lsp-deferred)
				 (typescript-mode . prettier-js-mode)         
				 (tsx-mode        . lsp-deferred)
				 (tsx-mode        . prettier-js-mode)
         ;; (typescript-mode . (lambda () (add-multiple-into-list 'prettify-symbols-alist
				 ;;  																						    '((">="  . "≥")
				 ;;  																							    ("<="  . "≤")
				 ;;  																							    ("!==" . "≠")
         ;;                                                    ("=>"  . "⇒")))
         )
  :custom
  (typescript-indent-level 2)
  
	:init
	(define-derived-mode tsx-mode typescript-mode "tsx")
	(add-hook 'tsx-mode #'subword-mode)
	(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-mode))
  )

(config-package prettier-js
  :after (typescript-mode))

(config-package tree-sitter
  :ensure t
  :hook ((tsx-mode . tree-sitter-hl-mode)))

(config-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(tsx-mode . tsx)))

(config-package vue-mode
  :mode "\\.vue\\'"
  :hook ((vue-mode . lsp-deferred)
         (vue-mode . prettier-js-mode))
  :custom
  (vue-html-tab-width 2)
  (js-indent-level 2)
  (mmm-submode-decoration-level 0)
  (css-indent-offset 2))

(config-package web-mode
  :mode "\\.html$")
