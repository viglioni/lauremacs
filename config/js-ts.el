;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;


(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook ((typescript-mode . lsp-deferred)
				 (typescript-mode . prettier-js-mode)
				 (tsx-mode        . lsp-deferred)
				 (tsx-mode        . prettier-js-mode)
				 (tsx-mode        . (lambda () (require 'web-mode))))
  :custom
  (typescript-indent-level 2)
	:init
	(define-derived-mode tsx-mode typescript-mode "tsx")
	(add-hook 'typescript-mode #'subword-mode)
	(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . tsx-mode))
	(with-eval-after-load "smartparens"
		(sp-local-pair 'typescript-mode "<" ">") :when '(sp-point-after-word-p)))

(use-package tree-sitter
  :ensure t
  :hook ((typescript-mode . tree-sitter-hl-mode)
				 (tsx-mode        . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(tsx-mode . tsx)))



(bind-lazy-function 'lsp-rename-ts-file
										'lauremacs-ide-lsp-ts-rename-file
										'lauremacs-ide-extra)

(bind-lazy-function 'send-buffer-to-repl
										'ts-repl-exec-ts-buffer
										'ts-repl)

(bind-lazy-function 'send-last-sexp-to-repl
										'ts-repl-send-last-sexp
										'ts-repl)

(bind-lazy-function 'open-ramda-docs
										'ramda-docs-open
										'ramda-docs)

(lauremacs-major-mode-leader
	:keymaps '(typescript-mode-map web-mode-map)
	"rf" '(lsp-rename-ts-file     :which-key "rename file")
	"s"  '(nil :which-key "ts-repl")
	"sb" '(send-buffer-to-repl    :which-key "send buffer to repl")
	"se" '(send-last-sexp-to-repl :which-key "send last sexp to repl")
	"d"  '(nil :which-key "docs")
	"dr" '(open-ramda-docs        :which-key "open ramda docs"))

(use-package prettier-js
  :after (typescript-mode))


(use-package web-mode
	:mode "\\.html\\'"
  :hook ((web-mode . prettier-js-mode))
  :init
  (lauremacs-major-mode-leader
    :keymaps '(web-mode-map tsx-mode-map)
    "i"  '(nil :which-key "html element")
    "ii" '(web-mode-element-insert-at-point :which-key "element insert at point")
    "iv" '(web-mode-element-vanish          :which-key "element vanish")
    "ik" '(web-mode-element-kill            :which-key "element kill")
    "is" '(web-mode-element-select          :which-key "element select")
    "iw" '(web-mode-element-wrap            :which-key "element wrap")
    "ir" '(web-mode-element-rename          :which-key "element rename")
    "ic" '(web-mode-element-clone           :which-key "element clone")
    "i/" '(web-mode-element-close           :which-key "element close")
    "ib" '(web-mode-element-beginning       :which-key "element beginning")
    "ie" '(web-mode-element-end             :which-key "element end"))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

