;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;;###autoload
(defun lauremacs/lsp-organize-imports-before-save ()
	"Run `lsp-organize-imports' before save."
	(when (bound-and-true-p lsp-mode)
		(if (lsp-organize-imports)
				(message "Organized imports!")
			(message "Didn't organize imports")))
	t)



;;;###autoload
(defun lauremacs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . lauremacs/lsp-mode-setup)
				 (lsp-mode . lsp-ui-mode)
				 (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq lsp-keymap-prefix "<f17>")
  (setq lsp-enable-on-type-formatting nil)
  :config
  (define-key lsp-mode-map (kbd "<f17>") lsp-command-map)) 

(use-package lsp-ui
  :commands lsp-ui-mode
	:init
	(setq lsp-ui-doc-show-with-cursor t))

(use-package helm-lsp
  :after lsp)

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-origami
	:after lsp)

(use-package lsp-haskell
	:after (lsp haskell-mode))

