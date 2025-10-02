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
  (lsp-headerline-breadcrumb-mode)

  (add-to-list 'lsp-file-watch-ignored-directories ".nvm"))

(config-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . lauremacs/lsp-mode-setup)
				 (lsp-mode . lsp-ui-mode)
				 (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq lsp-keymap-prefix "<f17>")
																				;(setq lsp-enable-on-type-formatting nil)
	(setq lsp-use-workspace-root-for-server-default-directory nil)
  (setq lsp-auto-guess-root t)
  :config
  (define-key lsp-mode-map (kbd "<f17>") lsp-command-map)
  (setq lsp-completion-provider :capf  ; Use completion-at-point-functions
        lsp-idle-delay 0.2)           ; Small delay helps with performance
  ) 

(config-package lsp-ui
  :commands lsp-ui-mode
	:init
	(setq lsp-ui-doc-show-with-cursor t))

(config-package helm-lsp
  :after lsp)

(config-package lsp-treemacs
  :after lsp)

(config-package lsp-origami
	:after lsp)

(config-package lsp-haskell
	:after (lsp haskell-mode))

