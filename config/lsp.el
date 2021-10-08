(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . efs/lsp-mode-setup)
	 (lsp-mode . lsp-ui-mode)
	 (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq lsp-keymap-prefix "<f17>")
  ;; (lauremacs-major-mode-leader
  ;;   :keymaps 'lsp-mode
  ;;   "" 'lsp-command-map)
  :config
  (define-key lsp-mode-map (kbd "<f17>") lsp-command-map)
  ) 

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package helm-lsp
  :after lsp)

(use-package lsp-treemacs
  :after lsp)

