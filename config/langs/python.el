;;; -*- lexical-binding: t; l-syntax: t -*-
(require 'snake-case-mode)

(config-package lsp-pyright
  :after python-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

;; pip install puthon-lsp-server pylsp-mypy flake8

(defun my/python-format-buffer-with-ruff ()
  "Format Python buffer with ruff using direnv exec."
  (interactive)
  (when (eq major-mode 'python-mode)
    (let* ((file (buffer-file-name))
           (default-directory (locate-dominating-file file "pyproject.toml"))
           (command (format "direnv exec . ruff format %s" (shell-quote-argument file))))
      (when default-directory
        (save-buffer)
        (shell-command command)
        (revert-buffer t t t)))))

(defun my/python-format-on-save ()
  "Run ruff format before saving Python files."
  (add-hook 'before-save-hook #'my/python-format-buffer-with-ruff nil t))

(config-package python-mode
  :mode "\\.py\\'"
  :hook ((python-mode . lsp-deferred)
         (python-mode . snake-case-mode)
         )
  :config
  (setq lsp-pylsp-plugins-django-enabled t)
  (setq lsp-pylsp-plugins-flake8-enabled t)
  (setq lsp-pylsp-plugins-pycodestyle-enabled nil))
