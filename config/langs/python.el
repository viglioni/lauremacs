;;; -*- lexical-binding: t; l-syntax: t -*-
(require 'snake-case-mode)
(require 'semicolon-mode)

(config-package lsp-pyright
  :after python-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

;; pip install puthon-lsp-server pylsp-mypy flake8

(config-package python-mode
  :mode "\\.py\\'"
  :hook ((python-mode . lsp-deferred)
         (python-mode . snake-case-mode)
         (python-mode . semicolon-mode))
  :config
  (setq lsp-pylsp-plugins-django-enabled t)
  (setq lsp-pylsp-plugins-flake8-enabled t)
  (setq lsp-pylsp-plugins-pycodestyle-enabled nil))
