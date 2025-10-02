(config-package python-mode
  :mode "\\.py\\'"
  :hook ((python-mode . lsp-deferred)))
