;;; -*- lexical-binding: t; l-syntax: t -*-
(config-package agent-shell
  :init
  (lauremacs-major-mode-leader
    :keymaps 'agent-shell-mode-map
    "<f17>" '(agent-shell-help-menu :which-key "Agent shell menu")
    )
  (require 'agent-shell-usage)
  )
