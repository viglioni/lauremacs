;;; -*- lexical-binding: t; l-syntax: t -*-
(config-package agent-shell
  :init
  (lauremacs-major-mode-leader
    :keymaps 'agent-shell-mode-map
    "<f17>" '(agent-shell-help-menu :which-key "Agent shell menu")
    )
  )

(defun lauremacs/ai-company-claude ()
  (interactive)
  (let ((default-directory "~/company"))
    (agent-shell-anthropic-start-claude-code)))

(defun lauremacs/ai-personal-claude ()
  (interactive)
  (let* ((default-directory "~/personal")
        (buff-name (agent-shell-anthropic-start-claude-code)))))
