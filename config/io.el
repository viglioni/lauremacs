(config-package emoji-cheat-sheet-plus
    :defer t
    :init
    (progn
      ;; enabled emoji in buffer
      (add-hook 'org-mode-hook 'emoji-cheat-sheet-plus-display-mode)
      ))
