;;
;; @author Laura Viglioni
;; 2025
;;
;; GNU Public License 3.0
;;
;; since: NEXT
;;
;; core-packages.el:
;; Must have packages for the project.
;;

;;; code:

(config-package l
  :mode ("\\.el\\'" . l-mode)
  :custom
  (l-syntax t)
  :config
  (l-syntax-advices))

(config-package general
  :defer t
  :init
  (general-create-definer lauremacs-major-mode-leader
    :prefix "<f17>")
  (general-create-definer lauremacs-leader
    :prefix "<f19>"))

(config-package which-key
  :defer t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(config-package magit
  :init
  (setq magit-display-buffer-function
        (lambda (buffer)
          (display-buffer
           buffer
           (cond ((and (derived-mode-p 'magit-mode)
                       (eq (with-current-buffer buffer major-mode)
                           'magit-status-mode))
                  nil)
                 ((memq (with-current-buffer buffer major-mode)
                        '(magit-process-mode
                          magit-revision-mode
                          magit-diff-mode
                          magit-stash-mode))
                  nil)
                 (t
                  '(display-buffer-same-window)))))))

(config-package buttercup
  :defer t)

(config-package solarized-theme
  :init
  (load-theme 'solarized-light t))

(config-package avy) 

(config-package ace-window
  :after avy)

(config-package evil
   :commands (evil-mode
             evil-window-down
             evil-window-left
             evil-window-up
             evil-window-right))

(provide 'core-packages)

;;; core-packages.el ends here.
