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
  ;;  :config
  ;; (l-syntax-advices)
  )

(config-package helm
  :custom
  (helm-M-x-fuzzy-match        t)
  (helm-apropos-fuzzy-match    t)
  (helm-buffers-fuzzy-matching t)
  (helm-imenu-fuzzy-match      t)
  (helm-lisp-fuzzy-completion  t)
  (helm-locate-fuzzy-match     t)
  (helm-recentf-fuzzy-match    t)
  (helm-semantic-fuzzy-match   t)
  (helm-ff-skip-boring-files   t)
  :init
  (helm-mode 1)
  )

(config-package helm-swoop
  :after helm)

(config-package helm-flx
	:after helm
	:init (helm-flx-mode 1))

(config-package helm-projectile
  :after (projectile helm))

(config-package helm-posframe
  :after helm
  :init
  (helm-posframe-enable))

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

(config-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

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

(config-package solarized-theme)

(config-package avy) 

(config-package ace-window
  :after avy)

(config-package evil
   :commands (evil-mode
             evil-window-down
             evil-window-left
             evil-window-up
             evil-window-right))

(use-package neotree
  :after (projectile)
  :custom
  (neo-smart-open t)
  (neo-autorefresh t)
  (neo-window-width 35)
  (neo-window-position 'right)
  (neo-theme (if (display-graphic-p) 'icons 'arrow))
  (neo-show-hidden-files t))


(provide 'core-packages)

;;; core-packages.el ends here.
