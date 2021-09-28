;;
;; Theme
;;

(load-theme 'spacemacs-dark t)

;;
;; Loading another packages
;;

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package general
  :init
  (general-create-definer lauremacs-major-mode-leader
                          :prefix "<f17>")
  (general-create-definer lauremacs-leader
                          :prefix "<f19>"))

(use-package evil)

(use-package helm
  :bind (("M-x" . 'helm-M-x))
  :init
  (lauremacs-leader "<f19>" '(helm-M-x :which-key "M-x")))

(use-package paredit
  :hook '((emacs-lisp-mode . paredit-mode)
          (eval-expression-minibuffer-setup-hook . paredit-mode)
          (ielm-mode-hook . paredit-mode)
          (lisp-mode-hook . paredit-mode)
          (lisp-interaction-mode-hook . paredit-mode)))

(use-package auto-complete
  :hook (prog-mode . auto-complete-mode)
  :init (ac-config-default))

;;
;; Load config files
;;

(let ((config-files (directory-files
                     (concat lauremacs-dir "/config")
                     t (rx (and (+ (| alphanumeric "." "-"))
                                ".el"
                                line-end)))))
  (mapcar 'load-file config-files))


;;
;; Custom variables
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
