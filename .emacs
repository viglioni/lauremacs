;;
;; Theme
;;

(load-theme 'spacemacs-dark t)

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
;; Loading another packages
;;

(use-package helm
  :bind (("M-x" . 'helm-M-x)))

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package general
  :init
  (general-define-key
   "<f19> <f19>" '(helm-M-x :which-key "M-x")))

(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode))

(use-package auto-complete
  :hook (prog-mode . auto-complete-mode)
  :init (ac-config-default))

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
