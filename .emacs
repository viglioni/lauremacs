;;
;; Theme
;;

(load-theme 'spacemacs-light t)

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
    :prefix "<f19>")
  (lauremacs-leader
    "s" '(nil :which-key "search")
    "g" '(nil :which-key "git")
    "a" '(nil :which-key "applications")))

(use-package evil)

(use-package smartparens
  :config
  (smartparens-global-mode t)
  (sp-local-pair '(emacs-lisp-mode) "'" "'" :actions nil))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode)
	:init
	(add-hook 'eshell-mode-hook 'highlight-parentheses-mode))

(use-package paredit
  :hook '((emacs-lisp-mode . paredit-mode)
					(eshell-mode . paredit-mode)
          (eval-expression-minibuffer-setup . paredit-mode)
          (ielm-mode . paredit-mode)
          (lisp-mode . paredit-mode)
	  (minibuffer-setup-hook . paredit-mode)
          (lisp-interaction-mode . paredit-mode))
  :init
	(add-hook 'eshell-mode-hook 'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package company
  :bind (("C-/" . 'company-complete)
	 :map company-search-map
	 ("s-." . 'company-select-previous)
	 ("s-," . 'company-select-next))
  :init
  (global-company-mode 1))

(use-package expand-region
  :init
  (lauremacs-leader
    "v" '(er/expand-region :which-key "expand region")))

(use-package magit
  :init
  (lauremacs-leader
    "gs" '(magit-status :which-key "magit status")))

(global-prettify-symbols-mode 1)

(use-package yaml-mode
	:mode "\\.ya?ml\\'")

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
