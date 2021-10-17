;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; configs related to emacs lisp mode
;;

(require 'eval-sexp-fu)

(add-hook 'emacs-lisp-mode-hook 'eval-sexp-fu-flash-mode)

(use-package smartparens
  :config
  (smartparens-global-mode t)
  (sp-local-pair '(emacs-lisp-mode) "'" "'" :actions nil))

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


(lauremacs-major-mode-leader
 :keymaps 'emacs-lisp-mode-map
 "c" '(nil :which-key "compile")
 "c" '(nil :which-key "eval")
 "cc" '(emacs-lisp-byte-compile :which-key "byte compile")
 "eb" '(eval-buffer :which-key "eval-buffer")
 "er" '(eval-region :which-key "eval region"))

