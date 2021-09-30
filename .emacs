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

(use-package company
  :bind (("C-/" . 'company-complete)
	 :map company-search-map
	 ("s-." . 'company-select-previous)
	 ("s-," . 'company-select-next))
  :init
  (global-company-mode 1))

(use-package multiple-cursors
  :bind (:map mc/keymap
	      ("<return>" . nil))
  :init
  (general-define-key
   :prefix "C-c m"
   "" '(nil :which-key "multi-cursor")
   "<mouse-1>" '(mc/add-cursor-on-click :which-key "add cursor on click")
   "m" '(mc/edit-lines :which-key "edit lines")
   "a" '(mc/edit-beginnings-of-lines :which-key "edit beginnings of lines")
   "e" '(mc/edit-ends-of-lines :which-key "edit ends of lines")
   "w" '(mc/mark-all-words-like-this :which-key "mark all words like this")
   "s" '(mc/mark-all-symbols-like-this :which-key "mark all symbols like this")
   "t" '(mc/mark-all-like-this :which-key "mark all like this")
   "r" '(mc/mark-all-in-region :which-key "mark all in region")
   "n" '(mc/mark-next-like-this :which-key "mark next like this")
   "p" '(mc/mark-previous-like-this :which-key "mark previous like this")
   "N" '(mc/skip-to-next-like-this :which-key "skip to next like this")
   "P" '(mc/skip-to-previous-like-this :which-key "skip to previous like this")
   "u" '(nil :which-key "unmark")
   "un" '(mc/unmark-next-like-this :which-key "unmark last like this")
   "up" '(mc/unmark-previous-like-this :which-key "unmark first like this"))
  (lauremacs-leader
   "m" '(nil :which-key "multi-cursor")
   "m <mouse-1>" '(mc/add-cursor-on-click :which-key "add cursor on click")
   "mm" '(mc/edit-lines :which-key "edit lines")
   "ma" '(mc/edit-beginnings-of-lines :which-key "edit beginnings of lines")
   "me" '(mc/edit-ends-of-lines :which-key "edit ends of lines")
   "mw" '(mc/mark-all-words-like-this :which-key "mark all words like this")
   "ms" '(mc/mark-all-symbols-like-this :which-key "mark all symbols like this")
   "mt" '(mc/mark-all-like-this :which-key "mark all like this")
   "mr" '(mc/mark-all-in-region :which-key "mark all in region")
   "mn" '(mc/mark-next-like-this :which-key "mark next like this")
   "mp" '(mc/mark-previous-like-this :which-key "mark previous like this")
   "mN" '(mc/skip-to-next-like-this :which-key "skip to next like this")
   "mP" '(mc/skip-to-previous-like-this :which-key "skip to previous like this")
   "mu" '(nil :which-key "unmark")
   "mun" '(mc/unmark-next-like-this :which-key "unmark last like this")
   "mup" '(mc/unmark-previous-like-this :which-key "unmark first like this")))

(use-package expand-region
  :init
  (lauremacs-leader
    "v" '(er/expand-region :which-key "expand region")))

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
