;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; prog mode
;;

(with-eval-after-load "prog-mode"
  (lauremacs-major-mode-leader
    :keymaps 'prog-mode-map
    "t"  '(nil
           :which-key "test")
    "tt" '(projectile-toggle-between-implementation-and-test
           :which-key "toggle between test and implementation")
    "to" '(projectile-find-implementation-or-test-other-window
           :which-key "find implementation or test other window")
    "tp" '(projectile-test-project
           :which-key "test project")))

;;
;; smartparens
;;

(use-package smartparens 
  :config
  (smartparens-global-mode t)
  :init
  (require 'smartparens-config)
  (sp-use-paredit-bindings))


;;
;; Paredit
;;

(use-package paredit
  :hook '((emacs-lisp-mode                  . enable-paredit-mode)
          (eshell-mode                      . enable-paredit-mode)
          (eval-expression-minibuffer-setup . enable-paredit-mode)
          (ielm-mode                        . enable-paredit-mode)
          (lisp-mode                        . enable-paredit-mode)
          (minibuffer-setup-hook            . enable-paredit-mode)
          (lisp-interaction-mode            . enable-paredit-mode))
  :init
  (add-hook 'eshell-mode-hook 'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))


;;
;; Doom modeline
;;

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)

  (doom-modeline-def-modeline 'lauremacs-modeline
    '(bar window-number buffer-info matches selection-info media-info)
    '(checker lsp word-count pdf-pages major-mode workspace-name vcs hud buffer-position))

  (doom-modeline-set-modeline 'lauremacs-modeline 'default)
  
  :custom
  (doom-modeline-buffer-file-name-style 'file-name)
  (doom-modeline-enable-word-count t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode emacs-lisp-mode)))


;;
;; Highlight indentation
;;

(use-package highlight-indentation
  :commands highlight-indentation-mode)

;;
;; rainbow delimiters
;;

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode))


;;
;; Show paren mode
;;

(use-package paren
  :commands show-paren-mode
  :hook (prog-mode . show-paren-mode)
  :init
  (set-face-attribute 'show-paren-mismatch nil
                      :background "red"
                      :foreground "black"
                      :underline nil)
  (set-face-attribute 'show-paren-match nil
                      :background "#c93360"
                      :foreground "#f4d6df"
                      :underline nil))

;;
;; Company
;;

(use-package company
  :bind (("C-/" . 'company-complete)
         :map company-search-map
         ("s-." . 'company-select-previous)
         ("s-," . 'company-select-next))
  :config
  :init
  (setq company-dabbrev-downcase nil)
  (global-company-mode 1))

(use-package company
  :after '(company org))

;;
;; Evil
;;

(use-package evil
  :commands (evil-mode
             evil-window-down
             evil-window-left
             evil-window-up
             evil-window-right))

;;
;; Prettify symbols
;;

(global-prettify-symbols-mode 1)

;;
;; Emojify mode
;;

(use-package emojify
  :commands (emojify-mode emojify-insert-emoji emojify-apropos-emoji)
  :hook '((org-mode          . emojify-mode)
          (magit-status-mode . emojify-mode)
          (text-mode         . emojify-mode))
  :init
  (lauremacs-leader
    "ie" '(emojify-insert-emoji :which-key "insert emoji")))

;;
;; Yasnippet
;;

(use-package yasnippet
  :init
  (yas-global-mode 1)
  :bind (:map yas-minor-mode-map
              ("M-/" . yas-expand)
              ("TAB" . nil)))

(use-package yasnippet-snippets
  :after yasnippet)

;;
;; Iedit
;;

(use-package iedit
  :commands iedit-mode
  :bind (:map iedit-mode-keymap)
  :init
  (lauremacs-leader
    "se" '(iedit-mode :which-key "iedit mode")))

;;
;; Flycheck / Flyspell
;;

(bind-lazy-function 'explain-error-at-point
                    'lauremacs-ide-explain-error-at-point
                    'lauremacs-ide-extra)

(use-package flycheck
  :init
  (global-flycheck-mode)
  
  (lauremacs-leader  
    "e"  '(:keymap flycheck-command-map :package flycheck :which-key "errors")
    "ee" '(explain-error-at-point                         :which-key "explain error at point"))
  
  (set-face-attribute 'flycheck-fringe-info nil
                      :background "#3a81c3"
                      :foreground "white")
  
  (set-face-attribute 'flycheck-fringe-error nil
                      :background "#e0211d"
                      :foreground "white")
  
  (set-face-attribute 'flycheck-fringe-warning nil
                      :background "#dc752f"
                      :foreground "white"))

(with-eval-after-load "flyspell"
  (setq flyspell-default-dictionary "en_GB")
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'flyspell-mode-hook
            '(lambda ()
               (define-key flyspell-mode-map (kbd "C-;") nil)
               (setq flyspell-mode-map (make-sparse-keymap)))))

;;
;; Hide show
;;

(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :init
  (general-define-key
   :prefix "C-c b"
   ""  '(nil           :which-key "hide/show code blocks")
   "s" '(hs-show-block :which-key "show")
   "h" '(hs-hide-block :which-key "hide")
   "l" '(hs-hide-level :which-key "hide level")
   "t" '(hs-hide-level :which-key "toggle hide/show")
   "S" '(hs-show-all   :which-key "show all")
   "H" '(hs-hide-all   :which-key "hide all")))


;;
;; Better jumper
;;

(use-package better-jumper
  :hook (prog-mode . better-jumper-mode)
  :init
  (lauremacs-leader
    "cj"  '(nil                         :which-key "jumper")
    "cjj" '(better-jumper-set-jump      :which-key "set jump")
    "cjb" '(better-jumper-jump-backward :which-key "jump backward")
    "cjf" '(better-jumper-jump-forward  :which-key "jump forward")))


;;
;; smerge
;;

(with-eval-after-load "smerge-mode"
  (lauremacs-leader
    "ge" '( :keymap smerge-basic-map
            :package smerge-mode
            :which-key "git diff - smerge")))


