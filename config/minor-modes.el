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
  :hook
  ((emacs-lisp-mode                  . enable-paredit-mode)
   (eval-expression-minibuffer-setup . enable-paredit-mode)
   (lisp-interaction-mode            . enable-paredit-mode)
   (lisp-mode                        . enable-paredit-mode)
   (minibuffer-exit                  . my/restore-paredit-key)
   (minibuffer-setup                 . my/conditionally-enable-paredit-mode)
   (minibuffer-setup                 . my/conditionally-enable-paredit-mode)
   (eshell-mode                      . (lambda ()
                                         (define-key eshell-mode-map
                                                     (kbd "<return>")
                                                     'eshell-send-input))))
  :config
  (defvar my/paredit-minibuffer-commands '(eval-expression
                                           ielm-eval-input
                                           pp-eval-expression
                                           eval-expression-with-eldoc
                                           ibuffer-do-eval
                                           ibuffer-do-view-and-eval
                                           org-ql-sparse-tree
                                           org-ql-search)
    "Interactive commands for which paredit should be enabled in the minibuffer.")

  (defun my/conditionally-enable-paredit-mode ()
    "Enable paredit during lisp-related minibuffer commands."
    (when (memq this-command my/paredit-minibuffer-commands)
      (enable-paredit-mode)
      (unbind-key (kbd "RET") paredit-mode-map)))

  (defun my/restore-paredit-key ()
    "Restore the RET binding that was disabled by
  my/conditionally-enable-paredit-mode."
    (bind-key (kbd "RET") #'paredit-newline paredit-mode-map)))

  ;;
  ;; Modeline
  ;;

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)

  ;; removed checker TODO: see later whats the issue
  (doom-modeline-def-modeline 'lauremacs-modeline
    '(bar window-number buffer-info matches selection-info media-info)
    '(buffer-position  lsp word-count pdf-pages major-mode workspace-name vcs hud))

  (doom-modeline-set-modeline 'lauremacs-modeline 'default)
  
  :custom
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-enable-word-count t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode emacs-lisp-mode)))

(use-package nyan-mode
  :custom
  (nyan-wavy-trail t)
  (nyan-animate-nyancat t)
  (nyan-bar-length 16)
  :init
  (nyan-mode))

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
                      :background "#3a81c3"
                      :foreground "white"
                      :underline nil))

;;
;; Company
;;


(use-package company
  :after yasnippet
  :bind (("C-/" . 'company-complete)
         ("s-." . 'company-select-previous)
         ("s-," . 'company-select-next)
         :map company-search-map)
  :custom
  (company-minimum-prefix-length 1)
  (company--idle-delay 0.05)
  (company-selection-wrap-around t)
  (company-dabbrev-downcase nil)
  :init
  (setq company-global-mode t)
  (setq company-backends '((company-capf :with company-yasnippet) company-yasnippet)
        ;; Don't auto-select candidates
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend))  
  ;; Show documentation when available
  (setq company-show-quick-access t)

  (use-package company-box
    :ensure t
    :hook (company-mode . company-box-mode)))

(use-package company-prescient
  :ensure t
  :after company
  :config (company-prescient-mode 1))

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
          (text-mode         . emojify-mode)))

;;
;; Yasnippet
;;
(use-package yasnippet
  :init
  (yas-global-mode 1)
  :custom
  ;; Use TAB as trigger key and fallback to original behavior
  (yas-trigger-key "TAB")
  (yas-fallback-behavior 'call-other-command)
  :bind (:map yas-minor-mode-map
              ("M-/" . yas-expand))) 
 
(use-package yasnippet-snippets
  :after yasnippet)

;;
;; Iedit
;;

(use-package iedit
  :commands iedit-mode
  :bind (:map iedit-mode-keymap))

;;
;; Flycheck / Flyspell
;;

(bind-lazy-function 'explain-error-at-point
                    'lauremacs-ide-explain-error-at-point
                    'lauremacs-ide-extra)

(use-package flycheck
  :init
  (global-flycheck-mode)
  
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
            #'(lambda ()
               (define-key flyspell-mode-map (kbd "C-;") nil)
               (setq flyspell-mode-map (make-sparse-keymap)))))

;;
;; Rainbow mode
;;

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))


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
  :hook (prog-mode . better-jumper-mode))

;;
;; horizontal line
;;
(with-eval-after-load "hl-line-mode"
  (global-hl-line-mode t))
