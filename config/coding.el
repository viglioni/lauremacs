;;; -*- lexical-binding: t; l-syntax: t -*-
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
           :which-key "test project"))

  ;; horizontal line
  (global-hl-line-mode t)
  
  ;; prettify symbols
  (global-prettify-symbols-mode 1))

;;
;; Languages
;;
(lauremacs/load config lsp)
(lauremacs/load config langs lisps)
(lauremacs/load config langs python)
(lauremacs/load config langs web)

;;
;; Packages
;;

(config-package flycheck
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

(config-package flyspell
  :custom
  (flyspell-default-dictionary "en_GB")
  :hook '((org-mode . flyspell-mode)
	 (flyspell-mode . #'(lambda ()
               (setq flyspell-mode-map (make-sparse-keymap))))))


(config-package iedit
  :commands iedit-mode
  :bind (:map iedit-mode-keymap))

(config-package yasnippet
  :init
  (yas-global-mode 1)
  :custom
  ;; Use TAB as trigger key and fallback to original behavior
  (yas-trigger-key "TAB")
  (yas-fallback-behavior 'call-other-command)
  :bind (:map yas-minor-mode-map
              ("M-/" . yas-expand))) 
 
(config-package yasnippet-snippets
  :after yasnippet)

(config-package company
  :after yasnippet  
  :bind (:map company-search-map
	            ("C-/" . 'company-complete)
              ("s-." . 'company-select-previous)
              ("s-," . 'company-select-next)
              )
  :custom
  (company-minimum-prefix-length 1)
  (company--idle-delay 0.05)
  (company-selection-wrap-around t)
  (company-dabbrev-downcase nil)
  (company-global-mode t)
  (company-backends '((company-capf :with company-yasnippet) company-yasnippet)
		    ;; Don't auto-select candidates
		    company-frontends '(company-pseudo-tooltip-frontend
					company-echo-metadata-frontend))  
  ;; Show documentation when available
  (company-show-quick-access t)
  :init
  (global-company-mode))

(config-package company-box
  :after company
  :hook (company-mode . company-box-mode))

(config-package company-prescient
  :after company
  :config (company-prescient-mode 1))

(config-package highlight-indentation
  :commands highlight-indentation-mode)

(config-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode))

(config-package paren
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



(config-package envrc
  :init
  (envrc-global-mode))
