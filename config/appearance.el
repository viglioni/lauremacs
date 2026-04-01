;;; -*- lexical-binding: t; l-syntax: t -*-
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode 1)           ; Disable the menu bar

(set-face-attribute 'default nil :font "Source Code Pro" :height 150)

;;
;; Misc
;;

(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil) ; Do not use tabs for indentation
(setq-default tab-width 2)

;;
;; Theme and modeline (needs package-manager to be loaded)
;;

(with-eval-after-load 'package-manager
  ;; Set theme
  (config-package spacemacs-theme
    :config
    (load-theme 'spacemacs-light t))

  ;; Doom modeline
  (config-package doom-modeline
    :init
    (setq doom-modeline-buffer-file-name-style 'relative-to-project
          doom-modeline-enable-word-count t
          doom-modeline-buffer-encoding nil
          doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode emacs-lisp-mode))
    :config
    ;; Define custom modeline layout
    (doom-modeline-def-modeline 'lauremacs-modeline
                                '(bar window-number buffer-info matches selection-info media-info)
                                '(buffer-position lsp word-count pdf-pages major-mode workspace-name vcs hud))
    
    ;; Set as default
    (doom-modeline-set-modeline 'lauremacs-modeline 'default)
    
    ;; Enable doom-modeline
    (doom-modeline-mode 1))

  ;; Nyan mode
  (config-package nyan-mode
    :init
    (setq nyan-wavy-trail t
          nyan-animate-nyancat t
          nyan-bar-length 16)
    (nyan-mode)))


