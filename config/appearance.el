;;; -*- lexical-binding: t; l-syntax: t -*-
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode 1)           ; Disable the menu bar

(set-face-attribute 'default nil :font "Source Code Pro" :height 150)

;;
;; set theme
;;

(when (package-installed-p 'spacemacs-theme)
  (load-theme 'spacemacs-light t))


;;
;; Misc
;;

(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil) ; Do not use tabs for indentation
(setq-default tab-width 2)


