;;
;; Garbage colector threshold
;;

(setq gc-cons-threshold (* 20 1024 1024)) ;; initial threshold

(add-hook 'emacs-startup-hook ;; threshold after init
          (lambda () (setq gc-cons-threshold (* 128 1024 1024)))) 

;;
;; Layout
;;

(setq inhibit-startup-message t)
(setq initial-buffer-choice lauremacs-buffer-name)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

(setq visible-bell t)       ; Set up the visible bell


(set-face-attribute 'default nil :font "Source Code Pro" :height 150)



