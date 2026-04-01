;;; e-ink-theme.el --- A light theme with pure white background

;; Copyright (C) 2026

;; Author: Laura
;; Version: 1.0
;; Package-Requires: ((emacs "24"))

;;; Commentary:
;; A light theme based on spacemacs-light with pure white background and black text.

;;; Code:

(deftheme e-ink
  "A light theme based on spacemacs-light with pure white background and black text.")

(let ((class '((class color) (min-colors 89)))
      ;; Base colors
      (bg "#ffffff")
      (fg "#000000")
      (bg-alt "#f5f5f5")
      (fg-alt "#333333")
      
      ;; Syntax highlighting (keeping spacemacs-light colors but adjusted)
      (keyword "#3a81c3")
      (string "#2d9574")
      (constant "#4e3163")
      (function "#6c3163")
      (variable "#715ab1")
      (type "#ba2f59")
      (comment "#9ca0a4")
      (warning "#dc752f")
      (error "#e0211d")
      
      ;; UI elements
      (cursor "#000000")
      (highlight "#e8e8e8")
      (hl-line "#f0f0f0")
      (selection "#d3d3d3")
      (match-bg "#ffff00")
      (border "#dddddd"))
  
  (custom-theme-set-faces
   'e-ink
   
   ;; Basic faces
   `(default ((,class (:foreground ,fg :background ,bg))))
   `(cursor ((,class (:background ,cursor))))
   `(region ((,class (:background ,selection))))
   `(highlight ((,class (:background ,highlight))))
   `(hl-line ((,class (:background ,hl-line))))
   `(fringe ((,class (:background ,bg))))
   `(vertical-border ((,class (:foreground ,border))))
   
   ;; Font lock (syntax highlighting)
   `(font-lock-builtin-face ((,class (:foreground ,keyword))))
   `(font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-constant-face ((,class (:foreground ,constant))))
   `(font-lock-function-name-face ((,class (:foreground ,function :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,keyword :weight bold))))
   `(font-lock-string-face ((,class (:foreground ,string))))
   `(font-lock-type-face ((,class (:foreground ,type))))
   `(font-lock-variable-name-face ((,class (:foreground ,variable))))
   `(font-lock-warning-face ((,class (:foreground ,warning :weight bold))))
   
   ;; Modeline
   `(mode-line ((,class (:background ,bg-alt :foreground ,fg :box (:line-width 2 :color ,border)))))
   `(mode-line-inactive ((,class (:background ,highlight :foreground ,comment :box (:line-width 2 :color ,border)))))
   
   ;; Line numbers
   `(line-number ((,class (:foreground ,comment :background ,bg))))
   `(line-number-current-line ((,class (:foreground ,fg :background ,hl-line :weight bold))))
   
   ;; Search
   `(isearch ((,class (:foreground ,fg :background ,match-bg :weight bold))))
   `(lazy-highlight ((,class (:background ,highlight :weight bold))))
   
   ;; Org mode
   `(org-level-1 ((,class (:foreground ,keyword :weight bold :height 1.3))))
   `(org-level-2 ((,class (:foreground ,function :weight bold :height 1.2))))
   `(org-level-3 ((,class (:foreground ,type :weight bold :height 1.1))))
   `(org-level-4 ((,class (:foreground ,variable :weight bold))))
   `(org-level-5 ((,class (:foreground ,constant :weight bold))))
   `(org-todo ((,class (:foreground ,error :weight bold))))
   `(org-done ((,class (:foreground ,string :weight bold))))
   `(org-link ((,class (:foreground ,keyword :underline t))))
   `(org-code ((,class (:foreground ,constant :background ,bg-alt))))
   `(org-block ((,class (:background ,bg-alt))))
   `(org-block-begin-line ((,class (:foreground ,comment :background ,bg-alt))))
   `(org-block-end-line ((,class (:foreground ,comment :background ,bg-alt))))
   
   ;; Company mode
   `(company-tooltip ((,class (:background ,bg-alt :foreground ,fg))))
   `(company-tooltip-selection ((,class (:background ,selection))))
   `(company-tooltip-common ((,class (:foreground ,keyword :weight bold))))
   
   ;; Helm
   `(helm-selection ((,class (:background ,selection))))
   `(helm-match ((,class (:foreground ,keyword :weight bold))))
   
   ;; Magit
   `(magit-section-highlight ((,class (:background ,hl-line))))
   `(magit-diff-added ((,class (:foreground ,string :background "#e6ffed"))))
   `(magit-diff-removed ((,class (:foreground ,error :background "#ffeef0"))))
   ))

(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'e-ink)

;;; e-ink-theme.el ends here
