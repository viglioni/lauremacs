(defun lauremacs/org-font-setup ()
  (interactive)
  ;; Replace list hyphen with dot
  (font-lock-add-keywords
   'org-mode
   '(("^ *\\([-]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :weight 'regular :height (cdr face)))

  ;; set a smaller font size for meta lines
  (set-face-attribute 'org-meta-line nil :height 100)
  (set-face-attribute 'org-drawer nil :height 100)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute  'org-block            nil  :inherit  'fixed-pitch              :foreground  nil  )
  (set-face-attribute  'org-code             nil  :inherit  '(shadow                  fixed-pitch))
  (set-face-attribute  'org-formula          nil  :inherit  '(fixed-pitch)
                       :weight 'semi-bold
                       :background (face-attribute 'org-table :background)
                       :foreground  "#3a81c3")
  (set-face-attribute  'org-table            nil  :inherit  '(shadow                  fixed-pitch))
  (set-face-attribute  'org-date             nil  :inherit  '(shadow                  fixed-pitch))
  (set-face-attribute  'org-verbatim         nil  :inherit  '(shadow                  fixed-pitch))
  (set-face-attribute  'org-special-keyword  nil  :inherit  '(font-lock-comment-face  fixed-pitch))
  (set-face-attribute  'org-meta-line        nil  :inherit  '(font-lock-comment-face  fixed-pitch))
  (set-face-attribute  'org-checkbox         nil  :inherit  'fixed-pitch)

  ;; Enlarge org inline latex previews
  (plist-put org-format-latex-options :scale 1.6))

(defun lauremacs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
	(lauremacs/org-font-setup)
  (visual-line-mode 1))

(config-package org-bullets
	:hook (org-mode		. org-bullets-mode)
	:custom
	(org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(config-package olivetti
	:hook '((org-mode				. olivetti-mode)
					(olivetti-mode	. '(lambda () (set-face-attribute 'olivetti-fringe nil
																											 :background "#d0cec7"))))
	:custom
	(olivetti-minimum-body-width 80)
	(olivetti-style 'fancy))

;; (config-package org-appear
;; 	:after org
;; 	:hook (org-mode . org-appear-mode))

