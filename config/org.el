;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

(require 'org-faces)


;;;###autoload
(defmacro define-org-cmd (&rest plist)
  "Receives a PLIST (:situation 'command)  as args to define which
command should be called on each situation. 
Obs.: the command will ONLY be called on the specific situation.
*~*~*
For now the supported keys are
:heading -> runs when cursor is over a heading
:table -> runs when cursor is over a table
:item -> runs when cursor is over an item 
*~*~*
example: (define-org-cmd :heading 'my-fn :table 'my-fn2)"
  (throw-if (oddp (length plist)) "arg list must have an even number of args")
  `(lambda ()
     (interactive)
     (cond
      ((org-at-heading-p) (funcall ,(plist-get plist :heading)))
      ((org-at-table-p)   (funcall ,(plist-get plist :table)))
      ((org-at-item-p)    (funcall ,(plist-get plist :item))))))


;;
;; org babel
;;

(with-eval-after-load "ob-core"
  ;; dont ask before running code
  (setq org-confirm-babel-evaluate nil))

(with-eval-after-load "ob-lob"
	;; org babel
	(org-babel-lob-ingest (join-path
												 lauremacs-config-dir
												 "org-mode-extra-configs.org")))

(defun lauremacs/org-font-setup ()
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

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun lauremacs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(bind-lazy-function 'org-insert-src
										'lauremacs/org-insert-source 
										'lauremacs-org-extensions)

(use-package org
  :hook (org-mode . lauremacs/org-mode-setup)
	:custom
	(org-startup-folded t)
  (org-startup-with-latex-preview nil)
  :init
  (lauremacs/org-font-setup)
	;; keymaps
  (general-define-key
   :keymaps 'org-mode-map
   "M-s-m" (define-org-cmd :heading 'org-promote-subtree)
	 "M-s-," (define-org-cmd
							 :heading 'org-move-subtree-down
							 :item    'org-move-item-down)
   "M-s-." (define-org-cmd
								 :heading 'org-move-subtree-up
								 :item    'org-move-item-up)
   "M-s-/" (define-org-cmd :heading 'org-demote-subtree))
	
	(lauremacs-major-mode-leader
		:keymaps 'org-mode-map
		"i" '(nil :which-key "insert")
		"ic" '(org-insert-src :which-key "insert code block source"))
	(org-babel-do-load-languages
   'org-babel-load-languages
   '((haskell . t)
     (clojure . t)
     (emacs-lisp . t)
     (python . t)
     (js . t)
     (C . t)
     (latex . t)
     (shell . t)
     (sql . t))))

(use-package org-bullets
	:after org
	:hook (org-mode . org-bullets-mode)
	:custom
	(org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
