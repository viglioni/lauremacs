;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

(require 'org-faces)
(require 'org-extra)


(bind-lazy-function 'org-compile-to-pdf 'LauTeX-compile-org-to-pdf 'org-extra)
(bind-lazy-function 'org-preview-latex-on-buffer 'LauTeX-preview-latex-on-buffer 'org-extra)
(bind-lazy-function 'org-insert-mathbb 'LauTex-insert-mathbb 'org-extra)
(bind-lazy-function 'org-insert-mathcal 'LauTex-insert-mathcal 'org-extra)
(bind-lazy-function 'org-add-bold-to-region 'org-extra-add-bold-to-region 'org-extra)
(bind-lazy-function 'org-add-italic-to-region 'org-extra-add-italic-to-region 'org-extra)
(bind-lazy-function 'org-add-code-to-region 'org-extra-add-code-to-region 'org-extra)
(bind-lazy-function 'org-add-strikethrough-to-region 'org-extra-add-strikethrough-to-region 'org-extra)
(bind-lazy-function 'org-add-verbatin-to-region 'org-extra-add-verbatin-to-region 'org-extra)
(bind-lazy-function 'org-add-underline-to-region 'org-extra-add-underline-to-region 'org-extra)





;;;###autoload
(defmacro define-org-cmd (&rest plist)
  "Receive a PLIST (:situation 'command) as args to define which
command should be called on each situation . 
Obs                                        . : the command will ONLY be called on the specific situation.
*~*~*
For now the supported keys are
:heading -> runs when cursor is over a heading
:table -> runs when cursor is over a table
:item -> runs when cursor is over an item
*~*~*
example: (define-org-cmd :heading 'my-fn :table 'my-fn2)"
  (throw-if (cl-oddp (length plist)) "arg list must have an even number of args")
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
												 lauremacs-config-dir-old
												 "org-mode-extra-configs.org")))


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

(defun lauremacs/add-org-agenda-files ()
	"Add all org files inside `lauremacs-agenda-dir' to `org-agenda-files'."
	(interactive)
	(let ((agenda-files (directory-files lauremacs-agenda-dir t "[a-zA-Z0-9-.]+\\.org$")))
		(defvar org-agenda-files nil)
		(print agenda-files)
		(dolist (file agenda-files)
			(add-to-list 'org-agenda-files file))))

(defun lauremacs/get-agenda-file-path (filename)
	(join-path lauremacs-agenda-dir (concat filename ".org")))

(bind-lazy-function 'org-insert-src
										'lauremacs/org-insert-source 
										'lauremacs-org-extensions)

(use-package org
  :hook '((org-mode . lauremacs/org-mode-setup)
          (org-mode . hl-line-mode)
          (org-mode . prettify-symbols-mode)
          (org-mode . '(lambda () (add-multiple-into-list 'prettify-symbols-alist
																										      '((">=" . "≥")
																											      ("<=" . "≤")
																											      ("!=" . "≠")
                                                            ("=>" . "⇒")
                                                            ("<=" . "⇐")
                                                            ("->" . "→")
                                                            ("<-" . "←"))))))
	:custom
	(org-hide-emphasis-markers t)
	(org-startup-folded t)
  (org-startup-with-latex-preview nil)
	(haskell-process-type 'stack-ghci)
	(org-highlight-latex-and-related '(latex script entities))
	(org-image-actual-width nil)
  (org-startup-with-inline-images t)
  (org-export-use-babel nil)
  (org-html-preamble nil)
  (org-html-postamble nil)
  :init
  
	;; org-agenda
	(lauremacs/add-org-agenda-files)
  (with-eval-after-load "ol"
    (add-to-list 'org-link-frame-setup '(file . find-file)))
  
	(setq org-capture-templates
				`(;; Health
					("h" "Health")
					
					("hs" "To Schedule" entry
					 (file+olp ,(lauremacs/get-agenda-file-path "health") "Todo")
					 "* TOSCHEDULE %?\n DEADLINE: %^t" :empty-lines 0)
					
					("ht" "To Schedule" entry
					 (file+olp ,(lauremacs/get-agenda-file-path "health") "Todo")
					 "* TODO %?\n DEADLINE: %^t" :empty-lines 0)
					
					("ha" "Appointment" entry
					 (file+olp+datetree ,(lauremacs/get-agenda-file-path "health"))
					 ,(concat "* %? :appoitments:\n"
										"<%<%Y-%m-%d %a %^{Time}>>")
					 :time-prompt t
					 :empty-lines 0)))
	
	;; keymaps
  (general-define-key
   :keymaps 'org-mode-map
   "C-M-<return>" '(org-insert-todo-heading :general "insert todo heading")
   "M-s-m" (define-org-cmd
            :heading 'org-promote-subtree
            :table   'org-table-move-column-left)
	 "M-s-," (define-org-cmd
						:heading 'org-move-subtree-down
						:item    'org-move-item-down
            :table   'org-table-move-row-down)
   "M-s-. " (define-org-cmd
						:heading 'org-move-subtree-up
						:item    'org-move-item-up
            :table   'org-table-move-row-up)
   "M-s-/" (define-org-cmd
            :heading 'org-demote-subtree
            :table   'org-table-move-column-right)
   "s-d" 'org-table-copy-down)
	
	(lauremacs-major-mode-leader
		:keymaps 'org-mode-map
		"T"		'(nil                              :which-key "toggle")
		"Tf"	'(org-fragtog-mode                 :which-key "toggle fragtog mode")
		"i"   '(nil                              :which-key "insert")
		"ic"  '(org-insert-src                   :which-key "insert code block source")
		"im"  '(nil                              :which-key "insert math")
		"imb" '(org-insert-mathbb                :which-key "insert mathbb")
		"imc" '(org-insert-mathcal               :which-key "insert mathcal")
		"l"   '(nil                              :which-key "LaTeX")
		"le"  '(nil                              :which-key "export")
		"lep" '(org-compile-to-pdf               :which-key "export to pdf")
		"p"		'(nil                              :which-key "preview")
		"pl"  '(nil                              :which-key "preview LaTeX")
		"plb" '(org-preview-latex-on-buffer      :which-key "preview LaTeX on buffer")
		"plc" '(org-clear-latex-preview          :which-key "clear LaTeX preview")
		"plt" '(org-latex-preview                :which-key "toggle LaTeX preview at point")
		"x"   '(nil                              :which-key "word")
		"xb"  '(org-add-bold-to-region           :which-key "bold")
		"xc"  '(org-add-code-to-region           :which-key "code")
		"xi"  '(org-add-italic-to-region         :which-key "italic")
		"xs"  '(org-add-strikethrough-to-region  :which-key "strikethrough")
		"xu"	'(org-add-underline-to-region      :which-key "underline")
		"xv"  '(org-add-verbatin-to-region       :which-key "verbatin")
    "r"   '(org-extra-recalc-buffer-and-save :which-key "recalc buff and save")
    "t"   '(nil                              :which-key "table")
    "ti"  '(nil                              :which-key "insert")
    "tic" '(org-table-insert-column          :which-key "insert column")
    "tih" '(org-table-insert-hline           :which-key "insert horizontal line")
    "tim" '(org-table-hline-and-move)        :which-key "insert hline and move"
    "tir" '(org-table-insert-row             :which-key "insert row")
    "td"  '(nil                              :which-key "delete")
    "tdr" '(org-table-kill-row               :which-key "delete row")
    "tdc" '(org-table-delete-column          :which-key "delete column"))

	;; LaTeX
	(sp-local-pair 'org-mode "$" "$" )
  (exec-path-when-cmd-not-found "latex")
	(setq-local company-backends
              (append '((company-math-symbols-latex company-latex-commands))
                      company-backends))
	
	;; Org babel
  (require 'ob-ts)
  (require 'ob-elixir)
	(org-babel-do-load-languages
   'org-babel-load-languages
   '((elixir		 . t)
     (sqlite     . t)
		 (haskell		 . t)
     (mermaid    . t)
     (clojure		 . t)
     (emacs-lisp . t)
		 (ts         . t)
     (python		 . t)
     (js				 . t)
     (C					 . t)
     (latex			 . t)
     (restclient . t)
     (sagemath   . t)
     (shell      . t)
     (sql        . t)))

  (setq org-babel-default-header-args:sage '((:session . t)
                                             (:results . "output")))
  :bind
  (:map org-mode-map
        (("C-c C-b" . org-mark-ring-goto)
         ("C-c c"   . org-table-blank-field)))
  )


(use-package org-bullets
	:after org
	:hook (org-mode		. org-bullets-mode)
	:custom
	(org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package olivetti
	:after org
	:hook '((org-mode				. olivetti-mode)
					(olivetti-mode	. '(lambda () (set-face-attribute 'olivetti-fringe nil
																											 :background "#d0cec7"))))
	:custom
	(olivetti-minimum-body-width 80)
	(olivetti-style 'fancy))


(use-package org-appear
	:after org
	:hook (org-mode . org-appear-mode))

(use-package org-fragtog
	:after org)

(use-package ob-elixir
	:after org)

(use-package ob-restclient
  :after org)

;;
;; Org roam
;;

(defconst default-org-roam-template
  '("d" "default" plain
    "%?"
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
    :unnarrowed t))

(defconst org-roam-math-template
  '("m" "math" plain
    "\n\n%?"
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                       "
# -*- eval: (org-math-mode 1); ispell-local-dictionary: \"en\"; -*-
#+title: ${title}
#+options: tex:t
#+startup: latexpreview
#+filetags: :math:")
    :unnarrowed t))

(defconst org-roam-private-template
  '("p" "private" plain
    "\n\n%?"
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                       "
# -*- ispell-local-dictionary: \"pt_BR\"; -*-
#+title: ${title}
#+options: tex:t
#+startup: latexpreview
#+filetags: :privado:")
    :unnarrowed t))

(defconst org-roam-recipees
  '("r" "recipees" plain
    "\n\n%?"
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                       "
# -*- ispell-local-dictionary: \"pt_BR\"; -*-
#+title: ${title}
#+filetags: :receitas:")
    :unnarrowed t))

(defun set-org-roam-templates ()
  (setq org-roam-capture-templates
        (list
         default-org-roam-template
         org-roam-math-template
         org-roam-private-template
         org-roam-recipees
         (org-extra-create-language-template-item "b" "brazilian" "pt_BR")
         (org-extra-create-language-template-item "i" "italiano" "it")
         (org-extra-create-language-template-item "n" "nederlands" "dutch")
         (org-extra-create-language-template-item "e" "español" "es")
         (org-extra-create-language-template-item "R" "ruskij" "ru"))))




(use-package company-math
  :after org)

(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory lauremacs-org-roam-files)
  (org-roam-complete-everywhere t)
  
  :init
  (set-org-roam-templates)
  (setq org-roam-v2-ack t)
  (setq org-roam-node-display-template "${title:*}${tags:20}")

  :config
  (org-roam-db-autosync-enable)
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n t" . org-roam-buffer-toggle)
   ("C-c n u" . org-id-get-create)
   ("C-c n f" . lauremacs-tabs-find-org-roam-node)
   ("C-c n I" . org-roam-node-insert)
   ("C-c n i" . org-extra-node-insert-immediate)))

(use-package org-roam-ui
  :after org-roam)

(use-package ox-gfm
  :after org)

(use-package ob-mermaid
  :after org)

(with-eval-after-load "org-num"
  (setq org-num-skip-unnumbered t))

(with-eval-after-load "ox-latex"
  ;; add abnt
  (add-to-list
   'org-latex-classes
   '("abntex2" "\\documentclass[11pt]{abntex2}"
     ("\\section{%s}"       . "\\section*{%s}")
     ("\\subsection{%s}"    . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}"     . "\\paragraph*{%s}")
     ("\\subparagraph{%s}"  . "\\subparagraph*{%s}"))))

;; (use-package ox-beamer
;;   :after org)

(use-package org-download
  :after org
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "./pics")
  (org-download-screenshot-method "screencapture -i %s")
  (org-download-image-org-width 600)
  :init
  (lauremacs-major-mode-leader
    :keymaps 'org-mode-map
    "d"  '(nil                           :which-key "org download")
    "dd" '(org-download-delete           :which-key "delete")
    "ds" '(org-download-screenshot       :which-key "screenshot")
    "dr" '(org-download-rename-at-point  :which-key "rename at point")
    "dR" '(org-download-rename-last-file :which-key "rename last file")
    "de" '(org-download-edit             :which-key "edit")
    "du" '(org-download-image            :which-key "image from url")
    "dy" '(org-download-clipboard        :which-key "paste image from clipboard"))
  :bind
  (("C-c C-d d" . org-download-delete)
   ("C-c C-d s" . org-download-screenshot)
   ("C-c C-d r" . org-download-rename-at-point)
   ("C-c C-d R" . org-download-rename-last-file)
   ("C-c C-d e" . org-download-edit)          
   ("C-c C-d u" . org-download-image)
   ("C-c C-d y" . org-download-yank)))

;;
;; Org math mode
;;

(use-package cdlatex
  :bind
  (("C-;" . (lambda () (interactive)
              (cdlatex-ensure-math)
              (cdlatex-math-symbol))))
  :init
  (setq cdlatex-math-symbol-prefix ?\;)) 

(define-minor-mode org-math-mode
  "Some config to write math on `org-mode'."
  :lighter "org-math-mode"
  (org-fragtog-mode 1)
  (org-cdlatex-mode 1)
  (lauremacs-cdlatex-add-math-symbols))

(defun lauremacs-cdlatex-add-math-symbols ()
  (add-multiple-into-list
   'cdlatex-math-symbol-alist-comb
   '(
     (?.  "\\cdot"   "\\dots")
     (?\; "\\;")
     (?C  ""         "\\mathbb{C}"   "\\arccos")  
     (?N  "\\nabla"  "\\mathbb{N}"   "\\exp")     
     (?Q  "\\Theta"  "\\mathbb{Q}")  
     (?R  "\\Re"     "\\mathbb{R}")  
     (?Z  ""         "\\mathbb{Z}")
     )))

;;
;; table
;;

(use-package valign
  :after org)

;;
;; org cycle
;;
(with-eval-after-load "org-cycle"
  (add-hook 'org-after-sorting-entries-or-items-hook (lambda () (org-cycle-global 2))))

(with-eval-after-load "ox"
  (setq org-export-with-toc nil))

(with-eval-after-load "ox-html"
  (setq org-html-self-link-headlines t))
