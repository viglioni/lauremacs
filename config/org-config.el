(lauremacs/load "config/org/org-appearance")
(lauremacs/load "config/org/checkbox")
(lauremacs/load "config/org/libs")
(lauremacs/load "config/org/roam")


(define-minor-mode org-math-mode
  "Some config to write math on `org-mode'."
  :lighter "org-math-mode")

;; (config-package org
;;   :hook '((org-mode . lauremacs/org-mode-setup)
;;           (org-mode . hl-line-mode)
;;           (org-mode . prettify-symbols-mode)
;;           ;; (org-mode . '(lambda () (add-multiple-into-list 'prettify-symbols-alist
;; 					;; 																					      '((">=" . "≥")
;; 					;; 																						      ("<=" . "≤")
;; 					;; 																						      ("!=" . "≠")
;;           ;;                                                   ("=>" . "⇒")
;;           ;;                                                   ("<=" . "⇐")
;;           ;;                                                   ("->" . "→")
;;           ;;                                                   ("<-" . "←")))))
;;           )
;; 	:custom
;; 	(org-hide-emphasis-markers t)
;; 	(org-startup-folded t)
;;   (org-startup-with-latex-preview nil)
;; 	(haskell-process-type 'stack-ghci)
;; 	(org-highlight-latex-and-related '(latex script entities))
;; 	(org-image-actual-width nil)
;;   (org-startup-with-inline-images t)
;;   (org-export-use-babel nil)
;;   (org-html-preamble nil)
;;   (org-html-postamble nil)

;;   :init
	
;; 	;; ;; keymaps
;;   ;; (general-define-key
;;   ;;  :keymaps 'org-mode-map
;;   ;;  "C-M-<return>" '(org-insert-todo-heading :general "insert todo heading")
;;   ;;  "M-s-m" (define-org-cmd
;;   ;;           :heading 'org-promote-subtree
;;   ;;           :table   'org-table-move-column-left)
;; 	;;  "M-s-," (define-org-cmd
;; 	;; 					:heading 'org-move-subtree-down
;; 	;; 					:item    'org-move-item-down
;;   ;;           :table   'org-table-move-row-down)
;;   ;;  "M-s-. " (define-org-cmd
;; 	;; 					:heading 'org-move-subtree-up
;; 	;; 					:item    'org-move-item-up
;;   ;;           :table   'org-table-move-row-up)
;;   ;;  "M-s-/" (define-org-cmd
;;   ;;           :heading 'org-demote-subtree
;;   ;;           :table   'org-table-move-column-right)
;;   ;;  "s-d" 'org-table-copy-down)
	
;; 	(lauremacs-major-mode-leader
;; 		:keymaps 'org-mode-map
;; 		;; "T"		'(nil                              :which-key "toggle")
;; 		;; "Tf"	'(org-fragtog-mode                 :which-key "toggle fragtog mode")
;; 		"i"   '(nil                              :which-key "insert")
;; 		;; "ic"  '(org-insert-src                   :which-key "insert code block source")
;; 		;; "im"  '(nil                              :which-key "insert math")
;; 		;; "imb" '(org-insert-mathbb                :which-key "insert mathbb")
;; 		;; "imc" '(org-insert-mathcal               :which-key "insert mathcal")
;; 		"l"   '(nil                              :which-key "LaTeX")
;; 		"le"  '(nil                              :which-key "export")
;; 		"lep" '(org-compile-to-pdf               :which-key "export to pdf")
;; 		;; "p"		'(nil                              :which-key "preview")
;; 		;; "pl"  '(nil                              :which-key "preview LaTeX")
;; 		;; "plb" '(org-preview-latex-on-buffer      :which-key "preview LaTeX on buffer")
;; 		;; "plc" '(org-clear-latex-preview          :which-key "clear LaTeX preview")
;; 		;; "plt" '(org-latex-preview                :which-key "toggle LaTeX preview at point")
;; 		;; "x"   '(nil                              :which-key "word")
;; 		;; "xb"  '(org-add-bold-to-region           :which-key "bold")
;; 		;; "xc"  '(org-add-code-to-region           :which-key "code")
;; 		;; "xi"  '(org-add-italic-to-region         :which-key "italic")
;; 		;; "xs"  '(org-add-strikethrough-to-region  :which-key "strikethrough")
;; 		;; "xu"	'(org-add-underline-to-region      :which-key "underline")
;; 		;; "xv"  '(org-add-verbatin-to-region       :which-key "verbatin")
;;     ;; "r"   '(org-extra-recalc-buffer-and-save :which-key "recalc buff and save")
;;     "t"   '(nil                              :which-key "table")
;;     "ti"  '(nil                              :which-key "insert")
;;     "tic" '(org-table-insert-column          :which-key "insert column")
;;     "tih" '(org-table-insert-hline           :which-key "insert horizontal line")
;;     "tim" '(org-table-hline-and-move)        :which-key "insert hline and move"
;;     "tir" '(org-table-insert-row             :which-key "insert row")
;;     "td"  '(nil                              :which-key "delete")
;;     "tdr" '(org-table-kill-row               :which-key "delete row")
;;     "tdc" '(org-table-delete-column          :which-key "delete column"))

;; 	;; ;; LaTeX
;; 	;; (sp-local-pair 'org-mode "$" "$" )
;;   ;; (exec-path-when-cmd-not-found "latex")
;; 	;; (setq-local company-backends
;;   ;;             (append '((company-math-symbols-latex company-latex-commands))
;;   ;;                     company-backends))
	
;;   ;; (require 'ob-ts)
;;   ;; (require 'ob-elixir)
;; 	;; (org-babel-do-load-languages
;;   ;;  'org-babel-load-languages
;;   ;;  '((elixir		 . t)
;;   ;;    (sqlite     . t)
;; 	;; 	 (haskell		 . t)
;;   ;;    (mermaid    . t)
;;   ;;    (clojure		 . t)
;;   ;;    (emacs-lisp . t)
;; 	;; 	 (ts         . t)
;;   ;;    (python		 . t)
;;   ;;    (js				 . t)
;;   ;;    (C					 . t)
;;   ;;    (latex			 . t)
;;   ;;    (restclient . t)
;;   ;;    (sagemath   . t)
;;   ;;    (shell      . t)
;;   ;;    (sql        . t)))

;;   ;; (setq org-babel-default-header-args:sage '((:session . t)
;;   ;;                                            (:results . "output")))
;;   :bind
;;   (:map org-mode-map
;;         (("C-c C-b" . org-mark-ring-goto)
;;          ("C-c c"   . org-table-blank-field)))
;;   )
