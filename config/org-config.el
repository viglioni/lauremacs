;;; -*- lexical-binding: t; l-syntax: t -*-
(lauremacs/load config org org-appearance)
(lauremacs/load config org checkbox)
(lauremacs/load config org libs)
(lauremacs/load config org roam)
(lauremacs/load config org babel)

(define-minor-mode org-math-mode
  "Some config to write math on `org-mode'."
  :lighter "org-math-mode")

(defun org-db-ui-insert-batch (table-name db-name data-table)
  "Insert in database DB-NAME in TABLE-NAME from org table DATA-TABLE.
Works with postgres."
  (require 's)
  (let* ((table-cols (string-join (car data-table) ", "))
         (table-rows (cdr data-table))
         (tmp-file   (format "/tmp/%s.csv" table-name)))
    ;; Converts org table to CSV
    (write-region
     (orgtbl-to-csv (cdr orgtable) nil) nil tmp-file)
    ;; Insert data in postgres from CSV file
    (print ;; prints result in org-babel call
     (shell-command-to-string
      (s-lex-format
       "psql -d ${db-name} -c \"COPY ${table-name}(${table-cols}) FROM STDIN WITH delimiter as ',' NULL AS '' csv\" < ${tmp-file}")))))

(cl-defmacro define-org-cmd (&key heading table item)
  "Define a context-aware org command based on cursor position.
If none of the conditions match, falls back to the original key binding.

Supported keys:
:heading -> runs when cursor is over a heading
:table -> runs when cursor is over a table
:item -> runs when cursor is over an item

Example: (define-org-cmd :heading 'my-fn :table 'my-fn2)"
  `(lambda ()
     (interactive)
     (cond
      ,@(when heading `(((org-at-heading-p) (funcall ,heading))))
      ,@(when table `(((org-at-table-p) (funcall ,table))))
      ,@(when item `(((org-at-item-p) (funcall ,item))))
      (t
       ;; Fallback to the original binding from parent keymaps
       (let* ((key (this-command-keys-vector))
              (local-map (current-local-map))
              ;; Look in parent keymaps, not the current local map where we just bound this command
              (original-cmd (when (keymapp local-map)
                              (or (lookup-key (keymap-parent local-map) key t)
                                  (lookup-key (current-global-map) key t)))))
         (when (and original-cmd
                    (not (numberp original-cmd)) ; not a prefix key
                    (commandp original-cmd))
           (call-interactively original-cmd)))))))


(config-package org
  :hook '((org-mode . lauremacs/org-mode-setup)
          (org-mode . hl-line-mode)
          (org-mode . prettify-symbols-mode)
          ;; (org-mode . '(lambda () (add-multiple-into-list 'prettify-symbols-alist
					;; 																					      '((">=" . "≥")
					;; 																						      ("<=" . "≤")
					;; 																						      ("!=" . "≠")
          ;;                                                   ("=>" . "⇒")
          ;;                                                   ("<=" . "⇐")
          ;;                                                   ("->" . "→")
          ;;                                                   ("<-" . "←")))))
          )
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
  (org-link-frame-setup '((file . find-file))) ; Open in same buffer instead of other window
  :init
	
	;; ;; keymaps
  ;; (general-define-key
  ;;  :keymaps 'org-mode-map
  ;;  "C-M-<return>" '(org-insert-todo-heading :general "insert todo heading")
  ;;  "M-s-m" (define-org-cmd
  ;;           :heading 'org-promote-subtree
  ;;           :table   'org-table-move-column-left)
	;;  "M-s-," (define-org-cmd
	;; 					:heading 'org-move-subtree-down
	;; 					:item    'org-move-item-down
  ;;           :table   'org-table-move-row-down)
  ;;  "M-s-. " (define-org-cmd
	;; 					:heading 'org-move-subtree-up
	;; 					:item    'org-move-item-up
  ;;           :table   'org-table-move-row-up)
  ;;  "M-s-/" (define-org-cmd
  ;;           :heading 'org-demote-subtree
  ;;           :table   'org-table-move-column-right)
  ;;  "s-d" 'org-table-copy-down)
	
	(lauremacs-major-mode-leader
		:keymaps 'org-mode-map
		;; "T"		'(nil                              :which-key "toggle")
		;; "Tf"	'(org-fragtog-mode                 :which-key "toggle fragtog mode")
		"i"   '(nil                              :which-key "insert")
		"ic"  '(lauremacs/org-insert-source :which-key "insert code block source")
		;; "im"  '(nil                              :which-key "insert math")
		;; "imb" '(org-insert-mathbb                :which-key "insert mathbb")
		;; "imc" '(org-insert-mathcal               :which-key "insert mathcal")
		"l"   '(nil                              :which-key "LaTeX")
		"le"  '(nil                              :which-key "export")
		"lep" '(org-compile-to-pdf               :which-key "export to pdf")
		;; "p"		'(nil                              :which-key "preview")
		;; "pl"  '(nil                              :which-key "preview LaTeX")
		;; "plb" '(org-preview-latex-on-buffer      :which-key "preview LaTeX on buffer")
		;; "plc" '(org-clear-latex-preview          :which-key "clear LaTeX preview")
		;; "plt" '(org-latex-preview                :which-key "toggle LaTeX preview at point")
		;; "x"   '(nil                              :which-key "word")
		;; "xb"  '(org-add-bold-to-region           :which-key "bold")
		;; "xc"  '(org-add-code-to-region           :which-key "code")
		;; "xi"  '(org-add-italic-to-region         :which-key "italic")
		;; "xs"  '(org-add-strikethrough-to-region  :which-key "strikethrough")
		;; "xu"	'(org-add-underline-to-region      :which-key "underline")
		;; "xv"  '(org-add-verbatin-to-region       :which-key "verbatin")
    ;; "r"   '(org-extra-recalc-buffer-and-save :which-key "recalc buff and save")
    "t"   '(nil                              :which-key "table")
    "ti"  '(nil                              :which-key "insert")
    "tic" '(org-table-insert-column          :which-key "insert column")
    "tih" '(org-table-insert-hline           :which-key "insert horizontal line")
    "tim" '(org-table-hline-and-move)        :which-key "insert hline and move"
    "tir" '(org-table-insert-row             :which-key "insert row")
    "td"  '(nil                              :which-key "delete")
    "tdr" '(org-table-kill-row               :which-key "delete row")
    "tdc" '(org-table-delete-column          :which-key "delete column"))

	;; ;; LaTeX
	;; (sp-local-pair 'org-mode "$" "$" )
  ;; (exec-path-when-cmd-not-found "latex")
	;; (setq-local company-backends
  ;;             (append '((company-math-symbols-latex company-latex-commands))
  ;;                     company-backends))
	
  :bind
  (:map org-mode-map
        (("C-c C-b" . org-mark-ring-goto)
         ("C-c c"   . org-table-blank-field)))
  )
