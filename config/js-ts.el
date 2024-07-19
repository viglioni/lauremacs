;; ;;
;; ;; @author Laura Viglioni
;; ;; 2021
;; ;; GNU Public License 3.0
;; ;;

(require 'projectile)
(require 'nvm)

(defvar previous-major-mode nil)
(make-variable-buffer-local 'previous-major-mode)
(put 'previous-major-mode 'permanent-local t)

;;;###autoload
(defun lauremacs/ts-load-web-mode ()
	(interactive)
	"Load `web-mode' and its definitions."
	(unless (equal previous-major-mode 'web-mode)
		(progn
			(setq previous-major-mode 'web-mode)
			(funcall 'web-mode)			
			(funcall 'tsx-mode))))

(defun lauremacs/tsx-load-web-mode (fn &rest args)
	"Run FN with ARGS in `web-mode' then return to `tsx-mode'."
	(when (equal major-mode 'tsx-mode)
		(web-mode)
		(apply fn args)
		(tsx-mode)))

(defun lauremacs/jest-test-this-file ()
  (interactive)
	(let ((compilation-read-command nil))
		(projectile--run-project-cmd
		 (concat "npm test -- " (buffer-file-name))
		 projectile-test-cmd-map
		 :show-prompt nil
		 :prompt-prefix "Test command: "
		 :save-buffers t
		 :use-comint-mode projectile-test-use-comint-mode)))

(defun lauremacs-ide--ts-mode? ()
  (and (contains? '(typescript-mode tsx-mode js-mode jsx-mode) major-mode)
       (boundp 'lsp-mode)
       lsp-mode))

(defun lauremacs-ide-ts-autofix-before-save ()
  (when (lauremacs-ide--ts-mode?)             
    (lsp-eslint-apply-all-fixes)))

;(setq lsp-eslint-server-command '("vscode-eslint-language-server" "--stdio"))

(add-hook 'before-save-hook 'lauremacs-ide-ts-auto-fix-before-save)


(with-eval-after-load "lsp-mode"
  (setq lsp-typescript-preferences-import-module-specifier "relative"))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook ((typescript-mode . lsp-deferred)
				 (typescript-mode . prettier-js-mode)
         (typescript-mode . (lambda ()
                              (exec-path-when-cmd-not-found "node")
                              (exec-path-when-cmd-not-found "prettier")))
				 (tsx-mode        . lsp-deferred)
				 (tsx-mode        . prettier-js-mode)
         (typescript-mode . (lambda () (add-multiple-into-list 'prettify-symbols-alist
																											    '((">="  . "≥")
																												    ("<="  . "≤")
																												    ("!==" . "≠")
                                                            ("=>"  . "⇒"))))))
  :custom
  (typescript-indent-level 2)
	:init
	(require 'web-mode)
	(require 'web-minor-mode)
	(require 'nvm)
	(define-derived-mode tsx-mode typescript-mode "tsx")
	(add-hook 'tsx-mode #'subword-mode)
	(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-mode))
  )

(use-package tree-sitter
  :ensure t
  :hook ((tsx-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(tsx-mode . tsx)))



(bind-lazy-function 'lsp-rename-ts-file
										'lauremacs-ide-lsp-ts-rename-file
 										'lauremacs-ide-extra)

(bind-lazy-function 'send-buffer-to-repl
										'ts-repl-exec-ts-buffer
										'ts-repl)

(bind-lazy-function 'send-last-sexp-to-repl
										'ts-repl-send-last-sexp
										'ts-repl)

(bind-lazy-function 'open-ramda-docs
										'ramda-docs-open
										'ramda-docs)

(bind-lazy-function 'open-ramda-docs-eww
										'ramda-docs-open-eww
										'ramda-docs)


(lauremacs-major-mode-leader
	:keymaps '(typescript-mode-map web-mode-map)
  "rf"     '(lauremacs-ide-lsp-ts-rename-file :which-key "rename file")
	"s"      '(nil                              :which-key "ts-repl")
	"sb"     '(send-buffer-to-repl              :which-key "send buffer to repl")
  "ra"     '(lsp-eslint-apply-all-fixes               :which-key "eslint fix all errors")
	"se"     '(send-last-sexp-to-repl           :which-key "send last sexp to repl")
  "tj"     '(lauremacs/jest-test-this-file    :which-key "test this file (jest)")
	"d"      '(nil                              :which-key "docs")
	"dr"     '(nil                              :which-key "ramda")
	"drr"    '(open-ramda-docs-eww              :which-key "open ramda docs inside emacs")
	"drw"    '(open-ramda-docs                  :which-key "open ramda docs"))

(use-package prettier-js
  :after (typescript-mode))

(use-package web-mode
	:mode ("\\.html\\'" ;; "\\.tsx\\'"
         )
  :hook ((web-mode . prettier-js-mode) (web-mode . lsp-deferred))
  :init
  (lauremacs-major-mode-leader
    :keymaps '(web-mode-map tsx-mode-map)
    "i"  '(nil :which-key "html element")
    "ii" '(web-mode-element-insert-at-point :which-key "element insert at point")
    "iv" '(web-mode-element-vanish          :which-key "element vanish")
    "ik" '(web-mode-element-kill            :which-key "element kill")
    "is" '(web-mode-element-select          :which-key "element select")
    "iw" '(web-mode-element-wrap            :which-key "element wrap")
    "ir" '(web-mode-element-rename          :which-key "element rename")
    "ic" '(web-mode-element-clone           :which-key "element clone")
    "i/" '(web-mode-element-close           :which-key "element close")
    "ib" '(web-mode-element-beginning       :which-key "element beginning")
    "ie" '(web-mode-element-end             :which-key "element end")
		"if" '(web-mode-fold-or-unfold          :which-key "fold/unfold element"))

  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(with-eval-after-load "eglot"
  (add-to-list 'eglot-server-programs '(web-mode . ("typescript-language-server" "--stdio"))))

(with-eval-after-load 'lsp
  (setf (alist-get 'web-mode lsp--formatting-indent-alist) 'web-mode-code-indent-offset))



