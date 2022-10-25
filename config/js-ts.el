;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

(require 'projectile)

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

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook ((typescript-mode . lsp-deferred)
				 (typescript-mode . prettier-js-mode)
				 (tsx-mode        . lsp-deferred)
				 (tsx-mode        . prettier-js-mode)
				 (tsx-mode        . lauremeacs/ts-load-web-mode))
  :custom
  (typescript-indent-level 2)
	:init
	(require 'web-mode)
	(require 'web-minor-mode)
	(require 'nvm)
	(define-derived-mode tsx-mode typescript-mode "tsx")
	(add-hook 'tsx-mode #'subword-mode)
	(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-mode)))

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
	"se"     '(send-last-sexp-to-repl           :which-key "send last sexp to repl")
  "t"      '(nil                              :which-key "test")
  "tj"     '(lauremacs/jest-test-this-file    :which-key "test this file (jest)")
	"d"      '(nil                              :which-key "docs")
	"dr"     '(nil                              :which-key "ramda")
	"drr"    '(open-ramda-docs-eww              :which-key "open ramda docs inside emacs")
	"drw"    '(open-ramda-docs                  :which-key "open ramda docs"))

(use-package prettier-js
  :after (typescript-mode))

(use-package web-mode
	:mode "\\.html\\'"
  :hook ((web-mode . prettier-js-mode))
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

	(dolist (fn '(web-mode-element-insert-at-point
								web-mode-element-vanish
								web-mode-element-kill
								web-mode-element-select
								web-mode-element-wrap
								web-mode-element-rename
								web-mode-element-clone
								web-mode-element-close
								web-mode-element-beginning
								web-mode-element-end
								web-mode-fold-or-unfold))
		(advice-add fn :around 'lauremacs/tsx-load-web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

