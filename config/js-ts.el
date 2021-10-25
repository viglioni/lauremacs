;;;###autoload
(defun use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint
          (and root
               (expand-file-name "node_modules/.bin/eslint"
                                 root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

;; (add-to-multiple-hooks
;;  'use-eslint-from-node-modules
;;  'typescript-mode-hook
;;  'json-mode-hook
;;  'web-mode-hook)

(use-package json-mode
	:mode "\\.json\\'"
	:hook ((json-mode . lsp-deferred)))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook ((typescript-mode . lsp-deferred)
				 (typescript-mode . prettier-js-mode))
  :custom
  (typescript-indent-level 2))

(use-package web-mode
  :mode "\\.tsx\\'"
  :hook ((web-mode . lsp-deferred)
				 (web-mode . prettier-js-mode))
  :init
  (lauremacs-major-mode-leader
    :keymaps 'web-mode-map
    "i" '(nil :which-key "html element")
    "ii" '(web-mode-element-insert-at-point :which-key "element insert at point")
    "iv" '(web-mode-element-vanish :which-key "element vanish")
    "ik" '(web-mode-element-kill :which-key "element kill")
    "is" '(web-mode-element-select :which-key "element select")
    "iw" '(web-mode-element-wrap :which-key "element wrap")
    "ir" '(web-mode-element-rename :which-key "element rename")
    "ic" '(web-mode-element-clone :which-key "element clone")
    "i/" '(web-mode-element-close :which-key "element close")
    "ib" '(web-mode-element-beginning :which-key "element beginning")
    "ie" '(web-mode-element-end :which-key "element end")
    )
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(bind-lazy-function 'lsp-rename-ts-file
										'lauremacs-ide-lsp-ts-rename-file
										'lauremacs-ide-extra)

(lauremacs-major-mode-leader
	:keymaps '(typescript-mode-map web-mode)
	"rf" '(lsp-rename-ts-file :which-key "rename file"))

(use-package prettier-js
  :after (typescript-mode))
