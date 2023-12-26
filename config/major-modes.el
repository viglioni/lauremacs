;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; Modes that require few configuration
;;

;;
;; Emacs lisp mode
;;

(require 'eval-sexp-fu)
(add-hook 'emacs-lisp-mode-hook 'eval-sexp-fu-flash-mode)
(require 'laurisp-core)


(add-hook 'emacs-lisp-mode-hook
					'(lambda () (add-multiple-into-list 'prettify-symbols-alist
																				 '(("fp/pipe"		. "▷")
																					 ("fp/partial"	. "∂")
																					 ("fp/compose"	. "○")))))

;;
;; Common lisp mode
;;

(use-package slime
  :init
  (setq inferior-lisp-program "sbcl")
  (lauremacs-major-mode-leader
    :keymaps 'slime-mode-map
    "s"  '(nil                        :which-key "repl")
    "ss" '(slime                      :which-key "open repl")
    "se" '(slime-eval-last-expression :which-key "eval last expression")
    "sb" '(slime-eval-buffer          :which-key "eval buffer")))

(add-hook 'lisp-mode-hook
					'(lambda () (add-multiple-into-list 'prettify-symbols-alist
																				 '(("fp/pipe"		. "▷")
																					 ("fp/partial"	. "∂")
																					 ("fp/compose"	. "○")))))



;;;###autoload
(lauremacs-major-mode-leader
	:keymaps 'emacs-lisp-mode-map
	"c"		'(nil														:which-key "compile")
	"cc"	'(emacs-lisp-byte-compile				:which-key "byte compile")
	"e"		'(nil														:which-key "eval")
	"eb"	'(eval-buffer										:which-key "eval-buffer")
	"er"	'(eval-region										:which-key "eval region")
	"="		'(nil														:which-key "format")
	"=="	'(lauremacs/buffer-indent				:which-key "indent buffer")
	"=r"	'(indent-region									:which-key "indent region")
	"=g"	'(lauremacs-align-general-sexp	:which-key "format codeblocks of general.el functions")
	"=d"	'(checkdoc											:which-key "checkdoc")
	"d"		'(nil														:which-key "documentation")
	"dh"	'(make-header										:which-key "make lib header")
	"du"	'(update-file-header						:which-key "update lib header")
	"dr"	'(make-revision									:which-key "make revision")
	"dd"	'(make-divider									:which-key "make divider")
	"dc"	'(make-box-comment							:which-key "make box comment")
	"dC"	'(make-box-comment-region				:which-key "make box comment region"))


;;
;; yaml
;;

(use-package yaml-mode
	:mode "\\.ya?ml\\'"
	:hook ((yaml-mode . highlight-indentation-mode)
				 (yaml-mode . prettier-js-mode)
         (yaml-mode . visual-line-mode)
         (yaml-mode . lsp-deferred)))


;;
;; Magit
;;

(use-package magit
  :init
  (setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer
         buffer
         (cond ((and (derived-mode-p 'magit-mode)
                     (eq (with-current-buffer buffer major-mode)
                         'magit-status-mode))
                nil)
               ((memq (with-current-buffer buffer major-mode)
                      '(magit-process-mode
                        magit-revision-mode
                        magit-diff-mode
                        magit-stash-mode))
                nil)
               (t
                '(display-buffer-same-window)))))))

(with-eval-after-load "magit"
  (defun lauremacs-gh--get-repo ()
    "Get Organization/Repo."
    (replace-regexp-in-string
     "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
     (magit-get "remote"
                (magit-get-push-remote)
                "url")))

  (defun lauremacs-gh--title ()
    "Get PR title from branch-name."
    (cl-destructuring-bind
        (prefix code first-word &rest description)
        (split-string (magit-get-current-branch) "-")
      (format "[%s-%s] - %s %s"
              (upcase prefix)
              code
              (capitalize first-word)
              (string-join description " "))))

  (defun lauremacs-gh-open-pr ()
    "Visit the current branch's PR on GitHub."
    (interactive)
    (let ((base-url "https://github.com/")
          (repo (lauremacs-gh--get-repo))
          (title (lauremacs-gh--title))
          (branch (magit-get-current-branch)))
      (browse-url
       (concat base-url repo "/compare/" branch
               "?quick_pull=1"
               "&title=" title
               "&assignees=Viglioni")))))

;;
;; Json mode 
;;

(use-package json-mode
	:mode "\\.json\\'"
	:hook ((json-mode . lsp-deferred)
				 (json-mode . prettier-js-mode)
				 (json-mode . highlight-indentation-mode)
				 (json-mode . visual-line-mode))
  :init
  (setq js-indent-level 2))


;;
;; REST Client
;;

(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode))


;;
;; Makefile
;;

(with-eval-after-load "make-mode"
  (add-hook 'makefile-mode-hook 'highlight-indentation-mode)
  (add-hook 'makefile-mode-hook 'indent-tabs-mode))

;;
;; Reading modes
;; epub & pdf
;;

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package pdf-tools
  :init
  (pdf-loader-install))

;;
;; Python / Sage
;;
;; brew install --cask sage
;;

(use-package sage-shell-mode
  :init
  (lauremacs-major-mode-leader
    :keymaps 'sage-shell:sage-mode-map
    "s" '(nil :which-key "repl")
    "sr" '(sage-shell-edit:send-region :which-key "send region")
    "sb" '(sage-shell-edit:send--buffer :which-key "send buffer")))

(use-package helm-sage
  :after '(sage-shell-mode helm))

(use-package ob-sagemath
  :after '(org sage-shell-mode))

;;
;; Mermaid
;;
(use-package mermaid-mode
  :mode "\\.mmd'")
