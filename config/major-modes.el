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
(require 'header2)


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
				 (yaml-mode . prettier-js-mode)))


;;
;; Magit
;;

(use-package magit
  :init
  (lauremacs-leader
    "gs" '(magit-status :which-key "magit status")))

;;
;; Json mode 
;;

(use-package json-mode
	:mode "\\.json\\'"
	:hook ((json-mode . lsp-deferred)
				 (json-mode . highlight-indentation-mode)
				 (json-mode . visual-line-mode)))


;;
;; REST Client
;;

(use-package restclient)


