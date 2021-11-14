;;; lauremacs-icons.el --- extra functions to define all-the-icons icons
;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;


;;; Commentary:
;; 

;;; Code:

(defvar lauremacs/all-icons nil
	"All icons in the package `all-the-icons'.")


;;
;; Custom icons functions
;;

;;;###autoload
(defun lauremacs//icons-add-package-name (package-name icon)
	"ICON is a (extension-name . icon-value).
PACKAGE-NAME is a string e.g. \"alltheicons\".
Return (extension-name icon-value package-name)"
	(let ((extension-name (car icon))
				(icon-value (cdr icon)))
		(list extension-name icon-value package-name)))

;;;###autoload
(defun lauremacs//icons-define-all-icons ()
	"Define the variable `lauremacs/all-icons'."
	(setq lauremacs/all-icons
				(append
				 (mapcar (fp/curry lauremacs//icons-add-package-name "faicon")
								 (all-the-icons-faicon-data))
				 (mapcar (fp/curry lauremacs//icons-add-package-name "alltheicon")
								 (all-the-icons-alltheicon-data))
				 (mapcar (fp/curry lauremacs//icons-add-package-name "fileicon")
								 (all-the-icons-fileicon-data))
				 (mapcar (fp/curry lauremacs//icons-add-package-name "oction")
								 (all-the-icons-octicon-data))
				 (mapcar (fp/curry lauremacs//icons-add-package-name "wicon")
								 (all-the-icons-wicon-data))
				 (mapcar (fp/curry lauremacs//icons-add-package-name "material")
								 (all-the-icons-material-data)))))

;;;###autoload
(defun lauremacs/icons-filter-by-regex (regex)
	"Find all icons in `all-the-icons' package that match REGEX."
	(seq-filter (lambda (icon) (string-match-p regex (car icon)))
							lauremacs/all-icons))

;;;###autoload
(defmacro remove-from-alist-if-exists (key alist)
	"Remove elements from ALIST matching KEY.
E.g.: (setq myalist '((1 . 2) (1 . 3) (4 . 5))).
\(remove-from-alist-if-exists 1 myalist) will change myalist to:
'((4 . 5))."
	`(let ((key-exists-in-alist? (bool (assoc ,key ,alist))))
		 (when key-exists-in-alist?
			 (setq ,alist (assoc-delete-all ,key , alist)))))

;;;###autoload
(defun lauremacs/icons-add-icon-to-alist (icon-list-symbol icon-definition)
	"Add icon definition to:
`all-the-icons-extension-icon-alist' or `all-the-icons-regexp-icon-alist'.
ICON-LIST-SYMBOL should be either:
`all-the-icons-extension-icon-alist' or `all-the-icons-regexp-icon-alist'.
ICON-DEFINITION is a list e.g. '(\"tsx\" all-the-icons-fileicon \"tsx\" :height 1.0 :v-adjust -0.1 :face all-the-icons-blue-alt)."
	(throw-unless (contains? '(all-the-icons-extension-icon-alist all-the-icons-regexp-icon-alist) icon-list-symbol)
								"Invalid icon-list-symbol.")
	(let* ((icon-list (eval icon-list-symbol))
				 (extension-name (car icon-definition)))
		(remove-from-alist-if-exists extension-name icon-list)
		(add-to-list icon-list-symbol icon-definition)))

(defvar lauremacs/icons-ts-default-config
	'(:height 1.0 :v-adjust 0.0 :face all-the-icons-blue)
	"Default icon config for typescript.")

;;;###autoload
(defun lauremacs/icons-set-icon (icon-config icon-alist extension-or-regex icon-finder icon-name)
	"ICON-CONFIG should be a list with `:height', `:v-adjust' and `:face'.
ICON-ALIST should be either:
`all-the-icons-regexp-icon-alist' or `all-the-icons-extension-icon-alist'.
EXTENSION-OR-REGEX is file extension or a filename regex to be matched.
e.g. \"tsx\" or \"-?spec\\.tsx$\".
ICON-FINDER should be an \"all-the-icons-<package-name>\" function like:
`all-the-icons-fileicon'.
ICON-NAME the name of the icon, e.g. \"test-react\"."
	(lauremacs/icons-add-icon-to-alist
	 icon-alist
	 (append  (list extension-or-regex icon-finder icon-name) icon-config)))

(defun lauremacs/icons-set-ts-mode-icon (&rest args)
	"Check `lauremacs/icons-set-icon' doc for ARGS."
	(apply
	 (fp/curry lauremacs/icons-set-icon lauremacs/icons-ts-default-config 'all-the-icons-extension-icon-alist)
	 args))

(defun lauremacs/icons-set-ts-regex-icon (&rest args)
	"Check `lauremacs/icons-set-icon' doc for ARGS."
	(apply
	 (fp/curry lauremacs/icons-set-icon lauremacs/icons-ts-default-config 'all-the-icons-regexp-icon-alist)
	 args))

(provide 'lauremacs-icons)

;;; lauremacs-icons.el ends here
