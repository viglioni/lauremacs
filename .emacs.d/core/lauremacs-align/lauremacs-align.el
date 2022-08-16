(require 'laurisp-core)

;;;###autoload
(defun lauremacs-align-general-sexp ()
	"Align binding functions from `general.el'.
Should be called when pointer is inside the function."
	(interactive)
	(let ((preffix "\\(\\s-*\\)"))
		(unless (region-active-p) (mark-paragraph))
		(align-regexp (region-beginning) (region-end) (concat preffix "'("))
		(align-regexp (region-beginning) (region-end) (concat preffix ":which-key"))))

;;;###autoload
(defun lauremacs-align-region-as-table ()
	"Align region at every space character."
	(interactive)
	(throw-unless (region-active-p) "Should be called only when a region is marked!")
	(let ((preffix "\\(\\s-*\\)"))
		(align-regexp (region-beginning) (region-end) (concat preffix " ")
									0  align-default-spacing t)))

(provide 'lauremacs-align)
