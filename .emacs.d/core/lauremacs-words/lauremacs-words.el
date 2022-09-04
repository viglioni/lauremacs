(use-dependencies 'go-translate)
(require 'laurisp-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; align ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; translate ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lauremacs-translate/pt->en ()
	"Translate words from pt to en."
	(interactive)
	(let ((gts-translate-list '(("pt" . "en")))
				(gts-default-translator
				 (gts-translator
					:picker (gts-prompt-picker :single t)
					:engines (list (gts-bing-engine) (gts-google-engine))
					:render (gts-buffer-render))))
		(gts-translate gts-default-translator)))


(defun lauremacs-translate/en->pt ()
	"Translate "
	(interactive)
	(let ((gts-translate-list '(("en" . "pt")))
				(gts-default-translator
				 (gts-translator
					:picker (gts-prompt-picker :single t)
					:engines (list (gts-bing-engine) (gts-google-engine))
					:render (gts-buffer-render))))
		(gts-translate gts-default-translator)))



(with-eval-after-load "general"
	(lauremacs-leader
		"x"		'(nil															:which-key "words")
		"xa"	'(nil															:which-key "align")
		"xag" '(lauremacs-align-general-sexp		:which-key "align general.el statements")
		"xat" '(lauremacs-align-region-as-table :which-key "align region as table")
		"xt"	'(nil															:which-key "translate")
		"xtt" '(lauremacs-translate/en->pt			:which-key "translate en -> pt")
		"xte" '(lauremacs-translate/pt->en			:which-key "translate pt -> en")
		"xc"	'(count-words											:which-key "count words")))
