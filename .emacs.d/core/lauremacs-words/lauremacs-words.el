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
  (save-excursion
	  (let ((preffix "\\(\\s-*\\)"))
      (search-backward-regexp (rx (| "(lauremacs-leader"
                                     "(lauremacs-major-mode-leader"
                                     "(general-define-key")))
      (er/expand-region 1)
      (align-regexp (region-beginning)
                    (region-end)
                    (concat preffix (rx (| "'(" "(list (fp/const-fn-interactive"))))
		  (align-regexp (region-beginning)
                    (region-end)
                    (concat preffix ":which-key"))
      (deactivate-mark))))

;;;###autoload
(defun lauremacs-align-region-as-table (&optional regexp)
	"Align region at every REGEXP character."
	(interactive "sEnter regexp: ")
	(throw-unless (region-active-p) "Should be called only when a region is marked!")
	(let ((preffix "\\(\\s-*\\)")
				(regex (or regexp " ")))
		(align-regexp (region-beginning) (region-end) (concat preffix regex)
									1  align-default-spacing t)))


(defun LauTeX-align-table ()
	(interactive)
	(let ((beg (region-beginning))
				(end (region-end)))
		(lauremacs-align-region-as-table "&")
		(lauremacs-align-region-as-table "\\\\\\\\")))

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
		"x"		'(nil                             :which-key "words")
		"xa"	'(nil                             :which-key "align")
		"xag" '(lauremacs-align-general-sexp    :which-key "align general.el statements")
		"xat" '(lauremacs-align-region-as-table :which-key "align region as table")
		"xt"	'(nil                             :which-key "translate")
		"xtt" '(lauremacs-translate/en->pt      :which-key "translate en -> pt")
		"xte" '(lauremacs-translate/pt->en      :which-key "translate pt -> en")
		"xC"	'(count-words                     :which-key "count words")))

(use-package string-inflection
  :ensure t
  :init
  (lauremacs-leader
    "xc"  '(nil                               :which-key "convert word case")
    "xcs" '(string-inflection-underscore      :which-key "convert to snake-case")
    "xcc" '(string-inflection-lower-camelcase :which-key "convert to camelCase")
    "xcp" '(string-inflection-camelcase       :which-key "convert to PascalCase")
    "xck" '(string-inflection-kebab-case      :which-key "convert to kebab-case")
    "xcu" '(string-inflection-upcase          :which-key "convert to UPPER_CASE")))

(provide 'lauremacs-words)
