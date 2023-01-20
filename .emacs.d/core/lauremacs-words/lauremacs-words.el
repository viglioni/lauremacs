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

(use-package go-translate
  :init
  (setq gts-translate-list '())
  (setq gts-default-translator nil))

(defun lauremacs-translate (from to)
  "Translate words FROM language TO language."
  (let ((gts-translate-list  (list (list from to)))
				(gts-default-translator
				 (gts-translator
					:picker (gts-prompt-picker :single t)
					:engines (list (gts-bing-engine) (gts-google-engine))
					:render (gts-buffer-render))))
    (print gts-translate-list)
		(gts-translate gts-default-translator)))

(defun lauremacs-translate/pt->en ()
	"Translate words from pt to en."
	(interactive)
	(lauremacs-translate "pt" "en"))


(defun lauremacs-translate/en->pt ()
	"Translate "
	(interactive)
	(lauremacs-translate "pt" "en"))

(defun lauremacs-translate/en->it ()
	"Translate from english to italian."
	(interactive)
  (lauremacs-translate "en" "it"))

(defun lauremacs-translate/it->en ()
	"Translate from italian to english."
	(interactive)
  (lauremacs-translate "it" "en"))

(defun lauremacs-translate/en->nl ()
	"Translate from english to dutch."
	(interactive)
  (lauremacs-translate "en" "nl"))

(defun lauremacs-translate/nl->en ()
	"Translate from dutch to english."
	(interactive)
  (lauremacs-translate "nl" "en"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Align ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Word cases ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Dutch functions ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar lauremacs-words-nl-nouns nil)

(defun lauremacs-words-nl--is-de (data)
  (string-match "<h4 class=\"mb-4\" style=\"color: #1b85bc;\">De " data))

(defun lauremacs-words-nl--is-het (data)
  (string-match "<h4 class=\"mb-4\" style=\"color: #1b85bc;\">Het " data))

(defun lauremacs-words-nl--article (data)
  "Get article from DATA."
  (cond ((lauremacs-words-nl--is-de  data) "de")
        ((lauremacs-words-nl--is-het data) "het")
        (t (error "word not found!"))))

(defun lauremacs-words-nl--get-article (noun)
  "Get article from NOUN in `lauremacs-words-nl-nouns'."
  (cdr (assoc noun lauremacs-words-nl-nouns)))

(defun lauremacs-words-nl--request (noun)
  (lauremacs-request-sync-get
   (concat "https://www.ensie.nl/de-of-het/" noun)))

(defun lauremacs-words-nl-het-of-de (noun)
  "Return if a dutch NOUN should be used with het or de article."
  (interactive "sInsert noun: ")
  (unless (lauremacs-words-nl--get-article noun)
    (message "Fetching data from www.ensie.nl...")
    (let ((data (or (lauremacs-words-nl--request noun) "asd")))
      (message data)
      (add-to-list 'lauremacs-words-nl-nouns
                   (cons noun (lauremacs-words-nl--article data)))))
  (message (lauremacs-words-nl--get-article noun)))


(provide 'lauremacs-words)
