(use-dependencies 'go-translate)
(require 'laurisp-core)
(require 'general)

;;
;; Russian keyboard
;;

(require 'quail-russian-qwerty)



;;;###autoload
(defun lauremacs-words-sort-lines ()
  "Call `sort-lines' or `org-sort' depending on the context."
  (interactive)
  (if (eq major-mode 'org-mode)
      (org-sort)
    (sort-lines nil (region-beginning) (region-end))))

(with-eval-after-load "general"
	(lauremacs-leader
		"x"	 '(nil                        :which-key "words")
    "xs" '(lauremacs-words-sort-lines :which-key "sort lines")
		"xC" '(count-words                :which-key "count words")))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Align ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load "general"
	(lauremacs-leader
		"xa"	'(nil                             :which-key "align")
		"xag" '(lauremacs-align-general-sexp    :which-key "align general.el statements")
		"xat" '(lauremacs-align-region-as-table :which-key "align region as table")))

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
        (t (error "Word not found!"))))

(defun lauremacs-words-nl--get-article (noun)
  "Get article from NOUN in `lauremacs-words-nl-nouns'."
  (cdr (assoc noun lauremacs-words-nl-nouns)))

(defun lauremacs-words-nl--request (noun)
  (lauremacs-request-sync-get
   (concat "https://www.ensie.nl/de-of-het/" noun)))

(defun lauremacs-words-nl-het-of-de (noun)
  "Return if a dutch NOUN should be used with het or de article."
  (interactive (list (read-string "Insert noun: " (word-at-point))))
  (unless (lauremacs-words-nl--get-article noun)
    (message "Fetching data from www.ensie.nl...")
    (let ((data (or (lauremacs-words-nl--request noun) "")))
      (message data)
      (add-to-list 'lauremacs-words-nl-nouns
                   (cons noun (lauremacs-words-nl--article data)))))
  (message (lauremacs-words-nl--get-article noun)))


(defun lauremacs-words-coniugazione-at-point ()
  (interactive)
  (get-buffer-create "*coniugazione*")
  (eww (format "https://www.coniugazione.it/verbo/%s.php" (word-at-point))
       ))

(provide 'lauremacs-words)
