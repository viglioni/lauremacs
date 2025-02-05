;;
;; @author Laura Viglioni
;; 2021~2022
;; GNU Public License 3.0
;;

;;
;; org-mode extra configs
;;

(load "./org-headers-skeleton.el")
(require 'helm)
(require 'functional)
(require 'ispell)

;;
;; LaTeX functions
;;

;;;###autoload
(defun LauTex-clear (&optional dir)
	"Clear LaTeX compile files from DIR."
	(interactive)
	(let ((directory (or dir default-directory)))
		(shell-command-to-string "rm *.aux *.log")))

;;;###autoload
(defun LauTex-compile-org-to-pdf ()
	"Compile org file to pdf."
  (interactive)
  (if (and (boundp 'org-beamer-mode) org-beamer-mode)
      (org-beamer-export-to-pdf)
    (org-latex-export-to-pdf)))

;;;###autoload
(defun LauTex-define-preview-settings (&optional img-scale)
  "Define latex format options using the theme."
  (interactive)
  (let* ((foreground-color (face-attribute 'default :foreground))
         (background-color (face-attribute 'default :background))
         (text-scale (float (if (boundp 'text-scale-mode-amount) text-scale-mode-amount 0)))
         (minimum-scale (or img-scale 13))
         (scale (/  (float (+ minimum-scale text-scale)) 10)))
    (plist-put org-format-latex-options :scale scale)
    (plist-put org-format-latex-options :foreground foreground-color)
    (plist-put org-format-latex-options :background background-color)))

;;;###autoload
(defun LauTeX-preview-latex-on-buffer ()
	"Preview all LaTeX math formulas on buffer."
  (interactive)
  (LauTex-define-preview-settings)
  (org-clear-latex-preview)
  (org-latex-preview '(16)))

;;;###autoload
(defun LauTex-insert-math-cmd (latex-cmd args)
	"Insert \mathbb{text}extra-text, where ARGS is '(text . extra-text).
Or args is just text."
	(let ((inside-text   (if (listp args) (car args) args))
				(external-text (if (listp args) (cdr args) "")))
		(insert (format "\\%s{%s}%s" latex-cmd inside-text external-text))))



(defun LauTex-helm-insert-cmd (latex-cmd candidates)
	"Return interactive function that call `LauTex-insert-math-cmd' using LATEX-CMD and CANDIDATES."
	(helm
	 :prompt "Choose/insert text: "
	 :sources (helm-build-sync-source "sets"
							:candidates candidates
							:action (fp/curry-deprecated 'LauTex-insert-math-cmd latex-cmd))))


(defun LauTex-insert-mathbb ()
	"Insert \mathbb{text} for a given option list."
	(interactive)
	(LauTex-helm-insert-cmd "mathbb"
													'(("natural"  . "N")
														("integers" . "Z")
														("rational" . "Q")
														("real"     . "R")
														("complex"  . "C")
														("primes"   . "P")
														("R^n"      . ("R" . "^n")))))


(defun LauTex-insert-mathcal ()
	"Insert \mathcal{text} for a given option list."
	(interactive)
	(LauTex-helm-insert-cmd "mathcal"
													'(("ring of integers" . "O")
														("O k"              . ("O" . "_K"))
														("ideal"            . "I")
														("canonical basis"  . "C"))))


;;
;; Org mode writing
;;

(defconst org-extra--emphasis-list
	'((bold . "*") (italic . "/") (code . "~")
		(strikethrough . "+") (verbatin . "=") (underline . "_")))

(defun org-extra--add-emph-to-region (emph)
	(let ((char (alist-get emph org-extra--emphasis-list)))
		(goto-char (region-beginning))
		(insert char)
		(goto-char (region-end))
		(insert char)))

(defun org-extra--add-emphasis (emph)
	(if (or (region-active-p) (word-at-point))
			(save-excursion
				(when (not (region-active-p))
					(er/expand-region 1))
				(org-extra--add-emph-to-region emph))
		(let ((char (alist-get emph org-extra--emphasis-list)))
			(insert char char)
			(left-char))))


;; create all org-extra-add-[bold, italic etc]-to-region functions
(dolist (emph-name (mapcar 'car org-extra--emphasis-list))
	(let ((fn-name (intern (format "org-extra-add-%s-to-region" emph-name))))
		(defalias fn-name
			(lambda ()
				"Add emph-nameasis to region"
				(interactive)
				(funcall 'org-extra--add-emphasis emph-name)))))

(defun org-extra-scale-inline-imgs ()
	(interactive)
	(let ((max-size 800)
				(osize (or olivetti-body-width 0))
				(wsize (window-pixel-width)))
		(min max-size osize wsize)))


;;;###autoload
(defun org-extra-set-file-dictionary ()
  (interactive)
  (helm
   :prompt "Choose a dictionary: "
   :sources (helm-build-sync-source "Valid dictionaries"
              :candidates 'ispell-valid-dictionary-list
              :action '(lambda (dic)
                         (funcall-interactively
                          'add-file-local-variable-prop-line
                          'ispell-local-dictionary dic)
                         (save-buffer)
                         (revert-buffer nil t)
                         (flyspell-buffer)))))

;;
;; Table 
;;

;;;###autoload
(defun org-extra-money-round (val)
  (fp/pipe val
    (fp/partial '* 100)
    'round
    'float
    (lambda (v) (/ v 100))))


;;;###autoload
(defun org-extra-recalc-buffer ()
  (interactive)
  (org-table-recalculate-buffer-tables)
  (org-babel-execute-buffer))

(defun org-extra-recalc-buffer-and-save ()
  (interactive)
  (org-extra-recalc-buffer)
  (save-buffer))

;;;###autoload
(defun org-extra-kill-line ()
  (interactive)
  (throw-unless (org-at-table-p) "Cursor is not over a table")
  (previous-line)
  (save-excursion
    (next-line)
    (beginning-of-line)
    (kill-line)
    (org-delete-backward-char 1)))


(defun org-extra--calc-chunk-size (len)
  (let* ((divisors '(15 14 13 12 11 10 9 8 7 6 5))
         (size (fp/pipe divisors
                 (fp/partial 'mapcar (fp/partial 'make-pair len))
                 (fp/partial 'asoc-filter-keys (fp/partial '= 0))
                 (lambda (alist) (asoc-sort-keys alist '>))
                 'car-safe
                 'cdr-safe)))
    (or size (org-extra--calc-chunk-size (inc len)))))

(defun org-extra--chunks (lst)
  (seq-partition lst (org-extra--calc-chunk-size (length lst))))

(defun org-extra--add-hlines (table)
  (append '(hline) table '(hline)))

;;;###autoload
(defun org-extra-generate-index-table (heading-rx)
  "Generate a table with index of all headings level 2 that match HEADING-RX."
  (require 'asoc)

  (let  ((headings '()))
    (defun make-pair (len d)
      (cons (% len d) d))

    (defun format-link (heading-text)
      (let ((link (replace-regexp-in-string " " "-" heading-text))
            (text (replace-regexp-in-string "[^0-9]" "" heading-text)))
        (format "[[readme.org#%s][%s]]" link text)))

    (org-map-entries
     (lambda ()
       (add-to-list 'headings
                    (org-element-property :title (org-element-at-point))
                    t))
     "LEVEL=2")


    (fp/pipe headings
      (fp/filter 'regex-matches heading-rx)
      (fp/map 'format-link)
      'org-extra--chunks
      'org-extra--add-hlines)))

;;
;; Org roam
;;
(defun org-extra-node-insert-immediate (arg &rest args)
  "Insert org-roam node even if it doesnt exist yet."
  (interactive "P")
  (let ((args (cons arg args)))
    (apply #'org-roam-node-insert args)))


(defmacro org-extra-create-language-template-item (keybind name lang-code)
  `'(,keybind
     ,name
     plain
     "\n\n%?"
     :if-new
     (file+head
      "%<%Y%m%d%H%M%S>-${slug}.org"
      ,(concat "# -*- ispell-local-dictionary: \"" lang-code "\"; -*-" "\n"
               "#+title: ${title}" "\n"
               "#+filetags: :" name ":" "\n"))
     :unnarrowed t))


(defun lauremacs//numis-sql (type)
  (pcase type
    ('coins "
\#+begin_src sqlite :exports results :results table :db \":memory:\" :colnames yes :mode csv :header on :var moedas=moedas \n
.import $moedas moedas\n
select valor, anverso, reverso, período from moedas where origem='' and pais='%s' order by cast(valor as real);\n
\#+end_src\n
")
    ('banknotes "
\#+begin_src sqlite :exports results :results table :db \":memory:\" :colnames yes :mode csv :header on :var cedulas=cedulas \n
.import $cedulas cedulas\n
select valor, frente, verso, periodo from cedulas  where origem='' and pais='%s' order by cast(valor as real);\n
\#+end_src\n
")))

(defun lauremacs//numis-insert-subheading (name type)
  (let ((id (fp/pipe name
              (fp/replace " " "-")
              'downcase)))
    (insert (format "
** %s
:PROPERTIES:
:CUSTOM_ID: %s
:END:

" name id))
    (insert (format (lauremacs//numis-sql type)  name))))

(require 'parse-org)

(defun lauremacs/numis-wish-list (type)
  "TYPE should be 'coins or 'banknotes"
  (parse-org-delete-subtree-by-name "Wish List")
  (let ((countries (parse-org-get-subheadings-titles "Tabelas Fonte")))
    (save-excursion
      (search-forward "* ")
      (move-beginning-of-line 1)
      (open-line 1)
      (org-insert-heading '(4) nil t)
      (insert "Wish List")
      (cl-loop for country in countries do
               (lauremacs//numis-insert-subheading country type))
      )))

;;
;; org roam extra
;;
;; inspired on https://github.com/jgru/org-roam-ui/tree/add-export-capability

(defun org-extra-add-publish-roam-tag ()
  "Add \"publish\" tag to current org roam file."
  (interactive)
  (org-roam-tag-add '("publish")))

(require 'org-roam-ui)

(defun org-roam-ui--get-nodes-by-tags (&optional tags)
  "Return all nodes that contain at least one tag in TAGS."
  (if tags
      (eval
       `(org-roam-db-query [:select [id
                                     file
                                     title
                                     level
                                     pos
                                     olp
                                     properties
                                     (funcall group-concat tag
                                              (emacsql-escape-raw \\\, ))]
                                    :as tags
                                    :from nodes
                                    :left-join tags
                                    :on (= id node_id)
                                    :where (in tag [,@tags]) 
                                    :group :by id]))
    (org-roam-ui--get-nodes)))


(defun org-roam-ui--make-graphdata (&optional tags)
  "Get roam data and make JSON."
  (let* ((nodes-names
          [id
           file
           title
           level
           pos
           olp
           properties
           tags])
         (old (not (fboundp 'org-roam-db-map-citations)))
         (links-db-rows (if old
                            (org-roam-ui--separate-ref-links
                             (org-roam-ui--get-links old))
                          (seq-concatenate
                           'list
                           (org-roam-ui--separate-ref-links
                            (org-roam-ui--get-cites))
                           (org-roam-ui--get-links))))
         (links-with-empty-refs (org-roam-ui--filter-citations links-db-rows))
         (empty-refs (delete-dups (seq-map
                                   (lambda (link)
                                     (nth 1 link))
                                   links-with-empty-refs)))
         (nodes-db-rows (org-roam-ui--get-nodes-by-tags tags))
         (fake-nodes (seq-map #'org-roam-ui--create-fake-node empty-refs))
         ;; Try to update real nodes that are reference with a title build
         ;; from their bibliography entry. Check configuration here for avoid
         ;; unneeded iteration though nodes.
         (retitled-nodes-db-rows (if org-roam-ui-retitle-ref-nodes
                                     (seq-map #'org-roam-ui--retitle-node
                                              nodes-db-rows)
                                   nodes-db-rows))
         (complete-nodes-db-rows (append retitled-nodes-db-rows fake-nodes))
         (response `((nodes . ,(mapcar
                                (apply-partially
                                 #'org-roam-ui-sql-to-alist
                                 (append nodes-names nil))
                                complete-nodes-db-rows))
                     (links . ,(mapcar
                                (apply-partially
                                 #'org-roam-ui-sql-to-alist
                                 '(source target type))
                                links-db-rows))
                     (tags . ,(seq-mapcat
                               #'seq-reverse
                               (org-roam-db-query
                                [:select :distinct tag :from tags]))))))
    (when old
      (message "[org-roam-ui] You are not using the latest version of org-roam.
This database model won't be supported in the future, please consider upgrading."))
    (json-encode
     `((type . "graphdata")
       (data . ,response)))))

(defun org-roam-ui--export-graphdata (file &optional tags)
  "Create a JSON-file containting graphdata."
  (print tags)
  (write-region (org-roam-ui--make-graphdata tags) nil file))

(defun org-roam-ui-export (&optional tags)
  "Export `org-roam-ui's-data for usage as static webserver.
Get only nodes that has at least one tag of TAGS.
If none is provided, get all nodes."
  (interactive "sTags to filter: ")
  (let* ((dir (read-file-name "Specify output directory:"))
         (graphdata-file (concat (file-name-as-directory dir) "graphdata.json"))
         (notes-dir (concat (file-name-as-directory dir) "notes/")))
    (org-roam-ui--export-graphdata graphdata-file
                                   (and (not (string= "" tags)) (split-string tags " ")))
    (make-directory notes-dir :parents)
    (mapcar (lambda (id)
              (let* ((cid (car id))
                     (content (org-roam-ui--get-text cid)))
                (write-region content nil (concat notes-dir cid) 'append)))
            (org-roam-db-query "select id from nodes;"))))


(cl-defun org-extra-create-id (&optional (size 4))
  (seq-take (uuidgen-4) size))

;;
;; sqlite
;;

(defun org-extra-sqlite-wrap-table (table)
  "Add hline on the beginning and end of a TABLE."
  `(hline ,@table hline))

(with-eval-after-load "ob-sqlite"
  (advice-add
   'org-babel-sqlite-offset-colnames
   :filter-return
   'org-extra-sqlite-wrap-table))


;;
;; Org as DB interface
;;

(require 's)

(defun org-db-ui-insert-batch (table-name db-name data-table)
  "Insert in database DB-NAME in TABLE-NAME from org table DATA-TABLE.
Works with postgres."
  (let* ((table-cols (string-join (car data-table) ", "))
         (table-rows (cdr data-table))
         (tmp-file   (format "/tmp/%s.csv" table-name)))
    ;; Converts org table to CSV
    (write-region
     (orgtbl-to-csv (cdr orgtable) nil) nil tmp-file)
    ;; Insert data in postgres from CSV file
    (print ;; prints result in org-babel call
     (shell-command-to-string
      (s-lex-format
       "psql -d ${db-name} -c \"COPY ${table-name}(${table-cols}) FROM STDIN WITH delimiter as ',' NULL AS '' csv\" < ${tmp-file}")))))


(defun org-db-ui--conditional (title row)
  (fp/pipe (list title row)
    (fp/partial 'apply 'mapcar* 'cons)
    (fp/filter (lambda (pair) (not (string= (cdr pair) ""))))
    (fp/map (lambda (pair) (format "and %s='%s'" (car pair) (cdr pair))))
    (fp/partial 's-join " ")
    (fp/concat "where 1=1 "))
  )

(defun org-db-ui-delete-batch (table-name db-name data-table)
  (let ((title (car data-table))
        (rows  (cdr data-table)))
    (mapcar
     (lambda (row)
       (let ((conditional (org-db-ui--conditional title row)))
         (print
          (shell-command-to-string
           (s-lex-format
            "psql -d ${db-name} -c \"delete from ${table-name} ${conditional}\"")))))
     rows)))






(provide 'org-extra)
