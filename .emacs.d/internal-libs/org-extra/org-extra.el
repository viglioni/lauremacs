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
							:action (fp/curry 'LauTex-insert-math-cmd latex-cmd))))


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
  (fp/upipe val
    (fp/partial '* 100)
    'round
    'float
    (lambda (v) (/ v 100))))


;;;###autoload
(defun org-extra-recalc-buffer ()
  (interactive)
  (org-table-recalculate-buffer-tables)
  (org-babel-execute-buffer))

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
         (size (fp/upipe divisors
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


    (fp/upipe headings
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




(provide 'org-extra)
