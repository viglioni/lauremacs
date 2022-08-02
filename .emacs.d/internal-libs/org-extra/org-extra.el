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

;;
;; LaTeX functions
;;

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


(provide 'org-extra)
