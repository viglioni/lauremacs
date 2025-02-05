;;; lauremacs-org-extensions.el --- org-mode related functions
;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;


;;; Commentary:
;; 

(require 'helm)
(require 'uuidgen)
(require 'functional)
;;; Code:

(message "loading lauremacs-org-extensions...")

(defconst lauremacs-org-extensions-dir (join-path
																				lauremacs-internal-libs-dir
																				"lauremacs-org-extensions"))


;;
;; Insert custom headers
;;


;; ;;;###autoload
;; (defun lauremacs-org--insert-custom-header (header-func)
;;   (save-excursion
;;     (beginning-of-buffer)
;;     (funcall (eval header-func))))

;; (setq lauremacs-org--helm-insert-custom-headers-srcs
;;       (helm-build-sync-source "Avaliable headers:"
;;         :candidates '(("haskell notebook" . 'org-haskell-notebook-header)
;;                       ("beamer presentation" . 'org-beamer-presentations-header)
;;                       ("latex articles" . 'org-latex-article-header))
;;         :action 'lauremacs-org--insert-custom-header))

;; ;;;###autoload
;; (defun lauremacs/org-insert-custom-headers ()
;;   (interactive)
;;   (throw-unless (derived-mode-p 'org-mode) "Not in org-mode!")
;;   (load-lib 'org-headers-skeletons)
;;   (helm :prompt "Choose a header: "
;;         :sources lauremacs-org--helm-insert-custom-headers-srcs))


;;
;;  Insert source on org-mode
;;

;;;###autoload
(defun lauremacs-org--insert-src-with-post (lang-name &optional post-func)
	"Insert org source block.
LANG-NAME: the language name.
POST-FUNC: reference for a function to run on :post exectution."
  (let ((post (if post-func (concat ":post " post-func "(*this*)") "")))
    (insert (concat "#+begin_src " lang-name " :exports both :results output " post
                    "\n\n"
                    "#+end_src"))))

;;;###autoload
(defun lauremacs-org--insert-sqlite (&optional tables-names)
  (interactive "sInsert tables names: ")
  (let ((formatted-var (fp/pipe tables-names
                         (fp/split " ")
                         (fp/map (lambda (name) (format "%s=%s" name name)))
                         (fp/join " ")
                         ))
        (import-tables (fp/pipe tables-names
                  (fp/split " ")
                  (fp/map (lambda (name) (format ".import $%s %s" name name)))
                  (fp/join "\n"))))
    (insert (format
             "#+begin_src sqlite :exports results :results table :db \":memory:\" :colnames yes :mode csv :header on :var %s \n%s\n\n #+end_src"
             formatted-var
             import-tables))))

;;;###autoload
(defun lauremacs-org--insert-laura-db ()
  (interactive)  
  (insert 
          "#+begin_src sql :exports results :results table :engine postgres :database laura_db \n\n#+end_src"))


;;;###autoload
(defun lauremacs-org--insert-src-with-session (lang-name &optional session-name)
  "Insert org source block for LANG-NAME with session SESSION-NAME."
  (insert (format
           "#+begin_src %s :exports both :results output :session %s \n\n #+end_src"
           lang-name
           (or session-name "sage"))))



;;;###autoload

(defun lauremacs-org--insert-src (lang-name)
	"Given the LANG-NAME, insert a org code block with proper :post function."
  (message lang-name)
  (cond
   ((string= "laura-db" lang-name) (lauremacs-org--insert-laura-db))
   ((string= "haskell" lang-name) (lauremacs-org--insert-src-with-post    lang-name "org-babel-haskell-formatter"))
   ((string= "clojure" lang-name) (lauremacs-org--insert-src-with-post    lang-name "org-babel-clojure-formatter"))
   ((string= "sage"    lang-name) (lauremacs-org--insert-src-with-session lang-name))
   ((string= "sqlite"  lang-name) (call-interactively 'lauremacs-org--insert-sqlite))
   (t (lauremacs-org--insert-src-with-post lang-name))))

(defconst lauremacs-org--helm-lang-sources
  (helm-build-sync-source "Language name"
    :candidates '(lambda ()
                   (fp/pipe org-babel-load-languages
                     (fp/map 'car)
                     (fp/partial 'seq-concatenate 'list '(laura-db))))
    :action 'lauremacs-org--insert-src))

(defconst lauremacs-org--helm-lang-sources-fallback
  (helm-build-dummy-source "Language name"
    :action 'lauremacs-org--insert-src))

;;;###autoload
(defun lauremacs/org-insert-source ()
	"Insert a org code block according to the variable `org-babel-load-languages'."
  (interactive)
  (if (eq 'org-mode major-mode)
      (progn
        (helm
         :history t
         :volatile nil
         :sources '(lauremacs-org--helm-lang-sources lauremacs-org--helm-lang-sources-fallback))
        (forward-line -1)
				(indent-for-tab-command))))





(provide 'lauremacs-org-extensions)

;;; lauremacs-org-extensions.el ends here
