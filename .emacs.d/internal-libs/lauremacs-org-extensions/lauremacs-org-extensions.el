;;; lauremacs-org-extensions.el --- org-mode related functions
;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;


;;; Commentary:
;; 

(require 'helm)
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
(defun lauremacs-org--insert-src (lang-name)
	"Given the LANG-NAME, insert a org code block with proper :post function."
  (cond
   ((string= "haskell" lang-name) (lauremacs-org--insert-src-with-post lang-name "org-babel-haskell-formatter"))
   ((string= "clojure" lang-name) (lauremacs-org--insert-src-with-post lang-name "org-babel-clojure-formatter"))
   (t (lauremacs-org--insert-src-with-post lang-name))))

(defconst lauremacs-org--helm-lang-sources
  (helm-build-sync-source "Language name"
    :candidates '(lambda () (mapcar 'car org-babel-load-languages))
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

;;
;; org jira
;;

;;;###autoload
;; (defun laurg-jira-copy-current-issue-url ()
;;   (interactive)
;;   (let* ((issue-key (org-jira-get-from-org 'issue 'key))
;;          (issue-url (concat jiralib-url "/browse/" issue-key)))
;;     (kill-new issue-url)
;;     (message (concat "copied " issue-url))
;;     issue-url))




(provide 'lauremacs-org-extensions)

;;; lauremacs-org-extensions.el ends here
