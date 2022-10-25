;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;


;;; Code:



(require 'laurisp-core)
(message "loading ramda-docs...")

(use-dependencies 'request 'helm 'eww)

;;
;; ramda-docs related functions
;;

(defvar ramda-html nil)
(setq ramda-docs-url "https://ramdajs.com/docs/")

;;;###autoload
(defun download-ramda-html ()
  (message "fetching ramda data...")
  (unless ramda-html
    (request ramda-docs-url
      :sync t
      :success (cl-function
                (lambda (&key data &allow-other-keys) (setq ramda-html data)))
      :error  (cl-function
               (lambda (&key error-thrown &allow-other-keys&rest _)
                 (error "Got error: %S" error-thrown))))))

;;;###autoload
(defun function-info (html-line)
  (let* ((filtered-string
          (fp/pipe html-line
                   ((replace-regexp-in-string
                     (rx (or "data-name" "data-category" "=" "\"" ">"))
                     "")
                    ((lambda (str) (split-string str " "))))))
         (fn-name (head filtered-string))
         (fn-category (nth 1 filtered-string)))
    (cons (format "(%s) %s" fn-category fn-name) fn-name)))

;;;###autoload
(defun parse-funcs-html (html)
  (regex-matches "data-name=\"[[:alpha:]_]*\" data-category=\"[[:alpha:]]*\"" html))

;;;###autoload
(defun helm-ramda-candidates ()
  (throw-if (any-nil? ramda-html) "ramda's page wasn't downloaded!")
  (fp/pipe ramda-html
           ((parse-funcs-html)
            (mapcar 'function-info)
            (alist-sort-by-cdr-ci))))

;;;###autoload
(defun open-ramda-doc-url (fn-name)
  (browse-url (concat ramda-docs-url "#" fn-name)))

;;;###autoload
(defun open-ramda-doc-url-eww (fn-name)
  (eww (concat ramda-docs-url "#" fn-name)))


;;;###autoload
(defun ramda-docs-open ()
  (interactive)
  (download-ramda-html)
  (helm :prompt "Choose function: "
        :sources (helm-build-sync-source "ramda functions"
                   :candidates 'helm-ramda-candidates
                   :action 'open-ramda-doc-url)))

;;;###autoload																		
(defun ramda-docs-open-eww ()
  (interactive)
  (download-ramda-html)
  (helm :prompt "Choose function: "
        :sources (helm-build-sync-source "ramda functions"
                   :candidates 'helm-ramda-candidates
                   :action 'open-ramda-doc-url-eww)))


(provide 'ramda-docs)

;;
;; warnings
;;

;; Entering directory ‘/Users/laura.viglioni/laurisp/personal-libs/ramda-docs/’
;; ramda-docs.el:16:7:Warning: assignment to free variable ‘ramda-docs-url’

;; In download-ramda-html:
;; ramda-docs.el:21:14:Warning: reference to free variable ‘ramda-docs-url’

;; In function-info:
;; ramda-docs.el:36:23:Warning: function ‘reduce’ from cl package called at
;; runtime

;; In helm-ramda-candidates:
;; ramda-docs.el:47:23:Warning: function ‘reduce’ from cl package called at
;; runtime

;; In open-ramda-doc-url:
;; ramda-docs.el:55:23:Warning: reference to free variable ‘ramda-docs-url’


