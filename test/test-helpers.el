;;
;; @author Laura Viglioni
;; 2025
;;
;; GNU Public License 3.0
;;
;; since: 0.1.0
;;
;; test-helpers.el:
;; Set up buttercup and some macro helpers.
;;

;;; code:

;;;###autoload
(defun load-test-file (file-name)
  "search for file and loads it"
  (mapcar 'load
          (directory-files-recursively "." (concat file-name ".el"))))

;;;###autoload
(defmacro context (description &rest body)
  "the same as describe, but more idiomatic and with defun indentation."
  (declare (indent defun))
  `(describe (concat "when " ,description)
             ,@body))

;;;###autoload
(defmacro test-it (description &rest body)
  "the same as it, but with defun indentation."
  (declare (indent defun))
  `(it (concat "it " ,description)
       ,@body))

(provide 'test-helpers)

;;; test-helpers.el ends here.
