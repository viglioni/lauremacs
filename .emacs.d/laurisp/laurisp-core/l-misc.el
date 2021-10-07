;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; misc related functions
;;

;;(require 'functional)

;;;###autoload
(defun compile-laurisp-core ()
  (interactive)
  (let* ((filename "laurisp-core.el")
         (files (directory-files "." t "^l-[a-z\\-].*\\.el$"))
         (content (fp/pipe files
                     ((mapcar 'get-string-from-file)
                      (string-join)))))
    (with-temp-buffer
      (insert content)
      (insert "\n\n(provide 'laurisp-core)\n")
      (write-file filename))
    (byte-compile-file filename)))


