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

;;;###autoload
(defmacro load-lib (lib-name)
  "requires a lib in external or personal lib dir. Usage example:
   (load-lib 'emacs-grammarly)"
  `(require ,lib-name))



;;;###autoloading
(defmacro bind-lazy-function (func-name lib-func-name package-name)
  "Creates an interactive of a lib that is not imported by default
   that loads it when is called
   Usage example:
   (bind-lazy-function 'spotify-func 'spotify-status 'spotilau)
   (global-set-key (kbd \"M-p M-p\") 'spotify-func)"
  `(defun ,(eval func-name) ()
     (interactive)
     (load-lib ,package-name)
     (call-interactively ,lib-func-name)))
