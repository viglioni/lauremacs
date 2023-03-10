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
         (content (fp/pipe-deprecated files
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

;;;###autoload
(defun require-without-throw (lib)
	"Require LIB and prints a message if it's not found.
E.g. \"(require-without-throw 'functional)\"."
	(if (require lib nil t)
			(message (concat "Loaded lib: " (symbol-name lib)))
		(message (concat "Can't load lib: " (symbol-name lib)))))

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


;;;###autoload
(defun use-dependencies (&rest libs)
	"Install, if necessary, all libraries and require them.
Argument &REST libs to be installed and required."
	(dolist (lib libs)
		(unless (package-installed-p lib)
			(package-refresh-contents)
			(package-install lib))
		(require lib)))


;;;###autoload
(defun add-multiple-into-list (lst items)
	"Add each item from ITEM into LST."
	(throw-unless (symbolp lst) "List should be a symbol.")
	(dolist (item items)
		(add-to-list lst item)))


;;;###autoload
(defun lauremacs-request-sync-get (url &optional on-failure)
  "URL must be a valid url.
ON-FAILURE is a function expecting one parameter: error-thrown."
  (require 'request)
  (let ((lauremacs-request-result nil)
        (on-fail (or on-failure
                     (lambda (err) (message "Got error: %S" err)))))
    (request url
      :sync t
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq lauremacs-request-result data)))
      :error
      (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                     (funcall on-fail error-thrown))))
    lauremacs-request-result))

;;;###autoload
(defun exec-path-when-cmd-not-found (cmd)
  "Run `exec-path-from-shell-initialize' when CMD is not found."
  (unless (executable-find cmd)
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)))

;;;###autoload
(defun display-ansi-colours ()
  "Display ansi-colours on current buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
