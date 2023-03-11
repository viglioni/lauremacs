;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; general related functions
;;

(defun throw-if (condition &optional error-description)
  "If CONDITION is true, thrown an error.
If ERROR-DESCRIPTION is true, use it as error message."
  (when condition (error (or error-description ""))))

(defun throw-unless (condition &optional error-description)
  "If CONDITION is false, thrown an error.
If ERROR-DESCRIPTION is true, use it as error message."
  (unless condition (error (or error-description ""))))


;;;###autoload
(defun add-to-multiple-hooks (function &rest hooks)
	"Add a FUNCTION to several HOOKS."
  (mapc (lambda (hook) (add-hook hook function)) hooks))
