;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; general related functions
;;

;;;###autoload
(defmacro throw-if (condition &optional error-description)
  "if condition is true, thrown an error"
  `(if ,condition (error (or ,error-description ""))))

(defmacro throw-unless (condition &optional error-description)
  "if condition is true, thrown an error"
  `(unless ,condition (error (or ,error-description ""))))

