;;
;; @author Laura Viglioni
;; 2021 - 2023
;; GNU Public License 3.0
;;

;;
;; general related functions
;;

(require 'functional)

;;; code:


;;
;; Load laurisp files
;;

(message "Loading laurisp-core...")

(dolist (file (directory-files "." t "^l-[a-zA-Z0-9-]+\\.el$"))
  (if (load file nil nil t) (message (format "Loaded %s" file))
    (message (format "shit: %s" file))))

(message "Loaded laurisp-core.")

(provide 'laurisp-core)
