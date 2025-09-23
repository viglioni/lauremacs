;;
;; @author Laura Viglioni
;; 2025
;;
;; GNU Public License 3.0
;;
;; since: NEXT
;;
;; helper functions to navigate buffers/windows
;;

;;;###autoload
(defun wb/switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer (car (buffer-list))))

;; todo create single window function
;; todo window layouts

(provide 'windows-and-buffers)
