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

;; todo create single window function
;; todo window layouts
;; docs


;;;###autoload
(defun wb/switch-to-last-buffer ()
  ;; todo use pipe
  (interactive)
  (switch-to-buffer
   (car
    (seq-filter
     (lambda (b) (not (string-match-p "*" (buffer-name b))))
     (cdr
      (if (projectile-project-p) (projectile-project-buffers) (buffer-list)))))))



;;;###autoload
(defun wb/switch-buffers ()  
  (interactive)
  (if (projectile-project-p)
      (helm-projectile-switch-to-buffer)
    (helm-buffers-list)))

(provide 'windows-and-buffers)
