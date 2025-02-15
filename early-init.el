;;
;; @author Laura Viglioni
;; 2025
;;
;; GNU Public License 3.0
;;
;; since: 0.1.0
;;
;; early-init.el:
;; 
;;


;;
;; Garbage colector threshold
;;

(setq gc-cons-threshold (* 20 1024 1024)) ;; initial threshold

(add-hook 'emacs-startup-hook ;; threshold after init
          (lambda () (setq gc-cons-threshold (* 128 1024 1024)))) 


(setq user-emacs-directory (file-truename "~/.emacs.d"))


;; Prevent loading other config files
(setq inhibit-default-init t)  ; Prevent loading of default.el
(setq site-run-file nil) 

(defun lauremacs/load (path &optional noerror)
  "Load an Emacs Lisp file from a specified PATH.
PATH is relative to the user's Emacs directory.
If NOERROR is given, don't throw error if file does not exist."
  (load (expand-file-name path user-emacs-directory) noerror))


(with-eval-after-load "warnings" ;; avoid warning flood of compiled functions
  (setq warning-minimum-level :error)) 

(setq backup-directory-alist
          `(("." . ,(concat user-emacs-directory "/backups"))))

;; load init.el before .emacs
(funcall
 (lambda ()
   "Ensure the init.el loads is at the start of .emacs file."
   (let ((emacs-file "~/.emacs")
         (load-cmd "(lauremacs/load \"./init.el\")"))
     (when (file-exists-p emacs-file)
       (with-temp-buffer
         (insert-file-contents emacs-file)
         (unless (looking-at load-cmd)
           (goto-char (point-min))
           (insert load-cmd)
           (write-region (point-min) (point-max) emacs-file)))))))
