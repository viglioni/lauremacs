;;
;; @author Laura Viglioni
;; 2025
;;
;; GNU Public License 3.0
;;
;; since: NEXT
;;
;; early-init.el:
;; 
;;

;; Lauremacs start time
(defvar lauremacs-start-time (current-time))

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
If NOERROR is non-nil, don't throw error if file does not exist."
  (load (expand-file-name path user-emacs-directory) noerror))


(with-eval-after-load "warnings" ;; avoid warning flood of compiled functions
  (setq warning-minimum-level :error)) 

(setq backup-directory-alist
          `(("." . ,(concat user-emacs-directory "/backups"))))


;;
;; avoid loading .emacs before init.el
;;

(let* ((user-emacs-temp-file (make-temp-file "emacs-init-file")))
  (when (file-exists-p "~/.emacs")
    ;; move .emacs to a temp file
    (shell-command-to-string
     (format "mv ~/.emacs %s"
             user-emacs-temp-file))
    (eval
     `(add-hook 'emacs-startup-hook
                (lambda ()
                  ;; move .emacs back
                  (shell-command-to-string
                   (format "mv %s ~/.emacs"
                           ,user-emacs-temp-file)))))))


;;
;; set theme
;;

(when (boundp 'solarized-theme)
  (load-theme 'solarized-light))

;;
;; set user init file if it exists
;;

(add-hook
 'emacs-startup-hook
 (lambda ()
   (let ((lauremacs-user-init-file (expand-file-name ".lauremacs" user-emacs-directory)))
     (cond
      ((file-exists-p lauremacs-user-init-file)
       (setq user-init-file lauremacs-user-init-file))
      ((file-exists-p "~/.emacs")
       (setq user-init-file "~/.emacs"))))))



;; Do not use tabs for indentation
(setq-default indent-tabs-mode nil)
