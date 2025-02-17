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
If NOERROR is given, don't throw error if file does not exist."
  (load (expand-file-name path user-emacs-directory) noerror))


(with-eval-after-load "warnings" ;; avoid warning flood of compiled functions
  (setq warning-minimum-level :error)) 

(setq backup-directory-alist
          `(("." . ,(concat user-emacs-directory "/backups"))))


;;
;; avoid loading .emacs when .lauremacs exists
;;

(let* ((lauremacs-user-init-file (expand-file-name ".lauremacs" user-emacs-directory))
      (user-emacs-temp-file      (make-temp-file "emacs-init-file"))
      (dot-lauremacs-exist-p     (file-exists-p lauremacs-user-init-file)))
  (when (and dot-lauremacs-exist-p (file-exists-p "~/.emacs"))
    ;; move .emacs to a temp file
    (shell-command-to-string
     (format "cat ~/.emacs > %s && rm ~/.emacs"
             user-emacs-temp-file))
    (add-hook 'emacs-startup-hook
              (lambda ()
                ;; move .emacs back
                (shell-command-to-string
                 (format "cat %s > ~/.emacs"
                         user-emacs-temp-file))
                ;; set .lauremacs as user-init-file
                (when dot-lauremacs-exist-p
                  (setq user-init-file lauremacs-user-init-file))))))


