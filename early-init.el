;;; -*- lexical-binding: t; -*-
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
(setq debug-on-error t)
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



(defmacro lauremacs/path (&rest path-parts)
  "Return the path relative to `emacs-user-directory'.
Accepts multiple arguments that will be joined with slashes.
Example: (lauremacs/path config langs ts.el) => ~/.emacs.d/config/langs/ts.el"
  `(expand-file-name
    (mapconcat #'symbol-name ',path-parts "/")
    user-emacs-directory))

(defmacro lauremacs/file-path (&rest path-parts)
  "Return the path relative to `emacs-user-directory', adding .el if not present.
Accepts multiple arguments that will be joined with slashes.
Example: (lauremacs/file-path config langs web) => ~/.emacs.d/config/langs/web.el"
  `(let ((path (lauremacs/path ,@path-parts)))
     (if (string-match "\\.el$" path)
         path
       (format "%s.el" path))))

(defmacro lauremacs/load (&rest path-parts-and-maybe-noerror)
  "Load an Emacs Lisp file from specified path parts.
Path parts are relative to the user's Emacs directory and will be joined.
If the last argument is t, it's treated as NOERROR flag.
Example: (lauremacs/load config langs web) or (lauremacs/load config langs web t)"
  (let* ((args path-parts-and-maybe-noerror)
         (noerror (and (eq (car (last args)) t) (setq args (butlast args)) t)))
    `(load (lauremacs/file-path ,@args) ,noerror)))

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
;; appearance
;;


(lauremacs/load config appearance)

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
       (setq user-init-file "~/.emacs"))))
      ;; user loads
   (load user-init-file)))


