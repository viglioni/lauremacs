;;; package-manager.el --- Set up straight.el and helpers.
;;
;; Author: Laura Viglioni
;; 2025
;;
;; GNU Public License 3.0
;;
;; since: 0.1.0
;;
;;; Code:

;;
;; install straight.el
;;

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;;
;; Update lock file after installing a package via elisp
;;

(defun lauremacs/update-versions-if-from-elisp (&rest _)
  (unless (eq real-this-command 'straight-use-package)
    (straight-freeze-versions)))

(advice-add 'straight-use-package :after #'lauremacs/update-versions-if-from-elisp)

(setq straight-use-package-by-default t)
(setq straight-check-for-modifications '(check-on-save find-when-checking))

;;
;; Update packages
;;

(defun lauremacs/update-package (package)
  "Update a single package and update lockfile."
  (interactive "sPackage name: ")
  (straight-pull-package package)
  ;; Update lockfile since this is called from Elisp
  (straight-freeze-versions))

(defun lauremacs/update-all-packages ()
  "Update all packages and update lockfile."
  (interactive)
  (straight-pull-all))

;;
;; sync packages with lock file
;;

(defun lauremacs/sync-straight-packages ()
  "Synchronize packages with lockfile based on
straight-use-package calls."
  (interactive)
  (let ((success t)
        (error-packages '()))
    
    ;; Main synchronization with error handling
    (condition-case-unless-debug err
        (progn
          (message "Starting package synchronization...")
          
          ;; Clean unused repos
          (condition-case cleanup-err
              (straight-remove-unused-repos)
            (error
             (setq success nil)
             (message "Warning: Error during cleanup: %s" cleanup-err)))
          
          ;; Prune build cache
          (condition-case prune-err
              (straight-prune-build-cache)
            (error
             (setq success nil)
             (message "Warning: Error during cache pruning: %s" prune-err)))
          
          ;; Freeze versions
          (condition-case freeze-err
              (straight-freeze-versions)
            (error
             (setq success nil)
             (push (format "Failed to freeze versions: %s" freeze-err) error-packages))))
      
      ;; Handle any unexpected errors
      ((debug error)
       (setq success nil)
       (message "Critical error during synchronization: %S" err)))
    
    ;; Final status report
    (if success
        (message "Package synchronization completed successfully")
      (message "Package synchronization completed with errors: %s" 
               (mapconcat #'identity error-packages ", ")))))

;; Add to after-init-hook
(add-hook 'after-init-hook #'lauremacs/sync-straight-packages)


;;; package-manager.el ends here
