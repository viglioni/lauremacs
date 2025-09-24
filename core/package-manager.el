;;; package-manager.el --- Set up straight.el and helpers.
;;
;; Author: Laura Viglioni
;; 2025
;;
;; GNU Public License 3.0
;;
;; since: NEXT
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

(require 'straight)

(setq straight-use-package-by-default t)
(setq straight-check-for-modifications '(check-on-save find-when-checking))


(defun pm/runtime-install ()
  (interactive)
  (cl-loop for package in (alist-get 'runtime-deps lauremacs-packages) do
           (pm//install package)))

(defun pm//install (package)
  (let ((name (car package))
        (spec (cdr package)))
    (pcase (pm//package-type spec)
      ('git    (straight-use-package `(,@package :type git :host github)))
      ('latest (straight-use-package name))
      (_       (error "Failed to install %s" name)))))


(defun pm//package-type (spec)
  (cond
   ((plist-get spec :repo) 'git)
   ((equal (car spec) :latest) 'latest)
   (t (error "Failed to match %s" spec))))

(defmacro config-package (package &rest args)
  "Configure PACKAGE with use-package syntax, ensuring it's in manifest."
  (declare (indent defun))
  
  ;; Check if package exists in manifest
  (let ((all-packages (append (alist-get 'runtime-deps lauremacs-packages)
                             (alist-get 'dev-deps lauremacs-packages))))
    (unless (assq package all-packages)
      (error "Package %s not found in lauremacs-packages manifest" package)))
  ;; Generate use-package form
  `(use-package ,package :straight t ,@args))


;;
;; Function calls
;;

(lauremacs/load "packages.el")
(pm/runtime-install)
(lauremacs/load "core/core-packages")




;;; package-manager.el ends here
