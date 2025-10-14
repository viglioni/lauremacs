;;; package-manager.el --- Set up straight.el and helpers. -*- lexical-binding: t; l-syntax: t -*-
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
  (lauremacs/load packages)
  (cl-loop for package in (alist-get 'runtime-deps (lauremacs-packages)) do
           (pm//install package)))

(defun pm//install (package)
  (let ((name (car package))
        (spec (cdr package)))
    (pcase (pm//package-type spec)
      ('git    (straight-use-package `(,@package :type git :host github)))
      ('latest (straight-use-package name))
      ('built-in (straight-use-package `(,name :type built-in)))
      ('recipee (straight-use-package package))
      (_       (error "Failed to install %s" name)))))


(defun pm//package-type (spec)
  (cond
   ((plist-get spec :repo) 'git)
   ((equal (car spec) :latest) 'latest)
   ((equal (car spec) :built-in) 'built-in)
   (t 'recipee)))

(defmacro config-package (package &rest args)
  "Configure PACKAGE with use-package syntax, ensuring it's in manifest."
  (declare (indent defun))
  (lauremacs/load packages)
  ;; Check if package exists in manifests
  (let ((all-packages (append (alist-get 'runtime-deps (lauremacs-packages))
                             (alist-get 'dev-deps (lauremacs-packages)))))
    (unless (or (assq package all-packages)
		(eq package 'magit)
		(eq package 'org))
      (error "Package %s not found in lauremacs-packages manifest" package)))
  ;; Generate use-package form
  `(use-package ,package :straight t ,@args))


;;
;; Function calls
;;

(pm/runtime-install)
(lauremacs/load core core-packages)




;;; package-manager.el ends here
