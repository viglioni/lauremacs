;;; amazonia.el --- Custom package manager based on straight.el -*- lexical-binding: t; l-syntax: t; -*-
;;
;; Author: Laura Viglioni
;; 2025
;;
;; GNU Public License 3.0
;;
;; since: NEXT
;;
;; comments:
;; deps: straight, l
;;
;; This is a custom package manager built on top of straight.el, providing
;; a declarative approach to package management with manifest-based installation,
;; automatic cleanup of unused packages, and convenient macros for package
;; configuration.
;;
;; Main features:
;; - Manifest-based package management (runtime-deps and dev-deps)
;; - Multiple package source types (git, latest, built-in, custom recipes)
;; - Runtime installation of packages from manifests
;; - Automatic cleanup of packages not in manifest
;; - Integration with use-package for declarative configuration
;; - Package validation to ensure all configured packages are in manifest
;;
;; Usage:
;; Define your packages in a manifest file (lauremacs-packages) with the
;; structure:
;;   '((runtime-deps . ((package-name :repo "user/repo")
;;                      (another-package :latest)
;;                      (builtin-package :built-in)))
;;     (dev-deps . ((dev-package :repo "user/dev-repo"))))
;;
;; Then use (amazonia/runtime-install) to install all runtime dependencies, and
;; (config-package package-name ...) to configure packages with use-package
;; syntax while ensuring they exist in the manifest.
;;
;;; Code:

(require 'straight)
(require 'l)
(require 'cl-macs)

(defun amazonia/runtime-install ()
  (interactive)
  (lauremacs/load packages)
  (cl-loop for package in (alist-get 'runtime-deps (lauremacs-packages)) do
           (amazonia//install package)))

(defun amazonia//install (package)
  (let ((name (car package))
        (spec (cdr package)))
    (pcase ( amazonia//package-type spec)
      ('git    (straight-use-package `(,@package :type git :host github)))
      ('latest (straight-use-package name))
      ('built-in (straight-use-package `(,name :type built-in)))
      ('recipee (straight-use-package package))
      (_       (error "Failed to install %s" name)))))


(defun amazonia//package-type (spec)
  (cond
   ((plist-get  spec  :repo)     'git)
   ((equal (car spec) :latest)   'latest)
   ((equal (car spec) :built-in) 'built-in)
   (t 'recipee)))


(defun amazonia//get-manifest-packages ()
  "Get list of all package names from the manifest."
  (lauremacs/load packages)
  (let ((all-packages (append (alist-get 'runtime-deps (lauremacs-packages))
                              (alist-get 'dev-deps (lauremacs-packages)))))
    (mapcar (lcomp #'symbol-name #'car) all-packages)))

(defun amazonia/clean-unused-packages ()
  "Remove all packages installed via package.el (not managed by straight.el)."
  (interactive)
  
  ;; delete installed packages from emacs default package manager
  (when (fboundp 'package-installed-p)
    (require 'package)
    (package-initialize)
    (mapcar (lcomp #'package-delete #'cadr) package-alist))

  ;; deleted unused packages installed with pm
  (straight--make-build-cache-available)
  (let ((manifest-packages (amazonia//get-manifest-packages))
        (all-packages      (hash-table-keys straight--build-cache)))    
    (cl-loop for package in all-packages
             unless (and (gethash package straight--profile-cache)
                         (not (memq package manifest-packages)))
             do (delete-directory (straight--build-dir package) 'rec)
             (message (format "deleted %s" package)))))


(provide 'amazonia)
;;; amazonia.el ends here
