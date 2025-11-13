;;; package-manager.el --- Helpers for package manager -*- lexical-binding: t; l-syntax: t; -*-
;;
;; Author: Laura Viglioni
;; 2025
;;
;; GNU Public License 3.0
;;
;; since: NEXT
;;
;; comments:
;; this code uses l.el
;;
;;; Code:

(require l)

(defun pm//get-manifest-packages ()
  "Get list of all package names from the manifest."
  (lauremacs/load packages)
  (let ((all-packages (append (alist-get 'runtime-deps (lauremacs-packages))
                              (alist-get 'dev-deps (lauremacs-packages)))))
    (mapcar (lcomp #'symbol-name #'car) all-packages)))

(defun pm/clean-unused-packages ()
  "Remove all packages installed via package.el (not managed by straight.el)."
  (interactive)
  
  ;; delete installed packages from emacs default package manager
  (when (fboundp 'package-installed-p)
    (require 'package)
    (package-initialize)
    (mapcar (lcomp #'package-delete #'cadr) package-alist))

  ;; deleted unused packages installed with pm
  (straight--make-build-cache-available)
  (let ((manifest-packages (pm//get-manifest-packages))
        (all-packages      (hash-table-keys straight--build-cache)))    
    (cl-loop for package in all-packages
             unless (and (gethash package straight--profile-cache)
                         (not (memq package manifest-packages)))
             do (delete-directory (straight--build-dir package) 'rec)
             (message (format "deleted %s" package)))))
