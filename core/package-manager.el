;;; package-manager.el --- Set up straight.el and helpers. -*- lexical-binding: t; -*-
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

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Temporarily use HTTPS for bootstrapping
(setq straight-vc-git-default-protocol 'https)

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
(setq straight-disable-compile '("l"))

(unless (featurep 'l)
  (straight-use-package '(l :repo "viglioni/l-el"
                            :branch "latest-release"
                            :type git
                            :host github)))

(defmacro config-package (package &rest args)
  "Configure PACKAGE with use-package syntax, ensuring it's in manifest."
  (declare (indent defun))
  (lauremacs/load packages)
  ;; Check if package exists in manifests
  (let ((all-packages (append (alist-get 'runtime-deps (lauremacs-packages))
                              (alist-get 'dev-deps (lauremacs-packages)))))
    (unless (or (assq package all-packages)
		            (eq package 'l)
		            )

      (error "Package %s not found in lauremacs-packages manifest" package)))
  ;; Generate use-package form
  `(use-package ,package :straight t ,@args))

;;
;; Function calls
;;

(require 'amazonia)
(amazonia/runtime-install)
(lauremacs/load core core-packages)
;(amazonia/clean-unused-packages)

(provide 'package-manager)

;;; package-manager.el ends here
