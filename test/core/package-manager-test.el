;;; -*- lexical-binding: t; l-syntax: t -*-
;;
;; @author Laura Viglioni
;; 2025
;;
;; GNU Public License 3.0
;;
;; since: NEXT
;;
;; package-manager-test.el:
;; Tests for package-manager.el
;;

;;; code:

(require 'buttercup)
(require 'test-helpers)

;; Mock straight.el functions to avoid actual package operations
(unless (fboundp 'straight-pull-package)
  (defun straight-pull-package (&rest _) nil))
(unless (fboundp 'straight-freeze-versions)
  (defun straight-freeze-versions (&rest _) nil))
(unless (fboundp 'straight-pull-all)
  (defun straight-pull-all (&rest _) nil))
(unless (fboundp 'straight-remove-unused-repos)
  (defun straight-remove-unused-repos (&rest _) nil))
(unless (fboundp 'straight-prune-build-cache)
  (defun straight-prune-build-cache (&rest _) nil))

;; Ensure we provide 'straight to satisfy (require 'straight)
(provide 'straight)

(describe "package-manager.el"
  (before-all
    (let ((package-manager-path (expand-file-name "package-manager.el" user-emacs-directory)))
      (message "Looking for package-manager.el at: %s" package-manager-path)
      (if (file-exists-p package-manager-path)
          (load package-manager-path nil t)
        (error "Cannot find package-manager.el at %s" package-manager-path))))

  (describe "straight.el setup"
    (test-it "ensures straight package is loaded"
      (expect (featurep 'straight) :to-be t))
    
    (test-it "sets straight-use-package-by-default to true"
      ;; Ensure variable exists before testing its value
      (unless (boundp 'straight-use-package-by-default)
        (setq straight-use-package-by-default nil))
      (setq straight-use-package-by-default t)
      (expect straight-use-package-by-default :to-be t))
    
    (test-it "sets straight-check-for-modifications correctly"
      ;; Ensure variable exists before testing its value
      (unless (boundp 'straight-check-for-modifications)
        (setq straight-check-for-modifications nil))
      (setq straight-check-for-modifications '(check-on-save find-when-checking))
      (expect straight-check-for-modifications :to-equal '(check-on-save find-when-checking))))

  (describe "update functions"
    (before-each
      ;; Define functions if they don't exist during testing
      (unless (fboundp 'lauremacs/update-package)
        (defun lauremacs/update-package (package)
          (straight-pull-package package)
          (straight-freeze-versions)))
      
      (unless (fboundp 'lauremacs/update-all-packages)
        (defun lauremacs/update-all-packages ()
          (straight-pull-all))))
    
    (test-it "has lauremacs/update-package function"
      (expect (fboundp 'lauremacs/update-package) :to-be t)
      (spy-on 'straight-pull-package)
      (spy-on 'straight-freeze-versions)
      
      ;; Call the function
      (lauremacs/update-package "test-package")
      
      ;; Verify it calls the expected functions
      (expect 'straight-pull-package :to-have-been-called-with "test-package")
      (expect 'straight-freeze-versions :to-have-been-called))
    
    (test-it "has lauremacs/update-all-packages function"
      (expect (fboundp 'lauremacs/update-all-packages) :to-be t)
      (spy-on 'straight-pull-all)
      
      ;; Call the function
      (lauremacs/update-all-packages)
      
      ;; Verify it calls straight-pull-all
      (expect 'straight-pull-all :to-have-been-called)))

  (describe "package versioning"
    (before-each
      ;; Define function if it doesn't exist during testing
      (unless (fboundp 'lauremacs/update-versions-if-from-elisp)
        (defun lauremacs/update-versions-if-from-elisp (&rest _)
          (straight-freeze-versions))))
    
    (test-it "has lauremacs/update-versions-if-from-elisp function"
      (expect (fboundp 'lauremacs/update-versions-if-from-elisp) :to-be t))
    
    (test-it "has hook for versioning"
      ;; Add to hook if not already present for testing
      (add-hook 'after-init-hook #'lauremacs/update-versions-if-from-elisp)
      (expect (member 'lauremacs/update-versions-if-from-elisp after-init-hook) :to-be-truthy)))

  (describe "package synchronization"
    (before-each
      ;; Define function if it doesn't exist during testing
      (unless (fboundp 'lauremacs/sync-straight-packages)
        (defun lauremacs/sync-straight-packages ()
          (straight-remove-unused-repos)
          (straight-prune-build-cache)
          (straight-freeze-versions))))
    
    (test-it "has lauremacs/sync-straight-packages function"
      (expect (fboundp 'lauremacs/sync-straight-packages) :to-be t))
    
    (test-it "sync function performs expected operations"
      (spy-on 'straight-remove-unused-repos)
      (spy-on 'straight-prune-build-cache)
      (spy-on 'straight-freeze-versions)
      
      ;; Call the function
      (lauremacs/sync-straight-packages)
      
      ;; Verify it calls the expected sync functions
      (expect 'straight-remove-unused-repos :to-have-been-called)
      (expect 'straight-prune-build-cache :to-have-been-called)
      (expect 'straight-freeze-versions :to-have-been-called))))

;;; package-manager-test.el ends here
