;;; -*- lexical-binding: t; -*-
;;
;; @author Laura Viglioni
;; 2025
;;
;; GNU Public License 3.0
;;
;; since: 0.1.0
;;
;; run-tests.el:
;; Run project tests.
;;

;;; code:

(setq lauremacs-test-directory "~/.emacs.d/test")
(add-to-list 'load-path lauremacs-test-directory)
(add-to-list 'load-path "~/.emacs.d/straight/build/buttercup/")

(require 'buttercup)
(require 'test-helpers)


;; Find and load all test files
(defvar buttercup-suits)
(setq buttercup-suites nil) ;; Clear any previous tests
(dolist (test-file (directory-files-recursively lauremacs-test-directory "-test.el$"))
  (load test-file nil))

;; Run tests
(buttercup-run)

;;; run-tests.el ends here
