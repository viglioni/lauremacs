;;; -*- lexical-binding: t; -*-
;;
;; @author Laura Viglioni
;; 2025
;;
;; GNU Public License 3.0
;;
;; since: 0.1.0
;;
;; early-init-test.el:
;; tests for early-init.el
;;

;;; code:



(require 'buttercup)
(require 'test-helpers)

(describe "early-init.el"
  (before-all
    (load (expand-file-name "early-init.el" user-emacs-directory)))

  (describe "lauremacs/load"
    (test-it "loads files relative to user-emacs-directory"
      (spy-on 'load)
      (lauremacs/load "test.el")
      (expect 'load :to-have-been-called-with
              (expand-file-name "test.el" user-emacs-directory) nil))

    (test-it "respects noerror parameter"
      (spy-on 'load)
      (lauremacs/load "test.el" t)
      (expect 'load :to-have-been-called-with
              (expand-file-name "test.el" user-emacs-directory) t)))

  (describe "garbage collector settings"
    (test-it "sets initial gc threshold to 20MB"
      (expect gc-cons-threshold :to-equal (* 20 1024 1024)))

    (test-it "has hook to set gc threshold to 128MB after startup"
      (let ((hook-fn (seq-find (lambda (fn)
                                (string-match-p "gc-cons-threshold"
                                              (prin1-to-string fn)))
                              emacs-startup-hook)))
        (expect hook-fn :not :to-be nil)
        (funcall hook-fn)
        (expect gc-cons-threshold :to-equal (* 128 1024 1024)))))

  (describe "backup settings"
    (test-it "sets backup directory inside user-emacs-directory"
      (expect (cdr (assoc "." backup-directory-alist))
              :to-equal (concat user-emacs-directory "/backups")))))


;;; early-init-test.el ends here.
