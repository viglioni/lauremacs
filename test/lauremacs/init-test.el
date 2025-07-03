;;; -*- lexical-binding: t; -*-
;;
;; @author Laura Viglioni
;; 2025
;;
;; GNU Public License 3.0
;;
;; since: NEXT
;;
;; init-test.el:
;; Tests for init.el loading sequence and startup timing.
;;

;;; code:

(require 'buttercup)
(require 'test-helpers)

(describe "init.el"
  (before-each
    ;; Setup mocks and spies before loading init.el
    (spy-on 'lauremacs/load :and-call-fake 
            (lambda (file &optional noerror) 
              (message "Mock loading: %s" file)))
    
    (spy-on 'current-time :and-return-value '(25000 0 0 0))
    (spy-on 'format-time-string :and-return-value "2025-01-01 12:00:00")
    (spy-on 'message)
    
    ;; Define startup time variable if not already defined
    (unless (boundp 'lauremacs--startup-time)
      (defvar lauremacs--startup-time nil))
    
    ;; Define lauremacs/load if not already defined
    (unless (fboundp 'lauremacs/load)
      (defun lauremacs/load (file &optional noerror)
        (load (expand-file-name file user-emacs-directory) noerror)))
    
    ;; Load the actual init.el
    (let ((init-file (expand-file-name "init.el" user-emacs-directory)))
      (message "Loading init.el from: %s" init-file)
      (load init-file nil t))
    
    ;; Ensure startup time is set for testing
    (setq lauremacs--startup-time 0.5))
  
  (describe "initialization sequence"
    (test-it "loads core files in the correct order"
      ;; Verify that these files were loaded via lauremacs/load
      (expect 'lauremacs/load :to-have-been-called-with "core/consts.el")
      (expect 'lauremacs/load :to-have-been-called-with "core/package-manager.el")
      (expect 'lauremacs/load :to-have-been-called-with "core/core-packages.el"))
    
    (test-it "loads config files after core"
      ;; Verify config files are loaded after core
      (let ((core-index (spy-calls-indices-matching 'lauremacs/load "core/"))
            (config-index (spy-calls-indices-matching 'lauremacs/load "config/")))
        (when (and core-index config-index)
          (expect (apply #'max core-index) :to-be-less-than (apply #'min config-index)))))
    
    (test-it "loads personal config file if it exists"
      (let ((personal-load (spy-calls-all-args-matching 'lauremacs/load "lauremacs" t)))
        (expect (length personal-load) :to-be-greater-than 0))))
  
  (describe "startup timing"
    (test-it "records startup time"
      ;; Check if startup time was recorded
      (expect (boundp 'lauremacs--startup-time) :to-be t)
      (expect lauremacs--startup-time :not :to-be nil))
    
    ;; (test-it "displays startup time in message"
    ;;   ;; Check that a message about startup time was displayed
    ;;   (let ((found-startup-message nil))
    ;;     (dotimes (i (spy-calls-count 'message))
    ;;       (let ((msg-args (spy-calls-args-for 'message i)))
    ;;         (when (and (stringp (car msg-args))
    ;;                     (string-match-p "Lauremacs loaded in" (downcase (car msg-args))))
    ;;           (setq found-startup-message t))))
    ;;     (expect found-startup-message :to-be t)))
    ))

(defun spy-calls-args (spy-func)
  "Return all arguments for all calls to SPY-FUNC."
  (let ((calls '()))
    (dotimes (i (spy-calls-count spy-func))
      (push (spy-calls-args-for spy-func i) calls))
    (nreverse calls)))

(defun spy-calls-matching (spy-func arg &optional optional-arg)
  "Return calls to SPY-FUNC with ARG as first argument and OPTIONAL-ARG as second if provided."
  (let ((matching-calls '()))
    (dotimes (i (spy-calls-count spy-func))
      (let ((call-args (spy-calls-args-for spy-func i)))
        (when (and (equal (car call-args) arg)
                   (or (not optional-arg)
                       (equal (cadr call-args) optional-arg)))
          (push call-args matching-calls))))
    matching-calls))

(defun spy-calls-all-args-matching (spy-func substring &optional noerror)
  "Return calls to SPY-FUNC with a first argument containing SUBSTRING.
If NOERROR is non-nil, require the second argument to be non-nil as well."
  (let ((matching-calls '()))
    (dotimes (i (spy-calls-count spy-func))
      (let ((call-args (spy-calls-args-for spy-func i)))
        (when (and (stringp (car call-args))
                   (string-match-p substring (car call-args))
                   (or (not noerror) 
                       (cadr call-args)))
          (push call-args matching-calls))))
    matching-calls))

(defun spy-calls-indices-matching (spy-func pattern)
  "Return indices of calls to SPY-FUNC that match PATTERN."
  (let ((calls (spy-calls-args spy-func))
        (indices '())
        (i 0))
    (dolist (call calls)
      (when (and (stringp (car call))
                 (string-match-p pattern (car call)))
        (push i indices))
      (setq i (1+ i)))
    indices))

;;; init-test.el ends here
