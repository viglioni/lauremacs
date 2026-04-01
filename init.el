;;; -*- lexical-binding: t; -*-
;;
;; @author Laura Viglioni
;; 2025
;;
;; GNU Public License 3.0
;;
;; since: NEXT
;;
;; init.el:
;; Defines basic configurations and loads everything else.
;;

;;; code:

(require 'cl-lib)

;;
;; add to loadpath
;;
(cl-loop for lib-path in
         (delete  (lauremacs/path lauremacs-libs readme.org)
                  (directory-files
                   (lauremacs/path lauremacs-libs) t "[a-z]"))
         do (add-to-list 'load-path lib-path))

;;
;; core loads
;;

(lauremacs/load core consts)
(lauremacs/load core package-manager)
(lauremacs/load core windows-and-buffers)

;;
;; config loads
;;
(lauremacs/load config magit)
(lauremacs/load config coding)
(lauremacs/load config org-config)
(lauremacs/load config compilation)
(lauremacs/load config ai)
(lauremacs/load config shortcuts)


;;
;; Run after startup
;;

(add-hook
 'emacs-startup-hook
 (lambda ()
  (global-auto-revert-mode)
   ;; Print lauremacs started time
   (message "Lauremacs loaded in %s with %d garbage collections."
            (format "%.2f seconds"
                    (float-time
                     (time-subtract after-init-time lauremacs-start-time)))
            gcs-done)))

;;; init.el ends here.

