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

;;
;; core loads
;;

(lauremacs/load "core/consts")
(lauremacs/load "core/package-manager")
(lauremacs/load "core/windows-and-buffers")

;;
;; config loads
;;

(lauremacs/load "config/coding")
(lauremacs/load "config/org-config")
(lauremacs/load "config/compilation")
(lauremacs/load "config/shortcuts")
(lauremacs/load-user-config)


;;
;; Run after startup
;;

(add-hook
 'emacs-startup-hook
 (lambda ()
   ;; Print lauremacs started time
   (message "Lauremacs loaded in %s with %d garbage collections."
            (format "%.2f seconds"
                    (float-time
                     (time-subtract after-init-time lauremacs-start-time)))
            gcs-done)))

;;; init.el ends here.

