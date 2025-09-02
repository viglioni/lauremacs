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

(switch-to-buffer "*Messages*")

(lauremacs/load "core/consts.el")
(lauremacs/load "core/package-manager.el")
(lauremacs/load ".lauremacs" :dont-throw)
(lauremacs/load "test/test-helpers.el")
(lauremacs/load "config/shortcuts.el")


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


