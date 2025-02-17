;;
;; @author Laura Viglioni
;; 2025
;;
;; GNU Public License 3.0
;;
;; since: 0.1.0
;;
;; init.el:
;; Defines basic configurations and loads everything else.
;;

;;; code:
(message "init")
(lauremacs/load "core/consts.el")
(lauremacs/load "./core/package-manager.el")
(lauremacs/load "./core/core-packages.el")

;(lauremacs/load ".lauremacs" )

(lauremacs/load "./test/test-helpers.el")

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                    (format "%.2f seconds"
                            (float-time
                             (time-subtract after-init-time lauremacs-start-time)))
                    gcs-done)))

;;; init.el ends here.


