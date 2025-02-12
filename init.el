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

(lauremacs/load "core/consts.el")
(lauremacs/load "./core/package-manager.el")
(lauremacs/load "./core/core-packages.el")

(lauremacs/load ".lauremacs" :dont-throw-if-does-not-exist)

;;; init.el ends here.


