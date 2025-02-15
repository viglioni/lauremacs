;;
;; @author Laura Viglioni
;; 2025
;;
;; GNU Public License 3.0
;;
;; since: 0.1.0
;;
;; core-packages.el:
;; Must have packages for the project.
;;

;;; code:

(require 'straight)

(use-package general
  :straight t
  :init
 (general-create-definer lauremacs-major-mode-leader
   :prefix "<f17>")
 (general-create-definer lauremacs-leader
   :prefix "<f19>"))

(use-package which-key
  :straight t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package buttercup
  :straight t)


;;; core-packages.el ends here.
