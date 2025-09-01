;;
;; @author Laura Viglioni
;; 2025
;;
;; GNU Public License 3.0
;;
;; since: NEXT
;;
;; core-packages.el:
;; Must have packages for the project.
;;

;;; code:

(config-package l
  :mode ("\\.el\\'" . l-mode)
  :custom
  (l-syntax t)
  :config
  (l-syntax-advices))

(config-package general
  :defer t
  :init
  (general-create-definer lauremacs-major-mode-leader
    :prefix "<f17>")
  (general-create-definer lauremacs-leader
    :prefix "<f19>"))

(config-package which-key
  :defer t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(config-package buttercup
  :defer t)


;;; core-packages.el ends here.
