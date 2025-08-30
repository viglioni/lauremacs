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

(require 'straight)

(use-package l
  :straight (l :type git :host github :repo "viglioni/l-el" :branch "latest-release")
  :mode ("\\.el\\'" . l-mode)
  :custom
  (l-syntax t)
  :config
  (l-syntax-advices))

(use-package general
  :straight t
  :defer t
  :init
  (general-create-definer lauremacs-major-mode-leader
    :prefix "<f17>")
  (general-create-definer lauremacs-leader
    :prefix "<f19>"))

(use-package which-key
  :straight t
  :defer t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package buttercup
  :straight t
  :defer t)


;;; core-packages.el ends here.
