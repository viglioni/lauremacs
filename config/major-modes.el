;;
;; Modes that require few configuration
;;


;;
;; yaml
;;

(use-package yaml-mode
	:mode "\\.ya?ml\\'"
	:hook ((yaml-mode . highlight-indentation-mode)
				 (yaml-mode . prettier-js-mode)))


;;
;; Magit
;;

(use-package magit
  :init
  (lauremacs-leader
    "gs" '(magit-status :which-key "magit status")))
