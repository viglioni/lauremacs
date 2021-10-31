;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; Perspective.el configs
;;

;;;###autoload
(defun lauremacs/persp-go-to-home (&rest args)
	"Switch to home buffer when creating new perspective.
ARGS defined only to have the form needed to be added `persp-before-switch-functions'."
	(switch-to-buffer lauremacs-buffer-name))


(defadvice persp-add-new (before go-to-home-buffer)
	(switch-to-buffer lauremacs-buffer-name))

(use-package "persp-mode"
	:custom
	;;(setq wg-morph-on nil) ;; switch off animation
  (persp-autokill-buffer-on-remove 'kill-weak)
	:init
	(persp-mode 1)
	(global-set-key (kbd "C-x b") #'persp-switch-to-buffer)
  (global-set-key (kbd "C-x k") #'persp-kill-buffer)
	(lauremacs-leader
		"l" '(:keymap persp-key-map :package persp-mode))
	(add-to-list 'persp-before-switch-functions 'lauremacs/persp-go-to-home))

