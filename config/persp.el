;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; Perspective.el configs
;;



(use-package "persp-mode"
	:custom
  (persp-autokill-buffer-on-remove                              'kill)
	(persp-add-buffer-on-after-change-major-mode                  t)
  (persp-auto-resume-time                                       -1)
	(persp-add-buffer-on-after-change-major-mode-filter-functions (list 'lauremacs/persp-filter-buffers))
	(persp-common-buffer-filter-functions                         nil)
  (persp-is-ibc-as-f-supported                                  t)
  (persp-nil-name                                               "Default")
  (persp-reset-windows-on-nil-window-conf                       t)
  (persp-set-last-persp-for-new-frames                          t)
  (persp-set-ido-hooks                                          t)
	:init
	(persp-mode 1)
	(lauremacs-leader
		"l"  '(nil :which-key "perspective/layout")
		"la" '(persp-add-buffer   :which-key "add buffer")
		"ll" '(persp-frame-switch :which-key "switch to frame")
		"lx" '(persp-kill         :which-key "kill perspective")
		"lr" '(persp-rename       :which-key "rename perspective")))

