;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; Perspective.el configs
;;



(with-eval-after-load "persp-mode"
    (defvar persp-mode-projectile-bridge-before-switch-selected-window-buffer nil)

    (persp-def-auto-persp "projectile"
      :parameters '((dont-save-to-file . t)
                    (persp-mode-projectile-bridge . t))
      :hooks '(projectile-before-switch-project-hook
               projectile-after-switch-project-hook
               projectile-find-file-hook
               find-file-hook)
      :dyn-env '((after-switch-to-buffer-adv-suspend t))
      :switch 'frame
      :predicate
      #'(lambda (buffer &optional state)
          (if (eq 'projectile-before-switch-project-hook
                  (alist-get 'hook state))
              state
            (and
             projectile-mode
             (buffer-live-p buffer)
             (buffer-file-name buffer)
             ;; (not git-commit-mode)
             (projectile-project-p)
             (or state t))))
      :get-name
      #'(lambda (state)
          (if (eq 'projectile-before-switch-project-hook
                  (alist-get 'hook state))
              state
            (push (cons 'persp-name
                        (concat "p) "
                                (with-current-buffer (alist-get 'buffer state)
                                  (projectile-project-name))))
                  state)
            state))
      :on-match
      #'(lambda (state)
          (let ((hook (alist-get 'hook state))
                (persp (alist-get 'persp state))
                (buffer (alist-get 'buffer state)))
            (case hook
              (projectile-before-switch-project-hook
               (let ((win (if (minibuffer-window-active-p (selected-window))
                              (minibuffer-selected-window)
                            (selected-window))))
                 (when (window-live-p win)
                   (setq persp-mode-projectile-bridge-before-switch-selected-window-buffer
                         (window-buffer win)))))

              (projectile-after-switch-project-hook
               (when (buffer-live-p
                      persp-mode-projectile-bridge-before-switch-selected-window-buffer)
                 (let ((win (selected-window)))
                   (unless (eq (window-buffer win)
                               persp-mode-projectile-bridge-before-switch-selected-window-buffer)
                     (set-window-buffer
                      win persp-mode-projectile-bridge-before-switch-selected-window-buffer)))))

              (find-file-hook
               (setcdr (assq :switch state) nil)))
            (if (case hook
                  (projectile-before-switch-project-hook nil)
                  (t t))
                (persp--auto-persp-default-on-match state)
              (setcdr (assq :after-match state) nil)))
          state)
      :after-match
      #'(lambda (state)
          (when (eq 'find-file-hook (alist-get 'hook state))
            (run-at-time 0.5 nil
                         #'(lambda (buf persp)
                             (when (and (eq persp (get-current-persp))
                                        (not (eq buf (window-buffer (selected-window)))))
                               ;; (switch-to-buffer buf)
                               (persp-add-buffer buf persp t nil)))
                         (alist-get 'buffer state)
                         (get-current-persp)))
          (persp--auto-persp-default-after-match state))))

(with-eval-after-load "persp-mode"
	(add-to-multiple-hooks
	 #'(lambda (b) (string-prefix-p "*" (buffer-name b)))
	 'persp-common-buffer-filter-functions
	 'persp-add-buffer-on-after-change-major-mode-filter-functions))


(use-package "persp-mode"
	:custom
	;;(setq wg-morph-on nil) ;; switch off animation
  (persp-autokill-buffer-on-remove 'kill-weak)
	:init
	(setq persp-add-buffer-on-after-change-major-mode nil
        persp-auto-resume-time  -1
        persp-is-ibc-as-f-supported t
        persp-nil-name lauremacs-default-layout-name
        persp-reset-windows-on-nil-window-conf t
        persp-set-last-persp-for-new-frames t ;; ?
        persp-set-ido-hooks t)

	(persp-mode 1)
	(lauremacs-leader
		"l" '(nil :which-key "perspective/layout")
		"ll" '(persp-frame-switch :which-key "switch to frame")
		"lx" '(persp-kill :which-key "kill perspective")))

