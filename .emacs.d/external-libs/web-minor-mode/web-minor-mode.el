;;
;; https://gist.github.com/bigodel/b49ae3e36902ad47f5a3fce4689a8b93
;;


(defvar-local web-minor--previous-electric-pair nil)
(put 'previous-major-mode 'permanent-local t)

(define-minor-mode web-minor-mode
  "Load `web-mode' as a `major-mode' and revert back to the previous.
This is done in order to load `web-mode''s variables and
functions properly in another `major-mode'.
The `web-minor-mode-map' inherits all bindings from
`web-mode-map', but it starts out as an empty map."
  :global nil
  :lighter " web"
  :keymap (make-sparse-keymap)
  (if (not web-minor-mode)
      (progn
        (unload-feature 'web-mode)
        (when web-minor--previous-electric-pair
          (electric-pair-mode t)))
    (package-install 'web-mode)
    (let ((previous-mode major-mode)
          (previous-change-mode-hooks change-major-mode-hook))
      ;; remove all `change-major-mode-hook', we don't want to change major
      ;; modes effectively, just pretend like we are
      (dolist (hook change-major-mode-hook)
        (remove-hook 'change-major-mode-hook hook :local))

      ;; dynamically bind `web-mode-hook' to nil because we don't want its hooks
      ;; to be run, since we are not actually using it as a major mode
      (dlet (web-mode-hook)
        (funcall-interactively #'web-mode))

      ;; gotta remove them again because they get reset when calling `web-mode'
      (dolist (hook change-major-mode-hook)
        (remove-hook 'change-major-mode-hook hook :local))

      ;; deactivate `electric-pair-mode', because `web-mode' already
      ;; handles it
      (setq web-minor--previous-electric-pair electric-pair-mode)
      (electric-pair-mode -1)

      ;; set the keymap to inherit the bindings from `web-mode-map'
      (set-keymap-parent web-minor-mode-map web-mode-map)
      ;; but don't keep the reload binding
      (define-key web-minor-mode-map "C-c C-r" nil)

      ;; `delay-mode-hooks' here acts by preveting the mode hooks to be run
      (delay-mode-hooks (funcall-interactively previous-mode))

      ;; reset the value of `change-major-mode-hook'
      (setq-local change-major-mode-hook previous-change-mode-hooks))
    ;; need to set it manually because it gets reset after changing major modes
    (setq web-minor-mode t)))


(provide 'web-minor-mode)
