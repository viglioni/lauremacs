;;; lauremacs-screen-modes.el --- some screen modes like pair programming, transparency...

(require 'linum)
(require 'helm)

;;
;; Pair programming mode
;;


;;; Commentary:
;; 

;;; Code:

(defvar lauremacs-state//pair-programming? nil)

;;;###autoload
(defun lauremacs/enable-pair-programming ()
  "Change some display configs to enhance sharing screen experience."
  (interactive)
	(setq lauremacs-state//pair-programming? t)
  (set-face-attribute 'default nil :height 175)
  (global-linum-mode 1)
  (with-current-buffer " *NeoTree*"
    (setq-local linum-mode nil))
  (neotree-toggle)
  (neotree-toggle))

;;;###autoload
(defun lauremacs/disable-pair-programming ()
  "Change some display configs to enhance screen experience."
  (interactive)
	(setq lauremacs-state//pair-programming? nil)
  (set-face-attribute 'default nil :height 150)
  (global-linum-mode 0))

;;;###autoload
(defun lauremacs/toggle-pair-programming ()
	"Toggle between pair programming mode and norma mode."
	(interactive)
	(if lauremacs-state//pair-programming?
			(lauremacs/disable-pair-programming)
		(lauremacs/enable-pair-programming)))

;;
;; Themes
;;

;;;###autoload
(defun lauremacs--theme-load-light ()
	"Load `spacemacs-light'."
	(interactive)
	(load-theme 'spacemacs-light t)
	(lauremacs/set-transparency 100))

;;;###autoload
(defun lauremacs--theme-load-dark ()
	"Load `spacemacs-dark'."
	(interactive)
	(load-theme 'spacemacs-dark t)
	(lauremacs/set-transparency 100))

;;;###autoload
(defun lauremacs--theme-load-transparent ()
	"Load a transparent version of `spacemacs-dark'."
	(interactive)
	(load-theme 'spacemacs-dark t)
	(lauremacs/set-transparency)
	(set-face-attribute 'fringe  nil :background "black")
	(set-face-attribute 'default nil :background "black"))

;;;###autoload
(defun lauremacs--theme-load-grey-ink ()
	"Load a transparent version of `grey-ink'."
	(interactive)
	(load-theme 'greyink t)
	(lauremacs/set-transparency 100)

  ;;
  ;; Flycheck colours
  ;;
  (defconst greyink-warning-colour "#c0c0c0")
  (defconst greyink-error-colour   "#848884")
  (defconst greyink-info-colour    "#E5E4E2")

  ;; info
  (set-face-attribute 'flycheck-info nil
                      :foreground "black"
                      :underline (list :color greyink-info-colour))
  (set-face-attribute 'flycheck-fringe-info nil
                      :background greyink-error-colour
                      :foreground "white")

  ;; error
  (set-face-attribute 'flycheck-error nil
                      :foreground "black"
                      :underline (list :color greyink-error-colour))
  (set-face-attribute 'flycheck-fringe-error nil
                      :background greyink-error-colour
                      :foreground "white")
  ;; warning
  (set-face-attribute 'flycheck-warning nil
                      :foreground "black"
                      :underline (list :color greyink-warning-colour))
  (set-face-attribute 'flycheck-fringe-warning nil
                      :background greyink-warning-colour
                      :foreground "white"))


;;;###autoload
(defun lauremacs/theme-load (theme)
	"Load THEME.  THEME should be `light', `dark', `grey-ink' or `transparent'."
	(cond ((equal theme 'light)       (lauremacs--theme-load-light))
				((equal theme 'dark)        (lauremacs--theme-load-dark))
				((equal theme 'transparent) (lauremacs--theme-load-transparent))
        ((equal theme 'grey-ink)    (lauremacs--theme-load-grey-ink))
				(t (throw-if t "THEME should be `light', `dark', `grey-ink' or `transparent'.")))
	(funcall major-mode))


;;;###autoload
(defun lauremacs/choose-theme ()
	"Choose theme."
	(interactive)
	(helm
	 :sources (helm-build-sync-source "themes"
							:candidates '(("dark"        . dark)
														("light"       . light)
                            ("grey"        . grey-ink)
														("transparent" . transparent))
							:action 'lauremacs/theme-load)
	 :prompt "Choose a theme to load:"))


;;
;; Transparency
;;

(defvar lauremacs-state//opacity 85
	"The value used as opacity when transparency is enabled.")

;;;###autoload
(defun lauremacs/toggle-transparency ()
	"Toggle transparency between alpha = `lauremacs-state//opacity' and 100."
	(interactive)
	(let* ((frame (selected-frame))
				 (current-alpha (or (car (frame-parameter frame
																								  'alpha)) 100))
				 (new-alpha (if (= current-alpha 100) lauremacs-state//opacity 100)))
		(set-frame-parameter frame 'alpha
												 (cons new-alpha new-alpha))))

;;;###autoload
(defun lauremacs/set-transparency (&optional opacity)
	"Set transparency to OPACITY.
The default value is `lauremacs-state//opacity'."
	(interactive "nInsert alpha from 20 to 100: ")
	(throw-unless (if (bool opacity) (numberp opacity) t) "OPACITY should be a number")
	(let* ((alpha (or opacity lauremacs-state//opacity))
				 (new-alpha (min (max alpha 20) 100))
				 (frame (selected-frame)))
		(set-frame-parameter frame 'alpha
												 (cons new-alpha new-alpha))
		(message (format "Transparency set to %s" new-alpha))))

(provide 'lauremacs-screen-modes)

;;; lauremacs-screen-modes.el ends here
