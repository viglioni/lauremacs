;;; lauremacs-screen-modes.el --- some screen modes like pair programming, transparency...

(require 'linum)
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
;; Theme toggle
;;


;;;###autoload
(defun lauremacs--theme-get-current ()
	"Get current theme."
	(car custom-enabled-themes))

;;;###autoload
(defun lauremacs--theme-get-other ()
	"Get the other spacemacs theme."
	(if (equal 'spacemacs-light (lauremacs--theme-get-current))
			'spacemacs-dark
		'spacemacs-light))

;;;###autoload
(defun lauremacs/toggle-spacemacs-theme ()
	"Toggle between `spacemacs-light' and `spacemacs-dark'."
	(interactive)
	(load-theme (lauremacs--theme-get-other) t))

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
	(throw-unless (numberp opacity) "OPACITY should be a number")
	(let* ((alpha (or opacity lauremacs-state//opacity))
				 (new-alpha (min (max alpha 20) 100))
				(frame (selected-frame)))
		(set-frame-parameter frame 'alpha
												 (cons new-alpha new-alpha))
		(message (format "Transparency set to %s" new-alpha))))

(provide 'lauremacs-screen-modes)

;;; lauremacs-screen-modes.el ends here
