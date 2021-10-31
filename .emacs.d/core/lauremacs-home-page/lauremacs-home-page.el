;;; lauremacs-home-page.el --- Configs and functions related to home page

;;; Commentary:
;; 

;;; Code:

(defconst lauremacs-home-banners-dir (join-path lauremacs-home-page-dir "banners"))
(defconst lauremacs-home-friday-gifs-dir (join-path lauremacs-home-banners-dir "fridays"))


;;
;; Banner
;;

;;;###autoload
(defun lauremacs-home--friday? ()
  "Return if today is friday.  () -> bool."
  (equal "Fri" (format-time-string "%a")))

;;;###autoload
(defun lauremacs-home/choose-gif ()
  "Chooses randomly a gif path from `lauremacs-home-banners-dir'.
If today is a friday, it gets from `lauremacs-home-friday-gifs-dir'.
\()->string"
  (let* ((dir (if (lauremacs-home--friday?)
                 lauremacs-home-friday-gifs-dir
                lauremacs-home-banners-dir))
         (gifs (directory-files dir t ".*\\.gif$"))
         (index (mod (random) (length gifs))))
    (nth index gifs)))



;;
;; Dashboard
;;

(use-package page-break-lines)
(use-package all-the-icons)

(use-package dashboard
  :config
  (setq dashboard-buffer-name lauremacs-buffer-name)
  (setq dashboard-items '((recents . 15)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "L A U R E M A C S")
  (setq dashboard-startup-banner  (if (display-graphic-p)
																			(lauremacs-home/choose-gif)
																		(join-path lauremacs-home-banners-dir "lauremacs-text-banner.txt")))
  (setq dashboard-footer-messages '("Made with love"))
  (setq dashboard-footer-icon
				(if (display-graphic-p)
						(all-the-icons-faicon "heart"
																	:height 1.1
																	:v-adjust -0.05
																	:face 'font-lock-keyword-face)
					"♥"))
  (dashboard-setup-startup-hook)
  :init
  (add-hook 'emacs-startup-hook 'dashboard-refresh-buffer))

(provide 'lauremacs-home-page)

;;; lauremacs-home-page.el ends here
