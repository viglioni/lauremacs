(setq lauremacs-home-banners-dir (join-path lauremacs-home-page-dir "banners"))
(setq lauremacs-home-friday-gifs-dir (join-path lauremacs-home-banners-dir "fridays"))


;;
;; Banner
;;

;;;###autoload
(defun lauremacs-home--friday? ()
  "returns if today is friday. () -> bool"
  (equal "Fri" (format-time-string "%a")))

;;;###autoload
(defun lauremacs-home/choose-gif ()
  "Chooses randomly a gif path from `lauremacs-home-banners-dir' or if it is friday,
   from `lauremacs-home-friday-gifs-dir'
   ()->string"
  (let* ((dir (if (lauremacs-home--friday?)
                 lauremacs-home-friday-gifs-dir
                lauremacs-home-banners-dir))
         (gifs (directory-files dir t ".*\\.gif$"))
         (index (mod (random) (length gifs))))
    (nth index gifs)))



;;
;; Dashboard
;;

(setq dashboard-buffer-name "*lauremacs*")

(use-package page-break-lines)
(use-package all-the-icons)

(use-package dashboard
  :config
  (setq dashboard-items '((recents . 15)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "L A U R E M A C S")
  (setq dashboard-startup-banner  (lauremacs-home/choose-gif))
  (setq dashboard-footer-messages '("Made with love"))
  (setq dashboard-footer-icon (all-the-icons-faicon "heart"
                                                     :height 1.1
                                                     :v-adjust -0.05
                                                     :face 'font-lock-keyword-face))
  (dashboard-setup-startup-hook)
  :init
  (add-hook 'emacs-startup-hook 'dashboard-refresh-buffer)
  )
