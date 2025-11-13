;;; windows-and-buffers.el --- Helper functions to navigate buffers/windows -*- lexical-binding: t; l-syntax: t -*-
;;
;; @author Laura Viglioni
;; 2025
;;
;; GNU Public License 3.0
;;
;; since: NEXT
;;
;; helper functions to navigate buffers/windows
;;

;;; Commentary:

;; todo create single window function
;; todo window layouts
;; docs

;;; Code:

(require 'helm-buffers)

;;;###autoload
(defun wb/switch-to-last-buffer ()
  ;; todo use pipe
  (interactive)
  (switch-to-buffer
   (car
    (seq-filter
     (lambda (b) (not (string-match-p "*" (buffer-name b))))
     (cdr
      (if (projectile-project-p) (projectile-project-buffers) (buffer-list)))))))



;;;###autoload
(defun wb/switch-buffers ()
  (interactive)
  (if (projectile-project-p)
      (helm :sources '(helm-source-projectile-buffers-list
                       ;; bug: only works after run once
                       helm-source-buffers-list)
            :buffer (concat "*helm projectile: " (projectile-project-name) "*")
            :truncate-lines helm-buffers-truncate-lines
            :prompt (projectile-prepend-project-name "Switch to buffer: "))
    (helm-buffers-list)))

;;;###autoload
(defun wb/open-emacs-config-in-posframe ()
  "Open a file from .emacs.d in a posframe."
  (interactive)
  (let* ((default-directory (expand-file-name "~/.emacs.d/"))
         (helm-projectile-fuzzy-match t))
    (helm :sources (helm-build-sync-source "Emacs config files"
                     :candidates (lambda ()
                                   (with-temp-buffer
                                     (hack-dir-local-variables-non-file-buffer)
                                     (let ((projectile-project-root default-directory))
                                       (projectile-current-project-files))))
                     :fuzzy-match t
                     :action (lambda (file)
                               (let* ((file-path (expand-file-name file default-directory))
                                      (buffer (find-file-noselect file-path))
                                      (posframe-buffer-name (format " *emacs-config-posframe: %s*"
                                                                    (file-name-nondirectory file))))
                                 ;; Create or update the posframe buffer with file contents
                                 (with-current-buffer (get-buffer-create posframe-buffer-name)
                                   (erase-buffer)
                                   (insert-buffer-substring buffer)
                                   (setq-local buffer-file-name file-path)
                                   (set-auto-mode)
                                   (set-buffer-modified-p nil)
                                   (local-set-key (kbd "C-c C-c")
                                                  (lambda ()
                                                    (interactive)
                                                    (posframe-delete posframe-buffer-name))))
                                 ;; Show the posframe
                                 (when (posframe-workable-p)
                                   (posframe-show posframe-buffer-name
                                                  :poshandler #'posframe-poshandler-frame-center
                                                  :cursor 'box
                                                  :width 100
                                                  :height 30
                                                  :border-width 2
                                                  :border-color (face-attribute 'default :foreground nil t)
                                                  :internal-border-width 1
                                                  :internal-border-color (face-attribute 'fringe :background nil t)
                                                  :accept-focus t
                                              )))))
          :buffer "*helm emacs.d*"
          :prompt "Emacs config file: ")))
 
(provide 'windows-and-buffers)
;;; windows-and-buffers.el ends here

 
