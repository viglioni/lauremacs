
;;
;; Theme
;;

(setq lauremacs-default-dark-theme 'transparent)
(lauremacs/theme-load 'system)

;;
;; Loading another packages
;;


(require 'web-search)
(require 'zap)

(use-package expand-region)

(add-hook 'prog-mode-hook
					(lambda ()
            (setq indent-tabs-mode nil)
            (flyspell-mode 1)))

;;
;; Load config files
;;

(defconst elisp-file-rx  (rx line-start
                             (+ (| "-" "." alphanumeric))
                             ".el"
                             line-end))

(let ((config-files (directory-files
										 lauremacs-config-dir-old
                     t
                     elisp-file-rx)))
  (mapcar 'load-file config-files))


;;
;; require private files
;;

(dolist (file (directory-files
               lauremacs-elisp-private-files
               nil
               elisp-file-rx))
  (fp/pipe file
    'file-name-sans-extension
    'intern
    'require-without-throw))

;; Load secret env variables
(require-without-throw 'env-private)

;; auto revert mode
(global-auto-revert-mode 1)

;;
;; Dealing with macos too many open files problem
;;
(defun file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))
(setq auto-revert-use-notify nil)

;;
;; Custom variables
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
