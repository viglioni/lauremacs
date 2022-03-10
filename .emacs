;;
;; Constants
;;

(setq private-files-dir "~/private-files/emacs-files")
(add-to-list 'load-path private-files-dir)

;;
;; Theme
;;

(lauremacs/theme-load 'transparent)


;;
;; Loading another packages
;;

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package general
  :init
  (general-create-definer lauremacs-major-mode-leader
    :prefix "<f17>")
  (general-create-definer lauremacs-leader
    :prefix "<f19>")
  (lauremacs-leader
    "s" '(nil :which-key "search")
    "g" '(nil :which-key "git")
    "a" '(nil :which-key "applications")
		"c" '(nil :which-key "coding")))


(use-package expand-region
  :init
  (lauremacs-leader
    "v" '(er/expand-region :which-key "expand region")))



;;
;; Load config files
;;

(let ((config-files (directory-files
										 lauremacs-config-dir
                     t (rx (and (+ (| alphanumeric "." "-"))
                                ".el"
                                line-end)))))
  (mapcar 'load-file config-files))

;; Load secret env variables
(require-without-throw 'env-private)

;;
;; exec path from shell
;;
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

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
