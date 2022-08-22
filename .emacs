
;;
;; Theme
;;

(lauremacs/theme-load 'light)


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
	(require 'web-search)
  (lauremacs-leader
    "g"   '(nil															:which-key "git")
    "a"   '(nil															:which-key "applications")
		"c"   '(nil															:which-key "coding")
		"s"   '(nil															:which-key "search")
		"sw"  '(nil															:which-key "web search")
		"sww" '(web-search											:which-key "web-search")
		"swg" '(web-search-google								:which-key "google search")
		"swd" '(web-search-duckduckgo						:which-key "duckduckgo search")
		"swb" '(web-search-brave								:which-key "brave search")
		"swy" '(web-search-youtube							:which-key "youtube search")
		"x"		'(nil															:which-key "words")
		"xa"	'(nil															:which-key "align")
		"xag" '(lauremacs-align-general-sexp		:which-key "align general.el statements")
		"xat" '(lauremacs-align-region-as-table :which-key "align region as table")))


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

;; auto revert mode
(global-auto-revert-mode 1)

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
