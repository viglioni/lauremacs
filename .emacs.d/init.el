;; *** REQUIRING PACKAGE, SETTING REPOS, REFRESHING SHOULD BE FIRST EMACS DO.
;;
;; PACKAGES
;;

;; INITIALIZE PACKAGE SOURCES
(require 'package)

;; ** SETTING GC IS A GOOD IDEA HERE.
;;
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms

;; ** INSTALL ALL PACKAGES in custom.el/package-selected-packages
(dolist (package package-selected-packages)
    (when (not (package-installed-p package))
      (package-install package)))

(require 'use-package) ;; use-package has a lot of cool features: https://github.com/jwiegley/use-package

;; USE-PACKAGE IS GOOD TO MANAGE PACKAGES NOT SO MUCH DOWNLOADING THOSE, A GOOD WAY IS W/ STRAIGHT
;; https://github.com/raxod502/straight.el
;;
;; (setq use-package-always-ensure t)

;; CONST

(defconst *lauremacs-dir* "~/lauremacs") ;; const as in most langs

;; LOAD FILES INDIVIDUALLY

;; * PATH
(push (expand-file-name "lisp" user-emacs-directory)
      load-path)

(require 'init-utils) ;; auxiliar utils functions ...
(require 'init-theme) ;; theme/modeline and the likes

;; ;; Load config files
;; ;;
;; (mapcar 'load-file
;;         (directory-files
;;          (concat *lauremacs-dir* "/config")
;;          t ".*\\.el$"))

;; (setq user-emacs-directory (join-path *lauremacs-dir ".emacs.d"))
;; (load-file (join-path user-emacs-directory "early-init.el"))
