;;
;; Early configuration
;;

(defun join-path (path filename)
  "concat path and file. Adds '/' to the end of the path if necessary"
  (concat path (if (string-match-p "/$" path) "" "/") filename))

(setq lauremacs-dir "~/lauremacs")
(setq lauremacs-d-dir (join-path lauremacs-dir ".emacs.d"))
(setq lauremacs-core-dir (join-path lauremacs-d-dir "core"))
(setq laurisp-dir (join-path lauremacs-d-dir "laurisp"))
(setq lauremacs-home-page-dir (join-path lauremacs-core-dir "lauremacs-home-page"))
(setq user-emacs-directory lauremacs-d-dir)
(setq lauremacs-buffer-name "*lauremacs*")
(setq lauremacs-external-libs-dir (join-path lauremacs-d-dir "external-libs"))

;; load early-init.el
(load-file (join-path user-emacs-directory "early-init.el"))


;;
;; Packages
;;

;; Initialize package sources
(require 'package)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;
;; Adds relevant dirs to load path
;;

(dolist (dir (list laurisp-dir
		   lauremacs-core-dir
		   lauremacs-external-libs-dir))
  (let ((default-directory dir))
    (normal-top-level-add-subdirs-to-load-path)))



;;
;; Load lauremacs core
;;

(require 'functional)
(require 'laurisp-core)

;; loads every lib inside `lauremacs-core-dir'
(let* ((lib-dirs (seq-filter
		 'file-directory-p
		 (directory-files lauremacs-core-dir t (rx (| alpha)))))
       (lib-names (mapcar
		   (lambda (dir) (replace-regexp-in-string ".*/" "" dir))
		   lib-dirs)))
  (dolist (lib lib-names)
    (require (intern lib))))



;;
;; Load personal file
;;


(load-file (concat lauremacs-dir "/.emacs"))




