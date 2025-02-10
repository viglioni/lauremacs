;;
;; Early configuration
;;

(defun join-path (path filename)
  "Concat PATH and FILENAME.  Add \"/\" to the end of the path if necessary."
  (concat path (if (string-match-p "/$" path) "" "/") filename))

(defconst lauremacs-dir "~/lauremacs")
(defconst lauremacs-config-dir-old (join-path lauremacs-dir "config"))
(defconst lauremacs-d-dir (join-path lauremacs-dir ".emacs.d"))
(defconst lauremacs-config-dir (join-path lauremacs-d-dir "config"))
(defconst lauremacs-core-dir (join-path lauremacs-d-dir "core"))
(defconst laurisp-dir (join-path lauremacs-d-dir "laurisp"))
(defconst laurisp-core-dir (join-path laurisp-dir "laurisp-core"))
(defconst lauremacs-home-page-dir (join-path lauremacs-core-dir "lauremacs-home-page"))
(setq user-emacs-directory lauremacs-d-dir)
(setq custom-theme-directory (join-path lauremacs-d-dir "custom-themes"))
(defconst lauremacs-buffer-name "*lauremacs*")
(defconst lauremacs-external-libs-dir (join-path lauremacs-d-dir "external-libs"))
(defconst lauremacs-internal-libs-dir (join-path lauremacs-d-dir "internal-libs"))
(defconst lauremacs-private-files-dir "~/private-files/emacs-files")
(defconst lauremacs-agenda-dir (join-path lauremacs-private-files-dir "org-agenda"))
(defconst lauremacs-elisp-private-files (join-path lauremacs-private-files-dir "elisp-files"))
(defconst lauremacs-org-roam-files "~/org-roam-files")

(with-eval-after-load "warnings" ;; avoid warning flood of compiled functions
  (setq warning-minimum-level :error)) 

(setq backup-directory-alist
          `(("." . ,(join-path user-emacs-directory "backups"))))

;;;###autoload
(defun lauremacs/reload-init ()
	"Reload init.el."
	(interactive)
	(load-file (join-path user-emacs-directory "init.el")))

;; load early-init.el
(load-file (join-path user-emacs-directory "early-init.el"))



;;
;; straight.el
;;

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; (setq package-enable-at-startup nil)

;;
;; Packages
;;


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

(setq use-package-always-ensure t)

(dolist (pkg '(
               bind-key
							 helm
							 general
							 which-key
							 expand-region
							 spacemacs-theme
               uuidgen
							 window-purpose
               all-the-icons
               dashboard
               go-translate
               web-mode
               exec-path-from-shell
               ))
	(add-to-list 'package-selected-packages pkg))

(package-install-selected-packages)

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package general
  :init
  (general-create-definer lauremacs-major-mode-leader
    :prefix "<f17>")
  (general-create-definer lauremacs-leader
    :prefix "<f19>"))




;;
;; Adds relevant dirs to load path
;;

(dolist (dir (list laurisp-dir
									 lauremacs-core-dir
									 lauremacs-private-files-dir
									 lauremacs-external-libs-dir
									 lauremacs-internal-libs-dir))
  (let ((default-directory dir))
    (normal-top-level-add-subdirs-to-load-path)))



;;
;; Load lauremacs core
;;

(dolist (file (directory-files laurisp-core-dir t "^l-[a-zA-Z0-9-]+\\.el$"))
  (if (load file nil nil t) (message (format "loaded %s" file))
    (message (format "failed load: %s" file))))


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




;;
;; Load configs
;;
(fp/pipe (directory-files lauremacs-config-dir t "^l-.*\\.el$")
  (fp/map 'load-file))
(fp/pipe (directory-files lauremacs-config-dir t "^_[a-zA-Z0-9]+\\.el$")
  (fp/map 'load-file))



