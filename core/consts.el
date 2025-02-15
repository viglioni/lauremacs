;;
;; @author Laura Viglioni
;; 2025
;;
;; GNU Public License 3.0
;;
;; since: 0.1.0
;;
;; consts.el:
;; Defines important constants to the project.
;;

;;; code:

(defun join-path (path filename)
  "Concat PATH and FILENAME.
Add \"/\" to the end of the path if necessary."
  (concat path (if (string-match-p "/$" path) "" "/") filename))

(defconst lauremacs-dir               user-emacs-directory)
(defconst lauremacs-buffer-name       "*lauremacs*")
(defconst lauremacs-config-dir        (join-path lauremacs-dir "config"))
(defconst lauremacs-core-dir          (join-path lauremacs-dir "core"))
(defconst lauremacs-home-page-dir     (join-path lauremacs-core-dir "lauremacs-home-page"))
(defconst lauremacs-internal-libs-dir (join-path lauremacs-dir "lauremacs-libs"))

(setq custom-theme-directory (join-path lauremacs-dir "custom-themes"))
(setq user-emacs-directory   lauremacs-dir)

;; TBD
;; (defconst lauremacs-private-files-dir "~/private-files/emacs-files")
;; (defconst lauremacs-agenda-dir (join-path lauremacs-private-files-dir "org-agenda"))
;; (defconst lauremacs-elisp-private-files (join-path lauremacs-private-files-dir "elisp-files"))
;; (defconst lauremacs-org-roam-files "~/org-roam-files")

;;; consts.el ends here.
