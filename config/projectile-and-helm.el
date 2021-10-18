;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; configs related to projectile, helm and neotree
;;

(use-package helm
  :bind (("M-x" . 'helm-M-x))
  :init
	(helm-mode 1)
  (lauremacs-leader "<f19>" '(helm-M-x :which-key "M-x"))
	(general-define-key
	 "C-x C-f" '(helm-find-files :which-key "find files")))

(use-package helm-swoop
  :init
  (lauremacs-leader
    "ss" '(helm-swoop :which-key "swoop")))

(use-package helm-flx
	:after helm
	:init (helm-flx-mode 1))

(use-package projectile
  :custom
  (projectile-indexing-method 'native)
  (projectile-globally-ignored-directories
   '(".cask"
     ".eldev"
     ".git"
     ".log"
     ".next"
     ".nyc_output"
     ".pub-cache"
     ".rush"
     ".svn"
     ".vscode"
     "android"
     "bundle*"
     "coverage"
     "dist"
     "dist-*"
     "ios"
     "node_modules"
     "out"
     "repl"
     "rush"
     "target"
     "temp"
     "venv"
     "webnext/common"))
  (projectile-globally-ignored-files
   '(
     "*-lock.json"
     "*.chunk.*"
     "*.gz"
     "*.jar"
     "*.log"
     "*.png"
     "*.pyc"
     "*.tar.gz"
     "*.tgz"
     "*.zip"
     ".DS_Store"
     ".lein-repl-history"
     ".packages"
     ))
  (projectile-project-search-path
   '("~/Loft/" "~/Personal/"  "~/Loft/webnext/apps"))

  (projectile-switch-project-action '(lambda ()
				(neotree-projectile-action)
				(projectile-find-file)))
  :init
  (projectile-mode 1)
  (lauremacs-leader
    "p" '(:keymap projectile-command-map
		  :package projectile
		  :which-key "projectile")))

(use-package helm-projectile
  :after (projectile helm)
  :init
  (lauremacs-leader
    "pp" '(helm-projectile-switch-project :which-key "switch project")
    "pf" '(helm-projectile-find-file :which-key "find file")))

(use-package helm-ag
  :after (helm-projectile)
  :init
  (lauremacs-leader "/" '(helm-do-grep-ag :which-key "search in project")))

(use-package neotree
  :after (projectile)
  :init
  (lauremacs-leader
    "pt" '(neotree-toggle :which-key "neotree toggle"))
  :custom
  (neo-theme (if (display-graphic-p) 'icons 'arrow))
  (neo-show-hidden-files t))
