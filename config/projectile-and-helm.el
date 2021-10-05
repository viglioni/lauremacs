;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; funcs 
;;

(use-package helm
  :bind (("M-x" . 'helm-M-x))
  :init
  (lauremacs-leader "<f19>" '(helm-M-x :which-key "M-x")))

(use-package helm-swoop
  :init
  (lauremacs-leader
    "ss" '(helm-swoop :which-key "swoop")))

(use-package projectile
  :custom
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
