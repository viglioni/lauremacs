;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; Projectile extra functions
;;




(defun find-project-type (project-type) 
	(seq-find
	 (lambda (proj) (equal (car proj) project-type))
	 projectile-project-types))


(defun get-project-type-plist (project-type)
	(let* ((lst (cdr (find-project-type project-type))))
		(list
		 :project-file     (plist-get lst (intern "project-file"))
		 :compilation-dir  (plist-get lst (intern "compilation-dir"))
		 :compile          (plist-get lst (intern "compile"))
		 :configure        (plist-get lst (intern "configure"))
		 :install          (plist-get lst (intern "install"))
		 :package          (plist-get lst (intern "package"))
		 :run              (plist-get lst (intern "run"))
		 :src-dir          (plist-get lst (intern "src-dir"))
		 :test             (plist-get lst (intern "test"))
		 :test-dir         (plist-get lst (intern "test-dir"))
		 :test-prefix      (plist-get lst (intern "test-prefix"))
		 :test-suffix      (plist-get lst (intern "test-suffix"))
		 :related-files-fn (plist-get lst (intern "related-files-fn")))))

(defun projectile-register-project-ext (project-type marker-files &key project-file compilation-dir configure compile install package test run test-suffix test-prefix src-dir test-dir related-files-fn ext))


;;
;; Neotree funcs, probably will moved do /core
;;

;;;###autoload
(defun lauremacs/neotree-show-file (&optional buff-name)
	"Show current file in neotree if it is open and there is a project open."
	(interactive)
	(let ((project-dir (projectile-project-root))
				(file-name (or buff-name (buffer-file-name))))
		(when (and project-dir (neo-global--window-exists-p))
			(neotree-dir project-dir)
			(neotree-find file-name))))

;;;###autoload
(defun neotree-toggle-project-dir ()
  "Toggle NeoTree using the project root (if any) and find file."
  (interactive)
	(let ((curr-name (buffer-file-name)))
		(neotree-toggle)
		(lauremacs/neotree-show-file curr-name)))


;;
;; Always display candidates at bottom window
;;

(add-to-list 'display-buffer-alist
                    `(,(rx bos "*helm" (* not-newline) "*" eos)
                         (display-buffer-in-side-window)
                         (inhibit-same-window . t)
                         (window-height . 0.4)))


;;
;; configs related to projectile, helm and neotree
;;

(use-package helm
  :bind (("M-x" . 'helm-M-x))
  :init
	(helm-mode 1)
	;; bindings
  (lauremacs-leader
		"<f19>" '(helm-M-x							:which-key "M-x")
		"cc"		'(helm-bookmarks				:which-key "bookmarks")
		"cr"		'(helm-bookmark-rename	:which-key "bookmark rename")
		"cd"		'(bookmark-delete				:which-key "bookmark delete")
		"cD"		'(bookmark-delete-all		:which-key "delete all bookmarks"))
	(general-define-key
	 :prefix "C-x"
	 "C-f" '(helm-find-files :which-key "find files")
	 "C-b" '(helm-buffers-list :which-key "list buffers"))
	(general-define-key
	 :prefix "C-h"
	 "a" '(helm-apropos :which-key "apropos")))

(use-package helm-swoop
  :init
  (lauremacs-leader
    "ss" '(helm-swoop :which-key "swoop")))

(use-package helm-descbinds
	:after helm
	:init (helm-descbinds-mode))

(use-package helm-flx
	:after helm
	:init (helm-flx-mode 1))

(use-package projectile
  :custom
	(projectile-sort-order 'recentf)
  (projectile-indexing-method 'native)
  (projectile-globally-ignored-directories
   '(
		 ".cache"
		 ".cask"
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
		 "*~"
     ))
  (projectile-project-search-path
   '("~/Company/" "~/Personal/"))

  :init
  (projectile-mode 1)
	
  (lauremacs-leader
    "p" '(:keymap projectile-command-map
		  :package projectile
		  :which-key "projectile"))
	
	(projectile-register-project-type
	 'typescript '("tsconfig.json")
	 :project-file "package.json"
	 :src-dir "src"
	 :test-dir "test"
	 :test-suffix ".spec")
	
	(projectile-register-project-type
	 'typescript-react '("tsconfig.json" "__tests__")
	 :project-file "package.json"
	 :src-dir "src"
	 :test-dir "__tests__"
	 :test-suffix ".spec"))

(use-package helm-projectile
  :after (projectile helm)
  :init
  (lauremacs-leader
    "pp" '(helm-projectile-switch-project :which-key "switch project")
    "pf" '(helm-projectile-find-file :which-key "find file")))

(use-package helm-ag
  :after (helm-projectile)
  :init
  (lauremacs-leader "/" '(helm-projectile-grep :which-key "search in project")))

(use-package neotree
  :after (projectile)
  :init
  (lauremacs-leader
    "pt" '(neotree-toggle-project-dir :which-key "neotree toggle"))
  :custom
  (neo-theme (if (display-graphic-p) 'icons 'arrow))
  (neo-show-hidden-files t))
