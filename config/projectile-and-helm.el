;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; Projectile extra functions
;;



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
      (neotree-projectile-action)
			(neotree-find file-name))))

;;;###autoload
(defun neotree-toggle-project-dir ()
  "Toggle NeoTree using the project root (if any) and find file."
  (interactive)
	(let ((curr-name (buffer-file-name)))
		(if (neo-global--window-exists-p)
        (neotree-toggle)
      (neotree-projectile-action))
		(lauremacs/neotree-show-file curr-name)))

;;;###autoload
(defun neotree-on-project-change ()
  (interactive)
  (when (neo-global--window-exists-p)
    (neotree-toggle-project-dir)))


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
		"cb"		'(nil										:which-key "bookmarks")
		"cbc"		'(helm-bookmarks				:which-key "bookmarks")
		"cbr"		'(helm-bookmark-rename	:which-key "bookmark rename")
		"cbd"		'(bookmark-delete				:which-key "bookmark delete")
		"cbD"		'(bookmark-delete-all		:which-key "delete all bookmarks"))
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
  (projectile-create-missing-test-files t)
	(projectile-sort-order 'recentf)
  (projectile-indexing-method 'alien)
  (projectile-globally-ignored-directories
   '(
     "*\\.stack-work"
     "*build"
     "*node_modules"
     "^node_modules$"
     "^\\.cache$"
     "^\\.cask$"
     "^\\.eldev$"
     "^\\.elixir_ls$"
     "^\\.git$"
     "^\\.log$"
     "^\\.next$"
     "^\\.nyc_output$"
     "^\\.pub-cache$"
     "^\\.rush$"
     "^\\.svn$"
     "^\\.vscode$"
     "^_build$"
     "^android$"
     "^bundle.*$"
     "^coverage$"
     "^deps$"
     "^dist$"
     "^dist-.*"
     "^elpa"
     "^ios$"
     "^out$"
     "^repl$"
     "^rush$"
     "^target$"
     "^temp$"
     "^venv$"
     ))
  (projectile-globally-ignored-files
   '(
     "*-lock.json"
     "*.chunk.*"
     "*.gz"
     "*.jar"
     "*.log"
     "*.min.*"
     "*.png"
     "*.pyc"
     "*.storyshot"
     "*.tar.gz"
     "*.tgz"
     "*.zip"
     ".DS_Store"
     ".lein-repl-history"
     ".packages"
     "*.js.map"
     "*~"
     "*.pack"
     "*.hex"
     "*.lock"
     ))
  (projectile-project-search-path
   '("~/Company/" "~/Personal/"))
  
  
  :init
  (projectile-mode 1)
  (setq projectile-switch-project-action '(lambda () (magit-status) (projectile-find-file)))
	
  (lauremacs-leader
    "p" '( :keymap projectile-command-map
		       :package projectile
		       :which-key "projectile")))

(use-package helm-projectile
  :after (projectile helm)
  :init
  (lauremacs-leader
    "pp" '(helm-projectile-switch-project :which-key "switch project")
    "pf" '(helm-projectile-find-file      :which-key "find file")))

(use-package helm-ag
  :after (helm-projectile)
  :init
  (lauremacs-leader
    "/" '(helm-projectile-grep :which-key "search in project")
    "\\" '(helm-do-ag :which-key "especific dir search")))

(use-package neotree
  :after (projectile)
  :init
  (lauremacs-leader
    "pt" '(neotree-toggle-project-dir :which-key "neotree toggle"))
  :custom
  (neo-smart-open t)
  (neo-autorefresh t)
  (neo-window-width 35)
  (neo-window-position 'right)
  (neo-theme (if (display-graphic-p) 'icons 'arrow))
  (neo-show-hidden-files t))

(use-package helm-make
	:after helm)
