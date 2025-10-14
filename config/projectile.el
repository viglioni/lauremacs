;;; -*- lexical-binding: t; l-syntax: t -*-
;;;###autoload
(defun lauremacs/switch-project ()
  "Switch to a project intelligently.
If in a git repo, open magit-status.
If not in a git repo, try to open README.org or README.md.
If no README exists, create and open .projectile file.
Finally, run projectile-find-file."
  (interactive)
  (let ((project-root (projectile-project-root)))
    (when project-root
      (if (file-exists-p (expand-file-name ".git" project-root))
          ;; Git repo exists
          (magit-status)
        ;; Not a git repo, look for README
        (let ((readme-org (expand-file-name "README.org" project-root))
              (readme-md (expand-file-name "README.md" project-root))
              (projectile-file (expand-file-name ".projectile" project-root)))
          (cond
           ((file-exists-p readme-org) (find-file readme-org))
           ((file-exists-p readme-md) (find-file readme-md))
           (t
            ;; No README, create/open .projectile
            (find-file projectile-file)))))
      ;; Always run projectile-find-file at the end
      (projectile-find-file))))


(config-package projectile
  :custom
  (projectile-switch-project-action #'lauremacs/switch-project)
  (projectile-create-missing-test-files t)
  (projectile-sort-order 'recentf)
  (projectile-indexing-method 'alien)
  (helm-projectile-fuzzy-match t)
  (projectile-globally-ignored-directories
   '(
     "*\\.stack-work"
     "*build"
     "*deps"
     "*node_modules"
     "^\\.cache"
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
     "^dist$"
     "^dist-.*"
     "^elpa"
     "^ios$"
     "^node_modules$"
     "^out$"
     "^repl$"
     "^rush$"
     "^target$"
     "^temp$"
     "^venv$"
     "^\\.venv$"
     "\\.venv/"     
     ))
  (projectile-globally-ignored-files
   '(
     "*.chunk.*"
     "*.cjs"
     "*.dets"
     "*.gz"
     "*.hex"
     "*.jar"
     "*.js.map"
     "*.lock.*"
     "*.log"
     "*.min.*"
     "*.pack"
     "*.png"
     "*.pyc"
     "*.storyshot"
     "*.tar.gz"
     "*.tgz"
     "*.zip"
     "*project.log*"
     "*~"
     ".DS_Store"
     ".lein-repl-history"
     ".packages"
     "*-lock.json"
     ))
  :init
  (projectile-mode 1))
