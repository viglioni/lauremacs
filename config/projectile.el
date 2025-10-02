(config-package projectile
  :custom
  (projectile-create-missing-test-files t)
  (projectile-sort-order 'recentf)
  (projectile-indexing-method 'alien)
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
     "*.lock"
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
  (projectile-project-search-path
   '("~/Company/" "~/Personal/"))
  
  :init
  (projectile-mode 1))
