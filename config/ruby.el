;;
;; Ruby config
;;

;; Install outside dependencies:
;; gem install solargraph
;; gem install rubocop
;; gem install pry

;; Add rbenv to PATH
(setenv "PATH" (concat (expand-file-name "~/.rbenv/shims") ":" 
                       (expand-file-name "~/.rbenv/bin") ":" 
                       (getenv "PATH")))

;; Add rbenv to exec-path
(setq exec-path (append (list (expand-file-name "~/.rbenv/shims")
                              (expand-file-name "~/.rbenv/bin"))
                        exec-path))

;; Set RBENV_ROOT
(setenv "RBENV_ROOT" (expand-file-name "~/.rbenv"))


;;
;; General config
;;

(use-package ruby-ts-mode
  :ensure t
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :hook ((ruby-ts-mode . subword-mode)
         (ruby-ts-mode . lsp-deferred)
         (ruby-ts-mode . snake-case-mode)
         (ruby-ts-mode . switch-semi-colon-mode))
  :init
  (lauremacs-major-mode-leader
		:keymaps 'ruby-ts-mode-map
		"s"  '(nil                     :which-key "repl")
		"sb" '(ruby-send-buffer        :which-key "send buffer")
    "sB" '(ruby-send-buffer-and-go :which-key "send buffer and go")
    "sr" '(ruby-send-region        :which-key "send region")
    "sR" '(ruby-send-region-and-go :which-key "send region and go")
		"ss" '(inf-ruby                :which-key "go to repl (project)")
    "sr" '(lauriex-recompile       :which-key "recompile"))) 


(with-eval-after-load 'lsp-mode
  (setq lsp-ruby-language-server 'solargraph)
  
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("solargraph" "stdio"))
                    :major-modes '(ruby-mode ruby-ts-mode)
                    :priority 1
                    :server-id 'solargraph))
  
  ;; Solargraph-specific settings
  (setq lsp-solargraph-use-bundler t)
  (setq lsp-solargraph-multi-root nil))

;; LSP UI enhancements (optional but recommended)

;; Interactive Ruby
(use-package inf-ruby
  :ensure t
  :hook (ruby-ts-mode . inf-ruby-minor-mode)
  :config
  ;; Use rbenv ruby
  (setq inf-ruby-default-implementation "ruby"))

;; Robe for live completion and docs
(use-package robe
  :ensure t
  :hook (ruby-ts-mode . robe-mode)
  :config
  ;; Add robe to company backends
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-robe)))

;; RuboCop integration
(use-package rubocop
  :ensure t
  :hook (ruby-ts-mode . rubocop-mode)
  :config
  ;; Auto-fix on save (optional)
  (setq rubocop-autocorrect-on-save t))

;; Rails-specific support
(use-package projectile-rails
  :ensure t
  :after projectile
  :hook (projectile-mode . projectile-rails-global-mode)
  :config
  (define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map))


;;
;; helper funcs
;;

;; TODO
;; ri -i



;;;###autoload
(defun lauremacs-ruby//create-file (file-path)
  "Create file if it does not exist.  FILE-PATH is relative to project root."
  (let* ((full-path (join-path (projectile-project-root) file-path))
         (dir-path (file-name-directory full-path)))
    (unless (file-exists-p full-path)
      (make-directory dir-path t)
      (write-region "" nil full-path)
      (message (format "Created file: %s" file-path)))
    file-path))

;;;###autoload
(defun lauremacs-ruby/impl-test-file (file)
  "Return the implementation/test file related to FILE."
  (cond ((string-match-p "^test" file)
         (fp/pipe file
           (fp/replace "^test" "lib")
           (fp/replace "_test\\.rb$" ".rb")
           'lauremacs-ruby//create-file
           ))
        ((string-match-p "^lib" file)
         (fp/pipe file
           (fp/replace "^lib" "test")
           (fp/replace "\\.rb$" "_test.rb")
           'lauremacs-ruby//create-file))))


(with-eval-after-load "projectile"
  (projectile-register-project-type
   'ruby-with-rakefile
   '("Rakefile")
   :src-dir "lib/"
   :run "rake run"
   :compile ""
   :install ""
   :project-file ""
   :test "rake test"
   :test-suffix "_test"
   :related-files-fn '(lauremacs-ruby/impl-test-file)))
