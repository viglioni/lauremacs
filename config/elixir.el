;;
;; @author Laura Viglioni
;; 2022
;; GNU Public License 3.0
;;


;;
;; Elixir
;;

(require 'snake-case-mode)

(defun lauremacs-elixir-grep ()
  "Grep in .ex .exs files."
  (interactive)
  (let ((initial-search (read-string "initial query: " (word-at-point))))
    (helm-do-ag (projectile-project-root)
                nil
                (concat "-G=*.exs? " initial-search))))


(use-package elixir-ts-mode
  :mode "\\.exs?\\'"
  :hook ((elixir-ts-mode . lsp-deferred)
         (elixir-ts-mode . snake-case-mode)
         (elixir-ts-mode . switch-semi-colon-mode)
				 (elixir-ts-mode . (lambda () (add-multiple-into-list 'prettify-symbols-alist
																											   '((">=" . "≥")
																												   ("<=" . "≤")
																												   ("!=" . "≠")
																												   ("=~" . "≅")
																												   ("<-" . "←")
																												   ("->" . "→")
																												   ("<-" . "←")
                                                           ("=>" . "⇒")
																												   ("|>" . "▷")))))
         (elixir-ts-mode . (lambda () (add-hook 'before-save-hook 'lsp-format-buffer nil t))))
  :init
  ;; download latest release from https://github.com/elixir-lsp/elixir-ls
                                        ;(add-to-list 'exec-path "~/elixir-ls")
  (setq lsp-elixir-server-command '("~/lexical/_build/dev/package/lexical/bin/start_lexical.sh"))
  (lauremacs-major-mode-leader
		:keymaps 'elixir-ts-mode-map
		"s"		'(nil											:which-key "repl")
		"se"	'(inf-elixir-send-line		:which-key "send line")
		"sb"	'(inf-elixir-send-buffer	:which-key "send buffer")
		"sr"	'(inf-elixir-send-region	:which-key "send region")
		"sS"	'(inf-elixir							:which-key "go to repl")
		"ss"  '(inf-elixir-project			:which-key "go to repl (project)")
		"=="	'(elixir-format						:which-key "format buffer")))


(use-package inf-elixir
	:after elixir-ts-mode)


(defun lauremacs-elixir-create-file (file-path)
  "Create file if it does not exist.  FILE-PATH is relative to project root."
  (let ((full-path (join-path (projectile-project-root) file-path)))
    (unless (file-exists-p full-path)
      (write-region "" nil full-path)
      (message (format "Created file: %s" file-path)))
    file-path))

(defun lauremacs-elixir-impl-test-file (file)
  "Return the implementation/test file related to FILE."
  (cond ((string-match-p "^test" file)
         (fp/pipe file
           (fp/replace "^test" "lib")
           (fp/replace "_test\\.exs$" ".ex")
           'lauremacs-elixir-create-file
           ))
        ((string-match-p "^lib" file)
         (fp/pipe file
           (fp/replace "^lib" "test")
           (fp/replace "\\.ex$" "_test.exs")
           'lauremacs-elixir-create-file))))


(with-eval-after-load "projectile"
  (projectile-register-project-type
   'elixir
   '("mix.exs")
   :src-dir "lib/"
   :run ""
   :compile "mix compile"
   :install "mix deps.get"
   :project-file ""
   :test "mix test"
   :test-suffix "_test"
   :related-files-fn '(lauremacs-elixir-impl-test-file)
   ))
