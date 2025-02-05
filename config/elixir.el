;;
;; @author Laura Viglioni
;; 2022
;; GNU Public License 3.0
;;

;;; Code:

;;
;; Elixir
;;

(require 'laurisp-core)

(use-package elixir-ts-mode
  :mode "\\.exs?\\'"
  :hook (
         (elixir-mode . lsp-deferred)
         (elixir-mode . snake-case-mode)
         (elixir-mode . switch-semi-colon-mode)
				 (elixir-mode . (lambda () (add-multiple-into-list 'prettify-symbols-alist
																											   '((">=" . "≥")
																												   ("<=" . "≤")
																												   ("!=" . "≠")
																												   ("=~" . "≅")
																												   ("<-" . "←")
																												   ("->" . "→")
																												   ("<-" . "←")
                                                           ("=>" . "⇒")
																												   ("|>" . "▷")))))
         (elixir-ts-mode . lsp-deferred)
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
         (elixir-mode . (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
         (elixir-ts-mode . (lambda () (add-hook 'before-save-hook 'elixir-format nil t))))
  :init
  ;; download latest release from https://github.com/elixir-lsp/elixir-ls
                                        (add-to-list 'exec-path "~/elixir-ls")
                                        ;  (setq lsp-elixir-server-command '("~/lexical/_build/dev/package/lexical/bin/start_lexical.sh"))
  ;(setq lsp-elixir-server-command '("~/elixir-ls/language_server.sh"))
  (require 'elauxir)
  (require 'snake-case-mode)
  (require 'lauriex)

  ;; heex
  (add-hook 'heex-ts-mode-hook 'lsp-deferred)
  (add-hook 'heex-ts-mode-hook (lambda () (add-hook 'before-save-hook 'lsp-format-buffer nil t)))
  
  (lauremacs-major-mode-leader
		:keymaps '(elixir-ts-mode-map heex-ts-mode-map elixir-mode-map)
		"s"      '(nil                    :which-key "repl")
		"sb"     '(lauriex-send-buffer    :which-key "send buffer")
		"ss"     '(lauriex                :which-key "go to repl (project)")
    "sS"     '(lauriex-server         :which-key "Repl with phoenix server")
    "sr"     '(lauriex-recompile      :which-key "recompile")
		"=="     '(elixir-format          :which-key "format buffer")
    "TAB"    '(elauxir-switch-ex-heex :which-key "toggle ex <-> heex")
    "tf"     '(elauxir-test-this-file :which-key "test this file")
    "tr"     '(elauxir-run-this-test  :which-key "run test at point")))


(use-package inf-elixir
	:after elixir-ts-mode
  :init
  (add-hook 'inf-elixir-mode-hook 'snake-case-mode))

(use-package flycheck-credo
  :after '(elixir-ts-mode flycheck)
  :init
  (flycheck-credo-setup))

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
   :related-files-fn '(elauxir-impl-test-file)
   ))

(with-eval-after-load "smartparens"
  (sp-local-pair 'heex-ts-mode "<%" " %>")
  (sp-local-pair 'heex-ts-mode "<." " />")
  (sp-local-pair 'elixir-ts-mode "<%" " %>")
  (sp-local-pair 'elixir-ts-mode "<." " />")
)
