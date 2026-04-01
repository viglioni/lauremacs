(require 'snake-case-mode)
(require 'semicolon-mode)

(config-package elixir-ts-mode
  :mode "\\.exs?\\'"
  :hook (
         (elixir-mode . lsp-deferred)
         (elixir-mode . snake-case-mode)
         (elixir-mode . semicolon-mode)
				 ;; (elixir-mode . (lambda () (add-multiple-into-list 'prettify-symbols-alist
				 ;;  																						'((">=" . "≥")
				 ;;  																							("<=" . "≤")
				 ;;  																							("!=" . "≠")
				 ;;  																							("=~" . "≅")
				 ;;  																							("<-" . "←")
				 ;;  																							("->" . "→")
				 ;;  																							("<-" . "←")
         ;;                                                ("=>" . "⇒")
				 ;;  																							("|>" . "▷")))))
         (elixir-ts-mode . lsp-deferred)
         (elixir-ts-mode . snake-case-mode)
         (elixir-ts-mode . semicolon-mode)
				 ;; (elixir-ts-mode . (lambda () (add-multiple-into-list 'prettify-symbols-alist
				 ;;  																						   '((">=" . "≥")
				 ;;  																							   ("<=" . "≤")
				 ;;  																							   ("!=" . "≠")
				 ;;  																							   ("=~" . "≅")
				 ;;  																							   ("<-" . "←")
				 ;;  																							   ("->" . "→")
				 ;;  																							   ("<-" . "←")
         ;;                                                   ("=>" . "⇒")
				 ;;  																							   ("|>" . "▷")))))
         (elixir-mode . (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
         (elixir-ts-mode . (lambda () (add-hook 'before-save-hook 'elixir-format nil t))))
  :init
  ;; download latest release from https://github.com/elixir-lsp/elixir-ls
  (add-to-list 'exec-path "~/elixir-ls")
                                        ;  (setq lsp-elixir-server-command '("~/lexical/_build/dev/package/lexical/bin/start_lexical.sh"))
                                        ;(setq lsp-elixir-server-command '("~/elixir-ls/language_server.sh"))
  ;; (require 'elauxir)
  ;; (require 'snake-case-mode)
  ;; (require 'lauriex)

  ;; ;; heex
  ;; (add-hook 'heex-ts-mode-hook 'lsp-deferred)
  ;; (add-hook 'heex-ts-mode-hook (lambda () (add-hook 'before-save-hook 'lsp-format-buffer nil t)))
  
  ;; (lauremacs-major-mode-leader
	;; 	:keymaps '(elixir-ts-mode-map heex-ts-mode-map elixir-mode-map)
	;; 	"s"      '(nil                    :which-key "repl")
	;; 	"sb"     '(lauriex-send-buffer    :which-key "send buffer")
	;; 	"ss"     '(lauriex                :which-key "go to repl (project)")
  ;;   "sS"     '(lauriex-server         :which-key "Repl with phoenix server")
  ;;   "sr"     '(lauriex-recompile      :which-key "recompile")
	;; 	"=="     '(elixir-format          :which-key "format buffer")
  ;;   "TAB"    '(elauxir-switch-ex-heex :which-key "toggle ex <-> heex")
  ;;   "tf"     '(elauxir-test-this-file :which-key "test this file")
  ;;   "tr"     '(elauxir-run-this-test  :which-key "run test at point"))
  )
