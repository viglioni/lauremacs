;;
;; @author Laura Viglioni
;; 2022
;; GNU Public License 3.0
;;


;;
;; Elixir
;;

(use-package elixir-mode
	:mode "\\.ex\\'"
	:hook ((elixir-mode . lsp-deferred)
				 (elixir-mode . (lambda () (add-multiple-into-list 'prettify-symbols-alist
																											'((">=" . "≥")
																												("<=" . "≤")
																												("!=" . "≠")
																												("=~" . "≅")
																												("<-" . "←")
																												("->" . "→")
																												("<-" . "←")
																												("|>" . "▷")))))
         (before-save . (lambda ()
                          (when (and (eq major-mode 'elixir-mode)
                                     (boundp 'lsp-mode)
                                     lsp-mode)
                            (lsp-format-buffer)))))
	:init
  ;; download latest release from https://github.com/elixir-lsp/elixir-ls
  (add-to-list 'exec-path "~/elixir-ls")
	(lauremacs-major-mode-leader
		:keymaps 'elixir-mode-map
		"s"		'(nil											:which-key "repl")
		"se"	'(inf-elixir-send-line		:which-key "send line")
		"sb"	'(inf-elixir-send-buffer	:which-key "send buffer")
		"sr"	'(inf-elixir-send-region	:which-key "send region")
		"ss"	'(inf-elixir							:which-key "go to repl")
		"sp"  '(inf-elixir-project			:which-key "go to repl (project)")
		"=="	'(elixir-format						:which-key "format buffer")))

(use-package inf-elixir
	:after elixir-mode)

