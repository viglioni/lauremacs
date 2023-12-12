;;
;; @author Laura Viglioni
;; 2022
;; GNU Public License 3.0
;;


;;
;; Elixir
;;

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
         (before-save . (lambda () (when (eq major-mode 'elixir-ts-mode)
                                (lsp-format-buffer)))))
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

;; (use-package elixir-mode
;; 	:mode "\\.ex\\'"
;; 	:hook ((elixir-mode . lsp-deferred)
;; 				 (elixir-mode . (lambda () (add-multiple-into-list 'prettify-symbols-alist
;; 																											'((">=" . "≥")
;; 																												("<=" . "≤")
;; 																												("!=" . "≠")
;; 																												("=~" . "≅")
;; 																												("<-" . "←")
;; 																												("->" . "→")
;; 																												("<-" . "←")
;;                                                         ("=>" . "⇒")
;; 																												("|>" . "▷")))))
;;          (before-save . (lambda ()
;;                           (when (and (eq major-mode 'elixir-mode)
;;                                      (boundp 'lsp-mode)
;;                                      lsp-mode)
;;                             (lsp-format-buffer)))))
;; 	:init
;;   ;; download latest release from https://github.com/elixir-lsp/elixir-ls
;;   ;(add-to-list 'exec-path "~/elixir-ls")
;;   (setq lsp-elixir-server-command '("~/lexical/_build/dev/package/lexical/bin/start_lexical.sh"))
;; 	(lauremacs-major-mode-leader
;; 		:keymaps 'elixir-mode-map
;; 		"s"		'(nil											:which-key "repl")
;; 		"se"	'(inf-elixir-send-line		:which-key "send line")
;; 		"sb"	'(inf-elixir-send-buffer	:which-key "send buffer")
;; 		"sr"	'(inf-elixir-send-region	:which-key "send region")
;; 		"ss"	'(inf-elixir							:which-key "go to repl")
;; 		"sp"  '(inf-elixir-project			:which-key "go to repl (project)")
;; 		"=="	'(elixir-format						:which-key "format buffer")))

(use-package inf-elixir
	:after elixir-ts-mode)

