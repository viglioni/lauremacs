;;
;; @author Laura Viglioni
;; 2022
;; GNU Public License 3.0
;;

;;
;; SQL mode
;;

(use-package sql
	:mode ("\\.sql\\'" . sql-mode)
	:hook ((sql-mode . lsp-deferred)
				 (sql-mode . sqlind-minor-mode)
				 (sql-interactive-mode . (lambda () (toggle-truncate-lines 1))))
	:init
	(require 'sqlau)
	(require-without-throw 'sql-private)
	(lauremacs-major-mode-leader
		:keymaps 'sql-mode-map
		"s"  '(nil                          :which-key "repl")
		"sb" '(sql-send-buffer              :which-key "send buffer to repl")
		"sf" '(sql-send-paragraph           :which-key "send paragraph to repl")
		"sr" '(sql-send-region              :which-key "send region to repl")
		"sB" '(sql-send-buffer-and-focus    :which-key "send buffer repl and focus")
		"sF" '(sql-send-paragraph-and-focus :which-key "send paragraph and focus")
		"sR" '(sql-send-region-and-focus    :which-key "send region and focus")
		"ss" '(sql-show-sqli-buffer         :which-key "show sqli buffer")
		"c"  '(nil                          :which-key "connection")
	  "cc" '(sql-connect                  :which-key "sql connect")
		"cb" '(sql-set-sqli-buffer          :which-key "set sqli buffer")
		"l"  '(nil                          :which-key "lsp functions")
		"ls" '(lsp-sql-switch-connection    :which-key "switch connections")
		"==" '(sqlfmt-buffer                :which-key "format buffer")
    "=b" '(sqlfmt-buffer                :which-key "format buffer")
		"=r" '(sqlfmt-region                :which-key "format region")))

(use-package sql-indent
	:after sql)

 
