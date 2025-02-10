;;; _shortcuts.el --- General keybindings

;; @author Laura Viglioni
;; 2023
;; GNU Public License 3.0

;;; Commentary:
;; 

;;; Code:

(defalias 'pop-shell 'lauremacs-pop-shell)
(defalias 'pop-shell-cmd 'lauremacs-pop-shell-cmd)
(defalias 'const 'fp/const-fn-interactive)
(defalias 'shell-cmd 'lauremacs-shell-cmd-other-window)

;;
;; Other binds
;;

(lauremacs-leader
  "TAB"   '(lauremacs/switch-to-last-buffer :which-key "alternate buffer")
	"!"     '(shell-command                   :which-key "shell command")
	"&"     '(async-shell-command             :which-key "async shell command")
  "<f19>" '(helm-M-x                        :which-key "M-x")
  "/"     '(helm-projectile-grep            :which-key "search in project")
  "\\"    '(helm-do-ag                      :which-key "especific dir search"))

;;
;; a- Applications
;;

(lauremacs-leader
  "a"   '(nil                                                :which-key "applications")

  ;; browser
  "ab"  '(eww                                                :which-key "browser (eww)")

  ;; calibre
  "ac"  '(nil                                                :which-key "calibre")
  "aca" '(calibredb-add                                      :which-key "add book")
  "acc" '(calibredb                                          :which-key "calibre")
  "acf" '(calibredb-find-helm                                :which-key "find")

  ;; docker
  "ad"  '(docker                                             :which-key "docker")

  ;; pdf tools
  "ap"  '(nil                                                :which-key "pdf tools")
  "apn" '(pdf-helper-next-page                               :which-key "next page")
  "app" '(pdf-helper-prev-page                               :which-key "previous page")

	;; pop shell
  "as"  '(nil                                                :which-key "shell")
	"asH" `(,(pop-shell-cmd "stack ghci" "haskell-stack-ghci") :which-key "haskell stack ghci")
	"asN" `(,(pop-shell-cmd "node" "node")                     :which-key "node")
	"asc" `(,(pop-shell-cmd "iex" "elixir-repl")               :which-key "elixir repl")
	"asc" `(,(pop-shell-cmd "lein repl" "clojure-lein-repl")   :which-key "clojure lein repl")
	"ash" `(,(pop-shell-cmd "ghci" "haskell-ghci")             :which-key "haskell ghci")
	"asi" `(,(pop-shell 'ielm)                                 :which-key "ielm")
	"asn" `(,(pop-shell-cmd "npx ts-node" "ts-node")           :which-key "ts node")
	"ast" `(,(pop-shell 'ansi-term (getenv "SHELL"))           :which-key "ansi-term")
  "asS" `(,(pop-shell-cmd "sage" "sage")                     :which-key "sage")
  "ase" `(,(pop-shell 'eshell)                               :which-key "eshell")
  "asp" `(,(pop-shell-cmd "python3" "python")                :which-key "python")
  "ass" `(,(pop-shell 'shell)                                :which-key "shell")
  "asx" `(,(pop-shell-cmd "iex" "elixir-repl")               :which-key "elixir repl")

  ;; full buffer shell
  "asf" '(nil                                                :which-key "full buffer shell")
	"asfH" `(,(shell-cmd "stack ghci" "haskell-stack-ghci")    :which-key "haskell stack ghci")
	"asfN" `(,(shell-cmd "node" "node")                        :which-key "node")
	"asfc" `(,(shell-cmd "iex" "elixir-repl")                  :which-key "elixir repl")
	"asfc" `(,(shell-cmd "lein repl" "clojure-lein-repl")      :which-key "clojure lein repl")
	"asfh" `(,(shell-cmd "ghci" "haskell-ghci")                :which-key "haskell ghci")
	"asfi" `(ielm                                              :which-key "ielm")
	"asfn" `(,(shell-cmd "npx ts-node" "ts-node")              :which-key "ts node")
	"asft" `(,(shell-cmd 'ansi-term "/bin/zsh")                :which-key "ansi-term")
  "asfS" `(,(shell-cmd "sage" "sage")                        :which-key "sage")
  "asfe" `(eshell                                            :which-key "eshell")
  "asfp" `(,(shell-cmd "python3" "python")                   :which-key "python")
  "asfs" `(shell                                             :which-key "shell")
  "asfx" `(,(shell-cmd "iex" "elixir-repl")                  :which-key "elixir repl")

  ;; translate
  "at"  '(nil                                                :which-key "translate")
  "atP" '(lauremacs-translate-from-brazilian-at-point        :which-key "translate from br at point")
  "atp" '(lauremacs-translate-to-brazilian-at-point          :which-key "translate to br at point")
  "att" '(lauremacs-translate-transient                      :which-key "translate")
  )

;;
;; b- Buffers
;;

(lauremacs-leader
  "b"  '(nil                                        :which-key "buffers")
	"bk" '(persp-kill-buffer                          :which-key "kill buffer")
  "bB" '(helm-buffers-list                          :which-key "list buffers")
  "bb" '(lauremacs/switch-buffer                    :which-key "list buffers")
  "bc" `(,(const 'switch-to-buffer "*compilation*") :which-key "switch to Messages buffer")
  "bh" `(,(const 'switch-to-buffer "*lauremacs*")   :which-key "switch to home buffer")
  "bm" `(,(const 'switch-to-buffer "*Messages*")    :which-key "switch to Messages buffer")
  "bs" `(,(const 'switch-to-buffer "*scratch*")     :which-key "switch to Messages buffer")
  "bt" `(,(const 'switch-to-buffer "task.org")      :which-key "switch to scratch buffer")
  )

;;
;; c- Code
;;

(lauremacs-leader
  "c"   '(nil                         :which-key "code")
  ;; jumper
  "cjb" '(better-jumper-jump-backward :which-key "jump backward")
  "cjf" '(better-jumper-jump-forward  :which-key "jump forward")
  "cjj" '(better-jumper-set-jump      :which-key "set jump point")

  ;; bookmark
  "cb"	'(nil                         :which-key "bookmarks")
	"cbD"	'(bookmark-delete-all         :which-key "delete all bookmarks")
	"cbc"	'(helm-bookmarks              :which-key "bookmarks")
	"cbd"	'(bookmark-delete             :which-key "bookmark delete")
	"cbr"	'(helm-bookmark-rename        :which-key "bookmark rename")

  ;; nvm
  "cn"	'(nil                         :which-key "nvm")
	"cnc" '(nvm-get-current             :which-key "show current nvm")
	"cnd" '(nvm-download                :which-key "download nvm")
	"cni" '(nvm-install                 :which-key "nvm install")
	"cnn" '(nvm-use                     :which-key "nvm use")
	"cnp" '(nvm-use-project-version     :which-key "use .nvmrc")
	"cnr" '(nvm-run-command             :which-key "nvm run command")
  )

;;
;; d- Database
;;

(lauremacs-leader
  "d"  '(nil         :which-key "Databases")
  "dc" '(sql-connect :which-key "SQL Connect"))


;;
;; e- Errors
;;

(lauremacs-leader
  ;; flycheck
  "e"  '(:keymap flycheck-command-map :package flycheck :which-key "errors")
  "ee" '(explain-error-at-point                         :which-key "explain error at point")
  )

;;
;; f- fold/unfold codeblocks
;;

(lauremacs-leader
    "f"  '(nil                           :which-key "fold/unfold codeblocks")
    "fa" '(origami-open-all-nodes        :which-key "open all nodes")
    "ff" '(origami-toggle-node           :which-key "toggle node")
    "fo" '(origami-open-node-recursively :which-key "open node recursively")
    "fs" '(origami-show-only-node        :which-key "show only node")
    )

;;
;; g- Git
;;

(lauremacs-leader
  "gd" '(magit-diff-dwim                               :which-key "diff")
  "ge" '(:keymap smerge-basic-map :package smerge-mode :which-key "git diff - smerge")
  "gf" '(magit-find-file                               :which-key "find file")
  "gp" '(lauremacs-gh-open-pr                          :which-key "open pr")
  "gs" '(magit-status                                  :which-key "magit status")
  )

;;
;; i- input/output
;;

(lauremacs-leader
	"i"   '(nil																 :which-key "input/output")
	"ii"  '(all-the-icons-insert							 :which-key "insert icon")
	"iu"  '(nil																 :which-key "uuid")
	"iuc" '(lauremacs/insert-uuid-to-clipboard :which-key "copy uuid")
	"iuu" '(lauremacs/insert-uuid							 :which-key "insert uuid")
  "ie"  '(emojify-insert-emoji               :which-key "insert emoji")
  "ik"  '(helm-show-kill-ring                :which-key "show kill ring")
  )


;;
;; k- AI assistants
;;
(lauremacs-leader
  "k"   '(nil                                 :which-key "AI assistants")
  "kF"  '(copilot-chat-custom-prompt-function :which-key "apply a custom prompt to the function body")
  "kR"  '(copilot-chat-review-whole-buffer    :which-key "review whole buffer")
  "kb"  '(nil                                 :which-key "buffers")
  "kbb" '(copilot-chat-add-current-buffer     :which-key "add current buffer")
  "kbf" '(copilot-chat-add-files-under-dir    :which-key "add files under dir")
  "kc"  '(copilot-chat-custom-prompt-function :which-key "copilot prompt in minibuffer")
  "kd"  '(nil                                 :which-key "documentation")
  "kdd" '(copilot-chat-doc                    :which-key "document selected code")
  "kdi" '(copilot-chat-insert-commit-message  :which-key "insert commit msg")
  "ke"  '(nil                                 :which-key "explain")
  "kee" '(copilot-chat-explain                :which-key "explain selected code")
  "kef" '(copilot-chat-explain-defun          :which-key "explain defun")
  "kes" '(copilot-chat-explain-symbol-at-line :which-key "explain symbol at point")
  "kf"  '(copilot-chat-fix                    :which-key "fix code at point")
  "kk"  '(copilot-complete                    :which-key "copilot complete")
  "kr"  '(copilot-chat-review                 :which-key "review code at point")
  "kt"  '(copilot-chat-transient              :which-key "copilot chat transient")
  "ky"  '(copilot-chat-yank                   :which-key "yank copilot code")
  )


;;
;; l- workspaces
;;

(lauremacs-leader
  "l"     '(nil                                         :which-key "workspaces")
  "l TAB" '(tab-bar-switch-to-recent-tab                :which-key "switch to last tab")
  "l1"    (list (fp/const-fn-interactive 'tab-select 1) :which-key "move to tab 1")
  "l2"    (list (fp/const-fn-interactive 'tab-select 2) :which-key "move to tab 2")
  "l3"    (list (fp/const-fn-interactive 'tab-select 3) :which-key "move to tab 3")
  "l4"    (list (fp/const-fn-interactive 'tab-select 4) :which-key "move to tab 4")
  "l5"    (list (fp/const-fn-interactive 'tab-select 5) :which-key "move to tab 5")
  "l6"    (list (fp/const-fn-interactive 'tab-select 6) :which-key "move to tab 6")
  "l7"    (list (fp/const-fn-interactive 'tab-select 7) :which-key "move to tab 7")
  "l8"    (list (fp/const-fn-interactive 'tab-select 8) :which-key "move to tab 8")
  "l9"    (list (fp/const-fn-interactive 'tab-select 9) :which-key "move to tab 9")
  "ll"    '(tab-switch                                  :which-key "switch workspace")
  "ln"    '(tab-bar-new-tab                             :which-key "new tab")
  "lp"    '(lauremacs-tab-new-project-tab               :which-key "new project workspace")
  "lx"    '(tab-close                                   :which-key "kill workspace")
  )

;;
;; m- Multicursor
;;

(lauremacs-leader
  "m"           '(nil                           :which-key "multi-cursor")
  "m <mouse-1>" '(mc/add-cursor-on-click        :which-key "add cursor on click")
  "mN"          '(mc/skip-to-next-like-this     :which-key "skip to next like this")
  "mP"          '(mc/skip-to-previous-like-this :which-key "skip to previous like this")
  "ma"          '(mc/edit-beginnings-of-lines   :which-key "edit beginnings of lines")
  "me"          '(mc/edit-ends-of-lines         :which-key "edit ends of lines")
  "mi"          '(nil                           :which-key "insert")
  "mil"         '(mc/insert-letters             :which-key "insert letters")
  "min"         '(mc/insert-numbers             :which-key "insert numbers")
  "ml"          '(mc/edit-lines                 :which-key "edit lines")
  "mm"          '(set-rectangular-region-anchor :which-key "set rectangular region")
  "mn"          '(mc/mark-next-like-this        :which-key "mark next like this")
  "mp"          '(mc/mark-previous-like-this    :which-key "mark previous like this")
  "mr"          '(mc/mark-all-in-region         :which-key "mark all in region")
  "ms"          '(mc/mark-all-symbols-like-this :which-key "mark all symbols like this")
  "mt"          '(mc/mark-all-like-this         :which-key "mark all like this")
  "mu"          '(nil                           :which-key "unmark")
  "mun"         '(mc/unmark-next-like-this      :which-key "unmark last like this")
  "mup"         '(mc/unmark-previous-like-this  :which-key "unmark first like this")
  "mw"          '(mc/mark-all-words-like-this   :which-key "mark all words like this")
  )


;;
;; n- Neotree
;;

(lauremacs-leader
  "n"  '(nil            :which-key "neotree")
  "nn" '(neotree-find   :which-key "neotree find")
  "nt" '(neotree-toggle :which-key "toggle"))


;;
;; p- Projectile
;;

(lauremacs-leader
  "p"  '(:keymap projectile-command-map :package projectile :which-key "projectile")
  "pf" '(helm-projectile-find-file                          :which-key "find file")
  "pp" '(helm-projectile-switch-project                     :which-key "switch project")
  "pr" '(projectile-replace-regexp                          :which-key "replace regexp")
  "pt" '(neotree-toggle-project-dir                         :which-key "neotree toggle")
  )

;;
;; r- Org-roam
;;

(lauremacs-leader
  "r"   '(nil                               :which-key "org-roam")
  "rI"  '(org-roam-node-insert              :which-key "node insert")
  "ra"  '(org-roam-tag-add                  :which-key "add tag")
  "rd"  '(nil                               :which-key "DB")
  "rds" '(org-roam-db-sync                  :which-key "db sync")
  "rf"  '(lauremacs-tabs-find-org-roam-node :which-key "node find")
  "ri"  '(org-extra-node-insert-immediate   :which-key "node insert")
  "ro"  '(org-roam-ui-open                  :which-key "open org-roam-ui")
  "rp"  '(org-extra-add-publish-roam-tag    :which-key "add publish tag")
  "rt"  '(org-roam-buffer-toggle            :which-key "toggle buffer")
  "ru"  '(org-id-get-create                 :which-key "add UUID to section")
  )


;;
;; s- Search
;;

(lauremacs-leader
	"s"    '(nil                                                               :which-key "search")
  ;; web
	"sw"   '(nil                                                               :which-key "web search")
	"swb"  '(web-search-brave                                                  :which-key "brave search")
	"swd"  '(web-search-duckduckgo                                             :which-key "duckduckgo search")
	"swg"  '(web-search-google                                                 :which-key "google search")
	"sww"  '(web-search                                                        :which-key "web-search")
	"swy"  '(web-search-youtube                                                :which-key "youtube search")
  ;; elixir
  "sx"   '(nil                                                               :which-key "elixir grep")
  "sxf"  '(nil                                                               :which-key "grep elixir functions")
  "sxff" `(,(elauxir--grep "*.ex$ def")                                    :which-key "elixir grep function names")
  "sxfp" `(,(elauxir--grep "*.ex$ defp\ ")                                   :which-key "elixir grep private function names")
  "sxh"  `(,(elauxir--grep "*.heex$")                                        :which-key "elixir grep heex files")
  "sxm"  `(,(elauxir--grep "*.ex$ defmodule")                                :which-key "elixir grep module names")
  "sxt"  `(,(elauxir--grep "*_test.exs$")                                    :which-key "elixir grep test files")
  "sxx"  `(,(elauxir--grep "*.exs?$")                                        :which-key "elixir grep")
  ;; typescript/react
  "st"   `(,(elauxir--grep "*.tsx$ --ignore=*native* --ignore=*mobile-app*") :which-key "tsx grep")
  ;; misc
  "sb"   '(helm-do-ag-buffers                                                :which-key "search buffers")
  "se"   '(iedit-mode                                                        :which-key "iedit mode")
  "ss"   '(helm-swoop                                                        :which-key "swoop")
  )

;;
;; t- Treemacs
;;

(lauremacs-leader
  "t"  '(nil                    :which-key "treemacs")
  "tg" '(treemacs-select-window :which-key "go to treemacs")
  "tt" '(treemacs               :which-key "toggle treemacs")
  )

;;
;; v- Expand region
;;

(lauremacs-leader
  "v" '(er/expand-region :which-key "expand region"))


;;
;; w- Windows
;;

(lauremacs-leader
  "w"   '(nil                                        :which-key "windows")
	"wk"  '(nil                                        :which-key "kill window")
	"wkb" '(purpose-delete-window-at-bottom            :which-key "delete bottom window")
  "w0"  '(delete-window                              :which-key "delete other windows")
  "w1"  '(lauremacs/window-split-single-column       :which-key "split single column")
  "w2"  '(lauremacs/window-split-double-columns      :which-key "split double columns")
  "w3"  '(lauremacs/window-split-triple-columns      :which-key "split double columns")
  "w4"  '(lauremacs/window-split-grid                :which-key "split windows in grid")
  "w="  `(,(const 'balance-windows)                  :which-key "balance windows")
  "wd"  `(,(const 'delete-window)                    :which-key "delete current window")
  "we"  `(,(const 'lauremacs/window-layout-toggle)   :which-key "delete current window")
  "wp"  '(lauremacs-buffer/pop-bottom                :which-key "pop buffer at bottom position")
  "wt"  '(lauremacs/toggle-current-window-dedication :which-key "toggle window dedication")
  )

;;
;; x- Words
;;

(lauremacs-leader
	"x"   '(nil                             :which-key "words")
	"xC"  '(count-words                     :which-key "count words")
  "xs"  '(lauremacs-words-sort-lines      :which-key "sort lines")
  
  ;; align
  "xa"	'(nil                             :which-key "align")
	"xag" '(lauremacs-align-general-sexp    :which-key "align general.el statements")
	"xat" '(lauremacs-align-region-as-table :which-key "align region as table")
  
  ;; string inflection
  "xc"  '(nil                               :which-key "convert word case")
  "xcc" '(string-inflection-lower-camelcase :which-key "convert to camelCase")
  "xck" '(string-inflection-kebab-case      :which-key "convert to kebab-case")
  "xcp" '(string-inflection-camelcase       :which-key "convert to PascalCase")
  "xcs" '(string-inflection-underscore      :which-key "convert to snake-case")
  "xcu" '(string-inflection-upcase          :which-key "convert to UPPER_CASE")
  )


;;
;; T- Toggle
;;

(lauremacs-leader
	"T"	 '(nil															 :which-key "toggle/choose")
	"TT" '(lauremacs/choose-theme            :which-key "choose theme")
	"Tl" '(global-display-line-numbers-mode	 :which-key "linum mode")
	"Tp" '(lauremacs/toggle-pair-programming :which-key "pair programming mode")
	"Tt" '(lauremacs/toggle-transparency		 :which-key "transparency")
  "Ti" '(highlight-indentation-mode        :which-key "highlight indentation")
  )

;;
;; project scripts
;;

(require 'project-scripts)

(general-define-key
 :prefix "<f17> <f17>"
 "" '(nil :which-key "language scripts")
 )

(general-define-key
 :prefix "<f17> <f17> <f19>"
 ""		'(nil									:which-key "common commands")
 "g"	'(go-to-script-buffer :which-key "go to script buffer")
 "h"	'(hide-script-buffer	:which-key "hide script buffer")
 "o"	'(open-script-buffer	:which-key "open script buffer")
 )

(general-define-key
 :prefix "<f17> <f17> n"
 ""   '(nil									:which-key "npm scripts")
 "d"  '(npm-install-dev-lib :which-key "install dev lib")
 "i"  '(npm-install-lib			:which-key "install lib")
 "l"  '(import-default-lib	:which-key "import default lib")
 "o"  '(nil									:which-key "open file")
 "op" '(open-package-json		:which-key "package.json")
 "r"  '(npm-choose-and-run	:which-key "run package.json script")
 )

(general-define-key
 :prefix "<f17> <f19>"
 ""	'(run-make-cmd	:which-key "run make command"))

(general-define-key
 :prefix "<f17> <f17> m"
 ""		'(nil						:which-key "makefile scripts")
 "r"	'(run-make-cmd	:which-key "run make command")
 )

(general-define-key
 :prefix "<f17> <f17> p"
 ""	 '(nil                            :which-key "project scripts")
 "t" '(project-scripts-create-ts-proj	:which-key "create ts project")
 )

(general-define-key
 :prefix "<f17> <f17>"
 "x" '(elauxir-mix :which-key "elixir mix")
 )


(provide '_shortcuts)

;;; _shortcuts.el ends here
