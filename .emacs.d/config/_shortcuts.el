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
  "acc" '(calibredb                                          :which-key "calibre")
  "aca" '(calibredb-add                                      :which-key "add book")
  "acf" '(calibredb-find-helm                                :which-key "find")

  ;; docker
  "ad"  '(docker                                             :which-key "docker")

  ;; pdf tools
  "ap"  '(nil                                                :which-key "pdf tools")
  "app" '(pdf-helper-prev-page                               :which-key "previous page")
  "apn" '(pdf-helper-next-page                               :which-key "next page")

	;; pop shell
  "as"  '(nil                                                :which-key "shell")
	"asH" `(,(pop-shell-cmd "stack ghci" "haskell-stack-ghci") :which-key "haskell stack ghci")
	"asN" `(,(pop-shell-cmd "node" "node")                     :which-key "node")
	"asc" `(,(pop-shell-cmd "iex" "elixir-repl")               :which-key "elixir repl")
	"asc" `(,(pop-shell-cmd "lein repl" "clojure-lein-repl")   :which-key "clojure lein repl")
  "ase" `(,(pop-shell 'eshell)                               :which-key "eshell")
	"ash" `(,(pop-shell-cmd "ghci" "haskell-ghci")             :which-key "haskell ghci")
	"asi" `(,(pop-shell 'ielm)                                 :which-key "ielm")
	"asn" `(,(pop-shell-cmd "npx ts-node" "ts-node")           :which-key "ts node")
  "asp" `(,(pop-shell-cmd "python3" "python")                :which-key "python")
	"ast" `(,(pop-shell 'ansi-term (getenv "SHELL"))           :which-key "ansi-term")
  "ass" `(,(pop-shell 'shell)                                :which-key "shell")
  "asS" `(,(pop-shell-cmd "sage" "sage")                     :which-key "sage")
  "asx" `(,(pop-shell-cmd "iex" "elixir-repl")               :which-key "elixir repl")

  ;; full buffer shell
  "asf" '(nil                                                :which-key "full buffer shell")
	"asfH" `(,(shell-cmd "stack ghci" "haskell-stack-ghci")    :which-key "haskell stack ghci")
	"asfN" `(,(shell-cmd "node" "node")                        :which-key "node")
	"asfc" `(,(shell-cmd "iex" "elixir-repl")                  :which-key "elixir repl")
	"asfc" `(,(shell-cmd "lein repl" "clojure-lein-repl")      :which-key "clojure lein repl")
  "asfe" `(eshell                                            :which-key "eshell")
	"asfh" `(,(shell-cmd "ghci" "haskell-ghci")                :which-key "haskell ghci")
	"asfi" `(ielm                                              :which-key "ielm")
	"asfn" `(,(shell-cmd "npx ts-node" "ts-node")              :which-key "ts node")
  "asfp" `(,(shell-cmd "python3" "python")                   :which-key "python")
	"asft" `(,(shell-cmd 'ansi-term "/bin/zsh")                :which-key "ansi-term")
  "asfs" `(shell                                             :which-key "shell")
  "asfS" `(,(shell-cmd "sage" "sage")                        :which-key "sage")
  "asfx" `(,(shell-cmd "iex" "elixir-repl")                  :which-key "elixir repl")

  ;; translate
  "at"  '(nil                                                :which-key "translate")
  "att" '(lauremacs-translate-transient                      :which-key "translate")
  "atP" '(lauremacs-translate-from-brazilian-at-point        :which-key "translate from br at point")
  "atp" '(lauremacs-translate-to-brazilian-at-point          :which-key "translate to br at point"))

;;
;; b- Buffers
;;

(lauremacs-leader
  "b"  '(nil                                              :which-key "buffers")
  "bb" '(lauremacs/switch-buffer                          :which-key "list buffers")
  "bB" '(helm-buffers-list                                :which-key "list buffers")
	"bk" '(persp-kill-buffer                                :which-key "kill buffer")
  "bc" `(,(const 'switch-to-buffer "*compilation*")       :which-key "switch to Messages buffer")
  "bm" `(,(const 'switch-to-buffer "*Messages*")          :which-key "switch to Messages buffer")
  "bs" `(,(const 'switch-to-buffer "*scratch*")           :which-key "switch to scratch buffer")
  "bh" `(,(const 'switch-to-buffer lauremacs-buffer-name) :which-key "switch to home buffer"))

;;
;; c- Code
;;

(lauremacs-leader
  "c"   '(nil                         :which-key "code")
  ;; jumper
  "cjj" '(better-jumper-set-jump      :which-key "set jump point")
  "cjb" '(better-jumper-jump-backward :which-key "jump backward")
  "cjf" '(better-jumper-jump-forward  :which-key "jump forward")

  ;; bookmark
  "cb"	'(nil                         :which-key "bookmarks")
	"cbc"	'(helm-bookmarks              :which-key "bookmarks")
	"cbr"	'(helm-bookmark-rename        :which-key "bookmark rename")
	"cbd"	'(bookmark-delete             :which-key "bookmark delete")
	"cbD"	'(bookmark-delete-all         :which-key "delete all bookmarks")

  ;; nvm
  "cn"	'(nil                         :which-key "nvm")
	"cnp" '(nvm-use-project-version     :which-key "use .nvmrc")
	"cnn" '(nvm-use                     :which-key "nvm use")
	"cni" '(nvm-install                 :which-key "nvm install")
	"cnr" '(nvm-run-command             :which-key "nvm run command")
	"cnd" '(nvm-download                :which-key "download nvm")
	"cnc" '(nvm-get-current             :which-key "show current nvm"))

;;
;; e- Errors
;;

(lauremacs-leader
  ;; flycheck
  "e"  '(:keymap flycheck-command-map :package flycheck :which-key "errors")
  "ee" '(explain-error-at-point                         :which-key "explain error at point"))

;;
;; g- Git
;;

(lauremacs-leader
  "gs" '(magit-status                                  :which-key "magit status")
  "gf" '(magit-find-file                               :which-key "find file")
  "gd" '(magit-diff-dwim                               :which-key "diff")
  "gp" '(lauremacs-gh-open-pr                          :which-key "open pr")
  "ge" '(:keymap smerge-basic-map :package smerge-mode :which-key "git diff - smerge"))

;;
;; i- input/output
;;

(lauremacs-leader
	"i"   '(nil																 :which-key "input/output")
	"iu"  '(nil																 :which-key "uuid")
	"iuu" '(lauremacs/insert-uuid							 :which-key "insert uuid")
	"iuc" '(lauremacs/insert-uuid-to-clipboard :which-key "copy uuid")
	"ii"  '(all-the-icons-insert							 :which-key "insert icon")
  "ie"  '(emojify-insert-emoji               :which-key "insert emoji"))

;;
;; l- workspaces
;;

(lauremacs-leader
  "l"     '(nil                                         :which-key "workspaces")
  "lp"    '(lauremacs-tab-new-project-tab               :which-key "new project workspace")
  "ln"    '(tab-bar-new-tab                             :which-key "new tab")
  "ll"    '(tab-switch                                  :which-key "switch workspace")
  "l TAB" '(tab-bar-switch-to-recent-tab                :which-key "switch to last tab")
  "lx"    '(tab-close                                   :which-key "kill workspace")
  "l1"    (list (fp/const-fn-interactive 'tab-select 1) :which-key "move to tab 1")
  "l2"    (list (fp/const-fn-interactive 'tab-select 2) :which-key "move to tab 2")
  "l3"    (list (fp/const-fn-interactive 'tab-select 3) :which-key "move to tab 3")
  "l4"    (list (fp/const-fn-interactive 'tab-select 4) :which-key "move to tab 4")
  "l5"    (list (fp/const-fn-interactive 'tab-select 5) :which-key "move to tab 5")
  "l6"    (list (fp/const-fn-interactive 'tab-select 6) :which-key "move to tab 6")
  "l7"    (list (fp/const-fn-interactive 'tab-select 7) :which-key "move to tab 7")
  "l8"    (list (fp/const-fn-interactive 'tab-select 8) :which-key "move to tab 8")
  "l9"    (list (fp/const-fn-interactive 'tab-select 9) :which-key "move to tab 9"))

;;
;; m- Multicursor
;;

(lauremacs-leader
  "m"           '(nil                           :which-key "multi-cursor")
  "m <mouse-1>" '(mc/add-cursor-on-click        :which-key "add cursor on click")
  "ml"          '(mc/edit-lines                 :which-key "edit lines")
  "ma"          '(mc/edit-beginnings-of-lines   :which-key "edit beginnings of lines")
  "me"          '(mc/edit-ends-of-lines         :which-key "edit ends of lines")
  "mw"          '(mc/mark-all-words-like-this   :which-key "mark all words like this")
  "ms"          '(mc/mark-all-symbols-like-this :which-key "mark all symbols like this")
  "mt"          '(mc/mark-all-like-this         :which-key "mark all like this")
  "mr"          '(mc/mark-all-in-region         :which-key "mark all in region")
  "mm"          '(set-rectangular-region-anchor :which-key "set rectangular region")
  "mn"          '(mc/mark-next-like-this        :which-key "mark next like this")
  "mp"          '(mc/mark-previous-like-this    :which-key "mark previous like this")
  "mN"          '(mc/skip-to-next-like-this     :which-key "skip to next like this")
  "mP"          '(mc/skip-to-previous-like-this :which-key "skip to previous like this")
  "mu"          '(nil                           :which-key "unmark")
  "mun"         '(mc/unmark-next-like-this      :which-key "unmark last like this")
  "mup"         '(mc/unmark-previous-like-this  :which-key "unmark first like this")
  "mi"          '(nil                           :which-key "insert")
  "min"         '(mc/insert-numbers             :which-key "insert numbers")
  "mil"         '(mc/insert-letters             :which-key "insert letters"))

;;
;; p- Projectile
;;

(lauremacs-leader
  "p"  '(:keymap projectile-command-map :package projectile :which-key "projectile")
  "pp" '(helm-projectile-switch-project                     :which-key "switch project")
  "pf" '(helm-projectile-find-file                          :which-key "find file")
  "pt" '(neotree-toggle-project-dir                         :which-key "neotree toggle"))

;;
;; r- Org-roam
;;

(lauremacs-leader
  "r"   '(nil                               :which-key "org-roam")
  "rd"  '(nil                               :which-key "DB")
  "rds" '(org-roam-db-sync                  :which-key "db sync")
  "ru"  '(org-id-get-create                 :which-key "add UUID to section")
  "rt"  '(org-roam-buffer-toggle            :which-key "toggle buffer")
  "rf"  '(lauremacs-tabs-find-org-roam-node :which-key "node find")
  "rI"  '(org-roam-node-insert              :which-key "node insert")
  "ri"  '(org-extra-node-insert-immediate   :which-key "node insert")
  "ra"  '(org-roam-tag-add                  :which-key "add tag")
  "ro"  '(org-roam-ui-open                  :which-key "open org-roam-ui"))

;;
;; s- Search
;;

(lauremacs-leader
	"s"   '(nil										:which-key "search")
  "ss"  '(helm-swoop            :which-key "swoop")
  "se"  '(iedit-mode            :which-key "iedit mode")
  "sx"  '(lauremacs-elixir-grep :which-key "elixir grep")
	"sw"  '(nil										:which-key "web search")
	"sww" '(web-search						:which-key "web-search")
	"swg" '(web-search-google			:which-key "google search")
	"swd" '(web-search-duckduckgo	:which-key "duckduckgo search")
	"swb" '(web-search-brave			:which-key "brave search")
	"swy" '(web-search-youtube		:which-key "youtube search"))

;;
;; t- Treemacs
;;

(lauremacs-leader
  "t"  '(nil                    :which-key "treemacs")
  "tt" '(treemacs               :which-key "toggle treemacs")
  "tg" '(treemacs-select-window :which-key "go to treemacs"))

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
  "w0"  '(delete-window                              :which-key "delete other windows")
  "w1"  '(lauremacs/window-split-single-column       :which-key "split single column")
  "w2"  '(lauremacs/window-split-double-columns      :which-key "split double columns")
  "w3"  '(lauremacs/window-split-triple-columns      :which-key "split double columns")
  "w4"  '(lauremacs/window-split-grid                :which-key "split windows in grid")
  "wt"  '(lauremacs/toggle-current-window-dedication :which-key "toggle window dedication")
  "w="  `(,(const 'balance-windows)                  :which-key "balance windows")
  "wd"  `(,(const 'delete-window)                    :which-key "delete current window")
  "we"  `(,(const 'lauremacs/window-layout-toggle)   :which-key "delete current window")
  "wp"  '(lauremacs-buffer/pop-bottom                :which-key "pop buffer at bottom position")
	"wk"  '(nil                                        :which-key "kill window")
	"wkb" '(purpose-delete-window-at-bottom            :which-key "delete bottom window"))

;;
;; x- Words
;;

(lauremacs-leader
	"x"   '(nil                             :which-key "words")
  "xs"  '(lauremacs-words-sort-lines      :which-key "sort lines")
	"xC"  '(count-words                     :which-key "count words")
  
  ;; align
  "xa"	'(nil                             :which-key "align")
	"xag" '(lauremacs-align-general-sexp    :which-key "align general.el statements")
	"xat" '(lauremacs-align-region-as-table :which-key "align region as table")
  
  ;; string inflection
  "xc"  '(nil                               :which-key "convert word case")
  "xcs" '(string-inflection-underscore      :which-key "convert to snake-case")
  "xcc" '(string-inflection-lower-camelcase :which-key "convert to camelCase")
  "xcp" '(string-inflection-camelcase       :which-key "convert to PascalCase")
  "xck" '(string-inflection-kebab-case      :which-key "convert to kebab-case")
  "xcu" '(string-inflection-upcase          :which-key "convert to UPPER_CASE"))


;;
;; T- Toggle
;;

(lauremacs-leader
	"T"		'(nil																:which-key "toggle/choose")
	"Tp"	'(lauremacs/toggle-pair-programming :which-key "pair programming mode")
	"TT"	'(lauremacs/choose-theme						:which-key "choose theme")
	"Tt"	'(lauremacs/toggle-transparency			:which-key "transparency")
	"Tl"	'(linum-mode												:which-key "linum mode"))

(provide '_shortcuts)




(provide '_shortcuts)

;;; _shortcuts.el ends here
