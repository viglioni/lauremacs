;;; -*- lexical-binding: t; l-syntax: t -*-
(require 'general)

;; Basic navigation

(general-define-key 
 "s-m"   'left-char
 "s-,"   'next-line
 "s-."   'previous-line
 "s-/"   'right-char
 "C-s-m" 'backward-word
 "C-s-," 'forward-paragraph
 "C-s-." 'backward-paragraph
 "C-s-/" 'forward-word
 "s-M"   'backward-word
 "s-<"   'forward-paragraph
 "s->"   'backward-paragraph
 "s-?"   'forward-word)

;; Window navigation (check also <leader>-W)

(general-define-key ;; walk through windows
 :prefix "C-x"
 "<up>" 'evil-window-up
 "<down>" 'evil-window-down
 "<left>" 'evil-window-left
 "<right>" 'evil-window-right
 "m" 'evil-window-left
 "," 'evil-window-down
 "." 'evil-window-up
 "/" 'evil-window-right
 "-" 'split-window-vertically
 "\\" 'split-window-horizontally)

(general-define-key "M-x" 'helm-M-x)

(lauremacs-leader
  "TAB"  '(wb/switch-to-last-buffer :which-key "prev buffer")
  "!"     '(shell-command       :which-key "shell command")
  "&"     '(async-shell-command :which-key "async shell command")
  "/"     '(helm-projectile-grep            :which-key "search in project")
  "<f19>" '(helm-M-x            :which-key "M-x")
  "\\"    '(helm-do-ag          :which-key "especific dir search"))


;;
;; <leader>-B
;; Buffers
;;

(require 'windows-and-buffers)

(lauremacs-leader
  "b" '(nil :which-key "buffers")
  "bb" '(wb/switch-buffers :which-key "project buffer list")
  "bB" '(helm-buffers-list :which-key "buffer list")
  )

;;
;; <leader>-F
;; Frames
;;

(require 'lauremacs-posframe)

(lauremacs-leader
  "f" '(nil :which-key "frames")
  "fe" '(lpf/open-lauremacs-config :which-key "emacs config in posframe")
  )


;;
;; <leader>-G
;; Git
;;

(lauremacs-leader
  "gd" '(magit-diff-dwim                               :which-key "diff")
  "ge" '(:keymap smerge-basic-map :package smerge-mode :which-key "git diff - smerge")
  "gf" '(magit-find-file                               :which-key "find file")
  "gn" '(lauremacs/magit-new-branch-from-main          :which-key "new branch from origin/main")
  "gp" '(lauremacs/gh-create-pr                        :which-key "create PR (simple)")
  "gP" '(lauremacs/ai-create-pr-with-claude            :which-key "create PR with AI")
  "gs" '(magit-status                                  :which-key "magit status")
  )


;;
;; <leader>-K
;; AI assistants
;;
(require 'claude-code)
(lauremacs-leader
 "k"   '(nil                                           :which-key "AI assistants")
 )



;;
;; <leader>-M
;; Multicursor
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
;; <leader>-P
;; Projectile
;;

(require 'projectile)
(lauremacs-leader
  "p"  '(:keymap projectile-command-map :package projectile :which-key "projectile")
  )


;;
;; <leader>-R
;; Org-roam
;;

(lauremacs-leader
  "r"   '(nil                               :which-key "org-roam")
  "rI"  '(org-roam-node-insert              :which-key "node insert")
  "ra"  '(org-roam-tag-add                  :which-key "add tag")
  "rd"  '(nil                               :which-key "DB")
  "rds" '(org-roam-db-sync                  :which-key "db sync")
  "rf"  '(org-roam-node-find              :which-key "node find")
  "ri"  '(org-extra-node-insert-immediate   :which-key "node insert")
  "ro"  '(org-roam-ui-open                  :which-key "open org-roam-ui")
  "rp"  '(org-extra-add-publish-roam-tag    :which-key "add publish tag")
  "rt"  '(org-roam-buffer-toggle            :which-key "toggle buffer")
  "ru"  '(org-id-get-create                 :which-key "add UUID to section")
  )



;;
;; <leader>-S
;; Search
;;

(lauremacs-leader
  "s" '(nil :which-key "search")
  "se" '(iedit-mode :which-key "iedit mode")
  "ss" '(helm-swoop :which-key "swoop"))

;;
;; <leader>-T
;; Tree
;;
(lauremacs-leader
  "t" '(nil :which-key "tree")
  "tt" '(neotree-find   :which-key "neotree find")
  "tn" '(neotree-toggle :which-key "toggle tree")
  )


;;
;; <leader>-V
;; Expand region
;;

(lauremacs-leader
  "v" '(er/expand-region :which-key "expand region"))

;;
;; <leader>-W
;; Windows
;;

(lauremacs-leader
  "w" '(nil :which-key "window")
  "w-" '(split-window-vertically :which-key "split horizontally")
  "w1" '(delete-other-windows :which-key "single window")
  "w=" '(balance-windws :which-key "balance windows")
  "w\\" '(split-window-horizontally :which-key "split vertically")
  "wf" '(delete-other-windows :which-key "single window")
  "wh" '(split-window-vertically :which-key "split horizontally")
  "wm" '(maximize-window :which-key "maximize window")
  "wq" '(ace-select-window :which-key "select window by number")
  "ws" '(ace-window-swap :which-key "swap")
  "wv" '(split-window-horizontally :which-key "split vertically")
  ;; todo create toggle here
  "ww"  '(balance-windows :which-key "balance windows")
  )
