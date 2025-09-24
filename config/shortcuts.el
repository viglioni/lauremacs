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

(lauremacs-leader
  "TAB"  '(wb/switch-to-last-buffer :which-key "prev buffer")
  "!"     '(shell-command       :which-key "shell command")
  "&"     '(async-shell-command :which-key "async shell command")
  "<f19>" '(helm-M-x            :which-key "M-x")
  "\\"    '(helm-do-ag          :which-key "especific dir search"))


;;
;; <leader>-B
;; Buffers
;;

(lauremacs-leader
  "b" '(nil :which-key "buffers")
  "bb" '(helm-buffers-list :which-key "buffer list")
  )

;;
;; <leader>-T
;; Tree
;;
(lauremacs-leader
  "t" '(nil :which-key "tree")
  "tt" '(neotree-toggle :which-key "toggle tree")
  )

;;
;; <leader>-W
;; Windows
;;

(lauremacs-leader
  "w" '(nil :which-key "window")
  "ws" '(ace-window-swap :which-key "swap")
  "w-" '(split-window-vertically :which-key "split horizontally")
  "wh" '(split-window-vertically :which-key "split horizontally")
  "w\\" '(split-window-horizontally :which-key "split vertically")
  "wv" '(split-window-horizontally :which-key "split vertically")
  "wf" '(delete-other-windows :which-key "single window")
  ;; todo create toggle here
  "wm" '(maximize-window :which-key "maximize window")
  "ww"  '(balance-windows :which-key "balance windows")
  "wq" '(ace-select-window :which-key "select window by number")
  )
