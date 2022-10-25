;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;


;;
;; Windows binds
;;

(lauremacs-leader
  "w" '(nil :which-key "windows")
  "w0" '(delete-window :which-key "delete other windows")
  "w1" '(lauremacs/window-split-single-column :which-key "split single column")
  "w2" '(lauremacs/window-split-double-columns :which-key "split double columns")
  "w3" '(lauremacs/window-split-triple-columns :which-key "split double columns")
  "w4" '(lauremacs/window-split-grid :which-key "split windows in grid")
  "wt" '(lauremacs/toggle-current-window-dedication :which-key "toggle window dedication")
  "w=" (list (fp/const-fn-interactive 'balance-windows) :which-key "balance windows")
  "wd" (list (fp/const-fn-interactive 'delete-window) :which-key "delete current window")
  "we" (list (fp/const-fn-interactive 'lauremacs/window-layout-toggle) :which-key "delete current window")
	"wk" '(nil :which-key "kill window")
	"wkb" '(purpose-delete-window-at-bottom :which-key "delete bottom window"))


;;
;; Buffers binds
;;

(lauremacs-leader
  "TAB" '(lauremacs/switch-to-last-buffer :which-key "alternate buffer")
  "b" '(nil :which-key "buffers")
  "bb" '(lauremacs/switch-buffer :which-key "list buffers")
  "bB" '(helm-buffers-list :which-key "list buffers")
	"bk" '(persp-kill-buffer :which-key "kill buffer")
  "bm" '((lambda () (interactive) (switch-to-buffer "*Messages*"))
         :which-key "switch to Messages buffer")
  "bs" '((lambda () (interactive) (switch-to-buffer "*scratch*"))
         :which-key "switch to scratch buffer")
  "bh" '((lambda () (interactive) (switch-to-buffer lauremacs-buffer-name))
         :which-key "switch to home buffer"))

;;
;; Walk through code / windows
;;

(general-define-key 
 "s-m" 'left-char
 "s-," 'next-line
 "s-." 'previous-line
 "s-/" 'right-char
 "C-s-m" 'backward-word
 "C-s-," 'forward-paragraph
 "C-s-." 'backward-paragraph
 "C-s-/" 'forward-word)

(general-define-key ;; walk through windows
 :prefix "C-x"
 "<up>" 'evil-window-up
 "<down>" 'evil-window-down
 "<left>" 'evil-window-left
 "<right>" 'evil-window-right
 "m" 'evil-window-left
 "," 'evil-window-down
 "." 'evil-window-up
 "/" 'evil-window-right)

(defun winum-assign-0-to-neotree ()
  (when (string-match-p (buffer-name) ".*\\*NeoTree\\*.*") 10))

(use-package winum
  :init
  (winum-mode)
  (add-to-list 'winum-assign-functions #'winum-assign-0-to-neotree)
  :bind (("M-0" . 'winum-select-window-0-or-10)
	       ("M-1" . 'winum-select-window-1)
	       ("M-2" . 'winum-select-window-2)
	       ("M-3" . 'winum-select-window-3)
	       ("M-4" . 'winum-select-window-4)
	       ("M-5" . 'winum-select-window-5)
	       ("M-6" . 'winum-select-window-6)
	       ("M-7" . 'winum-select-window-7)
	       ("M-8" . 'winum-select-window-8)
	       ("M-9" . 'winum-select-window-9)))

(use-package multiple-cursors
  :bind (:map mc/keymap
	      ("<return>" . nil))
  :init
	
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
    "mil"         '(mc/insert-letters             :which-key "insert letters")))


