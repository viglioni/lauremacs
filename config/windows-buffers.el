;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; funcs 
;;
                                        ;TODO: Remove from config file

;;;###autoload
(defun lauremacs--get-prev-buffers ()
  "Return list with previous buffers."
  (seq-filter 'buffer-file-name
              (delq (current-buffer) (buffer-list))))

;;;###autoload
(defun lauremacs/window-split-single-column ()
  "Set the layout to single column."
  (interactive)
  (delete-other-windows)
  (balance-windows))

;;;###autoload
(defun lauremacs/window-split-double-columns ()
  "Set the layout to two columns.
TODO: use perspectives
TODO: throw if not splitable"
  (interactive)
  (let ((prev-buffer (or (car (lauremacs--get-prev-buffers)) (current-buffer))))
    (delete-other-windows)
    (set-window-buffer (split-window-right) prev-buffer)
    (balance-windows)))

;;;###autoload
(defun lauremacs/window-split-triple-columns ()
  "Sets the layout to three columns."
  (interactive)
  (delete-other-windows)
  (let* ((prev-buffers (mapcar 'buffer-name (lauremacs--get-prev-buffers)))
         (snd-buffer (or (car prev-buffers) (current-buffer)))
         (trd-buffer (or (nth 1 prev-buffers) (current-buffer)))
         (second (split-window-right))
         (third (split-window second nil 'right)))
    (set-window-buffer second snd-buffer)
    (set-window-buffer third trd-buffer)
    (balance-windows)))

;;;###autoload
(defun lauremacs/window-split-grid ()
  "Sets the layout to a 2*2 grid"
  (interactive)
  (delete-other-windows)
  (let* ((prev-buffers (mapcar 'buffer-name (lauremacs--get-prev-buffers)))
         (snd-buffer (or (car prev-buffers) (current-buffer)))
         (trd-buffer (or (nth 1 prev-buffers) (current-buffer)))
         (fourth-buffer (or (nth 2 prev-buffers) (current-buffer)))
         (second (split-window-right))
         (third (split-window-below))
         (fourth (split-window second nil 'below)))
    (set-window-buffer second snd-buffer)
    (set-window-buffer third trd-buffer)
    (set-window-buffer fourth fourth-buffer)
    (balance-windows)))

(defun lauremacs/toggle-current-window-dedication ()
  "Toggle dedication state of a window."
  (interactive)
  (let* ((current-window (selected-window))
         (dedicated? (window-dedicated-p current-window)))
    (set-window-dedicated-p current-window (not dedicated?))
    (message "Window %sdedicated to %s"
             (if dedicated? "no longer " "")
             (buffer-name))))


;;;###autoload
(defun lauremacs/switch-to-last-buffer ()
  "Switches to last buffer"
  (interactive)
  (switch-to-buffer (car (lauremacs--get-prev-buffers))))

;;;###autoload
(defun lauremacs/windw-layout-toggle ()
  "Toggles between horizontal and vertical layout of two windows.
TODO: add throwif"
  (interactive)
  (when (not (= 2 (count-windows)))
    (error "Can't toggle window layout when the number of windows isn't two."))
  (let* ((current-is-horizontal? (car (car (window-tree))))
	 (first-window (nth 2 (window-tree)))
	 (second-window (nth 3 (window-tree)))
	 (split-fn (if current-is-horizontal? #'split-window-horizontally #'split-window-vertically))
	 )))

;;;###autoload
(defmacro fp/const-fn-interactive (fn &rest args)
  "Returns an interactive lambda function that executes fn with given args.
e.g.
(fp/const-fn-interactive 'concat \"several \" \"string \" \"args\")"
  `(lambda () (interactive) (apply ,fn (quote ,args))))


;;
;; Windows binds
;;

(lauremacs-leader
  "w" '(nil :which-key "windows")
  "w0" (list (fp/const-fn-interactive 'delete-other-windows) :which-key "delete other windows")
  "w1" '(lauremacs/window-split-single-column :which-key "split single column")
  "w2" '(lauremacs/window-split-double-columns :which-key "split double columns")
  "w3" '(lauremacs/window-split-triple-columns :which-key "split double columns")
  "w4" '(lauremacs/window-split-grid :which-key "split windows in grid")
  "wt" '(lauremacs/toggle-current-window-dedication :which-key "toggle window dedication")
  "w=" (list (fp/const-fn-interactive 'balance-windows) :which-key "balance windows")
  "wd" (list (fp/const-fn-interactive 'delete-window) :which-key "delete current window"))


;;
;; Buffers binds
;;

(lauremacs-leader
  "TAB" '(lauremacs/switch-to-last-buffer :which-key "alternate buffer")
  "b" '(nil :which-key "buffers")
  "bb" '(helm-buffers-list :which-key "list buffers")
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
 "/" 'evil-window-right
 "M-0" 'neotree-show)

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
  (general-define-key
   :prefix "C-c m"
   "" '(nil :which-key "multi-cursor")
   "<mouse-1>" '(mc/add-cursor-on-click :which-key "add cursor on click")
   "m" '(mc/edit-lines :which-key "edit lines")
   "a" '(mc/edit-beginnings-of-lines :which-key "edit beginnings of lines")
   "e" '(mc/edit-ends-of-lines :which-key "edit ends of lines")
   "w" '(mc/mark-all-words-like-this :which-key "mark all words like this")
   "s" '(mc/mark-all-symbols-like-this :which-key "mark all symbols like this")
   "t" '(mc/mark-all-like-this :which-key "mark all like this")
   "r" '(mc/mark-all-in-region :which-key "mark all in region")
   "R" '(set-rectangular-region-anchor :which-key "set rectangular region")
   "n" '(mc/mark-next-like-this :which-key "mark next like this")
   "p" '(mc/mark-previous-like-this :which-key "mark previous like this")
   "N" '(mc/skip-to-next-like-this :which-key "skip to next like this")
   "P" '(mc/skip-to-previous-like-this :which-key "skip to previous like this")
   "u" '(nil :which-key "unmark")
   "un" '(mc/unmark-next-like-this :which-key "unmark last like this")
   "up" '(mc/unmark-previous-like-this :which-key "unmark first like this")
   "i" '(nil :which-key "insert")
   "in" '(mc/insert-numbers :which-key "insert numbers")
   "il" '(mc/insert-letters :which-key "insert letters"))
  (lauremacs-leader
    "m" '(nil :which-key "multi-cursor")
    "m <mouse-1>" '(mc/add-cursor-on-click :which-key "add cursor on click")
    "mm" '(mc/edit-lines :which-key "edit lines")
    "ma" '(mc/edit-beginnings-of-lines :which-key "edit beginnings of lines")
    "me" '(mc/edit-ends-of-lines :which-key "edit ends of lines")
    "mw" '(mc/mark-all-words-like-this :which-key "mark all words like this")
    "ms" '(mc/mark-all-symbols-like-this :which-key "mark all symbols like this")
    "mt" '(mc/mark-all-like-this :which-key "mark all like this")
    "mr" '(mc/mark-all-in-region :which-key "mark all in region")
    "mR" '(set-rectangular-region-anchor :which-key "set rectangular region")
    "mn" '(mc/mark-next-like-this :which-key "mark next like this")
    "mp" '(mc/mark-previous-like-this :which-key "mark previous like this")
    "mN" '(mc/skip-to-next-like-this :which-key "skip to next like this")
    "mP" '(mc/skip-to-previous-like-this :which-key "skip to previous like this")
    "mu" '(nil :which-key "unmark")
    "mun" '(mc/unmark-next-like-this :which-key "unmark last like this")
    "mup" '(mc/unmark-previous-like-this :which-key "unmark first like this")
    "mi" '(nil :which-key "insert")
    "min" '(mc/insert-numbers :which-key "insert numbers")
    "mil" '(mc/insert-letters :which-key "insert letters")))


