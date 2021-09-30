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

(general-define-key ;; walk through code
 "s-m" 'left-char
 "s-," 'next-line
 "s-." 'previous-line
 "s-/" 'right-char
 "C-s-m" 'backward-word
 "C-s-," 'forward-paragraph
 "C-s-." 'backward-paragraph
 "C-s-/" 'forward-word)

;;
;; windows-buffers related config
;;

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

;;
;; Windows binds
;;

(lauremacs-leader
 "w" '(nil :which-key "windows")
 "w1" '(lauremacs/window-split-single-column :which-key "split single column")
 "w2" '(lauremacs/window-split-double-columns :which-key "split double columns")
 "w3" '(lauremacs/window-split-triple-columns :which-key "split double columns")
 "w4" '(lauremacs/window-split-grid :which-key "split windows in grid")
 "wt" '(lauremacs/toggle-current-window-dedication :which-key "toggle window dedication")
 "w=" (list (fp/const-fn-interactive 'balance-windows) :which-key "balance windows"))


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
;; Search binds
;;

(lauremacs-leader
  "s" '(nil :which-key "search")
  "ss" '(helm-swoop :which-key "swoop"))
