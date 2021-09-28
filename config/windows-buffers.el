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
         (snd-buffer (or (car prev-buffers) "*scratch*"))
         (trd-buffer (or (nth 1 prev-buffers) "*scratch*"))
         (second (split-window-right))
         (third (split-window second nil 'right)))
    (set-window-buffer second snd-buffer)
    (set-window-buffer third trd-buffer)
    (balance-windows)))

;;;###autoload
(defun lauremacs/switch-to-last-buffer ()
  "Switches to last buffer"
  (interactive)
  (switch-to-buffer (car (lauremacs--get-prev-buffers))))


;;
;; windows-buffers related config
;;

(general-define-key ;; walk through windows
 :prefix "C-x"
 "<up>" 'evil-window-up
 "<down>" 'evil-window-down
 "<left>" 'evil-window-left
 "<right>" 'evil-window-right
 "k" 'evil-window-up
 "j" 'evil-window-down
 "h" 'evil-window-left
 "l" 'evil-window-right)

;;
;; Windows binds
;;

(lauremacs-leader
 "w" '(nil :which-key "windows")
 "w1" '(lauremacs/window-split-single-column :which-key "split single column")
 "w2" '(lauremacs/window-split-double-columns :which-key "split double columns")
 "w3" '(lauremacs/window-split-triple-columns :which-key "split double columns"))


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
