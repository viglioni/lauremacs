;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; funcs 
;;
                                        ;TODO: Remove from config file

(defun lauremacs--get-prev-buffers ()
  "Return list with previous buffers."
  (seq-filter 'buffer-file-name
              (delq (current-buffer) (buffer-list))))

(defun lauremacs/window-split-single-column ()
  "Set the layout to single column."
  (interactive)
  (delete-other-windows)
  (balance-windows))

(defun lauremacs/window-split-double-columns ()
  "Set the layout to two columns.
TODO: use perspectives
TODO: throw if not splitable"
  (interactive)
  (let ((prev-buffer (or (car (lauremacs--get-prev-buffers)) (current-buffer))))
    (delete-other-windows)
    (set-window-buffer (split-window-right) prev-buffer)
    (balance-windows)))

;;
;; windows-buffers related config
;;


(use-package evil
  :bind (;; walk through windows
         ("C-x <up>" . 'evil-window-up)
         ("C-x <down>" . 'evil-window-down)
         ("C-x <left>" . 'evil-window-left)
         ("C-x <right>" . 'evil-window-right)))


(general-define-key
 :prefix "<f19> w"
 "" '(nil :which-key "windows")
 "1" '(lauremacs/window-split-single-column :which-key "split single column")
 "2" '(lauremacs/window-split-double-columns :which-key "split double columns"))
