;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; windows-buffers related config
;;


(use-package evil
  :bind (;; walk through windows
         ("C-x <up>" . 'evil-window-up)
         ("C-x <down>" . 'evil-window-down)
         ("C-x <left>" . 'evil-window-left)
         ("C-x <right>" . 'evil-window-right)))



