;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;


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
 "C-s-/" 'forward-word
 "s-M" 'backward-word
 "s-<" 'forward-paragraph
 "s->" 'backward-paragraph
 "s-?" 'forward-word)

(keymap-set global-map "C-s-/" 'forward-word)

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
	            ("<return>" . nil)))


