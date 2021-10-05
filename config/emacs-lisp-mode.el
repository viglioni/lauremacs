;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; configs related to emacs lisp mode
;;


(lauremacs-major-mode-leader
 :keymaps 'emacs-lisp-mode-map
 "c" '(nil :which-key "compile")
 "c" '(nil :which-key "eval")
 "cc" '(emacs-lisp-byte-compile :which-key "byte compile")
 "eb" '(eval-buffer :which-key "eval-buffer")
 "er" '(eval-region :which-key "eval region"))

