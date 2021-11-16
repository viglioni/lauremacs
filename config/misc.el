;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; Insert on buffer or clipboard
;;

(lauremacs-leader
	"i"   '(nil :which-key "insert")
	"iu"  '(nil :which-key "uuid")
	"iuu" '(lauremacs/insert-uuid              :which-key "insert uuid")
	"iuc" '(lauremacs/insert-uuid-to-clipboard :which-key "copy uuid")
	"ii"  '(all-the-icons-insert               :which-key "insert icon"))


