;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;


;;
;; Customizations
;;

(with-eval-after-load "all-the-icons"
	(lauremacs//icons-define-all-icons)
	;; typescript (ts/tsx) icons
	(lauremacs/icons-set-ts-mode-icon		"tsx"							'all-the-icons-fileicon	"tsx")
	(lauremacs/icons-set-ts-regex-icon	"-?spec\\.tsx$"		'all-the-icons-fileicon	"test-react")
	(lauremacs/icons-set-ts-regex-icon	"-?test\\.tsx$"		'all-the-icons-fileicon	"test-react")
	(lauremacs/icons-set-ts-regex-icon	"-?styles?\\.ts$"	'all-the-icons-faicon		"paint-brush")
  ;; Ebooks
  (lauremacs/icons-set-icon
   '(:height 1.0 :v-adjust 0.0 :face all-the-icons-red)
   'all-the-icons-extension-icon-alist
   "epub"
   'all-the-icons-faicon
   "book")
  (lauremacs/icons-set-icon
   '(:height 1.0 :v-adjust 0.0 :face all-the-icons-blue-alt)
   'all-the-icons-extension-icon-alist
   "mobi"
   'all-the-icons-faicon
   "book")
  (lauremacs/icons-set-icon
   '(:height 1.0 :v-adjust 0.0 :face all-the-icons-blue)
   'all-the-icons-extension-icon-alist
   "azw3"
   'all-the-icons-faicon
   "book"))
