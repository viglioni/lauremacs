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
	(lauremacs/icons-set-ts-regex-icon	"-?styles?\\.ts$"	'all-the-icons-faicon		"paint-brush"))
