;;
;; Keys
;;

(require 'helm)

(defcustom lauremacs-passwords
	nil
	"Alist of (key . value)"
	:type 'cons)

;;;###autoload
(defun lauremacs/add-passwords (passwds)
	"Add an alist of passwords (PASSWD) to `lauremacs-passwords'."
	(dolist (el passwds)
		(add-to-list 'lauremacs-passwords el t)))

;;;###autoload
(defun lauremacs/get-password ()
	"Show all keys and copy the password from selected key."
	(interactive)
	(helm
	 :promp "Choose a key: "
	 :buffer "*helm keys*"
	 :sources (helm-build-sync-source "Current keys: "
							:candidates 'lauremacs-passwords
							:action '(lambda (passwd)
												 (kill-new passwd)
												 (message "Copied password for key!")))))

(provide 'lauremacs-passwords)
