;;; lauremacs-insert.el --- Inserting stuff on buffer or clipboard
;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;;###autoload

;;; Commentary:
;; 

;;; Code:

(require 'uuidgen)

(defun lauremacs/insert-uuid-to-clipboard ()
  "Generate uuid and copies it to clipboard."
  (interactive)
  (let ((uuid (uuidgen-4)))
    (kill-new uuid)
    (message (format "copied %s to clipboard" uuid))
    uuid))

;;;###autoload
(defun lauremacs/insert-uuid ()
  "Insert random uuid."
  (interactive)
  (insert (uuidgen-4)))

;;;###autoload
(defun lauremacs/copy-buffer-path ()
	"Copies relative path to project root of the current buffer."
	(interactive)
	(fp/pipe (buffer-file-name)
		(fp/partial 'replace-regexp-in-string (projectile-project-root) "")
		'kill-new))


(provide 'lauremacs-insert)

;;; lauremacs-insert.el ends here
