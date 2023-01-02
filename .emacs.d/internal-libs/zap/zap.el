(defun zap-open-br (number)
	(interactive "sInsert number: ")
	(fp/upipe number
		(fp/partial 'replace-regexp-in-string "[^0-9]" "")
		(fp/partial 'concat "http://wa.me/55")
		'browse-url))

(defun zap-open (number)
  (interactive "sInsert number: ")
	(fp/upipe number
		(fp/partial 'replace-regexp-in-string "[^0-9]" "")
		(fp/partial 'concat "http://wa.me/")
		'browse-url))

(provide 'zap)
