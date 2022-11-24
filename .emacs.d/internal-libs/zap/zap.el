(defun zap-open (number)
	(interactive "sInsert number: ")
	(fp/upipe number
		(fp/partial 'replace-regexp-in-string "[^0-9]" "")
		(fp/partial 'concat "http://wa.me/55")
		'browse-url))


(provide 'zap)