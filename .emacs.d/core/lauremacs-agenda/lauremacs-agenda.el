(require 'laurisp-core)

(defun lauremacs--agenda-dir (file)
	(join-path lauremacs-agenda-dir file))


(defun lauremacs-agenda-gcal-fetch (filename-base gcal-url calendar-name)
  "FILENAME-BASE is the filanemabase.(org|ics) file.
GCAL-URL is the calendar download url.
CALENDAR-NAME is the name will be shown in `org-agenda'."
  (let ((ics-file (lauremacs--agenda-dir (concat filename-base ".ics")))
				(org-file (lauremacs--agenda-dir (concat filename-base ".org"))))
		(unless (file-exists-p org-file) (shell-command (format "touch %s" org-file)))
    (exec-path-when-cmd-not-found "ical2org.awk")
		(with-temp-buffer
			(shell-command (format "wget -O %s %s && CALENDAR=%s ical2org.awk < %s > %s"
														 ics-file gcal-url calendar-name ics-file org-file)
                     t))))


(provide 'lauremacs-agenda)
