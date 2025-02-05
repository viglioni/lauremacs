(require 'org-element)

(defun parse-org-delete-subtree-at-point ()
  "Delete subtree at point."
  (interactive)
  (let* ((ctx (org-element-context))
         (type (org-element-type ctx))
         (beg (org-element-property :begin ctx))
         (end (org-element-property :end ctx)))
    (when (eq type 'headline)
      (kill-region beg end))))

(defun parse-org-delete-subtree-by-name (name)
  "Delete subtree where heading is NAME."
  (org-map-entries
   (lambda ()
     (when (string= name (parse-org-get-heading-name))
       (parse-org-delete-subtree-at-point)))))

(defun parse-org-get-heading-name ()
  "Get heading name under cursor."
  (org-element-property :title (org-element-at-point)))

(defun parse-org-get-heading-paragraph ()
  "Get heading name under cursor."
  (org-element-property :paragraph (org-element-at-point)))

(defun parse-org-get-subheadings-titles (heading-name)
  "Get all subheading titles inside HEADING-NAME."
  (let ((titles))
    (org-map-entries
     (lambda ()
       (when (string= heading-name (parse-org-get-heading-name))
         (setq titles (cdr (org-map-entries 'parse-org-get-heading-name nil 'tree))))))
    titles))

(provide 'parse-org)
