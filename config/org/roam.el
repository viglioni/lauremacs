;;; -*- lexical-binding: t; l-syntax: t -*-
(lauremacs/load config org roam templates)

(defun org-extra-node-insert-immediate (arg &rest args)
  "Insert org-roam node even if it doesnt exist yet."
  (interactive "P")
  (let ((args (cons arg args)))
    (apply #'org-roam-node-insert args)))


(config-package org-roam
  :custom
   (org-roam-directory lauremacs-org-roam-files )
  (org-roam-complete-everywhere t)
  
 :init
  (lauremacs/org-roam-templates)
  (setq org-roam-v2-ack t)
  (setq org-roam-node-display-template "${title:*}${tags:20}")
  (org-roam-db-autosync-enable)
  
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n t" . org-roam-buffer-toggle)
         ("C-c n u" . org-id-get-create)
         ("C-c n f" . org-roam-node-find)
         ("C-c n I" . org-roam-node-insert)
         ("C-c n i" . org-extra-node-insert-immediate))
  )
