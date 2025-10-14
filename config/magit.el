;;; -*- lexical-binding: t; l-syntax: t -*-
(config-package magit
  :init
  (setq magit-display-buffer-function
        (lambda (buffer)
          (display-buffer
           buffer
           (cond ((and (derived-mode-p 'magit-mode)
                       (eq (with-current-buffer buffer major-mode)
                           'magit-status-mode))
                  nil)
                 ((memq (with-current-buffer buffer major-mode)
                        '(magit-process-mode
                          magit-revision-mode
                          magit-diff-mode
                          magit-stash-mode))
                  nil)
                 (t
                  '(display-buffer-same-window)))))))

(defun lauremacs/magit-new-branch-from-main ()
  "Fetch from origin, then create and checkout a new branch from origin/main or origin/master.
This command first fetches from origin, then checks if origin/main exists
(falling back to origin/master if not), and finally prompts for a new
branch name to create from the detected base branch."
  (interactive)
  (message "Fetching from origin...")
  (magit-call-git "fetch" "origin")
  (let* ((base-branch (if (magit-ref-exists-p "origin/main")
                          "origin/main"
                        "origin/master"))
         (branch-name (magit-read-string-ns
                       (format "Create and checkout branch from %s" base-branch))))
    (when branch-name
      (magit-branch-and-checkout branch-name base-branch))))
