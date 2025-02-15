;;; -*- lexical-binding: t; -*-

(require 'org)
(require 'ob)
(require 'ob-shell)

(org-babel-do-load-languages 
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)))

(setq org-confirm-babel-evaluate nil)

(let* ((base-dir user-emacs-directory)
       (dirs '("config" "core" "lauremacs-libs" "test"))
       (files (mapcan (lambda (dir)
                       (let ((full-dir (expand-file-name dir base-dir)))
                         (when (file-exists-p full-dir)
                           (directory-files full-dir t "readme\\.org$"))))
                     dirs)))
  (dolist (file files)
    (message "Processing %s..." file)
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (org-babel-execute-buffer)
      (write-file file))))
