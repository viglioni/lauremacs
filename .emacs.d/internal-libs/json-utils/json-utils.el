;;; json-utils.el --- Json Helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2021 Your Name

;; Author: Your Name <yourname@example.com>
;; Maintainer: Someone Else <someone@example.com>
;; Created: 14 Jul 2010
;; Keywords: languages
;; URL: https://example.com/foo
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.1") (json "1.4") (projectile "2.4.0-snapshot"))

;; This file is not part of GNU Emacs.

;; This file is free software


;;; Commentary:
;; This package.

;;; Code:

(require 'json)
(require 'projectile)
(require 'laurisp-core)

(message "loading json-utils...")

;;;###autoload
(defun json-utils-get-package-json ()
  "Gets package.json on the root of the project."
  (let ((json-file (join-path (or (projectile-project-root) ".") "package.json")))
    (throw-unless (file-exists-p json-file) "package.json not found!")
    json-file))


(defun json-utils--text-width (text)
  "get the size of the biggest line"
  (fp/pipe-deprecated text
    ((fp/split-deprecated "\n")
     (mapcar 'length)
     (apply 'max))))

																				;TODO: create buffer-window-utils!!
;;;###autoload
(defun json-utils--set-window-width (text current-win)
  "(str window) -> bool
Set window width the size of the largest line plus 5"
  (let ((width (+ 5 (json-utils--text-width text))))
    (window-resize current-win (- width (window-width current-win)) t)))

(defvar json-utils--buffer-name "*prettified-json*")


;;;###autoload
(defun json-utils-print-from-selection ()
  "Prints a prettified json selected"
  (interactive)
  (throw-unless (use-region-p) "No region selected!")
  (let* ((selected-text (buffer-substring-no-properties
												 (region-beginning) (region-end)))
         (parsed-json (shell-command-to-string
											 (concat "jq <<< '" selected-text "'")))
         (buff (get-buffer-create json-utils--buffer-name))
         (win (display-buffer-in-side-window
							 buff '((side . right) (display-buffer-mark-dedicated . t)))))
    (json-utils--set-window-width parsed-json win)
    (with-current-buffer json-utils--buffer-name
      (erase-buffer)
      (insert parsed-json)
      (funcall 'json-mode))))

(provide 'json-utils)
;;; json-utils.el ends here


