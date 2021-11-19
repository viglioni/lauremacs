;;; lang-scripts.el --- Lang Scripts  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Laura Viglioni

;; Author: Laura Viglioni <viglionilaura@gmail.com>
;; Maintainer: Laura Viglioni <viglionilaura@gmail.com>
;; Created: 28 Aug 2021
;; Keywords: 
;; URL: https://github.com/Viglioni/laurisp/tree/main/personal-libs/lang-scripts
;; Version:  0.0.1
;; Package-Requires: ((emacs "24.1"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;;; Commentary:
;; Base lib for opening a buffer to run scripts related to languages, for instance npm run dev

;;; Code:

(message "loading lang-scripts")

;;;###autoload
(defun lang-scripts--open-buffer (buff)
  "Opens a buffer.
   (buff) -> ()"
  (display-buffer-in-side-window buff '((side . bottom))))


;;;###autoload
(defun lang-scripts--is-npm-buff? (buff-or-buff-name)
  "Checks if a buffer name matches the regex of this lib created buffers
   (string | buffer) -> bool"
  (let ((buff-name (if (stringp buff-or-buff-name) buff-or-buff-name
                     (buffer-name buff-or-buff-name))))
    (bool (regex-matches (rx (and "*:" (+ (or alphanumeric "-")) "::" (+ (or alphanumeric "-" ":" "~" "/" "." "&")) ":*")) buff-name))))

;;;###autoload
(defun lang-scripts--get-buffer ()
  "Gets language script buffer, if any, throws otherwise
   () -> buffer | error"
  (let* ((bottom-window (purpose-get-bottom-window))
         (buff-name (and bottom-window (buffer-name (window-buffer bottom-window))))
         (is-npm-buff? buff-name))
    (throw-unless is-npm-buff? "no language script buffer is found")
    (get-buffer buff-name)))


;;;###autoload
(defun NS--active-buffers-alist ()
  "returns an alist (buffer-name . buffer)
   () -> alist string buffer"
  (fp/pipe (buffer-list)
    ((seq-filter 'lang-scripts--is-npm-buff? )
     (mapcar (lambda (buff) (cons (buffer-name buff) buff))))))

(defun lang-scripts--helm-buffer-source ()
  (helm-build-sync-source "Active npm buffers: "
    :volatile t
    :multiline nil
    :candidates (NS--active-buffers-alist)
    :action 'lang-scripts--open-buffer))

;;
;; API
;;

;;;###autoload
(defun lang-scripts:hide-buffer ()
  "Hides a buffer with a npm command running.
   It will hide only if it is on the bottom window and matches the regex
   () -> () | error"
  (interactive)
  (lang-scripts--get-buffer)
  (purpose-delete-window-at-bottom))

;;;###autoload
(defun lang-scripts:go-to-buffer ()
  "Focus on buffer or throws if no buffer is found
   () -> () | error"
  (interactive)
  (fp/pipe (lang-scripts--get-buffer)
    ((get-buffer-window)
     (select-window)))
  (goto-char (point-max)))

(defun lang-scripts:open-active-buffer ()
  "Lists all active npm buffers and opens the selected one"
  (interactive)
  (helm :promp "Choose a buffer to open: "
        :buffer "*helm active npm buffers*"
        :sources (lang-scripts--helm-buffer-source)))

;;;###autoload
(defun lang-scripts:run-script (script-cmd &optional dir)
  "@param (string) a npm command e.g. \"npm run dev\"
   @return void
   Runs this script in a dedicated async shell buffer. It will run on project root if dir is not specified."
  (let* ((cmd-name (fp/pipe script-cmd ((replace-regexp-in-string (rx (and "sh " (* anything) "&& ")) "")
																				(replace-regexp-in-string " " "-"))))
         (full-cmd (concat script-cmd "&& echo \"\n\nFinished!\n\" || echo \"\n\nFinished with errors.\n\""))
         (buff-name (concat "*:" (projectile-project-name) "::" cmd-name ":*"))
         (err-buff-name (concat "*:ERROR::" (projectile-project-name) "::" cmd-name ":*"))
         (buff (get-buffer-create buff-name)))
    (lang-scripts--open-buffer buff)
    (projectile-with-default-dir (or dir (projectile-project-root))
      (async-shell-command script-cmd buff-name err-buff-name))
    (set-window-dedicated-p (get-buffer-window buff) t)))


(provide 'lang-scripts)
;;; lang-scripts.el ends here

