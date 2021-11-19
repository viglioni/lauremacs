;;; make-scripts.el --- Make Scripts  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Laura Viglioni

;; Author: Laura Viglioni <viglionilaura@gmail.com>
;; Maintainer: Laura Viglioni <viglionilaura@gmail.com>
;; Created: 28 Aug 2021
;; Keywords: keywods
;; URL: https://github.com/Viglioni/laurisp/tree/main/personal-libs/make-scripts
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
;;  comentary

;;; Code:

(load-lib 'laurisp-core)
(load-lib 'functional)
(load-lib 'lang-scripts)
(require 'helm)
(require 'helm-make)

(defun MS--makefile-path (&optional local)
  (join-path (if local "." (projectile-project-root)) "Makefile"))

;;;###autoload
(defun MS--has-makefile? (&optional local)
  "Returns if there is a makefile in the project root dir
   if local is t, it will check on local dir
   (bool) -> bool"
  (file-exists-p (MS--makefile-path local)))


(defun MS--candidates (&optional local)
  (helm--make-target-list-qp (MS--makefile-path local)))



;;;###autoload
(defun make-scripts:run-command (&optional local)
  (interactive)
  (throw-unless (MS--has-makefile? local) "No makefile was found")
  (helm
   :prompt "Choose command to exec: "
   :sources (helm-build-sync-source "Avaliable scripts from your makefile"
              :candidates (MS--candidates)
              :action (lambda (cmd) (lang-scripts:run-script
                                (concat "make " cmd)
                                (when local "."))))))


(provide 'make-scripts)
;;; make-scripts.el ends here

