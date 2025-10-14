;;; lauremacs-posframe.el --- Posframe utilities for lauremacs -*- lexical-binding: t; l-syntax: t -*-
;;
;; Filename: lauremacs-posframe.el
;; Description: Posframe utilities for lauremacs configuration
;; Author: Laura Viglioni
;; Maintainer: Laura Viglioni
;; Created: 2025-10-08
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (posframe "1.4.4") (projectile "2.0") (helm "3.0"))
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Doc URL:
;; Keywords: convenience, frames
;; Compatibility: Emacs 26.1+
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This library provides posframe-based utilities for lauremacs,
;; including functions to open files in floating frames.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2025-10-08 - Initial creation
;;   - Added lpf/open-lauremacs-config
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'posframe)
(require 'projectile)
(require 'helm)

;;;###autoload
(defun lpf/open-lauremacs-config ()
  "Open a file from .emacs.d in a posframe."
  (interactive)
  (let* ((default-directory (expand-file-name "~/.emacs.d/"))
         (helm-projectile-fuzzy-match t))
    (helm :sources (helm-build-sync-source "Emacs config files"
                     :candidates (lambda ()
                                   (with-temp-buffer
                                     (hack-dir-local-variables-non-file-buffer)
                                     (let ((projectile-project-root default-directory))
                                       (projectile-current-project-files))))
                     :fuzzy-match t
                     :action (lambda (file)
                               (let* ((file-path (expand-file-name file default-directory))
                                      (buffer (find-file-noselect file-path))
                                      (posframe-buffer-name (format " *emacs-config-posframe: %s*"
                                                                    (file-name-nondirectory file))))
                                 ;; Create or update the posframe buffer with file contents
                                 (with-current-buffer (get-buffer-create posframe-buffer-name)
                                   (erase-buffer)
                                   (insert-buffer-substring buffer)
                                   (setq-local buffer-file-name file-path)
                                   (set-auto-mode)
                                   (set-buffer-modified-p nil)
                                   (local-set-key (kbd "C-c C-c")
                                                  (lambda ()
                                                    (interactive)
                                                    (posframe-delete posframe-buffer-name))))
                                 ;; Show the posframe
                                 (when (posframe-workable-p)
                                   (posframe-show posframe-buffer-name
                                                  :poshandler #'posframe-poshandler-frame-center
                                                  :cursor 'box
                                                  :width 100
                                                  :height 30
                                                  :border-width 2
                                                  :border-color (face-attribute 'default :foreground nil t)
                                                  :internal-border-width 1
                                                  :internal-border-color (face-attribute 'fringe :background nil t)
                                                  :accept-focus t
                                              )))))
          :buffer "*helm emacs.d*"
          :prompt "Emacs config file: ")))

(provide 'lauremacs-posframe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lauremacs-posframe.el ends here
