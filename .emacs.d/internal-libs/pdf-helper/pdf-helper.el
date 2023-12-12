;;; pdf-helper.el --- 
;; 
;; Filename: pdf-helper.el
;; Description: 
;; Author: Laura Viglioni
;; Maintainer: 
;; Created: Thu Jun 29 19:38:14 2023 (-0300)
;; Version: 
;; Package-Requires: ()
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
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


(require 'pdf-view)
(require 'lauremacs-window-buffer)

;;;###autoload
(defun pdf-helper--window-major-mode (window)
  "Return major mode from buffer displayed in WINDOW."
  (lauremacs-get-buffer-mode (window-buffer window)))

;;;###autoload
(defun pdf-helper--major-mode-window-alist ()
  "Build an alist with (major-mode . window) of all windows."
  (mapcar
   (lambda (window) (cons (pdf-helper--window-major-mode window) window))
   (window-list)))

;;;###autoload
(defun pdf-helper--get-pdf-window ()
  "Get the first window with `pdf-view-mode'."
  (alist-get 'pdf-view-mode (pdf-helper--major-mode-window-alist)))

;;;###autoload
(defun pdf-helper-prev-page ()
  "Run `pdf-view-previous-page' on the first window with `pdf-view-mode' without leaving the current buffer."
  (interactive)
  (let ((pdf-window (pdf-helper--get-pdf-window))
        (cur-window (car (window-list))))
    (select-window pdf-window)
    (pdf-view-previous-page-command)
    (select-window cur-window)))

;;;###autoload
(defun pdf-helper-next-page ()
  "Run `pdf-view-next-page' on the first window with `pdf-view-mode' without leaving the current buffer."
  (interactive)
  (let ((pdf-window (pdf-helper--get-pdf-window))
        (cur-window (car (window-list))))
    (select-window pdf-window)
    (pdf-view-next-page-command)
    (select-window cur-window)))

(provide 'pdf-helper)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pdf-helper.el ends here
