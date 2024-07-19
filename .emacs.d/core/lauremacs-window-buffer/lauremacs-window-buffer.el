;;; lauremacs-window-buffer.el --- set of functions to control buffers and windows
;;;###autoload

;; Copyright (C) 2021 Laura Viglioni

;; Author: Laura Viglioni <viglionilaura@gmail.com>
;; Maintainer: Laura Viglioni <viglionilaura@gmail.com>
;; Created: 05 10 2021
;; Keywords: 
;; URL: https://github.com/Viglioni/lauremacs
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
;; :)

;;; Code:

(require 'functional)
(require 'laurisp-core)
(use-dependencies 'helm 'projectile)

;;;###autoload
(defun lauremacs--get-buffers ()
  (if (projectile-project-p)
      (projectile-project-buffers)
    (buffer-list)))

;;;###autoload
(defun lauremacs-get-buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string
    major-mode))


;;;###autoload
(defun lauremacs--get-prev-buffers ()
  "Return list with previous buffers."
  (fp/pipe (lauremacs--get-buffers)
    (fp/filter (fp/compose 'not 'get-buffer-window))))

;;;###autoload
(defun lauremacs/window-split-single-column ()
  "Set the layout to single column."
  (interactive)
  (delete-other-windows)
  (balance-windows))

;;;###autoload
(defun lauremacs/window-split-double-columns ()
  "Set the layout to two columns.
TODO: use perspectives
TODO: throw if not splitable"
  (interactive)
  (let ((prev-buffer (or (car (lauremacs--get-prev-buffers)) (current-buffer))))
    (delete-other-windows)
    (set-window-buffer (split-window-right) prev-buffer)
    (balance-windows)))

;;;###autoload
(defun lauremacs/window-split-triple-columns ()
  "Set the layout to three columns."
  (interactive)
  (delete-other-windows)
  (let* ((prev-buffers (mapcar 'buffer-name (lauremacs--get-prev-buffers)))
         (snd-buffer (or (car prev-buffers) (current-buffer)))
         (trd-buffer (or (nth 1 prev-buffers) (current-buffer)))
         (second (split-window-right))
         (third (split-window second nil 'right)))
    (set-window-buffer second snd-buffer)
    (set-window-buffer third trd-buffer)
    (balance-windows)))

;;;###autoload
(defun lauremacs/window-split-grid ()
  "Set the layout to a 2*2 grid."
  (interactive)
  (delete-other-windows)
  (let* ((prev-buffers (mapcar 'buffer-name (lauremacs--get-prev-buffers)))
         (snd-buffer (or (car prev-buffers) (current-buffer)))
         (trd-buffer (or (nth 1 prev-buffers) (current-buffer)))
         (fourth-buffer (or (nth 2 prev-buffers) (current-buffer)))
         (second (split-window-right))
         (third (split-window-below))
         (fourth (split-window second nil 'below)))
    (set-window-buffer second snd-buffer)
    (set-window-buffer third trd-buffer)
    (set-window-buffer fourth fourth-buffer)
    (balance-windows)))

(defun lauremacs/toggle-current-window-dedication ()
  "Toggle dedication state of a window."
  (interactive)
  (let* ((current-window (selected-window))
         (dedicated? (window-dedicated-p current-window)))
    (set-window-dedicated-p current-window (not dedicated?))
    (message "Window %sdedicated to %s"
             (if dedicated? "no longer " "")
             (buffer-name))))


;;;###autoload
(defun lauremacs/switch-to-last-buffer ()
  "Switch to last buffer."
  (interactive)
  (switch-to-buffer (car (lauremacs--get-prev-buffers))))

;;;###autoload
(defun lauremacs/window-layout-toggle ()
  "Toggle between horizontal and vertical layout of two windows.
TODO: add throwif"
  (interactive)
  (when (not (= 2 (count-windows)))
    (error "Can't toggle window layout when the number of windows isn't two"))
  (let* ((w-tree (car (window-tree)))
				 (current-split-is-vertical? (car w-tree))
				 (first-window (nth 2 w-tree))
				 (second-window (nth 3 w-tree))
				 (second-window-state (window-state-get second-window))
				 (split-fn (if current-split-is-vertical? #'split-window-horizontally #'split-window-vertically)))
    (delete-other-windows first-window)
    (window-state-put second-window-state (funcall split-fn))))

;;;###autoload
(defun lauremacs/buffer-indent ()
	"Indent whole buffer."
	(interactive)
	(indent-region (point-min) (point-max))
	(align-entire))

;;;###autoload
(defun lauremacs/new-frame-projectile ()
	(interactive)
	(make-frame-command)
	(switch-to-buffer lauremacs-buffer-name)
	(lauremacs/window-split-single-column)
	(helm-projectile-switch-project))

;;;###autoload
(defun lauremacs/switch-buffer ()
  "Choose which switch buffer function must be used depending if it is on a project or not."
  (interactive)
  (if (projectile-project-p)
      (helm-projectile-switch-to-buffer)
    (helm-mini)))

;;;###autoload
(defun lauremacs-buffer//buff-exists? (buff-name)
  "Check if BUFF-NAME exists."
  (fp/pipe (buffer-list)
    (fp/map 'buffer-name)
    (fp/member buff-name)))

;;;###autoload
(defun lauremacs-buffer//pop-buf-bottom (buff-name)
  "Open BUFF-NAME in pop window."
  (let ((buff (get-buffer-create buff-name)))
    (display-buffer-in-side-window buff '((side . bottom)))
    (select-window (get-buffer-window buff))))


(defun lauremacs-buffer/pop-bottom ()
  (interactive)
  (helm
	 :promp "Choose a buffer: "
	 :buffer "*helm buffers*"
	 :sources (list (helm-build-sync-source "Project buffers"
							      :action 'lauremacs-buffer//pop-buf-bottom
							      :candidates (mapcar 'buffer-name (projectile-project-buffers)))
                  (helm-build-sync-source "Non project buffers"
							      :action 'lauremacs-buffer//pop-buf-bottom
							      :candidates (mapcar 'buffer-name (seq-difference (buffer-list) (projectile-project-buffers))))
                  (helm-build-dummy-source "New buffer"
                    :action 'lauremacs-buffer//pop-buf-bottom))))

(provide 'lauremacs-window-buffer)

;;; lauremacs-window-buffer.el ends here
