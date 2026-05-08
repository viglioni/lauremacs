;;; -*- lexical-binding: t; l-syntax: t -*-
(config-package vterm)
(config-package vterm-toggle)

(defun lauterm/open-specific-shell (buffer-name command)
  "Open or switch to a vterm buffer named BUFFER-NAME running COMMAND.
If a buffer named BUFFER-NAME already exists, switch to it.
Otherwise, create a new vterm buffer with that name, send COMMAND
to it, and press return to execute it."
  (if (get-buffer buffer-name)
      (switch-to-buffer buffer-name)
    (let ((vterm-buffer (vterm buffer-name)))
      (with-current-buffer vterm-buffer
        (vterm-send-string command)
        (vterm-send-return)))))

