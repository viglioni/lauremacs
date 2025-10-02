(require 'ansi-color)

(defun lauremacs/colorize-compilation-buffer ()
  (read-only-mode nil)
  (ansi-color-apply-on-region 1 (point))
  (read-only-mode 1))

(add-hook 'compilation-filter-hook 'lauremacs/colorize-compilation-buffer)

(add-hook 'compilation-mode-hook 'visual-line-mode)
