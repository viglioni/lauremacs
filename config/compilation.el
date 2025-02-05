;;
;; @author Laura Viglioni
;; 2024
;; GNU Public License 3.0
;;


;;
;; Open compilation window only if it is not shown
;;

(defun lauremacs//compilation-buffer-visible? ()
  "Return if compilation buffer is visible in some window/frame."
  (and
   (get-buffer "*compilation*")
   (get-buffer-window-list "*compilation*" nil t)))


(defadvice lauremacs//compilation-start
    (around inhibit-display
            (command &optional mode name-function highlight-regexp))
  "Do not pop up compilation buffer if it is already displayed in some frame/window."
  (when (lauremacs//compilation-buffer-visible?)
    (if (not (string-match "^\\(find\\|grep\\)" command))
        (cl-letf ((display-buffer   #'ignore)
                  (set-window-point #'ignoreco)
                  (goto-char        #'ignore))
          (save-window-excursion
            ad-do-it))
      ad-do-it)))

(ad-activate 'lauremacs//compilation-start)


;;
;; Colour configs 
;;

(require 'ansi-color)

(defun lauremacs/colorize-compilation-buffer ()
  (read-only-mode nil)
  (ansi-color-apply-on-region 1 (point))
  (read-only-mode 1))

(add-hook 'compilation-filter-hook 'lauremacs/colorize-compilation-buffer)

(add-hook 'compilation-mode-hook 'visual-line-mode)
