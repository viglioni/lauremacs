;;; init-utils.el --- Description -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defun l/join-path (path filename) ;; using namespace avoids conflict w/ external packages
  "concat path and file. Adds '/' to the end of the path if necessary"
  (concat path (if (string-match-p "/$" path) "" "/") filename))


(provide 'init-utils) ;; ** PROVIDE FILE TO BE REQUIRED BY OTHERS FILES
;;; init-utils.el ends here
