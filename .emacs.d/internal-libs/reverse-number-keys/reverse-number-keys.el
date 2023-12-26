;;; reverse-number-keys.el --- 
;; 
;; Filename: reverse-number-keys.el
;; Description: 
;; Author: Laura Viglioni
;; Maintainer: 
;; Created: Tue Dec 26 11:33:46 2023 (-0300)
;; Version: 0.0.1
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
;; 1 turns to ! and ! turns to 1
;; 2 turns to @ and @ turns to 2
;; etc
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

(defun rnk--insert (str)
  "Return an interactive lambda that will insert STR."
  (lambda () (interactive) (insert str)))

(define-minor-mode reverse-number-keys-mode
  "Reverse number keys.  For instance:
1 turns to ! and ! turns to 1
2 turns to @ and @ turns to 2
etc."
  :init-value nil
  :lighter " 123"
  :keymap `(("1" . ,(rnk--insert "!")) ("!" . ,(rnk--insert "1"))
            ("2" . ,(rnk--insert "@")) ("@" . ,(rnk--insert "2"))
            ("3" . ,(rnk--insert "#")) ("#" . ,(rnk--insert "3"))
            ("4" . ,(rnk--insert "$")) ("$" . ,(rnk--insert "4"))
            ("5" . ,(rnk--insert "%")) ("%" . ,(rnk--insert "5"))
            ("6" . ,(rnk--insert "^")) ("^" . ,(rnk--insert "6"))
            ("7" . ,(rnk--insert "&")) ("&" . ,(rnk--insert "7"))
            ("8" . ,(rnk--insert "*")) ("*" . ,(rnk--insert "8"))
            ("9" . ,(rnk--insert "(")) ("(" . ,(rnk--insert "9"))
            ("0" . ,(rnk--insert ")")) (")" . ,(rnk--insert "0"))))

(provide 'reverse-number-keys)

;;; reverse-number-keys.el ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reverse-number-keys.el ends here
