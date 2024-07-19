;;; lauriex.el --- 
;; 
;; Filename: lauriex.el
;; Description: 
;; Author: Laura Viglioni
;; Maintainer: 
;; Created: Sun Feb  4 17:25:10 2024 (-0300)
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

(require 'window)
(require 'vterm)
(require 'snake-case-mode)

(defun lauriex--get-dir-name (full-dir-path)
  "Get dir from FULL-DIR-PATH."
  (file-name-nondirectory (directory-file-name full-dir-path)))

(defun lauriex--get-mix-dir ()
  "Get dir with mix.exs file."
  (or (locate-dominating-file default-directory "mix.exs")
      default-directory))

(defun lauriex--buff-name ()
  "Get buffer name for iex dir."
  (format "*iex-%s*" (lauriex--get-dir-name (lauriex--get-mix-dir))))

(defmacro lauriex-with-mix-dir (&rest body)
  "Execute BODY with `default-dir' as the one with mix.exs."
  `(let ((default-directory (lauriex--get-mix-dir)))
     ,@body))


(defun lauriex--buff-has-proc ()
  "Return if iex buffer is running process."
  (process-live-p (get-buffer-process (lauriex--buff-name))))

(defun lauriex--get-buff ()
  "Get iex buffer."
  (if (lauriex--buff-has-proc)
      (get-buffer (lauriex--buff-name))
    (error "IEX buffer is dead, please restart it")))

(defun lauriex--send (code &optional no-new-line)
  "Send CODE to iex buffer."
  (when-let ((buff (lauriex--get-buff)))
    (with-current-buffer buff
      (comint-send-string buff (format "%s%s" code (if no-new-line "" "\n"))))
    (pop-to-buffer buff)))

(defun lauriex--create-iex ()
  "Create iex buffer."
  (lauriex-with-mix-dir
   (let* ((buff-name (lauriex--buff-name))
          (buff      (generate-new-buffer buff-name)))
     (with-current-buffer buff-name (lauriex-mode))
     (lauriex--send " iex -S mix")
     (pop-to-buffer buff))))

(defun lauriex--kill-buffer ()
  "Kill iex buffer."
  (kill-buffer (lauriex--buff-name)))

;;
;; API
;;

(defun lauriex-send-buffer ()
  "Send buffer to REPL."
  (interactive)
  (lauriex--send (buffer-string)))

(defun lauriex-recompile ()
  "Recompile iex."
  (interactive)
  (lauriex--send "recompile()"))

(defun lauriex ()
  "Open iex on current project."
  (interactive)
  (let* ((buff-name   (lauriex--buff-name))
         (has-proc    (not (not (lauriex--buff-has-proc))))
         (buff-exists (not (not (get-buffer buff-name)))))
    (pcase (list has-proc buff-exists)
      ('(nil nil) (lauriex--create-iex))
      ('(nil t)   (progn (kill-buffer buff-name) (lauriex--create-iex)))
      (_          (pop-to-buffer buff-name)))))


;;
;; Lauriex mode
;;

(defun lauriex--insert (str)
  "Return interactive lambda that sends STR to iex."
  `(lambda ()
     (interactive)
     (lauriex--send ,str t)))

(defvar lauriex-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Commands
    (keymap-set map "C-c r" 'lauriex-recompile)
    ;; Switches - with _
    (keymap-set map "-" (lauriex--insert "_"))
    (keymap-set map "_" (lauriex--insert "-"))
    ;; Switches : with ;
    (keymap-set map ":" (lauriex--insert ";"))
    (keymap-set map ";" (lauriex--insert ":"))
    ;; Misc inserts
    (keymap-set map "C-c p" (lauriex--insert "|> "))
    map))

(define-derived-mode lauriex-mode vterm-mode "Lauriex")



(provide 'lauriex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lauriex.el ends here
