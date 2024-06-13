;;; elauxir.el --- elixir helpers
;; 
;; Filename: elauxir.el
;; Description: Elixir helpers
;; Author: Laura Viglioni
;; Maintainer: Laura Viglioni
;; Created: Fri Feb 16 12:19:22 2024 (-0300)
;; Version: 0.0.1
;; Package-Requires: ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;; A list of elixir helper functions.
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

(require 'laurisp-core)
(require 'functional)
(require 'projectile)



;;
;; Grep
;;

;;;###autoload
(defun elauxir--grep (file-path-rx)
  "Return a function that grep in FILE-PATH-RX files.
FILE-PATH-RX is e.g. *.exs?$"
  (lambda ()
    (interactive)
    (let ((initial-search (read-string "initial query: " (if (region-active-p)
                                                             (region-string)
                                                           (word-at-point)))))
      (helm-do-ag (projectile-project-root)
                  nil
                  (concat "-G=" file-path-rx " " initial-search)))))


;;
;; Elixir Testing
;;

;;;###autoload
(defun elauxir-test-this-file ()
  "Test file where this command is executed."
  (interactive)
	(let ((compilation-read-command nil))
		(projectile--run-project-cmd
		 (concat "mix test " (buffer-file-name))
		 projectile-test-cmd-map
		 :show-prompt nil
		 :prompt-prefix "Test command: "
		 :save-buffers t
		 :use-comint-mode projectile-test-use-comint-mode)))

;;;###autoload
(defun elauxir-run-this-test ()
  "Test the test suit under the cursor."
  (interactive)
	(let ((compilation-read-command nil)
        (line (replace-regexp-in-string "[^0-9]" "" (what-line))))
		(projectile--run-project-cmd
		 (concat "mix test " (buffer-file-name) ":" line)
		 projectile-test-cmd-map
		 :show-prompt nil
		 :prompt-prefix "Test command: "
		 :save-buffers t
		 :use-comint-mode projectile-test-use-comint-mode)))


;;
;; Heex
;;

;;;###autoload
(defun elauxir-switch-ex-heex ()
  "Switch between `elixir-ts-mode' and `heex-ts-mode'."
  (interactive)
  (pcase major-mode
    ('elixir-ts-mode (heex-ts-mode))
    ('heex-ts-mode   (elixir-ts-mode)))
  (message (format "Switched to %s" major-mode)))


;;
;; Projectile impl-test-file
;;

;;;###autoload
(defun elauxir--create-file (file-path)
  "Create file if it does not exist.  FILE-PATH is relative to project root."
  (let* ((full-path (join-path (projectile-project-root) file-path))
         (dir-path (file-name-directory full-path)))
    (unless (file-exists-p full-path)
      (make-directory dir-path t)
      (write-region "" nil full-path)
      (message (format "Created file: %s" file-path)))
    file-path))

;;;###autoload
(defun elauxir-impl-test-file (file)
  "Return the implementation/test file related to FILE."
  (cond ((string-match-p "^test" file)
         (fp/pipe file
           (fp/replace "^test" "lib")
           (fp/replace "_test\\.exs$" ".ex")
           'elauxir--create-file
           ))
        ((string-match-p "^lib" file)
         (fp/pipe file
           (fp/replace "^lib" "test")
           (fp/replace "\\.ex$" "_test.exs")
           'elauxir--create-file))))

;;
;; Mix helpers
;;

(defconst elauxir--mix-buff-name
  "*mix-command*"
  "Buffer name from CMD.")

(defconst elauxir--mix-buff-err-name
    "*error:mix-command*"
    "Generate error buffer name from CMD.")

(defvar elauxir-helm-hist nil)

(defconst elauxir-mix-candidates
  `(;; mix default commands
    ("install"                 . "mix install")
    ("get deps"                . "mix deps.get")
    ;; ecto
    ("ecto generate migration" "mix ecto.gen.migration" "Migration name: ")
    ("ecto drop"               . "mix ecto.drop")
    ("ecto drop test env"      . "MIX_ENV=test mix ecto.drop")
    ("ecto setup"              . "mix ecto.setup")
    ("ecto setup test env"     . "MIX_ENV=test mix ecto.setup")
    ("ecto migrate all"        . "mix ecto.migrate && MIX_ENV=test mix ecto.migrate")
    ("ecto migrate local env"  . "mix ecto.migrate")
    ("ecto migrate test env"   . "MIX_ENV=test mix ecto.migrate")
    ("ecto rollback all"       . "mix ecto.rollback && MIX_ENV=test mix ecto.rollback")
    ("ecto rollback local env" . "mix ecto.rollback")
    ("ecto rollback test env"  . "MIX_ENV=test mix ecto.rollback")
    ))

(defun elauxir--run-cmd (cmd)
  "Execute CMD."
  (projectile-run-async-shell-command-in-root
   (format "echo %s && %s" cmd cmd)
   elauxir--mix-buff-name
   elauxir--mix-buff-err-name))

(cl-defmethod elauxir--mix-exec ((cmd string))
  "Execute CMD."
  (elauxir--run-cmd cmd))

(cl-defmethod elauxir--mix-exec ((cmd-list list))
  "CMD-LIST :: '(cmd prompt)."
  (let* ((cmd      (car cmd-list))
         (prompt   (cadr cmd-list))
         (suffix   (read-string prompt))
         (full-cmd (format "%s %s" cmd suffix)))
    (elauxir--run-cmd full-cmd)))

(defun elauxir-mix ()
  "Prompt helm to execute mix commands."
  (interactive)
  (helm :prompt "Choose mix command: "
        :sources (helm-build-sync-source "Mix commands: "
                   :volatile t
                   :multiline nil
                   :candidates 'elauxir-mix-candidates
                   :fuzzy-match t
                   :action 'elauxir--mix-exec)
        :buffer "*helm mix commands*"))

(provide 'elauxir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elauxir.el ends here
