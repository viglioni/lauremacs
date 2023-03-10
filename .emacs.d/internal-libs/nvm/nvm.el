;;; nvm.el --- library to handle node versions inside Emacs
;; 
;; Filename: nvm.el
;; Description: Handle node version inside Emacs
;; Author: Laura Viglioni
;; Maintainer: Laura Viglioni
;; Created: Thu Aug 25 10:01:24 2022 (-0300)
;; Version: 0.0.1
;; Package-Requires: ()
;; Last-Updated: Thu Aug 25 10:02:01 2022 (-0300)
;;           By: Laura Viglioni
;;     Update #: 1
;; URL: http://github.com/viglioni/lauremacs
;; Doc URL: 
;; Keywords: node
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


  
(require 'functional)
(require 'laurisp-core)
(require 'projectile)
(require 'helm)

(defconst nvm--download-url
	"https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash"
	"Nvm url download: `https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash'.")

(defvar nvm-dir (or (getenv "NVM_DIR") (file-truename "~/.nvm"))
	"Directory where nvm is installed.")

(defun nvm--exec-path-from-shell-initialize ()
	"When on macos, updates exec path."
	(use-dependencies 'exec-path-from-shell)
	(when (memq window-system '(mac ns x))
		(exec-path-from-shell-initialize)))

(defun nvm--path-rx ()
	"Return a regex string of the node bin path."
	(format "%s/versions/node/v[0-9.]+/bin:?" nvm-dir))

(defun nvm--remove-node-from-path ()
	"Remove node from Emacs path."
	(fp/pipe-deprecated (getenv "PATH")
		((replace-regexp-in-string (nvm--path-rx) "")
		 (setenv "PATH"))))

(defun nvm--add-node-to-path (version)
	"Add node to Emacs path.
VERSION should be of the form \"vXX.XX.X\"."
	(fp/pipe-deprecated (getenv "PATH")
		((concat (format "%s/versions/node/%s/bin:" nvm-dir version))
		 (setenv "PATH")))
	(nvm--exec-path-from-shell-initialize))

(defun nvm--use-version (version)
	"Add node to Emacs path.
VERSION should be of the form \"vXX.XX.X\"."
	(unless (nvm--version-instaled-p version)
		(nvm-install version))
	(nvm--remove-node-from-path)
	(nvm--add-node-to-path version)
	(message (concat "Using node " version)))

(defun nvm--shell-command-exists-p (cmd)
	"Check if CMD exists in the machine."
	(fp/pipe-deprecated cmd
		((concat "command -v ")
		 (shell-command-to-string)
		 (fp/is-empty?)
		 (not))))

(defun nvm--sh-path ()
	"Return nvm.sh path if exists, else throw error."
	(let ((nvm-sh (join-path nvm-dir "nvm.sh")))
		(if (file-exists-p nvm-sh) nvm-sh
			(error "File nvm.sh not found!"))))

(defun nvm--list-installed ()
	"Return a list with all node versions installed."
	(let ((versions-dir (join-path nvm-dir "/versions/node")))
		(if (file-directory-p versions-dir)
				(directory-files versions-dir nil "v.*")
			nil)))

(defun nvm--version-instaled-p (version)
	"Return if VERSION is installed.
VERSION should be on the form \"v.XX.XX.X\"."
	(fp/contains? (nvm--list-installed) version))

(defun nvm--stringify (str-or-num)
	"Return string from STR-OR-NUM."
	(if (numberp str-or-num)
			(number-to-string str-or-num)
		str-or-num))

(defun nvm--sync-install (version)
	"Install node VERSION using a syncronous function and return the version."
	(let* ((v (nvm--stringify version))
				 (cmd (format "source ~/.nvm/nvm.sh && nvm install %s" v)))
		(message (format "Installing node %s..." v))
		(fp/pipe-deprecated cmd
			((shell-command-to-string)
			 (regex-matches "node v[0-9.]+")
			 (car)
			 (regex-matches "v[0-9.]+")
			 (car)))))

(defun nvm-run-command (cmd &optional buff err-buff)
	"CMD is a nvm available command, e.g. \"use\", \"install\" etc.
BUFF and ERR-BUFF are the buffer names that the command will take place."
	(interactive "sinsert nvm command: ")
	(async-shell-command (format "source %s && nvm %s"
															 (nvm--sh-path) cmd)
											 (or buff			"*nvm-command*")
											 (or err-buff "*nvm-command-error*")))

(defun nvm-donwload (&optional engine not-install-latest)
	"Downloads nvm using ENGINE.
ENGINE should be 'curl or 'wget.
Curl is default.
If NOT-INSTALL-LATEST is non nil it won't run `nvm install latest'."
	(interactive)
	(let* ((eng (or engine 'curl))
				 (eng-name (symbol-name eng))
				 (cmd (cond ((eq eng 'curl) "curl -o- ")
										((eq eng 'wget) "wget -qO- ")))
				 (latest (if not-install-latest ""
									 " && source ~/.nvm/nvm.sh && nvm install node")))
		(throw-unless (nvm--shell-command-exists-p eng-name)
									(format "%s: command not found" eng-name))
		(async-shell-command (concat cmd nvm--download-url latest)
												 "*nvm-download*"
												 "*nvm-download-error*")))

(defun nvm-install (version)
	"Run `nvm install VERSION'.
VERSION can be a number or string.
E.g.:
\(nvm-install 16)
\(nvm-install \"16\")
\(nvm-install \"16.13.1\"."
	(interactive "sInsert version: ")
	(let ((v (nvm--stringify version)))
		(message (format "Installing %s..." v))
		(nvm-run-command (format "install %s" v))))

(defun nvm-get-current ()
	"Return current node version in Emacs path."
	(interactive)
	(let ((path (regex-matches (nvm--path-rx) (getenv "PATH"))))
		(when path
			(fp/pipe-deprecated path
				((car) (regex-matches "v[0-9.]+") (car))))))

(defun nvm-use-project-version ()
	"Use node version in `.nvmrc' file."
	(interactive)
	(let ((nvmrc (join-path (projectile-project-root) ".nvmrc")))
		(throw-unless (file-exists-p nvmrc) ".nvmrc not found!")
		(let ((version (fp/pipe-deprecated nvmrc
										 ((concat "cat ")
											(shell-command-to-string)
											(regex-matches "^v?[0-9.]+$")
											(car)))))
			(throw-unless version "invalid version")
			(nvm--use-version version))))

(defun nvm-use ()
	"Select a version to use."
	(interactive)
	(helm :prompt "Select or insert a node version to use: "
				:sources (list (helm-build-sync-source "node-versions"
												 :candidates (nvm--list-installed)
												 :action 'nvm--use-version)
											 (helm-build-dummy-source "install-node-version"
												 :action '(lambda (v) (nvm--use-version (nvm--sync-install v)))))))



(provide 'nvm)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nvm.el ends here
