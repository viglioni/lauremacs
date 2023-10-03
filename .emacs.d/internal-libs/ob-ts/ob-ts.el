;;; org-babel-ts.el --- Run typescript code inside org-babel

;; Copyright (C) 2022 Laura Viglioni

;; Author: Laura Viglioni <viglionilaura@gmail.com>
;; Maintainer: Laura Viglioni <viglionilaura@gmail.com>
;; Created: 23 Dec 2022
;; Keywords: keywods
;; URL: https://github.com/Viglioni/laurisp/tree/main/personal-libs/ts-repl
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
;; Emacs must be able to run `npx ts-node'.
;;
;; With this package, Emacs will be able to run typescript code using ES6 syntax and importing lubibraries
;; 

(require 'ob-js)

;;; Code:

(define-derived-mode ts-mode typescript-mode "ts")

(defconst org-babel-ts-project-dir (concat user-emacs-directory "/org-babel/typescript")
  "Path to the ts project all blocks will be runned in.")

(defconst org-babel-ts-src-dir (concat  org-babel-ts-project-dir "/src")
  "Path to the ts dir all blocks will be runned in.")

(defconst org-babel-ts-package-json
  "{
  \"name\": \"org-babel-ts\",
  \"version\": \"0.1.0\",
  \"private\": true,
  \"dependencies\": {
    \"@types/node\": \"~14.0.5\",
    \"eslint\": \"^7.32.0\",
    \"prettier\": \"~2.0.5\",
    \"ts-node\": \"^10.9.1\",
    \"typescript\": \"^4.9.4\"
  }
}" "Initial package.json to the project.")

(defconst org-babel-ts-tsconfig-json
  "{
  \"compilerOptions\": {
    \"module\": \"commonjs\",
    \"moduleResolution\": \"node\",
    \"declaration\": true,
    \"removeComments\": true,
    \"emitDecoratorMetadata\": true,
    \"experimentalDecorators\": true,
    \"allowSyntheticDefaultImports\": true,
    \"target\": \"ES2019\",
    \"lib\": [\"ES2019\"],
    \"sourceMap\": true,
    \"outDir\": \"./dist\",
    \"incremental\": true,
    \"esModuleInterop\": true,
    \"strict\": true
  },
  \"include\": [\"./src/**/*\"],
  \"exclude\": [\"node_modules\"]
}" "Initial tsconfig.json to the project.")

(defconst org-babel-ts-gitignore
  "node_modules
**/ob-ts.*
package-lock.json", "Initial gitignore file.")

(defun org-babel-ts--write-file (body file)
  "Write the BODY into FILE."
  (with-temp-buffer (insert body) (write-region nil nil file)))

(defun org-babel-ts--create-file-name ()
  "Create a random filename inside `org-babel-ts-src-dir'."
  (concat org-babel-ts-src-dir
          (concat "/" (make-temp-name "ob-ts.") ".ts")))

(defun org-babel-ts--delete-files ()
  "Delete all temporary files inside `org-babel-ts-src-dir'."
  (mapcar 'delete-file (directory-files org-babel-ts-src-dir t "ob-ts")))

(defun org-babel-ts--exec-cmd (cmd)
  "Execute CMD inside `org-babel-ts-project-dir'."
  (shell-command-to-string
   (concat "cd " org-babel-ts-project-dir " && " cmd)))

(defun org-babel-ts-create-project ()
  "Create the TS project to install the libraries and configs."
  (interactive)
  (make-directory org-babel-ts-src-dir t)
  (org-babel-ts--write-file org-babel-ts-package-json
                            (concat org-babel-ts-project-dir "/package.json"))
  (org-babel-ts--write-file org-babel-ts-tsconfig-json
                            (concat org-babel-ts-project-dir "/tsconfig.json"))
  (org-babel-ts--write-file org-babel-ts-gitignore
                            (concat org-babel-ts-project-dir "/.gitignore"))
  (org-babel-ts--exec-cmd "npm i"))

(defun org-babel-ts-install-lib (lib)
  "Run `npm run LIB' inside `org-babel-ts-project-dir'."
  (interactive "s/Insert lib name: ")
  (org-babel-ts--exec-cmd (concat "npm i " lib)))

(defun org-babel-execute:ts (body params)
  "Execute typescript block allowing ES6 syntax and external libraries.
BODY and PARAMS will be passed to `org-babel-execute:js'."
  (print body)
  (print params)
  (org-babel-ts--delete-files)
  (let* ((ts-file (org-babel-ts--create-file-name))
         (org-babel-js-cmd (concat "npx ts-node " ts-file)))
    (make-directory org-babel-ts-src-dir t)
    (org-babel-ts--write-file body ts-file)
    (org-babel-execute:js body params)))


(provide 'ob-ts)

;;; org-babel-ts.el ends here

