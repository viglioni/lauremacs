;;
;; @author Laura Viglioni
;; 2023
;; GNU Public License 3.0
;;

(message "loading project-scripts...")

(require 'fp)

(defconst PS--basic-dev-dependencies
  '("@types/node"
    "@typescript-eslint/eslint-plugin"
    "@typescript-eslint/parser"
    "eslint"
    "eslint-plugin-immutable"
    "eslint-plugin-unused-imports"
    "husky"
    "lint-staged"
    "prettier"
    "ts-node"
    "typescript"))

(defun PS--dev-install-cmd (dir &optional deps)
  (format
   "cd %s && npm i -D %s"
   dir
   (s-join " " (or deps PS--basic-dev-dependencies))))

(defun PS--write-basic-package-json (dir)
  (let ((content (json-encode-alist
                  `((name        . ,(PS--get-proj-name "/dir"))
                    (version     . "0.1.0")
                    (licence     . "GPL-3.0")
                    (scripts     . (("run" . "ts-node ./src/main.ts")))
                    (lint-staged . (("*" . ("eslint --fix --quiet"
                                            "prettier --write --loglevel error"))))))))
    (write-region content nil (join-path dir "package.json"))))



(defun PS--get-proj-name (dir)
  (fp/pipe dir
    (fp/replace "/$" "")
    'file-name-nondirectory))

(defun PS--copy-config-files (dir)
  (let* ((files-dir (join-path lauremacs-internal-libs-dir "project-scripts/files"))
         (files (directory-files files-dir t "\\.json$")))
    (dolist (file files)
      (let* ((filename (file-name-nondirectory file))
             (newname (join-path dir filename)))
        (copy-file file newname)))))

(defun PS--add-gitignore (dir)
  (write-region
   "node_modules/*
dist/*
out/*
.env
rest/*
build/*
*~"
   nil
   (join-path dir ".gitignore")))

(defun project-scripts-create-ts-proj (dir)
  "Create a typescript project on DIR."
  (interactive "DInsert a directory")
  (let ((src-dir (join-path dir "src")))
    (unless (file-directory-p src-dir) (make-directory src-dir t))
    (write-region "" nil (join-path src-dir "main.ts"))
    (PS--copy-config-files dir)
    (PS--write-basic-package-json dir)
    (PS--add-gitignore dir)
    (async-shell-command (PS--dev-install-cmd dir) "*dev-installs*")
    )
  (projectile-discover-projects-in-search-path))

(provide 'project-scripts)
