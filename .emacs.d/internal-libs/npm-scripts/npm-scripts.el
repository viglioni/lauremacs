;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

(message "loading npm-scripts...")

(require 'helm)
(require 'seq)
(load-lib 'functional)
(load-lib 'json-utils)
(load-lib 'lang-scripts)

;;
;; Vars and common funcs
;;

(defvar NS--default-import-allowed-modes
  '(typescript-mode typescript-tsx-mode js2-mode rjsx-mode js-mode web-mode))

;;;###autoload
(defun NS--allowed-mode? ()
  "Checks if current mode is allowed based on `NS--default-import-allowed-modes'"
  (contains? NS--default-import-allowed-modes major-mode))

;;;###autoload
(defun NS--has-package-json? ()
  "Returns if there is a package.json in the project root dir
   () -> bool"
  (file-exists-p (join-path (projectile-project-root) "package.json")))

(defvar NS--use-nvm "sh ~/.sh/bin/use-nvmrc.sh && "
	"Command to execute script use-nvmrc")

;;
;; NPM choose and run
;;

;;;###autoload
(defun NS--get-scripts (package-json)
  "@param package.json filepath
   @returns a list of all scripts in package.json"
  (fp/pipe package-json
    ((json-read-file)
     (alist-get 'scripts)
     (fp/alist-sort-by-car)
     (mapcar 'car))))

;;;###autoload
(defun NS--build-scripts (scripts-list)
  "@param a list of strings
   @returns an alist of (scriptname . \"npm run script\")
TODO: put this use nvm script in a clever way."
  (mapcar (lambda (script) (cons (format "%s" script)
                            (format "%s npm run %s" NS--use-nvm script)))
          scripts-list))


;;;###autoload
(defun NS--helm-candidates ()
  "Gets the script from package.json and returns an alist (name . command)"
  (fp/pipe (json-utils-get-package-json)
    ((NS--get-scripts)
     (NS--build-scripts))))

;;;###autoload
(defun NS--helm-scripts-source ()
  (helm-build-sync-source "Avaliable scripts on your package.json: "
    :volatile t
    :multiline nil
    :candidates (NS--helm-candidates)
    :action 'lang-scripts:run-script))


;; API

;;;###autoload
(defun npm-scripts:choose-and-run ()
  "Lists all avaliable scripts on package.json and runs the selected one"
  (interactive)
  (helm :prompt "Choose a script to run: "
        :sources (NS--helm-scripts-source)
        :buffer "*helm avaliable npm scripts*"))

;;
;; Install packages
;;

;;;###autoload
(defun npm-scripts:install (package-name)
  (interactive "sInsert package name or just hit enter to run \"npm i\": ")
  (throw-unless (NS--has-package-json?) "No package.json was found!")
  (lang-scripts:run-script (concat NS--use-nvm "npm install " package-name)))

;;;###autoload
(defun npm-scripts:install-dev (package-name)
  (interactive "sInsert package name: ")
  (throw-unless (NS--has-package-json?) "No package.json was found!")
  (throw-if (fp/is-empty? package-name) "package name can't be empty")
  (lang-scripts:run-script (concat NS--use-nvm "npm install -D " package-name)))


;;
;; Import default
;;

(defvar NS--default-import-lib-list
  '(("fp-ts/Array"        . "A")
    ("fp-ts/Either"       . "E")
		("fp-ts/lib/function" . "f")
		("fp-ts/Option"       . "O")
    ("fp-ts/IO"           . "IO")
    ("fp-ts/IOEither"     . "IOE")
    ("fp-ts/TaskEither"   . "TE")
    ("fp-ts/Task"         . "T")
    ("io-ts/Decoder"      . "D")
		("fp-ts/Random"       . "rand")
    ("ramda"              . "R")
    ("lodash/fp"          . "_")
    ("rxjs/operators"     . "rx")))

(defconst NS--default-import-candidates
  (mapcar (lambda (c) (cons (car c) c)) NS--default-import-lib-list))

;;;###autoload
(defun NS--lib-is-imported? (lib-name)
	"string -> boolean.
Check if LIB-NAME is already imported in the buffer."
	(bool (string-match-p
				 (format "^import.+%s[\"'];?$" lib-name)
				 (buffer-string))))

;;;###autoload
(defun NS--add-import-to-file (candidate)
	"TODO add organize imports"
  (let* ((import-as (cdr candidate))
         (lib-name (car candidate))
				 (import-line (format "import * as %s from '%s'\n"
															import-as lib-name)))
		(save-excursion
			(beginning-of-buffer)
			(if (not (NS--lib-is-imported? lib-name))
					(progn (insert import-line)
								 (message (concat "Inserted " lib-name)))
				(message (concat lib-name " was already inserted"))))))

;;;###autoload
(defun NS--default-import-helm-src ()
  (helm-build-sync-source "Default import: "
    :candidates 'NS--default-import-candidates
    :action 'NS--add-import-to-file))


;; API

;;;###autoload
(defun npm-scripts:import-default-lib ()
  "Add to the first empty line of the code an default js/ts import
  e.g.: import * as E from 'fp-ts/Either'
  according to the assoc list `NS--default-import-lib-list'.
  Will only add if the current buffer is in one of the modes listed in
  `NS--default-import-allowed-modes'"
  (interactive)
  (throw-unless (NS--allowed-mode?)"Not in a js/ts mode!")
  (helm
   :prompt "Choose lib to import default: "
   :sources (NS--default-import-helm-src)))

;;
;; Open package.json
;;

(defun npm-scripts:open-package-json ()
  "Opens package.json in a side buffer, throws if find none."
  (interactive)
  (let ((package-json (json-utils-get-package-json)))
    (find-file-other-window package-json)))


(provide 'npm-scripts)

;;
;; compiler warnings
;;

;; In NS--get-scripts:
;; npm-scripts.el:19:25:Warning: function ‘reduce’ from cl package called at
;; runtime

;; In NS--run-script:
;; npm-scripts.el:45:24:Warning: function ‘reduce’ from cl package called at
;; runtime

;; In NS--active-buffers-alist:
;; npm-scripts.el:81:15:Warning: function ‘reduce’ from cl package called at
;; runtime

;; In NS--helm-candidates:
;; npm-scripts.el:91:8:Warning: function ‘reduce’ from cl package called at
;; runtime

