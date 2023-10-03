;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; Insert on buffer or clipboard
;;

(lauremacs-leader
	"i"   '(nil																	:which-key "insert")
	"iu"  '(nil																	:which-key "uuid")
	"iuu" '(lauremacs/insert-uuid								:which-key "insert uuid")
	"iuc" '(lauremacs/insert-uuid-to-clipboard	:which-key "copy uuid")
	"ii"  '(all-the-icons-insert								:which-key "insert icon"))

;;
;; Lang scripts
;;

(bind-lazy-function 'npm-choose-and-run
										'npm-scripts:choose-and-run
										'npm-scripts)
(bind-lazy-function 'hide-script-buffer
										'lang-scripts:hide-buffer
										'lang-scripts)
(bind-lazy-function 'open-script-buffer
										'lang-scripts:open-active-buffer
										'lang-scripts)
(bind-lazy-function 'go-to-script-buffer
										'lang-scripts:go-to-buffer
										'lang-scripts)
(bind-lazy-function 'import-default-lib
										'npm-scripts:import-default-lib
										'npm-scripts)
(bind-lazy-function 'npm-install-lib
										'npm-scripts:install
										'npm-scripts)
(bind-lazy-function 'npm-install-dev-lib
										'npm-scripts:install-dev
										'npm-scripts)
(bind-lazy-function 'open-package-json
										'npm-scripts:open-package-json
										'npm-scripts)
(bind-lazy-function 'run-make-cmd
										'make-scripts:run-command
										'make-scripts)

(require 'project-scripts)

(general-define-key
 :prefix "<f17> <f17>"
 "" '(nil :which-key "language scripts"))

(general-define-key
 :prefix "<f17> <f17> <f17>"
 ""		'(nil									:which-key "common commands")
 "h"	'(hide-script-buffer	:which-key "hide script buffer")
 "o"	'(open-script-buffer	:which-key "open script buffer")
 "g"	'(go-to-script-buffer :which-key "go to script buffer"))

(general-define-key
 :prefix "<f17> <f17> n"
 ""   '(nil									:which-key "npm scripts")
 "r"  '(npm-choose-and-run	:which-key "run package.json script")
 "l"  '(import-default-lib	:which-key "import default lib")
 "i"  '(npm-install-lib			:which-key "install lib")
 "d"  '(npm-install-dev-lib :which-key "install dev lib")
 "o"  '(nil									:which-key "open file")
 "op" '(open-package-json		:which-key "package.json"))

(general-define-key
 :prefix "<f17> <f17> m"
 ""		'(nil						:which-key "makefile scripts")
 "r"	'(run-make-cmd	:which-key "run make command"))

(general-define-key
 :prefix "<f17> <f17> p"
 ""	 '(nil                            :which-key "project scripts")
 "t" '(project-scripts-create-ts-proj	:which-key "create ts project"))

;;
;; Toggle
;;


(lauremacs-leader
	"T"		'(nil																:which-key "toggle/choose")
	"Tp"	'(lauremacs/toggle-pair-programming :which-key "pair programming mode")
	"TT"	'(lauremacs/choose-theme						:which-key "choose theme")
	"Tt"	'(lauremacs/toggle-transparency			:which-key "transparency")
	"Tl"	'(linum-mode												:which-key "linum mode"))

;;
;; Calibre
;;

(use-package calibredb
  :init
  (setq calibredb-root-dir "~/boeken")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-program "/Applications/calibre.app/Contents/MacOS/calibredb")
  (setq calibredb-device-dir "/Volumes/Kindle")
  (setq calibredb-format-all-the-icons t)
  (lauremacs-leader
    "ac" '(nil :which-key "calibre")
    "acc" '(calibredb :which-key "calibre")
    "aca" '(calibredb-add :which-key "add book")
    "acf" '(calibredb-find-helm :which-key "find")))


;;
;; Docker
;;

(use-package docker
  :init
  (lauremacs-leader
    "ad" '(docker :which-key "docker")))

