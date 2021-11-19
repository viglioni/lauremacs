;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; Insert on buffer or clipboard
;;

(lauremacs-leader
	"i"   '(nil :which-key "insert")
	"iu"  '(nil :which-key "uuid")
	"iuu" '(lauremacs/insert-uuid              :which-key "insert uuid")
	"iuc" '(lauremacs/insert-uuid-to-clipboard :which-key "copy uuid")
	"ii"  '(all-the-icons-insert               :which-key "insert icon"))

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


(general-define-key
 :prefix "<f17> <f17>"
 "" '(nil :which-key "language scripts"))

(general-define-key
 :prefix "<f17> <f17> <f17>"
 ""  '(nil :which-key "common commands")
 "h" '(hide-script-buffer  :which-key "hide script buffer")
 "o" '(open-script-buffer  :which-key "open script buffer")
 "g" '(go-to-script-buffer :which-key "go to script buffer"))

(general-define-key
 :prefix "<f17> <f17> n"
 ""   '(nil :which-key "npm scripts")
 "r"  '(npm-choose-and-run  :which-key "run package.json script")
 "l"  '(import-default-lib  :which-key "import default lib")
 "i"  '(npm-install-lib     :which-key "install lib")
 "d"  '(npm-install-dev-lib :which-key "install dev lib")
 "o"  '(nil :which-key "open file")
 "op" '(open-package-json   :which-key "package.json"))

(general-define-key
 :prefix "<f17> <f17> m"
 ""  '(nil :which-key "makefile scripts")
 "r" '(run-make-cmd :which-key "run make command"))

;;
;; Toggle
;;

(defvar lauremacs-state//pair-programming? nil)

;;;###autoload
(defun lauremacs/enable-pair-programming ()
  "Change some display configs to enhance
pair-programming/sharing screen experience"
  (interactive)
	(setq lauremacs-state//pair-programming? t)
  (set-face-attribute 'default nil :height 175)
  (global-linum-mode 1)
  (with-current-buffer " *NeoTree*"
    (setq-local linum-mode nil))
  (neotree-toggle)
  (neotree-toggle))

;;;###autoload
(defun lauremacs/disable-pair-programming ()
  "Change some display configs to enhance
pair-programming/sharing screen experience"
  (interactive)
	(setq lauremacs-state//pair-programming? nil)
  (set-face-attribute 'default nil :height 150)
  (global-linum-mode 0))

;;;###autoload
(defun lauremacs/toggle-pair-programming ()
	"Toggle between pair programming mode and norma mode."
	(interactive)
	(if lauremacs-state//pair-programming?
			(lauremacs/disable-pair-programming)
		(lauremacs/enable-pair-programming)))

(lauremacs-leader
	"T"  '(nil :which-key "toggle")
	"Tp" '(lauremacs/toggle-pair-programming :which-key "pair programming mode"))
