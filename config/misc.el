;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;



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



;;
;; Calibre
;;

(use-package calibredb
  :init
  (setq calibredb-root-dir "~/boeken")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-program "/Applications/calibre.app/Contents/MacOS/calibredb")
  (setq calibredb-device-dir "/Volumes/Kindle")
  (setq calibredb-format-all-the-icons t))


;;
;; Docker
;;

(use-package docker)

