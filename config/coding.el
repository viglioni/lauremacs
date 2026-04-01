;;; -*- lexical-binding: t; l-syntax: t -*-
(with-eval-after-load "prog-mode"
  (lauremacs-major-mode-leader
    :keymaps 'prog-mode-map
    "t"  '(nil
           :which-key "test")
    "tt" '(projectile-toggle-between-implementation-and-test
           :which-key "toggle between test and implementation")
    "to" '(projectile-find-implementation-or-test-other-window
           :which-key "find implementation or test other window")
    "tp" '(projectile-test-project
           :which-key "test project"))

  ;; horizontal line
  (global-hl-line-mode t)
  
  ;; prettify symbols
  (global-prettify-symbols-mode 1)
  
  ;; auto-pair parentheses, brackets, quotes
  (electric-pair-mode 1))

;;
;; Languages
;;
(lauremacs/load config lsp)
(lauremacs/load config langs lisps)
(lauremacs/load config langs python)
(lauremacs/load config langs web)
(lauremacs/load config langs elixir)

;;
;; Packages
;;

(config-package flycheck
  :init
  (global-flycheck-mode)
  
  (set-face-attribute 'flycheck-fringe-info nil
                      :background "#3a81c3"
                      :foreground "white")
  
  (set-face-attribute 'flycheck-fringe-error nil
                      :background "#e0211d"
                      :foreground "white")
  
  (set-face-attribute 'flycheck-fringe-warning nil
                      :background "#dc752f"
                      :foreground "white"))

(config-package flyspell
  :custom
  (flyspell-default-dictionary "en_GB")
  :hook '((org-mode . flyspell-mode)
	 (flyspell-mode . #'(lambda ()
               (setq flyspell-mode-map (make-sparse-keymap))))))


(config-package iedit
  :commands iedit-mode
  :bind (:map iedit-mode-keymap))

(config-package yasnippet
  :init
  (yas-global-mode 1)
  :custom
  ;; Use TAB as trigger key and fallback to original behavior
  (yas-trigger-key "TAB")
  (yas-fallback-behavior 'call-other-command)
  :bind (:map yas-minor-mode-map
              ("M-/" . yas-expand))) 
 
(config-package yasnippet-snippets
  :after yasnippet)

(config-package company
  :after yasnippet  
  :bind (:map company-search-map
	            ("C-/" . 'company-complete)
              ("s-." . 'company-select-previous)
              ("s-," . 'company-select-next)
              )
  :custom
  (company-minimum-prefix-length 1)
  (company--idle-delay 0.05)
  (company-selection-wrap-around t)
  (company-dabbrev-downcase nil)
  (company-global-mode t)
  (company-backends '((company-capf :with company-yasnippet) company-yasnippet)
		    ;; Don't auto-select candidates
		    company-frontends '(company-pseudo-tooltip-frontend
					company-echo-metadata-frontend))  
  ;; Show documentation when available
  (company-show-quick-access t)
  :init
  (global-company-mode))

(config-package company-box
  :after company
  :hook (company-mode . company-box-mode))

(config-package company-prescient
  :after company
  :config (company-prescient-mode 1))

(config-package highlight-indentation
  :commands highlight-indentation-mode)

(config-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode))

(config-package paren
  :commands show-paren-mode
  :hook (prog-mode . show-paren-mode)
  :init
  (set-face-attribute 'show-paren-mismatch nil
                      :background "red"
                      :foreground "black"
                      :underline nil)
  (set-face-attribute 'show-paren-match nil
                      :background "#3a81c3"
                      :foreground "white"
                      :underline nil))



(config-package envrc
  :init
  (envrc-global-mode))

;; todo sql
(defvar sql-connection-alist nil)
(defun sqlau--add-to-sql-conection-alist (db-type name host port user password db)
	(add-to-list 'sql-connection-alist
							 (list name
										 (list 'sql-product `(quote ,db-type))
										 (list 'sql-user user)
										 (list 'sql-server host)
										 (list 'sql-port port)
										 (list 'sql-password password)
										 (list 'sql-database db))))

(defun sqlau--format-postgres-sqls (host port user password db)
  (format "host=%s port=%s user=%s password=%s dbname=%s"
          host port user password db))

(defun sqlau--url-formatter (type host port user password db)
	"TYPE should be 'postgres or 'mysql.
If postgres, return url \"postgresql://user:password@host:port/db\".
If mysql, return url \"user:password@tcp(host:port)/db\""
	(let* ((postgress-formatter "postgresql://%s:%s@%s:%s/%s")
				 (mysql-formatter "%s:%s@tcp(%s:%s)/%s")
				 (hexed-passwd (url-hexify-string (format "%s" password)))
				 (formatter (if (eq type 'postgres)
												postgress-formatter
											mysql-formatter)))
		;(throw-unless (contains? '(postgres mysql) type) "TYPE should be 'postgres or 'mysql.")
	  (format formatter user hexed-passwd host port db)))

(cl-defun sqlau-add-postgres-db (name &key (port 5432) user database password host)
	"Adds a postgres database to emacs and lsp
Args: NAME (symbol) to the database and a p-list of parameters
:port, :user, :password, :database, :host
The only optional is :port, its default value is 5432
e.g.:
(sqlau-sql-add-postgres-db 'my-db-name 
     :port 1234
     :user \"username\"
     :host \"my-host\"
     :database \"my-db\"
     :password \"mypassword\")"

 
	(let ((full-uri (sqlau--url-formatter 'postgres host port user password database))
				(data-src-name (sqlau--format-postgres-sqls host port user password database)))
                                        ;	(sqlau--add-to-lsp-sqls-connections "postgresql" data-src-name)
		(sqlau--add-to-sql-conection-alist 'postgres name host port user password full-uri)))

(sqlau-add-postgres-db
   'laura_db
   :user "lauraviglioni"
   :host "localhost"
   :database "laura_db"
   :password ""
   :port 5432)


;; (defgroup lauremacs-posframe nil
;;   "Lauremacs posframe"
;;   :prefix "lauremacs-posframe")

;; (defface lauremacs-posframe-border
;;   '((t (:inherit default :background "gray50")))
;;   "Face used by the ivy-posframe's border."
;;   :group 'lauremacs-posframe)

;; (require 'helm-posframe)

;; (when (posframe-workable-p)
;;   (defvar my-posframe-buffer (find-file "~/.zshrc"))
  
;;   (with-selected-frame
;;       (posframe-show
;;        my-posframe-buffer
;;                                         ;     :string "This is a test"
;;        :position (point)
;;        :poshandler #'posframe-poshandler-frame-center
;;        :respect-header-line t
;;        :border-width 2
;;        :refposhandler helm-posframe-refposhandler
;;        :border-color (face-attribute 'lauremacs-posframe-border :background nil t)
;;        :width (window-width)
;;        :min-height (/ (* 9  (window-width)) 16)
;;        :fringe 10
;;        :x-pixel-offset 20
;;        :y-pixel-offset 20
;;        :accept-focus t
;;        :override-parameters '((cursor-type . box))
;;        )
;;     ))


