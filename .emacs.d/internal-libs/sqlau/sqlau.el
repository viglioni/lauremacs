;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

(message "loading sqlau...")

(require 'functional)

;; Variables related to sql configs
(defconst lsp-sqls-connections nil)
(defconst sql-connection-alist nil)

;;;###autoload
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
		(throw-unless (contains? '(postgres mysql) type) "TYPE should be 'postgres or 'mysql.")
	  (format formatter user hexed-passwd host port db)))



;;;###autoload
(defun sqlau--add-to-lsp-sqls-connections (db-type data-src-name)
	(throw-unless (contains? '("postgresql" "mysql") db-type))
	(add-to-list 'lsp-sqls-connections
							 (list (cons 'driver db-type)
										 (cons 'dataSourceName data-src-name))))

;;;###autoload
(defun sqlau--add-to-sql-conection-alist (db-type name host port user password db)
	(add-to-list 'sql-connection-alist
							 (list name
										 (list 'sql-product `(quote ,db-type))
										 (list 'sql-user user)
										 (list 'sql-server host)
										 (list 'sql-port port)
										 (list 'sql-password password)
										 (list 'sql-database db))))


;;;###autoload
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

	(throw-if (any-nil? user database password host) "there are info missing")
	(let ((full-uri (sqlau--url-formatter 'postgres host port user password database))
				(data-src-name (sqlau--format-postgres-sqls host port user password database)))
		(sqlau--add-to-lsp-sqls-connections "postgresql" data-src-name)
		(sqlau--add-to-sql-conection-alist 'postgres name host port user password full-uri)))



;;;###autoload
(cl-defun sqlau-add-mysql-db (name &key (port 3306) user database password host)
	"Adds a mysql database to emacs and lsp.
Args: NAME (symbol) to the database in emacs and a p-list of parameters
:port, :user, :password, :database, :host
The only optional is :port, its default value is 3306
e.g.:
(sql-add-mysql-db 'my-db-name
   :port 1234
   :user \"username\"
   :host \"my-host\"
   :database \"my-db\"
   :password \"mypassword\")"
	(throw-if (any-nil? user database password host) "there are info missing")
	(let ((data-src-name (sqlau--url-formatter 'mysql host port user password database)))
		(sqlau--add-to-lsp-sqls-connections "mysql" data-src-name)
		(sqlau--add-to-sql-conection-alist 'mysql name host port user password database)))


;;;###autoload
(defun sqlau-clean-sql-variables ()
	"Set `lsp-sqls-connections' and `sql-connection-alist' to nil."
	(interactive)
	(setq lsp-sqls-connections nil)
	(setq sql-connection-alist nil))


;;
;; To solve the problem of new lines in SQL interactive buffer
;; code from https://emacs.stackexchange.com/questions/13315/sql-send-paragraph-results-in-mis-aligned-headers/18403#18403
;;

;; Silence compiler warnings
(defvar sql-product)
(defvar sql-prompt-regexp)
(defvar sql-prompt-cont-regexp)

(add-hook 'sql-interactive-mode-hook 'my-sql-interactive-mode-hook)
(defun my-sql-interactive-mode-hook ()
  "Custom interactive SQL mode behaviours. See `sql-interactive-mode-hook'."
  (when (eq sql-product 'postgres)
    ;; Allow symbol chars in database names in prompt.
    ;; Default postgres pattern was: "^\\w*=[#>] " (see `sql-product-alist').
    (setq sql-prompt-regexp "^\\(?:\\sw\\|\\s_\\)*=[#>] ")
    ;; Ditto for continuation prompt: "^\\w*[-(][#>] "
    (setq sql-prompt-cont-regexp "^\\(?:\\sw\\|\\s_\\)*[-(][#>] "))

  ;; Deal with inline prompts in query output.
  ;; Runs after `sql-interactive-remove-continuation-prompt'.
  (add-hook 'comint-preoutput-filter-functions
            'my-sql-comint-preoutput-filter :append :local))

(defun my-sql-comint-preoutput-filter (output)
  "Filter prompts out of SQL query output.

Runs after `sql-interactive-remove-continuation-prompt' in
`comint-preoutput-filter-functions'."
  ;; If the entire output is simply the main prompt, return that.
  ;; (i.e. When simply typing RET at the sqli prompt.)
  (if (string-match (concat "\\`\\(" sql-prompt-regexp "\\)\\'") output)
      output
    ;; Otherwise filter all leading prompts from the output.
    ;; Store the buffer-local prompt patterns before changing buffers.
    (let ((main-prompt sql-prompt-regexp)
          (any-prompt comint-prompt-regexp) ;; see `sql-interactive-mode'
          (prefix-newline nil))
      (with-temp-buffer
        (insert output)
        (goto-char (point-min))
        (when (looking-at main-prompt)
          (setq prefix-newline t))
        (while (looking-at any-prompt)
          (replace-match ""))
        ;; Prepend a newline to the output, if necessary.
        (when prefix-newline
          (goto-char (point-min))
          (unless (looking-at "\n")
            (insert "\n")))
        ;; Return the filtered output.
        (buffer-substring-no-properties (point-min) (point-max))))))

(defadvice sql-send-string (before my-prefix-newline-to-sql-string)
  "Force all `sql-send-*' commands to include an initial newline.

This is a trivial solution to single-line queries tripping up my
custom output filter.  (See `my-sql-comint-preoutput-filter'.)"
  (ad-set-arg 0 (concat "\n" (ad-get-arg 0))))
(ad-activate 'sql-send-string)

;;
;; SQL format
;; https://github.com/syl20bnr/spacemacs/blob/develop/layers/%2Blang/sql/local/sqlfmt/sqlfmt.el
;;


(defcustom sqlfmt-executable
  "sqlfmt"
  "Location of sqlfmt executable."
  :type 'string)

(defcustom sqlfmt-options
  '("--use-spaces")
  "Command line options to pass to sqlfmt."
  :type '(repeat string))

(defcustom sqlfmt-reuse-error-buffer
  t
  "Reuse the same buffer for sqlfmt errors, replacing content on new invocations, or generate new buffers on each invocation"
  :type 'boolean)

(defun sqlfmt-buffer ()
  (interactive)
  (sqlfmt-region (point-min) (point-max)))

(defun sqlfmt-region (start end)
  "Calls sqlfmt on region"
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if (not (and start end))
      (error "No region active, sqlfmt cancelled"))
  (let* ((sqlfmt-buffer-base "*sqlfmt*")
         (inhibit-read-only t)
         (orig-buffer (current-buffer))
         (orig-point (point))
         (tmpbuf (if sqlfmt-reuse-error-buffer
                     (get-buffer-create sqlfmt-buffer-base)
                   (generate-new-buffer sqlfmt-buffer-base)))
         (status-code (progn
                        (with-current-buffer tmpbuf
                          (erase-buffer)
                          (setq buffer-read-only t))
                        (apply #'call-process-region start end
                               sqlfmt-executable nil tmpbuf nil
                               sqlfmt-options))))
    (deactivate-mark)
    (if (eq status-code 0)
        (progn
          (with-current-buffer orig-buffer
            (delete-region start end)
            (insert-buffer tmpbuf)
            (kill-buffer tmpbuf)
            (goto-char orig-point))
          (message "sqlfmt applied"))
      (error "sqlfmt failed, see %s buffer for details." (buffer-name tmpbuf)))))



(provide 'sqlau)


