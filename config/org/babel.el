;;; fp.el --- functional library for emacs lisp -*- lexical-binding: t; l-syntax: t -*-
;; REMOVE

;;
;; Function composition / piping
;;

(require 'seq)
(require 'cl-lib)

(defun fp/id (arg)
	"Identity function.  Return ARG."
	arg)

(defun fp/partial (fn &rest init-args)
	"Return lambda with FN applied with INIT-ARGS."
	(lambda (&rest args)
		(apply fn (append init-args args))))

(defun fp/pipe (arg &rest fn-list)
  "Pipe ARG into uncurried functions (as FN-LIST)."
  (declare (indent defun))
	(cl-reduce (lambda (args fn) (apply fn (list args)))
						 fn-list
						 :initial-value arg))

(defun fp/compose (&rest fs)
	"Compose a list of functions FS from right to left."
	(cl-reduce (lambda (f g) (lambda (&rest args)
												(funcall f (apply g args))))
						 fs
						 :initial-value (fp/partial 'fp/id)))

;;
;; Sequence functions
;;

(defun fp//map-helper (type fn seq &rest args)
  "Helper function for `fp/map'.
Map FN using ARGS over SEQ and return the seq TYPE."
  (cl-map type
          (apply 'fp/partial (cons fn args))
          seq))

(cl-defmethod fp//map (_fn seq &rest _args)
  "Return error if `fp//map' is not defined for the SEQ type."
  (error (format "Type error: fp/map can't map over %s"
                 (type-of seq))))

(cl-defmethod fp//map (fn (lst list) &rest args)
  "Helper function for `fp/map'.  Apply FN using ARGS over list LST."
  (apply 'fp//map-helper `(list ,fn ,lst ,@args)))

(cl-defmethod fp//map (fn (vec vector) &rest args)
  "Helper function for `fp/map'.  Apply FN using ARGS over VEC vector."
  (apply 'fp//map-helper `(vector ,fn ,vec ,@args)))

(cl-defmethod fp//map (fn (str string) &rest args)
  "Helper function for `fp/map'.  Apply FN using ARGS over STR string."
  (apply 'fp//map-helper `(string ,fn ,str ,@args)))

(defun fp/map (fn &rest args)
	"Map FN using ARGS over an iterable (list, vector o string).
E.g.:
\(funcall \(fp/map \\='1+) \\='(1 2 3)) ;; (2 3 4)
\(funcall \(fp/map \\='* 2) [1 2 3]) ;; [2 4 6]
\(funcall \(fp/map \\='+ 1) \"abc\") ;; \"bcd\"
\(fp/pipe (list \"string\" \"asd\")
  \(fp/map \\='replace-regexp-in-string \"s\" \"S\")) ;; \(\"String\" \"aSd\")"
  (lambda (sequence)
    (apply 'fp//map `(,fn ,sequence ,@args))))


(defun fp/filter (fn &rest args)
  "Return a lambda with filter applied to FN and ARGS.
Work with lists, vectors and strings.
E.g.:
\(funcall \(fp/filter \\='cl-oddp) \\='\(1 2 3)) ;; \(1 3)
\(funcall \(fp/filter \\='cl-oddp) [1 2 3) ;; [1 3]
\(funcall \(fp/filter \\='cl-oddp) \"abc\") ;; \"ac\"
\(fp/pipe \\='\(\"string\" \"asd\")
  \(fp/filter \\='string-match-p \"g\")) ;; \(\"string\")"
  (fp/partial 'cl-remove-if-not (apply 'fp/partial (cons fn args))))

(defun fp/filter-unless (fn &rest args)
  "TODO"
  (fp/partial 'cl-remove-if (apply 'fp/partial (cons fn args))))


(defun fp/member (el)
  "Check if EL is in LST."
  (fp/compose 'bool (fp/partial 'member el)))

;;;###autoload
(defun fp/zip-alist (keys)
  "Zip KEYS and VALS in an alist."
  (fp/partial 'zip-alist keys))

;;
;; String functions
;;

;;;###autoload
(defun fp/split (separator)
  "Split STR using SEPARATOR."
  (lambda (str) (split-string str separator)))

(defun fp/replace (regexp replacement)
  "Replace REGEXP with REPLACEMENT (string) in STR."
  (lambda (str) (replace-regexp-in-string regexp replacement str)))

;; babel config

(require 'ob-elixir)


;; org config

(with-eval-after-load "ob-core"
  ;; (require 'ob-ts)
	(org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (elixir		 . t)
     ;;     (sqlite     . t)
     ;;		 (haskell		 . t)
     ;;  (mermaid    . t)
     ;;  (clojure		 . t)
     ;;   (emacs-lisp . t)
     ;;		 (ts         . t)
     ;;   (python		 . t)
     ;;  (js				 . t)
     ;;   (C					 . t)
     ;;     (latex			 . t)
     ;;     (restclient . t)
     ;;     (sagemath   . t)
     (shell      . t)
     (sql        . t)))
  
  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-default-header-args:sage '((:session . t)
                                             (:results . "output"))))


;;
;; Insert src block
;;

;;;###autoload
(defun lauremacs-org//insert-src-with-post (lang-name &optional post-func)
	"Insert org source block.
LANG-NAME: the language name.
POST-FUNC: reference for a function to run on :post exectution."
  (let ((post (if post-func (concat ":post " post-func "(*this*)") "")))
    (insert (concat "#+begin_src " lang-name " :exports both :results output " post
                    "\n\n"
                    "#+end_src"))))

;;;###autoload
(defun lauremacs-org//insert-sqlite (&optional tables-names)
  (interactive "sInsert tables names: ")
  (let ((formatted-var (fp/pipe tables-names
                         (fp/split " ")
                         (fp/map (lambda (name) (format "%s=%s" name name)))
                         (fp/join " ")
                         ))
        (import-tables (fp/pipe tables-names
                  (fp/split " ")
                  (fp/map (lambda (name) (format ".import $%s %s" name name)))
                  (fp/join "\n"))))
    (insert (format
             "#+begin_src sqlite :exports results :results table :db \":memory:\" :colnames yes :mode csv :header on :var %s \n%s\n\n #+end_src"
             formatted-var
             import-tables))))

;;;###autoload
(defun lauremacs-org//insert-db ()
  (interactive)
  (let* ((connections (mapcar 'car sql-connection-alist))
         (selected (completing-read "Select DB connection: " connections nil t)))
    (insert
     (format
      "#+begin_src sql :exports results :results table :engine postgres :dbconnection %s\n\n#+end_src"
      selected))))


;;;###autoload
(defun lauremacs-org//insert-src-with-session (lang-name &optional session-name)
  "Insert org source block for LANG-NAME with session SESSION-NAME."
  (insert (format
           "#+begin_src %s :exports both :results output :session %s \n\n #+end_src"
           lang-name
           (or session-name "sage"))))



;;;###autoload
(defun lauremacs-org//insert-src (lang-name)
	"Given the LANG-NAME, insert a org code block with proper :post function."
  (message lang-name)
  (cond
   ((string= "postgres-db" lang-name) (lauremacs-org//insert-db))
   ((string= "haskell" lang-name) (lauremacs-org//insert-src-with-post    lang-name "org-babel-haskell-formatter"))
   ((string= "clojure" lang-name) (lauremacs-org//insert-src-with-post    lang-name "org-babel-clojure-formatter"))
   ((string= "sage"    lang-name) (lauremacs-org//insert-src-with-session lang-name))
   ((string= "sqlite"  lang-name) (call-interactively 'lauremacs-org//insert-sqlite))
   (t (lauremacs-org//insert-src-with-post lang-name))))

(defconst lauremacs-org//helm-lang-sources
  (helm-build-sync-source "Language name"
    :candidates '(lambda ()
                   (fp/pipe org-babel-load-languages
                     (fp/map 'car)
                     (fp/partial 'seq-concatenate 'list '(postgres-db))))
    :action 'lauremacs-org//insert-src))

(defconst lauremacs-org//helm-lang-sources-fallback
  (helm-build-dummy-source "Language name"
    :action 'lauremacs-org//insert-src))

;;;###autoload
(defun lauremacs-org/insert-source ()
	"Insert a org code block according to the variable `org-babel-load-languages'."
  (interactive)
  (if (eq 'org-mode major-mode)
      (progn
        (helm
         :history t
         :volatile nil
         :sources '(lauremacs-org//helm-lang-sources lauremacs-org//helm-lang-sources-fallback))
        (forward-line -1)
				(indent-for-tab-command))))
