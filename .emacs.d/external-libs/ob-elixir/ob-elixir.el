;;; ob-elixir.el --- Org Babel functions for Elixir evaluation -*- lexical-binding: t; -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Homepage: https://github.com/lambdart/ob-elixir
;; Keywords: org-mode ob-elixir elixir iex
;; Version: 0.0.3 Alpha
;;
;; This file is NOT part of GNU Emacs.
;;
;;; MIT License
;;
;; Copyright (c) 2020 lambdart
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:
;;
;; Org Babel support for evaluating Elixir source code
;; blocks.
;;
;; To use ob-elixir in an org-babel source block,
;; the Elixir language must be enabled in the
;; custom org-babel-load-languages alist:
;;
;; (add-to-list 'org-babel-tangle-lang-exts '("elixir" . "iex"))
;;
;; Alternatively, running the following snippet during
;; Emacs initialization (or latter):
;;
;; (org-babel-do-load-languages
;;   'org-babel-load-languages
;;   '((emacs-lisp . t)
;;     (elixir . t)))
;;
;;; Code:

(require 'ob)
(require 'ob-comint)

(defcustom org-babel-elixir-program "iex"
  "Name of the program that will execute the Elixir source code block."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-elixir-mode 'elixir-mode
  "Elixir major mode."
  :group 'org-babel
  :type 'symbol)

(defcustom org-babel-elixir-timeout 60
  "Subprocess output timeout in seconds."
  :group 'org-babel
  :type 'integer)

(defcustom org-babel-elixir-table-flag nil
  "Non-nil means reassemble tables in the RESULTS."
  :group 'org-babel
  :type 'boolean)

(defcustom org-babel-elixir-none-var 'hline
  "Replace `Nil' in elixir tables with this before returning."
  :group 'org-babel
  :type 'symbol)

(defconst org-babel-header-args:elixir
  '((cookie . :any)
    (name .  :any)
    (remsh . :any)
    (sname . :any)
    (cwd   . :any)
    (S     . :any))
  "Elixir header arguments.")

(defvar org-babel-elixir-eoe-indicator-function
  (lambda () (format "%s" (random)))
  "End of evaluation indicator function.")

(defvar org-babel-elixir-eoe-indicator "\u2029"
  "End of evaluation indicator.")

(defun org-babel-elixir-insert-eoe-indicator (process)
  "Insert eoe indicator to PROCESS."
  (let ((indicator
         (if (functionp org-babel-elixir-eoe-indicator-function)
             (setq org-babel-elixir-eoe-indicator
                   (funcall org-babel-elixir-eoe-indicator-function))
           org-babel-elixir-eoe-indicator)))
    (setq org-babel-elixir-eoe-indicator
          (concat "\"" indicator "\""))
    (process-send-string
     process
     (format "\"%s\"\n" indicator))))

(defvar org-babel-elixir-filter-regexps
  '("\\(\\(iex\\|[.]+\\)\\(([^@]+@[^)]+)[0-9]+\\|([0-9]+)\\)> \\)"
    "\x1b\[[0-9;]*m"
    "\\`[ \t\n]*"
    "[ \t\n]*\\'"
    "\r"
    "\1^M"
    "^import_file([^)]+)\n"
    org-babel-elixir-eoe-indicator)
  "List of filter regex expressions.")

(defun org-babel-elixir-replace-filter (result regexp-or-symbol)
  "Replace RESULT with REGEXP-OR-SYMBOL."
  (let ((regexp (if (symbolp regexp-or-symbol)
                    (symbol-value regexp-or-symbol)
                  regexp-or-symbol)))
    (replace-regexp-in-string regexp "" result)))

(defvar org-babel-elixir-raw-output ""
  "Auxiliary variable to hold process output.")

(defvar org-babel-elixir-hline "nil"
  "Our hline string value.")

(defmacro org-babel-elixir--message (fmt &rest args)
  "Display a internal message at the bottom of the screen.
See `message' for more information about FMT and ARGS arguments."
  `(message (concat "[ob-elixir]: ",fmt) ,@args))

(defun org-babel-elixir-process-output ()
  "Return process output."
  (reverse org-babel-elixir-raw-output))

(defvar org-babel-elixir-process-ends nil)

(defun org-babel-elixir-process-wait ()
  "Wait process ends indicator."
  (while (eq org-babel-elixir-process-ends nil)
    (sleep-for 00.1)))

(defun org-babel-elixir-send-string (process string)
  "Send STRING to Elixir PROCESS."
  (let ((string (format "%s\n" string)))
    ;; clean process raw output
    (setq org-babel-elixir-raw-output '()
          org-babel-elixir-process-ends nil)
    ;; send string to process
    (process-send-string process string)
    ;; accept process output (default timeout 1 minute)
    (accept-process-output process org-babel-elixir-timeout nil t)
    ;; send end indicator
    (org-babel-elixir-insert-eoe-indicator process)
    ;; wait for the process
    (org-babel-elixir-process-wait)
    ;; return process raw output
    (org-babel-elixir-process-output)))

(defun org-babel-elixir-insert-string (process string)
  "Insert the STRING in the PROCESS buffer."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((moving (= (point) (process-mark process))))
        (save-excursion
          ;; insert the text, advancing the process marker.
          (goto-char (process-mark process))
          (insert string)
          (set-marker (process-mark process) (point)))
        (if moving (goto-char (process-mark process)))))))

(defun org-babel-elixir-process-filter (process output)
  "Filter PROCESS OUTPUT."
  ;; debug message
  (unless (process-live-p process)
    (progn
      (setq org-babel-elixir-process-ends t)
      (org-babel-elixir--message "process die")))
  ;; insertion filter (test)
  (org-babel-elixir-insert-string process output)
  ;; save raw process output
  (push output org-babel-elixir-raw-output)
  ;; update process indicator
  (if (string-match-p org-babel-elixir-eoe-indicator output)
    (setq org-babel-elixir-process-ends t)))

(defun org-babel-elixir-start-process (buffer-name params)
  "Parse the process PARAMS, start the process using its BUFFER-NAME."
  (let* ((buffer (get-buffer-create buffer-name))
         (program-args nil)
         (process nil))
    ;; with current buffer
    (with-current-buffer buffer
      ;; make a local environment variables for subprocesses list
      (make-local-variable 'process-environment)
      ;; set process environments list
      (setq process-environment (cons "TERM=vt100" process-environment)))
    ;; set program args
    (setq program-args (apply 'append
                              (org-babel-elixir-parse-process-params params)))
    ;; start the process
    (apply 'start-process buffer-name buffer org-babel-elixir-program program-args)
    ;; update process auxiliary variable
    (setq process (get-buffer-process buffer-name))
    ;; if process was properly created
    (and process
         ;; set process filter
         (set-process-filter process 'org-babel-elixir-process-filter)
         ;; send setup input (disable colors)
         (org-babel-elixir-send-string process
                                       "IEx.configure(colors: [enabled: false])"))
    ;; return process
    process))

(defun org-babel-elixir-parse-process-params (params)
  "Parse the process PARAMS."
  (let ((params-alist '((:sname "--sname")
                        (:name "--name")
                        (:cookie "--cookie")
                        (:remsh "--remsh")
                        (:S "-S")))
        ;; auxiliary variables
        (key nil)
        (option nil))
    ;; return process arguments list
    (delq nil
          (mapcar (lambda (param)
                    ;; update key and option
                    (setq key (car param))
                    (setq option (cdr param))
                    ;; get command line options
                    (when (assoc key params)
                      `(,(car option) ,(assoc-default key params))))
                  params-alist))))

(defun org-babel-elixir--trim-string (results)
  "Remove white spaces from RESULTS."
  (let ((regexps '("[ \t\n]*\\'" "\\`[ \t\n]*"))
        (string results))
    (dolist (regexp regexps)
      (setq string (replace-regexp-in-string regexp "" string)))
    string))

(defun org-babel-elixir-table-or-string (results)
  "Convert the RESULTS to a table or return the string."
  (let ((results (org-babel-script-escape
                  (org-babel-elixir--trim-string results))))
    (if (listp results)
        (mapcar (lambda (var)
                  (if (eq var 'nil)
                      org-babel-elixir-none-var
                    var))
                results)
      results)))

(defun org-babel-elixir-insert-results (results)
  "Insert the code block evaluation RESULTS."
  (org-babel-elixir-table-or-string results))

(defun org-babel-elixir-initiate-session (&optional session params)
  "Initialize a session named SESSION according to PARAMS."
  (unless (string= session "none")
    (let* ((session (or session org-babel-elixir-program))
           (buffer (format "*%s*" session)))
      ;; return session comint buffer was already created
      (if (org-babel-comint-buffer-livep buffer)
          session
        ;; verify if elixir REPL program exists
        (when (executable-find org-babel-elixir-program)
          ;; set comint buffer
          (setq buffer (apply 'make-comint
                              session
                              org-babel-elixir-program
                              nil
                              (and params (org-babel-elixir-parse-process-params params))))
          ;; error if (comint) buffer wasn't created
          (unless buffer
            (error "[ob-elixir]: Error, wasn't possible to create the process"))
          ;; otherwise return session
          session)))))

(defun org-babel-elixir-evaluate-external-process (body params)
  ;; &optional result-type result-params column-names-p row-names-p)
  "Start external process using its PARAMS and evaluate BODY."
  (let* ((name "*elixir-session-none*")
         ;; get default process
         (process (get-buffer-process name)))
    ;; restart iex process of necessary
    (unless (process-live-p process)
      ;; set process variable
      (setq process (org-babel-elixir-start-process name params)))
    ;; send string
    (let ((output (org-babel-elixir-send-string process body)))
      ;; if output size its not greater then 0, return nil,
      ;; otherwise the output
      (and (> (length output) 0) output))))

(defun org-babel-elixir-evaluate-session (session body)
  "Evaluate BODY in SESSION, i.e, the comint buffer."
  (butlast
   (nthcdr 3
           (org-babel-comint-with-output ((format "*%s*" session) "nil")
             (mapc (lambda (line)
                     (insert line "\n")
                     (comint-send-input))
                   `("IEx.configure(colors: [enabled: false])"
                     ,body
                     "IEx.configure(colors: [enabled: true])"))))
   4))

(defun org-babel-elixir-evaluate (session body &optional params)
  "Evaluate BODY elixir code in SESSION."
  (if session
      ;; use the comint buffer
      (org-babel-elixir-evaluate-session session body)
    ;; no session use external process
    (org-babel-elixir-evaluate-external-process body params)))

(defun org-babel-prep-session:elixir (session params)
  "Prepare SESSION according to the header arguments in PARAMS.
VARS contains resolved variable references."
  (let* ((session (org-babel-elixir-initiate-session session))
         (var-lines
          (org-babel-variable-assignments:elixir params)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (end-of-line 1) (insert var) (comint-send-input)
              (org-babel-comint-wait-for-output session))
            var-lines))
    session))

(defun org-babel-load-session:elixir (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:elixir session params)))
      (with-current-buffer buffer
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert (org-babel-chomp body)))
      buffer)))

(defun org-babel-elixir-var-to-elixir (var)
  "Convert an elisp value to an Elixir variable.
Convert an elisp value, VAR, into a string of Elixir source code
specifying a variable of the same value."
  (cond
   ((listp var)
    (concat "[" (mapconcat #'org-babel-elixir-var-to-elixir var ", ") "]"))
   ((vectorp var)
    (concat "{" (mapconcat #'org-babel-elixir-var-to-elixir var ", ") "}"))
   (t
    (if (equal var 'hline)
        org-babel-elixir-hline
      (format
       (if (and (stringp var) (string-match "[\r\n]" var)) "\"\"%S\"\"" "%S")
       (if (stringp var) (substring-no-properties var) var))))))

(defun org-babel-variable-assignments:elixir (params)
  "Return a list of Elixir statements assigning the block's variables.
The variables are defined in PARAMS."
  (mapcar
   (lambda (pair)
     (format "%s = %s\n"
             (car pair)
             (org-babel-elixir-var-to-elixir (cdr pair))))
   (org-babel--get-vars params)))

(defun org-babel-expand-body:elixir (body params)
  "Assign variables defined in PARAMS (if any), and save BODY in a temporary file."
  ;; variable assignments
  (let* ((vars (org-babel-variable-assignments:elixir params))
         (temp-file (org-babel-temp-file "elixir-"))
         (full-body (concat (mapconcat (lambda (var)
                                         var)
                                       vars "")
                            (org-babel-chomp body))))
    ;; insert into temporary file
    (with-temp-file temp-file
      (insert full-body))
    ;; return the full body
    (format "import_file(\"%s\")" temp-file)))

;;;###autoload
(defun org-babel-execute:elixir (body params)
  "Parse PARAMS and Evaluate BODY (elixir source code block).
This function is called by `org-babel-execute-src-block'."
  (let* ((params (org-babel-process-params params))
         (session (org-babel-elixir-initiate-session
                   (cdr (assoc :session params)) params))
         (result (mapconcat
                  'identity
                  (org-babel-elixir-evaluate
                   session
                   (org-babel-expand-body:elixir body params)
                   params)
                  ""))
         (result (seq-reduce
                  #'org-babel-elixir-replace-filter
                  org-babel-elixir-filter-regexps
                  result)))
    (org-babel-elixir-insert-results result)))

;; add elixir to org-babel language extensions
(add-to-list 'org-babel-tangle-lang-exts '("elixir" . "iex"))

(provide 'ob-elixir)

;;; ob-elixir.el ends here
