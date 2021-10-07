;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; general related functions
;;

;;;###autoload
(defmacro throw-if (condition &optional error-description)
  "if condition is true, thrown an error"
  `(if ,condition (error (or ,error-description ""))))

(defmacro throw-unless (condition &optional error-description)
  "if condition is true, thrown an error"
  `(unless ,condition (error (or ,error-description ""))))

;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; misc related functions
;;

;;(require 'functional)

;;;###autoload
(defun create-laurisp-core ()
  (interactive)
  (let* ((filename "laurisp-core.el")
         (files (directory-files "." t "^l-[a-z\\-].*\\.el$"))
         (content (fp/pipe files
                     ((mapcar 'get-string-from-file)
                      (string-join)))))
    (with-temp-buffer
      (insert content)
      (insert "\n\n(provide 'laurisp-core)\n")
      (write-file filename))
    (byte-compile-file filename)))


;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;
;;(require 'functional)
;; (require 'l-string)
;; (require 'l-general)

;;
;; bash related functions
;;

;;;###autoload
(defun gen-uuid-to-clipboard ()
  "generates uuid and copies it to clipboard"
  (interactive)
  (let ((uuid (uuidgen-4)))
    (kill-new uuid)
    (message (format "copied %s to clipboard" uuid))
    uuid))

;;;###autoload
(defun insert-uuid ()
  "inserts random uuid"
  (interactive)
  (insert (uuidgen-4))) 

;;;###autoload
(defun lpwd (&optional dir)
  "returns only the pwd path"
  (expand-file-name (or dir ".")))

;;;###autoload
(defun ls (&optional dir)
  "list files in directory"
  (throw-if (and dir (not (file-directory-p dir))) (concat dir " does not exist!"))
  (directory-files (or dir ".")))

;;;###autoload
(defun touch (filename &optional dir)
  "Creates a empty file if it does not exists, returns the file or nil"
  (throw-if (any-nil? filename) "filename is nil")
  (let* ((path (lpwd dir))
         (file (join-path path filename)))
    (if (file-exists-p file)
        (progn (print "file already exists") nil)
      (progn (write-region "" "" file) file))))

;;;###autoload
(defun echo-into (filename text)
  "echoes text into file"
  (throw-if (any-nil? filename text) "filename or text is nil")
  (throw-if (not (file-exists-p filename)) "filename does not exist!")
  (write-region text "" filename) t)

;;;###autoload
(defun count-non-empty-lines (file)
  (throw-if (any-nil? file) "file is nil")
  (fp/pipe file
     ((get-string-from-file)
      (funcall (lambda (string) (split-string string "\n")))
      (seq-filter (lambda (line) (not (equal 0 (string-match-p "^ *$" line)))))
      (length))))

;;;###autoload 
(defun count-all-laurisp-lines ()
  (interactive)
  (let* ((files-regexp (rx (| (and line-start
                                   (| "l" "test")
                                   (+ (any "-" letter))
                                   ".el"
                                   line-end)
                              (and line-start
                                   (+ (any "-" letter))
                                   ".snippet"
                                   line-end))))
         (files (directory-files-recursively "~/laurisp" files-regexp t))
         (lines (mapcar 'count-non-empty-lines files)))
    (print (apply '+ lines))))




;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; string related functions
;;

;;(require 'functional)
;;(require 'l-general)

;;;###autoload
(defun join-path (path filename)
  "concat path and file. Adds '/' to the end of the path if necessary"
  (throw-if (any-nil? path filename) "path or filename is nil")
  (concat path (if (string-match-p "/$" path) "" "/") filename))

;;;###autoload
(defun file-extension (filename extension)
  "returns filename.extension"
  (throw-if (any-nil? filename extension) "filename or extension is nil")
  (concat filename "." extension))

;;;###autoload
(defun regex-matches (regexp string &optional pos matches)
  "Returns a list of matches"
  (throw-if (any-nil? regexp string) "regexp or string is nil")
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      matches)))


;;;###autoload
(defun get-string-from-file (filepath)
  "Return filepath's file content in a string"
  (throw-if (any-nil? filepath) "filepath is nil")
  (throw-if (not (file-exists-p filepath)) "file does not exists")
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))

;;;###autoload
(defun remove-suffix (file-name)
  "remove suffix of file-name"
  (throw-if (any-nil? file-name) "file-name is nil")
  (replace-regexp-in-string "\\.[a-z]*$" ""  file-name))

;;;###autoload
(defun go-to-fst-empty-line ()
  "search the first empty line in buffer and go to it"
  (beginning-of-buffer) ;; TODO  use ‘(goto-char (point-min))’ instead.
  (search-forward-regexp "^$"))

;;;###autoload
(defun insert-on-fst-empty-line (text current-pos)
  "inserts text on the first empty line of the buffer and
    return the cursor to its position"
  (throw-if (any-nil? text current-pos) "text or current-pos is nil")
  (let* ((empty-line-pos (progn (go-to-fst-empty-line) (point)))
         (is-after? (> current-pos empty-line-pos))
         (return-pos (if is-after?
                         (+ current-pos (length text))
                       current-pos)))
    (insert text)
    (goto-char return-pos)
    t))

(defun fp/insert-on-fst-empty-line (text)
  "inserts text on the first empty line of the buffer and
    return the cursor to its position"
  (throw-unless (bool text) "text is nil")
  (save-excursion
    (go-to-fst-empty-line)
    (insert (concat text "\n"))))


;;;###autoload
(defun fp/split (separator text)
  "(str str) -> [str]
   Split a string using the given separator"
  (split-string text separator))

;;;###autoload
(defun fp/is-empty? (obj)
  "returns if list or string is empty
   (list | str) -> bool"
  (or (equal "" obj) (equal nil obj)))



(provide 'laurisp-core)
