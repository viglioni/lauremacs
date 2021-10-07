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




