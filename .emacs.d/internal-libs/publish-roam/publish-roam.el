;;; publish-roam.el ---
;; 
;; Filename: publish-roam.el
;; Description:
;; Author: Laura Viglioni
;; Maintainer:
;; Created: Fri Jul 19 14:44:31 2024 (-0300)
;; Version: 
;; Package-Requires: ()
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:


(require 'ox)

 
;;;;;;;;;;;;;;;;;;;
;; Datastructure ;;
;;;;;;;;;;;;;;;;;;;

(cl-defstruct roam-info
  "Datastructure used to publish org roam data."
  title file-path id timestamp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Converting ORG into HTML and publishing ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun publish-roam--query (tag-name)
  "Define the org roam query filtering files by TAG-NAME."
  `(org-roam-db-query
    [:select [nodes:title nodes:file nodes:id]
             :from nodes 
             :inner :join tags :as t1 :on (= nodes:id t1:node_id)
             :inner :join tags :as t2 :on (= nodes:id t2:node_id)
             :where (and (= t1:tag ,tag-name) (= t2:tag "publish"))]))

(defun publish-roam--file-timestamp (file-name)
  "Get timestamp from FILE-NAME."
 (replace-regexp-in-string "[^0-9]" "" file-name))

(defun publish-roam--convert-to-roam-info (query-result)
  "Convert org roam QUERY-RESULT into `roam-info'."
  (mapcar
   (lambda (row) (make-roam-info
             :title (car row)
             :file-path (cadr row)
             :id (caddr row)
             :timestamp (publish-roam--file-timestamp (cadr row))))
   query-result))

(defun publish-roam--sort-info (info-list)
  "Sort INFO-LIST by title."
  (sort
   info-list
   (lambda (info1 info2) (string< (roam-info-title info1) (roam-info-title info2)))))

(defun publish-roam--files-by-tag (tag-name)
  "Get all roam files which have the tag TAG-NAME.
Return a list of `roam-info'."
  (publish-roam--sort-info
   (publish-roam--convert-to-roam-info
    (eval (publish-roam--query tag-name)))))

(defun publish-roam--clean (publish-dir)
  "Clear all html from PUBLISH-DIR."
  (shell-command-to-string
   (format "rm %s/*.html" publish-dir)))

(defun publish-roam--fst-empty-line ()
  "Go to first empty line."
  (search-forward "\n\n"))

(defun publish-roam--read-file-as-str (filepath)
  "Read content from FILEPATH as string."
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))

(defun publish-roam--insert-css ()
  "Insert CSS header."
  (insert "#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"static/styles.css\" />\n\n"))

(defun publish-roam--insert-head (publish-dir)
  "Insert head from PUBLISH-DIR/static/head.html."
  (let* ((file (format "%s/static/head.html" publish-dir))
         (raw-content (publish-roam--read-file-as-str file))
         (content (replace-regexp-in-string "\n" "" raw-content)))
    (insert (format "#+HTML_HEAD: %s" content))))

(defun publish-roam--export-file-name (info publish-dir)
  "Return the HTML filepath from INFO and PUBLISH-DIR."
  (format "%s/%s.html" publish-dir (roam-info-timestamp info)))


(defun publish-roam--single-file (info publish-dir &optional not-evalp)
  "Convert single file to html in PUBLISH-DIR.
INFO is a `roam-info' struct.
If NOT-EVALP is non nil, it will not eval babel code in the exported file."
  (let ((org-export-with-section-numbers nil)
        (org-export-use-babel (not not-evalp)))
	  (with-temp-buffer
		  (insert-file-contents (roam-info-file-path info))
      (publish-roam--fst-empty-line)
		  (publish-roam--insert-css)
		  (publish-roam--insert-head publish-dir)
		  (org-export-to-file 'html (publish-roam--export-file-name info publish-dir)))))

(defun publish-roam--publish-all (info-list publish-dir &optional not-evalp)
  "Publish all roam files from INFO-LIST in PUBLISH-DIR.
If NOT-EVALP is non-nil, it won't execute any babel code."
  (mapcar
   (lambda (row) (publish-roam--single-file row publish-dir not-evalp))
   info-list))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creating index.html ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun publish-roam--schema-card (info)
  "HTML card schema from INFO."
  (format "<a href=\"%s.html\" class=\"card\">%s</a>"
          (roam-info-timestamp info)
          (roam-info-title info)))

(defun publish-roam--template-exist? (publish-dir)
  "Check if index template exists in PUBLISH-DIR/static."
  (file-exists-p (format "%s/static/index.template.html" publish-dir)))

(defun publish-roam--schema-index (publish-dir info-list)
  "Index page with links from INFO-LIST.  PUBLISH-DIR is the publish directory."
  (format
   (publish-roam--read-file-as-str (format "%s/static/index.template.html" publish-dir))
   (string-join (mapcar 'publish-roam--schema-card info-list) " ")))

(defun publish-roam--index-html (info-list publish-dir)
  "Create a index.html with HEADER-TITLE from INFO-LIST in PUBLISH-DIR."
  (if (publish-roam--template-exist? publish-dir)
      (write-region
       (publish-roam--schema-index publish-dir info-list)
       nil
       (format "%s/index.html" publish-dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fix roam links in HTML files ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun publish-roam--fix-links (info-list publish-dir)
  "Fix links in HTML files inside PUBLISH-DIR using INFO-LIST."
  (let ((files (directory-files publish-dir t "\\.html$")))
    (cl-loop
     for file in files do
     (cl-loop
      for info in info-list do
      (shell-command-to-string
       (format "gsed -i 's/id:%s/%s.html/g' %s"
               (roam-info-id info)
               (roam-info-timestamp info)
               file))))))

;;;;;;;;;;;;;;;;
;; Public API ;;
;;;;;;;;;;;;;;;;

(defun publish-roam-by-tag (tag publish-dir &optional not-evalp)
  "Publish all org-roam files with tag TAG and tag \"publish\".
The files will be converted to html inside PUBLISH-DIR.
An index page will be generated with links to each page.
If NOT-EVALP is non-nil, it won't execute any babel code."
  (let ((info-list (publish-roam--files-by-tag tag)))
    (publish-roam--clean publish-dir)
    (publish-roam--publish-all info-list publish-dir not-evalp)
    (publish-roam--index-html info-list publish-dir)
    (publish-roam--fix-links info-list publish-dir)))


(provide 'publish-roam)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; publish-roam.el ends here
