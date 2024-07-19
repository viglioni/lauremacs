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

(defun publish-roam--single-file (info publish-dir)
  "Convert single file to html in PUBLISH-DIR.
INFO is a `roam-info' struct."
  (let ((file-path (roam-info-file-path info)))
    (shell-command-to-string
     (format
      "pandoc -f org -t html5 --resource-path %s -H static/head.html --css=static/styles.css --standalone %s -o %s/%s.html"
      publish-dir
      file-path
      publish-dir
      (roam-info-timestamp info)))))

(defun publish-roam--publish-all (info-list publish-dir)
  "Publish all roam files from INFO-LIST in PUBLISH-DIR."
  (mapcar
   (lambda (row) (publish-roam--single-file row publish-dir))
   info-list))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creating index.html ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun publish-roam--schema-card (info)
  "html card schema from INFO."
  (format "<a href=\"%s.html\" class=\"card\">%s</a>"
          (roam-info-timestamp info)
          (roam-info-title info)))

(defun publish-roam--schema-index (title info-list)
  "Index page with links from INFO-LIST.  TITLE will be shown in header."
  (format
   "
<!doctype html>
<html lang=\"en\">
  <head>
    <meta charset=\"UTF-8\" />
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\" />
    <meta http-equiv=\"X-UA-Compatible\" content=\"ie=edge\" />
    <title>%s</title>
    <link rel=\"stylesheet\" href=\"static/styles.css\" />
    <link rel=\"preconnect\" href=\"https://fonts.googleapis.com\" />
    <link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin />
    <link
      href=\"https://fonts.googleapis.com/css2?family=Arsenal+SC:ital,wght@0,400;0,700;1,400;1,700&display=swap\"
      rel=\"stylesheet\"
    />
  </head>

  <body>
    <script src=\"static/index.js\"></script>
    <header>
      <h1>%s</h1>
    </header>
    <div class=\"card-wrapper\">
      %s
    </div>
  </body>
</html>
"
   title
   title
   (string-join (mapcar 'publish-roam--schema-card info-list) " ")))

(defun publish-roam--index-html (info-list publish-dir header-title)
  "Create a index.html with HEADER-TITLE from INFO-LIST in PUBLISH-DIR."
  (write-region
   (publish-roam--schema-index header-title info-list)
   nil
   (format "%s/index.html" publish-dir)))

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

(defun publish-roam-by-tag (tag publish-dir header-title)
  "Publish all org-roam files with tag TAG and tag \"publish\".
The files will be converted to html inside PUBLISH-DIR.
An index page will be generated with links to each page using also HEADER-TITLE."
  (let ((info-list (publish-roam--files-by-tag tag)))
    (publish-roam--clean publish-dir)
    (publish-roam--publish-all info-list publish-dir)
    (publish-roam--index-html info-list publish-dir header-title)
    (publish-roam--fix-links info-list publish-dir)))


(provide 'publish-roam)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; publish-roam.el ends here
