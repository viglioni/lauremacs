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
             :from tags
             :inner :join nodes :on (= nodes:id tags:node_id)
             :where (= tag ,tag-name)]))

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

(defun publish-roam--files-by-tag (tag-name)
  "Get all roam files which have the tag TAG-NAME.
Return a list of `roam-info'."
  (publish-roam--convert-to-roam-info
   (eval (publish-roam--query tag-name))))

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
      "pandoc -f org -t html5 --css=static/styles.css --standalone %s -o %s/%s.html"
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
    <title>HTML 5 Boilerplate</title>
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
   (string-join (mapcar 'publish-roam--schema-card info-list) " ")))

(defun publish-roam--index-html (info-list publish-dir header-title)
  "Create a index.html with HEADER-TITLE from INFO-LIST in PUBLISH-DIR."
  (write-region
   (publish-roam--schema-index header-title info-list)
   nil
   (format "%s/index.html" publish-dir)))

;;;;;;;;;;;;;;;;
;; Public API ;;
;;;;;;;;;;;;;;;;

(defun publish-roam-by-tag (tag publish-dir header-title)
  "Convert all org-roam files with TAG to html inside PUBLISH-DIR.
An index page will be generated with links to each page using also HEADER-TITLE."
  (let ((info-list (publish-roam--files-by-tag tag)))
    (publish-roam--clean publish-dir)
    (publish-roam--publish-all info-list publish-dir)
    (publish-roam--index-html info-list publish-dir header-title)
    (print (publish-roam--schema-index header-title info-list))))

