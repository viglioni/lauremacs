;;; web-search.el --- Ts Repl  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Laura Viglioni

;; Author: Laura Viglioni <viglionilaura@gmail.com>
;; Maintainer: Laura Viglioni <viglionilaura@gmail.com>
;; Created: 04 Jul 2021
;; Keywords: keywods
;; URL: https://github.com/Viglioni/lauremacs/tree/main/.emacs/internal-libs/web-search
;; Version:  0.0.1
;; Package-Requires: ((emacs "24.1") (helm "3.6.2"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;;; Code:

(require 'helm)

(defconst web-search-engines-table
	'((google     . "https://www.google.com/search?q=")
		(brave      . "https://search.brave.com/search?q=")
		(duckduckgo . "https://duckduckgo.com/?q=")
		(youtube    . "https://www.youtube.com/results?search_query=")))

(defconst web-search-engines (mapcar 'car web-search-engines-table))

;;;###autoload
(defun web-search--browse (engine query)
	"ENGINE should be one of `web-search-engines'.
QUERY is the string to be searched."
	(let ((url (alist-get engine web-search-engines-table)))
		(browse-url (concat url query))))

;;;###autoload
(defun web-search--create-fn-name (engine)
	"Return a symbol: web-search-ENGINE.
ENGINE should be a symbol or string."
	(let ((engine-name (if (stringp engine) engine (symbol-name engine))))
		(intern (concat "web-search-" engine-name))))

(defun web-search ()
	"Open browser with a query searched in a selected engine.
Engine is one of `web-search-engines-table'."
	(interactive)
	(helm
	 :prompt "Choose an engine:"
	 :sources (helm-build-sync-source "engine candidates"
								:candidates 'web-search-engines
								:action '(lambda (engine) 
													 (call-interactively
														(web-search--create-fn-name engine))))))


;;
;; Create interactive functions called web-search-(name)
;; where name comes from `web-search-engines-table'
;;
(dolist (engine web-search-engines)
	(let ((fn-name (web-search--create-fn-name engine)))
		(defalias fn-name
			(lambda (query)
				"Open browser with the QUERY searched on selected engine."
				(interactive "sInsert the query: ")
				(web-search--browse engine query)))))


(provide 'web-search)
