;;; fp.el --- functional library for emacs lisp -*- lexical-binding: t -*-
;; 
;; Filename: fp.el
;; Description: functional library for emacs lisp
;; Author: Laura Viglioni
;; Maintainer: Laura Viglioni
;; Created: Sat Mar 11 16:25:42 2023 (-0300)
;; Version: 1
;; Package-Requires: ()
;; URL: http://github.com/viglioni/lauremacs
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;;
;; All the functions in this library are intended to be used inside
;; pipes/compose etc
;; i.e. they all receive one parameter except for functions that
;; work with several parameters e.g
;; (funcall (fp/concat "a" "b") "c" "d") => "abcd"
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

(require 'seq)
(require 'cl-lib)

;;; Code:


;;
;; Function composition / piping
;;


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

(provide 'fp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fp.el ends here
