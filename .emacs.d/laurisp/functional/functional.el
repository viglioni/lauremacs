;;; functional.el --- Common functional programming functions -*- lexical-binding: t -*-

;; Author: Laura Viglioni <viglionilaura@gmail.com>
;; URL: https://github.com/Viglioni/laurisp
;; Keywords: functional programming
;; Version: 0.0.2
;; Package-Requires: ((emacs "25.1") (seq "2.21"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;;; Commentary:
;; A functional lib for emacs including compose, pipe, curry and more.

;;; Code:

(message "loading functional...")

;;
;; functional related functions
;;

(require 'seq)
(require 'cl) ;; change to cl-lib after removing `compose'.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; V2 (wip and not tested yet) ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; all functions V2 are meant to be used inside pipe, so all of them receives at most one argument.

;;;###autoload
(defun fp/any? (lst)
  "Return t if at least one element in LST is truthy:
[a] → Boolean."
	(bool (seq-reduce (lambda (acc val) (or acc val))
										lst nil)))

(defun fp/odd? (n)
	"Return if N is odd."
	(= (% n 2) 1))

(defun fp/even? (n)
	"Return if N is even."
	(= (% n 2) 0))

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
	(seq-reduce (lambda (args fn) (apply fn (list args)))
							fn-list
							arg))

(defun fp/compose (&rest fs)
	"Compose a list of functions FS from right to left."
	(seq-reduce (lambda (f g) (lambda (&rest args)
												 (funcall f (apply g args))))
							fs
							(fp/partial 'fp/id)))

(defun fp/map (fn &rest args)
	"Return a lambda with map applied to FN and ARGS.
E.g.:
\(funcall \(fp/map '1+) '\(1 2 3)) ;; '\(2 3 4)
\(funcall \(fp/map '* 2) '\(1 2 3)) ;; '\(2 4 6)
\(fp/pipe '\(\"string\" \"asd\")
  \(fp/map 'replace-regexp-in-string \"s\" \"S\")) ;; '\(\"String\" \"aSd\")"
	(fp/partial 'seq-map (apply 'fp/partial (cons fn args))))

(defun fp/filter (fn &rest args)
	"Return a lambda with filter applied to FN and ARGS.
E.g.:
\(funcall \(fp/filter 'fp/odd?) '\(1 2 3)) ;; '\(2 3 4)
\(fp/pipe '\(\"string\" \"asd\")
  \(fp/filter 'string-match-p \"g\")) ;; '\(\"string\")"
	(fp/partial 'seq-filter (apply 'fp/partial (cons fn args))))

(defun fp/reduce (initial-val fn)
	"Return a lambda with reduce applied with FN and INITIAL-VAL."
	(lambda (lst)
		(seq-reduce fn lst initial-val)))

(defun fp/const-fn (fn &rest args)
  "Return an interactive function that call FN applying ARGS."
  (lambda ()
    (interactive)
    (apply fn args)))


(defun fp/join (separator)
  "Join a list of strings using SEPARATOR.
\(funcall (fp/join \"-\") (list \"some\" \"string\"))
=> \"some-string\"."
  (fp/partial 's-join separator))


;;
;; V1
;;



(defmacro fp/curry-deprecated-expr (expr)
  "Curry an expression:
(fp/curry-deprecated-expr '(+ 1 2 3)) -> (fp/curry-deprecated + 1 2 3)."
  `(eval (seq-concatenate 'list '(fp/curry-deprecated) ,expr)))

(defmacro compose (&rest fn-list)
  "DEPRECATED. See `fp/compose'.

Compose functions (and curries them) from right to left.
   ((y -> z) ... (m -> o) ((a ... n) -> m) ) -> ((a ... n)->z)
   e.g.:
   (compose (+ 1) (* 2)) -> (lambda (arg1 ... argN) (+ 1 (* 2 arg1 ... argN)))"
  `(let ((curried-fn (quote ,(seq-map (lambda (fn) (fp/curry-deprecated-expr fn)) fn-list))))
     (reduce
      (lambda (f g)
        (lexical-let ((f f) (g g))
          (lambda (&rest args) (funcall f (apply g args)))))
      curried-fn
      :initial-value (fp/curry-deprecated identity))))

;;;###autoload
(defmacro fp/convert-to-symbol (anything)
	"Convert ANYTHING to symbol, if it is already a symbol, do nothing."
	`(if (ignore-errors (symbolp ,anything))
			,anything
		(quote ,anything)))


;;;###autoload
(defmacro fp/curry-deprecated (fn &rest initial-args)
  "DEPRECATED. See `fp/partial'.
Return the curried function:
(fp/curry-deprecated '+ 1 2 3) -> (lambda (argN ... argM) (+ 1 2 3 argN ... argM))"
  `(lambda (&rest args)
     (apply (fp/convert-to-symbol ,fn)
						(seq-concatenate 'list (list ,@initial-args) args))))

;;;###autoload
(defmacro fp/pipe-deprecated (arg fn-list)
  "DEPRECATED. See `fp/pipe'.

Pipe an argument into composed functions from left to right.
   a -> ((a -> b) (b -> c) ... (n -> m)) -> m
   e.g.:
   (fp/pipe-deprecated  5 ((+ 1) (* 2))) -> 12"
  (declare (indent defun))
  `(funcall (compose ,@(reverse  fn-list)) ,arg))

;;;###autoload
;; (defmacro fp/for-each (list fn &rest fn-args)
;; 	"Loop throught LIST applying FN with FN-ARGS and then return LIST.
;; E.g.: \(fp/for-each '(1 2) `add-to-list' 'mylist)."
;; 	(declare (indent defun))
;; 	`(progn (dolist (el ,list)
;; 						(funcall (fp/curry-deprecated ,fn ,@fn-args) el))
;; 					,list))

;;;###autoload
(defmacro fp/const-fn-interactive (fn &rest args)
  "Return an interactive lambda function that execute FN with given ARGS.
e.g.
\(fp/const-fn-interactive 'concat \"several \" \"string \" \"args\")"
  `(lambda () (interactive) (apply ,fn (quote ,args))))

;;;###autoload
(defun fp/const (result)
  "Return a function that will always return RESULT."
  (lambda (&rest _args) result))

(defun fp/range (to &optional from step)
  "Return a list with a range of numbers [FROM=0, TO[ using STEP=1."
  (let ((start (or from 0))
        (nstep (or step 1))
        (final (- to 1)))
    (number-sequence start final nstep)))



;;;###autoload
(defun fp/repeat (value number-of-times)
  "Return a list with VALUE repeated NUMBER-OF-TIMES."
  (fp/pipe (number-sequence 0 (- number-of-times 1))
    (fp/map (fp/const value))))


;;;###autoload
(defmacro compose-and-call (fn-list &rest args)
  "Since compose returns a function, this helper receives a list of
   functions and args and apply them to composed funcs
   (a ... n) -> ((y -> z) ... (m -> o) ((a ... n) -> m) ) -> z
    e.g.:
    (compose-and-call ((+ 1) (* 2)) 2 3) -> 13"
  `(funcall (compose ,@fn-list) ,@args))

(defun identity (arg)
  "DEPRECATED. See `fp/id'.
Identity function.
   a -> a"
  arg)
 (lambda (args) )
;;
;; Logic 
;;

;;;###autoload
(defun bool (x)
  "returns t if element is truthy, nil if its falsey
   a -> bool"
  (not (not x)))

;;;###autoload
(defun n-and (&rest args)
  "Not and.
   * -> bool"
  (not (all args)))

;;;###autoload
(defun n-or (&rest args)
  "Not or.
   * -> bool"
  (not (any args)))

;;
;; List 
;;

;;;###autoload
(defun all (lst)
  "Returns t if all elements in list are truthy
   [a] → Boolean"
  (bool (seq-reduce
         (lambda (acc val) (and acc val))
         lst t)))


;TODO: REMOVE THIS FUNC
;;;###autoload
(defun contains? (list element)
  "Returns t/nil if element is in list
   ([a] a) -> bool"
  (bool (member element list)))

;;;###autoload
(defun fp/contains? (list element)
  "Returns t/nil if element is in list
   ([a] a) -> bool"
  (bool (member element list)))


;;;###autoload
(defun head (list)
  "Returns the first element of a list
   [a] -> a | nil"
  (car list))

																				;TODO: implement last, init

;;;###autoload
(defun not-contains? (list element)
  "Returns t/nil if element is not in list
   ([a] a) -> bool"
  (if list (not (contains? list element))))

;;;###autoload
(defun tail (list)
  "Returns the list but its first element
   [a] -> [a] | nil"
  (cdr list))

;;;###autoload
(defun unzip (zipped-list)
  "unzip n lists
   [[a]] -> [[a]]
   e.g.:
   (unzip '((1 2 ... n) (1 2 ... n))) ->
   '((1 1 ... 1) (2 2 .... 2) ... (n n ... n)"
  (if (and zipped-list (all zipped-list))
      (let ((heads (mapcar* 'head zipped-list))
            (tails (mapcar* 'tail zipped-list)))
        (append (list heads) (unzip tails)))))

;;;###autoload
(defun zip (&rest lists)
  "zips n lists
   [[a]] -> [[a]]
   e.g.:
   (zip '(1 1) '(2 2) '(3 3)) -> '((1 2 3) (1 2 3))"
  (apply (fp/curry-deprecated  mapcar* 'list) lists))

;;
;; Number
;;

;;;###autoload
(defun inc (n)
  "Returns the increment of n
   Number -> Number"
	(+ 1 n))

;;
;; Type
;;

;;;###autoload
(defun all-nil? (&rest args)
  "return if all args are nil
   (* ... *) -> boolean
   e.g (all-nil? nil nil) -> t"
  (apply 'n-or args))

(defun any-nil? (&rest args)
  "return if any args are nil
   (* ... *) -> boolean
   e.g (any-nil? nil t nil) -> t"
  (apply 'n-and args))


;;
;; alist
;;
																				;TODO: test it
;;;###autoload
(defun fp/alist-sort-by-car (alist)
  (sort alist (lambda (a b) (string< (car a) (car b)))))

;;;###autoload
(defun alist-sort-by-cdr-ci (alist)
  "sort alist by cdr. case insensitive"
  (sort alist (lambda (a b) (string< (downcase (cdr a)) (downcase (cdr b))))))

(provide 'functional)

;;; functional.el ends here
