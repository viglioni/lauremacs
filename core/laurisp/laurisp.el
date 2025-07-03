;;; -*- lexical-binding: t; -*-
;;; laurisp.el --- Modern functional programming utilities for Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laura Viglioni

;; Author: Laura Viglioni
;; Version: 0.1.0
;; Package-Requires: ((emacs "29"))
;; Keywords: lisp, functional, programming, utilities
;; URL: https://github.com/lauravglioni/laurisp

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Laurisp provides a modern functional programming approach to writing
;; Emacs Lisp, drawing inspiration from Haskell and Elixir.
;;
;; This library introduces currying, partial application, and functional
;; composition utilities that make Emacs Lisp more expressive and closer
;; to modern functional programming paradigms.
;;
;; Key features:
;; - Automatic currying with `ldef'
;; - Partial application with `l-partial'
;; - Functional composition utilities
;; - Haskell-inspired syntax through `with-laurisp'
;;
;; Example usage:
;;
;;   (ldef add3 (x y z) (+ x y z))
;;   (funcall (add3 1 2) 3) ; => 6
;;
;;   (with-laurisp
;;     ((add3 1) 2 3)) ; => 6

;;; Code:


;;;;;;;;;
;; API ;;
;;;;;;;;;

(defun l-partial (fn &rest init-args)
  "Return a partially applied function with FN and INIT-ARGS.

Creates a new function that, when called, applies FN to the
combination of INIT-ARGS (provided now) and any additional
arguments (provided later).

Examples:
  (funcall (l-partial \\='+ 5) 3)     ;; => 8
  (funcall (l-partial \\='* 2 3) 4)   ;; => 24
  (funcall (l-partial \\='concat \"Hello, \") \"World!\")
  ;; => \"Hello, World!\"

FN can be a function symbol, lambda expression, or any callable.
INIT-ARGS are the initial arguments to partially apply to FN."
  (lambda (&rest args)
    (apply fn (append init-args args))))


 (defmacro ldef (name args &rest body)
  "Define a function that automatically curries when called with fewer arguments.

Creates a function NAME that can be called with any number of arguments up to
the full arity defined by ARGS. When called with fewer arguments than required,
it returns a partially applied function. When called with the full number of
arguments, it executes the function body.

IMPORTANT: Variadic arguments with &rest are NOT supported. ARGS must be a
simple list of parameter names without &rest, &optional, or other lambda list
keywords.

Examples:
  (ldef add3 (x y z) (+ x y z))
  (add3 1 2 3)        ;; => 6 (full application)
  (funcall (add3 1) 2 3)  ;; => 6 (partial application)
  (funcall (funcall (add3 1) 2) 3)  ;; => 6 (chained partial)

NAME is the function name to define.
ARGS is a list of parameter names (no &rest, &optional, etc.).
BODY is the function body to execute when fully applied."
  (let ((arity (length args)))
    `(cl-defun ,name (&rest call-args)
       (if (>= (length call-args) ,arity)
           (apply (lambda ,args ,@body) call-args)
         (apply #'l-partial #',name call-args)))))

(defmacro with-laurisp (&rest body)
  "Transform expressions to support curried function call syntax.

Enables the use of ((fn args) more-args) syntax within the macro body,
transforming such expressions into proper funcall forms.
This allows for more natural curried function composition and chaining.

The transformation converts:
  ((fn arg1) arg2 arg3)  =>  (funcall (fn arg1) arg2 arg3)
  (((fn arg1) arg2) arg3)  =>  (funcall (funcall (fn arg1) arg2) arg3)

Examples:
  (with-laurisp ((add3 1) 2 3))     ;; => 6
  (with-laurisp (((add3 1) 2) 3))   ;; => 6
  (with-laurisp (+ ((add3 1) 2 3) ((multiply3 2) 3 4)))  ;; => 30

BODY contains the expressions to transform.
Regular function calls and other expressions are left unchanged."
  `(progn ,@(mapcar #'l--transform-curry-calls body)))


(defmacro __ (block &optional arg)
  "Substitute all occurrences of \\=`__\\=' in BLOCK with ARG.

This macro provides a convenient way to create expressions
with placeholder substitution.
Every occurrence of the symbol \\=`__\\=' in BLOCK
will be replaced with ARG before evaluation.

BLOCK is the expression containing placeholder symbols \\=`__\\='.
ARG is the value that will replace all \\=`__\\=' placeholders.
If not provided, returns a function that expects one argument.

Example:
  (__ (+ __ (* __ 2)) 5)
  ;; Expands to: (+ 5 (* 5 2))
  ;; Evaluates to: 15

  (__ (+ __ (* __ 2)))
  ;; Returns a function that expects one argument
  ;; (funcall (__ (+ __ (* __ 2))) 5) evaluates to: 15

  (__ (list __ (car __) (cdr __)) \\='(1 2 3))
  ;; Expands to: (list (1 2 3) (car (1 2 3)) (cdr (1 2 3)))
  ;; Evaluates to: ((1 2 3) 1 (2 3))

The substitution is recursive, so nested lists and complex
expressions are handled correctly."
  (cl-labels ((substitute-__ (expr replacement)
                (cond
                 ((eq expr '__) replacement)
                 ((listp expr) (mapcar (lambda (x) (substitute-__ x replacement)) expr))
                 (t expr)))
              (has-__ (expr)
                (cond
                 ((eq expr '__) t)
                 ((listp expr) (cl-some #'has-__ expr))
                 (t nil))))
    (if (has-__ block)
        (if arg
            (substitute-__ block arg)
          `(lambda (x) ,(substitute-__ block 'x)))
      block)))

;;;;;;;;;;;;;;;;;;;;;;;
;; Private functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun l--transform-curry-calls (expr)
  "Transform curry call expressions recursively.

Internal function used by with-laurisp to transform expressions
containing curried function calls.
Identifies patterns like ((fn args) more-args) and
converts them to (funcall (fn args) more-args).

The transformation rules:
- ((fn args) more-args) becomes (funcall (fn args) more-args)
- Lambda expressions are preserved as-is
- Regular expressions are recursively processed
- Atoms are left unchanged

EXPR is the expression to transform, can be an atom, list, or nested structure.
Returns the transformed expression with curried calls
converted to funcall forms."
  (cond
   ((and (consp expr)
         (consp (car expr))
         (not (eq (caar expr) 'lambda)))
    ;; Transform ((fn args) more-args) to (funcall (fn args) more-args)
    `(funcall ,(l--transform-curry-calls (car expr)) ,@(cdr expr)))
   ((consp expr)
    (mapcar #'l--transform-curry-calls expr))
   (t expr)))

(provide 'laurisp)

;;; laurisp.el ends here
