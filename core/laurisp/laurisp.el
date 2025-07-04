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


(require 'cl-lib)

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
  "Define autocurried functions with pattern matching support.

I.e define a function that automatically curries when
called with fewer arguments.

Creates a function NAME that can be called with any number of
arguments up to the full arity defined by ARGS.
When called with fewer arguments than required,
it returns a partially applied function.
When called with the full number of
arguments, it executes the function body.

IMPORTANT: Variadic arguments with &rest are NOT supported.
ARGS must be a simple list of parameter names without &rest,
&optional, or other lambda list keywords.

PATTERN MATCHING:
Arguments can be specified as either symbols or lists for pattern matching.
- Symbol: (x) - matches any value, binds to x
- List: ((x value)) - matches only when x equals value

Pattern matching examples:
  (ldef fib ((n 0)) 0)                    ;; matches when n = 0
  (ldef fib ((n 1)) 1)                    ;; matches when n = 1
  (ldef fib (n) (+ (fib (- n 1)) (fib (- n 2))))  ;; general case
  
  (ldef greet ((name \"Alice\")) \"Hello, Alice!\")  ;; matches \"Alice\"
  (ldef greet (name) (concat \"Hi, \" name \"!\"))   ;; general case
  
  (ldef calc ((op '+) x y) (+ x y))       ;; matches when op = '+
  (ldef calc ((op '*) x y) (* x y))       ;; matches when op = '*
  (ldef calc (op x y) (error \"Unknown op: %s\" op))  ;; general case

CURRYING:
Functions defined with ldef automatically curry when called
with fewer arguments.

Currying examples:
  (ldef add3 (x y z) (+ x y z))
  (add3 1 2 3)        ;; => 6 (full application)
  (funcall (add3 1) 2 3)  ;; => 6 (partial application)
  (funcall (funcall (add3 1) 2) 3)  ;; => 6 (chained partial)

NAME is the function name to define.
ARGS is a list of parameter names (no &rest, &optional, etc.).
BODY is the function body to execute when fully applied."

  (let ((impl-name (intern (format "l-----%s-impl-" name)))
        (arity (length args)))
    `(progn
       ;; Define the actual implementation with pattern matching
       (cl-defmethod ,impl-name ,(l--parse-args args) ,@body)
       ;; Define the currying wrapper
       (defun ,name (&rest call-args)
         (if (>= (length call-args) ,arity)
             (apply #',impl-name call-args)
           (apply #'l-partial #',name call-args))))))


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

(cl-defmethod l--parse-arg ((arg list))
  "Parse ARG from `(symbol value)' to `(symbol (eql value))'."
  `(,(car arg) (equal ,(cadr arg))))

(cl-defmethod l--parse-arg ((arg symbol))
  "When ARG is a symbol, return it."
  arg)

(defun l--parse-args (args)
  "Parse a list of ARGS following =parse-arg' rules."
   (mapcar 'l--parse-arg args))

;; Extend cl-defmethod to accept 'equal


(defvar cl--generic-equal-used (make-hash-table :test #'equal))

(cl-generic-define-generalizer cl--generic-equal-generalizer
  140 (lambda (name &rest _) `(gethash ,name cl--generic-equal-used))
  (lambda (tag &rest _) (if (eq (car-safe tag) 'equal) (cdr tag))))


(cl-defmethod cl-generic-generalizers ((specializer (head equal)))
  "Support for (equal VAL) specializers.
These match if the argument is `equal' to VAL."
  (let* ((form (cadr specializer))
         (val (if (or (not (symbolp form)) (macroexp-const-p form))
                  (eval form t)
                form))
         (specializers (cdr (gethash val cl--generic-equal-used))))
    (cl-pushnew specializer specializers :test #'equal)
    (puthash val `(equal . ,specializers) cl--generic-equal-used))
  (list cl--generic-equal-generalizer))


(provide 'laurisp)
;;; laurisp.el ends here
