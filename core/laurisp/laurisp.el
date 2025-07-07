;;; package --- Summary ;;; -*- lexical-binding: t; -*-
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
;; Emacs Lisp, drawing inspiration from Common Lisp, Haskell and Elixir.
;;
;; This library introduces currying, partial application, pattern matching,
;; and placeholder substitution utilities that make Emacs Lisp more expressive
;; and closer to modern functional programming paradigms.
;;
;; Key features:
;; - Automatic currying with `ldef'
;; - Pattern matching with `ldef'
;; - Partial application with `l-partial'
;; - Placeholder substitution with `__'
;; - Custom syntax `with-laurisp'
;; - Optional syntax transformation via `laurisp-syntax'
;;
;; Configuration:
;; The `laurisp-syntax' variable controls syntax transformation behavior.
;; It can be set globally:
;;
;;   (setq laurisp-syntax t)
;;
;; Or locally in a file using a property line:
;;
;;   ;; -*- laurisp-syntax: t; -*-
;;
;; When enabled, this allows for more concise syntax transformations
;; and enhanced readability in functional compositions.
;;
;; Example usage:
;;
;;   (ldef add3 (x y z) (+ x y z))
;;   (funcall (add3 1 2) 3) ; => 6
;;
;;   (ldef greet ((name "Alice")) "Hello, Alice!")
;;   (ldef greet (name) (concat "Hi, " name "!"))
;;   (greet "Alice") ; => "Hello, Alice!"
;;   (greet "Bob")   ; => "Hi, Bob!"
;;
;;   (with-laurisp
;;     ((add3 1) 2 3)) ; => 6
;;
;;   (__ (+ __ (* __ 2)) 5) ; => 15
;;
;;   (funcall (l-partial '+ 10) 5) ; => 15

;;; Code:


(require 'cl-lib)

;;;;;;;;;
;; API ;;
;;;;;;;;;

(defvar laurisp-syntax nil
  "Controls whether laurisp syntax transformations are applied during evaluation.

When set to t globally, laurisp syntax transformations will be applied
to all elisp evaluation operations without requiring file-local variable
declarations in individual files.

When set to nil (the default), laurisp syntax transformations will only
be applied to files that explicitly declare laurisp-syntax as a file-local
variable using either:

  ;; -*- laurisp-syntax: t; -*-

or in the local variables section at the end of the file:

  ;; Local Variables:
  ;; laurisp-syntax: t
  ;; End:

This variable affects the behavior of `eval-last-sexp', `eval-region',
`eval-buffer', `load-file', and `load' when the appropriate advice
functions are installed via `laurisp-syntax-advices'.

Setting this to t globally allows you to use laurisp syntax everywhere
without adding file-local variable declarations to each file, while
setting it to nil provides more granular control on a per-file basis.")

(defun laurisp-syntax-advices ()
  "Add advice to evaluation functions for laurisp syntax support.
This function adds around advice to `eval-last-sexp', `eval-region',
`eval-buffer', `load-file', and `load' to enable laurisp syntax processing."
  (interactive)
  (advice-add 'eval-last-sexp :around #'laurisp--eval-last-sexp-advice)
  (advice-add 'eval-region    :around #'laurisp--eval-region-advice)
  (advice-add 'eval-buffer    :around #'laurisp--eval-buffer-advice)
  (advice-add 'load-file      :around #'laurisp--load-file-advice)
  (advice-add 'load           :around #'laurisp--load-file-advice))

(defun laurisp-syntax-remove-advices ()
  "Remove advice to evaluation functions for laurisp syntax support.
This function adds around advice to `eval-last-sexp', `eval-region',
`eval-buffer', `load-file', and `load' to enable laurisp syntax processing."
  (interactive)
  (advice-remove 'eval-last-sexp #'laurisp--eval-last-sexp-advice)
  (advice-remove 'eval-region    #'laurisp--eval-region-advice)
  (advice-remove 'eval-buffer    #'laurisp--eval-buffer-advice)
  (advice-remove 'load-file      #'laurisp--load-file-advice)
  (advice-remove 'load           #'laurisp--load-file-advice))

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

(defmacro l (&rest expr)
  "TODO doc EXPR."
  (let* ((pos (cl-position '-> expr))
         (args (cl-subseq expr 0 pos))
         (body (cl-subseq expr (1+ pos)))
         )
    `(lambda ,args ,@body)))

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extend cl-defmethod to accept 'equal           ;;
;; and predicates listed in `l--type-predicates'  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar l--type-predicates
  '((:function   . functionp)
    (:number     . numberp)
    (:integer    . integerp)
    (:float      . floatp)
    (:string     . stringp)
    (:symbol     . symbolp)
    (:list       . listp)
    (:cons       . consp)
    (:vector     . vectorp)
    (:hash-table . hash-table-p)
    (:buffer     . bufferp)
    (:callable   . (lambda (x) (or (functionp x) (subrp x))))
    (:sequence   . sequencep)
    (:atom       . atom)
    (:null       . null))
  "Mapping of type keywords to predicate functions.")
(defvar l--generic-equal-used (make-hash-table :test #'equal))
(defvar l--generic-predicate-used (make-hash-table :test 'equal))

;; generalizers with priority order
(cl-generic-define-generalizer l--generic-predicate-generalizer
  150 (lambda (name &rest _) `(gethash ,name l--generic-predicate-used))
  (lambda (tag &rest _) (if (eq (car-safe tag) 'predicate) (cdr tag))))

(cl-generic-define-generalizer l--generic-equal-generalizer
  140 (lambda (name &rest _) `(gethash ,name l--generic-equal-used))
  (lambda (tag &rest _) (if (eq (car-safe tag) 'equal) (cdr tag))))

(cl-defmethod cl-generic-generalizers ((specializer (head equal)))
  "Support for (equal VAL) SPECIALIZER.
These match if the argument is `equal' to VAL."
  (let* ((form (cadr specializer))
         (val (if (or (not (symbolp form)) (macroexp-const-p form))
                  (eval form t)
                form))
         (specializers (cdr (gethash val l--generic-equal-used))))
    (cl-pushnew specializer specializers :test #'equal)
    (puthash val `(equal . ,specializers) l--generic-equal-used))
  (list l--generic-equal-generalizer))

(cl-defmethod cl-generic-generalizers ((specializer (head predicate)))
  "Support for (predicate PRED) specializers."
    (print specializer)
  (let* ((pred (cadr specializer))
         (pred-key (if (symbolp pred) pred (prin1-to-string pred)))
         (specializers (cdr (gethash pred-key l--generic-predicate-used))))
    (cl-pushnew specializer specializers :test #'equal)
    (puthash pred-key `(predicate . ,specializers) l--generic-predicate-used))
  (list l--generic-predicate-generalizer))


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

(defun l--transform-sigils (expr)

  )

(cl-defmethod l--parse-arg ((arg list))
  "Parse ARG from `(symbol value)' to appropriate specializer.

This method transforms argument specifications from the ldef macro into
appropriate `cl-defmethod' specializers.  It handles two main cases:

1. Type specializers: (symbol :type) -> (symbol type)
   When the second element is a keyword, it's treated as a type specializer.
   The keyword is converted to a symbol by removing the leading colon.

2. Value specializers: (symbol value) -> (symbol (equal value))
   When the second element is any other value, it's treated as a value
   specializer using the `equal' predicate.

ARG must be a list of exactly two elements: \\(parameter-name specification)
where parameter-name is a symbol and specification is either a keyword
for type specialization or any value for value specialization.

Examples:
  \\(l--parse-arg \='(x :integer))     ;; => (x integer)
  \\(l--parse-arg \='(name \"Alice\")) ;; => (name (equal \"Alice\"))
  \\(l--parse-arg \='(op '+))          ;; => (op (equal +))
  \\(l--parse-arg \='(flag t))         ;; => (flag (equal t))

Returns a list suitable for use as a cl-defmethod parameter specializer."
  (let ((param (car arg))
        (spec (cadr arg)))
    (cond
     ;; ;; Case 1: (symbol :predicate-type) -> predicate check
     ;; ((and (keywordp spec) (assoc spec l--type-predicates))
     ;;  (let ((predicate (cdr (assoc spec l--type-predicates))))
     ;;    `(,param (predicate ,predicate))))
     ;; Case 2: (symbol :type) -> (symbol type) - type specializer
     ((keywordp spec)
      `(,param ,(intern (substring (symbol-name spec) 1))))
     ;; Case 3: (symbol value) -> (symbol (equal value)) - value specializer
     (t
      `(,param (equal ,spec))))))

(cl-defmethod l--parse-arg ((arg symbol))
  "When ARG is a symbol, return it."
  arg)

(defun l--parse-args (args)
  "Parse a list of ARGS following =parse-arg' rules."
   (mapcar 'l--parse-arg args))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use laurisp syntax without `with-laurisp' ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun laurisp--process-file-content (content)
  "Transform file CONTENT through with-laurisp."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (let ((forms '()))
      (while (not (eobp))
        (condition-case nil
            (push (read (current-buffer)) forms)
          (end-of-file nil)))
      (eval `(with-laurisp ,@(nreverse forms))))))


(defun laurisp--load-file-advice (orig-fun file &optional noerror nomessage)
  "Advice for load-file to handle laurisp-syntax."
  (if (and (stringp file) (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((laurisp-syntax (hack-local-variables-prop-line)))
          (if (cdr (assq 'laurisp-syntax laurisp-syntax))
              ;; Transform through with-laurisp
              (laurisp--process-file-content (buffer-string))
            ;; Regular loading
            (funcall orig-fun file noerror nomessage))))
    (funcall orig-fun file noerror nomessage)))


(defun laurisp--load-advice (orig-fun file &optional noerror nomessage nosuffix must-suffix)
  "Advice for load to handle laurisp-syntax."
  (if (and (stringp file) (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((laurisp-syntax (hack-local-variables-prop-line)))
          (if (cdr (assq 'laurisp-syntax laurisp-syntax))
              ;; Transform through with-laurisp
              (laurisp--process-file-content (buffer-string))
            ;; Regular loading
            (funcall orig-fun file noerror nomessage nosuffix must-suffix))))
    (funcall orig-fun file noerror nomessage nosuffix must-suffix)))


(defun laurisp--check-file-local-vars ()
  "Check if current buffer has laurisp-syntax enabled.

This function examines the current buffer for file-local variables,
specifically looking for the `laurisp-syntax' variable.  It processes
both the prop-line (first line) and the file-local variables section
at the end of the file.

This function is typically used to determine whether laurisp syntax
processing should be applied to the current buffer."
  (hack-local-variables-prop-line)
  (hack-local-variables))


(defun laurisp--eval-last-sexp-advice (orig-fun &optional eval-last-sexp-arg-internal)
  "Advice for `eval-last-sexp' to handle laurisp-syntax.

This around advice function intercepts calls to `eval-last-sexp' and
checks if the current buffer has `laurisp-syntax' enabled as a buffer-local
variable.  If so, it wraps the preceding S-expression in a `with-laurisp'
form before evaluation.

ORIG-FUN is the original `eval-last-sexp' function.
EVAL-LAST-SEXP-ARG-INTERNAL is the optional argument
passed to the original function.

The wrapping is achieved by temporarily redefining `elisp--preceding-sexp'
to return the S-expression wrapped in `with-laurisp', allowing the original
function to handle all other aspects of evaluation including output formatting."
  (if (and (boundp 'laurisp-syntax) laurisp-syntax)
      (let ((sexp (elisp--preceding-sexp)))
        ;; Wrap in with-laurisp and let original function handle everything
        (cl-letf (((symbol-function 'elisp--preceding-sexp)
                   (lambda () `(with-laurisp ,sexp))))
          (funcall orig-fun eval-last-sexp-arg-internal)))
    (funcall orig-fun eval-last-sexp-arg-internal)))

(defun laurisp--eval-region-advice (orig-fun start end &optional printflag read-function)
  "Advice for `eval-region' to handle laurisp-syntax.

This around advice function intercepts calls to `eval-region' and
checks if the current buffer has `laurisp-syntax' enabled as a
buffer-local variable.
If so, it wraps the region content in a `with-laurisp' form before evaluation.

ORIG-FUN is the original `eval-region' function.
START and END define the region boundaries.
PRINTFLAG and READ-FUNCTION are optional arguments
passed to the original function.

The wrapping is achieved by temporarily redefining
`buffer-substring-no-properties' to return the region content wrapped in
`with-laurisp', allowing the original function
to handle all other aspects of evaluation."
  (if (and (boundp 'laurisp-syntax) laurisp-syntax)
      (let ((original-code (buffer-substring-no-properties start end)))
        ;; Wrap in with-laurisp and let original function handle everything
        (cl-letf (((symbol-function 'buffer-substring-no-properties)
                   (lambda () (format "(with-laurisp %s)" original-code))))
          (funcall orig-fun start end printflag read-function)))
    (funcall orig-fun start end printflag read-function)))

(defun laurisp--eval-buffer-advice (orig-fun &optional buffer printflag filename unibyte)
  "Advice for `eval-buffer' to handle laurisp-syntax.

This around advice function intercepts calls to `eval-buffer' and
checks if the target buffer has `laurisp-syntax' enabled as a buffer-local
variable.  If so, it wraps the entire buffer content in a `with-laurisp'
form before evaluation.

ORIG-FUN is the original `eval-buffer' function.
BUFFER is the buffer to evaluate (defaults to current buffer).
PRINTFLAG, FILENAME, and UNIBYTE are optional arguments passed to
the original function.

The wrapping is achieved by temporarily redefining `buffer-string'
to return the buffer content wrapped in `with-laurisp', allowing the original
function to handle all other aspects of evaluation."
  (with-current-buffer (or buffer (current-buffer))
    (if (and (boundp 'laurisp-syntax) laurisp-syntax)
        (let ((original-content (buffer-string)))
          ;; Wrap in with-laurisp and let original function handle everything
          (cl-letf (((symbol-function 'buffer-string)
                     (lambda () (format "(with-laurisp %s)" original-content))))
            (funcall orig-fun buffer printflag filename unibyte)))
      (funcall orig-fun buffer printflag filename unibyte))))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; l-generic dispatcher ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar l-generic-registry (make-hash-table :test 'equal)
  "Registry of generic function methods.
Structure: function-name -> list of (specificity arity pattern-list body)")

(defun l-generic--calculate-specificity (pattern-list)
  "Calculate specificity score for PATTERN-LIST.
Higher score = more specific.
Value match: 1000, Type match: 100, Wildcard: 1"
  (cl-reduce #'+
             (mapcar (lambda (pattern)
                       (cond
                        ((listp pattern)
                         (let ((spec (cadr pattern)))
                           (cond
                            ((keywordp spec) 100)  ; type match
                            (t 1000))))            ; value match
                        ((and (symbolp pattern)
                              (string-prefix-p "_" (symbol-name pattern)))
                         1)                        ; wildcard with binding
                        (t 1)))                    ; regular wildcard
                     pattern-list)
             :initial-value 0))

(defun l-generic--generate-pattern-condition (pattern arg-index)
  "Generate condition for matching PATTERN against argument at ARG-INDEX."
  (cond
   ((listp pattern)
    (let ((param (car pattern))
          (spec (cadr pattern)))
      (cond
       ((keywordp spec)
        ;; Type match: (arg :integer) -> (integerp (nth 0 args))
        (let ((predicate (cdr (assoc spec l--type-predicates))))
          (if predicate
              `(,predicate (nth ,arg-index args))
            (error "Unknown type predicate: %s" spec))))
       (t
        ;; Value match: (arg "value") -> (equal (nth 0 args) "value")
        `(equal (nth ,arg-index args) ,spec)))))
   ((and (symbolp pattern)
         (string-prefix-p "_" (symbol-name pattern)))
    ;; Wildcard with binding: always true
    t)
   (t
    ;; Regular parameter: always true
    t)))

(defun l-generic--generate-bindings (pattern-list)
  "Generate let bindings for PATTERN-LIST parameters."
  (cl-loop for pattern in pattern-list
           for i from 0
           collect (let ((param (if (listp pattern) (car pattern) pattern)))
                     `(,param (nth ,i args)))))

(defun l-generic--generate-method-clause (method-spec)
  "Generate a cond clause for METHOD-SPEC."
  (let* ((specificity (nth 0 method-spec))
         (arity (nth 1 method-spec))
         (pattern-list (nth 2 method-spec))
         (body (nth 3 method-spec))
         (conditions (cl-loop for pattern in pattern-list
                             for i from 0
                             collect (l-generic--generate-pattern-condition pattern i)))
         (bindings (l-generic--generate-bindings pattern-list)))
    
    `((and ,@(remove t conditions))  ; Remove 'always true' conditions
      (let ,bindings
        ,@body))))

(defun l-generic--generate-dispatch-function (name methods)
  "Generate the complete dispatch function for NAME with METHODS."
  (let* ((methods-by-arity (cl-loop for method in methods
                                   for arity = (nth 1 method)
                                   collect (cons arity method)))
         (max-arity (if methods (apply #'max (mapcar #'car methods-by-arity)) 0))
         (min-arity (if methods (apply #'min (mapcar #'car methods-by-arity)) 0))
         (arity-groups (cl-loop for arity from min-arity to max-arity
                               collect (cons arity 
                                           (cl-remove-if-not 
                                            (lambda (method) (= (nth 1 method) arity))
                                            methods)))))
    
    `(defun ,name (&rest args)
       (let ((arity (length args)))
         (cond
          ,@(cl-loop for (arity . arity-methods) in arity-groups
                    when arity-methods
                    collect `((= arity ,arity)
                             (cond
                              ,@(mapcar #'l-generic--generate-method-clause arity-methods)
                              (t (error "PatternMatch error in '%s': couldn't match %S" 
                                       ',name args)))))
          ;; Currying case
          (t (apply #'l-partial #',name args)))))))

(defun l-generic--add-method (name arity pattern-list body)
  "Add a method to the registry and regenerate dispatch function."
  (let* ((specificity (l-generic--calculate-specificity pattern-list))
         (method-spec (list specificity arity pattern-list body))
         (current-methods (gethash name l-generic-registry '())))
    
    ;; Add new method and sort by specificity (descending)
    (puthash name 
             (sort (cons method-spec current-methods)
                   (lambda (a b) (> (car a) (car b))))
             l-generic-registry)
    
    ;; Regenerate dispatch function
    (eval (l-generic--generate-dispatch-function name (gethash name l-generic-registry)))))

(defun l-generic-cleanup (name)
  "Remove generic function NAME and all its methods."
  (interactive "SGeneric function name: ")
  (remhash name l-generic-registry)
  (fmakunbound name))

(defmacro l-generic (name args &rest body)
  "Define a method for generic function NAME with pattern matching and currying."
  (let* ((rest-pos (cl-position '&rest args))
         (fixed-args (if rest-pos (cl-subseq args 0 rest-pos) args))
         (rest-arg (if rest-pos (nth (1+ rest-pos) args) nil))
         (arity (length fixed-args))
         (has-rest rest-pos))
    
    (if has-rest
        ;; Handle &rest arguments - create a wrapper that transforms calls
        `(progn
           (defun ,name (&rest all-args)
             (if (>= (length all-args) ,arity)
                 (let (,@(cl-loop for arg in fixed-args
                                 for i from 0
                                 collect `(,arg (nth ,i all-args)))
                       (,rest-arg (nthcdr ,arity all-args)))
                   ,@body)
               (apply #'l-partial #',name all-args)))
           ',name)
      ;; Regular fixed-arity function
      `(progn
         (l-generic--add-method ',name ,arity ',args '(,@body))
         ',name))))

(defmacro ldef (name args &rest body)
  "Define autocurried functions with pattern matching support.

Creates a function NAME that automatically curries when called with fewer 
arguments and supports pattern matching on arguments.

ARGS is a list of parameter patterns supporting:
- Regular parameters: arg
- Wildcards: _ignore, _var (bind but conventionally ignore)  
- Type matches: (arg :integer), (arg :string), etc.
- Value matches: (arg \"specific-value\"), (arg 42), etc.

Methods are ordered by specificity (most specific first):
1. Value matches (1000 points each)
2. Type matches (100 points each)  
3. Wildcards (1 point each)

PATTERN MATCHING:
Arguments can be specified as either symbols or lists for pattern matching.
- Symbol: x - matches any value, binds to x
- Wildcard: _ignore - matches any value, binds but conventionally ignored
- Type match: (x :integer) - matches only when x satisfies integerp
- Value match: (x \"value\") - matches only when x equals \"value\"

Pattern matching examples:
  (ldef fib ((n 0)) 0)                    ;; matches when n = 0
  (ldef fib ((n 1)) 1)                    ;; matches when n = 1
  (ldef fib (n) (+ (fib (- n 1)) (fib (- n 2))))  ;; general case
  
  (ldef greet ((name \"Alice\")) \"Hello, Alice!\")  ;; matches \"Alice\"
  (ldef greet (name) (concat \"Hi, \" name \"!\"))   ;; general case
  
  (ldef calc ((op '+) x y) (+ x y))       ;; matches when op = '+
  (ldef calc ((op '*) x y) (* x y))       ;; matches when op = '*
  (ldef calc (_op _x _y) (error \"Unknown operation\"))  ;; fallback

CURRYING:
Functions defined with ldef automatically curry when called
with fewer arguments.

Currying examples:
  (ldef add3 (x y z) (+ x y z))
  (add3 1 2 3)        ;; => 6 (full application)
  (funcall (add3 1) 2 3)  ;; => 6 (partial application)
  (funcall (funcall (add3 1) 2) 3)  ;; => 6 (chained partial)

NAME is the function name to define.
ARGS is a list of parameter patterns.
BODY is the function body to execute when pattern matches and fully applied."
  `(l-generic ,name ,args ,@body))


(provide 'laurisp)
;;; laurisp.el ends here
