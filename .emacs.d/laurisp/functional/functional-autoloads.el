;;; functional-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "functional" "functional.el" (0 0 0 0))
;;; Generated autoloads from functional.el

(autoload 'fp/curry "functional" "\
Returns the curried function.
   ((* -> x) arg1, ..., argN) -> ((argN+1, ..., argM) -> x)
   e.g.:
   (fp/curry + 1 2 3) -> (lambda (argN ... argM) (+ 1 2 3 argN ... argM))

\(fn FN &rest INITIAL-ARGS)" nil t)

(autoload 'fp/pipe "functional" "\
Pipe an argument into composed functions from left to right.
   a -> ((a -> b) (b -> c) ... (n -> m)) -> m
   e.g.:
   (fp/pipe  5 ((+ 1) (* 2))) -> 12

\(fn ARG FN-LIST)" nil t)

(function-put 'fp/pipe 'lisp-indent-function 'defun)

(autoload 'compose-and-call "functional" "\
Since compose returns a function, this helper receives a list of
   functions and args and apply them to composed funcs
   (a ... n) -> ((y -> z) ... (m -> o) ((a ... n) -> m) ) -> z
    e.g.:
    (compose-and-call ((+ 1) (* 2)) 2 3) -> 13

\(fn FN-LIST &rest ARGS)" nil t)

(autoload 'bool "functional" "\
returns t if element is truthy, nil if its falsey
   a -> bool

\(fn X)" nil nil)

(autoload 'n-and "functional" "\
Not and.
   * -> bool

\(fn &rest ARGS)" nil nil)

(autoload 'n-or "functional" "\
Not or.
   * -> bool

\(fn &rest ARGS)" nil nil)

(autoload 'all "functional" "\
Returns t if all elements in list are truthy
   [a] → Boolean

\(fn LST)" nil nil)

(autoload 'any "functional" "\
Returns t if at least one element in list is truthy
   [a] → Boolean

\(fn LST)" nil nil)

(autoload 'contains\? "functional" "\
Returns t/nil if element is in list
   ([a] a) -> bool

\(fn LIST ELEMENT)" nil nil)

(autoload 'head "functional" "\
Returns the first element of a list
   [a] -> a | nil

\(fn LIST)" nil nil)

(autoload 'not-contains\? "functional" "\
Returns t/nil if element is not in list
   ([a] a) -> bool

\(fn LIST ELEMENT)" nil nil)

(autoload 'tail "functional" "\
Returns the list but its first element
   [a] -> [a] | nil

\(fn LIST)" nil nil)

(autoload 'unzip "functional" "\
unzip n lists
   [[a]] -> [[a]]
   e.g.:
   (unzip '((1 2 ... n) (1 2 ... n))) ->
   '((1 1 ... 1) (2 2 .... 2) ... (n n ... n)

\(fn ZIPPED-LIST)" nil nil)

(autoload 'zip "functional" "\
zips n lists
   [[a]] -> [[a]]
   e.g.:
   (zip '(1 1) '(2 2) '(3 3)) -> '((1 2 3) (1 2 3))

\(fn &rest LISTS)" nil nil)

(autoload 'inc "functional" "\
Returns the increment of n
   Number -> Number

\(fn N)" nil nil)

(autoload 'all-nil\? "functional" "\
return if all args are nil
   (* ... *) -> boolean
   e.g (all-nil? nil nil) -> t

\(fn &rest ARGS)" nil nil)

(autoload 'fp/alist-sort-by-car "functional" "\


\(fn ALIST)" nil nil)

(autoload 'alist-sort-by-cdr-ci "functional" "\
sort alist by cdr. case insensitive

\(fn ALIST)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "functional" '("any-nil?" "compose" "curry-expr" "identity")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; functional-autoloads.el ends here
