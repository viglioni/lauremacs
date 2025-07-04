;;; -*- lexical-binding: t; -*-
;;
;; @author Laura Viglioni
;; 2025
;;
;; GNU Public License 3.0
;;
;; since: NEXT
;;
;; laurisp-test.el:
;; Tests for laurisp.el
;;

;;; code:

(require 'buttercup)
(require 'test-helpers)



(describe "laurisp.el"
  (before-all
    (let ((laurisp-path (expand-file-name "core/laurisp/laurisp.el" user-emacs-directory)))
      (message "Looking for laurisp.el at: %s" laurisp-path)
      (if (file-exists-p laurisp-path)
          (load laurisp-path nil t)
        (error "Cannot find laurisp.el at %s" laurisp-path))))

  (describe "l-partial"
    (test-it "creates a partial function correctly"
             (let ((partial-fn (l-partial '+ 5)))
               (expect (funcall partial-fn 3) :to-equal 8)))
    
    (test-it "works with multiple initial arguments"
             (let ((partial-fn (l-partial '* 2 3)))
               (expect (funcall partial-fn 4) :to-equal 24)))

    (test-it "works with no initial arguments"
             (let ((partial-fn (l-partial '+)))
               (expect (funcall partial-fn 2 3) :to-equal 5)))

    (test-it "works with functions that return functions"
             (let ((partial-fn (l-partial 'l-partial '+ 5)))
               (expect (funcall (funcall partial-fn) 3) :to-equal 8)))

    (test-it "works with string functions"
             (let ((partial-fn (l-partial 'concat "Hello, ")))
               (expect (funcall partial-fn "World!") :to-equal "Hello, World!")))

    (test-it "works with list functions"
             (let ((partial-fn (l-partial 'append '(1 2))))
               (expect (funcall partial-fn '(3 4)) :to-equal '(1 2 3 4)))))

  (describe "ldef"
    (describe "basic function definition"
      (test-it "defines a function that works with all arguments provided"
               (ldef test-add (x y) (+ x y))
               (expect (test-add 3 4) :to-equal 7))
      
      (test-it "works with single argument functions"
               (ldef test-square (x) (* x x))
               (expect (test-square 5) :to-equal 25))
      
      (test-it "works with multiple argument functions"
               (ldef test-multiply (x y z) (* x y z))
               (expect (test-multiply 2 3 4) :to-equal 24)))

    (describe "edge cases"
      (test-it "works with zero argument functions"
               (ldef test-constant () 42)
               (expect (test-constant) :to-equal 42))

      (test-it "works with functions that return complex data"
               (ldef test-list-maker (x y z) (list x y z))
               (expect (test-list-maker 1 2 3) :to-equal '(1 2 3)))

      (test-it "works with functions that use multiple argument destructuring"
               (ldef test-variadic (x y &rest rest) (append (list x y) rest))
               (expect (test-variadic 1 2 3 4 5) :to-equal '(1 2 3 4 5)))

      (test-it "works with functions that modify arguments"
               (ldef test-modifier (x y) (cons (1+ x) (1+ y)))
               (expect (test-modifier 1 2) :to-equal '(2 . 3)))

      (test-it "works with functions that call other functions"
               (ldef test-caller (x y) (test-add x y))
               (expect (test-caller 5 7) :to-equal 12)))

    (describe "currying behavior"
      (before-all
        (ldef add3 (x y z) (+ x y z)))
      
      (test-it "works with all arguments at once"
               (expect (add3 1 2 3) :to-equal 6))
      
      (test-it "works with partial application - 1 arg then 2"
               (expect (funcall (add3 1) 2 3) :to-equal 6))
      
      (test-it "works with partial application - 2 args then 1"
               (expect (funcall (add3 1 2) 3) :to-equal 6))
      
      (test-it "works with chained partial applications"
               (expect (funcall (funcall (add3 1) 2) 3) :to-equal 6))
      
      (test-it "works with full currying chain"
               (expect (funcall (funcall (funcall (add3) 1) 2) 3) :to-equal 6)))

    (describe "advanced currying scenarios"
      (before-all
        (ldef multiply4 (w x y z) (* w x y z))
        (ldef concat3 (a b c) (concat a b c)))

      (test-it "works with 4-argument functions"
               (expect (multiply4 2 3 4 5) :to-equal 120))

      (test-it "works with 4-argument partial application"
               (expect (funcall (multiply4 2 3) 4 5) :to-equal 120))

      (test-it "works with string concatenation currying"
               (expect (funcall (concat3 "Hello") " " "World") :to-equal "Hello World"))

      (test-it "works with mixed data types in currying"
               (ldef mixed-fn (num str list) (list num str (length list)))
               (expect (funcall (mixed-fn 42) "test" '(1 2 3)) :to-equal '(42 "test" 3))))

    (describe "pattern matching with ldef"
      (before-all
        (ldef fib ((n 0)) 0)
        (ldef fib ((n 1)) 1)
        (ldef fib (n) (+ (fib (- n 1)) (fib (- n 2))))
        )

      (test-it "matches base case for n=0"
               (expect (fib 0) :to-equal 0))

      (test-it "matches base case for n=1"
               (expect (fib 1) :to-equal 1))

      (test-it "uses general case for n=2"
               (expect (fib 2) :to-equal 1))

      (test-it "uses general case for n=3"
               (expect (fib 3) :to-equal 2))

      (test-it "uses general case for n=4"
               (expect (fib 4) :to-equal 3))

      (test-it "uses general case for n=5"
               (expect (fib 5) :to-equal 5))

      (test-it "works with currying on pattern matched functions"
               (expect (funcall (fib) 3) :to-equal 2))

      (describe "multiple pattern matches"
        (before-all
          (ldef factorial ((n 0)) 1)
          (ldef factorial ((n 1)) 1)
          (ldef factorial (n) (* n (factorial (- n 1)))))

        (test-it "matches factorial base cases"
                 (expect (factorial 0) :to-equal 1)
                 (expect (factorial 1) :to-equal 1))

        (test-it "computes factorial recursively"
                 (expect (factorial 5) :to-equal 120))

        (test-it "works with currying on factorial"
                 (expect (funcall (factorial) 4) :to-equal 24)))

      (describe "pattern matching with different types"
        (before-all
          (ldef type-checker ((x nil)) "nil")
          (ldef type-checker ((x t)) "true")
          (ldef type-checker ((x 0)) "zero")
          (ldef type-checker (x) "other"))

        (test-it "matches nil value"
                 (expect (type-checker nil) :to-equal "nil"))

        (test-it "matches true value"
                 (expect (type-checker t) :to-equal "true"))

        (test-it "matches zero value"
                 (expect (type-checker 0) :to-equal "zero"))

        (test-it "uses general case for other values"
                 (expect (type-checker 42) :to-equal "other")
                 (expect (type-checker "hello") :to-equal "other")
                 (expect (type-checker '(1 2 3)) :to-equal "other")))

      (describe "pattern matching with strings"
        (before-all
          (ldef greet ((name "Alice")) "Hello, Alice!")
          (ldef greet ((name "Bob")) "Hey, Bob!")
          (ldef greet (name) (concat "Hi, " name "!")))

        (test-it "matches specific string patterns"
                 (expect (greet "Alice") :to-equal "Hello, Alice!")
                 (expect (greet "Bob") :to-equal "Hey, Bob!"))

        (test-it "uses general case for other strings"
                 (expect (greet "Charlie") :to-equal "Hi, Charlie!")))

      (describe "pattern matching with multiple arguments"
        (before-all
          (ldef calculator ((op '+) x y) (+ x y))
          (ldef calculator ((op '-) x y) (- x y))
          (ldef calculator ((op '*) x y) (* x y))
          (ldef calculator (op x y) (error "Unknown operation: %s" op)))

        (test-it "matches addition operation"
                 (expect (calculator '+ 3 4) :to-equal 7))

        (test-it "matches subtraction operation"
                 (expect (calculator '- 10 3) :to-equal 7))

        (test-it "matches multiplication operation"
                 (expect (calculator '* 5 6) :to-equal 30))

        (test-it "works with currying on pattern matched multi-arg functions"
                 (expect (funcall (calculator '+) 2 3) :to-equal 5)
                 (expect (funcall (calculator '+ 2) 3) :to-equal 5))))
    )

  (describe "with-laurisp"
    (before-all
      (ldef add3 (x y z) (+ x y z)))
    
    (test-it "works with all arguments at once"
             (expect (with-laurisp (add3 1 2 3)) :to-equal 6))
    
    (test-it "works with partial application - 1 arg then 2"
             (expect (with-laurisp ((add3 1) 2 3)) :to-equal 6))
    
    (test-it "works with partial application - 2 args then 1"
             (expect (with-laurisp ((add3 1 2) 3)) :to-equal 6))
    
    (test-it "works with chained partial applications"
             (expect (with-laurisp (((add3 1) 2) 3)) :to-equal 6))
    
    (test-it "works with full currying chain"
             (expect (with-laurisp ((((add3) 1) 2) 3)) :to-equal 6))

    (describe "complex transformation scenarios"
      (before-all
        (ldef multiply3 (x y z) (* x y z))
        (ldef subtract2 (x y) (- x y)))

      (test-it "works with nested function calls"
               (expect (with-laurisp (add3 (multiply3 2 3 4) 5 6)) :to-equal 35))

      (test-it "works with multiple curried expressions"
               (expect (with-laurisp (+ ((add3 1) 2 3) ((multiply3 2) 3 4))) :to-equal 30))

      (test-it "works with deeply nested currying"
               (expect (with-laurisp ((((add3) 1) 2) 3)) :to-equal 6))

      (test-it "works with mixed curried and normal calls"
               (expect (with-laurisp (+ (add3 1 2 3) ((subtract2 10) 4))) :to-equal 12))

      (test-it "preserves regular function calls"
               (expect (with-laurisp (+ 1 2 3)) :to-equal 6))

      (test-it "works with lambda expressions"
               (expect (with-laurisp (funcall (lambda (x) (+ x 1)) 5)) :to-equal 6))

      (test-it "works with quoted expressions"
               (expect (with-laurisp (car '(1 2 3))) :to-equal 1))

      (test-it "works with complex nested structures"
               (expect (with-laurisp (list ((add3 1) 2 3) ((multiply3 2 3) 4))) :to-equal '(6 24))))
    
    )

  (describe "__"
    (before-all
      (ldef delta (a b c) (- (* b b) (* 4 a c))))

    (test-it "works without currying"
             (expect (delta 1 2 3) :to-equal -8))

    (test-it "works with currying"
             (expect (funcall (delta 1 2) 3) :to-equal -8))

    (test-it "works as a placeholder for first argument"
             (expect (funcall (__ (delta __ 2 3)) 1) :to-equal -8))

    (test-it "works as a placeholder for second argument"
             (expect (funcall (__ (delta 1 __ 3)) 2) :to-equal -8))

    (test-it "works as a placeholder for third argument"
             (expect (funcall (__ (delta 1 2 __)) 3) :to-equal -8))

    (test-it "works without placeholder - behaves like normal currying"
             (expect (funcall (__ (delta 1 2)) 3) :to-equal -8)
             (expect  (__ (delta 1 2 3)) :to-equal -8)))

  
  )



    ;;; laurisp-test.el ends here
