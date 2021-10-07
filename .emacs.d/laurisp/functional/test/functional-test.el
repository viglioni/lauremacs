;;; functional-test.el --- Projectile's test suite -*- lexical-binding: t -*-


;;; Code:

(require 'buttercup)
(load "./test/buttercup-helpers")
(load "./functional.el")

;;
;; Functions
;;

(test-suite "#compose-and-call"
  (context "there are more than one arguments to be applied"
    (it-should "compose functions in the correct order and apply args"
      (expect (compose-and-call
               ((+ 1)
                (* 2 1)) 2 3))
      :to-be 13))
  (context "functions are from seq lib"
    :var ((test-list '(1 2 3 4 5 6 7 8 9 10))
          (expected 55))
    (it-should "compose functions correctly and apply args"
      (expect (compose-and-call
               ((apply '+)
                (seq-concatenate 'list '(1 2 3 4))
                (seq-map (lambda (x) (+ 1 x)))
                (seq-filter (lambda (el) (> el 5)))) test-list)
              :to-be expected)))
  (context "functions receive regex params" 
    (it-should "properly compose functions and apply args"
      (expect (compose-and-call
               ((replace-regexp-in-string "foo" "bar"))
               "foo bar") :to-equal "bar bar"))))

(test-suite "#fp/pipe"
  (it-should "pipe functions in the correct order"
    (expect (fp/pipe 10
               ((+ 1)
                (* 2)))
            :to-be 22)))

(test-suite "#compose"
  (it-should "compose in the correct order"
    (expect
     (funcall  (compose (+ 1) (* 2)) 10)
     :to-equal 21)))

(test-suite "#fp/curry"
  (context "there are several arguments"
    (it-should "curry correctly"
      (expect
       (funcall (fp/curry + 1 2 3 4 5) 6 7 8 9 10)
       :to-be 55))))

;;
;; Logic
;;

(test-suite "#n-and"
  (context "all truthy vals"
    (it-should "return nil"
      (expect (n-and 1 2 3 t '(1)) :to-be nil)))
  (context "at least one falsey element"
    (it-should "should return true"
      (expect (n-and 1 2 nil) :to-be t)
      (expect (n-and nil nil) :to-be t))))

(test-suite "#n-or"
  (context "all falsey vals"
    (it-should "return t"
      (expect (n-or nil nil) :to-be t)))
  (context "at least one truthy element"
    (it-should "should return nil"
      (expect (n-or 1 2 nil) :to-be nil)
      (expect (n-or 1 2) :to-be nil))))

;;
;; List
;;

(test-suite "#all"
  (context "list has at least one nil entry"
    (it-should "return nil"
      (expect (all '(1 2 3 nil 4)) :to-be nil)
      (expect (all '(nil nil nil)) :to-be nil)
      (expect (all '("asd" nil 1 nil)) :to-be nil)))
  (context "all entries are truthy"
    (it-should "return t"
      (expect (all '(1 "ase" '(nil) '())) :to-be t))))

(test-suite "#any"
  (context "list has at least one truthy entry"
    (it-should "return t"
      (expect (any '(1 2 3 nil 4)) :to-be t)
      (expect (any '(nil nil nil 1)) :to-be t)
      (expect (any '("asd" nil 1 nil)) :to-be t)))
  (context "all entries are nil"
    (it-should "return nil"
      (expect (any '(nil nil nil)) :to-be nil))))

(test-suite "#contains?"
  (context "list is nil"
    (it-should "return nil"
      (expect (contains? nil 2) :to-be nil)))
  (context "element is in the list"
    (it-should "return t"
      (expect (contains? '(1 2 3) 1) :to-be t)))
  (context "element is not in the list"
    (it-should "return nil"
      (expect (contains? '(1 2 3) 4) :to-be nil))))

(test-suite "#head"
  (context "list is not empty"
    (it-should "return the first element"
      (expect (head '(1 2 3)) :to-be 1)
      (expect (head '(1)) :to-be 1)))
  (context "list is empty"
    (it-should "return nil"
      (expect (head '()) :to-be nil)
      (expect (head nil) :to-be nil))))

(test-suite "#not-contains?"
  (context "list is nil"
    (it-should "return nil"
      (expect (not-contains? nil 2) :to-be nil)))
  (context "element is not in the list"
    (it-should "return t"
      (expect (not-contains? '(1 2 3) 4) :to-be t)))
  (context "element is in the list"
    (it-should "return nil"
      (expect (not-contains? '(1 2 3) 3) :to-be nil))))

(test-suite "#tail"
  (context "list is empty"
    (it-should "return nil"
      (expect (tail '()) :to-be nil)
      (expect (tail nil) :to-be nil)))
  (context "there is one elment"
    (it-should "return nil"
      (expect (tail '(1)) :to-be nil)))
  (context "list has at least two elements"
    (it-should "return the (n-1)th elements of a list"
      (expect (tail '(1 2 3 4)) :to-equal '(2 3 4)))))

(test-suite "#unzip"
  (context "list or sublists are empty"
    (it-should "return nil"
      (expect (unzip nil) :to-be nil)
      (expect (unzip '(nil nil nil)) :to-be nil)))
  (context "lists have different size"
    (it-should "unzip to the size of the smaller"
      (expect (unzip '((1 2) nil)) :to-be nil)
      (expect (unzip '((1 2) (1))) :to-equal '((1 1)))))
  (context "lists have the same size"
    (it-should "unzip properly"
      (expect (unzip '((1 2 3) (1 2 3))) :to-equal '((1 1) (2 2) (3 3))))))

(test-suite "#zip"
  (context "all lists are empty"
    (it-should "return one empty list"
      (expect (zip nil nil nil) :to-be nil)
      (expect (zip '() '() '()) :to-be nil)))
  (context "the lists have different sizes"
    (it-should "zip the size of the smaller list"
      (expect (zip '(1 2) '(1) nil) :to-be nil)
      (expect (zip '(1 2 3) '(1 2 3 4) '(1)) :to-equal '((1 1 1)))))
  (context "lists have the same length"
    (it-should "properly zip them"
      (expect (zip '(1 1 1 1) '(2 2 2 2) '(3 3 3 3) '(4 4 4 4))
              :to-equal '((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4)))))
  (context "lists have different types"
    (it-should "properly zip them"
      (expect (zip '(1 2) '("fst" "snd") '(("list one") ("list two")))
              :to-equal '((1 "fst" ("list one")) (2 "snd" ("list two")))))))

;;
;; Number
;;

(test-suite "#inc"
  (context "number is integer"
    (it-should "increment number"
      (expect (inc 1) :to-be 2)
      (expect (inc 0) :to-be 1)
      (expect (inc -1) :to-be 0)))
  (context "number is not intenger"
    (it-should "increment number"
      (expect (inc 1.1) :to-equal 2.1))))


;;
;; Types
;;

(test-suite "#any-nil?"
  (context "all truthy vals"
    (it-should "return nil"
    (expect (any-nil? 1 2 3 t '(1)) :to-be nil)))
  (context "at least one falsey element"
    (it-should "should return true"
      (expect (any-nil? 1 2 nil) :to-be t)
      (expect (any-nil? nil nil) :to-be t))))

(test-suite "#all-nil?"
  (context "all falsey vals"
    (it-should "return t"
      (expect (all-nil? nil nil) :to-be t)))
  (context "at least one truthy element"
    (it-should "should return nil"
      (expect (all-nil? 1 2 nil) :to-be nil)
      (expect (all-nil? 1 2) :to-be nil))))

