(in-package :clint-tests)

(defsuite clint ())
(defsuite special-forms (clint))
(defsuite arithmetic (clint))

(deftest quote (special-forms)
  (assert-eql ^x (eval-string "'x "))
  (assert-equal ^(a b c) (eval-string "'(a b c)")))

(deftest if (special-forms)
  (assert-eql 2   (eval-string "(if 1 2 3)"))
  (assert-eql 3   (eval-string "(if '() 2 3)"))
  (assert-eql 2   (eval-string "(if 1 2)"))
  (assert-eql nil (eval-string "(if 1 '())")))

(deftest progn (special-forms)
  (assert-eql 1 (eval-string "(progn 1)"))
  (assert-eql 2 (eval-string "(progn 1 2)"))
  (assert-eql 3 (eval-string "(progn 1 2 3)")))

(deftest lambda (special-forms)
  (assert-eql 10 (eval-string "((lambda (x y) (+ x y)) 3 7)"))
  (assert-eql 20 (eval-string "((lambda (x) (* 4 x)) 5)")))

(deftest + (arithmetic)
  (assert-eql 5 (eval-string "(+ 2 3)"))
  (assert-eql 10 (eval-string "(+ 5 5)"))
  (assert-eql 15 (eval-string "(+ 1 2 3 4 5)")))

(deftest - (arithmetic)
  (assert-eql 10  (eval-string "(- 15 5)"))
  (assert-eql 0   (eval-string "(- 10 10)"))
  (assert-eql -5  (eval-string "(- 10 15)"))
  (assert-eql -13 (eval-string "(- 1 2 3 4 5)")))

(deftest * (arithmetic)
  (assert-eql 20 (eval-string "(* 5 4)"))
  (assert-eql 15 (eval-string "(* 3 5)"))
  (assert-eql 24 (eval-string "(* 1 2 3 4)")))

(deftest / (arithmetic)
  (assert-eql 5/4 (eval-string "(/ 5 4)"))
  (assert-eql 2   (eval-string "(/ 10 5)"))
  (assert-eql 1/2 (eval-string "(/ 6 4 3)")))

(deftest +-*/ (arithmetic)
  (assert-eql 15 (eval-string "(* 5 (+ 3 (/ 8 (- 2 6)) (+ 1 1)))")))
