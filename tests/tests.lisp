(in-package :clint-tests)

(defsuite clint ())
(defsuite special-forms (clint))
(defsuite arithmetic (clint))
(defsuite appliers (clint))
(defsuite macros (clint))

(deftest quote (special-forms)
  (assert-eql ^'x (eval-string "'x "))
  (assert-equal ^'(a b c) (eval-string "'(a b c)")))

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

(deftest funcall (appliers)
  (assert-eql 8 (eval-string "(funcall (lambda (x y) (+ x y 5)) 1 2)"))
  (assert-eql 15 (eval-string "(funcall (lambda (x) (+ x 5)) 10)")))

(deftest apply (appliers)
  (assert-eql 8 (eval-string "(apply (lambda (x y) (+ x y 5)) '(1 2))"))
  (assert-eql 120 (eval-string "(apply (lambda (w x y z) (* w x y z))
                                       2 3 '(4 5))")))

(deftest eval (appliers)
  (assert-eql 8 (eval-string "(eval '(+ 3 5))"))
  (assert-eql 20 (eval-string "(eval '(+ (* 5 3) 5))")))

(deftest let (macros)
  (assert-eql 5 (eval-string "(let ((x 2) (y 3)) (+ x y))"))
  (assert-eql 10 (eval-string "(let ((x 15)) (- x 5))")))

(deftest cond (macros)
  (assert-eql 2 (eval-string "(cond (1 2) (3 4))"))
  (assert-eql 4 (eval-string "(cond ('() 2) (3 4))")))
