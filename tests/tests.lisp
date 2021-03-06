(in-package :clint-tests)

(defmacro defassert-cl (new-name old-name &optional (is-result t))
  "Define a macro, NEW-NAME that is equivalent to OLD-NAME but runs
   it's second argument, a string, through the Clint interpreter.
   The argument is-result should be nil if assertion is similar to
   assert-true where there is no result form."
  (let ((expect (gensym)) (result (gensym)) (args (gensym)))
    `(defmacro ,new-name ,(if is-result
			      `(,expect ,result &rest ,args)
			      `(,result &rest ,args))
       ,(if is-result
	    ``(,',old-name ,,expect (eval-string ,,result) ,@,args)
	    ``(,',old-name (eval-string ,,result) ,@,args)))))

(defassert-cl assert-eq-cl     assert-eq)
(defassert-cl assert-eql-cl    assert-eql)
(defassert-cl assert-equal-cl  assert-equal)
(defassert-cl assert-equalp-cl assert-equalp)
(defassert-cl assert-true-cl   assert-true  nil)
(defassert-cl assert-false-cl  assert-false nil)

(defsuite clint ())
(defsuite special-forms (clint))
(defsuite arithmetic (clint))
(defsuite appliers (clint))
(defsuite macros (clint))
(defsuite list (clint))
(defsuite setters (clint))
(defsuite predicates (clint))
(defsuite logic-macros (clint))
(defsuite symbols (clint))
(defsuite reader (clint))

(defsuite libs (clint))
(defsuite hofs (libs))
(defsuite list-lib (libs))
(defsuite fns (libs))

(deftest quote (special-forms)
  (assert-eql-cl ^'x "'x ")
  (assert-equal-cl ^'(a b c) "'(a b c)"))

(deftest if (special-forms)
  (assert-eql-cl 2   "(if 1 2 3)")
  (assert-eql-cl 3   "(if '() 2 3)")
  (assert-eql-cl 2   "(if 1 2)")
  (assert-eql-cl nil "(if 1 '())"))

(deftest progn (special-forms)
  (assert-eql-cl 1 "(progn 1)")
  (assert-eql-cl 2 "(progn 1 2)")
  (assert-eql-cl 3 "(progn 1 2 3)"))

(deftest lambda (special-forms)
  (assert-eql-cl 10 "((lambda (x y) (+ x y)) 3 7)")
  (assert-eql-cl 20 "((lambda (x) (* 4 x)) 5)")
  (assert-equal-cl '(30 110 60 115)
		   "(let* ((acc-gen (lambda (n) (lambda (x) (setf n (+ x n)))))
                           (a (funcall acc-gen 10))
                           (b (funcall acc-gen 100)))
                       (cons (funcall a 20)
                         (cons (funcall b 10)
                           (cons (funcall a 30)
                             (cons (funcall b 5)
                                '())))))"))

(deftest + (arithmetic)
  (assert-eql-cl 5 "(+ 2 3)")
  (assert-eql-cl 10 "(+ 5 5)")
  (assert-eql-cl 15 "(+ 1 2 3 4 5)"))

(deftest - (arithmetic)
  (assert-eql-cl 10  "(- 15 5)")
  (assert-eql-cl 0   "(- 10 10)")
  (assert-eql-cl -5  "(- 10 15)")
  (assert-eql-cl -13 "(- 1 2 3 4 5)"))

(deftest * (arithmetic)
  (assert-eql-cl 20 "(* 5 4)")
  (assert-eql-cl 15 "(* 3 5)")
  (assert-eql-cl 24 "(* 1 2 3 4)"))

(deftest / (arithmetic)
  (assert-eql-cl 5/4 "(/ 5 4)")
  (assert-eql-cl 2   "(/ 10 5)")
  (assert-eql-cl 1/2 "(/ 6 4 3)"))

(deftest +-*/ (arithmetic)
  (assert-eql-cl 15 "(* 5 (+ 3 (/ 8 (- 2 6)) (+ 1 1)))"))

(deftest funcall (appliers)
  (assert-eql-cl 8 "(funcall (lambda (x y) (+ x y 5)) 1 2)")
  (assert-eql-cl 15 "(funcall (lambda (x) (+ x 5)) 10)")
  (assert-eql-cl 15 "(funcall #'+ 1 2 3 4 5)")
  (assert-eql-cl 15 "(funcall '+  1 2 3 4 5)"))

(deftest apply (appliers)
  (assert-eql-cl 8 "(apply (lambda (x y) (+ x y 5)) '(1 2))")
  (assert-eql-cl 120 "(apply (lambda (w x y z) (* w x y z))
                                       2 3 '(4 5))")
  (assert-eql-cl 15 "(apply #'+ '(1 2 3 4 5))")
  (assert-eql-cl 15 "(apply '+  '(1 2 3 4 5))")
  ;; Testing that apply keeps the dynamic environment.
  (assert-true-cl
   "(let ((foo (lambda () *gensym-counter*)))
      (let ((*gensym-counter* 100000000))
        (eql (apply foo '()) 100000000)))"))

(deftest eval (appliers)
  (assert-eql-cl 8 "(eval '(+ 3 5))")
  (assert-eql-cl 20 "(eval '(+ (* 5 3) 5))")
  (assert-equal-cl 10 "(eval (list '+ 1 2 3 4))"))

(deftest let (macros)
  (assert-eql-cl 5 "(let ((x 2) (y 3)) (+ x y))")
  (assert-eql-cl 10 "(let ((x 15)) (- x 5))"))

(deftest cond (macros)
  (assert-eql-cl 2 "(cond (1 2) (3 4))")
  (assert-eql-cl 4 "(cond ('() 2) (3 4))"))

(deftest car (list)
  (assert-eql-cl 1 "(let ((x (cons 1 (cons 2 '())))) (car x))")
  (assert-eql-cl '() "(let ((x '())) (car x))")
  (assert-eql-cl ^'a "(car '(a . b))"))

(deftest cdr (list)
  (assert-equal-cl '(2) "(let ((x (cons 1 (cons 2 '())))) (cdr x))")
  (assert-eql-cl '() "(let ((x '())) (car x))")
  (assert-eql-cl ^'b "(cdr '(a . b))"))

(deftest setq (setters)
  (assert-eql-cl 5  "(let ((x 10)) (setq x 5) x)")
  (assert-eql-cl 10 "(let ((y 15)) (setq y 10) y)"))

(deftest setf (setters)
  (assert-equal-cl 5 "(let ((x 10)) (setf x 5) x)")
  (assert-equal-cl '(1 10) "(let ((x 10)) (setf x '(1 10)))"))

(deftest setf-car (setters)
  (assert-equal-cl '(10 2 3) "(let ((x (cons 1 (cons 2 (cons 3 '()))))) (setf (car x) 10) x)")
  (assert-equal-cl '(1 10 3) "(let ((x (cons 1 (cons 2 (cons 3 '()))))) (setf (car (cdr x)) 10) x)"))

(deftest setf-cdr (setters)
  (assert-equal-cl '(1 4 5) "(let ((x (cons 1 (cons 2 (cons 3 '()))))) (setf (cdr x) '(4 5)) x)")
  (assert-equal-cl '((2 10) 4 5) "(let ((x (cons (cons 2 (cons 10 '())) (cons 4 (cons 5 '())))))
                                              (setf (cdr (car x)) '(10)) x)"))

(deftest < (predicates)
  (assert-true-cl  "(< 5)")
  (assert-true-cl  "(< 5 10)")
  (assert-false-cl "(< 10 5)")
  (assert-true-cl  "(< 1 2 3)")
  (assert-false-cl "(< 2 3 1)")
  (assert-false-cl "(< 3 2 1)")
  (assert-false-cl "(< 1 3 2)")
  (assert-false-cl "(< 1 1)"))

(deftest > (predicates)
  (assert-true-cl  "(> 5)")
  (assert-true-cl  "(> 10 5)")
  (assert-false-cl "(> 5 10)")
  (assert-true-cl  "(> 3 2 1)")
  (assert-false-cl "(> 2 3 1)")
  (assert-false-cl "(> 2 1 3)")
  (assert-false-cl "(> 1 2 3)")
  (assert-false-cl "(> 1 3 2)")
  (assert-false-cl "(> 1 1)"))

(deftest <= (predicates)
  (assert-true-cl  "(<= 5)")
  (assert-true-cl  "(<= 5 10)")
  (assert-false-cl "(<= 10 5)")
  (assert-true-cl  "(<= 1 2 3)")
  (assert-false-cl "(<= 2 3 1)")
  (assert-false-cl "(<= 3 2 1)")
  (assert-false-cl "(<= 1 3 2)")
  (assert-true-cl  "(<= 1 1)"))

(deftest >= (predicates)
  (assert-true-cl  "(>= 5)")
  (assert-true-cl  "(>= 10 5)")
  (assert-false-cl "(>= 5 10)")
  (assert-true-cl  "(>= 3 2 1)")
  (assert-false-cl "(>= 2 3 1)")
  (assert-false-cl "(>= 2 1 3)")
  (assert-false-cl "(>= 1 2 3)")
  (assert-false-cl "(>= 1 3 2)")
  (assert-true-cl  "(>= 1 1)"))

(deftest = (predicates)
  (assert-true-cl  "(= 1)")
  (assert-true-cl  "(= 1 1)")
  (assert-false-cl "(= 5 10)")
  (assert-true-cl  "(= 1 1 1)")
  (assert-false-cl "(= 1 1 2)")
  (assert-false-cl "(= 1 2 1)")
  (assert-false-cl "(= 2 1 1)"))

(deftest evenp (predicates)
  (assert-true-cl  "(evenp 2)")
  (assert-true-cl  "(evenp 4)")
  (assert-false-cl "(evenp 3)")
  (assert-false-cl "(evenp 5)"))

(deftest oddp (predicates)
  (assert-true-cl  "(oddp 3)")
  (assert-true-cl  "(oddp 5)")
  (assert-false-cl "(oddp 2)")
  (assert-false-cl "(oddp 4)"))

(deftest zerop (predicates)
  (assert-true-cl  "(zerop 0)")
  (assert-false-cl "(zerop (- 1))")
  (assert-false-cl "(zerop 1)"))

(deftest plusp (predicates)
  (assert-true-cl  "(plusp 1)")
  (assert-false-cl "(plusp 0)")
  (assert-false-cl "(plusp (- 1))"))

(deftest minusp (predicates)
  (assert-true-cl  "(minusp (- 1))")
  (assert-false-cl "(minusp 0)")
  (assert-false-cl "(minusp 1)"))

(deftest null (predicates)
  (assert-true-cl  "(null '())")
  (assert-false-cl "(null 1)")
  (assert-false-cl "(null 'x)"))

(deftest atom (predicates)
  (assert-true-cl  "(atom '())")
  (assert-true-cl  "(atom 'x)")
  (assert-true-cl  "(atom 1)")
  (assert-false-cl "(atom '(1 2 3))"))

(deftest and (logic-macros)
  (assert-true-cl   "(and)")
  (assert-false-cl  "(and '())")
  (assert-eql-cl 10 "(and 10)")
  (assert-eql-cl 5  "(and 10 5)")
  (assert-false-cl  "(and '() 10)")
  (assert-false-cl  "(and 10 '())")
  (assert-false-cl  "(and '() '())"))

(deftest or (logic-macros)
  (assert-false-cl  "(or)")
  (assert-eql-cl 10 "(or 10)")
  (assert-false-cl  "(or '())")
  (assert-eql-cl 5  "(or 5 10)")
  (assert-eql-cl 5  "(or 5 '())")
  (assert-eql-cl 10 "(or '() 10)")
  (assert-false-cl  "(or '() '())"))

(deftest member-if (hofs)
  (assert-false-cl "(member-if #'evenp '())")
  (assert-true-cl  "(member-if #'evenp '(5 6))")
  (assert-true-cl  "(member-if #'oddp '(6 5))")
  (assert-false-cl "(member-if #'oddp '(4 6 2))")
  (assert-equal-cl '(6 5)
		   "(member-if #'evenp '(3 6 5))"))

(deftest member (hofs)
  (assert-false-cl "(member 5 '())")
  (assert-true-cl  "(member 5 '(1 2 3 4 5 6))")
  (assert-true-cl  "(member 6 '(6))")
  (assert-false-cl "(member 6 '(1 2 3 4 5 7))")
  (assert-equal-cl '(4 5 6) "(member 4 '(1 2 3 4 5 6))"))

(deftest find-if (hofs)
  (assert-false-cl "(find-if #'evenp '())")
  (assert-false-cl "(find-if #'evenp '(1 3 5))")
  (assert-eql-cl 5 "(find-if #'oddp '(6 5 4))")
  (assert-eql-cl 4 "(find-if #'evenp '(1 3 4))"))

(deftest find (hofs)
  (assert-false-cl "(find 5 '())")
  (assert-false-cl "(find 10 '(1 2 3 4 5))")
  (assert-eql-cl 5 "(find 5 '(1 3 5 10))"))

(deftest mapcar (hofs)
  (assert-equal-cl '(t nil t) "(mapcar #'oddp '(1 2 3))")
  (assert-equal-cl '(1 4 9) "(mapcar (lambda (x) (* x x)) '(1 2 3))"))

(deftest list (list-lib)
  (assert-equal-cl '() "(list)")
  (assert-equal-cl '(1 2 3) "(list 1 2 3)")
  (assert-equal-cl '(1 2 3 4 5) "(apply #'list 1 2 '(3 4 5))"))

(deftest list* (list-lib)
  (assert-eql-cl ^'a "(list* 'a)")
  (assert-equal-cl '(1 2 3) "(list* 1 '(2 3))")
  (assert-equal-cl '(1 . 2) "(list* 1 2)")
  (assert-equal-cl '(1 2 3 4 5 6) "(list* 1 2 3 '(4 5 6))"))

(deftest nthcdr (list-lib)
  (assert-equal-cl '(1 2 3) "(nthcdr 0 '(1 2 3))")
  (assert-equal-cl '(2 3)   "(nthcdr 1 '(1 2 3))")
  (assert-equal-cl '(3)     "(nthcdr 2 '(1 2 3))")
  (assert-equal-cl '()      "(nthcdr 3 '(1 2 3))")
  (assert-equal-cl '()      "(nthcdr 4 '(1 2 3))"))

(deftest equal (list-lib)
  (assert-true-cl  "(equal '() '())")
  (assert-false-cl "(equal '() 5)")
  (assert-true-cl  "(equal '(1 2 3) (list 1 2 3))")
  (assert-false-cl "(equal '(1 2 3) '(1 2))")
  (assert-false-cl "(equal '(1 2 3) '(1 3 3))"))

(deftest length (list-lib)
  (assert-eql-cl 0 "(length '())")
  (assert-eql-cl 1 "(length '(a))")
  (assert-eql-cl 2 "(length '(1 2))")
  (assert-eql-cl 3 "(length '(1 2 3))"))

(deftest append (list-lib)
  (assert-equal-cl '() "(append)")
  (assert-equal-cl '() "(append '())")
  (assert-equal-cl '() "(append '() '() '())")
  (assert-equal-cl '(1 2 3) "(append '() '(1 2 3) '())")
  (assert-equal-cl '(1 2 3 4 5 6) "(append '(1 2 3) '(4 5 6))"))

(deftest last (list-lib)
  (assert-equal-cl '(5) "(last '(1 2 3 4 5))")
  (assert-equal-cl ^'(c) "(last '(a b c))")
  (assert-equal-cl ^'(d e f) "(last '(a b c d e f) 3)"))

(deftest butlast (list-lib)
  (assert-equal-cl '(1 2 3 4) "(butlast '(1 2 3 4 5))")
  (assert-equal-cl ^'(a b) "(butlast '(a b c))")
  (assert-equal-cl ^'(a b c) "(butlast '(a b c d e f) 3)"))

(deftest nconc (list-lib)
  (assert-equal-cl '((1 2 3 4 5 6 7 8 9)
		     (1 2 3 4 5 6 7 8 9)
		     (4 5 6 7 8 9)
		     (7 8 9))
		   "(let ((x (list 1 2 3)) (y (list 4 5 6)) (z (list 7 8 9)))
                      (list (nconc x y z) x y z))"))

(deftest make-symbol (symbols)
  (let ((sym (eval-string "(make-symbol \"X\")")))
    (assert-equalp "X" (cln::cl-symbol-name sym))
    (assert-equal nil (cln::cl-symbol-package sym))))

(deftest gensym (symbols)
  (let ((sym (eval-string "(gensym)")))
    (assert-equal nil (cln::cl-symbol-package sym))))

(deftest gentemp (symbols)
  (let ((sym (eval-string "(gentemp)")))
    (assert-equalp "COMMON-LISP"
	(cln::cl-package-name
	  (cln::cl-symbol-package sym)))))

(deftest string-reader (reader)
  (assert-equal-cl "" "\"\"")
  (assert-equal-cl "hello" "\"hello\""))

(deftest char-reader (reader)
  ;; Need to implement #\Space etc.
  (assert-eql-cl #\x "#\\x")
  (assert-eql-cl #\) "#\\)")
  (assert-eql-cl #\" "#\\\""))

(deftest eval-read (reader)
  (assert-equal '(1 2 3)
    (with-input-from-string (in "#.(list 1 2 3)")
      (cln::cl-read in)))
  (assert-eql 10
    (with-input-from-string (in "#.(+ 5 5)")
      (cln::cl-read in))))

(deftest identity (fns)
  (assert-eql-cl 5 "(identity 5)")
  (assert-eql-cl ^'hello "(identity 'hello)"))

(deftest constantly (fns)
  (assert-eql-cl 5 "(funcall (constantly 5) 1 2 3)")
  (assert-equal-cl '(1 1 1) "(mapcar (constantly 1) '(5 6 7))"))
