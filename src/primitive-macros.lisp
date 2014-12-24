;; Primitive Clint macros.

(in-package :clint)

(defprimitive-macro let (bindings &rest exps)
  "The syntax is (let ((var val) ...) body). All of the variables
   will be bound to the respective values."
  ^`((lambda ,(mapcar #'car bindings) ,@exps)
     ,@(mapcar #'cadr bindings)))

(defprimitive-macro let* (bindings &rest exps)
  "Same thing as let but the variables are bound sequentially, not in
   parallel."
  (if (null bindings)
      ^`(progn ,@exps)
      ^`(let (,(car bindings)) (let* ,(cdr bindings) ,@exps))))

(defprimitive-macro cond (&rest clauses)
  "The syntax is (cond ((predicate consequence) ...)). Each predicate
   is evaluated until one returns non-nil. Then that predicates
   consequence is evaluated and that value is return as the result of
   the cond."
  (cond ((null clauses) nil)
        ;; If there is only a single part to a clause, we want to
        ;; return its value as the result of the cond if it is
        ;; non-nil.
	((null (cdar clauses))
	 (let ((g (cl-gensym)))
	   ^`(let ((,g ,(caar clauses)))
               (if ,g
                   ,g
                   (cond ,@(cdr clauses))))))
	(:else
	 ^`(if ,(caar clauses)
               ,(add-progn (cdar clauses))
               (cond ,@(cdr clauses))))))

(defprimitive-macro lambda (&rest body)
  "Wraps the lambda procedure with the function special form."
  ^`#'(lambda ,@body))

(defprimitive-macro setf (place val)
  "Set the value of PLACE to VAL."
  (multiple-value-bind (temps vals stores store-form access-form)
                       (cl-get-setf-expansion place)
    (declare (ignore access-form))
    ^`(let* (,@(mapcar #'list temps vals) (,(car stores) ,val))
        ,store-form)))

(defprimitive-macro push (val place)
  "Set PLACE to be a list whose car is VAL and whose cdr is the
   previous value of PLACE."
  (multiple-value-bind (temps vals stores store-form access-form)
                       (cl-get-setf-expansion place)
    ^`(let* (,@(mapcar #'list temps vals)
	     (,(car stores) (cons ,val ,access-form)))
	,store-form)))

(defprimitive-macro pop (place)
  "Sets PlACE to be the cdr of PLACE and return the first element."
  (multiple-value-bind (temps vals stores store-fun access-form)
                       (cl-get-setf-expansion place)
    (let ((val (gensym)))
      ^`(let* (,@(mapcar #'list temps vals)
	       (,val ,access-form)
	       (,(car stores) (cdr ,val)))
	  ,store-fun
	  (car ,val)))))

(defprimitive-macro defun (name args &rest body)
  "Define a procedure."
  ^`(progn ,(when (stringp (car body))
              `(setf (documentation ',name 'function) ,(car body)))
           (setf (fdefinition ',name) (lambda ,args ,@body))))

(defprimitive-macro defmacro (name args &rest body)
  "Define a macro."
  ^`(progn ,(when (stringp (car body))
              `(setf (documentation ',name 'function) ,(car body)))
           (setf (macro-function ',name) (lambda ,args ,@body))))

(defprimitive-macro and (&rest exps)
  "Evaluate each expression lazily. If any of them returns nil,
   the result is nil, otherwise return the value of the last
   expression."
  (cond ((null exps) ^'t)
        ((null (cdr exps)) (car exps))
        (:else ^`(if ,(car exps) (and ,@(cdr exps))))))

(defprimitive-macro or (&rest exps)
  "Evaluate each expression lazily. If any of them returns non-nil
   return its value, otherwise if none of them return non-nil, return
   nil."
  ^`(cond ,@(mapcar #'list exps)))

(defprimitive-macro prog1 (first &rest exps)
  "Evaluate each expression sequentially and return the value of the
   first one."
  (let ((g (cl-gensym)))
    ^`(let ((,g ,first))
	,@exps
	,g)))

(defprimitive-macro prog2 (first second &rest exps)
  "Evaluate each expression sequentially and return the value of the
   second one."
  ^`(progn ,first
	   (prog1 ,second ,@exps)))
