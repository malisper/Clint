;; The primitive macros.

(in-package :clint)

(defmacro defprimitive-macro (name args &body body)
  "Define a macro to be put in the interpreter."
  `(progn
     ,(when (stringp (car body))
        `(setf (cl-doc ^',name ^'function) ,(car body)))
     (setf (global-fn ^',name)
           (make-instance 'macro
             :macro-fn (make-instance 'prim-fn
                         :prim-code (lambda ,args ,@body))))))

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
	((null (cdar clauses))
	 (let ((g (make-instance 'cl-symbol
		    :name (symbol-name (gensym))
		    :package nil)))
	   ^`(let ((,g ,(caar clauses)))
               (if ,g
                   ,g
                   (cond ,@(cdr clauses))))))
	(:else
	 ^`(if ,(caar clauses)
               ,(add-progn (cdar clauses))
               (cond ,@(cdr clauses))))))

(defprimitive-macro lambda (&rest body)
  "Define a lambda procedure."
  ^`#'(lambda ,@body))
