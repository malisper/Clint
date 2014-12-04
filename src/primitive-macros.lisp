;; The primitive macros.

(in-package :clint)

(defmacro defprimitive-macro (name args &body body)
  "Define a macro to be put in the interpreter."
  `(let* ((cl-sym ^',name)
	  (pair (assoc cl-sym *fenv*))
	  (fn (make-instance 'macro
		:macro-fn (make-instance 'prim-fn
			    :prim-code (lambda ,args ,@body)))))
     (if pair
	 (setf (cadr pair) fn)
	 (push (list cl-sym fn) *fenv*))))

(defprimitive-macro let (bindings &rest exps)
  ^`((lambda ,(mapcar #'car bindings) ,@exps)
     ,@(mapcar #'cadr bindings)))

(defprimitive-macro cond (&rest clauses)
  (cond ((null clauses) nil)
	((null (cdar clauses))
	 (let ((g (make-instance 'cl-symbol
		    :name (symbol-name (gensym))
		    :package nil)))
	   ^`(let ((,g ,(caar clauses)))
               (if ,g
                   ,g
                   (^'cond ,@(cdr clauses))))))
	(:else
	 ^`(if ,(caar clauses)
               ,(add-progn (cdar clauses))
               (cond ,@(cdr clauses))))))

(defprimitive-macro lambda (&rest body)
  ^`(function (lambda ,@body)))
