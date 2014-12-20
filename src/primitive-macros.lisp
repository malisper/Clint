;; Primitive Clint macros.

(in-package :clint)

(defmacro defprimitive-macro (name args &body body)
  "Define a Clint macro whose body is evaluated by the ICL."
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

(defprimitive-macro defun (name args &rest body)
  "Define a procedure."
  ^`(progn ,(when (stringp (car body))
              `(setf (documentation ',name 'function) ,(car body)))
           (setf (symbol-function ',name) (lambda ,args ,@body))))

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
