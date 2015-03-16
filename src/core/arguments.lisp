;;;; Handles the binding of argument lists to the values. Some of
;;;; the binding methods are defined in arguments2 because they need
;;;; access the evaluator.

(in-package :clint)

(defun extend-envs (env denv args vals)
  "Extend a given environment. This works for both the variable
   environment and function environment."
  (loop for pair in (bind args vals :regular)
        if (cl-symbol-special (car pair))
          collect pair into dynamics
        else
          collect pair into lexicals
        finally (return (values (cons lexicals env)
                                (cons dynamics denv)))))

(defmacro with-extend-envs (env fenv args vals &body body)
  "Given a list of arguments and values, evaluate the body with the
   lexical and dynamic environments extended properly."
  (let ((new-env (gensym)) (new-denv (gensym)))
    `(multiple-value-bind (,new-env ,new-denv) (extend-envs ,env *denv* ,args ,vals)
       (let ((*env* ,new-env) (*fenv* ,fenv) (*denv* ,new-denv))
         ,@body))))

(defmacro with-global-env (&body body)
  "Evaluate body within the global lexical Clint environment."
  `(with-extend-envs *global-env* *global-fenv* '() '()
     ,@body))

(defparameter *lambda-list-keywords* ^'(&rest &optional &key))

(defgeneric bind (args vals kind)
  (:documentation "Bind the parameters of type KIND (optional, rest,
                   etc) to the given values."))

(defmethod bind (args vals (kind (eql :regular)))
  "Bind regular keyword arguments."
  (cond ((null args)
	 (if (null vals)
	     '()
	     (error "Too many values passed in.")))
	((member (car args) *lambda-list-keywords*)
	 (bind (cdr args) vals (car args)))
	((null vals)
	 (error "Not enough values passed in."))
	(:else (cons (list (car args) (car vals))
		     (bind (cdr args) (cdr vals) kind)))))

(defmethod bind (args vals (kind (eql ^'&rest)))
  "Bind a rest argument."
  (list (list (car args) vals)))
