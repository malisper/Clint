(defparameter *env* '())
(defparameter *fenv* '())

(defun get-val (var env)
  "Looks up the value of the variable VAR in the enviornment ENV.
   This works for both the variable environment and the function
   environment."
  (cadr (assoc var env)))

(defun cl-eval (exp &optional (env *env*) (fenv *fenv*))
  "Evaluates EXP in ENV."
  (cond ((symbolp exp) (get-val exp env))
	((atom exp) exp)
        ((case (first exp)
	   (quote (cadr exp))
           (function (get-val (cadr exp) fenv))
	   (if (cl-eval-if (cdr exp) env))
	   (progn (car (last (mapcar (lambda (x) (cl-eval x env fenv))
				     (cdr exp)))))
	   (lambda (make-instance 'lambda-fn
		    :args (cadr exp)
		    :code (add-progn (cddr exp))
		    :env  env
                    :fenv fenv))
	   (t (cl-apply (if (lambdap (car exp))
                            (cl-eval (car exp) env fenv)
                            (get-val (car exp) fenv))
                (mapcar (lambda (x) (cl-eval x env fenv))
                        (cdr exp))))))))

(defun cl-eval-if (code env)
  "Evaluates the code for an if expression in ENV. The argument CODE
   should be the list containing the code for the predicate, the code
   for the consequence, and optionally the code for the alternative."
  (if (cl-eval (car code) env)
      (cl-eval (cadr code) env)
      (and (caddr code)
	   (cl-eval (caddr code) env))))

(defun lambdap (x)
  "Is this the code for a lambda expression?"
  (and (listp x) (eql (car x) 'lambda)))

(defclass lambda-fn ()
  ((args :initarg :args :accessor fn-args)
   (code :initarg :code :accessor fn-code)
   (env  :initarg :env  :accessor fn-env)
   (fenv :initarg :fenv :accessor fn-fenv)))

(defclass prim-fn ()
  ((fn :initarg :prim-code :accessor prim-code)))

(defun add-progn (exps)
  "Adds a progn to a list of expressions."
  (cons 'progn exps))

(defgeneric cl-apply (f args)
  (:documentation "Applies a fn to a list of arguments."))

(defmethod cl-apply ((f lambda-fn) args)
  "Applies a lambda-fn to the arguments."
  (cl-eval (fn-code f)
	   (extend-env (fn-env f) (fn-args f) args)
           (fn-fenv f)))

(defmethod cl-apply ((f prim-fn) args)
  "Applies a primitive fn to the arguments."
  (apply (prim-code f) args))

(defun extend-env (env fn-args args)
  "Extend a given environment. This works for both variable
   environments and function environments."
  (append (mapcar #'list fn-args args) env))

(defmacro defprimitive (name args &body body)
  "Define a primitive to be put in the interpreter."
  `(push (list ',name (make-instance 'prim-fn
                      :prim-code (lambda ,args ,@body)))
         *fenv*))

(defprimitive + (&rest args)
  (apply #'+ args))

(defprimitive - (&rest args)
  (apply #'- args))

(defprimitive * (&rest args)
  (apply #'* args))

(defprimitive / (&rest args)
  (apply #'/ args))

(defprimitive funcall (f &rest args)
  (cl-apply f args))

(defprimitive apply (f &rest args)
  (cl-apply f (apply #'list* args)))
