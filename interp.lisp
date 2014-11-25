(defvar *env* '() "The global variable environment.")
(defvar *fenv* '() "The global function environment.")

(defun get-val (var env)
  "Looks up the value of the variable VAR in the enviornment ENV.
   This works for both the variable environment and the function
   environment."
  (cadr (assoc var env)))

(defun cl-eval (exp env fenv)
  "Evaluates EXP in ENV."
  (cond ((symbolp exp) (get-val exp env))
	((atom exp) exp)
        ((case (first exp)
	   (quote (cadr exp))
           (function (cl-function (cadr exp) env fenv))
	   (if (cl-eval-if (cdr exp) env fenv))
	   (progn (car (last (mapcar (lambda (x) (cl-eval x env fenv))
				     (cdr exp)))))
	   (lambda (make-instance 'lambda-fn
		    :args (cadr exp)
		    :code (add-progn (cddr exp))
		    :env  env
                    :fenv fenv))
	   (t (cl-apply (cl-function (car exp) env fenv)
                (mapcar (lambda (x) (cl-eval x env fenv))
                        (cdr exp))
                env
                fenv))))))

(defun cl-eval-if (code env fenv)
  "Evaluates the code for an if expression in ENV. The argument CODE
   should be the list containing the code for the predicate, the code
   for the consequence, and optionally the code for the alternative."
  (if (cl-eval (car code) env fenv)
      (cl-eval (cadr code) env fenv)
      (and (caddr code)
	   (cl-eval (caddr code) env fenv))))

(defun lambdap (x)
  "Is this the code for a lambda expression?"
  (and (listp x) (eql (car x) 'lambda)))

(defun cl-function (exp env fenv)
  "Returns the function that EXP names."
  (if (lambdap exp)
      (cl-eval exp env fenv)
      (get-val exp fenv)))

(defclass fn () ())

(defclass lambda-fn (fn)
  ((args :initarg :args :accessor fn-args)
   (code :initarg :code :accessor fn-code)
   (env  :initarg :env  :accessor fn-env)
   (fenv :initarg :fenv :accessor fn-fenv)))

(defclass prim-fn (fn)
  ((fn :initarg :prim-code :accessor prim-code)))

(defun add-progn (exps)
  "Adds a progn to a list of expressions."
  (cons 'progn exps))

(defun cl-apply (f args &optional (env *env*) (fenv *fenv*))
  "Apply a function or symbol to the arguments in the variable
   environment ENV and function environment FENV."
  (etypecase f
    (prim-fn   (apply (prim-code f) args))
    (lambda-fn (cl-eval (fn-code f)
                        (extend-env env (fn-args f) args)
                        fenv))
    (symbol    (cl-apply (get-val f fenv) args env fenv))))

(defun extend-env (env fn-args args)
  "Extend a given environment. This works for both variable
   environments and function environments."
  (append (mapcar #'list fn-args args) env))

(defmacro defprimitive (name args &body body)
  "Define a primitive to be put in the interpreter."
  `(let ((pair (assoc ',name *fenv*))
         (fn (make-instance 'prim-fn :prim-code (lambda ,args ,@body))))
     (if pair
         (setf (cadr pair) fn)
         (push (list ',name fn) *fenv*))))

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

(defprimitive eval (exp)
  (cl-eval exp *env* *fenv*))
