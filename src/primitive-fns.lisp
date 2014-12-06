;; The primitive fns.

(in-package :clint)

(defmacro defprimitive-fn (name args &body body)
  "Define a primitive to be put in the interpreter."
  `(progn
     ,(when (stringp (car body))
        `(setf (cl-doc ^',name ^'function) ,(car body)))
     (setf (global-fn ^',name)
           (make-instance 'prim-fn
             :prim-code (lambda ,args ,@body)))))

(defprimitive-fn + (&rest args)
  "Sums all of the arguments."
  (apply #'+ args))

(defprimitive-fn - (&rest args)
  "Subtracts every number after the first from the first."
  (apply #'- args))

(defprimitive-fn * (&rest args)
  "Multiplies all of the arguments."
  (apply #'* args))

(defprimitive-fn / (&rest args)
  "Div"
  (apply #'/ args))

(defprimitive-fn funcall (f &rest args)
  "Calls the procedure F on the arguments ARGS."
  (cl-apply f args))

(defprimitive-fn apply (f &rest args)
  "Applies the procedure F to every argument in ARGS with the last
   one being a list of more arguments."
  (cl-apply f (apply #'list* args)))

(defprimitive-fn eval (exp)
  "Evals a list as code."
  (cl-eval exp *env* *fenv*))

(defprimitive-fn package-name (pack)
  "Look up the name of the given package."
  (cl-package-name pack))

(defprimitive-fn find-package (name)
  "Returns the package that has the given name."
  (cl-find-package name))

(defprimitive-fn intern (&rest args)
  "Intern a string as a symbol into the given package."
  (apply #'cl-intern args))

(defprimitive-fn make-package (name)
  "Create a new package with the given name."
  (make-instance 'cl-package :name name))

(defprimitive-fn eq (x y)
  "Are these two things exact same object?"
  (eq x y))

(defprimitive-fn documentation (name type)
  "Look up the documentation for NAME under TYPE."
  (cl-doc name type))
