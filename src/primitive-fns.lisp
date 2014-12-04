;; The primitive fns.

(in-package :clint)

(defmacro defprimitive-fn (name args &body body)
  "Define a primitive to be put in the interpreter."
  `(setf (global-fn ^',name)
         (make-instance 'prim-fn
           :prim-code (lambda ,args ,@body))))

(defprimitive-fn + (&rest args)
  (apply #'+ args))

(defprimitive-fn - (&rest args)
  (apply #'- args))

(defprimitive-fn * (&rest args)
  (apply #'* args))

(defprimitive-fn / (&rest args)
  (apply #'/ args))

(defprimitive-fn funcall (f &rest args)
  (cl-apply f args))

(defprimitive-fn apply (f &rest args)
  (cl-apply f (apply #'list* args)))

(defprimitive-fn eval (exp)
  (cl-eval exp *env* *fenv*))

(defprimitive-fn package-name (pack)
  (cl-package-name pack))

(defprimitive-fn find-package (name)
  (cl-find-package name))

(defprimitive-fn intern (&rest args)
  (apply #'cl-intern args))

(defprimitive-fn make-package (name)
  (make-instance 'cl-package :name name))

(defprimitive-fn eq (x y)
  (eq x y))
