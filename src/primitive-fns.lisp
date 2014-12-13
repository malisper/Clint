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
  "Divides all of the arguments after the first, from the first."
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

(defprimitive-fn (setf documentation) (val name type)
  "Sets the documentation for the thing named by NAME of type TYPE."
  (setf (cl-doc name type) val))

(defprimitive-fn cons (x y)
  "Return a pair containing the elements X and Y."
  (cons x y))

(defprimitive-fn car (x)
  "Return the first element of a pair."
  (car x))

(defprimitive-fn (setf car) (val pair)
  "Sets the car of a pair."
  (setf (car pair) val))

(defprimitive-fn cdr (x)
  "Return the second element of a pair."
  (cdr x))

(defprimitive-fn (setf cdr) (val pair)
  "Sets the cdr of a pair."
  (setf (cdr pair) val))

(defprimitive-fn rplacd (pair val)
  "Sets the cdr of PAIR to VAL."
  (rplacd pair val))

(defprimitive-fn rplaca (pair val)
  "Sets the car of a cons pair to VAL."
  (rplaca pair val))

(defprimitive-fn symbol-function (name)
  "Returns the global procedure named by NAME."
  (val name *fenv*))

(defprimitive-fn (setf symbol-function) (val name)
  "Set the global procedure named by NAME to VAL."
  (setf (val name *fenv*) val))

(defprimitive-fn macro-function (name)
  "Returns the macro definition for NAME."
  (let ((fn (val name *fenv*)))
    (if (typep fn 'macro)
        fn
        nil)))

(defprimitive-fn (setf macro-function) (val name)
  "Sets the macro fn for NAME."
  (setf (val name *fenv*) (make-instance 'macro
                            :macro-fn val)))

(defprimitive-fn load (file)
  "Evaluates every expression in FILE."
  (cl-load file))

(defprimitive-fn evenp (n)
  "Is this number even?"
  (evenp n))

(defprimitive-fn oddp (n)
  "Is this number odd?"
  (oddp n))

(defprimitive-fn zerop (n)
  "Is this number zero?"
  (zerop n))

(defprimitive-fn plusp (n)
  "Is this number positive?"
  (plusp n))

(defprimitive-fn minusp (n)
  "Is this number negative?"
  (minusp n))
