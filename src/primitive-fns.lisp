;; Primitive Clint procedures.

(in-package :clint)

(defmacro defprimitive-fn (name args &body body)
  "Define a Clint procedure whose body is evaluated by the ICL."
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
  "Return a symbol in the given package that has the given name as
   its name. If no such symbol already exists, a new one is created."
  (apply #'cl-intern args))

(defprimitive-fn make-package (name)
  "Create a new package with the given name."
  (make-instance 'cl-package :name name))

(defprimitive-fn eq (x y)
  "Are these two things exact same object?"
  (eq x y))

(defprimitive-fn eql (x y)
  "Are these things the same object or do they represent the same
   number?"
  (eql x y))

(defprimitive-fn documentation (name type)
  "Look up the documentation for NAME under TYPE."
  (cl-doc name type))

(defprimitive-fn (setf documentation) (val name type)
  "Sets the documentation for the thing named by NAME of type TYPE."
  (setf (cl-doc name type) val))

(defprimitive-fn cons (x y)
  "Returns a cons pair containing the elements X and Y."
  (cons x y))

(defprimitive-fn car (x)
  "Returns the first element of a pair."
  (car x))

(defprimitive-fn (setf car) (val pair)
  "Sets the car of a pair."
  (setf (car pair) val))

(defprimitive-fn cdr (x)
  "Returns the second element of a pair."
  (cdr x))

(defprimitive-fn (setf cdr) (val pair)
  "Sets the cdr of a cons pair."
  (setf (cdr pair) val))

(defprimitive-fn rplacd (pair val)
  "Sets the cdr of PAIR to VAL."
  (rplacd pair val))

(defprimitive-fn rplaca (pair val)
  "Sets the car of a cons pair to VAL."
  (rplaca pair val))

(defprimitive-fn symbol-function (name)
  "Returns the global procedure named by NAME."
  (global-fn name))

(defprimitive-fn (setf symbol-function) (val name)
  "Set the global procedure named by NAME to VAL."
  (setf (global-fn name) val))

(defprimitive-fn macro-function (name)
  "Returns the procedure used for macroexpansion of the macro named by
   NAME."
  (let ((fn (val name *fenv*)))
    (if (typep fn 'macro)
        fn
        nil)))

(defprimitive-fn (setf macro-function) (val name)
  "Sets the macro-function for NAME."
  (setf (val name *fenv*) (make-instance 'macro
                            :macro-fn val)))

(defprimitive-fn load (file)
  "Evaluates every expression in the given file."
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

;; There is currently a problem with all of the predicates. They will
;; return the ICL symbol t instead of the Clint symbol t. As to what
;; effects this ultimately has, I am not sure. Everything should be
;; fine as long as the return value for a predicate is not compared
;; directly to the Clint symbol t.

(defprimitive-fn null (x)
  "Is this the empty list?"
  ;; I'm going to have to figure out how to make the empty list and
  ;; nil act the same in all cases.
  (null x))

(defprimitive-fn not (x)
  "If the argument is non-nil, return nil. If it is nil, return t."
  (not x))

;;; The following inequality predicates all have an extra arg outside
;;; of the rest argument because they cannot accept zero arguments.
(defprimitive-fn < (arg &rest args)
  "Are the arguments strictly increasing?"
  (apply #'< arg args))

(defprimitive-fn > (arg &rest args)
  "Are the arguments strictly decreasing?"
  (apply #'> arg args))

(defprimitive-fn <= (arg &rest args)
  "Are the arguments non-strictly increasing."
  (apply #'<= arg args))

(defprimitive-fn >= (arg &rest args)
  "Are the arguments non-strictly decreasing."
  (apply #'>= arg args))

(defprimitive-fn = (arg &rest args)
  "Are the arguments numerically equivalent."
  (apply #'= arg args))

(defprimitive-fn exit ()
  "Exit the Clint interpreter."
  (throw 'exit nil))

(defprimitive-fn make-symbol (name)
  "Create a new symbol with the given name."
  (cl-make-symbol name))

(defprimitive-fn gensym (&optional (name "G"))
  "Create a new symbol that is not eq with any other symbol."
  (cl-gensym name))
