;; Primitive Clint procedures.

(in-package :clint)

(add-prim '+ "Sums all of the arguments.")
(add-prim '- "Subtracts every number after the first from the first.")
(add-prim '* "Multiplies all of the arguments.")
(add-prim '/ "Divides all of the arguments after the first, from the first.")
(add-prim 'exp "Raise e to the power of the argument.")
(add-prim 'expt "Raise the first argument to the power of the second argument.")
(add-prim 'sin "Take the sine of the argument.")
(add-prim 'cos "Take the cosine of the argument.")
(add-prim 'tan "Take the tangent of the argument.")

(defprimitive-fn funcall (f &rest args)
  "Calls the procedure F on the arguments ARGS."
  (cl-apply f args))

(defprimitive-fn apply (f &rest args)
  "Applies the procedure F to every argument in ARGS with the last
   one being a list of more arguments."
  (cl-apply f (apply #'list* args)))

(defprimitive-fn eval (exp)
  "Evals a list as code."
  (cl-eval exp))

(defprimitive-fn package-name (pack)
  "Look up the name of the given package."
  (cl-package-name pack))

(defprimitive-fn find-package (name)
  "Returns the package that has the given name."
  (cl-find-package name))

(defprimitive-fn intern (name &optional package)
  "Return a symbol in the given package that has the given name as
   its name. If no such symbol already exists, a new one is created."
  (cl-intern name package))

(defprimitive-fn make-package (name)
  "Create a new package with the given name."
  (make-instance 'cl-package :name name))

(defprimitive-fn package-nicknames (package)
  "Returns the nicknames for the given package."
  (cl-package-nicks package))

(add-prim 'eq "Are these two things exact same object?")
(add-prim 'eql
  "Are these things the same object or do they represent the same
   number?")

(defprimitive-fn documentation (name type)
  "Look up the documentation for NAME under TYPE."
  (cl-doc name type))

(defprimitive-fn (setf documentation) (val name type)
  "Sets the documentation for the thing named by NAME of type TYPE."
  (setf (cl-doc name type) val))

(add-prim 'cons "Returns a cons pair containing the elements X and Y.")

(define-cxrs 4)

(add-prim 'rplacd "Sets the cdr of PAIR to VAL.")
(add-prim 'rplaca "Sets the car of a cons pair to VAL.")

(defprimitive-fn fdefinition (name)
  "Returns the global procedure named by NAME."
  (val-fn name))

(defprimitive-fn (setf fdefinition) (val name)
  "Set the global procedure named by NAME to VAL."
  (setf (val-fn name) val))

(defprimitive-fn macro-function (name)
  "Returns the procedure used for macroexpansion of the macro named by
   NAME."
  (let ((fn (val-fn name)))
    (if (typep fn 'macro)
        fn
        nil)))

(defprimitive-fn (setf macro-function) (val name)
  "Sets the macro-function for NAME."
  (setf (val-fn name)
        (make-instance 'macro
                       :macro-fn val)))

;;; There is currently a problem with all of the predicates. They will
;;; return the ICL symbol t instead of the Clint symbol t. As to what
;;; effects this ultimately has, I am not sure. Everything should be
;;; fine as long as the return value for a predicate is not compared
;;; directly to the Clint symbol t.

(add-prim 'evenp "Is this number even?")
(add-prim 'oddp "Is this number odd?")
(add-prim 'zerop "Is this number zero?")
(add-prim 'plusp "Is this number positive?")
(add-prim 'minusp "Is this number negative?")

;; I'm going to have to figure out how to make the empty list and
;; nil act the same in all cases.
(add-prim 'null "Is this the empty list?")
(add-prim 'not "If the argument is non-nil, return nil. If it is nil, return t.")

;;; The following inequality predicates all have an extra arg outside
;;; of the rest argument because they cannot accept zero arguments.
(add-prim '< "Are the arguments strictly increasing?")
(add-prim '> "Are the arguments strictly decreasing?")
(add-prim '<= "Are the arguments non-strictly increasing.")
(add-prim '>= "Are the arguments non-strictly decreasing.")
(add-prim '= "Are the arguments numerically equivalent.")

(defprimitive-fn exit ()
  "Exit the Clint interpreter."
  (throw 'exit nil))

(defprimitive-fn set-macro-character (char fn &rest args)
  "Set the reader macro function for CHAR to FN."
  (apply #'cl-set-macro-character
	 char
	 (lambda (&rest args)
	   (cl-apply fn args))
	 args))

(defprimitive-fn set-dispatch-macro-character (char subchar fn &rest args)
  "Whenever the first two arguments (which are chars) are read
   together, call the third argument which should be a procedure."
  (apply #'cl-set-dispatch-macro-character
	 char
	 subchar
	 (lambda (&rest args)
	   (cl-apply fn args))
	 args))

(defprimitive-fn get (symbol indicator &optional default)
  "Look up the value of INDICATOR in SYMBOL's plist."
  (or (cadr (member indicator (cl-symbol-plist symbol)))
      default))

(defprimitive-fn (setf get) (val symbol indicator)
  "Set the value of INDICATOR in SYMBOLS's plist."
  (let ((plist (cl-symbol-plist symbol)))
    (if (member indicator plist)
	(setf (cadr (member indicator plist)) val)
	(progn (setf plist (list* indicator val plist))
	       val))))

(defprimitive-fn make-hash-table ()
  "Create a hash table."
  (make-hash-table))

(add-prim 'gethash "Look up a key in a hash table.")
(add-prim 'remhash "Remove the given key in the table.")
(add-prim 'atom "Is this an atom?")

(defprimitive-fn (setf gethash) (val key tab)
  "Set a value in a hash table."
  (setf (gethash key tab) val))

(defprimitive-fn symbol-name (x)
  "Return the name of this symbol."
  (cl-symbol-name x))

(defprimitive-fn symbol-package (x)
  "Return the package of this symbol."
  (cl-symbol-package x))

(defprimitive-fn export (syms &optional (package cl-*package*))
  "Export SYM from PACKAGE."
  (dolist (sym (mklist syms))
    (setf (gethash sym (cl-package-externals (cl-find-package package)))
	  t))
  t)

(defprimitive-fn import (syms &optional (package cl-*package*))
  (dolist (sym (mklist syms))
    (setf (gethash (cl-symbol-name sym) (package-syms (cl-find-package package)))
	  sym))
  t)
