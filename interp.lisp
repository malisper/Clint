(defpackage :clint
  (:nicknames :cln :clint)
  (:use :cl))

(in-package :clint)

(defclass cl-symbol ()
  ((name    :initarg :name    :accessor cl-symbol-name)
   (package :initarg :package :accessor cl-symbol-package)))

(defclass cl-package ()
  ((name :initarg :name :accessor cl-package-name)
   ;; I feel like there should be some other name for this per the standard.
   (syms :initarg :syms :accessor package-syms :initform (make-hash-table :test #'equalp))))

(defmethod print-object ((obj cl-symbol) s)
  "Print a cl-symbol."
  (with-slots (name package) obj
    (format s "~A::~A" (cl-package-name package) name)))

(defclass fn () ())

(defclass lambda-fn (fn)
  ((args :initarg :args :accessor fn-args)
   (code :initarg :code :accessor fn-code)
   (env  :initarg :env  :accessor fn-env)
   (fenv :initarg :fenv :accessor fn-fenv)))

(defclass macro (fn)
  ((fn :initarg :macro-fn :accessor macro-fn)))

(defclass prim-fn (fn)
  ((prim-code :initarg :prim-code :accessor prim-code)))

;; Right now the ^ reader macro causes interning of the symbol into
;; the current package as opposed to the package at compile-time.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\^
    (lambda (stream char)
      (declare (ignore char))
      `(symbols->cl-symbols ',(read stream t nil t) "CL"))))

(defvar *env* '() "The global variable environment.")
(defvar *fenv* '() "The global function environment.")

(defmacro top-eval (exp)
  "A version of cl-eval that is meant to be used for interaction.
   This one automatically converts all symbols to cl-symbols and
   only requires a single argument."
  `(cl-eval (symbols->cl-symbols ',exp) *env* *fenv*))

(defun cl-eval (exp env fenv)
  "Evaluates EXP in ENV."
  (cond ((typep exp 'cl-symbol) (get-val exp env))
	((atom exp) exp)
        ((case (and (typep (car exp) 'cl-symbol) (intern (cl-symbol-name (car exp)))) ; A hack I want to get rid of.
	   (quote (cadr exp))
           (function (cl-function (cadr exp) env fenv))
	   (if (cl-eval-if (cdr exp) env fenv))
	   (progn (car (last (cl-eval-all (cdr exp) env fenv))))
	   (t (let ((x (cl-function (car exp) env fenv)))
		(if (typep x 'macro)
		    (cl-eval (cl-apply (macro-fn x) (cdr exp)) env fenv)
		    (cl-apply (cl-function (car exp) env fenv)
			      (cl-eval-all (cdr exp) env fenv)
			      env
			      fenv))))))))

(defun get-val (var env)
  "Looks up the value of the variable VAR in the enviornment ENV.
   This works for both the variable environment and the function
   environment."
  (let ((pair (assoc var env :test #'eq)))
    (if pair
	(cadr pair)
	(error "Unbound variable or procedure ~A." var))))

(defun cl-eval-all (exps env fenv)
  "Evaluates all of EXPS in the variable environment and function
   environment given. Returns a list of the results."
  (mapcar (lambda (x) (cl-eval x env fenv)) exps))

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
  (and (listp x) (eq (car x) ^lambda)))

(defun cl-function (exp env fenv)
  "Returns the function that EXP names."
  (if (lambdap exp)
      (make-instance 'lambda-fn
	:args (cadr exp)
	:code (add-progn (cddr exp))
	:env  env
	:fenv fenv)
      (get-val exp fenv)))

(defun add-progn (exps)
  "Adds a progn to a list of expressions."
  (cons ^progn exps))

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

(defmacro defprimitive-fn (name args &body body)
  "Define a primitive to be put in the interpreter."
  `(let* ((cl-sym ^,name)
	  (pair (assoc cl-sym *fenv*))
	  (fn (make-instance 'prim-fn :prim-code (lambda ,args ,@body))))
     (if pair
         (setf (cadr pair) fn)
         (push (list cl-sym fn) *fenv*))))

(defvar *packages* (make-hash-table :test #'equalp)
  "A hash-table containing all of the packages. Currently indexed
   by the name as a string.")

(defmethod initialize-instance :after ((pack cl-package) &key)
  "Adds this package to the *packages* global variable."
  (setf (gethash (cl-package-name pack) *packages*) pack))

(defun cl-find-package (name)
  "Returns the package of the given name."
  (gethash name *packages*))

(defun maptree (f tree)
  "Maps the procedure F over the tree TREE."
  (cond ((null tree) '())
	((atom tree) (funcall f tree))
	(:else (cons (maptree f (car tree))
		     (maptree f (cdr tree))))))

(defun symbols->cl-symbols (code &optional (package (get-val (cl-intern "*PACKAGE*" "CL") *env*)))
  "Converts all symbols given to symbols for the interpreter."
    (maptree (lambda (x)
	       (if (typep x 'symbol)
		   (let ((name (format nil "~W" x)))
		     (if (find #\: name)
			 (let* ((pos (position #\: name))
				(sym-name (subseq name (+ pos 1)))
				(pack-name (subseq name 0 pos)))
			   (cl-intern sym-name pack-name))
			 (cl-intern name package)))
		   x))
	     code))

(let* ((cl-package (or (cl-find-package "CL")
		       (make-instance 'cl-package :name "CL"))))
  (defun cl-intern (name &optional (package-designator (get-val ^*package* *env*)))
    "Interns a symbol in the interpreter in the given package."
    (let ((package (if (typep package-designator 'cl-package)
		       package-designator
		       (cl-find-package package-designator))))
      (with-slots (syms) package
	(or (gethash name syms)
	    (setf (gethash name syms)
		  (make-instance 'cl-symbol :name name :package package))))))
  (with-slots (syms) cl-package
    (pushnew (list ^*package* cl-package) *env* :test (lambda (x y) (eq (car x) (car y))))))

(defmacro defprimitive-macro (name args &body body)
  "Define a macro to be put in the interpreter."
  `(let* ((cl-sym ^,name)
	  (pair (assoc cl-sym *fenv*))
	  (fn (make-instance 'macro
		:macro-fn (make-instance 'prim-fn
			    :prim-code (lambda ,args ,@body)))))
     (if pair
	 (setf (cadr pair) fn)
	 (push (list cl-sym fn) *fenv*))))

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

(defprimitive-macro let (bindings &rest exps)
  `((,^lambda ,(mapcar #'car bindings) ,@exps)
    ,@(mapcar #'cadr bindings)))

(defprimitive-macro cond (&rest clauses)
  (cond ((null clauses) nil)
	((null (cdar clauses))
	 (let ((g (make-instance 'cl-symbol
		    :name (symbol-name (gensym))
		    :package nil)))
	   `(,^let ((,g ,(caar clauses)))
	      (,^if ,g
		    ,g
		    (,^cond ,@(cdr clauses))))))
	(:else
	 `(,^if ,(caar clauses)
		,(add-progn (cdar clauses))
		(,^cond ,@(cdr clauses))))))

(defprimitive-macro lambda (&rest body)
  `(,^function (,^lambda ,@body)))
