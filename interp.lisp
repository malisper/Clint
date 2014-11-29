(defpackage :clint
  (:nicknames :cln :clint)
  (:use :cl))

(in-package :clint)

(defclass cl-symbol ()
  ((name    :initarg :name    :accessor cl-symbol-name)
   (package :initarg :package :accessor cl-symbol-package)))

(defmethod print-object ((obj cl-symbol) s)
  "Print a cl-symbol."
  (with-slots (name) obj
    (format s "~A" name)))

;; Right now the ^ reader macro causes interning of the symbol into
;; the current package as opposed to the package at compile-time.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\^
    (lambda (stream char)
      (declare (ignore char))
      `(symbols->cl-symbols ',(read stream t nil t)))))

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

(defun add-progn (exps)
  "Adds a progn to a list of expressions."
  ;; Remove use of cl-intern.
  (cons (cl-intern "PROGN") exps))

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

(defvar *package-created* nil
  "True if the *package* variable has been bound.")

;; The following is very messy code that needs to be cleaned up.
(let* ((cl-package (if *package-created*
		       (cadr (assoc (cl-intern "*PACKAGE*") *env*))
		       (make-hash-table :test #'equalp)))
       (cl-package-name (if *package-created*
			    (cl-intern "*PACKAGE*")
			    (make-instance 'cl-symbol
					   :name "*PACKAGE*"
					   :package cl-package))))
  (unless *package-created*
    (setf (gethash "*PACKAGE*" cl-package) cl-package-name)
    (push (list cl-package-name cl-package) *env*)
    (setf *package-created* t))
  (defun cl-intern (name &optional (package (get-val cl-package-name *env*)))
    "Interns a symbol in the interpreter in the given package."
    (or (gethash name package)
	(setf (gethash name package)
	      (make-instance 'cl-symbol :name name :package package)))))

(defun maptree (f tree)
  "Maps the procedure F over the tree TREE."
  (cond ((null tree) '())
	((atom tree) (funcall f tree))
	(:else (cons (maptree f (car tree))
		     (maptree f (cdr tree))))))

(defun symbols->cl-symbols (code)
  "Converts all symbols given to symbols for the interpreter."
    (maptree (lambda (x)
	       (if (typep x 'symbol)
		   (cl-intern (symbol-name x))
		   x))
	     code))

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

(defprimitive-macro let (bindings &rest exps)
  ;; Remove use of cl-intern.
  `((,^lambda ,(mapcar #'car bindings) ,@exps)
    ,@(mapcar #'cadr bindings)))

(defprimitive-macro cond (&rest clauses)
  ;; Remove use of cl-intern.
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
  ;; Remove use of cl-intern.
  `(,^function (,^lambda ,@body)))
