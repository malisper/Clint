;; The core interpreter.

(in-package :clint)

(defvar *env* '() "The global variable environment.")
(defvar *fenv* '() "The global function environment.")

;; A reader macro to make typing in symbols for the interpreter
;; easier. It is basically equivalent to quote but interns all of
;; the symbols into the interpreter's CL package.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\^
    (lambda (stream char)
      (declare (ignore char))
      `(symbols->cl-symbols ',(read stream t nil t) "CL"))))

(defmacro top-eval (exp)
  "A version of cl-eval that is meant to be used for interaction.
   This one automatically converts all symbols to cl-symbols and
   only requires a single argument."
  `(cl-eval ^,exp *env* *fenv*))

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
                                (pack-name (subseq name 0 pos))
				(pos-back (position #\: name :from-end t))
				(sym-name (subseq name (+ pos-back 1))))
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
