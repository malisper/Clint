;; The core interpreter.

(in-package :clint)

(defun cl-eval (exp env fenv)
  "Evaluates EXP in ENV."
  (cond ((typep exp 'cl-symbol) (val exp env))
	((atom exp) exp)
        ((switch (car exp)
           ^'quote    (cadr exp)
           ^'function (cl-function (cadr exp) env fenv)
           ^'if       (cl-eval-if (cdr exp) env fenv)
           ^'progn    (car (last (cl-eval-all (cdr exp) env fenv)))
           ^'setq     (setf (val (cadr exp) env) (cl-eval (caddr exp) env fenv))
           ;; Else
           (let ((x (cl-function (car exp) env fenv)))
             (if (typep x 'macro)
                 (cl-eval (cl-apply (macro-fn x) (cdr exp)) env fenv)
                 (cl-apply x
                           (cl-eval-all (cdr exp) env fenv)
                           env
                           fenv)))))))

(defun cl-eval-all (exps env fenv)
  "Evaluates all of the expression in EXPS in the given variable
   environment and function environment given. Returns a list of the
   results. The only reason a cl is prepended is because this uses
   cl-eval instead of the ICL eval."
  (mapcar (lambda (x) (cl-eval x env fenv)) exps))

(defun cl-eval-if (code env fenv)
  "Evaluates the code for an if expression in ENV and FENV. The
   argument CODE should be a list containing the code for the
   predicate, the code for the consequence, and optionally the code
   for the alternative. The only reason a cl is prepended is because
   this uses cl-eval instead of the ICL eval."
  (destructuring-bind (predicate consequence &optional alternative) code
    (if (cl-eval predicate env fenv)
        (cl-eval consequence env fenv)
        (when alternative
          (cl-eval alternative env fenv)))))

(defun lambdap (x)
  "Is this code for a lambda expression?"
  (and (listp x) (eq (car x) ^'lambda)))

(defun cl-function (exp env fenv)
  "Returns the function that EXP names. Works for either symbols or
   lists representing lambda functions."
  (if (lambdap exp)
      (make-instance 'lambda-fn
	:args (cadr exp)
	:code (add-progn (cddr exp))
	:env  env
	:fenv fenv)
      (val exp fenv)))

(defun add-progn (exps)
  "Adds a progn to a list of expressions."
  (cons ^'progn exps))

(defun cl-apply (f args &optional (env *env*) (fenv *fenv*))
  "Apply a function or symbol to the arguments, in the variable
   environment ENV and function environment FENV."
  (etypecase f
    (prim-fn   (apply (prim-code f) args))
    (lambda-fn (cl-eval (fn-code f)
                        (extend-env (fn-env f) (fn-args f) args)
                        fenv))
    (symbol    (cl-apply (val f fenv) args env fenv))))

;; The procedure global-var needs to be used here because
;; cl-defparameter using the ^ reader macro which in turn uses
;; symbols->cl-symbols.
(defun symbols->cl-symbols (code &optional (package (global-var ^'*package*)))
  "Converts all symbols in the tree CODE, into Clint symbols. These
   symbols will belong to PACKAGE which defaults to the current value
   of the Clint variable *package*."
  (maptree (lambda (x)
             (if (typep x 'symbol)
                 (string->cl-symbol (symbol-name x) package)
                 x))
           code))

(defun string->cl-symbol (str &optional (package (global-var ^'*package*)))
  "Takes a string and returns the Clint symbol it represents. If there
   is no package attached to the string, it is interned into the
   Clint package PACKAGE."
  (if (not (find #\: str))
      (cl-intern str package)
      (let* ((pack-pos (position #\: str))
             (sym-pos  (position #\: str :from-end t)))
        (cl-intern (subseq str (+ sym-pos 1))
                   (subseq str 0 pack-pos)))))

;; This creates Clint's CL package if it does not already exist. This
;; line is early one because it is needed for cl-find-package.
(or (cl-find-package "CL") (make-instance 'cl-package :name "CL"))

(defun cl-intern (name &optional (designator (global-var ^'*package*)))
  "Interns a Clint symbol in the given Clint package."
  (let ((package (if (typep designator 'cl-package)
                     designator
                     (cl-find-package designator))))
    (with-slots (syms) package
      (or (gethash name syms)
          (setf (gethash name syms)
                (make-instance 'cl-symbol :name name :package package))))))

(cl-defparameter *package* *cl-package*
  (or (cl-find-package "CL") (make-instance 'cl-package :name "CL"))
  "The current clint-package.")

(defmethod print-object :before ((sym cl-symbol) s)
  "When printing a Clint symbol, if the symbols' package is not the
   same as the current package, prepend the package name to it."
  (let ((current-package *cl-package*))
    (with-slots (package) sym
      (if package
          (unless (eq package current-package)
            (format s "~A::" (cl-package-name package)))
          (format s "#:")))))

;; Define some global constants.
(cl-defparameter t *cl-t* ^'t "The truth value.")

;; There are some problems with making the Clint symbol nil and the
;; empty list the same thing.

;; (cl-defparameter nil *cl-nil* '() "The value of false and the empty list")


