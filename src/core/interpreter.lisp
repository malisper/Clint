;; The core interpreter.

(in-package :clint)

(defun top-eval (exp)
  "Evaluate an expression from the top-level."
  (cl-eval exp *env* *fenv* *denv*))

(defun cl-eval (exp env fenv denv)
  "Evaluates EXP in ENV."
  (cond ((typep exp 'cl-symbol) (val exp env denv))
	((atom exp) exp)
        ((switch (car exp)
           ^'quote    (cadr exp)
           ^'function (cl-function (cadr exp) env fenv)
           ^'if       (cl-eval-if (cdr exp) env fenv denv)
           ^'progn    (car (last (cl-eval-all (cdr exp) env fenv denv)))
           ^'setq     (setf (val (cadr exp) env denv) (cl-eval (caddr exp) env fenv denv))
           ;; Else
           (let ((x (cl-function (car exp) env fenv)))
             (if (typep x 'macro)
                 (cl-eval (cl-apply (macro-fn x) (cdr exp)) env fenv denv)
                 (cl-apply x
                           (cl-eval-all (cdr exp) env fenv denv)
                           env
                           fenv
                           denv)))))))

(defun cl-eval-all (exps env fenv denv)
  "Evaluates all of the expression in EXPS in the given variable
   environment and function environment given. Returns a list of the
   results. The only reason a cl is prepended is because this uses
   cl-eval instead of the ICL eval."
  (mapcar (lambda (x) (cl-eval x env fenv denv)) exps))

(defun cl-eval-if (code env fenv denv)
  "Evaluates the code for an if expression in ENV and FENV. The
   argument CODE should be a list containing the code for the
   predicate, the code for the consequence, and optionally the code
   for the alternative. The only reason a cl is prepended is because
   this uses cl-eval instead of the ICL eval."
  (destructuring-bind (predicate consequence &optional alternative) code
    (if (cl-eval predicate env fenv denv)
        (cl-eval consequence env fenv denv)
        (when alternative
          (cl-eval alternative env fenv denv)))))

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

(defun cl-apply (f args &optional (env *env*) (fenv *fenv*) (denv *denv*))
  "Apply a function or symbol to the arguments, in the variable
   environment ENV and function environment FENV."
  (etypecase f
    (prim-fn   (apply (prim-code f) args))
    (lambda-fn (multiple-value-bind
                     (new-env new-denv)
                     (extend-envs (fn-env f) denv (fn-args f) args)
                 (cl-eval (fn-code f) new-env (fn-fenv f) new-denv)))
    (cl-symbol (cl-apply (val f fenv) args env fenv))))
