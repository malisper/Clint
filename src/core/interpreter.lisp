;; The core interpreter.

(in-package :clint)

(defun cl-eval (exp)
  "Evaluates EXP in ENV."
  (cond ((typep exp 'cl-symbol) (val exp))
	((atom exp) exp)
        ((switch (car exp)
           ^'quote    (cadr exp)
           ^'function (cl-function (cadr exp))
           ^'if       (cl-eval-if (cdr exp))
           ^'progn    (car (last (cl-eval-all (cdr exp))))
           ^'setq     (setf (val (cadr exp)) (cl-eval (caddr exp)))
           ;; Else
           (let ((x (cl-function (car exp))))
             (if (typep x 'macro)
                 (cl-eval (cl-apply (macro-fn x) (cdr exp)))
                 (cl-apply x (cl-eval-all (cdr exp)))))))))

(defun cl-eval-all (exps)
  "Evaluates all of the expression in EXPS in the given variable
   environment and function environment given. Returns a list of the
   results. The only reason a cl is prepended is because this uses
   cl-eval instead of the ICL eval."
  (mapcar (lambda (x) (cl-eval x)) exps))

(defun cl-eval-if (code)
  "Evaluates the code for an if expression in ENV and FENV. The
   argument CODE should be a list containing the code for the
   predicate, the code for the consequence, and optionally the code
   for the alternative. The only reason a cl is prepended is because
   this uses cl-eval instead of the ICL eval."
  (destructuring-bind (predicate consequence &optional alternative) code
    (if (cl-eval predicate)
        (cl-eval consequence)
        (when alternative
          (cl-eval alternative)))))

(defun lambdap (x)
  "Is this code for a lambda expression?"
  (and (listp x) (eq (car x) ^'lambda)))

(defun cl-function (exp)
  "Returns the function that EXP names. Works for either symbols or
   lists representing lambda functions."
  (if (lambdap exp)
      (make-instance 'lambda-fn
	:args (cadr exp)
	:code (add-progn (cddr exp))
	:env  *env*
	:fenv *fenv*)
      (val-fn exp)))

(defun add-progn (exps)
  "Adds a progn to a list of expressions."
  (cons ^'progn exps))

(defgeneric cl-apply (f args)
  (:documentation
   "Apply the Clint procedure F to the given arguments in the given
   environments."))

(defmethod cl-apply ((f prim-fn) args)
  "Apply the primitive procedure to the arguments."
  (with-global-env
    (apply (prim-code f) args)))

(defmethod cl-apply ((f lambda-fn) args)
  "Apply the Clint lambda function to the arguments."
  (with-extend-envs (fn-env f) (fn-fenv f) (fn-args f) args
    (cl-eval (fn-code f))))

(defmethod cl-apply ((f cl-symbol) args)
  "Apply a symbol as a function."
  (with-global-env
    (cl-apply (val-fn f) args)))
