;; The core interpreter.

(in-package :clint)

;; A reader macro to make typing in symbols for the interpreter
;; easier. It will take the result of the form that comes after it (as
;; a tree) and convert all of the symbols in it to cl-symbols.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\^
    (lambda (stream char)
      (declare (ignore char))
      `(symbols->cl-symbols ,(read stream t nil t) "CL"))))

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
                 (cl-apply (cl-function (car exp) env fenv)
                           (cl-eval-all (cdr exp) env fenv)
                           env
                           fenv)))))))

(defun cl-eval-all (exps env fenv)
  "Evaluates all of EXPS in the variable environment and function
   environment given. Returns a list of the results."
  (mapcar (lambda (x) (cl-eval x env fenv)) exps))

(defun cl-eval-if (code env fenv)
  "Evaluates the code for an if expression in ENV. The argument CODE
   should be the list containing the code for the predicate, the code
   for the consequence, and optionally the code for the alternative."
  (destructuring-bind (predicate consequence &optional alternative) code
    (if (cl-eval predicate env fenv)
        (cl-eval consequence env fenv)
        (when alternative
          (cl-eval alternative env fenv)))))

(defun lambdap (x)
  "Is this the code for a lambda expression?"
  (and (listp x) (eq (car x) ^'lambda)))

(defun cl-function (exp env fenv)
  "Returns the function that EXP names."
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
  "Apply a function or symbol to the arguments in the variable
   environment ENV and function environment FENV."
  (etypecase f
    (prim-fn   (apply (prim-code f) args))
    (lambda-fn (cl-eval (fn-code f)
                        (extend-env (fn-env f) (fn-args f) args)
                        fenv))
    (symbol    (cl-apply (val f fenv) args env fenv))))

(defun symbols->cl-symbols (code &optional (package (global-var ^'*package*)))
  "Converts all symbols given to symbols for the interpreter."
    (maptree (lambda (x)
	       (if (typep x 'symbol)
                   (string->cl-symbol (symbol-name x) package)
                   x))
	     code))

(defun string->cl-symbol (str &optional (package (global-var ^'*package*)))
  "Takes a string and returns the cl-symbol it represents. If there is no
   package attached to the string, it is interned into PACKAGE."
  (if (not (find #\: str))
      (cl-intern str package)
      (let* ((pack-pos (position #\: str))
             (sym-pos  (position #\: str :from-end t)))
        (cl-intern (subseq str (+ sym-pos 1))
                   (subseq str 0 pack-pos)))))

(or (cl-find-package "CL") (make-instance 'cl-package :name "CL"))

(defun cl-intern (name &optional (designator (global-var ^'*package*)))
  "Interns a symbol in the interpreter in the given package."
  (let ((package (if (typep designator 'cl-package)
                     designator
                     (cl-find-package designator))))
    (with-slots (syms) package
      (or (gethash name syms)
          (setf (gethash name syms)
                (make-instance 'cl-symbol :name name :package package))))))

(setf (global-var ^'*package*) (cl-find-package "CL"))

(defmethod print-object :before ((sym cl-symbol) s)
  "Print the package of the symbol if it is not the current package."
  (let ((current-package (global-var ^'*package*)))
    (with-slots (package) sym
      (unless (eq package current-package)
        (format s "~A::" (cl-package-name package))))))
