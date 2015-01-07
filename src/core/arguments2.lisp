;;;; The rest of the parameter types are defined here.

(in-package :clint)

(defmethod bind (args vals (kind (eql ^'&optional)))
  "Bind optional arguments."
  (cond ((null args)
	 (if (null vals)
	     '()
	     (error "Too many values passed in.")))
	((member (car args) *lambda-list-keywords*)
	 (bind (cdr args) vals (car args)))
	((null vals)
	 (cons (if (atom (car args))
		   (list (car args) '())
		   ;; Currently the expressions for optional
		   ;; arguments can only refer to values in the
		   ;; global environment. The only way I can think of
		   ;; to fix this is to pass the environments to bind.
		   (list (caar args) (cl-eval (cadar args) *env* *fenv* *denv*)))
	       (bind (cdr args) '() kind)))
	(:else (cons (list (if (atom (car args))
			       (car args)
			       (caar args))
			   (car vals))
		     (bind (cdr args) (cdr vals) kind)))))
