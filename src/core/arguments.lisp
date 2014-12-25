;;;; Handles the binding of argument lists to the values.

(in-package :clint)

(defun extend-env (env args vals)
  "Extend a given environment. This works for both the variable
   environment and function environment."
  (cons (bind args vals ^'regular) env))

(defparameter *lambda-list-keywords* ^'(&rest))

(defgeneric bind (args vals kind)
  (:documentation "Bind the parameters of type KIND (optional, rest,
                   etc) to the given values."))

(defmethod bind (args vals (kind (eql ^'regular)))
  "Bind regular keyword arguments."
  (cond ((null args)
	 (unless (null vals)
	   (error "Too many arguments.")))
	((null vals)
	 (error "Not enough arguments."))
	((member (car args) *lambda-list-keywords*)
	 (bind (cdr args) vals (car args)))
	(:else (cons (list (car args) (car vals))
		     (bind (cdr args) (cdr vals) kind)))))

(defmethod bind (args vals (kind (eql ^'&rest)))
  "Bind a rest argument."
  (list (list (car args) vals)))
