;;;; Handles the binding of argument lists to the values. Some of
;;;; the binding methods are defined in arguments2 because they need
;;;; access the evaluator.

(in-package :clint)

(defun extend-env (env args vals)
  "Extend a given environment. This works for both the variable
   environment and function environment."
  (cons (bind args vals ^'regular) env))

(defparameter *lambda-list-keywords* ^'(&rest &optional))

(defgeneric bind (args vals kind)
  (:documentation "Bind the parameters of type KIND (optional, rest,
                   etc) to the given values."))

(defmethod bind (args vals (kind (eql ^'regular)))
  "Bind regular keyword arguments."
  (cond ((null args)
	 (if (null vals)
	     '()
	     (error "Too many values passed in.")))
	((member (car args) *lambda-list-keywords*)
	 (bind (cdr args) vals (car args)))
	((null vals)
	 (error "Not enough values passed in."))
	(:else (cons (list (car args) (car vals))
		     (bind (cdr args) (cdr vals) kind)))))

(defmethod bind (args vals (kind (eql ^'&rest)))
  "Bind a rest argument."
  (list (list (car args) vals)))
