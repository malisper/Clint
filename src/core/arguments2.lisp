;;;; The rest of the parameter types are defined here.

(in-package :clint)

(defmethod bind (parms args (kind (eql ^'&optional)))
  "Bind optional arguments."
  (cond ((null parms)
	 (if (null args)
	     '()
	     (error "Too many values passed in.")))
	((member (car parms) *lambda-list-keywords*)
	 (bind (cdr parms) args (car parms)))
	(t
	 (destructuring-bind (var &optional default
				  (var-p (make-instance 'cl-symbol :name nil :package nil)))
	                     (mklist (car parms))
	   ;; Currently the expressions for optional arguments can
	   ;; only refer to values in the global environment. The only
	   ;; way I can think of to fix this is to pass the
	   ;; environments to bind.
	   (if (null args)
	       (list* (list var (cl-eval default))
		      (list var-p nil)
		      (bind (cdr parms) (cdr args) kind))
	       (list* (list var (car args))
		      (list var-p t)
		      (bind (cdr parms) (cdr args) kind)))))))

(defmethod bind (parms args (kind (eql ^'&key)))
  "Bind keyword arguments."
  (cond ((null parms) '())
	((member (car parms) *lambda-list-keywords*)
	 (bind (cdr parms) args (car parms)))
	(:else
          (destructuring-bind (var &optional default
                                         (var-p (make-instance 'cl-symbol :name nil :package nil)))
                              (mklist (car parms))
            (let ((pair (member (cl-intern (cl-symbol-name var) "KEYWORD")
                                args)))
              (if (not pair)
                  (list* (list var (cl-eval default))
                         (list var-p nil)
                         (bind (cdr parms) args ^'&key))
                  (list* (list var (cadr pair))
                         (list var-p t)
                         (bind (cdr parms) args ^'&key))))))))
