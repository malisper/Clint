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
		   (list (caar args) (cl-eval (cadar args))))
	       (bind (cdr args) '() kind)))
	(:else (cons (list (if (atom (car args))
			       (car args)
			       (caar args))
			   (car vals))
		     (bind (cdr args) (cdr vals) kind)))))

(defmethod bind (args vals (kind (eql ^'&key)))
  "Bind optional arguments."
  (cond ((null args) '())
	((member (car args) *lambda-list-keywords*)
	 (bind (cdr args) vals (car args)))
	(:else
          (destructuring-bind (keyword &optional default-exp
                                         (passed-in? (make-instance 'cl-symbol :name nil :package nil)))
                              (mklist (car args))
            (let ((pair (member (cl-intern (cl-symbol-name keyword) "KEYWORD")
                                vals)))
              (if (not pair)
                  (list* (list keyword (cl-eval default-exp))
                         (list passed-in? nil)
                         (bind (cdr args) vals kind))
                  (list* (list keyword (cadr pair))
                         (list passed-in? t)
                         (bind (cdr args) vals kind))))))))
