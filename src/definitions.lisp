(in-package :clint)

(defmacro defprimitive-fn (name args &body body)
  "Define a Clint procedure whose body is evaluated by the ICL."
  `(progn
     ,(when (stringp (car body))
	    `(setf (cl-doc ^',name ^'function) ,(car body)))
     (setf (global-fn ^',name)
           (make-instance 'prim-fn
			  :prim-code (lambda ,args ,@body)))))

(defmacro defprimitive-macro (name args &body body)
  "Define a Clint macro whose body is evaluated by the ICL."
  `(progn
     ,(when (stringp (car body))
        `(setf (cl-doc ^',name ^'function) ,(car body)))
     (setf (global-fn ^',name)
           (make-instance 'macro
             :macro-fn (make-instance 'prim-fn
                         :prim-code (lambda ,args ,@body))))))

(defmacro defun-cl (name icl-name args &body body)
  "Define a procedure which is both a ICL procedure named by ICL-NAME
   and a Clint primiive named by NAME."
  `(progn (defun ,icl-name ,args ,@body)
	  ,(let ((g (gensym)))
	     `(defprimitive-fn ,name (&rest ,g)
		,(when (stringp (car body))
		   (car body))
		(apply #',icl-name ,g)))))
