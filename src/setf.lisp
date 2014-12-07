;; Implementation for setf.

(defparameter *setf-expanders* (make-hash-table)
  "A table containing all of the setf-expanders, indexed by the name
   of the procedure used to access the place.")

(defun cl-get-setf-expansion (form)
  "Returns five values needed for setf. They are a (1) list of
   temporary variables to be bound respectively to each form in (2) a
   list of expressions, (3) a symbol which will hold the result, (4)
   the expression to evaluate to do the setting, and (5) an expression
   whose result will be the current value of the place."
  (funcall (gethash (car form) *setf-expanders*
             (lambda (args)
               (let ((result (gensym))
                     (gensyms (loop for x in (cdr form) collect (gensym))))
                 (values gensyms
                         args
                         ^`(,result)
                         ^`(funcall #'(setf ,(car form)) ,result ,@gensyms)
                         ^`(,(car form) ,@gensyms)))))
           (cdr form)))

(defmacro cl-define-setf-expander (name args &body body)
  "Define a setf expander for NAME."
  `(setf (gethash ^',name *setf-expanders*)
         (lambda ,args ,@body)))
