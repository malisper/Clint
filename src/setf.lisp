;; Implementation for setf.

(in-package :clint)

(defparameter *setf-expanders* (make-hash-table)
  "A table containing all of the setf-expanders, indexed by the name
   of the procedure used to access the place.")

(defun cl-get-setf-expansion (form)
  "Returns five values needed for setf. They are a (1) list of
   temporary variables to be bound respectively to each form in (2) a
   list of expressions, (3) a symbol which will hold the result, (4)
   the expression to evaluate to do the setting, and (5) an expression
   whose result will be the current value of the place."
  (let ((result ^(gensym)))
    (if (typep form 'cl-symbol)
        (values '() '() `(,result) `(setq ,form ,result) form)
        (apply (gethash (car form) *setf-expanders*
                 (lambda (&rest args)
                   (let ((result (gensym))
                         (gensyms (loop for x in args collect (gensym))))
                       (values gensyms
                               args
                               ^`(,result)
                               ^`(funcall #'(setf ,(car form))
                                          ,result ,@gensyms)
                               ^`(,(car form) ,@gensyms)))))
                 (cdr form)))))

(defmacro cl-define-setf-expander (name args &body body)
  "Define a setf expander for NAME."
  `(setf (gethash ^',name *setf-expanders*)
         (lambda ,args ,@body)))

(defmacro cl-define-modify-macro (name lambda-list fn)
  "Define a modify macro."
  (let ((val (gensym)))
    `(defprimitive-macro ,name (,val ,@lambda-list)
       (multiple-value-bind (temps vals stores store-form access-form)
                            (cl-get-setf-expansion ,val)
         ^`(let* (,@(mapcar #'list temps vals)
                  (,@stores (,',fn ,access-form ,,@lambda-list)))
             ,store-form)))))

(defmacro cl-defsetf (access update)
  "Defines UPDATE to be called when using (setf (ACCESS ...) ...)."
  `(cl-define-setf-expander ,access (&rest args)
     (let ((gensyms (loop for a in args collect (gensym))) (result (gensym)))
       (values gensyms
               args
               ^`(,result)
               ^`(,',update ,@gensyms ,result)
               ^`(,',access ,@gensyms)))))
