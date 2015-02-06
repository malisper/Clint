(in-package :clint)

(defmacro defprimitive-fn (name args &body body)
  "Define a Clint procedure whose body is evaluated by the ICL."
  `(progn
     ,(when (stringp (car body))
	`(setf (cl-doc ^',name ^'function) ,(car body)))
     (setf (val-fn ^',name)
           (make-instance 'prim-fn
	     :prim-code (lambda ,args ,@body)))))

(defmacro defprimitive-macro (name args &body body)
  "Define a Clint macro whose body is evaluated by the ICL."
  `(progn
     ,(when (stringp (car body))
        `(setf (cl-doc ^',name ^'function) ,(car body)))
     (setf (val-fn ^',name)
           (make-instance 'macro
             :macro-fn (make-instance 'prim-fn
                         :prim-code (lambda ,args ,@body))))))

(defmacro defun-cl (names args &body body)
  "Define a Clint primitive and a ICL procedure at the same time. If
   NAMES is a list, the first element is the Clint primitive name and
   the seconds is the ICL procedure name. If it is a single symbol,
   that becomes the Clint primitive name and the ICL procedure name
   becomes that with 'cl-' prepended to the front."
  ;; First extract the names.
  (multiple-value-bind (clint-name icl-name)
                       (if (listp names)
                           (values-list names)
                           (values names (symb 'cl- names)))
    `(progn (defun ,icl-name ,args ,@body)
            ,(let ((g (gensym)))
               `(defprimitive-fn ,clint-name (&rest ,g)
                  ,(when (stringp (car body))
                     (car body))
                  (apply #',icl-name ,g))))))

(defun add-prim (name &optional doc)
  "Adds the primitive named by NAME to the interpreter. NAME should be
   a symbol which will serve as both the Clint name and the symbol
   will be applied to the arguments to implement the primitve."
  (setf (val-fn ^name)
        (make-instance 'prim-fn
          :prim-code (lambda (&rest args) (apply name args))))
  (when doc
    (setf (cl-doc ^name ^'function) doc)))
