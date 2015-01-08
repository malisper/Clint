;;;; A way to create variables that are both accessible from Clint
;;;; and the ICL.

(in-package :clint)

(defmacro cl-defparameter (name icl-name val &optional doc)
  "Define a Clint variable. A symbol macro for icl-name will be
   defined which will provide access to the Clint variable."
  `(progn ,(when doc
		 `(setf (cl-doc ^',name ^'variable) ,doc))
          (setf (cl-symbol-special ^',name) t)
          (setf (val ^',name *env* *denv*) ,val)
          (define-symbol-macro ,icl-name (val ^',name *env* *denv*))))
