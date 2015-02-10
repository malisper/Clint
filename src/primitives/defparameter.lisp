;;;; A way to create variables that are both accessible from Clint
;;;; and the ICL.

(in-package :clint)

(defmacro cl-defparameter (names val &optional doc)
  "Define a Clint variable. A symbol macro for icl-name will be
   defined which will provide access to the Clint variable."
  (multiple-value-bind (clint-name icl-name) (parse-names names)
    `(progn ,(when doc
               `(setf (cl-doc ^',clint-name ^'variable) ,doc))
            (setf (cl-symbol-special ^',clint-name) t)
            (setf (val ^',clint-name) ,val)
            (define-symbol-macro ,icl-name (val ^',clint-name)))))
