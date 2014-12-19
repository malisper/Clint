;;;; Utilities for interfacing between the ICL and Clint.

(in-package :clint)

;; A reader macro to make typing in Clint symbols easier. It will take
;; the result of the form that comes after it (as a tree) and convert
;; all of the symbols in it to Clint symbols.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\^
    (lambda (stream char)
      (declare (ignore char))
      `(symbols->cl-symbols ,(read stream t nil t) "CL"))))

(defmacro cl-defparameter (name icl-name val &optional doc)
  "Define a Clint variable. A symbol macro for icl-name will be
   defined which will provide access to the Clint variable."
  `(progn ,(when doc
             `(setf (cl-doc ^',name ^'variable) ,doc))
          (setf (val ^',name *env*) ,val)
          (define-symbol-macro ,icl-name (val ^',name *env*))))
