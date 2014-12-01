;; The implementation for symbols.

(in-package :clint)

(defclass cl-symbol ()
  ((name    :initarg :name    :accessor cl-symbol-name)
   (package :initarg :package :accessor cl-symbol-package)))

(defmethod print-object ((sym cl-symbol) s)
  "Print a cl-symbol."
  ;; Printing of the package is handled in interp.
  (format s "~A" (cl-symbol-name sym)))
