;; The implementation for Clint symbols.

(in-package :clint)

(defclass cl-symbol ()
  ((name    :initarg :name :accessor cl-symbol-name
            :documentation "The name of this symbol.")
   (package :initarg :package :accessor cl-symbol-package :initform nil
            :documentation "The package this symbol belongs to.")))

(defmethod print-object ((sym cl-symbol) s)
  "Prints the name of the Clint symbol."
  ;; Printing of the package is handled in interp.
  (format s "~A" (cl-symbol-name sym)))

(defun cl-make-symbol (name)
  "Create a Clint symbol with the given name. It is uninterned."
  (make-instance 'cl-symbol :name name))
