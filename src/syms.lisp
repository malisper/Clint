;; The implementation for symbols.

(in-package :clint)

(defclass cl-symbol ()
  ((name    :initarg :name    :accessor cl-symbol-name)
   (package :initarg :package :accessor cl-symbol-package)))

(defmethod print-object ((obj cl-symbol) s)
  "Print a cl-symbol."
  (with-slots (name package) obj
    (format s "~A::~A" (cl-package-name package) name)))
