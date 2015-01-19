;; The implementation for Clint symbols.

(in-package :clint)

(defclass cl-symbol ()
  ((name     :initarg :name :accessor cl-symbol-name
             :documentation "The name of this symbol.")
   (package  :initarg :package :accessor cl-symbol-package :initform nil
             :documentation "The package this symbol belongs to.")
   (plist    :initarg :plist   :accessor cl-symbol-plist :initform nil
	     :documentation "A symbol's plist.")
   (special  :initarg :special :accessor cl-symbol-special :initform nil
             :documentation "Is this variable dynamic?")))

(defmethod print-object ((sym cl-symbol) s)
  "Prints the name of the Clint symbol."
  ;; Printing of the package is handled in intern.
  (format s "~A" (cl-symbol-name sym)))
