;; Implementation of packages.

(in-package :clint)

(defvar *packages* (make-hash-table :test #'equalp)
  "A hash-table containing all of the packages. Currently indexed
   by the name as a string.")

(defclass cl-package ()
  ((name :initarg :name :accessor cl-package-name)
   ;; I feel like there should be some other name for this per the standard.
   (syms :initarg :syms :accessor package-syms :initform (make-hash-table :test #'equalp))))

(defmethod initialize-instance :after ((pack cl-package) &key)
  "Adds this package to the *packages* global variable."
  (setf (gethash (cl-package-name pack) *packages*) pack))

(defun cl-find-package (name)
  "Returns the package of the given name."
  (gethash name *packages*))
