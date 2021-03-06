;; Implementation of Clint packages and related procedures.

(in-package :clint)

(defvar *packages* (make-hash-table :test #'equal)
  "A hash-table containing all of the packages. Currently indexed
   by the name as a string.")

(defclass cl-package ()
  ((name :initarg :name :accessor cl-package-name
         :documentation "The name of the package.")
   (syms :initarg :syms :accessor package-syms :initform (make-hash-table :test #'equal)
         :documentation "The symbols contained in the package.")
   (nicks :initarg :nicks :accessor cl-package-nicks :initform '()
	  :documentation "The nicknames for the package.")
   (using :initarg :using :accessor cl-package-using :initform '()
          :documentation "A list of all of the packages being used by this one.")
   (externals :initarg :externals :accessor cl-package-externals :initform (make-hash-table)
	      :documentation "A hash-table containing all of the external symbols."))
  (:documentation "A Clint package."))

(defmethod initialize-instance :after ((pack cl-package) &key)
  "Adds this package to the ICL *packages* global variable."
  (setf (gethash (cl-package-name pack) *packages*) pack)
  (dolist (nick (cl-package-nicks pack))
    (setf (gethash nick *packages*) pack)))

(defun cl-find-package (name)
  "Returns the Clint package of the given name."
  (if (typep name 'cl-package)
      name
      (gethash name *packages*)))
