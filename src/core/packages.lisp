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
   (externals :initarg :externals :accessor cl-package-externals :initform (make-hash-table :test #'equal)
	      :documentation "A list of all of the external symbols."))
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

(defun lookup-symbol (name designator &optional internal)
  "Look up the symbol named by NAME in the given package. The
   argument INTERNAL is if it is possible to look at the internal
   symbols in the package."
  (let* ((package (cl-find-package designator))
	 (sym (gethash name (package-syms package))))
    (cond (sym (if (or internal (member sym (cl-package-externals package)))
		   sym
		   (error "The symbol ~A is not external in the package ~A"
			  sym package)))
	  (internal (setf (gethash name (package-syms package))
			  (make-instance 'cl-symbol :package package :name name)))
	  (:else (error "No symbol with the name ~A found in the package ~A"
			sym package)))))
