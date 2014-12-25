;; Implementation for procedures for looking up and setting Clint
;; documentation.

(in-package :clint)

(defparameter *docs* (make-hash-table :test #'equal)
  "A table mapping from a list containing the name and the
   type (function, variable, etc) of an object to the documentation
   string.")

(defun cl-doc (name type)
  "Looks up the documentation string for the object of type TYPE (function,
   variable, etc) that is named by NAME."
  (gethash (list name type) *docs*))

(defun (setf cl-doc) (str name type)
  "Sets the documentation string for the object of type TYPE (function,
   variable, etc) that is named by NAME."
  (setf (gethash (list name type) *docs*) str))
