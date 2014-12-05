;; Functions for working with documentation.

(defparameter *docs* (make-hash-table :test #'equal)
  "A table mapping from a list containing the name and the type to
   the actual documentation.")

(defun cl-doc (name type)
  "Looks up the documentation for the type TYPE under NAME."
  (gethash (list name type) *docs*))

(defun (setf cl-doc) (str name type)
  "Sets the documentation for the object of type TYPE and name NAME
   to STR."
  (setf (gethash (list name type) *docs*) str))
