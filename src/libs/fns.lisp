;;;; Some basic functions.

(defun identity (x)
  "Returns whatever is passed in."
  x)

(defun constantly (x)
  "Return a function that will always return whatever was initially
   passed in."
  (lambda (&rest args) x))
