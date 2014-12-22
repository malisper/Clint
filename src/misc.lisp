;;;; Miscellaneous ICL and Clint procedures procedures.

(in-package :clint)

(defun-cl make-symbol cl-make-symbol (name)
  "Create a symbol of the given name."
  (make-instance 'cl-symbol :name name))

(defun-cl gensym cl-gensym (&optional (name "G"))
  "Create a new Clint gensym with the given name."
  (prog1 (cl-make-symbol (format nil "~A~A" name cl-*gensym-counter*))
    (incf cl-*gensym-counter*)))
