;;;; Miscellaneous ICL procedures.

(in-package :clint)

(defun cl-gensym (&optional (name "G"))
  "Create a new Clint gensym with the given name."
  (prog1 (cl-make-symbol (format nil "~A~A" name cl-*gensym-counter*))
    (incf cl-*gensym-counter*)))
