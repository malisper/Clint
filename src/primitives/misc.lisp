;;;; Miscellaneous ICL and Clint procedures procedures.

(in-package :clint)

(defun-cl make-symbol cl-make-symbol (name)
  "Create a symbol of the given name."
  (make-instance 'cl-symbol :name name))

(defun-cl gensym cl-gensym (&optional (name "G"))
  "Create a new Clint gensym with the given name."
  (prog1 (cl-make-symbol (format nil "~A~A" name cl-*gensym-counter*))
    (incf cl-*gensym-counter*)))

(defun-cl gentemp cl-gentemp (&optional (prefix "G") (package cl-*package*))
  "Create a new Clint gensym interned in the given pacakge."
  (let ((name (format nil "~A~A" prefix cl-*gensym-counter*)))
    (if (gethash name (package-syms (cl-find-package package)))
	(progn (incf cl-*gensym-counter*)
	       (cl-gentemp prefix package))
	(cl-intern name package t))))
