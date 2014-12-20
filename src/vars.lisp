;;;; Definition for variables that need to be accessible in both
;;;; the ICL and Clint.

(in-package :clint)

(cl-defparameter *package* cl-*package*
  (or (cl-find-package "CL") (make-instance 'cl-package :name "CL"))
  "The current clint-package.")

(cl-defparameter *gensym-counter* cl-*gensym-counter* 0
  "The number to be appended to the name of the next gensym.")

(cl-defparameter t cl-t ^'t "The truth value.")

;; There are some problems with making the Clint symbol nil and the
;; empty list the same thing.

;; (cl-defparameter nil *cl-nil* '() "The value of false and the empty list")

