;;;; Definition for variables that need to be accessible in both
;;;; the ICL and Clint.

(in-package :clint)

(cl-defparameter *package* (cl-find-package "CL")
  "The current clint-package.")

(cl-defparameter *gensym-counter* 0
  "The number to be appended to the name of the next gensym.")

(cl-defparameter t ^'t "The truth value.")

(cl-defparameter * nil "The result of the previous expression.")
(cl-defparameter ** nil "The result of the second to last expression.")
(cl-defparameter *** nil "The result of the third to last expression.")

(cl-defparameter + nil "The previous expression.")
(cl-defparameter ++ nil "The second to last expression.")
(cl-defparameter +++ nil "The third to last expression.")

(cl-defparameter - nil "The current expression being evaluated.")


;; There are some problems with making the Clint symbol nil and the
;; empty list the same thing.

;; (cl-defparameter nil *cl-nil* '() "The value of false and the empty list")
