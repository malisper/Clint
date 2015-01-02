;;;; Definition for variables that need to be accessible in both
;;;; the ICL and Clint.

(in-package :clint)

(cl-defparameter *package* cl-*package* (cl-find-package "CL")
  "The current clint-package.")

(cl-defparameter *gensym-counter* cl-*gensym-counter* 0
  "The number to be appended to the name of the next gensym.")

(cl-defparameter t cl-t ^'t "The truth value.")

(cl-defparameter * cl-* nil
  "The result of the previous expression.")

(cl-defparameter ** cl-** nil
  "The result of the second to last expression.")

(cl-defparameter *** cl-*** nil
  "The result of the third to last expression.")

(cl-defparameter - cl-- nil
  "The current expression being evaluated.")

(cl-defparameter + cl-+ nil
  "The previous expression.")

(cl-defparameter ++ cl-++ nil
  "The second to last expression.")

(cl-defparameter +++ cl-+++ nil
  "The third to last expression.")

;; There are some problems with making the Clint symbol nil and the
;; empty list the same thing.

;; (cl-defparameter nil *cl-nil* '() "The value of false and the empty list")

