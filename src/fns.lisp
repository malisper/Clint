;; Implementation for fns (and macros).

(in-package :clint)

(defclass fn () ())

(defclass lambda-fn (fn)
  ((args :initarg :args :accessor fn-args)
   (code :initarg :code :accessor fn-code)
   (env  :initarg :env  :accessor fn-env)
   (fenv :initarg :fenv :accessor fn-fenv)))

(defclass macro (fn)
  ((fn :initarg :macro-fn :accessor macro-fn)))

(defclass prim-fn (fn)
  ((prim-code :initarg :prim-code :accessor prim-code)))
