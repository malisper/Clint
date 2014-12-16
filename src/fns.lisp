;; Implementation for Clint macros and procedures.

(in-package :clint)

(defclass fn () ()
  (:documentation "A Clint macro or procedure."))

(defclass lambda-fn (fn)
  ((args :initarg :args
         :accessor fn-args
         :documentation "The arguments this procedure needs.")
   
   (code :initarg :code
         :accessor fn-code
         :documentation "The body of this lambda function.")
   
   (env  :initarg :env
         :accessor fn-env
         :documentation "The var environment that is closed over.")
   
   (fenv :initarg :fenv
         :accessor fn-fenv
         :documentation "The fn environment that is closed over."))
  (:documentation "A Clint lambda function."))

(defclass macro (fn)
  ((fn :initarg :macro-fn :accessor macro-fn
       :documentation "The Clint function to be called to generate
                       the macroexpansion. This is another Clint fn."))
  (:documentation "A Clint macro."))

(defclass prim-fn (fn)
  ((prim-code :initarg :prim-code :accessor prim-code
              :documentation "An ICL procedure that should be called
                              when the primitive procedure is called."))
  (:documentation "A Clint primitive procedure."))
