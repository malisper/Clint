;; Sets up the global variable environemnt and function environment.

(in-package :clint)

(defvar *env*  (list '()) "The global variable environment.")
(defvar *fenv* (list '()) "The global function environment.")

(defun extend-env (env fn-args args)
  "Extend a given environment. This works for both variable
   environments and function environments."
  (cons (mapcar #'list fn-args args) env))

(defun cl-boundp (var env)
  "Is this variable bound in the given environment? If it is return
   the binding as a list containing VAR and its value. This is a
   misnomer because the actual boundp only works with global values.
   This one will return true for lexically bound symbols."
  (some (lambda (e)
          (assoc var e))
        env))

(defun binding (var env)
  "Looks up the value of the variable VAR in the enviornment ENV.
   This returns a list containing VAR as the first element and its
   value as the second. This works for both the variable environment
   and the function environment."
  (or (cl-boundp var env)
      (error "Unbound variable or procedure ~A." var)))

(defun val (var env)
  "Looks up the value of the variable VAR in the environment ENV.
   This will work for both the variable environment and the function
   environment."
  (cadr (binding var env)))

(defun (setf val) (val var env)
  "Sets the value of VAR in the given environment to VAL. If the
   variable VAR is unbound. It will be added to the global 
   environment (the last frame of the environment passed in)."
  (let ((binding (cl-boundp var env)))
    (if binding
        (setf (cadr binding) val)
        (push (list var val) (car (last env))))))

(defun global-var (var)
  "Looks up the value of the global variable named by VAR."
  (val var *env*))

(defun (setf global-var) (val var)
  "Set the value of VAR to VAL in the global variable environment."
  (setf (val var *env*) val))

(defun global-fn (name)
  "Looks up the value of the global function named by NAME."
  (val name *fenv*))

(defun (setf global-fn) (val name)
  "Sets the value of NAME to VAL in the global function environment."
  (setf (val name *fenv*) val))
