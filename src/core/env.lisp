;; The implementation for environments. This sets up the two
;; environments, as well as defines procedures for accessing values in
;; them.

(in-package :clint)

(defvar *global-env* (list '()) "The global variable environment.")
(defvar *global-fenv* (list '()) "The global function environment.")

(defvar *env*  *global-env* "The current variable environment.")
(defvar *fenv* *global-fenv* "The current function environment.")
(defvar *denv* (list '()) "The current dynamic variable environment.")

(defun cl-boundp (var env)
  "Is this variable bound in the given environment? If it is return
   the binding as a list containing VAR and its value. This is a
   misnomer because boundp only works with global values. This one
   will return true for lexically bound symbols."
  (some (lambda (e)
          ;; Use equal so (setf ...) can be used as a function name.
          (assoc var e :test #'equal))
        env))

(defun lookup (var)
  "Look up the value of VAR in either the global environment or
   lexical environment depending on whether it is special or not."
  (if (cl-symbol-special var)
      (cl-boundp var *denv*)
      (cl-boundp var *env*)))

(defun lookup-fn (fn)
  "Look up the function named by FN."
  (cl-boundp fn *fenv*))

(defun binding (var)
  "Look up the current value of VAR. Returns a pair containing the
   variable its value. Signals an error if there is no such variable."
  (or (lookup var) (error "Unbound variable ~A." var)))

(defun binding-fn (fn)
  "Look up the current value of FN. Returns a pair containing the
   function name and its value. Signals an error if there is no such
   function."
  (or (lookup-fn fn) (error "Unbound function ~A." fn)))

(defun val (var)
  "Look up the current value of VAR."
  (cadr (binding var)))

(defun val-fn (fn)
  "Look up the function associated with the given function name."
  (cadr (binding-fn fn)))

(defun (setf val) (val var)
  "Sets the value of VAR in the current environment to VAL. If the
   variable VAR is unbound. It will be added to the dynamic
   environment if it is special. Otherwise it will be added to the
   global lexical environment."
  (let ((binding (lookup var)))
    (if binding
        (setf (cadr binding) val)
        (progn (if (cl-symbol-special var)
                   (push (list var val) (car (last *denv*)))
                   (push (list var val) (car (last *env*))))
               val))))

(defun (setf val-fn) (val fn)
  "Sets the function associated with FN in the current environment to
   VAL. If the function is currently undefined, it is added to the
   global fn environment."
  (let ((binding (lookup-fn fn)))
    (if binding
        (setf (cadr binding) val)
        (progn (push (list fn val) (car (last *fenv*)))
               val))))
