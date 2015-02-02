;;;; These are some helper macros/procedures.

(in-package :clint)

(defmacro switchlet (var exp &rest clauses)
  "Equivalent to switch but binds EXP to VAR."
  `(let ((,var ,exp))
     ,(labels ((recur (left)
                      (cond ((null left) nil)
                            ((null (cdr left)) (car left))
                            (:else `(if (eql ,var ,(car left))
                                        ,(cadr left)
                                        ,(recur (cddr left)))))))
              (recur clauses))))

(defmacro switch (exp &rest clauses)
  "Similar to case, but with slightley different syntax and the
   expressions being compared against are evaluated. The syntax is:

   (switch <exp-1> <consequence-1> ... <consequence-n> [<else-exp>])"
  `(switchlet ,(gensym) ,exp ,@clauses))

(defun maptree (f tree)
  "Maps the procedure F over the tree TREE."
  (cond ((null tree) '())
	((atom tree) (funcall f tree))
	(:else (cons (maptree f (car tree))
		     (maptree f (cdr tree))))))

(defun mkstr (&rest args)
  "Returns the string representing all of the arguments."
  (with-output-to-string (*standard-output*)
    (mapc #'princ args)))

(defun symb (&rest args)
  "Returns a symbol representing all of the arguments."
  (values (intern (apply #'mkstr args))))
