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
  "Similar to case but the expressions being compared against are
   evaluated and has slightley different syntax. The syntax is

   (switch <exp-1> <consequence-1> ... <consequence-n> [<else-exp>])"
  `(switchlet ,(gensym) ,exp ,@clauses))
