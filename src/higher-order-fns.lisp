;;;; These are some higher order functions, meant to be loaded by
;;;; using load from within the interpreter.

(defun member-if (f xs)
  "Return the first tail of XS for which the the procedure F returns
   true when called on its first element."
  (cond ((null xs) '())
	((funcall f (car xs)) xs)
	('else (member-if f (cdr xs)))))

(defun find-if (f xs)
  "Return the first element of XS which satisfies the predicate F."
  (car (member-if f xs)))
