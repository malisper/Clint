;;;; These are some higher order functions, meant to be loaded by
;;;; using load from within the interpreter.

(defun member-if (f xs)
  "Return the first tail of XS for which the the procedure F returns
   true when called on its first element."
  (cond ((null xs) '())
	((funcall f (car xs)) xs)
        ;; Use 'else because keywords aren't defined yet.
	('else (member-if f (cdr xs)))))

(defun member (x xs)
  "Return the tail of XS for which the car of it is eql to X."
  (member-if (lambda (y) (eql x y)) xs))

(defun find-if (f xs)
  "Return the first element of XS which satisfies the predicate F."
  (car (member-if f xs)))

(defun find (x xs)
  "Returns the first element of XS that is eql with X."
  (car (member x xs)))

(defun maplist (f xs)
  "Map a procedure over all of the tails of a list. A list of all of
   the results is returned."
  (if (null xs)
      '()
      (cons (funcall f xs) (maplist f (cdr xs)))))

(defun mapcar (f xs)
  "Map a procedure over all of the elements of a list. Return a list
   of the results is returned."
  (maplist (lambda (x) (funcall f (car x))) xs))

(defun mapc (f xs)
  "Apply a procedure to every element in a list. The original
   list is returned."
  (mapcar f xs)
  xs)
