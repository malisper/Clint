(defun list (&rest args)
  "Return a list containing the given arguments."
  args)

(defun list* (&rest args)
  "Return a dotted list containing all of the elements."
  (if (null (cdr args))
      (car args)
      (cons (car args) (apply #'list* (cdr args)))))

(defun nthcdr (n xs)
  "Returns the Nth cdr of XS."
  (if (eql n 0)
      xs
      (nthcdr (- n 1) (cdr xs))))

(defun equal (x y)
  "Do these lists contain eql or equal items?"
  (or (eql x y)
      (and (not (atom x))
           (not (atom y))
           (equal (car x) (car y))
           (equal (cdr x) (cdr y)))))

(defun length (x)
  "Returns the length of a list."
  (if (null x)
      0
      (+ 1 (length (cdr x)))))

(defun append (&rest xss)
  "Append all of the arguments together."
  (cond ((null (cdr xss)) (car xss))
        ((null (car xss)) (apply #'append (cdr xss)))
        ('else (cons (caar xss) (apply #'append (cdar xss) (cdr xss))))))

(defun last (xs &optional (n 1))
  "Returns the last N elements of XS."
  (if (null (nthcdr n xs))
      xs
      (last (cdr xs) n)))

(defun nconc (&rest xss)
  "Appends lists together by modifying the tail of each one to point
   to the one after it."
  (cond ((null (cdr xss)) (car xss))
        ((null (car xss)) (apply #'nconc (cdr xss)))
        ('else (setf (cdr (last (car xss)))
		     (apply #'nconc (cdr xss)))
	       (car xss))))
