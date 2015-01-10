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
  (cond ((null (cdr xss)) (car xss))
        ((null (car xss)) (apply #'append (cdr xss)))
        ('else (cons (caar xss) (apply #'append (cdar xss) (cdr xss))))))
