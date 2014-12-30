(defun list (&rest args)
  "Return a list containing the given arguments."
  args)

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
