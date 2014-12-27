(defun list (&rest args)
  "Return a list containing the given arguments."
  args)

(defun nthcdr (n xs)
  "Returns the Nth cdr of XS."
  (if (eql n 0)
      xs
      (nthcdr (- n 1) (cdr xs))))
