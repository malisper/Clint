;;;; This file defines a macro (define-cxrs), which is capable of
;;;; defining all of the cxr functions (car, cdr, cadr, etc, and (setf
;;;; car), (setf cdr), (setf cadr), etc) at one time.

(in-package :clint)

(defun generate-n (xs n)
  "Generates all possible permutations of N elements of XS."
  (if (eql n 0)
      '(())
      (mapcan (lambda (elt)
                (mapcar (lambda (perm)
                          (cons elt perm))
                        (generate-n xs (- n 1))))
              xs)))

(defun range (n)
  "Generate the range from 1 to N."
  (loop for i from 1 to n collect i))

(defun generate-cxrs (n)
  "Generates all of the symbols for cxrs up to N levels deep,
   excluding cr."
  (mapcan (lambda (i)
            (mapcar (lambda (ads)
                      (intern (format nil "C~{~A~}R" ads)))
                    (generate-n '(a d) i)))
          (range n)))

(defmacro define-cxrs (n)
  "Define all of the cxrs nested from 1 to N."
  `(progn
     ,@(mapcar (lambda (cxr)
                 `(progn (add-prim ',cxr
                           ,(format nil "Return the ~A of X." cxr))
                         (defprimitive-fn (setf ,cxr) (val x)
                           ,(format nil "Set the value of the ~A of X to VAL." cxr)
                           (setf (,cxr x) val))))
               (generate-cxrs n))))
