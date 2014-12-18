;;;; Implementation for Clint's cons pairs.

(in-package :clint)

(defclass cl-cons ()
  ((car :accessor cl-car :initarg :car
        :documentation "The car of this pair.")
   (cdr :accessor cl-cdr :initarg :cdr
        :documentation "The cdr of this pair."))
  (:documentation "A Clint cons pair."))

(defmethod print-object ((xs cl-cons) str)
  "Prints Clint's cons pair."
  (princ "(" str)
  (loop for current = xs then (cl-cdr current)
        until (not (typep current 'cl-cons))
        ;; Print a space after before every element except the first.
        unless (eq current xs)
          do (princ " " str)
        do (format str "~A" (cl-car current))
        finally (if (null current)
                    (princ ")" str)
                    (format str ". ~A)" current))))

(defun cl-cons (x y)
  "Make a Clint cons pair of the elements X and Y."
  (make-instance 'cl-cons :car x :cdr y))

(defun cl-list (&rest args)
  "Make a Clint list containing ARGS."
  (let* ((result (cl-cons '() '())) (tail result))
    (loop for a in args
          do (setf tail (setf (cl-cdr tail) (cl-cons a '()))))
    (cl-cdr result)))
