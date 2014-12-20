;;;; Utilities for interfacing between the ICL and Clint.

(in-package :clint)

;; A reader macro to make typing in Clint symbols easier. It will take
;; the result of the form that comes after it (as a tree) and convert
;; all of the symbols in it to Clint symbols.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\^
    (lambda (stream char)
      (declare (ignore char))
      `(symbols->cl-symbols ,(read stream t nil t) "CL"))))

;; This needs to be here because it uses the ^ reader macro which in
;; turn depends on symbols->cl-symbols.
(defmacro cl-defparameter (name icl-name val &optional doc)
  "Define a Clint variable. A symbol macro for icl-name will be
   defined which will provide access to the Clint variable."
  `(progn ,(when doc
             `(setf (cl-doc ^',name ^'variable) ,doc))
          (setf (val ^',name *env*) ,val)
          (define-symbol-macro ,icl-name (val ^',name *env*))))

;; The procedure global-var needs to be used here because
;; cl-defparameter using the ^ reader macro which in turn uses
;; symbols->cl-symbols.
(defun symbols->cl-symbols (code &optional (package (global-var ^'*package*)))
  "Converts all symbols in the tree CODE, into Clint symbols. These
   symbols will belong to PACKAGE which defaults to the current value
   of the Clint variable *package*."
  (maptree (lambda (x)
             (if (typep x 'symbol)
                 (string->cl-symbol (symbol-name x) package)
                 x))
           code))

(defun string->cl-symbol (str &optional (package (global-var ^'*package*)))
  "Takes a string and returns the Clint symbol it represents. If there
   is no package attached to the string, it is interned into the
   Clint package PACKAGE."
  (if (not (find #\: str))
      (cl-intern str package)
      (let* ((pack-pos (position #\: str))
             (sym-pos  (position #\: str :from-end t)))
        (cl-intern (subseq str (+ sym-pos 1))
                   (subseq str 0 pack-pos)))))

;; This creates Clint's CL package if it does not already exist. This
;; line is early one because it is needed for cl-find-package.
(or (cl-find-package "CL") (make-instance 'cl-package :name "CL"))

(defun cl-intern (name &optional (designator (global-var ^'*package*)))
  "Interns a Clint symbol in the given Clint package."
  (let ((package (if (typep designator 'cl-package)
                     designator
                     (cl-find-package designator))))
    (with-slots (syms) package
      (or (gethash name syms)
          (setf (gethash name syms)
                (make-instance 'cl-symbol :name name :package package))))))

(cl-defparameter *package* *cl-package*
  (or (cl-find-package "CL") (make-instance 'cl-package :name "CL"))
  "The current clint-package.")

(defmethod print-object :before ((sym cl-symbol) s)
  "When printing a Clint symbol, if the symbols' package is not the
   same as the current package, prepend the package name to it."
  (let ((current-package *cl-package*))
    (with-slots (package) sym
      (if package
          (unless (eq package current-package)
            (format s "~A::" (cl-package-name package)))
          (format s "#:")))))

