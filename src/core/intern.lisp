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

;; The procedure val needs to be used here because cl-defparameter
;; using the ^ reader macro which in turn uses symbols->cl-symbols.
(defun symbols->cl-symbols (code &optional (package (val ^'*package*)))
  "Converts all symbols in the tree CODE, into Clint symbols. These
   symbols will belong to PACKAGE which defaults to the current value
   of the Clint variable *package*."
  (maptree (lambda (x)
             (if (typep x 'symbol)
                 (cl-intern (symbol-name x) package)
                 x))
           code))

;; This creates Clint's CL package if it does not already exist. This
;; line is early one because it is needed for cl-find-package.
(or (cl-find-package "CL") (make-instance 'cl-package :name "COMMON-LISP"
					              :nicks (list "CL")))

(or (cl-find-package "KEYWORD") (Make-instance 'cl-package :name "KEYWORD"))

(setf (gethash "*PACKAGE*" (package-syms (cl-find-package "CL")))
      (make-instance 'cl-symbol
        :name "*PACKAGE*" :package (cl-find-package "CL")
        :special t))

(defun current-package ()
  "Return the current package."
  (val (gethash "*PACKAGE*" (package-syms (cl-find-package "CL")))))

(defun (setf current-package) (val)
  "Set the current-package."
  (setf (val (gethash "*PACKAGE*" (package-syms (cl-find-package "CL"))))
        val))

(setf (current-package) (cl-find-package "CL"))

(defun internal (name package)
  "Is there a symbol with the given name that is internal in the
   given package? If so return it."
  (gethash name (package-syms package)))

;; This could be more consistent. It accepts a symbol unlike the
;; others which accept a string.
(defun external (sym package)
  "Is the given symbol external in the given package?"
  (gethash sym (cl-package-externals package)))

(defun inherited (name package &optional seen)
  "Is there a symbol with the name that is inherited by the given
   package? If so return it."
  (unless (member package seen)
    (some (lambda (p)
            (let ((sym (or (internal name p)
                           (inherited name p (cons package seen)))))
              ;; We need to test for sym then pass it to external
              ;; but still return it if everything is true.
              (and sym (external sym p) sym)))
          (cl-package-using package))))

(defun accessible (name package)
  "Is there a symbol with the given name that is accessible in the
   given package?"
  (or (internal name package)
      (inherited name package)))

(defun lookup-symbol (name designator)
  "Lookup the symbol with the given name in the given package.
   This acts like single colon notation."
  (let ((package (cl-find-package designator)))
    (unless package
      (error "Package: ~A does not exist" designator))
    (let ((sym (accessible name package)))
      (unless sym
	(error "Cannot find symbol ~A in package ~A" name package))
      (and sym
	   (external sym package)
	   sym))))

(defun cl-intern (name &optional (designator (current-package)))
  "Intern the symbol named by NAME in the given package. This acts
   like double colon notation."
  (let ((package (cl-find-package designator)))
    (if (not package)
        (error "Cannot find package ~A" designator)
        (or (accessible name package)
            (let ((sym (make-instance 'cl-symbol :name name :package package)))
              (setf (gethash name (package-syms package)) sym)
              ;; Make keywords self evaluating.
              (when (eq package (cl-find-package "KEYWORD"))
                (setf (val sym) sym))
              sym)))))

(defmethod print-object :before ((sym cl-symbol) s)
  "When printing a Clint symbol, if the symbols' package is not the
   same as the current package, prepend the package name to it."
  (let ((current-package (val ^'*package*))
        (package (cl-symbol-package sym)))
    (cond ((not package) (format s "#:"))
          ((eq package (cl-find-package "KEYWORD")) (format s ":"))
          (:else (unless (eq sym (accessible (cl-symbol-name sym) current-package))
                   (format s "~A::" (cl-package-name package)))))))
