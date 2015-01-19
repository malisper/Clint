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

;; The procedure global-var needs to be used here because
;; cl-defparameter using the ^ reader macro which in turn uses
;; symbols->cl-symbols.
(defun symbols->cl-symbols (code &optional (package (global-var ^'*package*)))
  "Converts all symbols in the tree CODE, into Clint symbols. These
   symbols will belong to PACKAGE which defaults to the current value
   of the Clint variable *package*."
  (maptree (lambda (x)
             (if (typep x 'symbol)
                 (cl-intern (symbol-name x) package t)
                 x))
           code))

;; This creates Clint's CL package if it does not already exist. This
;; line is early one because it is needed for cl-find-package.
(or (cl-find-package "CL") (make-instance 'cl-package :name "COMMON-LISP"
					              :nicks (list "CL")))

(setf (gethash "*PACKAGE*" (package-syms (cl-find-package "CL")))
      (make-instance 'cl-symbol :name "*PACKAGE*" :package (cl-find-package "CL")))

(setf (global-var (gethash "*PACKAGE*" (package-syms (cl-find-package "CL"))))
      (cl-find-package "CL"))

(defun cl-intern (name &optional (designator (global-var ^'*package*)) internal)
  "Look up the symbol named by NAME in the given package. The
   argument INTERNAL is if it is possible to look at the internal
   symbols in the package."
  (let* ((package (cl-find-package designator))
	 (sym (gethash name (package-syms package)))
	 ;; I want to get rid of the following hack.
	 (current-package (eq package (global-var (gethash "*PACKAGE*" (package-syms (cl-find-package "CL")))))))
    (cond (sym (if (or internal
		       current-package
		       (gethash sym (cl-package-externals package)))
		   sym
		   (error "The symbol ~A is not external in the package ~A"
			  sym package)))
	  ((or internal current-package)
	   (setf (gethash name (package-syms package))
		 (make-instance 'cl-symbol :package package :name name)))
	  (:else (error "No symbol with the name ~A found in the package ~A"
			name package)))))

(defmethod print-object :before ((sym cl-symbol) s)
  "When printing a Clint symbol, if the symbols' package is not the
   same as the current package, prepend the package name to it."
  (let ((current-package (global-var ^'*package*)))
    (with-slots (package) sym
      (if package
          (unless (eq package current-package)
            (format s "~A::" (cl-package-name package)))
          (format s "#:")))))

