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
                 (cl-intern (symbol-name x) package t)
                 x))
           code))

;; This creates Clint's CL package if it does not already exist. This
;; line is early one because it is needed for cl-find-package.
(or (cl-find-package "CL") (make-instance 'cl-package :name "COMMON-LISP"
					              :nicks (list "CL")))

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

(defun cl-intern (name &optional (designator (current-package)) internal)
  "Look up the symbol named by NAME in the given package. The
   argument INTERNAL is if it is possible to look at the internal
   symbols in the package."
  (let* ((package (cl-find-package designator))
	 (sym (gethash name (package-syms package)))
	 (current-package-p (eq package (current-package))))
    (cond (sym (if (or internal
		       current-package-p
		       (gethash sym (cl-package-externals package)))
		   sym
		   (error "The symbol ~A is not external in the package ~A"
			  sym package)))
	  ((or internal current-package-p)
	   (setf (gethash name (package-syms package))
		 (make-instance 'cl-symbol :package package :name name)))
	  (:else (error "No symbol with the name ~A found in the package ~A"
			name package)))))

(defun find-accessible-symbol (name package &optional (visited nil))
  "Determine if the symbol named by NAME is accessible from PACKAGE
   and if so return it. Visited is a list of all of the packages
   visited so far."
  ;; If we haven't visited any packages so far we can return an
  ;; internal symbol.
  (or (let ((sym (gethash name package)))
        (and (or (null visited) (gethash sym (cl-package-externals package)))
             sym))
      (some (lambda (pack)
              (find-accessible-symbol name pack (cons package visited)))
            (cl-package-using package))))

(defmethod print-object :before ((sym cl-symbol) s)
  "When printing a Clint symbol, if the symbols' package is not the
   same as the current package, prepend the package name to it."
  (let ((current-package (val ^'*package*))
        (package (cl-symbol-package sym)))
    (if package
        (unless (eq package current-package)
          (format s "~A::" (cl-package-name package)))
        (format s "#:"))))
