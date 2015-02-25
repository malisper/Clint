(defpackage :clint-asd
  (:use :cl :asdf)
  (:export :cl-file))

(in-package :clint-asd)

(defclass clint-file (source-file) ())

(defmethod perform ((o load-op) (file clint-file))
  "Load the file into Clint."
  ;; The Clint package is inaccessible until it is loaded.
  (funcall (intern "CL-LOAD" "CLINT") (car (input-files o file))))

(defmethod perform ((o compile-op) (file clint-file))
  "Cannot compile clint files, return nil."
  nil)

(defsystem "clint"
  :description "A Common Lisp Interpreter."
  :version "0.1"
  :author "malisper"
  :components ((:module "src"
                :components ((:file "package")
			     (:file "helper" :depends-on ("package"))
			     (:module "core"
                              :depends-on ("package" "helper")
                              :components ((:file "docs")
                                           (:file "syms")
                                           (:file "env" :depends-on ("syms"))
                                           (:file "packages")
                                           (:file "intern" :depends-on ("syms" "packages"))
                                           (:file "fns")
                                           (:file "arguments" :depends-on ("syms"))
                                           (:file "interpreter" :depends-on ("arguments"))
                                           (:file "arguments2" :depends-on ("arguments" "interpreter"))))
			     (:module "primitives"
                              :depends-on ("package" "helper" "core")
                              :components ((:file "defparameter")
                                           (:file "vars" :depends-on ("defparameter"))
                                           (:file "definitions")
                                           (:file "misc" :depends-on ("definitions"))
                                           (:file "setf" :depends-on ("misc"))
                                           (:file "reader" :depends-on ("defparameter" "vars" "misc"))
                                           (:file "cxr")
                                           (:file "primitive-fns" :depends-on ("definitions" "cxr"))
                                           (:file "primitive-macros" :depends-on ("definitions"))))
                             (:module "libs"
                              :depends-on ("core" "primitives")
                              :components ((:clint-file "higher-order-fns.lisp")
                                           (:clint-file "fns.lisp")
                                           (:clint-file "lists.lisp")
                                           (:clint-file "exports.lisp")))))))
