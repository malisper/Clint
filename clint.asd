(in-package :asdf-user)

(defsystem "clint"
  :description "A Common Lisp Interpreter."
  :version "0.1"
  :author "malisper"
  :components ((:module "src"
                :serial t
                :components ((:file "package")
			     (:file "helper")
			     (:module "core"
                                      :serial t
				      :components ((:file "docs")
						   (:file "syms")
						   (:file "env")
						   (:file "packages")
						   (:file "intern")
						   (:file "fns")
						   (:file "arguments")
						   (:file "interpreter")
						   (:file "arguments2")))
			     (:module "primitives"
                                      :serial t
				      :components ((:file "defparameter")
						   (:file "vars")
						   (:file "definitions")
						   (:file "misc")
						   (:file "setf")
						   (:file "reader")
                                                   (:file "cxr")
						   (:file "primitive-fns")
						   (:file "primitive-macros")))))))
