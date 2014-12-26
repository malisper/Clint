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
				      :components ((:file "docs")
						   (:file "env")
						   (:file "syms")
						   (:file "packages")
						   (:file "intern")
						   (:file "fns")
						   (:file "arguments")
						   (:file "interpreter")
						   (:file "arguments2")))
			     (:module "primitives"
				      :components ((:file "defparameter")
						   (:file "vars")
						   (:file "definitions")
						   (:file "misc")
						   (:file "setf")
						   (:file "reader")
						   (:file "primitive-fns")
						   (:file "primitive-macros")))))))
