(in-package :asdf-user)

(defsystem "clint"
  :description "A Common Lisp Interpreter."
  :version "0.1"
  :author "malisper"
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "helper")
                             (:file "docs")
                             (:file "env")
                             (:file "syms")
                             (:file "packages")
                             (:file "intern")
                             (:file "fns")
                             (:file "interpreter")
                             (:file "setf")
			     (:file "vars")
			     (:file "misc")
                             (:file "primitive-fns")
                             (:file "primitive-macros")
                             (:file "reader")))))
