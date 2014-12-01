(in-package :asdf-user)

(defsystem "clint"
  :description "A Common Lisp Interpreter."
  :version "0.1"
  :author "malisper"
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "syms")
                             (:file "packages")
                             (:file "fns")
                             (:file "interp")
                             (:file "primitive-fns")
                             (:file "primitive-macros")
                             (:file "reader")))))
