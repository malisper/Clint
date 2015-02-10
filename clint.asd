(in-package :asdf-user)

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
                                           (:file "primitive-macros" :depends-on ("definitions"))))))))
