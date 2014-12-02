(in-package :asdf-user)

(defsystem "clint-tests"
  :description "Tests for Clint"
  :depends-on ("clunit" "clint")
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "tests")))))
