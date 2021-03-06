(in-package :asdf-user)

(defsystem "clint-tests"
  :description "Tests for Clint"
  :depends-on ("clunit" "clint")
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "tests")))))

(defmethod perform ((op test-op) (c (eql (find-system :clint-tests))))
  (print (symbol-call :clint-tests :run-suite (intern* :clint :clint-tests))))
