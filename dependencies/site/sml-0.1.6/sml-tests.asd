(in-package :cl-user)

(defpackage :sml-tests-asd (:use :cl :asdf))

(in-package :sml-tests-asd)

(defsystem :sml-tests
  :name    "sml-tests"
  :author  "Tomoyuki Matsumoto <tomoyuki28jp@gmail.com>"
  :licence "BSD"
  :description "tests for sml"
  :depends-on  (:sml :fiveam :my-util)
  :components  ((:module "tests"
                         :components ((:file "sml")))))
