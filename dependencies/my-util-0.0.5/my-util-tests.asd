(in-package :cl-user)

(defpackage :my-util-tests-asd (:use :cl :asdf))

(in-package :my-util-tests-asd)

(defsystem :my-util-tests
  :name    "my-util-tests"
  :author  "Tomoyuki Matsumoto <tomoyuki28jp@gmail.com>"
  :licence "BSD"
  :description "tests for my-util"
  :depends-on  (:my-util :fiveam)
  :components  ((:module "tests"
                         :components
                         ((:file "my-util")))))
