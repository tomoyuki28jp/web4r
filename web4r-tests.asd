(in-package :cl-user)

(defpackage :web4r-tests-asd (:use :cl :asdf))

(in-package :web4r-tests-asd)

(defsystem :web4r-tests
  :name    "web4r-tests"
  :author  "Tomoyuki Matsumoto <tomoyuki28jp@gmail.com>"
  :licence "BSD"
  :description "tests for web4r"
  :depends-on  (:web4r :fiveam :drakma)
  :components  ((:module "tests"
                         :serial  t
                         :components
                         ((:file "package")
                          (:file "util")
                          (:file "cont")
                          (:file "server")
                          (:file "valid")
                          (:file "pager")
                          (:file "pclass")
                          (:file "user")
                          (:file "image")
                          ))))
