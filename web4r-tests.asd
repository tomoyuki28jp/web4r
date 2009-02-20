(in-package :cl-user)

(defpackage :web4r-tests-asd (:use :cl :asdf))

(in-package :web4r-tests-asd)

(defsystem :web4r-tests
  :serial  t
  :name    "web4r-tests"
  :author  "Tomoyuki Matsumoto <tomoyuki28jp@no-spam@yahoo.co.jp>"
  :licence "BSD"
  :description "web4r tests"
  :depends-on  (:web4r :fiveam)
  :components  ((:module :tests
                         :components
                         ((:file "package")
                          (:file "util")
                          (:file "shtml")
                          (:file "validations")
                          (:file "pager")))))
