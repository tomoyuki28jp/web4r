(in-package :cl-user)

(defpackage :web4r-tests-asd (:use :cl :asdf))

(in-package :web4r-tests-asd)

(defsystem :web4r-tests
 :serial  t
 :name    "web4r-tests"
 :author  "Tomoyuki Matsumoto <tomoyuki28jp@no-spam@yahoo.co.jp>"
 :licence "BSD"
 :description "Tests for web4r"
 :depends-on  (:fiveam :web4r :trivial-shell)
 :components  ((:module :tests
                :components
                ((:file "package")
                 (:file "util")
                 (:file "validations")
                 (:file "pager")))))
