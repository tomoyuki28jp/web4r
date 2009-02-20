(in-package :cl-user)

(defpackage :web4r-tests-asd (:use :cl :asdf))

(in-package :web4r-tests-asd)

(defsystem :web4r-tests
 :name    "web4r-tests"
 :author  "Tomoyuki Matsumoto <tomoyuki28jp@no-spam@yahoo.co.jp>"
 :version "0.0.1"
 :licence "BSD"
 :description "Tests for web4r"
 :depends-on  (:fiveam :web4r :trivial-shell)
 :components  ((:module :tests
                :components
                ((:file "package")
                 (:file "util")
                 (:file "validations")
                 (:file "pager")))))
