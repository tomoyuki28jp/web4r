(in-package :cl-user)

(defpackage :web4r-asd (:use :cl :asdf))

(in-package :web4r-asd)

(defsystem :web4r
  :name    "web4r"
  :author  "Tomoyuki Matsumoto <tomoyuki28jp@no-spam@yahoo.co.jp>"
  :version "0.0.1"
  :licence "BSD"
  :description "Common Lisp web application framework"
  :depends-on  (:cl-ppcre :usocket :bordeaux-threads :cl-fad :clsql :elephant
                          :trivial-shell :flexi-streams :rfc2388-binary :cl-gd)
  :components  ((:module :src
                         :components
                         ((:file "package")
                          (:file "util")
                          (:file "shtml")
                          (:file "validations")
                          (:file "server")
                          (:file "pager")
                          (:file "pclass")
                          (:file "image")
                          (:file "user")))))
