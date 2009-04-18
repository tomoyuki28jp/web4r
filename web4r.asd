(in-package :cl-user)

(defpackage :web4r-asd (:use :cl :asdf))

(in-package :web4r-asd)

(defsystem :web4r
  :version "0.1.9"
  :name    "web4r"
  :author  "Tomoyuki Matsumoto <tomoyuki28jp@gmail.com>"
  :licence "BSD"
  :description "Common Lisp web application framework"
  :depends-on  (:my-util
                :sml
                :inflector
                :hunchentoot
                :bordeaux-threads
                :elephant
                :trivial-shell
                :cl-gd
                :flexi-streams
                :trivial-backtrace)
  :components  ((:module "src"
                         :serial  t
                         :components
                         ((:file "package")
                          (:file "specials")
                          (:file "util")
                          (:file "debug")
                          (:file "cont")
                          (:file "server")
                          (:file "valid")
                          (:file "pager")
                          (:file "pclass")
                          (:file "pages")
                          (:file "user")
                          (:file "image")
                          ))))
