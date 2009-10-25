(in-package :cl-user)

(defpackage :inflector-asd (:use :cl :asdf))

(in-package :inflector-asd)

(defsystem :inflector
  :version "0.1.0"
  :name    "inflector"
  :author  "Tomoyuki Matsumoto <tomoyuki28jp@gmail.com>"
  :licence "BSD"
  :description "pluralize and singularize a word"
  :depends-on  (:cl-ppcre)
  :components  ((:file "inflector")))
