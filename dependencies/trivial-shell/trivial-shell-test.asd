#|

Author: Gary King
|#

(defpackage :trivial-shell-test-system (:use #:cl #:asdf))
(in-package :trivial-shell-test-system)

(defsystem trivial-shell-test
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Tests for trivial-shell"
  :components ((:module "tests"
		        :components ((:file "tests"))))
  :depends-on (lift trivial-shell))


