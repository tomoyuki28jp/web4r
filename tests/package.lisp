(in-package :cl-user)

(defpackage :web4r-tests
  (:use :cl :web4r :5am)
  (:shadowing-import-from :web4r :pass))

(in-package :web4r-tests)

(def-suite web4r-tests)
