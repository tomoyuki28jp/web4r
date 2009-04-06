(in-package :cl-user)

(defpackage :web4r-tests
  (:use :cl :web4r :5am :drakma)
  (:import-from :web4r :set-page :get-page)
  (:shadowing-import-from
   :hunchentoot :*header-stream* :cookie-path :cookie-expires
   :cookie-domain :parameter-error :cookie-name :cookie-value)
  (:shadowing-import-from :web4r :pass))

(in-package :web4r-tests)

(def-suite web4r)

(setf *indent-mode* nil)

(defvar *test-dir*
  (merge-pathnames "tests/" *web4r-dir*))

(defvar *test-file-dir*
  (merge-pathnames "files/" *test-dir*))

(defun test-file (file)
  (merge-pathnames file *test-file-dir*))
