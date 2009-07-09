(in-package :web4r-tests)
(in-suite web4r)

(test genpages
  (defpclass wiki ()
      ((title :length 256 :index t)
       (body  :length 3000)))
  (genpages wiki)

  (let ((class 'wiki))
    (with-new-cookie
      (let ((oid (ignore-errors
                   (http-make-instance class :title "title1" :body "body1"))))
        (is-true (not (null oid)))
        (is-true (http-test-get-instance-by-oid class oid))
        (is-true (http-test-update-instance
                  class oid '((title . "title1c") (body . "body1c"))))
        (is-true (http-test-drop-instance-by-oid class oid))))))
