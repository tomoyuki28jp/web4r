(in-package :web4r-tests)
(in-suite web4r)

(test with-post-parameters
  (with-post-parameters '(("foo" . "ok")
                          ("bar" . "ok"))
      (is (string= (hunchentoot:post-parameter "foo") "ok"))
      (is (string= (hunchentoot:post-parameter "bar") "ok"))))

(test with-get-parameters
  (with-get-parameters '(("foo" . "ok")
                          ("bar" . "ok"))
      (is (string= (hunchentoot:get-parameter "foo") "ok"))
      (is (string= (hunchentoot:get-parameter "bar") "ok"))))
