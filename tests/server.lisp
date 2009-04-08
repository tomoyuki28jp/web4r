(in-package :web4r-tests)
(in-suite web4r)

(test page-uri
  (is (string= (page-uri "page")
               (concat *host-uri* "page/")))
  (is (string= (page-uri "page" 1)
               (concat *host-uri* "page/1/")))
  (is (string= (page-uri "page" 1 2)
               (concat *host-uri* "page/1/2/"))))

(test page
  (defpage test (path1 path2)
    (p "path1: " path1 " path2: " path2))
  (is (string= (http-request (page-uri "test" "foo" "bar"))
                  "path1: foo path2: bar"))
  (multiple-value-bind (body status-code)
      (http-request (page-uri "TEST"))
    (is (= status-code 404)))
  (defpage test/test (path1 path2)
    (p "path1: " path1 " path2: " path2))
  (is (string= (http-request (page-uri "test" "test" "foo" "bar"))
                  "path1: foo path2: bar")))

(test set/get-page
  (setf web4r::*pages* nil)
  (let ((pages '("a1" "a1/a2" "a1/a2/a3" "b1" "b1/b2" "b1/b2/b3")))
    (loop for p in pages do (set-page p p))
    (loop for p in pages do (is (string= p (get-page p))))))

(test msgs
  (defpage test1 () (p (apply #'join " " (slot-value (get-msgs) 'msgs))))
  (defpage test2 () (page/msgs "test1" '("m1" "m2")))
  (is (string= (http-request (page-uri "test2")) "m1 m2"))
  (defpage test3 () (redirect/msgs (page-uri "test1") '("m1" "m2")))
  (let ((c (make-instance 'cookie-jar)))
    (is (string= (http-request (page-uri "test3") :cookie-jar c) "m1 m2"))))
