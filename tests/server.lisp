(in-package :web4r-tests)
(in-suite web4r)

(test page-uri
  (is (string= (page-uri "page")
               (concat *host-uri* "page/")))
  (is (string= (page-uri "page" 1)
               (concat *host-uri* "page/1/")))
  (is (string= (page-uri "page" 1 2)
               (concat *host-uri* "page/1/2/"))))

(test set-post-parameters
  (defpage test ()
    (set-post-parameters '(("foo" . "ok")))
    (p (hunchentoot:post-parameter "foo")))
  (is (string= (http-request (page-uri "test")) "ok")))

(test set-get-parameters
  (defpage test ()
    (set-get-parameters '(("foo" . "ok")))
    (p (hunchentoot:get-parameter "foo")))
  (is (string= (http-request (page-uri "test")) "ok")))

(test page
  (defpage test () (p "ok"))
  (is (string= (http-request (page-uri "test")) "ok"))
  (defpage test (path1 path2)
    (p "path1: " path1 " path2: " path2))
  (is (string= (http-request (page-uri "test" "foo" "bar"))
                  "path1: foo path2: bar"))
  (defpage test/test (path1 path2)
    (p "path1: " path1 " path2: " path2))
  (is (string= (http-request (page-uri "test" "test" "foo" "bar"))
                  "path1: foo path2: bar"))
  (defpage test (:get g1 g2) (p g1 " " g2))
  (is (string= (http-request
                (add-parameters (page-uri "test") "g1" "v1" "g2" "v2"))
               "v1 v2"))
  (defpage test (:post p1 p2) (p p1 " " p2))
  (is (string= (http-request (page-uri "test") :method :post
                             :parameters '(("p1" . "v1") ("p2" . "v2")))
               "v1 v2"))
  (defpage test/test (path1 path2 :post p1 p2 :get g1 g2)
    (p (join " " path1 path2 p1 p2 g1 g2)))
  (is (eq nil (http-request (page-uri "test" "test"))))
  (is (string= (http-request
                (add-parameters (page-uri "test" "test" "pp1" "pp2")
                                "g1" "gv1" "g2" "gv2")
                :method :post :parameters '(("p1" . "pv1") ("p2" . "pv2")))
               "pp1 pp2 pv1 pv2 gv1 gv2"))
  (defpage test/test ((path1 "dpp1") (path2 "dpp2")
                      :post (p1 "dp1") (p2 "dp2") :get (g1 "dg1") (g2 "dg2"))
    (p (join " " path1 path2 p1 p2 g1 g2)))
  (is (string= (http-request (page-uri "test" "test"))
               "dpp1 dpp2 dp1 dp2 dg1 dg2"))
  (is (string= (http-request
                (add-parameters (page-uri "test" "test" "pp1" "pp2")
                                "g1" "gv1" "g2" "gv2")
                :method :post :parameters '(("p1" . "pv1") ("p2" . "pv2")))
               "pp1 pp2 pv1 pv2 gv1 gv2")))

(test set/get-page
  (setf web4r::*pages* nil)
  (let ((pages '("a1" "a1/a2" "a1/a2/a3" "b1" "b1/b2" "b1/b2/b3")))
    (loop for p in pages do (set-page p p))
    (loop for p in pages do (is (string= p (get-page p))))))

(test msgs
  (defpage test1 ()
    (let ((msgs (get-msgs)))
      (p (if (typep msgs 'msgs) "msgs " "error ")
         (apply #'join " " (slot-value msgs 'msgs)))))
  (defpage test2 () (page/msgs "test1" '("m1" "m2")))
  (defpage test3 () (page/error-msgs "test1" '("m1" "m2")))
  (is (string= (http-request (page-uri "test2")) "msgs m1 m2"))
  (is (string= (http-request (page-uri "test3")) "error m1 m2"))
  (defpage test4 () (redirect/msgs (page-uri "test1") '("m1" "m2")))
  (defpage test5 () (redirect/error-msgs (page-uri "test1") '("m1" "m2")))
  (let ((c (make-instance 'cookie-jar)))
    (is (string= (http-request (page-uri "test4") :cookie-jar c)
                 "msgs m1 m2")))
  (let ((c (make-instance 'cookie-jar)))
    (is (string= (http-request (page-uri "test5") :cookie-jar c)
                 "error m1 m2"))))
