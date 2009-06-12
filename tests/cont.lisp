(in-package :drakma)

; To avoid this bug: 
; http://common-lisp.net/pipermail/drakma-devel/2009-February/000750.html
(defun send-cookie-p (cookie uri force-ssl)
  (declare (ignore cookie uri force-ssl))
  t)

(in-package :web4r-tests)
(in-suite web4r)

(defvar *host-uri*
  "http://localhost:8080/")

(defun host-uri ()
  *host-uri*)

(defun matched (x regex)
  (multiple-value-bind (match regs)
      (scan-to-strings regex x)
    (declare (ignore match))
    (elt regs 0)))

(defun cid (x)
  (matched x "name=\"cid\" value=\"([^\"]+)\""))

(defun cont/uri (x)
  (matched x "<a href=\"(.+)\""))

(test renew-cont-lifetime
  (defpage test ()
    (let* ((web4r::*cid->cont* (make-hash-table :test 'equal))
           (web4r::*cid-generated-order* (make-array 0 :fill-pointer 0 :adjustable t))
           (cid1 (web4r::set-cont 1))
           (cid2 (web4r::set-cont 2))
           (cid3 (web4r::set-cont 3))
           (cont2 (gethash cid2 web4r::*cid->cont*))
           (time2 (web4r::cont-generated-time cont2)))
      (declare (ignore cid1 cid3))
      (sleep 1)
      (renew-cont-lifetime cid2)
      (let* ((cont2* (gethash cid2 web4r::*cid->cont*))
             (time2* (web4r::cont-generated-time cont2*)))
        (unless (= time2 time2*)
          (p "ok")))
      (when (equal (elt web4r::*cid-generated-order* 2) cid2)
        (p "ok"))))
  (is (string= (http-request (page-uri "test")) "okok")))

(test cont-session
  (defpage test ()
    (a/cont (progn (setf (cont-session 'key) "ok")
                   (a/cont (p (cont-session 'key)) "click here"))
      "click here"))
  (defpage test2 ()
    (p (cont-session 'key)))
  (let* ((c  (make-instance 'cookie-jar))
         (u1 (cont/uri (http-request (page-uri "test") :cookie-jar c)))
         (u2 (cont/uri (http-request u1 :cookie-jar c))))
    (is-false (string= "ok" (http-request (page-uri "test2") :cookie-jar c)))
    (is       (string= "ok" (http-request u2 :cookie-jar c))))
  (defpage test ()
    (a/cont (progn (setf (cont-session 'key) "ok")
                   (rem-cont-session   'key)
                   (a/cont (p (cont-session 'key)) "click here"))
      "click here"))
  (let* ((c  (make-instance 'cookie-jar))
         (u1 (cont/uri (http-request (page-uri "test") :cookie-jar c)))
         (u2 (cont/uri (http-request u1 :cookie-jar c))))
    (is-false (string= "ok" (http-request u2 :cookie-jar c)))))

(test a/cont
  (defpage test ()
    (let ((v "ok"))
      (a/cont (p v) "click here")))
  (is-false (string= "ok" (http-request
                           (cont/uri (http-request (page-uri "test"))))))
  (let* ((c (make-instance 'cookie-jar))
         (u (cont/uri (http-request (page-uri "test") :cookie-jar c))))
    (is-false (string= "ok" (http-request u)))
    (is       (string= "ok" (http-request u :cookie-jar c)))
    (is-false (string= "ok" (http-request u :cookie-jar c)))))

(test form/cont
  (defpage test ()
    (let ((v "ok"))
      (form/cont (p v) "click here")))
  (let* ((cookie (make-instance 'cookie-jar))
         (cid    (cid (http-request (page-uri "test") :cookie-jar cookie)))
         (args   (list *host-uri* :method :post :parameters `(("cid" . ,cid))))
         (args/c (append args `(:cookie-jar ,cookie))))
    (is-false (string= "ok" (apply #'http-request args)))
    (is       (string= "ok" (apply #'http-request args/c)))
    (is-false (string= "ok" (apply #'http-request args/c)))))

(test last-post
  (defpage test ()
    (form/cont (progn
                 [p (last-post "one")]
                 (form/cont [p (last-post "two")]
                   (input-text "two")))
      (input-text "one")))
  (let* ((c  (make-instance 'cookie-jar))
         (r1 (http-request (page-uri "test") :cookie-jar c))
         (c1 (cid r1))
         (r2 (http-request *host-uri* :method :post :cookie-jar c
                           :parameters `(("cid" ,@c1) ("one" . "1st"))))
         (c2 (cid r2))
         (r3 (http-request *host-uri* :method :post :cookie-jar c
                           :parameters `(("cid" ,@c2) ("two" . "2nd")))))
    (is (string= (subseq (remove #\Newline r2) 0 10) "<p>1st</p>"))
    (is (string= (remove #\Newline r3) "<p>2nd</p>")))

  (defpage test ()
    (form/cont (a/cont (p (last-post "one") (last-post "two")) "click")
      (input-text "one")
      (input-text "two")))
  (let* ((c  (make-instance 'cookie-jar))
         (r1 (http-request (page-uri "test") :cookie-jar c))
         (c1 (cid r1))
         (r2 (http-request *host-uri* :method :post :cookie-jar c :parameters
                           `(("cid" ,@c1) ("one" . "1st") ("two" . "2nd"))))
         (r3 (http-request (cont/uri r2) :cookie-jar c)))
    (is (string= r3 "1st2nd")))

  (defpage test ()
    (form/cont (a/cont
                   (a/cont (p (last-post "one") (last-post "two")) "click")
                 "click")
      (input-text "one")
      (input-text "two")))
  (let* ((c  (make-instance 'cookie-jar))
         (r1 (http-request (page-uri "test") :cookie-jar c))
         (c1 (cid r1))
         (r2 (http-request *host-uri* :method :post :cookie-jar c :parameters
                           `(("cid" ,@c1) ("one" . "1st") ("two" . "2nd"))))
         (r3 (http-request (cont/uri r2) :cookie-jar c))
         (r4 (http-request (cont/uri r3) :cookie-jar c)))
    (is (string= r4 "1st2nd")))

  (defpage test ()
    (form/cont (a/cont (p (last-post "foo")) "click")
      (input-text "foo")))
  (let* ((c  (make-instance 'cookie-jar))
         (r1 (http-request (page-uri "test") :cookie-jar c))
         (c1 (cid r1))
         (r2 (http-request *host-uri* :method :post :cookie-jar c :parameters
                           `(("cid" ,@c1) ("foo" . "ok"))))
         (r3 (http-request (cont/uri r2) :cookie-jar c)))
    (is (string= r3 "ok"))))

(test multipart-form/cont
  (defpage test ()
    (multipart-form/cont (p "ok")))
  (let* ((c  (make-instance 'cookie-jar))
         (r1 (http-request (page-uri "test") :cookie-jar c))
         (c1 (cid r1))
         (r2 (http-request *host-uri*
                           :method :post :cookie-jar c
                           :parameters `(("cid" ,@c1)))))
    (is (not (null (scan "enctype=\"multipart/form-data\"" r1))))
    (is (string= r2 "ok"))))
