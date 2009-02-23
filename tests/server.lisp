(in-package :web4r-tests)
(in-suite web4r)

#| todo
multipart-form
|#

(defparameter *public-dir*
  (namestring (merge-pathnames "tests/public/" *web4r-dir*)))

(defun file-content= (file &optional content)
  (unless content
    (setf content (http-request (concat "http://localhost:8080/" file))))
  (let ((e (if (stringp content) 'base-char '(unsigned-byte 8))))
    (with-open-file (in (merge-pathnames file *public-dir*) :element-type e)
      (let* ((length (file-length in))
             (array  (make-array length :element-type e)))
        (read-sequence array in)
        (equalp array content)))))

(defun status-code= (uri status-code)
  (multiple-value-bind (body status-code* headers uri stream close reason-phrase)
      (http-request (concat "http://localhost:8080/" uri))
    (declare (ignore body headers uri stream close reason-phrase))
    (= status-code* status-code)))

(defun content-type= (uri content-type)
  (multiple-value-bind (body status-code headers uri stream close reason-phrase)
      (http-request (concat "http://localhost:8080/" uri))
    (declare (ignore body status-code uri stream close reason-phrase))
    (equalp content-type
            (awhen (assoc-ref :content-type headers)
              (car (split "; " it))))))

(defun test-rewrite-rule (uri)
  (when (aand (preg-match "(\.ico|\.gif|\.jpeg|\.jpg|\.png|)$" uri)
              (not (string= it "")))
    "/test.html"))

(defun matched-page ()
  (html/
   (body/
    (p/ "matched"))))

(defun defpage-test1 ()
  (html/
   (body/
    (p/ "defpage-test1"))))

; Drakma has a bug which drakma don't send cookies when uri is root directory
; even if cookie path is set to "/". This function is to avoid the bug.
(defun http-request* (uri &rest args)
  (apply #'http-request
         (append (list (replace-str "http://localhost:8080/"
                                    "http://localhost:8080//" uri))
                 args)))

(test server
  (let ((s (start-server
            (make-server :public-dir *public-dir* :port 8080 :timeout-sec 3))))
    (unwind-protect
         (progn
           ; static file content
           (is (file-content= "test"))
           (is (file-content= "test.css"))
           (is (file-content= "test.gif"))
           (is (file-content= "test.html"))
           (is (file-content= "test.ico"))
           (is (file-content= "test.jpeg"))
           (is (file-content= "test.js"))
           (is (file-content= "test.png"))
           (is (file-content= "test.txt"))
           (is (file-content= "test.zip"))
           (is-true (shtml= (web4r::%status-page 400)
                            (http-request "http://localhost:8080/../../../../../../etc/passwd")))
           (is-true (shtml= (web4r::%status-page 404)
                            (http-request "http://localhost:8080/nopage")))
           ; get-params
           (defpage get-params-test () (p/ (get-params)))
           (is-true (shtml= (p/ '(("k1" . "v1")("k2" . "v2")("k3" . "v3")))
                            (http-request
                             "http://localhost:8080/get-params-test?k1=v1&k2=v2&k3=v3")))
           ; get-param
           (defpage get-param-test () (p/ (get-param "k1")))
           (is-true (shtml= (p/ "v1")
                            (http-request
                             "http://localhost:8080/get-param-test?k1=v1")))
           ; post-params
           (defpage post-params-test () (p/ (post-params)))
           (is-true (shtml= (p/ '(("k1" . "v1")("k2" . "v2")))
                            (http-request
                             "http://localhost:8080/post-params-test"
                             :method :post
                             :parameters '(("k1" . "v1") ("k2" . "v2")))))
           ; post-param
           (defpage post-param-test () (p/ (post-param "k1")))
           (is-true (shtml= (p/ "v1")
                            (http-request
                             "http://localhost:8080/post-param-test"
                             :method :post :parameters '(("k1" . "v1")))))
           ; defpage
           (defpage defpage-test1 () (defpage-test1))
           (is-true (shtml= (defpage-test1)
                            (http-request "http://localhost:8080/defpage-test1")))
           (defpage defpage-test2 (p1 p2) (p/ p1 p2))
           (is-true (shtml= (p/ "pv1" "pv2")
                            (http-request "http://localhost:8080/defpage-test2/pv1/pv2/")))
           (defpage defpage-test3 (:get p1 p2) (p/ p1 p2))
           (is-true (shtml= (p/ "pv1" "pv2")
                            (http-request
                             "http://localhost:8080/defpage-test3?p1=pv1&p2=pv2")))
           (defpage defpage-test4 (:post p1 p2) (p/ p1 p2))
           (is-true (shtml= (p/ "pv1" "pv2")
                            (http-request
                             "http://localhost:8080/defpage-test4"
                             :method :post :parameters '(("p1" . "pv1") ("p2" . "pv2")))))
           ; page-lambda
           (defpage page-lambda-test1 ()
             (form/cont/ (page-lambda (:post foo) (p/ foo))
               (input-text/ "foo")))
           (let ((c (make-instance 'cookie-jar)))
             (multiple-value-bind (match regs)
                 (preg-match "NAME=\"cid\" VALUE=\"(.+)\""
                             (http-request
                              "http://localhost:8080/page-lambda-test1"
                              :cookie-jar c))
               (is-true (shtml= (p/ "ok")
                                (http-request  "http://localhost:8080//"
                                               :method :post
                                               :parameters
                                               (list (cons "cid" (elt regs 0))
                                                     (cons "foo" "ok"))
                                               :cookie-jar c)))))
           ; default-page
           (is-true (shtml= (default-page)
                            (http-request "http://localhost:8080/")))
           ; status code
           (is (status-code= "test"           200))
           (is (status-code= "test.css"       200))
           (is (status-code= "test.gif"       200))
           (is (status-code= "test.html"      200))
           (is (status-code= "test.ico"       200))
           (is (status-code= "test.jpeg"      200))
           (is (status-code= "test.js"        200))
           (is (status-code= "test.png"       200))
           (is (status-code= "test.txt"       200))
           (is (status-code= "test.zip"       200))
           (is (status-code= "../../../../../../etc/passwd" 400))
           (is (status-code= "nopage"         404))
           (is (status-code= "defpage-test1"  200))
           (is (status-code= ""               200))
           ; content type
           (is (content-type= "test"          nil))
           (is (content-type= "test.css"      "text/css" ))
           (is (content-type= "test.gif"      "image/gif"))
           (is (content-type= "test.html"     "text/html"))
           (is (content-type= "test.ico"      "image/x-icon"))
           (is (content-type= "test.jpeg"     "image/jpeg"))
           (is (content-type= "test.js"       "application/x-javascript"))
           (is (content-type= "test.png"      "image/png"))
           (is (content-type= "test.txt"      nil))
           (is (content-type= "test.zip"      "application/zip"))
           (is (content-type= "defpage-test1" "text/html"))
           (is (content-type= ""              "text/html"))
           ; rewrite rule
           (setf (web4r::server-rewrite-rule s) 'test-rewrite-rule)
           (is (file-content= "test.html"
                              (http-request "http://localhost:8080/test.gif")))
           (is (file-content= "test.html"
                              (http-request "http://localhost:8080/test.png")))
           (is (file-content= "test.html"))
           (is (file-content= "test.js"))
           (is (file-content= "test.css"))
           (setf (web4r::server-rewrite-rule s) nil)
           ; static route
           (add-route s :static "/nopage" 'matched-page)
           (is-true (shtml= (matched-page)
                            (http-request "http://localhost:8080/nopage")))
           (rem-route s :static "/nopage")
           (is-true (shtml= (web4r::%status-page 404)
                            (http-request "http://localhost:8080/nopage")))
           ; regex route
           (is-true (shtml= (web4r::%status-page 404)
                            (http-request "http://localhost:8080/matched1")))
           (add-route s :regex "^/matched" 'matched-page)
           (is-true (shtml= (matched-page)
                            (http-request "http://localhost:8080/matched1")))
           (is-true (shtml= (web4r::%status-page 404)
                            (http-request "http://localhost:8080/no/matched1")))
           (is-true (shtml= (matched-page)
                            (http-request "http://localhost:8080/matched2")))
           (rem-route s :regex "^/matched")
           (is-true (shtml= (web4r::%status-page 404)
                            (http-request "http://localhost:8080/matched1")))
           ; cookie
           (let ((r (random-hex-string 10)))
             (defpage cookie-set-test () (set-cookie "cookie-set-test1" r))
             (defpage cookie-get-test () (p/ (get-cookie "cookie-set-test1")))
             (let ((cookie (make-instance 'cookie-jar)))
               (http-request "http://localhost:8080/cookie-set-test" :cookie-jar cookie)
               (is-true (shtml= (p/ r)
                                (http-request "http://localhost:8080/cookie-get-test"
                                              :cookie-jar cookie)))))
           ; session
           (let ((r (random-hex-string 10)))
             (defpage session-set-test () (set-session "session-set-test1" r))
             (defpage session-get-test () (p/ (get-session "session-set-test1")))
             (let ((cookie (make-instance 'cookie-jar)))
               (http-request "http://localhost:8080/session-set-test" :cookie-jar cookie)
               (is-true (shtml= (p/ r)
                                (http-request "http://localhost:8080/session-get-test"
                                              :cookie-jar cookie)))))
           ; a/cont/
           (defpage a-cont-test ()
             (let ((v "ok"))
               (a/cont/ (p/ v) "click here")))
           (multiple-value-bind (match regs)
               (preg-match "<A HREF=\"(.+)\""
                           (http-request "http://localhost:8080/a-cont-test"))
             (is-false (shtml= (p/ "ok") (http-request* (elt regs 0)))))
           (let ((c (make-instance 'cookie-jar)))
             (multiple-value-bind (match regs)
                 (preg-match "<A HREF=\"(.+)\""
                             (http-request "http://localhost:8080/a-cont-test"
                                           :cookie-jar c))
               (is-true (shtml= (p/ "ok")
                                (http-request* (elt regs 0) :cookie-jar c)))))
           ; form/cont
           (defpage form-cont-test ()
             (let ((v "ok"))
               (form/cont/ (p/ v) "click here")))
           (let ((c (make-instance 'cookie-jar)))
             (multiple-value-bind (match regs)
                 (preg-match "NAME=\"cid\" VALUE=\"(.+)\""
                             (http-request
                              "http://localhost:8080/form-cont-test"
                              :cookie-jar c))
               (is-true (shtml= (p/ "ok")
                                (http-request  "http://localhost:8080//"
                                               :method :post
                                               :parameters
                                               (list (cons "cid" (elt regs 0)))
                                               :cookie-jar c)))))
           ; msgs
           (defpage msg-test1 () (p (set-msgs "ok")))
           (defpage msg-test2 () (p (get-msgs)))
           (is (equal nil
                      (http-request
                       (concat "http://localhost:8080/msg-test2?mid="
                               (http-request
                                "http://localhost:8080/msg-test1")))))
           (let ((c (make-instance 'cookie-jar)))
             (is (equal "(ok)"
                        (http-request
                         (concat "http://localhost:8080/msg-test2?mid="
                                 (http-request
                                  "http://localhost:8080/msg-test1"
                                  :cookie-jar c))
                         :cookie-jar c))))
           )
      (stop-server s))))

;(defparameter *srv* (start-server (make-server)))
;(stop-server *srv*)
;
;  (let ((s (start-server (make-server))))
;    (unwind-protect
;         (progn
;           )
;      (stop-server s)))
