(in-package :web4r-tests)
(in-suite web4r)

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
           ; status code
           (is (status-code= "test"      200))
           (is (status-code= "test.css"  200))
           (is (status-code= "test.gif"  200))
           (is (status-code= "test.html" 200))
           (is (status-code= "test.ico"  200))
           (is (status-code= "test.jpeg" 200))
           (is (status-code= "test.js"   200))
           (is (status-code= "test.png"  200))
           (is (status-code= "test.txt"  200))
           (is (status-code= "test.zip"  200))
           (is (status-code= "../../../../../../etc/passwd" 400))
           (is (status-code= "nopage"    404))
           ; content type
           (is (content-type= "test"       nil))
           (is (content-type= "test.css"  "text/css" ))
           (is (content-type= "test.gif"  "image/gif"))
           (is (content-type= "test.html" "text/html"))
           (is (content-type= "test.ico"  "image/x-icon"))
           (is (content-type= "test.jpeg" "image/jpeg"))
           (is (content-type= "test.js"   "application/x-javascript"))
           (is (content-type= "test.png"  "image/png"))
           (is (content-type= "test.txt"   nil))
           (is (content-type= "test.zip"  "application/zip"))
           ; rewrite rule
           (setf (web4r::server-rewrite-rule s) 'test-rewrite-rule)
           (is (file-content= "test.html"
                              (http-request "http://localhost:8080/test.gif")))
           (is (file-content= "test.html"
                              (http-request "http://localhost:8080/test.png")))
           (is (file-content= "test.html"))
           (is (file-content= "test.js"))
           (is (file-content= "test.css"))
           (setf (web4r::server-rewrite-rule s) nil))
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
      (stop-server s))))
