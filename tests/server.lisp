(in-package :web4r-tests)
(in-suite web4r)

(defparameter *public-dir*
  (namestring (merge-pathnames "tests/public/" *web4r-dir*)))

(defun file-content= (file)
  (let* ((content (http-request (concat "http://localhost:8080/" file)))
         (e (if (stringp content) 'base-char '(unsigned-byte 8))))
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

(test server
  (let ((s (start-server (make-server :public-dir *public-dir* :port 8080))))
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

    (stop-server s)))
