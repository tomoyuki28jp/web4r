(in-package :web4r-tests)
(in-suite web4r)

(test noimage
  (with-open-file (in (merge-pathnames "images/noimage.gif" *web4r-dir*)
                      :element-type '(unsigned-byte 8))
    (let* ((length (file-length in))
           (array  (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence array in)
      (is (equalp array (http-request (concat "http://localhost:8080/noimage")))))))

(defun image-file-p* (file)
  (image-file-p (merge-pathnames file *test-public-dir*)))

(test image-file-p
  (is (eq nil (image-file-p* "test")))
  (is (eq nil (image-file-p* "test.css")))
  (is (eq t   (image-file-p* "test.gif")))
  (is (eq nil (image-file-p* "test.html")))
  (is (eq nil (image-file-p* "test.ico")))
  (is (eq t   (image-file-p* "test.jpeg")))
  (is (eq nil (image-file-p* "test.js")))
  (is (eq t   (image-file-p* "test.png")))
  (is (eq nil (image-file-p* "test.txt")))
  (is (eq nil (image-file-p* "test.zip"))))

(defun dest-size= (width height max-width max-height w h)
  (multiple-value-bind (w* h*)
      (web4r::dest-size width height max-width max-height)
    (and (= w w*) (= h h*))))

(test dest-size
  (is (dest-size= 100 100 120 120    100 100))
  (is (dest-size= 100 100 50  50     50  50))
  (is (dest-size= 100 100 50  nil    50  50))
  (is (dest-size= 100 100 nil 50     50  50))
  (is (dest-size= 100 100 30  10     10  10))
  (is (dest-size= 230 160 100 100    100 70))
  (is (dest-size= 160 230 100 100    69  100))
  (is (dest-size= 150 100 100 100    100 67))
  (is (dest-size= 20  100 100 100    20  100))
  (is (dest-size= 100 20  100 100    100 20))
  (is (dest-size= 30  100 80  80     24  80)))

(test thumbnail-uri
  (is (equal "http:///thumbnail/?file=test.gif&width=250&height=200"
             (thumbnail-uri "test.gif" :width 250 :height 200)))
  (is (equal "http:///thumbnail/?file=test.png&width=200&height=250"
             (thumbnail-uri "test.png" :width 200 :height 250))))

(test thumbnail
  (defpage thumbnail-test-200 (:get file)
    (when (member file '("test.gif" "test.jpeg" "test.png") :test #'equalp)
      (setf file (concat (public-dir) file))
      (let* ((mime (web4r::content-type file))
             (type (image-type mime)))
        (cl-gd:with-image-from-file (im file type)
          (multiple-value-bind (w h) (cl-gd:image-size im)
            (multiple-value-bind (dw dh) (web4r::dest-size w h 200 200)
              (cl-gd:with-image (new dw dh)
                (setf (web4r::response-content-type *response*) mime)
                (setf (web4r::response-charset *response*) nil)
                (cl-gd:copy-image im new 0 0 0 0 w h :resize t
                                  :dest-width dw :dest-height dh)
                (cl-gd:write-image-to-stream
                 *http-binary-stream* type
                 :image new))))))))
  (is (file-content= "test.gif"
                     (http-request "http://localhost:8080/thumbnail/?file=test.gif&width=250&height=250")))
  (is (file-content= "test.png"
                     (http-request "http://localhost:8080/thumbnail/?file=test.png&width=250&height=250")))
  (is (file-content= "test.jpeg"
                     (http-request "http://localhost:8080/thumbnail/?file=test.jpeg&width=250&height=250")))
  (is (equalp (http-request "http://localhost:8080/thumbnail-test-200?file=test.gif")
              (http-request "http://localhost:8080/thumbnail/?file=test.gif&width=200&height=200")))
  (is (equalp (http-request "http://localhost:8080/thumbnail-test-200?file=test.png")
              (http-request "http://localhost:8080/thumbnail/?file=test.png&width=200&height=200")))
  (is (equalp (http-request "http://localhost:8080/thumbnail-test-200?file=test.jpeg")
              (http-request "http://localhost:8080/thumbnail/?file=test.jpeg&width=200&height=200"))))
