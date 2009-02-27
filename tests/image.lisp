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
  (is (dest-size= 100 100 50  50     50   50))
  (is (dest-size= 100 100 50  nil    50   50))
  (is (dest-size= 100 100 nil 50     50   50))
  (is (dest-size= 100 100 30  10     10   10))
  (is (dest-size= 230 160 100 100    100  70))
  (is (dest-size= 160 230 100 100    69  100))
  (is (dest-size= 150 100 100 100    100  67))
  (is (dest-size=  20 100 100 100     20 100))
  (is (dest-size=  100 20 100 100    100  20))
  (is (dest-size=  30 100 80 80       24  80)))

