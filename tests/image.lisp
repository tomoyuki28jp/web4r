(in-package :web4r-tests)
(in-suite web4r)

(defun mime-type= (file mime-type)
  (equal (mime-type (test-file file)) mime-type))

(test mime-type
  (is (mime-type=  "test"      nil))
  (is (mime-type=  "test.css"  "text/css" ))
  (is (mime-type=  "test.gif"  "image/gif"))
  (is (mime-type=  "test.html" "text/html"))
  (is (mime-type=  "test.ico"  "image/x-icon"))
  (is (mime-type=  "test.jpeg" "image/jpeg"))
  (is (mime-type=  "test.js"   "application/x-javascript"))
  (is (mime-type=  "test.png"  "image/png"))
  (is (mime-type=  "test.txt"  nil))
  (is (mime-type=  "test.zip"  "application/zip")))

(defun image-file-p* (file)
  (image-file-p (test-file file)))

(test image-file-p
  (is-false (image-file-p* "test"))
  (is-false (image-file-p* "test.css"))
  (is       (image-file-p* "test.gif"))
  (is-false (image-file-p* "test.html"))
  (is-false (image-file-p* "test.ico"))
  (is       (image-file-p* "test.jpeg"))
  (is-false (image-file-p* "test.js"))
  (is       (image-file-p* "test.png"))
  (is-false (image-file-p* "test.txt"))
  (is-false (image-file-p* "test.zip")))

(defun image-type= (file type)
  (eq (web4r::image-type (mime-type (test-file file))) type))

(test image-type
  (is (image-type=  "test"      nil))
  (is (image-type=  "test.css"  nil))
  (is (image-type=  "test.gif"  :gif))
  (is (image-type=  "test.html" nil))
  (is (image-type=  "test.ico"  nil))
  (is (image-type=  "test.jpeg" :jpeg))
  (is (image-type=  "test.js"   nil))
  (is (image-type=  "test.png"  :png))
  (is (image-type=  "test.txt"  nil))
  (is (image-type=  "test.zip"  nil)))

(defun file-content= (file uri)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (let* ((length (file-length in))
           (array  (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence array in)
      (equalp array (http-request uri)))))

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
  (is (equal (concat "/thumbnail/"
                     "?file=test.gif&type=&width=250&height=200")
             (thumbnail-uri "test.gif" :width 250 :height 200)))
  (is (equal (concat "/thumbnail/"
                     "?file=test.png&type=&width=200&height=250")
             (thumbnail-uri "test.png" :width 200 :height 250))))
