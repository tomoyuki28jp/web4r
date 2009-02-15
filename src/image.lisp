(in-package :web4r)

; --- No image --------------------------------------------------

(defun noimage ()
  (serve-file (concat *web4r-dir* "images/noimage.gif")
              :public-file-only nil))

(set-page 'noimage (lambda () (noimage)))

(defun noimage-uri ()
  (concat (host-uri) "noimage"))

; --- Thumbnail -------------------------------------------------

(defvar *thumbnail-width*  100)
(defvar *thumbnail-height* 100)

(defun image-file-p (file)
  (when (image-type (content-type file))
    t))

(defun image-type (content-type)
  (let ((s (split "/" (mkstr content-type))))
    (when (and (string= (car s) "image")
               (member (nth 1 s) '("png" "jpeg" "gif") :test #'equalp))
      (make-keyword (nth 1 s)))))

(defun dest-size (width height max-width max-height)
  (when (and max-width  (> width max-width))
    (setf height (round (* height  (/ max-width width)))
          width  max-width))
  (when (and max-height (> height max-height))
    (setf width (round (* width (/ max-height height)))
          height max-height))
  (values width height))

(defun thumbnail (file &key width height (public-file-only t))
  (let* ((mime (content-type file))
         (type (image-type mime)))
    (if (or (not (is-readable file))
            (null type)
            (and public-file-only (not (public-file-p file))))
        (noimage)
        (cl-gd:with-image-from-file (im file type)
               (multiple-value-bind (w h) (cl-gd:image-size im)
                 (multiple-value-bind (dw dh) (dest-size w h width height)
                   (if (and (= w dw) (= h dh))
                       (serve-file file)
                       (cl-gd:with-image (new dw dh)
                              (setf (response-content-type *response*) mime)
                              (setf (response-charset *response*) nil)
                              (cl-gd:copy-image im new 0 0 0 0 w h :resize t
                                     :dest-width dw :dest-height dh)
                              (cl-gd:write-image-to-stream
                                     *http-binary-stream* type
                                     :image new)))))))))

(defpage thumbnail (:get file width height)
  (thumbnail (concat (public-dir) file)
             :width (->int width) :height (->int height)))

(defun thumbnail-uri (file &key (width  *thumbnail-width*)
                                (height *thumbnail-height*))
  (add-get-params (page-uri "thumbnail")
                  "file" file "width" width "height" height))
