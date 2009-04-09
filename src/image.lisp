(in-package :web4r)
-
; --- Util ------------------------------------------------------

(defun mime-type (file)
  (let* ((file  (namestring file))
         (type  (trivial-shell:shell-command (concat "file " file)))
         (s (split #\Space (string-trim '(#\Newline) type))))
    (cond ((equalp "image" (nth 2 s))
           (cond ((equalp "PNG"  (nth 1 s)) "image/png")
                 ((equalp "JPEG" (nth 1 s)) "image/jpeg")
                 ((equalp "GIF"  (nth 1 s)) "image/gif")))
          ((or (equalp "icon" (car (last s)))
               (equalp "icon" (nth 3 s)))   "image/x-icon")
          ((equalp "HTML" (nth 1 s))        "text/html")
          ((equalp "text" (car (last s)))
           (let ((ext (string-downcase (car (last (split #\. file))))))
             (cond ((equal "js" ext)        "application/x-javascript")
                   ((equal "css" ext)       "text/css")
                   (t nil))))
          ((equalp "Zip"   (nth 1 s))       "application/zip"))))

(defun image-file-p (file)
  (when (image-type (mime-type file)) t))

(defun image-type (mime-type)
  (let ((s (split #\/ mime-type)))
    (when (and (string= (car s) "image")
               (member (nth 1 s) '("png" "jpeg" "gif") :test #'equalp))
      (->keyword (nth 1 s)))))

(defun image-file-path (file type)
  (let* ((dir  (image-dir type))
         (name (aand file (file-namestring it)))
         (file (and dir name (merge-pathnames file dir))))
    (when (probe-file file)
      file)))

; --- Image -----------------------------------------------------

(defun serve-image (file type)
  (aif (image-file-path file type)
       (handle-static-file it)
       (noimage)))

(defpage image (:get file type)
  (serve-image file type))

(defun image-uri (file &key type thumbnail)
  (let* ((file (or file ""))
         (type (or type (image-dir-type (directory-namestring file))))
         (name (file-namestring file)))
    (add-parameters (page-uri (if thumbnail "thumbnail" "image"))
                    "file" name "type" type)))

; --- No image --------------------------------------------------

(defun noimage ()
  (handle-static-file
   (merge-pathnames "images/noimage.gif" *web4r-dir*)))

(defpage noimage ()
  (noimage))

(defun noimage-uri ()
  (page-uri "noimage"))

; --- Thumbnail -------------------------------------------------

(defvar *thumbnail-width*  100)
(defvar *thumbnail-height* 100)

(defun dest-size (width height max-width max-height)
  (when (and max-width  (> width max-width))
    (setf height (round (* height  (/ max-width width)))
          width  max-width))
  (when (and max-height (> height max-height))
    (setf width (round (* width (/ max-height height)))
          height max-height))
  (values width height))

(defun thumbnail (file &key type width (height *thumbnail-height*))
  (let* ((file   (image-file-path file type))
         (mime   (aand file (mime-type it)))
         (type   (aand mime (image-type it)))
         (width  (or (aand width  (->int it)) *thumbnail-width*))
         (height (or (aand height (->int it)) *thumbnail-width*)))
    (if (or (null file) (null type))
        (noimage)
        (cl-gd:with-image-from-file (im file type)
          (multiple-value-bind (w h) (cl-gd:image-size im)
            (multiple-value-bind (dw dh) (dest-size w h width height)
              (if (and (= w dw) (= h dh))
                  (handle-static-file file)
                  (throw 'web4r-dispatcher
                    (flexi-streams:with-output-to-sequence (s :element-type :octet)
                      (cl-gd:with-image (new dw dh)
                        (setf (content-type*) mime)
                        (cl-gd:copy-image im new 0 0 0 0 w h :resize t
                                          :dest-width dw :dest-height dh)
                        (cl-gd:write-image-to-stream s type :image new)))))))))))

(defpage thumbnail (:get file type width height)
  (thumbnail file :type type :width width :height height))

(defun thumbnail-uri (file &key type width height)
  (add-parameters (image-uri file :type type :thumbnail t)
                  "width" width "height" height))
