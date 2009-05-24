(in-package :web4r)
-
; --- Util ------------------------------------------------------

(defun mime-type (file)
  "Returns a string mime type of the FILE."
  (let* ((file  (namestring file))
         (type  (trivial-shell:shell-command (concat "file " file)))
         (s (split #\Space (string-trim '(#\Newline) type))))
    (cond ((equalp "image" (ignore-errors (subseq (nth 2 s) 0 5)))
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
  "Returns true if the FILE is a image file and nil otherwise."
  (when (image-type (mime-type file)) t))

(defun image-type (mime-type)
  (let ((s (split #\/ mime-type)))
    (when (and (string= (car s) "image")
               (member (nth 1 s) '("png" "jpeg" "gif") :test #'equalp))
      (->keyword (nth 1 s)))))

(defun image-path (file type)
  "Returns a pathname of the image FILE. The TYPE must be a key of
 *image-public-dirs* such as 'upload' or 'tmp'."
  (when-let (dir (cdr (assoc type *image-public-dirs* :test #'equal)))
    (and file (probe-file (merge-pathnames file (symbol-value dir))))))

; --- Thumbnail -------------------------------------------------

(defun dest-size (width height max-width max-height)
  (when (and max-width  (> width max-width))
    (setf height (round (* height  (/ max-width width)))
          width  max-width))
  (when (and max-height (> height max-height))
    (setf width (round (* width (/ max-height height)))
          height max-height))
  (values width height))

(defun thumbnail (file &key type width height)
  "Displays a thumbnail of the image FILE with the size of WIDTH and HEIGHT.
 If you don't specify the WIDTH or HEIGHT, the values of *thumbnail-width*
 or *thumbnail-height* are used. The TYPE must be a key of *image-public-dirs*
 such as 'upload' or 'tmp'."
  (let* ((file   (image-path file type))
         (mime   (aand file (mime-type it)))
         (type   (aand mime (image-type it)))
         (width  (or (aand width  (->int it)) *thumbnail-width*))
         (height (or (aand height (->int it)) *thumbnail-height*)))
    (if (or (null file) (null type))
        (handle-static-file (public-path "images/noimage.gif"))
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
  "Returns a thumbnail uri of the image FILE with the size of WIDTH and HEIGHT.
 The TYPE must be a key of *image-public-dirs* such as 'upload' or 'tmp'."
  (add-parameters (page-uri "thumbnail")
                  "file" file "type" type  "width" width "height" height))
