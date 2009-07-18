(in-package :web4r)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun web4r-path (file)
    "Returns a pathname of the FILE under the web4r directory."
    (merge-pathnames file *web4r-dir*))

  (defun public-path (file)
    "Returns a pathname of the FILE under the public directory."
    (merge-pathnames file *public-dir*))

  (defun sml-path (file)
    "Returns a pathname of the FILE under the sml directory."
    (merge-pathnames file *sml-dir*))

  (defun example-path (file)
    "Returns a pathname of the FILE under the examples directory."
    (merge-pathnames
     file (merge-pathnames "examples/" *web4r-dir*))))

(defmacro load-sml-path (path &rest args)
  "Loads a sml file placed in the PATH under the sml directory."
  `(load-sml (sml-path ,path) ,@args))

(defun assoc* (idx alist &key (test #'eql))
  (reduce (lambda (alist idx)
            (when (consp alist) (cdr (assoc idx alist :test test))))
          (append (list alist) idx)))

(defun replace-assoc* (idx alist obj &key (test #'eql))
  (labels
      ((fn (new old)
         (acond ((atom old) new)
                ((atom new) (when new (append old (list new))))
                ((position-if
                  #'(lambda (x) (funcall test (car x) (caar new)))
                  old)
                 (setf (cdr (nth it old))
                       (fn (cdar new) (cdr (nth it old))))
                 old)
                (t (append old new)))))
    (fn (reduce #'(lambda (x y) (list (cons x y)))
                (append idx (list obj)) :from-end t)
        alist)))

(defun add-parameter (link key value)
  "Returns the LINK with a get parameter named KEY with the VALUE.

 Examples:
  (add-parameter \"http://host/\" \"k1\" \"v1\") ;=> \"http://host/?k1=v1\""
  (let ((param (concat key "=" value)))
    (multiple-value-bind (replaced matchp)
        (regex-replace (concat "(" key  "=[^&]+)") link param)
      (if matchp
          replaced
          (progn
            (unless (position #\? link)
              (setf link (concat link "?")))
            (if (string= (subseq link (1- (length link))) "?")
                (concat link param)
                (concat link "&" param)))))))

(defun add-parameters (link &rest parameters)
  "Returns the LINK with get PARAMETERS.

 Examples:
  (add-parameters \"http://host/\" \"k1\" \"v1\" \"k2\" \"v2\")
  ;=> \"http://host/?k1=v1&k2=v2\""
  (loop for (k v) on parameters by #'cddr
        as l = (add-parameter link k v) then (add-parameter l k v)
        finally (return l)))

(defun rem-parameter (link key)
  "Returns the LINK after removing a get parameter named KEY

 Examples:
  (rem-parameter \"http://host/?k1=v1\" \"k1\") ;=> \"http://host/\""
  (let* ((new (regex-replace (concat "(" key  "=[^&]+&?)") link ""))
         (len (length new)))
    (if (member (subseq new (1- len)) '("?" "&") :test #'equal)
        (subseq link 0 (1- len))
        new)))

(defun omit (obj maxlength &optional (omark "..."))
  "If the length of OBJ exceeds the MAXLENGTH, replaces the exceeded
 characters with the OMARK. Returns the OBJ. Note that if the OBJ is
 an instance of the sml::safe class, this returns the OBJ without
 making any change.

 Examples:
  (omit \"12345\" 3) ;=>  \"123...\""
  (if (typep obj 'safe)
      obj
      (let ((str (->string obj)))
        (if (<= (length str) maxlength)
            str
            (concat (subseq str 0 maxlength) omark)))))

(defun file-length* (file)
  (with-open-file (s file :if-does-not-exist nil)
    (aand s (file-length it))))

(defun time-format (format &optional (time (get-universal-time)))
  "Returns formatted string TIME according to the FORMAT.
  +--------+-----------------+
  | format |   description   |
  +--------+-----------------+
  | ~y     | year   (~4,'0d) |
  | ~m     | month  (~2,'0d) |
  | ~d     | date   (~2,'0d) |
  | ~h     | hour   (~2,'0d) |
  | ~i     | minute (~2,'0d) |
  | ~s     | second (~2,'0d) |
  +--------+-----------------+

 Examples: 
  (time-format \"~y-~m-~d ~h:~i:~s\" 3443621047) ;=> \"2009-02-15 02:24:07\""
  (multiple-value-bind (s i h d m y) (decode-universal-time time)
    (let ((y (format nil "~4,'0d" y))
          (m (format nil "~2,'0d" m))
          (d (format nil "~2,'0d" d))
          (h (format nil "~2,'0d" h))
          (i (format nil "~2,'0d" i))
          (s (format nil "~2,'0d" s)))
      (loop for f in '("~y" "~m" "~d" "~h" "~i" "~s")
            as r in  (list y m d h i s)
            do (setf format (regex-replace-all f format r)))
      format)))

(defun delete* (item sequence &rest keys)
  (let ((vector (apply #'delete item sequence keys)))
    #-:allegro vector
    #+:allegro (make-array (length vector)
                           :initial-contents vector
                           :fill-pointer t :adjustable t)))
