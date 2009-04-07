(in-package :web4r)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *web4r-dir*
    (awhen (load-time-value #.*compile-file-pathname*)
      (truename (merge-pathnames
                 "../" (directory-namestring it))))
    "The path of web4r directory"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *sml-dir* (merge-pathnames "sml/" *web4r-dir*)
    "The path of sml directory under the web4r directory"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun web4r-file-path (file)
    (merge-pathnames file *web4r-dir*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun sml-file-path (file)
    (merge-pathnames file *sml-dir*)))

(defun nl->br (x)
  (regex-replace-all #\Newline x (format nil "<br>~%")))

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
  (loop for (k v) on parameters by #'cddr
        as l = (add-parameter link k v) then (add-parameter l k v)
        finally (return l)))

(defun rem-parameter (link key)
  (let* ((new (regex-replace (concat "(" key  "=[^&]+&?)") link ""))
         (len (length new)))
    (if (member (subseq new (1- len)) '("?" "&") :test #'equal)
        (subseq link 0 (1- len))
        new)))

(defun omit (obj max &optional (omark "..."))
  (let ((str (->string obj)))
    (if (<= (length str) max)
        str
        (concat (subseq str 0 max) omark))))

(defun file-length* (file)
  (with-open-file (s file :if-does-not-exist nil)
    (aand s (file-length it))))

(defun time-format (format &optional (time (get-universal-time)))
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
