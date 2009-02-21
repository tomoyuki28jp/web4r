(in-package :web4r)

(defvar *http-char-stream* *standard-output*)

; --- Doctype ---------------------------------------------------

(defparameter *doctype-strict*
  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN
http://www.w3.org/TR/html4/strict.dtd\">")

(defparameter *doctype-transitional*
  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN
http://www.w3.org/TR/html4/loose.dtd\">")

(defparameter *doctype-frameset*
  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN
http://www.w3.org/TR/html4/frameset.dtd\">")

(defvar *doctype* *doctype-transitional*)

(defun doctype ()
  (format nil "~A~%" *doctype*))

; --- Escape ----------------------------------------------------

(defvar *escape* t)
(defvar *safe* nil)

(defun escape (obj)
  (cond ((not *escape*) obj)
        ((typep obj 'safe) (slot-value obj 'content))
        ((stringp obj) (escape-string obj))
        (t obj)))

(defun escape-string (string)
  (join "" (loop for c across string
                 collect (escape-char c))))

(defun escape-char (char)
  (case char
    (#\& "&amp;")
    (#\< "&lt;")
    (#\> "&gt;")
    (#\' "&#039;")
    (#\" "&quot;")
    (t (string char))))

(defclass safe ()
  ((content :initarg :content)))

(defmacro safe (&rest body)
  `(if *safe*
       ,@body
       (let ((*safe* t))
         (make-instance 'safe :content ,@body))))

; --- Output ----------------------------------------------------

(defun p (obj)
  (when obj
    (princ obj *http-char-stream*))
  nil)

(defun pe (obj)
  (p (escape obj)))

(defun pr (obj)
  (if (listp obj)
      (mapcar #'(lambda (x) (pe x)) obj)
      (pe obj)))

; --- Tags ------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *defined-tags* nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *non-breaking-start-tags*
    '(title textarea a b th td h1 h2 h3 h4 h5 li dt dd label option)))

(defvar *non-breaking-end-tags* '(a b))

(defun attr (attr value)
  (when value
    (format nil " ~A=~A" (escape attr) (qw (escape value)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun defined-tag-p (tag)
    (member tag *defined-tags*)))

(defmacro start-tag (tag attr)
  `(progn
     ,(when (string= tag "HTML")
            `(p (doctype)))
     (p (concat "<" ,tag))
     ,@attr
     (p ">")
     ,(unless (member (intern tag :web4r) *non-breaking-start-tags*)
        `(p ,(format nil "~%")))))

(defun end-tag (tag)
  (p (format nil "</~A>" tag))
  (unless (member (intern tag :web4r) *non-breaking-end-tags*)
    (p (format nil "~%"))))

(defmacro deftag (tag &key (end-tag t))
  (let ((tag  (mkstr tag)))
    `(defmacro ,(symb (concat tag "/")) (&rest rest)
       `(progn
          (start-tag ,',tag ,
                     (loop while (keywordp (car rest))
                        collect `(p (attr ,(pop rest) ,(pop rest)))))
          ,@(loop for b in rest when b collect
                 (if (listp b)
                     (if (defined-tag-p (car b))
                         b
                         `(pr ,b))
                     `(pe ,b)))
          ,',(when end-tag
               `(end-tag ,tag))))))

(defmacro deftags (end-tag &rest tags)
  `(progn
     ,@(loop for x in tags
            as tag = (symb (mkstr x "/"))
            collect `(deftag ,x :end-tag ,end-tag)
            collect `(export ',tag)
            collect `(push ',tag *defined-tags*))))

(deftags t
    bdo       big       blockquote body      button    caption
    cite      code      colgroup   dd        del       dfn
    div       dl        dt         em        fieldset  form
    font      h1        h2         h3        h4        h5
    h6        head      html       i         ins       kbd
    label     legend    li         map       noscript  object
    ol        optgroup  option     p         pre       q
    samp      script    select     small     span      strong
    style     sub       sup        table     textarea  tbody
    td        tfoot     th         thead     title     tr
    tt        ul        var        a         b)

(deftags nil
    area      base      basefont   col       frame     hr
    img       input     isindex    link      meta      param)

(defun br ()
  (format nil "<br>~%"))

(defun br/ ()
  (p (br)))

; --- Shtml -----------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *web4r-dir*
    (awhen (load-time-value #.*compile-file-pathname*)
      (truename (merge-pathnames "../" (directory-namestring it))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *shtml-dir* (merge-pathnames "shtml/" *web4r-dir*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *shtml* (make-hash-table)))

(defmacro define-shtml (name shtml)
  `(sethash ,name *shtml* ',shtml))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-shtml (name)
    (or (gethash name *shtml*)
        (error "shtml not defined: ~A" name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-shtml (shtml package)
    (if (probe-file shtml)
        (with-open-file (stream shtml)
          (with-standard-io-syntax 
            (let ((*package* package))
              (read stream nil nil))))
        (error "shtml file not found: ~A" shtml))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shtml-path (file)
    (merge-pathnames file *shtml-dir*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun web4r-path (file)
    (merge-pathnames file *web4r-dir*)))

(defmacro load-shtml (shtml)
  (read-shtml (eval shtml) *package*))

(defmacro %with-shtml (shtml &rest body)
  (labels ((by-tag (shtml)
             (let ((tag (make-keyword (substr (mkstr (car shtml)) 0 -1))))
               (awhen (position tag body) (nth (1+ it) body))))
           (by-id (shtml)
             (awhen (position :id shtml)
                    (let ((id (make-keyword (mkstr "#" (nth (1+ it) shtml)))))
                      (awhen (position id  body) (nth (1+ it) body)))))
           (apply! (shtml)
             (acond ((by-tag shtml) it)
                    ((by-id  shtml) it)
                    (t (declare (ignore it))
                       (loop for e in shtml collect
                            (if (listp e) (apply! e) e))))))
    (apply! shtml)))

(defmacro with-shtml ((name) &rest body)
  `(%with-shtml ,(get-shtml name) ,@body))

(defmacro with-shtml-file ((file) &rest body)
  `(%with-shtml ,(read-shtml (shtml-path file) *package*) ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-shtml :basic
      (html/ :lang "en"
             (head/
              (meta/ :http-equiv "content-type"
                     :content "text/html; charset=utf-8")
              (title/ "Default template title"))
             (body/
              (p/ "Default template body")))))

; --- Form ------------------------------------------------------

(defmacro input-text/ (name &rest args)
  `(input/ :type "text" :name ,name ,@args))

(defmacro input-checked/ (type input &rest args)
  (with-gensyms (value)
  `(let ((,value (mkstr ,(awhen (position :value args)
                           (nth (1+ it) args)))))
     (if (and ,value
              (or (and (listp ,input)
                       (member ,value ,input :test #'equal))
                  (string= (mkstr ,input) ,value)))
         (input/ :type ,type :checked "checked" ,@args)
         (input/ :type ,type ,@args)))))

(defmacro submit/ (&rest args)
  `(input/ :type "submit" ,@args))

(defun select-form/ (name values &optional selected)
    (select/ :name name :id name
             (loop for v in values
                   do (if (equal selected v)
                          (option/ :value v :selected "selected" v)
                          (option/ :value v v)))))

(defun select-date/ (name &key y m d (y-start 1900) (y-end 2030))
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore second minute hour))
    (flet ((int (x) (when x (->int x))))
      (select-form/
       (mkstr name "-Y")
       (loop for i from y-start to y-end collect i)
       (or (int y) year))
      (select-form/
       (mkstr name "-M")
       (loop for i from 1 to 12 collect i)
       (or (int m) month))
      (select-form/
       (mkstr name "-D")
       (loop for i from 1 to 31 collect i)
       (or (int d) date)))))

(defmacro form-for/cont/ (cont &key class instance (submit "submit"))
  `(%form/cont/ (multipart-form-p ,class) ,cont
     (table/
       (loop for s in (get-excluded-slots ,class)
             do (progn
                  (tr/ (td/ (form-label s)))
                  (tr/ (td/ (form-input s ,instance)))))
       (tr/ (td/ (submit/ :value ,submit))))))

(mapcar #'(lambda (x) (push x *defined-tags*))
        '(br/ input-text/ input-checked/ submit/ select-form/
          select-date/ form-for/cont/ ))
