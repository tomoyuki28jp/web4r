(in-package :sml)

(defvar *markup-lang* :xhtml
  "Marckup Language. Either :xhtml, :html or :xml")

(defvar *indent-mode* t
  "Indent mode flag")

(defvar *tab-width* 4
  "The number of spaces per indentation")

(defvar *indent-level* 0
  "Current level of indentation")

(defvar *sml-output* *standard-output*
  "Markup language output stream")

(defvar *xml-version* "1.0"
  "XML version")

(defvar *encoding* "UTF-8"
  "Encoding")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *safe* nil
    "Safe mode flag")

  (defvar *sml* (make-hash-table)
    "Hash table where the defined sml template are set to"))

(defvar *non-break* nil
  "Non-breaking flag")

(defvar *non-breaking-tags*
  '(title textarea a b th td h1 h2 h3 h4 h5 li dt dd label option span p)
  "Non-breaking tags")

; --- Doctype ---------------------------------------------------

(defvar *doctypes*
  '((:xhtml ((:strict . 
"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
             (:transitional . 
"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
             (:frameset . 
"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">")))
    (:html  ((:strict . 
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"
\"http://www.w3.org/TR/html4/strict.dtd\">")
             (:transitional . 
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
\"http://www.w3.org/TR/html4/loose.dtd\">")
             (:frameset . 
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\"
\"http://www.w3.org/TR/html4/frameset.dtd\">")))))

(defvar *doctype* :transitional
  "Default doctype")

(defun doctype ()
  (concat (when (member *markup-lang* '(:xml :xhtml))
            (format nil "<?xml version=\"~A\" encoding=\"~A\"?>~%"
                    *xml-version* *encoding*))
          (awhen (assoc *doctype*
                        (cadr (assoc *markup-lang* *doctypes*)))
            (concat (cdr it) #\Newline))))

; --- Escape ----------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass safe ()
    ((obj :initarg :obj)))

  (defmacro safe (&rest body)
    `(if *safe*
         (concat ,@body)
         (let ((*safe* t))
           (make-instance 'safe :obj (concat ,@body)))))

  (defun escape-char (char)
    (case char
      (#\& "&amp;")
      (#\< "&lt;")
      (#\> "&gt;")
      (#\' "&#039;")
      (#\" "&quot;")
      (t (string char))))

  (defgeneric escape (obj))

  (defmethod escape ((char character))
    (safe (escape-char char)))

  (defmethod escape ((string string))
    (safe (apply #'concat
                 (loop for c across string
                       collect (escape-char c)))))

  (defmethod escape ((safe safe)) safe)

  (defmethod escape ((obj t))
    (escape (->string obj)))

  (defun escape* (x)
    (slot-value (escape x) 'obj)))

; --- Output ----------------------------------------------------

(defun p (&rest args)
  (dolist (a args)
    (when-let (x (or (and (typep a 'safe) (slot-value a 'obj)) a))
      (princ x *sml-output*)))
  nil)

(defun pe (x)
  (p (escape* x)))

(defun pr (x)
  (if (and (consp x) (consp (cdr x)))
      (mapcar #'pr x)
      (unless (equal x '(nil))
        (pe x))))

; --- Util ------------------------------------------------------

(defun indent ()
  (when *indent-mode*
    (make-string (* *indent-level* *tab-width*)
                 :initial-element #\Space)))

(defun newline ()
  (when *indent-mode*
    #\Newline))

(defmacro sml->ml (&rest sml)
  `(let ((*sml-output* (make-string-output-stream)))
     ,@sml
     (get-output-stream-string *sml-output*)))

(defun non-breaking-tag-p (tag)
  (and (not (eq *markup-lang* :xml))
       (member-if #'(lambda (x) (equalp (symbol-name x) tag))
                  *non-breaking-tags*)
       t))

(defun attr (&rest args)
  (if (and (= 1 (length args)) (listp (car args)))
      (apply #'attr (car args))
      (aand (loop while args
                  as k = (pop args)
                  as v = (pop args)
                  when v collect 
                  (format nil " ~A=\"~A\""
                          (escape* (->string-down k)) (escape* v)))
            (apply #'concat it))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun attr? (x)
    (or (keywordp x)
        (and (listp x) (eq (car x) 'attr)))))

; --- Markup language read macro --------------------------------

(defmacro tag (&rest args)
  (let* ((end? (not (when (eq (car (last args)) '/)
                      (setf args (subseq args 0 (1- (length args)))))))
         (tag  (awhen (pop args) (escape* (->string-down it))))
         (non-break (gensym)))
    `(let ((,non-break (non-breaking-tag-p ,tag)))
       (declare (ignorable ,non-break))
       ,(when (and (string= tag "html")
                   (member *markup-lang* '(:html :xhtml)))
         `(p (doctype)))
       (p (unless *non-break* (indent)) "<" ,tag)
       ,@(loop while (attr? (car args))
               as x = (pop args)
               collect (if (keywordp x)
                           `(p (attr ,x ,(pop args)))
                           `(p ,x)))
       ,(if end? `(p ">")
                 `(p (if (eq *markup-lang* :html) ">" " />")
                     (unless  (or ,non-break *non-break*) (newline))))
       ,(when (= 0 (length args)) `(setf ,non-break t))
       ,(when end? `(unless  (or ,non-break *non-break*) (p (newline))))
       ,@(loop for i in args collect
               `(let ((*indent-level* (1+ *indent-level*))
                      (*non-break* ,non-break))
                  (pr ,i)))
       ,(when end? `(p (unless (or ,non-break *non-break*)
                         (concat (newline) (indent)))
                       "</" ,tag ">"
                       (unless *non-break* (newline)))))))

(set-macro-character #\] (get-macro-character #\)))
(set-macro-character #\[
  #'(lambda (stream char)
      (declare (ignore char))
      `(tag ,@(read-delimited-list #\] stream t))))

; --- Form utilities --------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-input (type sequence &optional alias)
    (or (and alias (eq (car sequence) alias))
        (and (eq (nth 1 sequence) 'input)
             (equalp (awhen (member :type sequence) (nth 1 it))
                     type))
        (remove nil (loop for s in sequence
                          when (listp s)
                          collect (find-input type s alias))))))

(defmacro form (&rest args)
  `(tag form ,@args
        ,(unless (find-input "submit" `',args 'submit)
          `(let ((*indent-level* (1+ *indent-level*))) (submit)))))

(defmacro multipart-form (&rest args)
  `(form :enctype "multipart/form-data" ,@args))

(defmacro input-text (name &rest args)
  `(tag input :type "text" :name ,name ,@args /))

(defmacro input-file (name &rest args)
  `(tag input :type "file" :name ,name ,@args /))

(defmacro submit (&rest args)
  `(tag input :type "submit" ,@args /))

(defmacro input-checked (type input &rest args)
  `(if ,(awhen (position :value args)
          (aand (nth (1+ it) args) `(string= (->string ,it) ,input)))
       (tag input :type ,type :checked "checked" ,@args /)
       (tag input :type ,type ,@args /)))

(defun select-form (name values &key selected id class)
  (tag select :name name :id (or id name) :class class
       (loop for v in values collect
             (tag option :value v :selected
                  (when (equal selected v) "selected") v))))

; --- Template --------------------------------------------------

(defmacro define-template (name sml)
  `(setf (gethash ,name *sml*) ',sml))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-template (name)
    (or (gethash name *sml*)
        (error "sml not defined: ~A" name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-sml (sml &optional package (readtable (copy-readtable)))
    (if (probe-file sml)
        (with-open-file (stream sml)
          (with-standard-io-syntax 
            (let ((*package*   package)
                  (*readtable* readtable))
              (aand (loop  as i = (read stream nil 'sml-end)
                          if (eq i 'sml-end) return r
                          else collect i into r)
                    (append '(progn) it)))))
        (error "sml file not found: ~A" sml))))

(defmacro load-sml (sml &optional (package *package*))
  (read-sml (eval sml) package))

(defmacro manipulate-sml (sml &rest args)
  (labels
      ((match (selector sml)
         (flet ((match? (1st attr)
                  (and (equal (subseq selector 0 1) 1st)
                       (aand (position attr sml)
                             (string= (->string (nth (1+ it) sml))
                                      (subseq selector 1))))))
           (or (match? "#" :id)
               (match? "." :class)
               (equal (->string-down (nth 1 sml))
                      (->string-down selector)))))
       (manipulate (sml arg)
         (loop for s in sml collect
               (if (listp s)
                   (or (when (match (->string (nth 1 arg)) s)
                         (case (car arg)
                           (append (append s (aand (->list (nth 2 arg))
                                                   (if (listp (car it))
                                                       it
                                                       (list it)))))
                           (replace `(progn ,@(subseq arg 2)))
                           (remove  "")
                           (otherwise (error "invalid manipulator: ~S"
                                             (car arg)))))
                       (manipulate s arg))
                   s))))
    (loop for arg in args do (setf sml (manipulate sml arg)))
    sml))

(defmacro with-template ((name) &rest body)
  `(manipulate-sml ,(get-template name) ,@body))

(defmacro with-sml-file (file-path &rest body)
  `(manipulate-sml ,(read-sml (eval file-path) *package*) ,@body))

; --- Misc ------------------------------------------------------

(defun nl->br (x)
  (regex-replace-all #\Newline x (sml->ml (tag br /))))
