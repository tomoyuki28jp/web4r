(in-package :web4r)

(defvar *cookie* nil)

(defvar *host-uri* "http://localhost:8080/")

(defvar *show-page-uri*
  #'(lambda (oid)
      (concat *host-uri* "customer/show/" oid "/")))

; --- Utilties --------------------------------------------------

(defmacro without-indenting (&rest body)
  "Executes BODY with setting sml:*indent-mode* nil."
  (with-gensyms (indent-mode result)
    `(let ((,indent-mode sml:*indent-mode*))
       (setf sml:*indent-mode* nil)
       (let ((,result ,@body))
         (setf sml:*indent-mode* ,indent-mode)
         ,result))))

(defmacro without-rewriting-urls (&rest body)
  "Executes BODY with setting hunchentoot:*rewrite-for-session-urls* nil."
  (with-gensyms (rewrite? result)
    `(let ((,rewrite? hunchentoot:*rewrite-for-session-urls*))
       (setf hunchentoot:*rewrite-for-session-urls* nil)
       (let ((,result ,@body))
         (setf hunchentoot:*rewrite-for-session-urls* ,rewrite?)
         ,result))))

(defun parse* (html)
  (aand (closure-html:parse html (closure-html:make-lhtml-builder))
        (cddr (cadddr it))))

(defun get-element-by-id (id lhtml)
  "Returns the first element in LHTML whose ID is specified."
  (get-element
   #'(lambda (x) (and (listp x) (eq (car x) :id) (string= (nth 1 x) id)))
   #'(lambda (l e) (declare (ignore e)) (cddr l))
   lhtml))

(defun get-element-by-tag (tag lhtml)
  "Returns the first element in LHTML whose TAG is specified."
  (get-element
   #'(lambda (x) (and (listp x) (eq (car x) tag)))
   #'(lambda (l e) (declare (ignore l)) (cadar e))
   lhtml))

(defun get-element (test getter lhtml)
  (loop for e in lhtml
        as  r = (when (and e (listp e))
                  (if (find-if test e)
                      (funcall getter lhtml e)
                      (get-element test getter e)))
        when r return r))

; --- Via HTTP --------------------------------------------------

(defun http-get-instance-by-oid (class oid)
  "Returns a list of slots' values like '((slot-symbol slot's-value) ...)
 of the CLASS INSTANCE specified by OID"
  (when (get-instance-by-oid class oid)
    (without-indenting
      (without-rewriting-urls
        (let ((page (parse* (drakma:http-request (funcall *show-page-uri* oid)
                                    :cookie-jar *cookie*))))
          (loop for s in (get-excluded-slots class)
                as id = (web4r::slot-id* class s)
                as e  = (get-element-by-id id page)
                when e collect (cons (slot-symbol s) e)))))))

(defun http-test-get-instance-by-oid (class oid)
  "Runs the test to check if the displayed slots' values of the CLASS instance
 specified by OID are right ones. This raises errors if there is any error and
 prints passed test results otherwise."
  (without-indenting
    (aif (get-instance-by-oid class oid)
         (let ((vals (http-get-instance-by-oid class oid)))
           (loop for s in (get-excluded-slots class)
                 as symb = (slot-symbol s)
                 as val  = (cdr (assoc symb vals))
                 as ans  = (aand (slot-display-value it s)
                                 (case (slot-input s)
                                   (:file (parse* (slot-value it 'sml:obj)))
                                   (:textarea (parse* (sml:nl->br it)))
                                   (otherwise (list (->string it)))))
                 as f = (concat "The slot '" symb "' of the class '" class "': ")
                 do (if (equal ans val)
                        (print (concat f "ok"))
                        (error (concat f "A returned value is '" val
                                       "' which is not equal to '" ans "'")))))
         (error
          "No such instance of the class '~S' associated with the oid '~D'"
          class oid))))
