(in-package :web4r)

(defvar *cookie-jar* nil)

(defvar *host-uri* "http://localhost:8080/")

(defvar *regist-page-uri*
  #'(lambda () (concat *host-uri* "regist")))

(defvar *loggedin-page-uri*
  #'(lambda () (concat *host-uri* "user/is/loggedin")))

(defvar *login-page-uri*
  #'(lambda () (concat *host-uri* "login")))

(defvar *logout-page-uri*
  #'(lambda () (concat *host-uri* "logout")))

(defvar *edit-page-uri*
  #'(lambda (class &optional oid)
      (concat *host-uri* (join "/" (->string-down class ) "edit" oid) "/")))

(defvar *show-page-uri*
  #'(lambda (class oid)
      (concat *host-uri* (join "/" (->string-down class ) "show" oid) "/")))

(defvar *delete-page-uri*
  #'(lambda (class oid)
      (concat *host-uri* (join "/" (->string-down class ) "delete" oid) "/")))

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

(defun element-getter (l e)
  (declare (ignore e))
  (aand (cddr l)
        (if (= (length it) 1)
            (car it)
            it)))

(defun get-element-by-id (id lhtml)
  "Returns the first element in LHTML whose ID is specified."
  (get-element
   #'(lambda (x) (and (listp x) (eq (car x) :id) (string= (nth 1 x) id)))
   #'element-getter
   lhtml))

(defun get-element-by-class (class lhtml)
  "Returns the first element in LHTML whose CLASS is specified."
  (get-element
   #'(lambda (x) (and (listp x) (eq (car x) :class) (string= (nth 1 x) class)))
   #'element-getter
   lhtml))

(defun get-element-by-tag (tag lhtml)
  "Returns the first element in LHTML whose TAG is specified."
  (get-element
   #'(lambda (x) (and (listp x) (eq (car x) tag)))
   #'(lambda (l e) (declare (ignore l)) (cadar e))
   lhtml))

(defun get-error-msgs (lhtml)
  (aand (get-element-by-class "errors" lhtml)
        (if (listp (car it))
            (mapcar #'cddr (get-element-by-class "errors" lhtml))
            (car (last it)))))

(defun get-element (test getter lhtml)
  (loop for e in lhtml
        as  r = (when (and e (listp e))
                  (if (find-if test e)
                      (funcall getter lhtml e)
                      (get-element test getter e)))
        when r return r))

(defun http-request* (uri &rest args)
  (apply #'drakma:http-request uri
         (append (aif *cookie-jar* (list :cookie-jar it))
                 args)))

(defun regex-matched (string regex)
  (multiple-value-bind (match? regs)
      (scan-to-strings regex string)
    (declare (ignore match?))
    (elt regs 0)))

(defun cid* (string)
  (regex-matched string "name=\"cid\" value=\"([^\"]+)\""))

(defun slots= (class instance alist)
  (loop for a in alist
        as slot = (get-slot class (car a))
        as ans  = (slot-display-value instance slot)
        unless (equal ans (cdr a))
        do (error "~S is not equal to ~S for the slot '~S' of the class '~S'"
                  (cdr a) ans (car a) class))
  t)

(defun slot-id** (class slot)
  (case slot
    (id   "user_id")
    (pass "user_pass")
    (otherwise (slot-id* class (get-slot class slot)))))

; --- Via HTTP --------------------------------------------------

(defun http-regist (args)
 "Registers a new user via http with args which must be an alist of user
 class slot's symbol/value pairs."
  (let* ((class  (user-class))
         (uri    (funcall *regist-page-uri*))
         (cid    (cid* (http-request* uri)))
         (params (mapcar #'(lambda (a)
                             (cons (slot-id** class (car a)) (cdr a)))
                         args)))
    (http-request* uri :method :post
                   :parameters (append params (list (cons "cid" cid))))))

(defun http-test-regist (args)
  "Executes (http-regist ARGS) and checks if the user was successfully
 registered. This raises an error if the registration failed and returns
 true otherwise."
  (let ((error (get-error-msgs (parse* (http-regist args))))
        (id    (cdr (assoc 'id args)))
        (class (user-class)))
    (unless id (error "User ID isn't specified."))
    (if (and (null error)
             (aand (car (get-instances-by-value class (user-id-slot) id))
                   (slots= class it args)))
        t
        (error "regist failed: ~S" error))))

(defun http-login (&key id pass)
  "Logs in the current user with ID and PASS via http."
  (let* ((uri (funcall *login-page-uri*))
         (cid (cid* (http-request* uri))))
    (http-request* uri :method :post :parameters
                   (append `(("user_id" . ,id) ("user_pass" . ,pass))
                           (list (cons "cid" cid))))))

(defun http-test-login (&key id pass)
  "Executes (http-login :id ID :pass PASS) and check if the user was
 successfully logged in. This raises an error if the test failed
 and returns true otherwise."
  (let ((html (http-login :id id :pass pass)))
    (if (string= (http-request* (funcall *loggedin-page-uri*)) "true")
        t
        (error "Login failed: ~S" (get-error-msgs (parse* html))))))

(defun http-logout ()
  "Logs out the current user via http."
  (http-request* (funcall *logout-page-uri*)))

(defun http-test-logout ()
  "Executes (http-logout) and check if the current user was successfully
 logged out. This raises and error if the test failed and returns true
 otherwise."
  (http-logout)
  (if (string= (http-request* (funcall *loggedin-page-uri*)) "false")
      t
      (error "Logout failed")))

;(defun http-make-instance (class &rest args)
;  (let* ((uri (funcall *edit-page-uri* 
;  )
;
;(defun http-test-make-instance (&rest args)
;  )

(defun http-get-instance-by-oid (class oid)
  "Gets and returns a list of slots' symbol/value alist pairs like
 '((slot-symbol . slot-value) ...) of the CLASS INSTANCE specified by OID
 via http"
  (if (get-instance-by-oid class oid)
      (without-indenting
        (without-rewriting-urls
          (let* ((uri  (funcall *show-page-uri* class oid))
                 (page (parse* (http-request* uri))))
            (loop for s in (get-excluded-slots class uri)
                  as id = (web4r::slot-id* class s)
                  as e  = (get-element-by-id id page)
                  when e collect (cons (slot-symbol s) e)))))
      (error "There is no object associated with the oid '~D'" oid)))

(defun http-test-get-instance-by-oid (class oid)
  "Executes (http-get-instance-by-oid CLASS OID) and check if the values
 are correct ones. This raises an error if the tests failed and returns true
 otherwise."
  (without-indenting
    (let ((ins (get-instance-by-oid class oid)))
      (loop for a in (http-get-instance-by-oid class oid)
            as slot = (get-slot class (car a))
            as ans  = (aand (slot-display-value ins slot)
                            (case (slot-input slot)
                              (:file (car (parse* (slot-value it 'sml:obj))))
                              (:textarea (parse* (sml:nl->br it)))
                              (otherwise (->string it))))
            unless (equal ans (cdr a))
            do (error "~S is not equal to ~S for the slot '~S' of the class '~S'"
                      (cdr a) ans (car a) class)
            finally (return t)))))

(defun http-drop-instance-by-oid (class oid)
  "Drops the CLASS instance specified by OID via http."
  (if (get-instance-by-oid class oid)
      (http-request* (funcall *delete-page-uri* class oid))
      (error "There is no object associated with the oid '~D'" oid)))

(defun http-test-drop-instance-by-oid (class oid)
  "Executes (http-drop-instance-by-oid CLASS OID) and check if the instance
 was successfully deleted. This raises an error if the test failed and returns
 true otherwise."
  (http-drop-instance-by-oid class oid)
  (if (get-instance-by-oid class oid)
      (error "Dropping an instance failed")
      t))
