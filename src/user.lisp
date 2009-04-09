(in-package :web4r)

(defun login-msg (key)
  (cdr (assoc key *login-msgs*)))

(defclass user-class ()
  ((class       :initform 'user      :initarg :class       :type symbol)
   (id-slot     :initform 'id        :initarg :id-slot     :type symbol)
   (pass-slot   :initform 'pass      :initarg :pass-slot   :type symbol)
   (id-label    :initform "ID"       :initarg :id-label    :type string)
   (pass-label  :initform "Password" :initarg :pass-label  :type string)
   (login-page  :initform "login"    :initarg :login-page  :type symbol)
   (logout-page :initform "logout"   :initarg :logout-page :type symbol)
   (regist-page :initform "regist"   :initarg :regist-page :type symbol)))

(defmethod initialize-instance :after ((user user-class) &key)
  (set-page (slot-value user 'login-page)  (lambda () (login-page)))
  (set-page (slot-value user 'logout-page) (lambda () (logout-page)))
  (set-page (slot-value user 'regist-page) (lambda () (regist-page))))

(defvar *user* (make-instance 'user-class))

(defpclass user ()
  ((id    :type :alnum  :length (4 12) :unique t)
   (pass  :type :alnum  :length (4 12) :input :password)))

(defun user-class ()      (slot-value *user* 'class))
(defun user-id-slot ()    (slot-value *user* 'id-slot))
(defun user-pass-slot ()  (slot-value *user* 'pass-slot))
(defun user-id-label ()   (slot-value *user* 'id-label))
(defun user-pass-label () (slot-value *user* 'pass-label))
(defun user-login-page () (slot-value *user* 'login-page))

(defgeneric user-id (user))
(defgeneric user-pass (user))
(defmethod user-id ((user user))   (slot-value user (user-id-slot)))
(defmethod user-pass ((user user)) (slot-value user (user-pass-slot)))

(defun get-user (user-id &optional user-pass)
  (awhen (get-instances-by-value (user-class) (user-id-slot) user-id)
    (if user-pass
        (find-if #'(lambda (u) (equal (user-pass u) user-pass)) it)
        (car it))))

(defun get-user-oid (user-id)
  (awhen (get-instances-by-value (user-class) (user-id-slot) user-id)
    (oid (car it))))

(defun login (user-oid user-id)
  (setf (session-value :user-oid) user-oid
        (session-value :user-id)  user-id))

(defun logout ()
  (destroy-conts-by-session *session*)
  (remove-session *session*))

(defun login-user ()
  (awhen (session-value :user-oid)
    (get-instance-by-oid (user-class) it)))

(defun login-user-id ()
  (session-value :user-id))

(defun login-user-oid ()
  (session-value :user-oid))

(defun login/cont (redirect-uri)
  (let ((id   (post-parameter "ID"))
        (pass (post-parameter "PASS")))
    (with-validations (((user-id-label)   id   '(:nullable nil))
                       ((user-pass-label) pass '(:nullable nil)))
      (lambda (e) (page/error-msgs "login" e))
      (aif (get-user id pass)
          (progn (login (oid it) id)
                 (redirect/msgs redirect-uri (login-msg :login-succeeded)))
           (page/error-msgs "login" (login-msg :login-failed))))))

(defun login-page (&optional (redirect-uri (host-uri)))
  (if (login-user)
      (redirect/msgs redirect-uri (login-msg :already-logged-in))
      (let ((*with-slots* (list (user-id-slot) (user-pass-slot))))
        (load-sml-path "login.sml"))))

(defun logout-page (&optional (redirect-uri (host-uri)))
  (logout)
  (setf (session *request*) nil)
  (start-session)
  (redirect/msgs redirect-uri (login-msg :logged-out)))

(defun regist-page (&optional (redirect-uri (host-uri)))
  (if (login-user)
      (redirect/msgs redirect-uri (login-msg :already-logged-in))
      (scaffold-edit (user-class) :redirect-uri redirect-uri)))

(defun owner-p (class slot oid)
  (aand (awhen oid (get-instance-by-oid class it))
        (eq (slot-value it slot) (login-user-oid))))
