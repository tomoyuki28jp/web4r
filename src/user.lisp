(in-package :web4r)

(defvar *msgs*
  #.(hash
     :login-failed       "Wrong username and password combination"
     :login-succeeded    "Logged in"
     :logged-out         "Logged out"
     :already-logged-in  "You are already logged in"))

(defun msg (key) (gethash key *msgs*))

(defclass user-class ()
  ((class       :initform 'user      :initarg :class       :type symbol)
   (id-slot     :initform 'id        :initarg :id-slot     :type symbol)
   (pass-slot   :initform 'pass      :initarg :pass-slot   :type symbol)
   (id-label    :initform "ID"       :initarg :id-label    :type string)
   (pass-label  :initform "Password" :initarg :pass-label  :type string)
   (login-page  :initform 'login     :initarg :login-page  :type symbol)
   (logout-page :initform 'logout    :initarg :logout-page :type symbol)
   (regist-page :initform 'regist    :initarg :regist-page :type symbol)))

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
  (let ((ins (ele:get-instances-by-value (user-class) (user-id-slot) user-id)))
    (cond (user-pass
           (loop for i in ins
                 when (equal (slot-value i (user-pass-slot)) user-pass)
                 do (return-from get-user i)))
          (ins (car ins)))))

(defun get-user-oid (user-id)
  (awhen (get-instances-by-value (user-class) (user-id-slot) user-id)
    (oid (car it))))

(defun login (user-oid user-id)
  (set-session
   (list (cons :user-oid user-oid)
         (cons :user-id  user-id))))

(defun logout ()
  (destroy-session))

(defun login-user ()
  (awhen (get-session :user-oid)
    (get-instance-by-oid (user-class) it)))

(defun login-user-id ()
  (get-session :user-id))

(defun login-user-oid ()
  (get-session :user-oid))

(defun login/cont (redirect-uri)
  (let* ((id (post-param "id"))
         (pass (post-param "pass"))
         (errors
          (append (validation-errors (user-id-label) id '(:nullable nil))
                  (validation-errors (user-pass-label) pass '(:nullable nil))))
         (user (unless errors (get-user id pass))))
    (if (or errors (null user))
        (page/error-msgs 'login (or errors (msg :login-failed)))
        (progn
          (login (oid user) id)
          (redirect/msgs redirect-uri (msg :login-succeeded))))))

(defun login-page (&key (redirect-uri (host-uri)))
  (if (login-user)
      (redirect/msgs redirect-uri (msg :already-logged-in))
      (let ((*with-slots* (list (user-id-slot) (user-pass-slot))))
        (load-shtml (shtml-path "login.shtml")))))

(defun logout-page (&key (redirect-uri (host-uri)))
  (logout)
  (redirect/msgs redirect-uri (msg :logged-out)))

(defun regist-page (&key (redirect-uri (host-uri)))
  (if (login-user)
      (redirect/msgs redirect-uri (msg :already-logged-in))
      (scaffold-edit-page (user-class) :redirect-uri redirect-uri)))
