(in-package :web4r)

(defun login-msg (key)
  "Returns a string log in/out message."
  (cdr (assoc key *login-msgs*)))

(defclass user* ()
  ((class       :initform 'user      :initarg :class       :type symbol
                :documentation "A symbol name of a user persistent class.")
   (id-slot     :initform 'id        :initarg :id-slot     :type symbol
                :documentation "A symbol name of a id slot.")
   (pass-slot   :initform 'pass      :initarg :pass-slot   :type symbol
                :documentation "A symbol name of a password slot.")
   (id-label    :initform "ID"       :initarg :id-label    :type string
                :documentation "A label of a id slot.")
   (pass-label  :initform "Password" :initarg :pass-label  :type string
                :documentation "A label of a password slot.")
   (login-page  :initform "login"    :initarg :login-page  :type symbol
                :documentation "A name of the login page.")
   (logout-page :initform "logout"   :initarg :logout-page :type symbol
                :documentation "A name of the logout page.")
   (regist-page :initform "regist"   :initarg :regist-page :type symbol
                :documentation "A name of the regist page."))
  (:documentation "An instance of this class, set to *user*, is used
 to manage authentication. The default user persistent class is 'user,
 and you can change it by defining a new user persistent class with
 extending the 'user class."))

(defmethod initialize-instance :after ((user user*) &key)
  (set-page (slot-value user 'login-page)  (lambda () (login-page)))
  (set-page (slot-value user 'logout-page) (lambda () (logout-page)))
  (set-page (slot-value user 'regist-page) (lambda () (regist-page)))
  (let ((class (slot-value user 'class)))
    (set-page (->string-down (join "/" 'ajax class 'unique))
              (page-lambda (oid)
                (p (unique-p* class (get-parameters*) oid))))))

(setf *user* (make-instance 'user*))

(defpclass user ()
  ((id   :format :alnum :length (4 12) :unique t
         :documentation "A user login ID.")
   (pass :format :alnum :length (4 12) :input :password
         :documentation "A user login password."))
  (:documentation "This is the default user persistent class used for
 the authentication. You can change it by overriding this class or
 defining a new persistent class with extending this class."))

(defun user-class ()
  "Returns a symbol name of the user persistent class."
  (slot-value *user* 'class))

(defun user-id-slot ()
  "Returns a symbol name of the id slot."
  (slot-value *user* 'id-slot))

(defun user-pass-slot ()
  "Returns a symbol name of the password slot."
  (slot-value *user* 'pass-slot))

(defun user-id-label ()
  "Returns a string label of the id slot."
  (slot-value *user* 'id-label))

(defun user-pass-label ()
  "Returns a string label of the password slot."
  (slot-value *user* 'pass-label))

(defun user-login-page ()
  "Returns a name of the login page."
  (slot-value *user* 'login-page))

(defgeneric user-id (user)
  (:documentation "Returns a string id of the USER."))
(defmethod user-id ((user user))
  (slot-value user (user-id-slot)))

(defgeneric user-pass (user)
  (:documentation "Returns a string password of the USER."))
(defmethod user-pass ((user user))
  (slot-value user (user-pass-slot)))

(defun get-user (user-id &optional user-pass)
  "Returns an instance of the user persistent class by the USER-ID and
 USER-PASS if any."
  (awhen (get-instances-by-value (user-class) (user-id-slot) user-id)
    (if user-pass
        (find-if #'(lambda (u) (equal (user-pass u) user-pass)) it)
        (car it))))

(defun get-user-oid (user-id)
  "Returns an integer oid (object id) of the user specified by the USER-ID if any.
 The oid is for an instance of the user persistent class."
  (awhen (get-instances-by-value (user-class) (user-id-slot) user-id)
    (oid (car it))))

(defun login (user-oid user-id)
  "Sets the session data, USER-OID and USER-ID, so that the current user
 is treated as a login user."
  (setf (session-value :user-oid) user-oid
        (session-value :user-id)  user-id))

(defun logout ()
  "Logs out the current user."
  (destroy-conts-by-session *session*)
  (remove-session *session*))

(defun login-user ()
  "Returns an instance of the user persistent class for the current user
 if the user is a login user."
  (awhen (session-value :user-oid)
    (get-instance-by-oid (user-class) it)))

(defun login-user-id ()
  "Returns a string id of the current user if the user is a login user."
  (session-value :user-id))

(defun login-user-oid ()
  "Returns an integer oid of the current user if the user is a login user.
 The oid is for an instance of the user persistent class."
  (session-value :user-oid))

(defun login/cont (redirect-uri)
  (let ((id   (post-parameter "user_id"))
        (pass (post-parameter "user_pass")))
    (with-validations (((user-id-label)   id   '(:required t))
                       ((user-pass-label) pass '(:required t)))
      (lambda (e) (page/error-msgs "login" e))
      (aif (get-user id pass)
          (progn (login (oid it) id)
                 (redirect/msgs redirect-uri (login-msg :login-succeeded)))
           (page/error-msgs "login" (login-msg :login-failed))))))

(defun login-page (&optional (redirect-uri (host-uri)))
  "Generates and displays a login page. After logging in, redirects the
 user to the REDIRECT-URI."
  (if (login-user)
      (redirect/msgs redirect-uri (login-msg :already-logged-in))
      (let ((*with-slots* (list (user-id-slot) (user-pass-slot))))
        (load-sml-path "login.sml"))))

(defun logout-page (&optional (redirect-uri (host-uri)))
  "Logs out the current user and redirects the user to the REDIRECT-URI."
  (logout)
  (setf (session *request*) nil)
  (start-session)
  (redirect/msgs redirect-uri (login-msg :logged-out)))

(defun regist-page (&optional (redirect-uri (host-uri)))
  "Generates and displays a registration page. After registering,
 redirects the user to the REDIRECT-URI."
  (if (login-user)
      (redirect/msgs redirect-uri (login-msg :already-logged-in))
      (edit-page (user-class) :redirect-uri redirect-uri)))

(defun owner-p (class slot oid)
  "Returns true if the value of the SLOT in the CLASS specified
 by the OID is eq to the login user oid."
  (aand (awhen oid (get-instance-by-oid class it))
        (eq (slot-value it slot) (login-user-oid))))

(defpage user/is/loggedin ()
  (p (if (login-user) "true" "false")))
