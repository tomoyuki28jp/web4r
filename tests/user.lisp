(in-package :web4r-tests)
(in-suite web4r)

(test user-class
  (defpclass test-user1 (user)
      ((email :format :email :unique t)))
  (setf *user*
        (make-instance 'user-class
          :class       'test-user1
          :id-label    "ID1"
          :pass-label  "Password1"
          :login-page  'login1
          :logout-page 'logout1
          :regist-page 'regist1))
  (is (eq (user-class) 'test-user1))
  (is (eq (user-id-slot) 'id))
  (is (eq (user-pass-slot) 'pass))
  (is (equal (user-id-label) "ID1"))
  (is (equal (user-pass-label) "Password1"))
  (web4r::drop-class-instances 'test-user1)
  (let ((i (make-instance 'test-user1 :id "id1" :pass "pass1")))
    (let ((user-id   (user-id   i))
          (user-pass (user-pass i)))
      (is (equal user-id   "id1"))
      (is (equal user-pass "pass1"))
      (is (eq i   (get-user user-id)))
      (is (eq i   (get-user user-id user-pass)))
      (is (eq nil (get-user user-id "pass123")))
      (is (eq (get-user-oid user-id) (oid i))))))

(test login
  (web4r::drop-class-instances 'test-user2)
  (defpclass test-user2 (user)
      ((email :format :email :unique t)))
  (setf *user* (make-instance 'user-class :class 'test-user2))
  (let ((i (make-instance 'test-user2 :id "user1" :pass :pass1)))
    (defpage login-test1 ()
      (login (oid i) (user-id i)))
    (defpage logout-test1 ()
      (logout))
    (defpage login-user-id-test ()
      (p (login-user-id)))
    (defpage login-user-oid-test ()
      (p (login-user-oid)))
    (let ((c (make-instance 'cookie-jar)))
      (http-request (page-uri "login-test1") :cookie-jar c)
      (is (equal (user-id i)
                 (http-request (page-uri "login-user-id-test")   :cookie-jar c)))
      (is (equal (->string (oid i))
                 (http-request (page-uri "login-user-oid-test")  :cookie-jar c)))
      (http-request (page-uri "logout") :cookie-jar c)
      (is (eq nil (http-request (page-uri "login-user-id-test")  :cookie-jar c)))
      (is (eq nil (http-request (page-uri "login-user-oid-test") :cookie-jar c))))))
