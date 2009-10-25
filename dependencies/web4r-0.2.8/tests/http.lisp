(in-package :web4r-tests)
(in-suite web4r)

(defmacro html (title &rest body)
  `[html :xmlns "http://www.w3.org/1999/xhtml" :lang "en"
         [head [meta :http-equiv "content-type"
                     :content "text/html; charset=utf-8" /]
               [link :rel "stylesheet" :href "/css/common.css"
                     :type "text/css" :charset "utf-8" /]
               [title ,title]]
         [body ,@body]])

(test http

  (defpclass blog-user (user)
    ((email :format :email :unique t)
     (blog-title :length 256)))
  
  (defpclass blog ()
    ((user-oid :index t :hide-for :all)
     (title :length 50)
     (body  :length 300)))
  
  (defun owner-p* (oid)
    (owner-p 'blog 'user-oid oid))
  
  (defun owner-check (oid)
    (unless (owner-p* oid)
      (redirect/error-msgs
       (page-uri "blog") "Illegal action")))
  
  (defpage blog (:default)
    (index-page 'blog))
  
  (defpage blog/show (oid)
    (show-page 'blog oid))
  
  (defpage blog/edit (oid :auth)
    (when oid (owner-check oid))
    (edit-page 'blog oid :slot-values
               `((user-oid . ,(login-user-oid)))))
  
  (defpage blog/delete (oid :auth)
    (owner-check oid)
    (delete-page 'blog oid))

  (defpage user/is/loggedin ()
    (p (if (login-user) "true" "false")))

  (drop-instances-by-class 'blog-user)
  (drop-instances-by-class 'blog)

  (let ((class 'blog)
        (id    "user1")
        (pass  "pass1"))
    (with-new-cookie
      (is-true (http-test-regist `((id    . ,id)
                                   (pass  . ,pass)
                                   (email . "1@1.com")
                                   (blog-title . "user1's blog"))))
      (is-true (http-test-login :id id :pass pass))
      (let ((oid (ignore-errors
                   (http-test-make-instance class :title "title1" :body "body1"))))
        (is-true (not (null oid)))
        (is-true (http-test-get-instance-by-oid class oid))
        (is-true (http-test-update-instance
                  class oid '((title . "title1c") (body . "body1c"))))
        (is-true (http-test-drop-instance-by-oid class oid)))
      (is-true (http-test-logout)))))
