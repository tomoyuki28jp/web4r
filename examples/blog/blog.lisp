; To run:
; 1. replace the parameter of open-store
;    http://common-lisp.net/project/elephant/doc/elephant.html#Getting-Started
; 2. compile and load this file
; 3. go to http://localhost:8080/

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :web4r))

(in-package :cl-user)
(defpackage :blog
  (:use :cl :web4r :elephant)
  (:shadowing-import-from :web4r :defpclass))
(in-package :blog)

(open-store
 '(:clsql (:postgresql "localhost" "test" "postgres" "pgpass")))

(defpclass blog-user (user)
  ((email :type :email :unique t)
   (blog-title :length 256)))

(defpclass blog-post ()
  ((user-oid :index t :hide t)
   (title :length 50   :size 40)
   (body  :length 3000 :rows 25 :cols 70)))

(defpclass comment ()
  ((blog-oid :index t :hide t)
   (name    :length 50)
   (comment :length 3000 :rows 5 :cols 50)))

(defpage default ()
  (multiple-value-bind (items pager)
      (per-page (get-instances-by-class 'blog-user))
    (load-shtml (web4r-path "examples/blog/shtml/index.shtml"))))

(defpage blog (user-id)
  (let ((slots (get-excluded-slots 'blog-post))
        (owner-p (aand user-id (equal it (login-user-id))))
        (uri (req-uri)))
    (multiple-value-bind (items pager)
        (per-page (get-instances-by-value
                   'blog-post 'user-oid (get-user-oid user-id)))
      (load-shtml (web4r-path "examples/blog/shtml/blog.shtml")))))

(defpage edit (oid :auth)
  (scaffold-edit-page
   'blog-post :oid oid :slot-values `((user-oid ,(login-user-oid)))
   :redirect-uri (page-uri "blog" (login-user-id))))

(defpage show (oid)
  (let* ((ins (get-instance-by-oid 'blog-post oid))
         (back (when ins (w/p (page-uri "blog" (blog-user-id ins))))))
    (multiple-value-bind (comments pager)
        (per-page (get-instances-by-value 'comment 'blog-oid oid))
      (load-shtml (web4r-path "examples/blog/shtml/show.shtml")))))

(defun blog-user-id (blog-ins)
  (user-id (get-instance-by-oid 'blog-user (user-oid blog-ins))))

(defun save-comment (oid)
  (aif (class-validation-errors 'comment)
       (page/error-msgs 'show it oid)
       (progn (make-pinstance 'comment `((blog-oid ,oid)))
              (redirect/msgs (w/p (page-uri "show" oid))
                             "Comment posted"))))

(defparameter *srv* (start-server))
;(stop-server *srv*)
