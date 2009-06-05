; To run:
; 1. Configure elephant
; 2. Replace the parameter of open-store if you don't have Elephant configured 
;    to work with the Berkeley DB
;    http://common-lisp.net/project/elephant/doc/elephant.html#Getting-Started
; 3. Compile and load this file
; 4. Go to http://localhost:8080/

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :web4r))

(in-package :cl-user)
(defpackage :blog
  (:use :cl :web4r :elephant :my-util :sml :hunchentoot)
  (:shadowing-import-from :web4r :defpclass))
(in-package :blog)

(open-store *example-bdb*)

(defpclass blog-user (user)
  ((email :format :email :unique t)
   (blog-title :length 256)))

(defpclass blog-post ()
  ((user-oid :index t :hide-for :all)
   (title :length 50   :size 40)
   (body  :length 3000 :rows 25 :cols 70)))

(defpclass comment ()
  ((blog-oid :index t :hide-for :all)
   (name    :length 50)
   (comment :length 3000 :rows 5 :cols 50)))

(defpage blog (:default)
  (multiple-value-bind (items pager)
      (per-page (get-instances-by-class 'blog-user))
    (load-sml (example-path "blog/sml/blog.sml"))))

(defpage blog/index (user-id)
  (let ((slots (get-excluded-slots 'blog-post))
        (owner-p (aand user-id (equal it (login-user-id)))))
    (multiple-value-bind (items pager)
        (per-page (get-instances-by-value 'blog-post 'user-oid (get-user-oid user-id)))
      (load-sml (example-path "blog/sml/blog_index.sml")))))

(defpage blog/edit (oid :auth)
  (when oid (blog-owner-check oid))
  (edit-page 'blog-post :oid oid :slot-values `((user-oid . ,(login-user-oid)))
             :redirect-uri (page-uri "blog" "index" (login-user-id))))

(defpage blog/delete (oid :auth)
  (blog-owner-check oid)
  (delete-page 'blog-post oid (my-page-uri)))

(defpage blog/show (oid)
  (let ((ins (get-instance-by-oid 'blog-post oid)))
    (multiple-value-bind (comments pager)
        (per-page (get-instances-by-value 'comment 'blog-oid oid))
      (load-sml (example-path "blog/sml/blog_show.sml")))))

(defun blog-owner-check (oid)
  (unless (owner-p 'blog-post 'user-oid oid)
    (redirect/error-msgs (my-page-uri) "Illegal action")))

(defun my-page-uri () (page-uri "blog" "index" (login-user-id)))

(defun post-comment/cont (oid)
  (edit/cont 'comment nil (request-uri*) :slot-values `((blog-oid . ,oid))))

(defvar *srv* (start-server))
;(stop-server *srv*)
