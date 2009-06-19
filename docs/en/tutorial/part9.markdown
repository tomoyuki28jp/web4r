Tutorial Part 9. Make an application without genpages macro: blog example
==========================================================================

Design policy of this example application
------------------------------------------
- Make an application without genpages macro
- Put lisp programs and sml codes together to make it easy to understand

The codes
----------
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (asdf:oos 'asdf:load-op :web4r))
    
    (in-package :cl-user)
    (defpackage :blog (:use :cl :web4r :sml :my-util))
    (in-package :blog)
    
    (ele:open-store '(:BDB "/tmp/db"))
    
    (defpclass blog-user (user)
      ((email :format :email :unique t)
       (blog-title :length 256)))
    
    (defpclass blog ()
      ((user-oid :index t :hide-for :all)
       (title :length 50   :size 40)
       (body  :length 3000 :rows 25 :cols 70)))
    
    (defun owner? (oid)
      (owner-p 'blog 'user-oid oid))
    
    (defun owner-check (oid)
      (unless (owner? oid)
        (redirect/error-msgs (page-uri "blog") "Illegal action")))
    
    ; you can use define-template or with-sml-file macro for this.
    (defmacro html (title &rest body)
      `[html :xmlns "http://www.w3.org/1999/xhtml" :lang "en"
             [head [meta :http-equiv "content-type"
                         :content "text/html; charset=utf-8" /]
                   [link :rel "stylesheet" :href "/css/common.css"
                         :type "text/css" :charset "utf-8" /]
                   [title ,title]]
             [body ,@body]])
    
    (defpage blog (:default)
      (multiple-value-bind (items pager slots)
          (items-per-page 'blog 'updated-at)
        (html "Listing blogs"
              (msgs)
              (page-links pager)
              (if items
                  [table
                   [tr (dolist (s slots) [th (slot-label s)])
                       (dotimes (x 3)    [th (safe "&nbsp;")])]
                   (dolist (i items)
                     [tr (dolist (s slots) [td (slot-display-value i s)])
                         (let ((oid (oid i)))
                           [td [a :href (page-uri "blog" "show" oid) "Show"]]
                           (when (owner? oid)
                             [td [a :href (page-uri "blog" "edit" oid) "Edit"]]
                             [td [a :href (page-uri "blog" "delete" oid) "Delete"]]))])]
                  [p "There is no blog"])
              [a :href (page-uri "blog" "edit") "New blog"]
              (if (login-user)
                  [a :href (page-uri "logout") "Logout"]
                  [div [a :href (page-uri "login")  "Login"] " | "
                       [a :href (page-uri "regist") "Sign up"]]))))
    
    (defpage blog/show (oid)
      (html "Show blog"
            (aif (get-instance-by-oid 'blog oid)
                 [table
                  (dolist (s (get-excluded-slots 'blog))
                    [tr [th (slot-label s)]
                        [td :id (slot-id s) (slot-display-value it s)]])]
                 [p "No such post."])))
    
    (defpage blog/edit (oid :auth)
      (when oid (owner-check oid))
      (let ((ins (get-instance-by-oid 'blog oid)))
        (html (concat (if oid "Editing" "New") " blog")
              (form-for/cont (edit/cont 'blog ins (page-uri "blog")
                                :slot-values `((user-oid . ,(login-user-oid))))
                :class 'blog :instance ins :submit (if oid "Update" "Create")))))
    
    (defpage blog/delete (oid :auth)
      (owner-check oid)
      (redirect/msgs (page-uri "blog")
                     (aif (drop-instance-by-oid 'blog oid)
                          "The blog post was deleted."
                          "No such post")))
    
    (start-server)
