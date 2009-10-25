Tutorial Part 9. genpagesマクロを使わずにアプリケーション作成: blog例
======================================================================

認証なし
---------

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (asdf:oos 'asdf:load-op :web4r))
    
    (in-package :cl-user)
    (defpackage :blog (:use :cl :web4r))
    (in-package :blog)
    
    (ele:open-store *example-bdb*)
    
    (defpclass blog ()
      ((title :length 50)
       (body  :length 300)))
    
    (defpage blog ()           (index-page  'blog))
    (defpage blog/show   (oid) (show-page   'blog oid))
    (defpage blog/edit   (oid) (edit-page   'blog oid))
    (defpage blog/delete (oid) (delete-page 'blog oid))
    
    (start-server)

認証あり
---------

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (asdf:oos 'asdf:load-op :web4r))
    
    (in-package :cl-user)
    (defpackage :blog (:use :cl :web4r))
    (in-package :blog)
    
    (ele:open-store '(:BDB "/tmp/db"))
    
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
    
    (start-server)
