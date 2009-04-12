; To run:
; 1. replace the parameter of open-store
;    http://common-lisp.net/project/elephant/doc/elephant.html#Getting-Started
; 2. compile and load this file
; 3. go to http://localhost:8080/customer

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :web4r))

(in-package :cl-user)
(defpackage :customer (:use :cl :web4r))
(in-package :customer)

(ele:open-store
 '(:clsql (:postgresql "localhost" "test" "postgres" "pgpass")))

(defpclass customer ()
  ((name         :length 50 :label "Full Name" :size 30)
   (password     :input :password :length (8 12) :hide t :comment "8-12 characters")
   (email        :type :email :unique t)
   (sex          :input :radio :options ("Male" "Female"))
   (marriage     :input :select :options ("single" "married" "divorced"))
   (hobbies      :input :checkbox :options ("sports" "music" "reading"))
   (birth-date   :type :date)
   (nickname     :length 50 :required nil)
   (phone-number :type (:regex "^\\d{3}-\\d{3}-\\d{4}$") :comment "xxx-xxx-xxxx")
   (zip-code     :type :integer :length (5 5) :comment "5 digit")
   (note         :length 3000 :rows 5 :cols 30)
   (image        :input :file :type :image :length (1000 500000) :required t)))

(defpage customer () (scaffold-index 'customer))
(defpage customer/show (oid)   (scaffold-show  'customer oid))
(defpage customer/delete (oid) (scaffold-delete 'customer oid))

(defpage customer/edit (oid)
  (let ((*with-slots* :all))
    (scaffold-edit 'customer :oid oid)))

(defparameter *srv* (start-server))
;(stop-server *srv*)
