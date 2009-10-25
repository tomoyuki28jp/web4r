;;; sql-tutorial.lisp
;;;
;;; part of
;;;
;;; Elephant: an object-oriented database for Common Lisp
;;;
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.



(asdf:operate 'asdf:load-op :elephant)
(asdf:operate 'asdf:load-op :ele-bdb)
(asdf:operate 'asdf:load-op :elephant-tests)
(in-package "ELEPHANT-TESTS")
(open-store *testdb-path*)
(add-to-root "my key" "my value")
(get-from-root "my key")

(setq foo (cons nil nil))

(add-to-root "my key" foo)
(add-to-root "my other key" foo)
(eq (get-from-root "my key")
                (get-from-root "my other key"))

(setf (car foo) T)

(get-from-root "my key")

(defclass my-persistent-class ()
        ((slot1 :accessor slot1)
         (slot2 :accessor slot2))
        (:metaclass persistent-metaclass))


(setq foo (make-instance 'my-persistent-class))

(add-to-root "foo" foo)

(add-to-root "bar" foo)

(eq (get-from-root "foo")
           (get-from-root "bar"))

(get-from-root "foo")
(setf (slot1 foo) "one")

(setf (slot2 foo) "two")
(slot1 foo)
(slot2 foo)
(setf (slot1 foo) "three")

(slot1 (get-from-root "bar"))

(setq *auto-commit* nil)
(with-transaction ()
        (setf (slot1 foo) 123456789101112)
        (setf (slot2 foo) "onetwothree..."))

(defvar *friends-birthdays* (make-btree))

(add-to-root "friends-birthdays" *friends-birthdays*)

(setf (get-value "Andrew" *friends-birthdays*)
     	(encode-universal-time 0 0 0 22 12 1976))
(setf (get-value "Ben" *friends-birthdays*)
     	(encode-universal-time 0 0 0 14 4 1976))

(get-value "Andrew" *friends-birthdays*)
(decode-universal-time *)
(defvar curs (make-cursor *friends-birthdays*))
 (cursor-close curs)
(setq curs (make-cursor *friends-birthdays*))
(cursor-current curs)
(cursor-first curs)
(cursor-next curs)
(cursor-next curs)
(cursor-close curs)
(with-transaction ()
  (with-btree-cursor (curs *friends-birthdays*)
    (loop
     (multiple-value-bind (more k v) (cursor-next curs)
       (unless more (return nil))
       (format t "~A ~A~%"  k v)))))

(defclass appointment ()
         ((date :accessor ap-date :initarg :date :type integer)
          (type :accessor ap-type :initarg :type :type string))
         (:metaclass persistent-metaclass))

(defvar *appointments* (with-transaction () (make-indexed-btree *store-controller*)))

(defun add-appointment (date type)
         (with-transaction ()
           (setf (get-value date *appointments*)
                 (make-instance 'appointment :date date :type type))))

(add-appointment (encode-universal-time 0 0 0 22 12 2004) "Birthday")
(add-appointment (encode-universal-time 0 0 0 14 4 2005) "Birthday")
(add-appointment (encode-universal-time 0 0 0 1 1 2005) "Holiday")
(defun key-by-type (secondary-db primary value)
         (declare (ignore secondary-db primary))
         (let ((type (ap-type value)))
           (when type
             (values t type))))
(with-transaction ()
         (add-index *appointments* :index-name 'by-type
                                   :key-form 'key-by-type
                                   :populate t))
(defvar *by-type* (get-index *appointments* 'by-type))

(decode-universal-time (ap-date (get-value "Holiday" *by-type*)))


(with-btree-cursor (curs *by-type*)
         (loop for (more? k v) =
               (multiple-value-list (cursor-set curs "Birthday"))
     	  then (multiple-value-list (cursor-next-dup curs))
               do
     	  (unless more? (return t))
     	  (multiple-value-bind (s m h d mo y)
     	      (decode-universal-time (ap-date v))
     	    (declare (ignore s m h))
     	    (format t "~A/~A/~A~%" mo d y))))


