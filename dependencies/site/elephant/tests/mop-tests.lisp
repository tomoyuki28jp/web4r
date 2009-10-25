;;; mop-tests.lisp
;;;
;;; part of
;;;
;;; Elephant: an object-oriented database for Common Lisp
;;;
;;; Copyright (c) 2004 by Andrew Blumberg and Ben Lee
;;; <ablumberg@common-lisp.net> <blee@common-lisp.net>
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :ele-tests)

(deftest non-transient-class-slot-1
    (signals-condition
      ;; This should fail (principle of least surprise)
      (defclass non-transient-class-slot-1 ()
	((slot3 :accessor slot3 :allocation :class))
	(:metaclass persistent-metaclass)))
  t)

(deftest non-transient-class-slot-2
    (signals-condition
      ;; as should this
      (defclass non-transient-class-slot-2 ()
	((slot3 :accessor slot3 :allocation :class :transient nil))
	(:metaclass persistent-metaclass)))
  t)

(deftest transient-class-slot
    (finishes
     ;; but this should be fine
     (defclass transient-class-slot ()
       ((slot3 :accessor slot3 :allocation :class :transient t))
       (:metaclass persistent-metaclass)))
  t)

(deftest class-definers
    (finishes
     (defclass p-class ()
       ((slot1 :accessor slot1)
	(slot2 :accessor slot2 :transient t)
	(slot3 :accessor slot3 :allocation :class :transient t))
       (:metaclass persistent-metaclass))
     (finalize-inheritance (find-class 'p-class))
     (defclass nonp-class ()
       ((slot1 :accessor slot1)
	(slot2 :accessor slot2)
	(slot3 :accessor slot3 :allocation :class)))
     (finalize-inheritance (find-class 'nonp-class))
     (defclass minus-p-class ()
       ((slot1 :accessor slot1 :transient t)
	(slot2 :accessor slot2)
	(slot3 :accessor slot3))
       (:metaclass persistent-metaclass))
     (finalize-inheritance (find-class 'minus-p-class))
     (defclass switch-transient ()
       ((slot1 :accessor slot1 :transient t)
	(slot2 :accessor slot2))
       (:metaclass persistent-metaclass))
     (finalize-inheritance (find-class 'switch-transient))
     (defclass make-persistent ()
       ((slot2 :accessor slot2))
       (:metaclass persistent-metaclass))
     (finalize-inheritance (find-class 'make-persistent)))
  t)

(deftest bad-inheritence
    (signals-condition
     ;; This should fail
     (defclass bad-inheritence (p-class) ()))
  t)

(deftest mixes
    (finishes
     ;; but this should be fine
     (defclass mix-1 (p-class nonp-class) ()
       (:metaclass persistent-metaclass))
     (finalize-inheritance (find-class 'mix-1))
     ;; This should be ok
     (defclass mix-2 (p-class minus-p-class) ()
       (:metaclass persistent-metaclass))
     (finalize-inheritance (find-class 'mix-2))
     ;; This should be ok
     (defclass mix-3 (minus-p-class p-class) ()
       (:metaclass persistent-metaclass))
     (finalize-inheritance (find-class 'mix-3))
     ;; This should be ok 
     (defclass mix-4 (switch-transient p-class) ()
       (:metaclass persistent-metaclass))
     (finalize-inheritance (find-class 'mix-4))
     ;; This should be ok
     (defclass mix-5 (p-class switch-transient) ()
       (:metaclass persistent-metaclass))
     (finalize-inheritance (find-class 'mix-5))
     ;; should work
     (defclass mix-6 (make-persistent p-class) ()
       (:metaclass persistent-metaclass))
     (finalize-inheritance (find-class 'mix-6)))
  t)

(deftest mixes-right-slots
    (are-not-null
     (typep (find-slot-def 'mix-1 'slot1) 'ele::persistent-slot-definition)
     (typep (find-slot-def 'mix-1 'slot2) 'ele::transient-slot-definition)
     (typep (find-slot-def 'mix-1 'slot3) 'ele::transient-slot-definition)
     (typep (find-slot-def 'mix-2 'slot1) 'ele::persistent-slot-definition)
     (typep (find-slot-def 'mix-2 'slot2) 'ele::persistent-slot-definition)
     (typep (find-slot-def 'mix-2 'slot3) 'ele::persistent-slot-definition)
     (typep (find-slot-def 'mix-3 'slot1) 'ele::persistent-slot-definition)
     (typep (find-slot-def 'mix-3 'slot2) 'ele::persistent-slot-definition)
     (typep (find-slot-def 'mix-3 'slot3) 'ele::persistent-slot-definition)
     (typep (find-slot-def 'mix-4 'slot1) 'ele::persistent-slot-definition)
     (typep (find-slot-def 'mix-4 'slot2) 'ele::persistent-slot-definition)
     (typep (find-slot-def 'mix-4 'slot3) 'ele::transient-slot-definition)
     (typep (find-slot-def 'mix-5 'slot1) 'ele::persistent-slot-definition)
     (typep (find-slot-def 'mix-5 'slot2) 'ele::persistent-slot-definition)
     (typep (find-slot-def 'mix-5 'slot3) 'ele::transient-slot-definition)
     (typep (find-slot-def 'mix-6 'slot1) 'ele::persistent-slot-definition)
     (typep (find-slot-def 'mix-6 'slot2) 'ele::persistent-slot-definition)
     (typep (find-slot-def 'mix-6 'slot3) 'ele::transient-slot-definition))
  t t t t t t t t t t t t t t t t t t)

(deftest inherit     
    (finishes
     (defclass make-persistent2 (p-class)
       ((slot2 :accessor slot2)
	(slot4 :accessor slot4 :transient t))
       (:metaclass persistent-metaclass))
     (finalize-inheritance (find-class 'make-persistent2)))
  t)

(deftest inherit-right-slots
    (are-not-null
     (typep (find-slot-def 'make-persistent2 'slot1) 
	    'ele::persistent-slot-definition)
     (typep (find-slot-def 'make-persistent2 'slot2) 
	    'ele::persistent-slot-definition)
     (typep (find-slot-def 'make-persistent2 'slot3) 
	    'ele::transient-slot-definition)
     (typep (find-slot-def 'make-persistent2 'slot4) 
	    'ele::transient-slot-definition))
  t t t t)

(deftest initform-classes
    (finishes
      (defclass p-initform-test () 
	((slot1 :initform 10)) 
	(:metaclass persistent-metaclass))
      (defclass p-initform-test-2 () 
	((slot1 :initarg :slot1 :initform 10)) 
	(:metaclass persistent-metaclass))
      )
  t)
      
(deftest initform-test
    (slot-value (make-instance 'p-initform-test :sc *store-controller*) 'slot1)
  10)

(deftest initarg-test
    (values
     (slot-value (make-instance 'p-initform-test-2 :sc *store-controller*) 'slot1)
     (slot-value (make-instance 'p-initform-test-2 :slot1 20 :sc *store-controller*) 'slot1))
  10 20)

(deftest no-eval-initform
    (finishes
      (defclass no-eval-initform ()
	((slot1 :initarg :slot1 :initform (error "Shouldn't be called")))
	(:metaclass persistent-metaclass))
      (make-instance 'no-eval-initform :slot1 "something" :sc *store-controller* )
      t)
  t)

(deftest redefclass
    (progn
      (defclass redef () () (:metaclass persistent-metaclass))
      (defclass redef () () (:metaclass persistent-metaclass))
      (is-not-null (subtypep 'redef 'persistent-object)))
  t)

(deftest makunbound
    (let ((p (make-instance 'p-class :sc *store-controller*)))
      (with-transaction (:store-controller *store-controller*)
	(setf (slot1 p) t)
	(slot-makunbound p 'slot1))
      (signals-condition (slot1 p)))
  t)

(deftest update-class
    (progn
      (defclass update-class () 
	((slot1 :initform 1 :accessor slot1))
	(:metaclass persistent-metaclass))
      (let* ((foo (make-instance 'update-class :sc *store-controller*)))
	(defclass update-class ()
	  ((slot2 :initform 2 :accessor slot2))
	  (:metaclass persistent-metaclass))
	(values
	 (slot2 foo)
	 (signals-condition (slot1 foo)))))
  2 t)

(deftest change-class
    (progn
      (defclass class-one ()
	((slot1 :initform 1 :accessor slot1))
	(:metaclass persistent-metaclass))

      (defclass class-two ()
	((slot1 :initform 0 :accessor slot1)
	 (slot2 :initform 2 :accessor slot2))
	(:metaclass persistent-metaclass))

	(let* ((foo (make-instance 'class-one :sc *store-controller*)))
	  (change-class foo (find-class 'class-two))
	  (values
	   (slot1 foo)
	   (slot2 foo))))
  1 2)

;;
;; ISE NOTE: This violates single backend testing, I've removed it for now
;;
;; (deftest change-class2
;;     (with-transaction (:store-controller *store-controller*)
;;       (let ((foo (make-btree *store-controller*)))
;; 	(change-class foo (find-class 
;; 			   (if (typep *store-controller* 'bdb-store-controller)
;; 			       'bdb-indexed-btree
;; 			       'sql-indexed-btree)
;; 			   ))
;; 	(is-not-null (indices foo))))
;;   t)

(deftest change-class3
    (progn
      (defclass class-one ()
	((slot1 :accessor slot1))
	(:metaclass persistent-metaclass))
      
      (defclass class-two ()
	((slot1 :initform 0 :accessor slot1)
	 (slot2 :initform 2 :accessor slot2))
	(:metaclass persistent-metaclass))

      	(let* ((foo (make-instance 'class-one :sc *store-controller*)))
	  (change-class foo (find-class 'class-two))
	  (values
	   (slot1 foo)
	   (slot2 foo))))
  0 2)


      
