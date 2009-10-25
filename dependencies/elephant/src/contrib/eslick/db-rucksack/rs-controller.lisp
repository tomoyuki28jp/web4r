;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; rs-controller.lisp -- Lisp interface to a Berkeley DB store
;;; 
;;; Initial version 6/4/2006 Ian Eslick
;;; <ieslick@common-lisp.net>
;;; 
;;; part of
;;;
;;; Elephant: an object-oriented database for Common Lisp
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;

(in-package #:db-rucksack)

(defclass rs-store-controller (store-controller) 
  ((rucksack :accessor rs-store-db :initform nil)
   (slot-btree :accessor 

(defun rs-test-and-construct (spec)
  (if (rs-store-spec-p spec)
      (make-instance 'rs-store-controller :spec spec)
      (error (format nil "Unrecognized spec: ~A" spec))))

(eval-when (:compile-toplevel :load-toplevel)
  (register-backend-con-init :rs 'rs-test-and-construct))

(defun rs-store-spec-p (spec)
  (and (eq (first spec) :rucksack)
       (typecase (second spec)
	 (pathname t)
	 (string t)
	 (otherwise nil))))

(defmethod open-controller ((sc rs-store-controller) &rest args &key &allow-other-keys)
  (setf (rs-store-db sc) (apply #'open-rucksack 
			     (second (controller-spec sc))
			     args)))

(defmethod close-controller ((sc rs-store-controller))
  (rucksack-commit)
  (close-rucksack (rs-store-db sc)))

(defmethod next-oid ((sc rs-store-controller))
  "TODO"
  ;; create a proxy object using elephant's persistent-slot list
  ;; so we can store slots in it
  )

;;; Persistent slot protocol

(defmethod persistent-slot-reader ((sc rs-store-controller) instance name)
  "It would be nice to reuse the object interface that rucksack provides,
   but this is a cheap hack to get it running"
  ;; create comparison function in lisp
  ;; store slot values in btrees  
  )



		 

   
