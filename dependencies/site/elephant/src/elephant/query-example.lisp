;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; query-example.lisp -- Simple example of conjuctive query interface
;;; 
;;; By Ian Eslick <ieslick common-lisp net>
;;; 
;;; part of
;;;
;;; Elephant: an object-oriented database for Common Lisp
;;;
;;; Copyright (c) 2004 by Andrew Blumberg and Ben Lee
;;; <ablumberg@common-lisp.net> <blee@common-lisp.net>
;;;
;;; Portions Copyright (c) 2005-2007 by Robert Read and Ian Eslick
;;; <rread common-lisp net> <ieslick common-lisp net>
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;

(in-package :elephant)

;; TEST DATA

(defparameter *constraint-spec*
  '(:BDB "/Users/eslick/Work/db/constraint/"))

(defun print-name (inst)
  (format t "Name: ~A~%" (slot-value inst 'name)))

(defpclass person ()
  ((name :initarg :name :index t)
   (salary :initarg :salary :index t)
   (department :initarg :dept)))

(defpclass department ()
  ((name :initarg :name)
   (manager :initarg :manager)))

(defparameter *names* 
    '("Jacob"
      "Emily"
"Michael"
"Emma"
"Joshua"
"Madison"
"Matthew"
"Abigail"
"Ethan"
"Olivia"
"Andrew"
"Isabella"
"Daniel"
"Hannah"
"Anthony"
"Samantha"
"Christopher"
"Ava"
"Joseph"
"Ashley"
))

(defun test-dataset ()
  (let* ((greg (make-instance 'person :name "Greg" :salary 100000))
	 (sally (make-instance 'person :name "Sally" :salary 110000))
	 (mkt (make-instance 'department :name "Marketing" :manager greg))
	 (engr (make-instance 'department :name "Engineering" :manager sally)))
    (setf (slot-value greg 'department) mkt)
    (setf (slot-value sally 'department) engr)
    (with-transaction ()
      (loop for i from 0 upto 500 do
	   (make-instance 'person 
			  :name (format nil "~A~A" (utils:random-element *names*) i)
			  :salary (floor (+ (* (random 1000) 150) 30000))
			  :department (if (= 1 (random 2)) mkt engr))))))

(defun print-person (person &optional (stream t))
  (format stream "name: ~A salary: ~A dept: ~A~%" 
	  (slot-value person 'name) (slot-value person 'salary) 
	  (slot-value (slot-value person 'department) 'name)))

(defun example-query1 ()
  "Performs a query against a single class.  Trivial string & integer matching"
  (map-class-query #'print-person
		   '((person name = "Greg")
		     (person salary >= 100000))))

(defun example-query2 (low-salary high-salary)
  "Parameterized query"
  (map-class-query #'print-person
		   `((person salary >= ,low-salary)
		     (person salary <= ,high-salary))))

		   
