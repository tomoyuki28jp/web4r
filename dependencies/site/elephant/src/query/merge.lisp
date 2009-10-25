;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; merge.lisp -- Implement efficient OID lists for merge-sort
;;; 
;;; Copyright (c) 2007 by  Ian S. Eslick
;;; <ieslick at common-lisp.net>
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Limited General Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;

(in-package :elephant)

;;
;; Quick and dirty oid-set abstraction
;;

;; Notes:
;; Add pool abstraction later to reuse tenured arrays
;; Use foreign memory?
;; Paged-to-disk approaches for when we blow out main memory?

(defparameter *initial-set-size* 1000)
(defparameter *set-growth-factor* 1.5)

;;
;; Create sets
;; 

(defun default-oid-set-elements ()
  (make-array 1000 
	      :element-type 'fixnum
	      :initial-element 0
	      :fill-pointer 0
	      :adjustable t))

(defclass oid-set ()
  ((elements :accessor set-elements :initarg :elements 
	     :initform (default-oid-set-elements))
   (oid-order :accessor set-ordered-p :initarg :ordered-p :initform nil)))

(defmethod push-oid (oid (set oid-set))
  "If values are ascending, set is built in sorted order"
  (vector-push-extend oid (set-elements set) (floor (* *set-growth-factor* (length (set-elements set)))))
  (setf (set-ordered-p set) nil)
  oid)

(defmethod pop-oid ((set oid-set))
  (vector-pop (set-elements set)))

;; do we need remove/insert?

;;
;; Operations on sets
;; 

(defmethod sorted-elements ((set oid-set))
  (if (set-ordered-p set)
      (set-elements set)
      (sort-set set)))

(defmethod sort-set ((set oid-set))
  "Sort the set elements and return the elements"
  (sort (set-elements set) #'<)
  (setf (set-ordered-p set) t)
  (set-elements set))

(defmethod sort-merge-sets ((set1 oid-set) (set2 oid-set) &optional (remove-duplicates t))
  (let ((new-elements
	 (merge '(array fixnum (*) :adjustable t :fill-pointer t) (sorted-elements set1) (sorted-elements set2) #'<)))
    (make-instance 'oid-set 
		   :elements (if remove-duplicates
				 (delete-duplicates new-elements)
				 new-elements)
		   :ordered-p t)))

(defmethod merge-sets ((set1 oid-set) (set2 oid-set) &optional (remove-duplicates t))
  (let ((target (make-instance 'oid-set :ordered-p t)))
    (let ((elts1 (sorted-elements set1))
	  (elts2 (sorted-elements set2))
	  (offset1 0)
	  (offset2 0)
	  (last nil))
      (loop until (or (= offset1 (fill-pointer elts1))
		      (= offset2 (fill-pointer elts2)))
	   do
	   (let ((elt1 (aref elts1 offset1))
		 (elt2 (aref elts2 offset2)))
	     (cond ((= elt1 elt2) 
		    (incf offset1) 
		    (incf offset2)
		    (unless (and remove-duplicates (eq last elt1))
		      (push-oid elt1 target) 
		      (setf last elt1)))
		   ((< elt1 elt2)
		    (push-oid elt1 target)
		    (incf offset1))
		   ((< elt2 elt1)
		    (push-oid elt2 target)
		    (incf offset2))))))
    target))

(defmethod intersect-sets ((set1 oid-set) (set2 oid-set))
  (let ((target (make-instance 'oid-set :ordered-p t)))
    (let ((elts1 (sorted-elements set1))
	  (elts2 (sorted-elements set2))
	  (offset1 0)
	  (offset2 0))
      (loop until (or (= offset1 (fill-pointer elts1))
		      (= offset2 (fill-pointer elts2)))
	   do
	   (let ((elt1 (aref elts1 offset1))
		 (elt2 (aref elts2 offset2)))
	     (cond ((= elt1 elt2)
		    (incf offset1)
		    (incf offset2)
		    (push-oid elt1 target))
		   ((< elt1 elt2)
		    (incf offset1))
		   ((< elt2 elt1)
		    (incf offset2))))))
    target))


;;
;; Test code
;;

(defun push-random-oids (set amount &optional (max 1000))
  (loop for i from 0 upto amount do
    (push-oid (random max) set))
  set)



