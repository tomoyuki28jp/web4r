;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; pset.lisp - A generic implementation of persistent sets that can be 
;;;             overridden for a more efficient implementation by a data 
;;;             store.  Uses btrees to emulate simple sets
;;; 
;;; By Ian Eslick <ieslick at common-lisp.net>
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

;;
;; Default implementation of persistent sets
;;

;;
;; Public API
;;

(defclass pset (persistent-collection) ()
  (:documentation "An unordered persistent collection of unique elements according to
                   serializer equal comparison"))

(defgeneric insert-item (item pset)
  (:documentation "Insert a new item into the pset"))

(defgeneric remove-item (item pset)
  (:documentation "Remove specified item from pset"))

(defgeneric map-pset (fn pset)
  (:documentation "Map operator for psets"))

(defgeneric find-item (item pset &key key test)
  (:documentation "Find a an item in the pset using key and test"))

(defgeneric pset-list (pset)
  (:documentation "Convert items of pset into a list for processing"))

(defgeneric build-pset (sc)
  (:documentation "Construct an empty default pset or backend specific pset.
                   This is an internal function used by make-pset"))

(defgeneric drop-pset (pset)
  (:documentation "Release pset storage to database for reuse"))

;; NOTE: Other operators?
;; - Efficient union, intersection and difference fn's exploiting an underlying
;;   sorted order?
;; - map delete operator?

;;
;; Default implementation based on btrees
;;

(defclass default-pset (pset)
  ((btree :accessor pset-btree :initarg :btree)))

(defmethod build-pset ((sc store-controller))
  "Default pset method; override if backend has better policy"
  (let ((btree (make-btree sc)))
    (make-instance 'default-pset :btree btree :sc sc :from-oid (oid btree))))

(defun make-pset (&key items pset (sc *store-controller*))
  (let ((new-pset (build-pset sc)))
    (when (and items pset)
      (error "Can only initialize a new pset with item list or pset to copy, not both"))
    (when items
      (mapc (lambda (item)
	      (insert-item item new-pset))
	    items))
    (when pset
      (map-pset (lambda (item)
		  (insert-item item new-pset))
		pset))
    new-pset))

(defmethod insert-item (item (pset default-pset))
  (setf (get-value item (pset-btree pset)) t)
  item)

(defmethod remove-item (item (pset default-pset))
  (remove-kv item (pset-btree pset))
  item)

(defmethod find-item (item (pset default-pset) &key key (test #'equal))
  (if (not (or key test))
      (get-value item (pset-btree pset))
      (map-btree (lambda (elt dc)
		   (declare (ignore dc))
		   (let ((cmpval (if key (funcall key elt) elt)))
		     (if (funcall test item cmpval)
			 (return-from find-item elt))))
		 (pset-btree pset))))

(defmethod map-pset (fn (pset default-pset))
  (map-btree fn (pset-btree pset))
  pset)

(defmethod pset-list ((pset default-pset))
  (let ((list nil))
    (flet ((collect (item)
	     (push item list)))
      (declare (dynamic-extent collect))
      (map-btree (lambda (item dc)
		   (declare (ignore dc))
		   (push item list))
		 (pset-btree pset)))
    list))

(defmethod drop-pset ((pset default-pset))
  (ensure-transaction (:store-controller *store-controller*)
    (with-btree-cursor (cur (pset-btree pset))
      (loop for exists? = (cursor-first cur)
	    then (cursor-next cur)
	    while exists?
            do (cursor-delete cur)))))
            
            
	  
	   

