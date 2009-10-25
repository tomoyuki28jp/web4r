;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; query.lisp -- Implement conjunctive syntax as example for elephant query interface
;;; 
;;; By Ian S. Eslick, <ieslick common-lisp net>
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

(defparameter *string-relation-functions*
  `((< . ,#'string<)
    (< . ,#'string<=)
    (> . ,#'string>)
    (> . ,#'string>=)
    (= . ,#'equal)
    (!= . ,(lambda (x y) (not (equal x y))))))

(defparameter *number-relation-functions*
  `((< . ,#'<)
    (> . ,#'>)
    (= . ,#'=)
    (!= . ,#'(lambda (x y) (not (= x y))))))

(defun relation-string-function (rel)
  (cdr (assoc rel *string-relation-functions*)))

(defun relation-number-function (rel)
  (cdr (assoc rel *number-relation-functions*)))

(defun test-relation (rel ival tvals)
  (assert (or (and (numberp ival) (numberp (first tvals)))
	      (and (stringp ival) (stringp (first tvals)))))
  (typecase ival
    (string (funcall (relation-string-function rel) ival (first tvals)))
    (number (funcall (relation-number-function rel) ival (first tvals)))))
      
(defun get-query-instances (constraints)
  "Get a list of instances according to the query constraints"
  (declare (dynamic-extent constraints))
  (let ((list nil))
    (flet ((collect (inst)
	     (push inst list)))
      (declare (dynamic-extent collect))
      (map-class-query #'collect constraints))))

(defun map-class-query (fn constraints)
  "Map instances using the query constaints to filter objects, exploiting
   slot indices (for last query) and stack allocated test closures.  This is
   a minimally optimizing version that uses the first index it finds, and 
   then does a nested loop join on the rest of the parameters."
  (declare (dynamic-extent constraints))
  (assert (not (null constraints)))
  (destructuring-bind (class slot relation &rest values) (first constraints)
    (flet ((filter-by-relation (inst)
	     (when (test-relation relation (slot-value inst slot) values)
	       (funcall fn inst))))
      (declare (dynamic-extent filter-by-relation))
      (if (null (cdr constraints))
	  (if (find-inverted-index class slot)
	      (if (= (length values) 1)
		  (progn
		    (map-inverted-index fn class slot (first values) (first values))
		    (map-inverted-index fn class slot (first values) (second values))))
	      (map-class #'filter-by-relation class))
	  (map-class-query #'filter-by-relation (cdr constraints))))))
       
;;
;; Conjunctions of indices
;;

;;(defun map-classes (fn classes)
;;  (map-index-list fn (mapcar #'find-class-index classes)))

;;(defun map-index-list (fn indices)
;;  (dolist (index indices)
;;    (map-index fn index)))
