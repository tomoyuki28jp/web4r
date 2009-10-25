;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; query.lisp -- Implement syntax for the elephant query engine
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
;; User query API
;;

(defmacro map-query (fn constraints)
  (let ((classes (constraint-expr-classes constraints))
	(plan (compile-query-plan (parse-constraints constraints))))
    `(map-query-plan ,fn ,plan)))

;;(defun map-constraints (fn classes plan)
;;  (declare (dynamic-extent sc))

;;
;; Wrappers and standard operations for map-constraints
;;

(defmacro with-collector ((collector-var &key function init expr) &body body)
  "Instantiates a list collector to pass to a mapping function and
   the whole expression returns the result of the collector.  For
   something other than list construction, expr can be used.  If function
   is used, it is assumed to be the name of a function of two variables
   of two arguments where the first argument is the collector variable
   and the second is the current value being mapped.  It should return
   the new value of the collector variable.  Otherwise it is an expression
   containing a followed by a function body
   has the form of an flet entry with one parameter (name (arg) body).
   Init is the initial value of the result variable"
  (with-gensyms (result-var elt)
    `(let ((,result-var ,init))
       (flet ((,collector-var
		  ,@(cond ((and (null expr) (null function))
			   `((,elt)
			     (push ,elt ,result-var)))
			  (function
			   `((,elt)
			     (setf ,result-var (funcall ,function ,result-var ,elt))))
			  (expr
			   (multiple-value-bind (arg &body body) expr
			     `((,arg) ,@body))))))
	 (declare (dynamic-extent ,collector-var))
	 ,@body))))
