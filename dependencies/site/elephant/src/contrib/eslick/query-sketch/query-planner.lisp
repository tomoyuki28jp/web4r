;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; query-planner.lisp -- Implement syntax for the elephant query engine
;;; 
;;; Copyright (c) 2007 by  Ian S. Eslick
;;; <ieslick at common-lisp.net>
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Limited General Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;

(in-package :elephant)

;; ================================================================================
;; Simple graph abstraction for keeping track of plan constraints
;; ================================================================================

(defclass edge ()
  ((type :accessor edge-type :initarg :type :initform nil)
   (source :accessor edge-source :initarg :source :initform nil)
   (target :accessor edge-target :initarg :target :initform nil)))

(defclass graph ()
  ((edges :accessor edge-list :initarg :edges :initform nil)
   (nodes :accessor node-list :initarg :nodes :initform nil)))

;; =============================
;; Query Plans
;; =============================

;; These operations are the basis for estimating the cost
;; of different orderings of query operations.  For simple
;; queries this is unnecessary, but for queries with constraints
;; between classes, this is very useful.

(defclass query-op ()
  ((set-size :accessor set-size :initarg :set-size :initform 0)
   (page-queries :accessor page-queries :initarg :page-queries :initform 0)
   (slot-queries :accessor slot-queries :initarg :slot-queries :initform 0)))

(defclass instance-op ()
  ((class :accessor op-class :initarg :cost)
   (constraints :accessor op-constraints :initarg :constraints)))

(defclass index-op (instance-op)
  ((index :accessor query-index :initarg :cons)))

(defclass scan-op (instance-op) 
  ()
  (:documentation "Scan the class applying the per-instance operator"))

;; intersection, unions
(defclass merge-op (query-op)
  ((merge-type :accessor merge-type :initarg :type :initform :intersection)))



(defun compute-constraint-graph (constraint-expr)
  ""

(defun compute-query-plan (constraints)
  "Given a constraint graph, compute a query plan"
