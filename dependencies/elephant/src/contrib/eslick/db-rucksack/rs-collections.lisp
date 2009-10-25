;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; rs-collections.lisp -- view Berkeley DBs as Lisp collections
;;; 
;;; Initial version 6/4/2006 Ian Eslick
;;; <ieslick@common-lisp.net>
;;;
;;; Copyright (c) 2006 by Ian Eslick
;;; <ablumberg@common-lisp.net> <blee@common-lisp.net>
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

(defclass rs-btree (btree) ()
  (:documentation "A Rucksack BTree"))

(defmethod build-btree ((sc rs-store-controller))
  (make-instance 'rs-btree :sc sc))

(defmethod 