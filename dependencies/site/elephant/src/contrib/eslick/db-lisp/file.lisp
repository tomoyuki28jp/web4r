;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; ele-lisp.asd -- ASDF system definition for elephant lisp backend
;;; 
;;; part of
;;;
;;; Elephant Object Oriented Database: Common Lisp Backend
;;;
;;; Copyright (c) 2007 by Ian Eslick
;;; <ieslick at common-lisp.net>
;;;
;;; Elephant Lisp Backend users are granted the rights to distribute
;;; and use this software as governed by the terms of the Lisp Lesser
;;; GNU Public License (http://opensource.franz.com/preamble.html),
;;; also known as the LLGPL.

(in-package :db-lisp)

(defclass binary-file ()
  ((path :initarg :path :initarg "" :accessor binary-file-path)
   (stream :initarg :stream :accessor binary-file-stream)))

(defmethod initialize-instance :after ((file binary-file) &key (if-does-not-exist :create) (if-exists :new-version))
  (assert (binary-file-path file))
  (setf (binary-file-stream file)
	(open (binary-file-path file)
	      :direction :io :element-type '(unsigned-byte 8)
	      :if-exists if-exists
	      :if-does-not-exist if-does-not-exist)))

(defmethod close-file ((bf binary-file))
  (close (binary-file-stream bf)))


