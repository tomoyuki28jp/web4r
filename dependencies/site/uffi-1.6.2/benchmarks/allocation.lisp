;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          allocation.cl
;;;; Purpose:       Benchmark allocation and slot-access speed
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Mar 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))
(in-package :cl-user)


(defun stk-int ()
  #+allegro
  (ff:with-stack-fobject (ptr :int)
    (setf (ff:fslot-value ptr) 0))
  #+lispworks
  (fli:with-dynamic-foreign-objects ((ptr :int))
    (setf (fli:dereference ptr) 0))
  #+cmu
  (alien:with-alien ((ptr alien:signed))
    (let ((p (alien:addr ptr)))
      (setf (alien:deref p) 0)))
  #+sbcl
  (sb-alien:with-alien ((ptr sb-alien:signed))
    (let ((p (sb-alien:addr ptr)))
      (setf (sb-alien:deref p) 0)))
  )

(defun stk-vector ()
  #+allegro
  (ff:with-stack-fobject (ptr '(:array :int 10) )
    (setf (ff:fslot-value ptr 5) 0))
  #+lispworks
  (fli:with-dynamic-foreign-objects ((ptr (:c-array :int 10)))
    (setf (fli:dereference ptr 5) 0))
  #+cmu
  (alien:with-alien ((ptr (alien:array alien:signed 10)))
    (setf (alien:deref ptr 5) 0))
  #+sbcl
  (sb-alien:with-alien ((ptr (sb-alien:array sb-alien:signed 10)))
    (setf (sb-alien:deref ptr 5) 0))
  )

(defun stat-int ()
  #+allegro
  (let ((ptr (ff:allocate-fobject :int :c)))
    (declare (dynamic-extent ptr))
    (setf (ff:fslot-value-typed :int :c ptr) 0)
    (ff:free-fobject ptr))
  #+lispworks
  (let ((ptr (fli:allocate-foreign-object :type :int)))
    (declare (dynamic-extent ptr))
    (setf (fli:dereference ptr) 0)
    (fli:free-foreign-object ptr))
  #+cmu
  (let ((ptr (alien:make-alien (alien:signed 32))))
    (declare ;;(type (alien (* (alien:unsigned 32))) ptr)
             (dynamic-extent ptr))
    (setf (alien:deref ptr) 0)
    (alien:free-alien ptr))
  #+sbcl
  (let ((ptr (sb-alien:make-alien (sb-alien:signed 32))))
    (declare ;;(type (alien (* (alien:unsigned 32))) ptr)
     (dynamic-extent ptr))
    (setf (sb-alien:deref ptr) 0)
    (sb-alien:free-alien ptr))
  )

(defun stat-vector ()
  #+allegro
  (let ((ptr (ff:allocate-fobject '(:array :int 10) :c)))
    (declare (dynamic-extent ptr))
    (setf (ff:fslot-value-typed '(:array :int 10) :c ptr 5) 0)
    (ff:free-fobject ptr))
  #+lispworks
  (let ((ptr (fli:allocate-foreign-object :type '(:c-array :int 10))))
    (declare (dynamic-extent ptr))
    (setf (fli:dereference ptr 5) 0)
    (fli:free-foreign-object ptr))
  #+cmu
  (let ((ptr (alien:make-alien (alien:array (alien:signed 32) 10))))
    (declare ;;(type (alien (* (alien:unsigned 32))) ptr)
             (dynamic-extent ptr))
    (setf (alien:deref ptr 5) 0)
    (alien:free-alien ptr))
  #+sbcl
  (let ((ptr (sb-alien:make-alien (sb-alien:array (sb-alien:signed 32) 10))))
    (declare ;;(type (sb-alien (* (sb-alien:unsigned 32))) ptr)
             (dynamic-extent ptr))
    (setf (sb-alien:deref ptr 5) 0)
    (sb-alien:free-alien ptr))
  )


(defun stk-vs-stat ()
  (format t "~&Stack allocation, Integer")
  (time (dotimes (i 1000)
          (dotimes (j 1000)
            (stk-int))))
  (format t "~&Static allocation, Integer")
  (time (dotimes (i 1000)
          (dotimes (j 1000)
            (stat-int))))
  (format t "~&Stack allocation, Vector")
  (time (dotimes (i 1000)
          (dotimes (j 1000)
            (stk-int))))
  (format t "~&Static allocation, Vector")
  (time (dotimes (i 1000)
          (dotimes (j 1000)
            (stat-int))))
)


(stk-vs-stat)



