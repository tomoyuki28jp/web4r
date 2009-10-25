;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          arrays.lisp
;;;; Purpose:       UFFI test arrays
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Mar 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package #:uffi-tests)

(uffi:def-constant +column-length+ 10)
(uffi:def-constant +row-length+ 10)

(uffi:def-foreign-type long-ptr (* :long))

(deftest :array.1
    (let ((a (uffi:allocate-foreign-object :long +column-length+))
          (results nil))
      (dotimes (i +column-length+)
        (setf (uffi:deref-array a '(:array :long) i) (* i i)))
      (dotimes (i +column-length+)
        (push (uffi:deref-array a '(:array :long) i) results))
      (uffi:free-foreign-object a)
      (nreverse results))
  (0 1 4 9 16 25 36 49 64 81))


(deftest :array.2
    (let ((a (uffi:allocate-foreign-object 'long-ptr +row-length+))
          (results nil))
      (dotimes (r +row-length+)
        (declare (fixnum r))
        (setf (uffi:deref-array a '(:array (* :long)) r)
              (uffi:allocate-foreign-object :long +column-length+))
        (let ((col (uffi:deref-array a '(:array (* :long)) r)))
          (dotimes (c +column-length+)
            (declare (fixnum c))
            (setf (uffi:deref-array col '(:array :long) c) (+ (* r +column-length+) c)))))

      (dotimes (r +row-length+)
        (declare (fixnum r))
        (let ((col (uffi:deref-array a '(:array (* :long)) r)))
          (dotimes (c +column-length+)
            (declare (fixnum c))
            (push (uffi:deref-array col '(:array :long) c) results))))
      (uffi:free-foreign-object a)
      (nreverse results))
  (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99))


