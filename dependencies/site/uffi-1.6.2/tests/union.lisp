;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          union.lisp
;;;; Purpose:       UFFI Example file to test unions
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Mar 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package #:uffi-tests)

(uffi:def-union tunion1
    (char :char)
  (int :int)
  (uint :unsigned-int)
  (sf :float)
  (df :double))

(defvar *u* (uffi:allocate-foreign-object 'tunion1))
(setf (uffi:get-slot-value *u* 'tunion1 'uint)
      #-(or sparc sparc-v9 powerpc ppc)
      (+ (* 1 (char-code #\A))
         (* 256 (char-code #\B))
         (* 65536 (char-code #\C))
         (* 16777216 128))
      #+(or sparc sparc-v9 powerpc ppc)
      (+ (* 16777216 (char-code #\A))
         (* 65536 (char-code #\B))
         (* 256 (char-code #\C))
         (* 1 128)))

(deftest :union.1
    (uffi:ensure-char-character
     (uffi:get-slot-value *u* 'tunion1 'char))
  #\A)

(deftest :union.2
    (uffi:ensure-char-integer
     (uffi:get-slot-value *u* 'tunion1 'char))
  65)

#-(or sparc sparc-v9 openmcl digitool)
(deftest :union.3 (plusp (uffi:get-slot-value *u* 'tunion1 'uint)) t)


(uffi:def-union foo-u
    (bar :pointer-self))

(uffi:def-foreign-type foo-u-ptr (* foo-u))

;; tests that compilation worked
(deftest :unions.4
  (with-foreign-object (p 'foo-u)
    t)
  t)

(deftest :unions.5
    (progn
      (uffi:def-foreign-type foo-union (:union foo-u))
      t)
  t)




