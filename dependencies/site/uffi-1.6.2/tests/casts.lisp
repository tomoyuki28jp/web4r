;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICAION
;;;;
;;;; Name:          casts.lisp
;;;; Purpose:       Tests of with-cast-pointer
;;;; Programmer:    Kevin M. Rosenberg / Edi Weitz
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2003-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package #:uffi-tests)

(uffi:def-function ("cast_test_int" cast-test-int)
    ()
  :module "uffi_tests"
  :returning :pointer-void)

(uffi:def-function ("cast_test_float" cast-test-float)
    ()
  :module "uffi_tests"
  :returning :pointer-void)

(deftest :cast.1
  (progn
    (uffi:with-cast-pointer (temp (cast-test-int) :int)
      (assert (= (uffi:deref-pointer temp :int) 23)))
    (let ((result (cast-test-int)))
      (uffi:with-cast-pointer (result2 result :int)
        (assert (= (uffi:deref-pointer result2 :int) 23)))
      (uffi:with-cast-pointer (temp result :int)
        (assert (= (uffi:deref-pointer temp :int) 23))))
    t)
  t)

(deftest :cast.2
    (progn
      (uffi:with-cast-pointer (temp (cast-test-float) :double)
        (assert (= (uffi:deref-pointer temp :double) 3.21d0)))
      (let ((result (cast-test-float)))
        (uffi:with-cast-pointer (result2 result :double)
          (assert (= (uffi:deref-pointer result2 :double) 3.21d0)))
        (uffi:with-cast-pointer (temp result :double)
          (assert (= (uffi:deref-pointer temp :double) 3.21d0))))
      t)
  t)

