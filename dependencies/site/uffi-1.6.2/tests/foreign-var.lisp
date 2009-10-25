;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          foreign-var
;;;; Purpose:       Tests of foreign variables
;;;; Authors:       Kevin M. Rosenberg and Edi Weitz
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2003-2005 by Kevin M. Rosenberg
;;;
;;;; *************************************************************************

(in-package #:uffi-tests)

(def-foreign-var "uchar_13" :unsigned-byte "uffi_tests")
(def-foreign-var "schar_neg_120" :byte "uffi_tests")
(def-foreign-var "uword_257" :unsigned-short "uffi_tests")
(def-foreign-var "sword_neg_321" :short "uffi_tests")
(def-foreign-var "uint_1234567" :int "uffi_tests")
(def-foreign-var "sint_neg_123456" :int "uffi_tests")
(def-foreign-var "float_neg_4_5" :float "uffi_tests")
(def-foreign-var "double_3_1" :double "uffi_tests")

(deftest :fvar.1 uchar-13 13)
(deftest :fvar.2 schar-neg-120 -120)
(deftest :fvar.3 uword-257 257)
(deftest :fvar.4 sword-neg-321 -321)
(deftest :fvar.5 uint-1234567 1234567)
(deftest :fvar.6 sint-neg-123456 -123456)
(deftest :fvar.7 float-neg-4-5 -4.5f0)
(deftest :fvar.8 double-3-1 3.1d0)

(uffi:def-foreign-var ("fvar_addend" *fvar-addend*) :int "uffi_tests")

(uffi:def-struct fvar-struct
    (i :int)
  (d :double))

(uffi:def-foreign-var ("fvar_struct" *fvar-struct*) fvar-struct
  "uffi_tests")

(uffi:def-function ("fvar_struct_int" fvar-struct-int)
    ()
  :returning :int
  :module "uffi_tests")

  (uffi:def-function ("fvar_struct_double" fvar-struct-double)
      ()
    :returning :double
    :module "uffi_tests")

(deftest :fvarst.1 *fvar-addend* 3)
(deftest :fvarst.2 (uffi:get-slot-value *fvar-struct* 'fvar-struct 'i) 42)
(deftest :fvarst.3 (= (+ *fvar-addend*
                        (uffi:get-slot-value *fvar-struct* 'fvar-struct 'i))
                     (fvar-struct-int))
  t)
(deftest :fvarst.4 (uffi:get-slot-value *fvar-struct* 'fvar-struct 'd) 3.2d0)
(deftest :fvarst.5 (= (uffi:get-slot-value *fvar-struct* 'fvar-struct 'd)
                     (fvar-struct-double))
  t)

(deftest fvarst.6
    (let ((orig *fvar-addend*))
      (incf *fvar-addend* 3)
      (prog1
          *fvar-addend*
        (setf *fvar-addend* orig)))
  6)

(deftest fvarst.7
    (let ((orig *fvar-addend*))
      (incf *fvar-addend* 3)
      (prog1
          (fvar-struct-int)
        (setf *fvar-addend* orig)))
  48)

(deftest fvarst.8
    (let ((orig (uffi:get-slot-value *fvar-struct* 'fvar-struct 'i)))
      (decf (uffi:get-slot-value *fvar-struct* 'fvar-struct 'i) 10)
      (prog1
          (fvar-struct-int)
        (setf (uffi:get-slot-value *fvar-struct* 'fvar-struct 'i) orig)))
  35)
