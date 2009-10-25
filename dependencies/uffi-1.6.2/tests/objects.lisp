;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          pointers.lisp
;;;; Purpose:       Test file for UFFI pointers
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2003-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package #:uffi-tests)

(deftest :chptr.1
    (let ((native-string "test string"))
      (uffi:with-foreign-string (fs native-string)
        (ensure-char-character
         (deref-pointer fs :char))))
  #\t)

(deftest :chptr.2
    (let ((native-string "test string"))
      (uffi:with-foreign-string (fs native-string)
        (ensure-char-character
         (deref-pointer fs :unsigned-char))))
  #\t)

(deftest :chptr.3
    (let ((native-string "test string"))
      (uffi:with-foreign-string (fs native-string)
        (ensure-char-integer
         (deref-pointer fs :unsigned-char))))
  116)

(deftest :chptr.4
    (let ((native-string "test string"))
      (uffi:with-foreign-string (fs native-string)
        (integerp
         (ensure-char-integer
          (deref-pointer fs :unsigned-char)))))
  t)

(deftest :chptr.5
    (let ((fs (uffi:allocate-foreign-object :unsigned-char 128)))
      (setf (uffi:deref-array fs '(:array :unsigned-char) 0)
            (uffi:ensure-char-storable #\a))
      (setf (uffi:deref-array fs '(:array :unsigned-char) 1)
            (uffi:ensure-char-storable (code-char 0)))
      (uffi:convert-from-foreign-string fs))
  "a")

;; This produces an array which needs fli:foreign-aref to access
;; rather than fli:dereference

#-lispworks
(deftest :chptr.6
    (uffi:with-foreign-object (fs '(:array :unsigned-char 128))
      (setf (uffi:deref-array fs '(:array :unsigned-char) 0)
            (uffi:ensure-char-storable #\a))
      (setf (uffi:deref-array fs '(:array :unsigned-char) 1)
            (uffi:ensure-char-storable (code-char 0)))
      (uffi:convert-from-foreign-string fs))
  "a")



