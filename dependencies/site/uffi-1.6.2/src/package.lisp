;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: UFFI -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Defines UFFI package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage #:uffi
  (:use #:cl)
  (:export

   ;; immediate types
   #:def-constant
   #:def-foreign-type
   #:def-type
   #:null-char-p

   ;; aggregate types
   #:def-enum
   #:def-struct
   #:get-slot-value
   #:get-slot-pointer
   #:def-array-pointer
   #:deref-array
   #:def-union

   ;; objects
   #:allocate-foreign-object
   #:free-foreign-object
   #:with-foreign-object
   #:with-foreign-objects
   #:size-of-foreign-type
   #:pointer-address
   #:deref-pointer
   #:ensure-char-character
   #:ensure-char-integer
   #:ensure-char-storable
   #:null-pointer-p
   #:make-null-pointer
   #:make-pointer
   #:pointer-address
   #:+null-cstring-pointer+
   #:char-array-to-pointer
   #:with-cast-pointer
   #:def-foreign-var
   #:convert-from-foreign-usb8
   #:def-pointer-var

   ;; string functions
   #:convert-from-cstring
   #:convert-to-cstring
   #:free-cstring
   #:with-cstring
   #:with-cstrings
   #:convert-from-foreign-string
   #:convert-to-foreign-string
   #:allocate-foreign-string
   #:with-foreign-string
   #:with-foreign-strings
   #:foreign-string-length

   ;; function call
   #:def-function

   ;; Libraries
   #:find-foreign-library
   #:load-foreign-library
   #:default-foreign-library-type
   #:foreign-library-types

   ;; OS
   #:run-shell-command
   #:getenv
   ))


