;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          atoifl.lisp
;;;; Purpose:       UFFI Example file to atoi/atof/atol
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Mar 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package #:uffi-tests)

(uffi:def-function ("atoi" c-atoi)
    ((str :cstring))
  :returning :int)

(uffi:def-function ("atol" c-atol)
    ((str :cstring))
  :returning :long)

(uffi:def-function ("atof" c-atof)
    ((str :cstring))
  :returning :double)

(defun atoi (str)
  "Returns a int from a string."
  (uffi:with-cstring (str-cstring str)
    (c-atoi str-cstring)))

(defun atof (str)
  "Returns a double float from a string."
  (uffi:with-cstring (str-cstring str)
    (c-atof str-cstring)))

(deftest :atoi.1 (atoi "123") 123)
(deftest :atoi.2 (atoi "") 0)
(deftest :atof.3 (atof "2.23") 2.23d0)
