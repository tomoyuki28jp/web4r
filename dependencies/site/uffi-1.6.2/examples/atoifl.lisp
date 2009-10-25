;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          atoifl.cl
;;;; Purpose:       UFFI Example file to atoi/atof/atol
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Mar 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package :cl-user)

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

#+examples-uffi
(progn
  (flet ((print-results (str)
           (format t "~&(atoi ~S) => ~S" str (atoi str))))
    (print-results "55")))


#+test-uffi
(progn
  (util.test:test (atoi "123") 123 :test #'eql
                  :fail-info "Error with atoi")
  (util.test:test (atoi "") 0 :test #'eql
                  :fail-info "Error with atoi")
  (util.test:test (atof "2.23") 2.23d0 :test #'eql
                  :fail-info "Error with atof")
  )

