;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          getenv.cl
;;;; Purpose:       UFFI Example file to get environment variable
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package :cl-user)


(uffi:def-function ("getenv" c-getenv)
    ((name :cstring))
  :returning :cstring)

(defun my-getenv (key)
  "Returns an environment variable, or NIL if it does not exist"
  (check-type key string)
  (uffi:with-cstring (key-native key)
    (uffi:convert-from-cstring (c-getenv key-native))))

#+examples-uffi
(progn
  (flet ((print-results (str)
           (format t "~&(getenv ~S) => ~S" str (my-getenv str))))
    (print-results "USER")
    (print-results "_FOO_")))


#+test-uffi
(progn
  (util.test:test (my-getenv "_FOO_") nil :fail-info "Error retrieving non-existent getenv")
  (util.test:test (and (stringp (my-getenv "USER"))
                       (< 0 (length (my-getenv "USER"))))
                  t :fail-info "Error retrieving getenv")
)

