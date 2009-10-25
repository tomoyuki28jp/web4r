;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          getenv.lisp
;;;; Purpose:       UFFI Example file to get environment variable
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package #:uffi-tests)


(uffi:def-function ("getenv" c-getenv)
    ((name :cstring))
  :returning :cstring)

(uffi:def-function ("setenv" c-setenv)
    ((name :cstring)
     (value :cstring)
     (overwrite :int))
  :returning :int)

(uffi:def-function ("unsetenv" c-unsetenv)
    ((name :cstring))
  :returning :void)

(defun my-getenv (key)
  "Returns an environment variable, or NIL if it does not exist"
  (check-type key string)
  (uffi:with-cstring (key-native key)
    (uffi:convert-from-cstring (c-getenv key-native))))

(defun my-setenv (key name &optional (overwrite t))
  "Returns an environment variable, or NIL if it does not exist"
  (check-type key string)
  (check-type name string)
  (setq overwrite (if overwrite 1 0))
  (uffi:with-cstrings ((key-native key)
                       (name-native name))
    (c-setenv key-native name-native (if overwrite 1 0))))

(defun my-unsetenv (key)
  "Returns an environment variable, or NIL if it does not exist"
  (check-type key string)
  (uffi:with-cstrings ((key-native key))
    (c-unsetenv key-native)))

(deftest :getenv.1 (progn
                    (my-unsetenv "__UFFI_FOO1__")
                    (my-getenv "__UFFI_FOO1__"))
  nil)
(deftest :getenv.2 (progn
                    (my-setenv "__UFFI_FOO1__" "UFFI-TEST")
                    (my-getenv "__UFFI_FOO1__"))
  "UFFI-TEST")



