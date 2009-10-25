;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          structs.lisp
;;;; Purpose:       Test file for UFFI structures
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package #:uffi-tests)

;; Compilation failure as reported by Edi Weitz


(uffi:def-struct foo
    (bar :pointer-self))

(uffi:def-foreign-type foo-ptr (* foo))

;; tests that compilation worked
(deftest :structs.1
  (with-foreign-object (p 'foo)
    t)
  t)

(deftest :structs.2
    (progn
      (uffi:def-foreign-type foo-struct (:struct foo))
      t)
  t)
