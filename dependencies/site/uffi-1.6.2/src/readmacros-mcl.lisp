;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: UFFI -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          readmacros-mcl.lisp
;;;; Purpose:       This file holds functions using read macros for MCL
;;;; Programmer:    Kevin M. Rosenberg/John Desoi
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package #:uffi)


;; trap macros don't work right directly in the macros
#+digitool
(defun new-ptr (size)
  (#_NewPtr size))

#+digitool
(defun dispose-ptr (ptr)
  (#_DisposePtr ptr))

#+openmcl
(defmacro new-ptr (size)
  `(ccl::malloc ,size))

#+openmcl
(defmacro dispose-ptr (ptr)
  `(ccl::free ,ptr))

