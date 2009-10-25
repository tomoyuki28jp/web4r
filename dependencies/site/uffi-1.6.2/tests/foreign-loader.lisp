;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          foreign-loader.lisp
;;;; Purpose:       Loads foreign libraries
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

;;; For CMUCL, it's necessary to load foreign files separate from their
;;; usage

(in-package uffi-tests)

#+clisp (uffi:load-foreign-library "/usr/lib/libz.so" :module "z")
#-clisp
(unless (uffi:load-foreign-library
         (uffi:find-foreign-library
          #-(or macosx darwin)
          "libz"
          #+(or macosx darwin)
          "z"
          (list (pathname-directory *load-pathname*)
                "/usr/local/lib/" #+(or 64bit x86-64) "/usr/lib64/"
                "/usr/lib/" "/zlib/"))
         :module "zlib"
         :supporting-libraries '("c"))
  (warn "Unable to load zlib"))

#+clisp (uffi:load-foreign-library "/home/kevin/debian/src/uffi/tests/uffi-c-test.so" :module "uffi_tests")
#-clisp
(unless (uffi:load-foreign-library
         (uffi:find-foreign-library
          '(#+(or 64bit x86-64) "uffi-c-test64" "uffi-c-test")
          (list (pathname-directory *load-truename*)
                "/usr/lib/uffi/"
                "/home/kevin/debian/src/uffi/tests/"))
         :supporting-libraries '("c")
         :module "uffi_tests")
  (warn "Unable to load uffi-c-test library"))

