;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          uffi.asd
;;;; Purpose:       ASDF system definition file for UFFI package
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Aug 2002
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2009 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(defpackage #:uffi-system (:use #:asdf #:cl))
(in-package #:uffi-system)

#+(or allegro lispworks cmu openmcl digitool cormanlisp sbcl scl)
(defsystem uffi
  :name "uffi"
  :author "Kevin Rosenberg <kevin@rosenberg.net>"
  :version "1.2.x"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "Lessor Lisp General Public License"
  :description "Universal Foreign Function Library for Common Lisp"
  :long-description "UFFI provides a universal foreign function interface (FFI) for Common Lisp. UFFI supports CMUCL, Lispworks, and AllegroCL."

  :components
  ((:module :src
	    :components
	    ((:file "package")
	     (:file "primitives" :depends-on ("package"))
	     #+(or openmcl digitool) (:file "readmacros-mcl" :depends-on ("package"))
	     (:file "objects" :depends-on ("primitives"))
	     (:file "aggregates" :depends-on ("primitives"))
	     (:file "strings" :depends-on ("primitives" "functions" "aggregates" "objects"))
	     (:file "functions" :depends-on ("primitives"))
	     (:file "libraries" :depends-on ("package"))
	     (:file "os" :depends-on ("package"))))
   ))

#+(or allegro lispworks cmu openmcl digitool cormanlisp sbcl scl)
(defmethod perform ((o test-op) (c (eql (find-system 'uffi))))
  (oos 'load-op 'uffi-tests)
  (oos 'test-op 'uffi-tests :force t))


