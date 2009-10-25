;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; berkeley-db.lisp -- FFI interface to Berkeley DB
;;; 
;;; Initial version 9/10/2004 by Ben Lee
;;; <blee@common-lisp.net>
;;; 
;;; part of
;;;
;;; Elephant: an object-oriented database for Common Lisp
;;;
;;; Copyright (c) 2004 by Andrew Blumberg and Ben Lee
;;; <ablumberg@common-lisp.net> <blee@common-lisp.net>
;;;
;;; Portions Copyright (c) 2005-2007 by Robert Read and Ian Eslick
;;; <rread common-lisp net> <ieslick common-lisp net>
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;

(in-package :cl-user)

(defpackage sleepycat 
  (:documentation "For legacy upgrades")
  (:use common-lisp))

(defpackage db-bdb
  (:documentation "A low-level UFFI-based interface to Berkeley
   DB to implement the elephant front-end framework.  Uses the
   libelebdb.c wrapper.  Partly intended to be usable outside
   Elephant, but with some magic for Elephant.  In general there
   is a 1-1 mapping from functions here and functions in
   Berkeley DB, so refer to their documentation for details.")
  (:use common-lisp uffi elephant-memutil elephant elephant-data-store elephant-utils)
  #+cmu
  (:use alien)
  #+sbcl
  (:use sb-alien)
  #+cmu
  (:import-from :sys
		#:sap+)
  #+sbcl
  (:import-from :sb-sys
		#:sap+)  
  #+openmcl
  (:import-from :ccl
		#:byte-length)
  (:export
   #:optimize-layout
   #:checkpoint))
