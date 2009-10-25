;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; elephant-tests.asd -- ASDF system definition for tests
;;; 
;;; Initial version 8/30/2004 by Ben Lee
;;; <blee@common-lisp.net>
;;; 
;;; part of
;;;
;;; Elephant: an object-oriented database for Common Lisp
;;;
;;; Copyright (c) 2004 by Andrew Blumberg and Ben Lee
;;; <ablumberg@common-lisp.net> <blee@common-lisp.net>
;;;
;;; This program is released under the following license
;;; ("GPL").  For differenct licensing terms, contact the
;;; copyright holders.
;;;
;;; This program is free software; you can redistribute it
;;; and/or modify it under the terms of the GNU General
;;; Public License as published by the Free Software
;;; Foundation; either version 2 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the
;;; implied warranty of MERCHANTABILITY or FITNESS FOR A
;;; PARTICULAR PURPOSE. See the GNU General Public License
;;; for more details.
;;;
;;; The GNU General Public License can be found in the file
;;; LICENSE which should have been distributed with this
;;; code.  It can also be found at
;;;
;;; http://www.opensource.org/licenses/gpl-license.php
;;;
;;; You should have received a copy of the GNU General
;;; Public License along with this program; if not, write
;;; to the Free Software Foundation, Inc., 59 Temple Place,
;;; Suite 330, Boston, MA 02111-1307 USA
;;;

(defsystem elephant-tests
  :name "elephant"
  :author "Ben Lee <blee@common-lisp.net>"
  :version "0.1"
  :maintainer "Ben Lee <blee@common-lisp.net>"
  :licence "Lessor Lisp General Public License"
  :description "Object database for Common Lisp"
  :long-description "An object-oriented database based on Berkeley DB, for CMUCL/SBCL, OpenMCL, Lispworks, and Allegro."
  
  :depends-on (:elephant :rt)
  :components
  ((:module :tests
	    :components
	    ((:file "elephant-tests")
	     (:file "testserializer")
	     (:file "testconditions")
	     (:file "mop-tests")
	     (:file "testcollections")
	     (:file "testindexing")
	     (:file "testmigration")
	     )
	    :serial t)))

(defsystem elephant-tests-bdb
  :name "elephant"
  :author "Ben Lee <blee@common-lisp.net>"
  :version "0.1"
  :maintainer "Ben Lee <blee@common-lisp.net>"
  :licence "Lessor Lisp General Public License"
  :description "Tests that only run under BDB"
  
  :depends-on (:elephant-tests)
  :components
  ((:module :tests
	    :components
	    ((:file "testbdb")))))

 
