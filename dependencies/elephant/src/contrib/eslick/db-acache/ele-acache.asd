;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; ele-acache.asd -- ASDF file for allegrocache backend
;;; 
;;; Initial version 2/18/2006 by Ian Eslick
;;; <ieslick@common-lisp.net>
;;; 
;;; part of
;;;
;;; Elephant: an object-oriented database for Common Lisp
;;;
;;; Copyright (c) 2004 by Andrew Blumberg and Ben Lee
;;; <ablumberg@common-lisp.net> <blee@common-lisp.net>
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(defsystem ele-acache
  :name "elephant"
  :author "Ben Lee <blee@common-lisp.net>"
  :version "0.6.0"
  :maintainer "Ben Lee <blee@common-lisp.net>"
  :licence "LLGPL"
  :description "Allegro cache backend for elephant"
  :components
  ((:module :src
	    :components
	    ((:module :db-acache
		      :components
		      ((:file "package")
		       (:file "acache-controller")
		       (:file "acache-transactions")
		       (:file "acache-collections"))
		      :serial t))))
  :depends-on (:elephant))


