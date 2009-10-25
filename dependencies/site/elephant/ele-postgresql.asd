;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; ele-postgresql.asd -- ASDF system definition for 
;;; a PostgreSQL based back-end for Elephant
;;; 
;;; Initial version 10/12/2005 by Robert L. Read
;;; <read@robertlread.net>
;;; 
;;; part of
;;;
;;; Elephant: an object-oriented database for Common Lisp
;;;
;;; Copyright (c) 2004 by Andrew Blumberg and Ben Lee
;;; <ablumberg@common-lisp.net> <blee@common-lisp.net>
;;;
;;; This program is released under the following license
;;; ("LLGPL"). 
;;;

(defsystem ele-postgresql
  :name "ele-postgresql"
  :author "Robert L. Read <read@robertlread.net>"
  :version "0.6.0"
  :maintainer "Robert L. Read <read@robertlread.net>"
  :licence "GPL"
  :description "PostgreSQL based Object respository for Common Lisp"
  
  :components
  ((:module :src
	    :components
	    ()))
  :depends-on (:ele-clsql :clsql-postgresql-socket))
