;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; ele-clsql.asd -- ASDF system definition for 
;;; a SQLite3 based back-end for Elephant
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

(defsystem ele-sqlite3
  :name "ele-sqlite3"
  :author "Robert L. Read <read@robertlread.net>"
  :version "0.6.0"
  :maintainer "Robert L. Read <read@robertlread.net>"
  :licence "GPL"
  :description "SQLite3 based Object respository for Common Lisp"
  :long-description "Including this loads the SQLite3 code; you may have to edit the pathname!"
  
  :components
  ((:module :src
	    :components
	    (
	)
	    :serial t))
  :depends-on (:ele-clsql :clsql-sqlite3))
