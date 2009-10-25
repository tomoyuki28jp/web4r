;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; elephant.asd -- ASDF system definition for elephant
;;; 
;;; Initial version 8/26/2004 by Ben Lee
;;; <blee@common-lisp.net>
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

(in-package :cl-user)

;; Forward definition
(defpackage elephant-system
  (:use :cl :asdf)
  (:export :elephant-c-source :compiler-options :foreign-libraries-to-load-first :get-config-option))

(defpackage ele-clsql-system
  (:use :cl :asdf :elephant-system))

(in-package :ele-clsql-system)

;;
;; Make sure asdf is loaded prior to evaluating the rest of the file (tacky)
;;
(eval-when (:compile-toplevel :load-toplevel  :execute)
  (unless (find-package 'clsql)
    (asdf:operate 'asdf:load-op 'clsql)))

(defmethod asdf:perform :after ((o asdf:load-op)
				(c (eql (asdf:find-system 'clsql))))
  (let ((paths (get-config-option :clsql-lib-paths (find-system :elephant)))
	(plp (find-symbol (symbol-name '#:push-library-path)
			  (find-package 'clsql))))
    (loop for path in paths do
	 (format t "Pushing ~A onto clsql::*FOREIGN-LIBRARY-SEARCH-PATHS*~%" path)
	 (funcall plp (pathname path)))))

(defsystem ele-clsql
  :name "elephant"
  :author "Ben Lee <blee@common-lisp.net>"
  :version "0.6.0"
  :maintainer "Robert L. Read <rread@common-lisp.net>"
  :licence "LLGPL"
  :description "SQL-based Object respository for Common Lisp"
  :long-description "An experimental CL-SQL based implementation of Elephant"
  :components
  ((:module :src
	    :components
	    ((:module :db-clsql
		      :components
		      ((:file "package")
		       (:file "sql-controller")
		       (:file "sql-transaction")
		       (:file "sql-collections"))
		      :serial t
		      ))))
  :depends-on (:elephant :clsql :cl-base64))


