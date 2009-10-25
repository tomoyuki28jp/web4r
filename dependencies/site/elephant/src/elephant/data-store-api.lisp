;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; backend.lisp -- Namespace support for data store packages
;;; 
;;; By Ian Eslick <ieslick common-lisp net>
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

(defmacro defpackage-import-exported (name source-package &rest args)
  "Define an export list, a source package and this macro will automatically
   import from that package the exported symbol names."
  (let* ((exports (find :export args :key #'car))
	 (imports `(:import-from ,source-package ,@(cdr exports))))
    `(defpackage ,name
       ,@(append args (list imports)))))

(defpackage-import-exported :elephant-data-store :elephant
  (:documentation "Data stores should use this to get access to internal symbols
   of elephant that importers of elephant shouldn't see.  Backends should also
   import elephant to get use-api generic function symbols, classes and globals")
  (:use #:elephant)
  (:export 
   ;; Variables
   #:*dbconnection-spec* 
   #:connection-is-indeed-open

   ;; Persistent objects
   #:oid #:get-con 
   #:next-oid 
   #:persistent-slot-writer
   #:persistent-slot-reader
   #:persistent-slot-boundp
   #:persistent-slot-makunbound

   ;; Controllers
   #:*elephant-code-version*
   #:open-controller
   #:close-controller
   #:database-version
   #:controller-spec
   #:controller-serializer-version
   #:controller-serialize
   #:controller-deserialize
   #:root #:spec #:class-root

   ;; Collections
   #:build-btree
   #:build-indexed-btree

   ;; Serializer tools/api's
   #:serialize #:deserialize
   #:deserialize-from-base64-string
   #:serialize-to-base64-string
   #:initialize-serializer
   #:serialize-database-version-key
   #:serialize-database-version-value
   #:deserialize-database-version-value

   ;; Cursor accessors
   #:cursor-btree
   #:cursor-oid
   #:cursor-initialized-p

   ;; Transactions
   #:*current-transaction*
   #:make-transaction-record
   #:transaction-store
   #:transaction-object
   #:execute-transaction
   #:controller-start-transaction
   #:controller-abort-transaction
   #:controller-commit-transaction

   ;; Registration
   #:register-data-store-con-init
   #:lookup-data-store-con-init
   #:get-user-configuration-parameter

   ;; Misc
   #:slot-definition-name
   #:slots-and-values
   #:struct-slots-and-values
   ))
		
