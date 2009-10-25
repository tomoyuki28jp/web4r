;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; package.lisp -- package definition
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
;;;

(in-package :cl-user)

(defpackage elephant-btrees
  (:use :closer-common-lisp)
  (:export
   #:cursor #:secondary-cursor #:make-cursor 
   #:with-btree-cursor #:cursor-close #:cursor-init
   #:cursor-duplicate #:cursor-current #:cursor-first
   #:cursor-last #:cursor-next #:cursor-next-dup
   #:cursor-next-nodup #:cursor-prev #:cursor-prev-nodup
   #:cursor-set #:cursor-set-range #:cursor-get-both
   #:cursor-get-both-range #:cursor-delete #:cursor-put
   #:cursor-pcurrent #:cursor-pfirst #:cursor-plast
   #:cursor-pnext #:cursor-pnext-dup #:cursor-pnext-nodup
   #:cursor-pprev #:cursor-pprev-nodup #:cursor-pset
   #:cursor-pset-range #:cursor-pget-both
   #:cursor-pget-both-range))

(defpackage elephant
  (:use :closer-common-lisp :elephant-memutil :elephant-btrees)
  (:nicknames ele :ele)
  (:documentation 
   "Elephant: an object-oriented database for Common Lisp with
    multiple backends for Berkeley DB, SQL and others.")
  (:export #:*store-controller* #:*current-transaction* #:*auto-commit*
	   #:*elephant-lib-path*

	   #:store-controller
	   #:open-store #:close-store #:with-open-store
	   #:add-to-root #:get-from-root #:remove-from-root #:root-existsp
	   #:flush-instance-cache #:optimize-storage

	   #:with-transaction
 	   #:start-ele-transaction #:commit-transaction #:abort-transaction 

 	   #:persistent #:persistent-object #:persistent-metaclass
	   #:persistent-collection #:defpclass

 	   #:btree #:make-btree #:get-value #:remove-kv #:existp #:map-btree 
	   #:indexed-btree #:make-indexed-btree
	   #:add-index #:get-index #:remove-index #:map-indices
	   #:btree-index #:get-primary-key
	   #:primary #:key-form #:key-fn

 	   #:btree-differ
 	   #:migrate #:*inhibit-slot-copy*

	   #:run-elephant-thread

	   ;; Class indexing management API
	   #:*default-indexed-class-synch-policy*
	   #:find-class-index #:find-inverted-index
	   #:enable-class-indexing #:disable-class-indexing
	   #:add-class-slot-index #:remove-class-slot-index
	   #:add-class-derived-index #:remove-class-derived-index
	   #:describe-db-class-index
	   #:report-indexed-classes
	   #:class-indexedp-by-name

	   ;; Low level cursor API
	   #:make-inverted-cursor #:make-class-cursor
	   #:with-inverted-cursor #:with-class-cursor

	   ;; Instance query API
	   #:get-instances-by-class 
	   #:get-instance-by-value
	   #:get-instances-by-value
	   #:get-instances-by-range
	   #:drop-instances
	   )
  )


(in-package "ELE")

#+cmu
(eval-when (:compile-toplevel)
  (proclaim '(optimize (ext:inhibit-warnings 3))))
