;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; migrate.lisp - 
;;; 
;;; Initial version by Robert Read <rread common-lisp net>
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

(defpackage elephant
  (:use :common-lisp :elephant-memutil :elephant-utils)
  (:nicknames :ele)
  (:documentation 
   "Elephant: an object-oriented database for Common Lisp with
    multiple backends for Berkeley DB, SQL and others.")
  #+cmu  
  (:import-from :pcl
		compute-class-precedence-list
		validate-superclass
		standard-slot-definition
		standard-direct-slot-definition
		standard-effective-slot-definition
		slot-definition-name
		slot-definition-initform
		slot-definition-initfunction
		direct-slot-definition-class
		effective-slot-definition-class
		compute-effective-slot-definition
		compute-effective-slot-definition-initargs
		class-slots
		slot-value-using-class
		slot-boundp-using-class
		slot-makunbound-using-class
		slot-definition-allocation
		slot-definition-initargs
		class-finalized-p
		finalize-inheritance
		ensure-class-using-class
		compute-slots
		initialize-internal-slot-functions
		slot-definition-reader-function
		slot-definition-writer-function
		slot-definition-boundp-function
		slot-definition-allocation-class
		class-slot-cells
		plist-value
		+slot-unbound+) 
  #+cmu  
  (:import-from :ext
		make-weak-pointer weak-pointer-value finalize)

  #+cmu  
  (:import-from :bignum
		%bignum-ref)

  #+sbcl 
  (:import-from :sb-mop 
		compute-class-precedence-list
		validate-superclass
		standard-slot-definition
		standard-direct-slot-definition
		standard-effective-slot-definition
		direct-slot-definition-class
		effective-slot-definition-class
		slot-definition-name
		slot-definition-initform
		slot-definition-initfunction
		compute-effective-slot-definition
		class-slots
		slot-value-using-class
		slot-boundp-using-class
		slot-makunbound-using-class
		slot-definition-allocation
		slot-definition-initargs
		class-finalized-p
		finalize-inheritance
		ensure-class-using-class
		compute-slots)                                
  #+sbcl
  (:import-from :sb-pcl
		initialize-internal-slot-functions
		compute-effective-slot-definition-initargs
		slot-definition-reader-function
		slot-definition-writer-function
		slot-definition-boundp-function
		slot-definition-allocation-class
		class-slot-cells
		plist-value
		+slot-unbound+)
  #+sbcl
  (:import-from :sb-ext
		make-weak-pointer weak-pointer-value finalize)

  #+sbcl
  (:import-from :sb-bignum
		%bignum-ref)

  #+allegro
  (:import-from :clos
		compute-class-precedence-list
		validate-superclass
		standard-slot-definition
		standard-direct-slot-definition
		standard-effective-slot-definition
		direct-slot-definition-class
		effective-slot-definition-class
		slot-definition-name
		slot-definition-initform
		slot-definition-initfunction
		compute-effective-slot-definition
		class-slots
		slot-value-using-class
		slot-boundp-using-class
		slot-makunbound-using-class
		slot-definition-allocation
		slot-definition-initargs
		class-finalized-p
		finalize-inheritance
		ensure-class-using-class
		compute-slots
		slot-definition-readers
                slot-definition-writers
                class-direct-slots
		)
  #+allegro
  (:import-from :excl
		compute-effective-slot-definition-initargs)
  #+openmcl
  (:import-from :ccl
		class-finalized-p
		finalize-inheritance
		ensure-class-using-class
		compute-class-precedence-list
		validate-superclass
		standard-slot-definition
		standard-direct-slot-definition
		standard-effective-slot-definition
		direct-slot-definition-class
		effective-slot-definition-class
		slot-definition-name
		slot-definition-initform
		slot-definition-initfunction
		compute-effective-slot-definition
		class-slots
		slot-value-using-class
		slot-boundp-using-class
		slot-makunbound-using-class
		slot-definition-allocation
		slot-definition-initargs
		compute-slots
		;; This stuff we need until we resolve the :transient
		;; slot specifier stuff
		make-effective-slot-definition
		slots-class
		%slot-definition-initfunction
		%slot-definition-documentation
		%slot-definition-initargs
		%slot-definition-initform
		%slot-definition-allocation
		%slot-definition-class
		%slot-definition-type)
  #+lispworks  
  (:import-from :clos
		class-finalized-p
		finalize-inheritance
		compute-class-precedence-list
		validate-superclass
		ensure-class-using-class
		standard-slot-definition
		standard-direct-slot-definition
		standard-effective-slot-definition
		slot-definition-name
		slot-definition-initform
		slot-definition-initfunction
		direct-slot-definition-class
		effective-slot-definition-class
		compute-effective-slot-definition
		compute-effective-slot-definition-initargs
		class-slots
		slot-value-using-class
		slot-boundp-using-class
		slot-makunbound-using-class
		slot-definition-allocation
		slot-definition-initargs
		compute-slots)
  (:export 
   #:*store-controller* 
   #:store-controller #:controller-root #:controller-class-root 
   #:open-store #:close-store #:with-open-store
   #:add-to-root #:get-from-root #:remove-from-root #:root-existsp #:map-root
   #:flush-instance-cache
   #:optimize-layout 

   #:persistent #:persistent-object #:persistent-metaclass #:defpclass
   #:persistent-collection #:drop-pobject

   #:pset #:make-pset #:insert-item #:remove-item #:map-pset #:find-item #:pset-list #:drop-pset

   #:btree #:make-btree
   #:get-value #:remove-kv #:existsp
   #:indexed-btree #:make-indexed-btree 
   #:btree-index
   #:add-index #:get-index #:remove-index #:map-indices
   #:get-primary-key #:primary #:key-form #:key-fn
   #:with-btree-cursor #:map-btree #:map-index #:drop-btree
   #:empty-btree-p #:dump-btree #:btree-keys #:btree-differ-p

   #:cursor #:secondary-cursor #:make-cursor #:make-simple-cursor
   #:cursor-close #:cursor-duplicate #:cursor-current #:cursor-first
   #:cursor-last #:cursor-next #:cursor-next-dup
   #:cursor-next-nodup #:cursor-prev #:cursor-prev-nodup #:cursor-prev-dup
   #:cursor-set #:cursor-set-range #:cursor-get-both
   #:cursor-get-both-range #:cursor-delete #:cursor-put
   #:cursor-pcurrent #:cursor-pfirst #:cursor-plast
   #:cursor-pnext #:cursor-pnext-dup #:cursor-pnext-nodup
   #:cursor-pprev #:cursor-pprev-dup #:cursor-pprev-nodup #:cursor-pset
   #:cursor-pset-range #:cursor-pget-both #:cursor-pget-both-range
   #:cursor-initialized-p

   #:find-class-index #:find-inverted-index
   #:enable-class-indexing #:disable-class-indexing
   #:add-class-slot-index #:remove-class-slot-index
   #:add-class-derived-index #:remove-class-derived-index
   #:describe-db-class-index
   #:report-indexed-classes
   #:class-indexedp-by-name

   #:map-class #:map-inverted-index
   #:get-instances-by-class 
   #:get-instance-by-value
   #:get-instances-by-value
   #:get-instances-by-range
   #:drop-instances
   #:make-inverted-cursor #:make-class-cursor
   #:with-inverted-cursor #:with-class-cursor
   #:*default-indexed-class-synch-policy*

   #:with-transaction #:ensure-transaction
   #:controller-start-transaction
   #:controller-abort-transaction
   #:controller-commit-transaction

   #:upgrade #:migrate
   #:set-oid-spec #:*inhibit-slot-copy* 
   #:add-symbol-conversion #:add-package-conversion
   #:*always-convert*
   #:translate-and-intern-symbol
   #:lookup-persistent-symbol
   #:lookup-persistent-symbol-id

   #:struct-constructor

   ;; Various error conditions
   #:cross-reference-error
   #:controller-lost-error
   #:persistent-class-not-indexed

   #:map-class-query
   #:get-query-instances
   )
  )

(in-package "ELE")

#+cmu
(eval-when (:compile-toplevel)
  (proclaim '(optimize (ext:inhibit-warnings 3))))

(defpackage :elephant-user
  (:use :common-lisp :elephant)
  (:nicknames :ele-user)
  (:documentation
   "A user package for experimenting with Elephant"))
  