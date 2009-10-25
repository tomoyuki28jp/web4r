;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; dcm-package.asd -- package definition for DCM
;;; 
;;; Copyright (c) 2005 Robert L. Read <read@robertlread.net>
;;; All rights reserverd.

(defpackage dcm
  (:documentation 
   "DCM is a very simple in-memory object prevalence system.")
  (:nicknames dcm :dcm)
;;  (:use common-lisp elephant ele-clsql)
  (:use common-lisp elephant)
  (:export 
;; These parameters are used to tell DCM how to connect 
;; to repositories
   #:*SLEEPYCAT-HOME*
   #:*POSTGRES-SPEC*
   #:*DCM-DEFAULT*
   #:*ELEPHANT-CAT*
   #:*DEF-STORE-NAME*
   
   #:key
   #:mtype
   #:key-equal
   #:dcm-equal
   #:max-key-value
   #:max-key
   #:managed-object
   #:mid
   #:k
   #:owner
   #:ownr
   #:tstamp

   #:dcm-tstmp

   #:mo-equal
   #:get-values
   #:randomize-slot-value
   #:get-user-defined-slots
   #:randomize-managed-object
   #:ExObject
   #:managed-handle
   #:test-randomize-managed-object
   #:max-key-value
   
   #:*DIR-CAT*
   #:director
   #:load-all
   #:delete-all-objects-from-director

   #:*HASH-CAT*
   #:hash-director
   #:get-all-objects
   #:get-all-objects-type
   #:get-all-objects-owned-by
   #:get-unused-key-value
   #:hash-values-reduce
   #:hash-keys-reduce
   #:register-obj
   #:lookup-obj
   #:delete-obj
   #:hash-dir-test
   #:*ELEPHANT-CAT*
   #:*basic-store-controller*
   #:init-elephant-controllers
   #:release-elephant-controllers

   #:elephant-director
   #:initialize-btree
   #:initialize
   #:register-many-random
   #:elephant-dir-test
   #:hash-ele-director
   #:hash-ele-dir-test
   #:*DIR-STRATEGIES*
   #:directory-factory
   #:init-director
   #:dir-test
   #:test-get-unused-key-value
   #:unused-key
   #:tm-register-then-lookup
   #:tm-get-all-objects
   #:tm-test-elephant
   #:run-all-dcm-tests
   #:test-ex-director
   #:get-director-by-class
   #:get-all-cur-objects
   #:get-all-objects-gen
   #:retire
   #:promote
   #:find-generation
   #:GenDir
)

)


