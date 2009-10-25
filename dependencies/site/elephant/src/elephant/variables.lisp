;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; utils.lisp -- utility functions
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
;;; Portions Copyright (c) 2005-2007 by Robert Read and Ian Eslick
;;; <rread common-lisp net> <ieslick common-lisp net>
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;

(in-package "ELEPHANT")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Versioning Support

(defvar *elephant-code-version* '(0 9 0)
  "The current database version supported by the code base")

(defvar *elephant-unmarked-code-version* '(0 6 0)
  "If a database is opened with existing data but no version then
   we assume it's version 0.6.0")

(defvar *elephant-properties-label* 'elephant::*database-properties*
  "This is the symbol used to store properties associated with the
   database in the controller-root through the new properties interface.
   Users attempting to directly write this variable will run into an
   error")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General support for user configurable parameters

(defvar *user-configurable-parameters*
  '((:berkeley-db-map-degree2 *map-using-degree2*)
    (:berkeley-db-cachesize *berkeley-db-cachesize*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Optimization parameters

(defvar *circularity-initial-hash-size* 50
  "This is the default size of the circularity cache used in the serializer")

(defparameter *map-using-degree2* t
  "This parameter enables an optimization for the Berkeley DB data store
   that allows a map operator to walk over a btree without locking all
   read data, it only locks written objects and the current object")

(defvar *berkeley-db-cachesize* 10485760
  "This parameter controls the size of the berkeley db data store page
   cache.  This parameter can be increased by to 4GB on 32-bit machines
   and much larger on other machines.  Using the db_stat utility to identify
   cache hit frequency on your application is a good way to tune this number.
   The default is 20 megabytes specified in bytes.  If you need to specify
   Gigbyte + cache sizes, talk to the developers!  This is ignored for
   existing databases that were created with different parameters")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Legacy Thread-local specials

#+(or cmu sbcl allegro)
(defvar *resourced-byte-spec* (byte 32 0)
  "Byte specs on CMUCL, SBCL and Allegro are conses.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Thread-local specials

(defvar *store-controller* nil 
  "The store controller which persistent objects talk to.")

(defvar *current-transaction* nil
  "The transaction which is currently in effect.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enables warnings of various kinds

(defvar *warn-on-manual-class-finalization* nil
  "Issue a printed warnings when the class mechanism has
   to finalize a class to access indexing information")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Forward references
;;
;; Elephant needs to export these symbols in order to 
;; properly load in asdf due to some circular dependencies
;; between lisp files 

(eval-when (:compile-toplevel :load-toplevel)
  (mapcar (lambda (symbol)
	    (intern symbol :elephant))
	  '("GET-CACHED-INSTANCE"
	    "SET-DB-SYNCH")))




