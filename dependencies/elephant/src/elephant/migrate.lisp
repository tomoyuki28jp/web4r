;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; migrate.lisp -- Migrate between repositories
;;; 
;;; Original version 2005 by Robert Read
;;; New version 2/19/2006 by Ian Eslick
;;; <ieslick@common-lisp.net>
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

;;
;; The generic function Migrate provides an interface to moving objects between
;; repositories and is used by the upgrade interface.
;;

;; LIMITATIONS:
;; - Migrate currently will not handle circular list objects
;;
;; - Indexed classes only have their class index copied if you use the
;;   top level migration.  Objects will be copied without slot data if you
;;   try to migrate an object outside of a store-to-store migration due to
;;   the class object belonging to one store or another
;;
;; - Migrate assumes that after migration, indexed classes belong to the
;;   target store. 
;;
;; - In general, migration is a one-time activity and afterwards (or after
;;   a validation test) the source store should be closed.  Any failures
;;   in migration should then be easy to catch.
;;
;; - Migrate keeps a memory-resident hash of all persistent objects;
;;   this is not as bad as it sounds as an object is only an oid reference 
;;   and a pointer to the store controller it belongs to.  However, you 
;;   may eventually run out of heap space for very large DB's.  You can use
;;   a third store controller to get around this by setting set-oid-spec to
;;   a valid, uncreated store specification.
;;
;; - Each top-level call to migration will be good about keeping track
;;   of already copied persistent objects.  However the hash is not
;;   saved between calls and there's no other way to do comparisons
;;   between objects across stores (different oid namespaces) so user
;;   beware of the pitfalls of partial migrations...
;;
;; - Migration does not maintain OID equivalence so any datastructures which
;;   index into those will have to have a way to reconstruct themselves (better
;;   to keep the object references themselves rather than oids in general)
;;   but they can overload the migrate method to accomplish this cleanly
;; 
;; CUSTOMIZE MIGRATION:
;; - To customize migration overload a version of migrate to specialize on
;;   your specific persistent class type.  
;;
;;   (defmethod migrate ((dst store-controller) (src my-class)))
;;
;;   In the body of this method you can call (call-next-method)
;;   to get a destination repository object with all the slots copied over
;;   to the target repository which you can then overwrite.  To avoid the
;;   default persistent slot copying, bind the dynamic variable 
;;   *inhibit-slot-copy* in your user method using 
;;   (with-inhibited-slot-copy () ...), a convenience macro.
;;


(defgeneric migrate (dst src)
  (:documentation 
   "Migrate an object from the src object, collection or controller
    to the dst controller.  Returns a copy of the object in the new
    store so you can drop it into a parent object or the root of
    the dst controller"))

;;
;; MIGRATE ALL OBJECTS IN SRC STORE-CONTROLLER TO THE 
;; (TYPICALLY FRESH) DST STORE-CONTROLLER
;;

(defmethod migrate ((dst store-controller) (src store-controller))
  "Perform a wholesale repository migration from the root. 
   Also acts as a poor man's GC if you copy to another store 
   of the same type!"
  ;; Indexed class slots can only be copied once the class metaobject is 
  ;; pointing at the new indices...but we know that class indices only contain
  ;; indexed persistent objects which (see below) are not copied by default
  ;; so we do the slot updates here
  (map-btree (lambda (classname classidx)
	       ;; Class indexes should never be copied already; this checks
               ;; for users breaking the class-index abstraction
	       (assert (not (object-was-copied-p classidx)))
	       (format t "Migrating class indexes for: ~A~%" classname)
	       (let ((newcidx
		      (with-transaction (:store-controller dst)
			(build-indexed-btree dst))))
		 ;; Add inverse indices to new main class index
		 (map-indices (lambda (name srciidx)
				(let ((key-form (key-form srciidx)))
				  (with-transaction (:store-controller dst)
				    (add-index newcidx
					       :index-name name 
					       :key-form key-form
					       :populate nil))))
			      classidx)
		 ;; Add the class index to the class root
		 (with-transaction (:store-controller dst)
		   (setf (get-value classname (controller-class-root dst)) newcidx))
		 ;; Update the class to point at objects in the new store
		 (setf (%index-cache (find-class classname)) newcidx)
		 ;; Migrate the index objects
		 (copy-cindex-contents newcidx classidx)
		 ;; And remember the class index just incase it's indexed elswhere 
		 ;; (and trips the assert above)
		 (register-copied-object classidx newcidx)))
	     (controller-class-root src))
  ;; Copy all other reachable objects
  (format t "Copying the root:~%")
  (map-btree (lambda (key value)
	       (let ((newval (migrate dst value)))
		 (unless (eq key *elephant-properties-label*)
		   (ensure-transaction (:store-controller dst :txn-nosync t)
		     (add-to-root key newval :sc dst)))))
	     (controller-root src))
  dst)

(defun copy-cindex-contents (new old)
  (let ((sc (get-con new))
	(count 1))
    (map-btree (lambda (oldoid oldinst)
		 (declare (ignore oldoid))
		 (when (= (mod (1- (incf count)) 1000) 0)
		   (format t "~A objects copied~%" count))
		 (let ((newinst (migrate sc oldinst)))
		   (ensure-transaction (:store-controller sc)
		     ;; This isn't redundant in most cases, but we may have
		     ;; indexed objects without slots and without a slot
		     ;; write the new index won't be updated in that case
		     (setf (get-value (oid newinst) new) newinst))))
	       old)))

;;
;; HANDLE DEFAULTS
;;

(defmethod migrate ((dst t) (src t))
  (error "Cannot migrate ~A of type ~A to destination of type ~A" src (type-of src) (type-of dst)))

(defmethod migrate ((dst store-controller) (src t))
  "Default: standard objects are automatically migrated"
  src)

;;
;; ERROR CHECKING
;;

(defmethod migrate :before ((dst store-controller) (src persistent))
  "This provides some sanity checking that we aren't trying to copy
   to the same controller.  We also need to be careful about deadlocking
   our transactions among the two gets/puts.  Each leaf migration should
   be in its own transaction to avoid too many write locks. "
  (let ((dst-spec (controller-spec dst)))
    (unless (object-was-copied-p src)
      (typecase src
	(store-controller (assert (not (equal dst-spec (controller-spec src)))))
	(persistent (assert (not (equal dst-spec (dbcn-spc-pst src)))))))))

(defmethod migrate :before ((dst store-controller) (src store-controller))
  "This method ensures that we reset duplicate object detection over the store-controller"
  (initialize-migrate-duplicate-detection))

(defmethod migrate :after ((dst store-controller) (src store-controller))
  "This method ensures that we reset duplicate object detection over the store-controller"
  (clear-migrate-duplicate-detection))

(defmethod migrate ((dst store-controller) (src standard-class))
  (error "Cannot migrate class objects (i.e. ~A)" src))

(defmethod migrate ((dst store-controller) (src function))
  (error "Cannot migrate function objects (i.e. ~A)" src))

;;
;; PERSISTENT OBJECTS
;;

(defvar *inhibit-slot-copy* nil)

(defmethod migrate ((dst store-controller) (src persistent))
   "Migrate a persistent object and apply a binary (lambda (dst src) ...) 
    function to the new object.  Users can override migrate by creating
    a function that calls the default copy and then does stuff with the
    slot values.  A dynamic variable: *inhibit-slot-copy* can be bound
    in the caller to keep the new object from having it's slots copied"
   ;; Copy or lookup persistent object
   (if (object-was-copied-p src)
       (retrieve-copied-object dst src)
       (copy-persistent-object dst src)))

(defun copy-persistent-object (dstsc src)
  "Copy the persistent object reference by making a new one and
   potentially copy over the slot values as well"
  (let* ((class (class-of src))
	 (dst (make-instance (class-of src) :sc dstsc)))
    (register-copied-object src dst)
    (unless (inhibit-indexed-slot-copy? dstsc class)
      (copy-persistent-slots dstsc dst (class-of src) src))
    dst))

(defun inhibit-indexed-slot-copy? (sc class)
  "Make sure that we don't copy slots if the user inhibits
   or if the class is indexed and has not yet migrated to
   the new store - the indexing copy will do this."
  (or *inhibit-slot-copy*
      (and (indexed class)
	   (not (equal (controller-spec sc)
		       (dbcn-spc-pst (%index-cache class)))))))

(defun copy-persistent-slots (dstsc dst class src)
  "Copy only persistent slots from src to dst"
  (ensure-transaction (:store-controller dstsc)
    (loop for slot-def in (persistent-slot-defs class) do
	 (when (slot-boundp-using-class class src slot-def)
;;	   (format t "Slotname: ~A  value: ~A~%" (elephant::slot-definition-name slot-def) 
;;		   (slot-value-using-class class src slot-def))
	   (let ((value (migrate dstsc (slot-value-using-class class src slot-def))))
	     (setf (slot-value-using-class class dst slot-def) value))))))

;;
;; User utilities for persistent objects
;;

(defmacro with-inhibited-slot-copy ((&key &allow-other-keys) &body body)
  "A user macro to support special slot handling in persistent objects"
  `(let ((*inhibit-slot-copy* t))
     (declare (special *inhibit-slot-copy*))
     ,@body))

;;
;; MIGRATE BTREE INDICES (override default persistent behavior)
;;

(defmethod migrate ((dst store-controller) (src btree))
  "Copy an index and it's contents to the target repository"
  (if (object-was-copied-p src)
      (retrieve-copied-object dst src)
      (let ((newbtree  (build-btree dst)))
	(ensure-transaction (:store-controller dst :txn-nosync t)
	  (copy-btree-contents dst newbtree src))
	(register-copied-object src newbtree)
	newbtree)))

(defmethod migrate ((dst store-controller) (src indexed-btree))
  "Also copy the inverse indices for indexed btrees"
  (if (object-was-copied-p src)
      (retrieve-copied-object dst src)
      (let ((newbtree 
	     (ensure-transaction (:store-controller dst :txn-nosync t)
	       (build-indexed-btree dst))))
	(ensure-transaction (:store-controller dst :txn-nosync t)
	  (copy-btree-contents dst newbtree src))
	(map-indices (lambda (name srciidx)
		       (format t "Adding index: ~A~%" name)
		       (let ((key-form (key-form srciidx)))
			 (ensure-transaction (:store-controller dst :txn-nosync t)
			   (add-index newbtree :index-name name :key-form key-form :populate t))))
		     src)
	(register-copied-object src newbtree)
	newbtree)))

(defmethod copy-btree-contents ((sc store-controller) dst src)
  (map-btree (lambda (key value)
	       (let ((newval (migrate sc value))
		     (newkey (migrate sc key)))
		   (setf (get-value newkey dst) newval)))
	     src))

;;
;; MIGRATE AGGREGATE LISP OBJECTS THAT MAY REFER TO OTHER PERSISTENT OBJECTS
;;

(defmethod migrate ((dst store-controller) (src standard-object))
  "If we have persistent objects that are unindexed and ONLY stored in
   a standard object slot that is referenced from the root, then it
   will only be copied by recursing through the slot substructure just
   as the serializer will, but copying any persistent objects found"
  (let ((svs (slots-and-values src)))
    (loop for i from 0 below (/ (length svs) 2) do
	 (let ((slotname (pop svs))
	       (value (pop svs)))
	   (setf (slot-value src slotname) (migrate dst value)))))
  src)


(defmethod migrate ((dst store-controller) (src structure-object))
  "Walks structure slot values and ensures that any persistent references
   are written back into the slot pointint to the new store"
  (let ((svs (struct-slots-and-values src)))
    (loop for i from 0 below (/ (length svs) 2) do
	 (let ((slotname (pop svs))
	       (value (pop svs)))
	   (setf (slot-value src slotname)
		 (migrate dst value)))))
  src)

(defmethod migrate ((dst store-controller) (src cons))
  "WARNING: This doesn't work for circular lists"
  (cons (migrate dst (car src))
	(migrate dst (cdr src))))

(defmethod migrate ((dst store-controller) (src array))
  "We only need to handle arrays of type 't' that point to other objects; 
   fixnum, float, etc arrays don't need to be copied"
  (loop for i fixnum from 0 below (array-total-size src) do
       (let ((value (row-major-aref src i)))
	 (setf (row-major-aref src i)
	       (migrate dst value))))
  src)

(defmethod migrate ((dst store-controller) (src hash-table))
  "Migrate each hash element as the types are non-uniform"
  (maphash (lambda (key value)
	     (setf (gethash key src)
		   (migrate dst value)))
	   src)
  src)

;;
;; MAINTAIN CORRESPONDENCE BETWEEN OLD STORE POBJS and NEW STORE POBJS
;;  

(defvar *oid-hash* (make-hash-table))
(defvar *oid-store* nil) 
(defvar *oid-spec* nil)
(defvar *oid-btree* nil)

(defun set-oid-spec (spec)
  "Set to nil to perform oid mapping in memory, set to a valid spec to
   perform the mapping on disk"
  (setf *oid-spec* spec))

(defun initialize-migrate-duplicate-detection ()
  "Reset oid map so that all references to a given object
   in the source only point to one copy in the target"
  (if *oid-spec*
      (progn
	(setf *oid-store* (open-store *oid-spec* :recover t))
	(setf *oid-btree* (make-btree *oid-store*))
	(setf *oid-hash* nil))
      (progn
	(setf *oid-hash* (make-hash-table))
	(setf *oid-btree* nil))))

(defun clear-migrate-duplicate-detection ()
  (when *oid-spec*
    (setf *oid-btree* nil)
    (close-store *oid-store*)
    (setf *oid-store* nil))
  (when *oid-hash* 
    (setf *oid-hash* nil)))

(defun object-was-copied-p (src)
  "Test whether a source object has been copied"
  (assert (subtypep (type-of src) 'persistent))
  (cond (*oid-btree*
	 (existsp (oid src) *oid-btree*))
	(*oid-hash*
	 (gethash (oid src) *oid-hash*))
	(t (warn "Test for persistent copy not inside top level call; returning nil")
	   nil)))


(defun register-copied-object (src dst)
  "When copying a source object, store it in the oid map"
  (assert (not (equal (dbcn-spc-pst src) (dbcn-spc-pst dst))))
  (when (or *oid-btree* *oid-hash*)
    (if *oid-btree*
	(setf (get-value (oid src) *oid-btree*)
	      (cons (oid dst) (type-of dst)))
	(setf (gethash (oid src) *oid-hash*) dst))))
  
(defun retrieve-copied-object (dst src)
  "Get a copied object from the oid map"
  (assert (subtypep (type-of dst) 'store-controller))
  (cond (*oid-btree*
	 (let ((record (get-value (oid src) *oid-btree*)))
	   (get-cached-instance dst (car record) (cdr record))))
	(*oid-hash*
	 (gethash (oid src) *oid-hash*))
	(t (error "Cannot retrieve an object from oid-to-oid map 
                   when not inside top-level call"))))
	 




