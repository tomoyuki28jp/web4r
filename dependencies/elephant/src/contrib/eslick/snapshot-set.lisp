;;;
;;; Copyright (c) 2007 Ian Eslick <ieslick common-lisp net>
;;;
;;; Simple snapshot sets.  Create a snapshot set for standard objects,
;;; work in-memory and then call snapshot to save the objects to 
;;; the underlying store-controller.
;;;


;;
;; Limitations:
;;
;; - Hashes can be registered as indexes of objects.  Keys should be
;;   simple (numbers, strings, symbols) although arrays are probably
;;   OK too.  Values should also be simple or subclasses of
;;   standard-object.  
;;
;; - When a snapshot is taken of a hash, all values that are
;;   standard objects are registered.  Any refs to registered
;;   objects are properly restored on retrieval
;; 
;; Easy extensions:
;; 
;; - Support arrays of objects as well as hash-tables
;; 
;; - Create a method standard-object-dirty-p that defaults to 't' but
;;   allows users to implement a method that avoids storing unchanged
;;   objects.
;;
;; - Enable versioned or named snapshots 
;;
;; - Better interface API (special object metaclass?) to create a more
;;   natural abstraction.  Could also add object journaling for
;;   prevalence dynamics via rucksack log model.  For example, writes
;;   to slots are saved to a persistent list that gets reused after
;;   snapshots (id slotname value).  Slot reads are as usual.
;;
;; - Avoid stack use during recursions.  Push new objects onto a stack
;;   for later processing so stack depth is constant.
;;
;; - In place restores.  A future version could traverse the existing
;;   object cache, dropping new references and restoring old ones
;;   according to the state of the snapshot-set on disk such that the
;;   existing in-memory lisp pointers were still valid..as long as there
;;   were not external pointers into objects that are dropped leading to
;;   an inconsistency.
;; 


(in-package :elephant)

(defpclass snapshot-set ()
  ((index :accessor snapshot-set-index :initform (make-btree))
   (next-id :accessor snapshot-set-next-id :initform 0)
   (root :accessor snapshot-set-root :initform nil)
   (cache :accessor snapshot-set-cache :initform (make-hash-table :weak-keys t) :transient t)
   (touched :accessor snapshot-set-touched 
            :initform (make-array 20 :element-type 'fixnum :initial-element 0 :fill-pointer t :adjustable t)
	    :transient t))
  (:documentation "Keeps track of a set of standard objects
    allowing a single snapshot call to update the store
    controller with the latest state of all objects registered with
    this set"))

(defmethod initialize-instance :after ((set snapshot-set) &key lazy-load &allow-other-keys)
  (unless lazy-load (restore set)))

;; =================
;; User methods
;; =================

(defmethod register-object ((object standard-object) (set snapshot-set))
  "Register a standard object.  Not recorded until snapshot is called on db"
  (aif (lookup-cached-id object set)
       (values object it)
       (let ((id (incf (snapshot-set-next-id set))))
	 (cache-snapshot-object id object set)
	 (values object id))))

(defmethod register-object ((hash hash-table) (set snapshot-set))
  "Adds a hash table to the snapshot set and registers any standard objects
   stored as values that are not already part of the snapshot.  Must call snapshot
   to save."
  (aif (lookup-cached-id hash set)
       (values hash it)
       (let ((id (incf (snapshot-set-next-id set))))
	 (cache-snapshot-object id hash set)
	 (values hash id))))

(defmethod register-object ((default t) (set snapshot-set))
  (error "Cannot register objects of type ~A" (type-of default)))

(defmethod unregister-object (object (set snapshot-set))
  "Drops the object from the cache and backing store"
  (let ((id (gethash object (snapshot-set-cache set))))
    (when (null id)
      (error "Object ~A not registered in ~A" object set))
    (drop-cached-object object set)
    (delete-snapshot-object id set)))

(defmethod snapshot-root ((set snapshot-set))
  "Get the snapshot root object"
  (when (snapshot-set-root set)
    (lookup-cached-object (snapshot-set-root set) set)))

(defmethod (setf snapshot-root) (value (set snapshot-set))
  "Specify a root object for the set.  There is only 1
   so it should be a hash or the root node of a graph"
  (setf (snapshot-set-root set) 
	(multiple-value-bind (obj id)
	    (register-object value set)
	  (declare (ignore obj))
	  id))
  value)

(defun map-set (fn set)
  "Iterates through all values in the active set, not the
   saved snapshot"
  (maphash (lambda (k v)
	     (declare (ignore v))
	     (funcall fn k))
	   (snapshot-set-cache set)))

(defmethod snapshot ((set snapshot-set))
  "Saves all objects in the set (and any objects reachable from the
   current set of objects) to the persistent store"
  (with-transaction (:store-controller (get-con (snapshot-set-index set)))
    (loop for (obj . id) in (get-cache-entries (snapshot-set-cache set)) do
	  (save-snapshot-object id obj set))
    (collect-untouched set))
  (values set t))

(defmethod restore ((set snapshot-set))
  "Restores a snapshot by setting the snapshot-set state to the last snapshot.
   If this is used during runtime, the user needs to drop all references
   to objects and retrieve again from the snapshot set.  Also used to initialize
   the set state when a set is created, for example pulled from the root of a
   store-controller, unless :lazy-load is specified"
  (clear-cache set)
  (map-btree (lambda (id object)
	       (load-snapshot-object id object set))
	     (snapshot-set-index set))
  (values set t))

;; ===============
;; Shorthand
;; ===============

;; Cache ops

(defun clear-cache (set)
  (clrhash (snapshot-set-cache set)))

(defun cache-snapshot-object (id obj set)
  (setf (gethash obj (snapshot-set-cache set)) id))

(defun lookup-cached-id (obj set)
  (gethash obj (snapshot-set-cache set)))

(defun lookup-cached-object (id set)
  (find-hash-key-by-value id (snapshot-set-cache set)))

(defun find-hash-key-by-value (value hash)
  (maphash (lambda (k v)
	     (when (eq v value) 
	       (return-from find-hash-key-by-value k)))
	   hash))

(defun drop-cached-object (obj set)
  (remhash obj (snapshot-set-cache set)))

(defun get-cache-entries (hash)
  (let ((result nil))
    (maphash (lambda (obj id)
	       (push (cons obj id) result))
	     hash)
    result))

;; Save objects

(defclass setref ()
  ((id :accessor snapshot-set-reference-id :initarg :id)))

(defun setrefp (obj)
  (eq (type-of obj) 'setref))

(defun standard-object-subclass-p (obj)
  (subtypep (type-of obj) 'standard-object))

(defun touch (id set)
  (vector-push-extend id (snapshot-set-touched set) 50))

(defun touched (id set)
  (find id (snapshot-set-touched set)))

(defun clear-touched (set)
  (loop for i fixnum from 0 upto (1- (length (snapshot-set-touched set))) do
       (setf (aref (snapshot-set-touched set) i) 0)))

(defun save-snapshot-object (id obj set)
  (unless (touched id set)
    (setf (get-value id (snapshot-set-index set))
	  (cond ((standard-object-subclass-p obj)
		 (save-proxy-object obj set))
		((hash-table-p obj)
		 (save-proxy-hash obj set))
		(t (error "Cannot only snapshot standard-objects and hash-tables"))))
    (touch id set))
  id)

(defun save-proxy-object (obj set)
  (let ((svs (subsets 2 (slots-and-values obj))))
    (if (some #'reify-class-p (mapcar #'second svs))
	(let ((proxy (make-instance (type-of obj))))
	  (loop for (slotname value) in svs do
	       (setf (slot-value proxy slotname)
		     (if (reify-class-p value)
			 (reify-value value set)
			 value)))
	  proxy)
	obj)))

(defun save-proxy-hash (hash set)
  (let ((proxy (make-hash-table)))
    (maphash (lambda (key value)
	       (setf (gethash key proxy)
		     (if (reify-class-p value)
			 (reify-value value set) 
			 value)))
	     hash)
    proxy))

(defun reify-class-p (obj)
  (or (standard-object-subclass-p obj)
      (hash-table-p obj)))

(defun reify-value (obj set)
  (multiple-value-bind (obj id) 
      (register-object obj set)
    (make-instance 'setref :id (save-snapshot-object id obj set))))

(defun collect-untouched (set)
  (map-btree (lambda (k v) 
	       (declare (ignore v))
	       (unless (touched k set)
		 (remove-kv k (snapshot-set-index set))))
	     (snapshot-set-index set))
  (clear-touched set))

;; Load objects

(defun load-snapshot-object (id object set)
  (let ((object (ifret object (get-value id (snapshot-set-index set)))))
    (cond ((standard-object-subclass-p object)
	   (load-proxy-object id object set))
	  ((hash-table-p object)
	   (load-proxy-hash id object set))
	  (t (error "Unrecognized type ~A for id ~A in set ~A" (type-of object) id set)))))

;; Need to create placeholder, then populate slots

(defun load-proxy-object (id obj set)
  (ifret (lookup-cached-object id set)
	 (progn
	   (cache-snapshot-object id obj set)
	   (let ((svs (subsets 2 (slots-and-values obj))))
	     (loop for (slotname value) in svs do
		  (when (setrefp value)
		    (setf (slot-value obj slotname)
			  (load-snapshot-object (snapshot-set-reference-id value) nil set)))))
	   obj)))
		   
(defun load-proxy-hash (id hash set)
  (ifret (lookup-cached-object id set)
	 (progn
	   (cache-snapshot-object id hash set)
	   (maphash (lambda (key value)
		      (when (setrefp value)
			(setf (gethash key hash)
			      (load-snapshot-object (snapshot-set-reference-id value) nil set))))
		    hash)
	   hash)))
		      

;; Delete from snapshot

(defun delete-snapshot-object (id set)
  (remove-kv id (snapshot-set-index set)))

;; ==============================
;; Tests
;; ==============================

(defclass snapshot-test ()
  ((slot1 :accessor slot1 :initarg :slot1)
   (slot2 :accessor slot2 :initarg :slot2)))

(defun make-stest (slot1 slot2)
  (make-instance 'snapshot-test :slot1 slot1 :slot2 slot2))

(defun test-snapshot ()
  "Requires open store"
  (let* ((set (make-instance 'snapshot-set))
	 (hash (make-hash-table))
	 (test1 (make-stest 1 2))
	 (test2 (make-stest 10 20))
	 (test3 (make-stest (make-stest 'one 'two) (make-stest 'three 'four)))
	 (test4 (make-stest (slot1 test3) (slot2 test3))))
    (loop for num from 1
          for obj in (list test1 test2 test3 test4) do
	  (setf (gethash num hash) obj))
    (setf (snapshot-root set) hash)
    (add-to-root 'set set)
    (snapshot set)
    ;; Clear
    (setf set nil)
    (setf hash nil)
    (elephant::flush-instance-cache *store-controller*)
    #+allegro (excl:gc)
    #+sbcl (cl-user::gc)
    ;; Reload
    (setf set (get-from-root 'set))
    (setf hash (snapshot-root set))
    (let ((t1 (gethash 1 hash))
	  (t2 (gethash 2 hash))
	  (t3 (gethash 3 hash))
	  (t4 (gethash 4 hash)))
      (values
       (eq 1 (slot1 t1))
       (eq 20 (slot2 t2))
       (eq (slot2 t3)
	   (slot2 t4))))))
