;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; classindex.lisp -- use btree collections to track objects by slot values
;;;                    via metaclass options or accessor :after methods
;;; 
;;; By Ian Eslick <ieslick at common-lisp.net>
;;;
;;; part of
;;;
;;; Copyright (c) 2004 by Andrew Blumberg and Ben Lee
;;; <ablumberg@common-lisp.net> <blee@common-lisp.net>
;;;
;;; Portions Copyright (c) 2005-2007 by Robert Read and Ian Eslick
;;; <rread common-lisp net> <ieslick common-lisp net>
;;;
;;; Elephant: an object-oriented database for Common Lisp
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Limited General Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;

(in-package "ELEPHANT")

(declaim #-elephant-without-optimize (optimize (speed 3) (safety 1)))

;; =================================
;;    LOW-LEVEL API SPECIFICATION
;; =================================

;;
;; Operates against the current *store-controller* but many 
;; accept a :sc keyword to change the controller.  The specific 
;; indices created can be specialized on the controller type.  
;; See the internal implementor protocol below
;;

(defgeneric find-class-index (persistent-metaclass &rest rest)
  (:documentation "This method is the way to access the class index via
    the class object.  We can always fetch it or we can cache it in
    the class itself.  It returns an indexed-btree."))

(defgeneric find-inverted-index (persistent-metaclass index-name &key null-on-fail)
  (:documentation "This method finds an inverted index defined on
   the class described by an instance of persistent-metaclass."))

(defgeneric enable-class-indexing (persistent-metaclass slot-names &rest rest)
  (:documentation "Enable a class instance index for this object.  It's
    an expensive thing to support on writes so know that you need it 
    before you do it."))

(defgeneric disable-class-indexing (persistent-metaclass &rest rest)
  (:documentation "Delete and remove class instance indexing and any
    secondary indices defined against it"))

(defgeneric add-class-slot-index (persistent-metaclass slot-name &rest rest)
  (:documentation "Add a per-slot class index option to the class
    index based on the class accessor method"))

(defgeneric remove-class-slot-index (persistent-metaclass slot-name &key sc)
  (:documentation "Remove the per-slot index from the db"))

(defgeneric add-class-derived-index (persistent-metaclass name derived-defun &rest rest)
  (:documentation "Add a simple secondary index to this class based on
    a function that computes a derived parameter.  WARNING: derived
    parameters are only valid on persistent slots.  An arbitrary function
    here will fail to provide consistency on transient slots or global
    data that is not stored in the persistent store.  Derived indexes are
    deleted and rebuilt when a class is redefined"))

(defgeneric remove-class-derived-index (persistent-metaclass name &rest rest)
  (:documentation "Remove a derived index by providing the derived name
   used to name the derived index"))

;; ==================================
;;    LOW-LEVEL CLASS INDEXING API
;; ==================================

(defmethod find-class-index ((class-name symbol) &key (sc *store-controller*) (errorp t))
  (find-class-index (find-class class-name) :sc sc :errorp errorp))

(defmethod class-indexedp-by-name ((class-name symbol) &key (sc *store-controller*))
  (declare (ignore sc))
  (let ((class (find-class class-name nil)))
    (when class (indexed class))))

(define-condition persistent-class-not-indexed ()
  ((class-obj :initarg :class :initarg nil :reader unindexed-class-obj))
  (:report (lambda (condition stream)
	     (format stream "Class ~A is not enabled for indexing"
		     (class-name (unindexed-class-obj condition))))))
		    
(defun signal-class-not-indexed (class)
  (cerror "Ignore and continue?"
          'persistent-class-not-indexed 
	  :class class))

(defmethod find-class-index ((class persistent-metaclass) &key (sc *store-controller*) (errorp t))
  (ensure-finalized class)
  (if (not (indexed class))
      (when errorp
	(signal-class-not-indexed class))
      (if (class-index-cached? class)
	  (%index-cache class) ;; we've got a cached reference, just return it
	  (multiple-value-bind (btree found)
	      (get-value (class-name class) (controller-class-root sc))
	    (if found
		(cache-existing-class-index class btree sc)
		(cache-new-class-index class sc))))))

(defun ensure-finalized (class)
  (when (not (class-finalized-p class))
    (when *warn-on-manual-class-finalization*
      (warn "Manually finalizing class ~A" (class-name class)))
    (finalize-inheritance class)))

(defun cache-existing-class-index (class btree sc)
  "If we have a persistent index already, assign, synchronize & return it"
  (let ((method (determine-synch-method class)))
    (setf (%index-cache class) btree)
    (synchronize-class-to-store class :sc sc :method method)
    btree))

(defun cache-new-class-index (class sc)
  "If not cached or persistent then this is a new class, make the new index"
  (if (indexed class)
      (enable-class-indexing class (indexing-record-slots (indexed-record class)) :sc sc)
      (signal-class-not-indexed class)))

(defmethod find-inverted-index ((class symbol) slot &key (null-on-fail nil))
  (find-inverted-index (find-class class) slot :null-on-fail null-on-fail))

(defmethod find-inverted-index ((class persistent-metaclass) slot &key (null-on-fail nil))
  (let* ((cidx (find-class-index class :errorp (not null-on-fail)))
	 (idx (or (get-index cidx slot)
		  (get-index cidx (make-derived-name slot)))))
    (if idx 
	idx 
	(if null-on-fail
	    nil
	    (cerror "Ignore and continue?"
		    "Inverted index ~A not found for class ~A with persistent slots: ~A" 
		    slot (class-name class) (car (%persistent-slots class)))))))

(defmethod find-inverted-index-names ((class persistent-metaclass))
  (let ((names nil))
    (map-indices (lambda (name idx) 
		   (declare (ignore idx)) 
		   (push name names))
		 (find-class-index class))
    names))

(defmethod close-controller :before ((sc store-controller))
  "Ensure the classes don't have stale references to closed stores!"
  (when (controller-class-root sc)
    (handler-case 
	(with-transaction (:store-controller sc :txn-sync t :retries 2)
	  (map-btree (lambda (class-name index)
		       (declare (ignore index))
		       (let ((class (find-class class-name nil)))
			 (when (and class (subtypep class 'persistent-metaclass))
			   (setf (%index-cache class) nil))))
		     (controller-class-root sc)))
      (t (e) (warn "Unable to clear class index caches ~A" e)))))
      

;; ============================
;;   METACLASS PROTOCOL HOOKS
;; ============================

(defmethod indexed-slot-writer ((class persistent-metaclass) (instance persistent-object) (slot-def persistent-slot-definition) new-value)
  "Anything that side effects a persistent-object slot should call this to keep
   the dependant indices in synch.  Only classes with derived indices need to
   update on writes to non-indexed slots.  This is a side effect of user-managed
   indices in Elephant - a necessity because we allow arbitrary lisp expressions to
   determine index value so without bi-directional pointers, the indices cannot 
   automatically update a changed indexed value in derived slots"
  (let ((slot-name (slot-definition-name slot-def))
	(oid (oid instance))
	(con (get-con instance)))
    (declare (type fixnum oid))
    (if (no-indexing-needed? class instance slot-def oid)
	(persistent-slot-writer con new-value instance slot-name)
	(let ((class-idx (find-class-index class)))
	  (ensure-transaction (:store-controller con)
	    (when (get-value oid class-idx)
	      (remove-kv oid class-idx))
	    (persistent-slot-writer con new-value instance slot-name)
	    (setf (get-value oid class-idx) instance))))))

(defmethod indexed-slot-makunbound ((class persistent-metaclass) (instance persistent-object) (slot-def persistent-slot-definition))
  (let ((class-idx (find-class-index class))
	(oid (oid instance))
	(sc (get-con instance)))
    (ensure-transaction (:store-controller sc)
      (let ((obj (get-value oid class-idx)))
	(remove-kv oid class-idx)
	(persistent-slot-makunbound sc instance (slot-definition-name slot-def))
	(setf (get-value oid class-idx) obj)))))

(defun no-indexing-needed? (class instance slot-def oid)
  (declare (ignore instance))
  (or (and (not (indexed slot-def)) ;; not indexed
	   (not (indexing-record-derived (indexed-record class)))) ;; no derived indexes
      (member oid *inhibit-indexing-list*))) ;; currently inhibited

;; ============================
;;   EXPLICIT INDEX MGMT API
;; ============================

(defmethod enable-class-indexing ((class persistent-metaclass) indexed-slot-names &key (sc *store-controller*))
  (let ((croot (controller-class-root sc)))
    (multiple-value-bind (btree found)
	(get-value (class-name class) croot)
      (when found 
	  (if (indexed class)
	      (error "Class is already enabled for indexing!  Run disable class indexing to clean up.")
	      (progn
		(let ((slots nil))
		  (map-indices (lambda (k v) (declare (ignore v)) (push k slots)) btree)
		  (warn "Class has pre-existing database index, enabling indexing for slots: ~A" 
			(setf indexed-slot-names (union slots indexed-slot-names)))))))
      ;; Put class instance index into the class root & cache it in the class object
      (update-indexed-record class indexed-slot-names :class-indexed t)
      (ensure-transaction (:store-controller sc)
	(when (not found)
	  (let ((class-idx (build-indexed-btree sc)))
	    (setf (get-value (class-name class) croot) class-idx)
	    (setf (%index-cache class) class-idx)))
	;; Add all the indexes
	(loop for slot in indexed-slot-names do
	     (unless (find-inverted-index class slot :null-on-fail t)
	       (add-class-slot-index class slot :populate nil :sc sc))))
	;; Sanity check
      (let ((record (indexed-record class)))
	(declare (ignorable record))
	(assert (indexed class)))
      (find-class-index class :sc sc :errorp t))))
  
(defmethod disable-class-indexing ((class-name symbol) &key (errorp t) (sc *store-controller*))
  (let ((class (find-class class-name errorp)))
    (when class
      (disable-class-indexing class :sc sc))))
  
(defmethod disable-class-indexing ((class persistent-metaclass) &key (sc *store-controller*) (errorp nil))
  "Disable any class indices from the database, even if the current class object is not
   officially indexed.  This ensures there is no persistent trace of a class index.  Storage
   is reclaimed also"
  (let ((class-idx (find-class-index class :sc sc :errorp errorp)))
    (if class-idx 
	(progn
	  (wipe-class-indexing class :sc sc)
	  (update-indexed-record class nil))
	(when errorp
	  (error "No class index exists in persistent store ~A" sc)
	  (return-from disable-class-indexing nil)))))

(defmethod wipe-class-indexing ((class persistent-metaclass) &key (sc *store-controller*))
  (wipe-class-indexing (class-name class) :sc sc))

(defmethod wipe-class-indexing ((class-name symbol) &key (sc *store-controller*))
  (let ((cindex (get-value class-name (controller-class-root sc)))
	(class (find-class class-name nil)))
    (when cindex
      ;; Delete all the values
      (with-transaction (:store-controller sc)
	(with-btree-cursor (cur cindex)
	  (loop while (cursor-next cur) do
	       (cursor-delete cur))))
      ;; Get the names of all indices & remove them 
      (let ((names nil))
	(map-indices (lambda (name secondary-index)
		       (declare (ignore secondary-index))
		       (push name names))
		     cindex)
	(dolist (name names)
	  (when (member name (class-slots class))
	    (if class 
		(remove-class-slot-index class name)
		(with-transaction (:store-controller sc)
		  (remove-index cindex name))))))
      ;; Drop the class instance index from the class root
      (with-transaction (:store-controller sc)
	(remove-kv class-name (controller-class-root sc)))
      (when class
	(setf (%index-cache class) nil)))))

(defmethod add-class-slot-index ((class symbol) slot-name &key (sc *store-controller*))
  (add-class-slot-index (find-class class) slot-name :sc sc))

(defmethod add-class-slot-index ((class persistent-metaclass) slot-name &key (sc *store-controller*) (populate t) (update-class t))
  (if (find-inverted-index class slot-name :null-on-fail t)
      (warn "Duplicate slot index named ~A requested for class ~A.  Ignoring." 
	    slot-name (class-name class))
      (progn
	(when update-class (register-indexed-slot class slot-name))
;;	(with-transaction (:store-controller sc)
	  (add-index (find-class-index class :sc sc)
		     :index-name slot-name 
		     :key-form (make-slot-key-form class slot-name)
		     :populate populate)
	  t)))

(defmethod remove-class-slot-index ((class symbol) slot-name &key (sc *store-controller*))
  (remove-class-slot-index (find-class class) slot-name :sc sc))
	     
(defmethod remove-class-slot-index ((class persistent-metaclass) slot-name &key 
				    (sc *store-controller*) (update-class t))
  (if (find-inverted-index class slot-name :null-on-fail t)
      (progn
	(when update-class (unregister-indexed-slot class slot-name))
	(with-transaction (:store-controller sc)
	  (remove-index (find-class-index class :sc sc) slot-name))
	t)
      (progn
	(warn "Slot index ~A not found for class ~A" slot-name (class-name class))
	nil)))

(defmethod add-class-derived-index ((class symbol) name derived-defun &key (sc *store-controller*) (populate t))
  (add-class-derived-index (find-class class) name derived-defun :sc sc :populate populate))

(defmethod add-class-derived-index ((class persistent-metaclass) name derived-defun &key 
				    (populate t) (sc *store-controller*) (update-class t))
  (let ((class-idx (find-class-index class :sc sc)))
    (if (find-inverted-index class (make-derived-name name) :null-on-fail t)
	(error "Duplicate derived index requested named ~A on class ~A" name (class-name class))
	(progn
	  (when update-class (register-derived-index class name))
	  (add-index class-idx
		     :index-name (make-derived-name name)
		     :key-form (make-derived-key-form derived-defun)
		     :populate populate)))))

(defmethod remove-class-derived-index ((class symbol) name &key (sc *store-controller*))
  (remove-class-derived-index (find-class class) name :sc sc))
	     
(defmethod remove-class-derived-index ((class persistent-metaclass) name &key 
				       (sc *store-controller*) (update-class t))
  (if (find-inverted-index class (make-derived-name name) :null-on-fail t)
      (progn
	(when update-class (unregister-derived-index class name))
	(with-transaction (:store-controller sc)
	  (remove-index (find-class-index class :sc sc) (make-derived-name name)))
	t)
      (progn
	(warn "Derived index ~A does not exist in ~A" name (class-name class))
	nil)))
    
;; ===================
;;   USER CURSOR API
;; ===================

(defgeneric make-inverted-cursor (class name)
  (:documentation "Define a cursor on the inverted (slot or derived) index"))

(defgeneric make-class-cursor (class)
  (:documentation "Define a cursor over all class instances"))


(defmethod make-inverted-cursor ((class persistent-metaclass) name)
  (make-cursor (find-inverted-index class name)))

(defmethod make-inverted-cursor ((class symbol) name)
  (make-cursor (find-inverted-index class name)))

(defmacro with-inverted-cursor ((var class name) &body body)
  "Bind the var argument to an inverted cursor on the index
   specified the provided class and index name"
  `(let ((,var (make-inverted-cursor ,class ,name)))
     (unwind-protect (progn ,@body)
       (cursor-close ,var))))

(defmethod make-class-cursor ((class persistent-metaclass))
  (make-cursor (find-class-index class)))

(defmethod make-class-cursor ((class symbol))
  (make-cursor (find-class-index class)))

(defmacro with-class-cursor ((var class) &body body)
  "Bind the var argument in the body to a class cursor on the
   index specified the provided class or class name"
  `(let ((,var (make-class-cursor ,class)))
     (unwind-protect (progn ,@body)
       (cursor-close ,var))))


;; ======================
;;    USER MAPPING API 
;; ======================

(defun map-class (fn class &key collect)
  "Perform a map operation over all instances of class.  Takes a
   function of one argument, a class instance.  Setting the collect
   keyword to true will return a list of the values returned from
   calls to fn on successive instances of the class."
  (let* ((class (if (symbolp class)
		    (find-class class)
		    class))
	 (class-idx (find-class-index class)))
    (flet ((map-fn (k v)
	     (declare (ignore k))
	     (funcall fn v)))
      (declare (dynamic-extent map-fn))
      (map-btree #'map-fn class-idx :collect collect))))

(defun map-inverted-index (fn class index &rest args &key start end (value nil value-p) from-end collect)
  "map-inverted-index maps a function of two variables, taking key
   and instance, over a subset of class instances in the order
   defined by the index.  Specify the class and index by quoted
   name.  The index may be a slot index or a derived index.

   Read the docstring for map-index for details on what the 
   various keywords do."
  (declare (dynamic-extent args)
	   (ignorable args))
  (let* ((index (if (symbolp index)
		    (find-inverted-index class index)
		    index)))
    (flet ((wrapper (key value pkey)
	     (declare (ignore pkey))
	     (funcall fn key value)))
      (declare (dynamic-extent wrapper))
      (if value-p
	  (map-index #'wrapper index :value value :collect collect)
	  (map-index #'wrapper index :start start :end end :from-end from-end :collect collect)))))
		    


;; =================
;;   USER SET API 
;; =================

(defgeneric get-instances-by-class (persistent-metaclass)
  (:documentation "Retrieve all instances from the class index as a list of objects"))

(defgeneric get-instance-by-value (persistent-metaclass slot-name value)
  (:documentation "Retrieve instances from a slot index by value.  Will return only the first
                  instance if there are duplicates."))

(defgeneric get-instances-by-value (persistent-metaclass slot-name value)
  (:documentation "Returns a list of all instances where the slot value is equal to value."))

(defgeneric get-instances-by-range (persistent-metaclass slot-name start end)
  (:documentation "Returns a list of all instances that match
                   values between start and end.  An argument of
                   nil to start or end indicates, respectively,
                   the lowest or highest value in the index"))


(defun identity2 (k v)
  (declare (ignore k))
  v)

(defun identity3 (k v pk)
  (declare (ignore k pk))
  v)

(defmethod get-instances-by-class ((class symbol))
  (get-instances-by-class (find-class class)))

(defmethod get-instances-by-class ((class persistent-metaclass))
  (map-class #'identity class :collect t))

(defmethod get-instances-by-value ((class symbol) slot-name value)
  (get-instances-by-value (find-class class) slot-name value))

(defmethod get-instances-by-value ((class persistent-metaclass) slot-name value)
  (declare (type (or string symbol) slot-name))
  (map-inverted-index #'identity2 class slot-name :value value :collect t))

(defmethod get-instance-by-value ((class symbol) slot-name value)
  (let ((list (get-instances-by-value (find-class class) slot-name value)))
    (when (consp list)
      (car list))))

(defmethod get-instance-by-value ((class persistent-metaclass) slot-name value)
  (let ((list (get-instances-by-value class slot-name value)))
    (when (consp list)
      (car list))))

(defmethod get-instances-by-range ((class symbol) slot-name start end)
  (get-instances-by-range (find-class class) slot-name start end))

(defmethod get-instances-by-range ((class persistent-metaclass) idx-name start end)
  (declare (type (or fixnum null) start end)
	   (type symbol idx-name))
  (map-inverted-index #'identity2 class idx-name :start start :end end :collect t))

(defun drop-instances (instances &key (sc *store-controller*))
  "Removes a list of persistent objects from all class indices
   and unbinds any slot values"
  (when instances
    (assert (consp instances))
    (do-subsets (subset 500 instances)
      (ensure-transaction (:store-controller sc)
	(mapc (lambda (instance)
		(drop-pobject instance)
		(remove-kv (oid instance) (find-class-index (class-of instance))))
	      subset)))))
       
