;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; collections.lisp -- view Berkeley DBs as Lisp collections
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

(in-package :db-bdb)

(declaim  #-elephant-without-optimize (optimize (speed 3) (safety 1) (space 0) (debug 0)))

(defclass bdb-btree (btree) ()
  (:documentation "A BerkleyDB implementation of a BTree"))

;; It would be nice if this were a macro or a function
;; that would allow all of its arguments to be passed through;
;; otherwise an initialization slot is inaccessible.
;; I'll worry about that later.

;; Do these things need to take &rest arguments?
(defmethod build-btree ((sc bdb-store-controller))
  (make-instance 'bdb-btree :sc sc))

(defmethod get-value (key (bt bdb-btree))
  (let ((sc (get-con bt)))
    (with-buffer-streams (key-buf value-buf)
      (buffer-write-oid (oid bt) key-buf)
      (serialize key key-buf sc)
      (let ((buf (db-get-key-buffered (controller-btrees sc)
				      key-buf value-buf
				      :transaction (my-current-transaction sc))))
	(if buf (values (deserialize buf sc) T)
	    (values nil nil))))))

(defmethod existsp (key (bt bdb-btree))
  (let ((sc (get-con bt)))
    (with-buffer-streams (key-buf value-buf)
      (buffer-write-oid (oid bt) key-buf)
      (serialize key key-buf sc)
      (let ((buf (db-get-key-buffered 
		  (controller-btrees sc)
		  key-buf value-buf
		  :transaction (my-current-transaction sc))))
	(if buf t
	    nil)))))


(defmethod (setf get-value) (value key (bt bdb-btree))
    (let ((sc (get-con bt)))
      (with-buffer-streams (key-buf value-buf)
	(buffer-write-oid (oid bt) key-buf)
	(serialize key key-buf sc)
	(serialize value value-buf sc)
	(db-put-buffered (controller-btrees sc)
			 key-buf value-buf
			 :transaction (my-current-transaction sc))))
    value)

(defmethod remove-kv (key (bt bdb-btree))
  (let ((sc (get-con bt)) )
    (with-buffer-streams (key-buf)
      (buffer-write-oid (oid bt) key-buf)
      (serialize key key-buf sc)
      (db-delete-buffered (controller-btrees sc) key-buf 
			  :transaction (my-current-transaction sc)))))

(defmethod optimize-layout ((bt bdb-btree) &key (freelist-only t) (free-space nil) &allow-other-keys)
  (optimize-layout (get-con bt)
		    :start-key (oid bt)
		    :end-key (oid bt)
		    :freelist-only freelist-only
		    :free-space free-space))

;; Secondary indices

(defclass bdb-indexed-btree (indexed-btree bdb-btree)
  ((indices :accessor indices :initform (make-hash-table))
   (indices-cache :accessor indices-cache :initform (make-hash-table)
	       :transient t))
  (:metaclass persistent-metaclass)
  (:documentation "A BDB-based BTree supports secondary indices."))

(defmethod shared-initialize :after ((instance bdb-indexed-btree) slot-names
				     &rest rest)
  (declare (ignore slot-names rest))
  (setf (indices-cache instance) (indices instance)))

(defmethod build-indexed-btree ((sc bdb-store-controller))
  (make-instance 'bdb-indexed-btree :sc sc))

(defmethod build-btree-index ((sc bdb-store-controller) &key primary key-form)
  (make-instance 'bdb-btree-index :primary primary :key-form key-form :sc sc))

(defmethod add-index ((bt bdb-indexed-btree) &key index-name key-form (populate t))
  (let ((sc (get-con bt)))
;; Setting the value of *store-controller* is unfortunately
;; absolutely required at present, I think because the copying 
;; of objects is calling "make-instance" without an argument.
;; I am sure I can find a way to make this cleaner, somehow.
    (if (and (not (null index-name))
	     (symbolp index-name)
	     (or (symbolp key-form) (listp key-form)))
	;; Can it be that this fails?
	(let ((index
	       (ensure-transaction (:store-controller sc)
		 (let ((ht (indices bt))
		       (index (build-btree-index sc 
						 :primary bt 
						 :key-form key-form)))
		   (setf (gethash index-name (indices-cache bt)) index)
		   (setf (gethash index-name ht) index)
		   (setf (indices bt) ht)
		   index))))
	  (when populate (populate bt index))
	  index)
	(error "Invalid index initargs!"))))

(defmethod populate ((bt bdb-indexed-btree) index)
  (let ((sc (get-con bt)))
    (with-buffer-streams (primary-buf secondary-buf)
      (flet ((index (key skey)
	       (buffer-write-oid (oid bt) primary-buf)
	       (serialize key primary-buf sc)
	       (buffer-write-oid (oid index) secondary-buf)
	       (serialize skey secondary-buf sc)
	       ;; should silently do nothing if
	       ;; the key/value already exists
	       (db-put-buffered 
		(controller-indices sc)
		secondary-buf primary-buf
		:transaction (my-current-transaction sc))
	       (reset-buffer-stream primary-buf)
	       (reset-buffer-stream secondary-buf)))
	(let ((key-fn (key-fn index))
	      (last-key nil)
	      (continue t))
	  (loop while continue
	     do
	     (ensure-transaction (:store-controller sc)
	       (with-btree-cursor (cursor bt)
		 (if last-key 
		     (cursor-set cursor last-key)
		     (cursor-first cursor))
		 (loop for i from 0 upto 1000
		    while continue
		    do
		      (multiple-value-bind (valid? k v) (cursor-current cursor)
			(unless valid? (return-from populate t))
			(multiple-value-bind (index? skey) (funcall key-fn index k v)
			  (when index? (index k skey))))
		      (multiple-value-bind (valid? k v) (cursor-next cursor)
			(declare (ignore v))
			(if valid? 
			    (setf last-key k)
			    (setf continue nil))))))))))))


(defmethod map-indices (fn (bt bdb-indexed-btree))
  (maphash fn (indices-cache bt)))
	
(defmethod get-index ((bt bdb-indexed-btree) index-name)
  (gethash index-name (indices-cache bt)))

(defmethod remove-index ((bt bdb-indexed-btree) index-name)
  (remhash index-name (indices-cache bt))
  (let ((indices (indices bt)))
    (remhash index-name indices)
    (setf (indices bt) indices)))

(defmethod (setf get-value) (value key (bt bdb-indexed-btree))
  "Set a key / value pair, and update secondary indices."
  (let ((sc (get-con bt)))
    (let ((indices (indices-cache bt)))
      (with-buffer-streams (key-buf value-buf secondary-buf)
	(buffer-write-oid (oid bt) key-buf)
	(serialize key key-buf sc)
	(serialize value value-buf sc)
	(ensure-transaction (:store-controller sc)
	  (db-put-buffered (controller-btrees sc)
			   key-buf value-buf
			   :transaction (my-current-transaction sc))
	  (loop for index being the hash-value of indices
	     do
	     (multiple-value-bind (index? secondary-key)
		 (funcall (key-fn index) index key value)
	       (when index?
		 ;; Manually write value into secondary index
		 (buffer-write-oid (oid index) secondary-buf)
		 (serialize secondary-key secondary-buf sc)
		 ;; should silently do nothing if the key/value already
		 ;; exists
		 (db-put-buffered (controller-indices sc)
				  secondary-buf key-buf
				  :transaction (my-current-transaction sc))
		 (reset-buffer-stream secondary-buf))))
	  value))))
  )

(defmethod remove-kv (key (bt bdb-indexed-btree))
  "Remove a key / value pair, and update secondary indices."
  (let ((sc (get-con bt)))
      (with-buffer-streams (key-buf secondary-buf)
	(buffer-write-oid (oid bt) key-buf)
	(serialize key key-buf sc)
	(ensure-transaction (:store-controller sc)
	  (let ((value (get-value key bt)))
	    (when value
	      (let ((indices (indices-cache bt)))
		(loop 
		   for index being the hash-value of indices
		   do
		   (multiple-value-bind (index? secondary-key)
		       (funcall (key-fn index) index key value)
		     (when index?
		       (buffer-write-oid (oid index) secondary-buf)
		       (serialize secondary-key secondary-buf sc)
		       ;; need to remove kv pairs with a cursor! --
		       ;; this is a C performance hack
		       (db-delete-kv-buffered 
			(controller-indices (get-con bt))
			secondary-buf key-buf
			:transaction (my-current-transaction sc))
		       (reset-buffer-stream secondary-buf))))
		(db-delete-buffered (controller-btrees (get-con bt)) 
				    key-buf
				    :transaction (my-current-transaction sc)))))))))

;; This also needs to build the correct kind of index, and 
;; be the correct kind of btree...

(defclass bdb-btree-index (btree-index bdb-btree)
  ()
  (:metaclass persistent-metaclass)
  (:documentation "A BDB-based BTree supports secondary indices."))

(defmethod get-value (key (bt bdb-btree-index))
  "Get the value in the primary DB from a secondary key."
  (let ((sc (get-con bt)))
    (with-buffer-streams (key-buf value-buf)
      (buffer-write-oid (oid bt) key-buf)
      (serialize key key-buf sc)
      (let ((buf (db-get-key-buffered 
		  (controller-indices-assoc sc)
		  key-buf value-buf
		  :transaction (my-current-transaction sc))))
	(if buf (values (deserialize buf sc) T)
	    (values nil nil))))))

(defmethod get-primary-key (key (bt btree-index))
  (let ((sc (get-con bt)))
    (with-buffer-streams (key-buf value-buf)
      (buffer-write-oid (oid bt) key-buf)
      (serialize key key-buf sc)
      (let ((buf (db-get-key-buffered 
		  (controller-indices sc)
		  key-buf value-buf
		  :transaction (my-current-transaction sc))))
	(if buf 
	    (let ((oid (buffer-read-oid buf)))
	      (values (deserialize buf sc) oid))
	    (values nil nil))))))

(defclass bdb-cursor (cursor)
  ((handle :accessor cursor-handle :initarg :handle))
  (:documentation "A cursor for traversing (primary) BDB-BTrees."))

(defmethod make-cursor ((bt bdb-btree))
  "Make a cursor from a btree."
  (let ((sc (get-con bt)))
    (make-instance 'bdb-cursor 
		   :btree bt
		   :handle (db-cursor (controller-btrees sc)
				      :transaction (my-current-transaction sc))
		   :oid (oid bt))))

(defmethod cursor-close ((cursor bdb-cursor))
  (db-cursor-close (cursor-handle cursor))
  (setf (cursor-initialized-p cursor) nil))

(defmethod cursor-duplicate ((cursor bdb-cursor))
  (make-instance (type-of cursor)
		 :initialized-p (cursor-initialized-p cursor)
		 :oid (cursor-oid cursor)
		 :handle (db-cursor-duplicate 
			  (cursor-handle cursor) 
			  :position (cursor-initialized-p cursor))))

(defmethod cursor-current ((cursor bdb-cursor))
  (when (cursor-initialized-p cursor)
    (let ((sc (get-con (cursor-btree cursor))))
      (with-buffer-streams (key-buf value-buf)
	(multiple-value-bind (key val)
	    (db-cursor-move-buffered (cursor-handle cursor) key-buf value-buf
				     :current t)
	  (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
	      (progn (setf (cursor-initialized-p cursor) t)
		     (values t (deserialize key sc)
			     (deserialize val sc)))
	      (setf (cursor-initialized-p cursor) nil)))))))

(defmethod cursor-first ((cursor bdb-cursor))
  (let ((sc (get-con (cursor-btree cursor))))
    (with-buffer-streams (key-buf value-buf)
      (buffer-write-oid (cursor-oid cursor) key-buf)
      (multiple-value-bind (key val)
	  (db-cursor-set-buffered (cursor-handle cursor) 
				  key-buf value-buf :set-range t)
	(if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
	    (progn (setf (cursor-initialized-p cursor) t)
		   (values t 
			   (deserialize key sc)
			   (deserialize val sc)))
	    (setf (cursor-initialized-p cursor) nil))))))
		 
(defmethod cursor-last ((cursor bdb-cursor))
  "A fast cursor last, but a bit 'hackish' by exploiting oid ordering"
  (let ((sc (get-con (cursor-btree cursor))))
  (with-buffer-streams (key-buf value-buf)
    ;; Go to the first element of the next btree
    (buffer-write-oid (+ (cursor-oid cursor) 1) key-buf)
    (if (db-cursor-set-buffered (cursor-handle cursor)
				key-buf value-buf :set-range t)
	(progn (reset-buffer-stream key-buf)
	       (reset-buffer-stream value-buf)
	       (multiple-value-bind (key val)
		   (db-cursor-move-buffered (cursor-handle cursor) 
					    key-buf value-buf :prev t)
		 (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
		     (progn
		       (setf (cursor-initialized-p cursor) t)
		       (values t (deserialize key sc)
			         (deserialize val sc)))
		     (setf (cursor-initialized-p cursor) nil))))
	(multiple-value-bind (key val)
	    (db-cursor-move-buffered (cursor-handle cursor) key-buf
				     value-buf :last t)
	  (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
	      (progn
		(setf (cursor-initialized-p cursor) t)
		(values t (deserialize key sc)
			(deserialize val sc )))
	      (setf (cursor-initialized-p cursor) nil)))))))

(defmethod cursor-next ((cursor bdb-cursor))
  (if (cursor-initialized-p cursor)
      (let ((sc (get-con (cursor-btree cursor))))
	(with-buffer-streams (key-buf value-buf)
	  (multiple-value-bind (key val)
	      (db-cursor-move-buffered (cursor-handle cursor) 
				       key-buf value-buf :next t)
	    (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
		(values t (deserialize key sc)
			(deserialize val sc))
		(setf (cursor-initialized-p cursor) nil)))))
      (cursor-first cursor)))
	  
(defmethod cursor-prev ((cursor bdb-cursor))
  (if (cursor-initialized-p cursor)
      (let ((sc (get-con (cursor-btree cursor))))
	(with-buffer-streams (key-buf value-buf)
	  (multiple-value-bind (key val)
	      (db-cursor-move-buffered (cursor-handle cursor)
				       key-buf value-buf :prev t)
	    (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
		(values t (deserialize key sc)
			(deserialize val sc))
		(setf (cursor-initialized-p cursor) nil))))
	(cursor-last cursor))))
	  
(defmethod cursor-set ((cursor bdb-cursor) key)
  (let ((sc (get-con (cursor-btree cursor))))
  (with-buffer-streams (key-buf value-buf)
    (buffer-write-oid (cursor-oid cursor) key-buf)
    (serialize key key-buf sc)
    (multiple-value-bind (k val)
	(db-cursor-set-buffered (cursor-handle cursor)
				key-buf value-buf :set t)
      (if k
	  (progn
	    (setf (cursor-initialized-p cursor) t)
	    (values t key (deserialize val sc)))
	  (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-set-range ((cursor bdb-cursor) key)
  (let ((sc (get-con (cursor-btree cursor))))
  (with-buffer-streams (key-buf value-buf)
    (buffer-write-oid (cursor-oid cursor) key-buf)
    (serialize key key-buf sc)
    (multiple-value-bind (k val)
	(db-cursor-set-buffered (cursor-handle cursor)
				key-buf value-buf :set-range t)
      (if (and k (= (buffer-read-oid k) (cursor-oid cursor)))
	  (progn (setf (cursor-initialized-p cursor) t)
		 (values t (deserialize k sc)
			 (deserialize val sc)))
	  (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-get-both ((cursor bdb-cursor) key value)
  (let ((sc (get-con (cursor-btree cursor))))
  (with-buffer-streams (key-buf value-buf)
    (buffer-write-oid (cursor-oid cursor) key-buf)
    (serialize key key-buf sc)
    (serialize value value-buf sc)
    (multiple-value-bind (k v)
	(db-cursor-get-both-buffered (cursor-handle cursor)
				     key-buf value-buf :get-both t)
      (declare (ignore v))
      (if k
	  (progn (setf (cursor-initialized-p cursor) t)
		 (values t key value))
	  (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-get-both-range ((cursor bdb-cursor) key value)
  (let ((sc (get-con (cursor-btree cursor))))
  (with-buffer-streams (key-buf value-buf)
    (buffer-write-oid (cursor-oid cursor) key-buf)
    (serialize key key-buf sc)
    (serialize value value-buf sc)
    (multiple-value-bind (k v)
	(db-cursor-get-both-buffered (cursor-handle cursor)
				     key-buf value-buf :get-both-range t)
      (if k
	  (progn (setf (cursor-initialized-p cursor) t)
		 (values t key (deserialize v sc)))
	  (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-delete ((cursor bdb-cursor))
  (if (cursor-initialized-p cursor)
      (with-buffer-streams (key-buf value-buf)
	(multiple-value-bind (key val)
	    (db-cursor-move-buffered (cursor-handle cursor) key-buf value-buf
				     :current t)
	  (declare (ignore val))
	  (when (and key (= (buffer-read-oid key) (cursor-oid cursor)))
	    ;; in case of a secondary index this should delete everything
	    ;; as specified by the BDB docs.
	    (remove-kv (deserialize key (get-con (cursor-btree cursor)))
		       (cursor-btree cursor)))
	  (setf (cursor-initialized-p cursor) nil)))
      (error "Can't delete with uninitialized cursor!")))

(defmethod cursor-put ((cursor bdb-cursor) value &key (key nil key-specified-p))
  "Put by cursor.  Not particularly useful since primaries
don't support duplicates.  Currently doesn't properly move
the cursor."
  (if key-specified-p
      (setf (get-value key (cursor-btree cursor)) value)
      (if (cursor-initialized-p cursor)
	  (with-buffer-streams (key-buf value-buf)
	    (multiple-value-bind (k v)
		(db-cursor-move-buffered (cursor-handle cursor) key-buf 
					 value-buf :current t)
	      (declare (ignore v))
	      (if (and k (= (buffer-read-oid k) (cursor-oid cursor)))
		  (setf (get-value 
			 (deserialize k (get-con (cursor-btree cursor))) 
			 (cursor-btree cursor)) 
			value)
		  (setf (cursor-initialized-p cursor) nil))))
	  (error "Can't put with uninitialized cursor!"))))

;; Secondary cursors

(defclass bdb-secondary-cursor (bdb-cursor) ()
  (:documentation "Cursor for traversing bdb secondary indices."))

(defmethod make-cursor ((bt bdb-btree-index))
  "Make a secondary-cursor from a secondary index."
  (let ((sc (get-con bt)))
    (make-instance 'bdb-secondary-cursor 
		   :btree bt
		   :handle (db-cursor (controller-indices-assoc sc)
				      :transaction (my-current-transaction sc))
		   :oid (oid bt))))

(defmethod cursor-pcurrent ((cursor bdb-secondary-cursor))
  (when (cursor-initialized-p cursor)
    (with-buffer-streams (key-buf pkey-buf value-buf)
      (multiple-value-bind (key pkey val)
	  (db-cursor-pmove-buffered (cursor-handle cursor) 
				    key-buf pkey-buf value-buf
				    :current t)
	(if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
	    (progn (setf (cursor-initialized-p cursor) t)
		   (let ((sc (get-con (cursor-btree cursor))))
		     (values t 
			     (deserialize key sc)
			     (deserialize val sc)
			     (progn (buffer-read-oid pkey) (deserialize pkey sc)))))
	    (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-pfirst ((cursor bdb-secondary-cursor))
  (with-buffer-streams (key-buf pkey-buf value-buf)
    (buffer-write-oid (cursor-oid cursor) key-buf)
    (multiple-value-bind (key pkey val)
	(db-cursor-pset-buffered (cursor-handle cursor) 
				 key-buf pkey-buf value-buf :set-range t)
      (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
	  (progn (setf (cursor-initialized-p cursor) t)
		 (let ((sc (get-con (cursor-btree cursor))))
		 (values t 
			 (deserialize key sc)
			 (deserialize val sc)
			 (progn (buffer-read-oid pkey) (deserialize pkey sc)))))
	  (setf (cursor-initialized-p cursor) nil)))))
		 
;;A bit of a hack.....
(defmethod cursor-plast ((cursor bdb-secondary-cursor))
  (let ((sc (get-con (cursor-btree cursor))))
  (with-buffer-streams (key-buf pkey-buf value-buf)
    (buffer-write-oid (+ (cursor-oid cursor) 1) key-buf)
    (if (db-cursor-set-buffered (cursor-handle cursor) 
				key-buf value-buf :set-range t)    
	(progn (reset-buffer-stream key-buf)
	       (reset-buffer-stream value-buf)
	       (multiple-value-bind (key pkey val)
		   (db-cursor-pmove-buffered (cursor-handle cursor) key-buf 
					     pkey-buf value-buf :prev t)
		 (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
		     (progn
		       (setf (cursor-initialized-p cursor) t)
		       (values t 
			       (deserialize key sc)
			       (deserialize val sc)
			       (progn (buffer-read-oid pkey) 
				      (deserialize pkey sc))))
		     (setf (cursor-initialized-p cursor) nil))))
	(multiple-value-bind (key pkey val)
	    (db-cursor-pmove-buffered (cursor-handle cursor) key-buf
				      pkey-buf value-buf :last t)
	  (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
	      (progn
		(setf (cursor-initialized-p cursor) t)
		(values t (deserialize key sc)
			(deserialize val sc)
			(progn (buffer-read-oid pkey) (deserialize pkey sc))))
	      (setf (cursor-initialized-p cursor) nil)))))))

(defmethod cursor-pnext ((cursor bdb-secondary-cursor))
  (if (cursor-initialized-p cursor)
      (with-buffer-streams (key-buf pkey-buf value-buf)
	(multiple-value-bind (key pkey val)
	    (db-cursor-pmove-buffered (cursor-handle cursor) 
				     key-buf pkey-buf value-buf :next t)
	  (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
	      (let ((sc (get-con (cursor-btree cursor))))
		(values t (deserialize key sc)
			(deserialize val sc)
			(progn (buffer-read-oid pkey) (deserialize pkey sc))))
	      (setf (cursor-initialized-p cursor) nil))))
      (cursor-pfirst cursor)))
	  
(defmethod cursor-pprev ((cursor bdb-secondary-cursor))
  (if (cursor-initialized-p cursor)
      (with-buffer-streams (key-buf pkey-buf value-buf)
	(multiple-value-bind (key pkey val)
	    (db-cursor-pmove-buffered (cursor-handle cursor)
				      key-buf pkey-buf value-buf :prev t)
	  (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
	      (let ((sc (get-con (cursor-btree cursor))))
		(values t (deserialize key sc)
			(deserialize val sc)
			(progn (buffer-read-oid pkey) (deserialize pkey sc))))
	      (setf (cursor-initialized-p cursor) nil))))
      (cursor-plast cursor)))

(defmethod cursor-pset ((cursor bdb-secondary-cursor) key)
  (let ((sc (get-con (cursor-btree cursor))))
    (with-buffer-streams (key-buf pkey-buf value-buf)
      (buffer-write-oid (cursor-oid cursor) key-buf)
      (serialize key key-buf sc)
      (multiple-value-bind (k pkey val)
	  (db-cursor-pset-buffered (cursor-handle cursor)
				   key-buf pkey-buf value-buf :set t)
	(if k
	    (progn
	      (setf (cursor-initialized-p cursor) t)
	      (values t key (deserialize val sc)
		      (progn (buffer-read-oid pkey) 
			     (deserialize pkey sc))))
	    (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-pset-range ((cursor bdb-secondary-cursor) key)
  (let ((sc (get-con (cursor-btree cursor))))
    (with-buffer-streams (key-buf pkey-buf value-buf)
      (buffer-write-oid (cursor-oid cursor) key-buf)
      (serialize key key-buf sc)
      (multiple-value-bind (k pkey val)
	  (db-cursor-pset-buffered (cursor-handle cursor)
				   key-buf pkey-buf value-buf :set-range t)
	(if (and k (= (buffer-read-oid k) (cursor-oid cursor)))
	    (progn (setf (cursor-initialized-p cursor) t)
		   (values t (deserialize k sc)
			   (deserialize val sc)
			   (progn (buffer-read-oid pkey) (deserialize pkey sc))))
	    (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-pget-both ((cursor bdb-secondary-cursor) key pkey)
  (with-buffer-streams (key-buf pkey-buf value-buf)
    (let ((primary-oid (oid (primary (cursor-btree cursor))))
	  (sc (get-con (cursor-btree cursor))))
      (buffer-write-oid (cursor-oid cursor) key-buf)
      (serialize key key-buf sc)
      (buffer-write-oid primary-oid pkey-buf)
      (serialize pkey pkey-buf sc)
      (multiple-value-bind (k p val)
	  (db-cursor-pget-both-buffered (cursor-handle cursor)
					key-buf pkey-buf value-buf :get-both t)
	(declare (ignore p))
	(if k
	    (progn (setf (cursor-initialized-p cursor) t)
		   (values t key (deserialize val sc) pkey))
	    (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-pget-both-range ((cursor bdb-secondary-cursor) key pkey)
  (with-buffer-streams (key-buf pkey-buf value-buf)
    (let ((primary-oid (oid (primary (cursor-btree cursor))))
	  (sc (get-con (cursor-btree cursor))))
      (buffer-write-oid (cursor-oid cursor) key-buf)
      (serialize key key-buf sc)
      (buffer-write-oid primary-oid pkey-buf)
      (serialize pkey pkey-buf sc)
      (multiple-value-bind (k p val)
	  (db-cursor-pget-both-buffered (cursor-handle cursor) key-buf 
					pkey-buf value-buf :get-both-range t)
	(if k
	    (progn (setf (cursor-initialized-p cursor) t)
		   (values t key (deserialize val sc)
			   (progn (buffer-read-oid p) (deserialize p sc))))
	    (setf (cursor-initialized-p cursor) nil))))))

(defmethod cursor-delete ((cursor bdb-secondary-cursor))
  "Delete by cursor: deletes ALL secondary indices."
  (if (cursor-initialized-p cursor)
      (with-buffer-streams (key-buf pkey-buf value-buf)
	(multiple-value-bind (key pkey val)
	    (db-cursor-pmove-buffered (cursor-handle cursor) key-buf pkey-buf
				      value-buf :current t)
	  (declare (ignore val))
	  (when (and key (= (buffer-read-oid key) (cursor-oid cursor))
		     (= (buffer-read-oid pkey) (oid (primary 
						     (cursor-btree cursor)))))
	    (remove-kv (deserialize pkey (get-con (cursor-btree cursor)))
		       (primary (cursor-btree cursor))))
	  (setf (cursor-initialized-p cursor) nil)))
      (error "Can't delete with uninitialized cursor!")))

(defmethod cursor-get-both ((cursor bdb-secondary-cursor) key value)
  "cursor-get-both not implemented for secondary indices.
Use cursor-pget-both."
  (declare (ignore key value)
         (ignorable cursor))
  (error "cursor-get-both not implemented on secondary
indices.  Use cursor-pget-both."))

(defmethod cursor-get-both-range ((cursor bdb-secondary-cursor) key value)
  "cursor-get-both-range not implemented for secondary indices.
Use cursor-pget-both-range."
  (declare (ignore key value)
         (ignorable cursor))
  (error "cursor-get-both-range not implemented on secondary indices.  Use cursor-pget-both-range."))

(defmethod cursor-put ((cursor bdb-secondary-cursor) value &rest rest)
  "Puts are forbidden on secondary indices.  Try adding to the primary."
  (declare (ignore rest value)
         (ignorable cursor))
  (error "Puts are forbidden on secondary indices.  Try adding to the primary."))

(defmethod cursor-next-dup ((cursor bdb-secondary-cursor))
  (when (cursor-initialized-p cursor)
    (with-buffer-streams (key-buf value-buf)
      (multiple-value-bind (key val)
	  (db-cursor-move-buffered (cursor-handle cursor)
				   key-buf value-buf :next-dup t)
	(if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
	    (values t (deserialize key (get-con (cursor-btree cursor))) 
		    (deserialize val (get-con (cursor-btree cursor))))
	    (setf (cursor-initialized-p cursor) nil))))))
	  
(defmethod cursor-next-nodup ((cursor bdb-secondary-cursor))
  (if (cursor-initialized-p cursor)
      (with-buffer-streams (key-buf value-buf)
	(multiple-value-bind (key val)
	    (db-cursor-move-buffered (cursor-handle cursor)
				     key-buf value-buf :next-nodup t)
	  (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
	      (values t (deserialize key (get-con (cursor-btree cursor))) 
		      (deserialize val (get-con (cursor-btree cursor))))
	      (setf (cursor-initialized-p cursor) nil))))
      (cursor-first cursor)))	  

(defmethod cursor-prev-nodup ((cursor bdb-secondary-cursor))
  (if (cursor-initialized-p cursor)
      (with-buffer-streams (key-buf value-buf)
	(multiple-value-bind (key val)
	    (db-cursor-move-buffered (cursor-handle cursor)
				     key-buf value-buf :prev-nodup t)
	  (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
	      (values t (deserialize key (get-con (cursor-btree cursor))) 
		      (deserialize val (get-con (cursor-btree cursor))))
	      (setf (cursor-initialized-p cursor) nil))))
      (cursor-last cursor)))

(defmethod cursor-pnext-dup ((cursor bdb-secondary-cursor))
  (when (cursor-initialized-p cursor)
    (with-buffer-streams (key-buf pkey-buf value-buf)
      (multiple-value-bind (key pkey val)
	  (db-cursor-pmove-buffered (cursor-handle cursor)
				    key-buf pkey-buf value-buf :next-dup t)
	(if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
	    (values t (deserialize key (get-con (cursor-btree cursor))) 
		    (deserialize val (get-con (cursor-btree cursor)))
		    (progn (buffer-read-oid pkey) (deserialize pkey (get-con (cursor-btree cursor)))))
	    (setf (cursor-initialized-p cursor) nil))))))
	  
(defmethod cursor-pnext-nodup ((cursor bdb-secondary-cursor))
  (if (cursor-initialized-p cursor)
      (with-buffer-streams (key-buf pkey-buf value-buf)
	(multiple-value-bind (key pkey val)
	    (db-cursor-pmove-buffered (cursor-handle cursor) key-buf
				      pkey-buf value-buf :next-nodup t)
	  (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
	      (values t (deserialize key (get-con (cursor-btree cursor))) 
		      (deserialize val (get-con (cursor-btree cursor)))
		      (progn (buffer-read-oid pkey) (deserialize pkey (get-con (cursor-btree cursor)))))
	      (setf (cursor-initialized-p cursor) nil))))
      (cursor-pfirst cursor)))

(defmethod cursor-pprev-nodup ((cursor bdb-secondary-cursor))
  (if (cursor-initialized-p cursor)
      (with-buffer-streams (key-buf pkey-buf value-buf)
	(multiple-value-bind (key pkey val)
	    (db-cursor-pmove-buffered (cursor-handle cursor) key-buf
				      pkey-buf value-buf :prev-nodup t)
	  (if (and key (= (buffer-read-oid key) (cursor-oid cursor)))
	      (values t (deserialize key (get-con (cursor-btree cursor)))
		      (deserialize val (get-con (cursor-btree cursor)))
		      (progn (buffer-read-oid pkey)
			     (deserialize pkey (get-con (cursor-btree cursor)))))
	      (setf (cursor-initialized-p cursor) nil))))
      (cursor-plast cursor)))

