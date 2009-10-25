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

(in-package "ELEPHANT")

#-elephant-without-optimize
(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (safety 1) (space 1))))

;;; collection types
;;; we're slot-less
(defclass persistent-collection (persistent) ()
  (:documentation "Abstract superclass of all collection types."))

;; I don't like having to put this here, as this is only used by
;; the extending class indexed-btree.  But I can't figure out 
;; how to make the :transient flag work on that class without 
;; creating a circularity in the class presidence list...

;; 
;; Our workhorse BTree
;;

(defun make-btree (&optional (sc *store-controller*))
  "Constructs a new BTree instance for use by the user.  Each backend
   returns its own internal type as appropriate and ensures that the 
   btree is associated with the store-controller that created it."
  (build-btree sc))

(defgeneric build-btree (sc)
  (:documentation 
   "Construct a btree of the appropriate type corresponding to this store-controller."))

(defclass btree (persistent-collection) ()
  (:documentation 
   "A hash-table like interface to a BTree, which stores things 
    in a semi-ordered fashion."))

(defgeneric get-value (key bt)
  (:documentation "Get a value from a Btree."))

(defgeneric (setf get-value) (value key bt)
  (:documentation "Put a key / value pair into a BTree."))

(defgeneric remove-kv (key bt)
  (:documentation "Remove a key / value pair from a BTree."))

(defgeneric existsp (key bt)
  (:documentation "Test existence of a key / value pair in a BTree"))

(defmethod optimize-layout ((bt t) &key &allow-other-keys)
  t)

(defgeneric drop-btree (bt)
  (:documentation "Delete all key-value pairs from the btree and
   render it an invalid object in the data store"))


;;
;; Btrees that support secondary indices
;;

(defun make-indexed-btree (&optional (sc *store-controller*))
  "Constructs a new indexed BTree instance for use by the user.
   Each backend returns its own internal type as appropriate and
   ensures that the btree is associated with the store-controller
   that created it."
  (build-indexed-btree sc))

(defgeneric build-indexed-btree (sc)
  (:documentation 
   "Construct a btree of the appropriate type corresponding to this store-controller."))

(defclass indexed-btree (btree) ()
  (:documentation "A BTree which supports secondary indices."))

(defgeneric add-index (bt &key index-name key-form populate)
  (:documentation 
   "Add a secondary index.  The indices are stored in an eq
hash-table, so the index-name should be a symbol.  key-form
should be a symbol naming a function, or a list which
defines a lambda -- actual functions aren't supported.  The
function should take 3 arguments: the secondary DB, primary
key and value, and return two values: a boolean indicating
whether to index this key / value, and the secondary key if
so.  If populate = t it will fill in secondary keys for
existing primary entries (may be expensive!)"))

(defgeneric get-index (bt index-name)
  (:documentation "Get a named index."))

(defgeneric remove-index (bt index-name)
  (:documentation "Remove a named index."))

(defgeneric map-indices (fn bt)
  (:documentation "Calls a two input function with the name and 
   btree-index object of all secondary indices in the btree"))

;;
;; Secondary Indices
;;

(defgeneric build-btree-index (sc &key primary key-form)
  (:documentation 
   "Construct a btree of the appropriate type corresponding to this store-controller."))

(defclass btree-index (btree)
  ((primary :type indexed-btree :reader primary :initarg :primary)
   (key-form :reader key-form :initarg :key-form :initform nil)
   (key-fn :type function :accessor key-fn :transient t))
  (:metaclass persistent-metaclass)
  (:documentation "Secondary index to an indexed-btree."))

(defmethod shared-initialize :after ((instance btree-index) slot-names
				     &rest rest)
  (declare (ignore slot-names rest))
  (let ((key-form (key-form instance)))
    (if (and (symbolp key-form) (fboundp key-form))
	(setf (key-fn instance) (fdefinition key-form))
	(setf (key-fn instance) (compile nil key-form)))))

(defgeneric get-primary-key (key bt)
  (:documentation "Get the primary key from a secondary key."))


;;
;; Some generic defaults for secondary indices 
;; (shouldn't implement in backend)
;;

(defmethod (setf get-value) (value key (bt btree-index))
  "Puts are not allowed on secondary indices.  Try adding to
the primary."
  (declare (ignore value key)
	   (ignorable bt))
  (error "Puts are forbidden on secondary indices.  Try adding to the primary."))

(defmethod remove-kv (key (bt btree-index))
  "Remove a key / value from the PRIMARY by a secondary
lookup, updating ALL other secondary indices."
  (remove-kv (get-primary-key key bt) (primary bt)))


;;
;; Cursors for all btree types
;;

(defclass cursor ()
  ((oid :accessor cursor-oid :type fixnum :initarg :oid)
   (initialized-p :accessor cursor-initialized-p
		  :type boolean :initform nil :initarg :initialized-p
		  :documentation "Predicate indicating whether
the btree in question is initialized or not.  Initialized means
that the cursor has a legitimate position, not that any
initialization action has been taken.  The implementors of this
abstract class should make sure that happens under the
sheets...  Cursors are initialized when you invoke an operation
that sets them to something (such as cursor-first), and are
uninitialized if you move them in such a way that they no longer
have a legimtimate value.")
   (btree :accessor cursor-btree :initarg :btree))
  (:documentation "A cursor for traversing (primary) BTrees."))

(defgeneric make-cursor (bt)
  (:documentation "Construct a cursor for traversing BTrees."))

(defgeneric make-simple-cursor (bt)
  (:documentation "Allow users to walk secondary indices and only 
                   get back primary keys rather than associated 
                   primary values"))

(defgeneric cursor-close (cursor)
  (:documentation 
   "Close the cursor.  Make sure to close cursors before the
enclosing transaction is closed!"))

(defgeneric cursor-duplicate (cursor)
  (:documentation "Duplicate a cursor."))

(defgeneric cursor-current (cursor)
  (:documentation 
   "Get the key / value at the cursor position.  Returns
has-pair key value, where has-pair is a boolean indicating
there was a pair."))

(defgeneric cursor-first (cursor)
  (:documentation 
   "Move the cursor to the beginning of the BTree, returning
has-pair key value."))

(defgeneric cursor-last (cursor)
  (:documentation 
   "Move the cursor to the end of the BTree, returning
has-pair key value."))

(defgeneric cursor-next (cursor)   
  (:documentation 
   "Advance the cursor, returning has-pair key value."))

(defgeneric cursor-prev (cursor)
  (:documentation 
   "Move the cursor back, returning has-pair key value."))

(defgeneric cursor-set (cursor key)
  (:documentation 
   "Move the cursor to a particular key, returning has-pair
key value."))

(defgeneric cursor-set-range (cursor key) 
  (:documentation 
   "Move the cursor to the first key-value pair with key
greater or equal to the key argument, according to the lisp
sorter.  Returns has-pair key value."))

(defgeneric cursor-get-both (cursor key value)
  (:documentation 
   "Moves the cursor to a particular key / value pair,
returning has-pair key value."))

(defgeneric cursor-get-both-range (cursor key value)
  (:documentation 
   "Moves the cursor to the first key / value pair with key
equal to the key argument and value greater or equal to the
value argument.  Not really useful for us since primaries
don't have duplicates.  Returns has-pair key value."))

(defgeneric cursor-delete (cursor)
  (:documentation 
   "Delete by cursor.  The cursor is at an invalid position,
and uninitialized, after a successful delete."))

(defgeneric cursor-put (cursor value &key key)
  (:documentation 
  "Overwrite value at current cursor location.  Currently does
  not properly move the cursor."))

(defclass secondary-cursor (cursor) ()
  (:documentation "Cursor for traversing secondary indices."))

(defgeneric cursor-pcurrent (cursor)
  (:documentation 
   "Returns has-tuple / secondary key / value / primary key
at the current position."))

(defgeneric cursor-pfirst (cursor)
  (:documentation 
   "Moves the key to the beginning of the secondary index.
Returns has-tuple / secondary key / value / primary key."))

(defgeneric cursor-plast (cursor)
  (:documentation 
   "Moves the key to the end of the secondary index.  Returns
has-tuple / secondary key / value / primary key."))

(defgeneric cursor-pnext (cursor)
  (:documentation 
   "Advances the cursor.  Returns has-tuple / secondary key /
value / primary key."))

(defgeneric cursor-pprev (cursor)
  (:documentation 
   "Moves the cursor back.  Returns has-tuple / secondary key
/ value / primary key."))

(defgeneric cursor-pset (cursor key)
  (:documentation 
  "Moves the cursor to a particular key.  Returns has-tuple
/ secondary key / value / primary key."))

(defgeneric cursor-pset-range (cursor key)
  (:documentation 
   "Move the cursor to the first key-value pair with key
greater or equal to the key argument, according to the lisp
sorter.  Returns has-pair secondary key value primary key."))

(defgeneric cursor-pget-both (cursor key value)
  (:documentation 
   "Moves the cursor to a particular secondary key / primary
key pair.  Returns has-tuple / secondary key / value /
primary key."))

(defgeneric cursor-pget-both-range (cursor key value)
  (:documentation 
   "Moves the cursor to a the first secondary key / primary
key pair, with secondary key equal to the key argument, and
primary key greater or equal to the pkey argument.  Returns
has-tuple / secondary key / value / primary key."))


(defgeneric cursor-next-dup (cursor)
  (:documentation 
   "Move to the next duplicate element (with the same key.)
Returns has-pair key value."))

(defgeneric cursor-next-nodup (cursor)
  (:documentation 
   "Move to the next non-duplicate element (with different
key.)  Returns has-pair key value."))

(defgeneric cursor-pnext-dup (cursor)
  (:documentation 
   "Move to the next duplicate element (with the same key.)
Returns has-tuple / secondary key / value / primary key."))

(defgeneric cursor-pnext-nodup (cursor)
  (:documentation 
   "Move to the next non-duplicate element (with different
key.)  Returns has-tuple / secondary key / value / primary
key."))


(defgeneric cursor-prev-dup (cursor)
  (:documentation 
   "Move to the previous duplicate element (with the same key.)
Returns has-pair key value."))

;; Default implementation.  Plan is to update both backends when BDB 4.6 comes out
(defmethod cursor-prev-dup ((cur cursor))
  (when (cursor-initialized-p cur)
    (multiple-value-bind (exists? skey-cur)
	(cursor-current cur)
      (declare (ignore exists?))
      (multiple-value-bind (exists? skey value)
	  (cursor-prev cur)
	(if (lisp-compare-equal skey-cur skey)
	    (values exists? skey value)
	    (setf (cursor-initialized-p cur) nil))))))

(defgeneric cursor-prev-nodup (cursor)
  (:documentation 
   "Move to the previous non-duplicate element (with
different key.)  Returns has-pair key value."))

(defgeneric cursor-pprev-dup (cursor)
  (:documentation 
   "Move to the previous duplicate element (with the same key.)
Returns has-tuple / secondary key / value / primary key."))

;; Default implementation.  Plan is to update both backends when BDB 4.6 comes out
(defmethod cursor-pprev-dup ((cur cursor))
  (when (cursor-initialized-p cur)
    (multiple-value-bind (exists? skey-cur)
	(cursor-current cur)
      (declare (ignore exists?))
      (multiple-value-bind (exists? skey value pkey)
	  (cursor-pprev cur)
	(if (lisp-compare-equal skey-cur skey)
	    (values exists? skey value pkey)
	    (setf (cursor-initialized-p cur) nil))))))

(defgeneric cursor-pprev-nodup (cursor)
  (:documentation 
   "Move to the previous non-duplicate element (with
different key.)  Returns has-tuple / secondary key / value /
primary key."))

(defmacro with-btree-cursor ((var bt) &body body)
  "Macro which opens a named cursor on a BTree (primary or
not), evaluates the forms, then closes the cursor."
  `(let ((,var (make-cursor ,bt)))
     (unwind-protect
	  (progn ,@body)
       (cursor-close ,var))))

(defmethod drop-btree ((bt btree))
  (ensure-transaction (:store-controller *store-controller*)
    (with-btree-cursor (cur bt)
      (loop for (exists? key) = (multiple-value-list (cursor-first cur))
	 then (multiple-value-list (cursor-next cur))
	 while exists?
	 do (remove-kv key bt)))))

;; =======================================
;;   Generic Mapping Functions
;; =======================================

(defun lisp-compare<= (a b)
  (etypecase a
    (number (<= a b))
    (string (string<= a b))
    (persistent (<= (oid a) (oid b)))
    (symbol (string<= (symbol-name a) (symbol-name b)))))

(defun lisp-compare-equal (a b)
  (equal a b))

(defgeneric map-btree (fn btree &rest args &key start end value from-end collect &allow-other-keys)
  (:documentation   "Map btree maps over a btree from the value start to the value of end.
   If values are not provided, then it maps over all values.  BTrees 
   do not have duplicates, but map-btree can also be used with indices
   in the case where you don't want access to the primary key so we 
   require a value argument as well for mapping duplicate value sets.
   The collect keyword will accumulate the results from
   each call of fn in a fresh list and return that list in the 
   same order the calls were made (first to last)."))

;; NOTE: the use of nil for the last element in a btree only works because the C comparison
;; function orders by type tag and nil is the highest valued type tag so nils are the last
;; possible element in a btree ordered by value.

(defmethod map-btree (fn (btree btree) &rest args &key start end (value nil value-set-p) 
		      from-end collect &allow-other-keys)
  (let ((end (if value-set-p value end))
	(results nil))
    (ensure-transaction (:store-controller (get-con btree) :degree-2 *map-using-degree2*)
      (with-btree-cursor (curs btree)
	(flet ((continue-p (key)
		 ;; Do we go to the next value?
		 (or (if from-end (null start) (null end))
		     (if from-end 
			 (or (not (lisp-compare<= key start))
			     (lisp-compare-equal key start))
			 (lisp-compare<= key end))))
		 (collector (k v)
		   (push (funcall fn k v) results)))
	  (let ((fn (if collect #'collector fn)))
	    (declare (dynamic-extent (function continue-p) (function collector)))
	    (multiple-value-bind (exists? key value)
		(cond (value-set-p
		       (cursor-set curs value))
		      ((and (not from-end) (null start))
		       (cursor-first curs))
		      ((and from-end (null end))
		       (cursor-last curs))
		      (t (if from-end
			     (cursor-set-range curs end)
			     (cursor-set-range curs start))))
	      (declare (dynamic-extent exists? k v))
	      (if exists?
		  (funcall fn key value)
		  (return-from map-btree nil))
	      (loop
		 (multiple-value-bind (exists? k v)
		     (if from-end
			 (cursor-prev curs)
			 (cursor-next curs))
		   (declare (dynamic-extent exists? k v))
		   (if (and exists? (continue-p k))
		       (funcall fn k v)
		       (return nil)))))))))
    results))

(defgeneric map-index (fn index &rest args &key start end value from-end collect &allow-other-keys)
  (:documentation "Map-index is like map-btree but for secondary indices, it
   takes a function of three arguments: key, value and primary
   key.  As with map-btree the keyword arguments start and end
   determine the starting element and ending element, inclusive.
   Also, start = nil implies the first element, end = nil implies
   the last element in the index.  If you want to traverse only a
   set of identical key values, for example all nil values, then
   use the value keyword which will override any values of start
   and end.  The collect keyword will accumulate the results from
   each call of fn in a fresh list and return that list in the 
   same order the calls were made (first to last)"))

(defmethod map-index (fn (index btree-index) &rest args 
		      &key start end (value nil value-set-p) from-end collect 
		      &allow-other-keys)
  (declare (dynamic-extent args))
  (unless (or (null start) (null end) (lisp-compare<= start end))
    (error "map-index called with start = ~A and end = ~A. Start must be less than or equal to end according to elephant::lisp-compare<=."
	   start end))
  (let ((sc (get-con index))
	(end (or value end))
	(from-end (and from-end (not value-set-p)))
	(results nil))
    (flet ((collector (k v pk)
	     (push (funcall fn k v pk) results)))
      (let ((fn (if collect #'collector fn)))
      (declare (dynamic-extent (function collector)))
      (ensure-transaction (:store-controller sc :degree-2 *map-using-degree2*)
	(with-btree-cursor (cur index)
	  (labels ((continue-p (key) 
		     ;; Do we go to the next value?
		     (or (if from-end (null start) (null end))
			 (if from-end 
			     (or (not (lisp-compare<= key start))
				 (lisp-compare-equal key start))
			     (lisp-compare<= key end))))
		   (value-increment () 
		     ;; Step to the next key value;
		     ;; from-end duplicate cursor is already there
		     (if from-end 
			 (cursor-current cur)
			 (cursor-pnext-nodup cur)))
		   (map-values () 
		     ;; Handle the next key value
		     (multiple-value-bind (exists? skey val pkey)
			 (value-increment)
		       (if (and exists? (continue-p skey))
			   (progn
			     (funcall fn skey val pkey)
			     (map-duplicates skey))
			   (return-from map-index 
			     (nreverse results)))))
		   (next-duplicate (key)
		     (if from-end
			 (pprev-dup-hack cur key)
			 (cursor-pnext-dup cur)))
		   (map-duplicates (key) 
		     ;; Map all duplicates for key value
		     (multiple-value-bind (exists? skey val pkey) 
			 (next-duplicate key)
		       (if exists?
			   (progn
			     (funcall fn skey val pkey)
			     (map-duplicates key))
			   (progn
			     (unless from-end
			       (cursor-pset cur key))
			     (map-values))))))
	    (declare (dynamic-extent (function map-values) (function next-duplicate) 
				     (function continue-p) (function map-duplicates)))
	    (multiple-value-bind (exists? skey val pkey)
		(cond (value-set-p
		       (cursor-pset cur value))
		      ((and (not from-end) (null start))
		       (cursor-pfirst cur))
		      ((and from-end (null end))
		       (cursor-plast cur))
		      (t (if from-end 
			     (pset-range-for-descending cur end)
			     (cursor-pset-range cur start))))
	      (if (and exists? (continue-p skey))
		  (progn
		    (funcall fn skey val pkey)
		    (map-duplicates skey))
		  nil)))))))))

(defun pset-range-for-descending (cur end)
  (if (cursor-pset cur end)
      (progn
	(cursor-next-nodup cur)
	(cursor-pprev cur))
      (progn
	(cursor-pset-range cur end)
	(cursor-pprev cur))))

(defun pprev-dup-hack (cur key)
  "Go back one step in a duplicate set, returns nil 
   if previous element is a different key.  More efficient than
   the current default implementation of cursor-pprev-dup"
  (multiple-value-bind (exists? skey value pkey)
      (cursor-pprev cur)
    (when (lisp-compare-equal key skey)
      (values exists? key value pkey))))


;; ===============================
;; Some generic utility functions
;; ===============================

(defmethod empty-btree-p ((btree btree))
  (ensure-transaction (:store-controller (get-con btree))
    (with-btree-cursor (cur btree)
      (multiple-value-bind (valid k) (cursor-next cur)
	(cond ((not valid) ;; truly empty
	       t)
	      ((and (eq btree (controller-root (get-con btree)))
		    (eq k *elephant-properties-label*)) ;; has properties
	       (not (cursor-next cur)))
	      (t nil))))))

(defun print-btree-entry (k v) 
  (format t "key: ~A / value: ~A~%" k v))

(defun dump-btree (bt &key (print-fn #'print-btree-entry) (count nil))
  "Print the contents of a btree for easy inspection & debugging"
  (format t "DUMP ~A~%" bt)
  (let ((i 0))
  (map-btree 
   (lambda (k v)
     (when (and count (>= (incf i) count))
       (return-from dump-btree))
     (funcall print-fn k v))
   bt)))

(defun print-btree-key-and-type (k v)
  (format t "key ~A / value type ~A~%" k (type-of v)))

(defun btree-keys (bt &key (print-fn #'print-btree-key-and-type) (count nil))
  (format t "BTREE keys and types for ~A~%" bt)
  (dump-btree bt :print-fn print-fn :count count))

(defmethod btree-differ-p ((x btree) (y btree))
;;  (assert (eq (get-con x) (get-con y)))
  (ensure-transaction (:store-controller (get-con x))
    (ensure-transaction (:store-controller (get-con y))
      (let ((cx1 (make-cursor x)) 
	    (cy1 (make-cursor y))
	    (done nil)
	    (rv nil)
	    (mx nil)
	    (kx nil)
	    (vx nil)
	    (my nil)
	    (ky nil)
	    (vy nil))
	(cursor-first cx1)
	(cursor-first cy1)
	(do ((i 0 (1+ i)))
	    (done nil)
	  (multiple-value-bind (m k v) (cursor-current cx1)
	    (setf mx m)
	    (setf kx k)
	    (setf vx v))
	  (multiple-value-bind (m k v) (cursor-current cy1)
	    (setf my m)
	    (setf ky k)
	    (setf vy v))
	  (if (not (and (equal mx my)
			(equal kx ky)
			(equal vx vy)))
	      (setf rv (list mx my kx ky vx vy)))
	  (setf done (and (not mx) (not mx)))
	  (cursor-next cx1)
	  (cursor-next cy1)
	  )
	(cursor-close cx1)
	(cursor-close cy1)
	rv
	))))
