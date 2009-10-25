;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; sql-controller.lisp -- Interface to a CLSQL based object store.
;;; 
;;; Initial version 10/12/2005 by Robert L. Read
;;; <read@robertlread.net>
;;; 
;;; part of
;;;
;;; Elephant: an object-oriented database for Common Lisp
;;;
;;; Copyright (c) 2005-2007 by Robert L. Read
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;

(in-package :db-clsql)

;;
;; The main SQL Controller Class
;;

;; Every actual CL-SQL connection has to be in a separate thread.
;; My solution to this is to keep a map of threads, and reuse
;; connections within a certain thread.
;; This seems to be effective under SBCL; as of 06-Feb-2007 we 
;; don't necessarily have a way to do this under the other implementations
;; (see src/utils/lock.lisp.)


(defclass sql-store-controller (store-controller)
  (
;;   (db :accessor controller-db :initarg :db :initform nil)
   (dbcons :accessor controller-db-table :initarg :db :initform nil)
   (uses-pk :accessor uses-pk-of :initarg :uses-pk)
   )
  (:documentation  "Class of objects responsible for the
    book-keeping of holding DB handles, the cache, table
    creation, counters, locks, the root (for garbage collection,)
    et cetera.  This is the Postgresql-specific subclass of store-controller."))

(defmethod supports-sequence ((sc sql-store-controller))
  (not (equal 
	(car (cadr (controller-spec sc)))
	:sqlite3)
       )
)

;; This should be much more elegant --- but as of Feb. 6, SBCL 1.0.2 has a weird,
;; unpleasant bug when ASDF tries to load this stuff.
;; (defvar *thread-table-lock* nil)
;;  (defvar *thread-table-lock* (sb-thread::make-mutex :name "thread-table-lock"))

(defvar *thread-table-lock* nil)

(defun insure-thread-table-lock ()
  (if (null *thread-table-lock*)
;;      nil
;;  (setq *thread-table-lock* (sb-thread::make-mutex :name "thread-table-lock"))
      (setq *thread-table-lock* (elephant-utils::ele-make-lock))
      )
)


(defun thread-hash ()
  (elephant-utils::ele-thread-hash-key)
)


(defmethod controller-db ((sc sql-store-controller))
  (elephant::ele-with-lock (*thread-table-lock*)
  (let ((curcon (gethash (thread-hash) (controller-db-table sc))))
    (if curcon
        curcon
        (let* ((dbtype (car (second (controller-spec sc))))
               (con (clsql:connect (cdr (second (controller-spec sc)))
                               :database-type dbtype
                               :pool t
                               :if-exists :new)))
          (setf (gethash (thread-hash) (controller-db-table sc))
                con)
          con)
        )
    )
  ))


(eval-when (:compile-toplevel :load-toplevel)
  (register-data-store-con-init :clsql 'sql-test-and-construct))

(defun sql-test-and-construct (spec)
  "Entry function for making SQL data store controllers"
  (if (sql-store-spec-p spec)
      (make-instance 'sql-store-controller 
		     :spec (if spec spec
			       '("localhost.localdomain" "test" "postgres" "")))
      (error (format nil "uninterpretable path/spec specifier: ~A" spec))))

(defun sql-store-spec-p (spec)
  (and (listp spec)
       (eq (first spec) :clsql)))

;;
;; Controller Indices
;;


;; When you build one of these, you have to put in the connection spec.
(defclass sql-btree (btree) ()
  (:documentation "A SQL implementation of a BTree"))

(defmethod build-btree ((sc sql-store-controller))
  (make-instance 'sql-btree :sc sc)
  )

(defmethod get-value (key (bt sql-btree))
  (let* ((sc (get-con bt)))
    (sql-get-from-clcn (oid bt) key sc)
    )
  )

(defmethod (setf get-value) (value key (bt sql-btree))
  (let* ((sc (get-con bt)))
    (sql-add-to-clcn (oid bt) key value sc)
    )
  )

(defmethod existsp (key (bt sql-btree))
  (let* ((sc (get-con bt)))
    (sql-from-clcn-existsp (oid bt) key sc)
    )
  )

(defmethod remove-kv (key (bt sql-btree))
  (let* ((sc (get-con bt)))
    (sql-remove-one-from-clcn (oid bt)
			      key
			      sc
			      ))
  )


;; Because these things are transient, I can't move them
;; directly into the class above.  I am not sure how best to
;; handle this problem.
(defclass sql-indexed-btree (indexed-btree sql-btree )
  ((indices :accessor indices :initform (make-hash-table))
   (indices-cache :accessor indices-cache :initform (make-hash-table)
		  :transient t))
  (:metaclass persistent-metaclass)
  (:documentation "A SQL-based BTree that supports secondary indices."))

(defmethod shared-initialize :after ((instance sql-indexed-btree) slot-names
				     &rest rest)
  (declare (ignore slot-names rest))
  (setf (indices-cache instance) (indices instance)))

(defmethod build-indexed-btree ((sc sql-store-controller))
  (make-instance 'sql-indexed-btree :sc sc))

(defmethod build-btree-index ((sc sql-store-controller) &key primary key-form)
  (make-instance 'sql-btree-index :primary primary :key-form key-form :sc sc))


;; ISE NOTE: Much of the index management functionality is common between 
;; bdb and sql - we could lift this along with indices and indices-cache 
;; up to the main elephant code base and introduce a new update-index 
;; generic function to handle the data store specific method for updating
(defmethod map-indices (fn (bt sql-indexed-btree))
  (maphash fn (indices-cache bt)))

(defmethod get-index ((bt sql-indexed-btree) index-name)
  (gethash index-name (indices-cache bt)))

(defmethod remove-index ((bt sql-indexed-btree) index-name)
  (remhash index-name (indices-cache bt))
  (let ((indices (indices bt)))
    (remhash index-name indices)
    (setf (indices bt) indices))
  )

(defmethod add-index ((bt sql-indexed-btree) &key index-name key-form populate)
  (let* ((sc (get-con bt)))
    (if (and (not (null index-name))
	     (symbolp index-name) (or (symbolp key-form) (listp key-form)))
	(let ((indices (indices bt))
	      (index (build-btree-index sc :primary bt :key-form key-form)))
	  (setf (gethash index-name (indices-cache bt)) index)
	  (setf (gethash index-name indices) index)
	  (setf (indices bt) indices)
	  (when populate
	    (let ((key-fn (key-fn index))
		  )
	      (with-transaction (:store-controller sc)
		(map-btree
		 #'(lambda (k v)
		     (multiple-value-bind (index? secondary-key)
			 (funcall key-fn index k v)
		       ;; This is a slow, DB cycle intensive operation.  It could chunked somehow,
		       ;; I think, probably making it 10 times faster.
		       (when index?
			 (unless (sql-from-clcn-key-and-value-existsp 
				  (oid index) secondary-key k sc)
			   (sql-add-to-clcn (oid index)
					    secondary-key
					    k
					    sc :insert-only t))
			 )))
		 bt))))
	  index)
	(error "Invalid index initargs!"))))

(defmethod (setf get-value) (value key (bt sql-indexed-btree))
  "Set a key / value pair, and update secondary indices."
  (let* ((sc (get-con bt))
	 (indices (indices-cache bt)))
    (with-transaction (:store-controller sc)
      (maphash 
       #'(lambda (k index) 
	   (declare (ignore k))
	   (multiple-value-bind (index? secondary-key)
	       (funcall (key-fn index) index key value)
	     (when index?
	       ;; This duplicates values that are already there...
	       (unless (sql-from-clcn-key-and-value-existsp 
			(oid index) secondary-key key sc)
		 (sql-add-to-clcn (oid index)
				  secondary-key
				  key
				  sc :insert-only t))
	       )))
       indices)
      ;; Now we place the actual value
      (sql-add-to-clcn (oid bt) key value sc)
      )
    value))

(defmethod remove-kv (key (bt sql-indexed-btree))
  "Remove a key / value pair, and update secondary indices."
  (declare (optimize (speed 3)))
  (let* (
	 (sc (get-con bt))
	 )
    (with-transaction (:store-controller sc)
      (let ((value (get-value key bt)))
	(when value
	  (let ((indices (indices-cache bt)))
	    (maphash 
	     #'(lambda (k index) 
		 (declare (ignore k))
		 (multiple-value-bind (index? secondary-key)
		     (funcall (key-fn index) index key value)
		   (when index?
		     ;; This function will in fact remove all of the 
		     ;; duplicate keys; but this is not how the BDB system works.
		     ;; It appears to me, based on the behavior of tests, that 
		     ;; this should remove the FIRST row that match not all.
		     (sql-remove-key-and-value-from-clcn (oid index)
							 secondary-key
							 key
							 sc)
		     ;; And furthermore, we have to remove the index entry
		     ;;		     (remove-kv secondary-key index)
		     )))
	     indices)
	    ;; Now we place the actual value
	    (sql-remove-from-clcn (oid bt) key sc))
	  )
	value))))


(defclass sql-btree-index (btree-index sql-btree)
  ()
  (:metaclass persistent-metaclass)
  (:documentation "A SQL-based BTree supports secondary indices."))


(clsql::locally-enable-sql-reader-syntax) 

;; Check that the table exists and is in proper form.
;; If it is not in proper form, signal an error, no 
;; way to recover from that automatically.  If it 
;; does not exist, return nil so we can create it later!


(defun version-table-exists (con)
  ;; we want to use ":owner :all" because we don't really care who created
  ;; the table, as long as we have the rights we need!
  (clsql:table-exists-p [version] :database con :owner :all)
  )
(defun sqlite3-harmless-read (sc)
  (let ((con (controller-db sc)))
    (if
     (equal 
      (car (cadr (controller-spec sc)))
      :sqlite3)
     (handler-case 
	 (clsql:query "select count(*) from keyvalue")
       ((clsql-sys::sql-database-error () nil)
	   )
       )
     )
    ))
(defun create-version-table (sc)
  (let ((con (controller-db sc)))
    (clsql::create-table [version]
			 '(
			   ([dbversion] text :not-null)
			   ) :database con
			 )
    (sqlite3-harmless-read sc)
    (let ((version 
	   (if (clsql:table-exists-p [keyvalue] :database con :owner :all)
	       (if (= 0 (caar (clsql:query "select count(*) from keyvalue")))
		   *elephant-code-version*
		   '(0 6 0))
	       *elephant-code-version*)))
      (clsql::insert-records :into [version]
			     :attributes '(dbversion)
			     :values (list (format nil "~A" version))
			     :database con)
      )))

;; These functions are probably not cross-database portable...
(defun keyvalue-table-exists (con)
  ;; we want to use ":owner :all" because we don't really care who created
  ;; the table, as long as we have the rights we need!
  (clsql:table-exists-p [keyvalue] :database con :owner :all)
  )

;; Our goal here is to see if the "pk" column exists....
;; if it does, we can use a certain optimization the sql-get-from-clcn-nth.
;; Post 6.1 versions should have it, but 6.0 versions won't. 
;; My goal here is to be as robust as possible; there is no portable way
;; to add a column nicely.  If you want to upgrade (which will really only
;; help if you use duplicate keys), then do a migration from your old repository 
;; to a new repository.
(defun query-uses-pk (con)
  ;; we want to use ":owner :all" because we don't really care who created
  ;; the table, as long as we have the rights we need!
  (member "pk" (clsql:list-attributes [keyvalue] :database con :owner :all)
	  :test 'equal)
  )


;; This is just an initial version; it is possible that 
;; we might someday wish to use blobs instead; certainly, I am
;; storing blobs now in the Berkeley-db and we meed to make sure 
;; we are properly testing that.  However, blobs are awkward to 
;; handle, so I am going to do this first...
(defun create-keyvalue-table (sc)
  ;; the "serial" specifiation here does not seem to work, (
  ;; apparently not supported by clsql, so I have to execute these
  ;; commands specifically.  This may be a database-dependent way of doing
  ;; things, but sequences in general are NOT standardized across RDBMS.
  ;; I prefer sequence to support the "get-next-oid" command, but there 
  ;; ARE other ways of doing it that could make this more portable.
  ;;    (execute-command create :database con)
  ;;    (execute-command idx-id :database con)
  ;;    (execute-command idx-key :database con)
  ;; Danger:  Rather than use 'serial as a type, CLSQL appears to support
  ;; CREATE-SEQUENCE and SEQUENCE-NEXT.  That would solve our problem!

  ;; ALL OF THIS needs to be inside a transaction.
  (let* ((con (controller-db sc)))
;; At one time this was conditional, but all NEW repositories should have this.
    (if (supports-sequence sc) 
	(progn
	  (clsql::create-sequence [serial] :database con)
	  (sqlite3-harmless-read sc)
	  (clsql::execute-command
	   (format nil "create table keyvalue (
 pk integer PRIMARY KEY DEFAULT nextval('serial'),
 clctn_id integer NOT NULL,
 key varchar NOT NULL,
 value varchar
 )")
	 :database con)
	  )
	(clsql::create-table [keyvalue]
			   ;; This is most likely to work with any database system..
			   '(
			     ([clctn_id] integer :not-null)
			     ([key] text :not-null)
			     ([value] text)
			     ) 
			   :database con)
      )
    (sqlite3-harmless-read sc)
;;   	      :constraints '("PRIMARY KEY (clctn_id key)"
;;   				     "UNIQUE (clctn_id,key)")

  ;; apparently in postgres this is failing pretty awfully because 
  ;; sequence-exists-p return nil and then we get an error that the sequence exists!
  ;;    (unless (sequence-exists-p [persistent_seq])
  (clsql::create-sequence [persistent_seq] :database con)
  ;; Leave room for root and class-root
  (clsql::set-sequence-position [persistent_seq] 2 :database con)
  ;;)
  ;;    (unless (index-exists-p [idx_clctn_id])
  (clsql::create-index [idx_clctn_id] :on [keyvalue]
		       :attributes '([clctn_id])
		       :database con)
  ;; )
  ;;    (unless (index-exists-p [idx_key])
  (clsql::create-index [idx_key] :on [keyvalue]
		       :attributes '([key])
		       :database con)
  ;;)
  ;; This is actually unique
  ;;    (unless (index-exists-p [idx_both])
  (clsql:create-index [idx_both] :on [keyvalue]
		      :attributes '([clctn_id] [key])
		      :database con)
    (sqlite3-harmless-read sc)
  ;;)
  ))

(defmethod database-version ((sc sql-store-controller))
  "A version determination for a given store
   controller that is independant of the serializer as the
   serializer is dispatched based on the code version which is a
   list of the form '(0 6 0)"
  (let* ((con (controller-db sc)))
    (let ((tuples
	   (clsql::select [dbversion] 
			  :from [version]
			  :database con)))
      ;; The table should exists, but there may or may not be a record there...
      (if tuples 
	  (read-from-string (caar tuples))
	  nil))))

(defmethod open-controller ((sc sql-store-controller)
			    ;; At present these three have no meaning
			    &key 
			    &allow-other-keys)
  (insure-thread-table-lock)
  (the sql-store-controller
    (let* ((dbtype (car (second (controller-spec sc))))
	   (path (cadr (second (controller-spec sc))))
	   (new-p (or (eq :memory path)
		      (not (probe-file path))))
	   (con (clsql:connect (cdr (second (controller-spec sc)))
			       :database-type dbtype
			       :pool t
			       :if-exists :old)))
     (setf (controller-db-table sc) (make-hash-table :test 'equal))
     (setf (gethash (thread-hash) (controller-db-table sc)) con)
;;      (setf (slot-value sc 'db) con)
      ;; Now we should make sure that the KEYVALUE table exists, and, if 
      ;; it does not, we need to create it..
      (unless (keyvalue-table-exists con)
	  (with-transaction (:store-controller sc)
	    (create-keyvalue-table sc)))
      (setf (uses-pk-of sc) (query-uses-pk con))
      (unless (version-table-exists con)
	(with-transaction (:store-controller sc)
	  (create-version-table sc)))
      (initialize-serializer sc)
      ;; These should get oid 0 and 1 respectively 
      (setf (slot-value sc 'root) (make-instance 'sql-btree :sc sc :from-oid 0))
      (setf (slot-value sc 'class-root) (make-instance 'sql-btree :sc sc :from-oid 1))
      sc)
    )
  )

(defmethod connection-ok-p ((sc sql-store-controller))
  (connection-ok-p-con (controller-db sc)))

(defun connection-ok-p-con (con)
  (let ((str (format nil "~A" con)))
    (search "OPEN" str)
  ))

(defmethod connection-really-ok-p ((sc sql-store-controller))
  ;; I don't really have a good way of doing this, but
  ;; one thing that is sure is that the the print form should
  ;; have OPEN and not CLOSED in it.
  )

(defmethod controller-status ((sc sql-store-controller))
;; This is a crummy way to deal with status; we really want
;; to return something we can compute against.
  (clsql:status)
  )


(defmethod reconnect-controller ((sc sql-store-controller))
  (clsql:reconnect :database (controller-db sc) :force nil)
  )

(defmethod close-controller ((sc sql-store-controller))
  (maphash #'(lambda (k v)
               (ignore-errors
                 (if (connection-ok-p-con v)
                     (clsql:disconnect :database v)
                     )
               )
               )
           (controller-db-table sc)
           )
    (setf (slot-value sc 'root) nil)
    )

;; Because this is part of the public
;; interface that I'm tied to, it has to accept a store-controller...
(defmethod next-oid ((sc sql-store-controller ))
  (let ((con (controller-db sc)))
    (clsql:sequence-next [persistent_seq]
			 :database con))
  )

;; if add-to-root is a method, then we can make it class dependent...
;; otherwise we have to change the original code.  There is 
;; almost no way to implement this without either changing the existing
;; file.  If we can introduce a layer of class indirectio there, then
;; we can control things properly.  In the meantime, I will implement
;; a proper method myself, but I will give it a name so it doesn't 
;; conflict with 'add-to-root.  'add-to-root can remain a convenience symbol,
;; that will end up calling this routine!
(defun sql-add-to-root (key value sc)
  (sql-add-to-clcn 0 key value sc)
  )

(defun sql-add-to-clcn (clcn key value sc
			&key (insert-only nil))
  (assert (integerp clcn))
  (let ((con (controller-db sc))
	(vbs 
	 (serialize-to-base64-string value sc))
	(kbs
	 (serialize-to-base64-string key sc))
	)
    (if (and (not insert-only) (sql-from-clcn-existsp clcn key sc))
	(clsql::update-records [keyvalue]
			       :av-pairs `((key ,kbs)
					   (clctn_id ,clcn)
					   (value ,vbs))
			       :where [and [= [clctn_id] clcn] [= [key] kbs]]
			       :database con)
	(clsql::insert-records :into [keyvalue]
			       :attributes '(key clctn_id value)
			       :values (list kbs clcn vbs)
			       :database con
			       ))
    )
  value
  )


(defun sql-get-from-root (key sc)
  (sql-get-from-clcn 0 key sc)
  )

;; This is a major difference betwen SQL and BDB:
;; BDB plans to give you one value and let you iterate, but
;; SQL by nature returns a set of values (when the keys aren't unique.)
;; 
;; I serious problem here is what to do if the things aren't unique.
;; According to the Elepahnt documentation, you should get one value 
;; (not clear which one, the "first" probably, and then use a 
;; cursor to iterate over duplicates.  
;; So although it is moderately clear how the cursor is supposed to 
;; work, I'm not sure how I'm supposed to know what value should be 
;; returend by this non-cursor function.
;; I suspect if I return the value that has the lowest OID, that will
;; match the behavior of the sleepycat function....
;; To do that I have to read in all of the values and deserialized them
;; This could be a good reason to keep the oids out, and separte, in 
;; a separate column.
(defun sql-get-from-clcn (clcn key sc)
  (assert (integerp clcn))
  (sql-get-from-clcn-nth clcn key sc 0)
  )

(defun sql-get-from-clcn-nth (clcn key sc n)
  (assert (and (integerp clcn) (integerp n)))
  (let* ((con (controller-db sc))
	 (kbs 
	  (serialize-to-base64-string key sc))
	 (offsetquery (if (uses-pk-of sc) 
			  (format nil "select value from keyvalue where clctn_id = ~A and key = '~A' order by pk offset ~A limit 1 "
				  clcn
				  kbs
				  n)
			  nil))
	 (tuples
	  (if (uses-pk-of sc)
	      (clsql::query offsetquery :database con)
	      (clsql::select [value] 
			     :from [keyvalue]
			     :where [and [= [clctn_id] clcn] [= [key] kbs]]
			     :database con
			     )
	      )
	   )
	 )
    ;; Get the lowest value by sorting and taking the first value;
    ;; this isn't a very good way to do things...
    ;; Note also that this will be extremely inefficient if 
    ;; you have for example, a boolean index function.
    ;; I could parametrize this routine to take an "nth"
    ;; parameter.  But there is almost no way to implement
    ;; that efficiently without changing the database structure;
    ;; but that's OK, I could add a column to support that 
    ;; relatively easily later on.
    (if (uses-pk-of sc)
	(if tuples
	    (values (deserialize-from-base64-string (caar tuples) sc)
		    t)
	    (values nil nil))
	(if (< n (length tuples))
	    (values (nth n (sort 
			    (mapcar 
			     #'(lambda (x)
				 (deserialize-from-base64-string (car x) sc))
			     tuples)
			    #'my-generic-less-than))
		    t)
	    (values nil nil))
	)
    ))

(defun sql-get-from-clcn-cnt (clcn key sc)
  (assert (integerp clcn))
  (let* ((con (controller-db sc))
	 (kbs (serialize-to-base64-string key sc))
	 (tuples
	  (clsql::select [count [value]]
			 :from [keyvalue]
			 :where [and [= [clctn_id] clcn] [= [key] kbs]]
			 :database con
			 )))
    (caar tuples)))

(defun sql-dump-clcn (clcn sc)
  (assert (integerp clcn))
  (let* ((con (controller-db sc))
	 (tuples
	  (if (uses-pk-of sc)
	   (clsql::select [pk] [key] [value]
			 :from [keyvalue]
			 :where [and [= [clctn_id] clcn]]
			 :database con
			 )
 	   (clsql::select [key] [value]
 			 :from [keyvalue]
 			 :where [and [= [clctn_id] clcn]]
 			 :database con
 			 )
	   )
	   )
	 )
    (mapcar #'(lambda (x) (mapcar #'(lambda (q) (deserialize-from-base64-string q sc)) x))
	    tuples)))

(defun sql-from-root-existsp (key sc)
  (sql-from-clcn-existsp 0 key sc)
  )

(defun sql-from-clcn-existsp (clcn key sc)
  (assert (integerp clcn))
  (let* ((con (controller-db sc))
	 (kbs (with-buffer-streams (out-buf)
		(serialize-to-base64-string key sc))
	   )
	 (tuples
	  (clsql::select [value] 
			 :from [keyvalue]
			 :where [and [= [clctn_id] clcn] [= [key] kbs]]
			 :database con
			 )))
    (if tuples
	t
	nil)
    ))


(defun sql-from-clcn-key-and-value-existsp (clcn key value sc)
  (assert (integerp clcn))
  (let* ((con (controller-db sc))
	 (kbs (serialize-to-base64-string key sc))
	 (vbs (serialize-to-base64-string value sc))
	 (tuples (clsql::select [value]
				:from [keyvalue]
				:where [and [= [clctn_id] clcn] [= [key] kbs]
				[= [value] vbs]]
				:database con)))
    (if tuples
	t
	nil)))

(defun sql-remove-from-root (key sc)
  (sql-remove-from-clcn 0 key sc)
  )


(defun sql-remove-from-clcn (clcn key sc)
  (assert (integerp clcn))
  (let ((con (controller-db sc))
	(kbs (serialize-to-base64-string key sc))
	)
    (clsql::delete-records :from [keyvalue]
			   :where [and [= [clctn_id] clcn] [= [key] kbs]]
			   :database con
			   ))
  )
(defun sql-remove-one-from-clcn (clcn key sc)
  (assert (integerp clcn))
  (let* ((con (controller-db sc))
	 (kbs (serialize-to-base64-string key sc))
	 ;; We want to remove the FIRST value, based on our ordering.
	 ;; have little choice but to read everything in and delete based on
	 ;; the "value field".
	 (tuples
	  (clsql::select [value] 
			 :from [keyvalue]
			 :where [and [= [clctn_id] clcn] [= [key] kbs]]
			 :database con
			 ))
	 (n (length tuples)))
    ;;    (format t "num tuples = ~A~%" n)
    (if (< n 1)
	nil
	(let ((to-remove nil))
	  (dolist (tuple tuples)
	    (if (or (null to-remove)
		    (my-generic-less-than (car tuple) to-remove))
		(setf to-remove (car tuple))))
	  (clsql::delete-records :from [keyvalue]
				 :where [and [= [clctn_id] clcn] [= [key] kbs]
				 [= [value] to-remove]]
				 :database con
				 )
	  )
	)
    )
  )

(defun sql-remove-key-and-value-from-clcn (clcn key value sc)
  (assert (integerp clcn))
  (let* ((con (controller-db sc))
	 (kbs (serialize-to-base64-string key sc))
	 (vbs (serialize-to-base64-string value sc)))
    (clsql::delete-records :from [keyvalue]
			   :where [and [= [clctn_id] clcn] [= [key] kbs]
			   [= [value] vbs]]
			   :database con
			   )
    ))
	
(clsql::restore-sql-reader-syntax-state) 

(defun form-slot-key (oid name)
  (format nil "~A ~A" oid name)
  )

(defmethod persistent-slot-writer ((sc sql-store-controller) new-value instance name)
  (sql-add-to-root
   (form-slot-key (oid instance) name)
   new-value
   sc)
  )

(defmethod persistent-slot-reader ((sc sql-store-controller) instance name)
  (multiple-value-bind (v existsp)
      (sql-get-from-root
       (form-slot-key (oid instance) name)
       sc)
    (if existsp
	v
	(error  'unbound-slot :instance instance :name name))))

(defmethod persistent-slot-boundp ((sc sql-store-controller) instance name)
  (if (sql-from-root-existsp
       (form-slot-key (oid instance) name)
       sc )
      t nil))

(defmethod persistent-slot-makunbound ((sc sql-store-controller) instance name)
  (sql-remove-from-root
   (form-slot-key (oid instance) name) 
   sc)
  instance)


