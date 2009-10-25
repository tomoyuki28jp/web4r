;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; controller.lisp -- Lisp interface to a Berkeley DB store
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

(defclass bdb-store-controller (store-controller)
  ((environment :type (or null pointer-void) 
		:accessor controller-environment)
   (metadata :type (or null pointer-void) :accessor controller-metadata)
   (db :type (or null pointer-void) :accessor controller-db :initform '())
   (btrees :type (or null pointer-void) :accessor controller-btrees)
   (indices :type (or null pointer-void) :accessor controller-indices)
   (indices-assoc :type (or null pointer-void)
		  :accessor controller-indices-assoc)
   (oid-db :type (or null pointer-void) :accessor controller-oid-db)
   (oid-seq :type (or null pointer-void) :accessor controller-oid-seq)
   (deadlock-pid :accessor controller-deadlock-pid :initform nil)
   (deadlock-input :accessor controller-deadlock-input :initform nil))
  (:documentation "Class of objects responsible for the
book-keeping of holding DB handles, the cache, table
creation, counters, locks, the root (for garbage collection,)
et cetera."))

;;
;; Data store Registry Support
;;

(defun bdb-test-and-construct (spec)
  (if (bdb-store-spec-p spec)
      (make-instance 'bdb-store-controller :spec spec)
      (error (format nil "uninterpretable spec specifier: ~A" spec))))

(eval-when (:compile-toplevel :load-toplevel)
  (register-data-store-con-init :bdb 'bdb-test-and-construct))

(defun bdb-store-spec-p (spec)
  (and (eq (first spec) :bdb)
       (typecase (second spec)
	 (pathname t)
	 (string t)
	 (otherwise nil))))

;;
;; Store-specific transaction support
;;

(defmacro my-current-transaction (sc)
  (let ((txn-rec (gensym)))
    `(let ((,txn-rec *current-transaction*))
       (if (and ,txn-rec (eq (transaction-store ,txn-rec) ,sc))
	   (transaction-object ,txn-rec)
	   +NULL-CHAR+))))
;;
;; Open/close     
;;

(defmethod open-controller ((sc bdb-store-controller) &key (recover nil)
			    (recover-fatal nil) (thread t) 
			    (deadlock-detect nil))
  (let ((env (db-env-create))
	(new-p (not (probe-file (make-pathname :defaults (second (controller-spec sc))
					       :name "%ELEPHANT")))))
    (setf (controller-environment sc) env)
    (db-env-set-flags env 0 :auto-commit t)
    (db-env-set-cachesize env 0 elephant::*berkeley-db-cachesize* 1)
    (db-env-set-timeout env 100000 :set-transaction-timeout t)
    (db-env-set-timeout env 100000 :set-lock-timeout t)
    (db-env-open env (namestring (second (controller-spec sc)))
		 :create t :init-rep nil :init-mpool t :thread thread
		 :init-lock t :init-log t :init-txn t 
		 :recover recover :recover-fatal recover-fatal
		 )
    (let ((metadata (db-create env))
	  (db (db-create env))
	  (btrees (db-create env))
	  (indices (db-create env))
	  (indices-assoc (db-create env)))

      ;; Open metadata database
      (setf (controller-metadata sc) metadata)
      (db-open metadata :file "%ELEPHANT" :database "%METADATA" 
	       :auto-commit t :type DB-BTREE :create t :thread t)

      ;; Establish database version if new
      (when new-p (set-database-version sc))

      ;; Initialize serializer so we can load proper sorting C function
      ;; based on serializer type
      (initialize-serializer sc)

      ;; Open main class, slot-value and index databases
      (setf (controller-db sc) db)
      (db-open db :file "%ELEPHANT" :database "%ELEPHANTDB" 
	       :auto-commit t :type DB-BTREE :create t :thread thread
	       :read-uncommitted t)

      (setf (controller-btrees sc) btrees)
      (db-bdb::db-set-lisp-compare btrees (controller-serializer-version sc))
      (db-open btrees :file "%ELEPHANT" :database "%ELEPHANTBTREES" 
	       :auto-commit t :type DB-BTREE :create t :thread thread
	       :read-uncommitted t)

      (setf (controller-indices sc) indices)
      (db-bdb::db-set-lisp-compare indices (controller-serializer-version sc))
      (db-bdb::db-set-lisp-dup-compare indices (controller-serializer-version sc))
      (db-set-flags indices :dup-sort t)
      (db-open indices :file "%ELEPHANT" :database "%ELEPHANTINDICES" 
	       :auto-commit t :type DB-BTREE :create t :thread thread
	       :read-uncommitted t)

      (setf (controller-indices-assoc sc) indices-assoc)
      (db-bdb::db-set-lisp-compare indices-assoc (controller-serializer-version sc))
      (db-bdb::db-set-lisp-dup-compare indices-assoc (controller-serializer-version sc))
      (db-set-flags indices-assoc :dup-sort t)
      (db-open indices-assoc :file "%ELEPHANT" :database "%ELEPHANTINDICES" 
	       :auto-commit t :type DB-UNKNOWN :thread thread
	       :read-uncommitted t)
      (db-bdb::db-fake-associate btrees indices-assoc :auto-commit t)
      
      (let ((db (db-create env)))
	(setf (controller-oid-db sc) db)
	(db-open db :file "%ELEPHANTOID" :database "%ELEPHANTOID" 
		 :auto-commit t :type DB-BTREE :create t :thread thread)
	(let ((oid-seq (db-sequence-create db)))
	  (db-sequence-set-cachesize oid-seq 100)
	  (db-sequence-set-flags oid-seq :seq-inc t :seq-wrap t)
	  (db-sequence-set-range oid-seq 0 most-positive-fixnum)
	  (db-sequence-initial-value oid-seq 0)
	  (db-sequence-open oid-seq "%ELEPHANTOID" :create t :thread t)
	  (setf (controller-oid-seq sc) oid-seq)))

      (setf (slot-value sc 'root)
	    (make-instance 'bdb-btree :from-oid -1 :sc sc))

      (setf (slot-value sc 'class-root)
	    (make-instance 'bdb-btree :from-oid -2 :sc sc))

      (when deadlock-detect
	(start-deadlock-detector sc))

      sc)))

(defmethod close-controller ((sc bdb-store-controller))
  (when (slot-value sc 'root)
    (stop-deadlock-detector sc)
    ;; no root
    (setf (slot-value sc 'class-root) nil)
    (setf (slot-value sc 'root) nil)
    ;; clean instance cache
    (flush-instance-cache sc)
    ;; close handles / environment
    (db-sequence-close (controller-oid-seq sc))
    (setf (controller-oid-seq sc) nil)
    (db-close (controller-oid-db sc))
    (setf (controller-oid-db sc) nil)
    (db-close (controller-indices-assoc sc))
    (setf (controller-indices-assoc sc) nil)
    (db-close (controller-indices sc))
    (setf (controller-indices sc) nil)
    (db-close (controller-btrees sc))
    (setf (controller-btrees sc) nil)
    (db-close (controller-db sc))
    (setf (controller-db sc) nil)
    (db-close (controller-metadata sc))
    (setf (controller-metadata sc) nil)
    (db-env-close (controller-environment sc))
    (setf (controller-environment sc) nil)
    nil))

(defmethod next-oid ((sc bdb-store-controller))
  "Get the next OID."
  (declare (type bdb-store-controller sc))
  (db-sequence-get-fixnum (controller-oid-seq sc) 1 :transaction +NULL-VOID+
			  :txn-nosync t))

;;
;; Store the database version
;;
;; For BDB this can be in a file; different data stores may require a different approach.

(defmethod database-version ((sc bdb-store-controller))
  "Elephant protocol to provide the version tag or nil if unmarked"
  (with-buffer-streams (key val)
    (serialize-database-version-key key)
    (let ((buf (db-get-key-buffered (controller-metadata sc)
				    key val
				    :transaction +NULL-VOID+)))
      (if buf (deserialize-database-version-value buf)
	  nil))))

(defun set-database-version (sc)
  "Internal use when creating new database"
  (with-buffer-streams (key val)
    (serialize-database-version-key key)
    (serialize-database-version-value *elephant-code-version* val)
    (db-put-buffered (controller-metadata sc)
		     key val
		     :transaction +NULL-VOID+)
    *elephant-code-version*))

;; (defmethod old-database-version ((sc bdb-store-controller))
;;    "A version determination for a given store
;;    controller that is independant of the serializer as the
;;    serializer is dispatched based on the code version which is a
;;    list of the form '(0 6 0)"
;;   (let ((version (elephant::controller-version-cached sc)))
;;     (if version version
;; 	(let ((path (make-pathname :name "VERSION" :defaults (second (controller-spec sc)))))
;; 	  (if (probe-file path)
;; 	      (with-open-file (stream path :direction :input)
;; 		(setf (elephant::controller-version-cached sc) (read stream)))
;; 	      (with-open-file (stream path :direction :output)
;; 		(setf (elephant::controller-version-cached sc)
;; 		      (write *elephant-code-version* :stream stream))))))))

;;
;; Automated Deadlock Support
;;

(defparameter *deadlock-type-alist*
  '((:oldest . "o")
    (:youngest . "y")
    (:timeout . "e")
    (:most . "m")
    (:least . "n")))

(defun lookup-deadlock-type (typestring)
  (let ((result (assoc typestring *deadlock-type-alist*)))
    (unless result
      (error "Unrecognized deadlock type '~A'" typestring))
    (cdr result)))

(defmethod start-deadlock-detector ((ctrl bdb-store-controller) &key (type :oldest) (time 0.1) log)
  (let ((process-handle 
	 (launch-background-program 
	  (second (controller-spec ctrl))
	  (namestring 
	   (make-pathname :defaults (get-user-configuration-parameter :berkeley-db-deadlock)))
	  :args `("-a" ,(lookup-deadlock-type type)
		       "-t" ,(format nil "~D" time)
		       ,@(when log (list "-L" (format nil "~A" log)))))))
    (declare (ignore str errstr))
    (setf (controller-deadlock-pid ctrl) process-handle)))
			
(defmethod stop-deadlock-detector ((ctrl bdb-store-controller))
  (when (controller-deadlock-pid ctrl)
    (kill-background-program (controller-deadlock-pid ctrl))))

;;
;; Enable program-based checkpointing
;;
  
(defmethod checkpoint ((sc bdb-store-controller) &key force (time 0) (log-size 0))
  "Forces a checkpoint of the db and flushes the memory pool to disk.
   Use keywords ':force t' to write the checkpoint under any
   condition, ':time N' to checkpoint based on if the number of
   minutes since the last checkpoint is greater than time and
   ':log-size N' to checkpoint if the data written to the log is
   greater than N kilobytes"
  (db-env-txn-checkpoint (controller-environment sc) log-size time :force force))

;;
;; Take advantage of release 4.4's compact storage feature.  Feature of BDB only
;;

(defmethod optimize-layout ((ctrl bdb-store-controller) &key start-key stop-key 
			    (freelist-only t) (free-space nil)
			    &allow-other-keys)
  "Tell the data store to optimize and reclaim storage between key values"
  (with-buffer-streams (start stop end)
    (if (null start-key)
	(progn 
	  (db-compact (controller-indices ctrl) nil nil end :transaction +NULL-VOID+)
	  (db-compact (controller-db ctrl) nil nil end :transaction +NULL-VOID+)
	  (db-compact (controller-btrees ctrl) nil nil end :transaction +NULL-VOID+))
	(progn
	  (serialize start-key start ctrl)
	  (when stop-key (serialize stop-key stop ctrl))
	  (db-compact (controller-indices ctrl) start
		      (when stop-key stop) end
		      :freelist-only freelist-only
		      :free-space free-space
		      :transaction +NULL-VOID+)
	  (db-compact (controller-db ctrl) nil
		      (when stop-key stop) end
		      :freelist-only freelist-only
		      :free-space free-space
		      :transaction +NULL-VOID+)
	  (db-compact (controller-btrees ctrl) nil
		      (when stop-key stop) end
		      :freelist-only freelist-only
		      :free-space free-space
		      :transaction +NULL-VOID+)))
    (values (deserialize end ctrl))))

